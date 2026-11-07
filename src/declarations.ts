import {
  type Capture,
  type Colon,
  DEFAULT_COMMENT_PREFIX,
  type Line,
  type Shaped,
  type Shapeless,
  type Template,
  classifyLine,
  classifyShape,
  fillTemplate,
  parseTemplate,
  placeholderNames,
} from "./lines.ts";

const META_NAME = "LanguageDeclarations";
const GRAMMAR_TEXT: ReadonlyMap<number, string> = new Map([
  [2, await Bun.file(new URL("./grammar/v2.typify", import.meta.url)).text()],
  [3, await Bun.file(new URL("./grammar/v3.typify", import.meta.url)).text()],
]);
const KNOWN_VERSION = Math.max(...GRAMMAR_TEXT.keys());
const KNOWN_VERSIONS = [...GRAMMAR_TEXT.keys()].join(" and ");

export type Diagnostic = {
  readonly line: number;
  readonly severity: "error" | "note";
  readonly text: string;
};

export const errorAt = (line: number, text: string): Diagnostic => ({
  line,
  severity: "error",
  text,
});

const noteAt = (line: number, text: string): Diagnostic => ({ line, severity: "note", text });

export const isError = (diagnostic: Diagnostic): boolean => diagnostic.severity === "error";

export const notALine = (line: number, { expected }: Shapeless): Diagnostic =>
  errorAt(line, `not a legal line: expected ${expected}`);

export type Domain =
  | { readonly kind: "literals"; readonly text: string; readonly words: readonly string[] }
  | { readonly kind: "sentence"; readonly text: string };

type Literals = Extract<Domain, { kind: "literals" }>;

export type Production =
  | { readonly shape: "bang"; readonly verb: Template }
  | { readonly shape: "colon"; readonly verb: Template; readonly argument: Template | Literals }
  | { readonly shape: "arrow"; readonly left: Template; readonly right: Template };

export type Form = {
  readonly line: number;

  readonly text: string;

  readonly production: Production | Shapeless;
};

export type Language = {
  readonly name: string;

  readonly line: number;

  readonly version: number | null;

  readonly commentPrefix: string;

  readonly order: "strict" | "free";
  readonly classes: ReadonlyMap<string, Domain>;

  readonly forms: readonly Form[];

  readonly once: ReadonlySet<string>;
};

export type Reading = {
  readonly diagnostics: readonly Diagnostic[];

  readonly meta: Language | null;

  readonly languages: ReadonlyMap<string, Language>;
};

type Row = { readonly number: number; readonly raw: string; readonly line: Shaped };

type Body = { readonly name: string; readonly line: number; readonly rows: readonly Row[] };

type Stated = { readonly line: number; readonly text: string };

export function readDeclarations(text: string): Reading {
  const rows = text.split("\n");
  const start = rows.findIndex(
    (raw) => classifyLine(raw, DEFAULT_COMMENT_PREFIX).shape !== "nothing",
  );
  const header = classifyLine(rows[start] ?? "", DEFAULT_COMMENT_PREFIX);
  const failed = (text: string): Reading => ({
    diagnostics: [errorAt(Math.max(start, 0) + 1, text)],
    meta: null,
    languages: new Map(),
  });
  if (header.shape !== "colon" || header.verb !== "typify") {
    return failed(
      "the file must open with `typify : <version>`; language bodies follow the selector",
    );
  }
  const version = WHOLE_NUMBER.test(header.argument) ? Number(header.argument) : NaN;
  const resource = GRAMMAR_TEXT.get(version);
  if (resource === undefined) {
    return failed(
      `unsupported typify version '${header.argument}'; this reader knows versions ${KNOWN_VERSIONS}`,
    );
  }
  const grammar = readGrammar(resource, version);
  if (grammar.meta === null) {
    throw new Error(`invalid canonical typify grammar version ${version}`);
  }
  const further = readFurtherBodies(rows, start + 1, grammar.meta);
  return {
    diagnostics: inLineOrder(further.diagnostics),
    meta: grammar.meta,
    languages: further.languages,
  };
}

export function readGrammar(text: string, expectedVersion: number): Reading {
  const rows = text.split("\n");
  const start = rows.findIndex(
    (raw) => classifyLine(raw, DEFAULT_COMMENT_PREFIX).shape !== "nothing",
  );
  const opening = classifyLine(rows[start] ?? "", DEFAULT_COMMENT_PREFIX);
  const failed = (diagnostics: readonly Diagnostic[]): Reading => ({
    diagnostics: inLineOrder(diagnostics),
    meta: null,
    languages: new Map(),
  });
  if (!GRAMMAR_TEXT.has(expectedVersion)) {
    return failed([errorAt(1, `unsupported canonical grammar version ${expectedVersion}`)]);
  }
  if (!isDeclare(opening) || opening.argument !== META_NAME) {
    return failed([
      errorAt(
        Math.max(start, 0) + 1,
        `a canonical grammar must open with \`declare : ${META_NAME}\``,
      ),
    ]);
  }
  const taken = takeBody(rows, start, META_NAME, DEFAULT_COMMENT_PREFIX, true);
  const read = readBody(taken.body, null);
  const versions = stated(taken.body.rows, "version");
  const diagnostics = [...taken.errors, ...read.diagnostics];
  if (versions.length !== 1 || versions[0]?.text !== String(expectedVersion)) {
    diagnostics.push(
      errorAt(
        versions[0]?.line ?? start + 1,
        `the canonical grammar must state version ${expectedVersion} exactly once`,
      ),
    );
  }
  for (let index = taken.next; index < rows.length; index += 1) {
    if (classifyLine(rows[index] ?? "", read.language.commentPrefix).shape !== "nothing") {
      diagnostics.push(
        errorAt(index + 1, "a canonical grammar resource contains exactly one body"),
      );
    }
  }
  if (diagnostics.some(isError)) return failed(diagnostics);
  return {
    diagnostics: inLineOrder(diagnostics),
    meta: read.language,
    languages: new Map([[read.language.name, read.language]]),
  };
}

const inLineOrder = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.toSorted((one, other) => one.line - other.line);

function versionOf(rows: readonly Row[]): number | null {
  const text = said(rows, "version")
    .map((line) => line.text)
    .findLast((candidate) => WHOLE_NUMBER.test(candidate));
  return text === undefined ? null : Number(text);
}

type Further = {
  readonly languages: ReadonlyMap<string, Language>;
  readonly diagnostics: readonly Diagnostic[];
};

function readFurtherBodies(rows: readonly string[], from: number, meta: Language): Further {
  const languages = new Map([[meta.name, meta]]);
  const diagnostics: Diagnostic[] = [];
  const sees: Stated[] = [];
  let index = from;
  while (index < rows.length) {
    const line = classifyLine(rows[index] ?? "", meta.commentPrefix);
    if (!isDeclare(line)) {
      diagnostics.push(...strayErrors(line, index + 1));
      index += 1;
      continue;
    }
    const taken = takeBody(rows, index, line.argument, meta.commentPrefix, false);
    const read = readBody(taken.body, meta);
    const earlier = languages.get(taken.body.name);
    if (earlier === undefined) languages.set(taken.body.name, read.language);
    else diagnostics.push(declaredTwice(taken.body, earlier));
    diagnostics.push(...taken.errors, ...read.diagnostics, ...nameErrors(taken.body, meta));
    sees.push(...said(taken.body.rows, "sees"));
    index = taken.next;
  }
  return { languages, diagnostics: [...diagnostics, ...seesErrors(sees, languages)] };
}

function strayErrors(line: Line, number: number): readonly Diagnostic[] {
  if (line.shape === "nothing") return [];
  if (line.shape === "none") return [notALine(number, line)];
  const expected = "a body opens with `declare : <Name>` and closes with `end!`";
  return [errorAt(number, `this line is outside any body: ${expected}`)];
}

const NAME = /^[A-Za-z][A-Za-z0-9]*$/;

function nameErrors(body: Body, meta: Language): readonly Diagnostic[] {
  const listed = meta.classes.get("Name")?.kind === "literals";
  if (listed || body.name === "" || NAME.test(body.name)) return [];
  const expected = "one word of letters and digits, beginning with a letter, is expected";
  return [errorAt(body.line, `declare : '${body.name}' is not a name: ${expected}`)];
}

function declaredTwice(body: Body, earlier: Language): Diagnostic {
  const expected = "a name is declared once";
  return errorAt(
    body.line,
    `${body.name} is declared twice (first at line ${earlier.line}): ${expected}`,
  );
}

function seesErrors(
  sees: readonly Stated[],
  languages: ReadonlyMap<string, Language>,
): readonly Diagnostic[] {
  const declared = [...languages.keys()].join(", ");
  return sees
    .filter(({ text }) => !languages.has(text))
    .map(({ line, text }) =>
      errorAt(
        line,
        `sees : ${text} names no body of this file: a declared name is expected (${declared})`,
      ),
    );
}

type Taken = {
  readonly body: Body;

  readonly next: number;
  readonly errors: readonly Diagnostic[];
};

const isDeclare = (line: Line): line is Colon => line.shape === "colon" && line.verb === "declare";

const isEnd = (line: Line): boolean => line.shape === "bang" && line.verb === "end";

function takeBody(
  rows: readonly string[],
  start: number,
  name: string,
  commentPrefix: string,
  followsOwnComment: boolean,
): Taken {
  const taken: Row[] = [];
  const errors: Diagnostic[] = [];
  const body: Body = { name, line: start + 1, rows: taken };
  let prefix = commentPrefix;
  for (let index = start; index < rows.length; index += 1) {
    const raw = rows[index] ?? "";
    const line = classifyLine(raw, prefix);
    if (line.shape === "nothing") continue;
    if (line.shape === "none") {
      errors.push(notALine(index + 1, line));
      continue;
    }
    if (index > start && isDeclare(line)) {
      const open = `${name}, opened at line ${start + 1}, has no end!`;
      errors.push(errorAt(index + 1, `a body may not open inside another: ${open}`));
      return { body, next: index, errors };
    }
    taken.push({ number: index + 1, raw, line });
    if (isEnd(line)) return { body, next: index + 1, errors };
    if (followsOwnComment && line.shape === "colon" && line.verb === "comment") {
      prefix = line.argument === "" ? prefix : line.argument;
    }
  }
  errors.push(errorAt(start + 1, `the body ${name} has no end!: a body closes with \`end!\``));
  return { body, next: rows.length, errors };
}

type Read = { readonly language: Language; readonly diagnostics: readonly Diagnostic[] };

function readBody(body: Body, meta: Language | null): Read {
  const language = languageOf(body);
  const through = meta ?? language;
  const resolve = resolver(language, through);

  const version = meta?.version ?? language.version ?? KNOWN_VERSION;
  return {
    language,
    diagnostics: [
      ...lineErrors(body.rows, through),
      ...pairingErrors(body.rows),
      ...productionErrors(language.forms),
      ...exampleErrors(body.rows, language.forms, resolve),
      ...onceErrors(body.rows, language),
      ...versionErrors(body.rows),
      ...unboundNotes(language, resolve, meta === null, version),
    ],
  };
}

function stated(rows: readonly Row[], verb: string): readonly Stated[] {
  return rows.flatMap(({ number, line }) =>
    line.shape === "colon" && line.verb === verb ? [{ line: number, text: line.argument }] : [],
  );
}

function said(rows: readonly Row[], verb: string): readonly Stated[] {
  return stated(rows, verb).filter(({ text }) => text !== "");
}

function languageOf(body: Body): Language {
  return {
    name: body.name,
    line: body.line,
    version: versionOf(body.rows),
    commentPrefix: said(body.rows, "comment").at(-1)?.text ?? DEFAULT_COMMENT_PREFIX,
    order: said(body.rows, "order").at(-1)?.text === "strict" ? "strict" : "free",
    classes: classesOf(body.rows),
    forms: stated(body.rows, "form").map(({ line, text }) => ({
      line,
      text,
      production: parseProduction(text),
    })),
    once: new Set(said(body.rows, "once").map(({ text }) => text)),
  };
}

const BINDS = " = ";

function classesOf(rows: readonly Row[]): ReadonlyMap<string, Domain> {
  return new Map(
    stated(rows, "is")
      .filter(({ text }) => text.includes(BINDS))
      .map(({ text }) => {
        const at = text.indexOf(BINDS);
        return [text.slice(0, at).trim(), parseDomain(text.slice(at + BINDS.length))] as const;
      }),
  );
}

function parseDomain(text: string): Domain {
  return text.includes("|")
    ? { kind: "literals", text: text.trim(), words: text.split("|").map((word) => word.trim()) }
    : { kind: "sentence", text: text.trim() };
}

function parseProduction(text: string): Production | Shapeless {
  const line = classifyShape(text);
  switch (line.shape) {
    case "bang":
      return { shape: "bang", verb: parseTemplate(line.verb) };
    case "colon":
      return {
        shape: "colon",
        verb: parseTemplate(line.verb),
        argument: parseArgument(line.argument),
      };
    case "arrow":
      return { shape: "arrow", left: parseTemplate(line.left), right: parseTemplate(line.right) };
    case "none":
      return line;
  }
}

function parseArgument(text: string): Template | Literals {
  const template = parseTemplate(text);
  const domain = parseDomain(text);
  return domain.kind === "literals" && placeholderNames(template).length === 0 ? domain : template;
}

function lineErrors(rows: readonly Row[], meta: Language): readonly Diagnostic[] {
  const resolve = resolver(meta, meta);
  return rows.flatMap(({ number, line }) => {
    const found = findForm(meta, resolve, line);
    return found.found ? [] : [errorAt(number, found.why)];
  });
}

function pairingErrors(rows: readonly Row[]): readonly Diagnostic[] {
  const formWithoutDoes =
    "this form is not followed by its does: the next line must be `does : <text>`";
  const doesWithoutForm = "this does has no form directly above it: a form has exactly one does";
  const errors: Diagnostic[] = [];
  let awaiting: number | null = null;
  for (const { number, line } of rows) {
    const verb = line.shape === "colon" ? line.verb : null;
    if (awaiting !== null && verb !== "does") errors.push(errorAt(awaiting, formWithoutDoes));
    if (awaiting === null && verb === "does") errors.push(errorAt(number, doesWithoutForm));
    awaiting = verb === "form" ? number : null;
  }
  return awaiting === null ? errors : [...errors, errorAt(awaiting, formWithoutDoes)];
}

function productionErrors(forms: readonly Form[]): readonly Diagnostic[] {
  return forms.flatMap(({ line, text, production }) =>
    production.shape === "none" && text !== ""
      ? [
          errorAt(
            line,
            `this production has none of the three line shapes: expected ${production.expected}`,
          ),
        ]
      : [],
  );
}

function exampleErrors(
  rows: readonly Row[],
  forms: readonly Form[],
  resolve: Resolver,
): readonly Diagnostic[] {
  return said(rows, "example").flatMap(({ line, text }) => {
    const form = forms.findLast((candidate) => candidate.line < line);
    if (form === undefined) {
      const expected = "an example gives a line for the form just declared";
      return [errorAt(line, `this example has no form above it: ${expected}`)];
    }
    if (form.production.shape === "none") return [];
    const example = classifyShape(text);
    if (example.shape === "none") {
      return [errorAt(line, `the example is not a legal line: expected ${example.expected}`)];
    }
    const missed = miss(form.production, example, resolve);
    const above = `the form above it, \`${form.text}\``;
    return missed === null
      ? []
      : [errorAt(line, `the example does not fit ${above}: ${missed.why}`)];
  });
}

export function verbOf(form: Form): string | null {
  const { production } = form;
  return production.shape === "bang" || production.shape === "colon" ? production.verb.text : null;
}

function onceErrors(rows: readonly Row[], language: Language): readonly Diagnostic[] {
  const verbs = language.forms.flatMap((form) => verbOf(form) ?? []);
  const offered = verbs.length > 0 ? verbs.join(", ") : "it has none";
  return said(rows, "once")
    .filter(({ text }) => !verbs.includes(text))
    .map(({ line, text }) =>
      errorAt(
        line,
        `once : ${text} names no form of ${language.name}: the verb of one of its forms is expected (${offered})`,
      ),
    );
}

const WHOLE_NUMBER = /^[0-9]+$/;

function versionErrors(rows: readonly Row[]): readonly Diagnostic[] {
  return said(rows, "version")
    .filter(({ text }) => !WHOLE_NUMBER.test(text))
    .map(({ line, text }) =>
      errorAt(line, `version : '${text}' is not a version: a whole number is expected`),
    );
}

function unboundNotes(
  language: Language,
  resolve: Resolver,
  isMeta: boolean,
  version: number,
): readonly Diagnostic[] {
  const firstUse = new Map<string, number>();
  for (const form of language.forms) {
    for (const name of placeholdersOf(form.production)) {
      if (resolve(name) === undefined && !firstUse.has(name)) firstUse.set(name, form.line);
    }
  }
  const where = isMeta ? META_NAME : `${language.name} or in ${META_NAME}`;
  return [...firstUse].map(([name, line]) =>
    noteAt(
      line,
      `<${name}> has no \`is\` line in ${where}; version ${version} reads it as free text`,
    ),
  );
}

function placeholdersOf(production: Production | Shapeless): readonly string[] {
  switch (production.shape) {
    case "bang":
      return placeholderNames(production.verb);
    case "colon":
      return [
        ...placeholderNames(production.verb),
        ...("words" in production.argument ? [] : placeholderNames(production.argument)),
      ];
    case "arrow":
      return [...placeholderNames(production.left), ...placeholderNames(production.right)];
    case "none":
      return [];
  }
}

export type Resolver = (name: string) => Domain | undefined;

export function resolver(own: Language, meta: Language): Resolver {
  return (name) => own.classes.get(name) ?? meta.classes.get(name);
}

export type Found =
  | { readonly found: true; readonly index: number; readonly form: Form }
  | { readonly found: false; readonly why: string };

export function findForm(language: Language, resolve: Resolver, line: Shaped): Found {
  const near: string[] = [];
  for (const [index, form] of language.forms.entries()) {
    if (form.production.shape === "none") continue;
    const missed = miss(form.production, line, resolve);
    if (missed === null) return { found: true, index, form };
    if (missed.near) near.push(`${missed.why} (form \`${form.text}\`)`);
  }
  return { found: false, why: near.length > 0 ? near.join("; ") : noSuchForm(language, line) };
}

export function capturesOf(form: Form, line: Shaped): readonly Capture[] | null {
  const production = form.production;
  let templates: [Template, string][];
  switch (production.shape) {
    case "bang":
      if (line.shape !== "bang") return null;
      templates = [[production.verb, line.verb]];
      break;
    case "colon":
      if (line.shape !== "colon") return null;
      templates = [[production.verb, line.verb]];
      if (!("words" in production.argument)) templates.push([production.argument, line.argument]);
      break;
    case "arrow":
      if (line.shape !== "arrow") return null;
      templates = [
        [production.left, line.left],
        [production.right, line.right],
      ];
      break;
    case "none":
      return null;
  }
  const captures: Capture[] = [];
  for (const [template, text] of templates) {
    const filled = fillTemplate(template, text);
    if (filled === null) return null;
    captures.push(...filled.map(({ name, text: token }) => ({ name, text: token.trim() })));
  }
  return captures;
}

function noSuchForm(language: Language, line: Shaped): string {
  const sameShape = language.forms
    .filter((form) => form.production.shape === line.shape)
    .map((form) => `\`${form.text}\``);
  const offered =
    sameShape.length > 0
      ? `its ${line.shape} forms are ${sameShape.join(", ")}`
      : `it declares no ${line.shape} form`;
  return `${language.name} declares no form for this line; ${offered}`;
}

type Miss = { readonly near: boolean; readonly why: string };

function miss(production: Production, line: Shaped, resolve: Resolver): Miss | null {
  const otherShape: Miss = { near: false, why: `a ${production.shape} line is expected` };
  switch (production.shape) {
    case "bang":
      return line.shape === "bang" ? verbMiss(production.verb, line.verb, resolve) : otherShape;
    case "colon": {
      if (line.shape !== "colon") return otherShape;
      const verb = verbMiss(production.verb, line.verb, resolve);
      return verb ?? nearMiss(argumentMisfit(production.argument, line.argument, resolve));
    }
    case "arrow": {
      if (line.shape !== "arrow") return otherShape;
      const left = misfit(production.left, line.left, resolve);
      return nearMiss(left ?? misfit(production.right, line.right, resolve));
    }
  }
}

const nearMiss = (why: string | null): Miss | null => (why === null ? null : { near: true, why });

function verbMiss(verb: Template, text: string, resolve: Resolver): Miss | null {
  const why = misfit(verb, text, resolve);
  if (why === null) return null;
  return placeholderNames(verb).length > 0
    ? { near: true, why }
    : { near: false, why: `the verb '${verb.text}' is expected` };
}

function argumentMisfit(
  argument: Template | Literals,
  text: string,
  resolve: Resolver,
): string | null {
  if ("words" in argument) {
    return argument.words.includes(text) ? null : `'${text}' is not one of: ${argument.text}`;
  }
  if (argument.text === "" && text !== "") {
    return `'${text}' is not expected: nothing follows the colon of this form`;
  }
  return misfit(argument, text, resolve);
}

function misfit(template: Template, text: string, resolve: Resolver): string | null {
  const captures = fillTemplate(template, text);
  if (captures === null) return `'${text}' does not fit \`${template.text}\``;
  const refusals = captures.map(({ name, text: token }) => refusal(resolve(name), name, token));
  return refusals.find((why) => why !== null) ?? null;
}

function refusal(domain: Domain | undefined, name: string, token: string): string | null {
  const text = token.trim();
  if (text === "") return `<${name}> is empty: a token is never empty`;
  if (domain?.kind === "literals" && !domain.words.includes(text)) {
    return `'${text}' is not one of: ${domain.text}`;
  }
  return null;
}
