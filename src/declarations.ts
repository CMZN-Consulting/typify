// Declarations: ARCHITECTURE.md sections 1 and 3, and steps 1 and 2 of section 4.
//
// A declarations file is a list of language bodies, each from `declare : <Name>` to `end!`. The first
// body declares the language the file itself is written in (the meta body). The reader validates that
// body against itself, verifies it against its own copy of the published meta body of the version the
// body states, and only then reads every further body through the meta forms. If the first body fails,
// nothing else is read.

import {
  classifyLine,
  classifyShape,
  DEFAULT_COMMENT_PREFIX,
  fillTemplate,
  parseTemplate,
  placeholderNames,
  type Colon,
  type Line,
  type Shaped,
  type Shapeless,
  type Template,
} from "./lines.ts";

/** The name of the meta language: the first body of every declarations file declares it. */
const META_NAME = "LanguageDeclarations";

/** The highest version of the meta language this reader knows. It reads version 2 as well. */
const KNOWN_VERSION = 3;

// The `Name` line of version 2: the languages of the first tree to use typify, as a closed list.
const NAME_V2 =
  "is : Name = LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats";

// The reader's own copy of the published version-2 meta body, one entry per line that is not nothing,
// stripped, with one blank on each side of the first colon. A tree copies the body whole as the first
// body of its file, and the reader verifies that copy against this one (README, "Using it from a tree").
const PUBLISHED_META_V2: readonly string[] = [
  "declare : LanguageDeclarations",
  "version : 2",
  "file : LanguageDeclarations.txt",
  "required!",
  "comment : #",
  NAME_V2,
  "is : verb = a word of letters, digits and dashes",
  "form : declare : <Name>",
  "does : open a language body named <Name>; the body runs to the next end!",
  "example : declare : Structure",
  "form : version : <number>",
  "does : the version of the language declared in this body; a reader refuses a higher one than it knows",
  "form : file : <path>",
  "does : programs of this language live at <path>; <Session>, <window> and <seat> in the path are placeholders filled from the tree",
  "example : file : <Session>/<window>/Structure.txt",
  "form : required!",
  "does : the program file must exist wherever the path template can be filled",
  "form : optional!",
  "does : the program file may be missing; then absent applies",
  "form : derived!",
  "does : the program file is written by the interpreter from the other programs and is never written by hand; a hand-written copy is overwritten",
  "form : comment : <prefix>",
  "does : lines whose first non-blank characters are <prefix> are comments; blank lines are nothing",
  "form : is : <name> = <domain>",
  "does : bind <name> as a token class; <domain> is either a list of literal words separated by | or a sentence saying what the class holds",
  "example : is : axis = vertical | horizontal",
  "form : form : <production>",
  "does : a legal program line; a production is one of three shapes: `<verb> : <argument>`, `<verb>!`, or `<left> -> <right>`",
  "form : does : <text>",
  "does : the meaning of the form immediately above; every form has exactly one",
  "form : example : <line>",
  "does : a legal program line for the form above, as a test vector; a reader may check it against the form",
  "form : once : <verb>",
  "does : the form with this verb may appear at most once in one program file",
  "form : order : strict | free",
  "does : strict means the forms appear in a program in the order they are declared here; free means any order; free when absent",
  "form : sees : <Name>",
  "does : programs of this language name things that programs of <Name> declare, and a reader checks that they exist",
  "form : absent : <default>",
  "does : if an optional program file is missing, the interpreter behaves as if it held <default>",
  "form : end!",
  "does : close the language body",
  "end!",
];

// Version 3 differs from version 2 in two lines: its version, and `Name`. Version 2 listed the
// languages of the first tree under `Name`, so no other tree could declare a language of its own.
// In version 3 `Name` is a sentence, and a new language is a new body (ARCHITECTURE.md, section 6).
const NAME_V3 = "is : Name = one word that names a language body of the file";
const PUBLISHED_META_V3: readonly string[] = PUBLISHED_META_V2.map((line) => {
  if (line === "version : 2") return "version : 3";
  return line === NAME_V2 ? NAME_V3 : line;
});

/** The published meta bodies this reader knows, by version. */
const PUBLISHED_META: ReadonlyMap<number, readonly string[]> = new Map([
  [2, PUBLISHED_META_V2],
  [3, PUBLISHED_META_V3],
]);

const KNOWN_VERSIONS = [...PUBLISHED_META.keys()].join(" and ");

/** One finding about one line of a file. A note is not an error. */
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

/** Section 2: a line that is none of the three shapes is an error. */
export const notALine = (line: number, { expected }: Shapeless): Diagnostic =>
  errorAt(line, `not a legal line: expected ${expected}`);

/** The domain of a token class (section 3): a list of literal words, or a sentence. */
export type Domain =
  | { readonly kind: "literals"; readonly text: string; readonly words: readonly string[] }
  | { readonly kind: "sentence"; readonly text: string };

type Literals = Extract<Domain, { kind: "literals" }>;

/** A production (section 3): one of the three line shapes, with placeholders in angle brackets. */
export type Production =
  | { readonly shape: "bang"; readonly verb: Template }
  | { readonly shape: "colon"; readonly verb: Template; readonly argument: Template | Literals }
  | { readonly shape: "arrow"; readonly left: Template; readonly right: Template };

/** One `form` line of a body. */
export type Form = {
  /** The line of the `form` line. */
  readonly line: number;
  /** The production as written. */
  readonly text: string;
  /** Shapeless when the text is none of the three shapes; no line can be of such a form. */
  readonly production: Production | Shapeless;
};

/** What one body declares, as far as this reader acts on it. */
export type Language = {
  readonly name: string;
  /** The line of the `declare` line. */
  readonly line: number;
  /** The version the body states, when it states a whole number; where it states several, the last. */
  readonly version: number | null;
  /** The comment prefix of this language's programs: `#` unless the body declares another. */
  readonly commentPrefix: string;
  /** `strict` when the body says `order : strict`; free when absent. */
  readonly order: "strict" | "free";
  readonly classes: ReadonlyMap<string, Domain>;
  /** The forms in the order they are declared. */
  readonly forms: readonly Form[];
  /** The verbs the body's `once` lines name. */
  readonly once: ReadonlySet<string>;
};

/** What reading a declarations file found. */
export type Reading = {
  /** Errors and notes, in line order. */
  readonly diagnostics: readonly Diagnostic[];
  /** The meta language, or null when the first body failed and nothing after it was read. */
  readonly meta: Language | null;
  /** The languages the file declares, by name, the meta language first; empty when the first body failed. */
  readonly languages: ReadonlyMap<string, Language>;
};

/** A line of a body that is not nothing, with its number and its text as written. */
type Row = { readonly number: number; readonly raw: string; readonly line: Shaped };

/** One body: its rows run from the `declare` line to the `end!` line, both included when present. */
type Body = { readonly name: string; readonly line: number; readonly rows: readonly Row[] };

/** The argument of one colon line, with the line's number. */
type Stated = { readonly line: number; readonly text: string };

/** Read a declarations file: steps 1 and 2 of the reading order. Nothing is executed or written. */
export function readDeclarations(text: string): Reading {
  const rows = text.split("\n");
  const first = readFirstBody(rows);
  if (first.meta === null) {
    return { diagnostics: inLineOrder(first.diagnostics), meta: null, languages: new Map() };
  }
  const further = readFurtherBodies(rows, first.next, first.meta);
  return {
    diagnostics: inLineOrder([...first.diagnostics, ...further.diagnostics]),
    meta: first.meta,
    languages: further.languages,
  };
}

const inLineOrder = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.toSorted((one, other) => one.line - other.line);

// --- Step 1: the first body ---------------------------------------------------------------------------

type First = {
  /** Null when the first body failed: the reader stops and reads nothing else. */
  readonly meta: Language | null;
  /** The index of the first row after the body. */
  readonly next: number;
  readonly diagnostics: readonly Diagnostic[];
};

/**
 * Step 1: the first body must be `declare : LanguageDeclarations`, of a version this reader knows; every
 * line of it must be of a form the body itself declares; and it must be the published meta body of
 * the version it states.
 */
function readFirstBody(rows: readonly string[]): First {
  const failed = (diagnostics: readonly Diagnostic[]): First => ({
    meta: null,
    next: rows.length,
    diagnostics,
  });
  const start = rows.findIndex(
    (raw) => classifyLine(raw, DEFAULT_COMMENT_PREFIX).shape !== "nothing",
  );
  const opening = classifyLine(rows[start] ?? "", DEFAULT_COMMENT_PREFIX);
  if (!isDeclare(opening) || opening.argument !== META_NAME) {
    const expected = `the file must open with \`declare : ${META_NAME}\`: the first body declares the language the file is written in`;
    return failed([errorAt(Math.max(start, 0) + 1, expected)]);
  }
  const taken = takeBody(rows, start, META_NAME, DEFAULT_COMMENT_PREFIX, true);
  const refused = refusedVersions(taken.body.rows);
  if (refused.length > 0) return failed(refused);
  const read = readBody(taken.body, null);
  const diagnostics = [
    ...taken.errors,
    ...read.diagnostics,
    ...differenceFromPublished(taken.body.rows),
  ];
  return diagnostics.some(isError)
    ? failed(diagnostics)
    : { meta: read.language, next: taken.next, diagnostics };
}

/** Section 3: a reader that knows a lower version than the body declares refuses the body and says so. */
function refusedVersions(rows: readonly Row[]): readonly Diagnostic[] {
  return said(rows, "version")
    .filter(({ text }) => WHOLE_NUMBER.test(text) && Number(text) > KNOWN_VERSION)
    .map(({ line, text }) =>
      errorAt(
        line,
        `${META_NAME} declares version ${text}; this reader knows versions ${KNOWN_VERSIONS} and refuses the body`,
      ),
    );
}

/**
 * The first body against the reader's copy of the version it states, line for line, comments and blank
 * lines left out; the first line that differs is named. A body that states no version this reader
 * knows is held against the highest. The copy ends at its only `end!` and so does a body that closes,
 * so neither is the start of the other: when they differ, they differ at a line both have. A body
 * that never closes is reported as such where it is taken.
 */
function differenceFromPublished(rows: readonly Row[]): readonly Diagnostic[] {
  const stated = versionOf(rows);
  const version = stated !== null && PUBLISHED_META.has(stated) ? stated : KNOWN_VERSION;
  const published = PUBLISHED_META.get(version) ?? PUBLISHED_META_V3;
  const at = rows.findIndex((row, index) => normalized(row.raw) !== published[index]);
  const row = rows[at];
  if (row === undefined) return [];
  const difference = `expected \`${published[at]}\`, found \`${normalized(row.raw)}\``;
  return [
    errorAt(
      row.number,
      `the first body is not the published version-${version} meta body: ${difference}`,
    ),
  ];
}

/** The version a body states: the last `version` line that holds a whole number, or null. */
function versionOf(rows: readonly Row[]): number | null {
  const text = said(rows, "version")
    .map((line) => line.text)
    .findLast((candidate) => WHOLE_NUMBER.test(candidate));
  return text === undefined ? null : Number(text);
}

/** A line as the comparison sees it: stripped, with one blank on each side of its first colon. */
function normalized(raw: string): string {
  const text = raw.trim();
  const at = text.indexOf(":");
  return at < 0 ? text : `${text.slice(0, at).trimEnd()} : ${text.slice(at + 1).trimStart()}`;
}

// --- Step 2: every further body -----------------------------------------------------------------------

type Further = {
  readonly languages: ReadonlyMap<string, Language>;
  readonly diagnostics: readonly Diagnostic[];
};

/**
 * Step 2: read every further body against the meta forms. Each opens with `declare` and closes with
 * `end!`; none opens inside another; no name is declared twice; a name is one word; text outside a
 * body is an error. The `sees` names are checked once every body has been read, so a body may name
 * one declared below it.
 */
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

/** Section 1: the file is a list of bodies; a line that is something and stands outside them is an error. */
function strayErrors(line: Line, number: number): readonly Diagnostic[] {
  if (line.shape === "nothing") return [];
  if (line.shape === "none") return [notALine(number, line)];
  const expected = "a body opens with `declare : <Name>` and closes with `end!`";
  return [errorAt(number, `this line is outside any body: ${expected}`)];
}

/**
 * Step 2: a name is one word. Where the meta body lists the names (version 2), the list has already
 * refused any other, so the rule speaks only where `Name` is a sentence (version 3). An empty name is
 * refused by the meta form.
 */
function nameErrors(body: Body, meta: Language): readonly Diagnostic[] {
  const listed = meta.classes.get("Name")?.kind === "literals";
  if (listed || body.name === "" || !/\s/.test(body.name)) return [];
  return [errorAt(body.line, `declare : '${body.name}' is not a name: one word is expected`)];
}

function declaredTwice(body: Body, earlier: Language): Diagnostic {
  const expected = "a name is declared once";
  return errorAt(
    body.line,
    `${body.name} is declared twice (first at line ${earlier.line}): ${expected}`,
  );
}

/** Section 3: `sees : <Name>` names another language of the file; the name must have a body. */
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

// --- Bodies -------------------------------------------------------------------------------------------

type Taken = {
  readonly body: Body;
  /** The index of the first row after the body. */
  readonly next: number;
  readonly errors: readonly Diagnostic[];
};

const isDeclare = (line: Line): line is Colon => line.shape === "colon" && line.verb === "declare";

const isEnd = (line: Line): boolean => line.shape === "bang" && line.verb === "end";

/**
 * Take the body whose `declare` line is at `start`: it runs to the next `end!` (section 1). A second
 * `declare` before that `end!` is an error and ends the body where it stands. In the first body the
 * comment prefix is `#` until the body's own `comment` line has been read (section 2, item 1); a
 * further body is read with the meta language's prefix, and its `comment` line is for its programs.
 */
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

/** Read one body through the meta forms. `meta` is null for the first body, which is read through its own. */
function readBody(body: Body, meta: Language | null): Read {
  const language = languageOf(body);
  const through = meta ?? language;
  const resolve = resolver(language, through);
  return {
    language,
    diagnostics: [
      ...lineErrors(body.rows, through),
      ...pairingErrors(body.rows),
      ...productionErrors(language.forms),
      ...exampleErrors(body.rows, language.forms, resolve),
      ...onceErrors(body.rows, language),
      ...versionErrors(body.rows),
      ...unboundNotes(language, resolve, meta),
    ],
  };
}

/** The arguments of a body's colon lines with one verb. */
function stated(rows: readonly Row[], verb: string): readonly Stated[] {
  return rows.flatMap(({ number, line }) =>
    line.shape === "colon" && line.verb === verb ? [{ line: number, text: line.argument }] : [],
  );
}

/**
 * The same, without the empty arguments. The meta form of each of these verbs takes a token, and a
 * token is never empty, so an empty argument is refused there; the rules below have nothing to add.
 */
function said(rows: readonly Row[], verb: string): readonly Stated[] {
  return stated(rows, verb).filter(({ text }) => text !== "");
}

/** What a body declares. Where a body states `comment` or `order` more than once, the last line stands. */
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

// The literal that the meta form `is : <name> = <domain>` puts between the class name and its domain.
const BINDS = " = ";

/** Section 3: token classes are declared with `is : <name> = <domain>`; the last line for a name stands. */
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

/** Section 3: a domain is a list of literal words separated by `|`, compared stripped, or a sentence. */
function parseDomain(text: string): Domain {
  return text.includes("|")
    ? { kind: "literals", text: text.trim(), words: text.split("|").map((word) => word.trim()) }
    : { kind: "sentence", text: text.trim() };
}

/** Section 3: a production is one of the three line shapes with placeholders in angle brackets. */
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

/** A colon production's argument with no placeholder and a `|` lists literal alternatives (`order : strict | free`). */
function parseArgument(text: string): Template | Literals {
  const template = parseTemplate(text);
  const domain = parseDomain(text);
  return domain.kind === "literals" && placeholderNames(template).length === 0 ? domain : template;
}

// --- The rules of a body ------------------------------------------------------------------------------

/** Section 3: inside a body the reader accepts only the forms the meta body declares. */
function lineErrors(rows: readonly Row[], meta: Language): readonly Diagnostic[] {
  const resolve = resolver(meta, meta);
  return rows.flatMap(({ number, line }) => {
    const found = findForm(meta, resolve, line);
    return found.found ? [] : [errorAt(number, found.why)];
  });
}

/** Section 3 and step 2: every form is followed by exactly one does, as the next line that is not nothing. */
function pairingErrors(rows: readonly Row[]): readonly Diagnostic[] {
  const formWithoutDoes =
    "this form is not followed by its does: the next line must be `does : <text>`";
  const doesWithoutForm = "this does has no form directly above it: a form has exactly one does";
  const errors: Diagnostic[] = [];
  let awaiting: number | null = null; // the line of a form whose does has not come yet
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

/** Section 3: `example` gives a legal line for the form just declared and is a test vector. */
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
    if (form.production.shape === "none") return []; // reported where the form is declared
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

/** The verb of a bang or colon form, as its production writes it; an arrow form has none. */
export function verbOf(form: Form): string | null {
  const { production } = form;
  return production.shape === "bang" || production.shape === "colon" ? production.verb.text : null;
}

/** Section 3: `once` says the form's verb may appear at most once, so it names the verb of a form of its body. */
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

// Digits only: a whole number, with no sign and no fraction.
const WHOLE_NUMBER = /^[0-9]+$/;

/** Section 3: `version : <number>` states the version of the language declared in that body. */
function versionErrors(rows: readonly Row[]): readonly Diagnostic[] {
  return said(rows, "version")
    .filter(({ text }) => !WHOLE_NUMBER.test(text))
    .map(({ line, text }) =>
      errorAt(line, `version : '${text}' is not a version: a whole number is expected`),
    );
}

/**
 * Section 3: a placeholder that no `is` line binds is unbound. Versions 2 and 3 read it as free text
 * and say so once per placeholder per body, at the form that uses it first, so that a mistyped class
 * name shows. The note names the version of the meta body the file is read through; `meta` is null
 * for the first body, which is its own.
 */
function unboundNotes(
  language: Language,
  resolve: Resolver,
  meta: Language | null,
): readonly Diagnostic[] {
  const firstUse = new Map<string, number>();
  for (const form of language.forms) {
    for (const name of placeholdersOf(form.production)) {
      if (resolve(name) === undefined && !firstUse.has(name)) firstUse.set(name, form.line);
    }
  }
  const where = meta === null ? META_NAME : `${language.name} or in ${META_NAME}`;
  const version = (meta ?? language).version ?? KNOWN_VERSION;
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

// --- Matching a line against a form -------------------------------------------------------------------

/** The token class a placeholder names, or undefined when the placeholder is unbound. */
export type Resolver = (name: string) => Domain | undefined;

/** A placeholder resolves to the class of its name in its own body, else in the meta body, else it is unbound. */
export function resolver(own: Language, meta: Language): Resolver {
  return (name) => own.classes.get(name) ?? meta.classes.get(name);
}

/** The form a line is, or why it is of no form. */
export type Found =
  | { readonly found: true; readonly index: number; readonly form: Form }
  | { readonly found: false; readonly why: string };

/**
 * Find the form of `language` that a line is. The forms are tried in the order they are declared and
 * the first that fits is the line's form.
 */
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

/** How a line misses a production. `near` when the shape and the verb are right and the rest is not. */
type Miss = { readonly near: boolean; readonly why: string };

/**
 * Match a line against a production: the same shape; a verb that fills the production's verb; an
 * argument, or two arrow sides, that fill the production's. Null when the line fits.
 */
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

/** A literal verb that differs belongs to another form; a placeholder verb that refuses its token is a near miss. */
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

/** Why text does not fill a template, or null when it does: the shape first, then each token against its class. */
function misfit(template: Template, text: string, resolve: Resolver): string | null {
  const captures = fillTemplate(template, text);
  if (captures === null) return `'${text}' does not fit \`${template.text}\``;
  const refusals = captures.map(({ name, text: token }) => refusal(resolve(name), name, token));
  return refusals.find((why) => why !== null) ?? null;
}

/**
 * Section 3: a token of a listed class must be one of its words; a sentence, like an unbound
 * placeholder, admits any non-empty text. The token is stripped before it is compared.
 */
function refusal(domain: Domain | undefined, name: string, token: string): string | null {
  const text = token.trim();
  if (text === "") return `<${name}> is empty: a token is never empty`;
  if (domain?.kind === "literals" && !domain.words.includes(text)) {
    return `'${text}' is not one of: ${domain.text}`;
  }
  return null;
}
