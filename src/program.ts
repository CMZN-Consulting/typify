import {
  type Diagnostic,
  type Form,
  type Language,
  capturesOf,
  errorAt,
  findForm,
  isError,
  notALine,
  resolver,
  verbOf,
} from "./declarations.ts";
import { type Capture, classifyLine } from "./lines.ts";

type Hit = { readonly index: number; readonly form: Form };

export type ParsedProgramLine = {
  readonly line: number;

  readonly raw: string;

  readonly formIndex: number;
  readonly form: Form;

  readonly captures: readonly Capture[];
};

export type ParsedProgram =
  | {
      readonly valid: true;
      readonly source: string;
      readonly lines: readonly ParsedProgramLine[];
      readonly diagnostics: readonly Diagnostic[];
    }
  | {
      readonly valid: false;
      readonly source: string;
      readonly diagnostics: readonly Diagnostic[];
    };

export function parseProgram(
  text: string,
  language: Language,
  meta: Language,
  source = "<input>",
): ParsedProgram {
  const resolve = resolver(language, meta);
  const diagnostics: Diagnostic[] = [];
  const lines: ParsedProgramLine[] = [];
  const firstUse = new Map<string, number>();
  let furthest: Hit | null = null;
  for (const [index, raw] of text.split("\n").entries()) {
    const number = index + 1;
    const line = classifyLine(raw, language.commentPrefix);
    if (line.shape === "nothing") continue;
    if (line.shape === "none") {
      diagnostics.push(notALine(number, line));
      continue;
    }
    const found = findForm(language, resolve, line);
    if (!found.found) {
      diagnostics.push(errorAt(number, found.why));
      continue;
    }
    const captures = capturesOf(found.form, line);
    if (captures === null) {
      diagnostics.push(errorAt(number, "the matched form could not be captured"));
      continue;
    }
    lines.push({ line: number, raw, formIndex: found.index, form: found.form, captures });
    const misplacement = misplaced(language, furthest, found);
    if (misplacement === null) furthest = found;
    else diagnostics.push(errorAt(number, misplacement));
    const verb = verbOf(found.form);
    if (verb === null || !language.once.has(verb)) continue;
    const first = firstUse.get(verb);
    if (first === undefined) firstUse.set(verb, number);
    else diagnostics.push(errorAt(number, repeated(language, verb, first)));
  }
  return diagnostics.some(isError)
    ? { valid: false, source, diagnostics }
    : { valid: true, source, lines, diagnostics };
}

export function readProgram(
  text: string,
  language: Language,
  meta: Language,
): readonly Diagnostic[] {
  return parseProgram(text, language, meta).diagnostics;
}

function misplaced(language: Language, furthest: Hit | null, hit: Hit): string | null {
  if (language.order !== "strict" || furthest === null || hit.index >= furthest.index) return null;
  const expected = `\`${hit.form.text}\` is declared before \`${furthest.form.text}\` and comes before it`;
  return `this line is out of order: ${language.name} is \`order : strict\`, so ${expected}`;
}

function repeated(language: Language, verb: string, first: number): string {
  return `'${verb}' may appear at most once in a program of ${language.name}; it first appears at line ${first}`;
}
