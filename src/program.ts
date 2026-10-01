// Programs: the per-program part of step 4 of ARCHITECTURE.md section 4.
//
// A program is read against the forms of its language: only declared forms, token classes honoured,
// `once` and `order` honoured. Where its file lives, what its `sees` names resolve to, and what a
// missing file means belong to the tree walk, which this reader does not do.

import {
  errorAt,
  findForm,
  notALine,
  resolver,
  verbOf,
  type Diagnostic,
  type Form,
  type Language,
} from "./declarations.ts";
import { classifyLine } from "./lines.ts";

/** A line that was read as a form: the form, and its place in the order of declaration. */
type Hit = { readonly index: number; readonly form: Form };

/**
 * Read one program of `language`; `meta` is the meta language of the same declarations file, where a
 * placeholder resolves when the language's own body does not bind it. Nothing is executed: a line whose
 * meaning is a command is text.
 */
export function readProgram(
  text: string,
  language: Language,
  meta: Language,
): readonly Diagnostic[] {
  const resolve = resolver(language, meta);
  const diagnostics: Diagnostic[] = [];
  const firstUse = new Map<string, number>(); // a once verb, and the line that used it first
  let furthest: Hit | null = null; // the latest-declared form read so far
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
    const misplacement = misplaced(language, furthest, found);
    if (misplacement === null) furthest = found;
    else diagnostics.push(errorAt(number, misplacement));
    const verb = verbOf(found.form);
    if (verb === null || !language.once.has(verb)) continue;
    const first = firstUse.get(verb);
    if (first === undefined) firstUse.set(verb, number);
    else diagnostics.push(errorAt(number, repeated(language, verb, first)));
  }
  return diagnostics;
}

/**
 * Section 3: `order : strict` says the forms of a program appear in the order they are declared. A form
 * may repeat; a line may not be of a form declared before the form of a line above it. Returns what is
 * wrong with the line's place, or null when it is in order.
 */
function misplaced(language: Language, furthest: Hit | null, hit: Hit): string | null {
  if (language.order !== "strict" || furthest === null || hit.index >= furthest.index) return null;
  const expected = `\`${hit.form.text}\` is declared before \`${furthest.form.text}\` and comes before it`;
  return `this line is out of order: ${language.name} is \`order : strict\`, so ${expected}`;
}

/** Section 3: `once` says the form's verb may appear at most once in one program file. */
function repeated(language: Language, verb: string, first: number): string {
  return `'${verb}' may appear at most once in a program of ${language.name}; it first appears at line ${first}`;
}
