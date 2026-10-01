// Lines: ARCHITECTURE.md section 2.
//
// A reader works line by line. It strips leading and trailing blanks from every line, then classifies
// it: nothing, a bang line, a colon line or an arrow line. A line that is none of these is an error.
// A production (section 3) is one of the same three shapes with placeholders in angle brackets, so the
// text of a production is taken apart and filled here as well.

/** The comment prefix assumed until a `comment` line says otherwise (section 2, item 1). */
export const DEFAULT_COMMENT_PREFIX = "#";

const BANG = "!";
const COLON = ":";
const ARROW = "->";

export type Nothing = { readonly shape: "nothing" };
export type Bang = { readonly shape: "bang"; readonly verb: string };
export type Colon = { readonly shape: "colon"; readonly verb: string; readonly argument: string };
export type Arrow = { readonly shape: "arrow"; readonly left: string; readonly right: string };
/** A line of one of the three shapes. */
export type Shaped = Bang | Colon | Arrow;
/** A line of no shape, which is an error; `expected` names what a legal line would have had. */
export type Shapeless = { readonly shape: "none"; readonly expected: string };
export type Line = Nothing | Shaped | Shapeless;

const NOTHING: Nothing = { shape: "nothing" };

const shapeless = (expected: string): Shapeless => ({ shape: "none", expected });

/**
 * Section 2: strip the line, then classify it. A blank line, or a line whose first non-blank characters
 * are the comment prefix, is nothing. A blank is anything `trim` strips: every white-space character,
 * so a space, a tab, the carriage return of a CRLF line end, and a byte-order mark at the top of a file.
 */
export function classifyLine(raw: string, commentPrefix: string): Line {
  const text = raw.trim();
  return text === "" || text.startsWith(commentPrefix) ? NOTHING : classifyShape(text);
}

/**
 * Classify stripped text that is not nothing. The shapes are tried in the order the page lists them
 * (bang, colon, arrow), so a line that holds a colon is a colon line even when it also holds an arrow.
 */
export function classifyShape(text: string): Shaped | Shapeless {
  if (isBang(text)) return { shape: "bang", verb: text.slice(0, -BANG.length) };
  if (text.includes(COLON)) return colonLine(text);
  if (text.includes(ARROW)) return arrowLine(text);
  return shapeless(
    "a bang line `verb!`, a colon line `verb : argument` or an arrow line `left -> right`",
  );
}

/** Item 2: one word ending in an exclamation mark, no colon, no arrow. */
function isBang(text: string): boolean {
  return (
    text.length > BANG.length &&
    text.endsWith(BANG) &&
    !text.includes(COLON) &&
    !text.includes(ARROW) &&
    !/\s/.test(text) // one word: no blank inside it
  );
}

/** Item 3: the first colon splits the line; blanks around it are stripped; the argument keeps its inner spacing. */
function colonLine(text: string): Colon | Shapeless {
  const at = text.indexOf(COLON);
  const verb = text.slice(0, at).trim();
  const argument = text.slice(at + COLON.length).trim();
  return verb === ""
    ? shapeless("a verb before the first colon")
    : { shape: "colon", verb, argument };
}

/** Item 4: the first arrow splits the line, both sides stripped. */
function arrowLine(text: string): Arrow | Shapeless {
  const at = text.indexOf(ARROW);
  const left = text.slice(0, at).trim();
  const right = text.slice(at + ARROW.length).trim();
  return left === "" || right === ""
    ? shapeless("text on both sides of the first arrow")
    : { shape: "arrow", left, right };
}

/** One part of a production's text: literal text, or a placeholder that names a token class. */
export type Part =
  | { readonly kind: "literal"; readonly text: string }
  | { readonly kind: "placeholder"; readonly name: string };

/** Literal text and placeholders, as a production writes them. */
export type Template = { readonly text: string; readonly parts: readonly Part[] };

/** The text one placeholder took from a line. */
export type Capture = { readonly name: string; readonly text: string };

// A placeholder is a name in angle brackets: a letter, then letters, digits, dashes or underscores.
// Any other "<" or ">" is literal text. The capturing group makes split() keep the placeholders, so
// the pieces alternate: literal text at the even indexes, a placeholder at the odd ones.
const PLACEHOLDER = /(<[A-Za-z][A-Za-z0-9_-]*>)/;

/** Take the text of a production (a verb, an argument, or one side of an arrow) apart. */
export function parseTemplate(text: string): Template {
  const parts = text
    .split(PLACEHOLDER)
    .map((piece, index): Part =>
      index % 2 === 1
        ? { kind: "placeholder", name: piece.slice(1, -1) }
        : { kind: "literal", text: piece },
    )
    .filter((part) => part.kind === "placeholder" || part.text !== "");
  return { text, parts };
}

/** The placeholder names of a template, in the order they are written. */
export function placeholderNames(template: Template): readonly string[] {
  return template.parts.flatMap((part) => (part.kind === "placeholder" ? [part.name] : []));
}

/**
 * Fill a template from text. Literal text matches exactly. A placeholder takes the shortest text that
 * reaches the leftmost occurrence of the literal after it; the last placeholder takes the rest. Nothing
 * is tried twice. Returns what each placeholder took, or null when the text does not have the shape.
 */
export function fillTemplate(template: Template, text: string): readonly Capture[] | null {
  const last = template.parts.findLastIndex((part) => part.kind === "placeholder");
  const captures: Capture[] = [];
  let rest = text;
  for (const [index, part] of template.parts.entries()) {
    if (part.kind === "literal") {
      if (!rest.startsWith(part.text)) return null;
      rest = rest.slice(part.text.length);
      continue;
    }
    const length = captureLength(rest, template.parts[index + 1], index === last);
    if (length < 0) return null;
    captures.push({ name: part.name, text: rest.slice(0, length) });
    rest = rest.slice(length);
  }
  return rest === "" ? captures : null;
}

/** How much of `rest` a placeholder takes, given the part written after it; -1 when that part is missing. */
function captureLength(rest: string, next: Part | undefined, isLast: boolean): number {
  if (next === undefined) return rest.length;
  if (next.kind === "placeholder") return 0; // nothing stands between the two: the shortest text is none
  if (isLast) return rest.endsWith(next.text) ? rest.length - next.text.length : -1;
  return rest.indexOf(next.text);
}
