export const DEFAULT_COMMENT_PREFIX = "#";

const BANG = "!";
const COLON = ":";
const ARROW = "->";

export type Nothing = { readonly shape: "nothing" };
export type Bang = { readonly shape: "bang"; readonly verb: string };
export type Colon = { readonly shape: "colon"; readonly verb: string; readonly argument: string };
export type Arrow = { readonly shape: "arrow"; readonly left: string; readonly right: string };

export type Shaped = Bang | Colon | Arrow;

export type Shapeless = { readonly shape: "none"; readonly expected: string };
export type Line = Nothing | Shaped | Shapeless;

const NOTHING: Nothing = { shape: "nothing" };

const shapeless = (expected: string): Shapeless => ({ shape: "none", expected });

export function classifyLine(raw: string, commentPrefix: string): Line {
  const text = raw.trim();
  return text === "" || text.startsWith(commentPrefix) ? NOTHING : classifyShape(text);
}

export function classifyShape(text: string): Shaped | Shapeless {
  if (isBang(text)) return { shape: "bang", verb: text.slice(0, -BANG.length) };
  if (text.includes(COLON)) return colonLine(text);
  if (text.includes(ARROW)) return arrowLine(text);
  return shapeless(
    "a bang line `verb!`, a colon line `verb : argument` or an arrow line `left -> right`",
  );
}

function isBang(text: string): boolean {
  return (
    text.length > BANG.length &&
    text.endsWith(BANG) &&
    !text.includes(COLON) &&
    !text.includes(ARROW) &&
    !/\s/.test(text)
  );
}

function colonLine(text: string): Colon | Shapeless {
  const at = text.indexOf(COLON);
  const verb = text.slice(0, at).trim();
  const argument = text.slice(at + COLON.length).trim();
  return verb === ""
    ? shapeless("a verb before the first colon")
    : { shape: "colon", verb, argument };
}

function arrowLine(text: string): Arrow | Shapeless {
  const at = text.indexOf(ARROW);
  const left = text.slice(0, at).trim();
  const right = text.slice(at + ARROW.length).trim();
  return left === "" || right === ""
    ? shapeless("text on both sides of the first arrow")
    : { shape: "arrow", left, right };
}

export type Part =
  | { readonly kind: "literal"; readonly text: string }
  | { readonly kind: "placeholder"; readonly name: string };

export type Template = { readonly text: string; readonly parts: readonly Part[] };

export type Capture = { readonly name: string; readonly text: string };

const PLACEHOLDER = /(<[A-Za-z][A-Za-z0-9_-]*>)/;

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

export function placeholderNames(template: Template): readonly string[] {
  return template.parts.flatMap((part) => (part.kind === "placeholder" ? [part.name] : []));
}

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

function captureLength(rest: string, next: Part | undefined, isLast: boolean): number {
  if (next === undefined) return rest.length;
  if (next.kind === "placeholder") return 0;
  if (isLast) return rest.endsWith(next.text) ? rest.length - next.text.length : -1;
  return rest.indexOf(next.text);
}
