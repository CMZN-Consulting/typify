import { type Diagnostic, type Language, isError, readDeclarations } from "../src/declarations.ts";

const ROOT = Bun.fileURLToPath(new URL("../", import.meta.url));

export const PUBLISHED = `${ROOT}src/grammar/v3.typify`;

export const PUBLISHED_V2 = `${ROOT}src/grammar/v2.typify`;

export const CLI = `${ROOT}src/cli.ts`;

export const fixture = (name: string): string => `${ROOT}test/fixtures/${name}`;

const texts = new Map<string, string>();
for (const pattern of ["test/fixtures/**/*.typify", "src/grammar/*.typify"]) {
  for await (const path of new Bun.Glob(pattern).scan({
    cwd: ROOT,
    absolute: true,
    onlyFiles: true,
  })) {
    texts.set(path, await Bun.file(path).text());
  }
}

export const textOf = (path: string): string => {
  const text = texts.get(path);
  if (text === undefined) throw new Error(`fixture was not preloaded: ${path}`);
  return text;
};

export const body = (name: string, ...lines: readonly string[]): string =>
  [`declare : ${name}`, ...lines, "end!"].join("\n");

export const withMeta = (...bodies: readonly string[]): string =>
  `typify : 3\n${bodies.join("\n\n")}\n`;

export const withMetaV2 = (...bodies: readonly string[]): string =>
  `typify : 2\n${bodies.join("\n\n")}\n`;

export const errorsOf = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.filter(isError);

export const notesOf = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.filter((diagnostic) => !isError(diagnostic));

export const errorLines = (text: string): readonly string[] =>
  errorsOf(readDeclarations(text).diagnostics).map(({ line, text: why }) => `${line}: ${why}`);

export function lineOf(text: string, needle: string): number {
  const index = text.split("\n").findIndex((line) => line.includes(needle));
  if (index < 0) throw new Error(`no line holds: ${needle}`);
  return index + 1;
}

export function languageOf(text: string, name: string): { language: Language; meta: Language } {
  const reading = readDeclarations(text);
  const language = reading.languages.get(name);
  const errors = errorsOf(reading.diagnostics);
  if (reading.meta === null || language === undefined || errors.length > 0) {
    throw new Error(`the declarations are not clean, or do not declare ${name}`);
  }
  return { language, meta: reading.meta };
}
