// Shared by the tests: where the files are, and small ways to build a declarations file and query a
// reading. The fixtures hold the published meta body and invented languages only.

import { readFileSync } from "node:fs";
import { join } from "node:path";
import { isError, readDeclarations, type Diagnostic, type Language } from "../src/declarations.ts";

const ROOT = join(import.meta.dir, "..");

/** The repository's own declarations file: the published version-2 meta body and nothing else. */
export const PUBLISHED = join(ROOT, "LanguageDeclarations.txt");

export const CLI = join(ROOT, "src", "cli.ts");

export const fixture = (name: string): string => join(ROOT, "test", "fixtures", name);

export const textOf = (path: string): string => readFileSync(path, "utf8");

/** One further body: its declare line, the given lines, its end! line. */
export const body = (name: string, ...lines: readonly string[]): string =>
  [`declare : ${name}`, ...lines, "end!"].join("\n");

/** A declarations file: the published meta body, a blank line, then the given further bodies. */
export const withMeta = (...bodies: readonly string[]): string =>
  `${textOf(PUBLISHED)}\n${bodies.join("\n\n")}\n`;

export const errorsOf = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.filter(isError);

export const notesOf = (diagnostics: readonly Diagnostic[]): readonly Diagnostic[] =>
  diagnostics.filter((diagnostic) => !isError(diagnostic));

/** The errors of a declarations file, each as `LINE: TEXT`. */
export const errorLines = (text: string): readonly string[] =>
  errorsOf(readDeclarations(text).diagnostics).map(({ line, text: why }) => `${line}: ${why}`);

/** The 1-based number of the first line of `text` that holds `needle`. */
export function lineOf(text: string, needle: string): number {
  const index = text.split("\n").findIndex((line) => line.includes(needle));
  if (index < 0) throw new Error(`no line holds: ${needle}`);
  return index + 1;
}

/** One language of a declarations file that must be clean, with the file's meta language. */
export function languageOf(text: string, name: string): { language: Language; meta: Language } {
  const reading = readDeclarations(text);
  const language = reading.languages.get(name);
  const errors = errorsOf(reading.diagnostics);
  if (reading.meta === null || language === undefined || errors.length > 0) {
    throw new Error(`the declarations are not clean, or do not declare ${name}`);
  }
  return { language, meta: reading.meta };
}
