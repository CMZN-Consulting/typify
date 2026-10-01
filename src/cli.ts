// The command line: `check` and `check-program`. The reader reads the files it is given and prints.
// It never executes, creates or writes anything (ARCHITECTURE.md section 5).

import { readFileSync } from "node:fs";
import { isError, readDeclarations, type Diagnostic, type Reading } from "./declarations.ts";
import { readProgram } from "./program.ts";

/** What one run prints and how it exits: 0 when clean, 1 on any error, 2 when the command line is wrong. */
export type Outcome = {
  readonly code: 0 | 1 | 2;
  readonly out: readonly string[];
  readonly err: readonly string[];
};

const USAGE = [
  "usage: bun src/cli.ts check DECLARATIONS_FILE",
  "       bun src/cli.ts check-program DECLARATIONS_FILE LANGUAGE PROGRAM_FILE",
  "       bun src/cli.ts --help",
];

const HELP = [
  "typify: a reader of versions 2 and 3 of the meta language. It validates and does nothing else.",
  "",
  ...USAGE,
  "",
  "check",
  "  Read a declarations file (ARCHITECTURE.md section 4, steps 1 and 2). The first body must be",
  "  `declare : LanguageDeclarations` at version 2 or 3, every line of it must be of a form the body",
  "  itself declares, and it must equal the reader's copy of the published meta body of that version;",
  "  if it fails, nothing else is read. Every further body is then read through the meta forms:",
  "  declare and end!, no body inside another, no name twice, a name of one word, one does for each",
  "  form, examples that fit their form, once naming a verb of the body, sees naming a body of the",
  "  file. Under version 2 a name must also be one the meta body lists.",
  "",
  "check-program",
  "  Check the declarations file as above, then read one program file against one language it",
  "  declares (the per-program part of step 4): only declared forms, token classes honoured, once",
  "  and order honoured. If the declarations file has an error, the program is not read. The notes",
  "  of the declarations file are printed by check, not here.",
  "",
  "Output",
  "  One line per finding: FILE:LINE: error: TEXT, or FILE:LINE: note: TEXT. A note is not an error:",
  "  a placeholder that no `is` line binds is read as free text and noted, so that a mistyped class",
  "  name shows. A last line, starting with `typify:`, sums up.",
  "",
  "Exit status",
  "  0 when clean, 1 on any error, 2 when the command line is wrong or a file cannot be read.",
  "",
  "Not implemented, and not guessed at",
  "  The tree walk and path templates (step 3), resolving sees names across programs, absent,",
  "  derived!, and the action step (step 5). Nothing is executed, created or written.",
];

/** Run one command line and say what to print and how to exit. Files are read; nothing is written. */
export function run(args: readonly string[]): Outcome {
  const [command, first, second, third, ...extra] = args;
  if (command === "--help") return { code: 0, out: HELP, err: [] };
  if (command === "check") {
    return first !== undefined && second === undefined
      ? check(first)
      : misuse("check takes one argument: DECLARATIONS_FILE");
  }
  if (command === "check-program") {
    return first !== undefined && second !== undefined && third !== undefined && extra.length === 0
      ? checkProgram(first, second, third)
      : misuse("check-program takes three arguments: DECLARATIONS_FILE LANGUAGE PROGRAM_FILE");
  }
  return misuse(
    command === undefined
      ? "a command is expected: check or check-program"
      : `'${command}' is not a command: check or check-program is expected`,
  );
}

function check(file: string): Outcome {
  const declarations = textOf(file);
  if ("problem" in declarations) return refused(declarations.problem);
  const reading = readDeclarations(declarations.text);
  return report(file, reading.diagnostics, declared(reading));
}

/** What a declarations file was found to declare, for the line that sums up. */
function declared(reading: Reading): string {
  return reading.meta === null
    ? "the first body failed, so nothing after it was read"
    : `declares ${[...reading.languages.keys()].join(", ")}`;
}

function checkProgram(declarationsFile: string, name: string, programFile: string): Outcome {
  const declarations = textOf(declarationsFile);
  if ("problem" in declarations) return refused(declarations.problem);
  const program = textOf(programFile);
  if ("problem" in program) return refused(program.problem);
  const reading = readDeclarations(declarations.text);
  if (reading.meta === null || reading.diagnostics.some(isError)) {
    const { code, out, err } = report(declarationsFile, reading.diagnostics, declared(reading));
    const unread = `typify: ${programFile}: not read, because ${declarationsFile} has errors`;
    return { code, out: [...out, unread], err };
  }
  const language = reading.languages.get(name);
  if (language === undefined) {
    return refused(`${declarationsFile} ${declared(reading)}, and no language named ${name}`);
  }
  const diagnostics = readProgram(program.text, language, reading.meta);
  return report(programFile, diagnostics, `a program of ${name}`);
}

/** One line per finding, `FILE:LINE: error: TEXT` or `FILE:LINE: note: TEXT`, then one line that sums up. */
function report(file: string, diagnostics: readonly Diagnostic[], read: string): Outcome {
  const errors = diagnostics.filter(isError).length;
  const findings = diagnostics.map(
    ({ line, severity, text }) => `${file}:${line}: ${severity}: ${text}`,
  );
  const notes = diagnostics.length - errors;
  const sum = [count(errors, "error"), ...(notes > 0 ? [count(notes, "note")] : [])].join(", ");
  return {
    code: errors > 0 ? 1 : 0,
    out: [...findings, `typify: ${file}: ${sum} (${read})`],
    err: [],
  };
}

function count(number: number, noun: string): string {
  if (number === 0) return `no ${noun}s`;
  return number === 1 ? `1 ${noun}` : `${number} ${noun}s`;
}

function misuse(problem: string): Outcome {
  return { code: 2, out: [], err: [`typify: ${problem}`, ...USAGE] };
}

function refused(problem: string): Outcome {
  return { code: 2, out: [], err: [`typify: ${problem}`] };
}

/** The text of a file, or why it cannot be read. */
function textOf(path: string): { readonly text: string } | { readonly problem: string } {
  try {
    return { text: readFileSync(path, "utf8") };
  } catch (cause) {
    const reason = cause instanceof Error ? cause.message : String(cause);
    return { problem: `${path} cannot be read: ${reason}` };
  }
}

if (import.meta.main) {
  const outcome = run(process.argv.slice(2));
  for (const line of outcome.out) console.log(line);
  for (const line of outcome.err) console.error(line);
  process.exitCode = outcome.code;
}
