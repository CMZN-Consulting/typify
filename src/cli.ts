import { type Diagnostic, type Reading, isError, readDeclarations } from "./declarations.ts";
import { readProgram } from "./program.ts";

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
  "  Read a declarations file beginning with `typify : 2` or `typify : 3`. The reader loads that",
  "  version's bundled grammar, then validates the file's language bodies through its forms:",
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

export async function run(args: readonly string[]): Promise<Outcome> {
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

async function check(file: string): Promise<Outcome> {
  const declarations = await textOf(file);
  if ("problem" in declarations) return refused(declarations.problem);
  const reading = readDeclarations(declarations.text);
  return report(file, reading.diagnostics, declared(reading));
}

function declared(reading: Reading): string {
  return reading.meta === null
    ? "the grammar selector failed, so no language bodies were read"
    : `declares ${[...reading.languages.keys()].join(", ")}`;
}

async function checkProgram(
  declarationsFile: string,
  name: string,
  programFile: string,
): Promise<Outcome> {
  const declarations = await textOf(declarationsFile);
  if ("problem" in declarations) return refused(declarations.problem);
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
  const program = await textOf(programFile);
  if ("problem" in program) return refused(program.problem);
  const diagnostics = readProgram(program.text, language, reading.meta);
  return report(programFile, diagnostics, `a program of ${name}`);
}

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

async function textOf(
  path: string,
): Promise<{ readonly text: string } | { readonly problem: string }> {
  try {
    return { text: await Bun.file(path).text() };
  } catch (cause) {
    const reason = cause instanceof Error ? cause.message : String(cause);
    return { problem: `${path} cannot be read: ${reason}` };
  }
}

if (import.meta.main) {
  const outcome = await run(Bun.argv.slice(2));
  if (outcome.out.length) await Bun.write(Bun.stdout, `${outcome.out.join("\n")}\n`);
  if (outcome.err.length) await Bun.write(Bun.stderr, `${outcome.err.join("\n")}\n`);
  process.exitCode = outcome.code;
}
