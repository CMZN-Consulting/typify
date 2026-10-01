// The command line: what the two commands print and how they exit.

import { describe, expect, test } from "bun:test";
import { run } from "../src/cli.ts";
import { CLI, fixture, PUBLISHED } from "./support.ts";

const VALID = fixture("declarations/valid.txt");
// A version-3 file whose one language has a name of its own, on no list: Pantry2.
const OWN_NAME = fixture("declarations/version-3-own-name.txt");

// FILE:LINE: error: TEXT, or FILE:LINE: note: TEXT; the file is everything up to the line number.
const FINDING = /^(.+):([0-9]+): (error|note): (.+)$/;

/** The findings a run printed, without the last line that sums up. */
const findings = (out: readonly string[]): readonly string[] => out.slice(0, -1);

describe("check", () => {
  test("the repository's own declarations file is clean: exit 0, notes only", () => {
    const { code, out, err } = run(["check", PUBLISHED]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    expect(findings(out)).toHaveLength(9);
    for (const line of findings(out)) {
      expect(line.match(FINDING)?.slice(1, 4)).toEqual([PUBLISHED, expect.any(String), "note"]);
    }
    expect(out.at(-1)).toBe(
      `typify: ${PUBLISHED}: no errors, 9 notes (declares LanguageDeclarations)`,
    );
  });

  test("unbound placeholders produce notes and exit 0", () => {
    const { code, out } = run(["check", VALID]);
    expect(code).toBe(0);
    expect(out).toContain(
      `${VALID}:71: note: <reader> has no \`is\` line in Contents or in LanguageDeclarations; version 2 reads it as free text`,
    );
    expect(out.at(-1)).toBe(
      `typify: ${VALID}: no errors, 11 notes (declares LanguageDeclarations, Contents, Structure)`,
    );
  });

  test("every error is one line, FILE:LINE: error: TEXT, and the exit is 1", () => {
    const file = fixture("declarations/form-without-does.txt");
    const { code, out, err } = run(["check", file]);
    expect(code).toBe(1);
    expect(err).toEqual([]);
    expect(out.filter((line) => line.includes(": error: "))).toEqual([
      `${file}:56: error: this form is not followed by its does: the next line must be \`does : <text>\``,
    ]);
    expect(out.at(-1)).toBe(
      `typify: ${file}: 1 error, 9 notes (declares LanguageDeclarations, Structure)`,
    );
  });

  // Under version 3 `Name` is a sentence, so the name on a declare line is the file's own choice.
  test("a version-3 file that declares a name of its own is clean: exit 0, notes only", () => {
    const { code, out, err } = run(["check", OWN_NAME]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    expect(findings(out)).toHaveLength(9);
    expect(out.at(-1)).toBe(
      `typify: ${OWN_NAME}: no errors, 9 notes (declares LanguageDeclarations, Pantry2)`,
    );
  });

  test("a version-2 file that declares a name off the list is refused: exit 1", () => {
    const file = fixture("declarations/name-outside-list.txt");
    const { code, out, err } = run(["check", file]);
    expect(code).toBe(1);
    expect(err).toEqual([]);
    expect(out.filter((line) => line.includes(": error: "))).toHaveLength(1);
    expect(out.at(-1)).toStartWith(`typify: ${file}: 1 error, `);
  });

  test("a first body that fails is the only thing reported, and the last line says so", () => {
    const file = fixture("declarations/meta-version-4.txt");
    expect(run(["check", file])).toEqual({
      code: 1,
      out: [
        `${file}:2: error: LanguageDeclarations declares version 4; this reader knows versions 2 and 3 and refuses the body`,
        `typify: ${file}: 1 error (the first body failed, so nothing after it was read)`,
      ],
      err: [],
    });
  });
});

describe("check-program", () => {
  test("a valid program: exit 0 and nothing but the last line", () => {
    const program = fixture("programs/shelf.txt");
    expect(run(["check-program", VALID, "Contents", program])).toEqual({
      code: 0,
      out: [`typify: ${program}: no errors (a program of Contents)`],
      err: [],
    });
  });

  test("a program with an error: exit 1 and one line for it, naming the program file", () => {
    const program = fixture("programs/shelf-once-violated.txt");
    expect(run(["check-program", VALID, "Contents", program])).toEqual({
      code: 1,
      out: [
        `${program}:3: error: 'shelf' may appear at most once in a program of Contents; it first appears at line 2`,
        `typify: ${program}: 1 error (a program of Contents)`,
      ],
      err: [],
    });
  });

  test("a program of a language with a name of its own, under a version-3 file", () => {
    const program = fixture("programs/pantry.txt");
    expect(run(["check-program", OWN_NAME, "Pantry2", program])).toEqual({
      code: 0,
      out: [`typify: ${program}: no errors (a program of Pantry2)`],
      err: [],
    });
    const wrong = fixture("programs/pantry-token-outside-class.txt");
    expect(run(["check-program", OWN_NAME, "Pantry2", wrong])).toEqual({
      code: 1,
      out: [
        `${wrong}:3: error: 'mustard' is not one of: jam | pickle | honey (form \`stock : <jar>\`)`,
        `typify: ${wrong}: 1 error (a program of Pantry2)`,
      ],
      err: [],
    });
  });

  test("the program is not read when the declarations file has an error", () => {
    const declarations = fixture("declarations/once-unknown-verb.txt");
    const program = fixture("programs/bookcase.txt");
    const { code, out } = run(["check-program", declarations, "Structure", program]);
    expect(code).toBe(1);
    expect(out.filter((line) => line.includes(": error: "))).toHaveLength(1);
    expect(out.filter((line) => line.startsWith(`${program}:`))).toEqual([]);
    expect(out.slice(-2)).toEqual([
      `typify: ${declarations}: 1 error, 9 notes (declares LanguageDeclarations, Structure)`,
      `typify: ${program}: not read, because ${declarations} has errors`,
    ]);
  });
});

describe("the command line itself", () => {
  const usage = [
    "usage: bun src/cli.ts check DECLARATIONS_FILE",
    "       bun src/cli.ts check-program DECLARATIONS_FILE LANGUAGE PROGRAM_FILE",
    "       bun src/cli.ts --help",
  ];

  test("a wrong command line exits 2 and prints the usage", () => {
    const wrong: readonly (readonly [readonly string[], string])[] = [
      [[], "typify: a command is expected: check or check-program"],
      [["run", VALID], "typify: 'run' is not a command: check or check-program is expected"],
      [["check"], "typify: check takes one argument: DECLARATIONS_FILE"],
      [["check", VALID, VALID], "typify: check takes one argument: DECLARATIONS_FILE"],
      [
        ["check-program", VALID, "Contents"],
        "typify: check-program takes three arguments: DECLARATIONS_FILE LANGUAGE PROGRAM_FILE",
      ],
    ];
    for (const [args, problem] of wrong) {
      expect(run(args)).toEqual({ code: 2, out: [], err: [problem, ...usage] });
    }
  });

  test("a file that cannot be read exits 2", () => {
    const missing = fixture("declarations/no-such-file.txt");
    const { code, out, err } = run(["check", missing]);
    expect(code).toBe(2);
    expect(out).toEqual([]);
    expect(err).toHaveLength(1);
    expect(err[0]).toStartWith(`typify: ${missing} cannot be read: `);
    expect(run(["check-program", VALID, "Contents", missing]).code).toBe(2);
  });

  test("a language the declarations file does not declare exits 2", () => {
    expect(run(["check-program", VALID, "Recipes", fixture("programs/shelf.txt")])).toEqual({
      code: 2,
      out: [],
      err: [
        `typify: ${VALID} declares LanguageDeclarations, Contents, Structure, and no language named Recipes`,
      ],
    });
  });

  // The versions the help names are the ones the reader refuses a body above (the `check` test of
  // a version-4 first body prints the same pair).
  test("--help says which versions it reads: 2 and 3", () => {
    const help = run(["--help"]).out.join("\n");
    expect(help).toStartWith("typify: a reader of versions 2 and 3 of the meta language.");
    expect(help).toContain("`declare : LanguageDeclarations` at version 2 or 3");
    expect(help).toContain("Under version 2 a name must also be one the meta body lists.");
  });

  test("--help exits 0 and says what the reader does not do", () => {
    const { code, out, err } = run(["--help"]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    const help = out.join("\n");
    for (const line of usage) expect(help).toContain(line);
    expect(help).toContain("Not implemented, and not guessed at");
    for (const missing of [
      "tree walk",
      "path templates",
      "sees",
      "absent",
      "derived!",
      "action step",
    ]) {
      expect(help).toContain(missing);
    }
  });
});

describe("as a process", () => {
  // The child must not inherit the colour settings of the run that spawned it: with FORCE_COLOR set,
  // Bun colours what the child writes to standard error, and the usage then starts with an escape
  // code, not with "typify:". FORCE_COLOR and CLICOLOR_FORCE are left out of the child's environment
  // and NO_COLOR is set, so the text read back is plain whatever the parent's environment holds.
  const { FORCE_COLOR: _force, CLICOLOR_FORCE: _clicolor, ...inherited } = process.env;
  const env = { ...inherited, NO_COLOR: "1" };
  const spawn = (...args: readonly string[]) => {
    const { exitCode, stdout, stderr } = Bun.spawnSync([process.execPath, CLI, ...args], { env });
    return { exitCode, stdout: stdout.toString(), stderr: stderr.toString() };
  };

  test("findings go to standard output and the exit status is 0 when clean", () => {
    const { exitCode, stdout, stderr } = spawn("check", PUBLISHED);
    expect(exitCode).toBe(0);
    expect(stderr).toBe("");
    expect(stdout.trimEnd().split("\n")).toHaveLength(10);
  });

  test("the exit status is 1 on an error", () => {
    const { exitCode, stdout } = spawn("check", fixture("declarations/no-shape.txt"));
    expect(exitCode).toBe(1);
    expect(stdout).toContain(":55: error: not a legal line: ");
  });

  test("the exit status is 2 on a wrong command line, and the usage goes to standard error", () => {
    const { exitCode, stdout, stderr } = spawn();
    expect(exitCode).toBe(2);
    expect(stdout).toBe("");
    expect(stderr).toStartWith("typify: a command is expected");
  });
});
