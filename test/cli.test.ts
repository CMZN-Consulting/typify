import { describe, expect, test } from "bun:test";

import { run } from "../src/cli.ts";
import { CLI, fixture } from "./support.ts";

const VALID = fixture("declarations/valid.typify");
const OWN_NAME = fixture("declarations/version-3-own-name.typify");
const FINDING = /^(.+):([0-9]+): (error|note): (.+)$/;

const findings = (out: readonly string[]): readonly string[] => out.slice(0, -1);

describe("check", () => {
  test("consumer declarations report their own notes without bundled grammar notes", async () => {
    const { code, out, err } = await run(["check", VALID]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    expect(findings(out)).toHaveLength(2);
    for (const line of findings(out)) {
      expect(line.match(FINDING)?.slice(1, 4)).toEqual([VALID, expect.any(String), "note"]);
      expect(line).not.toContain("has no `is` line in LanguageDeclarations;");
    }
    expect(out.some((line) => line.includes("<reader> has no `is` line in Contents"))).toBe(true);
    expect(out.at(-1)).toEndWith("(declares LanguageDeclarations, Contents, Structure)");
    expect(out.at(-1)).toContain("no errors");
  });

  test("every error names its source file and line, and the exit is 1", async () => {
    const file = fixture("declarations/form-without-does.typify");
    const { code, out, err } = await run(["check", file]);
    expect(code).toBe(1);
    expect(err).toEqual([]);
    const errors = findings(out).filter((line) => line.includes(": error: "));
    expect(errors).toHaveLength(1);
    expect(errors[0]?.match(FINDING)?.slice(1, 4)).toEqual([file, expect.any(String), "error"]);
    expect(errors[0]).toContain("this form is not followed by its does");
    expect(out.at(-1)).toContain("1 error");
  });

  test("a version-3 file can declare a name of its own", async () => {
    const { code, out, err } = await run(["check", OWN_NAME]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    expect(out.at(-1)).toEndWith("(declares LanguageDeclarations, Pantry2)");
    expect(out.at(-1)).toContain("no errors");
  });

  test("a version-2 name outside its grammar's list is refused", async () => {
    const file = fixture("declarations/name-outside-list.typify");
    const { code, out, err } = await run(["check", file]);
    expect(code).toBe(1);
    expect(err).toEqual([]);
    expect(out.filter((line) => line.includes(": error: "))).toHaveLength(1);
  });

  test("an unsupported grammar selector is reported before language bodies", async () => {
    const file = fixture("declarations/grammar-version-4.typify");
    const { code, out, err } = await run(["check", file]);
    expect(code).toBe(1);
    expect(err).toEqual([]);
    expect(findings(out)).toHaveLength(1);
    expect(out[0]).toStartWith(`${file}:1: error: `);
    expect(out[0]).toContain("4");
    expect(out.at(-1)).toContain("the grammar selector failed, so no language bodies were read");
  });
});

describe("check-program", () => {
  test("a valid program has no findings and exits 0", async () => {
    const program = fixture("programs/shelf.typify");
    expect(await run(["check-program", VALID, "Contents", program])).toEqual({
      code: 0,
      out: [`typify: ${program}: no errors (a program of Contents)`],
      err: [],
    });
  });

  test("a program error names the program file and exits 1", async () => {
    const program = fixture("programs/shelf-once-violated.typify");
    expect(await run(["check-program", VALID, "Contents", program])).toEqual({
      code: 1,
      out: [
        `${program}:2: error: 'shelf' may appear at most once in a program of Contents; it first appears at line 1`,
        `typify: ${program}: 1 error (a program of Contents)`,
      ],
      err: [],
    });
  });

  test("a program uses its selected language and token classes", async () => {
    const program = fixture("programs/pantry.typify");
    expect(await run(["check-program", OWN_NAME, "Pantry2", program])).toEqual({
      code: 0,
      out: [`typify: ${program}: no errors (a program of Pantry2)`],
      err: [],
    });
    const wrong = fixture("programs/pantry-token-outside-class.typify");
    expect(await run(["check-program", OWN_NAME, "Pantry2", wrong])).toEqual({
      code: 1,
      out: [
        `${wrong}:1: error: 'mustard' is not one of: jam | pickle | honey (form \`stock : <jar>\`)`,
        `typify: ${wrong}: 1 error (a program of Pantry2)`,
      ],
      err: [],
    });
  });

  test("a program is not read when its declarations have an error", async () => {
    const declarations = fixture("declarations/once-unknown-verb.typify");
    const program = fixture("programs/bookcase.typify");
    const { code, out } = await run(["check-program", declarations, "Structure", program]);
    expect(code).toBe(1);
    expect(out.filter((line) => line.includes(": error: "))).toHaveLength(1);
    expect(out.filter((line) => line.startsWith(`${program}:`))).toEqual([]);
    expect(out.at(-1)).toBe(`typify: ${program}: not read, because ${declarations} has errors`);
  });

  test("invalid declarations are reported before opening an absent program", async () => {
    const program = fixture("programs/not-read.typify");
    for (const declarations of [
      fixture("declarations/grammar-version-4.typify"),
      fixture("declarations/once-unknown-verb.typify"),
    ]) {
      const { code, out, err } = await run(["check-program", declarations, "Structure", program]);
      expect(code).toBe(1);
      expect(err).toEqual([]);
      expect(out.some((line) => line.includes(": error: "))).toBe(true);
      expect(out.at(-1)).toBe(`typify: ${program}: not read, because ${declarations} has errors`);
    }
  });

  test("an unknown language is reported before opening an absent program", async () => {
    const program = fixture("programs/not-read.typify");
    expect(await run(["check-program", VALID, "Recipes", program])).toEqual({
      code: 2,
      out: [],
      err: [
        `typify: ${VALID} declares LanguageDeclarations, Contents, Structure, and no language named Recipes`,
      ],
    });
  });
});

describe("the command line itself", () => {
  const usage = [
    "usage: bun src/cli.ts check DECLARATIONS_FILE",
    "       bun src/cli.ts check-program DECLARATIONS_FILE LANGUAGE PROGRAM_FILE",
    "       bun src/cli.ts --help",
  ];

  test("a wrong command line exits 2 and prints usage", async () => {
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
      expect(await run(args)).toEqual({ code: 2, out: [], err: [problem, ...usage] });
    }
  });

  test("an unreadable file exits 2", async () => {
    const missing = fixture("declarations/no-such-file.typify");
    const { code, out, err } = await run(["check", missing]);
    expect(code).toBe(2);
    expect(out).toEqual([]);
    expect(err).toHaveLength(1);
    expect(err[0]).toStartWith(`typify: ${missing} cannot be read: `);
    expect((await run(["check-program", VALID, "Contents", missing])).code).toBe(2);
  });

  test("an undeclared language exits 2", async () => {
    expect(
      await run(["check-program", VALID, "Recipes", fixture("programs/shelf.typify")]),
    ).toEqual({
      code: 2,
      out: [],
      err: [
        `typify: ${VALID} declares LanguageDeclarations, Contents, Structure, and no language named Recipes`,
      ],
    });
  });

  test("help describes the grammar selector and supported versions", async () => {
    const help = (await run(["--help"])).out.join("\n");
    expect(help).toStartWith("typify: a reader of versions 2 and 3 of the meta language.");
    expect(help).toContain("`typify : 2` or `typify : 3`");
    expect(help).toContain("Under version 2 a name must also be one the meta body lists.");
  });

  test("help exits 0 and states the reader's scope", async () => {
    const { code, out, err } = await run(["--help"]);
    expect(code).toBe(0);
    expect(err).toEqual([]);
    const help = out.join("\n");
    for (const line of usage) expect(help).toContain(line);
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
  const env: Record<string, string | undefined> = { ...Bun.env, NO_COLOR: "1" };
  delete env["FORCE_COLOR"];
  delete env["CLICOLOR_FORCE"];
  const spawn = (...args: readonly string[]) => {
    const { exitCode, stdout, stderr } = Bun.spawnSync([Bun.argv[0]!, CLI, ...args], { env });
    return {
      exitCode,
      stdout: new TextDecoder().decode(stdout),
      stderr: new TextDecoder().decode(stderr),
    };
  };

  test("valid findings use stdout and exit 0", () => {
    const { exitCode, stdout, stderr } = spawn("check", VALID);
    expect(exitCode).toBe(0);
    expect(stderr).toBe("");
    expect(stdout).toContain(`typify: ${VALID}: no errors`);
  });

  test("diagnostic errors exit 1", () => {
    const file = fixture("declarations/no-shape.typify");
    const { exitCode, stdout, stderr } = spawn("check", file);
    expect(exitCode).toBe(1);
    expect(stderr).toBe("");
    expect(stdout).toContain(": error: not a legal line: ");
  });

  test("command misuse uses stderr and exits 2", () => {
    const { exitCode, stdout, stderr } = spawn();
    expect(exitCode).toBe(2);
    expect(stdout).toBe("");
    expect(stderr).toStartWith("typify: a command is expected");
  });
});
