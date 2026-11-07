import { describe, expect, test } from "bun:test";

import { parseProgram, readProgram } from "../src/program.ts";
import { body, fixture, languageOf, textOf, withMeta, withMetaV2 } from "./support.ts";

const VALID = textOf(fixture("declarations/valid.typify"));

function errorsIn(declarations: string, name: string, program: string): readonly string[] {
  const { language, meta } = languageOf(declarations, name);
  return readProgram(program, language, meta).map(({ line, text }) => `${line}: ${text}`);
}

const shelf = (name: string): readonly string[] =>
  errorsIn(VALID, "Contents", textOf(fixture(`programs/${name}.typify`)));

describe("a program of a declared language", () => {
  test("a valid one has no error", () => {
    expect(shelf("shelf")).toEqual([]);
  });

  test("an undeclared line is an error", () => {
    expect(shelf("shelf-undeclared-line")).toEqual([
      "2: Contents declares no form for this line; its colon forms are `shelf : <label>`, `loan : <title> to <reader>`",
    ]);
  });

  test("a token outside a listed class is an error", () => {
    expect(shelf("shelf-token-outside-class")).toEqual([
      "2: 'memoir' is not one of: novel | essay | poetry (form `<title> -> <genre>`)",
    ]);
  });

  test("a once verb may appear at most once", () => {
    expect(shelf("shelf-once-violated")).toEqual([
      "2: 'shelf' may appear at most once in a program of Contents; it first appears at line 1",
    ]);
  });

  test("under order : strict the forms appear in the order they are declared", () => {
    expect(shelf("shelf-order-violated")).toEqual([
      "3: this line is out of order: Contents is `order : strict`, so `<title> -> <genre>` is declared before `loan : <title> to <reader>` and comes before it",
    ]);
  });

  test("the language's own comment prefix is honoured, and # is then not a comment", () => {
    expect(shelf("shelf-hash-line")).toEqual([
      "2: not a legal line: expected a bang line `verb!`, a colon line `verb : argument` or an arrow line `left -> right`",
    ]);
  });

  test("with no comment line the prefix is #, and with no order line any order is legal", () => {
    expect(errorsIn(VALID, "Structure", textOf(fixture("programs/bookcase.typify")))).toEqual([]);
  });
});

describe("token classes in a program", () => {
  test("a sentence and an unbound placeholder admit any text that is not empty", () => {
    expect(errorsIn(VALID, "Contents", "shelf : the one by the window\n")).toEqual([]);
    expect(errorsIn(VALID, "Contents", "shelf :\n")).toEqual([
      "1: <label> is empty: a token is never empty (form `shelf : <label>`)",
    ]);
  });

  test("a placeholder takes the shortest text: the first ` to ` ends the title", () => {
    const declarations = withMeta(
      body(
        "Contents",
        "is : title = Emma | Walden",
        "form : loan : <title> to <reader>",
        "does : a book out on loan",
      ),
    );
    expect(errorsIn(declarations, "Contents", "loan : Emma to Ada to Grace\n")).toEqual([]);
    expect(errorsIn(declarations, "Contents", "loan : Emma to Ada to Walden\n")).toEqual([]);
    expect(errorsIn(declarations, "Contents", "loan : Notes to Self to Ada\n")).toEqual([
      "1: 'Notes' is not one of: Emma | Walden (form `loan : <title> to <reader>`)",
    ]);
  });

  test("a token is stripped before it is compared with the words of its class", () => {
    const declarations = withMeta(
      body(
        "Contents",
        "is : title = Emma | Walden",
        "form : loan : <title> to <reader>",
        "does : a book out on loan",
        "form : <title> -> <reader>",
        "does : the same, as an arrow",
      ),
    );
    expect(errorsIn(declarations, "Contents", "loan : Emma   to   Ada\n")).toEqual([]);
    expect(errorsIn(declarations, "Contents", "   Walden   ->   Ada   \n")).toEqual([]);
  });

  test("a line with a colon before its arrow is read as a colon line", () => {
    expect(errorsIn(VALID, "Contents", "Walden: Life in the Woods -> essay\n")).toEqual([
      "1: Contents declares no form for this line; its colon forms are `shelf : <label>`, `loan : <title> to <reader>`",
    ]);
  });
});

describe("a form of the file's own that uses <Name>", () => {
  const rename = ["form : rename : <Name>", "does : rename the thing called <Name>"];
  const line = "rename : Cook Book\n";

  test("is free text under version 3, was held to the list under version 2, and is held to a class the body binds", () => {
    expect(errorsIn(withMeta(body("Workdir", ...rename)), "Workdir", line)).toEqual([]);
    expect(errorsIn(withMetaV2(body("Workdir", ...rename)), "Workdir", line)).toEqual([
      "1: 'Cook Book' is not one of: LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats (form `rename : <Name>`)",
    ]);
    const bound = body("Workdir", "is : Name = oak | pine", ...rename);
    expect(errorsIn(withMeta(bound), "Workdir", line)).toEqual([
      "1: 'Cook Book' is not one of: oak | pine (form `rename : <Name>`)",
    ]);
  });
});

describe("which form a line is", () => {
  const declarations = withMeta(
    body(
      "Contents",
      "order : strict",
      "once : size",
      "form : note : <text>",
      "does : a note in free words",
      "form : sealed!",
      "does : nothing is added below this line",
      "form : note : short | long",
      "does : a note of a fixed kind",
      "form : size : small | large",
      "does : the size of the shelf, as a word",
      "form : size : <width> by <depth>",
      "does : the size of the shelf, as two measures",
    ),
  );

  test("a line that fits two forms is of the one declared first", () => {
    expect(errorsIn(declarations, "Contents", "sealed!\nnote : short\n")).toEqual([
      "2: this line is out of order: Contents is `order : strict`, so `note : <text>` is declared before `sealed!` and comes before it",
    ]);
  });

  test("once counts the verb, across the forms that share it", () => {
    expect(errorsIn(declarations, "Contents", "size : small\nsize : 80 by 30\n")).toEqual([
      "2: 'size' may appear at most once in a program of Contents; it first appears at line 1",
    ]);
  });

  test("a line that fits no form says how it misses each form with its verb", () => {
    expect(errorsIn(declarations, "Contents", "size : medium\n")).toEqual([
      "1: 'medium' is not one of: small | large (form `size : small | large`); 'medium' does not fit `<width> by <depth>` (form `size : <width> by <depth>`)",
    ]);
  });
});

describe("order : strict", () => {
  test("a form may repeat, and the order never goes back", () => {
    const program = ["shelf : top", "Emma -> novel", "Walden -> essay", "loan : Emma to Ada"];
    expect(errorsIn(VALID, "Contents", program.join("\n"))).toEqual([]);
  });

  test("after a line out of order, the lines below are held to the furthest form read", () => {
    const program = [
      "loan : Emma to Ada",
      "shelf : top",
      "Emma -> novel",
      "loan : Walden to Grace",
    ];
    expect(errorsIn(VALID, "Contents", program.join("\n"))).toEqual([
      "2: this line is out of order: Contents is `order : strict`, so `shelf : <label>` is declared before `loan : <title> to <reader>` and comes before it",
      "3: this line is out of order: Contents is `order : strict`, so `<title> -> <genre>` is declared before `loan : <title> to <reader>` and comes before it",
    ]);
  });
});

describe("a parsed program for a consumer", () => {
  test("returns source positions, original lines and the exact first matched form", () => {
    const declarations = withMeta(
      body(
        "Notes",
        "form : note : <text>",
        "does : a note",
        "form : note : short | long",
        "does : a note of a fixed kind",
      ),
    );
    const { language, meta } = languageOf(declarations, "Notes");
    const firstForm = language.forms[0];
    if (firstForm === undefined) throw new Error("the fixture has no form");
    const raw = "  note : two  words  \r";
    const parsed = parseProgram(
      `# comment\r\n\r\n${raw}\nnote : short\n`,
      language,
      meta,
      "notes.typify",
    );
    expect(parsed.valid).toBe(true);
    if (!parsed.valid) throw new Error("a valid program was refused");
    expect(parsed.source).toBe("notes.typify");
    expect(parsed.diagnostics).toEqual([]);
    expect(parsed.lines).toHaveLength(2);
    expect(parsed.lines[0]).toEqual({
      line: 3,
      raw,
      formIndex: 0,
      form: firstForm,
      captures: [{ name: "text", text: "two  words" }],
    });
    expect(parsed.lines[1]?.form).toBe(firstForm);
    expect(parsed.lines[1]?.captures).toEqual([{ name: "text", text: "short" }]);
  });

  test("captures all three line shapes, verb placeholders and repeated names in source order", () => {
    const declarations = withMeta(
      body(
        "Notes",
        "is : label = top | bottom",
        "form : <label>!",
        "does : a label",
        "form : <label> : <text>",
        "does : a labelled note",
        "form : <label> -> <label>",
        "does : a pair of labels",
        "form : state : open | shut",
        "does : a literal choice",
      ),
    );
    const { language, meta } = languageOf(declarations, "Notes");
    const parsed = parseProgram(
      "top!\nbottom : a  note\ntop -> bottom\nstate : shut",
      language,
      meta,
    );
    expect(parsed.valid).toBe(true);
    if (!parsed.valid) throw new Error("a valid program was refused");
    expect(parsed.source).toBe("<input>");
    expect(parsed.lines.map(({ captures }) => captures)).toEqual([
      [{ name: "label", text: "top" }],
      [
        { name: "label", text: "bottom" },
        { name: "text", text: "a  note" },
      ],
      [
        { name: "label", text: "top" },
        { name: "label", text: "bottom" },
      ],
      [],
    ]);
  });

  test("captures use the same shortest split and stripped token values as validation", () => {
    const declarations = withMeta(
      body(
        "Notes",
        "is : title = Emma | Walden",
        "form : loan : <title> to <reader>",
        "does : a book on loan",
      ),
    );
    const { language, meta } = languageOf(declarations, "Notes");
    const parsed = parseProgram("loan : Emma   to   Ada to Grace", language, meta);
    expect(parsed.valid).toBe(true);
    if (!parsed.valid) throw new Error("a valid program was refused");
    expect(parsed.lines[0]?.captures).toEqual([
      { name: "title", text: "Emma" },
      { name: "reader", text: "Ada to Grace" },
    ]);
  });

  test("a comment-only program succeeds with no matched lines", () => {
    const { language, meta } = languageOf(VALID, "Contents");
    expect(parseProgram("// comment\n\n", language, meta)).toEqual({
      valid: true,
      source: "<input>",
      lines: [],
      diagnostics: [],
    });
  });

  test("any validation error withholds all lines and preserves compatibility diagnostics", () => {
    const { language, meta } = languageOf(VALID, "Contents");
    for (const program of [
      "shelf : top\nnot a line",
      "shelf : top\ncolour : red",
      "shelf : top\nWalden -> memoir",
      "shelf : top\nshelf : bottom",
      "loan : Emma to Ada\nshelf : top",
    ]) {
      const parsed = parseProgram(program, language, meta, "invalid.typify");
      expect(parsed.valid).toBe(false);
      expect(parsed.source).toBe("invalid.typify");
      expect("lines" in parsed).toBe(false);
      expect(parsed.diagnostics.length).toBeGreaterThan(0);
      expect(parsed.diagnostics).toEqual(readProgram(program, language, meta));
    }
  });
});
