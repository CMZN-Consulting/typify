// The per-program part of step 4 of ARCHITECTURE.md section 4: one program against one language.

import { describe, expect, test } from "bun:test";
import { readProgram } from "../src/program.ts";
import { body, fixture, languageOf, textOf, withMeta } from "./support.ts";

const VALID = textOf(fixture("declarations/valid.txt"));

/** The errors of a program of one language of a declarations file, each as `LINE: TEXT`. */
function errorsIn(declarations: string, name: string, program: string): readonly string[] {
  const { language, meta } = languageOf(declarations, name);
  return readProgram(program, language, meta).map(({ line, text }) => `${line}: ${text}`);
}

const shelf = (name: string): readonly string[] =>
  errorsIn(VALID, "Contents", textOf(fixture(`programs/${name}.txt`)));

describe("a program of a declared language", () => {
  test("a valid one has no error", () => {
    expect(shelf("shelf")).toEqual([]);
  });

  test("an undeclared line is an error", () => {
    expect(shelf("shelf-undeclared-line")).toEqual([
      "3: Contents declares no form for this line; its colon forms are `shelf : <label>`, `loan : <title> to <reader>`",
    ]);
  });

  test("a token outside a listed class is an error", () => {
    expect(shelf("shelf-token-outside-class")).toEqual([
      "3: 'memoir' is not one of: novel | essay | poetry (form `<title> -> <genre>`)",
    ]);
  });

  test("a once verb may appear at most once", () => {
    expect(shelf("shelf-once-violated")).toEqual([
      "3: 'shelf' may appear at most once in a program of Contents; it first appears at line 2",
    ]);
  });

  test("under order : strict the forms appear in the order they are declared", () => {
    expect(shelf("shelf-order-violated")).toEqual([
      "4: this line is out of order: Contents is `order : strict`, so `<title> -> <genre>` is declared before `loan : <title> to <reader>` and comes before it",
    ]);
  });

  test("the language's own comment prefix is honoured, and # is then not a comment", () => {
    expect(shelf("shelf-hash-line")).toEqual([
      "2: not a legal line: expected a bang line `verb!`, a colon line `verb : argument` or an arrow line `left -> right`",
    ]);
  });

  test("with no comment line the prefix is #, and with no order line any order is legal", () => {
    expect(errorsIn(VALID, "Structure", textOf(fixture("programs/bookcase.txt")))).toEqual([]);
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

  // The literal ` to ` is matched exactly, so the blanks beside it fall to the tokens: the title taken
  // here is `Emma` with two blanks after it. A token is stripped before it is held to its class.
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

  // Section 2: a line that holds a colon is a colon line. A title with a colon in it therefore turns
  // an arrow line into a colon line, which Contents has no form for.
  test("a line with a colon before its arrow is read as a colon line", () => {
    expect(errorsIn(VALID, "Contents", "Walden: Life in the Woods -> essay\n")).toEqual([
      "1: Contents declares no form for this line; its colon forms are `shelf : <label>`, `loan : <title> to <reader>`",
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

  // The page does not say which form a line is when it fits more than one. The reader tries the forms
  // in the order they are declared and takes the first that fits. Here `note : short` is of the first
  // form, not the third, so it is out of order.
  test("a line that fits two forms is of the one declared first", () => {
    expect(errorsIn(declarations, "Contents", "sealed!\nnote : short\n")).toEqual([
      "2: this line is out of order: Contents is `order : strict`, so `note : <text>` is declared before `sealed!` and comes before it",
    ]);
  });

  // Section 3: "`once` says the form's verb may appear at most once in one program file". The verb is
  // counted, so two forms that share a verb share the one appearance.
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
