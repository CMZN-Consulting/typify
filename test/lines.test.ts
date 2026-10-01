// ARCHITECTURE.md section 2: the line shapes, and the placeholders of a production (section 3).

import { describe, expect, test } from "bun:test";
import { classifyLine, classifyShape, fillTemplate, parseTemplate } from "../src/lines.ts";

const fill = (template: string, text: string) => fillTemplate(parseTemplate(template), text);

describe("nothing (item 1)", () => {
  test("a blank line is nothing", () => {
    expect(classifyLine("", "#")).toEqual({ shape: "nothing" });
    expect(classifyLine("  \t  ", "#")).toEqual({ shape: "nothing" });
  });

  test("a line whose first non-blank characters are the comment prefix is nothing", () => {
    expect(classifyLine("   # required!", "#")).toEqual({ shape: "nothing" });
    expect(classifyLine("// required!", "//")).toEqual({ shape: "nothing" });
  });

  test("the prefix is the declared one: under // a line starting with # is not a comment", () => {
    expect(classifyLine("# a note", "//").shape).toBe("none");
    expect(classifyLine("#end!", "//")).toEqual({ shape: "bang", verb: "#end" });
  });

  test("leading and trailing blanks are stripped, a carriage return and a byte-order mark too", () => {
    expect(classifyLine("﻿  end!  \r", "#")).toEqual({ shape: "bang", verb: "end" });
  });
});

describe("a bang line (item 2)", () => {
  test("one word ending in an exclamation mark", () => {
    expect(classifyShape("end!")).toEqual({ shape: "bang", verb: "end" });
    expect(classifyShape("<label>!")).toEqual({ shape: "bang", verb: "<label>" });
  });

  test("two words are not one word, and a lone exclamation mark has no verb", () => {
    expect(classifyShape("two words!").shape).toBe("none");
    expect(classifyShape("!").shape).toBe("none");
  });
});

describe("a colon line (item 3)", () => {
  test("the first colon splits it and the blanks around the colon are stripped", () => {
    expect(classifyShape("form            :   declare : <Name>")).toEqual({
      shape: "colon",
      verb: "form",
      argument: "declare : <Name>",
    });
  });

  test("the argument keeps its inner spacing and may itself contain colons and arrows", () => {
    expect(classifyShape("does : a  b   c: d -> e")).toEqual({
      shape: "colon",
      verb: "does",
      argument: "a  b   c: d -> e",
    });
  });

  test("the argument may be empty", () => {
    expect(classifyShape("absent :")).toEqual({ shape: "colon", verb: "absent", argument: "" });
  });

  // `<verb> : <argument>`: the page names no line whose verb is missing, so one is of no shape.
  test("a verb is needed before the colon", () => {
    expect(classifyShape(": argument")).toEqual({
      shape: "none",
      expected: "a verb before the first colon",
    });
  });
});

describe("an arrow line (item 4)", () => {
  test("the first arrow splits it, both sides stripped", () => {
    expect(classifyShape("left   ->   right -> more")).toEqual({
      shape: "arrow",
      left: "left",
      right: "right -> more",
    });
  });

  // `<left> -> <right>`: the page names no arrow line with a side missing, so one is of no shape.
  test("text is needed on both sides", () => {
    expect(classifyShape("-> right").shape).toBe("none");
    expect(classifyShape("left ->")).toEqual({
      shape: "none",
      expected: "text on both sides of the first arrow",
    });
  });
});

describe("the order of classification", () => {
  // Section 2 lists the shapes as bang, colon, arrow, and says of a bang line "no colon, no arrow".
  // It does not say what a line holding both a colon and an arrow is. The reader tries the shapes in
  // the order listed, so such a line is a colon line, wherever the arrow stands.
  test("a line that holds a colon is a colon line even when an arrow comes first", () => {
    expect(classifyShape("a -> b : c")).toEqual({ shape: "colon", verb: "a -> b", argument: "c" });
  });

  test("a word ending in an exclamation mark is not a bang line when it holds a colon or an arrow", () => {
    expect(classifyShape("go:now!")).toEqual({ shape: "colon", verb: "go", argument: "now!" });
    expect(classifyShape("go->there!")).toEqual({ shape: "arrow", left: "go", right: "there!" });
  });
});

describe("a line that is none of these", () => {
  test("is an error that says what a legal line is", () => {
    expect(classifyShape("this line has no shape")).toEqual({
      shape: "none",
      expected:
        "a bang line `verb!`, a colon line `verb : argument` or an arrow line `left -> right`",
    });
  });
});

describe("placeholders (section 3: a production is a line shape with placeholders in angle brackets)", () => {
  test("a placeholder is a name in angle brackets; the rest is literal text", () => {
    expect(parseTemplate("<name>=<value>").parts).toEqual([
      { kind: "placeholder", name: "name" },
      { kind: "literal", text: "=" },
      { kind: "placeholder", name: "value" },
    ]);
  });

  // The page says "placeholders in angle brackets" and does not say what a name is. The reader takes a
  // letter followed by letters, digits, dashes or underscores.
  test("angle brackets that do not hold a name are literal text", () => {
    for (const text of ["< x >", "a < b > c", "<two words>", "<9lives>", "<>"]) {
      expect(parseTemplate(text).parts).toEqual([{ kind: "literal", text }]);
    }
  });

  test("literal text matches exactly", () => {
    expect(fill("stack", "stack")).toEqual([]);
    expect(fill("stack", "stack up")).toBeNull();
    expect(fill("stack", "Stack")).toBeNull();
  });

  test("an empty template is filled by empty text only", () => {
    expect(fill("", "")).toEqual([]);
    expect(fill("", "anything")).toBeNull();
  });

  test("a placeholder takes the shortest text up to the leftmost literal after it; the last takes the rest", () => {
    expect(fill("<title> to <reader>", "Notes to Self to Ada")).toEqual([
      { name: "title", text: "Notes" },
      { name: "reader", text: "Self to Ada" },
    ]);
  });

  test("literal text after the last placeholder must end the line, and the placeholder takes the rest", () => {
    expect(fill("<count> copies", "3 copies")).toEqual([{ name: "count", text: "3" }]);
    expect(fill("<count> copies", "3 copies left")).toBeNull();
    expect(fill("<title> (lent)", "Emma (lent) (lent)")).toEqual([
      { name: "title", text: "Emma (lent)" },
    ]);
  });

  test("a literal that is missing leaves the template unfilled", () => {
    expect(fill("<name>=<value>", "no equals sign")).toBeNull();
    expect(fill("at <place>", "in Paris")).toBeNull();
  });

  // Leftmost-shortest, nothing tried twice: the first "=" ends <name> even when that leaves it empty.
  // Whether an empty token is admitted is the token class's business (it never is).
  test("the leftmost literal ends a placeholder even when the placeholder is left empty", () => {
    expect(fill("<name>=<value>", "=x=y")).toEqual([
      { name: "name", text: "" },
      { name: "value", text: "x=y" },
    ]);
  });

  // The page does not say what stands between two placeholders. With nothing between them the shortest
  // text for the first is none, so it takes nothing; no token is empty, so such a form fits no line.
  test("of two placeholders with nothing between them the first takes nothing", () => {
    expect(fill("<key><value>", "ab")).toEqual([
      { name: "key", text: "" },
      { name: "value", text: "ab" },
    ]);
  });
});
