// ARCHITECTURE.md sections 1 and 3, and steps 1 and 2 of section 4: reading a declarations file.

import { describe, expect, test } from "bun:test";
import { readDeclarations } from "../src/declarations.ts";
import {
  body,
  errorLines,
  errorsOf,
  fixture,
  lineOf,
  notesOf,
  PUBLISHED,
  PUBLISHED_V2,
  textOf,
  withMeta,
  withMetaV2,
} from "./support.ts";

const read = (name: string) => readDeclarations(textOf(fixture(`declarations/${name}.txt`)));

/** A small body that is clean: the lines every invented body in these tests starts from. */
const BOOKCASE = [
  "version : 1",
  "file : Structure.txt",
  "required!",
  "is : material = oak | pine | steel",
  "form : made-of : <material>",
  "does : what the bookcase is built from",
];

describe("the published meta body", () => {
  const reading = readDeclarations(textOf(PUBLISHED));

  test("validates itself and equals the reader's copy", () => {
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations"]);
    expect(reading.meta?.forms).toHaveLength(16);
  });

  test("uses nine placeholders that no `is` line binds, and each is noted once", () => {
    expect(notesOf(reading.diagnostics).map(({ line, text }) => `${line}: ${text}`)).toEqual(
      [
        "15: <number>",
        "17: <path>",
        "26: <prefix>",
        "28: <name>",
        "28: <domain>",
        "31: <production>",
        "33: <text>",
        "35: <line>",
        "43: <default>",
      ].map(
        (use) =>
          `${use} has no \`is\` line in LanguageDeclarations; version 3 reads it as free text`,
      ),
    );
  });
});

describe("the published version-2 meta body", () => {
  const reading = readDeclarations(textOf(PUBLISHED_V2));
  const notes = notesOf(reading.diagnostics);

  // The notes of a first body name the version of the body the file is read through, which for the
  // first body is the version it states: here 2, where the version-3 body's notes say 3.
  test("has the same nine notes as version 3, and they say version 2", () => {
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(notes).toHaveLength(9);
    for (const note of notes) {
      expect(note.text).toEndWith("; version 2 reads it as free text");
    }
    const version3 = notesOf(readDeclarations(textOf(PUBLISHED)).diagnostics);
    expect(notes.map(({ line }) => line)).toEqual(version3.map(({ line }) => line));
  });
});

describe("a file of valid bodies", () => {
  const reading = read("valid");

  test("has no error, and declares its languages in the order of the file", () => {
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect([...reading.languages.keys()]).toEqual([
      "LanguageDeclarations",
      "Contents",
      "Structure",
    ]);
  });

  test("an unbound placeholder is a note, not an error, once per placeholder per body", () => {
    const text = textOf(fixture("declarations/valid.txt"));
    const own = notesOf(reading.diagnostics).filter(({ line }) => line > 47);
    expect(own).toEqual([
      {
        line: lineOf(text, "shelf : <label>"),
        severity: "note",
        text: "<label> has no `is` line in Contents or in LanguageDeclarations; version 2 reads it as free text",
      },
      {
        line: lineOf(text, "loan : <title> to <reader>"),
        severity: "note",
        text: "<reader> has no `is` line in Contents or in LanguageDeclarations; version 2 reads it as free text",
      },
    ]);
  });

  test("what a body states is what the reader holds", () => {
    const contents = reading.languages.get("Contents");
    expect(contents?.commentPrefix).toBe("//");
    expect(contents?.order).toBe("strict");
    expect([...(contents?.once ?? [])]).toEqual(["shelf"]);
    expect(contents?.forms.map((form) => form.text)).toEqual([
      "shelf : <label>",
      "full!",
      "<title> -> <genre>",
      "loan : <title> to <reader>",
    ]);
    const structure = reading.languages.get("Structure");
    expect(structure?.commentPrefix).toBe("#");
    expect(structure?.order).toBe("free");
  });
});

describe("one fixture per error class", () => {
  const classes: readonly (readonly [string, number, RegExp])[] = [
    ["no-shape", 55, /^not a legal line: expected a bang line `verb!`, a colon line/],
    ["outside-body", 61, /^this line is outside any body: a body opens with `declare : <Name>`/],
    ["missing-end", 51, /^the body Structure has no end!: a body closes with `end!`$/],
    ["nested-declare", 58, /^a body may not open inside another: Structure, opened at line 51,/],
    ["duplicate-name", 60, /^Structure is declared twice \(first at line 51\)/],
    ["form-without-does", 56, /^this form is not followed by its does: the next line must be/],
    ["form-with-two-does", 58, /^this does has no form directly above it: a form has exactly one/],
    ["form-between-form-and-does", 57, /^this form is not followed by its does/],
    [
      "undeclared-line",
      55,
      /^LanguageDeclarations declares no form for this line; its colon forms/,
    ],
    [
      "example-mismatch",
      58,
      /^the example does not fit the form above it, `made-of : <material>`: 'marble' is not one of: oak \| pine \| steel$/,
    ],
    ["once-unknown-verb", 58, /^once : colour names no form of Structure: the verb of one of its/],
    ["sees-unknown-body", 57, /^sees : OrderOfSessions names no body of this file/],
    [
      "name-outside-list",
      51,
      /^'Recipes' is not one of: LanguageDeclarations \| OrderOfSessions \| Workdir \| Structure \| Contents \| Seats \(form `declare : <Name>`\)$/,
    ],
    [
      "altered-meta",
      10,
      /^the first body is not the published version-2 meta body: expected `is : Name = .* \| Seats`, found `is : Name = .* \| Seats \| Shelf`$/,
    ],
    [
      "meta-version-4",
      2,
      /^LanguageDeclarations declares version 4; this reader knows versions 2 and 3 and refuses the body$/,
    ],
    [
      "version-3-with-the-list-of-version-2",
      10,
      /^the first body is not the published version-3 meta body: expected `is : Name = one word that names a language body of the file`, found `is : Name = .* \| Seats`$/,
    ],
  ];

  test.each(classes)("%s: exactly one error, at line %d", (name, line, text) => {
    const errors = errorsOf(read(name).diagnostics);
    expect(errors).toHaveLength(1);
    expect(errors[0]?.line).toBe(line);
    expect(errors[0]?.text).toMatch(text);
  });
});

describe("step 1: the first body", () => {
  test("must be `declare : LanguageDeclarations`", () => {
    const expected =
      "the file must open with `declare : LanguageDeclarations`: the first body declares the language the file is written in";
    expect(errorLines("")).toEqual([`1: ${expected}`]);
    expect(errorLines(`\n\n${body("Structure", ...BOOKCASE)}\n`)).toEqual([`3: ${expected}`]);
    expect(errorLines(`order : strict\n${textOf(PUBLISHED)}`)).toEqual([`1: ${expected}`]);
  });

  // Step 1: "a meta body that fails its own forms stops the reader before anything else is read".
  // Here the does of the first form is gone. Both checks of the first body speak (its own forms, and
  // the reader's copy), and the body below, with its undeclared line, is never read.
  test("a first body that fails its own forms stops the reader before anything else is read", () => {
    const lines = textOf(PUBLISHED).split("\n");
    const broken = [...lines.slice(0, 12), ...lines.slice(13)].join("\n");
    const reading = readDeclarations(`${broken}\n${body("Structure", "colour : red")}\n`);
    expect(errorsOf(reading.diagnostics).map(({ line, text }) => `${line}: ${text}`)).toEqual([
      "12: this form is not followed by its does: the next line must be `does : <text>`",
      "13: the first body is not the published version-3 meta body: expected `does : open a language body named <Name>; the body runs to the next end!`, found `example : declare : Structure`",
    ]);
    expect(reading.meta).toBeNull();
    expect(reading.languages.size).toBe(0);
  });

  // Section 2, item 1: "Before the first body's `comment` line has been read, `#` is assumed". Once
  // it has been read the declared prefix holds, so a first body that declares another prefix turns
  // its own # lines (6 to 9) into lines of no shape, and is not the published body either.
  test("the first body's comment prefix holds from its comment line on", () => {
    const text = textOf(PUBLISHED).replace(/^comment( +):( +)#$/m, "comment$1:$2//");
    expect(errorLines(text).map((error) => error.slice(0, 60))).toEqual([
      "5: the first body is not the published version-3 meta body: ",
      "6: not a legal line: expected a bang line `verb!`, a colon l",
      "7: not a legal line: expected a bang line `verb!`, a colon l",
      "8: not a legal line: expected a bang line `verb!`, a colon l",
      "9: not a legal line: expected a bang line `verb!`, a colon l",
    ]);
  });

  test("a failed first body leaves nothing declared", () => {
    for (const name of ["altered-meta", "meta-version-4", "version-3-with-the-list-of-version-2"]) {
      const reading = read(name);
      expect(reading.meta).toBeNull();
      expect(reading.languages.size).toBe(0);
    }
  });

  // README, "Using it from a tree": "the reader verifies that the copy matches its own at that
  // version". The copy is compared line for line after stripping and with one blank on each side of
  // the first colon; comments and blank lines are left out, so a tree may write its own.
  test("the copy may differ from the published body in comments, blank lines and alignment", () => {
    const copy = textOf(PUBLISHED)
      .split("\n")
      .filter((line) => !line.startsWith("#"))
      .map((line) => line.replace(/\s*:\s*/, ":")) // the first colon only, with no blank around it
      .join("\n\n# a comment of the tree's own\n");
    expect(errorLines(copy)).toEqual([]);
  });

  // A body that states a version this reader holds no published body for, and not a higher one, is
  // held against the highest it knows.
  test("a first body of a version below the ones the reader knows is not the published body", () => {
    const text = textOf(PUBLISHED).replace(/^version( +):( +)3$/m, "version$1:$21");
    expect(errorLines(text)).toEqual([
      "2: the first body is not the published version-3 meta body: expected `version : 3`, found `version : 1`",
    ]);
    // The notes name the body the file was held against, never a version the reader does not know.
    const notes = notesOf(readDeclarations(text).diagnostics);
    expect(notes).toHaveLength(9);
    for (const note of notes) expect(note.text).toEndWith("version 3 reads it as free text");
  });

  // The body is chosen by the first `version` line, so a second one is itself the line that differs.
  test("a second version line is the line reported", () => {
    const stray = (text: string, line: string) => text.replace(/\nend!\n$/, `\n${line}\nend!\n`);
    expect(errorLines(stray(textOf(PUBLISHED), "version : 2"))).toEqual([
      "47: the first body is not the published version-3 meta body: expected `end!`, found `version : 2`",
    ]);
    expect(errorLines(stray(textOf(PUBLISHED_V2), "version : 3"))).toEqual([
      "47: the first body is not the published version-2 meta body: expected `end!`, found `version : 3`",
    ]);
  });

  // No `version` line: the body is held against the highest published body it knows, and the line
  // where its version should stand is the first that differs.
  test("a first body with no version line is held against the highest published body", () => {
    const text = textOf(PUBLISHED).replace(/^version +: +3\n/m, "");
    expect(errorLines(text)).toEqual([
      "2: the first body is not the published version-3 meta body: expected `version : 3`, found `file : LanguageDeclarations.txt`",
    ]);
    expect(readDeclarations(text).meta).toBeNull();
  });

  // `03` is a whole number (digits only) and is 3, so the body is held against version 3; it is not
  // the published line, which is `version : 3`.
  test("a first body that states version : 03 is held against version 3 and differs at that line", () => {
    const text = textOf(PUBLISHED).replace(/^version( +):( +)3$/m, "version$1:$203");
    expect(errorLines(text)).toEqual([
      "2: the first body is not the published version-3 meta body: expected `version : 3`, found `version : 03`",
    ]);
    expect(readDeclarations(text).meta).toBeNull();
  });

  // The reader holds one published body per version it knows and compares the first body with the
  // one of the version the body states. Version 3 differs from version 2 in the `Name` line alone,
  // so a body that states one version and carries the other's line differs at line 10.
  test("each version is held against its own published body", () => {
    expect(errorLines(textOf(PUBLISHED_V2))).toEqual([]);
    const asVersion2 = textOf(PUBLISHED).replace(/^version( +):( +)3$/m, "version$1:$22");
    expect(errorLines(asVersion2)).toEqual([
      "10: the first body is not the published version-2 meta body: expected `is : Name = LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats`, found `is : Name = one word that names a language body of the file`",
    ]);
  });

  test("a first body that never closes stops the reader", () => {
    const open = textOf(PUBLISHED).replace(/\nend!\n$/, "\n");
    expect(errorLines(open)).toEqual([
      "1: the body LanguageDeclarations has no end!: a body closes with `end!`",
    ]);
    expect(errorLines(`${open}\n${body("Structure", ...BOOKCASE)}\n`)).toEqual([
      "48: a body may not open inside another: LanguageDeclarations, opened at line 1, has no end!",
    ]);
  });
});

describe("step 2: every further body, read against the meta forms", () => {
  test("a clean body has no error", () => {
    expect(errorLines(withMeta(body("Structure", ...BOOKCASE)))).toEqual([]);
  });

  // Section 3: "A reader that knows a lower version than a body declares refuses the body". Read
  // literally that would refuse any body above version 3. This reader knows versions of the meta
  // language only; a further body's version is that language's own, and any whole number is accepted.
  test("a further body's version is its own: any whole number is accepted, and only a whole number", () => {
    const versioned = (version: string) =>
      withMeta(body("Structure", `version : ${version}`, ...BOOKCASE.slice(1)));
    expect(errorLines(versioned("7"))).toEqual([]);
    expect(errorLines(versioned("2.1"))).toEqual([
      "50: version : '2.1' is not a version: a whole number is expected",
    ]);
  });

  // `Language.version` of a further body is read from its `version` lines: the last that holds a whole
  // number, and null when it states none (a line that is not a whole number is an error and not counted).
  const versionOf = (...lines: readonly string[]) =>
    readDeclarations(withMeta(body("Structure", ...lines))).languages.get("Structure")?.version;

  test("a further body's Language.version is the number it states", () => {
    expect(versionOf("version : 7")).toBe(7);
    expect(versionOf("version : 007")).toBe(7);
  });

  test("a further body's Language.version is null when it states none", () => {
    expect(versionOf()).toBeNull();
    expect(versionOf("file : Structure.txt", "required!")).toBeNull();
    expect(versionOf("version : two")).toBeNull();
  });

  test("a further body's Language.version is the last whole number when it states several", () => {
    expect(versionOf("version : 1", "version : 2")).toBe(2);
    expect(versionOf("version : 2", "version : 1")).toBe(1);
    expect(versionOf("version : 4", "version : 2.1")).toBe(4);
  });

  // The page names the lines a body may hold and not the lines it must hold: no sentence asks for a
  // version, a file or one of required!, optional!, derived!.
  test("a body may hold nothing but its declare and its end!", () => {
    expect(errorLines(withMeta(body("Structure")))).toEqual([]);
  });

  test("once names the verb of a form of its body, wherever in the body it stands", () => {
    const text = withMeta(body("Structure", "once : made-of", ...BOOKCASE));
    expect(errorLines(text)).toEqual([]);
  });

  // "`once` says the form's verb may appear at most once": an arrow line has no verb, so no once line
  // can name an arrow form.
  test("once cannot name an arrow form", () => {
    const text = withMeta(
      body("Structure", ...BOOKCASE, "form : <from> -> <to>", "does : a move", "once : ->"),
    );
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "once : ->")}: once : -> names no form of Structure: the verb of one of its forms is expected (made-of)`,
    ]);
  });

  // The page does not say that a body states order, comment or a class name only once, and the meta
  // body puts no `once` on its own forms. The reader accepts the repeat and the last line stands.
  test("where a body states order, comment or a class twice, the last line stands", () => {
    const text = withMeta(
      body(
        "Structure",
        ...BOOKCASE,
        "order : strict",
        "order : free",
        "comment : //",
        "comment : ;",
        "is : material = glass | brass",
        "example : made-of : brass",
      ),
    );
    const reading = readDeclarations(text);
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(reading.languages.get("Structure")?.order).toBe("free");
    expect(reading.languages.get("Structure")?.commentPrefix).toBe(";");
  });

  // Section 2, item 1: the prefix is the one "declared for that language". A declarations file is
  // written in the meta language, so its comments take the meta body's prefix; the `comment` line of
  // a further body is for that language's programs.
  test("a further body's comment prefix is for its programs, not for the declarations file", () => {
    const text = withMeta(
      body("Structure", ...BOOKCASE, "comment : //", "# still a comment", "// no longer one"),
    );
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "// no longer one")}: not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

  test("a form with no does is reported even where the body never closes", () => {
    expect(errorLines(withMeta("declare : Structure\nform : shelf : <label>"))).toEqual([
      "49: the body Structure has no end!: a body closes with `end!`",
      "50: this form is not followed by its does: the next line must be `does : <text>`",
    ]);
  });

  test("outside a body a line of no shape is reported as that, once", () => {
    const text = withMeta(body("Structure", ...BOOKCASE), "stray words");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "stray words")}: not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

  // The body opens at line 49, so its lines are 50 to 55; the does at line 52 is not empty.
  test("an empty argument is refused by the meta form, and reported once", () => {
    const text = withMeta(
      body("Structure", "version :", "form :", "does : nothing", "example :", "once :", "sees :"),
    );
    expect(errorLines(text)).toEqual([
      "50: <number> is empty: a token is never empty (form `version : <number>`)",
      "51: <production> is empty: a token is never empty (form `form : <production>`)",
      "53: <line> is empty: a token is never empty (form `example : <line>`)",
      "54: <verb> is empty: a token is never empty (form `once : <verb>`)",
      "55: <Name> is empty: a token is never empty (form `sees : <Name>`)",
    ]);
  });

  test("a file with CRLF line ends, or a byte-order mark at its top, reads the same", () => {
    const text = withMeta(body("Structure", ...BOOKCASE));
    expect(errorLines(text.replaceAll("\n", "\r\n"))).toEqual([]);
    expect(errorLines(`\uFEFF${text}`)).toEqual([]);
  });

  test("of two bodies of one name the first stands", () => {
    expect(read("duplicate-name").languages.get("Structure")?.forms[0]?.text).toBe(
      "made-of : <material>",
    );
  });
});

describe("names (version 3)", () => {
  // Version 2 listed, in the meta body, the names a file could declare: the languages of the first
  // tree. Version 3 does not. A new language is a new body at the end of the file (section 6), and
  // its name is the word on its declare line.
  test("a file declares a language of any name", () => {
    const reading = readDeclarations(withMeta(body("Recipes", ...BOOKCASE)));
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations", "Recipes"]);
    expect(reading.meta?.version).toBe(3);
  });

  test("the same body under version 2 is refused by the list", () => {
    const text = withMetaV2(body("Recipes", ...BOOKCASE));
    expect(readDeclarations(textOf(PUBLISHED_V2)).meta?.version).toBe(2);
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "declare : Recipes")}: 'Recipes' is not one of: LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats (form \`declare : <Name>\`)`,
    ]);
  });

  // Step 2: a name is one word of letters and digits that begins with a letter. Under version 2 the
  // list has already refused the name, and the rule adds nothing to that.
  test("a name is one word of letters and digits that begins with a letter", () => {
    const expected = "one word of letters and digits, beginning with a letter, is expected";
    for (const name of ["Cook Book", "->", "end!", "42", "a:b", "A|B", "Cook-Book"]) {
      const text = withMeta(body(name, ...BOOKCASE));
      expect(errorLines(text)).toEqual([
        `${lineOf(text, `declare : ${name}`)}: declare : '${name}' is not a name: ${expected}`,
      ]);
    }
    expect(errorLines(withMeta(body("Recipes2", ...BOOKCASE)))).toEqual([]);
    const listed = withMetaV2(body("Cook Book", ...BOOKCASE));
    expect(errorLines(listed)).toEqual([
      `${lineOf(listed, "declare : Cook Book")}: 'Cook Book' is not one of: LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats (form \`declare : <Name>\`)`,
    ]);
  });

  test("a name is declared once, the meta language's included", () => {
    const twice = withMeta(body("Recipes"), body("Recipes"));
    expect(errorLines(twice).map((error) => error.replace(/^[0-9]+: /, ""))).toEqual([
      `Recipes is declared twice (first at line ${lineOf(twice, "declare : Recipes")}): a name is declared once`,
    ]);
    const again = withMeta(body("LanguageDeclarations"));
    expect(errorLines(again).map((error) => error.replace(/^[0-9]+: /, ""))).toEqual([
      "LanguageDeclarations is declared twice (first at line 1): a name is declared once",
    ]);
  });

  test("sees names a body of the file, whatever its name, above or below", () => {
    expect(errorLines(withMeta(body("Recipes", "sees : Pantry"), body("Pantry")))).toEqual([]);
    // Above: the name is declared before the line that sees it, and the meta body is above every body.
    expect(errorLines(withMeta(body("Pantry"), body("Recipes", "sees : Pantry")))).toEqual([]);
    expect(errorLines(withMeta(body("Recipes", "sees : LanguageDeclarations")))).toEqual([]);
    const unseen = withMeta(body("Recipes", "sees : Cellar"), body("Pantry"));
    expect(errorLines(unseen)).toEqual([
      `${lineOf(unseen, "sees : Cellar")}: sees : Cellar names no body of this file: a declared name is expected (LanguageDeclarations, Recipes, Pantry)`,
    ]);
  });
});

describe("productions (section 3)", () => {
  const withForms = (...lines: readonly string[]) =>
    withMeta(body("Structure", ...BOOKCASE, ...lines));

  // "A production is one of the three line shapes above": a row of placeholders is none of them.
  test("a production that is none of the three shapes is an error, and its example is not checked", () => {
    const text = withForms("form : <row> <seat>", "does : one seat", "example : A 12");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "form : <row> <seat>")}: this production has none of the three line shapes: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

  // "A production is one of the three line shapes above with placeholders in angle brackets": the
  // page does not keep placeholders out of the verb, and writes the shapes themselves as `<verb>!`
  // and `<verb> : <argument>`. A verb is literal text and placeholders like an argument, so a literal
  // verb must be the same word and a placeholder verb is filled from its class.
  test("a placeholder may stand as the verb of a bang or colon production", () => {
    const forms = [
      "is : label = top | middle | bottom",
      "form : <label>!",
      "does : a shelf",
      "form : <label> : <text>",
      "does : a note on a shelf",
    ];
    expect(errorLines(withForms(...forms, "example : middle : dusty"))).toEqual([]);
    const text = withForms(...forms.slice(0, 3), "example : attic!");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : attic!")}: the example does not fit the form above it, \`<label>!\`: 'attic' is not one of: top | middle | bottom`,
    ]);
  });

  test("a colon production whose argument has no placeholder and holds | lists literal alternatives", () => {
    const forms = ["form : door : open | shut", "does : the state of the glass door"];
    expect(errorLines(withForms(...forms, "example : door : shut"))).toEqual([]);
    const text = withForms(...forms, "example : door : ajar");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : door : ajar")}: the example does not fit the form above it, \`door : open | shut\`: 'ajar' is not one of: open | shut`,
    ]);
  });

  test("with a placeholder in it, an argument that holds | is literal text and placeholders", () => {
    const forms = ["form : filed : <material> | unknown", "does : a material, then the word"];
    expect(errorLines(withForms(...forms, "example : filed : oak | unknown"))).toEqual([]);
    const text = withForms(...forms, "example : filed : unknown");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : filed : unknown")}: the example does not fit the form above it, \`filed : <material> | unknown\`: 'unknown' does not fit \`<material> | unknown\``,
    ]);
  });

  // The rule above is stated for a colon production's argument. The page has no sentence on | in a
  // production at all, so on an arrow side the reader reads it as the literal text it is.
  test("on an arrow side | is literal text", () => {
    const forms = ["form : in | out -> <label>", "does : a book moved in or out"];
    expect(errorLines(withForms(...forms, "example : in | out -> top"))).toEqual([]);
    const text = withForms(...forms, "example : in -> top");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : in -> top")}: the example does not fit the form above it, \`in | out -> <label>\`: 'in' does not fit \`in | out\``,
    ]);
  });

  test("an empty argument in a colon production is legal and matches only an empty argument", () => {
    const forms = ["form : dust :", "does : dust every shelf"];
    expect(errorLines(withForms(...forms, "example : dust :"))).toEqual([]);
    const text = withForms(...forms, "example : dust : twice");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : dust : twice")}: the example does not fit the form above it, \`dust :\`: 'twice' is not expected: nothing follows the colon of this form`,
    ]);
  });

  // See test/lines.test.ts: with nothing between two placeholders the first takes nothing, and a
  // token is never empty. The form is accepted as declared and no line can be of it.
  test("a production with two placeholders and nothing between them fits no line", () => {
    const text = withForms("form : tag : <key><value>", "does : a tag", "example : tag : ab");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : tag : ab")}: the example does not fit the form above it, \`tag : <key><value>\`: <key> is empty: a token is never empty`,
    ]);
  });
});

describe("token classes (section 3)", () => {
  // The page says what a listed class and a sentence admit, and that a placeholder with no `is` line
  // is unbound; the published meta body uses nine. The reader takes one as free text and notes it, so that a
  // mistyped class name shows: here `materail` lets marble through, and the note says why.
  test("a mistyped class name is unbound: free text, and a note", () => {
    const text = withMeta(
      body(
        "Structure",
        ...BOOKCASE.slice(0, 4),
        "form : made-of : <materail>",
        "does : what the bookcase is built from",
        "example : made-of : marble",
      ),
    );
    const reading = readDeclarations(text);
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(notesOf(reading.diagnostics).filter(({ line }) => line > 47)).toEqual([
      {
        line: lineOf(text, "form : made-of : <materail>"),
        severity: "note",
        text: "<materail> has no `is` line in Structure or in LanguageDeclarations; version 3 reads it as free text",
      },
    ]);
  });

  test("an unbound placeholder used by two forms is noted once, at the first", () => {
    const text = withMeta(
      body(
        "Structure",
        "form : lent-to : <who>",
        "does : the borrower",
        "form : bought-by : <who>",
        "does : the buyer",
      ),
    );
    expect(notesOf(readDeclarations(text).diagnostics).filter(({ line }) => line > 47)).toEqual([
      {
        line: lineOf(text, "form : lent-to : <who>"),
        severity: "note",
        text: "<who> has no `is` line in Structure or in LanguageDeclarations; version 3 reads it as free text",
      },
    ]);
  });

  // The version-2 meta body is used here because it holds a listed class, `Name`; version 3 holds none.
  test("a placeholder resolves in its own body first, then in the meta body", () => {
    const form = ["form : named : <Name>", "does : takes the Name class"];
    const viaMeta = withMetaV2(body("Structure", ...BOOKCASE, ...form, "example : named : Ada"));
    expect(errorLines(viaMeta)).toEqual([
      `${lineOf(viaMeta, "example : named : Ada")}: the example does not fit the form above it, \`named : <Name>\`: 'Ada' is not one of: LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats`,
    ]);
    const own = "is : Name = Ada | Grace";
    expect(
      errorLines(withMetaV2(body("Structure", ...BOOKCASE, own, ...form, "example : named : Ada"))),
    ).toEqual([]);
  });

  // A placeholder resolves in its own body, else in the meta body, else it is unbound: a class that
  // another body declares is not seen.
  test("a class declared in another body is not seen", () => {
    const text = withMeta(
      body("Structure", ...BOOKCASE),
      body(
        "Contents",
        "form : stands-on : <material>",
        "does : what the shelf under the books is made of",
        "example : stands-on : marble",
      ),
    );
    const reading = readDeclarations(text);
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(notesOf(reading.diagnostics).filter(({ line }) => line > 47)).toEqual([
      {
        line: lineOf(text, "form : stands-on : <material>"),
        severity: "note",
        text: "<material> has no `is` line in Contents or in LanguageDeclarations; version 3 reads it as free text",
      },
    ]);
  });

  test("a sentence admits any text that is not empty", () => {
    const forms = ["is : label = a short name for a shelf", "form : shelf : <label>", "does : x"];
    expect(
      errorLines(withMeta(body("Structure", ...BOOKCASE, ...forms, "example : shelf : top left"))),
    ).toEqual([]);
    const text = withMeta(body("Structure", ...BOOKCASE, ...forms, "example : shelf :"));
    expect(errorLines(text).filter((error) => error.includes("the example"))).toEqual([
      `${lineOf(text, "example : shelf :")}: the example does not fit the form above it, \`shelf : <label>\`: <label> is empty: a token is never empty`,
    ]);
  });
});

describe("examples (section 3)", () => {
  // "`example` gives a legal line for the form just declared": the example is checked against that
  // form and no other, even where a form declared earlier would read the same line first in a program.
  test("an example is checked against the form declared just above it, and only that form", () => {
    const text = withMeta(
      body(
        "Structure",
        ...BOOKCASE,
        "form : note : <text>",
        "does : a note in free words",
        "form : note : short | long",
        "does : a note of a fixed kind",
        "example : note : short",
      ),
    );
    expect(errorLines(text)).toEqual([]);
  });

  test("an example before any form has nothing to be checked against", () => {
    const text = withMeta(body("Structure", "example : made-of : oak", ...BOOKCASE));
    expect(errorLines(text)).toEqual([
      "50: this example has no form above it: an example gives a line for the form just declared",
    ]);
  });

  test("an example must itself be a legal line", () => {
    const text = withMeta(body("Structure", ...BOOKCASE, "example : made of oak"));
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : made of oak")}: the example is not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });
});
