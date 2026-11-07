import { describe, expect, test } from "bun:test";

import { readDeclarations, readGrammar } from "../src/declarations.ts";
import {
  PUBLISHED,
  PUBLISHED_V2,
  body,
  errorLines,
  errorsOf,
  fixture,
  lineOf,
  notesOf,
  textOf,
  withMeta,
  withMetaV2,
} from "./support.ts";

const read = (name: string) => readDeclarations(textOf(fixture(`declarations/${name}.typify`)));

const BOOKCASE = [
  "version : 1",
  "file : Structure.typify",
  "required!",
  "is : material = oak | pine | steel",
  "form : made-of : <material>",
  "does : what the bookcase is built from",
];

describe("the library-owned canonical grammars", () => {
  test.each([
    [2, PUBLISHED_V2],
    [3, PUBLISHED],
  ] as const)("version %d validates itself", (version, path) => {
    const reading = readGrammar(textOf(path), version);
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(reading.meta?.version).toBe(version);
    expect(reading.meta?.forms).toHaveLength(16);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations"]);
    expect(notesOf(reading.diagnostics)).toHaveLength(9);
    for (const note of notesOf(reading.diagnostics)) {
      expect(note.text).toEndWith(`version ${version} reads it as free text`);
    }
  });

  test("a broken grammar does not produce a usable meta language", () => {
    const text = textOf(PUBLISHED).replace(/^does +: +open a language body.*\n/m, "");
    const reading = readGrammar(text, 3);
    expect(reading.meta).toBeNull();
    expect(reading.languages.size).toBe(0);
    expect(errorsOf(reading.diagnostics)).toEqual([
      {
        line: lineOf(text, "declare : <Name>"),
        severity: "error",
        text: "this form is not followed by its does: the next line must be `does : <text>`",
      },
    ]);
  });

  test("grammar resources must match their selected version and contain exactly one body", () => {
    expect(readGrammar(textOf(PUBLISHED), 2).meta).toBeNull();
    expect(readGrammar(textOf(PUBLISHED), 4).meta).toBeNull();
    expect(readGrammar(`${textOf(PUBLISHED)}\n${body("Structure")}`, 3).meta).toBeNull();
    const repeated = textOf(PUBLISHED).replace(/\nend!\n$/, "\nversion : 3\nend!\n");
    expect(readGrammar(repeated, 3).meta).toBeNull();
    const missing = textOf(PUBLISHED).replace(/^version +: +3\n/m, "");
    expect(readGrammar(missing, 3).meta).toBeNull();
  });

  test("a grammar that never closes or opens with another name fails", () => {
    expect(readGrammar(textOf(PUBLISHED).replace(/\nend!\n$/, "\n"), 3).meta).toBeNull();
    expect(readGrammar("declare : Structure\nend!\n", 3).meta).toBeNull();
  });

  test("canonical self-validation honors its own comment prefix", () => {
    const text = textOf(PUBLISHED).replace(/^comment +: +#$/m, "comment : //");
    const withWrongComment = `${text.replace(/\nend!\n$/, "\n# no longer a comment\nend!\n")}`;
    expect(
      errorsOf(readGrammar(withWrongComment, 3).diagnostics).some((d) =>
        d.text.startsWith("not a legal line"),
      ),
    ).toBe(true);
  });
});

describe("the compact grammar selector", () => {
  test.each([2, 3])("selects version %d without copying the grammar", (version) => {
    const reading = readDeclarations(`typify : ${version}\n\n${body("Structure", ...BOOKCASE)}\n`);
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(reading.meta?.version).toBe(version);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations", "Structure"]);
    expect(reading.diagnostics).toEqual([]);
    expect(reading.languages.get("Structure")?.line).toBe(3);
  });

  test("a selector alone has no application languages or inherited grammar notes", () => {
    const reading = readDeclarations("typify : 3\n");
    expect(reading.meta?.version).toBe(3);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations"]);
    expect(reading.diagnostics).toEqual([]);
  });

  test("the selector is the first nonblank, non-comment line", () => {
    const reading = readDeclarations(
      `\uFEFF# a note\r\n\r\n  typify : 3  \r\n${body("Structure", ...BOOKCASE).replaceAll("\n", "\r\n")}\r\n`,
    );
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect(reading.languages.get("Structure")?.line).toBe(4);
  });

  test.each(["", "version : 3", "declare : LanguageDeclarations\nend!", "required!"])(
    "rejects a missing selector: %s",
    (text) => {
      const reading = readDeclarations(text);
      expect(reading.meta).toBeNull();
      expect(reading.languages.size).toBe(0);
      expect(errorsOf(reading.diagnostics)).toHaveLength(1);
      expect(reading.diagnostics[0]?.text).toContain("typify : <version>");
    },
  );

  test.each(["", "1", "4", "2.1", "three"])(
    "refuses unsupported selector %s before reading bodies",
    (version) => {
      const reading = readDeclarations(
        `typify : ${version}\n${body("Structure", "colour : red")}\n`,
      );
      expect(reading.meta).toBeNull();
      expect(reading.languages.size).toBe(0);
      expect(errorsOf(reading.diagnostics)).toHaveLength(1);
      expect(reading.diagnostics[0]?.line).toBe(1);
      expect(reading.diagnostics[0]?.text).toContain("unsupported typify version");
    },
  );

  test("rejects copied meta blocks instead of treating them as consumer input", () => {
    expect(readDeclarations(textOf(PUBLISHED)).meta).toBeNull();
    expect(readDeclarations(textOf(PUBLISHED_V2)).meta).toBeNull();
    expect(errorsOf(read("redeclared-meta").diagnostics)).toHaveLength(1);
  });
});

describe("compact consumer fixtures", () => {
  test("the version-2 fixture retains only its own notes and language order", () => {
    const reading = read("valid");
    const text = textOf(fixture("declarations/valid.typify"));
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect([...reading.languages.keys()]).toEqual([
      "LanguageDeclarations",
      "Contents",
      "Structure",
    ]);
    expect(notesOf(reading.diagnostics).map(({ line, text }) => `${line}: ${text}`)).toEqual([
      `${lineOf(text, "form : shelf : <label>")}: <label> has no \`is\` line in Contents or in LanguageDeclarations; version 2 reads it as free text`,
      `${lineOf(text, "form : loan : <title> to <reader>")}: <reader> has no \`is\` line in Contents or in LanguageDeclarations; version 2 reads it as free text`,
    ]);
    expect(reading.languages.get("Contents")?.commentPrefix).toBe("//");
    expect(reading.languages.get("Contents")?.order).toBe("strict");
    expect([...(reading.languages.get("Contents")?.once ?? [])]).toEqual(["shelf"]);
  });

  test("a version-3 fixture declares its own name", () => {
    expect(errorsOf(read("version-3-own-name").diagnostics)).toEqual([]);
    expect([...read("version-3-own-name").languages.keys()]).toEqual([
      "LanguageDeclarations",
      "Pantry2",
    ]);
  });

  const classes: readonly (readonly [string, RegExp])[] = [
    ["no-shape", /^not a legal line/],
    ["outside-body", /^this line is outside any body/],
    ["missing-end", /^the body Structure has no end!/],
    ["nested-declare", /^a body may not open inside another/],
    ["duplicate-name", /^Structure is declared twice/],
    ["form-without-does", /^this form is not followed by its does/],
    ["form-with-two-does", /^this does has no form directly above it/],
    ["form-between-form-and-does", /^this form is not followed by its does/],
    ["undeclared-line", /^LanguageDeclarations declares no form for this line/],
    ["example-mismatch", /^the example does not fit the form above it/],
    ["once-unknown-verb", /^once : colour names no form/],
    ["sees-unknown-body", /^sees : OrderOfSessions names no body/],
    ["name-outside-list", /^'Recipes' is not one of:/],
    ["grammar-version-4", /^unsupported typify version '4'/],
  ];
  test.each(classes)("%s has exactly one located error", (name, message) => {
    const reading = read(name);
    const errors = errorsOf(reading.diagnostics);
    expect(errors).toHaveLength(1);
    expect(errors[0]?.text).toMatch(message);
    const text = textOf(fixture(`declarations/${name}.typify`));
    expect(errors[0]?.line).toBeGreaterThan(0);
    expect(errors[0]!.line).toBeLessThanOrEqual(text.split("\n").length);
  });
});

describe("step 2: every further body, read against the meta forms", () => {
  test("a clean body has no error", () => {
    expect(errorLines(withMeta(body("Structure", ...BOOKCASE)))).toEqual([]);
  });

  test("a further body's version is its own: any whole number is accepted, and only a whole number", () => {
    const versioned = (version: string) =>
      withMeta(body("Structure", `version : ${version}`, ...BOOKCASE.slice(1)));
    expect(errorLines(versioned("7"))).toEqual([]);
    const invalid = versioned("2.1");
    expect(errorLines(invalid)).toEqual([
      `${lineOf(invalid, "version : 2.1")}: version : '2.1' is not a version: a whole number is expected`,
    ]);
  });

  const versionOf = (...lines: readonly string[]) =>
    readDeclarations(withMeta(body("Structure", ...lines))).languages.get("Structure")?.version;

  test("a further body's Language.version is the number it states", () => {
    expect(versionOf("version : 7")).toBe(7);
    expect(versionOf("version : 007")).toBe(7);
  });

  test("a further body's Language.version is null when it states none", () => {
    expect(versionOf()).toBeNull();
    expect(versionOf("file : Structure.typify", "required!")).toBeNull();
    expect(versionOf("version : two")).toBeNull();
  });

  test("a further body's Language.version is the last whole number when it states several", () => {
    expect(versionOf("version : 1", "version : 2")).toBe(2);
    expect(versionOf("version : 2", "version : 1")).toBe(1);
    expect(versionOf("version : 4", "version : 2.1")).toBe(4);
  });

  test("a body may hold nothing but its declare and its end!", () => {
    expect(errorLines(withMeta(body("Structure")))).toEqual([]);
  });

  test("once names the verb of a form of its body, wherever in the body it stands", () => {
    const text = withMeta(body("Structure", "once : made-of", ...BOOKCASE));
    expect(errorLines(text)).toEqual([]);
  });

  test("once cannot name an arrow form", () => {
    const text = withMeta(
      body("Structure", ...BOOKCASE, "form : <from> -> <to>", "does : a move", "once : ->"),
    );
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "once : ->")}: once : -> names no form of Structure: the verb of one of its forms is expected (made-of)`,
    ]);
  });

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

  test("a further body's comment prefix is for its programs, not for the declarations file", () => {
    const text = withMeta(
      body("Structure", ...BOOKCASE, "comment : //", "# still a comment", "// no longer one"),
    );
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "// no longer one")}: not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

  test("a form with no does is reported even where the body never closes", () => {
    const text = withMeta("declare : Structure\nform : shelf : <label>");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "declare : Structure")}: the body Structure has no end!: a body closes with \`end!\``,
      `${lineOf(text, "form : shelf")}: this form is not followed by its does: the next line must be \`does : <text>\``,
    ]);
  });

  test("outside a body a line of no shape is reported as that, once", () => {
    const text = withMeta(body("Structure", ...BOOKCASE), "stray words");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "stray words")}: not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

  test("an empty argument is refused by the meta form, and reported once", () => {
    const text = withMeta(
      body("Structure", "version :", "form :", "does : nothing", "example :", "once :", "sees :"),
    );
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "version :")}: <number> is empty: a token is never empty (form \`version : <number>\`)`,
      `${lineOf(text, "form :")}: <production> is empty: a token is never empty (form \`form : <production>\`)`,
      `${lineOf(text, "example :")}: <line> is empty: a token is never empty (form \`example : <line>\`)`,
      `${lineOf(text, "once :")}: <verb> is empty: a token is never empty (form \`once : <verb>\`)`,
      `${lineOf(text, "sees :")}: <Name> is empty: a token is never empty (form \`sees : <Name>\`)`,
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
  test("a file declares a language of any name", () => {
    const reading = readDeclarations(withMeta(body("Recipes", ...BOOKCASE)));
    expect(errorsOf(reading.diagnostics)).toEqual([]);
    expect([...reading.languages.keys()]).toEqual(["LanguageDeclarations", "Recipes"]);
    expect(reading.meta?.version).toBe(3);
  });

  test("the same body under version 2 is refused by the list", () => {
    const text = withMetaV2(body("Recipes", ...BOOKCASE));
    expect(readGrammar(textOf(PUBLISHED_V2), 2).meta?.version).toBe(2);
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "declare : Recipes")}: 'Recipes' is not one of: LanguageDeclarations | OrderOfSessions | Workdir | Structure | Contents | Seats (form \`declare : <Name>\`)`,
    ]);
  });

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

  test("a production that is none of the three shapes is an error, and its example is not checked", () => {
    const text = withForms("form : <row> <seat>", "does : one seat", "example : A 12");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "form : <row> <seat>")}: this production has none of the three line shapes: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });

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

  test("a production with two placeholders and nothing between them fits no line", () => {
    const text = withForms("form : tag : <key><value>", "does : a tag", "example : tag : ab");
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : tag : ab")}: the example does not fit the form above it, \`tag : <key><value>\`: <key> is empty: a token is never empty`,
    ]);
  });
});

describe("token classes (section 3)", () => {
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
    expect(notesOf(reading.diagnostics).filter(({ line }) => line > 2)).toEqual([
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
    expect(notesOf(readDeclarations(text).diagnostics).filter(({ line }) => line > 2)).toEqual([
      {
        line: lineOf(text, "form : lent-to : <who>"),
        severity: "note",
        text: "<who> has no `is` line in Structure or in LanguageDeclarations; version 3 reads it as free text",
      },
    ]);
  });

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
    expect(notesOf(reading.diagnostics).filter(({ line }) => line > 2)).toEqual([
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
      `${lineOf(text, "example : made-of")}: this example has no form above it: an example gives a line for the form just declared`,
    ]);
  });

  test("an example must itself be a legal line", () => {
    const text = withMeta(body("Structure", ...BOOKCASE, "example : made of oak"));
    expect(errorLines(text)).toEqual([
      `${lineOf(text, "example : made of oak")}: the example is not a legal line: expected a bang line \`verb!\`, a colon line \`verb : argument\` or an arrow line \`left -> right\``,
    ]);
  });
});
