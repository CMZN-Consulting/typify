# Typify

A small, self-describing metalanguage for declaring and validating line-oriented languages. Its declaration grammar is itself written in Typify and shipped inside the package. Applications select a grammar version and declare their own languages; they do not copy the grammar into their files.

Typify reads declarations and checks programs. A consuming adapter assigns meaning, checks resources and permissions, and acts only after all its inputs pass validation.

## The name

Berger and Luckmann call it typification: the operation that turns a unique act into a repeatable kind, so that "there he goes again" becomes "this is how things are done", and a newcomer can perform it without the biography behind it. A `form` line is exactly that. It subsumes a recurring act under an anonymous, repeatable category that anyone occupying the seat can perform, and the `does` line beside it is the first level of legitimation, the meaning built into the vocabulary itself. What is sedimented behind a form, the recipe that makes it happen, is written once by a fluent hand and never by the one who uses the form. The reader typifies; hence the name.

## Install

Use Bun 1.4.4 or newer:

```sh
bun add 'git+https://github.com/CMZN-Consulting/typify.git#main'
```

Bun installs the package in `node_modules` and records the resolved Git revision in `bun.lock`. Commit the lockfile and use `bun install --frozen-lockfile` to reproduce that revision. A dependency can also name a full commit SHA instead of `main` when updates must be explicit.

The public import is `typify`. The package ships its TypeScript source and versioned declaration grammars, with no runtime package dependencies. Runtime and test I/O use Bun APIs. The CLI uses `process.exitCode` because Bun does not provide a separate native API for setting an exit status.

## Declare a language

[The complete catalog example](examples/catalog/LanguageDeclarations.typify) contains only a version selector and its own language:

```text
 typify : 3

 declare : Catalog
 version : 1
 file : catalog.typify
 required!
 is : category = book | journal
 is : title = a nonempty title
 form : item : <category> = <title>
 does : describe an item by its category and title
 example : item : book = Algebra
 end!
```

Leading and trailing whitespace is insignificant. `typify : 3` selects the bundled version-3 declaration grammar; `version : 1` states the application's Catalog language version. The adapter decides which application language versions it supports.

A program in that language is plain text:

```text
item : book = Algebra
item : journal = Market Research
```

Use `.typify` for declaration and program files. The reader takes explicit paths; a suffix does not select a language.

## The shape

There are three line shapes:

- `verb : argument`, split at the first colon;
- `verb!`, one word ending in an exclamation mark;
- `left -> right`, split at the first arrow when the line has no colon.

A language body opens with `declare : Name` and closes with `end!`. `form` declares a production, `does` describes its meaning, and `example` gives a local test vector for that form. `is` binds token classes. A domain containing `|` lists literal alternatives; a domain without `|` describes free text, even when that description is one word. Unknown classes also accept nonempty text and produce a note.

`comment` sets a program's comment prefix, `once` limits a form's verb to one use, and `order : strict` requires declaration order. `file`, `required!`, `optional!`, `derived!`, `absent` and `sees` describe tree relationships; this reader checks their declaration syntax but does not implement tree discovery or cross-program name resolution. The exported declaration model currently retains the information used by program validation rather than all descriptive metadata.

The first matching form wins. Captures use the leftmost, shortest split at the next literal, without backtracking. Examples are checked against their associated form; example reachability through comment handling and first-match dispatch is not currently checked.

## Use the library

```typescript
import { isError, parseProgram, readDeclarations } from "typify";

const declarations = readDeclarations(await Bun.file("LanguageDeclarations.typify").text());
if (declarations.meta === null || declarations.diagnostics.some(isError)) {
  throw new Error("Invalid declarations");
}

const catalog = declarations.languages.get("Catalog");
if (catalog === undefined) throw new Error("Catalog is not declared");

const program = parseProgram(
  await Bun.file("catalog.typify").text(),
  catalog,
  declarations.meta,
  "catalog.typify",
);
if (!program.valid) throw new Error("Invalid catalog");

for (const line of program.lines) {
  console.log(line.line, line.formIndex, line.captures);
}
```

A successful parsed program includes original lines, source positions, matched forms and ordered captures. Repeated capture names and inner spacing remain. A failed program exposes diagnostics without partial parsed records. `readProgram` retains the diagnostics-only interface.

Captures remain data. Passing an argument as an argv element avoids shell interpolation, but the receiving command can still interpret that argument. The adapter owns command selection, argument validation, permissions, resource limits and the effects of combining valid actions. Syntax validation alone does not establish that an action is safe.

## Command line

From a checkout:

```sh
bun install --frozen-lockfile
bun src/cli.ts check examples/catalog/LanguageDeclarations.typify
bun src/cli.ts check-program examples/catalog/LanguageDeclarations.typify Catalog examples/catalog/catalog.typify
```

Diagnostics name the file and line. Exit status is 0 when validation succeeds, 1 for validation errors and 2 for usage or file-reading errors. Notes do not fail validation. Invalid declarations are reported before a program file is opened.

## Grammar and versions

The canonical definitions live in `src/grammar/v2.typify` and `src/grammar/v3.typify`. The reader loads those package resources and validates each through its own declared forms. Their versioned snapshots support the corresponding language rules; there is no second hardcoded grammar body in TypeScript.

Version 3 allows applications to declare their own language names. Version 2 retains its historical closed name list. Existing matching and token rules remain versioned. Version 0.5 changes the declaration-file container: migrate old files by replacing the copied first meta body with `typify : 2` or `typify : 3`. Copied meta bodies are no longer accepted as application declaration files.

Tree walking, reference resolution, derived programs, execution and grammar-constrained generation are future design work. [ARCHITECTURE.md](ARCHITECTURE.md) defines the implemented boundary.

## Develop

```sh
bun test
bun run typecheck
bun run format:check
bun run check
```

Repository TypeScript source has no comments. Explanations belong in documentation, and synthetic comment strings in tests exercise the language's comment handling.
