# typify

A small language for declaring languages, written so that its files carry their own grammar. The first block of a `LanguageDeclarations.txt` declares the language the file is written in; every block after it declares a language that the programs of a tree are written in. A reader validates the first block against itself, reads the rest through it, checks every program in the tree against the forms its language declares, and only then acts.

It exists for one reason: so that a person, or a model, can say what should happen in a handful of declared forms, in words, and never write the shell that does it.

## The name

Berger and Luckmann call it typification: the operation that turns a unique act into a repeatable kind, so that "there he goes again" becomes "this is how things are done", and a newcomer can perform it without the biography behind it. A `form` line is exactly that. It subsumes a recurring act under an anonymous, repeatable category that anyone occupying the seat can perform, and the `does` line beside it is the first level of legitimation, the meaning built into the vocabulary itself. What is sedimented behind a form, the recipe that makes it happen, is written once by a fluent hand and never by the one who uses the form. The reader typifies; hence the name.

## The shape

Three line shapes and nothing else, after leading and trailing blanks are stripped:

- a colon line, `verb : argument`, split at the first colon;
- a bang line, `verb!`, one word ending in an exclamation mark;
- an arrow line, `left -> right`, split at the first arrow.

A language is a body between `declare : Name` and `end!`. Inside it, `version` states the language's version, `file` says where its programs live, `required!`, `optional!` or `derived!` says whether a program must exist, `comment` sets the comment prefix, `is` binds a token class, `form` declares a legal line with `does` giving its meaning and `example` a legal instance, `once`, `order` and `sees` constrain programs, and `absent` says what a missing optional program means. That is the whole meta language. It is declared, in itself, in [`LanguageDeclarations.txt`](LanguageDeclarations.txt), and [`ARCHITECTURE.md`](ARCHITECTURE.md) says in plain words how a reader reads it.

A declared language, trimmed:

```text
declare : Structure
version : 2
file    : <Session>/<window>/Structure.txt
required!
order   : strict
sees    : OrderOfSessions
is      : arrangement = stack | row | tiled
form    : layout : <arrangement>
does    : how the panes of this window are arranged; stack when absent
example : layout : stack
form    : seat : <seat>
does    : one pane, named <seat>; pane indexes follow the order of these lines from 0
example : seat : L0-seat
end!
```

A program in it is a few such lines in a file, and it reads as a sentence a stranger can act on.

## Interface, not interpreter

The reader is not an interpreter of the programs, and calling it one hides the property that matters. It is an interface: a closed surface of verbs. Each form stands for a recipe, and the recipe bottoms out in a fixed, small set of primitives that a host runs. Whoever writes a program sees the forms and nothing behind them; the shell, where there is one, is reachable only through a form that was declared, reviewed and committed.

Three things follow, and they are the reason the language exists for models that write plain text well and shell badly:

1. **The worst a program can do is the worst declared form.** The allow list is the declarations file, not a filter over free text.
2. **A program's text is data, never code.** A token class with a listed domain admits only its words; a free-text argument is handed to its primitive as one argument, never interpolated into a command line. Injection is not filtered out; it has no path in.
3. **A grammar falls out of the declarations.** The forms are regular enough that a reader can emit a formal grammar for them, and a sampler that decodes under that grammar cannot produce an undeclared line at all. An undeclared action is not caught after the fact; it is unsamplable.

Prior art, so that nobody mistakes this for new: Cucumber's step definitions bind English sentence patterns to code, and rot exactly when the vocabulary is left open; Inform 7 compiles English sentences; Ansible's modules are declared recipes that a host runs on its targets; Lean 4's `macro_rules` expand declared syntax into a smaller core; Attempto Controlled English translates a controlled natural language into logic; and grammar-constrained decoding is how a sampler is held to a grammar. typify takes from each the part that fits a tree of plain-text programs read by people and models alike.

## Using it from a tree

A tree that uses typify carries its own `LanguageDeclarations.txt`. Its first body is this repository's meta body at a declared version, copied whole, so that the file stays readable by a reader that has never seen this repository; the reader verifies that the copy matches its own at that version, and refuses a version higher than it knows. The tree's languages follow, and its programs live where their `file` templates say.

The first tree to use it stands up a desk of human and model seats under tmux: its languages declare sessions, windows, panes and mailboxes, and its reader is a shell script written to `ARCHITECTURE.md`.

## The reader

The reader this repository ships is a Bun package with no runtime dependencies, taken as a pinned dependency from this repository and moved deliberately, never by a pull at run time. It reads versions 2 and 3 of the meta language. It validates and does nothing else: it reads the files it is given and prints what it finds, and it never executes, creates or writes anything.

```text
bun src/cli.ts check DECLARATIONS_FILE
bun src/cli.ts check-program DECLARATIONS_FILE LANGUAGE PROGRAM_FILE
```

`check` reads a declarations file as steps 1 and 2 of [`ARCHITECTURE.md`](ARCHITECTURE.md) say. The first body must declare `LanguageDeclarations` at version 2 or 3, pass its own forms and equal the reader's copy of the published meta body of that version; if it does not, nothing else is read. Every further body is then read through the meta forms: it opens with `declare` and closes with `end!`, no body opens inside another, a name is one word and is declared once, every `form` has its one `does`, every `example` fits the form above it, every `once` names a verb of its body, and every `sees` names a body of the file.

`check-program` checks the declarations file the same way, then reads one program file against one language it declares: only declared forms, token classes honoured, `once` and `order` honoured.

Every error is one line, `FILE:LINE: error: TEXT`, and the exit status is 0 when the file is clean, 1 on any error and 2 when the command line is wrong or a file cannot be read. A placeholder that no `is` line binds is read as free text and reported as a note, `FILE:LINE: note: TEXT`, so that a mistyped class name shows; a note is not an error. A last line, starting with `typify:`, sums up.

What the reader does not do yet: walk a tree or fill a path template (step 3), resolve `sees` names across programs, apply `absent`, write a `derived!` program, or act (step 5). It does not guess at any of them.

## Versions

Version 3 is what is published here. Besides its `version` line it differs from version 2 in one line of the meta body. Version 2 listed, under `Name`, the names a file could declare, and the list held the languages of the first tree, so no other tree could declare a language of its own. In version 3 `Name` is a sentence: a new language is a new body at the end of the file, which is what section 6 of [`ARCHITECTURE.md`](ARCHITECTURE.md) said from the start. The reader reads both versions, each against its own published body, so a tree that carries the version 2 body keeps validating until it moves.

What is planned next, in the meta body:

- `acts`, a form's recipe: one or more lines expanding the form into primitives, filled from the line's tokens with the quoting done by the reader;
- the primitives themselves, fixed and few, run by an executor with argument lists rather than shell strings, and a `plan` mode that prints them without running;
- `by` and `since` on a body, so that a declaration says who declared it and when, the way an evidential language marks its source;
- path placeholders declared by the tree's own languages instead of named in the meta body;
- a usage report, so that a form no program uses is a candidate for removal rather than heritage;
- the grammar emitter.
