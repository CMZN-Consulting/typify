# How `LanguageDeclarations.txt` is read

This file is the meta space of the language: it says, in plain words, how a file that declares languages is to be read and acted on. It says nothing about what the declared languages are for; those speak for themselves, in the tree that carries them. A reader is correct when it does what this page says and nothing this page forbids.

## 1. What the file is

`LanguageDeclarations.txt` is a list of language bodies. Each body opens with `declare : <Name>` and closes with `end!`. The first body declares the language the file itself is written in, so the file carries its own grammar the way a Lisp carries its reader or a Lean prelude carries its own notation. Everything a reader needs to read the file is in its first body, and the reader trusts nothing else until that body has been read.

## 2. Lines

A reader works line by line. It strips leading and trailing blanks from every line, then classifies it:

1. **Nothing.** A blank line, or a line whose first non-blank characters are the comment prefix declared for that language. The prefix is `#` unless a body declares another. Before the first body's `comment` line has been read, `#` is assumed, which is why the meta body may carry comments above its own `comment` line.
2. **A bang line**, `<verb>!`: one word ending in an exclamation mark, no colon, no arrow.
3. **A colon line**, `<verb> : <argument>`: the first colon splits it; blanks around the colon are stripped; the argument keeps its inner spacing and may itself contain colons.
4. **An arrow line**, `<left> -> <right>`: the first arrow splits it, both sides stripped.

The shapes are tried in that order, so a line that holds a colon is a colon line even when it also holds an arrow. A line that is none of these is an error. Every error the reader reports names the file and the line number.

## 3. Bodies and forms

Inside a body the reader accepts only the forms the meta body declares, each written as `form : <production>` followed by exactly one `does : <text>`. A production is one of the three line shapes above with placeholders in angle brackets. The placeholders are filled by token classes declared with `is : <name> = <domain>`, where the domain is either a list of literal words separated by `|`, in which case the token must be one of them, or a sentence, in which case the reader accepts any non-empty text and leaves the meaning to the executor that acts on it. A placeholder names the class of that name in its own body or, failing that, in the meta body. One that neither binds is unbound: in versions 2 and 3 the reader accepts any non-empty text for it, as for a sentence, and reports it in a note, not an error, so that a mistyped class name shows.

Four forms qualify another form rather than standing on their own: `example` gives a legal line for the form just declared and is a test vector; `once` says the form's verb may appear at most once in one program file; `order : strict` says the forms of a program appear in the order they are declared; `sees : <Name>` says programs of this language name things that programs of another language declare, and the reader checks that they exist before acting.

Three forms bind a language to files: `file : <path>` names where its programs live, with placeholders in angle brackets filled from the tree (in versions 2 and 3 the placeholders are the ones the meta body names); `required!` and `optional!` say whether a program file must exist wherever the path template can be filled; `absent : <default>` says what an optional program means when the file is missing. `derived!` marks a language whose programs the reader writes from the others; a hand-written file at that path is overwritten, never read.

`version : <number>` states the version of the language declared in that body. A reader that knows a lower version than a body declares refuses the body and says so; it never guesses at forms it does not know.

A language's name is the word on its `declare` line: one word, declared once in a file. In version 2 the meta body listed the names a file could declare, and the list held the languages of the first tree, so no other tree could declare a language of its own. Version 3 changes that one line: `Name` is a sentence, and a file declares a language of any name. A reader of version 3 still reads a version 2 file, against the version 2 body and with its list.

## 4. The reading order

1. Read the first body. It must be `declare : LanguageDeclarations`, at a version the reader knows. Check every line of that body against the forms the body itself declares: the meta language validates itself, and a meta body that fails its own forms stops the reader before anything else is read.
2. Read every further body against the meta forms. Each must open with `declare` and close with `end!`; a body may not open inside another; a name is one word and may not be declared twice; every `form` must be followed by its `does`.
3. Walk the tree. A language whose path template has no placeholder is read first, and there is at least one. Every other template is filled from the tree, one placeholder per directory level, in the order the templates nest. Where a program of one language fixes the order of the things another language's template ranges over, that order wins over the directory listing.
4. For every language, fill its path template for every place in the tree it can be filled, and read each program that exists against the language's forms: only declared forms, token classes honoured, `once` and `order` honoured, `sees` names resolved. A required program that is missing is an error; an optional one that is missing is read as its `absent` text.
5. Only then act: create what the programs describe, write the derived programs, and print what was done.

Validation and action are separate steps, and a reader offers validation on its own, so a tree can be checked without touching anything.

## 5. What a reader must never do

- Execute a program line while reading or validating. A line whose meaning is a command is text until the action step.
- Act on a tree that failed validation, even in part.
- Touch what already exists. The action step creates what is missing and leaves what stands; running it twice is running it once. Taking down what a tree stood up is a separate command that says what it will remove and refuses without confirmation.
- Read or write outside the tree and the places its languages name, such as a server, a log directory or a mail root. Each of those is overridable from the environment, which is how a tree is exercised on an isolated server, or in a container, without touching the live one.
- Act against a thing the programs mark as not to be acted on. A language may declare such a mark (the first tree to use typify marks a human's pane as never typed into), and every tool that reads a derived program honours it.

## 6. Adding a language or a form

A new language is a new body at the end of the file; a new form is a `form` and `does` pair, with an `example`, in the body it belongs to. Bump that body's `version`. Then the reader is changed to act on it, its validator learns the form, and the tests gain a fixture that uses it. The meta body changes last and rarely; when it does, its version bumps and every reader must be reviewed, because every other body is read through it.
