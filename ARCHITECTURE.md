# How Typify reads declarations and programs

Typify is a metalanguage with a generic reader. Applications own their language declarations and programs. Adapters own operational meaning, resource checks and execution.

## 1. Grammar ownership

The package owns canonical, self-describing declaration grammars in `src/grammar/v2.typify` and `src/grammar/v3.typify`. Each grammar opens with `declare : LanguageDeclarations`, declares its version and forms, and closes with `end!`. The reader validates the resource through its own forms before using it.

The grammar appears once per supported version. TypeScript implements the bootstrap and matching algorithm; it does not contain another literal copy of the grammar. Consumers select a bundled grammar rather than copying one into their own tree.

An application declaration file begins with `typify : 2` or `typify : 3`, followed by its own language bodies. Blank lines and default `#` comments may precede the selector. An absent, malformed or unsupported selector is an error. A copied `LanguageDeclarations` body is not the application-file format.

The selector states the declaration grammar version. A body's `version` states that application's language version. A generic reader records application versions; an adapter rejects versions whose operational meaning it does not implement.

## 2. Lines and token matching

The reader strips a line's outer whitespace, including carriage returns and a leading byte-order mark. It then classifies the line in this order:

1. Blank text or text starting with the language's comment prefix is nothing.
2. A single word ending in `!`, without a colon or arrow, is a bang line.
3. Text containing a colon is a colon line, split at its first colon.
4. Text containing an arrow is an arrow line, split at its first arrow.
5. Other text is an error.

The default comment prefix is `#`. A declared prefix applies to that language's programs, while declaration bodies are read using the meta-language's prefix.

Productions contain literal text and angle-bracket placeholders. Captures take the shortest text up to the next literal, without backtracking; the final capture takes the remainder, excluding any final literal. Capture edges are trimmed and token values must be nonempty. Adjacent placeholders can therefore declare an unusable form; declaration linting is not implemented.

A token class resolves first in its own language and then in the selected meta language. Domains containing `|` enumerate literal alternatives. Other domains describe nonempty free text. An unresolved class also admits nonempty text and produces a note. Version 2 retains its historical closed `Name` class; version 3 permits application-defined language names.

## 3. Declaration validation

Every body opens with `declare : Name` and closes with `end!`. A body cannot open inside another. Names are unique alphanumeric words beginning with a letter. Each `form` has exactly one directly following `does` line, ignoring blank and comment lines.

An example is checked locally against its associated form and token classes. This does not establish that normal program dispatch will reach that form: comment prefixes and earlier forms may intercept it. `once` must name a verb of a declared form. `sees` must name a language declared in the file; it does not establish definitions, references or scope within programs.

The current public declaration model retains language names and positions, versions, comments, ordering, token classes, forms and repetition constraints. Other declaration metadata is checked as source syntax but not retained in that model. A consumer checks that `meta` is present and no diagnostic is an error before using the result.

`readGrammar` exposes canonical-resource validation for tooling and tests. Application consumers use `readDeclarations` with a compact declaration file.

## 4. Program validation

`parseProgram` uses languages from a valid declaration result. It applies comment handling, then tries forms in declaration order. The first matching form wins. It checks token classes, `once` constraints and strict ordering across the whole program.

Success exposes source labels, original lines, 1-based line positions, 0-based form indexes, matched forms and ordered captures. Repeated capture names remain separate entries. Any validation error withholds all parsed records. `readProgram` returns the same diagnostics through its compatibility interface.

A source label is metadata. Parsing a program does not open a file named by that label or execute its contents. CLI I/O uses Bun's file APIs and validates declarations before opening the requested program file.

## 5. Adapter boundary

A complete adapter validates all declaration and program inputs before acting. It adds semantic checks for its supported language versions, filesystem paths, references, resource limits, authorization and combinations of actions. `does` is explanatory text, and free-text captures remain data until the adapter deliberately assigns them meaning.

Typify does not currently walk trees, fill file templates, resolve program references, apply `absent`, create derived programs or execute recipes. Those features need explicit semantics before implementation. Operational concepts such as tmux seats, filesystem policy or identity belong to their adapters.

Execution safety depends on that adapter and the host's permissions. A bounded vocabulary and explicit argv handling help define a controlled interface; the validator alone cannot guarantee harmless effects or the behavior of a command receiving a valid argument.

Grammar emission and constrained generation are planned capabilities. They currently provide no implemented decoding guarantee.

## 6. Runtime and changes

The runtime is Bun. Native Bun APIs handle file I/O, process spawning, paths and streams where available. `process.exitCode` is the CLI's exit-status boundary because Bun has no corresponding native API. Source explanations live in documentation rather than code comments.

A new application language or form belongs in the consumer's declaration file and adapter. Change the meta language only for a general capability that belongs across consumers, version that change explicitly and preserve supported-version matching semantics deliberately.
