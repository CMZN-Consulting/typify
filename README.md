# Typify

Typify declares line-oriented languages, checks their programs and builds their meanings as data.

This branch develops version 1. There is no version 1 release yet. The stable 0.5 reader remains on main and tag v0.5.0.

With Racket 9.3 CS, run the source reader:

```sh
raco pkg install --scope user --no-setup --deps fail --name typify --link ./src
racket src/main.rkt --version
racket src/main.rkt emit Typify
```

A declaration file, catalog-language.typify:

```text
#lang typify
declare : Catalog
is : category = "book" | "journal"
is : title = /[^=]+/
term : item : category title
form : item : <category> = <title>
does : item <category> <title>
example : item : book = Algebra
count : item = 1..*
end!
```

A program, catalog.typify:

```text
item : journal = Market Research
```

```sh
racket src/main.rkt check catalog-language.typify Catalog catalog.typify --json
```

The terms field is [["item","journal","Market Research"]].

The runtime depends on the base package. Class matching uses interval automata and pinned Unicode 17.0 data. No solver is included. Source contains no comments; declarations carry formal syntax and terms, while prose belongs in the documentation.

Run `raco test test` for the tests, or `racket test/smoke.rkt` for a small integration check. Format source with `racket scripts/format.rkt --write`. GitHub Actions also measures grammar agreement and mutation strength. Kernel checks and complete conformance acceptance are separate; missing external evidence fails acceptance.

The language reference and installation guide will be published in the [wiki](https://github.com/CMZN-Consulting/typify/wiki) with the versioned release.
