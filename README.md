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

The emitted grammar recognizes line formation under the declared forms and classes. The reader separately checks program counts, ordering, library results and declaration references.

To inspect the host's derived state bounds and temporary work guards for declaration checks, run `racket src/main.rkt check catalog-language.typify --bounds --json`. These host bounds are separate from the language's declared caps.

Native diagnostics use the closed registry in [src/diagnostics.rkt](src/diagnostics.rkt). The reader collects independent diagnostics in file, line and column order, retaining detection order at the same position. A failed declaration prevents the program file from being opened. Empty class languages produce `N_CLASS_EMPTY` notes; notes do not change the exit status. Their witness records explicitly say `empty` and omit sample witnesses. A class containing the empty string still has a real member witness. File, usage and missing-file diagnostics have a null source range. `diagnostic-data-projection` selects each code's required comparison fields and ignores additional host data. Unregistered codes, missing fields and unknown kinds are implementation contract errors. Program diagnostics distinguish an invalid line shape, no matching structural form and a captured token outside its class; token ranges identify the actual program capture.

For the full tests, build the pinned grammar validator and set its executable path and revision using the preparation steps in [the CI workflow](.github/workflows/ci.yml), then run `raco test test`. Those preparation steps need Git, CMake and a C++ compiler. Run `racket test/smoke.rkt` for a small integration check that needs only Racket. Format source with `racket scripts/format.rkt --write`. GitHub Actions also measures grammar agreement and mutation strength. Kernel checks and complete conformance acceptance are separate; missing external evidence fails acceptance. `racket scripts/ci.rkt --defer-blind --output kernel-check.json` measures the internal kernel without executing the frozen corpus; blind-dependent acceptance gates remain explicitly pending in that mode. The workflow keeps internal checks in one job and runs frozen language and mutation replays in separate jobs on the same commit, with 30-minute and 60-minute replay limits respectively. `TYPIFY_BLIND_MODE` remains `deferred` until a new independent frozen handoff; full acceptance requires complete, matching evidence from all three jobs.

The language reference and installation guide will be published in the [wiki](https://github.com/CMZN-Consulting/typify/wiki) with the versioned release.
