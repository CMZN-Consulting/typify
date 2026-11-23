# Typify

Typify declares line-oriented languages, checks their programs and builds their meanings as data.

The 1.0.0 release targets [Linux x86_64](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-linux-x86_64.tar.gz), [Linux ARM64](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-linux-aarch64.tar.gz), [macOS ARM64](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-macos-aarch64.tar.gz) and [Windows x86_64](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-windows-x86_64.zip). These download links become available when the stable release is published. The [installation guide](docs/install.md) describes archive and manifest verification.

On Linux or macOS, start in a directory containing only your downloaded archive. Unpack it and add its executable directory to your current shell's path.

```sh
tar -xzpf typify-*.tar.gz
export PATH="$PWD/typify/bin:$PATH"
typify --version
```

On Windows, use PowerShell.

```powershell
Expand-Archive typify-windows-x86_64.zip -DestinationPath .
$env:Path = "$PWD\typify\bin;$env:Path"
typify --version
```

## Check a catalog

Save the declaration as `catalog-language.typify`.

```typify
#lang typify
declare : Catalog
file : catalog.typify
is : category = "book" | "journal"
is : title = /[^=]+/
term : item : category title
form : item : <category> = <title>
does : item <category> <title>
example : item : book = Algebra
count : item = 1..*
end!
```

Save the program as `catalog.typify`.

```typify
item : journal = Market Research
```

```sh
typify check catalog-language.typify Catalog catalog.typify --json
```

The `terms` field is `[["item","journal","Market Research"]]`. The example declares which words belong to a category and how an accepted line becomes an `item` term. A consumer gives that term its application-specific behavior.

## Ask what a word means

A program can contain `? : item`. To see the definition of `item` in this catalog, run the reader's `ask` command.

```sh
typify ask catalog-language.typify Catalog item
```

The answer includes the form you can write, the term it produces, its count and its example. The optional [MCP server](mcp/README.md) exposes word lookup, program checks and plan previews to editors and models.

## Work from source

With Racket 9.3 CS in a checkout, run the reader directly.

```sh
raco pkg install --scope user --no-setup --deps fail --name typify --link ./src
racket src/main.rkt --version
racket src/main.rkt emit Typify
```

The runtime depends on the base package. Class matching uses interval automata and pinned Unicode 17.0 data. No solver is included. Source contains no comments; declarations carry formal syntax and terms, while prose belongs in the documentation.

The emitted grammar recognizes line formation under the declared forms and classes. The reader separately checks program counts, ordering, library results and declaration references.

To inspect the host's derived state bounds and temporary work guards for declaration checks, run `racket src/main.rkt check catalog-language.typify --bounds --json`. These host bounds are separate from the language's declared caps.

Native diagnostics use the closed registry in [src/diagnostics.rkt](src/diagnostics.rkt). The reader collects independent diagnostics in file, line and column order, retaining detection order at the same position. A failed declaration prevents the program file from being opened. Empty class languages produce `N_CLASS_EMPTY` notes; notes do not change the exit status. Their witness records explicitly say `empty` and omit sample witnesses. A class containing the empty string still has a real member witness. File, usage and missing-file diagnostics have a null source range. `diagnostic-data-projection` selects each code's required comparison fields and ignores additional host data. Unregistered codes, missing fields and unknown kinds are implementation contract errors. Program diagnostics distinguish an invalid line shape, no matching structural form and a captured token outside its class; token ranges identify the actual program capture.

For the full tests, build the pinned grammar validator and set its executable path and revision using the preparation steps in [the CI workflow](.github/workflows/ci.yml), then run `raco test test`. Those preparation steps need Git, CMake and a C++ compiler. Run `racket test/smoke.rkt` for a small integration check that needs only Racket. Format source with `racket scripts/format.rkt --write`. GitHub Actions also measures grammar agreement and mutation strength. Kernel checks and complete conformance acceptance are separate; missing external evidence fails acceptance. `racket scripts/ci.rkt --defer-blind --output kernel-check.json` measures the internal kernel without executing the frozen corpus; blind-dependent acceptance gates remain explicitly pending in that mode. The workflow keeps internal checks in one job and runs frozen language and mutation replays in separate jobs on the same commit, with 30-minute and 60-minute replay limits respectively. CI sets `TYPIFY_BLIND_MODE` to `full` and runs both frozen replays. Stage-three acceptance requires matching evidence from all three jobs.

For editor integration, use the [diagnostic formats](docs/diagnostics.md). Consumers that execute or cache root plans can inspect the [ordered schedule](docs/plan-wire.md) and the [inputs the reader observed](docs/inputs.md). An implementation can replay the [public conformance corpus](test/corpus/MANIFEST.md) using the invocations and expected results in each case.
