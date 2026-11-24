# Typify

**Typify declares line-oriented languages, checks their programs and builds their meanings as data.**

[typify.science](https://typify.science) · [Installation Guide](docs/install.md) · [Language Wiki](https://github.com/CMZN-Consulting/typify/wiki/Language) · [Metalogic (Lean 4)](https://github.com/CMZN-Consulting/typify/wiki/Metalogic) · [Examples](https://github.com/CMZN-Consulting/typify/wiki/Examples) · [MCP Server](mcp/README.md) · [Packages](https://github.com/orgs/CMZN-Consulting/packages?repo_name=typify) · [Releases](https://github.com/CMZN-Consulting/typify/releases)

---

- **Self-describing `#lang typify`** — Declare lexical classes (`is`), typed constructors (`term`), surface line forms (`form`, `does`), examples (`example`), multiplicity bounds (`count`), pure path state, and ordered rules in plain line-oriented files.
- **Deterministic checking & grammar emission** — Exact interval automata over pinned Unicode 17.0 categories, closed native diagnostic codes with source ranges, and GBNF grammar emission (`typify emit`).
- **Machine-checked metalogic (`Lean 4`, zero `sorry`)** — Mechanically verified laws of [Formality](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Formality), [Conservative Extension](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Extension), and [Ontology Representation](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Ontology) in [`lean/`](lean).
- **Standalone native binaries & OCI packages** — Self-contained executables for Linux, macOS, and Windows with Sigstore build-provenance attestations, OCI packages on GitHub Packages (`ghcr.io`), and an optional stdio [MCP server](mcp/README.md).

## Install

Install the standalone reader from [typify.science](https://typify.science) (verifies every unpacked file against `typify/MANIFEST.txt`):

```sh
curl -fsSL https://typify.science/install.sh | sh
```

On Windows PowerShell:

```powershell
irm https://typify.science/install.ps1 | iex
```

### Shipped binaries & GitHub Packages

The 1.0.0 release publishes standalone archives on [GitHub Releases](https://github.com/CMZN-Consulting/typify/releases/tag/v1.0.0) and OCI binary artifacts under [GitHub Packages](https://github.com/orgs/CMZN-Consulting/packages?repo_name=typify) (`ghcr.io/cmzn-consulting/typify:1.0.0` multi-platform index plus per-platform packages). No Racket installation is required.

| Platform | Release archive | GitHub Package (OCI) |
| --- | --- | --- |
| Linux `x86_64` | [typify-linux-x86_64.tar.gz](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-linux-x86_64.tar.gz) | [`ghcr.io/cmzn-consulting/typify-linux-x86_64:1.0.0`](https://github.com/CMZN-Consulting/typify/pkgs/container/typify-linux-x86_64) |
| Linux `ARM64` | [typify-linux-aarch64.tar.gz](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-linux-aarch64.tar.gz) | [`ghcr.io/cmzn-consulting/typify-linux-aarch64:1.0.0`](https://github.com/CMZN-Consulting/typify/pkgs/container/typify-linux-aarch64) |
| macOS `ARM64` | [typify-macos-aarch64.tar.gz](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-macos-aarch64.tar.gz) | [`ghcr.io/cmzn-consulting/typify-macos-aarch64:1.0.0`](https://github.com/CMZN-Consulting/typify/pkgs/container/typify-macos-aarch64) |
| Windows `x86_64` | [typify-windows-x86_64.zip](https://github.com/CMZN-Consulting/typify/releases/download/v1.0.0/typify-windows-x86_64.zip) | [`ghcr.io/cmzn-consulting/typify-windows-x86_64:1.0.0`](https://github.com/CMZN-Consulting/typify/pkgs/container/typify-windows-x86_64) |

The [installation guide](docs/install.md) describes SLSA attestation (`gh attestation verify`) and manifest tree checks. To unpack an archive manually on Linux or macOS, start in a directory containing only your downloaded archive:

```sh
tar -xzpf typify-*.tar.gz
export PATH="$PWD/typify/bin:$PATH"
typify --version
```

On Windows, use PowerShell:

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

A program can contain `? : item`. To see the definition of `item` in this catalog, run the reader's `ask` command:

```sh
typify ask catalog-language.typify Catalog item
```

The answer includes the form you can write, the term it produces, its count and its example. The optional [MCP server](mcp/README.md) exposes word lookup, program checks and plan previews to editors and models.

## Metalogic (`Lean 4`)

[`lean/`](lean) mechanizes a facts-only declaration fragment of Typify v1 under `leanprover/lean4:v4.36.0` with zero comments, zero user axioms, and zero `sorry` placeholders:

| Law | Wiki page | Statement & Proof |
| --- | --- | --- |
| **Formality** | [Metalogic-Formality](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Formality) | [`FormalityStatement.lean`](lean/TypifyMetalogic/FormalityStatement.lean) · [`FormalityProof.lean`](lean/TypifyMetalogic/FormalityProof.lean) |
| **Conservative Extension** | [Metalogic-Extension](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Extension) | [`ExtensionStatement.lean`](lean/TypifyMetalogic/ExtensionStatement.lean) · [`ExtensionProof.lean`](lean/TypifyMetalogic/ExtensionProof.lean) |
| **Ontology Representation** | [Metalogic-Ontology](https://github.com/CMZN-Consulting/typify/wiki/Metalogic-Ontology) | [`OntologyStatement.lean`](lean/TypifyMetalogic/OntologyStatement.lean) · [`OntologyProof.lean`](lean/TypifyMetalogic/OntologyProof.lean) |

The release-blocking check `Lean / zero sorry` (`scripts/metalogic-check.py`) compiles every tracked `.lean` module, audits kernel axiom dependencies, reflects each `Prop` mechanically into canonical TeX, and replays [`test/metalogic-conformance.json`](test/metalogic-conformance.json) against the packaged reader.

## Documentation

- **Guides & Reference**: [Documentation Wiki](https://github.com/CMZN-Consulting/typify/wiki) · [Language](https://github.com/CMZN-Consulting/typify/wiki/Language) · [Command line](https://github.com/CMZN-Consulting/typify/wiki/Command-line) · [Examples](https://github.com/CMZN-Consulting/typify/wiki/Examples)
- **Repository Specs**: [Installing & Verifying Typify](docs/install.md) · [Diagnostic Formats](docs/diagnostics.md) · [Ordered Plan Wire Format](docs/plan-wire.md) · [Observed Inputs](docs/inputs.md) · [Stage 5 Consumer Gate](docs/stage5.md)
- **Conformance & Tooling**: [Public Conformance Corpus](test/corpus/MANIFEST.md) · [MCP Authoring Server](mcp/README.md) · [License & Third-Party Notices](LICENSE.md)

## Work from source

With Racket 9.3 CS in a checkout, run the reader directly:

```sh
raco pkg install --scope user --no-setup --deps fail --name typify --link ./src
raco make src/main.rkt
racket src/main.rkt --version
racket src/main.rkt emit Typify
```

- **Runtime & grammar**: The runtime depends only on the Racket `base` package. Class matching uses interval automata and pinned Unicode 17.0 data without an external solver. Source `.rkt` and `.lean` files contain no comments; declarations carry formal syntax and terms, while prose belongs in documentation. Emitted grammars recognize line formation under declared forms and classes, while the reader separately enforces counts, ordering, library evaluations, and declaration references. Inspect host-derived state bounds via `racket src/main.rkt check catalog-language.typify --bounds --json`.
- **Diagnostics**: Native diagnostics use the closed registry in [src/diagnostics.rkt](src/diagnostics.rkt) and report in deterministic file, line, column, and detection order. Empty class languages emit non-fatal `N_CLASS_EMPTY` notes with explicit `empty` witness records; file, usage, and missing-file diagnostics carry a null range; `diagnostic-data-projection` selects each code's required comparison fields.
- **Verification**: Run `racket test/smoke.rkt` for a fast Racket-only check, and format source with `racket scripts/format.rkt --write`. For the full test suite (`raco test test`), build the pinned independent GBNF validator (`CMake` and a C++ compiler) using the preparation steps in [`.github/workflows/ci.yml`](.github/workflows/ci.yml). `racket scripts/ci.rkt --defer-blind --output kernel-check.json` measures internal kernel checks separately from full frozen-corpus replay (`TYPIFY_BLIND_MODE=full`) and `Lean / zero sorry` verification.
