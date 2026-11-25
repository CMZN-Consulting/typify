# Diagnostic output

Use an output flag on the command that checks the input.

```sh
typify check catalog-language.typify Catalog catalog.typify --sarif
typify check catalog-language.typify Catalog catalog.typify --lsp
typify check catalog-language.typify Catalog catalog.typify --github-annotations
```

Choose one output format per command. Each projection writes diagnostics to standard output and preserves the command's exit status. Without an output flag, each command uses its usual format. Use `--json` to obtain the command's result together with its native diagnostics. A successful check with no diagnostics produces an empty SARIF results array, empty document diagnostic lists for LSP, or no annotation commands.

## Source ranges

Native diagnostic positions use one-based lines and Unicode code point columns, an exclusive end, and zero-based UTF-8 byte offsets. SARIF 2.1.0 reports `columnKind` as `unicodeCodePoints`. Its region also includes the byte offset and length. Artifact URIs encode special characters, and paths within the working directory are relative. Typed diagnostic data appears in the result's `properties`, including any paths carried by the native diagnostic. The `typify` member holds the native fingerprint fields and fix. Related term origins become related locations. [SARIF 2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/sarif-v2.1.0-os.html)

The LSP projection groups diagnostics under `documents`. Each entry contains the absolute document URI and a diagnostic list suitable for `textDocument/publishDiagnostics`. Lines start at zero. The character offset counts UTF-16 code units before each endpoint in the source text the reader actually analyzed. For example, in `tag : 🚀=bad`, the span of `bad` is columns 9–12 in the native diagnostic and characters 9–12 on zero-based line 0 in LSP. The rocket takes one code point and two UTF-16 units. The `unlocated` array carries native diagnostics with a null range, including file and usage errors. LSP requires a range on a document diagnostic. Native related data, fingerprints and the fix field remain available under the LSP diagnostic's `data` member. v1 emits a null fix. [LSP diagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic)

The GitHub projection produces one escaped workflow command per diagnostic. Errors produce `::error` and notes produce `::notice`. Each title names the diagnostic code. Multiline spans identify the first and last included lines. A span ending at the start of the following line excludes that line. Single-line spans use one-based columns through the last included code point. Unlocated messages omit coordinates. [GitHub Actions annotations](https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-commands)

## Stage 4 release evidence

Run the kernel, language and mutation jobs at `--stage 4` on the same source revision. Set `GITHUB_SHA` to the full candidate commit when replaying the workflow locally. Aggregate their JSON reports with the independently supplied measurements.

```sh
racket scripts/ci.rkt --stage 4 --aggregate kernel.json language.json mutation.json \
  --stage4-provider provider.json --extension-manifest-sha256 "$FROZEN_EXTENSION_MANIFEST_SHA256" \
  --output stage4.json
```

The aggregate requires every Stage 3 criterion, complete matching v12 replays against the separately pinned wire overlay, and the following provider fields. A failing release gate produces exit status 1. A single-platform kernel job may pass its own checks while the release gates remain open.

| Provider member | Measured data                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Header          | `schema` is `typify-stage4-provider-v1`, `measurementMode` is `measured`, `sourceRevision` is the candidate commit, and `readerBuild` is the build from `--version --json`.                                                                                                                                                                                                                                                                                        |
| `registry`      | `previousTag` and `previousRevision` identify the pinned v1.0.0-rc.1 checkout. `result` is the unmodified result of `racket scripts/check-diagnostic-registry.rkt PREVIOUS_CHECKOUT`.                                                                                                                                                                                                                                                                              |
| `projections`   | `sarifSchemaSha256` identifies the OASIS SARIF 2.1.0 OS schema. `sarifCodes` and `lspCodes` independently list every code measured in that format. `schemaDocuments` covers at least the v12 invocation count. `codeComparisons` and `rangeComparisons` each cover at least the registry size, counting agreement on a null range for unlocated diagnostics. `nonBmpComparisons` is positive. `schemaFailures`, `conversionFailures` and `rangeFailures` are zero. |
| `extension`     | `manifest` has the independently frozen `archiveSha256`, ordered invocation `ids`, and `doubleFailureId` naming the invocation with two failing library calls. `independent` is true. `results` has one object per frozen ID, with fields `id` and `pass`.                                                                                                                                                                                                         |
| `targets`       | Four objects named by `target` with values `linux-x86_64`, `linux-aarch64`, `macos-aarch64` and `windows-x86_64`. Each reports `v12Invocations`, `extensionInvocations` and the lowercase `transcriptSha256` of the complete native CLI capture. Each count matches its inventory and all four digests agree.                                                                                                                                                      |

The extension custodian supplies the SHA-256 of the manifest's compact canonical UTF-8 JSON, with sorted object keys, original array order and no final newline. The release job compares it with the measured manifest. Each platform job must capture the same ordered invocations and exact stdout, stderr and exit codes, preserving line endings and paths before hashing. The diagnostic and platform measurements come from the release jobs. The aggregate validates their receipts. It records the canonical provider digest and the independently supplied extension pin so a reviewer can identify the inputs used.

### Capturing the native transcripts

The existing baseline-only command is `python3 scripts/capture-release-corpus.py EXE V12 OUTPUT`. For a release, use the same externally approved inputs on each platform.

```sh
python3 scripts/capture-release-corpus.py "$EXE" "$V12" transcript.json \
  --extension-archive "$ARCHIVE" --extension-manifest "$MANIFEST" \
  --extension-manifest-sha256 "$FROZEN_EXTENSION_MANIFEST_SHA256" \
  --extension-root "$FRESH_FIXTURE_DIRECTORY" --target "$TARGET" > target.json
```

The frozen tar or compressed tar contains ordinary fixture files and directories. Its root may contain `capture.json`. Alternatively, pass `--extension-invocations INDEX.json` and include the SHA-256 of its exact file bytes as `invocationsSha256` in the independently approved extension manifest. The index is an object with `schema` equal to `typify-stage4-invocations-v1` and `runs` in manifest ID order. Each entry has exactly four members, for example `{"id":"sample/version","cwd":".","argv":["typify","--version","--json"],"exit":0}`. The working directory is relative to the archive root, arguments are a flat array of strings, and `exit` is 0, 1 or 2. Stage fixtures and operands must stay within those directories. Fixture archives carry file contents; host file ownership and mode are determined by the worker. Use frozen facts inputs for cases that require specified permissions or owners.

Use matching physical fixture coordinates for the v12 and extension roots across workers. For the reader's absolute input paths, `C:/typify-fixture` on Windows corresponds to `/Volumes/C/typify-fixture` on macOS and Linux. The parent of the designated extension directory must exist. The capturer creates that directory afresh and removes it after use. It declines a pre-existing directory.

`target.json` records the checked host platform, both observed invocation counts and the transcript hash. The transcript lists v12 invocations first and extension invocations second. Each row has an ID, process exit code, and separate base64 values for the original stdout and stderr bytes. The transcript envelope uses the same ASCII serialization and final LF on all platforms. Native paths, document URIs, non-ASCII output and CRLF remain observable. A mismatch identifies a difference for the release operator to investigate.
