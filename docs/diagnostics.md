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
