# Rechecking a root tree or plan

`typify tree DECLARATIONS ROOT --json` and `typify plan DECLARATIONS ROOT --json` report an `inputs` object. It records the observations made by that invocation. A consumer can compare them with a proposed root and validated facts document to decide whether it can reuse the original successful result. Keep the same reader build and invocation when reusing a result.

| Member         | Meaning                                                                                                                                                                                                                                                                                                                 |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `root`         | Lexically simplified absolute path of the root walk, using the host spelling below. A relative root is resolved against the invocation's working directory.                                                                                                                                                             |
| `factsRoot`    | The `root` member of the accepted facts document used by this invocation, or JSON null if no document was used.                                                                                                                                                                                                         |
| `declarations` | Each declaration file read, with its operand `path` and `sha256` of the original file bytes. Resolve a relative declaration operand against the original invocation directory.                                                                                                                                          |
| `paths`        | Each filesystem component tested during discovery and its `kind`. Paths are relative to `root`. The root itself is `.`. Kinds are `absent`, `file`, `directory`, `link` and `other`. A file is regular. Links are observed without traversal.                                                                           |
| `directories`  | Each directory listed for placeholder matching. A row contains its relative `path`, the full ordered `entries` of `name` and `kind`, and a `sha256` of the canonical entries array. Entry names include names that did not match a placeholder. Reading a directory listing does not read every file in that directory. |
| `programs`     | Each program file actually opened, with its root-relative `path` and `sha256` of the bytes before text decoding. Guarded programs contribute a digest if opened. The outer `programs` array records each program's status, including `defaulted`, `missing` and `absent-empty`; the tested paths record absence.        |
| `facts`        | Each evaluated query, with its `fact`, `args` and Boolean `held`, in order of first evaluation. Guards may query facts during a tree walk. A plan also records queries during expansion and execution. An unevaluated branch contributes no query.                                                                      |

Rows are deduplicated within each array in first-observed order. The outer `facts` field carries the digest of the entire facts document under the existing plan digest convention, or `none`. A changed whole-document digest can coexist with unchanged observations. Validate a replacement facts document under the same schema before comparing its query results.

### Absolute host paths

The collector and root walk use the same absolute coordinates. POSIX paths retain their spelling. A normal Windows drive path such as `c:\work\root` becomes `/Volumes/C/work/root`, and the drive root is `/Volumes/C`. A UNC path such as `\\Server\Share\work` becomes `/Volumes/UNC/Server/Share/work`. Keep the original case of host, share and component names. Only the drive letter is uppercased. Relative program paths use forward slashes. The collector uses these coordinates for its root and every fact argument. A symlink’s value is its reported target text.

Use an ordinary drive or UNC share for a Windows root. Drive-relative names such as `C:logs`, device namespace paths such as `\\?\` and `\\.\`, and named pipes and mailslots on a server are unsupported. Supplied facts documents already use Typify’s absolute path grammar and are validated as written. A cached plan from another host requires matching coordinates and observations there.

### Comparing observations

File hashes are lowercase hexadecimal SHA-256 of the bytes as stored, including line endings. For a directory, sort entries by Unicode scalar value of the name (lexicographic, with no case folding or normalization), and serialize the array as the reader's compact canonical JSON in UTF-8. The two keys of each entry appear in the order `kind`, `name`. There is no separating whitespace, byte-order mark or trailing newline. For instance, these literal UTF-8 bytes hash to `b011bd96b669bd76cf45412fed1917a53e5a04d50c85cfc18ed32ce1bbf7bea0`.

```json
[
  { "kind": "directory", "name": "a" },
  { "kind": "directory", "name": "b" },
  { "kind": "file", "name": "ø" }
]
```

In JSON strings, escape quotation marks and backslashes. Use the short escapes `\b`, `\t`, `\n`, `\f` and `\r` for their corresponding characters. Encode other U+0000–U+001F characters and U+007F as `\u` followed by four lowercase hexadecimal digits. Emit all other characters directly in UTF-8, including slashes and non-ASCII names.

For each point query compare whether the named fact with the recorded arguments is present. A present `mode` or `owner` fact stays present if its value changes. For `clean`, recompute the range query over the new facts document. The path must belong to the facts root, and a recorded directory or symlink with the marker name at least two component levels below the path makes the query false. An unlisted intermediate name can acquire such a marker. Checking only previously listed descendants would miss it.

The record describes observations during the walk. A failed command can report a prefix; its `root` can be null if discovery did not begin. Use successful results for reuse, and acquire a stable view of the files when changes may occur during comparison.
