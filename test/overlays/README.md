# Replaying the frozen v12 cases

Run `racket test/blind-corpus.rkt` to compare the reader with the v12 corpus plus the independently frozen Stage 4 additions. The JSON report includes both sets of custody hashes. A nonzero exit indicates a mismatch or incomplete verification. To compare against the original v12 wire, run `racket test/blind-corpus.rkt --legacy-v12`.

The v12 import contains the original files and expectations. The Stage 4 overlay adds expected object members for specific invocations. Its format is `typify-v12-stage4-overlay-v1`. Its header carries `baselineArchiveSha256`, `baselineTreeSha256`, `baselineReceiptSha256` and `entries`. The runner checks the overlay bytes against a separate SHA-256 pin before use.

Each entry names one existing invocation by its full `id`, repeats its exact `invocation` argument vector and lists additions in an `add` array. Each addition has a `path` array and a `value`. Strings in a path select object members. Integers select existing array elements by zero-based index. The new member must be absent and its parent must already exist. The accepted paths are the top-level `stdout_json` members `inputs`, `facts` and `schedule`, and the `sequence` member on an existing state row.

Within an added `inputs` object, a value of `{"$fixtureRoot":"root"}` at `root` requests the absolute root of that frozen invocation. The runner resolves its relative operand against the authenticated case directory while constructing the expectation. All output fields, concrete paths and values are then compared by the same v12 rules. This checks the expected local root separately on each host. A byte-identity comparison across hosts requires identical output bytes.
