# Measuring a consumer for Stage 5

Stage 5 accepts a specific reader build and a specific consumer revision. Supply the three Stage 4 job reports, the Stage 4 provider, the approved extension manifest digest, and a privately held consumer packet. Obtain the revision and packet digest independently of the packet.

```sh
racket scripts/ci.rkt --stage 5 --aggregate kernel.json language.json mutation.json \
  --stage4-provider provider.json --extension-manifest-sha256 "$EXTENSION_DIGEST" \
  --consumer-packet private.json --consumer-revision "$CONSUMER_REVISION" \
  --consumer-packet-sha256 "$PACKET_DIGEST" --output stage5.json
```

The command re-evaluates the Stage 4 evidence before reading the private packet. Exit zero requires all four additional gates. The public receipt carries the revision of the Stage 4 evidence, aggregate digest, gate decisions, counts and evidence digests. Keep the packet, commands, program names, tables and individual reviews with the consumer. To check the private packet on its own, run `racket scripts/check-consumer-release.rkt --stage5 private.json "$CONSUMER_REVISION" "$PACKET_DIGEST"`. This command reports only the consumer measurement.

## Private packet

Use schema `typify-consumer-release-v2`. Supply `consumerRevision` as a lowercase 40- or 64-digit hexadecimal revision and `version` as a successful capture of `typify --version --json`. A capture has integer `exit` and the exact UTF-8 `stdout` as a JSON string. All SHA-256 values use 64 lowercase hexadecimal digits. Digest JSON values using the [canonical JSON encoding](inputs.md) without a trailing newline. Hash original file bytes for the packet and source artifacts.

The `scope` object holds the frozen private inventory. Two `enumerators` attest its canonical digest in `inventorySha256`, each with its own `sourceSha256`. The `sources` object identifies four distinct sources through `oracleSha256`, `kernelSha256`, `nativeSha256` and `adapterSha256`. The blind author derives expected results from the declarations before seeing actual results. The custodian keeps that provenance and the underlying artifacts. A digest establishes which artifact a subsequent audit must inspect.

The inventory consists of `subjects`, `primitives`, `forms`, `exceptions`, `layers`, `operators`, `operatorsSha256` and `eligible`. Tests with an invented consumer construct a [complete packet](../test/test-stage5-consumer.rkt). Checkers match every ID against its inventory and reject missing or duplicate entries.

### Observed mount state

Each subject has an `id` and a `disposition`. The `mounts` array repeats the disposition for every ID. For a measured subject, `kernelSha256`, `planSha256` and the independent `oracle.rows[].mountSha256` must agree. Each hashes the entire canonical mount table, including the access mode of every row. Fix the table fields, row ordering and scope in advance. Including every observed path makes extra visible paths distinguishable from agreement.

A dormant, unenforced or unknown subject has a private `reason`. Omit all three table hashes for such subjects. Every subject remains in the coverage denominator. Only a measured subject with three equal tables counts as agreement. Unknowns, disagreements and zero measured subjects block the gate.

### Primitive and form audit

Each primitive ID is the triple `[declaringLanguage, name, argumentTypes]`, with an ordered array of type names. Form IDs are scope-qualified strings. In `audit.primitives`, include the ID, `redundant` Boolean, `exception` Boolean and an `evidenceSha256`. In `audit.forms`, include each ID, its `directHostEffect` Boolean, its `hostPrimitives` list and an `evidenceSha256`. Every form must reach the host only through inventoried primitives, and every primitive must have a distinct purpose. For each primitive in `scope.exceptions`, supply a reason, review digest and reviewer ID in `audit.exceptions`. The reviewer must differ from `audit.authorId`.

### Clause mutation

The independent enumerators freeze the inputs before any comparisons. Alongside the operator IDs, scope includes the following.

| Scope member | Contents |
| --- | --- |
| `operatorsSha256` | Canonical digest of `mutation.operatorDefinitions`. Each entry has `id`, `operatorSha256` and `eligibilitySha256`. Repeat the digest in `mutation.operatorsSha256`. |
| `eligible` | Ordered post-deduplication site IDs. Each ID is `[layer, qualifiedClauseSite, operatorId]`. |
| `mutationSites` | One entry per site ID, with the SHA-256 of the mutated source bytes in `mutantSha256` and its allowed `reach` pairs `[subjectId, field]`. Collapse byte-identical mutants before this inventory. |
| `mutationCases` | Ordered cases with an `id` and a `projections` array. Every case includes `plan`. A case may also require `tree`. The case ID `load` is reserved. |
| `mutationRowsSha256` | Digest of `oracle.mutationRows` in blind-author order. Each entry describes one scalar with `id`, `subject`, `field`, `law` (1 for mounts, 4 for tree/plan), `author`, `provenance: "blind-declaration"`, `case`, `projection`, `selector` and `expected`. |
| `equivalencesSha256` | Digest of `mutation.preRunEquivalences`, including an empty array when appropriate. |

Use `{"present":false}` for an absent scalar and `{"present":true,"value":null}` for a present JSON null. Present strings, booleans and numbers use the same wrapper. A completed comparison records the frozen row ID in `row`, plus its `expected` and observed `actual` wrappers. The `baseline` in `mutation` has `status: "measured"`, `calls` and `observations` over the entire blind table. A failed baseline voids the run.

A trial in `mutation.trials` names its site in `id` and supplies `mutantSha256`, `policyOnly`, `eligibleRows`, `calls`, `observations`, `status`, `killed` and `firstRow`. Eligible rows are in blind-author order and their subject and field must occur in the site's reach. When reach is nonempty and contains exclusively unmeasured subjects, only law 4 rows are eligible. Record a load and every case/projection in `calls` under `load` and the case IDs respectively. A failed load ends the call sequence. Each native trace records status, exit and the uncompressed byte length and SHA-256 of each stdout and stderr stream. Successful captures require exit zero and a valid semantic result. The measurement producer retains the original streams and supplies the observed native scalar for each eligible selector.

For a completed trial, the checker compares the actual scalars with the frozen expectations in order. `firstRow` is the first differing eligible ID, or null when all agree. `killed` and `status` must match this calculation. Report other native outcomes as `load-` or `evaluation-` followed by `refused`, `crash`, `timeout`, `process-error`, `launch-error` or `invalid-output`. Use `projection-error` for an ambiguous or otherwise invalid scalar selection. Every such trial stays in the denominator and earns zero kills.

Each scored trial with zero kills requires a private `reviewSha256` and `reviewerId` distinct from `mutation.runnerId`. A prior equivalence claim names `mutantSha256`, `claimantId`, `reviewerId`, `proofSha256`, `reviewSha256`, positive declaration line numbers in `lines` and a proof class of `same-canonical-form`, `unreachable-by-declaration` or `shadowed-by-clause`. The claimant, reviewer and blind row author must be independent. Only the frozen pre-run list removes trials. The release criterion permits at most 5 percent exclusions and requires at least 90 percent kills over the positive scored total.

The public result gives per-layer counts of completed agreements, credited kills, each unsuccessful native outcome, exclusions and the largest number of kills credited to one row. A structurally valid run below threshold still reports the counts, including layers with zero eligible sites. The checker verifies the reported scalars, native trace metadata and frozen sources. The consumer custodian retains the captures to verify each scalar against its selector and to establish the order of the freeze and the run.

### Facts, tree, plan and adapter

Two independently implemented collectors supply their source digests and full successful captures in `collectors`. Their UTF-8 output must match byte for byte, including whitespace and the final newline. The checker validates the facts document.

In `native.tree` and `native.plan`, supply the successful native command captures, `factsInputSha256` of the supplied facts bytes and an argument vector of the form `["typify", action, declarations, root, "--facts", factsFile, "--json"]`. The reader may leave facts unused. A command that used facts records their raw digest and root. A command that did not use facts records `none`, a null facts root and an empty fact read set. Both commands must agree on discovery inputs and program coordinates. `adapter` contains `treeOk`, `planStatus`, `factsInputSha256` and its private per-subject `rows`.

Each subject freezes ordered `treePaths` and `planPaths`. A selector `["programs", i]` contributes the entire zero-indexed native program record. For each subject the tree and plan select the same program indices. Shared selections across subjects are legal. Their union must cover every native program, including absent and defaulted program records. A plan may additionally select full schedule entries, `["schedule", i]`, or entire state cells, `["state", cell]`.

Omit `scope.treeFields` and `scope.planFields` to retain each complete record. A declared tree projection must retain at least `path`, `language`, `status` and `terms` and may retain `origins` and `placeholders`. A declared plan projection retains `path`, `language`, `status`, `plan`, `recorded`, `skips` and `ensure`. Nested field values stay whole.

For each subject, hash the canonical JSON array of selected values in selector order. Both the blind `oracle.rows` and independent `adapter.rows` contain the subject ID, `treeSha256` and `planSha256`. Each must match the corresponding native digest. Bind the full private oracle table with `oracle.rowsSha256`. A successful plan with status `unchecked` retains that status in its receipt.
