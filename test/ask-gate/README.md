# Typify v1 Stage-Three `ask` Gate

## Reference Oracle

- **Source**: `reference/ref_ask.py`
- **Language & Version**: `Python 3.15.0` (standard library only)

## Generation and Single-Word Commands

Run from `test/ask-gate/`:

```sh
python3 reference/ref_ask.py --generate-gate --decl declarations/ask-gate.typify --lang AskGate --out gate.json
```

Single-word invocation (prints the `--json` object and exits with status `0`, `1`, or `2`):

```sh
python3 reference/ref_ask.py declarations/ask-gate.typify AskGate <word>
```

## Gate Summary (`gate.json`)

- **Declaration**: `declarations/ask-gate.typify`
- **Language**: `AskGate`
- **Candidates (`15`)**: `Blpha`, `abcwxyz`, `alias-view`, `alpha`, `bind`, `emit-op`, `letter`, `mark-ready`, `module`, `mount-point`, `nodule`, `read-only-mount`, `ready`, `record-entry`, `tag-letter`
- **Set sizes**:
  - `typo`: `16616`
  - `unknown`: `33`
  - `witness`: `11`
  - **Total**: `16660`

## Witness Table (Pinned Readings vs. Rival Readings)

| Pinned Reading (`14:59:52Z` / `15:14:08Z` / `15:20:48Z` / `15:27:56Z` / `17:41:38Z`) | Rival Reading | Witness Word | Set in `gate.json` | Expected (`ref_ask.py`) | Rival Answer |
| :--- | :--- | :--- | :--- | :--- | :--- |
| `14:59:52Z` item 7: Optimal String Alignment distance (no substring edited twice) | Unrestricted Damerau-Levenshtein distance | `cawxyz` | `witness` | `near: []` (OSA distance `3 > 2` to `abcwxyz`) | `near: ["abcwxyz"]` (unrestricted Damerau distance `2 <= 2`) |
| `14:59:52Z` item 7: Case is significant | Case-insensitive matching | `Alpha` | `typo` | `near: ["Blpha", "alpha"]` (both at distance `1`) | `near: ["alpha"]` (case-insensitive distance `0`) |
| `14:59:52Z` item 8: Bound `max(n, 3) // 3` capped at `2` with `n = len(word)` | Alternative bound with `n = min(len(word), len(candidate))` | `unbind` | `witness` | `near: ["bind"]` (`len("unbind") = 6`, bound `2`, distance `2`) | `near: []` (`min(6, 4) = 4`, bound `1 < 2`) |
| `14:59:52Z` item 9: Candidates at the least distance only (ties in code-point order) | Every candidate within the bound, by distance then code point | `modules` | `typo` | `near: ["module"]` (distance `1` only) | `near: ["module", "nodule"]` (distance `1` and distance `2`) |
| `14:59:52Z` item 9: Shared hyphen-parts fallback when no candidate is within bound | No hyphen-parts fallback | `mount` | `unknown` | `near: ["mount-point", "read-only-mount"]` | `near: []` |
| `14:59:52Z` item 6: Candidates include `term` and `primitive` constructors as well as form verbs | Form verbs only as candidates | `emit-ox` | `typo` | `near: ["emit-op"]` (`emit-op` is a formless primitive) | `near: []` |
| `14:59:52Z` item 6: Classes are not candidates | Declared `is` classes offered as candidates | `taxonomx` | `witness` | `near: []` (`taxonomy` is an `is` class) | `near: ["taxonomy"]` |
| `14:59:52Z` item 6: Cells are not candidates | Declared `cell` names offered as candidates | `ledgex` | `witness` | `near: []` (`ledger` is a `cell`) | `near: ["ledger"]` |
| `14:59:52Z` item 6: Meta body's words are not candidates unless `Typify` is asked | Meta body words offered as candidates in user bodies | `declarx` | `witness` | `near: []` (`declare` is a meta word) | `near: ["declare"]` |
| `14:59:52Z` item 13: `ask` reads the asked language alone | All bodies in the declarations file searched | `foreign-op` | `witness` | `status: "undeclared"`, `near: ["emit-op"]` (via shared part `op`) | `status: "declared"` (from `SiblingLang`) |
| `15:14:08Z` item 3 & `15:27:56Z` item 3: Built-in class spellings `canonical-path`, `relative-path` (with no form bearing them) are `undeclared` and never candidates | Built-in classes treated as declared or candidates | `canonical-path`, `relative-path` | `witness` | `status: "undeclared"`, `near: []` | `status: "declared"` or `near: [<builtin>]` |
| `17:41:38Z` item 1: Word declared as both a form verb and a class answers the union (forms/constructors first, then `is` line once) | Only form/constructor lines or only class `is` line returned | `alias-view` | `witness` | `status: "declared"`, verb/constructor lines followed by `? : is : alias-view = "a" \| "b"` | Omits trailing `? : is : alias-view = "a" \| "b"` or omits form/constructor lines |
| `17:41:38Z` item 3: Form verb spelled `letter` is legal, declared, and a candidate, with no `is` line for the built-in `letter` | `letter` excluded from declared words/candidates as a built-in spelling | `letter` / `lettex` | `witness` / `typo` | `letter` -> `status: "declared"` (`form : letter : <word>`, no `is : letter` line); `lettex` -> `near: ["letter"]` | `letter` -> `status: "undeclared"`; `lettex` -> `near: []` |
| `17:41:38Z` item 4: Well-spelled `? : <word>` lines inside (`? : module`) and outside (`? : bind`) a body in a declarations file are passed over and declare nothing | Question lines in declarations file rejected or treated as declarations | all `16660` words (plus corpus case `101` for malformed `? : 123bad!`) | `typo` / `unknown` / `witness` | `diagnostics: []`, `exit_code: 0` (`101` -> `exit_code: 1`, `E_QUESTION`) | Declaration error on well-spelled `? : <word>` in `declarations/ask-gate.typify` |
| `14:59:52Z` item 3 & `15:20:48Z` item 2: `_`-for-`-` in unknown set fails D5 verb class | Verb class not checked or `_` admitted | `mount_point` | `unknown` | `exit_code: 2`, `E_USAGE` (`actual: "mount_point"`) | `exit_code: 0` |
| `15:20:48Z` item 5: Bang form `form : ready!` has candidate `ready`; `ready!` fails verb class | `ready!` treated as candidate or accepted verb | `readx` / `ready!` | `typo` / `witness` | `readx` -> `near: ["ready"]`; `ready!` -> `exit_code: 2`, `E_USAGE` | `readx` -> `near: []`; `ready!` -> `exit_code: 0` |
