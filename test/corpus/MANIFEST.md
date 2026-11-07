# Typify v1 Blind Verdict Corpus (v2) — MANIFEST

**Corpus Revision**: `v2` (updated for the `06:46:35Z`, `06:54:15Z`, `07:01:08Z`, and `07:33:42Z` corrections to `typify-v1-definition-2026-11-07.md`).

This corpus contains **100** blind test cases (`001`–`100`) derived strictly from the text of the Typify v1 definition (`typify-v1-definition-2026-11-07.md`, including Amendments 1 `A`–`D`, Amendment 2 `E`–`Q`, Amendment 3 `R`–`Z`, and all corrections through `07:33:42Z`). Every expected value carries provenance tags (`§1`–`§14`, `A`–`Z`) derived solely from the specification text.

## Cases Changed in `v2` (`06:46Z`, `06:54Z`, `07:01Z`, and `07:33Z` Corrections)

- **Declaration structure & `E_DECLARATION` 12 kinds (`007`–`011`, `018`–`022`, `052`–`053`, `068`, `087`, `095`, `096`, `098`)**: Updated to cover all 12 `E_DECLARATION` kinds (`header`, `body`, `duplicate`, `orphan`, `arity`, `capture`, `syntax`, `class-cycle`, `stratification`, `reference`, `conflict`, `effect`) with the payload conventions of `06:54Z` and `07:33Z`.
- **Shadowing, arity, and strict order payloads (`030`, `053`, `062`)**: Updated `E_FORM_SHADOW` in `030` (`form` = later shadowed form, `actual` = earlier shadowing form), `E_TERM_CLASS` `arity` in `053` (`class` = signature `"word word"`, `actual` = `1`), and `E_ORDER` in `062` (`expected` = `1`, `actual` = `0`, zero-based form ordinals).
- **Unified `origin` objects (`064`–`067`, `079`–`083`, `087`–`091`, `093`–`094`)**: Updated every term/state origin to `{"kind": "program", "path": ..., "line": n}` or `{"kind": "default", "path": ..., "line": n}` (`07:01Z`).
- **Positional `ensure` bindings (`<1>`..`<n>`), cell-named state predicates `(visible <cell> <key>)` / `(absent <cell> <key>)`, complete `join` tables, and `E_CONTEXT` (`079`–`081`, `083`, `085`, `087`, `089`–`090`, `094`)**: Updated `Mounts` (`079`, `080`, `089`, `090`) to `ensure : share : child-visible = (visible mounts <1>)`, `083` to `ensure : del-kv : key-absent = (absent store <1>)`, `081` to list unordered distinct pairs once, `085` to test `E_JOIN_LAW` with `expected: "complete"`, `087` to use `E_DECLARATION` kind `effect`, and `094` to test `E_CONTEXT` (`07:01Z`, `07:33Z`).

## Case Index (`001`–`100`)

| Case | Verdict | Sections & Amendments Covered | Summary |
| :--- | :--- | :--- | :--- |
| `001-line-shapes-all-clean` | `accepted` | `§2`, `§3`, `§4`, `§5`, `R`, `T`, `Y` | All three fixed kernel line shapes, outer blank stripping at split, and colon priority over arrow |
| `002-line-custom-comment-prefix` | `accepted` | `§2`, `§3`, `R`, `T`, `Y` | Declaration of custom comment prefix via 'comment : ;' |
| `003-err-line-malformed-bare-word` | `refused` | `§2`, `§9`, `K`, `R`, `Y` | Malformed kernel line: bare word with no ':', '->', or trailing '!' |
| `004-err-line-malformed-empty-colon-verb` | `refused` | `§2`, `§9`, `K`, `R`, `Y` | Malformed kernel line: colon line with empty verb before ':' |
| `005-err-line-malformed-empty-bang-verb` | `refused` | `§2`, `§9`, `K`, `R`, `Y` | Malformed kernel line: bang line whose verb begins with a digit |
| `006-err-line-malformed-arrow-missing-side` | `refused` | `§2`, `§9`, `K`, `R`, `Y` | Malformed kernel line: arrow line with empty right-hand side |
| `007-err-file-missing-lang-header` | `refused` | `§3`, `§9`, `K`, `R`, `Y` | Declaration file missing '#lang typify' on line 1 |
| `008-err-body-line-outside-declare` | `refused` | `§3`, `§9`, `K`, `R`, `Y` | Declaration line outside any 'declare : <Name> ... end!' body |
| `009-err-body-nested-declare` | `refused` | `§3`, `§9`, `K`, `Y` | Nested 'declare : InnerLang' before closing 'end!' of outer body |
| `010-err-body-unclosed-declare` | `refused` | `§3`, `§9`, `K`, `R`, `Y` | Unclosed 'declare : UnclosedLang' reaching EOF without 'end!' |
| `011-err-body-duplicate-language` | `refused` | `§3`, `§9`, `K`, `R`, `Y` | Duplicate language declaration 'declare : DupLang' in the same file |
| `012-err-body-invalid-version-and-prose` | `refused` | `§3`, `§9`, `K`, `L`, `R`, `Y` | Invalid non-integer version and forbidden prose verb inside declaration body |
| `013-class-literal-alternatives` | `accepted` / `refused` | `§4`, `F`, `K`, `R`, `T`, `Y` | Class literal alternatives with quoted literals: accepted exact match and refused non-member with range |
| `014-class-ranges-and-negated-ranges` | `accepted` | `§4`, `F`, `J`, `R`, `T`, `Y` | Slash-delimited character ranges and negated range /[^=]+/ intersecting allowed code points (03:56Z, 05:09Z) |
| `015-class-grouping-and-quantifiers` | `accepted` | `§4`, `F`, `R`, `T`, `Y` | Regular-core grouping, optional ?, + and bounded {m,n} quantifiers with lowered cap : repetition = 16 |
| `016-class-acyclic-references-and-builtin-letter` | `accepted` / `refused` | `§4`, `§14`, `F`, `K`, `R`, `T`, `Y` | Acyclic bare-name class reference and built-in single-letter class 'letter' juxtaposed with slash pattern (05:09Z) |
| `017-class-sequence-and-except` | `accepted` / `refused` | `§4`, `F`, `K`, `Q`, `R`, `T`, `Y` | Class juxtaposition sequence and weakest-binding finite literal exclusion (<class> except "a" | "b") |
| `018-err-class-host-predicate-forbidden` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: host regex escape outside regular core and unquoted bare word naming no class |
| `019-err-class-matches-control-wildcard` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: class range explicitly names forbidden Cc control code points |
| `020-err-class-matches-zwj-and-bidi` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: class explicitly names U+200D (ZWJ), U+202E (Bidi RLO), and U+00A0 (Zs != U+0020) |
| `021-err-class-cyclic-reference` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: cyclic class reference rejected |
| `022-err-class-unbound-placeholder` | `refused` | `§4`, `§5`, `K`, `R`, `Y` | Declaration error: placeholder in form without class declaration and placeholder in does not bound by form |
| `023-err-class-cap-automaton-states-exceeded` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: raising cap above default (8192 > 4096), bounded repetition > 256, and compact pattern automaton state cap > 4096 (04:44Z) |
| `024-err-class-cap-reference-depth-exceeded` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Declaration error: class reference chain depth exceeds cap (8) |
| `025-err-class-cap-token-length-exceeded` | `refused` | `§4`, `F`, `K`, `R`, `Y` | Program error: captured token on '/.+/' (1 control state per 04:44Z, '.' in regular core per 05:09Z) exceeds independent length counter 'cap : length = 16' |
| `026-err-emptiness-check-greedy-char-class` | `refused` | `§4`, `F`, `J`, `K`, `R`, `Y` | Declaration error: product-automaton emptiness check catches class matching its following '=' delimiter |
| `027-err-emptiness-check-overlap-ab-equal` | `refused` | `§4`, `F`, `J`, `K`, `R`, `Y` | Declaration error: product-automaton emptiness check rejects 'a' | 'a=b' before '=' and 'b' | 'b=c' |
| `028-err-emptiness-check-overlap-multichar-delim` | `refused` | `§4`, `F`, `J`, `K`, `R`, `Y` | Declaration error: product-automaton emptiness check rejects class containing multi-char delimiter ' = ' |
| `029-emptiness-check-sound-passes` | `accepted` | `§4`, `F`, `J`, `R`, `T`, `Y` | Product-automaton emptiness check passes for 'is : title = /[^=]+/' (05:09Z) separated from category by ' = ' |
| `030-err-form-shadowing-and-adjacent-slots` | `refused` | `§4`, `F`, `J`, `K`, `R`, `Y` | Declaration error: adjacent placeholders without literal delimiter and overlapping forms with non-disjoint automata |
| `031-hostile-universal-nul-and-esc` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (14 pairs): NUL (U+0000) and ESC (U+001B) refused across all 7 classes |
| `032-hostile-universal-cr-lf-del` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (14 pairs): CR (U+000D) and DEL (U+007F) refused across all 7 classes |
| `033-hostile-universal-bidi-and-linesep` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (14 pairs): Bidi RLO (U+202E) and Line Separator (U+2028) refused across all 7 classes |
| `034-hostile-universal-zwsp-bom-zwj` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (21 pairs): ZWSP (U+200B), BOM (U+FEFF), and ZWJ (U+200D per 03:56Z) refused across all 7 classes |
| `035-hostile-universal-homoglyph-and-combining` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (14 pairs): Cyrillic U+0430 homoglyph, combining acute U+0301, and NBSP U+00A0 refused across all 7 classes |
| `036-hostile-path-noncanonical-6` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y`, `H` | Hostile audit (10 canonical-path pairs): Storm 07 Gauss 6 class-specific path attacks + 8 non-canonical path spellings refused by built-in canonical-path (05:09Z) |
| `037-hostile-relpath-noncanonical-6` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (6 relpath pairs): traversal, leading slash, double slash, and trailing slash refused |
| `038-hostile-days-numeric-5` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (5 days pairs): negative, leading zero '007', overflow, scientific, and hex refused |
| `039-hostile-label-edge-spaces-2` | `refused` | `§2`, `§4`, `F`, `K`, `Q`, `R`, `Y` | Hostile audit: untrimmed capture inside '[ a ]' (04:19Z correction) and leading, trailing, or double internal spaces in label refused |
| `040-hostile-argv-json-escapes-8` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (8 argv pairs): unquoted argv, empty token, unescaped spaces, dangling backslash, and bad escapes refused |
| `041-hostile-identity-and-layout-3` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (5 ident/layout pairs): leading hyphen '-flag', leading digit, uppercase ident, and invalid layout enum refused |
| `042-hostile-percent-and-specifier-encodings` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y` | Hostile audit (5 encoding pairs): traversal and systemd specifier (%n, %i) spoofing refused |
| `043-hostile-gbnf-emission-injection` | `refused` | `§4`, `F`, `K`, `Q`, `R`, `U`, `Y`, `§8` | Hostile audit (3 grammar-injection pairs): tokens containing GBNF metasymbols '::=' and '[' refused |
| `044-multislot-1-catalog-item` | `accepted` | `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 1: Catalog 'item : <category> = <title>' with delimiter ' = ' |
| `045-multislot-2-systemd-bind-paths` | `accepted` | `§4`, `§5`, `F`, `J`, `Q`, `R`, `T`, `Y` | Multi-slot form 2: Three-slot 'bind : <path>:<path>:<mode>' with ':' delimiters |
| `046-multislot-3-http-header-rule` | `accepted` | `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 3: Bracketed verb and arrow delimiter 'header : [<verb>] <hname> => <hval>' |
| `047-multislot-4-network-route` | `accepted` | `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 4: Four-slot network route 'route : <ipv4>/<prefix> via <ipv4> dev <iface>' |
| `048-multislot-5-package-pin` | `accepted` | `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 5: Three-slot package pin 'pin : <pkg>@<ver>#<sha>' with '@' and '#' delimiters |
| `049-multislot-6-acl-grant` | `accepted` | `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 6: Parenthesized tuple 'grant : (<role>, <perm>) on <path>' |
| `050-multislot-7-arrow-rewrite-form` | `accepted` | `§2`, `§4`, `§5`, `F`, `J`, `R`, `T`, `Y` | Multi-slot form 7: Four-slot form with ':' and '->' delimiters |
| `051-terms-nested-and-literals` | `accepted` / `refused` | `§5`, `I`, `K`, `Q`, `R`, `T`, `U`, `Y` | First-order terms (04:19Z §5): parenthesized library call (join <path> <seg>) and literal constant "ro" accepted; nested user constructor (wrap <path>) at string position rejected |
| `052-err-form-without-does-and-does-without-form` | `refused` | `§4`, `§5`, `K`, `R`, `Y` | Declaration error: does without preceding form and form without paired does |
| `053-err-does-undeclared-term-and-arity` | `refused` | `§5`, `K`, `R`, `Y` | Declaration error: does supplies wrong arity to declared term or names undeclared term |
| `054-err-example-fails-form-and-orphan-example` | `refused` | `§4`, `K`, `R`, `Y` | Declaration error: orphan example before any form and example failing its form's class |
| `055-count-one-accepted-and-refused-zero` | `accepted` / `refused` | `§4`, `G`, `K`, `R`, `T`, `X`, `Y` | Count 1 (Amendment X): accepted when present once, refused when missing (0 occurrences), and withdrawn spelling 'one' rejected |
| `056-count-one-refused-duplicate` | `refused` | `§4`, `G`, `K`, `R`, `X`, `Y` | Count 1 (Amendment X): refused when constructor appears twice |
| `057-count-at-most-one-zero-one-two` | `accepted` / `refused` | `§4`, `G`, `K`, `R`, `T`, `X`, `Y` | Count 0..1 (Amendment X): accepted at 0 and 1 occurrences, refused at 2 occurrences |
| `058-count-at-least-one-zero-one-many` | `accepted` / `refused` | `§4`, `G`, `K`, `R`, `T`, `X`, `Y` | Count 1..* (Amendment X): refused at 0 occurrences, accepted at 1 or more occurrences |
| `059-count-any-default-multiplicity` | `accepted` | `§4`, `G`, `R`, `T`, `X`, `Y` | Count * (Amendment X): accepts 0, 1, or many occurrences |
| `060-form-default-line-when-omitted` | `accepted` | `§4`, `G`, `R`, `T`, `X`, `Y` | Form with default line supplies default term when omitted and uses explicit line when present |
| `061-err-restricting-form-fails-open` | `refused` | `§4`, `G`, `K`, `R`, `X`, `Y` | Declaration error: form marked 'restricts!' (Amendment X) with optional count 0..1 and no default line rejected |
| `062-order-strict-accepted-and-refused` | `accepted` / `refused` | `G`, `K`, `R`, `T`, `X`, `Y` | Strict order ('order : strict', Amendment X): accepted when forms follow declaration order contiguously, refused when out of order |
| `063-order-free-permuted-accepted` | `accepted` | `G`, `R`, `T`, `X`, `Y` | Free order (default when 'order : strict' is omitted): accepts interleaved forms in any order |
| `064-tree-present-and-slots` | `accepted` | `§7`, `§9`, `G`, `I`, `R`, `T`, `X`, `Y` | Tree declaration: present files matched, placeholder <service> bound across directory tree, and inner terms checked with origins |
| `065-tree-optional-defaulted` | `accepted` | `§7`, `§9`, `G`, `R`, `T`, `X`, `Y` | Tree optional program with default: absent config.typify reported as 'defaulted' and supplies default term |
| `066-tree-optional-absent-empty` | `accepted` | `§7`, `§9`, `G`, `R`, `T`, `X`, `Y` | Tree optional program without default: absent notes.typify reported as 'absent-empty' contributing zero terms |
| `067-err-tree-required-missing` | `refused` | `§7`, `§9`, `G`, `K`, `R`, `X`, `Y` | Tree error: required program (count = 1) absent from directory reported as 'missing' and refused |
| `068-sees-valid-and-undeclared-language` | `refused` | `§3`, `§7`, `K`, `R`, `Y` | Declaration error: 'sees' references a language not declared in the file |
| `069-ask-declared-form-and-rule` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `X`, `Y` | Question form (Amendment S): declared word 'item' answered with form, transitive classes, term, does, count, missing : error, and example in exact order (no status line) |
| `070-ask-declared-with-default` | `accepted` | `§6`, `§9`, `G`, `I`, `R`, `S`, `X`, `Y` | Question form (Amendment S): declared word 'access' with default line reports '? : missing : default : access : ro' |
| `071-ask-undeclared-near-damerau-single-edit` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `Y` | Question form (Amendment I, S): undeclared word 'bindd' returns '? : near : bind' (distance 1 <= threshold 1) and omits 'accepts' |
| `072-ask-undeclared-near-scaled-threshold-short-word` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `Y` | Question form (Amendment I, S): length-scaled threshold max(1, floor(L/4)) = 1 for 'cat' matches 'act' (transposition=1) and 'cut' (substitution=1) sorted lexicographically, excluding 'dog' (distance 3) |
| `073-ask-undeclared-near-hyphen-parts-and-ties` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `Y` | Question form (Amendment I, S): hyphen-split part matching finds 'mount-point' and 'read-only-mount' (part 'mount' distance 0) with ties broken lexicographically |
| `074-ask-undeclared-accepts-menu` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `Y` | Question form (Amendment I, S): undeclared word 'book' accepted by first placeholder <category> of form 'item : <category> = <title>' |
| `075-ask-undeclared-neither-near-nor-accepts` | `accepted` | `§6`, `§9`, `I`, `R`, `S`, `Y` | Question form (Amendment I, S): undeclared word 'price' with neither near words nor accepting first-slot forms emits only '? : price = undeclared' |
| `076-err-ask-invalid-verb-not-echoed` | `refused` | `§2`, `§6`, `I`, `K`, `R`, `S`, `Y` | Question form error: '? : <subject>' with invalid verb token rejected at column 5 without echoing the invalid token |
| `077-ask-ignored-in-program-count-and-order` | `accepted` | `§6`, `G`, `I`, `R`, `S`, `T`, `X`, `Y` | Question lines '? : <word>' inside a strict-order program do not disturb 'order : strict', counts, or emitted terms |
| `078-ask-meta-self-representation` | `accepted` | `§6`, `§14`, `I`, `R`, `S`, `X`, `Y` | Question form on '?' itself ('? : ?') in the self-describing meta declaration (Amendment S: no status line, missing : none) |
| `079-mounts-order-correct-mask-then-bind` | `accepted` | `§5`, `§9`, `B`, `H`, `I`, `Q`, `R`, `T`, `U`, `V`, `W`, `Y` | Mounts example ('rule : share <p> <m> -> mask (parent <p>) ; bind <p> <m>', 06:32Z): shadow stacking leaves /mounts/research unmasked and satisfies ensure : share : child-visible |
| `080-err-mounts-order-wrong-bind-then-mask` | `refused` | `§5`, `§9`, `B`, `H`, `I`, `K`, `Q`, `R`, `T`, `U`, `V`, `W`, `Y` | Mounts example in wrong order ('rule : share <p> <m> -> bind <p> <m> ; mask (parent <p>)'): ancestor put /mounts masks child /mounts/research and fails E_ENSURE |
| `081-cell-law-join-commutative-order-free` | `accepted` | `H`, `Q`, `R`, `T`, `U`, `V`, `Y` | Cell law 'cell : perms = join modes' (05:28Z U): unordered pair '"ro" "rw" = "rw"' listed once derives symmetric entry '"rw" "ro" = "rw"' |
| `082-cell-law-first-keeps-initial` | `accepted` | `H`, `R`, `T`, `U`, `Y` | Cell law 'cell : reg = first' (Amendment U): first write to a key wins and subsequent write to same key leaves state unchanged |
| `083-cell-write-delete-stage-reset` | `accepted` | `H`, `I`, `R`, `T`, `U`, `V`, `Y` | Cell write 'writes : del-kv : store = delete 1' with 'stage : del-kv = 1' removes key and satisfies (absent <k>) |
| `084-err-cell-join-table-not-commutative` | `refused` | `H`, `K`, `Q`, `R`, `U`, `Y` | Declaration error (05:28Z U): unordered pair {"ro", "rw"} listed twice in join table with conflicting results rejected |
| `085-err-cell-join-table-not-associative-or-idempotent` | `refused` | `H`, `K`, `Q`, `R`, `U`, `Y` | Declaration error (07:33Z U): join table over 'low' | 'mid' | 'high' missing unordered pair '"low" "high"' rejected with E_JOIN_LAW (expected: 'complete') |
| `086-err-cell-shadow-noncanonical-key-class` | `refused` | `H`, `K`, `Q`, `R`, `U`, `Y` | Declaration error (05:09Z U): 'cell : mounts = shadow stacking' keyed by class 'raw_path' not a subset of built-in 'canonical-path' rejected |
| `087-err-plan-unmodelled-primitive-unchecked` | `refused` / `unchecked` | `H`, `K`, `R`, `T`, `U`, `Y` | Primitive without writes rejected unless 'effect : <primitive> = external' is declared; external primitive makes plan report modelled '1 of 2' and 'unchecked' (Amendment U, Y) |
| `088-reads-fact-skip-when-absent` | `accepted` | `H`, `I`, `Q`, `R`, `T`, `U`, `V`, `W`, `Y` | Fact precondition 'reads : bind : exists = skip' and optional '(if (exists <p>) ...)' (Amendments U, V, W): absent path skipped cleanly |
| `089-err-reads-fact-reject-when-absent` | `refused` | `H`, `K`, `Q`, `R`, `T`, `U`, `V`, `W`, `Y` | Plan error: 'reads : bind : exists = reject' refuses plan when required path fact '/mounts/research' is absent from facts.json |
| `090-ensure-origin-distinguishes-writer` | `refused` | `H`, `I`, `K`, `Q`, `R`, `T`, `U`, `V`, `W`, `Y` | Cross-line origin tracking: second 'share : /mounts/beta ro' re-masks '/mounts' at step 3, hiding '/mounts/alpha' from line 1 and naming both origins in the failing check (K, U, Y) |
| `091-library-all-eight-functions-clean` | `accepted` | `§5`, `E`, `H`, `I`, `Q`, `R`, `T`, `U`, `V`, `Y` | All 8 library functions & predicates (06:00Z & 06:05Z E): parent, join, split, ancestors, concat, prefix, suffix, equal with ';' rule sequence |
| `092-err-library-parent-of-root-and-eval-order` | `refused` | `§5`, `E`, `I`, `J`, `K`, `Q`, `R`, `T`, `U`, `Y` | Library errors (06:05Z E, 06:36Z K): E_LIBRARY_PARENT on '(parent "/")' and E_LIBRARY_DOMAIN on '(join "/mounts" "/abs")' |
| `093-rule-foreach-under-fuel-cap` | `accepted` / `refused` | `E`, `H`, `I`, `K`, `R`, `T`, `U`, `V`, `Y` | Bounded iteration '(each <x> in (split <p>) ...)' with 'fuel : batch = 3' (06:05Z E, 06:36Z K): accepted at 3 components, refused with E_RULE_FUEL at 4 components |
| `094-rule-context-lookup-and-program-guard` | `accepted` | `§7`, `H`, `I`, `Q`, `R`, `T`, `U`, `V`, `W`, `X`, `Y`, `Z` | Root plan case (05:09Z W, Z): carries facts.json, resolves '(context base-dir)' with 'scope : base-dir = root' and 'guard : mount-rel = ...', and prints 'programs' + final 'state' |
| `095-err-rule-missing-action-for-constructor` | `refused` | `H`, `K`, `R`, `T`, `U`, `Y` | Declaration error (Amendment T): constructor in a body with primitives is neither primitive, rewritten by a rule, nor declared 'fact : <constructor>' |
| `096-err-rule-cycle-and-unstratified-forward-ref` | `refused` | `H`, `K`, `R`, `T`, `U`, `Y` | Declaration error: cyclic / unstratified rule expansion rejected at declaration time |
| `097-err-rule-duplicate-for-constructor` | `refused` | `H`, `K`, `R`, `T`, `U`, `Y` | Declaration error: two rules for the same constructor rejected with E_RULE_DUPLICATE |
| `098-err-rule-nonvariable-left-side` | `refused` | `H`, `K`, `R`, `T`, `U`, `Y` | Declaration error: rule left-hand side with literal argument instead of variable placeholder rejected |
| `099-emit-json-and-gbnf-catalog` | `accepted` | `§8`, `§9`, `F`, `J`, `R`, `X`, `Y` | Grammar emission ('typify emit', 06:00Z, 06:05Z, 06:20Z J): emits JSON grammar with comment, caps, qualified classes, and forms by default, and GBNF with '--gbnf' |
| `100-diagnostic-sarif-lsp-nonbmp-and-exit2` | `refused` / `usage_or_io_error` | `§9`, `K`, `R`, `T`, `Y` | Diagnostic range on non-BMP code point U+1F680 (start col 9 byteOffset 11, exclusive end col 12 byteOffset 14 per 05:28Z & 05:30Z), SARIF/LSP projections, and exit code 2 |
