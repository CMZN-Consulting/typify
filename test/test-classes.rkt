#lang racket/base

(require json
         rackunit
         (rename-in rackunit/text-ui (run-tests rackunit-run-tests))
         racket/list
         racket/runtime-path
         racket/string
         "../src/classes.rkt")

(provide run-tests)

(define small-caps (hash 'states 4096 'repetition 32 'reference-depth 16 'token-length 16))

(define-runtime-path hostile-path "corpus/hostile.json")

(define (one source (caps small-caps))
  (hash-ref (compile-classes (hash "subject" source) #:caps caps) "subject"))

(define (error-code code)
  (lambda (error) (and (class-error? error) (equal? (class-error-code error) code))))

(define (checks source accepted refused)
  (define class (one source))
  (for ((text (in-list accepted))) (check-true (class-accepts? class text) text))
  (for ((text (in-list refused))) (check-false (class-accepts? class text) text)))

(define (class-suite gates)
  (test-suite
   "classes"
   (test-case
    "exact literals, whole-token matching and literal exclusions"
    (checks "\"safe\"" '("safe") '("other" "safely" ""))
    (checks "\"rw\" | \"ro\"" '("rw" "ro") '("r" "rwx" "RO" " rw"))
    (checks "\"a|b\" | \"c\"" '("a|b" "c") '("a" "b"))
    (checks "/[a-z]+/ except \"bad\" | \"evil\"" '("good" "baddy" "evils") '("bad" "evil" ""))
    (checks "/.*/ except \"\"" '("a" " ") '("")))
   (test-case
    "class syntax uses quoted literals and bare references"
    (check-exn (error-code "class-reference") (lambda () (one "safe")))
    (check-exn (error-code "class-reference") (lambda () (one "rw | ro")))
    (check-exn (error-code "class-syntax") (lambda () (one "<letter>")))
    (check-exn (error-code "class-syntax") (lambda () (one "/<letter>+/")))
    (check-exn (error-code "class-except") (lambda () (one "\"safe\" except unsafe")))
    (checks "(\"a\" | \"b\") (\"c\" | \"d\") except \"ac\" | \"bd\"" '("ad" "bc") '("ac" "bd"))
    (checks "(\"a\" except \"a\") | \"b\"" '("b") '("a"))
    (checks "/ab*/" '("a" "ab" "abbb") '("" "abab")))
   (test-case
    "the first unescaped slash closes a pattern in every context"
    (checks "/[a\\/]+/" '("/" "a/a") '("b"))
    (checks "/\"a\\/b\"/" '("a/b") '("ab"))
    (check-exn (error-code "class-syntax") (lambda () (one "/[a/]+/")))
    (check-exn (error-code "class-syntax") (lambda () (one "/\"a/b\"/"))))
   (test-case
    "regular operators and code-point escapes"
    (checks "/[a-c]{2,3}/" '("aa" "abc" "ccc") '("a" "abcd" "dd"))
    (checks "/(\"ab\" | \"cd\")+/" '("ab" "cdab") '("a" "abcdx" ""))
    (checks "/[0-9]?/" '("" "0" "9") '("00" "a"))
    (checks "/[0-9]*/" '("" "01234") '("x" "0x"))
    (checks "/[0-9]+/" '("0" "1234") '("" "1x"))
    (checks "/[a-z]{2}/" '("ab" "zz") '("a" "abc"))
    (checks "/[^=]+/" '("Algebra" "Market Research") '("" "a=b"))
    (checks "/\\/.*/" '("/" "/a/b") '("a/b" ""))
    (checks "/[\\u0041-\\u005A]+/" '("A" "AZ") '("a"))
    (checks "/\"\\U00010400\"/" (list (string (integer->char 66560))) '("A")))
   (test-case
    "pattern shorthand ranges and braced escapes are host-independent"
    (checks "/\\d+/" '("0" "902") '("" "A" "١"))
    (checks "/\\w+/" '("A_z09" "_") '("é" "-" " "))
    (checks "/\\s+/" '(" " "  ") (list "" "a" "\t" "\n" "\r"))
    (checks "/[\\dA-F]+/" '("F09" "3A") '("G" "a"))
    (checks "/[^\\s]+/" '("abc" "_") '(" a" "a "))
    (checks "/\\x41\\u0042\\u{1F600}/" '("AB😀") '("AB" "ABx"))
    (check-exn (error-code "class-syntax") (lambda () (one "/\\x+1/")))
    (check-exn (error-code "class-syntax") (lambda () (one "/\\u{}/")))
    (check-exn (error-code "class-syntax") (lambda () (one "/[\\d-z]/")))
    (check-exn (error-code "class-syntax") (lambda () (one "/\\u{D800}/"))))
   (test-case
    "unsafe-only and length-empty languages remain legal class definitions"
    (define unsafe (one "/[\\x00-\\x1f]/"))
    (check-true (class-empty? unsafe))
    (check-false (class-accepts? unsafe (string #\nul)))
    (check-false (class-empty? (one "/\\s/")))
    (check-false (class-empty? (one "/a*/")))
    (check-true (class-empty? (one "/a{4}/" (hash 'length 3)))))
   (test-case
    "empty witness records have no invented members or near misses"
    (for
     ((class (in-list
              (list
               (one "/[\\x00-\\x1f]/")
               (one "/a{4}/" (hash 'length 3))
               (one "\"a\" except \"a\"")))))
     (define witnesses (class-witnesses class))
     (check-equal? (sort (hash-keys witnesses) symbol<?) '(bounds empty))
     (check-true (hash-ref witnesses 'empty))
     (check-equal? (hash-ref witnesses 'bounds) (class-count-bounds class)))
    (define nonempty (class-witnesses (one "\"\"")))
    (check-equal? (sort (hash-keys nonempty) symbol<?) '(boundary bounds near-miss shortest))
    (check-equal? (hash-ref nonempty 'shortest) "")
    (check-equal? (hash-ref nonempty 'boundary) ""))
   (test-case
    "batch class errors preserve declaring definitions and independent successes"
    (define errors '())
    (define classes
      (compile-classes
       (hash
        "bad-pattern"
        "/[/"
        "bad-reference"
        "unknown"
        "dependent"
        "bad-pattern"
        "good"
        "\"ok\"")
       #:caps
       small-caps
       #:order
       '("bad-reference" "bad-pattern" "dependent" "good")
       #:on-error
       (lambda (error) (set! errors (append errors (list error))))))
    (check-equal? (map class-error-code errors) '("class-reference" "class-syntax"))
    (check-equal?
     (map (lambda (error) (hash-ref (class-error-data error) 'definition-name)) errors)
     '("bad-reference" "bad-pattern"))
    (check-equal? (hash-ref (class-error-data (car errors)) 'class) "unknown")
    (check-equal? (hash-ref (class-error-data (car errors)) 'definition-source) "unknown")
    (check-false (hash-has-key? classes "dependent"))
    (check-true (class-accepts? (hash-ref classes "good") "ok"))
    (define failure
      (with-handlers
       ((class-error? values))
       (compile-classes (hash "owner" "missing") #:caps small-caps)
       #f))
    (check-true (class-error? failure))
    (check-equal? (hash-ref (class-error-data failure) 'definition-name) "owner"))
   (test-case
    "reference semantics include referenced literal exclusions"
    (define classes
      (compile-classes
       (hash "plain" "/[a-z]+/ except \"bad\"" "alias" "plain" "suffix" "alias \"!\"")
       #:caps
       small-caps))
    (check-true (class-accepts? (hash-ref classes "alias") "good"))
    (check-false (class-accepts? (hash-ref classes "alias") "bad"))
    (check-true (class-accepts? (hash-ref classes "suffix") "good!"))
    (check-false (class-accepts? (hash-ref classes "suffix") "bad!"))
    (check-exn
     (error-code "class-reference-cycle")
     (lambda () (compile-classes (hash "a" "b" "b" "a") #:caps small-caps)))
    (check-exn
     (error-code "class-reference")
     (lambda () (compile-classes (hash "a" "missing") #:caps small-caps)))
    (check-exn
     (error-code "class-reserved")
     (lambda () (compile-classes (hash "letter" "\"a\"") #:caps small-caps))))
   (test-case
    "class descriptions retain the parser tree before reference expansion"
    (define classes
      (compile-classes
       (hash
        "choice"
        "\"book\" | \"journal\""
        "pattern"
        "/[^=]+/"
        "sequence"
        "choice /[0-9]+/"
        "mixed"
        "\"literal\" | choice"
        "repeat"
        "choice{1,3}"
        "open"
        "choice+"
        "excluded"
        "choice except \"book\""
        "references"
        "choice pattern choice")
       #:caps
       small-caps))
    (define (qualified name) (string-append "Body." name))
    (define (description name) (class->description (hash-ref classes name) #:qualify qualified))
    (check-equal? (description "choice") (hash 'literals '("book" "journal")))
    (check-equal? (description "pattern") (hash 'pattern "[^=]+"))
    (check-equal?
     (description "sequence")
     (hash 'seq (list (hash 'ref "Body.choice") (hash 'pattern "[0-9]+"))))
    (check-equal?
     (description "mixed")
     (hash 'alt (list (hash 'literals '("literal")) (hash 'ref "Body.choice"))))
    (check-equal?
     (description "repeat")
     (hash 'repeat (hash 'node (hash 'ref "Body.choice") 'min 1 'max 3)))
    (check-equal?
     (description "open")
     (hash 'repeat (hash 'node (hash 'ref "Body.choice") 'min 1 'max 'null)))
    (check-equal?
     (description "excluded")
     (hash 'except (hash 'node (hash 'ref "Body.choice") 'literals '("book"))))
    (check-equal? (description "letter") (hash 'builtin "letter"))
    (check-equal? (description "canonical-path") (hash 'builtin "canonical-path"))
    (check-equal? (description "relative-path") (hash 'builtin "relative-path"))
    (check-equal? (class->references (hash-ref classes "references")) '("choice" "pattern"))
    (check-equal? (class->references (hash-ref classes "letter")) '())
    (check-equal? (class->references (hash-ref classes "relative-path")) '())
    (check-equal?
     (class->description (hash-ref classes "open"))
     (hash 'repeat (hash 'node (hash 'ref "choice") 'min 1 'max 'null)))
    (define raw "a\\/.*")
    (define escaped (one (string-append "/" raw "/")))
    (check-equal? (class->description escaped) (hash 'pattern raw))
    (define foreign
      (compiled-class
       "foreign"
       "source"
       (compiled-class-machine escaped)
       (compiled-class-caps escaped)
       (compiled-class-alphabet escaped)))
    (check-exn (error-code "class-description") (lambda () (class->description foreign)))
    (check-exn (error-code "class-description") (lambda () (class->references foreign))))
   (test-case
    "relative paths are a separate reserved built-in language"
    (define relative (one "relative-path"))
    (for
     ((text '("a" "a/b" ".hidden" "a..b" "α/β")))
     (check-true (class-accepts? relative text) text))
    (for
     ((text '("" "/" "/a" "a/" "a//b" "." ".." "a/./b" "a/../b")))
     (check-false (class-accepts? relative text) text))
    (check-equal? (class->description relative) (hash 'ref "relative-path"))
    (check-exn
     (error-code "class-reserved")
     (lambda () (compile-classes (hash "relative-path" "\"x\"") #:caps small-caps))))
   (test-case
    "pattern membership is independent of the receiving capture length"
    (define path (one "canonical-path" (hash 'length 1)))
    (check-true (class-pattern-accepts? path "/long"))
    (check-false (class-accepts? path "/long"))
    (check-false (class-pattern-accepts? path "/a/../b"))
    (check-false (class-pattern-accepts? path (string-append "/a" (string #\nul)))))
   (test-case
    "Unicode letters are pinned, multiple scripts and non-BMP included"
    (define single (one "letter"))
    (check-true (class-accepts? single "A"))
    (check-false (class-accepts? single ""))
    (check-false (class-accepts? single "AA"))
    (define letters (one "letter+"))
    (for
     ((text (in-list (list "Arbor" "İstanbul" "Ελλάδα" "東京" "अकर" (string (integer->char 66560))))))
     (check-true (class-accepts? letters text) text))
    (check-false (class-accepts? letters "A1"))
    (check-false (class-accepts? letters "A B"))
    (check-false (class-accepts? letters (string (integer->char 769))))
    (check-equal? unicode-version "17.0.0")
    (check-equal?
     unicode-source-sha256
     "d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e"))
   (test-case
    "canonical absolute paths keep component and global safety constraints"
    (define path (one "canonical-path"))
    (check-true (class-accepts? path "/"))
    (check-false (class-accepts? path "/a/"))
    (for
     ((text (in-list '("/a" "/a/b" "/.config" "/..cache" "/..." "/東京" "/a b" "/a\\b"))))
     (check-true (class-accepts? path text) text))
    (for
     ((text (in-list '("" "." ".." "relative" "a/b" "//" "/a//b" "/./a" "/a/../b" "/a/." "/a/.."))))
     (check-false (class-accepts? path text) text))
    (for
     ((point (in-list '(0 10 160 8205 8232 8233))))
     (check-false (class-accepts? path (string-append "/a" (string (integer->char point))))))
    (check-exn
     (error-code "class-reserved")
     (lambda () (compile-classes (hash "canonical-path" "\"other\"") #:caps small-caps)))
    (define narrower (one "/\\/a[0-9]+/"))
    (check-false (class-difference-witness narrower path))
    (define broad (one "/\\/.*/"))
    (define counterexample (class-difference-witness broad path))
    (check-true (string? counterexample))
    (check-true (class-accepts? broad counterexample))
    (check-false (class-accepts? path counterexample)))
   (test-case
    "relative paths have clean components and remain distinct from absolute paths"
    (define path (one "relative-path"))
    (for
     ((text (in-list '("a" "a/b" ".hidden" "a..b" "a b/c" "α/β"))))
     (check-true (class-accepts? path text) text))
    (for
     ((text (in-list '("" "/" "/a" "a/" "a//b" "." ".." "a/./b" "a/../b"))))
     (check-false (class-accepts? path text) text))
    (check-false (class-accepts? path (string-append "a/" (string (integer->char 8205)))))
    (check-exn
     (error-code "class-reserved")
     (lambda () (compile-classes (hash "relative-path" "\"other\"") #:caps small-caps)))
    (check-false (class-overlap-witness path (one "canonical-path")))
    (define alias
      (hash-ref (compile-classes (hash "alias" "relative-path") #:caps small-caps) "alias"))
    (check-equal? (class->references alias) '("relative-path"))
    (check-false (class-difference-witness alias path))
    (check-false (class-difference-witness path alias)))
   (test-case
    "the class alphabet excludes unsafe categories, ordinary space remains"
    (define any (one "/.*/"))
    (check-true (class-accepts? any "Market Research"))
    (for
     ((point
       (in-list '(0 9 10 13 127 160 173 1564 6158 8203 8204 8205 8232 8233 8239 8288 12288 65279))))
     (check-false
      (class-accepts? any (string-append "a" (string (integer->char point)) "b"))
      (number->string point 16)))
    (check-false (class-accepts? (one "/.*/" (hash-set small-caps 'allow-ascii-space #f)) "a b")))
   (test-case
    "compact state caps and independent length counters"
    (define class (hash-ref (compile-classes (hash "subject" "/.*/")) "subject"))
    (check-equal? (vector-length (automaton-transitions (compiled-class-machine class))) 1)
    (check-equal? (hash-ref (compiled-class-caps class) 'states) 4096)
    (check-equal? (hash-ref (compiled-class-caps class) 'reference-depth) 8)
    (check-equal? (hash-ref (compiled-class-caps class) 'token-length) 4096)
    (check-true (class-accepts? class (make-string 4096 #\a)))
    (check-false (class-accepts? class (make-string 4097 #\a)))
    (check-equal? (class-work-bound class) 4097)
    (define longer (one "/a*/" (hash 'length 3)))
    (define shorter (one "/a*/" (hash 'length 1)))
    (define difference (class-difference-witness longer shorter))
    (check-true (string? difference))
    (check-equal? (string-length difference) 2)
    (check-true (class-accepts? longer difference))
    (check-false (class-accepts? shorter difference))
    (check-false (class-difference-witness shorter longer)))
   (test-case
    "resource caps reject rather than truncate their proofs"
    (check-exn (error-code "class-repetition-cap") (lambda () (one "/a{33}/")))
    (check-exn
     (error-code "class-reference-cap")
     (lambda ()
       (compile-classes
        (hash "a" "b" "b" "c" "c" "d" "d" "\"x\"")
        #:caps
        (hash-set small-caps 'reference-depth 2))))
    (check-exn
     (error-code "class-state-cap")
     (lambda () (one "/ab/" (hash-set small-caps 'states 2)))))
   (test-case
    "overlap and difference are exact products with concrete witnesses"
    (define left (one "/[a-c]+/"))
    (define right (one "/[c-e]+/"))
    (check-false (class-difference-witness left left))
    (define overlap (class-overlap-witness left right))
    (check-true (string? overlap))
    (check-true (class-accepts? left overlap))
    (check-true (class-accepts? right overlap))
    (define difference (class-difference-witness left right))
    (check-true (string? difference))
    (check-true (class-accepts? left difference))
    (check-false (class-accepts? right difference))
    (check-false (class-overlap-witness (one "\"a\"") (one "\"b\"")))
    (check-false (class-difference-witness left (one "/[a-z]+/"))))
   (test-case
    "literal containment and unsafe first-delimiter placement differ"
    (define class (one "/b*a?/"))
    (check-false (class-contains-literal-witness class "aa"))
    (define overlap (class-delimiter-witness class "aa"))
    (check-true (string? overlap))
    (check-true (class-accepts? class overlap))
    (check-true
     (<
      (caar (regexp-match-positions #rx"aa" (string-append overlap "aa")))
      (string-length overlap)))
    (check-false (class-delimiter-witness (one "/[a-z]+/") " = "))
    (define delimiter-hit (class-delimiter-witness (one "/[a-z=]+/") "="))
    (check-true (string? delimiter-hit))
    (check-true (class-accepts? (one "/[a-z=]+/") delimiter-hit)))
   (test-case
    "internal field algebra preserves classes and lexer restrictions"
    (define shared (class-intersection (one "/[a-c]+/") (one "/[b-d]+/")))
    (check-true (class-accepts? shared "bc"))
    (check-false (class-accepts? shared "a"))
    (check-false (class-accepts? shared "d"))
    (define source (one "/[ ab:]+/ except \"ab\""))
    (define colon-free (class-without-literal source ":"))
    (check-true (class-accepts? colon-free "ba"))
    (check-false (class-accepts? colon-free "a:b"))
    (check-false (class-accepts? colon-free "ab"))
    (check-true (class-accepts? source "a:b"))
    (define overlapping (class-without-literal (one "/[ab]*/") "aba"))
    (check-true (class-accepts? overlapping "abba"))
    (check-false (class-accepts? overlapping "ababa"))
    (check-true (class-accepts? overlapping ""))
    (define empty (class-without-literal (one "/[ab]*/") ""))
    (check-false (class-accepts? empty ""))
    (check-false (class-accepts? empty "a"))
    (define field (class-trimmed (one "/[ ab]*/")))
    (check-true (class-accepts? field ""))
    (check-true (class-accepts? field "a  b"))
    (check-false (class-accepts? field " a"))
    (check-false (class-accepts? field "a "))
    (check-false (class-accepts? field " ")))
   (test-case
    "form composition keeps per-capture length bounds"
    (define piece (one "/a+/" (hash-set small-caps 'token-length 4)))
    (define form (class-sequence (list "do : " piece " = " piece)))
    (check-true (class-accepts? form "do : aaaa = aaaa"))
    (check-false (class-accepts? form "do : aaaaa = a"))
    (check-false (class-accepts? form "do : a = aaaaa"))
    (define same (class-sequence (list "do : " (one "/[a-z]+/"))))
    (define overlap (class-overlap-witness form same))
    (check-false overlap))
   (test-case
    "long variable captures compose without a redundant total-length product"
    (define variable
      (one "/a+/" (hash 'states 4096 'repetition 256 'reference-depth 16 'token-length 256)))
    (define form (class-sequence (list variable "=" variable)))
    (define longest (string-append (make-string 256 #\a) "=" (make-string 256 #\a)))
    (check-true (class-accepts? form longest))
    (check-false (class-accepts? form (string-append (make-string 257 #\a) "=a")))
    (check-false (class-accepts? form (string-append "a=" (make-string 257 #\a))))
    (check-equal? (hash-ref (compiled-class-caps form) 'token-length) 513)
    (check-true (<= (vector-length (automaton-transitions (compiled-class-machine form))) 1024))
    (define tighter (class-sequence (list variable "=" variable) #:caps (hash 'token-length 20)))
    (check-true
     (class-accepts? tighter (string-append (make-string 9 #\a) "=" (make-string 10 #\a))))
    (check-false
     (class-accepts? tighter (string-append (make-string 10 #\a) "=" (make-string 10 #\a))))
    (define string-cap
      (class-sequence (list variable "=" variable) #:caps (hash "max-token-length" 20)))
    (check-equal? (hash-ref (compiled-class-caps string-cap) 'token-length) 20)
    (check-false (class-accepts? string-cap longest))
    (define looser (class-sequence (list variable "=" variable) #:caps (hash 'token-length 600)))
    (check-true (class-accepts? looser longest))
    (check-true (<= (vector-length (automaton-transitions (compiled-class-machine looser))) 1024)))
   (test-case
    "witnesses replay and GBNF uses escaped scalar ranges"
    (define class (one "/[a-c]{2,4}/"))
    (define witnesses (class-witnesses class))
    (check-equal? (hash-ref witnesses 'shortest) "aa")
    (check-equal? (hash-ref witnesses 'boundary) "cccc")
    (check-true (class-accepts? class (hash-ref witnesses 'shortest)))
    (check-true (class-accepts? class (hash-ref witnesses 'boundary)))
    (check-false (class-accepts? class (hash-ref witnesses 'near-miss)))
    (define grammar (class->gbnf class #:name "subject"))
    (check-true (string-contains? grammar "subject ::="))
    (check-true (string-contains? grammar "\\u0061-\\u0063"))
    (check-equal? grammar (class->gbnf class #:name "subject")))
   (test-case
    "boundary witnesses choose maximum length before lexicographic branch ties"
    (define (boundary source) (hash-ref (class-witnesses (one source)) 'boundary))
    (check-equal? (boundary "\"z\" | \"aa\"") "aa")
    (check-equal? (boundary "\"az\" | \"ba\"") "ba")
    (check-equal? (boundary "\"aa\" | \"az\"") "az")
    (check-equal? (boundary "/[a-z]/ \"a\" | \"a\" \"zz\"") "azz")
    (check-equal? (boundary "/[a-z]{2}/ except \"zz\"") "zy")
    (check-equal? (boundary "\"\"") "")
    (check-true (hash-ref (class-witnesses (one "\"a\" except \"a\"")) 'empty)))
   (test-case
    "unsupported host escapes and invalid ranges are rejected"
    (for
     ((source (in-list '("@[host]:predicate" "/\\q+/" "/[z-a]/" "/a{3,2}/" "/[a-z/" "/\\uD800/"))))
     (check-exn class-error? (lambda () (one source)) source)))
   (test-case
    "registered typed hostile pairs and exact delimiter cases"
    (define corpus (call-with-input-file hostile-path read-json))
    (define classes (compile-classes (hash-ref corpus 'classes) #:caps (hash-ref corpus 'caps)))
    (define cases (hash-ref corpus 'cases))
    (check-equal? (length cases) 107)
    (check-equal? (length (hash-ref corpus 'consumerClasses)) 7)
    (define hostile-failures
      (for/sum
       ((row (in-list cases)))
       (if (class-accepts? (hash-ref classes (hash-ref row 'class)) (hash-ref row 'token)) 1 0)))
    (printf
     "hostile-corpus total=~a failures=~a source-sha256=~a\n"
     (length cases)
     hostile-failures
     (hash-ref (hash-ref corpus 'provenance) 'sourceSha256))
    (hash-set!
     gates
     'hostile-corpus
     (hash
      'total
      (length cases)
      'failures
      hostile-failures
      'corpus
      "test/corpus/hostile.json"
      'provenance
      (hash-ref (hash-ref corpus 'provenance) 'sourceSha256)))
    (check-equal? hostile-failures 0)
    (for
     ((row (in-list (hash-ref corpus 'positiveControls))))
     (check-true (class-accepts? (hash-ref classes (hash-ref row 'class)) (hash-ref row 'token))))
    (define written-witnesses (make-hash))
    (define witness-failures
      (for/sum
       (((name class) (in-hash classes)))
       (define witnesses (class-witnesses class))
       (hash-set! written-witnesses name witnesses)
       (if (and (class-accepts? class (hash-ref witnesses 'shortest))
                (class-accepts? class (hash-ref witnesses 'boundary))
                (not (class-accepts? class (hash-ref witnesses 'near-miss))))
         0
         1)))
    (printf "class-witnesses total=~a failures=~a\n" (hash-count classes) witness-failures)
    (hash-set!
     gates
     'class-witnesses
     (hash
      'total
      (hash-count classes)
      'failures
      witness-failures
      'classes
      (hash-count classes)
      'written
      (hash-count written-witnesses)
      'witnesses
      (make-immutable-hash (hash->list written-witnesses))))
    (check-equal? witness-failures 0)
    (define (first-literal text literal)
      (for/first
       ((index (in-range (add1 (- (string-length text) (string-length literal)))))
        #:when
        (string=? literal (substring text index (+ index (string-length literal)))))
       index))
    (define delimiter-cases (hash-ref corpus 'delimiterCases))
    (define delimiter-failures
      (for/sum
       ((row (in-list delimiter-cases)))
       (define class (one (hash-ref row 'class)))
       (define literal (hash-ref row 'literal))
       (define witness (class-delimiter-witness class literal))
       (define expected (hash-ref row 'unsafe))
       (if (and (eq? (and witness #t) expected)
                (or (not witness)
                    (and (class-accepts? class witness)
                         (<
                          (first-literal (string-append witness literal) literal)
                          (string-length witness)))))
         0
         1)))
    (printf
     "delimiter-emptiness total=~a failures=~a\n"
     (length delimiter-cases)
     delimiter-failures)
    (hash-set!
     gates
     'delimiter-emptiness
     (hash 'total (length delimiter-cases) 'failures delimiter-failures))
    (check-equal? delimiter-failures 0))))

(define (run-tests)
  (define gates (make-hash))
  (define suite (class-suite gates))
  (define count
    (foldts-test-suite
     (lambda (_suite _name _before _after seed) seed)
     (lambda (_suite _name _before _after _seed children) children)
     (lambda (_case _name _action seed) (add1 seed))
     0
     suite))
  (define failures (rackunit-run-tests suite 'normal))
  (hash
   'suite
   "classes"
   'count
   count
   'pass
   (- count failures)
   'fail
   failures
   'gates
   (make-immutable-hash (hash->list gates))))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
