#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/emission.rkt"
         "../src/language.rkt"
         "test-decoder.rkt")

(provide run-tests)

(define (external-primitive-fixture lines)
  (append
   lines
   (for/list
    ((line (in-list lines)) #:when (string-prefix? line "primitive : "))
    (string-append "effect : " (cadr (string-split line " : ")) " = external"))))

(define (source lines)
  (string-append
   (string-join
    (append
     '("#lang typify" "declare : Edge" "version : 1" "cap : length = 32")
     (external-primitive-fixture lines)
     '("end!"))
    "\n")
   "\n"))

(define (body lines) (hash-ref (read-declarations (source lines)) "Edge"))

(define (verdict text language)
  (with-handlers
   ((exn:fail:typify?
     (lambda (failure)
       (define diagnostic (exn:fail:typify-diagnostic failure))
       (hash 'accepted #f 'code (diagnostic-code diagnostic) 'data (diagnostic-data diagnostic)))))
   (define result (check-program text language))
   (hash 'accepted #t 'terms (hash-ref result 'terms))))

(define (declaration-verdict text #:meta (meta #f))
  (with-handlers
   ((exn:fail:typify?
     (lambda (failure)
       (define diagnostic (exn:fail:typify-diagnostic failure))
       (hash 'accepted #f 'code (diagnostic-code diagnostic) 'data (diagnostic-data diagnostic)))))
   (read-declarations text #:meta meta)
   (hash 'accepted #t)))

(define (run-tests)
  (define checks 0)
  (define failures 0)
  (define details '())
  (define compiled (make-hasheq))
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (set! failures (add1 failures))
      (set! details (cons (hash 'context context 'actual actual 'expected expected) details))))
  (define (oracle language)
    (hash-ref! compiled language (lambda () (parse-gbnf (language->gbnf language)))))
  (define (line-check language text expected)
    (define observed (line-accepts? text language))
    (define decoded (gbnf-accepts? (oracle language) text))
    (check observed expected (hash 'input text 'source "reader formation"))
    (check decoded expected (hash 'input text 'source "independent emitted-grammar matcher")))
  (define (reject-declaration lines code)
    (check
     (hash-ref (declaration-verdict (source lines)) 'code "accepted")
     code
     (hash 'source "declaration" 'input lines)))
  (define colon-language
    (body
     '("is : text = /.*/"
       "primitive : output : text"
       "form : <text> : yes"
       "does : output <text>")))
  (line-check colon-language "alpha:yes" #t)
  (line-check colon-language " alpha : yes " #t)
  (line-check colon-language "alpha:beta:yes" #f)
  (line-check colon-language "# ignored:otherwise-invalid" #t)
  (line-check colon-language "" #t)
  (line-check colon-language " \t " #t)
  (for
   ((text '("alpha:yes\n" "alpha:yes\r" "alpha:yes\nalpha:yes")))
   (line-check colon-language text #f))
  (define hash-tag-language
    (body
     '("comment : //"
       "is : word = /[a-z]+/"
       "primitive : output : word"
       "form : #tag : <word>"
       "does : output <word>")))
  (line-check hash-tag-language "#tag:a" #t)
  (line-check hash-tag-language "#tag:123" #f)
  (line-check hash-tag-language "// ignored:otherwise-invalid" #t)
  (check
   (hash-ref (verdict "#tag:a" hash-tag-language) 'terms)
   '(("output" "a"))
   "hash-prefixed form survives a different program comment prefix")
  (define arrow-right-language
    (body
     '("is : text = /.*/"
       "primitive : output : text"
       "form : go -> <text>"
       "does : output <text>")))
  (line-check arrow-right-language "go->alpha" #t)
  (line-check arrow-right-language "go->alpha:beta" #f)
  (line-check arrow-right-language "go : alpha" #f)
  (define arrow-left-language
    (body
     '("is : text = /.*/"
       "primitive : output : text"
       "form : <text> -> yes"
       "does : output <text>")))
  (line-check arrow-left-language "alpha->yes" #t)
  (line-check arrow-left-language "alpha->beta->yes" #f)
  (line-check arrow-left-language "alpha:beta->yes" #f)
  (define bang-language
    (body
     '("is : text = /.*/" "primitive : output : text" "form : <text>!" "does : output <text>")))
  (for ((text '("hello!" " hello! "))) (line-check bang-language text #t))
  (for ((text '("!" "two words!" "a!b!" "a->b!" "a:b!"))) (line-check bang-language text #f))
  (define bracket-language
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "form : item : [<word>]"
       "does : output <word>")))
  (line-check bracket-language "item:[a]" #t)
  (line-check bracket-language " item : [a] " #t)
  (line-check bracket-language "item:[ a ]" #f)
  (line-check bracket-language "item:[a ]" #f)
  (define disjoint-language
    (body
     '("is : a = \"x\""
       "is : b = a"
       "primitive : compact : a"
       "primitive : spaced : b"
       "form : item : [<a>]"
       "does : compact <a>"
       "form : item : [ <b> ]"
       "does : spaced <b>")))
  (line-check disjoint-language "item:[x]" #t)
  (line-check disjoint-language "item:[ x ]" #t)
  (check
   (hash-ref (verdict "item:[x]" disjoint-language) 'terms)
   '(("compact" "x"))
   "compact exact capture")
  (check
   (hash-ref (verdict "item:[ x ]" disjoint-language) 'terms)
   '(("spaced" "x"))
   "spaced literal boundary")
  (define recorded-bounds (declaration-check-bounds disjoint-language))
  (check (hash-ref recorded-bounds 'recorded) #t "successful checks have recorded bounds")
  (check (immutable? recorded-bounds) #t "check metadata is immutable")
  (check
   (hash-ref (declaration->jsexpr disjoint-language) 'checks)
   recorded-bounds
   "declaration JSON includes the recorded bounds")
  (check
   (length (hash-ref recorded-bounds 'signature-inclusions))
   2
   "both successful form argument inclusions are recorded")
  (check
   (length (hash-ref recorded-bounds 'delimiters))
   2
   "both successful trailing delimiters are recorded")
  (check
   (length (hash-ref recorded-bounds 'overlaps))
   1
   "only the actual same-shape overlap pair is recorded")
  (check
   (map (lambda (entry) (hash-ref entry 'ordinal)) (hash-ref recorded-bounds 'forms))
   '(0 1)
   "form bounds retain declaration order")
  (define class-names
    (map (lambda (entry) (hash-ref entry 'name)) (hash-ref recorded-bounds 'classes)))
  (check class-names (sort class-names string<?) "class bounds have stable name order")
  (for*
   ((kind '(signature-inclusions delimiters overlaps))
    (entry (in-list (hash-ref recorded-bounds kind))))
   (check
    (exact-positive-integer? (hash-ref (hash-ref entry 'bounds) 'states-upper-bound))
    #t
    (hash 'check kind 'claim "derived state upper bound")))
  (define text-bounds
    (hash-ref
     (for/first
      ((entry (in-list (hash-ref (declaration-check-bounds colon-language) 'classes)))
       #:when
       (equal? (hash-ref entry 'name) "text"))
      entry)
     'bounds))
  (check
   (hash-ref text-bounds 'pattern-control-states)
   1
   "wildcard compact pattern has one control independent of its length counter")
  (check
   (hash-ref text-bounds 'length-counter-positions)
   33
   "length counter is reported independently")
  (check
   (hash-ref text-bounds 'bounded-states-upper-bound)
   33
   "derived product is not the control-state cap")
  (define first-form (car (language-forms disjoint-language)))
  (define-values (first-left first-right) (form-automata disjoint-language first-form))
  (define-values (again-left again-right) (form-automata disjoint-language first-form))
  (check (eq? first-left again-left) #t "left field automaton is reused by body and form identity")
  (check
   (eq? first-right again-right)
   #t
   "right field automaton is reused by body and form identity")
  (define repeated-body
    (hash-ref (read-declarations (emit-declarations (hash "Edge" disjoint-language))) "Edge"))
  (define-values
   (repeated-left repeated-right)
   (form-automata repeated-body (car (language-forms repeated-body))))
  (check
   (eq? first-left repeated-left)
   #f
   "separately read bodies do not share cached left automata")
  (check
   (eq? first-right repeated-right)
   #f
   "separately read bodies do not share cached right automata")
  (check
   (declaration-check-bounds repeated-body)
   recorded-bounds
   "recorded bounds survive structural declaration round trip")
  (check
   (hash-ref
    (declaration-check-bounds (struct-copy language disjoint-language (version "2")))
    'recorded)
   #f
   "a manually constructed body does not inherit a checked declaration receipt")
  (reject-declaration '("is : word = /[a-z]+/" "primitive : output : <word>") "E_DECLARATION")
  (reject-declaration
   '("is : a = \"x\" | \"y\""
     "is : b = \"y\" | \"z\""
     "primitive : first : a"
     "primitive : second : b"
     "form : item : <a>"
     "does : first <a>"
     "form : item : <b>"
     "does : second <b>")
   "E_FORM_SHADOW")
  (reject-declaration
   '("is : token = \"a\" | \"b\""
     "primitive : pair : token token"
     "form : <token> : <token>"
     "does : pair <token> <token>")
   "E_DECLARATION")
  (reject-declaration
   '("is : digits = /[0-9]+/" "primitive : output : digits" "form : go!" "does : output nope")
   "E_TERM_CLASS")
  (reject-declaration
   '("is : token = \"safe\""
     "is : digits = /[0-9]+/"
     "primitive : output : digits"
     "form : go : <token>"
     "does : output <token>")
   "E_TERM_CLASS")
  (reject-declaration
   '("is : word = /[a-z]+/"
     "primitive : inner : word"
     "primitive : output : word"
     "form : go : <word>"
     "does : output (inner <word>)")
   "E_TERM_NESTED")
  (reject-declaration
   '("primitive : output : value" "form : go!" "does : output (ancestors /)")
   "E_TERM_CLASS")
  (reject-declaration
   '("primitive : output : value" "form : go!" "does : output (split /a/b)")
   "E_TERM_CLASS")
  (define wrong-library-result
    (with-handlers
     ((exn:fail:typify?
       (lambda (failure) (hash 'code (diagnostic-code (exn:fail:typify-diagnostic failure))))))
     (verdict
      "calculate!"
      (body
       '("is : digits = /[0-9]+/"
         "primitive : output : digits"
         "form : calculate!"
         "does : output (concat a b)")))))
  (check
   (hash-ref wrong-library-result 'code "accepted")
   "E_TERM_CLASS"
   "library result checked before constructing the receiving term")
  (define alternative-meta
    (body
     '("primitive : declare-language : name"
       "primitive : close-language"
       "form : open : <name>"
       "does : declare-language <name>"
       "form : finish!"
       "does : close-language")))
  (check
   (hash-ref
    (declaration-verdict "#lang typify\nopen : Sample\nfinish!\n" #:meta alternative-meta)
    'accepted)
   #t
   "alternate meta surface preserves kernel operations")
  (define invalid-meta
    (body
     '("primitive : declare-language"
       "primitive : close-language"
       "form : begin!"
       "does : declare-language"
       "form : finish!"
       "does : close-language")))
  (check
   (hash-ref
    (declaration-verdict "#lang typify\nbegin!\nfinish!\n" #:meta invalid-meta)
    'code
    "accepted")
   "E_TERM_CLASS"
   "alternate meta action must satisfy original kernel arity")
  (define ordering
    (declaration-verdict
     (source '("primitive : earlier : missing-a" "primitive : later : missing-b"))))
  (check (hash-ref ordering 'code "accepted") "E_DECLARATION" "stable constructor validation code")
  (check
   (hash-ref (hash-ref ordering 'data (hash)) 'actual #f)
   "missing-a"
   "first declared invalid constructor wins")
  (define path-lines
    '("is : path = /\\/.*/"
      "primitive : output : path"
      "form : go : <path>"
      "does : output (parent <path>)"))
  (define path-language (body path-lines))
  (line-check path-language "go:/data/file" #t)
  (check
   (hash-ref (verdict "go:/data/file" path-language) 'terms)
   '(("output" "/data"))
   "successful innermost library evaluation")
  (define contract-results '())
  (define contract-comparisons 0)
  (define contract-failures 0)
  (define not-admitted 0)
  (define (contract-case id lines input expected-code obligation)
    (define admitted
      (with-handlers
       ((exn:fail:typify?
         (lambda (failure)
           (define diagnostic (exn:fail:typify-diagnostic failure))
           (hash 'admitted #f 'code (diagnostic-code diagnostic)))))
       (hash 'admitted #t 'body (body lines))))
    (check (hash-ref admitted 'admitted) #t (hash 'probe id 'judgment "declaration admission"))
    (define result
      (if (hash-ref admitted 'admitted)
        (let* ((language (hash-ref admitted 'body))
               (semantic-result (verdict input language))
               (formation-result (line-accepts? input language))
               (grammar-result (gbnf-accepts? (oracle language) input))
               (same? (equal? formation-result grammar-result)))
          (check formation-result #t (hash 'probe id 'judgment "formation"))
          (check
           (hash-ref semantic-result 'code "accepted")
           expected-code
           (hash 'probe id 'judgment "semantic refusal"))
          (set! contract-comparisons (add1 contract-comparisons))
          (unless same? (set! contract-failures (add1 contract-failures)))
          (hash
           'id
           id
           'obligation
           obligation
           'input
           input
           'admitted
           #t
           'formation-accepts
           formation-result
           'semantic
           semantic-result
           'expected-semantic-code
           expected-code
           'emitted-grammar-accepts
           grammar-result
           'agrees
           same?
           'grammar-sha256
           (content-hash (language->gbnf language))))
        (begin
          (set! not-admitted (add1 not-admitted))
          (hash
           'id
           id
           'obligation
           obligation
           'input
           input
           'admitted
           #f
           'declaration-code
           (hash-ref admitted 'code)))))
    (set! contract-results (cons result contract-results)))
  (contract-case
   "partial-library-parent-root"
   path-lines
   "go:/"
   "E_LIBRARY_PARENT"
   "A formation-valid capture reaches the specified parent-root error")
  (contract-case
   "total-library-result-class"
   '("is : digits = /[0-9]+/"
     "primitive : output : digits"
     "form : calculate!"
     "does : output (concat a b)")
   "calculate!"
   "E_TERM_CLASS"
   "A formation-valid line computes a string outside the receiving signature class")
  (contract-case
   "program-count-one-empty"
   '("is : word = /[a-z]+/"
     "primitive : output : word"
     "form : go : <word>"
     "does : output <word>"
     "count : output = 1")
   ""
   "E_PROGRAM_COUNT"
   "An ignored empty line cannot satisfy a whole-program count requirement")
  (define contextual-language
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "form : go : <word>"
       "does : output <word>"
       "count : output = 0..1")))
  (define component-lines '("go:a" "go:b"))
  (define context-observation
    (hash
     'id
     "count-at-most-one-composition"
     'individual-formation-acceptance
     (map (lambda (input) (line-accepts? input contextual-language)) component-lines)
     'individual-emitted-acceptance
     (map (lambda (input) (gbnf-accepts? (oracle contextual-language) input)) component-lines)
     'joined-reader
     (verdict (string-join component-lines "\n") contextual-language)
     'scope
     "Line formation agreement does not establish whole-program count satisfaction"))
  (define contract-gate
    (hash
     'total
     contract-comparisons
     'failures
     contract-failures
     'not-admitted
     not-admitted
     'scope
     "Equality of emitted single-line grammar membership and reader formation; semantic refusals checked separately"
     'status
     (cond
      ((zero? contract-comparisons) "pending")
      ((zero? contract-failures) "passed")
      (else "failed"))
     'counterexamples
     (reverse contract-results)
     'contextual-observations
     (list context-observation)
     'provenance
     "Internal definition-derived probes, not the blind verdict corpus"))
  (check
   (positive? contract-comparisons)
   #t
   "at least one emission-contract comparison was measured")
  (define total-checks (+ checks contract-comparisons))
  (define total-failures (+ failures contract-failures))
  (hash
   'suite
   "language-edge"
   'checks
   total-checks
   'count
   total-checks
   'pass
   (- total-checks total-failures)
   'fail
   total-failures
   'status
   (cond
    ((positive? (+ failures contract-failures)) "failed")
    ((zero? contract-comparisons) "pending")
    (else "passed"))
   'ordinary-checks
   checks
   'ordinary-failures
   failures
   'gates
   (hash 'emission-contract contract-gate)
   'details
   (reverse details)))

(module+ main (displayln (canonical-json (run-tests))))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
