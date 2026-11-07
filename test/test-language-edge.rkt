#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/emission.rkt"
         "../src/language.rkt"
         "test-decoder.rkt")

(provide run-tests)

(define (source lines)
  (string-append
   (string-join
    (append '("#lang typify" "declare : Edge" "version : 1" "cap : length = 32") lines '("end!"))
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
    (define observed (verdict text language))
    (define decoded (gbnf-accepts? (oracle language) text))
    (check (hash-ref observed 'accepted) expected (hash 'input text 'source "reader"))
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
  (reject-declaration
   '("is : a = \"x\" | \"y\""
     "is : b = \"y\" | \"z\""
     "primitive : first : a"
     "primitive : second : b"
     "form : item : <a>"
     "does : first <a>"
     "form : item : <b>"
     "does : second <b>")
   "E_FORM_OVERLAP")
  (reject-declaration
   '("is : token = \"a\" | \"b\""
     "primitive : pair : token token"
     "form : <token> : <token>"
     "does : pair <token> <token>")
   "E_TEMPLATE_DUPLICATE")
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
   '("primitive : output : value" "form : go!" "does : output (split a /)")
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
   "E_META_SIGNATURE"
   "alternate meta action must satisfy original kernel arity")
  (define ordering
    (declaration-verdict
     (source '("primitive : earlier : missing-a" "primitive : later : missing-b"))))
  (check
   (hash-ref ordering 'code "accepted")
   "E_CLASS_REFERENCE"
   "stable constructor validation code")
  (check
   (hash-ref (hash-ref ordering 'data (hash)) 'class #f)
   "missing-a"
   "first declared invalid constructor wins")
  (define path-lines
    '("is : path = /[/].*/"
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
  (define (contract-case id lines input obligation)
    (define admitted
      (with-handlers
       ((exn:fail:typify?
         (lambda (failure)
           (define diagnostic (exn:fail:typify-diagnostic failure))
           (hash 'admitted #f 'code (diagnostic-code diagnostic)))))
       (hash 'admitted #t 'body (body lines))))
    (define result
      (if (hash-ref admitted 'admitted)
        (let* ((language (hash-ref admitted 'body))
               (reader-result (verdict input language))
               (grammar-result (gbnf-accepts? (oracle language) input))
               (same? (equal? (hash-ref reader-result 'accepted) grammar-result)))
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
           'reader
           reader-result
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
   "A formation-valid capture reaches the specified parent-root error")
  (contract-case
   "total-library-result-class"
   '("is : digits = /[0-9]+/"
     "primitive : output : digits"
     "form : calculate!"
     "does : output (concat a b)")
   "calculate!"
   "A formation-valid line computes a string outside the receiving signature class")
  (contract-case
   "program-count-one-empty"
   '("is : word = /[a-z]+/"
     "primitive : output : word"
     "form : go : <word>"
     "does : output <word>"
     "count : output = 1")
   ""
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
     'individual-reader-acceptance
     (map (lambda (input) (hash-ref (verdict input contextual-language) 'accepted)) component-lines)
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
     "Literal equality of emitted single-line grammar membership and check-program acceptance"
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
