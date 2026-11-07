#lang racket/base

(require racket/list "../src/diagnostics.rkt")

(provide run-tests)

(define (contract-refusal? thunk) (with-handlers ((exn:fail:contract? (lambda (_) #t))) (thunk) #f))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-diagnostics "~a: expected ~s; received ~s" context expected actual)))
  (define registry
    '(("E_LINE_SHAPE" actual)
      ("E_FORM_NONE" actual)
      ("E_FORM_SHADOW" form actual)
      ("E_CLASS" class token)
      ("E_CLASS_UNDECLARED" class)
      ("E_CLASS_OVERLAP" form class token)
      ("E_CAP" cap limit actual)
      ("E_EXAMPLE" form actual)
      ("E_PROGRAM_COUNT" constructor expected actual)
      ("E_ORDER" form expected actual)
      ("E_REQUIRED" path)
      ("E_TERM_CLASS" kind constructor class actual)
      ("E_TERM_NESTED" constructor actual)
      ("E_CONSTRUCTOR_UNBOUND" constructor)
      ("E_RULE_DUPLICATE" constructor rule)
      ("E_RULE_CYCLE" constructor)
      ("E_RULE_FUEL" constructor limit actual)
      ("E_LIBRARY_DOMAIN" constructor expected actual)
      ("E_LIBRARY_PARENT" path)
      ("E_JOIN_LAW" cell expected actual)
      ("E_KEY_CANONICAL" cell class)
      ("E_ENSURE" ensure key)
      ("E_FACT_REJECT" fact key)
      ("E_RESTRICTS" form constructor)
      ("E_QUESTION" actual)
      ("E_FILE" path)
      ("E_USAGE" actual)
      ("E_DECLARATION" kind actual)))
  (check
   diagnostic-codes
   (sort (map car registry) string<?)
   "the public registry is closed over the specified codes")
  (for
   ((entry (in-list registry)))
   (define code (car entry))
   (define fields (cdr entry))
   (define expected
     (for/hash
      ((field (in-list fields)))
      (values
       field
       (if (eq? field 'kind)
         (if (equal? code "E_DECLARATION") "header" "literal")
         (symbol->string field)))))
   (define native (hash-set expected 'host-details (hash 'implementation "synthetic")))
   (check
    (hash-ref diagnostic-data-fields code)
    fields
    "every code has exactly its specified compared data fields")
   (check
    (diagnostic-data-projection code native)
    expected
    "projection discards host-specific extra data")
   (check
    (diagnostic-data-projection
     code
     (for/hash (((key value) (in-hash native))) (values (symbol->string key) value)))
    expected
    "decoded object keys use the same projection")
   (define item (make-diagnostic code "Synthetic diagnostic" #:data native))
   (check (diagnostic-data item) native "native diagnostics retain useful extra fields")
   (check
    (hash-ref (diagnostic->jsexpr item) 'data)
    native
    "native wire diagnostics retain useful extra fields")
   (for
    ((field (in-list fields)))
    (check
     (contract-refusal? (lambda () (diagnostic-data-projection code (hash-remove native field))))
     #t
     "every missing compared field is an infrastructure contract failure")
    (check
     (contract-refusal?
      (lambda () (make-diagnostic code "Synthetic diagnostic" #:data (hash-remove native field))))
     #t
     "construction does not invent missing diagnostic data")))
  (for*
   (((code kinds) (in-hash diagnostic-kinds)) (kind (in-list kinds)))
   (define fields (hash-ref diagnostic-data-fields code))
   (define data
     (for/hash ((field (in-list fields))) (values field (if (eq? field 'kind) kind "synthetic"))))
   (check
    (hash-ref (diagnostic-data-projection code data) 'kind)
    kind
    "each registered discriminator is accepted"))
  (for
   ((case (in-list
           (list
            (list "E_DECLARATION" (hash 'kind "literal" 'actual "synthetic"))
            (list "E_DECLARATION" (hash 'kind "unregistered" 'actual "synthetic"))
            (list "E_DECLARATION" (hash 'kind 'header 'actual "synthetic"))
            (list
             "E_TERM_CLASS"
             (hash 'kind "header" 'constructor "item" 'class "word" 'actual "a"))
            (list
             "E_TERM_CLASS"
             (hash 'kind "unregistered" 'constructor "item" 'class "word" 'actual "a"))))))
   (check
    (contract-refusal? (lambda () (diagnostic-data-projection (car case) (cadr case))))
    #t
    "kind discrimination is specific to its code"))
  (check
   (diagnostic-data-projection "E_USAGE" (hash 'actual #f))
   (hash 'actual #f)
   "a false value is present data, not a missing field")
  (check
   (diagnostic-data-projection 'E_USAGE (hash 'actual "synthetic"))
   (hash 'actual "synthetic")
   "symbol code inputs keep their established normalization")
  (check
   (contract-refusal?
    (lambda () (diagnostic-data-projection "E_USAGE" (hash 'actual "a" "actual" "a"))))
   #t
   "ambiguous JSON key spellings are rejected")
  (check
   (contract-refusal? (lambda () (diagnostic-data-projection "E_UNKNOWN" (hash 'actual "a"))))
   #t
   "unregistered codes cannot enter a comparison")
  (check
   (contract-refusal?
    (lambda () (make-diagnostic "E_UNKNOWN" "Synthetic diagnostic" #:data (hash 'actual "a"))))
   #t
   "unregistered codes cannot enter native diagnostics")
  (check
   (contract-refusal? (lambda () (make-diagnostic "E_USAGE" "Synthetic diagnostic")))
   #t
   "omitting required data is visible")
  (check
   (contract-refusal? (lambda () (diagnostic-data-projection "E_USAGE" '(actual))))
   #t
   "diagnostic data must be an object")
  (define original
    (make-diagnostic
     "E_CLASS"
     "Synthetic diagnostic"
     #:span
     (src-span "folder\\source.typify" 2 3 2 5 7 11)
     #:data
     (hash 'class "word" 'token "123")
     #:rule
     "token-class"
     #:verb
     "item"
     #:ordinal
     2
     #:content
     "item : 123"))
  (define moved
    (make-diagnostic
     "E_CLASS"
     "Changed message"
     #:span
     (src-span "folder/source.typify" 5 3 5 5 17 21)
     #:data
     (hash 'class "word" 'token "123")
     #:rule
     "token-class"
     #:verb
     "item"
     #:ordinal
     2
     #:content
     "item : 123"))
  (check (diagnostic-file original) "folder/source.typify" "file paths remain portable")
  (check
   (diagnostic-fingerprints original)
   (diagnostic-fingerprints moved)
   "fingerprints keep rule, verb, ordinal and content semantics")
  (check
   (hash-ref (diagnostic-fingerprints original) 'rule)
   "token-class"
   "an explicit fingerprint rule is not rewritten")
  (check
   (contract-refusal?
    (lambda () (diagnostic->jsexpr (struct-copy diagnostic original (code "E_UNKNOWN")))))
   #t
   "direct struct construction cannot bypass the wire registry")
  (check
   (contract-refusal?
    (lambda () (diagnostic->jsexpr (struct-copy diagnostic original (data (hash))))))
   #t
   "direct struct construction cannot bypass required wire fields")
  (hash 'suite "diagnostics" 'count checks 'pass checks 'fail 0))

(module+
 test
 (define result (run-tests))
 (unless (equal? (hash-ref result 'fail) 0) (error 'test "Checks failed" result)))
