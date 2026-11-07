#lang racket/base

(require racket/string "../src/language.rkt" "../src/diagnostics.rkt")

(provide run-tests)

(define (source rows)
  (string-append "#lang typify\ndeclare : Demo\n" (string-join rows "\n") "\nend!\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-declaration-diagnostics "~a: expected ~s; received ~s" context expected actual)))
  (define (refusal text)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (read-declarations text #:file "declaration.typify")
     (error 'test-declaration-diagnostics "The invalid declaration was accepted")))
  (define (kind rows expected)
    (define item (refusal (source rows)))
    (check (diagnostic-code item) "E_DECLARATION" "declaration code")
    (check (hash-ref (diagnostic-data item) 'kind) expected "declaration kind")
    (check
     (hash-has-key?
      (diagnostic-data-projection (diagnostic-code item) (diagnostic-data item))
      'actual)
     #t
     "declaration carries compared actual data"))
  (kind '("cap : states = 0") "syntax")
  (kind '("is : a = b" "is : b = a") "class-cycle")
  (kind '("primitive : output" "primitive : output") "duplicate")
  (kind '("does : missing") "orphan")
  (kind '("count : missing = 1") "reference")
  (kind '("primitive : output : value" "form : run!" "does : output") "arity")
  (kind '("primitive : output : value" "form : run!" "does : output <missing>") "capture")
  (kind '("primitive : output" "rule : output -> output") "conflict")
  (kind
   '("primitive : output : value"
     "term : first : value"
     "term : second : value"
     "rule : first <x> -> second <x>"
     "rule : second <x> -> output <x>")
   "stratification")
  (define self-cycle (refusal (source '("term : loop : value" "rule : loop <x> -> loop <x>"))))
  (check (diagnostic-code self-cycle) "E_RULE_CYCLE" "a self edge is a genuine rewrite cycle")
  (check (hash-ref (diagnostic-data self-cycle) 'constructor) "loop" "cycle names its constructor")
  (define capture-error
    (refusal
     (source
      '("is : source = \"word\""
        "is : target = /[0-9]+/"
        "primitive : output : target"
        "form : run : <source>"
        "does : output <source>"))))
  (check
   (diagnostic-data-projection (diagnostic-code capture-error) (diagnostic-data capture-error))
   (hash 'kind "capture" 'constructor "output" 'class "target" 'actual "source")
   "capture refusal compares classes rather than a host-chosen witness")
  (define adjacent
    (refusal
     (source
      '("is : a = \"a\""
        "is : b = \"b\""
        "primitive : output : a b"
        "form : run : <a><b>"
        "does : output <a> <b>"))))
  (check
   (diagnostic-data-projection (diagnostic-code adjacent) (diagnostic-data adjacent))
   (hash 'form "run : <a><b>" 'class "a" 'token "")
   "adjacent placeholders use the full form and first class")
  (define cap-error (refusal (source '("cap : states = 4097"))))
  (check (diagnostic-code cap-error) "E_CAP" "raising a declared cap is a cap refusal")
  (check
   (diagnostic-data-projection (diagnostic-code cap-error) (diagnostic-data cap-error))
   (hash 'cap "states" 'limit 4096 'actual 4097)
   "declared cap compares limit and attempted value")
  (hash 'suite "declaration-diagnostics" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
