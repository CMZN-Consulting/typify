#lang racket/base

(require json racket/list "../src/diagnostics.rkt" "../src/language.rkt" "witness-report.rkt")

(provide run-tests)

(define source
  (string-append
   "#lang typify\ndeclare : Witnesses\ncap : length = 3\n"
   "is : unsafe = /\\x00/\nis : beyond = /a{4}/\n"
   "is : epsilon = \"\"\nis : live = \"yes\"\nend!\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-witness-report "~a: expected ~s; received ~s" context expected actual)))
  (define-values
   (model diagnostics)
   (read-declarations/diagnostics source #:file "witness-language.typify"))
  (check (diagnostics-error? diagnostics) #f "empty classes remain legal declarations")
  (define report
    (string->jsexpr (jsexpr->string (measure-witnesses (list (cons "fixture" model))))))
  (check (hash-ref report 'failures) 0 "mixed empty and nonempty class records pass")
  (check (hash-ref report 'exact-coverage) #t "empty records preserve class coverage")
  (check (hash-ref report 'written) (hash-ref report 'classes) "each class has a serialized record")
  (define (record name)
    (findf (lambda (entry) (equal? (hash-ref entry 'class) name)) (hash-ref report 'witnesses)))
  (for
   ((name '("unsafe" "beyond")))
   (define entry (record name))
   (define witnesses (hash-ref entry 'witnesses))
   (check (hash-ref entry 'status) "empty" "empty classes are explicitly recorded")
   (check (hash-ref witnesses 'empty) #t "wire records language emptiness")
   (check (hash-ref entry 'passed) #t "emptiness is checked independently")
   (for
    ((field '(shortest boundary near-miss)))
    (check (hash-has-key? witnesses field) #f "an empty class has no sample witnesses")
    (check
     (hash-has-key? (hash-ref entry 'replay) field)
     #f
     "no sample replay is claimed for an empty class")))
  (define epsilon (record "epsilon"))
  (check (hash-ref epsilon 'empty-language) #f "an epsilon member is not an empty language")
  (check
   (hash-ref (hash-ref epsilon 'witnesses) 'shortest)
   ""
   "the empty string remains a real member witness")
  (check
   (hash-ref (hash-ref epsilon 'witnesses) 'boundary)
   ""
   "epsilon-only class has its real boundary witness")
  (check
   (hash-ref (hash-ref (record "live") 'witnesses) 'shortest)
   "yes"
   "ordinary nonempty witnesses remain present")
  (hash 'suite "witness-report" 'checks checks 'status "passed"))

(module+ test (run-tests))
