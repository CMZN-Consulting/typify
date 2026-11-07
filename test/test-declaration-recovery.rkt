#lang racket/base

(require racket/list racket/string "../src/language.rkt" "../src/diagnostics.rkt")

(provide run-tests)

(define (source rows)
  (string-append "#lang typify\ndeclare : Recovery\n" (string-join rows "\n") "\nend!\n"))

(define ordinary-rows
  '("primitive : put : canonical-path"
    "writes : put : ledger = put 1"
    "form : put : <canonical-path>"
    "does : put <canonical-path>"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected description)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'declaration-recovery "~a: expected ~s, got ~s" description expected actual)))
  (define (read-errors rows)
    (define-values
     (model diagnostics)
     (read-declarations/diagnostics (source rows) #:file "declaration.typify"))
    (check (hash? model) #t "diagnostic reader returns its partial declaration model")
    diagnostics)
  (define missing (read-errors ordinary-rows))
  (check
   (map diagnostic-code missing)
   '("E_DECLARATION")
   "missing cell retains its typed declaration refusal")
  (check
   (hash-ref (diagnostic-data (car missing)) 'kind)
   "reference"
   "missing cell is an unresolved reference")
  (check
   (hash-ref (diagnostic-data (car missing)) 'actual)
   "ledger"
   "the original missing cell name is preserved")
  (check
   (src-span-start-line (diagnostic-range (car missing)))
   4
   "the missing reference is anchored at its writes line")
  (define legacy-failure
    (with-handlers
     ((exn:fail:typify? values))
     (read-declarations (source ordinary-rows) #:file "declaration.typify")
     #f))
  (check
   (exn:fail:typify? legacy-failure)
   #t
   "legacy reader raises a native refusal instead of a host lookup error")
  (check
   (map diagnostic-code (exn:fail:typify-diagnostics legacy-failure))
   '("E_DECLARATION")
   "legacy aggregate preserves the original diagnostic")
  (define independent (read-errors (append (drop-right ordinary-rows 1) '("does : put <missing>"))))
  (check
   (map diagnostic-code independent)
   '("E_DECLARATION" "E_DECLARATION")
   "recovery still reports an independent bad capture")
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'kind)) independent)
   '("reference" "capture")
   "only cell-dependent checks are skipped")
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'actual)) independent)
   '("ledger" "<missing>")
   "recovery preserves both offending source values")
  (define rewritten
    (read-errors
     '("primitive : put : canonical-path"
       "writes : put : ledger = put 1"
       "term : action : canonical-path"
       "rule : action <p> -> put <p>"
       "form : action : <canonical-path>"
       "does : action <canonical-path>")))
  (check
   (map diagnostic-code rewritten)
   '("E_DECLARATION")
   "rule-right-side validation also tolerates the recovered missing cell")
  (define-values
   (fixed fixed-diagnostics)
   (read-declarations/diagnostics
    (source (cons "cell : ledger = shadow" ordinary-rows))
    #:file
    "declaration.typify"))
  (check fixed-diagnostics '() "a declared cell retains ordinary acceptance")
  (check
   (hash-ref (check-program "put : /example" (hash-ref fixed "Recovery")) 'terms)
   '(("put" "/example"))
   "valid primitive meanings remain unchanged")
  (define contextual
    (hash-ref
     (read-declarations
      (source
       '("primitive : output : canonical-path"
         "effect : output = external"
         "term : base : canonical-path"
         "fact : base"
         "term : action"
         "rule : action -> output (context base)"
         "form : action!"
         "does : action")))
     "Recovery"))
  (define checked (check-program "action!" contextual #:file "program.typify"))
  (define context-error
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (primitive-events
      contextual
      (hash-ref checked 'terms)
      (hash-ref checked 'origins)
      #:spans
      (hash-ref checked '_term-spans))
     #f))
  (check
   (and context-error (diagnostic-code context-error))
   "E_CONTEXT"
   "missing context remains a native expansion refusal")
  (check
   (hash-ref (diagnostic-data context-error) 'constructor)
   "base"
   "missing context preserves its constructor")
  (check
   (hash-ref (diagnostic-data context-error) 'key)
   '()
   "a missing constructor context carries an empty key array")
  (hash 'suite "declaration-recovery" 'checks checks 'status "passed"))

(module+ test (run-tests))
