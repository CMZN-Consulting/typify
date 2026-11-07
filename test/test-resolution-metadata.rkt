#lang racket/base

(require racket/list racket/string "../src/diagnostics.rkt" "../src/language.rkt")

(provide run-tests)

(define (body guard meaning)
  (hash-ref
   (read-declarations
    (string-join
     (list
      "#lang typify"
      "declare : Metadata"
      "cell : ledger = shadow"
      "primitive : probe : value relative-path value"
      "writes : probe : ledger = put 3 \"seen\""
      (string-append "guard : probe = " guard)
      "form : run : <relative-path>"
      (string-append "does : " meaning)
      "end!")
     "\n")
    #:file
    "metadata.typify")
   "Metadata"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected description)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'resolution-metadata "~a: expected ~s, got ~s" description expected actual)))
  (define (refuse thunk)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostics))
     (thunk)
     (error 'resolution-metadata "Expected a native diagnostic")))
  (define meaning "probe (concat   \"left  \" \"λ\") <relative-path> (parent /)")
  (define pending-body (body "(exists <2>)" meaning))
  (define pending-views '())
  (define facts-called 0)
  (define (stop-second views)
    (lambda (body name index class value origin scope carrying)
      (views index (current-resolution-arguments))
      (when (= index 1)
        (raise-typify
         "E_CONTEXT"
         "Synthetic missing scope"
         #:data
         (hash 'constructor name 'key (current-resolution-arguments))))
      (values value class)))
  (define pending-errors
    (refuse
     (lambda ()
       (check-program
        "run : a/b"
        pending-body
        #:file
        "input.typify"
        #:facts
        (lambda (_name _arguments _origin) (set! facts-called (add1 facts-called)) #t)
        #:resolve
        (stop-second
         (lambda (index view)
           (set! pending-views (append pending-views (list (cons index view))))))))))
  (check (map diagnostic-code pending-errors) '("E_CONTEXT") "a later parent is not evaluated")
  (check facts-called 0 "scope failure precedes the fact query")
  (check
   pending-views
   '((1 "(concat   \"left  \" \"λ\")" "a/b" "(parent /)"))
   "pending components preserve the authored parentheses, blanks and quoted text")
  (check
   (hash-ref (diagnostic-data (car pending-errors)) 'key)
   (cdr (car pending-views))
   "the source view is available to the existing resolver")
  (define evaluated-body (body "(exists (concat <1> <2>))" meaning))
  (define evaluated-views '())
  (define evaluated-errors
    (refuse
     (lambda ()
       (check-program
        "run : a/b"
        evaluated-body
        #:facts
        (lambda (_name _arguments _origin) #t)
        #:resolve
        (stop-second
         (lambda (index view)
           (set! evaluated-views (append evaluated-views (list (cons index view))))))))))
  (check
   (map diagnostic-code evaluated-errors)
   '("E_CONTEXT")
   "guard evaluation stays left to right")
  (check
   evaluated-views
   '((0 "left  λ" "a/b" "(parent /)") (1 "left  λ" "a/b" "(parent /)"))
   "evaluated components carry decoded results and pending components stay authored")
  (define guarded
    (check-program "run : a/b" pending-body #:facts (lambda (_name _arguments _origin) #f)))
  (check (hash-ref guarded 'guarded?) #t "a false guard leaves the raw expression unevaluated")
  (define expansion-views '())
  (define expansion-errors
    (refuse
     (lambda ()
       (primitive-events
        pending-body
        (hash-ref guarded 'raw-terms)
        (hash-ref guarded 'raw-origins)
        #:spans
        (hash-ref guarded '_raw-term-spans)
        #:resolve
        (stop-second
         (lambda (index view)
           (set! expansion-views (append expansion-views (list (cons index view))))))))))
  (check
   (map diagnostic-code expansion-errors)
   '("E_CONTEXT")
   "expansion does not force the later parent")
  (check
   expansion-views
   '((0 "left  λ" "a/b" "(parent /)") (1 "left  λ" "a/b" "(parent /)"))
   "raw expansion uses the same evaluated-prefix metadata")
  (define relative-body
    (hash-ref
     (read-declarations
      (string-join
       '("#lang typify"
         "declare : RelativeMetadata"
         "cell : ledger = shadow"
         "primitive : probe : relative-path relative-path value"
         "writes : probe : ledger = put 2"
         "guard : probe = (exists (concat <1> <2>))"
         "form : run!"
         "does : probe a b (parent /)"
         "end!")
       "\n"))
     "RelativeMetadata"))
  (define relative-views '())
  (define relative-errors
    (refuse
     (lambda ()
       (check-program
        "run!"
        relative-body
        #:facts
        (lambda (_name _arguments _origin) #t)
        #:resolve
        (lambda (body name index class value origin scope carrying)
          (set! relative-views
            (append relative-views (list (cons index (current-resolution-arguments)))))
          (when (= index 1)
            (raise-typify
             "E_CONTEXT"
             "Synthetic missing scope"
             #:data
             (hash 'constructor name 'key (current-resolution-arguments))))
          (values "/joined/a" "canonical-path"))))))
  (check
   (map diagnostic-code relative-errors)
   '("E_CONTEXT")
   "the second relative argument still fails")
  (check
   relative-views
   '((0 "a" "b" "(parent /)") (1 "a" "b" "(parent /)"))
   "earlier scope joining does not replace the supplied key in diagnostics")
  (check (current-resolution-arguments) #f "argument context does not escape the resolver call")
  (hash 'suite "resolution-metadata" 'checks checks 'status "passed"))

(module+ test (run-tests))
