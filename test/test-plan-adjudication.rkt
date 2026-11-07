#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/model.rkt"
         "../src/plan.rkt")

(provide run-tests)

(define (declaration name rows)
  (string-join
   (append (list "#lang typify" (string-append "declare : " name)) rows '("end!"))
   "\n"))

(define (body name rows)
  (hash-ref (read-declarations (declaration name rows) #:file "adjudication.typify") name))

(define (program body text path)
  (define checked (check-program text body #:file path))
  (hash
   'body
   body
   'path
   path
   'status
   "present"
   'placeholders
   (hash)
   'enclosing
   '()
   'terms
   (hash-ref checked 'terms)
   'origins
   (hash-ref checked 'origins)
   'effective-lines
   (hash-ref checked 'effective-lines)
   '_term-spans
   (hash-ref checked '_term-spans)))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-plan-adjudication "~a: expected ~s; received ~s" context expected actual)))
  (define (diagnostics name rows)
    (define-values
     (_ errors)
     (read-declarations/diagnostics (declaration name rows) #:file "adjudication.typify"))
    (filter (lambda (item) (equal? (diagnostic-severity item) "error")) errors))
  (define key-errors
    (diagnostics
     "KeyAnchor"
     '("is : raw = /[a-z]+/"
       "primitive : store : raw"
       "writes : store : entries = put 1"
       "cell : entries = shadow stacking")))
  (check (map diagnostic-code key-errors) '("E_KEY_CANONICAL") "key class refusal is unchanged")
  (define key-span (diagnostic-range (car key-errors)))
  (check
   (list
    (src-span-start-line key-span)
    (src-span-start-column key-span)
    (src-span-end-line key-span)
    (src-span-end-column key-span))
   '(6 1 6 33)
   "key refusal covers the full cell line")
  (define deferred
    (body
     "DeferredKey"
     '("is : raw = /[a-z]+/"
       "primitive : clear : raw"
       "writes : clear : entries = delete 1"
       "cell : entries = shadow stacking")))
  (define deferred-error
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (model-check-stacking-write!
      (language-model deferred)
      (hash-ref (compiled-model-writes (language-model deferred)) "clear")
      1)
     #f))
  (check
   (and deferred-error (src-span-start-line (diagnostic-range deferred-error)))
   6
   "deferred canonical-key validation also anchors the cell")
  (for*
   ((law '("shadow" "first")) (operation '("put" "delete")))
   (define write-line (format "writes : store : entries = ~a 1" operation))
   (define errors
     (diagnostics
      "ShortPlain"
      (list
       "is : office = \"a\""
       "is : seat = \"b\""
       "file : <office>/<seat>/policy.typify"
       (string-append "cell : entries = " law)
       "primitive : store : office"
       write-line)))
   (check (map diagnostic-code errors) '("E_DECLARATION") "plain short writes are refused once")
   (check
    (diagnostic-data (car errors))
    (hash 'kind "partition" 'actual write-line)
    "short write names its full declaration")
   (check
    (src-span-start-line (diagnostic-range (car errors)))
    8
    "short write is anchored at writes even without a supplying form"))
  (define plain-rows
    '("is : seat = \"a\""
      "file : <seat>/policy.typify"
      "cell : entries = shadow"
      "primitive : store : seat canonical-path"
      "writes : store : entries = put 2"
      "form : add : <canonical-path>"
      "does : store <seat> <canonical-path>"))
  (define plain (body "Plain" plain-rows))
  (define plain-plan
    (plan-program plain "add : /entry" #:captures (hash "seat" "a") #:path "a/policy.typify"))
  (check
   (hash-ref (car (hash-ref (hash-ref plain-plan 'state) "entries")) 'key)
   '("a" "/entry")
   "a plain cell retains its template partition key")
  (define wrong
    (diagnostics
     "WrongPrefix"
     (append (drop-right plain-rows 1) '("does : store a <canonical-path>"))))
  (check
   (map diagnostic-code wrong)
   '("E_DECLARATION")
   "an equal literal cannot replace a template capture")
  (check
   (hash-ref (diagnostic-data (car wrong)) 'kind)
   "partition"
   "wrong provenance uses partition")
  (check (src-span-start-line (diagnostic-range (car wrong))) 9 "wrong provenance covers does")
  (define fixed
    (body
     "FixedTemplate"
     '("file : policy.typify"
       "cell : entries = first"
       "primitive : store : value"
       "writes : store : entries = put 1"
       "form : add : <value>"
       "does : store <value>")))
  (check
   (hash-ref (plan-program fixed "add : key" #:path "policy.typify") 'plan)
   '(("store" "key"))
   "a template with no captures remains unpartitioned")
  (for
   ((clause '("reads : store : exists = skip" "guard : store = exists")))
   (define errors
     (diagnostics
      "FactArity"
      (list
       "is : seat = \"a\""
       "file : <seat>/policy.typify"
       "cell : entries = first"
       "primitive : store : seat canonical-path value"
       "writes : store : entries = put 3"
       clause)))
   (check
    (map diagnostic-code errors)
    '("E_DECLARATION")
    "bare unary facts refuse two post-partition components")
   (check (hash-ref (diagnostic-data (car errors)) 'kind) "arity" "bare fact refusal is arity")
   (check
    (src-span-start-line (diagnostic-range (car errors)))
    8
    "bare fact refusal covers its clause"))
  (define guarded
    (body
     "PlainGuard"
     (append plain-rows '("guard : store = exists" "reads : store : owner = reject"))))
  (define guard-args '())
  (define checked
    (check-program
     "add : /entry"
     guarded
     #:file
     "a/policy.typify"
     #:captures
     (hash "seat" "a")
     #:facts
     (lambda (name args origin) (set! guard-args (cons args guard-args)) (values #t #t))))
  (check guard-args '(("/entry")) "plain guards drop all template components")
  (define read-plan
    (plan-program
     guarded
     "add : /entry"
     #:path
     "a/policy.typify"
     #:captures
     (hash "seat" "a")
     #:facts
     (hash
      'collector
      "synthetic"
      'root
      "/"
      'facts
      (list
       (hash 'fact "exists" 'args '("/entry"))
       (hash 'fact "owner" 'args '("/entry") 'value "1000")))))
  (check
   (hash-ref read-plan 'plan)
   '(("store" "a" "/entry"))
   "plain reads query the post-partition key")
  (define clean-guard
    (body
     "CleanGuard"
     '("is : seat = \"a\""
       "file : <seat>/policy.typify"
       "cell : entries = shadow"
       "primitive : store : seat canonical-path value"
       "writes : store : entries = put 3"
       "guard : store = clean"
       "reads : store : (exists <2>) = skip"
       "form : add : <canonical-path> = <value>"
       "does : store <seat> <canonical-path> <value>")))
  (set! guard-args '())
  (define clean-checked
    (check-program
     "add : /entry = mark"
     clean-guard
     #:file
     "a/policy.typify"
     #:captures
     (hash "seat" "a")
     #:facts
     (lambda (name args origin) (set! guard-args (cons args guard-args)) (values #t #t))))
  (check
   guard-args
   '(("/entry" "mark"))
   "a binary bare fact receives all post-partition components")
  (define false-body
    (body
     "FalseBody"
     '("primitive : emit : value"
       "effect : emit = external"
       "term : maybe : canonical-path"
       "fuel : maybe = 1"
       "rule : maybe <p> -> (if (exists <p>) (each <part> in (ancestors (parent /)) (if (exists /unreached) (emit (parent /)))))"
       "form : run : <canonical-path>"
       "does : maybe <canonical-path>")))
  (define false-checked (check-program "run : /entry" false-body #:file "false.typify"))
  (define queries '())
  (define false-events
    (primitive-events
     false-body
     (hash-ref false-checked 'terms)
     (hash-ref false-checked 'origins)
     #:spans
     (hash-ref false-checked '_term-spans)
     #:fact
     (lambda (name args origin) (set! queries (cons (list name args) queries)) #f)))
  (check (hash-ref false-events 'events) '() "a false wrapper emits no effects")
  (check
   queries
   '(("exists" ("/entry")))
   "a false wrapper never evaluates nested sources or conditions")
  (check
   (map (lambda (item) (hash-ref item 'constructor)) (hash-ref false-events 'skips))
   '("emit")
   "false nested wrappers identify the syntactically suppressed leaf")
  (define fail-body
    (body
     "FailedBounds"
     '("primitive : emit : value"
       "effect : emit = external"
       "form : run : <canonical-path>"
       "does : emit (parent <canonical-path>)")))
  (define observed '())
  (define fail-errors
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostics))
     (plan-program
      fail-body
      "run : /\nrun : /"
      #:path
      "failed.typify"
      #:on-bound
      (lambda (bound) (set! observed (cons bound observed))))
     '()))
  (check
   (map diagnostic-code fail-errors)
   '("E_LIBRARY_PARENT" "E_LIBRARY_PARENT")
   "failed bound prepass preserves every independent diagnostic")
  (check observed (list (hash 'c 1 'n 2 'limit 2)) "failed input reports its computed bound once")
  (define fuel-body
    (body
     "FuelBounds"
     '("primitive : emit : value"
       "effect : emit = external"
       "term : run : canonical-path"
       "fuel : run = 2"
       "rule : run <p> -> (each <x> in (split <p>) (emit <x>))"
       "form : run : <canonical-path>"
       "does : run <canonical-path>")))
  (define later
    (body
     "LaterBounds"
     '("primitive : emit : value"
       "effect : emit = external"
       "term : run : value"
       "rule : run <x> -> emit <x> ; emit <x> ; emit <x>"
       "form : run : <value>"
       "does : run <value>")))
  (set! observed '())
  (define fuel-error
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (plan-programs
      (list
       (program fuel-body "run : /a/b/c" "first.typify")
       (program later "run : seed" "later.typify"))
      #:on-bound
      (lambda (bound) (set! observed (cons bound observed))))
     #f))
  (check
   (and fuel-error (diagnostic-code fuel-error))
   "E_RULE_FUEL"
   "root prepass preserves the fuel refusal")
  (check
   observed
   (list (hash 'c 3 'n 2 'limit 6))
   "a later program contributes before an earlier expansion fails")
  (hash 'suite "plan-adjudication" 'checks checks 'status "passed"))

(module+ test (run-tests))
