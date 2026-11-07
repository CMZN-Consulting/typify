#lang racket/base

(require racket/list
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "../src/model.rkt"
         "../src/plan.rkt")

(provide run-tests)

(define (source name rows)
  (string-append "#lang typify\ndeclare : " name "\n" (string-join rows "\n") "\nend!\n"))

(define (body name rows)
  (hash-ref (read-declarations (source name rows) #:file "model-rulings.typify") name))

(define (record body text path #:enclosing (enclosing '()) #:captures (captures (hash)))
  (define checked (check-program text body #:file path #:captures captures))
  (hash
   'body
   body
   'path
   path
   'status
   "present"
   'placeholders
   captures
   'terms
   (hash-ref checked 'terms)
   'origins
   (hash-ref checked 'origins)
   'effective-lines
   (hash-ref checked 'effective-lines)
   '_term-spans
   (hash-ref checked '_term-spans)
   'enclosing
   enclosing))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-model-rulings "~a: expected ~s; received ~s" context expected actual)))
  (define spaces (compile-classes (hash "sample" "/ab[ ]cd/")))
  (check (class-accepts? (hash-ref spaces "sample") "ab cd") #t "pattern spaces are literal")
  (check (class-accepts? (hash-ref spaces "sample") "abcd") #f "pattern spaces are not discarded")
  (define layout (compile-classes (hash "sample" "/(\"ab\" | \"cd\")+/")))
  (check
   (class-accepts? (hash-ref layout "sample") "abcd")
   #t
   "pattern layout remains insignificant")
  (check
   (class-accepts? (hash-ref layout "sample") "ab cd")
   #f
   "layout does not become matched text")
  (define laws (compile-classes (hash "sample" (hash-ref meta-classes "law"))))
  (for
   ((value '("join modes" "shadow stacking" "first stacking")))
   (check
    (class-accepts? (hash-ref laws "sample") value)
    #t
    "multiword cell laws match their meta class"))
  (check (class-accepts? (hash-ref laws "sample") "joinmodes") #f "join requires its space")
  (define roles
    (compile-classes
     (hash
      "alias"
      "relative-path"
      "alias-next"
      "alias"
      "pattern"
      "/[a-z]+/"
      "sequence"
      "relative-path \"\""
      "group"
      "(relative-path)"
      "excepted"
      "relative-path except \"x\"")))
  (for
   ((name '("relative-path" "alias" "alias-next")))
   (check (relative-path-role? roles name) #t "bare reference aliases preserve nominal role"))
  (for
   ((name '("pattern" "sequence" "group" "excepted" "canonical-path")))
   (check
    (relative-path-role? roles name)
    #f
    "structural equivalence does not confer nominal role"))
  (define-values
   (_missing missing)
   (read-declarations/diagnostics
    (source "Missing" '("primitive : first-action" "primitive : second-action"))
    #:file
    "missing-effects.typify"))
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'actual)) missing)
   '("first-action" "second-action")
   "independent missing-effect diagnostics accumulate")
  (check
   (map diagnostic-code missing)
   '("E_DECLARATION" "E_DECLARATION")
   "model diagnostics are native")
  (define-values
   (_bad bad)
   (read-declarations/diagnostics
    (source
     "BadWrites"
     '("primitive : first-action"
       "primitive : second-action"
       "writes : first-action : malformed"
       "writes : second-action : malformed"))
    #:file
    "bad-writes.typify"))
  (check (length bad) 2 "invalid write rows do not create missing-effect cascades")
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'kind)) bad)
   '("syntax" "syntax")
   "independent malformed model rows both report")
  (define merge
    (body
     "Merge"
     '("is : entry-key = \"account\""
       "is : mode = \"ro\" | \"rw\""
       "cell : entries = join modes"
       "join : modes : \"ro\" \"rw\" = \"rw\""
       "primitive : put : entry-key mode"
       "writes : put : entries = put 1"
       "form : put : <entry-key> = <mode>"
       "does : put <entry-key> <mode>")))
  (check
   (hash-ref (compiled-model-joins (language-model merge)) "modes")
   (hash (cons "ro" "ro") "ro" (cons "rw" "rw") "rw" (cons "ro" "rw") "rw" (cons "rw" "ro") "rw")
   "finite joins include derived symmetry and checked idempotent diagonals")
  (define merged (plan-program merge "put : account = ro\nput : account = rw"))
  (check
   (hash-ref (car (hash-ref (hash-ref merged 'state) "entries")) 'value)
   '("rw")
   "join fold uses the validated finite table")
  (define final
    (body
     "Final"
     '("is : entry-key = \"account\""
       "is : payload = \"old\" | \"new\""
       "cell : entries = shadow"
       "primitive : put : entry-key payload"
       "primitive : clear : entry-key"
       "writes : put : entries = put 1"
       "writes : clear : entries = delete 1"
       "ensure : put : present-at-end = (bound entries <1>)"
       "form : put : <entry-key> = <payload>"
       "does : put <entry-key> <payload>"
       "form : clear : <entry-key>"
       "does : clear <entry-key>")))
  (define failed '())
  (define final-report
    (plan-program
     final
     "put : account = old\nput : account = new\nclear : account"
     #:path
     "final-program.typify"
     #:on-error
     (lambda (item) (set! failed (cons item failed)))))
  (check
   (map diagnostic-code (reverse failed))
   '("E_ENSURE" "E_ENSURE")
   "all false final-state ensures report")
  (check
   (map (lambda (item) (hash-ref item 'ok)) (hash-ref final-report 'ensure))
   '(#f #f)
   "false ensure entries remain observable")
  (check
   (map (lambda (item) (hash-ref (hash-ref item 'origin) 'line)) (hash-ref final-report 'ensure))
   '(1 2)
   "each producing term contributes exactly one final-state ensure")
  (check
   (hash-ref (hash-ref final-report 'state) "entries")
   '()
   "delete is folded before final-state checks")
  (define relative
    (body
     "RelativeGuard"
     '("is : rel = relative-path"
       "cell : mounts = shadow stacking"
       "primitive : workdir : canonical-path"
       "effect : workdir = external"
       "primitive : bind : rel"
       "writes : bind : mounts = put 1"
       "guard : bind = exists"
       "form : directory : <canonical-path>"
       "does : workdir <canonical-path>"
       "form : bind : <rel>"
       "does : bind <rel>")))
  (define facts
    (hash
     'collector
     "synthetic"
     'root
     "/"
     'facts
     (list (hash 'fact "exists" 'args '("/base/leaf")))))
  (define guarded-plan
    (plan-program
     relative
     "directory : /base\nbind : leaf"
     #:facts
     facts
     #:path
     "relative-program.typify"))
  (check
   (hash-ref guarded-plan 'plan)
   (list (hash 'term '("workdir" "/base") 'unchecked #t) '("bind" "/base/leaf"))
   "relative guard queries the same resolved key as the planner")
  (check (hash-ref guarded-plan 'skips) '() "present resolved fact does not guard the program")
  (define partitioned
    (body
     "Partitioned"
     '("is : seat = \"a\" | \"b\""
       "is : path = canonical-path"
       "is : mode = \"rw\""
       "cell : mounts = shadow stacking"
       "primitive : bind : seat path mode"
       "primitive : mask : seat path"
       "primitive : clear : seat"
       "term : view : seat path"
       "fact : view"
       "writes : bind : mounts = put 2"
       "writes : mask : mounts = put 2"
       "writes : clear : mounts = delete 1"
       "ensure : view : visible-at-end = (visible mounts <1> <2>)"
       "form : bind : <seat> @ <path>"
       "does : bind <seat> <path> rw"
       "form : mask : <seat> @ <path>"
       "does : mask <seat> <path>"
       "form : clear : <seat>"
       "does : clear <seat>"
       "form : view : <seat> @ <path>"
       "does : view <seat> <path>")))
  (define partition-errors '())
  (define partition-plan
    (plan-program
     partitioned
     "bind : a @ /vault/child\nbind : b @ /vault/child\nmask : a @ /vault\nview : a @ /vault/child\nview : b @ /vault/child"
     #:path
     "partition-program.typify"
     #:on-error
     (lambda (item) (set! partition-errors (cons item partition-errors)))))
  (check
   (map diagnostic-code partition-errors)
   '("E_ENSURE")
   "one partition's ancestor mask produces one refusal")
  (check
   (map (lambda (item) (hash-ref item 'ok)) (hash-ref partition-plan 'ensure))
   '(#f #t)
   "a mask affects only keys with equal partition components")
  (check
   (hash-ref (diagnostic-data (car partition-errors)) 'key)
   '("a" "/vault/child")
   "state predicate diagnostics retain the complete partitioned key")
  (define cleared
    (plan-program
     partitioned
     "bind : a @ /vault/child\nbind : b @ /vault/child\nmask : a @ /vault\nclear : a\nview : b @ /vault/child"))
  (check
   (map (lambda (entry) (hash-ref entry 'key)) (hash-ref (hash-ref cleared 'state) "mounts"))
   '(("b" "/vault/child"))
   "delete-one clears a partition by tuple prefix")
  (define ancestor
    (body
     "Ancestor"
     '("term : deny-exec : value" "form : deny : <value>" "does : deny-exec <value>")))
  (define inherited
    (body
     "Inherited"
     '("term : deny-exec : value"
       "fact : deny-exec"
       "primitive : emit : value"
       "effect : emit = external"
       "term : batch : value"
       "fuel : batch = 4"
       "rule : batch <ignored> -> (each <item> in (inherited deny-exec) (emit <item>))"
       "form : run!"
       "does : batch seed")))
  (define near (record ancestor "deny : sudo\ndeny : su" "root/near/policy.typify"))
  (define far (record ancestor "deny : root-only" "root/policy.typify"))
  (define hidden
    (hash-set (record ancestor "deny : hidden" "root/hidden/policy.typify") 'status "guarded"))
  (define child
    (record inherited "run!" "root/near/child/policy.typify" #:enclosing (list near hidden far)))
  (define inherited-plan (plan-programs (list child)))
  (check
   (hash-ref (car (hash-ref inherited-plan 'programs)) 'plan)
   (map (lambda (word) (hash 'term (list "emit" word) 'unchecked #t)) '("sudo" "su" "root-only"))
   "inherited iterates all first arguments nearest-first and excludes guarded programs")
  (define templated
    (body
     "Templated"
     '("is : seat = \"a\" | \"b\""
       "is : path = canonical-path"
       "file : <seat>/policy.typify"
       "cell : mounts = shadow stacking"
       "primitive : bind : seat path"
       "writes : bind : mounts = put 2"
       "form : bind : <path>"
       "does : bind <seat> <path>")))
  (define template-plan
    (plan-programs
     (list (record templated "bind : /vault" "a/policy.typify" #:captures (hash "seat" "a")))))
  (check
   (hash-ref (car (hash-ref template-plan 'programs)) 'plan)
   '(("bind" "a" "/vault"))
   "template captures reach partitioned primitive arguments without per-seat constructors")
  (hash 'suite "model-rulings" 'checks checks 'status "passed"))

(module+ test (run-tests))
