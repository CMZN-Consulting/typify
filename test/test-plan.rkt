#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/model.rkt"
         "../src/plan.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (body name lines)
  (hash-ref
   (read-declarations
    (string-append
     (string-join
      (append
       (list "#lang typify" (string-append "declare : " name) "cap : length = 32")
       lines
       '("end!"))
      "\n")
     "\n")
    #:file
    (string-append name "-language.typify"))
   name))

(define (record body text path #:enclosing (enclosing '()))
  (define checked (check-program text body #:file path))
  (hash
   'body
   body
   'path
   path
   'placeholders
   (hash)
   'status
   "present"
   'terms
   (hash-ref checked 'terms)
   'origins
   (hash-ref checked 'origins)
   'effective-lines
   (hash-ref checked 'effective-lines)
   'line-spans
   (for/hash
    ((line (in-list (read-lines text #:file path))))
    (values (src-span-start-line (line-record-span line)) (line-record-span line)))
   'enclosing
   enclosing))

(define (facts entries) (hash 'collector "synthetic" 'root "/" 'facts entries))

(define (facts-hash value)
  (string-append "sha256:" (content-hash (string-append (canonical-json value) "\n"))))

(define (store)
  (body
   "Store"
   (list
    "is : key = \"account\""
    "is : payload = \"old\" | \"new\""
    "cell : entries = shadow"
    "primitive : put : key payload"
    "writes : put : entries = put 1"
    "form : put : <key> = <payload>"
    "does : put <key> <payload>")))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-plan "~a: expected ~s; received ~s" context expected actual)))
  (define (reject thunk code)
    (define item
      (with-handlers
       ((exn:fail:typify? exn:fail:typify-diagnostic))
       (thunk)
       (error 'test-plan "The operation unexpectedly succeeded")))
    (check (diagnostic-code item) code "the refusal uses the specified native code")
    item)
  (define shadow (store))
  (define shadow-result
    (plan-program shadow "put : account = old\nput : account = new" #:path "store.typify"))
  (check
   (hash-ref shadow-result 'plan)
   '(("put" "account" "old") ("put" "account" "new"))
   "logical keys remain literal and do not require workdir")
  (check
   (hash-ref (car (hash-ref (hash-ref shadow-result 'state) "entries")) 'value)
   '("new")
   "shadow keeps the latest value")
  (check
   (hash-ref (car (hash-ref (hash-ref shadow-result 'state) "entries")) 'origin)
   (hash 'kind "program" 'path "store.typify" 'line 2)
   "shadow keeps the producing origin")
  (check (hash-ref shadow-result 'modelled) "2 of 2" "each applied write is modelled")
  (check (hash-ref shadow-result 'status) "ok" "a fully modelled plan is ok")
  (define first
    (body
     "First"
     '("is : key = \"account\""
       "is : payload = \"old\" | \"new\""
       "cell : entries = first"
       "primitive : put : key payload"
       "writes : put : entries = put 1"
       "form : put : <key> = <payload>"
       "does : put <key> <payload>")))
  (define first-result
    (plan-program first "put : account = old\nput : account = new" #:path "first.typify"))
  (check
   (hash-ref (car (hash-ref (hash-ref first-result 'state) "entries")) 'value)
   '("old")
   "first keeps the earliest value")
  (check
   (hash-ref (car (hash-ref (hash-ref first-result 'state) "entries")) 'origin)
   (hash 'kind "program" 'path "first.typify" 'line 1)
   "first retains the original provenance")
  (define joined
    (body
     "Joined"
     '("is : key = \"account\""
       "is : mode = \"ro\" | \"rw\""
       "cell : entries = join modes"
       "join : modes : \"ro\" \"rw\" = \"rw\""
       "primitive : put : key mode"
       "writes : put : entries = put 1"
       "form : put : <key> = <mode>"
       "does : put <key> <mode>")))
  (define joined-result
    (plan-program joined "put : account = ro\nput : account = rw\nput : account = ro"))
  (check
   (hash-ref (car (hash-ref (hash-ref joined-result 'state) "entries")) 'value)
   '("rw")
   "the symmetric join and implicit diagonals determine the folded value")
  (define incomplete
    (reject
     (lambda ()
       (body
        "Incomplete"
        '("is : key = \"account\""
          "is : mode = \"ro\" | \"rw\" | \"admin\""
          "cell : entries = join modes"
          "join : modes : \"ro\" \"rw\" = \"rw\""
          "primitive : put : key mode"
          "writes : put : entries = put 1")))
     "E_JOIN_LAW"))
  (check
   (hash-ref (diagnostic-data incomplete) 'expected)
   "complete"
   "an omitted unordered pair is a complete-table failure")
  (reject
   (lambda ()
     (body
      "NonAssociative"
      '("is : key = \"account\""
        "is : mode = \"a\" | \"b\" | \"c\""
        "cell : entries = join modes"
        "join : modes : \"a\" \"b\" = \"b\""
        "join : modes : \"b\" \"c\" = \"c\""
        "join : modes : \"a\" \"c\" = \"a\""
        "primitive : put : key mode"
        "writes : put : entries = put 1")))
   "E_JOIN_LAW")
  (define tuple
    (body
     "Tuple"
     '("is : account = \"a\" | \"b\""
       "is : order = \"x\" | \"y\""
       "is : mode = \"ro\" | \"rw\""
       "cell : entries = shadow"
       "primitive : put : account order mode"
       "writes : put : entries = put 2"
       "primitive : reset : account"
       "writes : reset : entries = delete 1"
       "primitive : wipe"
       "writes : wipe : entries = delete 0"
       "form : put : <account>/<order> = <mode>"
       "does : put <account> <order> <mode>"
       "form : reset : <account>"
       "does : reset <account>"
       "form : wipe!"
       "does : wipe")))
  (define tuple-program "put : a/x = ro\nput : a/y = rw\nput : b/x = ro\nreset : a")
  (define tuple-result (plan-program tuple tuple-program))
  (check
   (map (lambda (entry) (hash-ref entry 'key)) (hash-ref (hash-ref tuple-result 'state) "entries"))
   '(("b" "x"))
   "delete compares tuple components rather than string prefixes")
  (check
   (hash-ref
    (hash-ref (plan-program tuple (string-append tuple-program "\nwipe!")) 'state)
    "entries")
   '()
   "delete zero clears the complete cell")
  (define (mounts reversed?)
    (body
     "Mounts"
     (list
      "is : path = canonical-path"
      "is : mode = \"rw\" | \"ro\""
      "cell : mounts = shadow stacking"
      "primitive : mask : path"
      "writes : mask : mounts = put 1"
      "primitive : bind : path mode"
      "writes : bind : mounts = put 1"
      "term : vault : path"
      (if reversed?
        "rule : vault <p> -> bind <p> rw ; mask (parent <p>)"
        "rule : vault <p> -> mask (parent <p>) ; bind <p> rw")
      "ensure : vault : child-visible = (visible mounts <1>)"
      "form : vault : <path>"
      "does : vault <path>")))
  (define mounted (plan-program (mounts #f) "vault : /vaults/research" #:path "mounts.typify"))
  (check
   (hash-ref mounted 'plan)
   '(("mask" "/vaults") ("bind" "/vaults/research" "rw"))
   "the Mounts rule emits mask before bind")
  (check
   (map (lambda (entry) (hash-ref entry 'masked)) (hash-ref (hash-ref mounted 'state) "mounts"))
   '(#t #f)
   "a zero-value put is masked and a valued put is bound")
  (check
   (hash-ref mounted 'ensure)
   (list
    (hash
     'ensure
     "child-visible"
     'ok
     #t
     'origin
     (hash 'kind "program" 'path "mounts.typify" 'line 1)))
   "ensures use positional bindings and exact unified origins")
  (reject (lambda () (plan-program (mounts #t) "vault : /vaults/research")) "E_ENSURE")
  (define staged
    (body
     "Staged"
     '("is : key = \"account\""
       "is : payload = \"early\" | \"late\""
       "cell : entries = shadow"
       "primitive : put : key payload"
       "writes : put : entries = put 1"
       "primitive : later : key payload"
       "writes : later : entries = put 1"
       "stage : later = 1"
       "form : early!"
       "does : put account early"
       "form : late!"
       "does : later account late")))
  (define-values
   (staged-report staged-events)
   (plan-programs/details
    (list (record staged "late!" "a.typify") (record staged "early!" "b.typify"))))
  (check
   (map (lambda (event) (car (hash-ref event 'term))) staged-events)
   '("put" "later")
   "global stage order takes precedence over tree order")
  (check
   (hash-ref (car (hash-ref (hash-ref staged-report 'state) "entries")) 'value)
   '("late")
   "the final root state follows globally staged order")
  (define external
    (body
     "External"
     '("primitive : emit : value"
       "effect : emit = external"
       "form : emit : <value>"
       "does : emit <value>")))
  (define external-result (plan-program external "emit : hello"))
  (check
   (hash-ref external-result 'plan)
   (list (hash 'term '("emit" "hello") 'unchecked #t))
   "external operations carry explicit unchecked markers")
  (check (hash-ref external-result 'status) "unchecked" "external plans never claim ok")
  (check
   (hash-ref external-result 'modelled)
   "0 of 1"
   "external operations are not counted as modelled")
  (define (reader behavior)
    (body
     "Reader"
     (list
      "is : path = canonical-path"
      "is : mode = \"rw\""
      "cell : entries = shadow"
      "primitive : bind : path mode"
      "writes : bind : entries = put 1"
      (string-append "reads : bind : exists = " behavior)
      "form : bind : <path>"
      "does : bind <path> rw")))
  (define absent-facts (facts '()))
  (define skipped
    (plan-program
     (reader "skip")
     "bind : /target"
     #:path
     "reader.typify"
     #:facts
     absent-facts
     #:facts-hash
     (facts-hash absent-facts)))
  (check (hash-ref skipped 'plan) '() "a missing skip fact suppresses the primitive")
  (check
   (hash-ref skipped 'skips)
   (list
    (hash
     'constructor
     "bind"
     'fact
     "exists"
     'args
     '("/target")
     'reason
     "read"
     'origin
     (hash 'kind "program" 'path "reader.typify" 'line 1)))
   "a read skip records its actual queried arguments and origin")
  (reject
   (lambda ()
     (plan-program
      (reader "reject")
      "bind : /target"
      #:facts
      absent-facts
      #:facts-hash
      (facts-hash absent-facts)))
   "E_FACT_REJECT")
  (define present-facts (facts (list (hash 'fact "exists" 'args '("/target") 'value #t))))
  (check
   (hash-ref
    (plan-program
     (reader "reject")
     "bind : /target"
     #:facts
     present-facts
     #:facts-hash
     (facts-hash present-facts))
    'plan)
   '(("bind" "/target" "rw"))
   "a present fact admits the write")
  (define no-write (reject (lambda () (body "NoWrite" '("primitive : emit"))) "E_DECLARATION"))
  (check
   (hash-ref (diagnostic-data no-write) 'kind)
   "effect"
   "an unmodelled primitive needs explicit external authorization")
  (reject
   (lambda ()
     (body
      "Noncanonical"
      '("is : key = \"relative\""
        "cell : mounts = shadow stacking"
        "primitive : put : key"
        "writes : put : mounts = put 1")))
   "E_KEY_CANONICAL")
  (define delete-only
    (body
     "DeleteOnly"
     '("cell : entries = shadow"
       "primitive : wipe"
       "primitive : clear"
       "writes : wipe : entries = delete 0"
       "writes : clear : entries = delete 0"
       "form : wipe!"
       "does : wipe")))
  (check
   (length (model-check-notes (language-model delete-only)))
   1
   "delete-only declarations retain an explicit unresolved-width note")
  (define width-note (car (model-check-notes (language-model delete-only))))
  (check (diagnostic-code width-note) "N_CELL_WIDTH" "unresolved width has its own note code")
  (check (diagnostic-severity width-note) "note" "unresolved width is informational")
  (check (diagnostic-data width-note) (hash 'cell "entries") "the note identifies only its cell")
  (check
   (list
    (src-span-file (diagnostic-range width-note))
    (src-span-start-line (diagnostic-range width-note))
    (src-span-end-line (diagnostic-range width-note)))
   '("DeleteOnly-language.typify" 7 7)
   "the note points to the first writes line in source order")
  (reject (lambda () (plan-program delete-only "wipe!")) "E_DECLARATION")
  (define composed-reset
    (plan-programs
     (list
      (record shadow "put : account = old" "a.typify")
      (record delete-only "wipe!" "b.typify"))))
  (check
   (hash-ref (hash-ref composed-reset 'state) "entries")
   '()
   "a delete-only body obtains its key width from a composed put writer")
  (define relative
    (body
     "Relative"
     '("is : rel = relative-path"
       "is : path = canonical-path"
       "is : mode = \"rw\""
       "cell : mounts = shadow stacking"
       "primitive : bind : rel mode"
       "writes : bind : mounts = put 1"
       "term : workdir : path"
       "fact : workdir"
       "ensure : bind : resolved = (bound mounts <1>)"
       "form : directory : <path>"
       "does : workdir <path>"
       "form : bind : <rel>"
       "does : bind <rel> rw")))
  (define relative-result
    (plan-program relative "directory : /base\nbind : leaf" #:path "relative.typify"))
  (check
   (hash-ref relative-result 'plan)
   '(("bind" "/base/leaf" "rw"))
   "a bare alias retains the nominal relative-path role")
  (check
   (hash-ref (car (hash-ref (hash-ref relative-result 'state) "mounts")) 'key)
   '("/base/leaf")
   "the receiving canonical path is the folded key")
  (check
   (hash-ref (car (hash-ref relative-result 'ensure)) 'ok)
   #t
   "primitive ensures query the already resolved positional argument")
  (reject (lambda () (plan-program relative "bind : leaf")) "E_CONTEXT")
  (define conditional
    (body
     "Conditional"
     '("is : path = canonical-path"
       "is : mode = \"rw\""
       "cell : entries = shadow"
       "primitive : bind : path mode"
       "writes : bind : entries = put 1"
       "term : optional : path"
       "rule : optional <p> -> if (exists <p>) (bind <p> rw)"
       "form : bind : <path>"
       "does : optional <path>")))
  (define conditional-result
    (plan-program
     conditional
     "bind : /target"
     #:path
     "conditional.typify"
     #:facts
     absent-facts
     #:facts-hash
     (facts-hash absent-facts)))
  (check
   (hash-ref conditional-result 'skips)
   (list
    (hash
     'constructor
     "optional"
     'fact
     "exists"
     'args
     '("/target")
     'reason
     "if"
     'origin
     (hash 'kind "program" 'path "conditional.typify" 'line 1)))
   "conditional skips retain actual predicate arguments")
  (define guard-body
    (body
     "Guarded"
     '("is : path = canonical-path"
       "is : mode = \"rw\""
       "cell : entries = shadow"
       "primitive : bind : path mode"
       "writes : bind : entries = put 1"
       "guard : bind = exists"
       "form : bind : <path>"
       "does : bind <path> rw")))
  (define guarded-result
    (plan-program
     guard-body
     "bind : /target"
     #:path
     "guarded.typify"
     #:facts
     absent-facts
     #:facts-hash
     (facts-hash absent-facts)))
  (check (hash-ref guarded-result 'plan) '() "a false program guard emits no operations")
  (check
   (hash-ref guarded-result 'skips)
   (list
    (hash
     'constructor
     "bind"
     'fact
     "exists"
     'args
     '("/target")
     'reason
     "guard"
     'origin
     (hash 'kind "program" 'path "guarded.typify" 'line 1)))
   "guard observations are forwarded without re-evaluating guarded terms")
  (hash 'suite "plan" 'count checks 'pass checks 'fail 0))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail)) (error 'test "Checks failed" result)))
