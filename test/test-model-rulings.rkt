#lang racket/base

(require racket/list
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "../src/model.rkt"
         "../src/syntax.rkt"
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
   'line-spans
   (for/hash
    ((line (in-list (read-lines text #:file path))))
    (values (src-span-start-line (line-record-span line)) (line-record-span line)))
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
       "file : <seat>/policy.typify"
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
       "form : bind : <path>"
       "does : bind <seat> <path> rw"
       "form : mask : <path>"
       "does : mask <seat> <path>"
       "form : clear!"
       "does : clear <seat>"
       "form : view : <path>"
       "does : view <seat> <path>")))
  (define partition-errors '())
  (define partition-plan
    (plan-programs
     (list
      (record
       partitioned
       "bind : /vault/child\nmask : /vault\nview : /vault/child"
       "a/policy.typify"
       #:captures
       (hash "seat" "a"))
      (record
       partitioned
       "bind : /vault/child\nview : /vault/child"
       "b/policy.typify"
       #:captures
       (hash "seat" "b")))
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
    (plan-programs
     (list
      (record
       partitioned
       "bind : /vault/child\nmask : /vault\nclear!"
       "a/policy.typify"
       #:captures
       (hash "seat" "a"))
      (record
       partitioned
       "bind : /vault/child\nview : /vault/child"
       "b/policy.typify"
       #:captures
       (hash "seat" "b")))))
  (check
   (map (lambda (entry) (hash-ref entry 'key)) (hash-ref (hash-ref cleared 'state) "mounts"))
   '(("b" "/vault/child"))
   "delete-one clears a partition by tuple prefix")
  (define inherited-bodies
    (read-declarations
     (string-append
      (source
       "Ancestor"
       '("term : deny-exec : value" "form : deny : <value>" "does : deny-exec <value>"))
      (string-join
       '("declare : Inherited"
         "primitive : emit : value"
         "effect : emit = external"
         "term : batch : value"
         "fuel : batch = 4"
         "rule : batch <ignored> -> (each <item> in (inherited Ancestor.deny-exec) (emit <item>))"
         "form : run!"
         "does : batch seed"
         "end!")
       "\n"))
     #:file
     "inherited-language.typify"))
  (define ancestor (hash-ref inherited-bodies "Ancestor"))
  (define inherited (hash-ref inherited-bodies "Inherited"))
  (define near (record ancestor "deny : sudo\ndeny : su" "root/near/policy.typify"))
  (define far (record ancestor "deny : root-only" "root/policy.typify"))
  (define hidden
    (hash-set*
     (record ancestor "deny : hidden" "root/near/hidden-policy.typify")
     'status
     "guarded"
     'skips
     (list
      (hash
       'constructor
       "deny-exec"
       'fact
       "exists"
       'args
       '("/hidden")
       'reason
       "guard"
       'origin
       (hash 'kind "program" 'path "root/near/hidden-policy.typify" 'line 1)))))
  (define child
    (record inherited "run!" "root/near/child/policy.typify" #:enclosing (list near hidden far)))
  (define inherited-plan (plan-programs (list child)))
  (check
   (hash-ref (car (hash-ref inherited-plan 'programs)) 'plan)
   (map (lambda (word) (hash 'term (list "emit" word) 'unchecked #t)) '("sudo" "su" "root-only"))
   "inherited iterates all first arguments nearest-first and excludes guarded programs")
  (check
   (hash-ref (car (hash-ref inherited-plan 'programs)) 'skips)
   (list
    (hash
     'reason
     "guard"
     'fact
     "exists"
     'args
     '("/hidden")
     'program
     "root/near/hidden-policy.typify"))
   "lookup guard skips use the distinct union wire in the reading program")
  (check
   (hash-has-key? inherited-plan 'skips)
   #f
   "root plans contain skips only in their program entries")
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
  (check
   (hash-ref
    (plan-program templated "bind : /vault" #:path "a/policy.typify" #:captures (hash "seat" "a"))
    'plan)
   '(("bind" "a" "/vault"))
   "standalone planner forwards explicitly bound template captures")
  (check (tuple-key<? '("/a") '("/a/child")) #t "path strings compare by code point")
  (check (tuple-key<? '("a") '("a" "")) #t "proper tuple prefixes come first")
  (check (tuple-key<? '("a" "z") '("aa" "")) #t "the first differing component decides")
  (check (tuple-key<? '("a" "z") '("a" "zz")) #t "component string prefixes come first")
  (check (tuple-key<? '("\"") '("A")) #t "decoded quote precedes A despite JSON escaping")
  (check (tuple-key<? '("a") '("a")) #f "equal keys are not strictly ordered")
  (define ordered-keys '("/a/child" "\"" "A" "/a" "\\"))
  (define ordered-body
    (body
     "DecodedOrder"
     (list
      (string-append
       "is : order-key = "
       (string-join (map (lambda (value) (format "~s" value)) ordered-keys) " | "))
      "cell : entries = shadow"
      "primitive : write : order-key"
      "writes : write : entries = put 1"
      "form : add : <order-key>"
      "does : write <order-key>")))
  (define ordered-plan
    (plan-program
     ordered-body
     (string-join (map (lambda (key) (string-append "add : " key)) ordered-keys) "\n")))
  (check
   (map (lambda (entry) (hash-ref entry 'key)) (hash-ref (hash-ref ordered-plan 'state) "entries"))
   '(("\"") ("/a") ("/a/child") ("A") ("\\"))
   "wire order follows decoded keys rather than their JSON text")
  (define tuple-body
    (body
     "TupleOrder"
     '("is : left = \"a\" | \"aa\""
       "is : right = \"\" | \"z\" | \"zz\""
       "cell : entries = shadow"
       "primitive : write : left right"
       "writes : write : entries = put 2"
       "form : add : [<left>] [<right>]"
       "does : write <left> <right>")))
  (define tuple-plan (plan-program tuple-body "add : [aa] []\nadd : [a] [zz]\nadd : [a] [z]"))
  (check
   (map (lambda (entry) (hash-ref entry 'key)) (hash-ref (hash-ref tuple-plan 'state) "entries"))
   '(("a" "z") ("a" "zz") ("aa" ""))
   "tuple wire order compares components independently")
  (for
   ((cell-rows
     (in-list (list '() '("cell : first-cell = join modes" "cell : second-cell = join modes")))))
   (define-values
    (_model diagnostics)
    (read-declarations/diagnostics
     (source
      "JoinIdentity"
      (append
       cell-rows
       '("join : modes : \"a\" \"b\" = \"a\""
         "join : modes : \"a\" \"c\" = \"c\""
         "join : modes : \"b\" \"c\" = \"b\"")))
     #:file
     "join-identity.typify"))
   (check
    (map diagnostic-code diagnostics)
    '("E_JOIN_LAW")
    "each table is checked once, used twice or unused")
   (check
    (hash-ref (diagnostic-data (car diagnostics)) 'cell)
    "modes"
    "join errors identify their table")
   (check
    (hash-ref (diagnostic-data (car diagnostics)) 'expected)
    "associative"
    "the three-value table is genuinely non-associative")
   (check
    (src-span-start-line (diagnostic-range (car diagnostics)))
    (+ 3 (length cell-rows))
    "a table law failure covers the first join line"))
  (define-values
   (_duplicate duplicate-table)
   (read-declarations/diagnostics
    (source
     "DuplicatePair"
     '("join : modes : \"a\" \"b\" = \"a\""
       "join : modes : \"a\" \"b\" = \"b\""
       "join : modes : \"a\" \"b\" = \"c\""))
    #:file
    "duplicate-pair.typify"))
  (check
   (map diagnostic-code duplicate-table)
   '("E_JOIN_LAW")
   "a conflicting pair does not cause table-law cascades")
  (check
   (diagnostic-data (car duplicate-table))
   (hash 'cell "modes" 'expected "once" 'actual "a b")
   "duplicate pair data names once and the second listing's pair")
  (check
   (src-span-start-line (diagnostic-range (car duplicate-table)))
   4
   "conflicting duplicate covers its second listing")
  (define-values
   (_undeclared undeclared-table)
   (read-declarations/diagnostics
    (source "UndeclaredTable" '("cell : entries = join missing"))
    #:file
    "undeclared-table.typify"))
  (check
   (map diagnostic-code undeclared-table)
   '("E_DECLARATION")
   "a table cannot be silently invented")
  (check
   (diagnostic-data (car undeclared-table))
   (hash 'kind "reference" 'actual "missing")
   "undeclared table uses the reference diagnostic")
  (define-values
   (_diagonal diagonal)
   (read-declarations/diagnostics
    (source "Diagonal" '("join : modes : \"\\x61\" \"a\" = \"b\""))
    #:file
    "diagonal.typify"))
  (check (map diagnostic-code diagonal) '("E_JOIN_LAW") "a wrong diagonal has one law failure")
  (check
   (diagnostic-data (car diagonal))
   (hash 'cell "modes" 'expected "idempotent" 'actual "a")
   "idempotence reports one decoded literal")
  (define-values
   (_partial partial)
   (read-declarations/diagnostics (source "UnusedPartial" '("join : modes : \"a\" \"b\" = \"c\""))))
  (check partial '() "unused partial tables do not claim complete or closed checks")
  (define-values
   (_closed closed)
   (read-declarations/diagnostics
    (source
     "Closed"
     '("is : entry-key = \"x\""
       "is : mode = \"a\" | \"b\""
       "cell : entries = join modes"
       "join : modes : \"a\" \"b\" = \"\\x63\""
       "primitive : put : entry-key mode"
       "writes : put : entries = put 1"))))
  (check (map diagnostic-code closed) '("E_JOIN_LAW") "used tables are checked for class closure")
  (check
   (diagnostic-data (car closed))
   (hash 'cell "modes" 'expected "closed" 'actual "c")
   "closure names the decoded outside-domain literal")
  (define-values
   (_shared shared)
   (read-declarations/diagnostics
    (source
     "SharedDomain"
     '("is : entry-key = \"x\""
       "is : mode-a = \"a\" | \"b\""
       "is : mode-b = \"a\" | \"c\""
       "cell : first-cell = join modes"
       "cell : second-cell = join modes"
       "join : modes : \"a\" \"b\" = \"b\""
       "primitive : put-first : entry-key mode-a"
       "writes : put-first : first-cell = put 1"
       "primitive : put-second : entry-key mode-b"
       "writes : put-second : second-cell = put 1"))
    #:file
    "shared-domain.typify"))
  (check
   (map diagnostic-code shared)
   '("E_DECLARATION")
   "shared tables reject conflicting cell value domains")
  (check
   (diagnostic-data (car shared))
   (hash 'kind "conflict" 'actual "second-cell")
   "the second cell identifies the class conflict")
  (check
   (src-span-start-line (diagnostic-range (car shared)))
   7
   "shared class conflict covers the second cell line")
  (define coverage
    (body
     "Coverage"
     '("is : seat = \"a\" | \"b\""
       "is : path = canonical-path"
       "file : <seat>/policy.typify"
       "cell : mounts = shadow stacking"
       "primitive : bind : seat path value"
       "writes : bind : mounts = put 2"
       "primitive : mask : seat path"
       "writes : mask : mounts = put 2"
       "term : inspect : seat path"
       "fact : inspect"
       "ensure : inspect : covered-result = (covered mounts <1> <2>)"
       "ensure : inspect : uncovered-result = (uncovered mounts <1> <2>)"
       "form : bind : <path>"
       "does : bind <seat> <path> rw"
       "form : mask : <path>"
       "does : mask <seat> <path>"
       "form : inspect : <path>"
       "does : inspect <seat> <path>")))
  (for
   ((entry
     (in-list
      (list
       (list "bind : /root\ninspect : /root/child" #f '(#t #f))
       (list "bind : /root\ninspect : /root" #f '(#f #t))
       (list "mask : /root\ninspect : /root/child" #f '(#f #t))
       (list "bind : /root\nmask : /\ninspect : /root/child" #f '(#f #t))
       (list "bind : /root" "inspect : /root/child" '(#f #t))))))
   (define diagnostics '())
   (define report
     (plan-programs
      (filter
       values
       (list
        (record coverage (car entry) "a/policy.typify" #:captures (hash "seat" "a"))
        (and (cadr entry)
             (record coverage (cadr entry) "b/policy.typify" #:captures (hash "seat" "b")))))
      #:on-error
      (lambda (item) (set! diagnostics (cons item diagnostics)))))
   (check
    (map (lambda (item) (hash-ref item 'ok)) (hash-ref report 'ensure))
    (caddr entry)
    "coverage tests visible proper ancestors in the same partition")
   (check
    (map diagnostic-code diagnostics)
    '("E_ENSURE")
    "covered and uncovered are exact negations"))
  (define state-only
    (body
     "StateOnly"
     '("is : entry-key = \"book\""
       "cell : bookmarks = shadow"
       "primitive : remember : entry-key"
       "writes : remember : bookmarks = put 1 \"denied\""
       "effect : remember = state"
       "ensure : remember : present = (bound bookmarks <1>)"
       "form : remember : <entry-key>"
       "does : remember <entry-key>")))
  (check
   (model-state? (language-model state-only) "remember")
   #t
   "state effect metadata is separate from external")
  (define-values
   (state-report state-events)
   (plan-program/details state-only "remember : book" #:path "state-program.typify"))
  (check (hash-ref state-report 'plan) '() "a state writer has no host action")
  (check
   (hash-ref state-report 'recorded)
   (list
    (hash
     'term
     '("remember" "book")
     'origin
     (hash 'kind "program" 'path "state-program.typify" 'line 1)))
   "recorded terms retain their original primitive arguments and exact wire origin")
  (check (hash-ref state-report 'origins) '() "top-level origins refer only to executed terms")
  (check
   (hash-ref state-report 'modelled)
   "0 of 0"
   "recorded writers do not inflate executed modelling counts")
  (check
   (hash-ref (car (hash-ref (hash-ref state-report 'state) "bookmarks")) 'value)
   '("denied")
   "a literal appends only to the state entry")
  (check
   (hash-ref (car state-events) 'state-only?)
   #t
   "detail events identify recorded work for host-count consumers")
  (check
   (hash-ref (car (hash-ref state-report 'ensure)) 'ok)
   #t
   "recorded writers participate in final-state ensures")
  (check
   (hash-ref state-report 'bound)
   (hash 'c 1 'n 1 'limit 1)
   "the bound counts recorded events")
  (define state-root
    (plan-programs (list (record state-only "remember : book" "state-program.typify"))))
  (check
   (hash-has-key? state-root 'recorded)
   #f
   "root reports keep recorded terms only in each program")
  (check
   (hash-ref (car (hash-ref state-root 'programs)) 'recorded)
   (hash-ref state-report 'recorded)
   "recorded origins retain kind, path, and line in both plan modes")
  (define variable-values
    (body
     "VariableValues"
     '("is : entry-key = \"book\""
       "cell : entries = shadow"
       "primitive : mask : entry-key"
       "writes : mask : entries = put 1"
       "primitive : skeleton : entry-key"
       "writes : skeleton : entries = put 1 \"one\""
       "primitive : write : entry-key value"
       "writes : write : entries = put 1 \"tail\""
       "form : mask : <entry-key>"
       "does : mask <entry-key>"
       "form : skeleton : <entry-key>"
       "does : skeleton <entry-key>"
       "form : write : <entry-key> = <value>"
       "does : write <entry-key> <value>")))
  (for
   ((entry
     (in-list
      (list
       (cons "mask : book" '())
       (cons "skeleton : book" '("one"))
       (cons "write : book = head" '("head" "tail"))))))
   (define result (plan-program variable-values (car entry)))
   (check
    (hash-ref (car (hash-ref (hash-ref result 'state) "entries")) 'value)
    (cdr entry)
    "one cell admits heterogeneous zero, one, and two-value puts"))
  (check
   (hash-ref (plan-program variable-values "write : book = head") 'plan)
   '(("write" "book" "head"))
   "an appended state literal never changes the primitive wire")
  (define composition
    (read-declarations
     (string-append
      (source
       "First"
       '("is : ka = \"book\""
         "cell : entries = shadow"
         "primitive : put : ka"
         "writes : put : entries = put 1"
         "form : put : <ka>"
         "does : put <ka>"))
      (string-join
       '("declare : Second"
         "is : kb = \"book\""
         "cell : entries = shadow"
         "file : a/program.typify"
         "primitive : put : kb"
         "writes : put : entries = put 1"
         "form : put : <kb>"
         "does : put <kb>"
         "end!")
       "\n"))
     #:file
     "shared-order.typify"))
  (define key-conflict
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (plan-programs
      (list
       (record (hash-ref composition "Second") "put : book" "a/program.typify")
       (record (hash-ref composition "First") "put : book" "z/program.typify")))
     #f))
  (check
   (and key-conflict (diagnostic-code key-conflict))
   "E_DECLARATION"
   "equal key denotations do not bypass named-class agreement")
  (check
   (and key-conflict (diagnostic-data key-conflict))
   (hash 'kind "conflict" 'actual "entries")
   "shared metadata rejects key class names rather than values")
  (check
   (and key-conflict (src-span-start-line (diagnostic-range key-conflict)))
   12
   "shared metadata is resolved in declaration order despite reversed tree order")
  (define absent-conflict
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (plan-program (hash-ref composition "First") "put : book" #:path "z/program.typify")
     #f))
  (check
   (and absent-conflict (diagnostic-data absent-conflict))
   (hash 'kind "conflict" 'actual "entries")
   "one-program composition includes a conflicting language with no located record")
  (check
   (and absent-conflict (src-span-start-line (diagnostic-range absent-conflict)))
   12
   "an absent language still participates in declaration-ordered metadata")
  (define (delete-width-source width)
    (source
     "DeletePartition"
     (list
      "is : office = \"a\""
      "is : seat = \"b\""
      "file : <office>/<seat>/policy.typify"
      "cell : entries = shadow stacking"
      "primitive : bind : office seat canonical-path value"
      "writes : bind : entries = put 3"
      (if (= width 1) "primitive : remove : office" "primitive : remove : office seat")
      (format "writes : remove : entries = delete ~a" width))))
  (define-values
   (_short-delete short-delete-errors)
   (read-declarations/diagnostics (delete-width-source 1) #:file "short-delete.typify"))
  (define short-write-line "writes : remove : entries = delete 1")
  (check
   (map diagnostic-code short-delete-errors)
   '("E_DECLARATION")
   "an unused short delete is refused once before any supplying term")
  (check
   (diagnostic-data (car short-delete-errors))
   (hash 'kind "partition" 'actual short-write-line)
   "a short delete reports the entire authored writes line")
  (check
   (let ((range (diagnostic-range (car short-delete-errors))))
     (list
      (src-span-start-line range)
      (src-span-start-column range)
      (src-span-end-line range)
      (src-span-end-column range)))
   (list 10 1 10 (add1 (string-length short-write-line)))
   "a short delete covers its complete writes declaration")
  (define-values
   (_partition-delete partition-delete-errors)
   (read-declarations/diagnostics (delete-width-source 2)))
  (check
   partition-delete-errors
   '()
   "deleting a complete partition prefix remains legal without a terminal path")
  (define-values
   (_appearance appearance-errors)
   (read-declarations/diagnostics
    (source
     "JoinAppearance"
     '("join : modes : \"z\" \"a\" = \"z\""
       "join : modes : \"z\" \"m\" = \"m\""
       "join : modes : \"a\" \"m\" = \"a\""))
    #:file
    "join-appearance.typify"))
  (check
   (map diagnostic-code appearance-errors)
   '("E_JOIN_LAW")
   "an unused table still checks its defined associativity compositions")
  (check
   (diagnostic-data (car appearance-errors))
   (hash 'cell "modes" 'expected "associative" 'actual "z a m")
   "an unauthored triple follows first appearance rather than alphabetic order")
  (check
   (src-span-start-line (diagnostic-range (car appearance-errors)))
   3
   "an unused table's derived witness covers its first join declaration")
  (check (hash-ref state-report 'skips) '() "standalone plans retain their skips field")
  (check
   (hash-ref (car (hash-ref state-root 'programs)) 'skips)
   '()
   "root entries retain empty skips fields")
  (hash 'suite "model-rulings" 'checks checks 'status "passed"))

(module+ test (run-tests))
