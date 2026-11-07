#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/plan.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (body name rows)
  (hash-ref
   (read-declarations
    (string-join
     (append (list "#lang typify" (string-append "declare : " name)) rows '("end!"))
     "\n")
    #:file
    "retention-declarations.typify")
   name))

(define (record body text path)
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
   'line-spans
   (for/hash
    ((line (in-list (read-lines text #:file path))))
    (values (src-span-start-line (line-record-span line)) (line-record-span line)))
   '_term-spans
   (hash-ref checked '_term-spans)))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-plan-retention "~a: expected ~s; received ~s" context expected actual)))
  (define (refuse thunk)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (thunk)
     (error 'test-plan-retention "Expected a native diagnostic")))
  (define absent-facts (hash 'collector "synthetic" 'root "/" 'facts '()))
  (define present-facts
    (hash 'collector "synthetic" 'root "/" 'facts (list (hash 'fact "exists" 'args '("/entry")))))
  (define (schedule poison? ensure?)
    (body
     "Schedule"
     (append
      '("cell : entries = shadow"
        "cell : notes = shadow"
        "primitive : skip-host : canonical-path"
        "writes : skip-host : entries = put 1 \"skip\""
        "reads : skip-host : directory = skip"
        "reads : skip-host : (exists (parent /)) = reject"
        "stage : skip-host = 0"
        "primitive : skip-record : canonical-path"
        "writes : skip-record : notes = put 1 \"skip\""
        "effect : skip-record = state"
        "reads : skip-record : directory = skip"
        "stage : skip-record = 1"
        "primitive : remember : canonical-path"
        "writes : remember : notes = put 1 \"kept\""
        "effect : remember = state"
        "stage : remember = 2"
        "primitive : external : canonical-path"
        "effect : external = external"
        "stage : external = 3"
        "primitive : reject : canonical-path"
        "writes : reject : entries = put 1 \"blocked\""
        "reads : reject : exists = reject"
        "stage : reject = 4"
        "primitive : later : canonical-path"
        "writes : later : entries = put 1 \"later\"")
      (list
       (if poison? "reads : later : (exists (parent /)) = skip" "reads : later : exists = skip"))
      '("stage : later = 5"
        "primitive : remember-later : canonical-path"
        "writes : remember-later : notes = put 1 \"later\""
        "effect : remember-later = state")
      (list
       (if poison?
         "reads : remember-later : (exists (parent /)) = skip"
         "reads : remember-later : exists = skip"))
      '("stage : remember-later = 6"
        "term : run : canonical-path"
        "rule : run <p> -> skip-host <p> ; skip-record <p> ; remember <p> ; external <p> ; reject <p> ; later <p> ; remember-later <p>"
        "form : run : <canonical-path>"
        "does : run <canonical-path>")
      (if ensure?
        '("ensure : run : empty = (absent entries <1>)"
          "ensure : run : masked = (masked entries <1>)")
        '()))))
  (define captured '())
  (define rejected
    (refuse
     (lambda ()
       (plan-program
        (schedule #t #f)
        "run : /entry"
        #:path
        "one.typify"
        #:facts
        absent-facts
        #:facts-hash
        "saved-facts"
        #:on-failure-report
        (lambda (report) (set! captured (cons report captured)))))))
  (check
   (diagnostic-code rejected)
   "E_FACT_REJECT"
   "rejection occurs before the later poisonous read")
  (check (length captured) 1 "typed rejection publishes one report")
  (define failed (car captured))
  (define host-plan
    (list
     (hash 'term '("external" "/entry") 'unchecked #t)
     '("reject" "/entry")
     '("later" "/entry")))
  (check (hash-ref failed 'plan) host-plan "A skips while B and undecided C remain")
  (check (hash-ref failed 'modelled) "2 of 3" "external and state-only counts stay separate")
  (check
   (map (lambda (item) (hash-ref item 'constructor)) (hash-ref failed 'skips))
   '("skip-host" "skip-record")
   "only already-decided skips are retained")
  (check
   (map (lambda (item) (hash-ref item 'term)) (hash-ref failed 'recorded))
   '(("remember" "/entry") ("remember-later" "/entry"))
   "skipped records leave while later undecided records remain")
  (check
   (hash-ref failed 'origins)
   (make-list 3 (hash 'kind "program" 'path "one.typify" 'line 1))
   "origins align only with retained host actions")
  (check (hash-ref failed 'state) (hash) "a rejecting read exposes no partial state")
  (check (hash-ref failed 'ensure) '() "a rejecting read invents no final invariants")
  (check (hash-ref failed 'status) "failed" "retained intent is still a failure")
  (check (hash-ref failed 'facts) "saved-facts" "the provided facts identity survives")
  (check (immutable? failed) #t "failure projection is immutable")
  (define notifications 0)
  (define success
    (plan-program
     (schedule #f #f)
     "run : /entry"
     #:path
     "one.typify"
     #:facts
     present-facts
     #:on-failure-report
     (lambda (_) (set! notifications (add1 notifications)))))
  (check notifications 0 "successful execution emits no failure callback")
  (check (hash-ref success 'plan) host-plan "successful and failure paths share skip projection")
  (check
   (hash-ref success 'recorded)
   (hash-ref failed 'recorded)
   "recorded uses the same projector")
  (define ensure-errors '())
  (define ensured
    (plan-program
     (schedule #f #t)
     "run : /entry"
     #:path
     "one.typify"
     #:facts
     present-facts
     #:on-error
     (lambda (item) (set! ensure-errors (cons item ensure-errors)))
     #:on-failure-report
     (lambda (_) (set! notifications (add1 notifications)))))
  (check
   (map diagnostic-code ensure-errors)
   '("E_ENSURE" "E_ENSURE")
   "collected ensures keep every native error")
  (check
   (hash-ref ensured 'plan)
   host-plan
   "a collected false ensure retains the full admitted plan")
  (check (hash-ref ensured 'status) "failed" "collected ensure reports failed status")
  (check
   (hash-ref (car (hash-ref (hash-ref ensured 'state) "entries")) 'value)
   '("later")
   "a false ensure keeps the completed state")
  (check (length (hash-ref ensured 'ensure)) 2 "every evaluated ensure entry is retained")
  (check notifications 0 "collected errors return normally without a failure callback")
  (set! captured '())
  (define raised-ensures
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostics))
     (plan-program
      (schedule #f #t)
      "run : /entry"
      #:path
      "one.typify"
      #:facts
      present-facts
      #:on-failure-report
      (lambda (report) (set! captured (cons report captured))))
     '()))
  (check
   (map diagnostic-code raised-ensures)
   '("E_ENSURE" "E_ENSURE")
   "default ensure behavior raises the ordered aggregate")
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'ensure)) raised-ensures)
   '("empty" "masked")
   "default diagnostics preserve ensure declaration order")
  (check (length captured) 1 "default ensure aggregation publishes one complete report")
  (check
   (length (hash-ref (car captured) 'ensure))
   2
   "the first false ensure does not truncate the report")
  (check
   (hash-ref (car captured) 'state)
   (hash-ref ensured 'state)
   "raised ensure keeps completed state")
  (define first
    (body
     "First"
     '("primitive : output : canonical-path"
       "effect : output = external"
       "form : run : <canonical-path>"
       "does : output <canonical-path>")))
  (define second
    (body
     "Second"
     '("cell : other = shadow"
       "primitive : blocked : canonical-path"
       "writes : blocked : other = put 1 \"kept\""
       "reads : blocked : exists = reject"
       "form : run : <canonical-path>"
       "does : blocked <canonical-path>")))
  (set! captured '())
  (refuse
   (lambda ()
     (plan-programs
      (list
       (record first "run : /entry" "same.typify")
       (record second "run : /entry" "same.typify"))
      #:facts
      absent-facts
      #:on-failure-report
      (lambda (report) (set! captured (cons report captured))))))
  (define rooted (car captured))
  (check
   (map
    (lambda (item) (list (hash-ref item 'path) (hash-ref item 'language)))
    (hash-ref rooted 'programs))
   '(("same.typify" "First") ("same.typify" "Second"))
   "root identities include language")
  (check
   (map (lambda (item) (length (hash-ref item 'plan))) (hash-ref rooted 'programs))
   '(1 1)
   "same-path programs keep their own event groups")
  (check (hash-has-key? rooted 'recorded) #f "root recorded stays per program")
  (check (hash-has-key? rooted 'skips) #f "root skips stay per program")
  (define-values
   (root-success root-events)
   (plan-programs/details
    (list (record first "run : /entry" "same.typify") (record second "run : /entry" "same.typify"))
    #:facts
    present-facts))
  (check
   (map (lambda (item) (hash-ref item 'language)) (hash-ref root-success 'programs))
   '("First" "Second")
   "successful root entries carry language too")
  (check (length root-events) 2 "details API preserves its event return value")
  (define fuel
    (body
     "Fuel"
     '("primitive : output : value"
       "effect : output = external"
       "term : run : canonical-path"
       "fuel : run = 1"
       "rule : run <p> -> (each <x> in (split <p>) (output <x>))"
       "form : run : <canonical-path>"
       "does : run <canonical-path>")))
  (set! captured '())
  (define expansion-error
    (refuse
     (lambda ()
       (plan-programs/details
        (list
         (record first "run : /entry" "earlier.typify")
         (record fuel "run : /a/b" "later.typify"))
        #:on-failure-report
        (lambda (report) (set! captured (cons report captured)))))))
  (check
   (diagnostic-code expansion-error)
   "E_RULE_FUEL"
   "later root expansion still fails normally")
  (check captured '() "an earlier expanded program is never published as a complete root")
  (define read-error
    (body
     "ReadError"
     '("cell : errors = shadow"
       "primitive : probe : canonical-path"
       "writes : probe : errors = put 1 \"kept\""
       "reads : probe : (exists (parent /)) = reject"
       "form : run : <canonical-path>"
       "does : probe <canonical-path>")))
  (define normalization-error
    (refuse
     (lambda ()
       (plan-program/details
        read-error
        "run : /entry"
        #:facts
        absent-facts
        #:on-failure-report
        (lambda (report) (set! captured (cons report captured)))))))
  (check
   (diagnostic-code normalization-error)
   "E_LIBRARY_PARENT"
   "read argument normalization is not a fact rejection")
  (check captured '() "read argument exceptions never publish a retained plan")
  (for
   ((scope '("program" "parent")))
   (define relative
     (body
      "Relative"
      (list
       "primitive : output : relative-path"
       "effect : output = external"
       (string-append "scope : output = " scope)
       "form : run : <relative-path>"
       "does : output <relative-path>")))
   (define error (refuse (lambda () (plan-program relative "run : a/b"))))
   (check (diagnostic-code error) "E_CONTEXT" "a missing workdir or parent keeps its diagnostic")
   (check
    (hash-ref (diagnostic-data error) 'key)
    '("a/b")
    "relative path remains one tuple component"))
  (for
   ((scope '("program" "parent")))
   (define partitioned
     (body
      "PartitionedRelative"
      (list
       "is : seat = \"a\""
       "file : <seat>/policy.typify"
       "cell : entries = shadow"
       "primitive : store : seat relative-path value"
       "writes : store : entries = put 2"
       (string-append "scope : store = " scope)
       "form : run : <relative-path> = <value>"
       "does : store <seat> <relative-path> <value>")))
   (define error
     (refuse
      (lambda ()
        (plan-program
         partitioned
         "run : a/b = payload"
         #:path
         "a/policy.typify"
         #:captures
         (hash "seat" "a")))))
   (check (diagnostic-code error) "E_CONTEXT" "a partitioned scope failure keeps its native code")
   (check
    (hash-ref (diagnostic-data error) 'key)
    '("a" "a/b")
    "scope failure carries the whole write key and excludes its payload"))
  (define pending-key
    (body
     "PendingKey"
     '("is : seat = \"a\""
       "file : <seat>/policy.typify"
       "cell : entries = shadow"
       "primitive : store : seat relative-path value"
       "writes : store : entries = put 3"
       "scope : store = parent"
       "guard : store = clean"
       "form : run : <relative-path>"
       "does : store <seat> <relative-path> (parent   /)")))
  (define pending-error
    (refuse
     (lambda ()
       (plan-program
        pending-key
        "run : a/b"
        #:path
        "a/policy.typify"
        #:captures
        (hash "seat" "a")
        #:facts
        absent-facts))))
  (check
   (diagnostic-code pending-error)
   "E_CONTEXT"
   "scope failure does not evaluate a pending poisonous key expression")
  (check
   (hash-ref (diagnostic-data pending-error) 'key)
   '("a" "a/b" "(parent   /)")
   "pending key components preserve exact authored expression text")
  (define evaluated-prefix
    (body
     "EvaluatedPrefix"
     '("is : seat = \"a\""
       "file : <seat>/policy.typify"
       "cell : entries = shadow"
       "primitive : store : seat value relative-path"
       "writes : store : entries = put 3"
       "scope : store = parent"
       "guard : store = clean"
       "form : run : <relative-path>"
       "does : store <seat> (concat / entry) <relative-path>")))
  (define prefix-error
    (refuse
     (lambda ()
       (plan-program
        evaluated-prefix
        "run : a/b"
        #:path
        "a/policy.typify"
        #:captures
        (hash "seat" "a")
        #:facts
        absent-facts))))
  (check
   (diagnostic-code prefix-error)
   "E_CONTEXT"
   "a later scope failure keeps its native diagnostic")
  (check
   (hash-ref (diagnostic-data prefix-error) 'key)
   '("a" "/entry" "a/b")
   "an evaluated prefix contributes its decoded pre-resolution value")
  (define relative-payload
    (body
     "RelativePayload"
     '("cell : entries = shadow"
       "primitive : store : canonical-path relative-path"
       "writes : store : entries = put 1"
       "scope : store = parent"
       "form : run : <canonical-path> <relative-path>"
       "does : store <canonical-path> <relative-path>")))
  (define payload-error (refuse (lambda () (plan-program relative-payload "run : /entry a/b"))))
  (check
   (diagnostic-code payload-error)
   "E_CONTEXT"
   "a relative payload scope failure keeps its native diagnostic")
  (check
   (hash-ref (diagnostic-data payload-error) 'key)
   '("a/b")
   "a relative argument outside the declared write key remains a singleton")
  (hash 'suite "plan-retention" 'checks checks 'status "passed"))

(module+ test (run-tests))
