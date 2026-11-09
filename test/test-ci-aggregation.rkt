#lang racket/base

(require json
         racket/file
         racket/list
         "../scripts/ci.rkt"
         (submod "../scripts/ci.rkt" test-support)
         "blind-corpus.rkt")

(provide run-tests)

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-ci-aggregation "~a: expected ~s; received ~s" context expected actual)))
  (define (refuses? thunk) (with-handlers ((exn:fail? (lambda (_) #t))) (thunk) #f))
  (define directory (make-temporary-file "ci-aggregation~a" 'directory))
  (define revision (or (getenv "GITHUB_SHA") (make-string 40 #\a)))
  (define baseline
    (hash
     'available
     #t
     'complete
     #t
     'selected-complete
     #t
     'stopped-early
     #f
     'setup-error
     'null
     'infrastructure-failures
     0
     'total
     1
     'attempted
     1
     'requested
     1
     'failures
     0
     'case-total
     1
     'case-failures
     0
     'comparison-policy
     (hash 'fixture "synthetic observations only")
     'results
     (list
      (hash
       'id
       "fixture/check"
       'case-id
       "fixture"
       'command
       "check"
       'stage
       1
       'pass
       #t
       'kind
       "compared"))
     'mismatches
     '()))
  (define scoped (blind-command-scope baseline '("check" "emit") #:stage 1))
  (define (test-result name)
    (hash
     'name
     name
     'pass
     #t
     'count
     1
     'passed
     1
     'failed
     0
     'contract
     #t
     'aborted
     #f
     'result
     (cond
      ((equal? name "test/test-language.rkt")
       (hash 'acceptance-stage 1 'blind-full-baseline baseline 'gates (hash)))
      ((equal? name "test/test-meta-mutation.rkt")
       (hash
        'acceptance-stage
        1
        'blind-full-baseline
        baseline
        'blind-baseline
        scoped
        'gates
        (hash)))
      (else (hash 'gates (hash))))))
  (define (with-tests report tests)
    (hash-set
     report
     'kernel
     (hash-set*
      (hash-ref report 'kernel)
      'tests
      tests
      'pass
      (andmap (lambda (test) (hash-ref test 'pass)) tests)
      'count
      (for/sum ((test (in-list tests))) (hash-ref test 'count))
      'passed
      (for/sum ((test (in-list tests))) (hash-ref test 'passed))
      'failed
      (for/sum ((test (in-list tests))) (hash-ref test 'failed)))))
  (define (report group)
    (with-tests
     (hash
      'schema
      "typify-ci-v1"
      'source-revision
      revision
      'acceptance-stage
      1
      'suite-group
      group
      'report-complete
      #t
      'measurement-mode
      (if (equal? group "kernel") "kernel-only" "full")
      'blind-deferred
      (equal? group "kernel")
      'runtime
      (hash 'pass #t)
      'kernel
      (hash 'format (hash 'pass #t) 'publication (hash 'pass #t) 'package (hash 'pass #t)))
     (map test-result (group-modules group))))
  (define kernel (report "kernel"))
  (define language (report "language"))
  (define mutation (report "mutation"))
  (define paths
    (map
     (lambda (name) (build-path directory name))
     '("kernel.json" "language.json" "mutation.json")))
  (define (write-reports reports)
    (for
     ((path (in-list paths)) (value (in-list reports)))
     (call-with-output-file
      path
      (lambda (output) (write-json value output))
      #:exists
      'truncate/replace)))
  (define (aggregate reports #:stage (stage 1))
    (write-reports reports)
    (keyword-apply run-stage-aggregate '(#:stage) (list stage) paths))
  (define (at-stage report stage)
    (with-tests
     (hash-set report 'acceptance-stage stage)
     (for/list
      ((test (in-list (hash-ref (hash-ref report 'kernel) 'tests))))
      (define result (hash-ref test 'result))
      (hash-set
       test
       'result
       (cond
        ((equal? (hash-ref test 'name) "test/test-language.rkt")
         (hash-set result 'acceptance-stage stage))
        ((equal? (hash-ref test 'name) "test/test-meta-mutation.rkt")
         (hash-set*
          result
          'acceptance-stage
          stage
          'blind-baseline
          (blind-stage-scope (hash-ref result 'blind-full-baseline) stage)))
        (else result))))))
  (dynamic-wind
   void
   (lambda ()
     (define matched (aggregate (list kernel language mutation)))
     (check (hash-ref (hash-ref matched 'replay-validation) 'pass) #t "complete replays match")
     (check (hash-ref (hash-ref matched 'kernel) 'pass) #t "matching job checks remain successful")
     (check
      (hash-ref (hash-ref matched 'stage1) 'pass)
      #f
      "missing gate evidence cannot pass acceptance")
     (check
      (hash-ref (hash-ref matched 'execution) 'count)
      (+ (length (group-modules "kernel")) 2)
      "all actually executed reported checks retain their count")
     (define stage-two (map (lambda (report) (at-stage report 2)) (list kernel language mutation)))
     (define second (aggregate stage-two #:stage 2))
     (check (hash-ref second 'acceptance-stage) 2 "stage is explicit in acceptance report")
     (check
      (hash-has-key? second 'stage1)
      #f
      "stage two does not relabel the accepted stage-one result")
     (check
      (hash-ref (hash-ref second 'replay-validation) 'pass)
      #t
      "matching stage-two baselines remain valid observations")
     (check
      (hash-ref (hash-ref second 'stage2) 'pass)
      #f
      "complete replay without stage-two measurement gates cannot pass acceptance")
     (check
      (map
       (lambda (gate) (hash-ref gate 'name))
       (filter
        (lambda (gate)
          (member
           (hash-ref gate 'name)
           '("house-mount-state"
             "house-primitive-audit"
             "house-rule-mutation"
             "plan-state-bounds"
             "facts-collector-agreement")))
        (hash-ref (hash-ref second 'stage2) 'gates)))
      '("house-mount-state"
        "house-primitive-audit"
        "house-rule-mutation"
        "plan-state-bounds"
        "facts-collector-agreement")
      "stage two requires all five new gates beyond the original fourteen")
     (check
      (refuses? (lambda () (aggregate (list kernel (cadr stage-two) (caddr stage-two)) #:stage 2)))
      #t
      "reports from different acceptance stages cannot be combined")
     (check
      (refuses? (lambda () (aggregate stage-two)))
      #t
      "default stage one cannot consume stage-two reports")
     (define stage-three
       (map (lambda (report) (at-stage report 3)) (list kernel language mutation)))
     (define third (aggregate stage-three #:stage 3))
     (check (hash-ref third 'acceptance-stage) 3 "stage three is explicit")
     (check
      (hash-ref (hash-ref third 'replay-validation) 'pass)
      #t
      "stage-three replay contracts agree")
     (check
      (hash-ref (hash-ref third 'stage3) 'pass)
      #f
      "missing ask evidence cannot pass stage three")
     (check
      (map (lambda (gate) (hash-ref gate 'name)) (hash-ref (hash-ref third 'stage3) 'unscored))
      '("house-mount-state" "house-primitive-audit" "house-rule-mutation")
      "HOUSE providers stay visible as unscored stage-five evidence")
     (check
      (filter
       (lambda (name) (member name '(house-mount-state house-primitive-audit house-rule-mutation)))
       (gate-names 3))
      '()
      "HOUSE providers do not block stage-three acceptance")
     (check
      (filter (lambda (name) (member name '(ask-exact-replay ask-round-trip))) (gate-names 3))
      '(ask-exact-replay ask-round-trip)
      "both new ask measurements are required")
     (check (refuses? (lambda () (gate-names 4))) #t "unsupported acceptance stages are refused")
     (define ask-evidence
       (hash
        'total
        3
        'failures
        0
        'complete
        #t
        'blind
        #t
        'independent
        #t
        'reference-executed
        #f
        'parsed-files
        1
        'declarations
        (list (hash 'words 3))
        'roundtrip-checks
        9
        'roundtrip-failures
        0))
     (check
      (gate-valid? 'ask-exact-replay ask-evidence)
      #t
      "complete frozen answer replay is admitted")
     (check
      (gate-valid? 'ask-exact-replay (hash-set ask-evidence 'reference-executed #t))
      #f
      "running the reference source invalidates the replay evidence")
     (check
      (gate-valid? 'ask-exact-replay (hash-set ask-evidence 'total 4))
      #f
      "gate size must equal the declared words actually replayed")
     (check
      (gate-valid? 'ask-round-trip (hash-set ask-evidence 'total 9))
      #t
      "round-trip checks have their own measured denominator")
     (check
      (gate-valid? 'ask-round-trip (hash-set* ask-evidence 'total 9 'roundtrip-failures 1))
      #f
      "one failed round-trip check invalidates its gate")
     (define deferred-entry
       (hash 'id "deferred" 'reason "definition pending" 'sha256 (make-string 64 #\a)))
     (check
      (baseline-complete?
       (hash-set* baseline 'deferred (list deferred-entry) 'inventory-case-total 2))
      #t
      "explicit deferred cases are inventoried separately from compared invocations")
     (check
      (baseline-complete?
       (hash-set* baseline 'deferred (list deferred-entry) 'inventory-case-total 3))
      #f
      "an unaccounted manifest case makes the replay incomplete")
     (check
      (baseline-complete?
       (hash-set*
        baseline
        'deferred
        (list (hash-set deferred-entry 'id "fixture"))
        'inventory-case-total
        2))
      #f
      "a deferred case cannot also have a compared invocation")
     (define bounds
       (hash
        'total
        10000
        'distinct-declarations
        10000
        'failures
        0
        'plan-violations
        0
        'state-violations
        0
        'canary-found
        #t
        'complete
        #t))
     (check
      (gate-valid? 'plan-state-bounds bounds)
      #t
      "bounds evidence has all required measured fields")
     (check
      (gate-valid? 'plan-state-bounds (hash-remove bounds 'canary-found))
      #f
      "a missing bounds canary cannot pass")
     (check
      (gate-valid? 'plan-state-bounds (hash-set bounds 'complete #f))
      #f
      "partial generated coverage cannot pass")
     (check
      (gate-valid? 'plan-state-bounds (hash-set* bounds 'total 9999 'distinct-declarations 9999))
      #f
      "fewer than ten thousand declarations cannot pass")
     (define rule-mutation
       (hash
        'total
        10
        'failures
        0
        'seeded
        10
        'killed
        9
        'registered
        #t
        'complete
        #t
        'baseline-valid
        #t))
     (check
      (gate-valid? 'house-rule-mutation rule-mutation)
      #t
      "rule mutation accepts ninety percent")
     (check
      (gate-valid? 'house-rule-mutation (hash-set rule-mutation 'killed 8))
      #f
      "rule mutation below ninety percent is refused")
     (check
      (gate-valid? 'house-rule-mutation (hash-set rule-mutation 'baseline-valid #f))
      #f
      "invalid baseline never validates rule mutation")
     (check
      (gate-valid?
       'house-mount-state
       (hash 'total 17 'failures 0 'seats 17 'mismatches 0 'independent-observations #f))
      #f
      "mount states require independent observations")
     (check
      (gate-valid?
       'house-primitive-audit
       (hash
        'total
        16
        'failures
        0
        'effect-forms
        16
        'primitives
        17
        'form-specific-except-keygen
        0
        'deriving-effects
        0))
      #f
      "an audit cannot exceed sixteen primitives")
     (check
      (gate-valid? 'facts-collector-agreement (hash 'total 1 'failures 0 'independent #f))
      #f
      "facts agreement must use an independent collector")
     (delete-file (caddr paths))
     (check
      (refuses? (lambda () (apply run-stage1-aggregate paths)))
      #t
      "missing artifact is refused")
     (check
      (refuses?
       (lambda () (aggregate (list kernel (hash-set language 'report-complete #f) mutation))))
      #t
      "an incomplete checkpoint is refused")
     (check
      (refuses?
       (lambda ()
         (aggregate
          (list
           kernel
           (hash-set
            language
            'source-revision
            (make-string 40 (if (equal? revision (make-string 40 #\b)) #\c #\b)))
           mutation))))
      #t
      "a different source revision is refused")
     (check
      (refuses?
       (lambda () (aggregate (list kernel (hash-set language 'suite-group "mutation") mutation))))
      #t
      "a mislabeled job is refused")
     (define original-tests (hash-ref (hash-ref kernel 'kernel) 'tests))
     (define first-test (car original-tests))
     (define failed
       (aggregate
        (list
         (with-tests
          kernel
          (cons (hash-set* first-test 'pass #f 'passed 0 'failed 1) (cdr original-tests)))
         language
         mutation)))
     (check (hash-ref (hash-ref failed 'kernel) 'pass) #f "a failed input module remains failed")
     (check
      (hash-ref (hash-ref failed 'kernel) 'failed)
      1
      "an observed assertion failure remains counted")
     (define aborted
       (aggregate
        (list
         (with-tests
          kernel
          (cons
           (hash-set* first-test 'pass #f 'count 0 'passed 0 'contract #f 'aborted #t)
           (cdr original-tests)))
         language
         mutation)))
     (check (hash-ref (hash-ref aborted 'kernel) 'pass) #f "an aborted module prevents acceptance")
     (check
      (hash-ref (hash-ref aborted 'kernel) 'aborted-suite-count)
      1
      "abort count stays separate")
     (check
      (hash-ref (hash-ref aborted 'kernel) 'failed)
      0
      "an abort creates no invented failed assertion")
     (define mutation-test (car (hash-ref (hash-ref mutation 'kernel) 'tests)))
     (define (mutation-with-result result)
       (with-tests mutation (list (hash-set mutation-test 'result result))))
     (define mutation-result (hash-ref mutation-test 'result))
     (define disagreement
       (aggregate
        (list
         kernel
         language
         (mutation-with-result
          (hash-set
           mutation-result
           'blind-full-baseline
           (hash-set baseline 'comparison-policy (hash 'fixture "different")))))))
     (check
      (hash-ref (hash-ref disagreement 'replay-validation) 'full-baseline-equal)
      #f
      "full baseline disagreement is visible")
     (check
      (hash-ref (hash-ref disagreement 'kernel) 'pass)
      #f
      "baseline disagreement prevents acceptance")
     (define different-scope
       (aggregate
        (list
         kernel
         language
         (mutation-with-result
          (hash-set
           mutation-result
           'blind-baseline
           (hash-set scoped 'results (list (hash 'id "different"))))))))
     (check
      (hash-ref (hash-ref different-scope 'replay-validation) 'full-baseline-equal)
      #t
      "unchanged full baseline remains matched")
     (check
      (hash-ref (hash-ref different-scope 'replay-validation) 'scoped-baseline-equal)
      #f
      "scoped disagreement is visible")
     (check
      (hash-ref (hash-ref different-scope 'replay-validation) 'scoped-invocation-ids-equal)
      #f
      "scoped invocation identities are compared")
     (check
      (hash-ref (hash-ref different-scope 'stage1) 'pass)
      #f
      "scoped disagreement cannot pass acceptance")
     (define (matching-baselines changed)
       (aggregate
        (list
         kernel
         (with-tests
          language
          (list
           (hash-set
            (car (hash-ref (hash-ref language 'kernel) 'tests))
            'result
            (hash 'acceptance-stage 1 'blind-full-baseline changed 'gates (hash)))))
         (mutation-with-result
          (hash-set*
           mutation-result
           'blind-full-baseline
           changed
           'blind-baseline
           (blind-command-scope changed '("check" "emit") #:stage 1))))))
     (define incomplete-baselines (matching-baselines (hash-set baseline 'complete #f)))
     (check
      (hash-ref (hash-ref incomplete-baselines 'replay-validation) 'full-baseline-equal)
      #t
      "identical incomplete observations still compare equal")
     (check
      (hash-ref (hash-ref incomplete-baselines 'replay-validation) 'pass)
      #f
      "equal incomplete baselines are not accepted")
     (define missing-completeness (matching-baselines (hash-remove baseline 'selected-complete)))
     (check
      (hash-ref (hash-ref missing-completeness 'replay-validation) 'pass)
      #f
      "missing baseline completeness cannot pass")
     (check
      (refuses?
       (lambda ()
         (aggregate
          (list
           (with-tests
            kernel
            (cons (hash-set* first-test 'passed 0 'failed 1) (cdr original-tests)))
           language
           mutation))))
      #t
      "a pass flag inconsistent with failed assertions is refused")
     (define two-units
       (hash-set*
        baseline
        'total
        2
        'attempted
        2
        'requested
        2
        'results
        (append
         (hash-ref baseline 'results)
         (list
          (hash
           'id
           "fixture/emit"
           'case-id
           "fixture"
           'command
           "emit"
           'stage
           1
           'pass
           #t
           'kind
           "compared")))))
     (define two-scoped (blind-command-scope two-units '("check" "emit") #:stage 1))
     (check
      (baseline-complete? two-scoped)
      #t
      "two native scoped invocations in one case remain complete")
     (check
      (baseline-complete? (hash-set* two-scoped 'total 1 'invocations 2 'invocation-failures 0))
      #t
      "the case-count gate alias uses its explicit invocation count")
     (check
      (hash-ref (hash-ref (matching-baselines two-units) 'replay-validation) 'pass)
      #t
      "native baseline equality preserves the two-unit one-case protocol")
     (define outside-failure
       (hash-set*
        two-units
        'total
        3
        'attempted
        3
        'requested
        3
        'failures
        1
        'case-total
        2
        'case-failures
        1
        'results
        (append
         (hash-ref two-units 'results)
         (list
          (hash
           'id
           "outside/plan"
           'case-id
           "outside"
           'command
           "plan"
           'stage
           2
           'pass
           #f
           'kind
           "compared")))))
     (check
      (hash-ref (hash-ref (matching-baselines outside-failure) 'replay-validation) 'pass)
      #t
      "complete matching full replays may contain outside-stage semantic failures"))
   (lambda () (delete-directory/files directory)))
  (hash 'suite "ci-aggregation" 'checks checks 'status "passed"))

(module+ test (run-tests))
