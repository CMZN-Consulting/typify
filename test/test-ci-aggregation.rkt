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
      ((equal? name "test/test-language.rkt") (hash 'blind-full-baseline baseline 'gates (hash)))
      ((equal? name "test/test-meta-mutation.rkt")
       (hash 'blind-full-baseline baseline 'blind-baseline scoped 'gates (hash)))
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
  (define (aggregate reports) (write-reports reports) (apply run-stage1-aggregate paths))
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
            (hash 'blind-full-baseline changed 'gates (hash)))))
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
