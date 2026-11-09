#lang racket/base

(require json
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "format.rkt"
         "leak-check.rkt")

(provide run-kernel-check run-stage1-aggregate run-stage-aggregate)

(define test-modules
  '("test/smoke.rkt"
    "test/test-automata-minimize.rkt"
    "test/test-classes.rkt"
    "test/test-syntax.rkt"
    "test/test-diagnostics.rkt"
    "test/test-declaration-diagnostics.rkt"
    "test/test-declaration-recovery.rkt"
    "test/test-resolution-metadata.rkt"
    "test/test-program-diagnostics.rkt"
    "test/test-reader-routed.rkt"
    "test/test-stage3-meta.rkt"
    "test/test-questions.rkt"
    "test/test-ask-gate.rkt"
    "test/test-stage3-cli.rkt"
    "test/test-recovery-rulings.rkt"
    "test/test-depth-literals.rkt"
    "test/test-ci-aggregation.rkt"
    "test/test-witness-report.rkt"
    "test/blind-corpus.rkt"
    "test/test-language.rkt"
    "test/test-language-edge.rkt"
    "test/test-library.rkt"
    "test/test-near.rkt"
    "test/test-defaults.rkt"
    "test/test-emission.rkt"
    "test/test-bounds.rkt"
    "test/test-rules.rkt"
    "test/test-plan.rkt"
    "test/test-plan-adjudication.rkt"
    "test/test-plan-retention.rkt"
    "test/test-plan-generated.rkt"
    "test/test-model-rulings.rkt"
    "test/test-keyed-counts.rkt"
    "test/test-lookups.rkt"
    "test/test-tree.rkt"
    "test/test-facts.rkt"
    "test/test-stage2-cli.rkt"
    "test/test-decoder.rkt"
    "test/test-import-corpus.rkt"
    "test/test-publication.rkt"
    "test/test-meta-mutation.rkt"))

(define language-module "test/test-language.rkt")

(define mutation-module "test/test-meta-mutation.rkt")

(define-runtime-path blind-runner-path "../test/blind-corpus.rkt")

(define (group-modules group)
  (cond
   ((member group '("all" "kernel")) test-modules)
   ((equal? group "language") (list language-module))
   ((equal? group "mutation") (list mutation-module))
   (else (raise-argument-error 'group-modules "all, kernel, language or mutation" group))))

(define stage-one-gate-names
  '(blind-verdict
    structural-round-trip
    decoder-differential
    delimiter-emptiness
    meta-mutation
    byte-smoke
    hostile-corpus
    class-witnesses
    declaration-witnesses
    outside-witnesses
    outside-structural-roundtrip
    worked-example-classes
    emission-contract
    meta-selfcheck))

(define stage-two-gate-names
  '(house-mount-state
    house-primitive-audit
    house-rule-mutation
    plan-state-bounds
    facts-collector-agreement))

(define stage-three-gate-names '(ask-exact-replay ask-round-trip))

(define stage-five-gate-names '(house-mount-state house-primitive-audit house-rule-mutation))

(define (unscored-gates stage)
  (if (= stage 3)
    (for/list
     ((name (in-list stage-five-gate-names)))
     (hash 'name (symbol->string name) 'status "unscored-missing-evidence" 'stage 5))
    '()))

(define (acceptance-stage? stage) (and (exact-integer? stage) (member stage '(1 2 3)) #t))

(define (stage-key stage)
  (unless (acceptance-stage? stage) (raise-argument-error 'stage-key "1, 2 or 3" stage))
  (case stage ((1) 'stage1) ((2) 'stage2) ((3) 'stage3)))

(define (gate-names stage)
  (stage-key stage)
  (append
   stage-one-gate-names
   (if (>= stage 2)
     (filter
      (lambda (name) (or (= stage 2) (not (member name stage-five-gate-names))))
      stage-two-gate-names)
     '())
   (if (= stage 3) stage-three-gate-names '())))

(define (field data key (default #f))
  (if (hash? data)
    (hash-ref data key (lambda () (hash-ref data (symbol->string key) default)))
    default))

(define (natural? value) (exact-nonnegative-integer? value))

(define (positive? value) (and (natural? value) (> value 0)))

(define (invocation-ids baseline)
  (define results (field baseline 'results))
  (and (list? results) (map (lambda (result) (field result 'id)) results)))

(define (baseline-complete? baseline)
  (define results (field baseline 'results))
  (define ids (invocation-ids baseline))
  (define case-ids (and (list? results) (map (lambda (result) (field result 'case-id)) results)))
  (define units (field baseline 'invocations (field baseline 'total)))
  (define unit-failures (field baseline 'invocation-failures (field baseline 'failures)))
  (define deferred (field baseline 'deferred '()))
  (define deferred-ids (and (list? deferred) (map (lambda (entry) (field entry 'id)) deferred)))
  (define full-cases (field baseline 'full-case-total (field baseline 'case-total)))
  (and (hash? baseline)
       (eq? (field baseline 'available) #t)
       (eq? (field baseline 'complete) #t)
       (eq? (field baseline 'selected-complete) #t)
       (eq? (field baseline 'stopped-early) #f)
       (equal? (field baseline 'setup-error) 'null)
       (equal? (field baseline 'infrastructure-failures) 0)
       (list? (field baseline 'mismatches))
       (positive? units)
       (equal? units (field baseline 'attempted))
       (equal? units (field baseline 'requested))
       (list? ids)
       (andmap string? ids)
       (= (length ids) units)
       (= (length ids) (length (remove-duplicates ids)))
       (andmap string? case-ids)
       (list? deferred-ids)
       (andmap string? deferred-ids)
       (= (length deferred-ids) (length (remove-duplicates deferred-ids)))
       (not (ormap (lambda (id) (member id case-ids)) deferred-ids))
       (natural? full-cases)
       (equal?
        (field baseline 'inventory-case-total (+ full-cases (length deferred-ids)))
        (+ full-cases (length deferred-ids)))
       (equal? (field baseline 'case-total) (length (remove-duplicates case-ids)))
       (equal?
        (field baseline 'total)
        (if (hash-has-key? baseline 'invocations) (field baseline 'case-total) units))
       (for/and
        ((result (in-list results)))
        (and (equal? (field result 'kind) "compared") (boolean? (field result 'pass))))
       (equal? unit-failures (count (lambda (result) (not (field result 'pass))) results))
       (equal?
        (field baseline 'case-failures)
        (for/sum
         ((case-id (in-list (remove-duplicates case-ids))))
         (if (for/and
              ((result (in-list results)) #:when (equal? (field result 'case-id) case-id))
              (field result 'pass))
           0
           1)))))

(define (text? value) (and (string? value) (not (string=? value ""))))

(define (json-safe value)
  (cond
   ((hash? value)
    (for/hasheq
     (((key item) (in-hash value)))
     (values (if (symbol? key) key (string->symbol (format "~a" key))) (json-safe item))))
   ((list? value) (map json-safe value))
   ((symbol? value) (if (eq? value 'null) 'null (symbol->string value)))
   ((or (string? value) (boolean? value) (real? value)) value)
   (else (format "~a" value))))

(define (run-test root name #:blind? (blind? #t) #:stage (stage 1))
  (fprintf (current-error-port) "Running ~a\n" name)
  (define output (open-output-string))
  (define errors (open-output-string))
  (with-handlers
   ((exn:fail?
     (lambda (error)
       (hasheq
        'name
        name
        'pass
        #f
        'count
        0
        'passed
        0
        'failed
        0
        'contract
        #f
        'aborted
        #t
        'abort-reason
        "suite-raised"
        'error
        (exn-message error)
        'detail
        (cond
         ((class-error? error)
          (hasheq 'code (class-error-code error) 'data (class-error-data error)))
         ((exn:fail:typify? error) (diagnostic->jsexpr (exn:fail:typify-diagnostic error)))
         (else 'null))
        'stdout
        (get-output-string output)
        'stderr
        (get-output-string errors)))))
   (define result
     (parameterize
      ((current-output-port output) (current-error-port errors))
      (define runner (dynamic-require (build-path root name) 'run-tests))
      (if (member
           name
           '("test/test-language.rkt" "test/test-meta-mutation.rkt" "test/test-ask-gate.rkt"))
        (runner #:blind? blind? #:stage stage)
        (runner))))
   (define check-style?
     (and (positive? (field result 'checks)) (member (field result 'status) '("passed" passed))))
   (define count (if check-style? (field result 'checks) (field result 'count)))
   (define passed (if check-style? count (field result 'pass)))
   (define failed (if check-style? 0 (field result 'fail)))
   (define valid?
     (and (positive? count) (natural? passed) (natural? failed) (= count (+ passed failed))))
   (hasheq
    'name
    name
    'pass
    (and valid? (zero? failed))
    'count
    (if valid? count 0)
    'passed
    (if valid? passed 0)
    'failed
    (if valid? failed 0)
    'contract
    valid?
    'aborted
    (not valid?)
    'abort-reason
    (if valid? 'null "invalid-result-contract")
    'result
    result
    'stdout
    (get-output-string output)
    'stderr
    (get-output-string errors))))

(define (gate-valid? name evidence)
  (define total (field evidence 'total))
  (define failures (field evidence 'failures))
  (and (positive? total)
       (not (equal? (field evidence 'status) "deferred-blind"))
       (natural? failures)
       (zero? failures)
       (case name
         ((blind-verdict)
          (and (eq? (field evidence 'complete) #t)
               (eq? (field evidence 'blind) #t)
               (eq? (field evidence 'independent) #t)
               (text? (field evidence 'corpus))
               (text? (field evidence 'provenance))))
         ((decoder-differential)
          (and (string? (field evidence 'decoder-revision))
               (regexp-match? #px"^[0-9a-f]{40}$" (field evidence 'decoder-revision))
               (natural? (field evidence 'outside-formats))
               (>= (field evidence 'outside-formats) 5)
               (positive? (field evidence 'delimiter-insertions))))
         ((structural-round-trip)
          (and (eq? (field evidence 'complete) #t)
               (positive? (field evidence 'public-declarations))
               (positive? (field evidence 'blind-declarations))))
         ((meta-mutation)
          (define seeded (field evidence 'seeded))
          (define killed (field evidence 'killed))
          (define singles (field evidence 'single-point-total))
          (and (positive? seeded)
               (natural? killed)
               (<= killed seeded)
               (>= (* killed 100) (* seeded 95))
               (positive? singles)
               (equal? singles (field evidence 'single-point-killed))
               (eq? (field evidence 'blind-corpus) #t)
               (eq? (field evidence 'excludes-round-trip) #t)))
         ((hostile-corpus) (= total 107))
         ((class-witnesses)
          (and (positive? (field evidence 'classes))
               (equal? (field evidence 'classes) (field evidence 'written))))
         ((declaration-witnesses)
          (and (eq? (field evidence 'complete) #t)
               (eq? (field evidence 'exact-coverage) #t)
               (positive? (field evidence 'classes))
               (equal? (field evidence 'classes) (field evidence 'written))))
         ((outside-witnesses)
          (and (eq? (field evidence 'exact-coverage) #t)
               (positive? (field evidence 'classes))
               (equal? (field evidence 'classes) (field evidence 'written))
               (positive? (field evidence 'namespaces))
               (>= (field evidence 'namespaces) 5)))
         ((outside-structural-roundtrip)
          (and (positive? (field evidence 'namespaces))
               (>= (field evidence 'namespaces) 5)
               (= total (field evidence 'namespaces))))
         ((worked-example-classes)
          (define coverage (field evidence 'coverage '()))
          (and (equal? (field evidence 'host-predicates) 0)
               (list? coverage)
               (andmap
                (lambda (probe)
                  (and (text? (field probe 'class))
                       (positive? (field probe 'accepted))
                       (positive? (field probe 'refused))))
                coverage)
               (equal?
                (sort (map (lambda (probe) (field probe 'class)) coverage) string<?)
                '("Catalog.category" "Catalog.title" "Mounts.mode" "Mounts.path"))))
         ((emission-contract) (equal? (field evidence 'not-admitted) 0))
         ((meta-selfcheck)
          (and (eq? (field evidence 'term-normalization) #t)
               (equal? (field evidence 'exemptions) 0)
               (positive? (field evidence 'primitive-count))))
         ((house-mount-state)
          (and (= total 17)
               (equal? (field evidence 'seats) 17)
               (equal? (field evidence 'mismatches) 0)
               (eq? (field evidence 'independent-observations) #t)))
         ((house-primitive-audit)
          (and (equal? (field evidence 'effect-forms) 16)
               (positive? (field evidence 'primitives))
               (<= (field evidence 'primitives) 16)
               (equal? (field evidence 'form-specific-except-keygen) 0)
               (equal? (field evidence 'deriving-effects) 0)))
         ((house-rule-mutation)
          (define seeded (field evidence 'seeded))
          (define killed (field evidence 'killed))
          (and (eq? (field evidence 'registered) #t)
               (eq? (field evidence 'complete) #t)
               (eq? (field evidence 'baseline-valid) #t)
               (positive? seeded)
               (natural? killed)
               (<= killed seeded)
               (>= (* killed 100) (* seeded 90))))
         ((plan-state-bounds)
          (and (eq? (field evidence 'complete) #t)
               (natural? (field evidence 'distinct-declarations))
               (>= (field evidence 'distinct-declarations) 10000)
               (= total (field evidence 'distinct-declarations))
               (equal? (field evidence 'plan-violations) 0)
               (equal? (field evidence 'state-violations) 0)
               (eq? (field evidence 'canary-found) #t)))
         ((facts-collector-agreement) (eq? (field evidence 'independent) #t))
         ((ask-exact-replay)
          (and (eq? (field evidence 'complete) #t)
               (eq? (field evidence 'blind) #t)
               (eq? (field evidence 'independent) #t)
               (eq? (field evidence 'reference-executed) #f)
               (positive? (field evidence 'parsed-files))
               (list? (field evidence 'declarations))
               (pair? (field evidence 'declarations))
               (equal?
                total
                (for/sum
                 ((entry (in-list (field evidence 'declarations))))
                 (field entry 'words 0)))))
         ((ask-round-trip)
          (and (eq? (field evidence 'complete) #t)
               (eq? (field evidence 'blind) #t)
               (eq? (field evidence 'independent) #t)
               (eq? (field evidence 'reference-executed) #f)
               (equal? total (field evidence 'roundtrip-checks))
               (equal? (field evidence 'roundtrip-failures) 0)))
         (else #t))))

(define (measure-gates tests #:stage (stage 1))
  (for/list
   ((name (in-list (gate-names stage))))
   (define evidence-name (if (eq? name 'facts-collector-agreement) 'facts-collector name))
   (define supplied
     (for/list
      ((test tests) #:when (field (field (field test 'result) 'gates) evidence-name))
      (cons (field test 'name) (field (field (field test 'result) 'gates) evidence-name))))
   (cond
    ((null? supplied) (hasheq 'name (symbol->string name) 'pass #f 'status "missing-evidence"))
    ((pair? (cdr supplied))
     (hasheq
      'name
      (symbol->string name)
      'pass
      #f
      'status
      "duplicate-evidence"
      'sources
      (map car supplied)))
    (else
     (define source (caar supplied))
     (define evidence (cdar supplied))
     (define passed?
       (and (for/and ((test tests) #:when (equal? (field test 'name) source)) (field test 'pass))
            (gate-valid? name evidence)))
     (hasheq
      'name
      (symbol->string name)
      'pass
      passed?
      'source
      source
      'status
      (cond
       (passed? "measured")
       ((equal? (field evidence 'status) "deferred-blind") "deferred-blind")
       (else "failed-evidence"))
      'evidence
      evidence)))))

(define (package-metadata root)
  (with-handlers
   ((exn:fail? (lambda (error) (hasheq 'pass #f 'error (exn-message error)))))
   (define forms
     (call-with-input-file
      (build-path root "src" "info.rkt")
      (lambda (input)
        (read-line input)
        (let loop ((forms '()))
          (define form (read input))
          (if (eof-object? form) (reverse forms) (loop (cons form forms)))))))
   (define (declared name)
     (for/first
      ((form forms)
       #:when
       (and (list? form) (= (length form) 3) (eq? (car form) 'define) (eq? (cadr form) name)))
      (caddr form)))
   (define collection (declared 'collection))
   (define dependencies (declared 'deps))
   (hasheq
    'pass
    (and (equal? collection "typify") (equal? dependencies ''("base")))
    'collection
    collection
    'dependencies
    dependencies)))

(define (run-kernel-check
         (given-root (current-directory))
         #:checkpoint
         (checkpoint #f)
         #:blind?
         (blind? #t)
         #:group
         (group "all")
         #:stage
         (stage 1))
  (define acceptance-key (stage-key stage))
  (define modules (group-modules group))
  (when (and (equal? group "kernel") blind?)
    (raise-arguments-error 'run-kernel-check "the kernel group requires --defer-blind"))
  (define root (simplify-path (path->complete-path given-root)))
  (define completed '())
  (define (save-progress phase)
    (when checkpoint
      (call-with-output-file
       checkpoint
       (lambda (output)
         (write-json
          (json-safe
           (hasheq
            'schema
            "typify-ci-v1"
            'source-revision
            (or (getenv "GITHUB_SHA") 'null)
            'acceptance-stage
            stage
            'suite-group
            group
            'report-complete
            #f
            'measurement-mode
            (if blind? "full" "kernel-only")
            'blind-deferred
            (not blind?)
            'phase
            phase
            'kernel
            (hasheq 'pass #f 'tests completed)
            acceptance-key
            (hasheq 'pass #f 'status "measurement-incomplete")))
          output)
         (newline output))
       #:exists
       'truncate/replace)))
  (define tests
    (for/list
     ((name modules))
     (save-progress name)
     (define result (run-test root name #:blind? blind? #:stage stage))
     (set! completed (append completed (list result)))
     (save-progress "between-suites")
     result))
  (define formatting (format-repository root))
  (define publication (check-repository root))
  (define package (package-metadata root))
  (define gates (measure-gates tests #:stage stage))
  (define runtime
    (hasheq
     'version
     (version)
     'vm
     (system-type 'vm)
     'pass
     (and (equal? (version) "9.3") (eq? (system-type 'vm) 'chez-scheme))))
  (define kernel-pass?
    (and (hash-ref runtime 'pass)
         (hash-ref formatting 'pass)
         (hash-ref publication 'pass)
         (hash-ref package 'pass)
         (andmap (lambda (test) (hash-ref test 'pass)) tests)))
  (define aborted-suites
    (for/list ((test (in-list tests)) #:when (field test 'aborted #f)) (hash-ref test 'name)))
  (json-safe
   (hasheq
    'schema
    "typify-ci-v1"
    'source-revision
    (or (getenv "GITHUB_SHA") 'null)
    'acceptance-stage
    stage
    'suite-group
    group
    'report-complete
    #t
    'measurement-mode
    (if blind? "full" "kernel-only")
    'blind-deferred
    (not blind?)
    'runtime
    runtime
    'kernel
    (hasheq
     'pass
     kernel-pass?
     'tests
     tests
     'format
     formatting
     'publication
     publication
     'package
     package
     'count
     (apply + (map (lambda (test) (hash-ref test 'count)) tests))
     'passed
     (apply + (map (lambda (test) (hash-ref test 'passed)) tests))
     'failed
     (apply + (map (lambda (test) (hash-ref test 'failed)) tests))
     'aborted-suite-count
     (length aborted-suites)
     'aborted-suites
     aborted-suites)
    acceptance-key
    (hasheq
     'pass
     (and blind? kernel-pass? (andmap (lambda (gate) (hash-ref gate 'pass)) gates))
     'status
     (if blind? "measured" "deferred-blind")
     'unscored
     (unscored-gates stage)
     'gates
     gates))))

(define (run-stage1-aggregate kernel-path language-path mutation-path)
  (run-stage-aggregate kernel-path language-path mutation-path #:stage 1))

(define (run-stage-aggregate kernel-path language-path mutation-path #:stage (stage 1))
  (define acceptance-key (stage-key stage))
  (define reports
    (for/list
     ((path (in-list (list kernel-path language-path mutation-path))))
     (call-with-input-file path read-json)))
  (define revision (field (car reports) 'source-revision))
  (unless (and (string? revision) (regexp-match? #px"^[0-9a-f]{40}$" revision))
    (raise-arguments-error 'run-stage1-aggregate "source revision is absent or invalid"))
  (define expected-revision (getenv "GITHUB_SHA"))
  (when (and expected-revision (not (equal? expected-revision revision)))
    (raise-arguments-error
     'run-stage1-aggregate
     "evidence does not belong to this workflow commit"))
  (for
   ((report (in-list reports)) (group '("kernel" "language" "mutation")))
   (define kernel (field report 'kernel))
   (define tests (field kernel 'tests))
   (unless (and
            (equal? (field report 'schema) "typify-ci-v1")
            (eq? (field report 'report-complete) #t)
            (equal? (field report 'suite-group) group)
            (equal? (field report 'source-revision) revision)
            (equal? (field report 'acceptance-stage) stage)
            (equal? (field report 'blind-deferred) (equal? group "kernel"))
            (equal?
             (field report 'measurement-mode)
             (if (equal? group "kernel") "kernel-only" "full"))
            (list? tests)
            (equal?
             (sort (map (lambda (test) (field test 'name)) tests) string<?)
             (sort (group-modules group) string<?))
            (for/and
             ((test (in-list tests)))
             (and (boolean? (field test 'pass))
                  (or (not (member (field test 'name) (list language-module mutation-module)))
                      (equal? (field (field test 'result) 'acceptance-stage) stage))
                  (natural? (field test 'count))
                  (natural? (field test 'passed))
                  (natural? (field test 'failed))
                  (= (field test 'count) (+ (field test 'passed) (field test 'failed)))
                  (or (and (eq? (field test 'contract) #t)
                           (eq? (field test 'aborted) #f)
                           (positive? (field test 'count))
                           (equal? (field test 'pass) (zero? (field test 'failed))))
                      (and (eq? (field test 'contract) #f)
                           (eq? (field test 'aborted) #t)
                           (eq? (field test 'pass) #f)
                           (zero? (field test 'count))
                           (zero? (field test 'passed))
                           (zero? (field test 'failed))))))
            (for/and
             ((key '(count passed failed)))
             (and (natural? (field kernel key))
                  (= (field kernel key) (for/sum ((test (in-list tests))) (field test key))))))
     (raise-arguments-error
      'run-stage1-aggregate
      "required evidence is incomplete or belongs to the wrong group or revision"
      "group"
      group)))
  (define kernel-report (car reports))
  (define language-result (field (car (field (field (cadr reports) 'kernel) 'tests)) 'result))
  (define mutation-result (field (car (field (field (caddr reports) 'kernel) 'tests)) 'result))
  (define language-baseline (field language-result 'blind-full-baseline))
  (define mutation-baseline (field mutation-result 'blind-full-baseline))
  (define mutation-scoped (field mutation-result 'blind-baseline))
  (define scope (dynamic-require blind-runner-path 'blind-stage-scope))
  (define language-scoped (and (hash? language-baseline) (scope language-baseline stage)))
  (define baselines-present?
    (and (hash? language-baseline)
         (hash? mutation-baseline)
         (hash? mutation-scoped)
         (list? (invocation-ids language-baseline))
         (list? (invocation-ids mutation-baseline))
         (list? (invocation-ids mutation-scoped))
         (hash? (field language-baseline 'comparison-policy))
         (hash? (field mutation-baseline 'comparison-policy))))
  (define full-match? (and baselines-present? (equal? language-baseline mutation-baseline)))
  (define scoped-match? (and baselines-present? (equal? language-scoped mutation-scoped)))
  (define full-complete?
    (and (baseline-complete? language-baseline) (baseline-complete? mutation-baseline)))
  (define scoped-complete?
    (and (baseline-complete? language-scoped) (baseline-complete? mutation-scoped)))
  (define replay-valid? (and full-match? scoped-match? full-complete? scoped-complete?))
  (define selected-tests
    (append
     (filter
      (lambda (test) (not (member (field test 'name) (list language-module mutation-module))))
      (field (field kernel-report 'kernel) 'tests))
     (field (field (cadr reports) 'kernel) 'tests)
     (field (field (caddr reports) 'kernel) 'tests)))
  (define gates (measure-gates selected-tests #:stage stage))
  (define reports-pass?
    (for/and
     ((report (in-list reports)))
     (and replay-valid?
          (eq? (field (field report 'runtime) 'pass) #t)
          (eq? (field (field report 'kernel) 'pass) #t)
          (for/and
           ((key '(format publication package)))
           (eq? (field (field (field report 'kernel) key) 'pass) #t))
          (for/and
           ((test (in-list (field (field report 'kernel) 'tests))))
           (and (eq? (field test 'pass) #t)
                (eq? (field test 'contract) #t)
                (eq? (field test 'aborted) #f))))))
  (define aborted
    (append-map
     (lambda (report)
       (for/list
        ((test (in-list (field (field report 'kernel) 'tests))) #:when (field test 'aborted #f))
        (string-append (field report 'suite-group) ":" (field test 'name))))
     reports))
  (define (total key) (for/sum ((report (in-list reports))) (field (field report 'kernel) key 0)))
  (json-safe
   (hash
    'schema
    "typify-ci-v1"
    'source-revision
    revision
    'acceptance-stage
    stage
    'suite-group
    "aggregate"
    'report-complete
    #t
    'measurement-mode
    "full"
    'blind-deferred
    #f
    'replay-validation
    (hash
     'pass
     replay-valid?
     'baselines-present
     baselines-present?
     'full-baseline-equal
     full-match?
     'full-baseline-complete
     full-complete?
     'scoped-baseline-equal
     scoped-match?
     'scoped-baseline-complete
     scoped-complete?
     'full-invocation-ids-equal
     (and baselines-present?
          (equal? (invocation-ids language-baseline) (invocation-ids mutation-baseline)))
     'scoped-invocation-ids-equal
     (and baselines-present?
          (equal? (invocation-ids language-scoped) (invocation-ids mutation-scoped)))
     'comparison-policy-equal
     (and baselines-present?
          (equal?
           (field language-baseline 'comparison-policy)
           (field mutation-baseline 'comparison-policy))))
    'input-reports
    reports
    'kernel
    (hash
     'pass
     reports-pass?
     'tests
     selected-tests
     'count
     (for/sum ((test (in-list selected-tests))) (field test 'count))
     'passed
     (for/sum ((test (in-list selected-tests))) (field test 'passed))
     'failed
     (for/sum ((test (in-list selected-tests))) (field test 'failed))
     'aborted-suite-count
     (length aborted)
     'aborted-suites
     aborted)
    'execution
    (hash
     'count
     (total 'count)
     'passed
     (total 'passed)
     'failed
     (total 'failed)
     'scope
     "All reported checks across the internal job and both replay jobs; repeated internal checks are included. The kernel test list uses replay results for language and mutation gate evidence.")
    acceptance-key
    (hash
     'pass
     (and reports-pass? (andmap (lambda (gate) (field gate 'pass)) gates))
     'status
     (if replay-valid? "measured" "baseline-replay-disagreement")
     'unscored
     (unscored-gates stage)
     'gates
     gates))))

(module+ test-support (provide group-modules baseline-complete? gate-valid? gate-names stage-key))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (define (usage)
   (displayln
    "usage: racket scripts/ci.rkt [--stage 1|2|3] [--output FILE] [--defer-blind] [--group all|kernel|language|mutation] | --aggregate KERNEL LANGUAGE MUTATION [--stage 1|2|3] [--output FILE]"
    (current-error-port))
   (exit 2))
 (define-values
  (output-path blind? group aggregate-paths stage)
  (let loop ((remaining arguments)
             (output #f)
             (blind? #t)
             (group "all")
             (aggregate-paths #f)
             (stage #f))
    (match
     remaining
     ('()
      (when (and aggregate-paths (or (not blind?) (not (equal? group "all")))) (usage))
      (values output blind? group aggregate-paths (or stage 1)))
     ((list* "--defer-blind" rest)
      (if blind? (loop rest output #f group aggregate-paths stage) (usage)))
     ((list* "--stage" value rest)
      (if (and (not stage) (member value '("1" "2" "3")))
        (loop rest output blind? group aggregate-paths (string->number value))
        (usage)))
     ((list* "--group" name rest)
      (if (and (equal? group "all") (member name '("all" "kernel" "language" "mutation")))
        (loop rest output blind? name aggregate-paths stage)
        (usage)))
     ((list* "--aggregate" kernel language mutation rest)
      (if aggregate-paths
        (usage)
        (loop rest output blind? group (list kernel language mutation) stage)))
     ((list* "--output" path rest)
      (if output (usage) (loop rest path blind? group aggregate-paths stage)))
     (_ (usage)))))
 (define result
   (with-handlers
    ((exn:fail?
      (lambda (error)
        (hasheq
         'schema
         "typify-ci-v1"
         'acceptance-stage
         stage
         'kernel
         (hasheq 'pass #f 'error (exn-message error))
         (stage-key stage)
         (hasheq
          'pass
          #f
          'status
          (if aggregate-paths "evidence-unavailable" "kernel-unavailable"))))))
    (if aggregate-paths
      (keyword-apply run-stage-aggregate '(#:stage) (list stage) aggregate-paths)
      (run-kernel-check #:checkpoint output-path #:blind? blind? #:group group #:stage stage))))
 (when output-path
   (call-with-output-file
    output-path
    (lambda (output) (write-json result output) (newline output))
    #:exists
    'truncate/replace))
 (write-json result)
 (newline)
 (exit (if (field (field result (if aggregate-paths (stage-key stage) 'kernel)) 'pass) 0 1)))
