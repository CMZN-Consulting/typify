#lang racket/base

(require json
         racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "format.rkt"
         "leak-check.rkt")

(provide run-kernel-check)

(define test-modules
  '("test/smoke.rkt"
    "test/test-automata-minimize.rkt"
    "test/test-classes.rkt"
    "test/test-syntax.rkt"
    "test/test-diagnostics.rkt"
    "test/test-declaration-diagnostics.rkt"
    "test/test-program-diagnostics.rkt"
    "test/test-reader-routed.rkt"
    "test/blind-corpus.rkt"
    "test/test-language.rkt"
    "test/test-language-edge.rkt"
    "test/test-library.rkt"
    "test/test-defaults.rkt"
    "test/test-emission.rkt"
    "test/test-bounds.rkt"
    "test/test-decoder.rkt"
    "test/test-import-corpus.rkt"
    "test/test-publication.rkt"
    "test/test-meta-mutation.rkt"))

(define gate-names
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

(define (field data key (default #f))
  (if (hash? data)
    (hash-ref data key (lambda () (hash-ref data (symbol->string key) default)))
    default))

(define (natural? value) (exact-nonnegative-integer? value))

(define (positive? value) (and (natural? value) (> value 0)))

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

(define (run-test root name #:blind? (blind? #t))
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
      (if (member name '("test/test-language.rkt" "test/test-meta-mutation.rkt"))
        (runner #:blind? blind?)
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
         (else #t))))

(define (measure-gates tests)
  (for/list
   ((name gate-names))
   (define supplied
     (for/list
      ((test tests) #:when (field (field (field test 'result) 'gates) name))
      (cons (field test 'name) (field (field (field test 'result) 'gates) name))))
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
         (blind? #t))
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
            'measurement-mode
            (if blind? "full" "kernel-only")
            'blind-deferred
            (not blind?)
            'phase
            phase
            'kernel
            (hasheq 'pass #f 'tests completed)
            'stage1
            (hasheq 'pass #f 'status "measurement-incomplete")))
          output)
         (newline output))
       #:exists
       'truncate/replace)))
  (define tests
    (for/list
     ((name test-modules))
     (save-progress name)
     (define result (run-test root name #:blind? blind?))
     (set! completed (append completed (list result)))
     (save-progress "between-suites")
     result))
  (define formatting (format-repository root))
  (define publication (check-repository root))
  (define package (package-metadata root))
  (define gates (measure-gates tests))
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
    'stage1
    (hasheq
     'pass
     (and blind? kernel-pass? (andmap (lambda (gate) (hash-ref gate 'pass)) gates))
     'status
     (if blind? "measured" "deferred-blind")
     'gates
     gates))))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (define (usage)
   (displayln "usage: racket scripts/ci.rkt [--output FILE] [--defer-blind]" (current-error-port))
   (exit 2))
 (define-values
  (output-path blind?)
  (let loop ((remaining arguments) (output #f) (blind? #t))
    (match
     remaining
     ('() (values output blind?))
     ((list* "--defer-blind" rest) (if blind? (loop rest output #f) (usage)))
     ((list* "--output" path rest) (if output (usage) (loop rest path blind?)))
     (_ (usage)))))
 (define result
   (with-handlers
    ((exn:fail?
      (lambda (error)
        (hasheq
         'schema
         "typify-ci-v1"
         'kernel
         (hasheq 'pass #f 'error (exn-message error))
         'stage1
         (hasheq 'pass #f 'status "kernel-unavailable")))))
    (run-kernel-check #:checkpoint output-path #:blind? blind?)))
 (when output-path
   (call-with-output-file
    output-path
    (lambda (output) (write-json result output) (newline output))
    #:exists
    'truncate/replace))
 (write-json result)
 (newline)
 (exit (if (field (field result 'kernel) 'pass) 0 1)))
