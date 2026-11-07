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
    "test/test-language.rkt"
    "test/test-language-edge.rkt"
    "test/test-decoder.rkt"
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

(define (run-test root name)
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
        1
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
      ((dynamic-require (build-path root name) 'run-tests))))
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
    (if (natural? count) count 0)
    'passed
    (if (natural? passed) passed 0)
    'failed
    (if valid? failed 1)
    'contract
    valid?
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
       (natural? failures)
       (zero? failures)
       (case name
         ((blind-verdict)
          (and (eq? (field evidence 'blind) #t)
               (eq? (field evidence 'independent) #t)
               (text? (field evidence 'corpus))
               (text? (field evidence 'provenance))))
         ((decoder-differential)
          (and (string? (field evidence 'decoder-revision))
               (regexp-match? #px"^[0-9a-f]{40}$" (field evidence 'decoder-revision))
               (natural? (field evidence 'outside-formats))
               (>= (field evidence 'outside-formats) 5)
               (positive? (field evidence 'delimiter-insertions))))
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
         ((worked-example-classes) (equal? (field evidence 'host-predicates) 0))
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
      (if passed? "measured" "failed-evidence")
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

(define (run-kernel-check (given-root (current-directory)))
  (unless (equal? (getenv "GITHUB_ACTIONS") "true")
    (error 'ci "the full suite runs on GitHub Actions only"))
  (define root (simplify-path (path->complete-path given-root)))
  (define tests (for/list ((name test-modules)) (run-test root name)))
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
  (json-safe
   (hasheq
    'schema
    "typify-ci-v1"
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
     (apply + (map (lambda (test) (hash-ref test 'failed)) tests)))
    'stage1
    (hasheq
     'pass
     (and kernel-pass? (andmap (lambda (gate) (hash-ref gate 'pass)) gates))
     'gates
     gates))))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (unless (or (null? arguments) (and (= (length arguments) 2) (equal? (car arguments) "--output")))
   (displayln "usage: racket scripts/ci.rkt [--output FILE]" (current-error-port))
   (exit 2))
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
    (run-kernel-check)))
 (when (pair? arguments)
   (call-with-output-file
    (cadr arguments)
    (lambda (output) (write-json result output) (newline output))
    #:exists
    'truncate/replace))
 (write-json result)
 (newline)
 (exit (if (field (field result 'kernel) 'pass) 0 1)))
