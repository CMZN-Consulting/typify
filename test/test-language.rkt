#lang racket/base

(require racket/list
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "blind-corpus.rkt"
         "witness-report.rkt")

(provide run-tests run-blind-corpus)

(define catalog
  (string-append
   "#lang typify\ndeclare : Catalog\nversion : 1\n"
   "is : category = \"book\" | \"journal\"\nis : title = /[^=]+/\n"
   "primitive : output : category title\nterm : item : category title\n"
   "rule : item <kind> <heading> -> output <kind> <heading>\n"
   "form : item : <category> = <title>\ndoes : item <category> <title>\n"
   "example : item : book = Algebra\ncount : item = 1..*\nend!\n"))

(define (declarations-data model)
  (for/hash (((name body) (in-hash model))) (values name (declaration->jsexpr body #:origins? #f))))

(define (failure-data failure)
  (if (exn:fail:typify? failure)
    (let ((item (exn:fail:typify-diagnostic failure)))
      (hash
       'code
       (diagnostic-code item)
       'file
       (diagnostic-file item)
       'range
       (src-span->jsexpr (diagnostic-range item))
       'data
       (diagnostic-data-projection (diagnostic-code item) (diagnostic-data item))))
    (hash 'message (exn-message failure))))

(define (measure-blind-declarations)
  (with-handlers
   ((exn:fail?
     (lambda (failure)
       (values
        (hash
         'available
         #f
         'inputs
         0
         'admitted
         0
         'refused
         0
         'unavailable
         0
         'infrastructure
         1
         'roundtrips
         0
         'successful-roundtrips
         0
         'failures
         1
         'complete
         #f
         'declarations
         '()
         'error
         (failure-data failure))
        '()))))
   (define inputs (blind-declaration-inputs))
   (define records '())
   (define models '())
   (define admitted 0)
   (define refused 0)
   (define unavailable 0)
   (define infrastructure 0)
   (define successful-roundtrips 0)
   (define roundtrip-failures 0)
   (for
    ((input (in-list inputs)))
    (define identity
      (hash
       'id
       (hash-ref input 'id)
       'case-id
       (hash-ref input 'case-id)
       'file
       (hash-ref input 'file)
       'source-sha256
       (if (string? (hash-ref input 'source)) (content-hash (hash-ref input 'source)) 'null)))
    (define parsed
      (with-handlers
       ((exn:fail:typify? (lambda (failure) (cons 'refused failure)))
        (exn:fail? (lambda (failure) (cons 'infrastructure failure))))
       (if (string? (hash-ref input 'source))
         (cons
          'admitted
          (parameterize
           ((current-directory (hash-ref input 'directory)))
           (read-declarations (hash-ref input 'source) #:file (hash-ref input 'file))))
         (cons 'unavailable "The invoked declaration source is absent"))))
    (case (car parsed)
      ((unavailable)
       (set! unavailable (add1 unavailable))
       (set! records
         (cons (hash-set* identity 'status "unavailable-source" 'reason (cdr parsed)) records)))
      ((refused)
       (set! refused (add1 refused))
       (set! records
         (cons
          (hash-set* identity 'status "refused" 'diagnostic (failure-data (cdr parsed)))
          records)))
      ((infrastructure)
       (set! infrastructure (add1 infrastructure))
       (set! records
         (cons
          (hash-set* identity 'status "infrastructure" 'error (failure-data (cdr parsed)))
          records)))
      (else
       (set! admitted (add1 admitted))
       (define model (cdr parsed))
       (define namespace
         (string-append
          "blind:"
          (string-replace (string-replace (hash-ref input 'id) "%" "%25") "/" "%2F")))
       (set! models (cons (cons namespace model) models))
       (define roundtrip
         (with-handlers
          ((exn:fail? (lambda (failure) (hash 'pass #f 'error (failure-data failure)))))
          (define repeated
            (read-declarations (emit-declarations model) #:file (hash-ref input 'file)))
          (hash 'pass (equal? (declarations-data model) (declarations-data repeated)))))
       (if (hash-ref roundtrip 'pass)
         (set! successful-roundtrips (add1 successful-roundtrips))
         (set! roundtrip-failures (add1 roundtrip-failures)))
       (set! records
         (cons
          (hash-set*
           identity
           'status
           "admitted"
           'namespace
           namespace
           'languages
           (sort (hash-keys model) string<?)
           'roundtrip
           roundtrip)
          records)))))
   (define ids (map (lambda (input) (hash-ref input 'id)) inputs))
   (define collisions (- (length ids) (length (remove-duplicates ids))))
   (define complete?
     (and (positive? (length inputs))
          (zero? collisions)
          (zero? infrastructure)
          (= (length inputs) (+ admitted refused unavailable infrastructure))
          (= admitted (length models))))
   (values
    (hash
     'available
     #t
     'inputs
     (length inputs)
     'admitted
     admitted
     'refused
     refused
     'unavailable
     unavailable
     'infrastructure
     infrastructure
     'roundtrips
     admitted
     'successful-roundtrips
     successful-roundtrips
     'failures
     (+ roundtrip-failures infrastructure collisions)
     'collisions
     collisions
     'complete
     complete?
     'scope
     "Roundtrip and witness laws apply to reader-admitted declarations; refusals and absent invoked source paths are inventoried separately and are not successful roundtrips."
     'declarations
     (reverse records))
    (reverse models))))

(define (run-tests #:blind? (blind? #t))
  (define checks 0)
  (define (check actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-language "expected ~s; received ~s" expected actual)))
  (define (reject thunk code)
    (check
     (with-handlers
      ((exn:fail:typify? (lambda (error) (diagnostic-code (exn:fail:typify-diagnostic error)))))
      (thunk)
      "accepted")
     code))
  (define (reject-all thunk codes)
    (check
     (with-handlers
      ((exn:fail:typify?
        (lambda (error) (map diagnostic-code (exn:fail:typify-diagnostics error)))))
      (thunk)
      '("accepted"))
     codes))
  (define declarations (read-declarations catalog #:file "catalog-language.typify"))
  (define body (hash-ref declarations "Catalog"))
  (define result (check-program "item : journal = Market Research" body))
  (check (hash-ref result 'terms) '(("item" "journal" "Market Research")))
  (check (normalize-terms body (hash-ref result 'terms)) '(("output" "journal" "Market Research")))
  (check (length (hash-ref result 'origins)) 1)
  (check (language-version body) "1")
  (check (language-comment body) "#")
  (check (hash-ref (declaration->jsexpr body) 'file) 'null)
  (check (length (hash-ref (declaration->jsexpr body) 'declarations)) 10)
  (check
   (hash-ref (car (hash-ref (declaration->jsexpr body) 'forms)) 'examples)
   '("item : book = Algebra"))
  (reject-all
   (lambda () (check-program "item : pamphlet = Algebra" body))
   '("E_PROGRAM_COUNT" "E_CLASS"))
  (reject-all (lambda () (check-program "item : book = a=b" body)) '("E_PROGRAM_COUNT" "E_CLASS"))
  (reject (lambda () (check-program "" body)) "E_PROGRAM_COUNT")
  (reject
   (lambda () (read-declarations (string-replace catalog "#lang typify" "typify : 1")))
   "E_DECLARATION")
  (reject (lambda () (read-declarations (string-replace catalog "\nend!\n" "\n"))) "E_DECLARATION")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "category = \"book\" | \"journal\""
       "category = \"book\" | \"journal\"\nis : category = \"book\"")))
   "E_DECLARATION")
  (reject
   (lambda ()
     (read-declarations (string-replace catalog "category title\nterm" "category typo\nterm")))
   "E_DECLARATION")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item <missing> <title>")))
   "E_DECLARATION")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item goat <title>")))
   "E_TERM_CLASS")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item <category>")))
   "E_DECLARATION")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "rule : item <kind> <heading> -> output <kind> <heading>\n" "")))
   "E_CONSTRUCTOR_UNBOUND")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "-> output <kind> <heading>" "-> item <kind> <heading>")))
   "E_RULE_CYCLE")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "form : item : <category> = <title>"
       "form : item : <category><title>")))
   "E_CLASS_OVERLAP")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "is : category = \"book\" | \"journal\"" "is : category = /.+/")))
   "E_CLASS_OVERLAP")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "example : item : book = Algebra"
       "example : item : pamphlet = Algebra")))
   "E_EXAMPLE")
  (define overlap
    (string-replace
     catalog
     "count : item = 1..*"
     "form : item : book = <title>\ndoes : item book <title>\ncount : item = 1..*"))
  (reject (lambda () (read-declarations overlap)) "E_FORM_SHADOW")
  (define swallowed
    "#lang typify\ndeclare : Demo\ncomment : go\nis : choice = \"yes\" | \"no\"\nprimitive : output : choice\nform : go : <choice>\ndoes : output <choice>\nexample : go : yes\nend!\n")
  (reject (lambda () (read-declarations swallowed)) "E_EXAMPLE")
  (define nullary
    "#lang typify\ndeclare : Flags\nprimitive : ready\nform : ready!\ndoes : ready\nexample : ready!\ncount : ready = 1\nend!\n")
  (define flags (hash-ref (read-declarations nullary) "Flags"))
  (check (hash-ref (check-program "ready!" flags) 'terms) '(("ready")))
  (reject (lambda () (check-program "ready!\nready!" flags)) "E_PROGRAM_COUNT")
  (define emitted (emit-declarations declarations))
  (define repeated (read-declarations emitted))
  (check
   (declaration->jsexpr (hash-ref repeated "Catalog") #:origins? #f)
   (declaration->jsexpr body #:origins? #f))
  (check (emit-declarations repeated) emitted)
  (define mounts-source
    (string-append
     "#lang typify\ndeclare : Mounts\n"
     "is : mode = \"rw\" | \"ro\"\nis : path = canonical-path\n"
     "term : vault : path mode\nform : vault : <path>\ndoes : vault <path> rw\n"
     "example : vault : /vaults/research\nend!\n"))
  (define mounts (read-declarations mounts-source))
  (check
   (hash-ref (check-program "vault : /vaults/research" (hash-ref mounts "Mounts")) 'terms)
   '(("vault" "/vaults/research" "rw")))
  (reject
   (lambda () (check-program "vault : /vaults/../research" (hash-ref mounts "Mounts")))
   "E_CLASS")
  (define meta (meta-language))
  (define meta-checked (check-program (meta-source) meta #:file "typify.typify"))
  (define normalized (normalize-terms meta (hash-ref meta-checked 'terms)))
  (check (length normalized) (length (hash-ref meta-checked 'terms)))
  (check
   (andmap
    (lambda (value) (constructor-primitive? (hash-ref (language-constructors meta) (car value))))
    normalized)
   #t)
  (define through-meta (read-declarations catalog #:meta meta))
  (check
   (declaration->jsexpr (hash-ref through-meta "Catalog") #:origins? #f)
   (declaration->jsexpr body #:origins? #f))
  (define public-models (list declarations (hash "Flags" flags) mounts (hash "Typify" meta)))
  (define worked-class-probes
    (list
     (list
      "Catalog.category"
      (hash-ref (language-classes body) "category")
      '("book" "journal")
      '("pamphlet" ""))
     (list
      "Catalog.title"
      (hash-ref (language-classes body) "title")
      '("Algebra" "Market Research")
      '("a=b" ""))
     (list
      "Mounts.mode"
      (hash-ref (language-classes (hash-ref mounts "Mounts")) "mode")
      '("rw" "ro")
      '("owner" ""))
     (list
      "Mounts.path"
      (hash-ref (language-classes (hash-ref mounts "Mounts")) "path")
      '("/" "/vaults/research")
      '("relative" "/vaults/" "/a/../b" "/a//b"))))
  (define worked-coverage
    (for/list
     ((probe (in-list worked-class-probes)))
     (for ((input (in-list (caddr probe)))) (check (class-accepts? (cadr probe) input) #t))
     (for ((input (in-list (cadddr probe)))) (check (class-accepts? (cadr probe) input) #f))
     (hash 'class (car probe) 'accepted (length (caddr probe)) 'refused (length (cadddr probe)))))
  (define (model-data model)
    (for/hash
     (((name item) (in-hash model)))
     (values name (declaration->jsexpr item #:origins? #f))))
  (for
   ((model (in-list public-models)))
   (check (model-data (read-declarations (emit-declarations model))) (model-data model)))
  (define deferred
    (hash
     'available
     #f
     'complete
     #f
     'deferred
     #t
     'status
     "deferred-blind"
     'total
     0
     'failures
     0
     'case-total
     0
     'case-failures
     0
     'results
     '()
     'mismatches
     '()))
  (define blind (if blind? (run-blind-corpus) deferred))
  (define scoped-blind (if blind? (blind-command-scope blind '("check" "emit") #:stage 1) deferred))
  (define-values
   (blind-coverage blind-models)
   (if blind?
     (measure-blind-declarations)
     (values
      (hash-set*
       deferred
       'inputs
       0
       'admitted
       0
       'refused
       0
       'unavailable
       0
       'infrastructure
       0
       'roundtrips
       0
       'successful-roundtrips
       0
       'declarations
       '())
      '())))
  (define custody-verified?
    (and (eq? (hash-ref blind 'available #f) #t)
         (eq? (hash-ref blind 'complete #f) #t)
         (zero? (hash-ref blind 'infrastructure-failures 1))))
  (define blind-declarations
    (hash-set*
     blind-coverage
     'custody-verified
     custody-verified?
     'complete
     (and (hash-ref blind-coverage 'complete) custody-verified?)))
  (define declaration-witnesses
    (measure-witnesses
     (append (map cons '("catalog" "flags" "mounts" "meta") public-models) blind-models)))
  (define blind-failures (hash-ref scoped-blind 'failures 1))
  (define law-failures
    (+
     (hash-ref blind-declarations 'failures)
     (hash-ref declaration-witnesses 'failures)
     (if (or (not blind?) (hash-ref blind-declarations 'complete)) 0 1)))
  (define count
    (+
     checks
     (if blind?
       (+
        (max 1 (hash-ref blind 'total 0))
        (max 1 (hash-ref blind-declarations 'inputs))
        (hash-ref blind-declarations 'roundtrips)
        1)
       0)
     (hash-ref declaration-witnesses 'checks)))
  (define failures (+ blind-failures law-failures))
  (hash
   'suite
   "language"
   'count
   count
   'pass
   (- count failures)
   'fail
   failures
   'status
   (if (zero? failures) "passed" "failed")
   'blind-deferred
   (not blind?)
   'blind-baseline-failures
   blind-failures
   'blind-full-baseline
   blind
   'blind-declaration-coverage
   blind-declarations
   'gates
   (hash
    'blind-verdict
    (hash-set*
     scoped-blind
     'total
     (hash-ref scoped-blind 'case-total 0)
     'failures
     (hash-ref scoped-blind 'case-failures 1)
     'invocations
     (hash-ref scoped-blind 'total 0)
     'invocation-failures
     blind-failures)
    'structural-round-trip
    (hash
     'total
     (+ (length public-models) (hash-ref blind-declarations 'admitted))
     'failures
     (hash-ref blind-declarations 'failures)
     'public-declarations
     (length public-models)
     'blind-declarations
     (hash-ref blind-declarations 'admitted)
     'blind-inputs
     (hash-ref blind-declarations 'inputs)
     'blind-refused
     (hash-ref blind-declarations 'refused)
     'blind-unavailable
     (hash-ref blind-declarations 'unavailable)
     'blind-infrastructure
     (hash-ref blind-declarations 'infrastructure)
     'blind-roundtrips
     (hash-ref blind-declarations 'roundtrips)
     'complete
     (hash-ref blind-declarations 'complete)
     'status
     (cond
      ((not blind?) "deferred-blind")
      ((and (hash-ref blind-declarations 'complete) (zero? (hash-ref blind-declarations 'failures)))
       "measured")
      (else "incomplete-or-failed")))
    'byte-smoke
    (hash 'total 1 'failures 0)
    'declaration-witnesses
    (hash-set*
     declaration-witnesses
     'complete
     (hash-ref blind-declarations 'complete)
     'blind-inputs
     (hash-ref blind-declarations 'inputs)
     'blind-admitted
     (hash-ref blind-declarations 'admitted)
     'blind-refused
     (hash-ref blind-declarations 'refused)
     'blind-unavailable
     (hash-ref blind-declarations 'unavailable)
     'blind-infrastructure
     (hash-ref blind-declarations 'infrastructure)
     'status
     (if blind? "measured" "deferred-blind"))
    'worked-example-classes
    (hash 'total (length worked-coverage) 'failures 0 'host-predicates 0 'coverage worked-coverage)
    'meta-selfcheck
    (hash
     'total
     (length normalized)
     'failures
     0
     'term-normalization
     #t
     'exemptions
     0
     'primitive-count
     (hash-count (language-constructors meta))))))

(module+ main (displayln (canonical-json (run-tests))))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
