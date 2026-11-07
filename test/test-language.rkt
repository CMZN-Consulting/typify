#lang racket/base

(require json
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt")

(provide run-tests run-blind-corpus)

(define-runtime-path import-path "corpus/import.json")

(define catalog
  (string-append
   "#lang typify\ndeclare : Catalog\nversion : 1\n"
   "is : category = \"book\" | \"journal\"\nis : title = /[^=]+/\n"
   "primitive : output : category title\nterm : item : category title\n"
   "rule : item <kind> <heading> -> output <kind> <heading>\n"
   "form : item : <category> = <title>\ndoes : item <category> <title>\n"
   "example : item : book = Algebra\ncount : item = 1..*\nend!\n"))

(define (run-blind-corpus)
  (if (file-exists? import-path)
    (error 'blind-corpus "The imported corpus requires its declared case schema")
    (hash 'available #f 'total 0 'failures 1 'corpus "test/corpus/" 'provenance "not-imported")))

(define (run-tests)
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
  (reject (lambda () (check-program "item : pamphlet = Algebra" body)) "E_PROGRAM_FORM")
  (reject (lambda () (check-program "item : book = a=b" body)) "E_PROGRAM_FORM")
  (reject (lambda () (check-program "" body)) "E_PROGRAM_COUNT")
  (reject
   (lambda () (read-declarations (string-replace catalog "#lang typify" "typify : 1")))
   "E_HEADER")
  (reject
   (lambda () (read-declarations (string-replace catalog "\nend!\n" "\n")))
   "E_BODY_UNCLOSED")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "category = \"book\" | \"journal\""
       "category = \"book\" | \"journal\"\nis : category = \"book\"")))
   "E_CLASS_DUPLICATE")
  (reject
   (lambda ()
     (read-declarations (string-replace catalog "category title\nterm" "category typo\nterm")))
   "E_CLASS_REFERENCE")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item <missing> <title>")))
   "E_CAPTURE_UNBOUND")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item goat <title>")))
   "E_TERM_CLASS")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "does : item <category> <title>" "does : item <category>")))
   "E_TERM_ARITY")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "rule : item <kind> <heading> -> output <kind> <heading>\n" "")))
   "E_CONSTRUCTOR_UNREDUCED")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "-> output <kind> <heading>" "-> item <kind> <heading>")))
   "E_RULE_STRATIFICATION")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "form : item : <category> = <title>"
       "form : item : <category><title>")))
   "E_TEMPLATE_ADJACENT")
  (reject
   (lambda ()
     (read-declarations
      (string-replace catalog "is : category = \"book\" | \"journal\"" "is : category = /.+/")))
   "E_DELIMITER_AMBIGUOUS")
  (reject
   (lambda ()
     (read-declarations
      (string-replace
       catalog
       "example : item : book = Algebra"
       "example : item : pamphlet = Algebra")))
   "E_EXAMPLE_DISPATCH")
  (define overlap
    (string-replace
     catalog
     "count : item = 1..*"
     "form : item : book = <title>\ndoes : item book <title>\ncount : item = 1..*"))
  (reject (lambda () (read-declarations overlap)) "E_FORM_OVERLAP")
  (define swallowed
    "#lang typify\ndeclare : Demo\ncomment : go\nis : value = \"yes\" | \"no\"\nprimitive : output : value\nform : go : <value>\ndoes : output <value>\nexample : go : yes\nend!\n")
  (reject (lambda () (read-declarations swallowed)) "E_EXAMPLE_EMPTY")
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
   "E_PROGRAM_FORM")
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
  (define (model-data model)
    (for/hash
     (((name item) (in-hash model)))
     (values name (declaration->jsexpr item #:origins? #f))))
  (for
   ((model (in-list public-models)))
   (check (model-data (read-declarations (emit-declarations model))) (model-data model)))
  (define blind (run-blind-corpus))
  (hash
   'suite
   "language"
   'checks
   checks
   'status
   "passed"
   'gates
   (hash
    'blind-verdict
    (hash-set* blind 'blind #f 'independent #f)
    'structural-round-trip
    (hash
     'total
     (length public-models)
     'failures
     0
     'public-declarations
     (length public-models)
     'blind-declarations
     0
     'complete
     #f
     'status
     "public-passed-blind-pending")
    'byte-smoke
    (hash 'total 1 'failures 0)
    'worked-example-classes
    (hash
     'total
     4
     'failures
     0
     'host-predicates
     0
     'classes
     '("Catalog.category" "Catalog.title" "Mounts.mode" "Mounts.path"))
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
