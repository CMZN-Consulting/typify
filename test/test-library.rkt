#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         (submod "../src/language.rkt" test-support))

(provide run-tests)

(define span (src-span "library-input.typify" 1 1 1 2 0 1))

(define (library name . args) (library-call name args span))

(define (source lines)
  (string-append
   (string-join
    (append '("#lang typify" "declare : Library" "cap : length = 32") lines '("end!"))
    "\n")
   "\n"))

(define (body lines) (hash-ref (read-declarations (source lines)) "Library"))

(define (refusal thunk)
  (with-handlers
   ((exn:fail:typify? (lambda (error) (exn:fail:typify-diagnostic error))))
   (thunk)
   #f))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-library "~a: expected ~s; received ~s" context expected actual)))
  (define (reject thunk code context)
    (define diagnostic (refusal thunk))
    (check (and diagnostic (diagnostic-code diagnostic)) code context)
    diagnostic)
  (check (hash-ref library-arities "split") '(1) "split has one argument")
  (check (library "parent" "/a/b") "/a" "parent removes the final component")
  (check (library "parent" "/a") "/" "parent of a root child is root")
  (check (library "parent" "/東京/Ελλάδα") "/東京" "parent preserves Unicode components")
  (reject (lambda () (library "parent" "/")) "E_LIBRARY_PARENT" "root has no parent")
  (reject (lambda () (library "parent" "")) "E_LIBRARY_DOMAIN" "empty is outside the path domain")
  (check (library "join" "/a" "b") "/a/b" "join inserts one slash")
  (check (library "join" "/" "a") "/a" "join keeps a canonical root prefix")
  (check (library "join" "/東京" "Ελλάδα") "/東京/Ελλάδα" "join preserves Unicode components")
  (for
   ((absolute '("/" "/b" "/a/b")))
   (define diagnostic
     (reject
      (lambda () (library "join" "/a" absolute))
      "E_LIBRARY_DOMAIN"
      "join refuses an absolute second argument"))
   (check
    (hash-ref (diagnostic-data diagnostic) 'argument)
    2
    "join refusal identifies the second argument"))
  (check (library "split" "/a/b") '("a" "b") "split returns path components")
  (check (library "split" "/") '() "root has zero path components")
  (check (library "split" "/東京/Ελλάδα") '("東京" "Ελλάδα") "split preserves Unicode components")
  (check (library "ancestors" "/a/b/c") '("/" "/a" "/a/b") "proper ancestors run root to parent")
  (check (library "ancestors" "/a") '("/") "root is the only ancestor of a root child")
  (check (library "ancestors" "/") '() "root has no proper ancestor")
  (check (library "concat" "ab" "cd") "abcd" "concat joins strings without a separator")
  (check (library "concat" "" "") "" "concat preserves empty strings")
  (check (library "concat" "東京" "😀") "東京😀" "concat preserves code points")
  (define invalid-paths
    (append
     '("" "." ".." "relative" "//" "/a/" "/a//b" "/./a" "/a/../b" "/a/." "/a/..")
     (for/list
      ((point '(0 9 10 13 160 8205 8232 8233)))
      (string-append "/a" (string (integer->char point)) "b"))))
  (for*
   ((function '("parent" "split" "ancestors")) (path (in-list invalid-paths)))
   (reject
    (lambda () (library-call function (list path) span))
    "E_LIBRARY_DOMAIN"
    "path functions reject noncanonical or unsafe input"))
  (for
   ((path (in-list invalid-paths)))
   (define diagnostic
     (reject
      (lambda () (library "join" path "b"))
      "E_LIBRARY_DOMAIN"
      "join validates its canonical first argument"))
   (check
    (hash-ref (diagnostic-data diagnostic) 'argument)
    1
    "join checks its first argument first"))
  (for
   ((relative '("" "/" "/b" "a/" "a//b" "." ".." "./a" "a/./b" "a/../b" "a/..")))
   (reject
    (lambda () (library "join" "/a" relative))
    "E_LIBRARY_DOMAIN"
    "join requires clean nonempty relative components"))
  (check (library "join" "/a" "b/c") "/a/b/c" "join permits multiple clean relative components")
  (check (library "join" "/a" "b c") "/a/b c" "ordinary space is allowed inside a component")
  (for*
   ((function '("concat" "prefix" "suffix" "equal"))
    (point '(0 9 10 13 160 8205 8232 8233))
    (position '(1 2)))
   (define unsafe (string (integer->char point)))
   (reject
    (lambda () (library-call function (if (= position 1) (list unsafe "a") (list "a" unsafe)) span))
    "E_LIBRARY_DOMAIN"
    "string functions use the same allowed code-point domain"))
  (check (library "concat" "a " " b") "a  b" "allowed strings retain ordinary spaces")
  (define long-component (make-string 4097 #\a))
  (check
   (library "parent" (string-append "/" long-component))
   "/"
   "the library path domain has no implicit capture-length limit")
  (check
   (string-length (library "concat" long-component "b"))
   4098
   "string functions do not acquire an unrelated capture-length limit")
  (for
   ((case (in-list
           '(("prefix" "ab" "abc" #t)
             ("prefix" "bc" "abc" #f)
             ("suffix" "bc" "abc" #t)
             ("suffix" "ab" "abc" #f)
             ("equal" "abc" "abc" #t)
             ("equal" "ab" "abc" #f)))))
   (check
    (library-call (car case) (list (cadr case) (caddr case)) span)
    (cadddr case)
    "predicate results are booleans"))
  (for
   ((predicate '("prefix" "suffix" "equal")))
   (define diagnostic
     (reject
      (lambda ()
        (body
         (list
          "primitive : output : value"
          "form : go!"
          (string-append "does : output (" predicate " a b)"))))
      "E_TERM_CLASS"
      "does cannot put a predicate in a string position"))
   (check
    (hash-ref (diagnostic-data diagnostic) 'actual)
    "predicate"
    "predicate refusal carries its result type")
   (check
    (hash-ref (diagnostic-data diagnostic) 'allowed-contexts)
    '("if" "ensure")
    "predicate refusal names its only allowed contexts")
   (reject
    (lambda ()
      (body
       (list
        "primitive : output : value"
        "term : transform : value"
        (string-append "rule : transform <input> -> output (" predicate " a <input>)")
        "form : go : <value>"
        "does : transform <value>")))
    "E_TERM_CLASS"
    "a rule cannot put a predicate in a string position"))
  (for
   ((case '(("concat a b" "string") ("prefix a b" "predicate") ("split /a/b" "list"))))
   (define diagnostic
     (reject
      (lambda ()
        (body
         (list "primitive : output : value" "form : go!" (string-append "does : " (car case)))))
      "E_TERM_CLASS"
      "a top-level does builds a constructor rather than returning a library value"))
   (check
    (hash-ref (diagnostic-data diagnostic) 'actual)
    (cadr case)
    "top-level library refusal reports its result type"))
  (reject
   (lambda ()
     (body '("primitive : output : value" "form : go!" "does : output (concat a (equal b b))")))
   "E_TERM_CLASS"
   "a predicate cannot be hidden inside a string-valued library call")
  (for
   ((expression '("(split /a/b)" "(ancestors /a/b)")))
   (define diagnostic
     (reject
      (lambda ()
        (body
         (list
          "primitive : output : value"
          "form : go!"
          (string-append "does : output " expression))))
      "E_TERM_CLASS"
      "list-valued functions are unavailable in stage-one string positions"))
   (check
    (hash-ref (diagnostic-data diagnostic) 'actual)
    "list"
    "list refusal carries its result type"))
  (define composed
    (body
     '("primitive : output : value"
       "form : go!"
       "does : output (concat (parent /a/b) (join / c))")))
  (check
   (hash-ref (check-program "go!" composed) 'terms)
   '(("output" "/a/c"))
   "nested string calls evaluate innermost first")
  (define parent-first
    (body
     '("primitive : output : value value" "form : go!" "does : output (parent /) (join /a /b)")))
  (define join-first
    (body
     '("primitive : output : value value" "form : go!" "does : output (join /a /b) (parent /)")))
  (reject
   (lambda () (check-program "go!" parent-first))
   "E_LIBRARY_PARENT"
   "the first failing library argument is reported left to right")
  (reject
   (lambda () (check-program "go!" join-first))
   "E_LIBRARY_DOMAIN"
   "reversing failing arguments reverses the first diagnostic")
  (reject
   (lambda () (normalize-terms composed '(("output" ("concat" ("parent" "/") ("join" "/a" "/b"))))))
   "E_LIBRARY_PARENT"
   "normalization uses the same first-failure order")
  (hash 'suite "library" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
