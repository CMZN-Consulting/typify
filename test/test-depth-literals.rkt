#lang racket/base

(require racket/list racket/string "../src/classes.rkt" "../src/syntax.rkt" "../src/language.rkt")

(provide run-tests)

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-depth-literals "~a: expected ~s; received ~s" context expected actual)))
  (define (compile/batch sources order (caps (hash)))
    (define errors '())
    (define classes
      (compile-classes
       sources
       #:order
       order
       #:caps
       caps
       #:on-error
       (lambda (error) (set! errors (cons error errors)))))
    (values classes (reverse errors)))
  (define-values
   (cycle-classes cycle-errors)
   (compile/batch (hash "entry" "z" "z" "a" "a" "z" "safe" "\"ok\"") '("entry" "z" "a" "safe")))
  (check (length cycle-errors) 1 "one diagnostic per strongly connected component")
  (check (class-error-code (car cycle-errors)) "class-reference-cycle" "cycle classification")
  (check
   (hash-ref (class-error-data (car cycle-errors)) 'class)
   "z"
   "cycle anchors at earliest SCC declaration")
  (check
   (hash-ref (class-error-data (car cycle-errors)) 'definition-name)
   "z"
   "cycle provenance uses the covered is line")
  (check
   (hash-ref (class-error-data (car cycle-errors)) 'chain)
   '("z" "a")
   "component members retain declaration order")
  (check
   (andmap (lambda (name) (not (hash-has-key? cycle-classes name))) '("entry" "z" "a"))
   #t
   "cycle-dependent classes do not compile")
  (check
   (class-accepts? (hash-ref cycle-classes "safe") "ok")
   #t
   "independent classes survive cycle recovery")
  (define-values
   (_multiple multiple-errors)
   (compile/batch (hash "z" "a" "a" "z" "self" "self" "p" "q" "q" "p") '("self" "z" "a" "q" "p")))
  (check
   (map (lambda (error) (hash-ref (class-error-data error) 'class)) multiple-errors)
   '("self" "z" "q")
   "separate cycles report once each in declaration order")
  (define chain-order (for/list ((index (in-range 11))) (format "c~a" index)))
  (define chain-sources
    (for/hash
     ((name (in-list chain-order)) (index (in-naturals)))
     (values name (if (zero? index) "\"x\"" (format "c~a" (sub1 index))))))
  (define-values
   (chain-classes chain-errors)
   (compile/batch chain-sources chain-order (hash 'depth 8)))
  (check
   (class-accepts? (hash-ref chain-classes "c8") "x")
   #t
   "eight edges from depth-zero class fit cap eight")
  (check
   (map
    (lambda (error)
      (list
       (hash-ref (class-error-data error) 'definition-name)
       (hash-ref (class-error-data error) 'actual)))
    chain-errors)
   '(("c9" 9) ("c10" 10))
   "all independent depth violations carry the exact full depth")
  (check
   (andmap (lambda (error) (equal? (class-error-code error) "class-reference-cap")) chain-errors)
   #t
   "depth diagnostics use the dedicated cap code")
  (define-values
   (inherited inherited-errors)
   (compile/batch
    (hash "inherited" "letter" "local" "inherited" "independent" "/[ab]+/")
    '("inherited" "local" "independent")
    (hash 'depth 1)))
  (check
   (class-accepts? (hash-ref inherited "inherited") "λ")
   #t
   "a built-in reference contributes one depth edge")
  (check
   (class-accepts? (hash-ref inherited "independent") "abba")
   #t
   "regular operators do not add reference depth")
  (check
   (map (lambda (error) (hash-ref (class-error-data error) 'actual)) inherited-errors)
   '(2)
   "inherited aliases use the same depth policy")
  (check
   (map line-record-kind (read-lines "item : a\rb\nleft\r -> right\n# note\r : value"))
   '(colon arrow colon)
   "refused points do not suppress recognizable line shapes")
  (check
   (line-record-right (car (read-lines "item : a\rb")))
   "a\rb"
   "lone CR stays in the captured token")
  (define (argument-value source) (literal-value (car (term-arguments (parse-term source)))))
  (for
   ((entry
     (in-list
      (list
       (cons "output ro" "ro")
       (cons "output \"ro\"" "ro")
       (cons "output \"two words\"" "two words")
       (cons "output \"\"" "")
       (cons "output \"a\\\"b\"" "a\"b")
       (cons "output \"a\\\\b\"" "a\\b")
       (cons "output \"\\x72\\u006f\"" "ro")
       (cons "output \"\\u{1F600}\"" "😀")
       (cons "output \"(<name>)\"" "(<name>)")))))
   (check
    (argument-value (car entry))
    (cdr entry)
    "quoted values follow the class-literal notation"))
  (check
   (instantiate-term (parse-term "output \"ro\"") (hash))
   (instantiate-term (parse-term "output ro") (hash))
   "quoted and bare spellings instantiate to the same argument")
  (define quoted (car (term-arguments (parse-term "emit \"λ 😀\"" #:file "quoted.typify"))))
  (check (literal-value quoted) "λ 😀" "decoded Unicode string")
  (check
   (literal-span quoted)
   (src-span "quoted.typify" 1 6 1 11 5 14)
   "literal span covers original quotes in code points and bytes")
  (for
   ((source
     (in-list
      '("output \"unterminated"
        "output \"bad\\q\""
        "output \"bad\\d\""
        "output \"\\uD800\""
        "output ab\"cd\""
        "output \"a\"b"
        "\"output\" value"))))
   (define error
     (with-handlers ((exn:fail:typify? exn:fail:typify-diagnostic)) (parse-term source) #f))
   (check
    (and error (diagnostic-code error))
    "E_DECLARATION"
    "malformed quoting is a declaration diagnostic")
   (check
    (and error (hash-ref (diagnostic-data error) 'kind))
    "syntax"
    "malformed quoting is syntax"))
  (define declaration
    (string-append
     "#lang typify\ndeclare : Quoted\nis : sample = /[a-z ]+/\n"
     "primitive : output : sample\nform : ready!\ndoes : output \"two words\"\nend!\n"))
  (define body
    (hash-ref (read-declarations declaration #:file "quoted-declaration.typify") "Quoted"))
  (check
   (hash-ref (check-program "ready!" body) 'terms)
   '(("output" "two words"))
   "does uses the decoded argument")
  (define cr-declaration
    (string-append
     "#lang typify\ndeclare : CR\nis : sample = /[a-z]+/\n"
     "primitive : output : sample\nform : item : <sample>\ndoes : output <sample>\nend!\n"))
  (define cr-body (hash-ref (read-declarations cr-declaration) "CR"))
  (define cr-diagnostics (hash-ref (check-program/diagnostics "item : a\rb" cr-body) 'diagnostics))
  (check
   (map diagnostic-code cr-diagnostics)
   '("E_CLASS")
   "class refusal follows a normally parsed CR-bearing shape")
  (check
   (hash-ref (diagnostic-data (car cr-diagnostics)) 'token)
   "a\rb"
   "refused token preserves lone CR")
  (define-values
   (_cycle-model native-cycle)
   (read-declarations/diagnostics
    "#lang typify\ndeclare : Cycle\nis : z = a\nis : a = z\nend!\n"
    #:file
    "cycle.typify"))
  (check (map diagnostic-code native-cycle) '("E_DECLARATION") "native cycle appears once")
  (check
   (diagnostic-data (car native-cycle))
   (hash 'kind "class-cycle" 'actual "z")
   "native cycle payload names covered class")
  (check
   (src-span-start-line (diagnostic-range (car native-cycle)))
   3
   "native cycle anchors at earliest is line")
  (define-values
   (_depth-model native-depth)
   (read-declarations/diagnostics
    "#lang typify\ndeclare : Depth\ncap : depth = 2\nis : local-base = \"x\"\nis : local-alias = local-base\nis : local-next = local-alias\nis : local-overflow = local-next\nend!\n"
    #:file
    "depth.typify"))
  (define depth-error (findf (lambda (item) (equal? (diagnostic-code item) "E_CAP")) native-depth))
  (check
   (and depth-error (diagnostic-data depth-error))
   (hash 'cap "depth" 'limit 2 'actual 3)
   "native cap reports exact structural depth")
  (check
   (and depth-error (src-span-start-line (diagnostic-range depth-error)))
   7
   "native depth cap covers the overflowing class is line")
  (hash 'suite "depth-literals" 'checks checks 'status "passed"))

(module+ test (run-tests))
