#lang racket/base

(require "../src/language.rkt" "../src/syntax.rkt")

(provide run-tests)

(define sample
  (string-append
   "#lang typify\n"
   "declare : Catalog\n"
   "is : category = \"book\" | \"journal\"\n"
   "is : title = /[^=]+/\n"
   "term : item : category title\n"
   "form : item : <category> = <title>\n"
   "does : item <category> <title>\n"
   "example : item : book = Algebra\n"
   "count : item = 1..*\n"
   "end!\n"))

(define (run-tests)
  (define declarations (read-declarations sample #:file "catalog-language.typify"))
  (define body (hash-ref declarations "Catalog"))
  (define checked (check-program "item : journal = Market Research\n" body #:file "catalog.typify"))
  (unless (equal? (hash-ref checked 'terms) '(("item" "journal" "Market Research")))
    (error 'smoke "The catalog meaning differs"))
  (unless (equal? (normalize-terms body (hash-ref checked 'terms)) (hash-ref checked 'terms))
    (error 'smoke "A primitive meaning changed during normalization"))
  (unless (equal?
           (declaration->jsexpr body #:origins? #f)
           (declaration->jsexpr
            (hash-ref (read-declarations (emit-declarations declarations)) "Catalog")
            #:origins?
            #f))
    (error 'smoke "The structural round trip differs"))
  (hash 'suite "smoke" 'checks 3 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
