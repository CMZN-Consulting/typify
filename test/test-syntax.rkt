#lang racket/base

(require racket/list "../src/syntax.rkt")

(provide run-tests)

(define (run-tests)
  (define checks 0)
  (define (check actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-syntax "expected ~s; received ~s" expected actual)))
  (define (reject procedure code)
    (check
     (with-handlers
      ((exn:fail:typify? (lambda (failure) (diagnostic-code (exn:fail:typify-diagnostic failure)))))
      (procedure)
      "accepted")
     code))
  (define lines
    (read-lines
     "  # annotation\r\n a : b -> c \r\nleft -> right\rflag!\ninvalid\n"
     #:file
     "input.txt"))
  (check (map line-record-kind lines) '(comment colon arrow bang unclassified blank))
  (check (line-record-left (second lines)) "a")
  (check (line-record-right (second lines)) "b -> c")
  (check (line-record-left (third lines)) "left")
  (check (line-record-right (third lines)) "right")
  (check (line-record-left (fourth lines)) "flag")
  (check (line-record-kind (car (read-lines "form : end!"))) 'colon)
  (check (line-record-kind (car (read-lines "a -> b : c"))) 'colon)
  (check (line-record-kind (car (read-lines "a -> b!"))) 'arrow)
  (check (line-record-kind (car (read-lines "!"))) 'unclassified)
  (check (line-record-kind (car (read-lines "a b!"))) 'unclassified)
  (check (line-record-kind (car (read-lines "// note" #:comment-prefix "//"))) 'comment)
  (check (line-record-kind (car (read-lines "# literal : value" #:comment-prefix #f))) 'colon)
  (define unicode-lines (read-lines "  item : 😀x\r\nnext!" #:file "folder\\input.txt"))
  (define first-span (line-record-span (car unicode-lines)))
  (check first-span (src-span "folder/input.txt" 1 3 1 12 2 14))
  (check (line-left-span (car unicode-lines)) (src-span "folder/input.txt" 1 3 1 7 2 6))
  (check (line-right-span (car unicode-lines)) (src-span "folder/input.txt" 1 10 1 12 9 14))
  (check (line-record-span (second unicode-lines)) (src-span "folder/input.txt" 2 1 2 6 16 21))
  (check (line-record-raw (car unicode-lines)) "  item : 😀x")
  (check (line-record-left (car (read-lines " word : value"))) " word")
  (define anchor (src-span "input.txt" 1 1 1 1 0 0))
  (check (span-slice anchor "α😀\r\nz" 1 2) (src-span "input.txt" 1 2 1 3 2 6))
  (check (span-slice anchor "α😀\r\nz" 0 5) (src-span "input.txt" 1 1 2 2 0 9))
  (define parts (parse-template "item : <category> = <title>"))
  (check (map slot-name (filter slot? parts)) '("category" "title"))
  (check (match-template parts "item : book = Algebra") (hash "category" "book" "title" "Algebra"))
  (check
   (match-template parts "item : book = Market Research")
   (hash "category" "book" "title" "Market Research"))
  (check (match-template parts "wrong : book = Algebra") #f)
  (check
   (match-template (parse-template "<a> = <b> ; done") "x = y = z ; done")
   (hash "a" "x" "b" "y = z"))
  (check (match-template (parse-template "tag <x> end") "tag a end trailing") #f)
  (check (match-template (parse-template "<x>:") ":") (hash "x" ""))
  (check (match-template (parse-template "δ<word>ω") "δ😀ω") (hash "word" "😀"))
  (check (match-template (parse-template "<x>") " \tvalue\t ") (hash "x" " \tvalue\t "))
  (check (match-template (parse-template "[<word>]") "[ a ]") (hash "word" " a "))
  (check (match-template (parse-template "[<word>]") "[a]") (hash "word" "a"))
  (check (match-template (parse-template "<x>") " value ") (hash "x" " value "))
  (check (match-template (parse-template "literal") "literal") (hash))
  (check (match-template (parse-template "") "") (hash))
  (reject (lambda () (parse-template "<x> and <x>")) "E_TEMPLATE_DUPLICATE")
  (reject (lambda () (parse-template "<a><b>")) "E_TEMPLATE_ADJACENT")
  (reject (lambda () (parse-template "<a")) "E_TEMPLATE")
  (reject (lambda () (parse-template "< spaced >")) "E_TEMPLATE")
  (define expression (parse-term "item <category> (concat <title> suffix)"))
  (check (term-constructor expression) "item")
  (check
   (instantiate-term expression (hash "category" "book" "title" "Market Research"))
   '("item" "book" ("concat" "Market Research" "suffix")))
  (check (instantiate-term (parse-term "(bound <path>)") (hash "path" "/data")) '("bound" "/data"))
  (check (instantiate-term (parse-term "stop") (hash)) '("stop"))
  (check
   (instantiate-term (parse-term "value <x>") (hash "x" "(not executable)"))
   '("value" "(not executable)"))
  (check (term-span (parse-term "  value 😀  ")) (src-span "<input>" 1 3 1 10 2 12))
  (define located (parse-term "emit <x>" #:span (src-span "terms.txt" 4 8 4 16 30 38)))
  (check (capture-span (first (term-arguments located))) (src-span "terms.txt" 4 13 4 16 35 38))
  (reject (lambda () (parse-term "")) "E_TERM")
  (reject (lambda () (parse-term "()")) "E_TERM")
  (reject (lambda () (parse-term "call (inner <x>")) "E_TERM")
  (reject (lambda () (parse-term "call <x")) "E_TERM")
  (reject (lambda () (parse-term "call <x>tail")) "E_TERM")
  (reject (lambda () (parse-term "call < spaced >")) "E_TERM")
  (reject (lambda () (parse-term "<x>")) "E_TERM")
  (reject (lambda () (parse-term "call )")) "E_TERM")
  (reject (lambda () (parse-term "(call) extra")) "E_TERM")
  (reject (lambda () (instantiate-term (parse-term "call <x>") (hash))) "E_CAPTURE")
  (define d1
    (make-diagnostic
     "E_TERM"
     "message"
     #:span
     (src-span "a\\b.txt" 2 3 2 5 7 11)
     #:verb
     "emit"
     #:ordinal
     2
     #:content
     "emit value"))
  (define d2
    (make-diagnostic
     "E_TERM"
     "message"
     #:span
     (src-span "a/b.txt" 5 3 5 5 17 21)
     #:verb
     "emit"
     #:ordinal
     2
     #:content
     "emit value"))
  (check (diagnostic-file d1) "a/b.txt")
  (check (diagnostic-fingerprints d1) (diagnostic-fingerprints d2))
  (check
   (hash-ref (diagnostic->jsexpr d1) 'range)
   (hasheq 'startLine 2 'startColumn 3 'endLine 2 'endColumn 5 'startByte 7 'endByte 11))
  (check (canonical-json (hash 'z 1 'a 2)) "{\"a\":2,\"z\":1}")
  (check
   (canonical-json (hash 'unicode "😀" 'array (list "x" (hash 'b 2 'a 1))))
   (canonical-json (hash 'array (list "x" (hash 'a 1 'b 2)) 'unicode "😀")))
  (check (string-length (content-hash "value")) 64)
  (hasheq 'suite "syntax" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
