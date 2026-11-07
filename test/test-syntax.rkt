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
     "  # annotation\r\n a : b -> c \r\nleft -> right\nflag!\ninvalid\n"
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
  (for
   ((source '(":" " : value" "-> right" "left ->" " -> " "_bad!" "-bad!" "é!")))
   (check (line-record-kind (car (read-lines source))) 'unclassified))
  (for ((source '("0!" "A-b-9!"))) (check (line-record-kind (car (read-lines source))) 'bang))
  (check (line-record-kind (car (read-lines "verb :"))) 'colon)
  (check (verb-word? "0Alpha-beta") #t)
  (check (verb-word? "Alpha_beta") #f)
  (check (verb-word? "") #f)
  (define lone-cr (read-lines "a : x\rb : y\nnext!" #:file "cr.txt"))
  (check (map line-record-kind lone-cr) '(unclassified bang))
  (check (line-record-raw (car lone-cr)) "a : x\rb : y")
  (check (line-record-span (car lone-cr)) (src-span "cr.txt" 1 1 1 12 0 11))
  (check (line-record-span (second lone-cr)) (src-span "cr.txt" 2 1 2 6 12 17))
  (check (line-record-kind (car (read-lines "# comment\rhidden"))) 'unclassified)
  (check (line-record-kind (car (read-lines "\r"))) 'unclassified)
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
  (check (span-slice anchor "α\rz" 0 3) (src-span "input.txt" 1 1 1 4 0 4))
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
  (define (located-match template source)
    (call-with-values (lambda () (match-template/located (parse-template template) source)) list))
  (check (located-match "δ<word>ω" "δ😀ω") (list (hash "word" "😀") (hash "word" (cons 1 2))))
  (check
   (located-match "[<first>] = [<second>]" "[same] = [same]")
   (list (hash "first" "same" "second" "same") (hash "first" (cons 1 5) "second" (cons 10 14))))
  (check
   (located-match "<a> = <b> ; done" "x = y = z ; done")
   (list (hash "a" "x" "b" "y = z") (hash "a" (cons 0 1) "b" (cons 4 9))))
  (check (located-match "[<empty>]" "[]") (list (hash "empty" "") (hash "empty" (cons 1 1))))
  (check (located-match "literal" "literal") (list (hash) (hash)))
  (check (located-match "" "") (list (hash) (hash)))
  (check (located-match "tag <x> end" "tag a end trailing") (list #f #f))
  (check (located-match "<x> end" "a missing") (list #f #f))
  (check (located-match "<a> = <b> ; done" "x = y ; stop") (list #f #f))
  (define duplicate-fields (car (read-lines "  α[😀]  :  α[😀]  " #:file "captures.txt")))
  (define-values
   (left-captures left-offsets)
   (match-template/located (parse-template "α[<word>]") (line-record-left duplicate-fields)))
  (define-values
   (right-captures right-offsets)
   (match-template/located (parse-template "α[<word>]") (line-record-right duplicate-fields)))
  (check left-captures (hash "word" "😀"))
  (check right-captures (hash "word" "😀"))
  (check (and (immutable? left-captures) (immutable? left-offsets)) #t)
  (check (and (immutable? right-captures) (immutable? right-offsets)) #t)
  (check
   (span-slice
    (line-left-span duplicate-fields)
    (line-record-left duplicate-fields)
    (car (hash-ref left-offsets "word"))
    (cdr (hash-ref left-offsets "word")))
   (src-span "captures.txt" 1 5 1 6 5 9))
  (check
   (span-slice
    (line-right-span duplicate-fields)
    (line-record-right duplicate-fields)
    (car (hash-ref right-offsets "word"))
    (cdr (hash-ref right-offsets "word")))
   (src-span "captures.txt" 1 14 1 15 18 22))
  (define empty-field (car (read-lines "  item :   " #:file "empty.txt")))
  (define-values
   (empty-captures empty-offsets)
   (match-template/located (parse-template "<empty>") (line-record-right empty-field)))
  (check empty-captures (hash "empty" ""))
  (check empty-offsets (hash "empty" (cons 0 0)))
  (check
   (span-slice
    (line-right-span empty-field)
    (line-record-right empty-field)
    (car (hash-ref empty-offsets "empty"))
    (cdr (hash-ref empty-offsets "empty")))
   (src-span "empty.txt" 1 9 1 9 8 8))
  (reject (lambda () (parse-template "<x> and <x>")) "E_DECLARATION")
  (reject (lambda () (parse-template "<a><b>")) "E_CLASS_OVERLAP")
  (reject (lambda () (parse-template "<a")) "E_DECLARATION")
  (reject (lambda () (parse-template "< spaced >")) "E_DECLARATION")
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
  (reject (lambda () (parse-term "")) "E_DECLARATION")
  (reject (lambda () (parse-term "()")) "E_DECLARATION")
  (reject (lambda () (parse-term "call (inner <x>")) "E_DECLARATION")
  (reject (lambda () (parse-term "call <x")) "E_DECLARATION")
  (reject (lambda () (parse-term "call <x>tail")) "E_DECLARATION")
  (reject (lambda () (parse-term "call < spaced >")) "E_DECLARATION")
  (reject (lambda () (parse-term "<x>")) "E_DECLARATION")
  (reject (lambda () (parse-term "call )")) "E_DECLARATION")
  (reject (lambda () (parse-term "(call) extra")) "E_DECLARATION")
  (reject (lambda () (instantiate-term (parse-term "call <x>") (hash))) "E_DECLARATION")
  (define d1
    (make-diagnostic
     "E_DECLARATION"
     "message"
     #:span
     (src-span "a\\b.txt" 2 3 2 5 7 11)
     #:data
     (hash 'kind "syntax" 'actual "emit value")
     #:verb
     "emit"
     #:ordinal
     2
     #:content
     "emit value"))
  (define d2
    (make-diagnostic
     "E_DECLARATION"
     "message"
     #:span
     (src-span "a/b.txt" 5 3 5 5 17 21)
     #:data
     (hash 'kind "syntax" 'actual "emit value")
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
   (hasheq
    'start
    (hasheq 'line 2 'column 3 'byteOffset 7)
    'end
    (hasheq 'line 2 'column 5 'byteOffset 11)))
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
