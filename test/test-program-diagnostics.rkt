#lang racket/base

(require racket/list racket/string "../src/diagnostics.rkt" "../src/language.rkt")

(provide run-tests)

(define (external-primitive-fixture lines)
  (append
   lines
   (for/list
    ((line (in-list lines)) #:when (string-prefix? line "primitive : "))
    (string-append "effect : " (cadr (string-split line " : ")) " = external"))))

(define (source lines)
  (string-append
   (string-join
    (append
     '("#lang typify" "declare : Diagnostics" "cap : length = 32")
     (external-primitive-fixture lines)
     '("end!"))
    "\n")
   "\n"))

(define (body lines) (hash-ref (read-declarations (source lines)) "Diagnostics"))

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
      (error 'test-program-diagnostics "~a: expected ~s; received ~s" context expected actual)))
  (define (reject thunk code data)
    (define diagnostic (refusal thunk))
    (check (and diagnostic (diagnostic-code diagnostic)) code "native classification")
    (check (diagnostic-data diagnostic) data "native compared data")
    diagnostic)
  (define bracket
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "form : item : [<word>]"
       "does : output <word>")))
  (for
   ((text '("unshaped" "!" "two words!" "item" ":")))
   (reject (lambda () (check-program text bracket)) "E_LINE_SHAPE" (hash 'actual text)))
  (for
   ((text '("unknown!" "other : [a]" "item : missing-brackets" "item : [a")))
   (reject (lambda () (check-program text bracket)) "E_FORM_NONE" (hash 'actual text)))
  (define outer-raw " \tother : [a]\t ")
  (reject (lambda () (check-program outer-raw bracket)) "E_FORM_NONE" (hash 'actual "other : [a]"))
  (reject
   (lambda () (check-program "item:[123]" bracket))
   "E_CLASS"
   (hash 'class "word" 'token "123"))
  (define exact-spaces
    (reject
     (lambda () (check-program "item:[ a ]" bracket #:file "program.typify"))
     "E_CLASS"
     (hash 'class "word" 'token " a ")))
  (check
   (diagnostic-range exact-spaces)
   (src-span "program.typify" 1 7 1 10 6 9)
   "a capture range includes its exact inner spaces")
  (define empty-capture
    (reject
     (lambda () (check-program "item:[]" bracket #:file "program.typify"))
     "E_CLASS"
     (hash 'class "word" 'token "")))
  (check
   (diagnostic-range empty-capture)
   (src-span "program.typify" 1 7 1 7 6 6)
   "an empty failed capture has an exact empty range")
  (define alternatives
    (body
     '("is : a = \"x\""
       "is : b = \"y\""
       "primitive : first : a"
       "primitive : second : b"
       "form : item : <a>"
       "does : first <a>"
       "form : item : <b>"
       "does : second <b>")))
  (check
   (hash-ref (check-program "item:y" alternatives) 'terms)
   '(("second" "y"))
   "a later fully typed match wins over an earlier structural class failure")
  (check (line-accepts? "item:y" alternatives) #t "formation uses the same successful selection")
  (reject (lambda () (check-program "item:z" alternatives)) "E_CLASS" (hash 'class "a" 'token "z"))
  (define both-sides
    (body
     '("is : zleft = /[a-z]+/"
       "is : aright = /[a-z]+/"
       "primitive : pair : zleft aright"
       "form : <zleft> : [<aright>]"
       "does : pair <zleft> <aright>")))
  (define left-first
    (reject
     (lambda () (check-program "9:[8]" both-sides #:file "program.typify"))
     "E_CLASS"
     (hash 'class "zleft" 'token "9")))
  (check
   (diagnostic-range left-first)
   (src-span "program.typify" 1 1 1 2 0 1)
   "left-field slot failure precedes right-field failure regardless of hash order")
  (define same-side
    (body
     '("is : zfirst = /[a-z]+/"
       "is : asecond = /[a-z]+/"
       "primitive : pair : zfirst asecond"
       "form : pair : <zfirst>-<asecond>"
       "does : pair <zfirst> <asecond>")))
  (reject
   (lambda () (check-program "pair:9-8" same-side))
   "E_CLASS"
   (hash 'class "zfirst" 'token "9"))
  (define unicode
    (body
     '("is : broad = /[a-z0-9😀]+/"
       "is : digits = /[0-9]+/"
       "primitive : pair : broad digits"
       "form : range : <broad>|<digits>"
       "does : pair <broad> <digits>")))
  (define prefix " \t range \t: \t")
  (define token "😀x")
  (define before (string-append "# ignored\r\n" prefix token "|"))
  (define unicode-result
    (reject
     (lambda ()
       (check-program (string-append before token " \t") unicode #:file "unicode-program.typify"))
     "E_CLASS"
     (hash 'class "digits" 'token token)))
  (define column (+ (string-length prefix) (string-length token) 2))
  (define byte (bytes-length (string->bytes/utf-8 before)))
  (check
   (diagnostic-range unicode-result)
   (src-span
    "unicode-program.typify"
    2
    column
    2
    (+ column (string-length token))
    byte
    (+ byte (bytes-length (string->bytes/utf-8 token))))
   "duplicate token text retains the second capture's code-point and UTF-8 location")
  (for
   ((text '("" " \t " "# anything : malformed")))
   (check (hash-ref (check-program text bracket) 'terms) '() "ignored lines remain valid"))
  (reject
   (lambda () (read-declarations "#lang typify\ndeclare : 3bad\nend!\n"))
   "E_CLASS"
   (hash 'class "name" 'token "3bad"))
  (reject
   (lambda () (read-declarations "#lang typify\ndeclare : Sample\nversion : nope\nend!\n"))
   "E_CLASS"
   (hash 'class "number" 'token "nope"))
  (reject
   (lambda () (read-declarations "#lang typify\ndeclare : Sample\ncap : unknown = 3\nend!\n"))
   "E_CLASS"
   (hash 'class "limit" 'token "unknown"))
  (reject
   (lambda () (read-declarations "#lang typify\nunknown : value\n"))
   "E_FORM_NONE"
   (hash 'actual "unknown : value"))
  (reject
   (lambda () (read-declarations "#lang typify\nunshaped\n"))
   "E_LINE_SHAPE"
   (hash 'actual "unshaped"))
  (define alternative-meta
    (body
     '("primitive : declare-language : name"
       "primitive : close-language"
       "form : open : <name>"
       "does : declare-language <name>"
       "form : finish!"
       "does : close-language")))
  (reject
   (lambda () (read-declarations "#lang typify\nopen : 3bad\nfinish!\n" #:meta alternative-meta))
   "E_CLASS"
   (hash 'class "name" 'token "3bad"))
  (reject
   (lambda () (read-declarations "#lang typify\ndeclare : Sample\n" #:meta alternative-meta))
   "E_FORM_NONE"
   (hash 'actual "declare : Sample"))
  (reject
   (lambda () (read-declarations "#lang typify\nunshaped\n" #:meta alternative-meta))
   "E_LINE_SHAPE"
   (hash 'actual "unshaped"))
  (hash 'suite "program-diagnostics" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
