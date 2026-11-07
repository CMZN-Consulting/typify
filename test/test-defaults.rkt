#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (source lines)
  (string-append
   (string-join
    (append '("#lang typify" "declare : Defaults" "cap : length = 32") lines '("end!"))
    "\n")
   "\n"))

(define (body lines)
  (hash-ref (read-declarations (source lines) #:file "defaults-language.typify") "Defaults"))

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
      (error 'test-defaults "~a: expected ~s; received ~s" context expected actual)))
  (define (reject thunk code context)
    (define diagnostic (refusal thunk))
    (check (and diagnostic (diagnostic-code diagnostic)) code context)
    diagnostic)
  (define shared
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "primitive : marker : word"
       "form : first : <word>"
       "does : output <word>"
       "default : first : alpha"
       "default : first : beta"
       "form : middle : <word>"
       "does : marker <word>"
       "default : middle : gamma"
       "form : last : <word>"
       "does : output <word>"
       "default : last : delta"
       "form : explicit : <word>"
       "does : output <word>")))
  (define empty-shared (check-program "" shared))
  (check
   (hash-ref empty-shared 'terms)
   '(("output" "alpha") ("output" "beta") ("marker" "gamma") ("output" "delta"))
   "all defaults activate in declaration order, without regrouping constructors")
  (check
   (map (lambda (origin) (hash-ref origin 'kind)) (hash-ref empty-shared 'origins))
   '("default" "default" "default" "default")
   "every default term has a default origin")
  (check
   (hash-ref (check-program "middle : own" shared) 'terms)
   '(("marker" "own") ("output" "alpha") ("output" "beta") ("output" "delta"))
   "activation uses the own-term snapshot and appends every missing-constructor default")
  (check
   (hash-ref (check-program "explicit : own" shared) 'terms)
   '(("output" "own") ("marker" "gamma"))
   "an explicit constructor from a form without defaults suppresses all its forms' defaults")
  (check
   (hash-ref (check-program "last : own" shared) 'terms)
   '(("output" "own") ("marker" "gamma"))
   "an explicit term from a different defaulted form suppresses every sibling default")
  (check
   (hash-ref (check-program "explicit : own\nmiddle : mine" shared) 'terms)
   '(("output" "own") ("marker" "mine"))
   "own terms of all constructors suppress every default")
  (define single-lines
    '("is : word = /[a-z]+/"
      "primitive : output : word"
      "form : item : <word>"
      "does : output <word>"
      "default : item : fallback"
      "count : output = 1"))
  (define single (body single-lines))
  (define single-result (check-program "" single #:file "program.typify"))
  (check
   (hash-ref single-result 'terms)
   '(("output" "fallback"))
   "default activation precedes the exact-one count")
  (check
   (hash-ref (check-program "item : explicit" single) 'terms)
   '(("output" "explicit"))
   "an explicit exact-one constructor suppresses its default before counting")
  (define default-origin (car (hash-ref single-result 'origins)))
  (define declaration-record
    (list-ref (read-lines (source single-lines) #:file "defaults-language.typify") 7))
  (check
   default-origin
   (hash
    'kind
    "default"
    'file
    "defaults-language.typify"
    'range
    (src-span->jsexpr (line-record-span declaration-record))
    'source
    "item : fallback")
   "default origins retain the declaring file, full range and source payload")
  (define mixed (check-program "middle : own" shared #:file "program.typify"))
  (define own-origin (car (hash-ref mixed 'origins)))
  (check (src-span? own-origin) #t "ordinary program origins keep their original representation")
  (check (src-span-file own-origin) "program.typify" "own origins identify the program file")
  (check (src-span-start-line own-origin) 1 "own origins retain the program line")
  (check
   (hash-ref (cadr (hash-ref mixed 'origins)) 'source)
   "first : alpha"
   "defaults follow own origins in the same order as their terms")
  (define multiple-under-one
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "form : item : <word>"
       "does : output <word>"
       "default : item : alpha"
       "default : item : beta"
       "count : output = 1")))
  (define count-error
    (reject
     (lambda () (check-program "" multiple-under-one))
     "E_PROGRAM_COUNT"
     "multiple activated defaults are not truncated to satisfy a count"))
  (check
   (hash-ref (diagnostic-data count-error) 'actual)
   2
   "the count includes every activated default")
  (define optional (body (append '("file : program.typify" "optional!") single-lines)))
  (check
   (hash-ref (check-program "" optional) 'terms)
   '(("output" "fallback"))
   "an absent optional program's empty input activates only its defaults")
  (define strict
    (body
     '("order : strict"
       "is : word = /[a-z]+/"
       "primitive : output : word"
       "primitive : marker : word"
       "form : first : <word>"
       "does : output <word>"
       "default : first : fallback"
       "form : middle : <word>"
       "does : marker <word>")))
  (check
   (hash-ref (check-program "middle : own" strict) 'terms)
   '(("marker" "own") ("output" "fallback"))
   "strict order applies to own lines before earlier-form defaults are appended")
  (reject
   (lambda () (check-program "middle : own\nfirst : explicit" strict))
   "E_PROGRAM_ORDER"
   "strict own-line ordering is still enforced")
  (define rewritten
    (body
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "term : setting : word"
       "rule : setting <input> -> output <input>"
       "form : item : <word>"
       "does : setting <word>"
       "default : item : fallback"
       "count : setting = 1")))
  (define rewritten-result (check-program "" rewritten))
  (check
   (hash-ref rewritten-result 'terms)
   '(("setting" "fallback"))
   "defaults build the same declared constructor as explicit lines")
  (check
   (normalize-terms rewritten (hash-ref rewritten-result 'terms))
   '(("output" "fallback"))
   "default terms follow the ordinary rule normalization path")
  (hash 'suite "defaults" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
