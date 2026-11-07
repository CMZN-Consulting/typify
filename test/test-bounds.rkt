#lang racket/base

(require json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/main.rkt")

(provide run-tests)

(define-runtime-path main-path "../src/main.rkt")

(define declarations
  (string-append
   "#lang typify\n"
   "declare : Zeta\ncap : length = 8\nis : word = /[a-z]+/\n"
   "primitive : output : word\n"
   "form : first : [<word>]\ndoes : output <word>\n"
   "form : second : (<word>)\ndoes : output <word>\n"
   "count : output = 1\nend!\n"
   "declare : Alpha\ncap : length = 8\nterm : flag\nend!\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-bounds "~a: expected ~s; received ~s" context expected actual)))
  (define (request arguments) (call-with-values (lambda () (run-command arguments)) list))
  (define (reject arguments code)
    (define observed
      (with-handlers
       ((exn:fail:typify? (lambda (error) (diagnostic-code (exn:fail:typify-diagnostic error)))))
       (request arguments)
       "accepted"))
    (check observed code (format "native refusal for ~s" arguments)))
  (define executable
    (or (find-executable-path "racket") (error 'test-bounds "Racket executable is unavailable")))
  (define (execute arguments)
    (define-values
     (process output input errors)
     (apply subprocess #f #f #f executable main-path arguments))
    (close-output-port input)
    (define stdout (port->string output))
    (define stderr (port->string errors))
    (close-input-port output)
    (close-input-port errors)
    (subprocess-wait process)
    (hash 'exit (subprocess-status process) 'stdout stdout 'stderr stderr))
  (define directory (make-temporary-file "typify-bounds-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define declaration-path (path->string (build-path directory "language.typify")))
     (define program-path (path->string (build-path directory "program.typify")))
     (define invalid-path (path->string (build-path directory "invalid.typify")))
     (call-with-output-file declaration-path (lambda (output) (display declarations output)))
     (call-with-output-file program-path (lambda (output) (display "first : [hello]" output)))
     (call-with-output-file invalid-path (lambda (output) (display "first : [123]" output)))
     (define ordinary (request (list "check" declaration-path "--json")))
     (check (length ordinary) 3 "run-command retains its three returned values")
     (check
      ordinary
      (list (hash 'ok #t 'terms '() 'diagnostics '()) #t #f)
      "normal declaration check response is unchanged")
     (define selected-arguments (list "check" declaration-path "Zeta" program-path))
     (check
      (car (request (append selected-arguments '("--json"))))
      (hash 'ok #t 'terms '(("output" "hello")) 'diagnostics '())
      "normal program check response is unchanged")
     (define parsed (read-declarations declarations #:file declaration-path))
     (define expected-all
       (hash
        'languages
        (for/list
         ((name '("Alpha" "Zeta")))
         (hash 'language name 'checks (declaration-check-bounds (hash-ref parsed name))))
        'host
        (hash 'name "racket" 'version (version))))
     (define all-response (request (list "check" declaration-path "--bounds" "--json")))
     (check
      all-response
      (list expected-all #t #f)
      "all-language JSON bounds match the recorded API data")
     (define expected-selected
       (hash
        'languages
        (list (cadr (hash-ref expected-all 'languages)))
        'host
        (hash-ref expected-all 'host)))
     (check
      (request (append selected-arguments '("--bounds" "--json")))
      (list expected-selected #t #f)
      "a selected program reports only its validated language")
     (check
      (car (request (list "--bounds" "check" declaration-path "--json")))
      expected-all
      "the bounds flag is recognized before the command")
     (define text (render-bounds expected-selected))
     (for
      ((pattern
        (list
         #px"Class [^\n]+bounded-states<=[0-9]+"
         #px"Form [^\n]+whole-states<=[0-9]+"
         #px"Inclusion [^\n]+product-states<=[0-9]+"
         #px"Delimiter [^\n]+product-states<=[0-9]+"
         #px"Overlap [^\n]+product-states<=[0-9]+")))
      (check
       (regexp-match? pattern text)
       #t
       "the text projection prints numeric derived upper bounds"))
     (check
      (string-contains? text "Host-dependent")
      #t
      "the report identifies its host-dependent representation")
     (define clean-text (execute selected-arguments))
     (check (hash-ref clean-text 'exit) 0 "clean text check succeeds")
     (check (hash-ref clean-text 'stdout) "" "ordinary successful text check stays silent")
     (check (hash-ref clean-text 'stderr) "" "ordinary successful text check writes no error")
     (define bounds-text (execute (append selected-arguments '("--bounds"))))
     (check (hash-ref bounds-text 'exit) 0 "text bounds check succeeds")
     (check (hash-ref bounds-text 'stdout) text "the executable prints the numeric text projection")
     (check (hash-ref bounds-text 'stderr) "" "bounds text is not an error")
     (define bounds-json (execute (list "check" declaration-path "--bounds" "--json")))
     (check (hash-ref bounds-json 'exit) 0 "JSON bounds check succeeds")
     (check
      (canonical-json (string->jsexpr (hash-ref bounds-json 'stdout)))
      (canonical-json expected-all)
      "the executable prints the JSON report directly")
     (reject (list "emit" "Typify" "--bounds") "E_USAGE")
     (reject (list "emit" "Typify" "--unknown") "E_USAGE")
     (reject (list "check" declaration-path "--unknown") "E_USAGE")
     (reject (list "check" declaration-path "--gbnf") "E_USAGE")
     (reject (list "check" declaration-path "Zeta" invalid-path "--bounds") "E_PROGRAM_FORM")
     (reject
      (list "check" declaration-path "Missing" program-path "--bounds")
      "E_LANGUAGE_UNDECLARED")
     (define usage (execute (list "emit" "Typify" "--bounds" "--json")))
     (check (hash-ref usage 'exit) 2 "bounds on emit has the usage exit status")
     (define usage-json (string->jsexpr (hash-ref usage 'stdout)))
     (check
      (hash-ref (car (hash-ref usage-json 'diagnostics)) 'code)
      "E_USAGE"
      "bounds on emit reports a native usage diagnostic")
     (define invalid
       (execute (list "check" declaration-path "Zeta" invalid-path "--bounds" "--json")))
     (check (hash-ref invalid 'exit) 1 "bounds does not bypass program validation")
     (check
      (hash-has-key? (string->jsexpr (hash-ref invalid 'stdout)) 'languages)
      #f
      "a failing check does not report successful derived bounds"))
   (lambda () (delete-directory/files directory)))
  (hash 'suite "bounds" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
