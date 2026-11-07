#lang racket/base

(require json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/main.rkt"
         "../src/model.rkt")

(provide run-tests)

(define-runtime-path main-path "../src/main.rkt")

(define declarations
  (string-append
   "#lang typify\n"
   "declare : Zeta\ncap : length = 8\nis : word = /[a-z]+/\n"
   "primitive : output : word\n"
   "form : first : [<word>]\ndoes : output <word>\n"
   "form : second : (<word>)\ndoes : output <word>\n"
   "count : output = 1\neffect : output = external\nend!\n"
   "declare : Alpha\ncap : length = 8\nterm : flag\nend!\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-bounds "~a: expected ~s; received ~s" context expected actual)))
  (define (request arguments) (call-with-values (lambda () (run-command arguments)) list))
  (define (reject arguments code)
    (define expected (if (list? code) code (list code)))
    (define observed
      (with-handlers
       ((exn:fail:typify?
         (lambda (error) (map diagnostic-code (exn:fail:typify-diagnostics error)))))
       (request arguments)
       "accepted"))
    (check observed expected (format "native refusal for ~s" arguments)))
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
     (define (expected-limits names)
       (for*/fold
        ((limits
          (for/fold
           ((limits (hash)))
           (((name limit)
             (in-hash
              (for/fold
               ((guards class-host-guards))
               (((key value) (in-hash model-host-guards)))
               (hash-set guards key value)))))
           (hash-set limits name (hash 'limit limit)))))
        ((name (in-list names))
         ((cap limit) (in-hash (hash "states" 4096 "repetition" 256 "depth" 8 "length" 8))))
        (hash-set limits (string-append name "." cap) (hash 'limit limit))))
     (define expected-bounds (expected-limits '("Alpha" "Zeta")))
     (define expected-all (hash 'ok #t 'bounds expected-bounds 'diagnostics '()))
     (define all-response (request (list "check" declaration-path "--bounds" "--json")))
     (check
      all-response
      (list expected-all #t #f)
      "all-language JSON bounds match the recorded API data")
     (define expected-selected (hash 'ok #t 'diagnostics '() 'bounds (expected-limits '("Zeta"))))
     (check
      (request (append selected-arguments '("--bounds" "--json")))
      (list expected-selected #t #f)
      "a selected program reports only its validated language")
     (check
      (car (request (list "--bounds" "check" declaration-path "--json")))
      expected-all
      "the bounds flag is recognized before the command")
     (define text (render-bounds (hash-ref expected-selected 'bounds)))
     (check
      (hash-ref (hash-ref expected-bounds "Zeta.length") 'limit)
      8
      "the selected body lowers its effective length cap")
     (check
      (hash-ref (hash-ref expected-bounds "Alpha.states") 'limit)
      4096
      "defaulted effective caps are printed by language name")
     (for
      (((name value) (in-hash (hash-ref expected-selected 'bounds))))
      (check
       (string-contains? text (format "~a: ~a\n" name (hash-ref value 'limit)))
       #t
       "text output prints one named limit per line"))
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
     (reject
      (list "check" declaration-path "Zeta" invalid-path "--bounds")
      '("E_PROGRAM_COUNT" "E_CLASS"))
     (reject (list "check" declaration-path "Missing" program-path "--bounds") "E_USAGE")
     (define usage (execute (list "emit" "Typify" "--bounds" "--json")))
     (check (hash-ref usage 'exit) 2 "bounds on emit has the usage exit status")
     (define usage-json (string->jsexpr (hash-ref usage 'stdout)))
     (check
      (hash-ref (car (hash-ref usage-json 'diagnostics)) 'code)
      "E_USAGE"
      "bounds on emit reports a native usage diagnostic")
     (check
      (hash-ref (hash-ref (car (hash-ref usage-json 'diagnostics)) 'data) 'actual)
      "--bounds"
      "usage data identifies the invalid option")
     (define missing-path (path->string (build-path directory "missing.typify")))
     (define missing (execute (list "check" missing-path "--json")))
     (check (hash-ref missing 'exit) 2 "unreadable input retains the command failure status")
     (define missing-diagnostic
       (car (hash-ref (string->jsexpr (hash-ref missing 'stdout)) 'diagnostics)))
     (check (hash-ref missing-diagnostic 'code) "E_FILE" "file errors use the closed registry")
     (check
      (hash-ref (hash-ref missing-diagnostic 'data) 'path)
      (string-replace missing-path "\\" "/")
      "file errors compare the requested path rather than host error text")
     (define separated-path (string-append (path->string directory) "/absent\\nested.typify"))
     (define separated (execute (list "check" separated-path "--json")))
     (define separated-diagnostic
       (car (hash-ref (string->jsexpr (hash-ref separated 'stdout)) 'diagnostics)))
     (check
      (hash-ref (hash-ref separated-diagnostic 'data) 'path)
      (string-replace separated-path "\\" "/")
      "compared file paths use portable separators")
     (define invalid
       (execute (list "check" declaration-path "Zeta" invalid-path "--bounds" "--json")))
     (check (hash-ref invalid 'exit) 1 "bounds does not bypass program validation")
     (check
      (hash-has-key? (string->jsexpr (hash-ref invalid 'stdout)) 'bounds)
      #f
      "a failing check does not report successful derived bounds")
     (display-to-file
      (string-join
       '("#lang typify"
         "declare : A"
         "primitive : tick"
         "effect : tick = external"
         "term : twice"
         "rule : twice -> tick ; tick"
         "form : a!"
         "does : twice"
         "end!"
         "declare : B"
         "primitive : tick"
         "effect : tick = external"
         "term : thrice"
         "fuel : thrice = 5"
         "rule : thrice -> tick ; tick ; tick"
         "form : b!"
         "does : thrice"
         "end!")
       "\n")
      declaration-path
      #:exists
      'truncate/replace)
     (define rule-bounds
       (hash-ref (car (request (list "check" declaration-path "--bounds" "--json"))) 'bounds))
     (check (hash-ref rule-bounds 'c) 3 "the composition c is the largest per-body bound")
     (check (hash-ref rule-bounds 'k) 1 "the composition k bounds each folded state event")
     (check
      (hash-ref (hash-ref rule-bounds "B.thrice.fuel") 'limit)
      5
      "constructor fuel caps use qualified names"))
   (lambda () (delete-directory/files directory)))
  (hash 'suite "bounds" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
