#lang racket/base

(require json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/main.rkt")

(provide run-tests)

(define-runtime-path main-path "../src/main.rkt")

(define declarations
  (string-join
   '("#lang typify"
     "declare : Catalog"
     "file : catalog.typify"
     "required!"
     "is : category = \"book\" | \"journal\""
     "is : title = /[^=]+/"
     "is : under_score = /[a-z]+/"
     "term : item : category title"
     "form : item : <category> = <title>"
     "does : item <category> <title>"
     "example : item : book = Algebra"
     "count : item = 1..*"
     "end!"
     "declare : Other"
     "is : payload = /[a-z]+/"
     "term : price : payload"
     "form : price : <payload>"
     "does : price <payload>"
     "end!"
     "declare : Symbols"
     "is : payload = /[a-z]+/"
     "term : mark : payload"
     "form : λ : <payload>"
     "does : mark <payload>"
     "form : --visible : <payload>"
     "does : mark <payload>"
     "end!")
   "\n"))

(define item-lines
  '("? : form : item : <category> = <title>"
    "? : file : catalog.typify"
    "? : does : item <category> <title>"
    "? : term : item : category title"
    "? : is : category = \"book\" | \"journal\""
    "? : is : title = /[^=]+/"
    "? : example : item : book = Algebra"
    "? : count : item = 1..*"
    "? : missing : refused"))

(define meta-question-lines
  '("? : form : ? : <verb>"
    "? : does : question <verb>"
    "? : primitive : question : verb"
    "? : is : verb = /[A-Za-z0-9][A-Za-z0-9-]*/"
    "? : effect : question = external"
    "? : example : ? : form"
    "? : count : question = *"
    "? : missing : empty"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (if
             (and (hash? actual) (hash? expected))
             (equal? (canonical-json actual) (canonical-json expected))
             (equal? actual expected))
      (error 'test-stage3-cli "~a: expected ~s; received ~s" context expected actual)))
  (define (capture arguments)
    (define output (open-output-string))
    (define errors (open-output-string))
    (define-values
     (status diagnostics)
     (parameterize
      ((current-output-port output) (current-error-port errors))
      (execute-command arguments)))
    (hash 'exit status 'stdout (get-output-string output) 'stderr (get-output-string errors)))
  (define (response arguments) (string->jsexpr (hash-ref (capture arguments) 'stdout)))
  (define root (make-temporary-file "stage3-cli~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define path (path->string (build-path root "language.typify")))
     (define bad-path (path->string (build-path root "bad.typify")))
     (define absent-path (path->string (build-path root "absent.typify")))
     (define program (path->string (build-path root "catalog.typify")))
     (display-to-file declarations path)
     (define (ask language word) (list "ask" path language word "--json"))
     (define declared (capture (ask "Catalog" "item")))
     (check (hash-ref declared 'exit) 0 "a declared answer exits zero")
     (check (hash-ref declared 'stderr) "" "clean JSON answers have no stderr")
     (check
      (string->jsexpr (hash-ref declared 'stdout))
      (hash
       'word
       "item"
       'status
       "declared"
       'lines
       item-lines
       'near
       '()
       'accepts
       '()
       'diagnostics
       '())
      "the CLI returns the full declared answer object")
     (define declared-text (capture (list "ask" path "Catalog" "item")))
     (check (hash-ref declared-text 'exit) 0 "a text answer exits zero")
     (check
      (hash-ref declared-text 'stdout)
      (string-append (string-join item-lines "\n") "\n")
      "text prints exactly one answer line per line")
     (check (hash-ref declared-text 'stderr) "" "clean text answers have no diagnostics")
     (define unknown (capture (ask "Catalog" "price")))
     (check (hash-ref unknown 'exit) 0 "an undeclared word is a successful question")
     (check
      (string->jsexpr (hash-ref unknown 'stdout))
      (hash
       'word
       "price"
       'status
       "undeclared"
       'lines
       '("? : price = undeclared")
       'near
       '()
       'accepts
       '()
       'diagnostics
       '())
      "another body's word remains undeclared and supplies no candidates")
     (check (hash-ref unknown 'stderr) "" "unknown words produce no diagnostic")
     (check
      (hash-ref (response (ask "Catalog" "itme")) 'lines)
      '("? : itme = undeclared" "? : near : item")
      "a transposition prints its nearest word")
     (check
      (hash-ref (response (ask "Catalog" "under_score")) 'lines)
      '("? : is : under_score = /[a-z]+/")
      "a declared class bypasses the undeclared-word spelling test")
     (check
      (hash-ref (response (ask "Symbols" "λ")) 'status)
      "declared"
      "a declared Unicode form verb is answered")
     (check
      (hash-ref (response (list "ask" path "Symbols" "--json" "--" "--visible")) 'status)
      "declared"
      "the option terminator admits a declared flag-like word")
     (define flag-word (capture (list "ask" path "Catalog" "--" "--json")))
     (check (hash-ref flag-word 'exit) 2 "a positional flag-like unknown word is still usage")
     (check
      (hash-ref flag-word 'stdout)
      ""
      "a positional JSON spelling does not select JSON errors")
     (check
      (and (string-contains? (hash-ref flag-word 'stderr) "[E_USAGE]") #t)
      #t
      "a post-terminator flag spelling retains text diagnostics")
     (check
      (hash-ref (response (ask "Typify" "?")) 'lines)
      meta-question-lines
      "the canonical question answer is the eight lines pinned by the definition")
     (check
      (hash-ref (response (ask "Catalog" "letter")) 'status)
      "undeclared"
      "built-in letter is not an authored declaration")
     (for
      ((word '("" "_bad" "bad_name" "word word" "λx")))
      (define rejected (capture (ask "Catalog" word)))
      (define answer (string->jsexpr (hash-ref rejected 'stdout)))
      (define diagnostic (car (hash-ref answer 'diagnostics)))
      (check (hash-ref rejected 'exit) 2 "invalid undeclared words exit two")
      (check (sort (hash-keys answer) symbol<?) '(diagnostics ok) "usage has no answer fields")
      (check (hash-ref diagnostic 'code) "E_USAGE" "invalid words produce the usage code")
      (check (hash-ref diagnostic 'file) "" "word usage has no source file")
      (check (hash-ref diagnostic 'range) 'null "word usage has no source range")
      (check
       (hash-ref (hash-ref diagnostic 'data) 'actual)
       word
       "usage retains the word as written")
      (check (hash-ref diagnostic 'fix) 'null "v1 offers no diagnostic fix")
      (check (hash-has-key? diagnostic 'near) #f "near belongs only to successful ask answers"))
     (define unknown-language (capture (ask "Missing" "item")))
     (check (hash-ref unknown-language 'exit) 2 "an unknown language is usage")
     (check
      (hash-ref
       (hash-ref
        (car (hash-ref (string->jsexpr (hash-ref unknown-language 'stdout)) 'diagnostics))
        'data)
       'actual)
      "Missing"
      "language usage names the requested body")
     (define unreadable (capture (list "ask" absent-path "Typify" "?" "--json")))
     (check (hash-ref unreadable 'exit) 2 "the meta query still reads the declarations file")
     (define unreadable-answer (string->jsexpr (hash-ref unreadable 'stdout)))
     (check
      (sort (hash-keys unreadable-answer) symbol<?)
      '(diagnostics ok)
      "file failure has no answer")
     (check
      (hash-ref (car (hash-ref unreadable-answer 'diagnostics)) 'code)
      "E_FILE"
      "unreadable declarations use the file diagnostic")
     (display-to-file "declare : Bad\nis : broken = missing\nend!" bad-path)
     (define failed (capture (list "ask" bad-path "Typify" "_bad" "--json")))
     (define failed-answer (string->jsexpr (hash-ref failed 'stdout)))
     (check (hash-ref failed 'exit) 1 "declaration errors precede word usage")
     (check
      (sort (hash-keys failed-answer) symbol<?)
      '(diagnostics ok)
      "failed declarations have no answer")
     (check
      (map (lambda (item) (hash-ref item 'code)) (hash-ref failed-answer 'diagnostics))
      '("E_DECLARATION" "E_CLASS_UNDECLARED")
      "D1 reports both declaration errors")
     (for
      ((flag '("--bounds" "--gbnf")))
      (define incompatible (capture (append (ask "Catalog" "item") (list flag))))
      (check (hash-ref incompatible 'exit) 2 "ask refuses command-specific flags")
      (check
       (hash-ref
        (hash-ref
         (car (hash-ref (string->jsexpr (hash-ref incompatible 'stdout)) 'diagnostics))
         'data)
        'actual)
       flag
       "usage names the incompatible flag"))
     (define facts-flag (capture (append (ask "Catalog" "item") (list "--facts" absent-path))))
     (check (hash-ref facts-flag 'exit) 2 "ask never opens a facts file")
     (display-to-file
      (string-replace declarations "declare : Catalog" "declare : Catalog\nis : empty = /\\x00/")
      path
      #:exists
      'truncate/replace)
     (define noted (capture (ask "Catalog" "item")))
     (define noted-answer (string->jsexpr (hash-ref noted 'stdout)))
     (check (hash-ref noted 'exit) 0 "declaration notes do not fail the answer")
     (check (hash-ref noted 'stderr) "" "JSON carries its notes in the object")
     (check
      (hash-ref noted-answer 'lines)
      item-lines
      "unrelated notes do not change declaring lines")
     (check
      (map (lambda (item) (hash-ref item 'code)) (hash-ref noted-answer 'diagnostics))
      '("N_CLASS_EMPTY")
      "the answer preserves D1 notes")
     (define combined (open-output-string))
     (parameterize
      ((current-output-port combined) (current-error-port combined))
      (execute-command (list "ask" path "Catalog" "item")))
     (define together (get-output-string combined))
     (check
      (and (string-contains? together "[N_CLASS_EMPTY]\n? : form : item") #t)
      #t
      "the text projection emits notes before the answer")
     (display-to-file "item : book = Algebra\n? : itme\n? : missing" program)
     (define checked
       (parameterize
        ((current-directory root))
        (response (list "check" path "Catalog" "catalog.typify" "--json"))))
     (check
      (hash-ref checked 'ok)
      #t
      (format
       "check passes over well-spelled program questions: ~s"
       (hash-ref checked 'diagnostics)))
     (check (hash-ref checked 'terms) '(("item" "book" "Algebra")) "questions add no program terms")
     (check
      (ormap (lambda (item) (hash-has-key? item 'near)) (hash-ref checked 'diagnostics))
      #f
      "check never suggests a near word")
     (display-to-file "item : book = Algebra\n? : _bad" program #:exists 'truncate/replace)
     (define bad-question
       (parameterize
        ((current-directory root))
        (response (list "check" path "Catalog" "catalog.typify" "--json"))))
     (check
      (map (lambda (item) (hash-ref item 'code)) (hash-ref bad-question 'diagnostics))
      '("N_CLASS_EMPTY" "E_QUESTION")
      "a malformed program question is E_QUESTION")
     (define executable
       (or (find-executable-path "racket") (error 'test-stage3-cli "Racket is unavailable")))
     (define-values
      (process output input errors)
      (subprocess #f #f #f executable main-path "ask" path "Catalog" "item" "--json"))
     (close-output-port input)
     (define stdout (port->string output))
     (define stderr (port->string errors))
     (close-input-port output)
     (close-input-port errors)
     (subprocess-wait process)
     (check (subprocess-status process) 0 "the executable entrypoint returns the ask exit code")
     (check
      (hash-ref (string->jsexpr stdout) 'lines)
      item-lines
      "the entrypoint emits the JSON answer")
     (check stderr "" "the entrypoint puts notes in JSON")
     (define questioned-declarations
       (string-append
        (string-replace
         (string-replace declarations "#lang typify" "#lang typify\n? : outside")
         "does : item <category> <title>"
         "? : item\ndoes : item <category> <title>")
        "\n? : final"))
     (display-to-file questioned-declarations path #:exists 'truncate/replace)
     (define clean-questions (capture (ask "Catalog" "item")))
     (check
      (hash-ref clean-questions 'exit)
      0
      "declaration questions inside and outside bodies are ignored")
     (check
      (hash-ref (string->jsexpr (hash-ref clean-questions 'stdout)) 'lines)
      item-lines
      "declaration questions neither declare words nor alter form attachment")
     (display-to-file
      (string-replace declarations "#lang typify" "#lang typify\n? : _bad")
      path
      #:exists
      'truncate/replace)
     (define failed-declaration-question (capture (ask "Typify" "?")))
     (define question-refusal (string->jsexpr (hash-ref failed-declaration-question 'stdout)))
     (check
      (hash-ref failed-declaration-question 'exit)
      1
      "a malformed declaration question exits one")
     (check
      (sort (hash-keys question-refusal) symbol<?)
      '(diagnostics ok)
      "a malformed declaration question has no answer fields")
     (check
      (map (lambda (item) (hash-ref item 'code)) (hash-ref question-refusal 'diagnostics))
      '("E_QUESTION")
      "the declaration's question failure keeps its native code")
     (define question-diagnostic (car (hash-ref question-refusal 'diagnostics)))
     (check
      (hash-ref (hash-ref question-diagnostic 'data) 'actual)
      "? : _bad"
      "the declaration question reports the covered line")
     (check
      (hash-ref (hash-ref (hash-ref question-diagnostic 'range) 'start) 'line)
      2
      "the declaration question range locates the source line")
     (define failed-question-text (capture (list "ask" path "Catalog" "item")))
     (check
      (hash-ref failed-question-text 'exit)
      1
      "text mode keeps the declaration failure status")
     (check
      (hash-ref failed-question-text 'stdout)
      ""
      "a declaration question failure prints no answer")
     (check
      (and (string-contains? (hash-ref failed-question-text 'stderr) "[E_QUESTION]") #t)
      #t
      "text mode prints the declaration question diagnostic"))
   (lambda () (delete-directory/files root)))
  (hash 'suite "stage3-cli" 'checks checks 'status "passed"))

(module+ test (run-tests))
