#lang racket/base

(require json
         file/sha1
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/main.rkt")

(provide run-tests)

(define-runtime-path main-path "../src/main.rkt")

(define declaration
  (string-join
   '("#lang typify"
     "declare : Records"
     "is : payload = /[a-z]+/"
     "cell : entries = shadow"
     "primitive : store : canonical-path payload"
     "writes : store : entries = put 1"
     "primitive : clear : canonical-path"
     "writes : clear : entries = delete 1"
     "form : store : <payload> = <canonical-path>"
     "does : store <canonical-path> <payload>"
     "form : clear : <canonical-path>"
     "does : clear <canonical-path>"
     "ensure : store : retained = (bound entries <1>)"
     "end!")
   "\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-stage2-cli "~a: expected ~s; received ~s" context expected actual)))
  (define (request arguments)
    (define-values (result json? grammar?) (run-command arguments))
    result)
  (define (refusal arguments)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (request arguments)
     (error 'test-stage2-cli "The invalid command was accepted")))
  (define executable
    (or (find-executable-path "racket") (error 'test-stage2-cli "Racket is unavailable")))
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
  (define root (make-temporary-file "stage2-cli~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define declarations (path->string (build-path root "language.typify")))
     (define program (path->string (build-path root "data.typify")))
     (define facts (path->string (build-path root "facts.json")))
     (define absent (path->string (build-path root "not-readable.json")))
     (define (write-declaration text)
       (display-to-file text declarations #:exists 'truncate/replace))
     (define (write-program text) (display-to-file text program #:exists 'truncate/replace))
     (write-declaration declaration)
     (write-program "store : blue = /sample")
     (define good-arguments (list "plan" declarations "Records" program))
     (define good (request good-arguments))
     (check (hash-ref good 'status) "ok" "fully modelled plan status")
     (check (hash-ref good 'plan) '(("store" "/sample" "blue")) "the CLI plans one program")
     (check (hash-ref good 'facts) "none" "a fact-free declaration needs no facts")
     (check
      (request (append good-arguments (list "--facts" absent)))
      good
      "an unused explicit facts file is not opened")
     (check (hash-ref (car (hash-ref good 'ensure)) 'ok) #t "successful ensure is retained")
     (write-program "store : blue = /sample\nclear : /sample")
     (define failed (request good-arguments))
     (check (hash-ref failed 'status) "failed" "an error diagnostic fails the plan status")
     (check
      (hash-ref failed 'plan)
      '(("store" "/sample" "blue") ("clear" "/sample"))
      "failed ensures preserve the whole plan")
     (check (hash-ref failed 'state) (hash "entries" '()) "failed ensures preserve final state")
     (check
      (map (lambda (entry) (hash-ref entry 'ok)) (hash-ref failed 'ensure))
      '(#f)
      "every failed ensure remains observable")
     (check
      (map (lambda (entry) (hash-ref entry 'code)) (hash-ref failed 'diagnostics))
      '("E_ENSURE")
      "failed ensures append native diagnostics")
     (define failed-execution (execute (append good-arguments '("--json"))))
     (check (hash-ref failed-execution 'exit) 1 "failed ensure exits one")
     (check
      (hash-has-key? (string->jsexpr (hash-ref failed-execution 'stdout)) 'state)
      #t
      "failed JSON invocation retains the state wire")
     (write-program "store : blue = /sample")
     (write-declaration (string-replace declaration "end!" "file : data.typify\nend!"))
     (define tree (request (list "tree" declarations (path->string root) "--facts" absent)))
     (check (hash-ref tree 'ok) #t "tree has an ok field")
     (check (hash-ref tree 'diagnostics) '() "tree diagnostics are always present")
     (check
      (map (lambda (entry) (hash-ref entry 'path)) (hash-ref tree 'programs))
      '("data.typify")
      "tree is an object with ordered programs")
     (check
      (hash-ref (car (hash-ref tree 'programs)) 'language)
      "Records"
      "the public tree entry identifies its language")
     (define tree-text (execute (list "tree" declarations (path->string root))))
     (check (hash-ref tree-text 'stdout) "present data.typify\n" "tree text status precedes path")
     (define root-plan (request (list "plan" declarations (path->string root))))
     (check
      (hash-ref (car (hash-ref root-plan 'programs)) 'plan)
      '(("store" "/sample" "blue"))
      "root plan retains each program's plan")
     (define root-text (execute (list "plan" declarations (path->string root))))
     (check
      (hash-ref root-text 'stdout)
      "ok\ndata.typify [\"store\",\"/sample\",\"blue\"]\n"
      "plan text prefixes primitive terms with program paths")
     (write-declaration (string-replace declaration "end!" "reads : store : exists = reject\nend!"))
     (define missing-facts (refusal good-arguments))
     (check (diagnostic-code missing-facts) "E_USAGE" "one-program plans never invent a facts root")
     (check
      (hash-ref (diagnostic-data missing-facts) 'actual)
      "--facts"
      "one-program plans identify the missing facts flag")
     (define facts-bytes
       #"{ \"collector\":\"typify 1.0\", \"root\":\"/\", \"facts\":[{\"fact\":\"exists\",\"args\":[\"/sample\"]}] }\n")
     (call-with-output-file facts (lambda (out) (write-bytes facts-bytes out)))
     (define factful (request (append good-arguments (list "--facts" facts))))
     (check
      (hash-ref factful 'facts)
      (string-append "sha256:" (bytes->hex-string (sha256-bytes facts-bytes)))
      "supplied facts hash their exact bytes")
     (display-to-file "{}" facts #:exists 'truncate/replace)
     (define malformed (refusal (append good-arguments (list "--facts" facts))))
     (check (diagnostic-code malformed) "E_FILE" "malformed facts use the file code")
     (check
      (hash-ref (diagnostic-data malformed) 'path)
      facts
      "malformed facts identify their file")
     (define ignored-check (request (list "check" declarations "Records" program "--facts" facts)))
     (check
      (hash-ref ignored-check 'terms)
      '(("store" "/sample" "blue"))
      "check ignores a malformed supplied facts file")
     (check
      (hash-ref (request (list "check" declarations "--facts" absent)) 'ok)
      #t
      "declaration-only check also ignores supplied facts")
     (define incompatible (refusal (list "emit" declarations "Records" "--gbnf" "--json")))
     (check
      (hash-ref (diagnostic-data incompatible) 'actual)
      "--gbnf"
      "GBNF and JSON flags are mutually exclusive")
     (define grammar (request (list "emit" declarations "Records" "--json")))
     (check
      (hash-has-key? grammar 'diagnostics)
      #f
      "emit prints the grammar without a report wrapper")
     (define failed-emit (execute (list "emit" absent "Records" "--json")))
     (check (hash-ref failed-emit 'exit) 1 "failed declaration emission exits one")
     (check
      (sort (hash-keys (string->jsexpr (hash-ref failed-emit 'stdout))) symbol<?)
      '(diagnostics ok)
      "failed emit prints the native failure object")
     (write-declaration
      (string-join
       '("#lang typify"
         "declare : StateFacts"
         "cell : memory = shadow"
         "primitive : remember : canonical-path"
         "effect : remember = state"
         "writes : remember : memory = put 1 \"noted\""
         "form : remember : <canonical-path>"
         "does : remember <canonical-path>"
         "end!")
       "\n"))
     (write-program "remember : /sample")
     (define recorded (request (list "plan" declarations "StateFacts" program)))
     (check (hash-ref recorded 'plan) '() "state-only terms are not executed host actions")
     (check (hash-ref recorded 'origins) '() "top-level origins align with executed terms")
     (check (hash-ref recorded 'modelled) "0 of 0" "modelled counts only executed actions")
     (check
      (hash-ref recorded 'recorded)
      (list
       (hash 'term '("remember" "/sample") 'origin (hash 'kind "program" 'path program 'line 1)))
      "recorded terms carry their own compact origins")
     (check
      (hash-ref (car (hash-ref (hash-ref recorded 'state) "memory")) 'value)
      '("noted")
      "literal write values appear only in the state entry")
     (define recorded-text (execute (list "plan" declarations "StateFacts" program)))
     (check
      (hash-ref recorded-text 'stdout)
      (format "ok\n~a recorded [\"remember\",\"/sample\"]\n" program)
      "text output distinguishes recorded terms")
     (define slot-directory (build-path root "SLOT[2]"))
     (make-directory slot-directory)
     (display-to-file "ready!" (build-path slot-directory "data.typify"))
     (write-declaration
      (string-join
       '("#lang typify"
         "declare : Slots"
         "is : seat = /[0-9]+/"
         "file : SLOT[<seat>]/data.typify"
         "cell : assignments = shadow"
         "primitive : tag : seat"
         "writes : tag : assignments = put 1"
         "form : ready!"
         "does : tag <seat>"
         "end!")
       "\n"))
     (parameterize
      ((current-directory root))
      (define slot-check (request (list "check" declarations "Slots" "SLOT[2]/data.typify")))
      (check
       (hash-ref slot-check 'terms)
       '(("tag" "2"))
       "one-program check binds file-template captures")
      (define slot-plan (request (list "plan" declarations "Slots" "SLOT[2]/data.typify")))
      (check
       (hash-ref slot-plan 'plan)
       '(("tag" "2"))
       "one-program plan binds file-template captures")
      (define mismatch (refusal (list "plan" declarations "Slots" program)))
      (check
       (hash-ref (diagnostic-data mismatch) 'actual)
       program
       "a template mismatch names the path as given"))
     (write-declaration
      (string-join
       '("#lang typify"
         "declare : First"
         "term : entry : canonical-path"
         "end!"
         "declare : Second"
         "term : entry : canonical-path"
         "end!"
         "declare : Consumer"
         "primitive : use : canonical-path"
         "effect : use = external"
         "form : use!"
         "does : use (context entry)"
         "end!")
       "\n"))
     (for
      ((arguments
        (in-list
         (list
          (list "check" declarations)
          (list "check" declarations "Consumer" absent)
          (list "tree" declarations absent)
          (list "plan" declarations absent)
          (list "plan" declarations "Consumer" absent)
          (list "emit" declarations "Consumer")))))
      (define item (refusal arguments))
      (check
       (diagnostic-data item)
       (hash 'kind "reference" 'actual "entry")
       "every implemented command refuses ambiguous lookups before opening its input"))
     (define refused-root (execute (list "plan" declarations absent "--json")))
     (define refused-root-json (string->jsexpr (hash-ref refused-root 'stdout)))
     (check (hash-has-key? refused-root-json 'skips) #f "a failed root plan has no top-level skips")
     (check
      (hash-has-key? refused-root-json 'recorded)
      #f
      "a failed root plan has no top-level recorded terms")
     (for
      ((arguments
        (in-list
         (list
          '("unrecognized" "--json")
          '("plan" "--facts")
          '("plan" "--unknown")
          '("plan" "--facts" "one" "--facts" "two"))))
       (token (in-list '("unrecognized" "--facts" "--unknown" "--facts"))))
      (define item (refusal arguments))
      (check (diagnostic-code item) "E_USAGE" "invalid CLI shape is a usage error")
      (check (diagnostic-file item) "" "usage errors have an empty file")
      (check (diagnostic-range item) #f "usage errors have no range")
      (check
       (hash-ref (diagnostic-data item) 'actual)
       token
       "usage errors name the offending token")))
   (lambda () (delete-directory/files root)))
  (hash 'suite "stage2-cli" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
