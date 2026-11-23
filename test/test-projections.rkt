#lang racket/base

(require json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/main.rkt"
         "../src/projections.rkt"
         "../src/diagnostics.rkt")

(provide run-tests)

(define-runtime-path corpus "corpus/100-diagnostic-sarif-lsp-nonbmp-and-exit2")

(define-runtime-path main-path "../src/main.rkt")

(define (run-tests)
  (define checks 0)
  (define (check actual expected what)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-projections "~a: expected ~s, got ~s" what expected actual)))
  (define (command args expected-code json?)
    (define output (open-output-string))
    (define errors (open-output-string))
    (define-values
     (status native)
     (parameterize
      ((current-output-port output) (current-error-port errors))
      (execute-command args)))
    (check status expected-code args)
    (check (get-output-string errors) "" "one output stream")
    (if json? (string->jsexpr (get-output-string output)) (get-output-string output)))
  (parameterize
   ((current-directory corpus))
   (define args '("check" "declarations.typify" "UnicodeColAudit" "program.typify"))
   (define (run format) (command (append args (list format)) 1 #t))
   (define native (car (hash-ref (run "--json") 'diagnostics)))
   (define sr (car (hash-ref (run "--sarif") 'runs)))
   (define result (car (hash-ref sr 'results)))
   (define location (hash-ref (car (hash-ref result 'locations)) 'physicalLocation))
   (define region (hash-ref location 'region))
   (check (hash-ref sr 'columnKind) "unicodeCodePoints" "SARIF column units")
   (check (hash-ref result 'ruleId) (hash-ref native 'code) "same native code")
   (check
    (hash-ref (hash-ref location 'artifactLocation) 'uri)
    "program.typify"
    "portable artifact")
   (check
    (map
     (lambda (k) (hash-ref region k))
     '(startLine startColumn endLine endColumn byteOffset byteLength))
    '(1 9 1 12 11 3)
    "SARIF native range")
   (define lsp (run "--lsp"))
   (define located (append-map (lambda (d) (hash-ref d 'diagnostics)) (hash-ref lsp 'documents)))
   (define expected
     (hasheq 'start (hasheq 'line 0 'character 9) 'end (hasheq 'line 0 'character 12)))
   (check (hash-ref (car located) 'range) expected "UTF-16 after a supplementary character")
   (define-values
    (process stdout stdin stderr)
    (apply subprocess #f #f #f (find-executable-path "racket") main-path (append args '("--lsp"))))
   (close-output-port stdin)
   (define child-report (read-json stdout))
   (define child-errors (port->string stderr))
   (close-input-port stdout)
   (close-input-port stderr)
   (subprocess-wait process)
   (check (subprocess-status process) 1 "standalone CLI exit after refusal")
   (check child-errors "" "standalone CLI diagnostics stream")
   (check child-report lsp "standalone CLI UTF-16 and relative source resolution")
   (check (hash-ref lsp 'unlocated) '() "no lost locations")
   (define annotation (command (append args '("--github-annotations")) 1 #f))
   (check
    (string-prefix?
     annotation
     "::error file=program.typify,line=1,endLine=1,col=9,endColumn=11,title=E_CLASS::")
    #t
    "GitHub annotation")
   (for
    ((mode (in-list '("--sarif" "--lsp" "--github-annotations"))))
    (define result
      (command (list "check" "missing.typify" mode) 2 (not (equal? mode "--github-annotations"))))
    (case (string->symbol mode)
      ((--sarif)
       (define loc
         (hash-ref
          (car (hash-ref (car (hash-ref (car (hash-ref result 'runs)) 'results)) 'locations))
          'physicalLocation))
       (check (hash-has-key? loc 'region) #f "missing file has no region"))
      ((--lsp)
       (check
        (hash-ref (car (hash-ref result 'unlocated)) 'code)
        "E_FILE"
        "missing file stays unlocated"))
      (else
       (check
        (string-prefix? result "::error file=missing.typify,title=E_FILE::")
        #t
        "file-level annotation"))))
   (check
    (hash-ref
     (car (hash-ref (command '("--lsp" "--json" "check" "declarations.typify") 2 #t) 'unlocated))
     'code)
    "E_USAGE"
    "conflicting output modes")
   (check (command '("check" "declarations.typify" "--github-annotations") 0 #f) "" "clean check")
   (check
    (hash-ref
     (car (hash-ref (command '("check" "declarations.typify" "--sarif") 0 #t) 'runs))
     'results)
    '()
    "clean SARIF")
   (define empty-report (command '("check" "declarations.typify" "--lsp") 0 #t))
   (check
    (map (lambda (doc) (hash-ref doc 'diagnostics)) (hash-ref empty-report 'documents))
    '(())
    "clear document diagnostics"))
  (with-projection-sources
   (lambda ()
     (define file "program%:q,\n.typify")
     (record-source-text file file "🚀a\r\n💡z")
     (define span (src-span file 1 2 2 2 4 11))
     (define related (hash 'path file 'kind "program" 'line 2))
     (define native-range (src-span->jsexpr span))
     (define native-related (hash 'file file 'range native-range 'language "Example"))
     (define fix (hash 'description "A proposed edit"))
     (define items
       (for/list
        ((code (in-list diagnostic-codes)))
        (define data
          (for/hash
           ((field (in-list (hash-ref diagnostic-data-fields code))))
           (values field (if (eq? field 'kind) (car (hash-ref diagnostic-kinds code)) "sample"))))
        (diagnostic->jsexpr
         (make-diagnostic
          code
          "message%\r\nwith second line"
          #:span
          span
          #:related
          (list related native-related)
          #:fix
          fix
          #:severity
          (if (string-prefix? code "N_") 'note 'error)
          #:data
          data))))
     (define (render mode)
       (with-output-to-string (lambda () (write-diagnostic-projection mode items))))
     (define sarif (car (hash-ref (string->jsexpr (render "--sarif")) 'runs)))
     (define report (string->jsexpr (render "--lsp")))
     (define lsp (hash-ref (car (hash-ref report 'documents)) 'diagnostics))
     (define annotations (render "--github-annotations"))
     (check
      (length (string-split annotations "\n"))
      (length diagnostic-codes)
      "one command per diagnostic")
     (check
      (string-contains? annotations "::notice file=program%25%3Aq%2C%0A.typify")
      #t
      "escaped note properties")
     (check (string-contains? annotations "message%25%0D%0Awith second line") #t "escaped text")
     (check (string-contains? annotations ",col=") #f "multiline location uses lines only")
     (define through-newline
       (hash-set (car items) 'range (src-span->jsexpr (src-span file 1 2 2 1 4 7))))
     (define line-command
       (with-output-to-string
        (lambda () (write-diagnostic-projection "--github-annotations" (list through-newline)))))
     (check
      (string-contains? line-command "line=1,endLine=1,")
      #t
      "exclusive end at next line start")
     (check
      (map (lambda (r) (hash-ref r 'ruleId)) (hash-ref sarif 'results))
      diagnostic-codes
      "every code in SARIF")
     (define sample (car (hash-ref sarif 'results)))
     (check
      (hash-ref (hash-ref (hash-ref sample 'properties) 'typify) 'fix)
      (hasheq 'description "A proposed edit")
      "SARIF native fix")
     (check
      (hash-ref (hash-ref (car lsp) 'data) 'fix)
      (hasheq 'description "A proposed edit")
      "LSP native fix")
     (check
      (string-length (hash-ref (hash-ref sample 'partialFingerprints) 'typify/v1))
      64
      "stable SARIF fingerprint")
     (check
      (sort
       (append
        (map (lambda (d) (hash-ref d 'code)) lsp)
        (map (lambda (d) (hash-ref d 'code)) (hash-ref report 'unlocated)))
       string<?)
      diagnostic-codes
      "every code accounted for in LSP")
     (for
      ((d (in-list lsp)))
      (check
       (hash-ref d 'range)
       (hasheq 'start (hasheq 'line 0 'character 2) 'end (hasheq 'line 1 'character 2))
       "multiline UTF-16 range with non-BMP at both ends")
      (check (length (hash-ref d 'relatedInformation)) 2 "related line and range")
      (check (hash-ref d 'severity) (if (string-prefix? (hash-ref d 'code) "N_") 3 1) "severity"))
     (check
      (length (hash-ref (car (hash-ref sarif 'results)) 'relatedLocations))
      2
      "SARIF related locations")))
  (with-projection-sources
   (lambda ()
     (define file "missing.typify")
     (record-source-path file (build-path "project" file))
     (define missing
       (diagnostic->jsexpr
        (make-diagnostic
         "E_FILE"
         "missing file"
         #:span
         (src-span file 1 1 1 1 0 0)
         #:data
         (hash 'path file))))
     (define (project mode)
       (string->jsexpr
        (with-output-to-string (lambda () (write-diagnostic-projection mode (list missing))))))
     (define result (car (hash-ref (car (hash-ref (project "--sarif") 'runs)) 'results)))
     (define physical (hash-ref (car (hash-ref result 'locations)) 'physicalLocation))
     (check
      (hash-ref (hash-ref physical 'artifactLocation) 'uri)
      "project/missing.typify"
      "unread tree path")
     (check
      (hash-ref (project "--lsp") 'documents)
      '()
      "failed read has no publishDiagnostics clear")))
  (hash 'suite "diagnostic-projections" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
