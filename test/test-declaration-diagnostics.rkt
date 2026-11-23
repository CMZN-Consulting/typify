#lang racket/base

(require racket/list
         racket/file
         racket/string
         "../src/main.rkt"
         "../src/language.rkt"
         (only-in "../src/meta.rkt" current-meta-classes)
         "../src/diagnostics.rkt")

(provide run-tests)

(define (source rows)
  (string-append
   "#lang typify\ndeclare : Demo\n"
   (string-join
    (append
     rows
     (for/list
      ((row (in-list (remove-duplicates rows)))
       #:when
       (regexp-match? #px"^primitive : [^:]+(?: : .+)?$" row))
      (string-append "effect : " (cadr (string-split row " : ")) " = external")))
    "\n")
   "\nend!\n"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-declaration-diagnostics "~a: expected ~s; received ~s" context expected actual)))
  (define (refusal text)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (read-declarations text #:file "declaration.typify")
     (error 'test-declaration-diagnostics "The invalid declaration was accepted")))
  (define (kind rows expected)
    (define item (refusal (source rows)))
    (check (diagnostic-code item) "E_DECLARATION" "declaration code")
    (check (hash-ref (diagnostic-data item) 'kind) expected "declaration kind")
    (check
     (hash-has-key?
      (diagnostic-data-projection (diagnostic-code item) (diagnostic-data item))
      'actual)
     #t
     "declaration carries compared actual data"))
  (kind '("cap : states = 0") "syntax")
  (kind '("is : a = b" "is : b = a") "class-cycle")
  (kind '("primitive : output" "primitive : output") "duplicate")
  (kind '("does : missing") "orphan")
  (kind '("count : missing = 1") "reference")
  (kind '("primitive : output" "term : action : value value" "rule : action <x> -> output") "arity")
  (kind '("primitive : output : value" "form : run!" "does : output <missing>") "capture")
  (kind '("primitive : output" "rule : output -> output") "conflict")
  (kind
   '("primitive : output : value"
     "term : first : value"
     "term : second : value"
     "rule : first <x> -> second <x>"
     "rule : second <x> -> output <x>")
   "stratification")
  (define self-cycle (refusal (source '("term : loop : value" "rule : loop <x> -> loop <x>"))))
  (check (diagnostic-code self-cycle) "E_RULE_CYCLE" "a self edge is a genuine rewrite cycle")
  (check (hash-ref (diagnostic-data self-cycle) 'constructor) "loop" "cycle names its constructor")
  (define capture-error
    (refusal
     (source
      '("is : source = \"word\""
        "is : target = /[0-9]+/"
        "primitive : output : target"
        "form : run : <source>"
        "does : output <source>"))))
  (check
   (diagnostic-data-projection (diagnostic-code capture-error) (diagnostic-data capture-error))
   (hash 'kind "capture" 'constructor "output" 'class "target" 'actual "source")
   "capture refusal compares classes rather than a host-chosen witness")
  (define adjacent
    (refusal
     (source
      '("is : a = \"a\""
        "is : b = \"b\""
        "primitive : output : a b"
        "form : run : <a><b>"
        "does : output <a> <b>"))))
  (check
   (diagnostic-data-projection (diagnostic-code adjacent) (diagnostic-data adjacent))
   (hash 'form "run : <a><b>" 'class "a" 'token "")
   "adjacent placeholders use the full form and first class")
  (define cap-error (refusal (source '("cap : states = 4097"))))
  (check (diagnostic-code cap-error) "E_CAP" "raising a declared cap is a cap refusal")
  (check
   (diagnostic-data-projection (diagnostic-code cap-error) (diagnostic-data cap-error))
   (hash 'cap "states" 'limit 4096 'actual 4097)
   "declared cap compares limit and attempted value")
  (define width-header
    (string-append
     "#lang typify\r\ndeclare : Counters\r\n"
     "is : différence = \"🐢\"\r\nprimitive : tally : text\r\n"
     "\r\ncell : écho-store = shadow\r\n"))
  (define (width-source line)
    (string-append
     width-header
     line
     "\r\n"
     (if (string-prefix? (string-trim line) "count :") "effect : tally = external\r\n" "")
     "end!\r\n"))
  (define (check-width label head token tail)
    (define text (width-source (string-append head token tail)))
    (define-values (decls diagnostics) (read-declarations/diagnostics text))
    (define actual (filter (lambda (d) (equal? (diagnostic-severity d) "error")) diagnostics))
    (check (map diagnostic-code actual) (list "E_CLASS") label)
    (check (map diagnostic-data actual) (list (hash 'class "width" 'token token)) label)
    (define span (diagnostic-range (car actual)))
    (define before (string-append width-header head))
    (define byte-start (bytes-length (string->bytes/utf-8 before)))
    (check
     (list
      (src-span-start-line span)
      (src-span-end-line span)
      (src-span-start-column span)
      (src-span-end-column span)
      (src-span-start-byte span)
      (src-span-end-byte span))
     (list
      7
      7
      (add1 (string-length head))
      (add1 (+ (string-length head) (string-length token)))
      byte-start
      (+ byte-start (bytes-length (string->bytes/utf-8 token))))
     label))
  (check-width
   "put width repeated in name and quoted value"
   "  writes : tally : écho-store = put  "
   "tally"
   "  \"tally\" ")
  (check-width "non-BMP write width" " writes : tally : écho-store = put " "🐢" " \"🐢\"")
  (check-width "bare put width" "writes : tally : écho-store = put " "-4" "")
  (check-width "noncanonical leading zero" "writes : tally : écho-store = put " "01" "")
  (check-width "delete width" "writes : tally : écho-store = delete " "猫" "")
  (check-width "keyed count width" " count : tally = 0..1 by " "-4" "")
  (define-values
   (emitted-body emitted-errors)
   (read-declarations/diagnostics
    (width-source "count : tally = 0..1 by 🐢")
    #:meta
    (meta-language)))
  (check
   (for/list
    ((item (in-list emitted-errors)) #:when (equal? (diagnostic-code item) "E_CLASS"))
    (diagnostic-data item))
   (list (hash 'class "width" 'token "🐢"))
   "the emitted meta selects the keyed width refusal")
  (for
   ((token (in-list '("-3" "wrong"))))
   (define-values
    (ignored recovered)
    (parameterize
     ((current-meta-classes (hash-set (current-meta-classes) "width" "/.*/")))
     (read-declarations/diagnostics (width-source (string-append "count : tally = 1 by " token)))))
   (check
    (for/list
     ((d (in-list recovered)) #:when (equal? (diagnostic-severity d) "error"))
     (list (diagnostic-code d) (hash-ref (diagnostic-data d) 'actual #f)))
    (list (list "E_TERM_CLASS" token))
    "the kernel rejects an invalid synthesized width"))
  (define alternative-meta
    (hash-ref
     (read-declarations
      (source
       '("primitive : declare-language : name"
         "primitive : close-language"
         "primitive : set-order : order-mode"
         "primitive : declare-keyed-count : name multiplicity width"
         "is : tail = \"star = 1 by bad\""
         "form : open : <name>"
         "does : declare-language <name>"
         "form : finish!"
         "does : close-language"
         "form : count : <name> = <multiplicity> by <width>"
         "does : declare-keyed-count <name> <multiplicity> <width>"
         "form : count : <tail>"
         "does : set-order strict"
         "form : writes : <value>"
         "does : set-order free")))
     "Demo"))
  (define (custom-order line)
    (language-order
     (hash-ref
      (read-declarations
       (string-append "#lang typify\nopen : Sample\n" line "\nfinish!\n")
       #:meta
       alternative-meta)
      "Sample")))
  (check
   (custom-order "count : star = 1 by bad")
   "strict"
   "a complete fallback wins after a failed keyed capture")
  (check
   (custom-order "writes : star : écho-store = put -9")
   "free"
   "a custom writes form retains its own operation")
  (define (write-verdict line)
    (define-values (decls diagnostics) (read-declarations/diagnostics (width-source line)))
    (for/list
     ((item (in-list diagnostics)) #:when (= 7 (src-span-start-line (diagnostic-range item))))
     (list (diagnostic-code item) (hash-ref (diagnostic-data item) 'kind #f))))
  (for
   ((line
     (in-list
      '("writes : tally : écho-store = put"
        "writes : tally : écho-store = put one unquoted"
        "writes : tally : écho-store = delete one \"extra\""))))
   (check (write-verdict line) '(("E_DECLARATION" "syntax")) "malformed writes keep syntax"))
  (check
   (write-verdict "writes : tally : écho-store = put 2")
   '(("E_DECLARATION" "arity"))
   "decimal write exceeding arity keeps formation error")
  (check (write-verdict "writes : tally : écho-store = put 1 \"1\"") '() "legal quoted put")
  (define directory (make-temporary-file "typify-declaration-order~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define declaration-path (build-path directory "broken.typify"))
     (call-with-output-file
      declaration-path
      (lambda (output)
        (display
         "#lang typify\ndeclare : Demo\nversion : nope\ncap : states = zero\nend!\n"
         output)))
     (define missing-program (build-path directory "must-not-be-read.typify"))
     (define items
       (with-handlers
        ((exn:fail:typify? exn:fail:typify-diagnostics))
        (call-with-values
         (lambda ()
           (run-command
            (list
             "check"
             (path->string declaration-path)
             "Demo"
             (path->string missing-program)
             "--json")))
         (lambda _ '()))))
     (check
      (map diagnostic-code items)
      '("E_CLASS" "E_CLASS")
      "failed declaration reports all errors without opening the missing program")
     (check
      (map (lambda (item) (src-span-start-line (diagnostic-range item))) items)
      '(3 4)
      "declaration diagnostics are sorted by source position"))
   (lambda () (delete-directory/files directory)))
  (define note
    (make-diagnostic
     "N_CLASS_EMPTY"
     "Class language is empty"
     #:severity
     'note
     #:span
     (src-span "source.typify" 3 1 3 17 20 36)
     #:data
     (hash 'class "empty")))
  (define notice-output (open-output-string))
  (parameterize
   ((current-error-port notice-output))
   (display-diagnostics (list (diagnostic->jsexpr note))))
  (check
   (regexp-match?
    #rx"note: Class language is empty.*N_CLASS_EMPTY"
    (get-output-string notice-output))
   #t
   "successful text commands can display nonfatal notes")
  (hash 'suite "declaration-diagnostics" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
