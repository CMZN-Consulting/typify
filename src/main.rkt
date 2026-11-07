#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/promise
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "emission.rkt"
         "language.rkt"
         "meta.rkt")

(provide run-command render-bounds display-diagnostics)

(define typify-version "1.0")

(define (public-data value)
  (cond
   ((src-span? value) (src-span->jsexpr value))
   ((hash? value) (for/hash (((key item) (in-hash value))) (values key (public-data item))))
   ((list? value) (map public-data value))
   (else value)))

(define (read-source path)
  (with-handlers
   ((exn:fail:filesystem?
     (lambda (error)
       (raise-typify
        "E_FILE"
        "The requested file cannot be read"
        #:span
        (src-span path 1 1 1 1 0 0)
        #:data
        (hash 'path (string-replace path "\\" "/"))))))
   (file->string path)))

(define (load-declarations/diagnostics path)
  (define-values
   (declarations diagnostics)
   (read-declarations/diagnostics (read-source path) #:file path))
  (raise-diagnostics diagnostics)
  (values declarations diagnostics))

(define (with-diagnostics result diagnostics)
  (if (null? diagnostics)
    result
    (hash-set result 'diagnostics (map diagnostic->jsexpr diagnostics))))

(define (display-diagnostics items)
  (for
   ((item (in-list items)))
   (define span (hash-ref item 'range 'null))
   (define start (if (hash? span) (hash-ref span 'start) (hash)))
   (fprintf
    (current-error-port)
    "~a:~a:~a: ~a: ~a [~a]\n"
    (hash-ref item 'file)
    (hash-ref start 'line 0)
    (hash-ref start 'column 0)
    (hash-ref item 'severity)
    (hash-ref item 'message)
    (hash-ref item 'code))))

(define (select-language declarations name)
  (hash-ref
   declarations
   name
   (lambda () (raise-typify "E_USAGE" "The language is not declared" #:data (hash 'actual name)))))

(define (bounds-report bodies)
  (hash
   'languages
   (for/list
    ((body (in-list (sort bodies string<? #:key language-name))))
    (hash 'language (language-name body) 'checks (declaration-check-bounds body)))
   'host
   (hash 'name "racket" 'version (version) 'guards class-host-guards)))

(define (render-bounds report)
  (define output (open-output-string))
  (define host (hash-ref report 'host))
  (fprintf
   output
   "Derived automaton bounds (~a ~a)\n"
   (hash-ref host 'name)
   (hash-ref host 'version))
  (displayln "Host-dependent representation and product-state upper bounds." output)
  (for
   ((name (in-list (sort (hash-keys (hash-ref host 'guards)) string<?))))
   (fprintf output "Host guard ~a: ~a\n" name (hash-ref (hash-ref host 'guards) name)))
  (for
   ((entry (in-list (hash-ref report 'languages))))
   (define checks (hash-ref entry 'checks))
   (fprintf output "Language ~a\n" (hash-ref entry 'language))
   (unless (hash-ref checks 'recorded #f) (displayln "  Checks were not recorded." output))
   (for
    ((item (in-list (hash-ref checks 'classes '()))))
    (define bounds (hash-ref item 'bounds))
    (fprintf
     output
     "  Class ~a: controls=~a, bounded-states<=~a, edge-groups<=~a, intervals<=~a\n"
     (hash-ref item 'name)
     (hash-ref bounds 'pattern-control-states)
     (hash-ref bounds 'bounded-states-upper-bound)
     (hash-ref bounds 'bounded-edge-groups-upper-bound)
     (hash-ref bounds 'bounded-interval-pieces-upper-bound)))
   (for
    ((item (in-list (hash-ref checks 'forms '()))))
    (define right (hash-ref item 'right))
    (fprintf
     output
     "  Form ~a: left-states<=~a, right-states<=~a, whole-states<=~a\n"
     (hash-ref item 'ordinal)
     (hash-ref (hash-ref item 'left) 'bounded-states-upper-bound)
     (if (hash? right) (hash-ref right 'bounded-states-upper-bound) "none")
     (hash-ref (hash-ref item 'whole) 'bounded-states-upper-bound)))
   (for
    ((item (in-list (hash-ref checks 'signature-inclusions '()))))
    (fprintf
     output
     "  Inclusion ~a[~a] <~a> -> ~a: product-states<=~a\n"
     (hash-ref item 'constructor)
     (hash-ref item 'argument)
     (hash-ref item 'capture)
     (hash-ref item 'class)
     (hash-ref (hash-ref item 'bounds) 'states-upper-bound)))
   (for
    ((item (in-list (hash-ref checks 'delimiters '()))))
    (fprintf
     output
     "  Delimiter form ~a <~a> before ~s: product-states<=~a\n"
     (hash-ref item 'form)
     (hash-ref item 'capture)
     (hash-ref item 'delimiter)
     (hash-ref (hash-ref item 'bounds) 'states-upper-bound)))
   (for
    ((item (in-list (hash-ref checks 'overlaps '()))))
    (fprintf
     output
     "  Overlap forms ~a/~a: product-states<=~a\n"
     (hash-ref item 'earlier)
     (hash-ref item 'later)
     (hash-ref (hash-ref item 'bounds) 'states-upper-bound))))
  (get-output-string output))

(define (run-command arguments)
  (define json? (and (member "--json" arguments) #t))
  (define gbnf? (and (member "--gbnf" arguments) #t))
  (define bounds? (and (member "--bounds" arguments) #t))
  (for
   ((value (in-list arguments)) #:when (string-prefix? value "--"))
   (unless (member value '("--json" "--gbnf" "--bounds" "--version"))
     (raise-typify
      "E_USAGE"
      "The command-line flag is not recognized"
      #:data
      (hash 'actual value))))
  (define positional
    (filter (lambda (value) (not (member value '("--json" "--gbnf" "--bounds")))) arguments))
  (when (and bounds? (not (and (pair? positional) (equal? (car positional) "check"))))
    (raise-typify
     "E_USAGE"
     "--bounds is available only with check"
     #:data
     (hash 'actual "--bounds")))
  (when (and gbnf? (not (and (pair? positional) (equal? (car positional) "emit"))))
    (raise-typify "E_USAGE" "--gbnf is available only with emit" #:data (hash 'actual "--gbnf")))
  (define (checked-response checked (body #f))
    (define diagnostics (hash-ref checked 'diagnostics '()))
    (raise-diagnostics diagnostics)
    (define response
      (hash
       'ok
       #t
       'terms
       (hash-ref checked 'terms '())
       'diagnostics
       (map diagnostic->jsexpr diagnostics)))
    (if (and body (ormap constructor-rule (hash-values (language-constructors body))))
      (hash-set response 'bound (declaration-bound body))
      response))
  (define result
    (match
     positional
     ((list "--version") (hash 'version typify-version))
     ((list "check" path)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (if bounds?
        (with-diagnostics (bounds-report (hash-values declarations)) diagnostics)
        (checked-response (hash 'diagnostics diagnostics))))
     ((list "check" path name program)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define body (select-language declarations name))
      (define checked (check-program/diagnostics (read-source program) body #:file program))
      (define complete
        (hash-set
         checked
         'diagnostics
         (diagnostics-sort (append diagnostics (hash-ref checked 'diagnostics '())))))
      (raise-diagnostics (hash-ref complete 'diagnostics))
      (if bounds?
        (with-diagnostics (bounds-report (list body)) (hash-ref complete 'diagnostics))
        (checked-response complete body)))
     ((list "emit" "Typify")
      (if gbnf? (hash 'text (language->gbnf (meta-language))) (language->grammar (meta-language))))
     ((list "emit" path name)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define body (select-language declarations name))
      (with-diagnostics
       (if gbnf? (hash 'text (language->gbnf body)) (language->grammar body))
       diagnostics))
     (_
      (raise-typify
       "E_USAGE"
       "usage: typify check DECLARATIONS [LANGUAGE PROGRAM] [--bounds] [--json] | emit Typify | emit DECLARATIONS LANGUAGE [--gbnf] [--json]"
       #:data
       (hash 'actual arguments)))))
  (values (public-data result) json? gbnf?))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (define json? (and (member "--json" arguments) #t))
 (with-handlers
  ((exn:fail:typify?
    (lambda (error)
      (define items (exn:fail:typify-diagnostics error))
      (if json?
        (displayln
         (canonical-json (hash 'ok #f 'terms '() 'diagnostics (map diagnostic->jsexpr items))))
        (for
         ((item (in-list items)))
         (define span (diagnostic-range item))
         (fprintf
          (current-error-port)
          "~a:~a:~a: ~a: ~a [~a]\n"
          (diagnostic-file item)
          (if span (src-span-start-line span) 0)
          (if span (src-span-start-column span) 0)
          (diagnostic-severity item)
          (diagnostic-message item)
          (diagnostic-code item))))
      (exit
       (if (ormap (lambda (item) (member (diagnostic-code item) '("E_USAGE" "E_FILE"))) items)
         2
         1)))))
  (define-values (result json-output? grammar?) (run-command arguments))
  (unless json-output? (display-diagnostics (hash-ref result 'diagnostics '())))
  (cond
   ((equal? arguments '("--version")) (displayln typify-version))
   ((and (member "--bounds" arguments) (not json-output?)) (display (render-bounds result)))
   ((and (pair? arguments) (equal? (car arguments) "check") (not json-output?)) (void))
   ((and (hash-has-key? result 'text) (not json-output?)) (display (hash-ref result 'text)))
   (else (displayln (canonical-json result))))))
