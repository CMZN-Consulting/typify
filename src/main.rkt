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

(provide run-command render-bounds)

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

(define (load-declarations path) (read-declarations (read-source path) #:file path))

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
    (define response (hash 'ok #t 'terms (hash-ref checked 'terms '()) 'diagnostics '()))
    (if (and body (ormap constructor-rule (hash-values (language-constructors body))))
      (hash-set response 'bound (declaration-bound body))
      response))
  (define result
    (match
     positional
     ((list "--version") (hash 'version typify-version))
     ((list "check" path)
      (define declarations (load-declarations path))
      (if bounds? (bounds-report (hash-values declarations)) (checked-response (hash))))
     ((list "check" path name program)
      (define body (select-language (load-declarations path) name))
      (define checked (check-program (read-source program) body #:file program))
      (if bounds? (bounds-report (list body)) (checked-response checked body)))
     ((list "emit" "Typify")
      (if gbnf? (hash 'text (language->gbnf (meta-language))) (language->grammar (meta-language))))
     ((list "emit" path name)
      (define body (select-language (load-declarations path) name))
      (if gbnf? (hash 'text (language->gbnf body)) (language->grammar body)))
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
      (define item (exn:fail:typify-diagnostic error))
      (if json?
        (displayln
         (canonical-json (hash 'ok #f 'terms '() 'diagnostics (list (diagnostic->jsexpr item)))))
        (fprintf
         (current-error-port)
         "~a:~a:~a: ~a: ~a [~a]\n"
         (diagnostic-file item)
         (src-span-start-line (diagnostic-range item))
         (src-span-start-column (diagnostic-range item))
         (diagnostic-severity item)
         (diagnostic-message item)
         (diagnostic-code item)))
      (exit (if (member (diagnostic-code item) '("E_USAGE" "E_FILE")) 2 1)))))
  (define-values (result json-output? grammar?) (run-command arguments))
  (cond
   ((equal? arguments '("--version")) (displayln typify-version))
   ((and (member "--bounds" arguments) (not json-output?)) (display (render-bounds result)))
   ((and (pair? arguments) (equal? (car arguments) "check") (not json-output?)) (void))
   ((and (hash-has-key? result 'text) (not json-output?)) (display (hash-ref result 'text)))
   (else (displayln (canonical-json result))))))
