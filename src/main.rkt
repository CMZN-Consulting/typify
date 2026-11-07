#lang racket/base

(require json
         file/sha1
         racket/file
         racket/list
         racket/match
         racket/path
         racket/promise
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "emission.rkt"
         "language.rkt"
         "meta.rkt"
         "model.rkt"
         "facts.rkt"
         "plan.rkt"
         "tree.rkt"
         "syntax.rkt")

(provide run-command execute-command render-bounds render-tree render-plan display-diagnostics)

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
  (define limits
    (for*/fold
     ((limits (hash)))
     ((body (in-list bodies)) ((name default) (in-hash meta-cap-defaults)))
     (hash-set
      limits
      (string-append (language-name body) "." name)
      (hash 'limit (hash-ref (language-caps body) name default)))))
  (define fuelled
    (for*/fold
     ((limits limits))
     ((body (in-list bodies)) ((name limit) (in-hash (compiled-model-fuels (language-model body)))))
     (hash-set limits (string-append (language-name body) "." name ".fuel") (hash 'limit limit))))
  (define guarded
    (for/fold
     ((limits fuelled))
     (((name limit)
       (in-hash
        (for/fold
         ((guards class-host-guards))
         (((key value) (in-hash model-host-guards)))
         (hash-set guards key value)))))
     (hash-set limits name (hash 'limit limit))))
  (if (for/or
       ((body (in-list bodies)))
       (ormap constructor-rule (hash-values (language-constructors body))))
    (for/fold
     ((limits guarded))
     ((name '(c k)))
     (hash-set
      limits
      name
      (for/fold
       ((maximum 0))
       ((body (in-list bodies)))
       (max maximum (hash-ref (declaration-bound body) name)))))
    guarded))

(define (render-bounds report)
  (apply
   string-append
   (for/list
    ((name (in-list (sort (hash-keys report) string<? #:key (lambda (key) (format "~a" key))))))
    (define value (hash-ref report name))
    (format "~a: ~a\n" name (if (hash? value) (hash-ref value 'limit) value)))))

(define (program-captures body path)
  (cond
   ((not (language-file body)) (hash))
   (else
    (define given (if (eq? (system-type 'os) 'windows) (string-replace path "\\" "/") path))
    (define templates (string-split (language-file body) "/" #:trim? #f))
    (define components (string-split given "/" #:trim? #f))
    (define bindings
      (and (= (length templates) (length components))
           (for/fold
            ((bindings (hash)))
            ((source (in-list templates)) (part (in-list components)))
            (define captures (and bindings (match-template (parse-template source) part)))
            (and captures
                 (for/and
                  (((name value) (in-hash captures)))
                  (and (class-accepts? (hash-ref (language-classes body) name) value)
                       (or (not (hash-has-key? bindings name))
                           (equal? (hash-ref bindings name) value))))
                 (for/fold
                  ((result bindings))
                  (((name value) (in-hash captures)))
                  (hash-set result name value))))))
    (unless bindings
      (raise-typify
       "E_USAGE"
       "The program path does not match its language template"
       #:data
       (hash 'actual path)))
    bindings)))

(define (checked-bounds bodies diagnostics)
  (hash 'ok #t 'bounds (bounds-report bodies) 'diagnostics (map diagnostic->jsexpr diagnostics)))

(define (read-facts path)
  (define portable-path (string-replace path "\\" "/"))
  (define bytes
    (with-handlers
     ((exn:fail:filesystem?
       (lambda (_)
         (raise-typify
          "E_FILE"
          "The facts file cannot be read"
          #:span
          (src-span path 1 1 1 1 0 0)
          #:data
          (hash 'path (string-replace path "\\" "/"))))))
     (file->bytes path)))
  (define input (open-input-bytes bytes))
  (define data
    (with-handlers
     ((exn:fail:read?
       (lambda (_)
         (raise-typify
          "E_FILE"
          "The facts file is not JSON"
          #:span
          (src-span path 1 1 1 1 0 0)
          #:data
          (hash 'path portable-path)))))
     (define value (read-json input))
     (unless (eof-object? (read-json input))
       (raise-typify
        "E_FILE"
        "The facts file contains multiple documents"
        #:span
        (src-span path 1 1 1 1 0 0)
        #:data
        (hash 'path portable-path)))
     value))
  (values
   (validate-facts data #:path portable-path)
   (string-append "sha256:" (bytes->hex-string (sha256-bytes bytes)))))

(define (facts-input required? path root)
  (cond
   ((not required?) (values #f "none"))
   (path (define-values (data hash) (read-facts path)) (values data (if required? hash "none")))
   (required?
    (define data (collect-facts root))
    (values
     data
     (string-append
      "sha256:"
      (bytes->hex-string
       (sha256-bytes (string->bytes/utf-8 (string-append (canonical-json data) "\n")))))))
   (else (values #f "none"))))

(define (render-tree programs)
  (apply
   string-append
   (for/list
    ((program (in-list programs)))
    (format "~a ~a\n" (hash-ref program 'status) (hash-ref program 'path)))))

(define (render-plan report events)
  (string-append
   (hash-ref report 'status)
   "\n"
   (apply
    string-append
    (for/list
     ((event (in-list events)))
     (format
      "~a ~a~a~a\n"
      (hash-ref (hash-ref event 'program) 'path)
      (if (hash-ref event 'state-only? #f) "recorded " "")
      (canonical-json (hash-ref event 'term))
      (if (hash-ref event 'modelled?) "" " unchecked"))))))

(define (parse-options arguments)
  (let loop ((remaining arguments) (positionals '()) (options (hash)))
    (cond
     ((null? remaining) (values (reverse positionals) options))
     ((equal? (car remaining) "--facts")
      (when (or
             (hash-has-key? options "--facts")
             (null? (cdr remaining))
             (string-prefix? (cadr remaining) "--"))
        (raise-typify "E_USAGE" "--facts needs one file" #:data (hash 'actual "--facts")))
      (loop (cddr remaining) positionals (hash-set options "--facts" (cadr remaining))))
     ((member (car remaining) '("--json" "--gbnf" "--bounds"))
      (when (hash-has-key? options (car remaining))
        (raise-typify
         "E_USAGE"
         "A command-line flag was repeated"
         #:data
         (hash 'actual (car remaining))))
      (loop (cdr remaining) positionals (hash-set options (car remaining) #t)))
     ((and (string-prefix? (car remaining) "--") (not (equal? (car remaining) "--version")))
      (raise-typify
       "E_USAGE"
       "The command-line flag is not recognized"
       #:data
       (hash 'actual (car remaining))))
     (else (loop (cdr remaining) (cons (car remaining) positionals) options)))))

(define (run-command arguments)
  (define-values (result json? grammar? text) (run-command/details arguments))
  (values result json? grammar?))

(define (run-command/details arguments #:on-bound (on-bound void))
  (define-values (positional options) (parse-options arguments))
  (define json? (hash-ref options "--json" #f))
  (define gbnf? (hash-ref options "--gbnf" #f))
  (define bounds? (hash-ref options "--bounds" #f))
  (define facts-path (hash-ref options "--facts" #f))
  (define text-output #f)
  (when (and gbnf? json?)
    (raise-typify "E_USAGE" "--gbnf cannot be combined with --json" #:data (hash 'actual "--gbnf")))
  (when (and
         facts-path
         (not (and (pair? positional) (member (car positional) '("check" "tree" "plan")))))
    (raise-typify
     "E_USAGE"
     "--facts is available only with check, tree or plan"
     #:data
     (hash 'actual "--facts")))
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
  (define (planned-response report events declaration-diagnostics ensure-diagnostics)
    (define diagnostics (diagnostics-sort (append declaration-diagnostics ensure-diagnostics)))
    (define complete
      (hash-set*
       report
       'diagnostics
       (map diagnostic->jsexpr diagnostics)
       'status
       (if (diagnostics-error? diagnostics) "failed" (hash-ref report 'status))))
    (set! text-output (render-plan complete events))
    complete)
  (define result
    (match
     positional
     ((list "--version") (hash 'version typify-version))
     ((list "check" path)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (if bounds?
        (checked-bounds (hash-values declarations) diagnostics)
        (checked-response (hash 'diagnostics diagnostics))))
     ((list "check" path name program)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define body (select-language declarations name))
      (define captures (program-captures body program))
      (define checked
        (check-program/diagnostics
         (read-source program)
         body
         #:file
         program
         #:captures
         captures
         #:guard
         #f))
      (define complete
        (hash-set
         checked
         'diagnostics
         (diagnostics-sort (append diagnostics (hash-ref checked 'diagnostics '())))))
      (raise-diagnostics (hash-ref complete 'diagnostics))
      (if bounds?
        (checked-bounds (list body) (hash-ref complete 'diagnostics))
        (checked-response complete body)))
     ((list "facts" root) (collect-facts root))
     ((list "tree" path root)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define-values
       (facts facts-hash)
       (facts-input (tree-facts-required? declarations) facts-path root))
      (define checked
        (read-tree/diagnostics
         declarations
         root
         #:facts
         facts
         #:guard-resolver
         (lambda (record raw-terms raw-origins)
           (program-resolver (hash-set* record 'terms raw-terms 'origins raw-origins)))))
      (define entries (tree->jsexpr (hash-ref checked 'programs)))
      (define complete-diagnostics
        (diagnostics-sort (append diagnostics (hash-ref checked 'diagnostics))))
      (set! text-output (render-tree entries))
      (hash
       'ok
       (not (diagnostics-error? complete-diagnostics))
       'programs
       entries
       'diagnostics
       (map diagnostic->jsexpr complete-diagnostics)))
     ((list "plan" path root)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define-values
       (facts facts-hash)
       (facts-input
        (for/or ((body (in-hash-values declarations))) (language-needs-facts? body))
        facts-path
        root))
      (define checked
        (read-tree/diagnostics
         declarations
         root
         #:facts
         facts
         #:guard-resolver
         (lambda (record raw-terms raw-origins)
           (program-resolver (hash-set* record 'terms raw-terms 'origins raw-origins)))))
      (define tree-diagnostics (hash-ref checked 'diagnostics))
      (define programs (hash-ref checked 'programs))
      (when (diagnostics-error? tree-diagnostics) (on-bound (programs-bound programs)))
      (raise-diagnostics tree-diagnostics)
      (define ensure-diagnostics '())
      (define-values
       (report events)
       (plan-programs/details
        programs
        #:on-bound
        on-bound
        #:facts
        facts
        #:facts-hash
        facts-hash
        #:on-error
        (lambda (item) (set! ensure-diagnostics (cons item ensure-diagnostics)))))
      (planned-response report events (append diagnostics tree-diagnostics) ensure-diagnostics))
     ((list "plan" path name program)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define body (select-language declarations name))
      (define captures (program-captures body program))
      (when (and (language-needs-facts? body) (not facts-path))
        (raise-typify
         "E_USAGE"
         "A one-program plan needs an explicit facts file"
         #:data
         (hash 'actual "--facts")))
      (define-values
       (facts facts-hash)
       (facts-input (language-needs-facts? body) facts-path (current-directory)))
      (define ensure-diagnostics '())
      (define-values
       (report events)
       (plan-program/details
        body
        (read-source program)
        #:on-bound
        on-bound
        #:captures
        captures
        #:path
        program
        #:facts
        facts
        #:facts-hash
        facts-hash
        #:on-error
        (lambda (item) (set! ensure-diagnostics (cons item ensure-diagnostics)))))
      (planned-response report events diagnostics ensure-diagnostics))
     ((list "emit" "Typify")
      (if gbnf? (hash 'text (language->gbnf (meta-language))) (language->grammar (meta-language))))
     ((list "emit" path name)
      (define-values (declarations diagnostics) (load-declarations/diagnostics path))
      (define body (select-language declarations name))
      (if gbnf? (hash 'text (language->gbnf body)) (language->grammar body)))
     (_
      (raise-typify
       "E_USAGE"
       "usage: typify check DECLARATIONS [LANGUAGE PROGRAM] [--bounds] [--json] | facts ROOT | tree DECLARATIONS ROOT [--facts FILE] | plan DECLARATIONS ROOT [--facts FILE] | plan DECLARATIONS LANGUAGE PROGRAM [--facts FILE] | emit Typify | emit DECLARATIONS LANGUAGE [--gbnf] [--json]"
       #:data
       (hash 'actual (if (null? positional) "" (car positional)))))))
  (values
   (public-data
    (if (or (hash-has-key? result 'diagnostics)
            (not json?)
            (and (pair? positional) (equal? (car positional) "emit")))
      result
      (hash-set result 'diagnostics '())))
   json?
   gbnf?
   text-output))

(define (command-error-result arguments items #:bound (bound 'null))
  (define diagnostics (map diagnostic->jsexpr items))
  (define positional
    (with-handlers
     ((exn:fail:typify? (lambda (_) '())))
     (define-values (parsed options) (parse-options arguments))
     parsed))
  (define command (if (null? positional) "" (car positional)))
  (cond
   ((ormap (lambda (item) (equal? (diagnostic-code item) "E_USAGE")) items)
    (hash 'ok #f 'diagnostics diagnostics))
   ((equal? command "tree") (hash 'ok #f 'programs '() 'diagnostics diagnostics))
   ((equal? command "plan")
    (define empty
      (hash
       'status
       "failed"
       'state
       (hash)
       'bound
       (if (ormap
            (lambda (item)
              (member (diagnostic-code item) '("E_DECLARATION" "E_KEY_CANONICAL" "E_JOIN_LAW")))
            items)
         'null
         bound)
       'modelled
       "0 of 0"
       'facts
       "none"
       'origins
       '()
       'ensure
       '()
       'diagnostics
       diagnostics))
    (if (= (length positional) 4)
      (hash-set* empty 'plan '() 'recorded '() 'skips '())
      (hash-set empty 'programs '())))
   ((equal? command "facts")
    (hash
     'collector
     "typify 1.0"
     'root
     (if (pair? (cdr positional)) (cadr positional) "")
     'facts
     '()
     'diagnostics
     diagnostics))
   ((equal? command "emit") (hash 'ok #f 'diagnostics diagnostics))
   (else (hash 'ok #f 'terms '() 'diagnostics diagnostics))))

(define (execute-command arguments)
  (define json? (and (member "--json" arguments) #t))
  (define failure-bound 'null)
  (with-handlers
   ((exn:fail:typify?
     (lambda (error)
       (define items (exn:fail:typify-diagnostics error))
       (define native (map diagnostic->jsexpr items))
       (if json?
         (displayln (canonical-json (command-error-result arguments items #:bound failure-bound)))
         (display-diagnostics native))
       (values
        (if (and (not (ormap (lambda (item) (equal? (diagnostic-code item) "E_USAGE")) items))
                 (let-values (((positional options) (parse-options arguments)))
                   (and (pair? positional) (equal? (car positional) "emit"))))
          1
          (if (ormap (lambda (item) (member (diagnostic-code item) '("E_USAGE" "E_FILE"))) items)
            2
            1))
        native))))
   (define-values
    (result json-output? grammar? text-output)
    (run-command/details arguments #:on-bound (lambda (bound) (set! failure-bound bound))))
   (define diagnostics (hash-ref result 'diagnostics '()))
   (unless json-output? (display-diagnostics diagnostics))
   (cond
    ((equal? arguments '("--version")) (displayln typify-version))
    ((and (member "--bounds" arguments) (not json-output?))
     (display (render-bounds (hash-ref result 'bounds))))
    ((and (hash-has-key? result 'terms) (hash-has-key? result 'ok) (not json-output?)) (void))
    ((and text-output (not json-output?)) (display text-output))
    ((and (hash-has-key? result 'text) (not json-output?)) (display (hash-ref result 'text)))
    (else (displayln (canonical-json result))))
   (values
    (if (for/or ((item (in-list diagnostics))) (equal? (hash-ref item 'severity) "error")) 1 0)
    diagnostics)))

(module+
 main
 (define-values
  (status diagnostics)
  (execute-command (vector->list (current-command-line-arguments))))
 (exit status))
