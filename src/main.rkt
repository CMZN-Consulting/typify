#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/promise
         racket/string
         "diagnostics.rkt"
         "emission.rkt"
         "language.rkt"
         "meta.rkt")

(provide run-command)

(define typify-version "1.0")

(define (public-data value)
  (cond
   ((src-span? value) (src-span->jsexpr value))
   ((hash? value) (for/hash (((key item) (in-hash value))) (values key (public-data item))))
   ((list? value) (map public-data value))
   (else value)))

(define (load-declarations path) (read-declarations (file->string path) #:file path))

(define (select-language declarations name)
  (hash-ref
   declarations
   name
   (lambda ()
     (raise-typify
      "E_LANGUAGE_UNDECLARED"
      "The language is not declared"
      #:data
      (hash 'language name)))))

(define (run-command arguments)
  (define json? (and (member "--json" arguments) #t))
  (define gbnf? (and (member "--gbnf" arguments) #t))
  (define positional (filter (lambda (value) (not (member value '("--json" "--gbnf")))) arguments))
  (define result
    (match
     positional
     ((list "--version") (hash 'version typify-version))
     ((list "check" path)
      (define declarations (load-declarations path))
      (hash
       'valid
       #t
       'languages
       (for/list
        ((name (in-list (sort (hash-keys declarations) string<?))))
        (declaration->jsexpr (hash-ref declarations name)))))
     ((list "check" path name program)
      (define body (select-language (load-declarations path) name))
      (check-program (file->string program) body #:file program))
     ((list "emit" "Typify")
      (cond
       (gbnf? (hash 'text (language->gbnf (meta-language))))
       (json? (hash 'grammar (language->grammar (meta-language))))
       (else (hash 'text (meta-source)))))
     ((list "emit" path name)
      (define body (select-language (load-declarations path) name))
      (if gbnf? (hash 'text (language->gbnf body)) (language->grammar body)))
     (_
      (raise-typify
       "E_USAGE"
       "usage: typify check DECLARATIONS [LANGUAGE PROGRAM] | emit Typify | emit DECLARATIONS LANGUAGE [--gbnf] [--json]"))))
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
        (displayln (canonical-json (hash 'valid #f 'diagnostics (list (diagnostic->jsexpr item)))))
        (fprintf
         (current-error-port)
         "~a:~a:~a: ~a: ~a [~a]\n"
         (diagnostic-file item)
         (src-span-start-line (diagnostic-range item))
         (src-span-start-column (diagnostic-range item))
         (diagnostic-severity item)
         (diagnostic-message item)
         (diagnostic-code item)))
      (exit (if (equal? (diagnostic-code item) "E_USAGE") 2 1))))
   (exn:fail:filesystem?
    (lambda (error)
      (define item
        (make-diagnostic
         "E_UNREADABLE"
         "The requested file cannot be read"
         #:data
         (hash 'reason (exn-message error))))
      (if json?
        (displayln (canonical-json (hash 'valid #f 'diagnostics (list (diagnostic->jsexpr item)))))
        (displayln (exn-message error) (current-error-port)))
      (exit 2))))
  (define-values (result json-output? grammar?) (run-command arguments))
  (cond
   ((equal? arguments '("--version")) (displayln typify-version))
   ((and (hash-has-key? result 'text) (not json-output?)) (display (hash-ref result 'text)))
   (else (displayln (canonical-json result))))))
