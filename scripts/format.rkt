#lang racket/base

(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/pretty
         racket/string
         "leak-check.rkt")

(provide format-repository)

(define (format-source text)
  (when (source-comment-offset text) (error 'format "source comments are forbidden"))
  (define header (regexp-match #px"^#lang[^\n]*\n" text))
  (unless header (error 'format "expected a language header"))
  (define input (open-input-string (substring text (string-length (car header)))))
  (define output (open-output-string))
  (display (car header) output)
  (parameterize
   ((read-accept-reader #f) (read-accept-lang #f) (pretty-print-columns 100))
   (let loop ()
     (define form (read input))
     (unless (eof-object? form) (newline output) (pretty-write form output) (loop))))
  (get-output-string output))

(define (format-repository (given-root (current-directory)) #:write? (write? #f))
  (define root (simplify-path (path->complete-path given-root)))
  (define changed '())
  (define errors '())
  (define count 0)
  (for
   ((path
     (in-directory
      root
      (lambda (path)
        (and (not (link-exists? path))
             (not (member (path->string (file-name-from-path path)) '(".git" "compiled")))))))
    #:when
    (and (file-exists? path)
         (not (link-exists? path))
         (regexp-match? #rx"[.]rkt$" (path->string path))))
   (set! count (add1 count))
   (define name (string-replace (path->string (find-relative-path root path)) "\\" "/"))
   (with-handlers
    ((exn:fail?
      (lambda (error)
        (set! errors (cons (hasheq 'file name 'message (exn-message error)) errors)))))
    (define original (file->string path))
    (define formatted (format-source original))
    (unless (equal? original formatted)
      (set! changed (cons name changed))
      (when write? (display-to-file formatted path #:exists 'truncate/replace)))))
  (hasheq
   'pass
   (and (null? errors) (or write? (null? changed)))
   'files
   count
   'changed
   (reverse changed)
   'errors
   (reverse errors)))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (unless (or (null? arguments) (equal? arguments '("--check")) (equal? arguments '("--write")))
   (displayln "usage: racket scripts/format.rkt [--check | --write]" (current-error-port))
   (exit 2))
 (define result (format-repository #:write? (equal? arguments '("--write"))))
 (write-json result)
 (newline)
 (exit (if (hash-ref result 'pass) 0 1)))
