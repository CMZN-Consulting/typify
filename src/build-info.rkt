#lang racket/base

(require (for-syntax
          racket/base
          compiler/cm-accomplice
          file/sha1
          racket/file
          racket/list
          racket/path
          racket/string))

(provide typify-version typify-build)

(define typify-version "1.0.1")

(define-syntax (reader-source-build stx)
  (define root (path-only (syntax-source stx)))
  (define sources
    (sort
     (for/list
      ((path
        (in-directory
         root
         (lambda (dir)
           (and (not (link-exists? dir))
                (not (equal? (file-name-from-path dir) (string->path "compiled")))))))
       #:when
       (and (file-exists? path)
            (not (link-exists? path))
            (equal? (path-get-extension path) #".rkt")))
      (define relative (find-relative-path root path))
      (cons (string-join (map path->string (explode-path relative)) "/") path))
     string<?
     #:key
     car))
  (define out (open-output-bytes))
  (for
   ((entry (in-list sources)))
   (define path (cdr entry))
   (register-external-file path)
   (fprintf
    out
    "~a  ~a\n"
    (bytes->hex-string (call-with-input-file path sha256-bytes))
    (car entry)))
  (datum->syntax
   stx
   (string-append "sha256:" (bytes->hex-string (sha256-bytes (get-output-bytes out))))))

(define typify-build (reader-source-build))
