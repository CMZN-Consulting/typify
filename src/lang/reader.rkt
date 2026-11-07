#lang racket/base

(require racket/port racket/string)

(provide read read-syntax get-info)

(define (read-syntax source input)
  (define remaining (port->string input))
  (define text
    (string-append
     "#lang typify"
     (if (or (string-prefix? remaining "\n") (string-prefix? remaining "\r")) "" "\n")
     remaining))
  (define file
    (cond ((path? source) (path->string source)) ((string? source) source) (else "<input>")))
  (datum->syntax
   #f
   `(module typify-module racket/base
      (require typify/language)
      (provide declarations)
      (define declarations (read-declarations ,text #:file ,file)))))

(define (read input) (syntax->datum (read-syntax #f input)))

(define (get-info key default) default)
