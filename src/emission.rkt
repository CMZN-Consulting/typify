#lang racket/base

(require racket/list racket/string "classes.rkt" "language.rkt" "syntax.rkt")

(provide language->gbnf language->grammar)

(define (quoted text)
  (define (escape character)
    (define value (char->integer character))
    (cond
     ((= value 34) "\\\"")
     ((= value 92) "\\\\")
     ((and (<= 32 value 126)) (string character))
     (else
      (define width (if (<= value 65535) 4 8))
      (define digits (number->string value 16))
      (string-append
       (if (= width 4) "\\u" "\\U")
       (make-string (- width (string-length digits)) #\0)
       digits))))
  (string-append "\"" (apply string-append (map escape (string->list text))) "\""))

(define (language->grammar body)
  (hash
   'language
   (declaration->jsexpr body)
   'classes
   (for/hash
    ((name (in-list (sort (hash-keys (language-classes body)) string<?))))
    (values name (class-witnesses (hash-ref (language-classes body) name))))))

(define (language->gbnf body)
  (define rules (make-hash))
  (define (register machine name)
    (define grammar (class->gbnf-rules machine #:name name))
    (for (((key value) (in-hash (hash-ref grammar 'rules)))) (hash-set! rules key value))
    (hash-ref grammar 'start))
  (define alternatives
    (for/list
     ((candidate (in-list (language-forms body))) (index (in-naturals)))
     (define-values (left right) (form-automata body candidate))
     (define prefix (string-append "form-" (number->string index)))
     (string-append
      (register left (string-append prefix "-left"))
      (case (form-kind candidate)
        ((bang) (string-append " " (quoted "!")))
        ((colon)
         (string-append
          " spacing "
          (quoted ":")
          " spacing "
          (register right (string-append prefix "-right"))))
        ((arrow)
         (string-append
          " spacing "
          (quoted "->")
          " spacing "
          (register right (string-append prefix "-right"))))))))
  (hash-set! rules "spacing" "[ \\t]*")
  (hash-set!
   rules
   "root"
   (string-append
    "spacing ("
    (string-join
     (append
      (list "\"\"")
      (if (positive? (string-length (language-comment body)))
        (list (string-append (quoted (language-comment body)) " [^\\r\\n]*"))
        '())
      alternatives)
     " | ")
    ") spacing"))
  (string-append
   (string-join
    (for/list
     ((name (in-list (sort (hash-keys rules) string<?))))
     (string-append name " ::= " (hash-ref rules name)))
    "\n")
   "\n"))
