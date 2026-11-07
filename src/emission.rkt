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
  (define local-names
    (for/hash
     ((row (in-list (language-rows body))) #:when (eq? (hash-ref row 'kind) 'class))
     (values (hash-ref (hash-ref row 'captures) "name") #t)))
  (define (qualify name)
    (string-append (if (hash-has-key? local-names name) (language-name body) "Typify") "." name))
  (define classes (language-classes body))
  (define roots
    (remove-duplicates
     (append-map
      (lambda (candidate)
        (for/list
         ((part (in-list (append (form-left candidate) (or (form-right candidate) '()))))
          #:when
          (slot? part))
         (slot-name part)))
      (language-forms body))))
  (define reachable
    (let loop ((pending roots) (seen (hash)))
      (cond
       ((null? pending) seen)
       ((hash-has-key? seen (car pending)) (loop (cdr pending) seen))
       (else
        (define name (car pending))
        (loop
         (append (class->references (hash-ref classes name)) (cdr pending))
         (hash-set seen name #t))))))
  (define caps (compiled-class-caps (hash-ref classes "letter")))
  (hash
   'language
   (language-name body)
   'version
   (string->number (language-version body))
   'caps
   (hash
    'states
    (hash-ref caps 'states)
    'repetition
    (hash-ref caps 'repetition)
    'depth
    (hash-ref caps 'reference-depth)
    'length
    (hash-ref caps 'token-length))
   'classes
   (for/hash
    ((name (in-list (sort (hash-keys reachable) string<?))))
    (values (qualify name) (class->description (hash-ref classes name) #:qualify qualify)))
   'forms
   (for/list
    ((candidate (in-list (language-forms body))))
    (define left (template-production (form-left candidate)))
    (define verb
      (if (and (memq (form-kind candidate) '(colon bang))
               (andmap string? (form-left candidate))
               (positive? (string-length left))
               (for/and ((character (in-string left))) (not (char-whitespace? character))))
        left
        'null))
    (define description
      (hash
       'shape
       (symbol->string (form-kind candidate))
       'production
       (form-source candidate)
       'verb
       verb))
    (if (eq? (form-kind candidate) 'arrow)
      (hash-set* description 'left left 'right (template-production (form-right candidate)))
      description))))

(define (template-production parts)
  (apply
   string-append
   (for/list
    ((part (in-list parts)))
    (if (slot? part) (string-append "<" (slot-name part) ">") part))))

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
