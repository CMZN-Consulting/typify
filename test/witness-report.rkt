#lang racket/base

(require json racket/list racket/string "../src/classes.rkt" "../src/language.rkt")

(provide measure-witnesses)

(define (segment? value)
  (and (string? value) (positive? (string-length value)) (not (string-contains? value "/"))))

(define (duplicates values)
  (define seen (make-hash))
  (define repeated (make-hash))
  (for
   ((value (in-list values)))
   (if (hash-has-key? seen value) (hash-set! repeated value #t) (hash-set! seen value #t)))
  (sort (hash-keys repeated) string<?))

(define (measure-class namespace name body class-name)
  (define qualified (string-join (list namespace name class-name) "/"))
  (define identity
    (hash 'qualified-class-name qualified 'namespace namespace 'language name 'class class-name))
  (with-handlers
   ((exn:fail?
     (lambda (failure)
       (hash-set*
        identity
        'written
        #f
        'checks
        0
        'passed
        #f
        'error
        (if (class-error? failure)
          (hash
           'code
           (class-error-code failure)
           'data
           (class-error-data failure)
           'message
           (exn-message failure))
          (hash 'message (exn-message failure)))))))
   (define class (hash-ref (language-classes body) class-name))
   (define serialized
     (jsexpr->string
      (hash-set* identity 'witnesses (class-witnesses class) 'caps (compiled-class-caps class))))
   (define written (string->jsexpr serialized))
   (define witnesses (hash-ref written 'witnesses))
   (define shortest (hash-ref witnesses 'shortest))
   (define boundary (hash-ref witnesses 'boundary))
   (define near (hash-ref witnesses 'near-miss))
   (define empty? (and (eq? shortest #f) (eq? boundary #f)))
   (define overlap (and empty? (class-overlap-witness class class)))
   (define shortest-ok?
     (if empty? (eq? shortest #f) (and (string? shortest) (class-accepts? class shortest))))
   (define boundary-ok?
     (if empty? (eq? boundary #f) (and (string? boundary) (class-accepts? class boundary))))
   (define near-ok? (and (string? near) (not (class-accepts? class near))))
   (define identity-ok? (equal? name (language-name body)))
   (define empty-ok? (or (not empty?) (eq? overlap #f)))
   (hash-set*
    written
    'written
    #t
    'checks
    (if empty? 5 4)
    'empty-language
    empty?
    'replay
    (hash
     'shortest
     shortest-ok?
     'boundary
     boundary-ok?
     'near-miss
     near-ok?
     'identity
     identity-ok?
     'self-overlap-checked
     empty?
     'self-overlap
     overlap
     'empty-language
     empty-ok?)
    'passed
    (and shortest-ok? boundary-ok? near-ok? identity-ok? empty-ok?))))

(define (measure-witnesses models)
  (unless (and
           (list? models)
           (for/and
            ((entry (in-list models)))
            (and (pair? entry) (segment? (car entry)) (hash? (cdr entry)))))
    (raise-argument-error
     'measure-witnesses
     "alist of namespace strings and language hashes"
     models))
  (define namespace-names (sort (remove-duplicates (map car models)) string<?))
  (define namespace-collisions (duplicates (map car models)))
  (define descriptions
    (for*/list
     ((entry (in-list (sort models string<? #:key car)))
      (name (in-list (sort (hash-keys (cdr entry)) string<?)))
      (class-name
       (in-list
        (let ((body (hash-ref (cdr entry) name)))
          (unless (and (segment? name) (language? body))
            (error 'measure-witnesses "language registry has an invalid entry"))
          (sort (hash-keys (language-classes body)) string<?)))))
     (unless (segment? class-name) (error 'measure-witnesses "class registry has an invalid name"))
     (list (car entry) name (hash-ref (cdr entry) name) class-name)))
  (define expected
    (sort
     (for/list
      ((description (in-list descriptions)))
      (string-join (list (car description) (cadr description) (cadddr description)) "/"))
     string<?))
  (define collisions (duplicates expected))
  (define witnesses
    (for/list ((description (in-list descriptions))) (apply measure-class description)))
  (define written
    (sort
     (for/list
      ((record (in-list witnesses)) #:when (hash-ref record 'written))
      (hash-ref record 'qualified-class-name))
     string<?))
  (define exact-coverage?
    (and (null? collisions) (null? namespace-collisions) (equal? written expected)))
  (define failures
    (+
     (count (lambda (record) (not (hash-ref record 'passed))) witnesses)
     (length collisions)
     (length namespace-collisions)
     (if (equal? written expected) 0 1)))
  (hash
   'status
   (if (zero? failures) "passed" "failed")
   'total
   (length descriptions)
   'classes
   (length descriptions)
   'written
   (length written)
   'failures
   failures
   'checks
   (+ 3 (for/sum ((record (in-list witnesses))) (hash-ref record 'checks)))
   'namespaces
   (length namespace-names)
   'namespace-names
   namespace-names
   'namespace-collisions
   namespace-collisions
   'qualified-class-names
   written
   'expected-qualified-class-names
   expected
   'collisions
   collisions
   'exact-coverage
   exact-coverage?
   'witnesses
   witnesses))
