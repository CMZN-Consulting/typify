#lang racket/base

(require file/sha1 json racket/list racket/string)

(provide (struct-out src-span)
         (struct-out diagnostic)
         (struct-out exn:fail:typify)
         (struct-out exn:fail:typify:aggregate)
         exn:fail:typify-diagnostics
         diagnostics-sort
         diagnostics-error?
         raise-diagnostics
         make-diagnostic
         raise-typify
         diagnostic->jsexpr
         src-span->jsexpr
         canonical-json
         content-hash
         diagnostic-codes
         diagnostic-data-fields
         diagnostic-kinds
         diagnostic-data-projection)

(struct
 src-span
 (file start-line start-column end-line end-column start-byte end-byte)
 #:transparent)

(struct diagnostic (code severity file range message related fix data fingerprints) #:transparent)

(struct exn:fail:typify exn:fail (diagnostic) #:transparent)

(struct exn:fail:typify:aggregate exn:fail:typify (diagnostics) #:transparent)

(define (exn:fail:typify-diagnostics error)
  (if (exn:fail:typify:aggregate? error)
    (exn:fail:typify:aggregate-diagnostics error)
    (list (exn:fail:typify-diagnostic error))))

(define (diagnostics-error? items)
  (ormap (lambda (item) (equal? (diagnostic-severity item) "error")) items))

(define (diagnostics-sort items)
  (define (position item)
    (define span (diagnostic-range item))
    (list
     (diagnostic-file item)
     (if span (src-span-start-line span) 0)
     (if span (src-span-start-column span) 0)))
  (define (position<? left right)
    (cond
     ((string<? (car left) (car right)) #t)
     ((string<? (car right) (car left)) #f)
     ((< (cadr left) (cadr right)) #t)
     ((> (cadr left) (cadr right)) #f)
     (else (< (caddr left) (caddr right)))))
  (sort items position<? #:key position #:cache-keys? #t))

(define (raise-diagnostics items)
  (define ordered (diagnostics-sort items))
  (define first-error (findf (lambda (item) (equal? (diagnostic-severity item) "error")) ordered))
  (when first-error
    (raise
     (exn:fail:typify:aggregate
      (diagnostic-message first-error)
      (current-continuation-marks)
      first-error
      ordered))))

(define diagnostic-data-fields
  (hash
   "N_CELL_WIDTH"
   '(cell)
   "N_CLASS_EMPTY"
   '(class)
   "E_LINE_SHAPE"
   '(actual)
   "E_FORM_NONE"
   '(actual)
   "E_FORM_SHADOW"
   '(form actual)
   "E_CLASS"
   '(class token)
   "E_CLASS_UNDECLARED"
   '(class)
   "E_CLASS_OVERLAP"
   '(form class token)
   "E_CAP"
   '(cap limit actual)
   "E_EXAMPLE"
   '(form actual)
   "E_PROGRAM_COUNT"
   '(constructor expected actual)
   "E_ORDER"
   '(form expected actual)
   "E_REQUIRED"
   '(path)
   "E_TERM_CLASS"
   '(kind constructor class actual)
   "E_TERM_NESTED"
   '(constructor actual)
   "E_CONSTRUCTOR_UNBOUND"
   '(constructor)
   "E_RULE_DUPLICATE"
   '(constructor rule)
   "E_RULE_CYCLE"
   '(constructor)
   "E_RULE_FUEL"
   '(constructor limit actual)
   "E_LIBRARY_DOMAIN"
   '(constructor expected actual)
   "E_LIBRARY_PARENT"
   '(path)
   "E_JOIN_LAW"
   '(cell expected actual)
   "E_KEY_CANONICAL"
   '(cell class)
   "E_ENSURE"
   '(ensure key)
   "E_FACT_REJECT"
   '(fact key)
   "E_RESTRICTS"
   '(form constructor)
   "E_QUESTION"
   '(actual)
   "E_FILE"
   '(path)
   "E_USAGE"
   '(actual)
   "E_DECLARATION"
   '(kind actual)
   "E_CONTEXT"
   '(constructor key)))

(define diagnostic-codes (sort (hash-keys diagnostic-data-fields) string<?))

(define diagnostic-kinds
  (hash
   "E_DECLARATION"
   '("header"
     "body"
     "duplicate"
     "orphan"
     "arity"
     "capture"
     "syntax"
     "class-cycle"
     "stratification"
     "reference"
     "conflict")
   "E_TERM_CLASS"
   '("literal" "capture" "result" "arity" "sort")))

(define (text value)
  (cond ((symbol? value) (symbol->string value)) ((path? value) (path->string value)) (else value)))

(define (diagnostic-data-projection code data)
  (define name (text code))
  (define fields
    (hash-ref
     diagnostic-data-fields
     name
     (lambda ()
       (raise-arguments-error
        'diagnostic-data-projection
        "unregistered diagnostic code"
        "code"
        name))))
  (unless (hash? data) (raise-argument-error 'diagnostic-data-projection "hash?" data))
  (define projected
    (for/hash
     ((field (in-list fields)))
     (define spelling (symbol->string field))
     (define symbol-key? (hash-has-key? data field))
     (define string-key? (hash-has-key? data spelling))
     (unless (or symbol-key? string-key?)
       (raise-arguments-error
        'diagnostic-data-projection
        "missing required diagnostic data field"
        "code"
        name
        "field"
        field))
     (when (and symbol-key? string-key?)
       (raise-arguments-error
        'diagnostic-data-projection
        "duplicate diagnostic data field spellings"
        "code"
        name
        "field"
        field))
     (values field (hash-ref data (if symbol-key? field spelling)))))
  (define kinds (hash-ref diagnostic-kinds name #f))
  (when (and kinds (not (member (hash-ref projected 'kind) kinds)))
    (raise-arguments-error
     'diagnostic-data-projection
     "unregistered diagnostic kind"
     "code"
     name
     "kind"
     (hash-ref projected 'kind)))
  projected)

(define (portable-file file) (string-replace (text file) "\\" "/"))

(define (content-hash content) (bytes->hex-string (sha256-bytes (string->bytes/utf-8 content))))

(define (make-diagnostic
         code
         message
         #:span
         (span (src-span "<input>" 1 1 1 1 0 0))
         #:severity
         (severity 'error)
         #:related
         (related '())
         #:fix
         (fix #f)
         #:data
         (data (hasheq))
         #:rule
         (rule code)
         #:verb
         (verb "")
         #:ordinal
         (ordinal 0)
         #:content
         (content ""))
  (diagnostic-data-projection code data)
  (diagnostic
   (text code)
   (text severity)
   (portable-file (src-span-file span))
   (if (member (text code) '("E_REQUIRED" "E_FILE" "E_USAGE")) #f span)
   message
   related
   fix
   data
   (hasheq
    'rule
    (text rule)
    'verb
    (text verb)
    'useOrdinal
    ordinal
    'primaryLocationLineHash
    (content-hash content))))

(define (raise-typify
         code
         message
         #:span
         (span (src-span "<input>" 1 1 1 1 0 0))
         #:severity
         (severity 'error)
         #:related
         (related '())
         #:fix
         (fix #f)
         #:data
         (data (hasheq))
         #:rule
         (rule code)
         #:verb
         (verb "")
         #:ordinal
         (ordinal 0)
         #:content
         (content ""))
  (raise
   (exn:fail:typify
    message
    (current-continuation-marks)
    (make-diagnostic
     code
     message
     #:span
     span
     #:severity
     severity
     #:related
     related
     #:fix
     fix
     #:data
     data
     #:rule
     rule
     #:verb
     verb
     #:ordinal
     ordinal
     #:content
     content))))

(define (src-span->jsexpr span)
  (if (not span)
    'null
    (hasheq
     'start
     (hasheq
      'line
      (src-span-start-line span)
      'column
      (src-span-start-column span)
      'byteOffset
      (src-span-start-byte span))
     'end
     (hasheq
      'line
      (src-span-end-line span)
      'column
      (src-span-end-column span)
      'byteOffset
      (src-span-end-byte span)))))

(define (diagnostic->jsexpr item)
  (diagnostic-data-projection (diagnostic-code item) (diagnostic-data item))
  (hasheq
   'code
   (diagnostic-code item)
   'severity
   (diagnostic-severity item)
   'file
   (diagnostic-file item)
   'range
   (src-span->jsexpr (diagnostic-range item))
   'message
   (diagnostic-message item)
   'related
   (diagnostic-related item)
   'fix
   (or (diagnostic-fix item) 'null)
   'data
   (diagnostic-data item)
   'fingerprints
   (diagnostic-fingerprints item)))

(define (canonical-json value)
  (define out (open-output-string))
  (define (emit item)
    (cond
     ((hash? item)
      (define entries
        (sort
         (for/list
          (((key val) (in-hash item)))
          (unless (or (string? key) (symbol? key))
            (raise-argument-error 'canonical-json "string or symbol object key" key))
          (cons (text key) val))
         string<?
         #:key
         car))
      (unless (= (length entries) (length (remove-duplicates (map car entries))))
        (raise-arguments-error 'canonical-json "duplicate JSON object keys" "value" item))
      (write-char #\{ out)
      (for
       ((entry (in-list entries)) (index (in-naturals)))
       (unless (zero? index) (write-char #\, out))
       (write-json (car entry) out)
       (write-char #\: out)
       (emit (cdr entry)))
      (write-char #\} out))
     ((or (list? item) (vector? item))
      (write-char #\[ out)
      (for
       ((entry (if (vector? item) (in-vector item) (in-list item))) (index (in-naturals)))
       (unless (zero? index) (write-char #\, out))
       (emit entry))
      (write-char #\] out))
     (else (write-json item out))))
  (emit value)
  (get-output-string out))
