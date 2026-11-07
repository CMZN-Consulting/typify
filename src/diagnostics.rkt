#lang racket/base

(require file/sha1 json racket/list racket/string)

(provide (struct-out src-span)
         (struct-out diagnostic)
         (struct-out exn:fail:typify)
         make-diagnostic
         raise-typify
         diagnostic->jsexpr
         src-span->jsexpr
         canonical-json
         content-hash
         diagnostic-codes)

(struct
 src-span
 (file start-line start-column end-line end-column start-byte end-byte)
 #:transparent)

(struct diagnostic (code severity file range message related fix data fingerprints) #:transparent)

(struct exn:fail:typify exn:fail (diagnostic) #:transparent)

(define diagnostic-codes
  '("E_TEMPLATE" "E_TEMPLATE_DUPLICATE" "E_TEMPLATE_ADJACENT" "E_TERM" "E_CAPTURE"))

(define (text value)
  (cond ((symbol? value) (symbol->string value)) ((path? value) (path->string value)) (else value)))

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
  (diagnostic
   (text code)
   (text severity)
   (portable-file (src-span-file span))
   span
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
  (hasheq
   'startLine
   (src-span-start-line span)
   'startColumn
   (src-span-start-column span)
   'endLine
   (src-span-end-line span)
   'endColumn
   (src-span-end-column span)
   'startByte
   (src-span-start-byte span)
   'endByte
   (src-span-end-byte span)))

(define (diagnostic->jsexpr item)
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
