#lang racket/base

(require racket/path racket/string "diagnostics.rkt")

(provide physical-root-path physical-path-name)

(define (path-text value)
  (cond
   ((path? value) (path->string value))
   ((path-for-some-system? value) (bytes->string/utf-8 (path->bytes value)))
   (else value)))

(define (unsupported-windows! value)
  (raise-typify
   "E_USAGE"
   "Expected an ordinary Windows drive or UNC share path"
   #:data
   (hash 'actual value)))

(define (windows-spelling value)
  (define text (string-replace value "\\" "/"))
  (when (or
         (regexp-match?
          #px"(?i:^//[?.](?:/|$)|^/\\?\\?(?:/|$)|^/Device(?:/|$)|^//[^/]+/(?:pipe|mailslot)(?:/|$))"
          text)
         (regexp-match? #px"^[A-Za-z]:(?:[^/]|$)" text)
         (and (string-prefix? text "//") (not (regexp-match? #px"^//[^/]+/[^/]+(?:/|$)" text))))
    (unsupported-windows! value))
  text)

(define (physical-root-path value)
  (when (eq? (system-path-convention-type) 'windows) (windows-spelling (path-text value)))
  (apply build-path (explode-path (simplify-path (path->complete-path value) #f))))

(define (physical-path-name
         value
         (convention
          (if (path-for-some-system? value)
            (path-convention-type value)
            (system-path-convention-type))))
  (define raw (path-text value))
  (define text
    (cond
     ((eq? convention 'windows)
      (define native (windows-spelling raw))
      (cond
       ((regexp-match #px"^([A-Za-z]):(/.*)$" native)
        =>
        (lambda (parts) (string-append "/Volumes/" (string-upcase (cadr parts)) (caddr parts))))
       ((regexp-match #px"^//([^/]+)/([^/]+)(/.*)?$" native)
        =>
        (lambda (parts)
          (when (or (member (cadr parts) '("." "..")) (member (caddr parts) '("." "..")))
            (unsupported-windows! raw))
          (string-append "/Volumes/UNC/" (cadr parts) "/" (caddr parts) (or (cadddr parts) ""))))
       (else (unsupported-windows! raw))))
     (else raw)))
  (if (equal? text "/") text (regexp-replace #px"/+$" text "")))
