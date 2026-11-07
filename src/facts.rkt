#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/promise
         racket/string
         "classes.rkt"
         "diagnostics.rkt")

(provide collect-facts fact-query validate-facts)

(define fact-names '("exists" "directory" "symlink" "mode" "owner"))

(define fact-path-class (delay (hash-ref (compile-classes (hash)) "canonical-path")))

(define (canonical-fact-path? value)
  (and (string? value) (class-accepts? (force fact-path-class) value)))

(define (portable path)
  (define raw (if (path? path) (path->string path) path))
  (define text (if (eq? (system-type 'os) 'windows) (string-replace raw "\\" "/") raw))
  (if (or (equal? text "/") (regexp-match? #px"^[A-Za-z]:/$" text))
    text
    (regexp-replace #px"/+$" text "")))

(define (facts-error path)
  (raise-typify
   "E_FILE"
   "The file does not contain canonical v1 facts"
   #:span
   (src-span path 1 1 1 1 0 0)
   #:data
   (hash 'path path)))

(define (validate-facts data #:path (path "<facts>"))
  (unless (and
           (hash? data)
           (equal? (hash-ref data 'collector #f) "typify 1.0")
           (canonical-fact-path? (hash-ref data 'root #f))
           (or (not (hash-has-key? data 'omitted))
               (exact-positive-integer? (hash-ref data 'omitted)))
           (list? (hash-ref data 'facts #f)))
    (facts-error path))
  (define seen (make-hash))
  (for
   ((item (in-list (hash-ref data 'facts))))
   (unless (and
            (hash? item)
            (member (hash-ref item 'fact #f) fact-names)
            (list? (hash-ref item 'args #f))
            (= (length (hash-ref item 'args)) 1)
            (andmap canonical-fact-path? (hash-ref item 'args))
            (or (not (hash-has-key? item 'value))
                (boolean? (hash-ref item 'value))
                (string? (hash-ref item 'value)))
            (case (string->symbol (hash-ref item 'fact))
              ((mode)
               (and (string? (hash-ref item 'value #f))
                    (regexp-match? #px"^[0-7]{4}$" (hash-ref item 'value))))
              ((owner)
               (and (string? (hash-ref item 'value #f))
                    (regexp-match? #px"^[0-9]+$" (hash-ref item 'value))))
              ((symlink) (string? (hash-ref item 'value #f)))
              (else #t)))
     (facts-error path))
   (define identity (cons (hash-ref item 'fact) (hash-ref item 'args)))
   (when (hash-has-key? seen identity) (facts-error path))
   (hash-set! seen identity #t))
  data)

(define (fact-query data name arguments)
  (cond
   ((equal? name "clean")
    (define clean?
      (and data
           (= (length arguments) 2)
           (andmap string? arguments)
           (let* ((base (car arguments))
                  (name (cadr arguments))
                  (prefix (if (equal? base "/") "/" (string-append base "/"))))
             (not
              (for/or
               ((item (in-list (hash-ref data 'facts)))
                #:when
                (member (hash-ref item 'fact) '("directory" "symlink")))
               (define path (car (hash-ref item 'args)))
               (and (not (equal? path base))
                    (string-prefix? path prefix)
                    (>=
                     (length (string-split (substring path (string-length prefix)) "/" #:trim? #f))
                     2)
                    (equal? (last (string-split path "/" #:trim? #f)) name)))))))
    (if clean? (values #t #t) (values #f #f)))
   (else
    (define item
      (and data
           (for/first
            ((item (in-list (hash-ref data 'facts)))
             #:when
             (and (equal? (hash-ref item 'fact) name) (equal? (hash-ref item 'args) arguments)))
            item)))
    (if item (values #t (hash-ref item 'value #t)) (values #f #f)))))

(define (octal-mode mode)
  (define digits (number->string (bitwise-and mode 4095) 8))
  (string-append (make-string (- 4 (string-length digits)) #\0) digits))

(define (collect-facts given-root)
  (define root (simplify-path (path->complete-path given-root) #f))
  (unless (canonical-fact-path? (portable root))
    (raise-typify
     "E_USAGE"
     "The normalized facts root must be a canonical path"
     #:data
     (hash 'actual (if (path? given-root) (path->string given-root) given-root))))
  (define entries '())
  (define omitted 0)
  (define (file-error path)
    (raise-typify
     "E_FILE"
     "Filesystem metadata cannot be collected"
     #:span
     (src-span (portable path) 1 1 1 1 0 0)
     #:data
     (hash 'path (portable path))))
  (define (add! name path (value #f) (has-value? #f))
    (define entry (hash 'fact name 'args (list (portable path))))
    (set! entries (cons (if has-value? (hash-set entry 'value value) entry) entries)))
  (define (walk path)
    (with-handlers
     ((exn:fail:filesystem? (lambda (_) (file-error path))))
     (define type (file-or-directory-type path #f))
     (when type
       (define link? (or (link-exists? path) (memq type '(link directory-link))))
       (define metadata (file-or-directory-stat path #t))
       (cond
        ((canonical-fact-path? (portable path))
         (add! "exists" path)
         (when link? (add! "symlink" path (path->string (resolve-path path)) #t))
         (when (and (eq? type 'directory) (not link?)) (add! "directory" path))
         (add! "mode" path (octal-mode (hash-ref metadata 'mode)) #t)
         (add! "owner" path (number->string (hash-ref metadata 'user-id)) #t))
        (else (set! omitted (add1 omitted))))
       (when (and (eq? type 'directory) (not link?))
         (when (link-exists? path) (file-error path))
         (for
          ((name (in-list (sort (directory-list path) string<? #:key path->string))))
          (walk (build-path path name)))))))
  (unless (file-or-directory-type root #f) (file-error root))
  (walk root)
  (define ordered (sort entries string<? #:key canonical-json))
  (define data (hash 'collector "typify 1.0" 'root (portable root) 'facts ordered))
  (validate-facts
   (if (positive? omitted) (hash-set data 'omitted omitted) data)
   #:path
   (portable root)))
