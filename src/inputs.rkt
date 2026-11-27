#lang racket/base

(require file/sha1
         racket/file
         racket/path
         racket/string
         "diagnostics.rkt"
         "physical-path.rkt"
         (only-in "projections.rkt" record-source-text))

(provide current-root-inputs
         call-with-root-inputs
         inputs->jsexpr
         observe-root!
         observe-facts-root!
         observe-path!
         observe-directory!
         observe-fact!
         input-source-text)

(struct observations (rows (reversed #:mutable)))

(struct input-record ((root #:mutable) (facts-root #:mutable) groups))

(define current-root-inputs (make-parameter #f))

(define (call-with-root-inputs operation started)
  (define groups
    (for/hash
     ((kind '(declarations paths directories programs facts)))
     (values kind (observations (make-hash) '()))))
  (define records (input-record 'null 'null groups))
  (started records)
  (parameterize
   ((current-root-inputs records))
   (hash-set (operation) 'inputs (inputs->jsexpr records))))

(define (inputs->jsexpr records)
  (for/fold
   ((result (hash 'root (input-record-root records) 'factsRoot (input-record-facts-root records))))
   (((kind group) (in-hash (input-record-groups records))))
   (hash-set result kind (reverse (observations-reversed group)))))

(define (observe! kind key row)
  (define group (hash-ref (input-record-groups (current-root-inputs)) kind))
  (define rows (observations-rows group))
  (define prior (hash-ref rows key #f))
  (cond
   ((not prior)
    (hash-set! rows key row)
    (set-observations-reversed! group (cons row (observations-reversed group))))
   ((not (equal? prior row))
    (define name (if (string? key) key (car key)))
    (raise-typify
     "E_FILE"
     "An input changed during the walk"
     #:span
     (src-span name 1 1 1 1 0 0)
     #:data
     (hash 'path name)))))

(define (portable-path value)
  (define text (if (path? value) (path->string value) value))
  (cond
   ((not (eq? (system-type 'os) 'windows)) text)
   ((complete-path? value) (physical-path-name value))
   (else (string-replace text "\\" "/"))))

(define (observe-root! root)
  (when (current-root-inputs)
    (set-input-record-root! (current-root-inputs) (physical-path-name root))))

(define (observe-facts-root! facts)
  (when (and (current-root-inputs) facts)
    (set-input-record-facts-root! (current-root-inputs) (hash-ref facts 'root))))

(define (observe-path! path kind)
  (when (current-root-inputs) (observe! 'paths path (hash 'path path 'kind kind))))

(define (digest bytes) (bytes->hex-string (sha256-bytes bytes)))

(define (observe-directory! path entries)
  (observe!
   'directories
   path
   (hash
    'path
    path
    'entries
    entries
    'sha256
    (digest (string->bytes/utf-8 (canonical-json entries))))))

(define (observe-fact! name arguments held?)
  (when (current-root-inputs)
    (observe! 'facts (cons name arguments) (hash 'fact name 'args arguments 'held held?))))

(define (input-source-text kind display-path physical-path #:bytes (given #f))
  (define observed-path (and (current-root-inputs) (portable-path display-path)))
  (define bytes (or given (file->bytes physical-path)))
  (when observed-path
    (observe! kind observed-path (hash 'path observed-path 'sha256 (digest bytes))))
  (record-source-text
   display-path
   (path->complete-path physical-path)
   (bytes->string/utf-8 bytes #\�)))
