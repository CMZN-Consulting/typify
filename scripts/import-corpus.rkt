#lang racket/base

(require file/sha1 json racket/file racket/list racket/path racket/port racket/string)

(provide import-corpus corpus-inventory)

(define (sha256-file path)
  (call-with-input-file path (lambda (input) (bytes->hex-string (sha256-bytes input)))))

(define (digest? text) (and (string? text) (regexp-match? #px"^[0-9a-f]{64}$" text)))

(define (inside? root candidate)
  (define root-parts (explode-path root))
  (define candidate-parts (explode-path candidate))
  (and (<= (length root-parts) (length candidate-parts))
       (equal? root-parts (take candidate-parts (length root-parts)))))

(define (kind path)
  (when (or (link-exists? path) (memq (file-or-directory-type path #t) '(link directory-link)))
    (error 'import-corpus "corpus paths must not be symlinks"))
  (case (bitwise-and (hash-ref (file-or-directory-stat path #t) 'mode) 61440)
    ((16384) 'directory)
    ((32768) 'file)
    (else (error 'import-corpus "corpus paths must be regular files or directories"))))

(define (present? path) (and (file-or-directory-type path #f) #t))

(define (entry-name path)
  (define name (path->string path))
  (when (or
         (member name '("." ".." ".git"))
         (string-contains? name "\\")
         (for/or
          ((character (in-string name)))
          (memq (char-general-category character) '(cc cf zl zp))))
    (error 'import-corpus "a corpus path contains a reserved or nonportable component"))
  name)

(define (ordered-entries directory) (sort (directory-list directory) string<? #:key path->string))

(define (directory-source source)
  (define path (if (path? source) source (string->path source)))
  (unless (and (absolute-path? path) (eq? (kind path) 'directory))
    (error 'import-corpus "source must be an explicit absolute directory"))
  (simplify-path path #t))

(define (snapshot source cases)
  (define files '())
  (define directories '())
  (define total 0)
  (define (walk path components)
    (define relative (string-join components "/"))
    (case (kind path)
      ((directory)
       (set! directories (cons relative directories))
       (for
        ((entry (ordered-entries path)))
        (walk (build-path path entry) (append components (list (entry-name entry))))))
      ((file)
       (define size (file-size path))
       (when (> size (* 16 1024 1024))
         (error 'import-corpus "a corpus file exceeds the import size limit"))
       (set! total (+ total size))
       (when (> total (* 64 1024 1024))
         (error 'import-corpus "corpus exceeds the total import size limit"))
       (set! files (cons (hasheq 'path relative 'bytes size 'sha256 (sha256-file path)) files)))))
  (for
   ((name cases))
   (define directory (build-path source name))
   (unless (eq? (kind directory) 'directory)
     (error 'import-corpus "every case must be a directory"))
   (unless (eq? (kind (build-path directory "expected.json")) 'file)
     (error 'import-corpus "every case must carry an expected.json file"))
   (walk directory (list name)))
  (define ordered-files (sort files string<? #:key (lambda (item) (hash-ref item 'path))))
  (define ordered-directories (sort directories string<?))
  (define pairs
    (sort
     (append
      (for/list ((item ordered-files)) (list (hash-ref item 'path) (hash-ref item 'sha256)))
      (for/list ((name ordered-directories)) (list (string-append name "/") "directory")))
     string<?
     #:key
     car))
  (define encoded
    (call-with-output-bytes (lambda (output) (write-json pairs output) (newline output))))
  (hasheq
   'cases
   cases
   'files
   ordered-files
   'directories
   ordered-directories
   'bytes
   total
   'tree-sha256
   (bytes->hex-string (sha256-bytes encoded))
   'tree-hash-format
   "sha256-sorted-json-pairs-newline-v1"))

(define (corpus-inventory source)
  (define directory (directory-source source))
  (define cases (map entry-name (ordered-entries directory)))
  (when (null? cases) (error 'import-corpus "an empty corpus cannot be imported"))
  (when (member "import.json" cases)
    (error 'import-corpus "a case name collides with the import receipt"))
  (snapshot directory cases))

(define (relative-path name) (apply build-path (string-split name "/")))

(define (copy-snapshot source destination inventory)
  (for
   ((name (hash-ref inventory 'directories)))
   (make-directory* (build-path destination (relative-path name))))
  (for
   ((item (hash-ref inventory 'files)))
   (define relative (relative-path (hash-ref item 'path)))
   (define from (build-path source relative))
   (unless (and (eq? (kind from) 'file) (equal? (sha256-file from) (hash-ref item 'sha256)))
     (error 'import-corpus "source changed before copying a file"))
   (define to (build-path destination relative))
   (copy-file from to #f)
   (file-or-directory-permissions to 420)))

(define (import-corpus source expected provenance #:root (given-root (current-directory)))
  (unless (and (digest? expected) (digest? provenance))
    (error 'import-corpus "tree and handoff SHA256 values must be explicit lowercase hex"))
  (define root (simplify-path (path->complete-path given-root) #t))
  (unless (and
           (file-exists? (build-path root "src" "info.rkt"))
           (directory-exists? (build-path root "test")))
    (error 'import-corpus "run from the source checkout"))
  (define directory (directory-source source))
  (when (inside? root directory)
    (error 'import-corpus "an external corpus cannot originate in the reader checkout"))
  (define inventory (corpus-inventory directory))
  (unless (equal? (hash-ref inventory 'tree-sha256) expected)
    (error 'import-corpus "source tree SHA256 differs from the authorized value"))
  (define destination (build-path root "test" "corpus"))
  (unless (inside? root (simplify-path destination #t))
    (error 'import-corpus "corpus destination resolves outside the source checkout"))
  (when (link-exists? destination)
    (error 'import-corpus "corpus destination must not be a symlink"))
  (define cases (hash-ref inventory 'cases))
  (define receipt-path (build-path destination "import.json"))
  (define receipt
    (hash-set*
     inventory
     'schema
     "typify-corpus-import-v2"
     'layout
     "test/corpus/<case-id>/"
     'verbatim
     #t
     'provenance
     (hasheq 'kind "external-blind-handoff" 'sha256 provenance)))
  (cond
   ((present? receipt-path)
    (unless (and
             (eq? (kind receipt-path) 'file)
             (equal? (call-with-input-file receipt-path read-json) receipt)
             (equal? (snapshot destination cases) inventory))
      (error 'import-corpus "existing import differs or is incomplete; no file was overwritten"))
    receipt)
   (else
    (for
     ((name cases))
     (when (present? (build-path destination name))
       (error 'import-corpus "a destination case already exists; no file was overwritten")))
    (make-directory* destination)
    (define staging (make-temporary-file "corpus~a" 'directory destination))
    (define committed '())
    (define complete? #f)
    (dynamic-wind
     void
     (lambda ()
       (copy-snapshot directory staging inventory)
       (unless (and
                (equal? (corpus-inventory directory) inventory)
                (equal? (snapshot staging cases) inventory))
         (error 'import-corpus "source changed during import or copied tree differs"))
       (define receipt-temporary (make-temporary-file "receipt~a.tmp" #f staging))
       (call-with-output-file
        receipt-temporary
        (lambda (output) (write-json receipt output) (newline output))
        #:exists
        'truncate/replace)
       (file-or-directory-permissions receipt-temporary 420)
       (for
        ((name cases))
        (rename-file-or-directory (build-path staging name) (build-path destination name) #f)
        (set! committed (cons name committed)))
       (rename-file-or-directory receipt-temporary receipt-path #f)
       (set! complete? #t)
       receipt)
     (lambda ()
       (unless complete?
         (for ((name committed)) (delete-directory/files (build-path destination name))))
       (when (directory-exists? staging) (delete-directory/files staging)))))))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (with-handlers
  ((exn:fail?
    (lambda (failure)
      (write-json (hasheq 'pass #f 'message (exn-message failure)))
      (newline)
      (exit 1))))
  (cond
   ((and (= (length arguments) 2) (equal? (first arguments) "--inventory"))
    (write-json (hasheq 'pass #t 'inventory (corpus-inventory (second arguments))))
    (newline))
   ((and (= (length arguments) 6)
         (equal? (first arguments) "--source")
         (equal? (third arguments) "--sha256")
         (equal? (fifth arguments) "--provenance"))
    (write-json
     (hasheq
      'pass
      #t
      'import
      (import-corpus (second arguments) (fourth arguments) (sixth arguments))))
    (newline))
   (else
    (displayln
     "usage: racket scripts/import-corpus.rkt --inventory DIRECTORY | --source DIRECTORY --sha256 HASH --provenance HASH"
     (current-error-port))
    (exit 2)))))
