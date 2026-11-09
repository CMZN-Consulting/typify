#lang racket/base

(require file/sha1 json racket/file racket/list racket/path racket/port racket/string)

(provide import-corpus
         corpus-inventory
         import-ask-gate
         ask-gate-inventory
         verify-ask-gate
         current-ask-gate-custody
         validate-deferred)

(define current-ask-gate-custody (make-parameter #f))

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

(define (snapshot source cases (metadata '()))
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
  (for
   ((name metadata))
   (define path (build-path source name))
   (unless (and (equal? name "MANIFEST.md") (eq? (kind path) 'file))
     (error 'import-corpus "root metadata must be the regular file MANIFEST.md"))
   (walk path (list name)))
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
  (define inventory
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
  (if (null? metadata) inventory (hash-set inventory 'metadata metadata)))

(define (corpus-inventory source)
  (define directory (directory-source source))
  (define entries (map entry-name (ordered-entries directory)))
  (define metadata (filter (lambda (name) (equal? name "MANIFEST.md")) entries))
  (define cases (filter (lambda (name) (not (equal? name "MANIFEST.md"))) entries))
  (when (null? cases) (error 'import-corpus "an empty corpus cannot be imported"))
  (when (member "import.json" cases)
    (error 'import-corpus "a case name collides with the import receipt"))
  (snapshot directory cases metadata))

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

(define (validate-deferred entries cases manifest)
  (unless (and (list? entries) (string? manifest))
    (error 'import-corpus "deferrals require explicit entries and manifest text"))
  (define ids '())
  (for
   ((entry (in-list entries)))
   (unless (and
            (hash? entry)
            (equal? (sort (hash-keys entry) symbol<?) '(id manifest-entry reason sha256))
            (string? (hash-ref entry 'id))
            (member (hash-ref entry 'id) cases)
            (not (member (hash-ref entry 'id) ids))
            (string? (hash-ref entry 'reason))
            (positive? (string-length (hash-ref entry 'reason)))
            (digest? (hash-ref entry 'sha256))
            (string? (hash-ref entry 'manifest-entry))
            (positive? (string-length (hash-ref entry 'manifest-entry)))
            (string-contains? manifest (hash-ref entry 'manifest-entry))
            (for/and
             ((field '(id reason sha256)))
             (string-contains? (hash-ref entry 'manifest-entry) (hash-ref entry field)))
            (regexp-match? #px"(?i:deferred)" (hash-ref entry 'manifest-entry)))
     (error 'import-corpus "a deferral lacks an exact manifest entry, reason, or content SHA256"))
   (set! ids (cons (hash-ref entry 'id) ids)))
  entries)

(define (import-corpus
         source
         expected
         provenance
         #:root
         (given-root (current-directory))
         #:deferred
         (deferred '()))
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
  (define metadata (hash-ref inventory 'metadata '()))
  (define imported-entries (append cases metadata))
  (define receipt-path (build-path destination "import.json"))
  (define checked-deferred
    (validate-deferred
     deferred
     cases
     (if (member "MANIFEST.md" metadata) (file->string (build-path directory "MANIFEST.md")) "")))
  (define base-receipt
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
  (define receipt
    (if (null? checked-deferred) base-receipt (hash-set base-receipt 'deferred checked-deferred)))
  (cond
   ((present? receipt-path)
    (unless (and
             (eq? (kind receipt-path) 'file)
             (equal? (call-with-input-file receipt-path read-json) receipt)
             (equal? (snapshot destination cases metadata) inventory))
      (error 'import-corpus "existing import differs or is incomplete; no file was overwritten"))
    receipt)
   (else
    (for
     ((name imported-entries))
     (when (present? (build-path destination name))
       (error 'import-corpus "a destination import entry already exists; no file was overwritten")))
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
                (equal? (snapshot staging cases metadata) inventory))
         (error 'import-corpus "source changed during import or copied tree differs"))
       (define receipt-temporary (make-temporary-file "receipt~a.tmp" #f staging))
       (call-with-output-file
        receipt-temporary
        (lambda (output) (write-json receipt output) (newline output))
        #:exists
        'truncate/replace)
       (file-or-directory-permissions receipt-temporary 420)
       (for
        ((name imported-entries))
        (rename-file-or-directory (build-path staging name) (build-path destination name) #f)
        (set! committed (cons name committed)))
       (rename-file-or-directory receipt-temporary receipt-path #f)
       (set! complete? #t)
       receipt)
     (lambda ()
       (unless complete?
         (for ((name committed)) (delete-directory/files (build-path destination name))))
       (when (directory-exists? staging) (delete-directory/files staging)))))))

(define (ask-gate-inventory source #:receipt? (receipt? #f))
  (define directory (directory-source source))
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
         (error 'ask-gate-inventory "a frozen gate file exceeds the import size limit"))
       (set! total (+ total size))
       (when (> total (* 64 1024 1024))
         (error 'ask-gate-inventory "the frozen gate exceeds the total import size limit"))
       (set! files (cons (hasheq 'path relative 'bytes size 'sha256 (sha256-file path)) files)))))
  (define entries (map entry-name (ordered-entries directory)))
  (unless (equal?
           (sort (if receipt? (remove "import.json" entries) entries) string<?)
           '("README.md" "declarations" "gate.json" "reference"))
    (error
     'ask-gate-inventory
     "the gate envelope must contain README.md, declarations, gate.json and reference"))
  (unless (and
           (eq? (kind (build-path directory "README.md")) 'file)
           (eq? (kind (build-path directory "gate.json")) 'file)
           (eq? (kind (build-path directory "declarations")) 'directory)
           (eq? (kind (build-path directory "reference")) 'directory))
    (error 'ask-gate-inventory "the frozen gate envelope has incorrect entry kinds"))
  (for
   ((name '("README.md" "declarations" "gate.json" "reference")))
   (walk (build-path directory name) (list name)))
  (define ordered-files (sort files string<? #:key (lambda (item) (hash-ref item 'path))))
  (unless (and
           (ormap (lambda (item) (string-prefix? (hash-ref item 'path) "reference/")) ordered-files)
           (ormap
            (lambda (item) (string-prefix? (hash-ref item 'path) "declarations/"))
            ordered-files))
    (error 'ask-gate-inventory "the frozen gate requires declarations and reference source files"))
  (define ordered-directories (sort directories string<?))
  (define pairs
    (sort
     (append
      (for/list ((item ordered-files)) (list (hash-ref item 'path) (hash-ref item 'sha256)))
      (for/list ((name ordered-directories)) (list (string-append name "/") "directory")))
     string<?
     #:key
     car))
  (hasheq
   'files
   ordered-files
   'directories
   ordered-directories
   'bytes
   total
   'tree-sha256
   (bytes->hex-string
    (sha256-bytes
     (call-with-output-bytes (lambda (output) (write-json pairs output) (newline output)))))
   'tree-hash-format
   "sha256-sorted-json-pairs-newline-v1"))

(define (verify-ask-gate source (custody (current-ask-gate-custody)))
  (unless (and
           (hash? custody)
           (for/and
            ((field '(archive-sha256 tree-sha256 receipt-sha256)))
            (digest? (hash-ref custody field #f))))
    (error 'verify-ask-gate "the frozen gate requires explicit authorized custody SHA256 pins"))
  (define directory (directory-source source))
  (define receipt-path (build-path directory "import.json"))
  (unless (and
           (eq? (kind receipt-path) 'file)
           (equal? (sha256-file receipt-path) (hash-ref custody 'receipt-sha256)))
    (error 'verify-ask-gate "the gate import receipt differs from its authorized SHA256"))
  (define receipt (call-with-input-file receipt-path read-json))
  (define inventory (ask-gate-inventory directory #:receipt? #t))
  (unless (and
           (equal? (hash-ref receipt 'schema #f) "typify-ask-gate-import-v1")
           (eq? (hash-ref receipt 'verbatim #f) #t)
           (equal? (hash-ref receipt 'tree-sha256 #f) (hash-ref custody 'tree-sha256))
           (equal?
            (hash-ref (hash-ref receipt 'provenance (hash)) 'sha256 #f)
            (hash-ref custody 'archive-sha256))
           (for/and
            ((field '(files directories bytes tree-sha256 tree-hash-format)))
            (equal? (hash-ref receipt field #f) (hash-ref inventory field))))
    (error 'verify-ask-gate "the gate's frozen tree or receipt differs from the handoff"))
  receipt)

(define (import-ask-gate source expected provenance #:root (given-root (current-directory)))
  (unless (and (digest? expected) (digest? provenance))
    (error 'import-ask-gate "tree and handoff SHA256 values must be explicit lowercase hex"))
  (define root (simplify-path (path->complete-path given-root) #t))
  (define directory (directory-source source))
  (when (inside? root directory)
    (error 'import-ask-gate "an external gate cannot originate in the reader checkout"))
  (unless (and
           (file-exists? (build-path root "src" "info.rkt"))
           (directory-exists? (build-path root "test")))
    (error 'import-ask-gate "run from the source checkout"))
  (define inventory (ask-gate-inventory directory))
  (unless (equal? (hash-ref inventory 'tree-sha256) expected)
    (error 'import-ask-gate "the source gate differs from the authorized tree SHA256"))
  (define destination (build-path root "test" "ask-gate"))
  (when (or (link-exists? destination) (not (inside? root (simplify-path destination #t))))
    (error 'import-ask-gate "the gate destination resolves outside the checkout"))
  (define receipt
    (hash-set*
     inventory
     'schema
     "typify-ask-gate-import-v1"
     'layout
     "test/ask-gate/"
     'verbatim
     #t
     'provenance
     (hasheq 'kind "external-blind-handoff" 'sha256 provenance)))
  (cond
   ((present? destination)
    (unless (and
             (directory-exists? destination)
             (file-exists? (build-path destination "import.json"))
             (equal?
              (call-with-input-file (build-path destination "import.json") read-json)
              receipt)
             (equal? (ask-gate-inventory destination #:receipt? #t) inventory))
      (error 'import-ask-gate "the existing gate differs; no file was overwritten"))
    receipt)
   (else
    (define staging (make-temporary-file "ask-gate~a" 'directory (build-path root "test")))
    (dynamic-wind
     void
     (lambda ()
       (copy-snapshot directory staging inventory)
       (unless (and
                (equal? (ask-gate-inventory directory) inventory)
                (equal? (ask-gate-inventory staging) inventory))
         (error 'import-ask-gate "the gate source changed or its copy differs"))
       (call-with-output-file
        (build-path staging "import.json")
        (lambda (output) (write-json receipt output) (newline output)))
       (rename-file-or-directory staging destination #f)
       receipt)
     (lambda () (when (directory-exists? staging) (delete-directory/files staging)))))))

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
