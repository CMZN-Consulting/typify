#lang racket/base

(require file/sha1 json racket/file racket/list racket/path racket/port racket/string)

(provide check-licenses glob->regexp parse-general-category merge-intervals unicode-tables)

(define skipped-names '(".git" "compiled" "node_modules" ".lake"))

(define hex-digest #px"^[0-9a-f]{64}$")

(define (sha256-file path) (bytes->hex-string (sha256-bytes (file->bytes path))))

(define (glob->regexp glob)
  (define out (open-output-string))
  (write-string "^" out)
  (let loop ((index 0))
    (when (< index (string-length glob))
      (define char (string-ref glob index))
      (cond
       ((and (char=? char #\*)
             (< (add1 index) (string-length glob))
             (char=? (string-ref glob (add1 index)) #\*))
        (write-string ".*" out)
        (loop (+ index 2)))
       ((char=? char #\*) (write-string "[^/]*" out) (loop (add1 index)))
       ((char=? char #\?) (write-string "[^/]" out) (loop (add1 index)))
       (else (write-string (regexp-quote (string char)) out) (loop (add1 index))))))
  (write-string "$" out)
  (pregexp (get-output-string out)))

(define (relative root path)
  (string-replace (path->string (find-relative-path root path)) "\\" "/"))

(define (repository-files root)
  (sort
   (for/list
    ((path
      (in-directory
       root
       (lambda (path)
         (and (not (link-exists? path))
              (not (member (path->string (file-name-from-path path)) skipped-names))))))
     #:unless
     (or (directory-exists? path) (equal? path (build-path root ".git"))))
    (relative root path))
   string<?))

(define (merge-intervals ranges)
  (define sorted (sort ranges < #:key car))
  (reverse
   (for/fold
    ((out '()))
    ((range (in-list sorted)))
    (cond
     ((and (pair? out) (<= (car range) (add1 (cdar out))))
      (cons (cons (caar out) (max (cdar out) (cdr range))) (cdr out)))
     (else (cons range out))))))

(define (parse-general-category text)
  (define table (make-hash))
  (for
   ((line (in-list (string-split text "\n" #:trim? #f))))
   (define match
     (regexp-match #px"^([0-9A-F]{4,6})(?:\\.\\.([0-9A-F]{4,6}))?\\s*;\\s*([A-Z][a-z])\\s" line))
   (when match
     (define low (string->number (cadr match) 16))
     (define high (if (caddr match) (string->number (caddr match) 16) low))
     (hash-update! table (cadddr match) (lambda (old) (cons (cons low high) old)) '())))
  table)

(define (category-union table names)
  (merge-intervals (append* (for/list ((name (in-list names))) (hash-ref table name '())))))

(define surrogates '(55296 . 57343))

(define (unicode-tables table)
  (hasheq
   'letters
   (category-union table '("Lu" "Ll" "Lt" "Lm" "Lo"))
   'unsafe
   (category-union table '("Cc" "Cf" "Cs" "Zl" "Zp" "Zs"))
   'racket-unsafe
   (category-union table '("Cc" "Cf" "Zl" "Zp" "Zs"))))

(define (read-definition path name)
  (define text (file->string path))
  (define header (regexp-match #px"^#lang[^\n]*\n" text))
  (define input (open-input-string (substring text (if header (string-length (car header)) 0))))
  (parameterize
   ((read-accept-reader #f) (read-accept-lang #f))
   (let loop ()
     (define form (read input))
     (cond
      ((eof-object? form) #f)
      ((and (pair? form) (eq? (car form) 'define) (pair? (cdr form)) (eq? (cadr form) name))
       (define value (caddr form))
       (if (and (pair? value) (eq? (car value) 'quote)) (cadr value) value))
      (else (loop))))))

(define (read-python-intervals path)
  (define text (file->string path))
  (define start (regexp-match-positions #px"UNSAFE = \\(" text))
  (and start
       (let* ((from (cdar start))
              (end (regexp-match-positions #px"\n\\)" text from))
              (block (substring text from (and end (caar end)))))
         (for/list
          ((match (in-list (regexp-match* #px"\\((\\d+), (\\d+)\\)" block #:match-select values))))
          (cons (string->number (cadr match)) (string->number (caddr match)))))))

(define (valid-url? text) (and (string? text) (regexp-match? #px"^https://[^\\s]+$" text)))

(define (check-licenses
         root-given
         inventory
         #:release?
         (release? #f)
         #:unicode-source
         (unicode-source #f)
         #:archive
         (archive #f)
         #:native-receipt
         (native-receipt #f))
  (define root (simplify-path (path->complete-path root-given)))
  (define issues '())
  (define (issue! file kind (detail ""))
    (set! issues (cons (hasheq 'file file 'kind (symbol->string kind) 'detail detail) issues)))
  (define licenses (hash-ref inventory 'licenses (hasheq)))
  (define groups (hash-ref inventory 'groups '()))
  (define data (hash-ref inventory 'data '()))
  (define embedded (hash-ref inventory 'embedded '()))
  (define dependencies (hash-ref inventory 'dependencies '()))
  (define license-name (hash-ref inventory 'license-file "LICENSE"))
  (define license-path (build-path root license-name))
  (define license-text (if (file-exists? license-path) (file->string license-path) #f))
  (unless license-text (issue! license-name 'license-file-missing))
  (define (needs-notice! owner license)
    (define record (hash-ref licenses (string->symbol (format "~a" license)) #f))
    (cond
     ((not record) (issue! owner 'license-unknown (format "~a" license)))
     ((not license-text) (void))
     ((not (string-contains? license-text (hash-ref record 'notice "")))
      (issue! owner 'notice-missing (format "~a" license)))))
  (define (check-confirmation! owner entry)
    (when (equal? (hash-ref entry 'origin #f) "contributed")
      (define confirmed (hash-ref entry 'confirmed #f))
      (cond
       ((not (boolean? confirmed)) (issue! owner 'confirmation-malformed))
       ((and confirmed
             (not
              (and (string? (hash-ref entry 'confirmed-by #f))
                   (string? (hash-ref entry 'confirmed-on #f)))))
        (issue! owner 'confirmation-incomplete))
       ((and release? (not confirmed))
        (issue! owner 'unconfirmed (hash-ref entry 'authorship ""))))))
  (define (check-provenance! owner entry)
    (define upstream (hash-ref entry 'upstream #f))
    (cond
     ((not (hash? upstream)) (issue! owner 'provenance-missing))
     ((not (valid-url? (hash-ref upstream 'url #f))) (issue! owner 'provenance-incomplete "url"))
     ((not
       (and (string? (hash-ref upstream 'sha256 #f))
            (regexp-match? hex-digest (hash-ref upstream 'sha256))))
      (issue! owner 'provenance-incomplete "sha256"))
     ((not (non-empty-string? (format "~a" (hash-ref upstream 'version ""))))
      (issue! owner 'provenance-incomplete "version"))))
  (define group-patterns
    (for/list
     ((group (in-list groups)))
     (cons group (map glob->regexp (hash-ref group 'match '())))))
  (define data-by-path (make-hash))
  (for
   ((entry (in-list data)))
   (define path (hash-ref entry 'path #f))
   (cond
    ((not (string? path)) (issue! "<inventory>" 'entry-malformed))
    ((hash-has-key? data-by-path path) (issue! path 'duplicate-entry))
    (else (hash-set! data-by-path path entry))))
  (for
   ((group (in-list groups)))
   (define id (format "group:~a" (hash-ref group 'id "?")))
   (unless (member (hash-ref group 'origin #f) '("original" "contributed" "notice"))
     (issue! id 'group-origin (format "~a" (hash-ref group 'origin #f))))
   (needs-notice! id (hash-ref group 'license #f))
   (check-confirmation! id group))
  (for
   ((entry (in-hash-values data-by-path)))
   (define path (hash-ref entry 'path))
   (define origin (hash-ref entry 'origin #f))
   (unless (member
            origin
            '("original" "contributed" "third-party" "derived" "generated" "lockfile"))
     (issue! path 'entry-origin (format "~a" origin)))
   (needs-notice! path (hash-ref entry 'license #f))
   (when (member origin '("third-party" "derived")) (check-provenance! path entry))
   (check-confirmation! path entry)
   (define file (build-path root path))
   (cond
    ((not (file-exists? file)) (issue! path 'data-missing))
    ((not (equal? (sha256-file file) (hash-ref entry 'sha256 #f))) (issue! path 'data-changed))))
  (define files (repository-files root))
  (for
   ((name (in-list files)))
   (define by-data (hash-ref data-by-path name #f))
   (define by-groups
     (for/list
      ((pair (in-list group-patterns))
       #:when
       (for/or ((pattern (cdr pair))) (regexp-match? pattern name)))
      (car pair)))
   (cond
    ((and by-data (pair? by-groups)) (issue! name 'ambiguous "listed as data and by a group"))
    ((and (not by-data) (null? by-groups)) (issue! name 'unlisted))
    ((> (length by-groups) 1) (issue! name 'ambiguous "matches several groups"))))
  (for
   ((entry (in-list embedded)))
   (define id (format "embedded:~a" (hash-ref entry 'id "?")))
   (check-provenance! id entry)
   (needs-notice! id (hash-ref entry 'license #f))
   (for
    ((name (in-list (hash-ref entry 'files '()))))
    (define file (build-path root name))
    (cond
     ((not (file-exists? file)) (issue! name 'embedded-missing id))
     (else
      (define text (file->string file))
      (define upstream (hash-ref entry 'upstream (hasheq)))
      (unless (string-contains? text (hash-ref upstream 'sha256 "-")) (issue! name 'pin-missing id))
      (unless (string-contains? text (hash-ref upstream 'url "-"))
        (issue! name 'source-missing id)))))
   (when (and unicode-source (equal? (hash-ref entry 'id #f) "unicode-general-category"))
     (define upstream (hash-ref entry 'upstream))
     (cond
      ((not (equal? (sha256-file unicode-source) (hash-ref upstream 'sha256)))
       (issue! (path->string unicode-source) 'upstream-hash id))
      (else
       (define tables (unicode-tables (parse-general-category (file->string unicode-source))))
       (define rkt (hash-ref entry 'racket-table-file "src/classes.rkt"))
       (define py (hash-ref entry 'python-table-file "test/facts-path.py"))
       (define (compare! name got want) (unless (equal? got want) (issue! name 'table-differs id)))
       (when (file-exists? (build-path root rkt))
         (compare!
          rkt
          (read-definition (build-path root rkt) 'unicode-letter-intervals)
          (hash-ref tables 'letters))
         (compare!
          rkt
          (read-definition (build-path root rkt) 'unicode-unsafe-intervals)
          (hash-ref tables 'racket-unsafe)))
       (when (file-exists? (build-path root py))
         (compare! py (read-python-intervals (build-path root py)) (hash-ref tables 'unsafe)))))))
  (for
   ((entry (in-list (hash-ref inventory 'runtime-notices '()))))
   (define id (format "runtime:~a" (hash-ref entry 'component "?")))
   (check-provenance! id entry)
   (needs-notice! id (hash-ref entry 'license #f)))
  (define native (hash-ref inventory 'native-content '()))
  (for
   ((row (in-list native)))
   (define id (format "native:~a:~a" (hash-ref row 'platform "?") (hash-ref row 'name "?")))
   (unless (member (hash-ref row 'kind #f) '("shipped" "embedded")) (issue! id 'native-kind))
   (unless (regexp-match? hex-digest (format "~a" (hash-ref row 'sha256 "")))
     (issue! id 'native-sha256))
   (check-provenance! id row)
   (needs-notice! id (hash-ref row 'license #f)))
  (when native-receipt
    (define receipt (call-with-input-file native-receipt read-json))
    (define asset (hash-ref (hash-ref receipt 'asset (hasheq)) 'name ""))
    (define named (regexp-match #px"^typify-(.+)[.](?:tar[.]gz|zip)$" (format "~a" asset)))
    (define platform (and named (cadr named)))
    (cond
     ((not platform) (issue! (format "~a" asset) 'native-receipt-asset))
     (else
      (define rows (filter (lambda (row) (equal? (hash-ref row 'platform #f) platform)) native))
      (define runtime-files (hash-ref inventory 'native-runtime-files '()))
      (define seen (make-hash))
      (define (account! kind name sha)
        (define row
          (for/first
           ((row (in-list rows))
            #:when
            (and (equal? (hash-ref row 'kind #f) kind)
                 (equal? (hash-ref row (if (equal? kind "shipped") 'path 'name) #f) name)))
           row))
        (cond
         ((not row) (issue! (format "~a:~a" platform name) 'native-unlisted))
         (else
          (hash-set! seen row #t)
          (unless (equal? (hash-ref row 'sha256 #f) sha)
            (issue! (format "~a:~a" platform name) 'native-changed)))))
      (for
       ((item (in-list (hash-ref receipt 'shipped '()))))
       (when (and
              (equal? (hash-ref item 'kind #f) "file")
              (string? (hash-ref item 'format #f))
              (not (member (hash-ref item 'path #f) runtime-files)))
         (account! "shipped" (hash-ref item 'path #f) (hash-ref item 'sha256 #f))))
      (for
       ((item (in-list (hash-ref receipt 'embedded '()))))
       (account! "embedded" (hash-ref item 'exportName #f) (hash-ref item 'sha256 #f)))
      (for
       ((row (in-list rows)))
       (unless (hash-ref seen row #f)
         (issue! (format "~a:~a" platform (hash-ref row 'name "?")) 'native-missing))))))
  (define lock-path (build-path root (hash-ref inventory 'lockfile "mcp/bun.lock")))
  (when (pair? dependencies)
    (define allowed (hash-ref inventory 'allowed-dependency-licenses '()))
    (define listed (make-hash))
    (for
     ((dep (in-list dependencies)))
     (define key (format "~a@~a" (hash-ref dep 'name "?") (hash-ref dep 'version "?")))
     (hash-set! listed key dep)
     (unless (member (hash-ref dep 'license #f) allowed)
       (issue! key 'dependency-license (format "~a" (hash-ref dep 'license #f)))))
    (cond
     ((not (file-exists? lock-path)) (issue! (path->string lock-path) 'lockfile-missing))
     (else
      (define locked
        (for/hash
         ((match
           (in-list
            (regexp-match*
             #px"\\[\"([^\"]+@[^\"]+)\", \"[^\"]*\", \\{[^\n]*?\\}, \"([^\"]+)\"\\]"
             (file->string lock-path)
             #:match-select
             values))))
         (values (cadr match) (caddr match))))
      (for
       ((key (in-hash-keys locked)))
       (define dep (hash-ref listed key #f))
       (cond
        ((not dep) (issue! key 'dependency-unlisted))
        ((not (equal? (hash-ref dep 'integrity #f) (hash-ref locked key)))
         (issue! key 'dependency-integrity))))
      (for
       ((key (in-hash-keys listed)))
       (unless (hash-has-key? locked key) (issue! key 'dependency-stale))))))
  (when archive
    (define directory (simplify-path (path->complete-path archive)))
    (define packed (build-path directory "typify" license-name))
    (define manifest (build-path directory "typify" "MANIFEST.txt"))
    (cond
     ((not (file-exists? packed)) (issue! license-name 'archive-license-missing))
     ((not (and license-text (equal? (file->string packed) license-text)))
      (issue! license-name 'archive-license-differs)))
    (define listed
      (if (file-exists? manifest)
        (for/list ((line (in-list (file->lines manifest)))) (string-split line "  " #:trim? #f))
        '()))
    (cond
     ((not (file-exists? manifest)) (issue! "MANIFEST.txt" 'archive-manifest-missing))
     ((and (file-exists? packed)
           (not
            (member
             (string-append "typify/" license-name)
             (map (lambda (row) (and (= (length row) 3) (caddr row))) listed))))
      (issue! license-name 'archive-license-unlisted))
     ((and (file-exists? packed)
           (not
            (for/or
             ((row (in-list listed)))
             (and (= (length row) 3)
                  (equal? (caddr row) (string-append "typify/" license-name))
                  (equal? (car row) (sha256-file packed))))))
      (issue! license-name 'archive-license-manifest-hash))))
  (hasheq 'pass (null? issues) 'files (length files) 'release release? 'issues (reverse issues)))

(module+
 main
 (define release? #f)
 (define unicode-source #f)
 (define archive #f)
 (define native-receipt-path #f)
 (define root (current-directory))
 (define inventory-path #f)
 (let loop ((remaining (vector->list (current-command-line-arguments))))
   (cond
    ((null? remaining) (void))
    ((equal? (car remaining) "--release") (set! release? #t) (loop (cdr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--unicode"))
     (set! unicode-source (cadr remaining))
     (loop (cddr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--native-receipt"))
     (set! native-receipt-path (cadr remaining))
     (loop (cddr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--archive"))
     (set! archive (cadr remaining))
     (loop (cddr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--root"))
     (set! root (cadr remaining))
     (loop (cddr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--inventory"))
     (set! inventory-path (cadr remaining))
     (loop (cddr remaining)))
    (else
     (displayln
      "usage: racket scripts/license-check.rkt [--root DIR] [--inventory FILE] [--release] [--unicode FILE] [--archive DIR] [--native-receipt FILE]"
      (current-error-port))
     (exit 2))))
 (define inventory
   (with-handlers
    ((exn:fail? (lambda (_) (displayln "unreadable inventory" (current-error-port)) (exit 2))))
    (call-with-input-file
     (or inventory-path (build-path root "scripts" "license-inventory.json"))
     read-json)))
 (define result
   (check-licenses
    root
    inventory
    #:release?
    release?
    #:unicode-source
    (and unicode-source (string->path unicode-source))
    #:archive
    archive
    #:native-receipt
    native-receipt-path))
 (write-json result)
 (newline)
 (exit (if (hash-ref result 'pass) 0 1)))
