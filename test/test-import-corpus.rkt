#lang racket/base

(require file/sha1 racket/file racket/list racket/path "../scripts/import-corpus.rkt")

(provide run-tests)

(define expected-tree-sha256 "f3248ffe071d97c30173e48893389e802b0416e47636abe06383ff17c4f46dcc")

(define provenance (make-string 64 #\a))

(define manifest-bytes #"# Corpus\r\nFrozen author inventory.\r\n")

(define expected-manifest-sha256 "63b03d52d04a55f030a1cf5a0dd72a9467a8fe5190be2e6b32cfb7109243aca4")

(define expected-manifest-tree-sha256
  "45f8d55fa82da7047bedf990a631db074892feaf0e5e17208b513a7283f665b2")

(define fixed-files
  (list
   (cons "declaration.typify" #"#lang typify\r\ndeclare : Example\r\nend!\r\n")
   (cons "input.typify" (string->bytes/utf-8 "label : café\r\n"))
   (cons "expected.json" #"opaque expected bytes\r\n")
   (cons "facts.json" #"{\"facts\":[]}\n")))

(define expected-file-hashes
  '(("case-a/declaration.typify" "fecdc2d3d48fe6ada0333a25ad84c31e918c9b71c40c2a2d4455f0b9ab9a48a8")
    ("case-a/expected.json" "2d88d906020ad80595bcf9ba1cd93c6fd35db4795ba3afc205c6e4da6f4ad263")
    ("case-a/facts.json" "ea8f8aa764cc08b3a61c9716dceb60e995c3b27eae17e4d1d9fefa169f53029a")
    ("case-a/input.typify" "4faf596738f6b3d9e092e2fbc81501664d93400d5707a0597df35e5037677b2c")))

(define (write-bytes-file path bytes)
  (call-with-output-file
   path
   (lambda (output) (write-bytes bytes output))
   #:exists
   'truncate/replace))

(define (checkout temporary name)
  (define root (build-path temporary name))
  (make-directory* (build-path root "src"))
  (make-directory* (build-path root "test"))
  (write-bytes-file (build-path root "src" "info.rkt") #"#lang info\n")
  root)

(define (rejects? procedure) (with-handlers ((exn:fail? (lambda (_) #t))) (procedure) #f))

(define (ask-custody-controls check)
  (define temporary (make-temporary-file "ask-custody~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define source (build-path temporary "source"))
     (make-directory* (build-path source "declarations"))
     (make-directory* (build-path source "reference"))
     (define files
       (list
        (cons "README.md" #"Frozen reference language/version and generation command.\r\n")
        (cons "gate.json" #"{\"declarations\":[]}\r\n")
        (cons "declarations/gate.typify" #"#lang typify\r\ndeclare : Gate\r\nend!\r\n")
        (cons
         "reference/never-run.rkt"
         #"#lang racket/base\n; frozen source data\n(error 'reference \"must not execute\")\n")))
     (for ((item files)) (write-bytes-file (build-path source (car item)) (cdr item)))
     (define inventory (ask-gate-inventory source))
     (define expected (hash-ref inventory 'tree-sha256))
     (define root (checkout temporary "checkout"))
     (define destination (build-path root "test" "ask-gate"))
     (define receipt (import-ask-gate source expected provenance #:root root))
     (define receipt-path (build-path destination "import.json"))
     (define receipt-bytes (file->bytes receipt-path))
     (define custody
       (hash
        'archive-sha256
        provenance
        'tree-sha256
        expected
        'receipt-sha256
        (bytes->hex-string (sha256-bytes receipt-bytes))))
     (check
      "ask-gate-envelope-and-reference-imported-verbatim"
      (lambda ()
        (and (equal? (hash-ref receipt 'schema) "typify-ask-gate-import-v1")
             (equal? (hash-ref receipt 'layout) "test/ask-gate/")
             (eq? (hash-ref receipt 'verbatim) #t)
             (for/and
              ((item files))
              (equal? (file->bytes (build-path destination (car item))) (cdr item)))))
      #t)
     (check
      "ask-gate-custody-pins-verify"
      (lambda () (verify-ask-gate destination custody))
      receipt)
     (check
      "ask-gate-import-is-idempotent"
      (lambda ()
        (and (equal? (import-ask-gate source expected provenance #:root root) receipt)
             (equal? (file->bytes receipt-path) receipt-bytes)))
      #t)
     (check
      "ask-gate-requires-explicit-handoff-pins"
      (lambda () (rejects? (lambda () (verify-ask-gate destination #f))))
      #t)
     (define reference (build-path destination "reference" "never-run.rkt"))
     (write-bytes-file reference #"changed reference\n")
     (check
      "ask-gate-changed-reference-refused"
      (lambda () (rejects? (lambda () (verify-ask-gate destination custody))))
      #t)
     (check
      "ask-gate-refuses-overwriting-changed-destination"
      (lambda ()
        (and (rejects? (lambda () (import-ask-gate source expected provenance #:root root)))
             (equal? (file->bytes reference) #"changed reference\n")))
      #t)
     (write-bytes-file reference (cdr (assoc "reference/never-run.rkt" files)))
     (write-bytes-file (build-path destination "reference" "extra.rkt") #"unlisted\n")
     (check
      "ask-gate-unlisted-reference-refused"
      (lambda () (rejects? (lambda () (verify-ask-gate destination custody))))
      #t)
     (delete-file (build-path destination "reference" "extra.rkt"))
     (write-bytes-file (build-path destination "README.md") #"changed metadata\n")
     (check
      "ask-gate-changed-readme-refused"
      (lambda () (rejects? (lambda () (verify-ask-gate destination custody))))
      #t)
     (define wrong-root (checkout temporary "wrong-hash"))
     (check
      "ask-gate-wrong-source-hash-refused-before-copy"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-ask-gate source (make-string 64 #\0) provenance #:root wrong-root)))
             (not (directory-exists? (build-path wrong-root "test" "ask-gate")))))
      #t)
     (define entry
       (hash
        'id
        "case-a"
        'reason
        "awaiting a pinned definition"
        'sha256
        provenance
        'manifest-entry
        (string-append "case-a deferred: awaiting a pinned definition; sha256 " provenance)))
     (define manifest (hash-ref entry 'manifest-entry))
     (check
      "empty-deferral-list-keeps-all-cases-active"
      (lambda () (validate-deferred '() '("case-a") ""))
      '())
     (check
      "explicit-deferral-is-bound-to-an-exact-manifest-entry"
      (lambda () (validate-deferred (list entry) '("case-a") manifest))
      (list entry))
     (check
      "deferral-without-manifest-authority-refused"
      (lambda () (rejects? (lambda () (validate-deferred (list entry) '("case-a") ""))))
      #t)
     (check
      "deferral-of-unlisted-case-refused"
      (lambda () (rejects? (lambda () (validate-deferred (list entry) '("case-b") manifest))))
      #t)
     (check
      "duplicate-deferral-refused"
      (lambda () (rejects? (lambda () (validate-deferred (list entry entry) '("case-a") manifest))))
      #t)
     (check
      "deferral-without-content-digest-refused"
      (lambda ()
        (rejects?
         (lambda () (validate-deferred (list (hash-remove entry 'sha256)) '("case-a") manifest))))
      #t))
   (lambda () (delete-directory/files temporary))))

(define (run-tests)
  (define count 0)
  (define passed 0)
  (define failures '())
  (define (check name procedure expected)
    (set! count (add1 count))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! failures (cons (hasheq 'name name 'error (exn-message failure)) failures)))))
     (define actual (procedure))
     (if (equal? actual expected)
       (set! passed (add1 passed))
       (set! failures (cons (hasheq 'name name 'mismatch #t) failures)))))
  (define temporary (make-temporary-file "corpus-custody~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define source (build-path temporary "source"))
     (define source-case (build-path source "case-a"))
     (make-directory* (build-path source-case "root" "empty"))
     (for ((item fixed-files)) (write-bytes-file (build-path source-case (car item)) (cdr item)))
     (define root (checkout temporary "checkout"))
     (define destination (build-path root "test" "corpus"))
     (define destination-case (build-path destination "case-a"))
     (define receipt-path (build-path destination "import.json"))
     (define receipt #f)
     (define receipt-bytes #f)
     (define (original-files? directory)
       (for/and
        ((item fixed-files))
        (equal? (file->bytes (build-path directory (car item))) (cdr item))))
     (check
      "independent-known-tree-hash"
      (lambda () (hash-ref (corpus-inventory source) 'tree-sha256))
      expected-tree-sha256)
     (check
      "independent-file-hashes-and-directory-shape"
      (lambda ()
        (define inventory (corpus-inventory source))
        (list
         (for/list
          ((item (hash-ref inventory 'files)))
          (list (hash-ref item 'path) (hash-ref item 'sha256)))
         (hash-ref inventory 'directories)
         (hash-ref inventory 'bytes)))
      (list expected-file-hashes '("case-a" "case-a/root" "case-a/root/empty") 90))
     (check
      "copy-and-custody-receipt"
      (lambda ()
        (set! receipt (import-corpus source expected-tree-sha256 provenance #:root root))
        (set! receipt-bytes (file->bytes receipt-path))
        (list
         (hash-ref receipt 'schema)
         (hash-ref receipt 'tree-sha256)
         (hash-ref receipt 'verbatim)
         (hash-ref receipt 'cases)
         (hash-ref (hash-ref receipt 'provenance) 'sha256)))
      (list "typify-corpus-import-v2" expected-tree-sha256 #t '("case-a") provenance))
     (check
      "verbatim-crlf-unicode-opaque-expectation-and-facts"
      (lambda () (and (original-files? source-case) (original-files? destination-case)))
      #t)
     (check
      "empty-directory-preserved"
      (lambda () (directory-exists? (build-path destination-case "root" "empty")))
      #t)
     (check
      "idempotent-import-does-not-rewrite-receipt"
      (lambda ()
        (and (equal? (import-corpus source expected-tree-sha256 provenance #:root root) receipt)
             (equal? (file->bytes receipt-path) receipt-bytes)
             (original-files? destination-case)))
      #t)
     (write-bytes-file (build-path source-case "input.typify") #"changed source\r\n")
     (check
      "changed-source-with-stale-hash-refused"
      (lambda ()
        (and (rejects?
              (lambda () (import-corpus source expected-tree-sha256 provenance #:root root)))
             (original-files? destination-case)
             (equal? (file->bytes receipt-path) receipt-bytes)))
      #t)
     (check
      "new-source-hash-cannot-overwrite-an-existing-import"
      (lambda ()
        (define changed (hash-ref (corpus-inventory source) 'tree-sha256))
        (and (not (equal? changed expected-tree-sha256))
             (rejects? (lambda () (import-corpus source changed provenance #:root root)))
             (original-files? destination-case)))
      #t)
     (write-bytes-file
      (build-path source-case "input.typify")
      (cdr (assoc "input.typify" fixed-files)))
     (write-bytes-file (build-path destination-case "input.typify") #"changed destination\n")
     (check
      "changed-destination-refused-without-repair"
      (lambda ()
        (and (rejects?
              (lambda () (import-corpus source expected-tree-sha256 provenance #:root root)))
             (equal?
              (file->bytes (build-path destination-case "input.typify"))
              #"changed destination\n")
             (equal? (file->bytes receipt-path) receipt-bytes)))
      #t)
     (define collision-root (checkout temporary "collision"))
     (define collision-case (build-path collision-root "test" "corpus" "case-a"))
     (make-directory* collision-case)
     (write-bytes-file (build-path collision-case "keep.input") #"keep\r\n")
     (check
      "preexisting-case-refused-without-overwrite"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus source expected-tree-sha256 provenance #:root collision-root)))
             (equal? (file->bytes (build-path collision-case "keep.input")) #"keep\r\n")
             (not (file-exists? (build-path collision-case "declaration.typify")))
             (not (file-exists? (build-path collision-root "test" "corpus" "import.json")))))
      #t)
     (define wrong-hash-root (checkout temporary "wrong-hash"))
     (check
      "failed-hash-leaves-no-fixture"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus source (make-string 64 #\0) provenance #:root wrong-hash-root)))
             (not (directory-exists? (build-path wrong-hash-root "test" "corpus")))))
      #t)
     (define source-manifest (build-path source "MANIFEST.md"))
     (write-bytes-file source-manifest manifest-bytes)
     (check
      "optional-manifest-in-inventory-and-independent-tree-hash"
      (lambda ()
        (define inventory (corpus-inventory source))
        (list
         (hash-ref inventory 'cases)
         (hash-ref inventory 'metadata)
         (car (hash-ref inventory 'files))
         (hash-ref inventory 'bytes)
         (hash-ref inventory 'tree-sha256)))
      (list
       '("case-a")
       '("MANIFEST.md")
       (hasheq 'path "MANIFEST.md" 'bytes 36 'sha256 expected-manifest-sha256)
       126
       expected-manifest-tree-sha256))
     (define manifest-root (checkout temporary "manifest-checkout"))
     (define manifest-destination (build-path manifest-root "test" "corpus"))
     (define destination-manifest (build-path manifest-destination "MANIFEST.md"))
     (define manifest-receipt-path (build-path manifest-destination "import.json"))
     (define manifest-receipt #f)
     (define manifest-receipt-bytes #f)
     (check
      "manifest-copied-verbatim-with-compatible-v2-receipt"
      (lambda ()
        (set! manifest-receipt
          (import-corpus source expected-manifest-tree-sha256 provenance #:root manifest-root))
        (set! manifest-receipt-bytes (file->bytes manifest-receipt-path))
        (list
         (hash-ref manifest-receipt 'schema)
         (hash-ref manifest-receipt 'cases)
         (hash-ref manifest-receipt 'metadata)
         (file->bytes destination-manifest)
         (original-files? (build-path manifest-destination "case-a"))))
      (list "typify-corpus-import-v2" '("case-a") '("MANIFEST.md") manifest-bytes #t))
     (check
      "manifest-import-idempotency"
      (lambda ()
        (and (equal?
              (import-corpus source expected-manifest-tree-sha256 provenance #:root manifest-root)
              manifest-receipt)
             (equal? (file->bytes manifest-receipt-path) manifest-receipt-bytes)
             (equal? (file->bytes destination-manifest) manifest-bytes)))
      #t)
     (write-bytes-file source-manifest #"changed manifest\n")
     (check
      "changed-source-manifest-refused-without-overwrite"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus
                 source
                 expected-manifest-tree-sha256
                 provenance
                 #:root
                 manifest-root)))
             (equal? (file->bytes destination-manifest) manifest-bytes)
             (equal? (file->bytes manifest-receipt-path) manifest-receipt-bytes)))
      #t)
     (delete-file source-manifest)
     (check
      "removed-source-manifest-refused-without-overwrite"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus
                 source
                 expected-manifest-tree-sha256
                 provenance
                 #:root
                 manifest-root)))
             (equal? (file->bytes destination-manifest) manifest-bytes)))
      #t)
     (write-bytes-file source-manifest manifest-bytes)
     (write-bytes-file destination-manifest #"changed destination manifest\n")
     (check
      "changed-destination-manifest-refused-without-repair"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus
                 source
                 expected-manifest-tree-sha256
                 provenance
                 #:root
                 manifest-root)))
             (equal? (file->bytes destination-manifest) #"changed destination manifest\n")
             (equal? (file->bytes manifest-receipt-path) manifest-receipt-bytes)))
      #t)
     (define manifest-collision-root (checkout temporary "manifest-collision"))
     (define manifest-collision-destination (build-path manifest-collision-root "test" "corpus"))
     (make-directory* manifest-collision-destination)
     (write-bytes-file (build-path manifest-collision-destination "MANIFEST.md") #"keep manifest\n")
     (check
      "preexisting-manifest-refused-before-case-copy"
      (lambda ()
        (and (rejects?
              (lambda ()
                (import-corpus
                 source
                 expected-manifest-tree-sha256
                 provenance
                 #:root
                 manifest-collision-root)))
             (equal?
              (file->bytes (build-path manifest-collision-destination "MANIFEST.md"))
              #"keep manifest\n")
             (not (directory-exists? (build-path manifest-collision-destination "case-a")))
             (not (file-exists? (build-path manifest-collision-destination "import.json")))))
      #t)
     (define other-metadata (build-path source "README.md"))
     (write-bytes-file other-metadata #"not authorized metadata\n")
     (check
      "other-root-metadata-refused"
      (lambda () (rejects? (lambda () (corpus-inventory source))))
      #t)
     (delete-file other-metadata)
     (delete-file source-manifest)
     (make-directory source-manifest)
     (check
      "manifest-directory-refused"
      (lambda () (rejects? (lambda () (corpus-inventory source))))
      #t)
     (delete-directory source-manifest)
     (write-bytes-file source-manifest manifest-bytes)
     (define metadata-only (build-path temporary "metadata-only"))
     (make-directory metadata-only)
     (write-bytes-file (build-path metadata-only "MANIFEST.md") manifest-bytes)
     (check
      "manifest-is-not-a-case"
      (lambda () (rejects? (lambda () (corpus-inventory metadata-only))))
      #t)
     (unless (eq? (system-type 'os) 'windows)
       (let ((file-link (build-path source-case "linked.typify"))
             (directory-link (build-path source-case "linked-directory")))
         (make-file-or-directory-link (build-path source-case "input.typify") file-link)
         (check
          "file-symlink-refused-during-inventory"
          (lambda () (rejects? (lambda () (corpus-inventory source))))
          #t)
         (delete-file file-link)
         (make-file-or-directory-link
          (path->directory-path (build-path source-case "root"))
          directory-link)
         (check
          "directory-symlink-refused-during-inventory"
          (lambda () (rejects? (lambda () (corpus-inventory source))))
          #t)
         (delete-file directory-link)
         (delete-file source-manifest)
         (make-file-or-directory-link (build-path source-case "input.typify") source-manifest)
         (check
          "manifest-symlink-refused-during-inventory"
          (lambda () (rejects? (lambda () (corpus-inventory source))))
          #t)
         (delete-file source-manifest))))
   (lambda () (delete-directory/files temporary)))
  (ask-custody-controls check)
  (hasheq
   'suite
   "corpus-custody"
   'count
   count
   'pass
   passed
   'fail
   (- count passed)
   'details
   (reverse failures)
   'symlink-scope
   (if (eq? (system-type 'os) 'windows) "unmeasured-on-windows" "file-and-directory")
   'oracle
   "Fixed byte vectors and tree hash independently computed with Python hashlib; no reader verdict schema interpreted."))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail))
   (error 'corpus-custody "custody checks failed: ~s" result)))
