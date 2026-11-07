#lang racket/base

(require racket/file racket/list racket/path "../scripts/import-corpus.rkt")

(provide run-tests)

(define expected-tree-sha256 "f3248ffe071d97c30173e48893389e802b0416e47636abe06383ff17c4f46dcc")

(define provenance (make-string 64 #\a))

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
         (delete-file directory-link))))
   (lambda () (delete-directory/files temporary)))
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
