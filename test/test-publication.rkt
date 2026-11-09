#lang racket/base

(require file/sha1
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../scripts/leak-check.rkt"
         "../scripts/format.rkt"
         "../scripts/import-corpus.rkt")

(provide run-tests)

(define-runtime-path repository-root "..")

(define case-directory "test/corpus/047-multislot-4-network-route")

(define fixture-hashes
  (list
   (cons "declarations.typify" "d436add2061182aa8547c1c0c93cc9bd9f46bc9050ef182fd52f5347b1b3d89f")
   (cons "program.typify" "79b453736b6d986f7fe4074b449f38e7f56d4d99c948ef8b0350b1a72bafdb53")
   (cons "expected.json" "27e699961d18ba9ca840f090df0faecce7e9ddfa6b18978da41ebc462364e546")))

(define expected-addresses
  (for/list
   ((parts '((10 0 0 0) (192 168 1 1) (172 16 0 0) (10 0 0 1))))
   (string-join (map number->string parts) ".")))

(define (write-bytes-file path bytes)
  (make-directory* (path-only path))
  (call-with-output-file
   path
   (lambda (output) (write-bytes bytes output))
   #:exists
   'truncate/replace))

(define (fixture-bytes name) (file->bytes (build-path repository-root case-directory name)))

(define (new-root temporary name #:fixtures? (fixtures? #t))
  (define root (build-path temporary name))
  (for
   ((directory '("src" "test" "scripts" ".github")))
   (make-directory* (build-path root directory)))
  (for ((file '("README.md" "LICENSE.md"))) (write-bytes-file (build-path root file) #""))
  (when fixtures?
    (for
     ((fixture fixture-hashes))
     (write-bytes-file
      (build-path root case-directory (car fixture))
      (fixture-bytes (car fixture)))))
  root)

(define (issue? report kind (file #f) (line #f))
  (for/or
   ((item (hash-ref report 'issues)))
   (and (equal? (hash-ref item 'kind) kind)
        (or (not file) (equal? (hash-ref item 'file) file))
        (or (not line) (= (hash-ref item 'line) line)))))

(define (run-tests)
  (define count 0)
  (define passed 0)
  (define failures '())
  (define (check name procedure expected)
    (set! count (add1 count))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! failures (cons (hash 'name name 'error (exn-message failure)) failures)))))
     (define actual (procedure))
     (if (equal? actual expected)
       (set! passed (add1 passed))
       (set! failures (cons (hash 'name name 'mismatch #t) failures)))))
  (define temporary (make-temporary-file "publication-custody~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define exact-root (new-root temporary "exact"))
     (check
      "exact-frozen-files-admitted-and-recorded"
      (lambda ()
        (define result (check-repository exact-root))
        (list
         (hash-ref result 'pass)
         (for/list
          ((item (hash-ref result 'admissions)))
          (list (hash-ref item 'file) (hash-ref item 'sha256) (hash-ref item 'allowed-addresses)))))
      (list
       #t
       (sort
        (for/list
         ((fixture fixture-hashes))
         (list (string-append case-directory "/" (car fixture)) (cdr fixture) expected-addresses))
        string<?
        #:key
        car)))
     (define changed-root (new-root temporary "changed"))
     (define program-path (string-append case-directory "/program.typify"))
     (write-bytes-file
      (build-path changed-root program-path)
      (bytes-append (fixture-bytes "program.typify") #"\n"))
     (check
      "changed-file-hash-removes-admission"
      (lambda ()
        (define result (check-repository changed-root))
        (and (not (hash-ref result 'pass))
             (issue? result "network-address" program-path)
             (= (length (hash-ref result 'admissions)) 2)
             (not
              (for/or
               ((item (hash-ref result 'admissions)))
               (equal? (hash-ref item 'file) program-path)))))
      #t)
     (define other-root (new-root temporary "other-path" #:fixtures? #f))
     (define other-path "test/corpus/another-case/program.typify")
     (write-bytes-file (build-path other-root other-path) (fixture-bytes "program.typify"))
     (check
      "identical-bytes-at-another-path-have-no-admission"
      (lambda ()
        (define result (check-repository other-root))
        (and (issue? result "network-address" other-path) (null? (hash-ref result 'admissions))))
      #t)
     (define extra-root (new-root temporary "extra" #:fixtures? #f))
     (define extra-path (build-path temporary "publication-input.txt"))
     (write-bytes-file extra-path (fixture-bytes "program.typify"))
     (check
      "extra-input-with-identical-bytes-has-no-admission"
      (lambda ()
        (define result (check-repository extra-root #:extra (list extra-path)))
        (and (issue? result "network-address" "<publication-input>")
             (null? (hash-ref result 'admissions))))
      #t)
     (check
      "admitted-file-is-still-scanned-for-denied-values"
      (lambda ()
        (define result (check-repository exact-root #:deny expected-addresses))
        (and (not (hash-ref result 'pass))
             (issue? result "private-vocabulary")
             (not (issue? result "network-address"))
             (= (length (hash-ref result 'admissions)) 3)))
      #t)
     (define secrets-root (new-root temporary "secrets"))
     (define secret-bytes
       (string->bytes/utf-8
        (string-append
         "\n-----BEGIN "
         "PRIVATE KEY-----\n"
         "ghp_"
         (make-string 24 #\A)
         "\n/"
         "home"
         "/example/private\n"
         "fixture."
         "pem\n")))
     (write-bytes-file
      (build-path secrets-root program-path)
      (bytes-append (fixture-bytes "program.typify") secret-bytes))
     (check
      "changed-admission-path-still-rejects-secret-signatures"
      (lambda ()
        (define result (check-repository secrets-root))
        (and (not (hash-ref result 'pass))
             (andmap
              (lambda (kind) (issue? result kind program-path))
              '("private-key" "credential" "home-path" "key-file"))
             (= (length (hash-ref result 'admissions)) 2)))
      #t)
     (define address-root (new-root temporary "other-address"))
     (define other-address (string-join '("10" "0" "0" "2") "."))
     (define original-program (bytes->string/utf-8 (fixture-bytes "program.typify")))
     (define added-line
       (+
        2
        (for/sum ((character (in-string original-program))) (if (char=? character #\newline) 1 0))))
     (write-bytes-file
      (build-path address-root program-path)
      (string->bytes/utf-8 (string-append original-program "\n" other-address "\n")))
     (check
      "another-address-at-an-admission-path-is-rejected"
      (lambda ()
        (define result (check-repository address-root))
        (and (issue? result "network-address" program-path added-line)
             (= (length (hash-ref result 'admissions)) 2)))
      #t)
     (define comment-root (new-root temporary "comment" #:fixtures? #f))
     (write-bytes-file
      (build-path comment-root "src" "sample.rkt")
      (string->bytes/utf-8 (string-append "#lang racket/base\n" ";" " forbidden\n")))
     (check
      "source-comment-check-retained"
      (lambda () (issue? (check-repository comment-root) "source-comment" "src/sample.rkt"))
      #t)
     (define gate-source (build-path temporary "gate-source"))
     (define reference-bytes
       #"#lang racket/base\n; frozen reference data\n(error 'reference \"must never run\")\n")
     (for
      ((item
        (list
         (cons "README.md" #"Reference language and generation command.\n")
         (cons "gate.json" #"{\"declarations\":[]}\n")
         (cons "declarations/gate.typify" #"#lang typify\ndeclare : Gate\nend!\n")
         (cons "reference/source.rkt" reference-bytes))))
      (write-bytes-file (build-path gate-source (car item)) (cdr item)))
     (define gate-root (new-root temporary "frozen-gate" #:fixtures? #f))
     (write-bytes-file (build-path gate-root "src" "info.rkt") #"#lang info\n")
     (define gate-tree (hash-ref (ask-gate-inventory gate-source) 'tree-sha256))
     (define archive (make-string 64 #\a))
     (import-ask-gate gate-source gate-tree archive #:root gate-root)
     (define gate-path (build-path gate-root "test" "ask-gate"))
     (define reference-path (build-path gate-path "reference" "source.rkt"))
     (define pins
       (hash
        'archive-sha256
        archive
        'tree-sha256
        gate-tree
        'receipt-sha256
        (call-with-input-file
         (build-path gate-path "import.json")
         (lambda (input) (bytes->hex-string (sha256-bytes input))))))
     (parameterize
      ((current-ask-gate-custody pins))
      (check
       "frozen-reference-comments-are-hash-bound-data"
       (lambda () (hash-ref (check-repository gate-root) 'pass))
       #t)
      (check
       "formatter-preserves-verified-reference-bytes"
       (lambda ()
         (and (hash-ref (format-repository gate-root #:write? #t) 'pass)
              (equal? (file->bytes reference-path) reference-bytes)))
       #t)
      (write-bytes-file reference-path (bytes-append reference-bytes #"  (void)\n"))
      (define changed-reference (file->bytes reference-path))
      (check
       "changed-reference-loses-comment-exception-and-custody"
       (lambda ()
         (define result (check-repository gate-root))
         (and (issue? result "frozen-custody")
              (issue? result "source-comment" "test/ask-gate/reference/source.rkt")))
       #t)
      (check
       "formatter-refuses-unverified-reference-without-rewriting-it"
       (lambda ()
         (and (not (hash-ref (format-repository gate-root #:write? #t) 'pass))
              (equal? (file->bytes reference-path) changed-reference)))
       #t)
      (write-bytes-file reference-path (bytes-append reference-bytes secret-bytes))
      (check
       "frozen-reference-remains-subject-to-secret-signature-scans"
       (lambda ()
         (define result (check-repository gate-root))
         (andmap
          (lambda (kind) (issue? result kind "test/ask-gate/reference/source.rkt"))
          '("private-key" "credential" "home-path" "key-file")))
       #t)))
   (lambda () (delete-directory/files temporary)))
  (hash
   'suite
   "publication-admissions"
   'count
   count
   'pass
   passed
   'fail
   (- count passed)
   'details
   (reverse failures)))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail))
   (error 'publication-admissions "publication admission checks failed: ~s" result)))
