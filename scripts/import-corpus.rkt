#lang racket/base

(require file/sha1 json racket/file racket/list racket/path racket/string)

(provide import-corpus)

(define (sha256-file path)
  (call-with-input-file path (lambda (input) (bytes->hex-string (sha256-bytes input)))))

(define (digest? text) (and (string? text) (regexp-match? #px"^[0-9a-f]{64}$" text)))

(define (inside? root candidate)
  (define root-parts (explode-path root))
  (define candidate-parts (explode-path candidate))
  (and (<= (length root-parts) (length candidate-parts))
       (equal? root-parts (take candidate-parts (length root-parts)))))

(define (import-corpus source expected provenance #:root (given-root (current-directory)))
  (unless (and (digest? expected) (digest? provenance))
    (error 'import-corpus "source and handoff SHA256 values must be explicit lowercase hex"))
  (define root (simplify-path (path->complete-path given-root) #t))
  (unless (and
           (file-exists? (build-path root "src" "info.rkt"))
           (directory-exists? (build-path root "test")))
    (error 'import-corpus "run from the source checkout"))
  (define source-path (if (path? source) source (string->path source)))
  (unless (and
           (absolute-path? source-path)
           (regexp-match? #rx"[.]json$" (path->string source-path))
           (file-exists? source-path)
           (not (link-exists? source-path)))
    (error
     'import-corpus
     "source must be an explicit absolute regular JSON file without a symlink"))
  (define actual-source (simplify-path source-path #t))
  (when (inside? root actual-source)
    (error 'import-corpus "an external corpus cannot originate in the reader checkout"))
  (when (> (file-size actual-source) (* 16 1024 1024))
    (error 'import-corpus "corpus exceeds the import size limit"))
  (unless (equal? (sha256-file actual-source) expected)
    (error 'import-corpus "source SHA256 differs from the authorized value"))
  (define directory (build-path root "test" "corpus"))
  (unless (inside? root (simplify-path directory #t))
    (error 'import-corpus "corpus destination resolves outside the source checkout"))
  (make-directory* directory)
  (when (link-exists? directory) (error 'import-corpus "corpus destination must not be a symlink"))
  (define destination (build-path directory "blind.json"))
  (define receipt-path (build-path directory "import.json"))
  (when (or (link-exists? destination) (link-exists? receipt-path))
    (error 'import-corpus "corpus files must not be symlinks"))
  (define receipt
    (hasheq
     'schema
     "typify-corpus-import-v1"
     'file
     "blind.json"
     'sha256
     expected
     'bytes
     (file-size actual-source)
     'verbatim
     #t
     'provenance
     (hasheq 'kind "external-blind-handoff" 'sha256 provenance)))
  (cond
   ((or (file-exists? destination) (file-exists? receipt-path))
    (unless (and
             (file-exists? destination)
             (file-exists? receipt-path)
             (equal? (sha256-file destination) expected)
             (equal? (call-with-input-file receipt-path read-json) receipt))
      (error 'import-corpus "existing import differs or is incomplete; no file was overwritten"))
    receipt)
   (else
    (define temporary (make-temporary-file "corpus~a.tmp" #f directory))
    (define receipt-temporary (make-temporary-file "receipt~a.tmp" #f directory))
    (dynamic-wind
     void
     (lambda ()
       (copy-file actual-source temporary #t)
       (unless (and
                (equal? (sha256-file actual-source) expected)
                (equal? (sha256-file temporary) expected))
         (error 'import-corpus "source changed during import or copied bytes differ"))
       (file-or-directory-permissions temporary 420)
       (call-with-output-file
        receipt-temporary
        (lambda (output) (write-json receipt output) (newline output))
        #:exists
        'truncate/replace)
       (file-or-directory-permissions receipt-temporary 420)
       (rename-file-or-directory temporary destination #f)
       (rename-file-or-directory receipt-temporary receipt-path #f)
       receipt)
     (lambda ()
       (when (file-exists? temporary) (delete-file temporary))
       (when (file-exists? receipt-temporary) (delete-file receipt-temporary)))))))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (unless (and
          (= (length arguments) 6)
          (equal? (first arguments) "--source")
          (equal? (third arguments) "--sha256")
          (equal? (fifth arguments) "--provenance"))
   (displayln
    "usage: racket scripts/import-corpus.rkt --source FILE --sha256 HASH --provenance HASH"
    (current-error-port))
   (exit 2))
 (with-handlers
  ((exn:fail?
    (lambda (failure)
      (write-json (hasheq 'pass #f 'message (exn-message failure)))
      (newline)
      (exit 1))))
  (define result (import-corpus (second arguments) (fourth arguments) (sixth arguments)))
  (write-json (hasheq 'pass #t 'import result))
  (newline)))
