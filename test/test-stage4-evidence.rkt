#lang racket/base

(require racket/list
         "../scripts/stage4-evidence.rkt"
         "../src/build-info.rkt"
         "../src/diagnostics.rkt")

(provide run-tests)

(define (run-tests)
  (define checks 0)
  (define (check actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'stage4-evidence "expected ~s, got ~s" expected actual)))
  (define rev (make-string 40 #\a))
  (define digest (make-string 64 #\b))
  (define manifest
    (hash 'archiveSha256 digest 'ids '("cases/first" "cases/order") 'doubleFailureId "cases/order"))
  (define pin (content-hash (canonical-json manifest)))
  (define extension
    (hash
     'manifest
     manifest
     'independent
     #t
     'results
     (for/list ((id (in-list (hash-ref manifest 'ids)))) (hash 'id id 'pass #t))))
  (define rows
    (for/list
     ((name '("windows-x86_64" "linux-aarch64" "linux-x86_64" "macos-aarch64")))
     (hash 'target name 'transcriptSha256 digest 'v12Invocations 7 'extensionInvocations 2)))
  (define registry
    (hash
     'previousTag
     "v1.0.0-rc.1"
     'previousRevision
     "487dc51e3afe1d251866f7a77312184da5e5c779"
     'result
     (hash 'pass #t 'added '() 'removed '() 'previousCount 31 'currentCount 31)))
  (define projections
    (hash
     'sarifSchemaSha256
     "ad6db49878699b091f3eeb765b6e29e92a34bad4da88664d000c923b549c3a25"
     'sarifCodes
     diagnostic-codes
     'lspCodes
     diagnostic-codes
     'rangeComparisons
     (length diagnostic-codes)
     'schemaDocuments
     7
     'codeComparisons
     (length diagnostic-codes)
     'nonBmpComparisons
     1
     'schemaFailures
     0
     'rangeFailures
     0
     'conversionFailures
     0))
  (define provider
    (hash
     'schema
     "typify-stage4-provider-v1"
     'measurementMode
     "measured"
     'sourceRevision
     rev
     'readerBuild
     typify-build
     'registry
     registry
     'projections
     projections
     'targets
     rows
     'extension
     extension))
  (define (passed p name #:pin (given-pin pin) #:replay? (replay? #t))
    (define gates (stage4-evidence p rev given-pin replay? 7))
    (hash-ref (findf (lambda (g) (equal? (hash-ref g 'name) (symbol->string name))) gates) 'pass))
  (for ((name (in-list stage4-gate-names))) (check (passed provider name) #t))
  (check (passed provider 'frozen-wire-replay #:replay? #f) #f)
  (for
   ((bad
     (list
      (cdr rows)
      (cons (cadr rows) (cdr rows))
      (cons (hash-set (car rows) 'transcriptSha256 (make-string 64 #\c)) (cdr rows))
      (cons (hash-set (car rows) 'v12Invocations 0) (cdr rows))
      (cons (hash-set (car rows) 'v12Invocations 6) (cdr rows))
      (cons (hash-set (car rows) 'extensionInvocations 1) (cdr rows)))))
   (check (passed (hash-set provider 'targets bad) 'four-target-bytes) #f))
  (for
   ((bad
     (list
      (hash-set provider 'sourceRevision (make-string 40 #\c))
      (hash-set provider 'readerBuild digest)
      (hash-set provider 'measurementMode "synthetic")
      (hash-remove provider 'schema))))
   (for ((name (in-list (cdr stage4-gate-names)))) (check (passed bad name) #f)))
  (for
   ((bad
     (list
      (hash-set extension 'independent #f)
      (hash-set extension 'results (cdr (hash-ref extension 'results)))
      (hash-set extension 'results (make-list 2 (car (hash-ref extension 'results))))
      (hash-set extension 'manifest (hash-set manifest 'doubleFailureId "missing")))))
   (check (passed (hash-set provider 'extension bad) 'independent-extension) #f))
  (for
   ((untrusted-pin (in-list (list #f digest))))
   (check (passed provider 'independent-extension #:pin untrusted-pin) #f)
   (check (passed provider 'four-target-bytes #:pin untrusted-pin) #f))
  (for
   ((bad
     (list
      (hash-remove projections 'nonBmpComparisons)
      (hash-set projections 'schemaDocuments 0)
      (hash-set projections 'schemaFailures 1)
      (hash-set projections 'sarifCodes (cdr diagnostic-codes))
      (hash-set projections 'lspCodes (cdr diagnostic-codes))
      (hash-set projections 'rangeComparisons 1)
      (hash-set projections 'rangeFailures 1))))
   (check (passed (hash-set provider 'projections bad) 'diagnostic-projections) #f))
  (check
   (passed
    (hash-set
     provider
     'registry
     (hash-set registry 'result (hash-set (hash-ref registry 'result) 'removed '("E_CONTEXT"))))
    'diagnostic-registry)
   #f)
  (hash 'suite "stage4-evidence" 'checks checks 'status "passed"))

(module+ test (run-tests))
