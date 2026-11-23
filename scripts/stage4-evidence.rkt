#lang racket/base

(require racket/list "../src/build-info.rkt" "../src/diagnostics.rkt")

(provide stage4-gate-names stage4-evidence)

(define stage4-gate-names
  '(frozen-wire-replay
    diagnostic-registry
    diagnostic-projections
    independent-extension
    four-target-bytes))

(define targets '("linux-aarch64" "linux-x86_64" "macos-aarch64" "windows-x86_64"))

(define sarif-schema "ad6db49878699b091f3eeb765b6e29e92a34bad4da88664d000c923b549c3a25")

(define (field object key) (and (hash? object) (hash-ref object key #f)))

(define (sha? value) (and (string? value) (regexp-match? #px"^[0-9a-f]{64}$" value)))

(define (ids? values)
  (and (list? values)
       (andmap (lambda (v) (and (string? v) (positive? (string-length v)))) values)
       (not (check-duplicates values))))

(define (registry? registry)
  (define r (field registry 'result))
  (define previous (field r 'previousCount))
  (define added (field r 'added))
  (and (equal? (field registry 'previousTag) "v1.0.0-rc.1")
       (equal? (field registry 'previousRevision) "487dc51e3afe1d251866f7a77312184da5e5c779")
       (eq? (field r 'pass) #t)
       (null? (field r 'removed))
       (ids? added)
       (andmap (lambda (name) (member name diagnostic-codes)) added)
       (equal? previous 31)
       (equal? (+ previous (length added)) (length diagnostic-codes))
       (equal? (field r 'currentCount) (length diagnostic-codes))))

(define (projections? p count)
  (define documents (field p 'schemaDocuments))
  (define compared (field p 'codeComparisons))
  (and (for/and
        ((key '(sarifCodes lspCodes)))
        (define codes (field p key))
        (and (ids? codes) (equal? (sort codes string<?) diagnostic-codes)))
       (equal? (field p 'sarifSchemaSha256) sarif-schema)
       (exact-positive-integer? count)
       (exact-positive-integer? documents)
       (>= documents count)
       (exact-positive-integer? compared)
       (>= compared (length diagnostic-codes))
       (exact-positive-integer? (field p 'rangeComparisons))
       (>= (field p 'rangeComparisons) (length diagnostic-codes))
       (exact-positive-integer? (field p 'nonBmpComparisons))
       (equal? (field p 'schemaFailures) 0)
       (equal? (field p 'conversionFailures) 0)
       (equal? (field p 'rangeFailures) 0)))

(define (extension? extension pin)
  (define manifest (field extension 'manifest))
  (define inventory (field manifest 'ids))
  (define results (field extension 'results))
  (and (sha? pin)
       (equal? pin (content-hash (canonical-json manifest)))
       (sha? (field manifest 'archiveSha256))
       (ids? inventory)
       (pair? inventory)
       (member (field manifest 'doubleFailureId) inventory)
       (eq? (field extension 'independent) #t)
       (list? results)
       (for/and ((r (in-list results))) (and (string? (field r 'id)) (eq? #t (field r 'pass))))
       (equal? (sort inventory string<?) (sort (map (lambda (r) (field r 'id)) results) string<?))))

(define (targets? rows v12-count extension-count)
  (and (exact-positive-integer? v12-count)
       (exact-positive-integer? extension-count)
       (list? rows)
       (= (length rows) (length targets))
       (for/and
        ((r (in-list rows)))
        (and (string? (field r 'target))
             (sha? (field r 'transcriptSha256))
             (equal? (field r 'v12Invocations) v12-count)
             (equal? (field r 'extensionInvocations) extension-count)))
       (equal? (sort (map (lambda (r) (field r 'target)) rows) string<?) targets)
       (= (length (remove-duplicates (map (lambda (r) (field r 'transcriptSha256)) rows))) 1)))

(define (stage4-evidence provider revision extension-pin pinned-replay? v12-count)
  (define bound?
    (and (equal? (field provider 'schema) "typify-stage4-provider-v1")
         (equal? (field provider 'measurementMode) "measured")
         (equal? (field provider 'sourceRevision) revision)
         (equal? (field provider 'readerBuild) typify-build)))
  (define extension (field provider 'extension))
  (define admitted-extension? (and bound? (extension? extension extension-pin)))
  (define results
    (list
     pinned-replay?
     (and bound? (registry? (field provider 'registry)))
     (and bound? (projections? (field provider 'projections) v12-count))
     admitted-extension?
     (and admitted-extension?
          (targets?
           (field provider 'targets)
           v12-count
           (length (field (field extension 'manifest) 'ids))))))
  (for/list
   ((name (in-list stage4-gate-names)) (pass? (in-list results)))
   (hash
    'name
    (symbol->string name)
    'pass
    (and pass? #t)
    'status
    (cond
     (pass? "measured")
     ((eq? name 'frozen-wire-replay) "unverified-wire-replay")
     ((not provider) "missing-provider")
     ((not bound?) "invalid-provider")
     ((and (not (sha? extension-pin)) (memq name '(independent-extension four-target-bytes)))
      "missing-independent-pin")
     (else "failed-evidence")))))
