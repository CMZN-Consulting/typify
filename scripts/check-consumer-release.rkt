#lang racket/base

(require json
         file/sha1
         racket/file
         racket/list
         racket/string
         "../src/build-info.rkt"
         "../src/diagnostics.rkt"
         (only-in "ci.rkt" consumer-gate-names consumer-gate-valid?))

(provide check-consumer-release)

(define (field object key) (and (hash? object) (hash-ref object key #f)))

(define (sha? value) (and (string? value) (regexp-match? #px"^[0-9a-f]{64}$" value)))

(define (revision? value)
  (and (string? value) (regexp-match? #px"^(?:[0-9a-f]{40}|[0-9a-f]{64})$" value)))

(define (failure code) (hasheq 'pass #f 'error code))

(define (read-one port)
  (define document (read-json port))
  (and (eof-object? (read-json port)) document))

(define (native-output record)
  (and (equal? 0 (field record 'exit))
       (string? (field record 'stdout))
       (let* ((output (read-one (open-input-string (field record 'stdout))))
              (diagnostics (field output 'diagnostics)))
         (and (list? diagnostics)
              (for/and
               ((d (in-list diagnostics)))
               (member (field d 'severity) '("note" "warning" "info")))
              output))))

(define (source-files? files)
  (and (list? files)
       (pair? files)
       (for/and
        ((entry (in-list files)))
        (and (non-empty-string? (field entry 'path)) (sha? (field entry 'sha256))))))

(define (matching-inputs? tree plan)
  (define before (field tree 'inputs))
  (define after (field plan 'inputs))
  (and (non-empty-string? (field before 'root))
       (equal? (field before 'root) (field after 'root))
       (for/and
        ((key '(declarations programs)))
        (and (source-files? (field before key)) (equal? (field before key) (field after key))))))

(define (matches-projection? document projection roots)
  (define path (field projection 'path))
  (define missing (gensym))
  (and (list? path)
       (pair? path)
       (pair? (cdr path))
       (member (car path) roots)
       (sha? (field projection 'sha256))
       (let ((value
              (for/fold
               ((node document))
               ((step (in-list path)))
               (cond
                ((and (hash? node) (string? step)) (hash-ref node (string->symbol step) missing))
                ((and (list? node) (exact-nonnegative-integer? step) (< step (length node)))
                 (list-ref node step))
                (else missing)))))
         (and (not (eq? value missing))
              (equal? (content-hash (canonical-json value)) (field projection 'sha256))))))

(define (adapter-agrees? adapter subjects tree plan)
  (define rows (field adapter 'observations))
  (and (eq? (field adapter 'tree-ok) #t)
       (equal? (field adapter 'plan-status) (field plan 'status))
       (list? rows)
       (= (length rows) (length subjects))
       (for/and
        ((r (in-list rows)))
        (and (non-empty-string? (field r 'id))
             (matches-projection? tree (field r 'tree) '("programs"))
             (matches-projection? plan (field r 'plan) '("programs" "schedule" "state"))))
       (equal? (sort (map (lambda (r) (field r 'id)) rows) string<?) (sort subjects string<?))
       (not
        (check-duplicates
         (for/list
          ((r (in-list rows)))
          (list (field (field r 'tree) 'path) (field (field r 'plan) 'path)))))))

(define (check-consumer-release path revision packet-sha)
  (with-handlers
   ((exn:fail? (lambda (_) (failure "unreadable-or-invalid-packet"))))
   (let/ec
    stop
    (define (need condition code) (unless condition (stop (failure code))))
    (need (and (revision? revision) (sha? packet-sha)) "invalid-pin")
    (define bytes (file->bytes path))
    (need (equal? packet-sha (bytes->hex-string (sha256-bytes bytes))) "packet-pin-mismatch")
    (define packet (read-one (open-input-bytes bytes)))
    (need (equal? (field packet 'schema) "typify-consumer-release-v1") "invalid-packet-schema")
    (need (equal? (field packet 'consumer-revision) revision) "consumer-revision-mismatch")
    (define version (native-output (field packet 'version)))
    (need
     (and (equal? (field version 'version) typify-version)
          (equal? (field version 'build) typify-build))
     "reader-build-mismatch")
    (define gates (field packet 'gates))
    (need
     (for/and ((gate (in-list consumer-gate-names))) (consumer-gate-valid? gate (field gates gate)))
     "invalid-consumer-gate")
    (define inventories
      (for/hasheq
       ((gate (in-list consumer-gate-names)))
       (values gate (sort (field (field gates gate) 'inventory) string<?))))
    (define sources (field packet 'provenance))
    (need
     (equal? (content-hash (canonical-json inventories)) (field sources 'inventory-sha256))
     "inventory-pin-mismatch")
    (define expected-source (field sources 'expected-source-sha256))
    (define observed-source (field sources 'observed-source-sha256))
    (need
     (and (sha? expected-source)
          (sha? observed-source)
          (not (equal? expected-source observed-source)))
     "missing-independent-provenance")
    (define tree (native-output (field packet 'tree)))
    (define plan (native-output (field packet 'plan)))
    (need
     (and (eq? (field tree 'ok) #t) (list? (field tree 'programs)) (pair? (field tree 'programs)))
     "tree-did-not-pass")
    (need
     (and (member (field plan 'status) '("ok" "unchecked"))
          (list? (field plan 'schedule))
          (hash? (field plan 'state))
          (list? (field plan 'programs))
          (pair? (field plan 'programs)))
     "plan-did-not-pass")
    (need (matching-inputs? tree plan) "inconsistent-reader-inputs")
    (define subjects (hash-ref inventories 'consumer-mount-state))
    (need (adapter-agrees? (field packet 'adapter) subjects tree plan) "adapter-disagreement")
    (hasheq
     'pass
     #t
     'packetSha256
     packet-sha
     'readerBuild
     typify-build
     'subjects
     (length subjects)
     'effectForms
     (length (hash-ref inventories 'consumer-primitive-audit))
     'primitives
     (field (field gates 'consumer-primitive-audit) 'primitives)
     'mutants
     (length (hash-ref inventories 'consumer-rule-mutation))
     'killed
     (field (field gates 'consumer-rule-mutation) 'killed)
     'uncheckedPlans
     (if (equal? (field plan 'status) "unchecked") 1 0)))))

(module+
 main
 (define args (vector->list (current-command-line-arguments)))
 (unless (= (length args) 3)
   (displayln
    "usage: racket scripts/check-consumer-release.rkt PACKET.json EXPECTED_CONSUMER_REVISION EXPECTED_PACKET_SHA256"
    (current-error-port))
   (exit 2))
 (define result (apply check-consumer-release args))
 (write-json result)
 (newline)
 (exit (if (hash-ref result 'pass) 0 1)))
