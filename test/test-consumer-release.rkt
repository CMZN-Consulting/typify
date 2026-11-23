#lang racket/base

(require json
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         racket/system
         "../src/diagnostics.rkt"
         "../src/main.rkt"
         "../scripts/check-consumer-release.rkt")

(provide run-tests)

(define-runtime-path release-check "../scripts/check-consumer-release.rkt")

(define (run-tests)
  (define checks 0)
  (define (check result expected)
    (set! checks (add1 checks))
    (unless (equal? result expected)
      (error 'consumer-release "expected ~s, received ~s" expected result)))
  (define root (make-temporary-file "consumer-release~a" 'directory))
  (define file (build-path root "packet.json"))
  (define declaration (path->string (build-path root "declarations")))
  (define revision (make-string 40 #\c))
  (define secret "private-consumer-fixture")
  (define (native argv)
    (define out (open-output-string))
    (define-values
     (code diagnostics)
     (parameterize ((current-output-port out)) (execute-command (append argv '("--json")))))
    (check code 0)
    (hash 'exit code 'stdout (get-output-string out)))
  (define (with-output record transform)
    (hash-set
     record
     'stdout
     (canonical-json (transform (string->jsexpr (hash-ref record 'stdout))))))
  (define (receipt id props)
    (hash
     'complete
     #t
     'total
     1
     'failures
     0
     'inventory
     (list id)
     'observations
     (list (hash-set props 'id id))))
  (define (put-packet obj)
    (define text (canonical-json obj))
    (call-with-output-file
     file
     (lambda (out) (write-bytes (string->bytes/utf-8 text) out))
     #:exists
     'truncate/replace)
    (content-hash text))
  (define (cli digest)
    (define output (open-output-string))
    (define errors (open-output-string))
    (define status
      (parameterize
       ((current-output-port output) (current-error-port errors))
       (system*/exit-code (find-executable-path "racket") release-check file revision digest)))
    (check (get-output-string errors) "")
    (check (string-contains? (get-output-string output) secret) #f)
    (values status (string->jsexpr (get-output-string output))))
  (dynamic-wind
   void
   (lambda ()
     (display-to-file
      (string-join
       '("#lang typify"
         "declare : Records"
         "is : payload = /[a-z]+/"
         "cell : entries = shadow"
         "primitive : store : canonical-path payload"
         "writes : store : entries = put 1"
         "form : store : <payload> = <canonical-path>"
         "does : store <canonical-path> <payload>"
         "file : data.typify"
         "end!")
       "\n")
      declaration)
     (display-to-file "store : blue = /sample" (build-path root "data.typify"))
     (define tree (native (list "tree" declaration (path->string root))))
     (define plan (native (list "plan" declaration (path->string root))))
     (define version (native '("--version")))
     (define table-digest (content-hash "measured table fixture"))
     (define gates
       (hasheq
        'consumer-mount-state
        (hash-set*
         (receipt secret (hash 'predicted-sha256 table-digest 'observed-sha256 table-digest))
         'mismatches
         0
         'independent-observations
         #t)
        'consumer-primitive-audit
        (hash-set*
         (receipt "store" (hash 'primitives '("put")))
         'primitives
         1
         'effect-forms
         1
         'deriving-effects
         0
         'form-specific-except-keygen
         0)
        'consumer-rule-mutation
        (hash-set*
         (receipt "remove-form" (hash 'killed #t))
         'seeded
         1
         'killed
         1
         'registered
         #t
         'baseline-valid
         #t)))
     (define sources
       (hash
        'expected-source-sha256
        (content-hash "adapter fixture definition")
        'observed-source-sha256
        (content-hash "reader measurement fixture")
        'inventory-sha256
        (content-hash
         (canonical-json
          (for/hasheq (((k v) (in-hash gates))) (values k (hash-ref v 'inventory)))))))
     (define expected (content-hash (canonical-json '("store" "/sample" "blue"))))
     (define row
       (hash
        'id
        secret
        'tree
        (hash 'path '("programs" 0 "terms" 0) 'sha256 expected)
        'plan
        (hash 'path '("schedule" 0 "term") 'sha256 expected)))
     (define adapter (hash 'tree-ok #t 'plan-status "ok" 'observations (list row)))
     (define packet
       (hash
        'schema
        "typify-consumer-release-v1"
        'consumer-revision
        revision
        'version
        version
        'tree
        tree
        'plan
        plan
        'adapter
        adapter
        'provenance
        sources
        'gates
        gates))
     (define (changed-rows rows) (hash-set packet 'adapter (hash-set adapter 'observations rows)))
     (define (changed-projection command key value)
       (changed-rows (list (hash-set row command (hash-set (hash-ref row command) key value)))))
     (define packet-digest (put-packet packet))
     (define-values (status valid) (cli packet-digest))
     (check status 0)
     (check (hash-ref valid 'pass) #t)
     (check (hash-ref valid 'subjects) 1)
     (check (hash-ref valid 'uncheckedPlans) 0)
     (check (hash-ref valid 'packetSha256) packet-digest)
     (check
      (check-consumer-release file (make-string 40 #\a) packet-digest)
      (hasheq 'pass #f 'error "consumer-revision-mismatch"))
     (for
      ((mutation
        (in-list
         (list
          (hash-remove packet 'version)
          (hash-set
           packet
           'version
           (with-output version (lambda (v) (hash-set v 'build table-digest))))
          (hash-set packet 'provenance (hash-set sources 'inventory-sha256 table-digest))
          (hash-set
           packet
           'provenance
           (hash-set sources 'expected-source-sha256 (hash-ref sources 'observed-source-sha256)))
          (hash-set packet 'gates (hash-remove gates 'consumer-rule-mutation))
          (hash-set packet 'tree (hash-set tree 'exit 1))
          (hash-set packet 'tree (hash-set tree 'stdout "{} {}"))
          (hash-set packet 'plan (with-output plan (lambda (v) (hash-set v 'status "failed"))))
          (hash-set
           packet
           'tree
           (with-output
            tree
            (lambda (v) (hash-set v 'diagnostics (list (hash 'severity "error"))))))
          (hash-set
           packet
           'tree
           (with-output
            tree
            (lambda (v) (hash-set v 'inputs (hash-set (hash-ref v 'inputs) 'root "other")))))
          (changed-rows '())
          (changed-rows (list (hash-set row 'id "unregistered")))
          (changed-projection 'plan 'sha256 table-digest)
          (changed-projection 'tree 'path '("ok"))
          (changed-projection 'plan 'path '("schedule" 999 "term"))))))
      (define result (check-consumer-release file revision (put-packet mutation)))
      (check (hash-ref result 'pass) #f)
      (check (string-contains? (canonical-json result) secret) #f))
     (define external-source
       (string-replace
        (string-replace (file->string declaration) "cell : entries = shadow\n" "")
        "writes : store : entries = put 1"
        "effect : store = external"))
     (display-to-file external-source declaration #:exists 'truncate/replace)
     (define external
       (hash-set*
        packet
        'tree
        (native (list "tree" declaration (path->string root)))
        'plan
        (native (list "plan" declaration (path->string root)))))
     (check
      (hash-ref (check-consumer-release file revision (put-packet external)) 'error)
      "adapter-disagreement")
     (define external-agreed
       (hash-set external 'adapter (hash-set adapter 'plan-status "unchecked")))
     (check
      (hash-ref (check-consumer-release file revision (put-packet external-agreed)) 'uncheckedPlans)
      1)
     (define-values (failed-status failed) (cli packet-digest))
     (check failed-status 1)
     (check (hash-ref failed 'error) "packet-pin-mismatch")
     (check
      (check-consumer-release (build-path root secret) revision packet-digest)
      (hasheq 'pass #f 'error "unreadable-or-invalid-packet")))
   (lambda () (delete-directory/files root)))
  (hash 'checks checks 'status "passed" 'suite "consumer-release"))

(module+ test (run-tests))
