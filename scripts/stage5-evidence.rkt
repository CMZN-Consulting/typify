#lang racket/base

(require file/sha1
         json
         racket/file
         racket/list
         racket/match
         racket/string
         "../src/build-info.rkt"
         "../src/diagnostics.rkt"
         (only-in "../src/facts.rkt" validate-facts))

(provide check-stage5-consumer stage5-gate-names)

(define stage5-gate-names
  '(consumer-mount-state consumer-primitive-audit consumer-rule-mutation consumer-correspondence))

(define (sha? s) (and (string? s) (regexp-match? #px"^[a-f0-9]{64}$" s)))

(define (revision? s) (and (string? s) (regexp-match? #px"^(?:[a-f0-9]{40}|[a-f0-9]{64})$" s)))

(define (digest x) (content-hash (canonical-json x)))

(define (get o key) (hash-ref o key))

(define (need v) (unless v (error 'evidence "criterion failed")))

(define (unique? xs) (and (list? xs) (not (check-duplicates xs))))

(define (strings? xs) (and (unique? xs) (andmap non-empty-string? xs)))

(define (positive-list? xs) (and (list? xs) (pair? xs)))

(define (same-inventory? a b)
  (and (unique? a)
       (unique? b)
       (equal? (sort (map canonical-json a) string<?) (sort (map canonical-json b) string<?))))

(define (index rows ids)
  (need (same-inventory? ids (map (lambda (r) (get r 'id)) rows)))
  (for/hash ((r (in-list rows))) (values (get r 'id) r)))

(define (one-json port)
  (define parsed (read-json port))
  (need (and (hash? parsed) (eof-object? (read-json port))))
  parsed)

(define (one-text text) (one-json (open-input-string text)))

(define (capture r)
  (need (equal? 0 (get r 'exit)))
  (define stdout (get r 'stdout))
  (need (string? stdout))
  (define out (one-text stdout))
  (need
   (for/and
    ((d (in-list (get out 'diagnostics))))
    (member (get d 'severity) '("note" "info" "warning"))))
  out)

(define (distinct-sources! . xs) (need (and (andmap sha? xs) (unique? xs))))

(define (attested! records value)
  (match-define (list first second) records)
  (distinct-sources! (get first 'sourceSha256) (get second 'sourceSha256))
  (for ((r records)) (need (equal? (digest value) (get r 'inventorySha256)))))

(define (mount-counts p scope oracle)
  (define ids (map (lambda (r) (get r 'id)) (get scope 'subjects)))
  (define table (index (get p 'mounts) ids))
  (define measured 0)
  (define agreements 0)
  (define dormant 0)
  (define unenforced 0)
  (define unknown 0)
  (for
   ((subject (in-list (get scope 'subjects))))
   (define id (get subject 'id))
   (define row (hash-ref table id))
   (define status (get subject 'disposition))
   (need (equal? status (get row 'disposition)))
   (cond
    ((equal? status "measured")
     (define x (get (hash-ref oracle id) 'mountSha256))
     (define kernel (get row 'kernelSha256))
     (define planned (get row 'planSha256))
     (need (andmap sha? (list x kernel planned)))
     (set! measured (add1 measured))
     (when (and (equal? x kernel) (equal? x planned)) (set! agreements (add1 agreements))))
    ((member status '("dormant" "unenforced" "unknown"))
     (need
      (and (non-empty-string? (get subject 'reason))
           (not (hash-has-key? row 'kernelSha256))
           (not (hash-has-key? row 'planSha256))
           (not (hash-has-key? (hash-ref oracle id) 'mountSha256))))
     (cond
      ((equal? status "dormant") (set! dormant (add1 dormant)))
      ((equal? status "unenforced") (set! unenforced (add1 unenforced)))
      (else (set! unknown (add1 unknown)))))
    (else (need #f))))
  (hasheq
   'pass
   (and (positive? measured) (= agreements measured) (zero? unknown))
   'subjects
   (length ids)
   'agreements
   agreements
   'measured
   measured
   'dormant
   dormant
   'unenforced
   unenforced
   'unknown
   unknown))

(define (primitive-id? v)
  (match
   v
   ((list lang name args)
    (and (non-empty-string? lang)
         (non-empty-string? name)
         (list? args)
         (andmap non-empty-string? args)))
   (_ #f)))

(define (audit-counts p scope)
  (define ids (get scope 'primitives))
  (define forms (get scope 'forms))
  (define exceptions (get scope 'exceptions))
  (define audit (get p 'audit))
  (need
   (and (positive-list? ids)
        (andmap primitive-id? ids)
        (strings? forms)
        (pair? forms)
        (unique? exceptions)
        (andmap (lambda (id) (member id ids)) exceptions)))
  (define observations (index (get audit 'primitives) ids))
  (define examined (index (get audit 'forms) forms))
  (define reviewed (index (get audit 'exceptions) exceptions))
  (for
   ((id ids))
   (define r (hash-ref observations id))
   (need (and (eq? (get r 'redundant) #f) (sha? (get r 'evidenceSha256))))
   (need (equal? (get r 'exception) (and (member id exceptions) #t))))
  (for
   ((r (in-hash-values reviewed)))
   (need
    (and (non-empty-string? (get r 'reason))
         (sha? (get r 'reviewSha256))
         (non-empty-string? (get r 'reviewerId))
         (non-empty-string? (get audit 'authorId))
         (not (equal? (get r 'reviewerId) (get audit 'authorId))))))
  (for
   ((r (in-hash-values examined)))
   (need
    (and (eq? (get r 'directHostEffect) #f)
         (sha? (get r 'evidenceSha256))
         (list? (get r 'hostPrimitives))
         (andmap (lambda (id) (member id ids)) (get r 'hostPrimitives)))))
  (hasheq 'primitives (length ids) 'forms (length forms) 'reviewedExceptions (length exceptions)))

(define (cell? v)
  (and (hash? v)
       (case (hash-ref v 'present 'invalid)
         ((#f) (= (hash-count v) 1))
         ((#t)
          (and (= (hash-count v) 2)
               (hash-has-key? v 'value)
               (let ((x (get v 'value))) (or (eq? x 'null) (boolean? x) (string? x) (real? x)))))
         (else #f))))

(define (review! r by)
  (need
   (and (sha? (get r 'reviewSha256))
        (non-empty-string? (get r 'reviewerId))
        (not (equal? by (get r 'reviewerId))))))

(define (native-status op)
  (define s (get op 'status))
  (define rc (get op 'exit))
  (for
   ((port '(stdout stderr)))
   (define b (get op port))
   (need (and (sha? (get b 'sha256)) (exact-nonnegative-integer? (get b 'bytes)))))
  (need
   (cond
    ((equal? s "ok") (equal? rc 0))
    ((equal? s "refused") (equal? rc 1))
    ((equal? s "crash") (and (exact-integer? rc) (negative? rc)))
    ((equal? s "process-error") (exact-positive-integer? rc))
    ((member s '("timeout" "launch-error")) (eq? rc 'null))
    ((equal? s "invalid-output") (equal? rc 0))
    (else #f)))
  s)

(define (native-run-status run cases)
  (define calls (get run 'calls))
  (define load-status (native-status (get calls 'load)))
  (cond
   ((equal? load-status "ok")
    (need
     (same-inventory?
      (map symbol->string (hash-keys calls))
      (cons "load" (map (lambda (c) (get c 'id)) cases))))
    (define failures
      (for*/list
       ((c cases) (action (get c 'projections)))
       (define ops (get calls (string->symbol (get c 'id))))
       (need (same-inventory? (map symbol->string (hash-keys ops)) (get c 'projections)))
       (native-status (get ops (string->symbol action)))))
    (or (findf (lambda (s) (not (equal? s "ok"))) failures) "ok"))
   (else (need (= (hash-count calls) 1)) load-status)))

(define (first-mismatch r rows)
  (define seen (get r 'observations))
  (need (equal? (map (lambda (o) (get o 'row)) seen) (map (lambda (v) (get v 'id)) rows)))
  (for
   ((entry seen) (blind rows))
   (need (and (equal? (get entry 'expected) (get blind 'expected)) (cell? (get entry 'actual)))))
  (or (for/first ((v seen) #:unless (equal? (get v 'actual) (get v 'expected))) (get v 'row))
      'null))

(define (mutation-counts p scope)
  (let/ec
   stop
   (define sites (get scope 'eligible))
   (define layers (get scope 'layers))
   (define ops (get scope 'operators))
   (need
    (and (strings? layers)
         (pair? layers)
         (strings? ops)
         (pair? ops)
         (positive-list? sites)
         (unique? sites)))
   (define reg (get p 'mutation))
   (define author (get reg 'runnerId))
   (need (non-empty-string? author))
   (define defs (get reg 'operatorDefinitions))
   (define def-hash (digest defs))
   (need
    (and (equal? def-hash (get reg 'operatorsSha256))
         (equal? def-hash (get scope 'operatorsSha256))))
   (for
    ((r (in-hash-values (index defs ops))))
    (need (and (sha? (get r 'operatorSha256)) (sha? (get r 'eligibilitySha256)))))
   (define blind (get (get p 'oracle) 'mutationRows))
   (need (and (positive-list? blind) (equal? (digest blind) (get scope 'mutationRowsSha256))))
   (define row-ids (map (lambda (r) (get r 'id)) blind))
   (need (strings? row-ids))
   (define subjects
     (for/hash ((r (get scope 'subjects))) (values (get r 'id) (get r 'disposition))))
   (define cases (get scope 'mutationCases))
   (define case-ids (map (lambda (r) (get r 'id)) cases))
   (need (and (strings? case-ids) (pair? case-ids) (not (member "load" case-ids))))
   (define case-table (index cases case-ids))
   (for
    ((c cases))
    (define modes (get c 'projections))
    (need
     (and (strings? modes)
          (member "plan" modes)
          (andmap (lambda (x) (member x '("tree" "plan"))) modes))))
   (for
    ((r blind))
    (need
     (and (hash-has-key? subjects (get r 'subject))
          (non-empty-string? (get r 'field))
          (member (get r 'law) '(1 4))
          (equal? "blind-declaration" (get r 'provenance))
          (non-empty-string? (get r 'author))
          (cell? (get r 'expected))
          (positive-list? (get r 'selector))
          (member (get r 'projection) (get (get case-table (get r 'case)) 'projections)))))
   (define baseline (get reg 'baseline))
   (unless (and
            (equal? "ok" (native-run-status baseline cases))
            (equal? (get baseline 'status) "measured")
            (eq? 'null (first-mismatch baseline blind)))
     (stop
      (hasheq
       'pass
       #f
       'status
       "void-baseline"
       'seeded
       (length sites)
       'scored
       0
       'killed
       0
       'perLayer
       '())))
   (define registry (index (get scope 'mutationSites) sites))
   (define byte-ids (map (lambda (s) (get (get registry s) 'mutantSha256)) sites))
   (need (and (unique? byte-ids) (andmap sha? byte-ids)))
   (define claims (get reg 'preRunEquivalences))
   (need (equal? (digest claims) (get scope 'equivalencesSha256)))
   (define removed (map (lambda (q) (get q 'mutantSha256)) claims))
   (need (and (unique? removed) (andmap (lambda (id) (member id byte-ids)) removed)))
   (for
    ((q claims))
    (define by (get q 'claimantId))
    (review! q by)
    (need
     (and (non-empty-string? by)
          (sha? (get q 'proofSha256))
          (positive-list? (get q 'lines))
          (andmap exact-positive-integer? (get q 'lines))
          (member
           (get q 'class)
           '("same-canonical-form" "unreachable-by-declaration" "shadowed-by-clause"))
          (for/and ((r blind)) (not (member (get r 'author) (list by (get q 'reviewerId))))))))
   (define scored-sites
     (filter (lambda (s) (not (member (get (get registry s) 'mutantSha256) removed))) sites))
   (define trials (index (get reg 'trials) scored-sites))
   (define outcomes (for/hash ((layer layers)) (values layer (make-hash))))
   (define credited (make-hash))
   (define (inc! layer outcome) (hash-update! (get outcomes layer) outcome add1 0))
   (for
    ((s sites))
    (match-define (list layer site op) s)
    (need (and (member layer layers) (non-empty-string? site) (member op ops)))
    (define def (get registry s))
    (define reach (get def 'reach))
    (need
     (and (unique? reach)
          (andmap
           (match-lambda
            ((list person field) (and (hash-has-key? subjects person) (non-empty-string? field)))
            (_ #f))
           reach)))
    (define sha (get def 'mutantSha256))
    (define policy?
      (and (pair? reach)
           (for/and ((pair reach)) (not (equal? (get subjects (car pair)) "measured")))))
    (when policy? (inc! layer 'policyOnly))
    (cond
     ((member sha removed) (inc! layer 'equivalent))
     (else
      (define r (get trials s))
      (need
       (and (equal? sha (get r 'mutantSha256))
            (equal? policy? (get r 'policyOnly))
            (not (hash-has-key? r 'equivalentProofSha256))))
      (define eligible
        (for/list
         ((row blind)
          #:when
          (and (or (not policy?) (= 4 (get row 'law)))
               (member (list (get row 'subject) (get row 'field)) reach)))
         row))
      (need (equal? (get r 'eligibleRows) (map (lambda (row) (get row 'id)) eligible)))
      (define measured (native-run-status r cases))
      (define status (get r 'status))
      (define completed? (member status '("killed" "survived")))
      (define first (if completed? (first-mismatch r eligible) 'null))
      (define killed? (not (eq? first 'null)))
      (need (and (equal? (get r 'firstRow) first) (equal? (get r 'killed) killed?)))
      (define outcome
        (cond
         (completed?
          (need (and (equal? measured "ok") (equal? killed? (equal? status "killed"))))
          (if killed? 'killed 'survived))
         (else
          (define diagnostics (get r 'observations))
          (need
           (or (null? diagnostics)
               (and (equal? status "projection-error")
                    (= (length diagnostics) 1)
                    (member (get (car diagnostics) 'row) (get r 'eligibleRows))
                    (non-empty-string? (get (car diagnostics) 'reason))
                    (not (hash-has-key? (car diagnostics) 'actual)))))
          (define suffix (string->symbol (regexp-replace #px"^(load|evaluation)-" status "")))
          (need
           (if (equal? measured "ok")
             (equal? status "projection-error")
             (and (equal? measured (symbol->string suffix))
                  (string-prefix?
                   status
                   (if (equal? "ok" (native-status (get (get r 'calls) 'load)))
                     "evaluation-"
                     "load-")))))
          (case suffix
            ((refused crash timeout) suffix)
            ((process-error) 'processError)
            ((launch-error) 'launchError)
            ((invalid-output) 'invalidOutput)
            ((projection-error) 'projectionError)
            (else (need #f))))))
      (if killed? (hash-update! credited (list layer first) add1 0) (review! r author))
      (inc! layer outcome))))
   (define categories
     '(killed
       survived
       refused
       crash
       timeout
       processError
       launchError
       invalidOutput
       projectionError
       equivalent))
   (define by-layer
     (for/list
      ((layer (sort layers string<?)))
      (define table (get outcomes layer))
      (define c (for/hasheq ((k categories)) (values k (hash-ref table k 0))))
      (define total (apply + (hash-values c)))
      (define count (- total (get c 'equivalent)))
      (hash-set*
       c
       'layerSha256
       (content-hash layer)
       'enumerated
       total
       'scored
       count
       'policyOnly
       (hash-ref table 'policyOnly 0)
       'topRowKills
       (for/fold
        ((n 0))
        (((key count) (in-hash credited)) #:when (equal? layer (car key)))
        (max n count))
       'state
       (cond
        ((zero? total) "zero-site")
        ((zero? count) "all-equivalent")
        ((zero? (get c 'killed)) "all-survived")
        (else "scored")))))
   (define (sum k) (for/sum ((r by-layer)) (get r k)))
   (define total (sum 'scored))
   (define kills (sum 'killed))
   (define equivalent-cap? (<= (* (length removed) 100) (* 5 (length sites))))
   (define threshold? (and (positive? total) (>= (* kills 100) (* total 90))))
   (hasheq
    'pass
    (and equivalent-cap? threshold?)
    'status
    (cond
     ((not equivalent-cap?) "equivalence-limit")
     (threshold? "scored")
     (else "below-threshold"))
    'seeded
    (length sites)
    'scored
    total
    'killed
    kills
    'survived
    (sum 'survived)
    'unsuccessful
    (- total kills (sum 'survived))
    'equivalent
    (length removed)
    'perLayer
    by-layer)))

(define (program-indices paths)
  (need (and (positive-list? paths) (unique? paths)))
  (define indices
    (for/list
     ((path (in-list paths)) #:when (match path ((list "programs" _) #t) (_ #f)))
     (define i (cadr path))
     (need (exact-nonnegative-integer? i))
     i))
  (need (pair? indices))
  indices)

(define (row-fields scope family)
  (define required
    (case family
      ((tree) '("path" "language" "status" "terms"))
      ((plan) '("path" "language" "status" "plan" "recorded" "skips" "ensure"))))
  (define optional (case family ((tree) '("placeholders" "origins")) ((plan) '())))
  (define fields (hash-ref scope (if (eq? family 'tree) 'treeFields 'planFields) #f))
  (when fields
    (need
     (and (strings? fields)
          (andmap (lambda (k) (member k fields)) required)
          (andmap (lambda (k) (member k (append required optional))) fields))))
  fields)

(define (projection document paths family fields)
  (digest
   (for/list
    ((path paths))
    (match
     path
     ((list "programs" (? exact-nonnegative-integer? i))
      (define row (list-ref (get document 'programs) i))
      (if fields
        (for/hasheq ((key fields)) (define k (string->symbol key)) (values k (get row k)))
        row))
     ((list "schedule" (? exact-nonnegative-integer? i))
      (need (eq? family 'plan))
      (list-ref (get document 'schedule) i))
     ((list "state" (? non-empty-string? cell))
      (need (eq? family 'plan))
      (get (get document 'state) (string->symbol cell)))
     (_ (need #f))))))

(define (correspondence-counts p scope oracle)
  (match-define (list a b) (get p 'collectors))
  (distinct-sources! (get a 'sourceSha256) (get b 'sourceSha256))
  (define bytes (get a 'stdout))
  (need
   (and (equal? 0 (get a 'exit))
        (equal? 0 (get b 'exit))
        (string? bytes)
        (equal? bytes (get b 'stdout))))
  (define facts (validate-facts (one-text bytes)))
  (define fact-digest (content-hash bytes))
  (define commands (get p 'native))
  (define (output k)
    (define capture-record (get commands k))
    (match-define (list "typify" cmd decl root "--facts" path "--json") (get capture-record 'argv))
    (need
     (and (equal? cmd (symbol->string k))
          (andmap non-empty-string? (list decl root path))
          (equal? (get capture-record 'factsInputSha256) fact-digest)))
    (define doc (capture capture-record))
    (define reads (get doc 'inputs))
    (need
     (and (non-empty-string? (get reads 'root))
          (if (equal? (get doc 'facts) "none")
            (and (eq? (get reads 'factsRoot) 'null) (null? (get reads 'facts)))
            (and (equal? (get reads 'factsRoot) (get facts 'root))
                 (equal? (get doc 'facts) (string-append "sha256:" fact-digest))))))
    doc)
  (define tree (output 'tree))
  (define plan (output 'plan))
  (define adapter (get p 'adapter))
  (need
   (and (eq? (get tree 'ok) #t)
        (eq? (get adapter 'treeOk) #t)
        (member (get plan 'status) '("ok" "unchecked"))
        (equal? (get plan 'status) (get adapter 'planStatus))
        (equal? fact-digest (get adapter 'factsInputSha256))))
  (need (equal? (cddr (get (get commands 'tree) 'argv)) (cddr (get (get commands 'plan) 'argv))))
  (for
   ((k '(root declarations programs paths directories)))
   (need (equal? (get (get tree 'inputs) k) (get (get plan 'inputs) k))))
  (for
   ((k '(declarations programs)))
   (need (list? (get (get tree 'inputs) k)))
   (when (eq? k 'declarations) (need (pair? (get (get tree 'inputs) k))))
   (for
    ((r (get (get tree 'inputs) k)))
    (need (and (non-empty-string? (get r 'path)) (sha? (get r 'sha256))))))
  (define subjects (get scope 'subjects))
  (define observations (index (get adapter 'rows) (map (lambda (s) (get s 'id)) subjects)))
  (define tree-fields (row-fields scope 'tree))
  (define plan-fields (row-fields scope 'plan))
  (define (program-coordinates doc fields)
    (for/list
     ((r (in-list (get doc 'programs))))
     (define coordinates (list (get r 'path) (get r 'language) (get r 'status)))
     (need (and (andmap non-empty-string? coordinates) (for/and ((k fields)) (list? (get r k)))))
     coordinates))
  (need
   (equal?
    (program-coordinates tree '(terms))
    (program-coordinates plan '(plan recorded skips ensure))))
  (need (and (list? (get plan 'schedule)) (hash? (get plan 'state))))
  (define covered
    (for/list
     ((s subjects))
     (define id (get s 'id))
     (define r (hash-ref observations id))
     (define x (hash-ref oracle id))
     (define tree-coverage (program-indices (get s 'treePaths)))
     (define plan-coverage (program-indices (get s 'planPaths)))
     (need (same-inventory? tree-coverage plan-coverage))
     (for
      ((kind '(treeSha256 planSha256))
       (family '(tree plan))
       (doc (list tree plan))
       (fields (list tree-fields plan-fields))
       (paths (list (get s 'treePaths) (get s 'planPaths))))
      (define measured (projection doc paths family fields))
      (need (and (equal? measured (get x kind)) (equal? measured (get r kind)))))
     tree-coverage))
  (define all-programs (length (get tree 'programs)))
  (need (same-inventory? (remove-duplicates (append* covered)) (range all-programs)))
  (hasheq
   'subjects
   (length subjects)
   'programs
   all-programs
   'collectors
   2
   'factsSha256
   fact-digest
   'uncheckedPlans
   (if (equal? (get plan 'status) "unchecked") 1 0)))

(define (check-stage5-consumer path revision packet-sha)
  (define (failed) (hasheq 'pass #f 'error "invalid-consumer-evidence"))
  (with-handlers
   ((exn:fail? (lambda (_) (failed))))
   (need (and (revision? revision) (sha? packet-sha)))
   (define raw (file->bytes path))
   (need (equal? packet-sha (bytes->hex-string (sha256-bytes raw))))
   (define p (one-json (open-input-bytes raw)))
   (need
    (and (equal? (get p 'schema) "typify-consumer-release-v2")
         (equal? (get p 'consumerRevision) revision)))
   (define version (capture (get p 'version)))
   (need
    (and (equal? (get version 'build) typify-build) (equal? (get version 'version) typify-version)))
   (define scope (get p 'scope))
   (attested! (get p 'enumerators) scope)
   (define subjects (map (lambda (s) (get s 'id)) (get scope 'subjects)))
   (need (and (pair? subjects) (strings? subjects)))
   (define oracle-data (get p 'oracle))
   (define oracle (index (get oracle-data 'rows) subjects))
   (need (equal? (get oracle-data 'rowsSha256) (digest (get oracle-data 'rows))))
   (define sources (get p 'sources))
   (distinct-sources!
    (get sources 'oracleSha256)
    (get sources 'kernelSha256)
    (get sources 'nativeSha256)
    (get sources 'adapterSha256))
   (define gates
     (for/list
      ((gate stage5-gate-names)
       (measure
        (list
         (lambda () (mount-counts p scope oracle))
         (lambda () (audit-counts p scope))
         (lambda () (mutation-counts p scope))
         (lambda () (correspondence-counts p scope oracle)))))
      (with-handlers
       ((exn:fail? (lambda (_) (hasheq 'name (symbol->string gate) 'pass #f))))
       (define observation (measure))
       (hash-set* observation 'name (symbol->string gate) 'pass (hash-ref observation 'pass #t)))))
   (hasheq
    'pass
    (andmap (lambda (r) (get r 'pass)) gates)
    'gates
    gates
    'packetSha256
    packet-sha
    'readerBuild
    typify-build
    'scopeSha256
    (digest scope))))
