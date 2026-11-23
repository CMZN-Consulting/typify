#lang racket/base

(require json
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         racket/system
         "../src/main.rkt"
         "../src/diagnostics.rkt"
         "../scripts/stage5-evidence.rkt")

(provide run-tests)

(define-runtime-path ci "../scripts/ci.rkt")

(define (run-tests)
  (define checks 0)
  (define (check x expected)
    (set! checks (add1 checks))
    (unless (equal? x expected) (error 'stage5 "expected ~s, got ~s" expected x)))
  (define tmp (make-temporary-file "measure-consumer~a" 'directory))
  (define root (build-path tmp "fixture"))
  (define declarations (build-path tmp "declarations.typify"))
  (define fact-file (build-path tmp "facts.json"))
  (define subject-names '("Amber" "Cedar" "Violet"))
  (define revision (make-string 40 #\d))
  (define (digest v) (content-hash (canonical-json v)))
  (define (source name) (content-hash name))
  (define (record args)
    (define out (open-output-string))
    (define-values
     (status native)
     (parameterize ((current-output-port out)) (execute-command (append args '("--json")))))
    (check status 0)
    (hash 'exit status 'stdout (get-output-string out) 'argv (append '("typify") args '("--json"))))
  (define (put v)
    (define file (build-path tmp "packet.json"))
    (define text (canonical-json v))
    (display-to-file text file #:exists 'truncate/replace)
    (check-stage5-consumer file revision (content-hash text)))
  (define (update-row rows f) (cons (f (car rows)) (cdr rows)))
  (dynamic-wind
   void
   (lambda ()
     (make-directory root)
     (display-to-file
      (string-append
       "#lang typify\n"
       (apply
        string-append
        (for/list
         ((n subject-names))
         (display-to-file (format "at : /~a\n" n) (build-path root (string-append n ".typify")))
         (format
          "declare : ~a\nfile : ~a.typify\ncell : entries = shadow\nprimitive : store : canonical-path\nwrites : store : entries = put 1\nform : at : <canonical-path>\ndoes : store <canonical-path>\nend!\n"
          n
          n))))
      declarations)
     (define facts (record (list "facts" (path->string root))))
     (define raw (hash-ref facts 'stdout))
     (display-to-file raw fact-file)
     (define fh (content-hash raw))
     (define (run action)
       (hash-set
        (record
         (list
          action
          (path->string declarations)
          (path->string root)
          "--facts"
          (path->string fact-file)))
        'factsInputSha256
        fh))
     (define native (hash 'tree (run "tree") 'plan (run "plan")))
     (define subjects
       (for/list
        ((s subject-names) (i (in-naturals)) (d '("measured" "dormant" "unenforced")))
        (hash
         'id
         s
         'disposition
         d
         'reason
         d
         'treePaths
         (list (list "programs" i))
         'planPaths
         (list (list "programs" i)))))
     (define mounts
       (for/list
        ((s subjects))
        (define r (hash 'id (hash-ref s 'id) 'disposition (hash-ref s 'disposition)))
        (if (equal? "measured" (hash-ref s 'disposition))
          (hash-set* r 'kernelSha256 (source "mount") 'planSha256 (source "mount"))
          r)))
     (define rows
       (for/list
        ((s subject-names) (i (in-naturals)))
        (define terms (list (list "store" (string-append "/" s))))
        (define base (hash 'path (string-append s ".typify") 'language s 'status "present"))
        (define r
          (hash
           'id
           s
           'treeSha256
           (digest (list (hash-set base 'terms terms)))
           'planSha256
           (digest (list (hash-set* base 'plan terms 'recorded '() 'ensure '() 'skips '())))))
        (if (zero? i) (hash-set r 'mountSha256 (source "mount")) r)))
     (define primitive-ids (for/list ((s subject-names)) (list s "store" '("canonical-path"))))
     (define ops
       (list
        (hash
         'id
         "remove"
         'operatorSha256
         (source "operator")
         'eligibilitySha256
         (source "eligible"))))
     (define sites
       (for/list
        ((i (in-range 10)))
        (list (if (zero? i) "small" "large") (number->string i) "remove")))
     (define cell (hash 'present #t 'value "present"))
     (define gone (hash 'present #f))
     (define (blind-row name subject field law)
       (hash
        'id
        name
        'subject
        subject
        'field
        field
        'law
        law
        'author
        "Custodian"
        'case
        "base"
        'projection
        "plan"
        'selector
        (list "programs" 0 "status")
        'provenance
        "blind-declaration"
        'expected
        cell))
     (define blind-rows
       (list
        (blind-row "alpha" "Amber" "mode" 4)
        (blind-row "beta" "Amber" "mode" 4)
        (blind-row "outside" "Cedar" "status" 1)))
     (define (site-def s) (hash 'id s 'mutantSha256 (digest s) 'reach '(("Amber" "mode"))))
     (define census-sites (map site-def sites))
     (define cases (list (hash 'id "base" 'projections '("plan"))))
     (define (native-call status exit)
       (hash
        'status
        status
        'exit
        exit
        'stdout
        (hash 'sha256 (source "out") 'bytes 3)
        'stderr
        (hash 'sha256 (source "") 'bytes 0)))
     (define success (native-call "ok" 0))
     (define calls (hash 'load success 'base (hash 'plan success)))
     (define (observation r actual) (hash 'row (hash-ref r 'id) 'expected cell 'actual actual))
     (define baseline
       (hash
        'calls
        calls
        'status
        "measured"
        'observations
        (map (lambda (r) (observation r cell)) blind-rows)))
     (define scope
       (hash
        'subjects
        subjects
        'mutationCases
        cases
        'mutationRowsSha256
        (digest blind-rows)
        'mutationSites
        census-sites
        'equivalencesSha256
        (digest '())
        'treeFields
        '("path" "language" "status" "terms")
        'primitives
        primitive-ids
        'forms
        subject-names
        'exceptions
        (list (car primitive-ids))
        'layers
        '("large" "small" "unused")
        'operators
        '("remove")
        'operatorsSha256
        (digest ops)
        'eligible
        sites))
     (define (censuses s)
       (for/list
        ((id '("first" "second")))
        (hash 'sourceSha256 (source id) 'inventorySha256 (digest s))))
     (define audit
       (hash
        'authorId
        "Writer"
        'primitives
        (for/list
         ((id primitive-ids) (i (in-naturals)))
         (hash 'id id 'redundant #f 'exception (zero? i) 'evidenceSha256 (digest id)))
        'forms
        (for/list
         ((n subject-names) (id primitive-ids))
         (hash 'id n 'directHostEffect #f 'hostPrimitives (list id) 'evidenceSha256 (source n)))
        'exceptions
        (list
         (hash
          'id
          (car primitive-ids)
          'reason
          "separate capability"
          'reviewerId
          "Verifier"
          'reviewSha256
          (source "exception")))))
     (define (trial s kill?)
       (define r
         (hash
          'id
          s
          'mutantSha256
          (digest s)
          'policyOnly
          #f
          'eligibleRows
          '("alpha" "beta")
          'status
          (if kill? "killed" "survived")
          'killed
          kill?
          'firstRow
          (if kill? "beta" 'null)
          'calls
          calls
          'observations
          (list
           (observation (car blind-rows) cell)
           (observation (cadr blind-rows) (if kill? gone cell)))))
       (if kill? r (hash-set* r 'reviewerId "Verifier" 'reviewSha256 (source "signed"))))
     (define trials (for/list ((site sites) (i (in-naturals))) (trial site (positive? i))))
     (define mut
       (hash
        'operatorDefinitions
        ops
        'operatorsSha256
        (digest ops)
        'baseline
        baseline
        'preRunEquivalences
        '()
        'runnerId
        "Operator"
        'trials
        trials))
     (define collectors
       (for/list ((id '("facts-a" "facts-b"))) (hash-set facts 'sourceSha256 (source id))))
     (define oracle (hash 'rows rows 'rowsSha256 (digest rows) 'mutationRows blind-rows))
     (define ad-rows (map (lambda (r) (hash-remove r 'mountSha256)) rows))
     (define adapter (hash 'treeOk #t 'planStatus "ok" 'factsInputSha256 fh 'rows ad-rows))
     (define p
       (hash
        'schema
        "typify-consumer-release-v2"
        'version
        (record '("--version"))
        'consumerRevision
        revision
        'scope
        scope
        'enumerators
        (censuses scope)
        'sources
        (hash
         'oracleSha256
         (source "oracle")
         'nativeSha256
         (source "native")
         'adapterSha256
         (source "adapter")
         'kernelSha256
         (source "kernel"))
        'oracle
        oracle
        'mounts
        mounts
        'audit
        audit
        'mutation
        mut
        'collectors
        collectors
        'native
        native
        'adapter
        adapter))
     (define result (put p))
     (check result (if (hash-ref result 'pass) result 'should-pass))
     (define gates (hash-ref result 'gates))
     (check (length gates) 4)
     (check
      (map (lambda (k) (hash-ref (car gates) k)) '(subjects agreements dormant unenforced))
      '(3 1 1 1))
     (check (hash-ref (caddr gates) 'killed) 9)
     (check (length (hash-ref (caddr gates) 'perLayer)) 3)
     (for
      ((s (in-list (append subject-names (list (path->string root) "small" "Verifier")))))
      (check (string-contains? (canonical-json result) s) #f))
     (define (fails q gate)
       (define bad (put q))
       (check (hash-ref bad 'pass) #f)
       (when gate (check (hash-ref (list-ref (hash-ref bad 'gates) gate) 'pass) #f)))
     (fails
      (hash-set
       p
       'mounts
       (update-row mounts (lambda (r) (hash-set r 'kernelSha256 (source "changed")))))
      0)
     (define unknown
       (hash-set
        scope
        'subjects
        (update-row subjects (lambda (r) (hash-set r 'disposition "unknown")))))
     (fails (hash-set* p 'scope unknown 'enumerators (censuses unknown)) 0)
     (define missing-scope
       (hash-set
        scope
        'subjects
        (list-set
         subjects
         1
         (hash-set* (cadr subjects) 'disposition "unknown" 'reason "observation unavailable"))))
     (define missing
       (put
        (hash-set*
         p
         'scope
         missing-scope
         'enumerators
         (censuses missing-scope)
         'mounts
         (list-set mounts 1 (hash-set (cadr mounts) 'disposition "unknown")))))
     (define missing-gate (car (hash-ref missing 'gates)))
     (check
      (list
       (hash-ref missing-gate 'subjects)
       (hash-ref missing-gate 'agreements)
       (hash-ref missing-gate 'unknown)
       (hash-ref missing-gate 'pass))
      '(3 1 1 #f))
     (fails (hash-set p 'mounts (cdr mounts)) 0)
     (fails (hash-set p 'audit (hash-set audit 'exceptions '())) 1)
     (fails
      (hash-set
       p
       'audit
       (hash-set
        audit
        'forms
        (update-row (hash-ref audit 'forms) (lambda (r) (hash-set r 'directHostEffect #t)))))
      1)
     (fails
      (hash-set
       p
       'mutation
       (hash-set mut 'trials (update-row trials (lambda (r) (hash-remove r 'reviewSha256)))))
      2)
     (define (test-mutation trial1)
       (hash-set p 'mutation (hash-set mut 'trials (list-set trials 1 trial1))))
     (define scored (cadr trials))
     (fails (test-mutation (trial (cadr sites) #f)) 2)
     (define bad-first (hash-set scored 'firstRow "alpha"))
     (fails (test-mutation bad-first) 2)
     (fails
      (test-mutation (hash-set scored 'observations (list (observation (caddr blind-rows) gone))))
      2)
     (fails
      (test-mutation
       (hash-set
        scored
        'observations
        (list (observation (car blind-rows) cell) (hash 'row "beta" 'expected gone 'actual cell))))
      2)
     (fails
      (test-mutation
       (hash-set
        scored
        'observations
        (list
         (observation (car blind-rows) cell)
         (observation (cadr blind-rows) (hash 'present #t 'value '(1 2))))))
      2)
     (fails (test-mutation (hash-set scored 'calls (hash 'load success))) 2)
     (fails (test-mutation (hash-set scored 'eligibleRows '("beta"))) 2)
     (fails (test-mutation (hash-set scored 'policyOnly #t)) 2)
     (for
      ((kind '("refused" "crash" "timeout" "process-error" "launch-error" "invalid-output"))
       (exit '(1 -9 null 1 null 0))
       (key '(refused crash timeout processError launchError invalidOutput)))
      (define broken (native-call kind exit))
      (define traces (hash 'load success 'base (hash 'plan broken)))
      (fails (test-mutation (hash-set scored 'calls traces)) 2)
      (define stopped
        (hash-set*
         scored
         'calls
         traces
         'status
         (string-append "evaluation-" kind)
         'observations
         '()
         'killed
         #f
         'firstRow
         'null
         'reviewerId
         "Verifier"
         'reviewSha256
         (source "signed")))
      (define counted (list-ref (hash-ref (put (test-mutation stopped)) 'gates) 2))
      (check
       (list
        (hash-ref counted 'pass)
        (hash-ref counted 'scored)
        (hash-ref counted 'killed)
        (hash-ref counted 'unsuccessful)
        (apply + (map (lambda (l) (hash-ref l key)) (hash-ref counted 'perLayer))))
       '(#f 10 8 1 1)))
     (define load-failed
       (hash-set*
        scored
        'calls
        (hash 'load (native-call "refused" 1))
        'status
        "load-refused"
        'killed
        #f
        'firstRow
        'null
        'observations
        '()
        'reviewerId
        "Verifier"
        'reviewSha256
        (source "signed")))
     (define load-count (list-ref (hash-ref (put (test-mutation load-failed)) 'gates) 2))
     (check
      (list
       (hash-ref load-count 'scored)
       (hash-ref load-count 'killed)
       (hash-ref load-count 'status)
       (hash-ref load-count 'unsuccessful))
      '(10 8 "below-threshold" 1))
     (fails (test-mutation (hash-set* load-failed 'killed #t 'firstRow "alpha")) 2)
     (define projection-error
       (hash-set*
        load-failed
        'status
        "projection-error"
        'calls
        calls
        'observations
        (list (hash 'row "beta" 'reason "ambiguous"))))
     (define projection-count (list-ref (hash-ref (put (test-mutation projection-error)) 'gates) 2))
     (check (hash-ref projection-count 'unsuccessful) 1)
     (fails (test-mutation (hash-set projection-error 'reviewerId "Operator")) 2)
     (define (with-mutation new-scope new-mut)
       (hash-set* p 'scope new-scope 'enumerators (censuses new-scope) 'mutation new-mut))
     (define (with-claims claims universe attempt)
       (define new-scope
         (hash-set*
          scope
          'eligible
          universe
          'mutationSites
          (map site-def universe)
          'equivalencesSha256
          (digest claims)))
       (with-mutation new-scope (hash-set* mut 'preRunEquivalences claims 'trials attempt)))
     (define proof
       (hash
        'mutantSha256
        (digest (car sites))
        'class
        "same-canonical-form"
        'lines
        '(1)
        'claimantId
        "Claimant"
        'reviewerId
        "Verifier"
        'proofSha256
        (source "proof")
        'reviewSha256
        (source "review")))
     (define extra (for/list ((i (in-range 10 20))) (list "large" (number->string i) "remove")))
     (define eligible (append sites extra))
     (define with-trials (append (cdr trials) (map (lambda (s) (trial s #t)) extra)))
     (check (hash-ref (put (with-claims (list proof) eligible with-trials)) 'pass) #t)
     (fails (with-claims (list proof) sites (cdr trials)) 2)
     (fails (with-claims (list (hash-set proof 'reviewerId "Custodian")) eligible with-trials) 2)
     (fails (with-claims (list (hash-remove proof 'proofSha256)) eligible with-trials) 2)
     (fails (test-mutation (hash-set scored 'equivalentProofSha256 (source "after run"))) 2)
     (fails (with-mutation scope (hash-set mut 'preRunEquivalences (list proof))) 2)
     (fails
      (with-mutation
       scope
       (hash-set
        mut
        'baseline
        (hash-set
         baseline
         'observations
         (update-row (hash-ref baseline 'observations) (lambda (r) (hash-set r 'actual gone))))))
      2)
     (fails (with-mutation scope (hash-set mut 'trials (cdr trials))) 2)
     (fails
      (hash-set
       p
       'collectors
       (list (car collectors) (hash-set (cadr collectors) 'stdout (string-append raw " "))))
      3)
     (fails (hash-set p 'collectors (list (car collectors) (car collectors))) 3)
     (fails
      (hash-set
       p
       'native
       (hash-set
        native
        'tree
        (hash-set (hash-ref native 'tree) 'factsInputSha256 (source "elsewhere"))))
      3)
     (fails (hash-set p 'adapter (hash-set adapter 'rows (cdr ad-rows))) 3)
     (fails
      (hash-set
       p
       'adapter
       (hash-set
        adapter
        'rows
        (update-row ad-rows (lambda (r) (hash-set r 'planSha256 (source "another plan"))))))
      3)
     (define (with-projections scope1 rows1)
       (hash-set*
        p
        'scope
        scope1
        'enumerators
        (censuses scope1)
        'oracle
        (hash-set* oracle 'rowsSha256 (digest rows1) 'rows rows1)
        'adapter
        (hash-set adapter 'rows (map (lambda (r) (hash-remove r 'mountSha256)) rows1))))
     (define (with-row index source)
       (list-set
        rows
        index
        (hash-set*
         (list-ref rows index)
         'treeSha256
         (hash-ref source 'treeSha256)
         'planSha256
         (hash-ref source 'planSha256))))
     (define subset
       (list-set
        subjects
        1
        (hash-set* (cadr subjects) 'treePaths '(("programs" 0)) 'planPaths '(("programs" 0)))))
     (define subset-rows (with-row 1 (car rows)))
     (fails (with-projections (hash-set scope 'subjects subset) subset-rows) 3)
     (define shared
       (list-set
        subjects
        1
        (hash-set*
         (cadr subjects)
         'treePaths
         '(("programs" 0) ("programs" 1))
         'planPaths
         '(("programs" 0) ("programs" 1)))))
     (define (expected-row name family)
       (define base (hash 'path (string-append name ".typify") 'language name 'status "present"))
       (define terms (list (list "store" (string-append "/" name))))
       (if (eq? family 'tree)
         (hash-set base 'terms terms)
         (hash-set* base 'plan terms 'recorded '() 'ensure '() 'skips '())))
     (define (shared-hash family)
       (digest (map (lambda (n) (expected-row n family)) (take subject-names 2))))
     (define shared-rows
       (list-set
        rows
        1
        (hash-set* (cadr rows) 'treeSha256 (shared-hash 'tree) 'planSha256 (shared-hash 'plan))))
     (check
      (hash-ref (put (with-projections (hash-set scope 'subjects shared) shared-rows)) 'pass)
      #t)
     (define narrowed
       (hash-set
        scope
        'subjects
        (update-row subjects (lambda (r) (hash-set r 'treePaths '(("programs" 0 "path")))))))
     (fails (with-projections narrowed rows) 3)
     (fails (with-projections (hash-set scope 'treeFields '("path")) rows) 3)
     (fails (hash-set p 'oracle (hash-set oracle 'rowsSha256 (source "unfrozen"))) #f)
     (define out (open-output-string))
     (define rc
       (parameterize
        ((current-output-port out))
        (system*/exit-code
         (find-executable-path "racket")
         ci
         "--stage"
         "5"
         "--aggregate"
         "missing-kernel"
         "missing-language"
         "missing-mutation")))
     (check rc 1)
     (define cli (string->jsexpr (get-output-string out)))
     (check (hash-ref cli 'acceptance-stage) 5)
     (check (hash-ref (hash-ref cli 'stage5) 'pass) #f))
   (lambda () (delete-directory/files tmp)))
  (hash 'suite "stage5-consumer" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (void (run-tests)))
