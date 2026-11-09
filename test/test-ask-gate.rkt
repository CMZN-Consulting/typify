#lang racket/base

(require json
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         "../scripts/import-corpus.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "../src/questions.rkt"
         "../src/syntax.rkt")

(provide run-tests run-ask-gate answer-round-trip answer-comparison replay-gate)

(define-runtime-path default-gate-root "ask-gate")

(define (answer-comparison answer)
  (unless (hash? answer) (raise-argument-error 'answer-comparison "hash?" answer))
  (if (hash-has-key? answer 'diagnostics)
    (hash-set
     answer
     'diagnostics
     (for/list
      ((item (in-list (hash-ref answer 'diagnostics))))
      (define code (hash-ref item 'code))
      (hash
       'code
       code
       'file
       (hash-ref item 'file)
       'range
       (hash-ref item 'range)
       'data
       (diagnostic-data-projection code (hash-ref item 'data)))))
    answer))

(define (answer-round-trip answer)
  (define failures '())
  (define checks 0)
  (define (check name actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (set! failures (cons (hash 'check name 'expected expected 'actual actual) failures))))
  (cond
   ((hash-has-key? answer 'word)
    (define lines (hash-ref answer 'lines))
    (for
     ((line (in-list lines)) (index (in-naturals)))
     (define parsed
       (with-handlers ((exn:fail? (lambda (_) '()))) (read-lines line #:file "answer.typify")))
     (check
      (format "line/~a" index)
      (and (= (length parsed) 1)
           (eq? (line-record-kind (car parsed)) 'colon)
           (equal? (line-record-left (car parsed)) "?"))
      #t))
    (define status-lines
      (filter
       (lambda (line) (string-prefix? line (string-append "? : " (hash-ref answer 'word) " = ")))
       lines))
    (check
     "status"
     status-lines
     (if (equal? (hash-ref answer 'status) "undeclared")
       (list (string-append "? : " (hash-ref answer 'word) " = undeclared"))
       '()))
    (check
     "near"
     (filter (lambda (line) (string-prefix? line "? : near : ")) lines)
     (map (lambda (word) (string-append "? : near : " word)) (hash-ref answer 'near))))
   (else
    (check "refusal-envelope" (sort (hash-keys answer) symbol<?) '(diagnostics ok))
    (check "refusal-status" (hash-ref answer 'ok #t) #f)))
  (hash 'checks checks 'failures (reverse failures)))

(define (validate-gate data)
  (unless (and
           (hash? data)
           (equal? (hash-keys data) '(declarations))
           (list? (hash-ref data 'declarations))
           (pair? (hash-ref data 'declarations)))
    (error 'ask-gate "the gate must contain a nonempty declarations array"))
  (for
   ((entry (in-list (hash-ref data 'declarations))))
   (unless (and
            (hash? entry)
            (string? (hash-ref entry 'path #f))
            (let ((parts (string-split (hash-ref entry 'path) "/" #:trim? #f)))
              (and (= (length parts) 2)
                   (equal? (car parts) "declarations")
                   (not (member (cadr parts) '("" "." "..")))
                   (string-suffix? (cadr parts) ".typify")
                   (not (string-contains? (cadr parts) "\\"))
                   (for/and
                    ((character (in-string (cadr parts))))
                    (not (memq (char-general-category character) '(cc cf zl zp))))))
            (string? (hash-ref entry 'language #f))
            (list? (hash-ref entry 'candidates #f))
            (andmap string? (hash-ref entry 'candidates))
            (equal?
             (length (hash-ref entry 'candidates))
             (length (remove-duplicates (hash-ref entry 'candidates))))
            (hash? (hash-ref entry 'sizes #f))
            (equal? (sort (hash-keys (hash-ref entry 'sizes)) symbol<?) '(typo unknown witness))
            (list? (hash-ref entry 'words #f)))
     (error 'ask-gate "a gate declaration has an invalid path, language, candidate list or sizes"))
   (for
    ((word (in-list (hash-ref entry 'words))))
    (unless (and
             (hash? word)
             (string? (hash-ref word 'word #f))
             (member (hash-ref word 'set #f) '("typo" "unknown" "witness"))
             (member (hash-ref word 'exit_code #f) '(0 2))
             (hash? (hash-ref word 'stdout_json #f)))
      (error 'ask-gate "a frozen word lacks its set, complete expected object or exit status")))
   (for
    ((set '(typo unknown witness)))
    (define words
      (map
       (lambda (row) (hash-ref row 'word))
       (filter
        (lambda (row) (equal? (hash-ref row 'set) (symbol->string set)))
        (hash-ref entry 'words))))
    (define size (hash-ref (hash-ref entry 'sizes) set))
    (unless (and
             (exact-nonnegative-integer? size)
             (= size (length words))
             (equal? words (sort (remove-duplicates words) string<?)))
      (error 'ask-gate "a set differs from its declared size or code-point order"))))
  data)

(define (replay-gate data load-source #:ask (ask ask-language))
  (validate-gate data)
  (define loaded (make-hash))
  (define meta #f)
  (define failures '())
  (define roundtrip-failures '())
  (define infrastructure '())
  (define words 0)
  (define roundtrip-checks 0)
  (define declarations '())
  (define (failure-observation failure)
    (if (exn:fail:typify? failure)
      (let ((diagnostics (exn:fail:typify-diagnostics failure)))
        (hash
         'exit_code
         (if (ormap
              (lambda (item) (member (diagnostic-code item) '("E_FILE" "E_USAGE")))
              diagnostics)
           2
           1)
         'stdout_json
         (hash 'ok #f 'diagnostics (map diagnostic->jsexpr diagnostics))
         'infrastructure
         #f))
      (hash
       'exit_code
       'null
       'stdout_json
       'null
       'infrastructure
       (hash 'kind "host" 'message (exn-message failure)))))
  (define (load path)
    (hash-ref!
     loaded
     path
     (lambda ()
       (with-handlers
        ((exn:fail? (lambda (failure) (hash 'failure (failure-observation failure)))))
        (define-values
         (bodies diagnostics)
         (read-declarations/diagnostics (load-source path) #:file path))
        (raise-diagnostics diagnostics)
        (hash 'bodies bodies 'diagnostics diagnostics)))))
  (for
   ((entry (in-list (hash-ref data 'declarations))))
   (define path (hash-ref entry 'path))
   (define name (hash-ref entry 'language))
   (define sizes (hash-ref entry 'sizes))
   (printf
    "ask-gate ~a ~a typo=~a unknown=~a witness=~a\n"
    path
    name
    (hash-ref sizes 'typo)
    (hash-ref sizes 'unknown)
    (hash-ref sizes 'witness))
   (define loaded-entry (load path))
   (define selected
     (if (hash-has-key? loaded-entry 'failure)
       loaded-entry
       (with-handlers
        ((exn:fail? (lambda (failure) (hash 'failure (failure-observation failure)))))
        (hash
         'body
         (if (equal? name "Typify")
           (begin
             (unless meta (set! meta (hash-ref (read-declarations (meta-source)) "Typify")))
             meta)
           (hash-ref
            (hash-ref loaded-entry 'bodies)
            name
            (lambda ()
              (raise-typify
               "E_USAGE"
               "The language is not declared"
               #:data
               (hash 'actual name)))))))))
   (define body (hash-ref selected 'body #f))
   (define actual-candidates
     (and body
          (with-handlers
           ((exn:fail? (lambda (failure) (failure-observation failure))))
           (sort (question-candidates body) string<?))))
   (define expected-candidates (sort (hash-ref entry 'candidates) string<?))
   (unless (equal? actual-candidates expected-candidates)
     (define failure
       (hash
        'path
        path
        'language
        name
        'kind
        "candidates"
        'expected
        expected-candidates
        'actual
        actual-candidates))
     (set! failures (cons failure failures))
     (printf "ask-gate failure ~a\n" (canonical-json failure)))
   (when (and (hash? actual-candidates) (hash-ref actual-candidates 'infrastructure #f))
     (set! infrastructure
       (cons
        (hash
         'path
         path
         'language
         name
         'kind
         "candidates"
         'error
         (hash-ref actual-candidates 'infrastructure))
        infrastructure)))
   (for
    ((row (in-list (hash-ref entry 'words))))
    (set! words (add1 words))
    (define word (hash-ref row 'word))
    (define actual
      (with-handlers
       ((exn:fail? failure-observation))
       (if body
         (hash
          'exit_code
          0
          'stdout_json
          (ask body word #:diagnostics (hash-ref loaded-entry 'diagnostics))
          'infrastructure
          #f)
         (hash-ref selected 'failure))))
    (define exit-code (hash-ref actual 'exit_code))
    (define answer (hash-ref actual 'stdout_json))
    (when (hash-ref actual 'infrastructure)
      (set! infrastructure
        (cons
         (hash
          'path
          path
          'language
          name
          'word
          word
          'set
          (hash-ref row 'set)
          'error
          (hash-ref actual 'infrastructure))
         infrastructure)))
    (define expected (hash-ref row 'stdout_json))
    (define comparison-error #f)
    (define same-answer?
      (with-handlers
       ((exn:fail? (lambda (failure) (set! comparison-error (exn-message failure)) #f)))
       (equal?
        (canonical-json (answer-comparison answer))
        (canonical-json (answer-comparison expected)))))
    (unless (and (equal? exit-code (hash-ref row 'exit_code)) same-answer?)
      (define failure
        (hash
         'path
         path
         'language
         name
         'word
         word
         'set
         (hash-ref row 'set)
         'expected
         (hash 'exit_code (hash-ref row 'exit_code) 'stdout_json expected)
         'actual
         actual
         'comparison-error
         (or comparison-error 'null)))
      (set! failures (cons failure failures))
      (printf "ask-gate failure ~a\n" (canonical-json failure)))
    (define roundtrip
      (if (hash-ref actual 'infrastructure)
        (hash 'checks 0 'failures '())
        (with-handlers
         ((exn:fail?
           (lambda (failure)
             (hash
              'checks
              1
              'failures
              (list (hash 'check "roundtrip-host-error" 'error (exn-message failure)))))))
         (answer-round-trip answer))))
    (set! roundtrip-checks (+ roundtrip-checks (hash-ref roundtrip 'checks)))
    (for
     ((difference (in-list (hash-ref roundtrip 'failures))))
     (define failure
       (hash-set* difference 'path path 'language name 'word word 'set (hash-ref row 'set)))
     (set! roundtrip-failures (cons failure roundtrip-failures))
     (printf "ask-gate roundtrip failure ~a\n" (canonical-json failure))))
   (set! declarations
     (cons
      (hash 'path path 'language name 'sizes sizes 'words (length (hash-ref entry 'words)))
      declarations)))
  (hash
   'complete
   #t
   'total
   words
   'candidate-checks
   (length declarations)
   'failures
   (length failures)
   'mismatches
   (reverse failures)
   'roundtrip-checks
   roundtrip-checks
   'roundtrip-failures
   (length roundtrip-failures)
   'roundtrip-mismatches
   (reverse roundtrip-failures)
   'infrastructure-failures
   (length infrastructure)
   'infrastructure-errors
   (reverse infrastructure)
   'declarations
   (reverse declarations)
   'parsed-files
   (count (lambda (entry) (hash-has-key? entry 'bodies)) (hash-values loaded))
   'load-attempts
   (hash-count loaded)
   'reference-executed
   #f
   'comparison-policy
   "Exact objects and ordered lists except diagnostics, compared by code,file,range and registry-required data; raw expected and native objects retained on every mismatch"))

(define (run-ask-gate #:root (root default-gate-root))
  (define receipt (verify-ask-gate root))
  (define data (call-with-input-file (build-path root "gate.json") read-json))
  (define result (replay-gate data (lambda (path) (file->string (build-path root path)))))
  (hash-set*
   result
   'blind
   #t
   'independent
   #t
   'corpus
   "test/ask-gate/"
   'provenance
   "Frozen external reference answers; reference source never executed"
   'archive-sha256
   (hash-ref (hash-ref receipt 'provenance) 'sha256)
   'tree-sha256
   (hash-ref receipt 'tree-sha256)))

(define (run-tests #:blind? (blind? #t) #:stage (stage 3) #:gate-provider? (gate-provider? #f))
  (define checks 0)
  (define (check actual expected description)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-ask-gate "~a: expected ~s; received ~s" description expected actual)))
  (define (unknown word (near '()))
    (hash
     'word
     word
     'status
     "undeclared"
     'lines
     (cons
      (string-append "? : " word " = undeclared")
      (map (lambda (item) (string-append "? : near : " item)) near))
     'near
     near
     'accepts
     '()
     'diagnostics
     '()))
  (define source
    "#lang typify\ndeclare : Gate\nis : payload = \"red\"\nterm : emit : payload\nform : say : <payload>\ndoes : emit <payload>\nend!\n")
  (define entry
    (hash
     'path
     "declarations/gate.typify"
     'language
     "Gate"
     'candidates
     '("emit" "say")
     'sizes
     (hash 'typo 1 'unknown 1 'witness 1)
     'words
     (list
      (hash 'word "sax" 'set "typo" 'exit_code 0 'stdout_json (unknown "sax" '("say")))
      (hash 'word "zzzz" 'set "unknown" 'exit_code 0 'stdout_json (unknown "zzzz"))
      (hash 'word "lost" 'set "witness" 'exit_code 0 'stdout_json (unknown "lost")))))
  (define loads 0)
  (define synthetic
    (replay-gate
     (hash 'declarations (list entry entry))
     (lambda (_) (set! loads (add1 loads)) source)))
  (check loads 1 "a declaration is parsed once even when more than one entry uses it")
  (check (hash-ref synthetic 'total) 6 "every frozen word is replayed")
  (check (hash-ref synthetic 'failures) 0 "hand-established synthetic answers compare exactly")
  (check (hash-ref synthetic 'roundtrip-failures) 0 "synthetic answer lines round trip")
  (check (hash-ref synthetic 'reference-executed) #f "the reference source is data")
  (define broken
    (hash-set
     entry
     'words
     (map (lambda (row) (hash-set row 'stdout_json (unknown "wrong"))) (hash-ref entry 'words))))
  (define mismatches (replay-gate (hash 'declarations (list broken)) (lambda (_) source)))
  (check (hash-ref mismatches 'total) 3 "comparison failures do not stop later words")
  (check (hash-ref mismatches 'failures) 3 "every mismatching object is retained")
  (check (length (hash-ref mismatches 'mismatches)) 3 "all full failures are available")
  (define refusal-diagnostic
    (hash
     'code
     "E_USAGE"
     'file
     ""
     'range
     'null
     'data
     (hash 'actual "bad_name")
     'severity
     "different"
     'message
     "independent reference prose"
     'related
     '("reference-only")
     'fix
     "ignored-reference-field"
     'fingerprints
     (hash 'reference "different")))
  (define refusal-answer (hash 'ok #f 'diagnostics (list refusal-diagnostic)))
  (define refusal-entry
    (hash-set*
     entry
     'sizes
     (hash 'typo 0 'unknown 1 'witness 0)
     'words
     (list (hash 'word "bad_name" 'set "unknown" 'exit_code 2 'stdout_json refusal-answer))))
  (define refusal-result
    (replay-gate (hash 'declarations (list refusal-entry)) (lambda (_) source)))
  (check
   (hash-ref refusal-result 'failures)
   0
   "refusal diagnostics ignore independent reference prose and native extras")
  (define changed-prose
    (replay-gate
     (hash 'declarations (list refusal-entry))
     (lambda (_) source)
     #:ask
     (lambda (_body word #:diagnostics _diagnostics)
       (raise-typify "E_USAGE" "A different native message" #:data (hash 'actual word)))))
  (check
   (hash-ref changed-prose 'failures)
   0
   "changing native prose and its fingerprint preserves comparison")
  (define (refusal-with diagnostic)
    (hash-set
     refusal-entry
     'words
     (list
      (hash
       'word
       "bad_name"
       'set
       "unknown"
       'exit_code
       2
       'stdout_json
       (hash 'ok #f 'diagnostics (list diagnostic))))))
  (define wrong-code
    (replay-gate
     (hash
      'declarations
      (list
       (refusal-with (hash-set* refusal-diagnostic 'code "E_FILE" 'data (hash 'path "bad_name")))))
     (lambda (_) source)))
  (check (hash-ref wrong-code 'failures) 1 "the diagnostic code remains exact")
  (define wrong-data
    (replay-gate
     (hash
      'declarations
      (list (refusal-with (hash-set refusal-diagnostic 'data (hash 'actual "another_name")))))
     (lambda (_) source)))
  (check (hash-ref wrong-data 'failures) 1 "registered diagnostic data remains exact")
  (define wrong-range
    (replay-gate
     (hash
      'declarations
      (list
       (refusal-with
        (hash-set
         refusal-diagnostic
         'range
         (hash
          'start
          (hash 'line 1 'column 1 'byteOffset 0)
          'end
          (hash 'line 1 'column 2 'byteOffset 1))))))
     (lambda (_) source)))
  (check (hash-ref wrong-range 'failures) 1 "the diagnostic range remains exact")
  (define missing-data
    (replay-gate
     (hash 'declarations (list (refusal-with (hash-set refusal-diagnostic 'data (hash)))))
     (lambda (_) source)))
  (check
   (hash-ref missing-data 'failures)
   1
   "a missing registered data field is an explicit failure")
  (define raw-failure (car (hash-ref wrong-data 'mismatches)))
  (check
   (hash-ref
    (car (hash-ref (hash-ref (hash-ref raw-failure 'expected) 'stdout_json) 'diagnostics))
    'message)
   "independent reference prose"
   "a failure retains the full raw expected diagnostic")
  (check
   (hash-has-key?
    (car (hash-ref (hash-ref (hash-ref raw-failure 'actual) 'stdout_json) 'diagnostics))
    'fingerprints)
   #t
   "a failure retains the full raw native diagnostic")
  (define wrong-envelope
    (replay-gate
     (hash
      'declarations
      (list
       (hash-set
        refusal-entry
        'words
        (list
         (hash
          'word
          "bad_name"
          'set
          "unknown"
          'exit_code
          2
          'stdout_json
          (hash-set refusal-answer 'ok #t))))))
     (lambda (_) source)))
  (check (hash-ref wrong-envelope 'failures) 1 "non-diagnostic answer fields stay exact")
  (define note-reference
    (hash-set
     (unknown "lost")
     'diagnostics
     (list
      (hash
       'code
       "N_CLASS_EMPTY"
       'file
       "declarations/gate.typify"
       'range
       'null
       'data
       (hash 'class "unused")
       'message
       "reference explanation"))))
  (define note-native
    (hash-set
     (unknown "lost")
     'diagnostics
     (list
      (hash
       'code
       "N_CLASS_EMPTY"
       'file
       "declarations/gate.typify"
       'range
       'null
       'data
       (hash 'class "unused")
       'message
       "native explanation"
       'severity
       "note"))))
  (check
   (canonical-json (answer-comparison note-native))
   (canonical-json (answer-comparison note-reference))
   "declaration notes use the same registry-based diagnostic projection")
  (define first-word-failure
    (replay-gate
     (hash 'declarations (list entry))
     (lambda (_) source)
     #:ask
     (lambda (body word #:diagnostics diagnostics)
       (when (equal? word "sax") (error 'synthetic-host "injected first-word failure"))
       (ask-language body word #:diagnostics diagnostics))))
  (check (hash-ref first-word-failure 'total) 3 "a host error does not stop later words")
  (check (hash-ref first-word-failure 'failures) 1 "a host error is one failed comparison")
  (check
   (hash-ref first-word-failure 'infrastructure-failures)
   1
   "a host error stays infrastructure")
  (define second-entry (hash-set entry 'path "declarations/second.typify"))
  (define failed-declaration
    (replay-gate
     (hash 'declarations (list entry second-entry))
     (lambda (path)
       (if (equal? path "declarations/gate.typify")
         (error 'synthetic-host "injected declaration load failure")
         source))))
  (check (hash-ref failed-declaration 'total) 6 "a failed declaration retains every requested word")
  (check (hash-ref failed-declaration 'parsed-files) 1 "a later declaration still parses")
  (check
   (hash-ref failed-declaration 'load-attempts)
   2
   "each failed or successful path is loaded once")
  (check
   (hash-ref failed-declaration 'infrastructure-failures)
   3
   "each word of the failed declaration retains its host failure")
  (define refused-declaration
    (replay-gate
     (hash 'declarations (list entry second-entry))
     (lambda (path)
       (if (equal? path "declarations/gate.typify") "#lang typify\ndeclare : Gate\n" source))))
  (check
   (hash-ref refused-declaration 'total)
   6
   "a native declaration refusal does not stop the next body")
  (check
   (hash-ref refused-declaration 'infrastructure-failures)
   0
   "a native declaration refusal is not mislabeled as infrastructure")
  (check
   (for/and
    ((failure (in-list (hash-ref refused-declaration 'mismatches)))
     #:when
     (hash-has-key? failure 'word))
    (define actual (hash-ref failure 'actual))
    (and (equal? (hash-ref actual 'exit_code) 1)
         (pair? (hash-ref (hash-ref actual 'stdout_json) 'diagnostics))))
   #t
   "each native refusal retains its actual diagnostics and exit status")
  (define malformed (hash-set (unknown "lost") 'lines '("? : lost = undeclared" "near : say")))
  (check
   (hash-ref (answer-round-trip malformed) 'failures)
   (list (hash 'check "line/1" 'expected #t 'actual #f))
   "a line outside the reserved question verb fails the round trip")
  (define swapped (hash-set (unknown "lost" '("say" "emit")) 'near '("emit" "say")))
  (check
   (length (hash-ref (answer-round-trip swapped) 'failures))
   1
   "the rebuilt near lines preserve list order")
  (define invalid-size (hash-set entry 'sizes (hash 'typo 2 'unknown 1 'witness 1)))
  (check
   (with-handlers
    ((exn:fail? (lambda (_) #t)))
    (validate-gate (hash 'declarations (list invalid-size)))
    #f)
   #t
   "a mislabeled set size is refused")
  (define measured (and blind? (>= stage 3) (run-ask-gate)))
  (define deferred
    (hash
     'total
     0
     'failures
     0
     'complete
     #f
     'status
     "deferred-blind"
     'roundtrip-checks
     0
     'roundtrip-failures
     0
     'reference-executed
     #f))
  (define evidence (or measured deferred))
  (define replay-count (if measured (hash-ref measured 'total) 0))
  (define roundtrip-count (if measured (hash-ref measured 'roundtrip-checks) 0))
  (define failures
    (if measured (+ (hash-ref measured 'failures) (hash-ref measured 'roundtrip-failures)) 0))
  (define count
    (+ checks replay-count roundtrip-count (if measured (hash-ref measured 'candidate-checks) 0)))
  (hash
   'suite
   "ask-gate"
   'acceptance-stage
   stage
   'count
   count
   'pass
   (- count failures)
   'fail
   failures
   'status
   (if (zero? failures) "passed" "failed")
   'blind-deferred
   (not measured)
   'measurement
   evidence
   'follow-on-measurements
   "Prefix sweep and dialogue pilot deferred; neither is an acceptance condition"
   'gates
   (if gate-provider?
     (hash
      'ask-exact-replay
      evidence
      'ask-round-trip
      (hash-set*
       evidence
       'total
       (hash-ref evidence 'roundtrip-checks)
       'failures
       (hash-ref evidence 'roundtrip-failures)))
     (hash))))

(module+ test (run-tests #:blind? #f))
