#lang racket/base

(require file/sha1
         json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/main.rkt"
         (only-in "test-decoder.rkt" parse-gbnf gbnf-accepts?))

(provide run-blind-corpus
         blind-command-scope
         blind-stage-commands
         blind-stage-scope
         blind-case-inventory
         blind-declaration-inputs
         default-blind-root
         run-tests)

(define-runtime-path default-blind-root "corpus")

(define archive-sha256 "ece3d4d5e94e427c7807a0e558fa46772525aecddd2167bcf5df04ed47e7bece")

(define tree-sha256 "4a4cbdbc01e115a50db35c91361c6ed747fa11f14425f52b50d441cf90561887")

(define receipt-sha256 "e0b56962c8b8c98765dd990fe9bbe3bdf6524193f64436c38efec2621bfe6ba6")

(define missing (gensym 'missing))

(define missing-json (hash '$missing #t))

(define (read-data path) (call-with-input-file path read-json))

(define (digest path)
  (call-with-input-file path (lambda (input) (bytes->hex-string (sha256-bytes input)))))

(define (field value key) (if (hash? value) (hash-ref value key missing) missing))

(define (visible value) (if (eq? value missing) missing-json value))

(define (case-directories root)
  (if (directory-exists? root)
    (for/list
     ((name (in-list (sort (directory-list root) string<? #:key path->string)))
      #:when
      (and (regexp-match? #px"^[0-9]{3}-" (path->string name))
           (directory-exists? (build-path root name))))
     (build-path root name))
    '()))

(define (blind-case-inventory (root default-blind-root))
  (define receipt-path (build-path root "import.json"))
  (define receipt (and (file-exists? receipt-path) (load-receipt root)))
  (define directories
    (if receipt
      (for/list ((name (in-list (hash-ref receipt 'cases)))) (build-path root name))
      (case-directories root)))
  (append-map
   (lambda (directory)
     (define fallback-id (path->string (file-name-from-path directory)))
     (with-handlers
      ((exn:fail?
        (lambda (error)
          (list
           (hash
            'id
            fallback-id
            'case-id
            fallback-id
            'subcase
            'null
            'directory
            directory
            'expected
            (hash)
            'case
            (hash)
            'inventory-error
            (exn-message error))))))
      (safe-path root fallback-id)
      (define expected-path (build-path directory "expected.json"))
      (when receipt
        (define relative (string-append fallback-id "/expected.json"))
        (define sealed
          (for/first
           ((item (in-list (hash-ref receipt 'files)))
            #:when
            (equal? (hash-ref item 'path) relative))
           item))
        (unless (and
                 sealed
                 (file-exists? expected-path)
                 (not (link-exists? expected-path))
                 (= (file-size expected-path) (hash-ref sealed 'bytes))
                 (equal? (digest expected-path) (hash-ref sealed 'sha256)))
          (error 'blind-corpus "A case's sealed expected.json is missing or changed")))
      (define data (read-data (build-path directory "expected.json")))
      (define case-id (hash-ref data 'case_id))
      (unless (equal? case-id fallback-id)
        (error 'blind-corpus "A case ID differs from its physical directory name"))
      (define (entry expectation subcase)
        (hash
         'id
         (if (eq? subcase 'null) case-id (string-append case-id "/" subcase))
         'case-id
         case-id
         'subcase
         subcase
         'directory
         directory
         'expected
         expectation
         'case
         data))
      (append
       (if (hash-has-key? data 'invocation) (list (entry data 'null)) '())
       (for/list
        ((subcase (in-list (hash-ref data 'subcases '()))))
        (entry
         (if (and (hash-has-key? data 'diagnostics) (not (hash-has-key? subcase 'diagnostics)))
           (hash-set subcase 'diagnostics (hash-ref data 'diagnostics))
           subcase)
         (hash-ref subcase 'name))))))
   directories))

(define (blind-declaration-inputs (root default-blind-root))
  (define seen (make-hash))
  (define receipt (load-receipt root))
  (for/fold
   ((inputs '()) #:result (reverse inputs))
   ((entry (in-list (blind-case-inventory root))))
   (define invocation (hash-ref (hash-ref entry 'expected) 'invocation '()))
   (cond
    ((and (>= (length invocation) 3)
          (member (cadr invocation) '("check" "tree" "plan" "ask" "emit"))
          (not (equal? (caddr invocation) "Typify")))
     (define file (caddr invocation))
     (define id (string-append (hash-ref entry 'case-id) "/" file))
     (cond
      ((hash-has-key? seen id) inputs)
      (else
       (hash-set! seen id #t)
       (define path (build-path (hash-ref entry 'directory) file))
       (verify-case-inputs! root receipt (hash-ref entry 'case-id))
       (cons
        (hash
         'id
         id
         'case-id
         (hash-ref entry 'case-id)
         'file
         file
         'directory
         (hash-ref entry 'directory)
         'source
         (if (file-exists? path) (file->string path) #f))
        inputs))))
    (else inputs))))

(define (safe-path root relative)
  (unless (and
           (string? relative)
           (positive? (string-length relative))
           (relative-path? (string->path relative))
           (not (string-contains? relative "\\"))
           (andmap
            (lambda (part) (not (member part '("" "." ".."))))
            (string-split relative "/" #:trim? #f)))
    (error 'blind-corpus "The receipt contains a nonportable relative path"))
  (for/fold
   ((path root))
   ((part (in-list (string-split relative "/"))))
   (define next (build-path path part))
   (when (link-exists? next) (error 'blind-corpus "A corpus input resolves through a symlink"))
   next))

(define (load-receipt root)
  (define path (build-path root "import.json"))
  (unless (and (file-exists? path) (not (link-exists? path)) (equal? (digest path) receipt-sha256))
    (error 'blind-corpus "The import receipt is absent or differs from the frozen receipt"))
  (define receipt (read-data path))
  (unless (and
           (equal? (hash-ref receipt 'schema #f) "typify-corpus-import-v2")
           (eq? (hash-ref receipt 'verbatim #f) #t)
           (equal? (hash-ref receipt 'tree-sha256 #f) tree-sha256)
           (equal? (hash-ref (hash-ref receipt 'provenance (hash)) 'sha256 #f) archive-sha256)
           (= (length (hash-ref receipt 'cases '())) 100))
    (error 'blind-corpus "The frozen receipt's custody metadata does not match"))
  (for
   ((entry (in-list (hash-ref receipt 'files)))
    #:unless
    (regexp-match? #px"^[0-9]{3}-[^/]+/" (hash-ref entry 'path)))
   (define path (safe-path root (hash-ref entry 'path)))
   (unless (and
            (file-exists? path)
            (= (file-size path) (hash-ref entry 'bytes))
            (equal? (digest path) (hash-ref entry 'sha256)))
     (error 'blind-corpus "The frozen root metadata is missing or has changed")))
  (define allowed
    (append
     '("import.json" "hostile.json")
     (hash-ref receipt 'cases)
     (hash-ref receipt 'metadata '())))
  (for
   ((entry (in-list (directory-list root))))
   (unless (member (path->string entry) allowed)
     (error 'blind-corpus "The corpus root contains an unexpected input")))
  receipt)

(define (verify-case-inputs! root receipt case-id)
  (define prefix (string-append case-id "/"))
  (define directories
    (for/hash
     ((relative (in-list (hash-ref receipt 'directories)))
      #:when
      (or (equal? relative case-id) (string-prefix? relative prefix)))
     (values relative #t)))
  (for
   ((relative (in-list (hash-ref receipt 'directories)))
    #:when
    (or (equal? relative case-id) (string-prefix? relative prefix)))
   (define path (safe-path root relative))
   (when (file-exists? path) (error 'blind-corpus "A receipt directory was replaced by a file"))
   (make-directory* path))
  (define listed
    (for/hash
     ((entry (in-list (hash-ref receipt 'files)))
      #:when
      (string-prefix? (hash-ref entry 'path) prefix))
     (define relative (hash-ref entry 'path))
     (define path (safe-path root relative))
     (unless (and
              (file-exists? path)
              (= (file-size path) (hash-ref entry 'bytes))
              (equal? (digest path) (hash-ref entry 'sha256)))
       (error 'blind-corpus "A frozen case input is missing or has changed: ~a" relative))
     (values relative #t)))
  (define (walk path relative)
    (for
     ((name (in-list (directory-list path))))
     (define next-relative (string-append relative "/" (path->string name)))
     (define next (safe-path root next-relative))
     (cond
      ((directory-exists? next)
       (unless (hash-has-key? directories next-relative)
         (error 'blind-corpus "A case contains an unexpected directory: ~a" next-relative))
       (walk next next-relative))
      ((not (hash-has-key? listed next-relative))
       (error 'blind-corpus "A case contains an unexpected input: ~a" next-relative)))))
  (walk (safe-path root case-id) case-id))

(define (observe entry)
  (define invocation (hash-ref (hash-ref entry 'expected) 'invocation))
  (define arguments
    (if (and (pair? invocation) (equal? (car invocation) "typify")) (cdr invocation) invocation))
  (define output (open-output-string))
  (define errors (open-output-string))
  (define native '())
  (define exit-code 0)
  (define infrastructure #f)
  (with-handlers
   ((exn:fail? (lambda (error) (set! infrastructure (exn-message error)))))
   (parameterize
    ((current-directory (hash-ref entry 'directory))
     (current-output-port output)
     (current-error-port errors))
    (define-values (status diagnostics) (execute-command arguments))
    (set! exit-code status)
    (set! native diagnostics)))
  (define stdout (get-output-string output))
  (define parsed
    (with-handlers
     ((exn:fail? (lambda (error) missing)))
     (if (string=? stdout "") missing (string->jsexpr stdout))))
  (hash
   'exit_code
   (if infrastructure missing exit-code)
   'verdict
   (if infrastructure
     "infrastructure"
     (case exit-code ((0) "accepted") ((1) "refused") (else "usage_or_io_error")))
   'stdout_json
   parsed
   'stdout_gbnf
   stdout
   'stdout
   stdout
   'stderr
   (get-output-string errors)
   'diagnostics
   native
   'infrastructure
   infrastructure))

(define (pointer path)
  (string-append
   "/"
   (string-join
    (map
     (lambda (part) (string-replace (string-replace (format "~a" part) "~" "~0") "/" "~1"))
     path)
    "/")))

(define (compare-observation
         expected
         actual
         #:id
         (id "synthetic")
         #:case-id
         (case-id id)
         #:subcase
         (subcase 'null))
  (define differences '())
  (define (difference path wanted got reason)
    (set! differences
      (cons
       (hash
        'id
        id
        'case-id
        case-id
        'subcase
        subcase
        'path
        (pointer path)
        'expected
        (visible wanted)
        'actual
        (visible got)
        'reason
        reason
        'missing
        (cond
         ((and (eq? wanted missing) (eq? got missing)) "both")
         ((eq? wanted missing) "expected")
         ((eq? got missing) "actual")
         (else "none")))
       differences)))
  (define (diagnostic-at? path)
    (and (>= (length path) 2)
         (exact-nonnegative-integer? (last path))
         (equal? (list-ref path (- (length path) 2)) "diagnostics")))
  (define (compare-diagnostic wanted got path)
    (for
     ((key '(code file range)))
     (compare (field wanted key) (field got key) (append path (list (symbol->string key)))))
    (define code (field wanted 'code))
    (define fields (hash-ref diagnostic-data-fields code #f))
    (unless fields
      (difference (append path '("code")) code (field got 'code) "unregistered-expected-code"))
    (define required
      (or fields (if (hash? (field wanted 'data)) (hash-keys (field wanted 'data)) '())))
    (for
     ((key (in-list required)))
     (define wanted-field (field (field wanted 'data) key))
     (define got-field (field (field got 'data) key))
     (if (or (eq? wanted-field missing) (eq? got-field missing))
       (difference
        (append path (list "data" (symbol->string key)))
        wanted-field
        got-field
        "required-diagnostic-field")
       (compare wanted-field got-field (append path (list "data" (symbol->string key))))))
    (define kinds (hash-ref diagnostic-kinds code #f))
    (define kind (field (field wanted 'data) 'kind))
    (when (and kinds (not (eq? kind missing)) (not (member kind kinds)))
      (difference
       (append path '("data" "kind"))
       kind
       (field (field got 'data) 'kind)
       "unregistered-expected-kind")))
  (define (compare wanted got path)
    (cond
     ((diagnostic-at? path) (compare-diagnostic wanted got path))
     ((and (eq? wanted missing) (eq? got missing)) (difference path wanted got "missing-both"))
     ((hash? wanted)
      (unless (or (hash? got) (eq? got missing)) (difference path wanted got "object-type"))
      (when (and (eq? got missing) (zero? (hash-count wanted)))
        (difference path wanted got "missing-object"))
      (for
       ((key (in-list (sort (hash-keys wanted) symbol<?))))
       (compare (hash-ref wanted key) (field got key) (append path (list (symbol->string key)))))
      (when (hash? got)
        (for
         ((key (in-list (sort (hash-keys got) symbol<?))) #:unless (hash-has-key? wanted key))
         (difference
          (append path (list (symbol->string key)))
          missing
          (hash-ref got key)
          "unexpected-field"))))
     ((list? wanted)
      (unless (and (list? got) (= (length wanted) (length got)))
        (difference
         (append path '("length"))
         (length wanted)
         (if (list? got) (length got) got)
         "list-length"))
      (for
       ((item (in-list wanted)) (index (in-naturals)))
       (compare
        item
        (if (and (list? got) (< index (length got))) (list-ref got index) missing)
        (append path (list index))))
      (when (list? got)
        (for
         ((index (in-range (length wanted) (length got))))
         (difference (append path (list index)) missing (list-ref got index) "unexpected-item"))))
     ((not (equal? wanted got)) (difference path wanted got "value"))))
  (for
   ((key '(exit_code verdict stdout_json stdout_gbnf diagnostics))
    #:when
    (hash-has-key? expected key))
   (compare (hash-ref expected key) (field actual key) (list (symbol->string key))))
  (define json-output (field actual 'stdout_json))
  (for
   ((mapping
     '((terms terms)
       (tree_entries self)
       (ask_lines lines)
       (ask_json self)
       (plan plan)
       (predicted_state state)
       (bound bound)
       (modelled modelled)
       (ensure ensure)
       (skips skips)
       (facts_hash facts)
       (programs programs))))
   (when (hash-has-key? expected (car mapping))
     (compare
      (hash-ref expected (car mapping))
      (if (eq? (cadr mapping) 'self) json-output (field json-output (cadr mapping)))
      (list (symbol->string (car mapping))))))
  (when (hash-has-key? expected 'gbnf_assertions)
    (define assertions (hash-ref expected 'gbnf_assertions))
    (define path '("gbnf_assertions"))
    (define source (field actual 'stdout_gbnf))
    (cond
     ((not (hash? assertions)) (difference path assertions missing "invalid-gbnf-assertions"))
     (else
      (for
       ((key (in-list (sort (hash-keys assertions) symbol<?)))
        #:unless
        (memq key '(required_nonterminals accepts_lines rejects_lines schematic_note)))
       (difference
        (append path (list (symbol->string key)))
        (hash-ref assertions key)
        missing
        "unsupported-gbnf-assertion"))
      (cond
       ((not (string? source)) (difference path assertions source "missing-gbnf-output"))
       (else
        (with-handlers
         ((exn:fail?
           (lambda (failure)
             (difference
              path
              assertions
              (hash 'source source 'error (exn-message failure))
              "gbnf-parse-error"))))
         (define grammar (parse-gbnf source))
         (define names
           (for/list
            ((line (in-list (string-split source "\n" #:trim? #f)))
             #:do
             ((define header (regexp-match #px"^([A-Za-z0-9-]+)[ \t]*::=" (string-trim line))))
             #:when
             header)
            (cadr header)))
         (for
          ((key '(required_nonterminals accepts_lines rejects_lines))
           #:when
           (hash-has-key? assertions key))
          (define probes (hash-ref assertions key))
          (cond
           ((not (and (list? probes) (andmap string? probes)))
            (difference
             (append path (list (symbol->string key)))
             probes
             missing
             "invalid-gbnf-assertions"))
           (else
            (for
             ((probe (in-list probes)) (index (in-naturals)))
             (define probe-path (append path (list (symbol->string key) index)))
             (cond
              ((eq? key 'required_nonterminals)
               (unless (member probe names)
                 (difference probe-path probe missing "missing-nonterminal")))
              (else
               (define accepted? (gbnf-accepts? grammar probe))
               (unless (equal? accepted? (eq? key 'accepts_lines))
                 (difference
                  probe-path
                  probe
                  (hash 'line probe 'accepted accepted?)
                  (if accepted? "gbnf-line-accepted" "gbnf-line-refused")))))))))))))))
  (for
   ((key '(lsp_range_utf16_zero_based sarif_projection)) #:when (hash-has-key? expected key))
   (difference
    (list (symbol->string key))
    (hash-ref expected key)
    missing
    "unsupported-projection"))
  (reverse differences))

(define (expected-stage entry)
  (define expected (hash-ref entry 'expected))
  (define diagnostics
    (append
     (hash-ref expected 'diagnostics '())
     (let ((output (hash-ref expected 'stdout_json #f)))
       (if (hash? output) (hash-ref output 'diagnostics '()) '()))))
  (define (model-diagnostic? item)
    (or (member
         (hash-ref item 'code "")
         '("E_JOIN_LAW" "E_KEY_CANONICAL" "E_ENSURE" "E_FACT_REJECT"))
        (equal? (hash-ref (hash-ref item 'data (hash)) 'kind #f) "effect")
        (let* ((file (hash-ref item 'file #f))
               (range (hash-ref item 'range #f))
               (line (and (hash? range) (hash-ref (hash-ref range 'start (hash)) 'line #f))))
          (and (string? file)
               (exact-positive-integer? line)
               (let ((path (safe-path (hash-ref entry 'directory) file)))
                 (and (file-exists? path)
                      (let ((lines (regexp-split #rx"\r?\n" (file->string path))))
                        (and (<= line (length lines))
                             (regexp-match?
                              #px"^[ \t]*(?:cell|writes|join|reads|ensure|effect)[ \t]*:"
                              (list-ref lines (sub1 line)))))))))))
  (cond
   ((or (hash-has-key? expected 'sarif_projection)
        (hash-has-key? expected 'lsp_range_utf16_zero_based))
    4)
   ((ormap model-diagnostic? diagnostics) 2)
   (else 1)))

(define (run-blind-corpus
         #:root
         (root default-blind-root)
         #:ids
         (ids #f)
         #:stop-on-failure?
         (stop-on-failure? #f))
  (define setup-error #f)
  (define receipt
    (with-handlers
     ((exn:fail? (lambda (error) (set! setup-error (exn-message error)) #f)))
     (load-receipt root)))
  (define inventory
    (with-handlers
     ((exn:fail? (lambda (error) (set! setup-error (exn-message error)) '())))
     (blind-case-inventory root)))
  (define selected
    (if ids
      (append
       (filter (lambda (entry) (member (hash-ref entry 'id) ids)) inventory)
       (for/list
        ((id (in-list ids))
         #:unless
         (ormap (lambda (entry) (equal? (hash-ref entry 'id) id)) inventory))
        (hash
         'id
         id
         'case-id
         id
         'subcase
         'null
         'directory
         root
         'expected
         (hash)
         'inventory-error
         "The requested invocation ID is absent from the frozen inventory")))
      inventory))
  (define results '())
  (define all-differences '())
  (define case-results (make-hash))
  (define infrastructure-count 0)
  (for
   ((entry (in-list selected))
    #:break
    (and stop-on-failure? (pair? results) (not (hash-ref (car results) 'pass))))
   (define case-id (hash-ref entry 'case-id))
   (define id (hash-ref entry 'id))
   (define subcase (hash-ref entry 'subcase))
   (define expected (hash-ref entry 'expected))
   (define actual
     (with-handlers
      ((exn:fail? (lambda (error) (hash 'infrastructure (exn-message error)))))
      (when setup-error (error 'blind-corpus "~a" setup-error))
      (when (hash-has-key? entry 'inventory-error)
        (error 'blind-corpus "~a" (hash-ref entry 'inventory-error)))
      (verify-case-inputs! root receipt case-id)
      (observe entry)))
   (define infrastructure (hash-ref actual 'infrastructure #f))
   (define differences
     (compare-observation expected actual #:id id #:case-id case-id #:subcase subcase))
   (when infrastructure
     (set! infrastructure-count (add1 infrastructure-count))
     (set! differences
       (cons
        (hash
         'id
         id
         'case-id
         case-id
         'subcase
         subcase
         'path
         "/infrastructure"
         'expected
         (hash 'execution "native-cli")
         'actual
         infrastructure
         'reason
         "infrastructure"
         'missing
         "none")
        differences)))
   (define passed? (null? differences))
   (hash-update! case-results case-id (lambda (prior) (and prior passed?)) #t)
   (set! results
     (cons
      (hash
       'id
       id
       'case-id
       case-id
       'subcase
       subcase
       'pass
       passed?
       'command
       (let ((invocation (hash-ref expected 'invocation '())))
         (if (>= (length invocation) 2) (cadr invocation) "unknown"))
       'stage
       (expected-stage entry)
       'kind
       (if infrastructure "infrastructure" "compared")
       'expected
       (hash
        'exit_code
        (visible (field expected 'exit_code))
        'verdict
        (visible (field expected 'verdict)))
       'actual
       (hash
        'exit_code
        (visible (field actual 'exit_code))
        'verdict
        (visible (field actual 'verdict))
        'diagnostic-codes
        (map (lambda (item) (hash-ref item 'code)) (hash-ref actual 'diagnostics '())))
       'mismatch-count
       (length differences))
      results))
   (set! all-differences (append all-differences differences)))
  (define requested (length selected))
  (define attempted (length results))
  (when (and setup-error (zero? attempted))
    (set! infrastructure-count (add1 infrastructure-count))
    (set! all-differences
      (list
       (hash
        'id
        "inventory"
        'case-id
        "inventory"
        'subcase
        'null
        'path
        "/infrastructure"
        'expected
        (hash 'cases 100 'custody "frozen-receipt")
        'actual
        setup-error
        'reason
        "infrastructure"
        'missing
        "none"))))
  (define selected-complete?
    (and (= attempted requested) (not setup-error) (zero? infrastructure-count)))
  (define complete? (and (not ids) selected-complete? (= (hash-count case-results) 100)))
  (define failures (count (lambda (result) (not (hash-ref result 'pass))) results))
  (hash
   'available
   (and receipt #t)
   'blind
   (and receipt #t)
   'independent
   (and receipt #t)
   'corpus
   "test/corpus/"
   'provenance
   "Frozen external blind archive, imported verbatim and checked against its receipt"
   'archive-sha256
   archive-sha256
   'tree-sha256
   tree-sha256
   'receipt-sha256
   receipt-sha256
   'total
   attempted
   'attempted
   attempted
   'requested
   requested
   'failures
   failures
   'case-total
   (hash-count case-results)
   'case-failures
   (count not (hash-values case-results))
   'corpus-case-total
   (length (case-directories root))
   'complete
   complete?
   'selected-complete
   selected-complete?
   'stopped-early
   (< attempted requested)
   'infrastructure-failures
   infrastructure-count
   'setup-error
   (or setup-error 'null)
   'comparison-policy
   (hash
    'semantic-objects
    "exact-key-sets"
    'lists
    "ordered"
    'gbnf
    "Exact text when stdout_gbnf is specified; independent structural assertions when gbnf_assertions is specified"
    'unexpected-bound
    "mismatch"
    'semantic-provenance-key
    "compared"
    'diagnostics
    "code,file,range and registered required data; native extras ignored"
    'missing-required-data
    "explicit mismatch with missing sentinel and discriminator"
    'unsupported-projections
    "explicit missing implementation mismatch")
   'results
   (reverse results)
   'mismatches
   all-differences))

(define (blind-stage-commands stage)
  (case stage
    ((1) '("check" "emit"))
    ((2) '("check" "emit" "tree" "plan"))
    (else (raise-argument-error 'blind-stage-commands "1 or 2" stage))))

(define (blind-stage-scope observed stage)
  (blind-command-scope observed (blind-stage-commands stage) #:stage stage))

(define (blind-command-scope observed commands #:stage (stage +inf.0))
  (define results
    (filter
     (lambda (result)
       (and (member (hash-ref result 'command "unknown") commands)
            (<= (hash-ref result 'stage 1) stage)))
     (hash-ref observed 'results '())))
  (define ids (map (lambda (result) (hash-ref result 'id)) results))
  (define cases (remove-duplicates (map (lambda (result) (hash-ref result 'case-id)) results)))
  (define failures (count (lambda (result) (not (hash-ref result 'pass))) results))
  (define complete? (hash-ref observed 'complete #f))
  (hash-set*
   observed
   'scope
   commands
   'stage-limit
   (if (equal? stage +inf.0) 'null stage)
   'full-invocations
   (hash-ref observed 'total 0)
   'full-case-total
   (hash-ref observed 'case-total 0)
   'total
   (length results)
   'attempted
   (length results)
   'requested
   (length results)
   'failures
   failures
   'case-total
   (length cases)
   'case-failures
   (for/sum
    ((case-id (in-list cases)))
    (if (for/and
         ((result (in-list results)) #:when (equal? (hash-ref result 'case-id) case-id))
         (hash-ref result 'pass))
      0
      1))
   'complete
   complete?
   'results
   results
   'mismatches
   (filter
    (lambda (difference) (member (hash-ref difference 'id) ids))
    (hash-ref observed 'mismatches '()))))

(define (run-tests)
  (define checks 0)
  (define (check actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'blind-comparison "expected ~s, got ~s" expected actual)))
  (define usage
    (observe
     (hash
      'directory
      (current-directory)
      'expected
      (hash 'invocation '("typify" "unknown-fixture" "--json")))))
  (check (hash-ref usage 'infrastructure) #f)
  (check (hash-ref usage 'exit_code) 2)
  (check (hash-ref (hash-ref usage 'stdout_json) 'ok) #f)
  (check (map (lambda (item) (hash-ref item 'code)) (hash-ref usage 'diagnostics)) '("E_USAGE"))
  (define execution-root (make-temporary-file "cli-observation~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (display-to-file
      (string-append
       "#lang typify\ndeclare : Failure\ncap : length = 64\n"
       "primitive : put : canonical-path\ncell : entries = shadow\n"
       "writes : put : entries = put 1\nensure : put : must-bind = (bound entries <1>)\n"
       "form : put : <canonical-path>\ndoes : put <canonical-path>\nend!\n")
      (build-path execution-root "declaration.typify"))
     (display-to-file "put : /example\n" (build-path execution-root "program.data"))
     (define refusal
       (observe
        (hash
         'directory
         execution-root
         'expected
         (hash
          'invocation
          '("typify" "plan" "declaration.typify" "Failure" "program.data" "--json")))))
     (check (hash-ref refusal 'infrastructure) #f)
     (check (hash-ref refusal 'exit_code) 1)
     (check (hash-ref (hash-ref refusal 'stdout_json) 'status) "failed")
     (check (length (hash-ref (hash-ref refusal 'stdout_json) 'plan)) 1)
     (check
      (map (lambda (item) (hash-ref item 'code)) (hash-ref refusal 'diagnostics))
      '("E_ENSURE")))
   (lambda () (delete-directory/files execution-root)))
  (define invocation '("typify" "check" "language.typify" "Sample" "program.typify" "--json"))
  (define count-diagnostic
    (hash
     'code
     "E_PROGRAM_COUNT"
     'file
     "program.typify"
     'range
     'null
     'data
     (hash 'constructor "vault" 'expected "0..1" 'actual 2 'key '("/a"))))
  (define count-observation (hash 'diagnostics (list count-diagnostic)))
  (check (compare-observation count-observation count-observation) '())
  (define changed-key
    (hash-set count-diagnostic 'data (hash-set (hash-ref count-diagnostic 'data) 'key '("/b"))))
  (define missing-key
    (hash-set count-diagnostic 'data (hash-remove (hash-ref count-diagnostic 'data) 'key)))
  (check (pair? (compare-observation count-observation (hash 'diagnostics (list changed-key)))) #t)
  (check (pair? (compare-observation count-observation (hash 'diagnostics (list missing-key)))) #t)
  (define diagnostic
    (hash
     'code
     "E_CLASS"
     'file
     "program.typify"
     'range
     (hash 'start 1 'end 2)
     'data
     (hash 'class "word" 'token "bad")))
  (define expected
    (hash
     'invocation
     invocation
     'exit_code
     1
     'verdict
     "refused"
     'stdout_json
     (hash 'ok #f 'terms '() 'diagnostics (list diagnostic))))
  (define actual
    (hash
     'exit_code
     1
     'verdict
     "refused"
     'stdout_json
     (hash
      'ok
      #f
      'terms
      '()
      'diagnostics
      (list
       (hash-set*
        diagnostic
        'message
        "host wording"
        'data
        (hash 'class "word" 'token "bad" 'extra 3))))))
  (check (compare-observation expected actual) '())
  (define incomplete
    (hash-set
     expected
     'stdout_json
     (hash
      'ok
      #f
      'terms
      '()
      'diagnostics
      (list (hash-set* diagnostic 'code "E_DECLARATION" 'data (hash 'actual "bad"))))))
  (define declaration-actual
    (hash
     'exit_code
     1
     'verdict
     "refused"
     'stdout_json
     (hash
      'ok
      #f
      'terms
      '()
      'diagnostics
      (list
       (hash-set* diagnostic 'code "E_DECLARATION" 'data (hash 'kind "syntax" 'actual "bad"))))))
  (define missing-fields (compare-observation incomplete declaration-actual))
  (check (ormap (lambda (item) (equal? (hash-ref item 'missing) "expected")) missing-fields) #t)
  (check (ormap (lambda (item) (equal? (hash-ref item 'expected) missing-json)) missing-fields) #t)
  (check
   (ormap
    (lambda (item) (equal? (hash-ref item 'missing) "both"))
    (compare-observation incomplete actual))
   #t)
  (check
   (pair? (compare-observation (hash 'stdout_json '("a" "b")) (hash 'stdout_json '("b" "a"))))
   #t)
  (define clean
    (hash
     'invocation
     invocation
     'exit_code
     0
     'stdout_json
     (hash 'ok #t 'terms '() 'diagnostics '())))
  (define extra-bound
    (hash
     'exit_code
     0
     'stdout_json
     (hash 'ok #t 'terms '() 'diagnostics '() 'bound (hash 'c 1 'k 1))))
  (check (pair? (compare-observation clean extra-bound)) #t)
  (check
   (compare-observation
    clean
    (hash 'exit_code 0 'stdout_json (hash 'ok #t 'terms '() 'diagnostics '())))
   '())
  (check
   (pair?
    (compare-observation
     (hash-set clean 'stdout_json (hash 'ok #t 'terms '() 'diagnostics '() 'bound (hash 'c 2 'k 1)))
     extra-bound))
   #t)
  (check
   (pair?
    (compare-observation
     (hash 'stdout_json (hash 'classes (hash 'a 1)))
     (hash 'stdout_json (hash 'classes (hash 'a 1 'b 2)))))
   #t)
  (check
   (equal?
    (hash-ref (car (compare-observation (hash 'stdout_json 'null) (hash))) 'actual)
    missing-json)
   #t)
  (check (pair? (compare-observation (hash 'lsp_range_utf16_zero_based (hash 'start 0)) (hash))) #t)
  (check
   (pair?
    (compare-observation
     (hash 'stdout_json (hash 'provenance "a"))
     (hash 'stdout_json (hash 'provenance "b"))))
   #t)
  (define scoped
    (blind-command-scope
     (hash
      'complete
      #t
      'total
      3
      'case-total
      2
      'results
      (list
       (hash 'id "a/check" 'case-id "a" 'command "check" 'pass #t)
       (hash 'id "a/ask" 'case-id "a" 'command "ask" 'pass #f)
       (hash 'id "b/emit" 'case-id "b" 'command "emit" 'pass #f))
      'mismatches
      (list (hash 'id "a/ask") (hash 'id "b/emit")))
     '("check" "emit")))
  (check (hash-ref scoped 'total) 2)
  (check (hash-ref scoped 'case-total) 2)
  (check (hash-ref scoped 'case-failures) 1)
  (check (hash-ref scoped 'failures) 1)
  (check (map (lambda (item) (hash-ref item 'id)) (hash-ref scoped 'results)) '("a/check" "b/emit"))
  (check (hash-ref scoped 'mismatches) (list (hash 'id "b/emit")))
  (check (hash-ref scoped 'full-invocations) 3)
  (check (hash-ref scoped 'complete) #t)
  (define staged
    (blind-command-scope
     (hash
      'complete
      #t
      'total
      4
      'case-total
      4
      'results
      (list
       (hash 'id "kernel" 'case-id "kernel" 'command "check" 'stage 1 'pass #t)
       (hash 'id "model" 'case-id "model" 'command "check" 'stage 2 'pass #f)
       (hash 'id "projection" 'case-id "projection" 'command "check" 'stage 4 'pass #f)
       (hash 'id "grammar" 'case-id "grammar" 'command "emit" 'stage 1 'pass #t))
      'mismatches
      (list (hash 'id "model") (hash 'id "projection")))
     '("check" "emit")
     #:stage
     1))
  (check (hash-ref staged 'total) 2)
  (check (hash-ref staged 'failures) 0)
  (check (hash-ref staged 'mismatches) '())
  (check (map (lambda (item) (hash-ref item 'id)) (hash-ref staged 'results)) '("kernel" "grammar"))
  (define second-stage
    (blind-stage-scope
     (hash
      'complete
      #t
      'total
      7
      'case-total
      7
      'results
      (for/list
       ((id '("check" "emit" "tree" "plan" "ask" "later-plan" "later-check"))
        (command '("check" "emit" "tree" "plan" "ask" "plan" "check"))
        (stage '(1 1 2 2 3 3 4)))
       (hash 'id id 'case-id id 'command command 'stage stage 'pass #t))
      'mismatches
      '())
     2))
  (check
   (map (lambda (item) (hash-ref item 'id)) (hash-ref second-stage 'results))
   '("check" "emit" "tree" "plan"))
  (check (hash-ref second-stage 'stage-limit) 2)
  (check (hash-ref second-stage 'scope) '("check" "emit" "tree" "plan"))
  (check (hash-ref second-stage 'total) 4)
  (check (with-handlers ((exn:fail? (lambda (_) #t))) (blind-stage-commands 3) #f) #t)
  (check (expected-stage (hash 'expected (hash 'diagnostics (list (hash 'code "E_JOIN_LAW"))))) 2)
  (check
   (expected-stage
    (hash
     'expected
     (hash 'diagnostics (list (hash 'code "E_DECLARATION" 'data (hash 'kind "effect"))))))
   2)
  (check (expected-stage (hash 'expected (hash 'diagnostics (list (hash 'code "E_CLASS"))))) 1)
  (define grammar-source
    "root ::= tag \" \" label\ntag ::= \"one\" | \"two\"\nlabel ::= \"small\"\n")
  (define grammar-assertions
    (hash
     'required_nonterminals
     '("root" "tag" "label")
     'accepts_lines
     '("one small" "two small")
     'rejects_lines
     '("three small" "one large")
     'schematic_note
     "Grammar spelling is separate from the asserted language."))
  (define (grammar-differences assertions (output (hash 'stdout_gbnf grammar-source)))
    (compare-observation (hash 'gbnf_assertions assertions) output))
  (define (reasons differences) (map (lambda (item) (hash-ref item 'reason)) differences))
  (check (grammar-differences grammar-assertions) '())
  (check
   (reasons (grammar-differences (hash 'required_nonterminals '("missing"))))
   '("missing-nonterminal"))
  (check
   (reasons (grammar-differences (hash 'accepts_lines '("one large"))))
   '("gbnf-line-refused"))
  (check
   (reasons (grammar-differences (hash 'rejects_lines '("one small"))))
   '("gbnf-line-accepted"))
  (check
   (reasons (grammar-differences grammar-assertions (hash 'stdout_gbnf "root ::= undeclared\n")))
   '("gbnf-parse-error"))
  (check (reasons (grammar-differences grammar-assertions (hash))) '("missing-gbnf-output"))
  (check (reasons (grammar-differences "not an assertion object")) '("invalid-gbnf-assertions"))
  (check
   (reasons (grammar-differences (hash 'accepts_lines "not a list")))
   '("invalid-gbnf-assertions"))
  (check
   (reasons (grammar-differences (hash 'unknown_assertion '("one small"))))
   '("unsupported-gbnf-assertion"))
  (define skips
    (list
     (hash
      'constructor
      "step"
      'fact
      "exists"
      'args
      '("/sample")
      'reason
      "read"
      'origin
      (hash 'kind "program" 'path "input.typify" 'line 1))))
  (check (compare-observation (hash 'skips skips) (hash 'stdout_json (hash 'skips skips))) '())
  (check (pair? (compare-observation (hash 'skips skips) (hash 'stdout_json (hash 'skips '())))) #t)
  (check (pair? (compare-observation (hash 'skips '()) (hash 'stdout_json (hash)))) #t)
  (hash 'suite "blind-comparison" 'checks checks 'status "passed"))

(module+ test-support (provide compare-observation missing missing-json))

(module+ test (run-tests))

(module+ main (displayln (canonical-json (run-blind-corpus))))
