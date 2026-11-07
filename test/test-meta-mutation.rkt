#lang racket/base

(require json
         racket/file
         racket/list
         racket/path
         racket/string
         "../src/language.rkt"
         "../src/meta.rkt"
         "blind-corpus.rkt")

(provide run-tests mutation-operators)

(struct mutation (id family change forms classes) #:transparent)

(struct oracle (id run expected) #:transparent)

(define mutation-operators
  '(remove-row
    replace-leading-literal
    replace-constructor
    replace-argument-class
    replace-class-pattern
    remove-literal-alternative))

(define ordinary-source
  (string-join
   '("#lang typify"
     "declare : Lexicon"
     "version : 7"
     "comment : //"
     "is : color = \"blue\" | \"red\""
     "is : text = /[A-Za-z ]+/"
     "primitive : output : color text"
     "primitive : stop"
     "term : wrap : color text"
     "term : halt"
     "term : annotation : text"
     "fact : annotation"
     "rule : wrap <c> <t> -> output <c> <t>"
     "rule : halt -> stop"
     "form : item : <color> = <text>"
     "does : wrap <color> <text>"
     "restricts!"
     "example : item : blue = Warm sky"
     "default : item : red = Calm sky"
     "count : wrap = 1..*"
     "form : halt!"
     "does : halt"
     "example : halt!"
     "file : entries.data"
     "required!"
     "order : strict"
     "sees : Auxiliary_2-N"
     "note : quoting"
     "cap : states = 4096"
     "cap : repetition = 256"
     "cap : depth = 8"
     "cap : length = 4096"
     "cell : content = shadow"
     "writes : output : content = put 1"
     "reads : output : exists = reject"
     "ensure : wrap : stable = (bound <c>)"
     "join : modes : \"ro\" \"ro\" = \"ro\""
     "join : modes : \"ro\" \"rw\" = \"rw\""
     "join : modes : \"rw\" \"ro\" = \"rw\""
     "join : modes : \"rw\" \"rw\" = \"rw\""
     "effect : stop = external"
     "stage : output = 0"
     "guard : wrap = exists"
     "scope : wrap = program"
     "scope : halt = parent"
     "scope : annotation = root"
     "fuel : wrap = 32"
     "end!"
     "declare : Auxiliary_2-N"
     "optional!"
     "end!")
   "\n"))

(define (ordinary-model)
  (define declarations (read-declarations ordinary-source))
  (define body (hash-ref declarations "Lexicon"))
  (define auxiliary (hash-ref declarations "Auxiliary_2-N"))
  (define checked (check-program "// annotation\nitem : blue = Warm sky\nhalt!" body))
  (list
   (language-name body)
   (language-version body)
   (language-comment body)
   (language-order body)
   (language-file body)
   (language-required? body)
   (language-required? auxiliary)
   (language-sees body)
   (language-notes body)
   (hash-ref (language-caps body) "states")
   (map car (form-defaults (car (language-forms body))))
   (hash-ref checked 'terms)
   (normalize-terms body (hash-ref checked 'terms))))

(define ordinary-expected
  '("Lexicon"
    "7"
    "//"
    "strict"
    "entries.data"
    #t
    #f
    ("Auxiliary_2-N")
    ("quoting")
    4096
    ("item : red = Calm sky")
    (("wrap" "blue" "Warm sky") ("halt"))
    (("output" "blue" "Warm sky") ("stop"))))

(define (counter-source multiplicity)
  (string-append
   "#lang typify\ndeclare : Counter\norder : free\nprimitive : tick\n"
   "form : tick!\ndoes : tick\ncount : tick = "
   multiplicity
   "\nend!\n"))

(define (metadata-model)
  (define body (hash-ref (read-declarations ordinary-source) "Lexicon"))
  (list
   (constructor-fact? (hash-ref (language-constructors body) "annotation"))
   (form-restricting? (car (language-forms body)))
   (for/list
    ((row (language-rows body))
     #:when
     (memq (hash-ref row 'kind) '(cell writes reads ensure join effect stage guard scope fuel)))
    (define captures (hash-ref row 'captures))
    (list
     (symbol->string (hash-ref row 'kind))
     (for/list
      ((name (sort (hash-keys captures) string<?)))
      (list name (hash-ref captures name)))))))

(define metadata-expected
  '(#t
    #t
    (("cell" (("name" "content") ("value" "shadow")))
     ("writes" (("name" "output") ("value" "content = put 1")))
     ("reads" (("name" "output") ("value" "exists = reject")))
     ("ensure" (("constructor" "wrap") ("name" "stable") ("value" "(bound <c>)")))
     ("join" (("name" "modes") ("value" "\"ro\" \"ro\" = \"ro\"")))
     ("join" (("name" "modes") ("value" "\"ro\" \"rw\" = \"rw\"")))
     ("join" (("name" "modes") ("value" "\"rw\" \"ro\" = \"rw\"")))
     ("join" (("name" "modes") ("value" "\"rw\" \"rw\" = \"rw\"")))
     ("effect" (("name" "stop")))
     ("stage" (("name" "output") ("number" "0")))
     ("guard" (("name" "wrap") ("value" "exists")))
     ("scope" (("name" "wrap") ("scope-mode" "program")))
     ("scope" (("name" "halt") ("scope-mode" "parent")))
     ("scope" (("name" "annotation") ("scope-mode" "root")))
     ("fuel" (("name" "wrap") ("number" "32"))))))

(define (counter-model multiplicity program)
  (define body (hash-ref (read-declarations (counter-source multiplicity)) "Counter"))
  (hash-ref (check-program program body) 'terms))

(define (rejects? procedure) (with-handlers ((exn:fail? (lambda (_) #t))) (procedure) #f))

(define (fact-model)
  (define source
    (string-join
     '("#lang typify"
       "declare : Catalog"
       "is : category = \"book\" | \"journal\""
       "is : category-alias = category"
       "is : title = /[A-Za-z ]+/"
       "term : item : category-alias title"
       "form : item : <category-alias> = <title>"
       "does : item <category-alias> <title>"
       "count : item = 1..*"
       "end!")
     "\n"))
  (define body (hash-ref (read-declarations source) "Catalog"))
  (hash-ref (check-program "item : journal = Market Research" body) 'terms))

(define internal-oracles
  (list
   (oracle "ordinary-declaration-and-meaning" ordinary-model ordinary-expected)
   (oracle "metadata-retention-and-term-classification" metadata-model metadata-expected)
   (oracle "facts-and-bare-class-reference" fact-model '(("item" "journal" "Market Research")))
   (oracle
    "mixed-constructor-needs-fact-marker"
    (lambda ()
      (rejects?
       (lambda ()
         (read-declarations
          "#lang typify\ndeclare : Mixed\nprimitive : output\nterm : unclassified\nend!"))))
    #t)
   (oracle
    "restricting-form-needs-required-count-or-default"
    (lambda ()
      (rejects?
       (lambda ()
         (read-declarations
          "#lang typify\ndeclare : Restricted\nterm : denial\nform : deny!\ndoes : denial\nrestricts!\nend!"))))
    #t)
   (oracle
    "withdrawn-count-words"
    (lambda ()
      (for/list
       ((word '("one" "at-most-one" "at-least-one" "any")))
       (rejects? (lambda () (counter-model word "tick!")))))
    '(#t #t #t #t))
   (oracle "count-one" (lambda () (counter-model "1" "tick!")) '(("tick")))
   (oracle "count-optional-empty" (lambda () (counter-model "0..1" "")) '())
   (oracle
    "count-optional-two"
    (lambda () (rejects? (lambda () (counter-model "0..1" "tick!\ntick!"))))
    #t)
   (oracle "count-required" (lambda () (counter-model "1..*" "tick!")) '(("tick")))
   (oracle "count-required-empty" (lambda () (rejects? (lambda () (counter-model "1..*" "")))) #t)
   (oracle "count-unrestricted-empty" (lambda () (counter-model "*" "")) '())
   (oracle
    "count-unrestricted-many"
    (lambda () (counter-model "*" "tick!\ntick!\ntick!"))
    '(("tick") ("tick") ("tick")))
   (oracle
    "unknown-meta-line"
    (lambda ()
      (rejects?
       (lambda () (read-declarations "#lang typify\ndeclare : Sample\ninvented : value\nend!"))))
    #t)
   (oracle
    "negative-version"
    (lambda ()
      (rejects?
       (lambda () (read-declarations "#lang typify\ndeclare : Sample\nversion : -1\nend!"))))
    #t)))

(define (replace-at values index replacement)
  (for/list ((value values) (position (in-naturals))) (if (= position index) replacement value)))

(define (pattern-spans source)
  (define size (string-length source))
  (define (quoted-end index)
    (let loop ((position (add1 index)))
      (cond
       ((>= position size) (error 'mutation "unterminated class literal"))
       ((char=? (string-ref source position) #\\) (loop (+ position 2)))
       ((char=? (string-ref source position) #\") (add1 position))
       (else (loop (add1 position))))))
  (define (pattern-end index)
    (let loop ((position (add1 index)) (range? #f))
      (cond
       ((>= position size) (error 'mutation "unterminated class pattern"))
       ((char=? (string-ref source position) #\\) (loop (+ position 2) range?))
       ((char=? (string-ref source position) #\[) (loop (add1 position) #t))
       ((char=? (string-ref source position) #\]) (loop (add1 position) #f))
       ((and (not range?) (char=? (string-ref source position) #\/)) (add1 position))
       (else (loop (add1 position) range?)))))
  (let loop ((position 0) (spans '()))
    (cond
     ((>= position size) (reverse spans))
     ((char=? (string-ref source position) #\") (loop (quoted-end position) spans))
     ((char=? (string-ref source position) #\/)
      (define end (pattern-end position))
      (loop end (cons (cons position end) spans)))
     (else (loop (add1 position) spans)))))

(define (quoted-literal? source) (regexp-match? #px"^\"(?:[^\"\\\\]|\\\\.)*\"$" source))

(define (make-mutations forms classes)
  (define seeds '())
  (define (add! id family change changed-forms changed-classes)
    (set! seeds (cons (mutation id family change changed-forms changed-classes) seeds)))
  (for
   ((row forms) (index (in-naturals)))
   (define target (format "~a/~a" index (hash-ref row 'kind)))
   (define (row! family field value)
     (add!
      (string-append (symbol->string family) "/" target)
      family
      (hasheq
       'row
       index
       'field
       (symbol->string field)
       'before
       (if (symbol? (hash-ref row field))
         (symbol->string (hash-ref row field))
         (hash-ref row field))
       'after
       (if (symbol? value) (symbol->string value) value))
      (replace-at forms index (hash-set row field value))
      classes))
   (add!
    (string-append "remove-row/" target)
    'remove-row
    (hasheq 'row index 'field "row" 'before (symbol->string (hash-ref row 'kind)) 'after "removed")
    (append (take forms index) (drop forms (add1 index)))
    classes)
   (row! 'replace-leading-literal 'production (string-append "mutated-" (hash-ref row 'production)))
   (row!
    'replace-constructor
    'constructor
    (string-append (hash-ref row 'constructor) "-unrecognized"))
   (for
    ((argument (hash-ref row 'arguments)) (position (in-naturals)))
    (define replacement (if (equal? argument "number") "ordering" "number"))
    (add!
     (format "replace-argument-class/~a/~a" target position)
     'replace-argument-class
     (hasheq 'row index 'field "arguments" 'position position 'before argument 'after replacement)
     (replace-at
      forms
      index
      (hash-set row 'arguments (replace-at (hash-ref row 'arguments) position replacement)))
     classes)))
  (for
   ((name (sort (hash-keys classes) string<?)))
   (define source (hash-ref classes name))
   (for
    ((span (pattern-spans source)) (index (in-naturals)))
    (define before (substring source (car span) (cdr span)))
    (define replacement
      (cond
       ((equal? name "number") (if (equal? before "/0/") "/1/" "/0/"))
       ((equal? before "/A/") "/B/")
       (else "/A/")))
    (define after
      (string-append (substring source 0 (car span)) replacement (substring source (cdr span))))
    (add!
     (format "replace-class-pattern/~a/~a" name index)
     'replace-class-pattern
     (hasheq 'class name 'position index 'before before 'after replacement)
     forms
     (hash-set classes name after)))
   (define alternatives (string-split source " | "))
   (when (and (> (length alternatives) 1) (andmap quoted-literal? alternatives))
     (for
      ((alternative alternatives) (index (in-naturals)))
      (define replacement
        (string-join (append (take alternatives index) (drop alternatives (add1 index))) " | "))
      (add!
       (format "remove-literal-alternative/~a/~a" name index)
       'remove-literal-alternative
       (hasheq 'class name 'position index 'before alternative 'after "removed")
       forms
       (hash-set classes name replacement)))))
  (reverse seeds))

(define (evaluate-oracle item)
  (with-handlers
   ((exn:fail?
     (lambda (failure) (hasheq 'id (oracle-id item) 'pass #f 'exception (exn-message failure)))))
   (define actual ((oracle-run item)))
   (hasheq 'id (oracle-id item) 'pass (equal? actual (oracle-expected item)))))

(define (blind-oracle #:ids (ids #f) #:stop-on-failure? (stop? #f))
  (with-handlers
   ((exn:fail?
     (lambda (failure)
       (hasheq
        'available
        #f
        'total
        0
        'failures
        1
        'status
        "missing-or-unreadable-oracle"
        'message
        (exn-message failure)))))
   (run-blind-corpus #:ids ids #:stop-on-failure? stop?)))

(define (field data name (default #f)) (if (hash? data) (hash-ref data name default) default))

(define (run-tests)
  (define forms (current-meta-forms))
  (define classes (current-meta-classes))
  (define baseline (map evaluate-oracle internal-oracles))
  (define baseline-failures (count (lambda (result) (not (hash-ref result 'pass))) baseline))
  (define blind-full-baseline (blind-oracle))
  (define blind-baseline (blind-command-scope blind-full-baseline '("check" "emit")))
  (define blind-baseline-results (field blind-baseline 'results '()))
  (define blind-baseline-shape?
    (and (for/and
          ((result (in-list blind-baseline-results)))
          (and (hash? result)
               (string? (field result 'id))
               (hash-has-key? result 'pass)
               (boolean? (field result 'pass))))
         (=
          (length blind-baseline-results)
          (length
           (remove-duplicates (map (lambda (result) (field result 'id)) blind-baseline-results))))))
  (define blind-available? (and (eq? (field blind-baseline 'available) #t) blind-baseline-shape?))
  (define blind-passing-ids
    (for/list
     ((result (in-list blind-baseline-results)) #:when (eq? (field result 'pass) #t))
     (field result 'id)))
  (define blind-baseline-failures
    (count (lambda (result) (not (eq? (field result 'pass) #t))) blind-baseline-results))
  (define blind-valid?
    (and blind-available?
         (exact-positive-integer? (field blind-baseline 'total))
         (eq? (field blind-baseline 'complete) #t)
         (equal? (field blind-baseline 'failures) 0)
         (zero? blind-baseline-failures)))
  (define seeds (make-mutations forms classes))
  (define measurements
    (if (zero? baseline-failures)
      (for/list
       ((seed seeds))
       (parameterize
        ((current-meta-forms (mutation-forms seed)) (current-meta-classes (mutation-classes seed)))
        (define blind-result
          (and blind-available?
               (pair? blind-passing-ids)
               (blind-oracle #:ids blind-passing-ids #:stop-on-failure? #t)))
        (define blind-results (if blind-result (field blind-result 'results '()) '()))
        (define blind-result-valid?
          (or (not blind-result)
              (and (eq? (field blind-result 'available) #t)
                   (zero? (field blind-result 'infrastructure-failures 1))
                   (pair? blind-results)
                   (=
                    (length blind-results)
                    (length
                     (remove-duplicates (map (lambda (result) (field result 'id)) blind-results))))
                   (for/and
                    ((result (in-list blind-results)))
                    (and (member (field result 'id) blind-passing-ids)
                         (equal? (field result 'kind) "compared")
                         (hash-has-key? result 'pass)
                         (boolean? (field result 'pass))))
                   (equal?
                    (map (lambda (result) (field result 'id)) blind-results)
                    (take blind-passing-ids (length blind-results)))
                   (or (and (or (eq? (field blind-result 'selected-complete) #t)
                                (=
                                 (field blind-result 'attempted -1)
                                 (field blind-result 'requested -2)))
                            (= (length blind-results) (length blind-passing-ids)))
                       (and (eq? (field blind-result 'stopped-early) #t)
                            (ormap
                             (lambda (result) (eq? (field result 'pass) #f))
                             blind-results))))))
        (define blind-witness
          (and blind-result-valid?
               (for/first
                ((result (in-list blind-results))
                 #:when
                 (and (equal? (field result 'kind) "compared") (eq? (field result 'pass) #f)))
                result)))
        (define internal-attempted 0)
        (define internal
          (and (not blind-witness)
               (for/or
                ((item internal-oracles))
                (set! internal-attempted (add1 internal-attempted))
                (define result (evaluate-oracle item))
                (and (not (hash-ref result 'pass)) (oracle-id item)))))
        (hasheq
         'id
         (mutation-id seed)
         'family
         (symbol->string (mutation-family seed))
         'change
         (mutation-change seed)
         'killed
         (and (or internal blind-witness) #t)
         'internal-witness
         (or internal 'null)
         'blind-witness
         (if blind-witness (field blind-witness 'id) 'null)
         'blind-observation
         (or blind-witness 'null)
         'blind-mismatches
         (if blind-witness (field blind-result 'mismatches '()) '())
         'blind-attempted
         (length blind-results)
         'blind-requested
         (if blind-result (length blind-passing-ids) 0)
         'internal-attempted
         internal-attempted
         'blind-measurement-valid
         blind-result-valid?)))
      '()))
  (define seeded (length seeds))
  (define killed (count (lambda (measurement) (hash-ref measurement 'killed)) measurements))
  (define complete? (= (length measurements) seeded))
  (define survivors (- seeded killed))
  (define blind-killed
    (count (lambda (measurement) (string? (hash-ref measurement 'blind-witness))) measurements))
  (define internally-killed
    (count (lambda (measurement) (string? (hash-ref measurement 'internal-witness))) measurements))
  (define blind-measurement-errors
    (count
     (lambda (measurement) (not (hash-ref measurement 'blind-measurement-valid)))
     measurements))
  (define blind-invocations
    (for/sum ((measurement (in-list measurements))) (hash-ref measurement 'blind-attempted)))
  (define internal-invocations
    (for/sum ((measurement (in-list measurements))) (hash-ref measurement 'internal-attempted)))
  (define checks
    (+
     (length baseline)
     (max 1 (field blind-baseline 'total 0))
     (length measurements)
     blind-invocations
     internal-invocations))
  (define failures
    (+
     baseline-failures
     (field blind-baseline 'failures 1)
     (- (length measurements) killed)
     blind-measurement-errors))
  (define gate
    (hasheq
     'total
     seeded
     'failures
     (+ survivors baseline-failures blind-measurement-errors (if blind-valid? 0 1))
     'seeded
     seeded
     'killed
     killed
     'blind-killed
     blind-killed
     'internally-killed
     internally-killed
     'blind-invocations
     blind-invocations
     'internal-invocations
     internal-invocations
     'blind-baseline-passing
     (length blind-passing-ids)
     'blind-baseline-failing
     blind-baseline-failures
     'blind-measurement-errors
     blind-measurement-errors
     'single-point-total
     seeded
     'single-point-killed
     killed
     'excludes-round-trip
     #t
     'blind-corpus
     blind-valid?
     'status
     (cond
      ((not (zero? baseline-failures)) "invalid-baseline")
      ((not blind-available?) "missing-blind-evidence")
      ((not blind-valid?) "invalid-blind-baseline")
      ((positive? blind-measurement-errors) "blind-measurement-infrastructure-failure")
      ((not complete?) "incomplete-measurement")
      ((positive? survivors) "surviving-mutations")
      (else "measured"))))
  (hasheq
   'suite
   "meta-mutation"
   'count
   checks
   'pass
   (- checks failures)
   'fail
   failures
   'status
   (if (zero? failures) "passed" "failed")
   'gates
   (hasheq 'meta-mutation gate)
   'baseline
   baseline
   'blind-full-baseline
   blind-full-baseline
   'blind-baseline
   blind-baseline
   'operator-set
   (map symbol->string mutation-operators)
   'denominator
   "One application of each registered operator at every eligible field of the current immutable meta table and class sources; no equivalent-mutant filtering."
   'blind-algorithm
   "Run the frozen baseline first; only baseline-passing invocation IDs are eligible. For each registered mutation, stop at its first newly failing eligible blind invocation; otherwise run the independent internal oracles until their first failure. A non-clean blind baseline invalidates the overall gate, and baseline-failing invocations never kill mutants. Roundtrip and witness laws are excluded."
   'measurements
   measurements))

(module+ main (write-json (run-tests)) (newline))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
