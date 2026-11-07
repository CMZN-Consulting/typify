#lang racket/base

(require json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../src/language.rkt"
         "../src/meta.rkt")

(provide run-tests mutation-operators)

(struct mutation (id family change forms classes) #:transparent)

(struct oracle (id run expected) #:transparent)

(define-runtime-path language-tests "test-language.rkt")

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
     "is : color = blue | red"
     "is : text = /[A-Za-z ]+/"
     "primitive : output : color text"
     "primitive : stop"
     "term : wrap : color text"
     "term : halt"
     "rule : wrap <c> <t> -> output <c> <t>"
     "rule : halt -> stop"
     "form : item : <color> = <text>"
     "does : wrap <color> <text>"
     "example : item : blue = Warm sky"
     "default : item : red = Calm sky"
     "count : wrap = at-least-one"
     "form : halt!"
     "does : halt"
     "example : halt!"
     "file : entries.data"
     "required!"
     "order : strict"
     "sees : Auxiliary"
     "note : quoting"
     "cap : states = 4096"
     "cell : content = shadow"
     "writes : output : content = put 1"
     "reads : output : exists = reject"
     "ensure : stable : (bound data)"
     "end!"
     "declare : Auxiliary"
     "optional!"
     "end!")
   "\n"))

(define (ordinary-model)
  (define declarations (read-declarations ordinary-source))
  (define body (hash-ref declarations "Lexicon"))
  (define auxiliary (hash-ref declarations "Auxiliary"))
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
    ("Auxiliary")
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

(define (counter-model multiplicity program)
  (define body (hash-ref (read-declarations (counter-source multiplicity)) "Counter"))
  (hash-ref (check-program program body) 'terms))

(define (rejects? procedure) (with-handlers ((exn:fail? (lambda (_) #t))) (procedure) #f))

(define internal-oracles
  (list
   (oracle "ordinary-declaration-and-meaning" ordinary-model ordinary-expected)
   (oracle "count-one" (lambda () (counter-model "one" "tick!")) '(("tick")))
   (oracle "count-affine-empty" (lambda () (counter-model "at-most-one" "")) '())
   (oracle
    "count-affine-two"
    (lambda () (rejects? (lambda () (counter-model "at-most-one" "tick!\ntick!"))))
    #t)
   (oracle "count-relevant" (lambda () (counter-model "at-least-one" "tick!")) '(("tick")))
   (oracle
    "count-relevant-empty"
    (lambda () (rejects? (lambda () (counter-model "at-least-one" ""))))
    #t)
   (oracle "count-unrestricted-empty" (lambda () (counter-model "any" "")) '())
   (oracle
    "count-unrestricted-many"
    (lambda () (counter-model "any" "tick!\ntick!\ntick!"))
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
   (cond
    ((and (string-prefix? source "/") (string-suffix? source "/"))
     (define replacement (if (equal? name "number") "/0/" "/A/"))
     (unless (equal? source replacement)
       (add!
        (string-append "replace-class-pattern/" name)
        'replace-class-pattern
        (hasheq 'class name 'before source 'after replacement)
        forms
        (hash-set classes name replacement))))
    (else
     (define alternatives (string-split source " | "))
     (for
      ((alternative alternatives) (index (in-naturals)) #:when (> (length alternatives) 1))
      (define replacement
        (string-join (append (take alternatives index) (drop alternatives (add1 index))) " | "))
      (add!
       (format "remove-literal-alternative/~a/~a" name index)
       'remove-literal-alternative
       (hasheq 'class name 'position index 'before alternative 'after "removed")
       forms
       (hash-set classes name replacement))))))
  (reverse seeds))

(define (evaluate-oracle item)
  (with-handlers
   ((exn:fail?
     (lambda (failure) (hasheq 'id (oracle-id item) 'pass #f 'exception (exn-message failure)))))
   (define actual ((oracle-run item)))
   (hasheq 'id (oracle-id item) 'pass (equal? actual (oracle-expected item)))))

(define (blind-oracle)
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
   (define runner (dynamic-require language-tests 'run-blind-corpus))
   (runner)))

(define (field data name (default #f)) (if (hash? data) (hash-ref data name default) default))

(define (run-tests)
  (unless (equal? (getenv "GITHUB_ACTIONS") "true")
    (error 'test-meta-mutation "mutation enumeration runs on GitHub Actions only"))
  (define forms (current-meta-forms))
  (define classes (current-meta-classes))
  (define baseline (map evaluate-oracle internal-oracles))
  (define baseline-failures (count (lambda (result) (not (hash-ref result 'pass))) baseline))
  (define blind-baseline (blind-oracle))
  (define blind-available? (eq? (field blind-baseline 'available) #t))
  (define blind-valid?
    (and blind-available?
         (exact-positive-integer? (field blind-baseline 'total))
         (equal? (field blind-baseline 'failures) 0)))
  (define seeds (make-mutations forms classes))
  (define measurements
    (if (and (zero? baseline-failures) (or (not blind-available?) blind-valid?))
      (for/list
       ((seed seeds))
       (parameterize
        ((current-meta-forms (mutation-forms seed)) (current-meta-classes (mutation-classes seed)))
        (define internal
          (for/or
           ((item internal-oracles))
           (define result (evaluate-oracle item))
           (and (not (hash-ref result 'pass)) (oracle-id item))))
        (define blind-result (and blind-valid? (blind-oracle)))
        (define blind-killed?
          (and blind-result
               (or (not (eq? (field blind-result 'available) #t))
                   (not (equal? (field blind-result 'failures) 0)))))
        (hasheq
         'id
         (mutation-id seed)
         'family
         (symbol->string (mutation-family seed))
         'change
         (mutation-change seed)
         'killed
         (and (or internal blind-killed?) #t)
         'internal-witness
         (or internal 'null)
         'blind-witness
         (and blind-killed? #t))))
      '()))
  (define seeded (length seeds))
  (define killed (count (lambda (measurement) (hash-ref measurement 'killed)) measurements))
  (define complete? (= (length measurements) seeded))
  (define survivors (- seeded killed))
  (define gate
    (hasheq
     'total
     seeded
     'failures
     (+ survivors baseline-failures (if blind-valid? 0 1))
     'seeded
     seeded
     'killed
     killed
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
      ((not blind-valid?) "missing-blind-evidence")
      ((not complete?) "incomplete-measurement")
      ((positive? survivors) "surviving-mutations")
      (else "measured"))))
  (hasheq
   'suite
   "meta-mutation"
   'count
   (length baseline)
   'pass
   (- (length baseline) baseline-failures)
   'fail
   baseline-failures
   'gates
   (hasheq 'meta-mutation gate)
   'baseline
   baseline
   'blind-baseline
   blind-baseline
   'operator-set
   (map symbol->string mutation-operators)
   'denominator
   "One application of each registered operator at every eligible field of the current immutable meta table and class sources; no equivalent-mutant filtering."
   'measurements
   measurements))

(module+ main (write-json (run-tests)) (newline))
