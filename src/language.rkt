#lang racket/base

(require racket/list
         racket/match
         racket/promise
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "meta.rkt"
         "model.rkt"
         "syntax.rkt")

(provide (struct-out language)
         (struct-out form)
         (struct-out constructor)
         read-declarations
         read-declarations/diagnostics
         check-program
         check-program/diagnostics
         line-accepts?
         emit-declarations
         declaration->jsexpr
         normalize-terms
         meta-language
         declaration-bound
         declaration-check-bounds
         language-model
         language-keyed-counts
         language-template-names
         language-composition
         evaluate-rule-expression
         primitive-events
         language-needs-facts?
         program-guarded?
         lookup-reference
         program-lookup
         current-lookup-observer
         (struct-out lookup-observation)
         form-automata)

(struct constructor (name signature primitive? fact? rule span ordinal) #:transparent)

(struct lookup-observation (values origins skips) #:transparent)

(define current-lookup-observer (make-parameter void))

(define language-lookup-cache (make-weak-hasheq))

(define language-template-cache (make-weak-hasheq))

(define language-composition-cache (make-ephemeron-hasheq))

(define current-partition-model (make-parameter #f))

(define current-template-names (make-parameter '()))

(define (language-template-names body) (hash-ref language-template-cache body '()))

(define (language-composition body) (hash-ref language-composition-cache body (list body)))

(define (lookup-reference body reference)
  (hash-ref
   (hash-ref language-lookup-cache body (hash))
   reference
   (lambda ()
     (if (string-contains? reference ".")
       reference
       (string-append (language-name body) "." reference)))))

(define (program-lookup program reference origin)
  (define qualified (lookup-reference (hash-ref program 'body) reference))
  (define parts (string-split qualified "." #:trim? #f))
  (unless (= (length parts) 2)
    (raise-arguments-error
     'program-lookup
     "a checked lookup reference is required"
     "reference"
     reference))
  (define entries '())
  (define skips '())
  (for
   ((record (in-list (hash-ref program 'enclosing '())))
    #:when
    (equal? (language-name (hash-ref record 'body)) (car parts)))
   (if (equal? (hash-ref record 'status "present") "guarded")
     (set! skips
       (append
        skips
        (for/list
         ((skip (in-list (hash-ref record 'skips '())))
          #:when
          (and (equal? (hash-ref skip 'reason) "guard") (hash-has-key? skip 'origin)))
         (hash
          'reason
          "guard"
          'fact
          (hash-ref skip 'fact)
          'args
          (hash-ref skip 'args)
          'program
          (hash-ref record 'path)))))
     (for
      ((value (in-list (hash-ref record 'terms '())))
       (source (in-list (hash-ref record 'origins '())))
       #:when
       (equal? (car value) (cadr parts)))
      (unless (= (length value) 2)
        (raise-arguments-error
         'program-lookup
         "the checked lookup constructor must be unary"
         "reference"
         qualified))
      (set! entries (append entries (list (cons (cadr value) source)))))))
  (lookup-observation (map car entries) (map cdr entries) skips))

(struct
 form
 (source kind left right meaning examples defaults span ordinal restricting?)
 #:transparent)

(struct
 language
 (name
  version
  comment
  order
  classes
  class-sources
  constructors
  forms
  counts
  file
  required?
  defaults
  sees
  notes
  caps
  rows
  span)
 #:transparent)

(define (fail code message span (data (hash))) (raise-typify code message #:span span #:data data))

(define (declaration-fail kind actual message span (extra (hash)))
  (fail "E_DECLARATION" message span (hash-set* extra 'kind kind 'actual actual)))

(define current-diagnostic-sink (make-parameter #f))

(define current-bootstrap? (make-parameter #f))

(define term-source-cache (make-weak-hasheq))

(define value-origin-cache (make-weak-hasheq))

(define value-meaning-spans (make-weak-hasheq))

(define form-restriction-spans (make-weak-hasheq))

(define current-program-span (make-parameter #f))

(define current-source-lines (make-parameter (hash)))

(define current-source-text-lines (make-parameter (hash)))

(define (source-line-span span)
  (hash-ref (current-source-lines) (cons (src-span-file span) (src-span-start-line span)) span))

(define (routed-diagnostic item)
  (define code (diagnostic-code item))
  (define span (diagnostic-range item))
  (define whole-line?
    (or (member
         code
         '("E_LINE_SHAPE"
           "E_FORM_NONE"
           "E_QUESTION"
           "E_ORDER"
           "E_EXAMPLE"
           "E_CLASS_UNDECLARED"
           "E_CLASS_OVERLAP"
           "E_FORM_SHADOW"
           "E_JOIN_LAW"
           "E_KEY_CANONICAL"
           "E_DECLARATION"))
        (and (equal? code "E_CAP") (not (equal? (hash-ref (diagnostic-data item) 'cap) "length")))))
  (define selected
    (cond
     ((and (current-program-span)
           (member
            code
            '("E_ENSURE" "E_FACT_REJECT" "E_CONTEXT" "E_LIBRARY_DOMAIN" "E_LIBRARY_PARENT")))
      (current-program-span))
     ((and span whole-line?) (source-line-span span))
     (else span)))
  (define data (diagnostic-data item))
  (define kind (hash-ref data 'kind #f))
  (define line-payload?
    (or (member code '("E_LINE_SHAPE" "E_FORM_NONE" "E_QUESTION"))
        (and (equal? code "E_DECLARATION")
             (member kind '("header" "body" "orphan" "syntax"))
             (not (and (equal? kind "body") (equal? (hash-ref data 'actual #f) "EOF"))))))
  (define raw
    (and selected
         (hash-ref
          (current-source-text-lines)
          (cons (src-span-file selected) (src-span-start-line selected))
          #f)))
  (define projected-data
    (if (and line-payload? raw)
      (hash-set data 'actual (string-trim raw #px"[ \t]" #:repeat? #t))
      data))
  (if selected
    (struct-copy
     diagnostic
     item
     (file (src-span-file selected))
     (range selected)
     (data projected-data))
    (struct-copy diagnostic item (data projected-data))))

(define (record-diagnostics! items)
  (define sink (current-diagnostic-sink))
  (if sink
    (for ((item (in-list items))) (sink (routed-diagnostic item)))
    (raise-diagnostics items)))

(define (recover thunk (fallback #f) #:span (span #f))
  (with-handlers
   ((exn:fail:typify?
     (lambda (error)
       (record-diagnostics!
        (for/list
         ((item (in-list (exn:fail:typify-diagnostics error))))
         (if span (struct-copy diagnostic item (file (src-span-file span)) (range span)) item)))
       fallback)))
   (thunk)))

(define (parse-source-term source span)
  (define expression (parse-term source #:span span))
  (define (remember item)
    (when (term? item)
      (define begin (- (src-span-start-byte (term-span item)) (src-span-start-byte span)))
      (define finish (- (src-span-end-byte (term-span item)) (src-span-start-byte span)))
      (hash-set!
       term-source-cache
       item
       (bytes->string/utf-8 (subbytes (string->bytes/utf-8 source) begin finish)))
      (for-each remember (term-arguments item))))
  (remember expression)
  expression)

(define (term-source expression)
  (hash-ref
   term-source-cache
   expression
   (lambda ()
     (string-join
      (cons
       (term-constructor expression)
       (for/list
        ((argument (in-list (term-arguments expression))))
        (cond
         ((term? argument) (string-append "(" (term-source argument) ")"))
         ((capture? argument) (string-append "<" (capture-name argument) ">"))
         (else (literal-value argument)))))
      " "))))

(define (reserved-name? name)
  (or (member name builtin-class-names)
      (hash-has-key? library-arities name)
      (hash-has-key? (current-meta-classes) name)))

(define (term-data value)
  (cond
   ((term? value) (cons (term-constructor value) (map term-data (term-arguments value))))
   ((capture? value) (hash 'capture (capture-name value)))
   ((literal? value) (literal-value value))
   (else value)))

(define (words text) (string-split text))

(define (top-level-delimiters source delimiter)
  (define width (string-length delimiter))
  (define size (string-length source))
  (let loop ((index 0) (depth 0) (quoted? #f) (positions '()))
    (cond
     ((>= index size) (reverse positions))
     (quoted?
      (case (string-ref source index)
        ((#\\) (loop (min size (+ index 2)) depth #t positions))
        ((#\") (loop (add1 index) depth #f positions))
        (else (loop (add1 index) depth #t positions))))
     (else
      (case (string-ref source index)
        ((#\") (loop (add1 index) depth #t positions))
        ((#\() (loop (add1 index) (add1 depth) #f positions))
        ((#\)) (loop (add1 index) (max 0 (sub1 depth)) #f positions))
        (else
         (if (and (zero? depth)
                  (<= (+ index width) size)
                  (equal? (substring source index (+ index width)) delimiter))
           (loop (+ index width) depth #f (cons index positions))
           (loop (add1 index) depth #f positions))))))))

(define (class-run thunk span)
  (with-handlers
   ((class-error?
     (lambda (error)
       (define data (class-error-data error))
       (define code (class-error-code error))
       (define-values
        (native-code native-data)
        (cond
         ((equal? code "class-reference")
          (if (hash-has-key? data 'class)
            (values "E_CLASS_UNDECLARED" (hash 'class (hash-ref data 'class)))
            (raise error)))
         ((equal? code "class-state-cap")
          (values
           "E_CAP"
           (hash 'cap "states" 'limit (hash-ref data 'cap) 'actual (hash-ref data 'states))))
         ((equal? code "class-repetition-cap")
          (values
           "E_CAP"
           (hash 'cap "repetition" 'limit (hash-ref data 'cap) 'actual (hash-ref data 'count))))
         ((equal? code "class-reference-cap")
          (values
           "E_CAP"
           (hash 'cap "depth" 'limit (hash-ref data 'cap) 'actual (hash-ref data 'actual))))
         ((equal? code "class-work-cap")
          (values
           "E_CAP"
           (hash
            'cap
            (hash-ref data 'resource)
            'limit
            (hash-ref data 'limit)
            'actual
            (hash-ref data 'actual))))
         ((equal? code "class-reserved")
          (values "E_DECLARATION" (hash 'kind "duplicate" 'actual (hash-ref data 'class))))
         ((equal? code "class-reference-cycle")
          (values "E_DECLARATION" (hash 'kind "class-cycle" 'actual (hash-ref data 'class))))
         ((member code '("class-syntax" "class-except" "class-host-predicate"))
          (if (hash-has-key? data 'source)
            (values "E_DECLARATION" (hash 'kind "syntax" 'actual (hash-ref data 'source)))
            (raise error)))
         ((equal? code "class-caps")
          (cond
           ((hash-has-key? data 'value)
            (values "E_DECLARATION" (hash 'kind "syntax" 'actual (hash-ref data 'value))))
           ((hash-has-key? data 'cap)
            (values "E_DECLARATION" (hash 'kind "syntax" 'actual (hash-ref data 'cap))))
           (else (raise error))))
         (else (raise error))))
       (fail native-code (exn-message error) span native-data))))
   (thunk)))

(define meta-class-cache (make-hash))

(define (compiled-meta-classes)
  (hash-ref!
   meta-class-cache
   (current-meta-classes)
   (lambda () (compile-classes (current-meta-classes)))))

(define current-reader-meta (make-parameter #f))

(define (parts production span)
  (with-handlers
   ((exn:fail:typify?
     (lambda (error)
       (define item (exn:fail:typify-diagnostic error))
       (if (equal? (diagnostic-code item) "E_CLASS_OVERLAP")
         (raise
          (struct-copy
           exn:fail:typify
           error
           (diagnostic
            (struct-copy
             diagnostic
             item
             (data (hash-set (diagnostic-data item) 'form production))))))
         (raise error)))))
   (parts/internal production span)))

(define (parts/internal production span)
  (define classified (car (read-lines production #:file (src-span-file span) #:comment-prefix "")))
  (define record
    (if (and (eq? (line-record-kind classified) 'unclassified)
             (string-suffix? production "!")
             (not (regexp-match? #rx"[:\r\n]|->" production))
             (regexp-match? #rx"<" production))
      (struct-copy
       line-record
       classified
       (kind 'bang)
       (left (substring production 0 (sub1 (string-length production)))))
      classified))
  (when (memq (line-record-kind record) '(blank comment unclassified))
    (declaration-fail "syntax" production "A form must have a bang, colon or arrow shape" span))
  (values
   (line-record-kind record)
   (parse-template
    (line-record-left record)
    #:span
    (span-slice
     span
     production
     (- (src-span-start-column (line-left-span record)) 1)
     (- (src-span-end-column (line-left-span record)) 1)))
   (and (memq (line-record-kind record) '(colon arrow))
        (parse-template
         (line-record-right record)
         #:span
         (span-slice
          span
          production
          (- (src-span-start-column (line-right-span record)) 1)
          (- (src-span-end-column (line-right-span record)) 1))))))

(define (form-captures kind left right record)
  (and (eq? kind (line-record-kind record))
       (let ((a (match-template left (line-record-left record)))
             (b (if right (match-template right (line-record-right record)) (hash))))
         (and a
              b
              (for/fold ((result a)) (((key value) (in-hash b))) (hash-set result key value))))))

(define (captures-fit? captures classes)
  (for/and
   (((name value) (in-hash captures)))
   (and (hash-has-key? classes name) (class-accepts? (hash-ref classes name) value))))

(define (validate-meta-arguments! name arguments signature span)
  (unless (= (length arguments) (length signature))
    (fail
     "E_TERM_CLASS"
     "The meta action has the wrong number of arguments"
     span
     (hash
      'kind
      "arity"
      'constructor
      name
      'class
      "signature"
      'actual
      (length arguments)
      'expected
      (length signature))))
  (for
   ((value (in-list arguments)) (class-name (in-list signature)))
   (unless (hash-has-key? (compiled-meta-classes) class-name)
     (fail
      "E_CLASS_UNDECLARED"
      "The meta signature names an undeclared class"
      span
      (hash 'class class-name)))
   (unless (and
            (string? value)
            (class-accepts? (hash-ref (compiled-meta-classes) class-name) value))
     (fail
      "E_TERM_CLASS"
      "The meta action violates its argument class"
      span
      (hash 'kind "result" 'constructor name 'class class-name 'actual value)))))

(define (meta-row record)
  (when (and
         (not (current-reader-meta))
         (eq? (line-record-kind record) 'colon)
         (member (line-record-left record) '("term" "primitive"))
         (regexp-match? #px"^[^:]+:[ \t]*$" (line-record-right record)))
    (declaration-fail
     "syntax"
     (line-record-raw record)
     "A nullary constructor has no trailing colon"
     (line-record-span record)))
  (define-values
   (classified-hit class-failure)
   (if (current-reader-meta)
     (scan-forms (current-reader-meta) record)
     (scan-templates
      (current-meta-forms)
      record
      (compiled-meta-classes)
      (lambda (entry) (parts (hash-ref entry 'production) (line-record-span record))))))
  (unless classified-hit
    (refuse-form!
     record
     (if (and class-failure (equal? (capture-failure-name class-failure) "constructor"))
       (struct-copy capture-failure class-failure (name "name"))
       class-failure)))
  (define from-language
    (and (current-reader-meta)
         (let ((hit classified-hit))
           (and hit
                (let* ((value (term-value (form-meaning (car hit)) (cdr hit) (current-reader-meta)))
                       (normalized (normalize-terms (current-reader-meta) (list value))))
                  (unless (= (length normalized) 1)
                    (declaration-fail
                     "arity"
                     normalized
                     "A meta line must reduce to exactly one meta primitive"
                     (line-record-span record)))
                  (define action (car normalized))
                  (define entry
                    (for/first
                     ((row (in-list meta-forms))
                      #:when
                      (equal? (hash-ref row 'constructor) (car action)))
                     row))
                  (unless entry
                    (declaration-fail
                     "reference"
                     (car action)
                     "The meta primitive is not in the kernel"
                     (line-record-span record)))
                  (validate-meta-arguments!
                   (car action)
                   (cdr action)
                   (hash-ref entry 'arguments)
                   (line-record-span record))
                  (hash
                   'kind
                   (hash-ref entry 'kind)
                   'captures
                   (for/hash
                    ((name (in-list (hash-ref entry 'arguments))) (argument (in-list (cdr action))))
                    (values name argument))
                   'record
                   record))))))
  (or from-language
      (and (not (current-reader-meta))
           (for/or
            ((entry (in-list (list (car classified-hit)))))
            (define-values
             (kind left right)
             (parts (hash-ref entry 'production) (line-record-span record)))
            (define captures (cdr classified-hit))
            (and captures
                 (captures-fit? captures (compiled-meta-classes))
                 (let* ((slots (filter slot? (append left (or right '()))))
                        (arguments (map (lambda (part) (hash-ref captures (slot-name part))) slots))
                        (signature (hash-ref entry 'arguments))
                        (operation
                         (for/first
                          ((original (in-list meta-forms))
                           #:when
                           (equal? (hash-ref original 'constructor) (hash-ref entry 'constructor)))
                          original)))
                   (unless operation
                     (declaration-fail
                      "reference"
                      (hash-ref entry 'constructor)
                      "The meta primitive is not in the kernel"
                      (line-record-span record)))
                   (validate-meta-arguments!
                    (hash-ref entry 'constructor)
                    arguments
                    signature
                    (line-record-span record))
                   (hash
                    'kind
                    (hash-ref operation 'kind)
                    'captures
                    (for/hash
                     ((name (in-list (hash-ref operation 'arguments)))
                      (argument (in-list arguments)))
                     (values name argument))
                    'record
                    record)))))
      (fail
       "E_FORM_NONE"
       "The line is not a declared meta form"
       (line-record-span record)
       (hash 'actual (line-record-raw record)))))

(define (capture-value row key)
  (define captures (hash-ref row 'captures))
  (hash-ref
   captures
   key
   (lambda ()
     (define aliases
       (cond
        ((equal? key "value") '("prefix" "production" "term" "pattern" "law"))
        ((equal? key "ordering") '("order-mode"))
        (else '())))
     (define found
       (for/first ((alias (in-list aliases)) #:when (hash-has-key? captures alias)) alias))
     (if found
       (hash-ref captures found)
       (error 'capture-value "Missing internal row field: ~a" key)))))

(define (row-span row) (line-record-span (hash-ref row 'record)))

(define (row-source row) (line-record-raw (hash-ref row 'record)))

(define (ordered-constructors constructors)
  (sort (hash-values constructors) < #:key constructor-ordinal))

(define (check-rule-cycles! constructors rule-lefts)
  (define indices (make-hash))
  (define lowlinks (make-hash))
  (define active (make-hash))
  (define stack '())
  (define next-index 0)
  (define components '())
  (define (dependencies name)
    (define item (hash-ref constructors name))
    (for/list
     ((right (in-list (append-map effect-term-calls (cdr (constructor-rule item)))))
      #:when
      (let ((next (hash-ref constructors (term-constructor right) #f)))
        (and next (constructor-rule next))))
     (term-constructor right)))
  (define (position name) (src-span-start-byte (term-span (hash-ref rule-lefts name))))
  (define (visit name)
    (hash-set! indices name next-index)
    (hash-set! lowlinks name next-index)
    (set! next-index (add1 next-index))
    (set! stack (cons name stack))
    (hash-set! active name #t)
    (for
     ((next (in-list (dependencies name))))
     (cond
      ((not (hash-has-key? indices next))
       (visit next)
       (hash-set! lowlinks name (min (hash-ref lowlinks name) (hash-ref lowlinks next))))
      ((hash-ref active next #f)
       (hash-set! lowlinks name (min (hash-ref lowlinks name) (hash-ref indices next))))))
    (when (= (hash-ref lowlinks name) (hash-ref indices name))
      (define component
        (let pop ((members '()))
          (define member (car stack))
          (set! stack (cdr stack))
          (hash-remove! active member)
          (define collected (cons member members))
          (if (equal? member name) collected (pop collected))))
      (when (or (> (length component) 1) (member name (dependencies name)))
        (set! components (cons (sort component < #:key position) components)))))
  (for
   ((name (in-list (sort (hash-keys rule-lefts) < #:key position))))
   (unless (hash-has-key? indices name) (visit name)))
  (for
   ((component (in-list (sort components < #:key (lambda (names) (position (car names)))))))
   (define name (car component))
   (recover
    (lambda ()
      (fail
       "E_RULE_CYCLE"
       "Rewrite dependencies form a cycle"
       (term-span (hash-ref rule-lefts name))
       (hash 'constructor name))))))

(define (effect-term-calls expression)
  (define args (term-arguments expression))
  (cond
   ((equal? (term-constructor expression) "if")
    (if (and (= (length args) 2) (term? (cadr args))) (effect-term-calls (cadr args)) '()))
   ((equal? (term-constructor expression) "each")
    (if (and (= (length args) 4) (term? (cadddr args))) (effect-term-calls (cadddr args)) '()))
   (else (list expression))))

(define fact-names '("exists" "directory" "symlink" "mode" "owner" "clean"))

(define state-predicate-names '("bound" "masked" "visible" "absent" "covered" "uncovered"))

(define library-predicate-names '("prefix" "suffix" "equal"))

(define (expression-arity-fail expression signature)
  (fail
   "E_TERM_CLASS"
   "The expression has the wrong number of arguments"
   (term-span expression)
   (hash
    'kind
    "arity"
    'constructor
    (term-constructor expression)
    'class
    signature
    'actual
    (length (term-arguments expression)))))

(define (validate-string-expression!
         expression
         constructors
         classes
         captures
         upper
         record-check
         #:receiving
         (receiving "string"))
  (cond
   ((literal? expression) (void))
   ((capture? expression)
    (unless (hash-has-key? captures (capture-name expression))
      (declaration-fail
       "capture"
       (string-append "<" (capture-name expression) ">")
       "The expression uses an unbound capture"
       (capture-span expression))))
   ((term? expression)
    (when (hash-has-key? constructors (term-constructor expression))
      (fail
       "E_TERM_NESTED"
       "A string expression cannot contain a user constructor"
       (term-span expression)
       (hash 'constructor receiving 'actual (term-source expression))))
    (unless (or
             (hash-has-key? library-arities (term-constructor expression))
             (equal? (term-constructor expression) "context")
             (member (term-constructor expression) fact-names)
             (member (term-constructor expression) state-predicate-names))
      (declaration-fail
       "reference"
       (term-constructor expression)
       "The expression names no function"
       (term-span expression)))
    (unless (member (term-constructor expression) '("parent" "join" "concat" "context"))
      (fail
       "E_TERM_CLASS"
       "The expression must have string sort"
       (term-span expression)
       (hash
        'kind
        "sort"
        'constructor
        (term-constructor expression)
        'class
        "string"
        'actual
        (if (member (term-constructor expression) '("split" "ancestors" "inherited"))
          "list"
          "predicate"))))
    (validate-term! expression constructors classes captures upper #:record-check record-check))
   (else
    (declaration-fail
     "syntax"
     (term-data expression)
     "The expression is not a string expression"
     (src-span "<input>" 1 1 1 1 0 0)))))

(define (validate-predicate! expression constructors classes captures upper model record-check)
  (unless (term? expression)
    (declaration-fail
     "syntax"
     (term-data expression)
     "A predicate must be a parenthesized call"
     (if (literal? expression) (literal-span expression) (capture-span expression))))
  (define name (term-constructor expression))
  (define args (term-arguments expression))
  (unless (or
           (member name library-predicate-names)
           (member name state-predicate-names)
           (member name fact-names))
    (declaration-fail
     "reference"
     name
     "The predicate is not a library, state or collector predicate"
     (term-span expression)))
  (when (and (member name library-predicate-names) (not (= (length args) 2)))
    (expression-arity-fail expression "string string"))
  (when (and (equal? name "clean") (not (= (length args) 2)))
    (expression-arity-fail expression "canonical-path string"))
  (when (member name state-predicate-names)
    (unless (and
             (pair? args)
             (literal? (car args))
             (hash-has-key? (compiled-model-cells model) (literal-value (car args))))
      (declaration-fail
       "reference"
       (if (pair? args) (term-data (car args)) "")
       "The state predicate names no declared cell"
       (term-span expression))))
  (when (member name state-predicate-names)
    (define width (model-key-width model (literal-value (car args))))
    (when (and width (not (= (length args) (add1 width))))
      (expression-arity-fail
       expression
       (string-join (cons "cell" (make-list width "string")) " "))))
  (when (and
         (member name '("covered" "uncovered"))
         (not
          (model-cell-stacking?
           (hash-ref (compiled-model-cells model) (literal-value (car args))))))
    (declaration-fail
     "syntax"
     (term-data expression)
     "Coverage predicates require a stacking cell"
     (term-span expression)))
  (for
   ((argument (in-list args)))
   (validate-string-expression!
    argument
    constructors
    classes
    captures
    upper
    record-check
    #:receiving
    name)))

(define (validate-list-expression! expression constructors classes captures upper record-check)
  (when (and (term? expression) (equal? (term-constructor expression) "context"))
    (unless (= (length (term-arguments expression)) 1) (expression-arity-fail expression "string"))
    (fail
     "E_TERM_CLASS"
     "A list source cannot receive a string-valued context"
     (term-span expression)
     (hash 'kind "sort" 'constructor "context" 'class "list" 'actual "string")))
  (unless (and
           (term? expression)
           (member (term-constructor expression) '("split" "ancestors" "inherited")))
    (declaration-fail
     "syntax"
     (term-data expression)
     "A list source must be split, ancestors or inherited"
     (cond
      ((term? expression) (term-span expression))
      ((literal? expression) (literal-span expression))
      (else (capture-span expression)))))
  (when (and
         (member (term-constructor expression) '("split" "ancestors"))
         (not (= (length (term-arguments expression)) 1)))
    (expression-arity-fail expression "canonical-path"))
  (when (equal? (term-constructor expression) "inherited")
    (validate-term! expression constructors classes captures upper #:record-check record-check))
  (for
   ((argument (in-list (term-arguments expression))))
   (validate-string-expression!
    argument
    constructors
    classes
    captures
    upper
    record-check
    #:receiving
    (term-constructor expression))))

(define (validate-effect! expression constructors classes captures upper model fuel record-check)
  (define name (term-constructor expression))
  (define args (term-arguments expression))
  (cond
   ((equal? name "if")
    (unless (and (= (length args) 2) (term? (cadr args)))
      (declaration-fail
       "syntax"
       (term-data expression)
       "if requires a predicate and an effect term"
       (term-span expression)))
    (when (and (term? (car args)) (member (term-constructor (car args)) state-predicate-names))
      (declaration-fail
       "syntax"
       (term-data (car args))
       "State predicates are evaluated in ensure clauses"
       (term-span (car args))))
    (validate-predicate! (car args) constructors classes captures upper model record-check)
    (validate-effect! (cadr args) constructors classes captures upper model fuel record-check))
   ((equal? name "each")
    (unless (and
             (= (length args) 4)
             (capture? (car args))
             (literal? (cadr args))
             (equal? (literal-value (cadr args)) "in")
             (term? (cadddr args)))
      (declaration-fail
       "syntax"
       (term-data expression)
       "each requires a binder, in, a list and an effect term"
       (term-span expression)))
    (unless fuel
      (declaration-fail
       "syntax"
       (term-data expression)
       "each requires a fuel declaration on its constructor"
       (term-span expression)))
    (validate-list-expression! (caddr args) constructors classes captures upper record-check)
    (validate-effect!
     (cadddr args)
     constructors
     classes
     (hash-set captures (capture-name (car args)) 'string)
     upper
     model
     fuel
     record-check))
   (else
    (unless (hash-has-key? constructors name)
      (declaration-fail
       "reference"
       name
       "A rule emits declared constructors"
       (term-span expression)))
    (validate-term! expression constructors classes captures upper #:record-check record-check))))

(struct declaration-automata (fields whole checks) #:transparent)

(define declaration-automata-cache (make-weak-hasheq))

(define language-model-cache (make-weak-hasheq))

(define language-keyed-count-cache (make-weak-hasheq))

(define (language-model body) (hash-ref language-model-cache body))

(define (language-keyed-counts body) (hash-ref language-keyed-count-cache body (hash)))

(define (validate-example-meaning body expression bindings)
  (define deferred (gensym 'lookup-value))
  (define (argument value)
    (cond
     ((literal? value) (literal-value value))
     ((capture? value) (hash-ref bindings (capture-name value) deferred))
     ((term? value)
      (if (member (term-constructor value) '("context" "inherited"))
        deferred
        (let ((arguments (map argument (term-arguments value))))
          (if (or (member deferred arguments)
                  (not (hash-has-key? library-arities (term-constructor value))))
            deferred
            (library-call (term-constructor value) arguments (term-span value))))))
     (else deferred)))
  (define item (hash-ref (language-constructors body) (term-constructor expression) #f))
  (when item
    (for
     ((value (in-list (map argument (term-arguments expression))))
      (class-name (in-list (constructor-signature item))))
     (unless (or
              (eq? value deferred)
              (and (string? value)
                   (class-accepts? (hash-ref (language-classes body) class-name) value)))
       (fail
        "E_TERM_CLASS"
        "An example's computed argument fails its receiving class"
        (term-span expression)
        (hash
         'kind
         "result"
         'constructor
         (constructor-name item)
         'class
         class-name
         'actual
         value))))))

(define (constructor-descriptors constructors)
  (for/hash
   (((name item) (in-hash constructors)))
   (values
    name
    (hash
     'signature
     (constructor-signature item)
     'primitive?
     (constructor-primitive? item)
     'rule?
     (and (constructor-rule item) #t)
     'fact?
     (constructor-fact? item)
     'ordinal
     (constructor-ordinal item)))))

(define check-bounds-scope
  "Recorded successful signature inclusion, delimiter and form overlap checks; automaton size and product-state upper bounds, not CPU steps or elapsed time")

(define (declaration-check-bounds body)
  (define cached (hash-ref declaration-automata-cache body #f))
  (or (and cached (declaration-automata-checks cached))
      (hash 'recorded #f 'scope check-bounds-scope)))

(define (field-automaton classes candidate right?)
  (define template (if right? (form-right candidate) (form-left candidate)))
  (and template
       (let* ((raw
               (class-sequence
                (map
                 (lambda (part) (if (slot? part) (hash-ref classes (slot-name part)) part))
                 template)
                #:name
                "field"))
              (trimmed (class-trimmed raw))
              (forbidden
               (case (form-kind candidate)
                 ((colon) (if right? '() '(":")))
                 ((arrow) (if right? '(":") '(":" "->")))
                 ((bang) '(":" "->" "!" " ")))))
         (define lexical-restricted
           (for/fold
            ((machine trimmed))
            ((literal (in-list forbidden)))
            (class-without-literal
             machine
             (if (symbol? literal) (symbol->string literal) literal))))
         (define helper-caps
           (hash-set
            (compiled-class-caps lexical-restricted)
            'states
            (hash-ref default-class-caps 'states)))
         (define restricted
           (if right?
             lexical-restricted
             (class-intersection
              lexical-restricted
              (hash-ref
               (compile-classes (hash "ordinary" "/.*/ except \"?\"") #:caps helper-caps)
               "ordinary"))))
         (if (or (eq? (form-kind candidate) 'bang)
                 (eq? (form-kind candidate) 'arrow)
                 (and (eq? (form-kind candidate) 'colon) (not right?)))
           (class-intersection
            restricted
            (hash-ref
             (compile-classes
              (hash
               "nonempty"
               (if (eq? (form-kind candidate) 'bang) "/[A-Za-z0-9][A-Za-z0-9-]*/" "/.+/"))
              #:caps
              helper-caps)
             "nonempty"))
           restricted))))

(define (form-automata body candidate)
  (define cached (hash-ref declaration-automata-cache body #f))
  (define fields (and cached (hash-ref (declaration-automata-fields cached) candidate #f)))
  (if fields
    (values (car fields) (cdr fields))
    (values
     (field-automaton (language-classes body) candidate #f)
     (field-automaton (language-classes body) candidate #t))))

(define (read-declarations text #:file (file "<input>") #:meta (meta #f))
  (define-values (model diagnostics) (read-declarations/diagnostics text #:file file #:meta meta))
  (raise-diagnostics diagnostics)
  model)

(define (read-declarations/diagnostics text #:file (file "<input>") #:meta (meta #f))
  (define diagnostics '())
  (define source-lines
    (for/hash
     ((record (in-list (read-lines text #:file file))))
     (values
      (cons
       (src-span-file (line-record-span record))
       (src-span-start-line (line-record-span record)))
      (line-record-span record))))
  (define source-text-lines
    (for/hash
     ((record (in-list (read-lines text #:file file))))
     (values
      (cons
       (src-span-file (line-record-span record))
       (src-span-start-line (line-record-span record)))
      (line-record-raw record))))
  (define model
    (parameterize
     ((current-reader-meta meta)
      (current-bootstrap? (equal? text (meta-source)))
      (current-source-lines source-lines)
      (current-source-text-lines source-text-lines)
      (current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics)))))
     (read-declarations/internal text file)))
  (values model (diagnostics-sort (reverse diagnostics))))

(define (read-declarations/internal text file)
  (define lines (read-lines text #:file file))
  (define header-valid?
    (and (pair? lines)
         (not (regexp-match? #rx"\r" (line-record-raw (car lines))))
         (equal?
          (string-trim (line-record-raw (car lines)) #px"[ \t]" #:repeat? #t)
          "#lang typify")))
  (unless header-valid?
    (recover
     (lambda ()
       (fail
        "E_DECLARATION"
        "A declaration file must open with #lang typify"
        (if (pair? lines) (line-record-span (car lines)) (src-span file 1 1 1 1 0 0))
        (hash 'kind "header" 'actual (if (pair? lines) (line-record-raw (car lines)) ""))))))
  (define bodies '())
  (define current #f)
  (define opening #f)
  (define rows '())
  (define enclosing '())
  (define (finish-body!)
    (define completed (recover (lambda () (compile-body current rows opening))))
    (when completed (set! bodies (cons completed bodies)))
    (cond
     ((pair? enclosing)
      (define frame (car enclosing))
      (set! current (car frame))
      (set! opening (cadr frame))
      (set! rows (caddr frame))
      (set! enclosing (cdr enclosing)))
     (else (set! current #f) (set! opening #f) (set! rows '()))))
  (for
   ((record (in-list (if header-valid? (cdr lines) lines))))
   (recover
    (lambda ()
      (case (line-record-kind record)
        ((blank) (void))
        ((comment)
         (fail
          "E_DECLARATION"
          "Declaration prose belongs outside the declaration"
          (line-record-span record)
          (hash 'kind "syntax" 'actual (line-record-raw record))))
        (else
         (define row (meta-row record))
         (case (hash-ref row 'kind)
           ((declare)
            (when current
              (recover
               (lambda ()
                 (fail
                  "E_DECLARATION"
                  "Close the current language before declaring another"
                  (row-span row)
                  (hash 'kind "body" 'actual (row-source row)))))
              (set! enclosing (cons (list current opening rows) enclosing)))
            (set! current (capture-value row "name"))
            (set! opening (row-span row))
            (set! rows '()))
           ((end)
            (unless current
              (declaration-fail "body" (row-source row) "No language is open" (row-span row)))
            (finish-body!))
           (else
            (unless current
              (declaration-fail
               "body"
               (row-source row)
               "Declare a language before its contents"
               (row-span row)))
            (set! rows (append rows (list row))))))))))
  (let loop ()
    (when current
      (recover (lambda () (declaration-fail "body" "EOF" "The language has no end!" opening)))
      (finish-body!)
      (loop)))
  (define ordered-bodies
    (sort bodies < #:key (lambda (body) (src-span-start-byte (language-span body)))))
  (define result
    (for/fold
     ((result (hash)))
     ((body (in-list ordered-bodies)))
     (recover
      (lambda ()
        (when (hash-has-key? result (language-name body))
          (declaration-fail
           "duplicate"
           (language-name body)
           "The language is declared twice"
           (language-span body)))))
     (if (hash-has-key? result (language-name body))
       result
       (hash-set result (language-name body) body))))
  (for*
   ((body (in-list ordered-bodies))
    (row (in-list (language-rows body)))
    #:when
    (eq? (hash-ref row 'kind) 'sees))
   (define name (capture-value row "name"))
   (unless (hash-has-key? result name)
     (recover
      (lambda ()
        (fail
         "E_DECLARATION"
         "The referenced language is not declared"
         (row-span row)
         (hash 'kind "reference" 'actual name))))))
  (define declared-constructors
    (for*/fold
     ((declared (hash)))
     ((body (in-list ordered-bodies)) (name (in-list (hash-keys (language-constructors body)))))
     (hash-update declared name (lambda (bodies) (append bodies (list body))) '())))
  (for
   ((body (in-list ordered-bodies)))
   (hash-set! language-lookup-cache body (hash))
   (hash-set! language-composition-cache body ordered-bodies))
  (define (check-lookups body expression)
    (when (term? expression)
      (when (and
             (member (term-constructor expression) '("context" "inherited"))
             (= (length (term-arguments expression)) 1)
             (literal? (car (term-arguments expression))))
        (define name (literal-value (car (term-arguments expression))))
        (recover
         (lambda ()
           (define pieces (string-split name "." #:trim? #f))
           (define candidates
             (if (= (length pieces) 1)
               (hash-ref declared-constructors name '())
               (if (and (= (length pieces) 2)
                        (hash-has-key? result (car pieces))
                        (hash-has-key?
                         (language-constructors (hash-ref result (car pieces)))
                         (cadr pieces)))
                 (list (hash-ref result (car pieces)))
                 '())))
           (unless (= (length candidates) 1)
             (raise-typify
              "E_DECLARATION"
              "The lookup must name exactly one declaring language"
              #:span
              (term-span expression)
              #:data
              (hash 'kind "reference" 'actual name)
              #:related
              (for/list
               ((candidate (in-list candidates)))
               (hash
                'language
                (language-name candidate)
                'file
                (src-span-file (language-span candidate))
                'range
                (src-span->jsexpr (language-span candidate))))))
           (define target (car candidates))
           (define constructor-name (last pieces))
           (define item (hash-ref (language-constructors target) constructor-name))
           (unless (= (length (constructor-signature item)) 1)
             (declaration-fail
              "arity"
              name
              "A lookup requires a unary constructor"
              (term-span expression)))
           (hash-set!
            language-lookup-cache
            body
            (hash-set
             (hash-ref language-lookup-cache body)
             name
             (string-append (language-name target) "." constructor-name))))))
      (for-each (lambda (argument) (check-lookups body argument)) (term-arguments expression))))
  (for
   ((body (in-list ordered-bodies)))
   (for ((candidate (in-list (language-forms body)))) (check-lookups body (form-meaning candidate)))
   (for
    ((item (in-list (ordered-constructors (language-constructors body)))))
    (when (constructor-rule item)
      (for-each (lambda (right) (check-lookups body right)) (cdr (constructor-rule item)))))
   (define model (language-model body))
   (for*
    ((clauses (in-hash-values (compiled-model-ensures model))) (clause (in-list clauses)))
    (check-lookups body (model-ensure-expression clause)))
   (for*
    ((clauses (in-hash-values (compiled-model-reads model))) (clause (in-list clauses)))
    (check-lookups body (model-read-fact clause)))
   (for
    ((expression (in-hash-values (compiled-model-guards model))))
    (check-lookups body expression)))
  result)

(define (compile-body name rows opening)
  (parameterize
   ((current-partition-model #f) (current-template-names '()))
   (class-run (lambda () (compile-body/internal name rows opening)) opening)))

(define (compile-body/internal name rows opening)
  (define sources (hash))
  (define source-rows (make-hash))
  (define cap-rows (make-hash))
  (define invalid-classes (make-hash))
  (define caps (hash))
  (define constructors (hash))
  (define constructor-order '())
  (define pending-forms '())
  (define forms '())
  (define counts (hash))
  (define keyed-counts (hash))
  (define rules '())
  (define rule-attempts (make-hash))
  (define rule-lefts (make-hash))
  (define facts (hash))
  (define properties (hash))
  (define defaults '())
  (define sees '())
  (define notes '())
  (define current-form #f)
  (define signature-checks '())
  (define delimiter-checks '())
  (define overlap-checks '())
  (define (record-signature! site entry)
    (set! signature-checks (cons (hash-set entry 'site site) signature-checks)))
  (define (property! key value row)
    (when (hash-has-key? properties key)
      (fail
       "E_DECLARATION"
       "The property is declared twice"
       (row-span row)
       (hash 'kind "duplicate" 'actual (symbol->string key))))
    (set! properties (hash-set properties key value)))
  (define (finish-form!)
    (when current-form
      (define pending current-form)
      (set! current-form #f)
      (unless (or (hash-ref pending 'meaning #f) (hash-ref pending 'meaning-seen? #f))
        (declaration-fail
         "syntax"
         (hash-ref pending 'source)
         "Every form requires does"
         (hash-ref pending 'span)))
      (when (hash-ref pending 'meaning #f)
        (set! pending-forms (append pending-forms (list pending))))))
  (for
   ((row (in-list rows)))
   (define kind (hash-ref row 'kind))
   (define span (row-span row))
   (when (and
          current-form
          (not (hash-ref current-form 'meaning #f))
          (not (memq kind '(does restricts))))
     (recover finish-form!))
   (recover
    (lambda ()
      (case kind
        ((class)
         (define key (capture-value row "name"))
         (when (or
                (and (reserved-name? key) (not (current-bootstrap?)))
                (member key '("letter" "canonical-path"))
                (hash-has-key? sources key))
           (declaration-fail "duplicate" key "The class is declared twice or reserved" span))
         (set! sources (hash-set sources key (capture-value row "value")))
         (hash-set! source-rows key row))
        ((cap)
         (define key (capture-value row "limit"))
         (when (hash-has-key? caps key)
           (declaration-fail "duplicate" key "The cap is declared twice" span))
         (define value (string->number (capture-value row "number")))
         (unless (positive? value)
           (declaration-fail "syntax" (row-source row) "Every cap must be positive" span))
         (unless (and
                  (hash-has-key? meta-cap-defaults key)
                  (<= value (hash-ref meta-cap-defaults key)))
           (fail
            "E_CAP"
            "A body may lower a cap, never raise it"
            span
            (hash 'cap key 'limit (hash-ref meta-cap-defaults key) 'actual value)))
         (set! caps (hash-set caps key value))
         (hash-set! cap-rows key row))
        ((version) (property! 'version (capture-value row "number") row))
        ((comment) (property! 'comment (capture-value row "value") row))
        ((order) (property! 'order (capture-value row "ordering") row))
        ((file) (property! 'file (capture-value row "value") row))
        ((required) (property! 'required #t row))
        ((optional) (property! 'required #f row))
        ((term term-zero primitive primitive-zero)
         (define key (capture-value row "name"))
         (define signature
           (if (memq kind '(term-zero primitive-zero)) '() (words (capture-value row "value"))))
         (when (and (not (memq kind '(term-zero primitive-zero))) (null? signature))
           (declaration-fail
            "syntax"
            (row-source row)
            "A nullary constructor has no trailing colon"
            span))
         (define kernel-library?
           (and (current-bootstrap?)
                (eq? kind 'primitive)
                (hash-has-key? library-arities key)
                (equal? signature (make-list (car (hash-ref library-arities key)) "value"))))
         (when (or
                (hash-has-key? constructors key)
                (member key '("if" "each" "context"))
                (and (reserved-name? key) (not kernel-library?)))
           (fail
            "E_DECLARATION"
            "The constructor is declared twice or reserved"
            span
            (hash 'kind "duplicate" 'actual key)))
         (set! constructors
           (hash-set
            constructors
            key
            (constructor
             key
             signature
             (and (memq kind '(primitive primitive-zero)) #t)
             #f
             #f
             span
             (length constructor-order))))
         (set! constructor-order (append constructor-order (list key))))
        ((fact)
         (define key (capture-value row "name"))
         (when (hash-has-key? facts key)
           (declaration-fail "duplicate" key "The fact is declared twice" span))
         (set! facts (hash-set facts key span)))
        ((form)
         (finish-form!)
         (set! current-form
           (hash
            'source
            (capture-value row "value")
            'span
            span
            'production-span
            (line-right-span (hash-ref row 'record))
            'meaning
            #f
            'restricting?
            #f
            'examples
            '()
            'defaults
            '())))
        ((does)
         (unless current-form
           (declaration-fail "orphan" (row-source row) "does must follow a form" span))
         (when (hash-ref current-form 'meaning-seen? #f)
           (declaration-fail "duplicate" (row-source row) "The form has two meanings" span))
         (set! current-form (hash-set current-form 'meaning-seen? #t))
         (set! current-form
           (hash-set
            current-form
            'meaning
            (parse-source-term
             (capture-value row "value")
             (line-right-span (hash-ref row 'record))))))
        ((restricts)
         (unless current-form
           (declaration-fail "orphan" (row-source row) "restricts! must belong to a form" span))
         (when (hash-ref current-form 'restricting? #f)
           (declaration-fail "duplicate" (row-source row) "The restriction is declared twice" span))
         (hash-set! form-restriction-spans (hash-ref current-form 'span) span)
         (set! current-form (hash-set current-form 'restricting? #t)))
        ((example default)
         (unless current-form
           (declaration-fail
            "orphan"
            (row-source row)
            "An example or default must follow a form"
            span))
         (define key (if (eq? kind 'example) 'examples 'defaults))
         (set! current-form
           (hash-set
            current-form
            key
            (append (hash-ref current-form key) (list (cons (capture-value row "value") span))))))
        ((count count-keyed)
         (define key (capture-value row "name"))
         (define width (if (eq? kind 'count-keyed) (string->number (capture-value row "width")) 0))
         (define selected (if (zero? width) counts keyed-counts))
         (when (hash-has-key? selected key)
           (declaration-fail
            "duplicate"
            (row-source row)
            "The constructor has two counts of one kind"
            span))
         (if (zero? width)
           (set! counts (hash-set counts key (capture-value row "multiplicity")))
           (set! keyed-counts
             (hash-set
              keyed-counts
              key
              (hash 'multiplicity (capture-value row "multiplicity") 'width width)))))
        ((rule) (set! rules (append rules (list row))))
        ((sees) (set! sees (append sees (list (capture-value row "name")))))
        ((note) (set! notes (append notes (list (capture-value row "text")))))
        ((cell)
         (define key (capture-value row "name"))
         (when (or (reserved-name? key) (hash-has-key? properties (cons 'cell key)))
           (declaration-fail "duplicate" key "The cell is declared twice or reserved" span))
         (set! properties (hash-set properties (cons 'cell key) #t)))
        ((writes reads ensure join effect effect-state stage guard scope fuel) (void))
        (else
         (declaration-fail
          "reference"
          (row-source row)
          "The meta operation has no implementation"
          span))))))
  (recover finish-form!)
  (define class-sources
    (for/fold
     ((merged (current-meta-classes)))
     (((key value) (in-hash sources)))
     (hash-set merged key value)))
  (define classes
    (compile-classes
     class-sources
     #:caps
     caps
     #:order
     (append
      (sort (hash-keys (current-meta-classes)) string<?)
      (for/list
       ((row (in-list rows)) #:when (eq? (hash-ref row 'kind) 'class))
       (capture-value row "name")))
     #:on-error
     (lambda (error)
       (define key (hash-ref (class-error-data error) 'definition-name #f))
       (when key (hash-set! invalid-classes key #t))
       (define source-row (and key (hash-ref source-rows key #f)))
       (define span (if source-row (row-span source-row) opening))
       (recover (lambda () (class-run (lambda () (raise error)) span))))))
  (for
   ((row (in-list rows)) #:when (eq? (hash-ref row 'kind) 'class))
   (define key (capture-value row "name"))
   (define class (hash-ref classes key #f))
   (when (and class (class-empty? class))
     (record-diagnostics!
      (list
       (make-diagnostic
        "N_CLASS_EMPTY"
        "The class language is empty after allowed-point intersection"
        #:severity
        'note
        #:span
        (row-span row)
        #:data
        (hash 'class key))))))
  (define template-captures
    (let ((file (hash-ref properties 'file #f)))
      (if file
        (let* ((row (for/first ((row (in-list rows)) #:when (eq? (hash-ref row 'kind) 'file)) row))
               (parts
                (recover
                 (lambda () (parse-template file #:span (line-right-span (hash-ref row 'record))))
                 '())))
          (current-template-names (map slot-name (filter slot? parts)))
          (for/fold
           ((bindings (hash)))
           ((part (in-list parts)) #:when (slot? part))
           (define key (slot-name part))
           (unless (or (hash-has-key? classes key) (hash-has-key? class-sources key))
             (recover
              (lambda ()
                (declaration-fail
                 "capture"
                 (string-append "<" key ">")
                 "A file-template placeholder has no class"
                 (row-span row)))))
           (hash-set bindings key (hash-ref classes key #f))))
        (hash))))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (for
    ((class-name (in-list (constructor-signature item))))
    (recover
     (lambda ()
       (unless (hash-has-key? classes class-name)
         (unless (hash-has-key? class-sources class-name)
           (fail
            "E_DECLARATION"
            "A constructor uses an undeclared class"
            (constructor-span item)
            (hash 'kind "reference" 'actual class-name))))))))
  (for
   ((row (in-list rules)))
   (recover
    (lambda ()
      (define source (capture-value row "value"))
      (define attempted (regexp-match #px"^[ \t]*([^ \t()<>\"]+)" source))
      (when (and attempted (hash-has-key? constructors (cadr attempted)))
        (hash-set! rule-attempts (cadr attempted) #t))
      (define arrows (top-level-delimiters source "->"))
      (unless (= (length arrows) 1)
        (declaration-fail "syntax" source "A rule has one arrow" (row-span row)))
      (define value-span (line-right-span (hash-ref row 'record)))
      (define arrow-position (car arrows))
      (define left-text (substring source 0 arrow-position))
      (define left (parse-source-term left-text (span-slice value-span source 0 arrow-position)))
      (define key (term-constructor left))
      (unless (hash-has-key? constructors key)
        (declaration-fail "reference" key "A rule names an undeclared constructor" (row-span row)))
      (define item (hash-ref constructors key))
      (when (constructor-primitive? item)
        (declaration-fail "conflict" key "A primitive cannot also be rewritten" (row-span row)))
      (when (constructor-rule item)
        (fail
         "E_RULE_DUPLICATE"
         "A primitive cannot be rewritten and a constructor has at most one rule"
         (term-span left)
         (hash 'constructor key 'rule source)))
      (unless (and
               (= (length (term-arguments left)) (length (constructor-signature item)))
               (andmap capture? (term-arguments left)))
        (fail
         "E_DECLARATION"
         "A rule left side contains only its constructor and variables"
         (row-span row)
         (hash
          'kind
          (if (= (length (term-arguments left)) (length (constructor-signature item)))
            "syntax"
            "arity")
          'actual
          (term-source left))))
      (define names (map capture-name (term-arguments left)))
      (when (check-duplicates names)
        (declaration-fail
         "duplicate"
         (check-duplicates names)
         "A rule variable is repeated"
         (row-span row)))
      (for
       ((name (in-list names)) #:when (hash-has-key? template-captures name))
       (declaration-fail
        "duplicate"
        (string-append "<" name ">")
        "A rule variable repeats a file-template placeholder"
        (row-span row)))
      (define right-start (+ arrow-position 2))
      (define right-source (substring source right-start))
      (define cuts
        (append (top-level-delimiters right-source ";") (list (string-length right-source))))
      (define right
        (let loop ((remaining cuts) (offset 0))
          (if (null? remaining)
            '()
            (cons
             (parse-source-term
              (substring right-source offset (car remaining))
              (span-slice value-span source (+ right-start offset) (+ right-start (car remaining))))
             (loop (cdr remaining) (add1 (car remaining)))))))
      (hash-set! rule-lefts key left)
      (set! constructors
        (hash-set constructors key (struct-copy constructor item (rule (cons names right))))))))
  (define effect-body? (ormap constructor-primitive? (hash-values constructors)))
  (for
   ((key (in-list (sort (hash-keys facts) string<?))))
   (recover
    (lambda ()
      (define span (hash-ref facts key))
      (unless (hash-has-key? constructors key)
        (declaration-fail "reference" key "The fact constructor is undeclared" span))
      (when (or
             (constructor-primitive? (hash-ref constructors key))
             (constructor-rule (hash-ref constructors key)))
        (declaration-fail
         "conflict"
         key
         "A fact cannot also be a primitive or rewritten effect"
         span)))))
  (set! constructors
    (for/hash
     (((key item) (in-hash constructors)))
     (values
      key
      (struct-copy constructor item (fact? (or (not effect-body?) (hash-has-key? facts key)))))))
  (define model
    (compile-model
     rows
     (constructor-descriptors constructors)
     classes
     opening
     #:template-names
     (current-template-names)
     #:on-error
     (lambda (item) (record-diagnostics! (list item)))))
  (current-partition-model model)
  (record-diagnostics! (model-check-notes model))
  (check-rule-cycles! constructors rule-lefts)
  (define (constructor-use-span item)
    (define uses
      (append
       (for/list
        ((pending (in-list pending-forms))
         #:when
         (equal? (term-constructor (hash-ref pending 'meaning)) (constructor-name item)))
        (term-span (hash-ref pending 'meaning)))
       (for*/list
        ((owner (in-list (ordered-constructors constructors)))
         #:when
         (constructor-rule owner)
         (right (in-list (append-map effect-term-calls (cdr (constructor-rule owner)))))
         #:when
         (equal? (term-constructor right) (constructor-name item)))
        (term-span right))))
    (if (null? uses) (constructor-span item) (car (sort uses < #:key src-span-start-byte))))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (recover
    (lambda ()
      (unless (or
               (constructor-primitive? item)
               (constructor-fact? item)
               (constructor-rule item)
               (hash-ref rule-attempts (constructor-name item) #f))
        (fail
         "E_CONSTRUCTOR_UNBOUND"
         "A constructor must be primitive or have one rule"
         (constructor-use-span item)
         (hash 'constructor (constructor-name item))))))
   (when (constructor-rule item)
     (define captures
       (for/fold
        ((bindings template-captures))
        ((name (in-list (car (constructor-rule item))))
         (class-name (in-list (constructor-signature item))))
        (hash-set
         bindings
         name
         (hash-ref
          classes
          (if (relative-path-role? classes class-name) "canonical-path" class-name)
          #f))))
     (for
      ((right (in-list (cdr (constructor-rule item)))))
      (recover
       (lambda ()
         (validate-effect!
          right
          constructors
          classes
          captures
          (constructor-ordinal item)
          model
          (model-fuel model (constructor-name item))
          (lambda (entry)
            (record-signature!
             (hash 'kind "rewrite" 'ordinal (constructor-ordinal item))
             entry))))))))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (define captures
     (for/fold
      ((bindings template-captures))
      ((class-name (in-list (constructor-signature item))) (index (in-naturals 1)))
      (hash-set
       bindings
       (number->string index)
       (hash-ref
        classes
        (if (relative-path-role? classes class-name) "canonical-path" class-name)
        #f))))
   (for
    ((clause (in-list (hash-ref (compiled-model-ensures model) (constructor-name item) '()))))
    (recover
     (lambda ()
       (validate-predicate!
        (model-ensure-expression clause)
        constructors
        classes
        captures
        #f
        model
        void))))
   (for
    ((clause (in-list (hash-ref (compiled-model-reads model) (constructor-name item) '())))
     #:when
     (term? (model-read-fact clause)))
    (recover
     (lambda ()
       (validate-predicate! (model-read-fact clause) constructors classes captures #f model void))))
   (define guard (hash-ref (compiled-model-guards model) (constructor-name item) #f))
   (when (term? guard)
     (recover (lambda () (validate-predicate! guard constructors classes captures #f model void)))))
  (for
   ((pending (in-list pending-forms)) (index (in-naturals)))
   (recover
    (lambda ()
      (let/ec
       skip-form
       (define span (hash-ref pending 'span))
       (define-values
        (kind left right)
        (parts (hash-ref pending 'source) (hash-ref pending 'production-span)))
       (define slots (append (filter slot? left) (if right (filter slot? right) '())))
       (when (check-duplicates (map slot-name slots))
         (declaration-fail
          "duplicate"
          (string-append "<" (check-duplicates (map slot-name slots)) ">")
          "A placeholder occurs on both sides of the line"
          span))
       (for
        ((placeholder (in-list slots)))
        (when (hash-has-key? template-captures (slot-name placeholder))
          (declaration-fail
           "duplicate"
           (string-append "<" (slot-name placeholder) ">")
           "A form placeholder repeats a file-template placeholder"
           span)))
       (define missing-placeholder? #f)
       (for
        ((placeholder (in-list slots)))
        (unless (hash-has-key? classes (slot-name placeholder))
          (set! missing-placeholder? #t)
          (unless (hash-has-key? class-sources (slot-name placeholder))
            (recover
             (lambda ()
               (fail
                "E_DECLARATION"
                "A form placeholder has no class"
                span
                (hash 'kind "capture" 'actual (string-append "<" (slot-name placeholder) ">"))))))))
       (for
        ((side (in-list (filter values (list left right)))))
        (for
         ((a (in-list side)) (b (in-list (if (pair? side) (append (cdr side) (list #f)) '()))))
         (when (and
                (slot? a)
                (hash-has-key? classes (slot-name a))
                (string? b)
                (positive? (string-length b)))
           (define class (hash-ref classes (slot-name a)))
           (define witness (class-delimiter-witness class b))
           (when witness
             (fail
              "E_CLASS_OVERLAP"
              "A placeholder can contain its following delimiter"
              span
              (hash
               'form
               (hash-ref pending 'source)
               'class
               (slot-name a)
               'token
               b
               'witness
               witness)))
           (set! delimiter-checks
             (cons
              (hash
               'form
               index
               'capture
               (slot-name a)
               'delimiter
               b
               'bounds
               (class-check-count-bounds class #:literal b))
              delimiter-checks)))))
       (define meaning (hash-ref pending 'meaning))
       (define meaning-constructor (hash-ref constructors (term-constructor meaning) #f))
       (define missing-signature-class?
         (and meaning-constructor
              (ormap
               (lambda (class-name) (not (hash-has-key? classes class-name)))
               (constructor-signature meaning-constructor))))
       (unless (and
                (hash-has-key? constructors (term-constructor meaning))
                (not (hash-has-key? library-arities (term-constructor meaning))))
         (if (hash-has-key? library-arities (term-constructor meaning))
           (fail
            "E_TERM_CLASS"
            "does must build a declared constructor"
            (term-span meaning)
            (hash
             'kind
             "sort"
             'constructor
             (term-constructor meaning)
             'class
             "constructor"
             'expected
             "declared-constructor"
             'actual
             (cond
              ((member (term-constructor meaning) '("prefix" "suffix" "equal")) "predicate")
              ((member (term-constructor meaning) '("ancestors" "split" "inherited")) "list")
              (else "string"))))
           (declaration-fail
            "reference"
            (term-constructor meaning)
            "does must build a declared constructor"
            (term-span meaning))))
       (validate-term!
        meaning
        constructors
        classes
        (for/fold
         ((bindings template-captures))
         ((part (in-list slots)))
         (hash-set bindings (slot-name part) (hash-ref classes (slot-name part) #f)))
        #f
        #:record-check
        (lambda (entry) (record-signature! (hash 'kind "form" 'ordinal index) entry)))
       (when (or missing-placeholder? missing-signature-class?) (skip-form #f))
       (set! forms
         (append
          forms
          (list
           (form
            (hash-ref pending 'source)
            kind
            left
            right
            meaning
            (hash-ref pending 'examples)
            (hash-ref pending 'defaults)
            span
            index
            (hash-ref pending 'restricting? #f)))))))))
  (for
   ((candidate (in-list forms)) #:when (form-restricting? candidate))
   (recover
    (lambda ()
      (define multiplicity (hash-ref counts (term-constructor (form-meaning candidate)) "*"))
      (unless (or (member multiplicity '("1" "1..*")) (pair? (form-defaults candidate)))
        (fail
         "E_RESTRICTS"
         "A restricting form needs a required multiplicity or a default"
         (hash-ref form-restriction-spans (form-span candidate) (form-span candidate))
         (hash
          'form
          (form-source candidate)
          'constructor
          (term-constructor (form-meaning candidate))))))))
  (for
   ((key
     (in-list
      (sort (remove-duplicates (append (hash-keys counts) (hash-keys keyed-counts))) string<?))))
   (recover
    (lambda ()
      (unless (hash-has-key? constructors key)
        (fail
         "E_DECLARATION"
         "A count names an undeclared constructor"
         (row-span
          (for/first
           ((row (in-list rows))
            #:when
            (and (memq (hash-ref row 'kind) '(count count-keyed))
                 (equal? (capture-value row "name") key)))
           row))
         (hash 'kind "reference" 'actual key))))))
  (define body
    (language
     name
     (hash-ref properties 'version "1")
     (hash-ref properties 'comment "#")
     (hash-ref properties 'order "free")
     classes
     sources
     constructors
     forms
     counts
     (hash-ref properties 'file #f)
     (hash-ref properties 'required #f)
     defaults
     sees
     notes
     caps
     rows
     opening))
  (for
   ((row (in-list rows)) #:when (eq? (hash-ref row 'kind) 'note))
   (recover
    (lambda ()
      (unless (hash-has-key? constructors (capture-value row "constructor"))
        (declaration-fail
         "reference"
         (capture-value row "constructor")
         "The note names no declared constructor"
         (row-span row))))))
  (hash-set! language-keyed-count-cache body keyed-counts)
  (hash-set! language-template-cache body (current-template-names))
  (for
   (((key constraint) (in-hash keyed-counts)) #:when (hash-has-key? constructors key))
   (when (>
          (hash-ref constraint 'width)
          (length (constructor-signature (hash-ref constructors key))))
     (define row
       (for/first
        ((row (in-list rows))
         #:when
         (and (eq? (hash-ref row 'kind) 'count-keyed) (equal? (capture-value row "name") key)))
        row))
     (recover
      (lambda ()
        (declaration-fail
         "syntax"
         (row-source row)
         "The count key is wider than the constructor signature"
         (row-span row))))))
  (define fields
    (for/hasheq
     ((candidate (in-list forms)))
     (values
      candidate
      (cons (field-automaton classes candidate #f) (field-automaton classes candidate #t)))))
  (define (form-language candidate)
    (define pair (hash-ref fields candidate))
    (class-sequence
     (append
      (list (car pair))
      (case (form-kind candidate)
        ((colon) (list " : "))
        ((arrow) (list " -> "))
        ((bang) (list "!")))
      (if (eq? (form-kind candidate) 'bang) '() (list (cdr pair))))
     #:name
     "form"))
  (define machines
    (for/hasheq ((candidate (in-list forms))) (values candidate (form-language candidate))))
  (define default-values '())
  (for*
   ((earlier (in-list forms))
    (later (in-list forms))
    #:when
    (and (< (form-ordinal earlier) (form-ordinal later))
         (eq? (form-kind earlier) (form-kind later))))
   (recover
    (lambda ()
      (define left-machine (hash-ref machines earlier))
      (define right-machine (hash-ref machines later))
      (define witness (class-overlap-witness left-machine right-machine))
      (when witness
        (fail
         "E_FORM_SHADOW"
         "Two forms accept the same line"
         (form-span later)
         (hash 'form (form-source later) 'actual (form-source earlier) 'witness witness)))
      (set! overlap-checks
        (cons
         (hash
          'earlier
          (form-ordinal earlier)
          'later
          (form-ordinal later)
          'bounds
          (class-check-count-bounds left-machine right-machine))
         overlap-checks)))))
  (for*
   ((candidate (in-list forms))
    (pair (in-list (append (form-examples candidate) (form-defaults candidate)))))
   (recover
    (lambda ()
      (define records
        (filter
         (lambda (record) (not (memq (line-record-kind record) '(blank comment))))
         (read-lines
          (car pair)
          #:file
          (src-span-file (cdr pair))
          #:comment-prefix
          (language-comment body))))
      (unless (= (length records) 1)
        (fail
         "E_EXAMPLE"
         "An example must dispatch to its intended form"
         (cdr pair)
         (hash 'form (form-source candidate) 'actual (car pair))))
      (define hit (find-form body (car records)))
      (unless (and hit (= (form-ordinal (car hit)) (form-ordinal candidate)))
        (fail
         "E_EXAMPLE"
         "An example does not reach its intended form"
         (cdr pair)
         (hash 'form (form-source candidate) 'actual (car pair))))
      (when (member pair (form-examples candidate))
        (define witnesses
          (for/hash
           (((name class) (in-hash template-captures)))
           (values name (and class (hash-ref (class-witnesses class) 'shortest #f)))))
        (parameterize
         ((current-program-span (cdr pair)))
         (validate-example-meaning
          body
          (form-meaning (car hit))
          (merge-captures witnesses (cdr hit)))))
      (when (member pair (form-defaults candidate))
        (define row
          (for/first
           ((row (in-list rows))
            #:when
            (and (eq? (hash-ref row 'kind) 'default) (equal? (row-span row) (cdr pair))))
           row))
        (define arguments
          (for/list
           ((argument (in-list (term-arguments (form-meaning (car hit))))))
           (cond
            ((literal? argument) (literal-value argument))
            ((and (capture? argument) (hash-has-key? (cdr hit) (capture-name argument)))
             (hash-ref (cdr hit) (capture-name argument)))
            (else
             (declaration-fail
              "syntax"
              (if row (row-source row) (string-append "default : " (car pair)))
              "Default arguments must be literal values"
              (cdr pair))))))
        (set! default-values
          (append
           default-values
           (list
            (list
             (cons (term-constructor (form-meaning (car hit))) arguments)
             (cdr pair)
             row))))))))
  (define reported-defaults (make-hash))
  (for*
   ((name
     (in-list
      (sort (remove-duplicates (append (hash-keys counts) (hash-keys keyed-counts))) string<?)))
    (constraint
     (in-list
      (append
       (if (hash-has-key? counts name)
         (list (hash 'multiplicity (hash-ref counts name) 'width 0))
         '())
       (if (hash-has-key? keyed-counts name) (list (hash-ref keyed-counts name)) '()))))
    #:when
    (member (hash-ref constraint 'multiplicity) '("1" "0..1")))
   (define quantities (make-hash))
   (for
    ((entry (in-list default-values))
     #:when
     (and (equal? (caar entry) name) (<= (hash-ref constraint 'width) (length (cdar entry)))))
    (define key (take (cdar entry) (hash-ref constraint 'width)))
    (define quantity (add1 (hash-ref quantities key 0)))
    (hash-set! quantities key quantity)
    (when (and (= quantity 2) (not (hash-has-key? reported-defaults (cadr entry))))
      (hash-set! reported-defaults (cadr entry) #t)
      (recover
       (lambda ()
         (declaration-fail
          "conflict"
          (if (caddr entry) (row-source (caddr entry)) "default")
          "The constructor's defaults exceed a count's multiplicity"
          (cadr entry)))))))
  (define checks
    (hash
     'recorded
     #t
     'scope
     check-bounds-scope
     'classes
     (for/list
      ((key (in-list (sort (hash-keys classes) string<?))))
      (hash 'name key 'bounds (class-count-bounds (hash-ref classes key))))
     'forms
     (for/list
      ((candidate (in-list forms)))
      (define pair (hash-ref fields candidate))
      (hash
       'ordinal
       (form-ordinal candidate)
       'left
       (class-count-bounds (car pair))
       'right
       (if (cdr pair) (class-count-bounds (cdr pair)) 'null)
       'whole
       (class-count-bounds (hash-ref machines candidate))))
     'signature-inclusions
     (reverse signature-checks)
     'delimiters
     (reverse delimiter-checks)
     'overlaps
     (reverse overlap-checks)))
  (hash-set! declaration-automata-cache body (declaration-automata fields machines checks))
  (hash-set! language-model-cache body model)
  body)

(define (validate-term!
         expression
         constructors
         classes
         captures
         upper
         #:record-check
         (record-check void))
  (define name (term-constructor expression))
  (define args (term-arguments expression))
  (define item (hash-ref constructors name #f))
  (unless (or item (hash-has-key? library-arities name) (equal? name "context"))
    (fail
     "E_DECLARATION"
     "The term constructor is undeclared"
     (term-span expression)
     (hash 'kind "reference" 'actual name)))
  (when (and upper item (not (constructor-primitive? item)) (> (constructor-ordinal item) upper))
    (fail
     "E_DECLARATION"
     "A rule may refer only to earlier constructors, primitives and library functions"
     (term-span expression)
     (hash 'kind "stratification" 'actual name)))
  (define arities
    (cond
     (item (list (length (constructor-signature item))))
     ((equal? name "context") '(1))
     (else (hash-ref library-arities name))))
  (unless (member (length args) arities)
    (fail
     "E_TERM_CLASS"
     "The term has the wrong number of arguments"
     (term-span expression)
     (hash
      'kind
      "arity"
      'constructor
      name
      'class
      (if item
        (string-join (constructor-signature item) " ")
        (string-join (make-list (car arities) "string") " "))
      'actual
      (length args))))
  (define model (current-partition-model))
  (define write (and model (hash-ref (compiled-model-writes model) name #f)))
  (when (and
         write
         (model-cell-stacking? (hash-ref (compiled-model-cells model) (model-write-cell write))))
    (define partitions (length (current-template-names)))
    (unless (if
             (eq? (model-write-operation write) 'put)
             (= (model-write-width write) (add1 partitions))
             (<= (model-write-width write) (add1 partitions)))
      (declaration-fail
       "partition"
       (term-source expression)
       "The stacking key width must follow the holding file template"
       (term-span expression)))
    (for
     ((argument
       (in-list (take args (min (model-write-width write) (length (current-template-names))))))
      (expected (in-list (current-template-names))))
     (unless (and (capture? argument) (equal? (capture-name argument) expected))
       (declaration-fail
        "partition"
        (term-source expression)
        "A partition argument must be its holding file-template placeholder"
        (term-span expression)))))
  (when (and (member name '("context" "inherited")) (not (literal? (car args))))
    (declaration-fail
     "syntax"
     (term-source expression)
     "The lookup names one literal constructor"
     (term-span expression)))
  (for
   ((argument (in-list args)) (index (in-naturals)))
   (define receiving-class (if item (list-ref (constructor-signature item) index) "string"))
   (cond
    ((capture? argument)
     (unless (if
              (hash? captures)
              (hash-has-key? captures (capture-name argument))
              (member (capture-name argument) captures))
       (fail
        "E_DECLARATION"
        "The meaning refers to an unbound capture"
        (capture-span argument)
        (hash 'kind "capture" 'actual (string-append "<" (capture-name argument) ">")))))
    ((term? argument)
     (unless (or
              (hash-has-key? library-arities (term-constructor argument))
              (equal? (term-constructor argument) "context"))
       (fail
        "E_TERM_NESTED"
        "A string-class position permits a library call, not a nested user constructor"
        (term-span argument)
        (hash 'constructor name 'actual (term-source argument))))
     (when (member (term-constructor argument) '("ancestors" "split" "inherited"))
       (fail
        "E_TERM_CLASS"
        "A string-class position cannot receive a list-valued library call"
        (term-span argument)
        (hash
         'kind
         "sort"
         'constructor
         name
         'class
         receiving-class
         'expected
         "string"
         'actual
         "list")))
     (when (member (term-constructor argument) '("prefix" "suffix" "equal"))
       (fail
        "E_TERM_CLASS"
        "A predicate is permitted only in if or ensure, not a string-class position"
        (term-span argument)
        (hash
         'constructor
         name
         'kind
         "sort"
         'class
         receiving-class
         'expected
         "string"
         'actual
         "predicate"
         'allowed-contexts
         '("if" "ensure"))))
     (validate-term! argument constructors classes captures upper #:record-check record-check))
    ((literal? argument) (void))
    (else
     (declaration-fail
      "syntax"
      (term-data expression)
      "The term contains an invalid argument"
      (term-span expression)))))
  (when item
    (for
     ((argument (in-list args))
      (class-name (in-list (constructor-signature item)))
      (index (in-naturals))
      #:when
      (hash-has-key? classes class-name))
     (when (and
            (literal? argument)
            (not (class-accepts? (hash-ref classes class-name) (literal-value argument))))
       (fail
        "E_TERM_CLASS"
        "A literal term argument fails its declared class"
        (term-span expression)
        (hash
         'kind
         "literal"
         'constructor
         name
         'class
         class-name
         'actual
         (literal-value argument))))
     (when (and
            (capture? argument)
            (hash? captures)
            (compiled-class? (hash-ref captures (capture-name argument) #f)))
       (define source-class (hash-ref captures (capture-name argument)))
       (define target-class (hash-ref classes class-name))
       (define witness (class-difference-witness source-class target-class))
       (when witness
         (fail
          "E_TERM_CLASS"
          "The capture class is not contained in the receiving argument class"
          (term-span expression)
          (hash
           'kind
           "capture"
           'constructor
           name
           'class
           class-name
           'actual
           (compiled-class-name source-class)
           'capture
           (capture-name argument)
           'witness
           witness)))
       (record-check
        (hash
         'constructor
         name
         'argument
         index
         'capture
         (capture-name argument)
         'class
         class-name
         'bounds
         (class-check-count-bounds source-class target-class)))))))

(struct capture-failure (name token span limit) #:transparent)

(define (scan-templates candidates record classes templates)
  (define (field-failure parts captures offsets text span)
    (for/first
     ((part (in-list parts))
      #:when
      (and (slot? part)
           (not
            (class-accepts?
             (hash-ref classes (slot-name part))
             (hash-ref captures (slot-name part))))))
     (define offset (hash-ref offsets (slot-name part)))
     (capture-failure
      (slot-name part)
      (hash-ref captures (slot-name part))
      (span-slice span text (car offset) (cdr offset))
      (hash-ref (compiled-class-caps (hash-ref classes (slot-name part))) 'token-length))))
  (let loop ((remaining candidates) (first-failure #f))
    (cond
     ((null? remaining) (values #f first-failure))
     (else
      (define candidate (car remaining))
      (define-values (kind left right) (templates candidate))
      (cond
       ((not (eq? kind (line-record-kind record))) (loop (cdr remaining) first-failure))
       (else
        (define-values
         (left-captures left-offsets)
         (match-template/located left (line-record-left record)))
        (define-values
         (right-captures right-offsets)
         (if right
           (match-template/located right (line-record-right record))
           (values (hash) (hash))))
        (cond
         ((not (and left-captures right-captures)) (loop (cdr remaining) first-failure))
         (else
          (define failure
            (or (field-failure
                 left
                 left-captures
                 left-offsets
                 (line-record-left record)
                 (line-left-span record))
                (and right
                     (field-failure
                      right
                      right-captures
                      right-offsets
                      (line-record-right record)
                      (line-right-span record)))))
          (if failure
            (loop (cdr remaining) (or first-failure failure))
            (values
             (cons
              candidate
              (for/fold
               ((captures left-captures))
               (((key value) (in-hash right-captures)))
               (hash-set captures key value)))
             first-failure))))))))))

(define (scan-forms body record)
  (scan-templates
   (language-forms body)
   record
   (language-classes body)
   (lambda (candidate)
     (values (form-kind candidate) (form-left candidate) (form-right candidate)))))

(define (find-form body record) (define-values (hit failure) (scan-forms body record)) hit)

(define (refuse-form! record failure)
  (cond
   ((eq? (line-record-kind record) 'unclassified)
    (fail
     "E_LINE_SHAPE"
     "The line has none of the three shapes"
     (line-record-span record)
     (hash 'actual (line-record-raw record))))
   (failure
    (if (> (string-length (capture-failure-token failure)) (capture-failure-limit failure))
      (fail
       "E_CAP"
       "The captured token exceeds the length cap"
       (capture-failure-span failure)
       (hash
        'cap
        "length"
        'limit
        (capture-failure-limit failure)
        'actual
        (string-length (capture-failure-token failure))))
      (fail
       "E_CLASS"
       "The captured token fails its class"
       (capture-failure-span failure)
       (hash 'class (capture-failure-name failure) 'token (capture-failure-token failure)))))
   (else
    (fail
     "E_FORM_NONE"
     "The line matches no structural form"
     (line-record-span record)
     (hash 'actual (line-record-raw record))))))

(define (question-line? record)
  (or (equal? (line-record-left record) "?")
      (and (eq? (line-record-kind record) 'unclassified)
           (regexp-match?
            #px"^\\?(?:$|[ \t:!])"
            (string-trim (line-record-raw record) #px"[ \t]" #:repeat? #t)))))

(define (valid-question? record)
  (and (eq? (line-record-kind record) 'colon)
       (equal? (line-record-left record) "?")
       (verb-word? (line-record-right record))))

(define (line-accepts? text body)
  (and (not (regexp-match? #rx"[\r\n]" text))
       (let ((record (car (read-lines text #:comment-prefix (language-comment body)))))
         (if (memq (line-record-kind record) '(blank comment))
           #t
           (if (question-line? record)
             (valid-question? record)
             (and (find-form body record) #t))))))

(define (missing-context constructor origin)
  (define span (current-program-span))
  (unless span
    (raise-arguments-error 'context "the producing source span is unavailable" "origin" origin))
  (fail
   "E_CONTEXT"
   "No enclosing program supplies the requested constructor"
   span
   (hash 'constructor constructor 'key "")))

(define (inherited-values inherited context constructor origin)
  (define result
    (cond
     (inherited (inherited constructor origin))
     ((eq? context missing-context) '())
     (else (list (context constructor origin)))))
  (define observed
    (if (lookup-observation? result)
      result
      (lookup-observation result (and (list? result) (make-list (length result) #f)) '())))
  (unless (and
           (list? (lookup-observation-values observed))
           (andmap string? (lookup-observation-values observed))
           (list? (lookup-observation-origins observed))
           (=
            (length (lookup-observation-values observed))
            (length (lookup-observation-origins observed)))
           (list? (lookup-observation-skips observed)))
    (raise-arguments-error
     'inherited
     "the lookup must return strings with aligned origin metadata"
     "result"
     result))
  ((current-lookup-observer) observed)
  (lookup-observation-values observed))

(define (context-value inherited context constructor origin)
  (define values (inherited-values inherited context constructor origin))
  (if (pair? values) (car values) (missing-context constructor origin)))

(define (merge-captures inherited local)
  (for/fold ((bindings inherited)) (((name value) (in-hash local))) (hash-set bindings name value)))

(define (producing-spans body terms spans)
  (define result
    (or spans
        (for/list
         ((value (in-list terms)))
         (hash-ref value-origin-cache value (language-span body)))))
  (unless (and (list? result) (= (length terms) (length result)) (andmap src-span? result))
    (raise-arguments-error 'producing-spans "terms need aligned exact source spans"))
  result)

(define (absent-fact name arguments origin) #f)

(define (query-fact query name arguments origin list-value?)
  (define answer (call-with-values (lambda () (query name arguments origin)) list))
  (cond
   ((= (length answer) 2) (if list-value? (if (car answer) (cadr answer) #f) (and (car answer) #t)))
   ((= (length answer) 1) (if list-value? (car answer) (and (car answer) #t)))
   (else (raise-arguments-error 'query-fact "a fact query returns a value or presence and value"))))

(define (json-expression value span)
  (cond
   ((and (list? value) (pair? value) (string? (car value)))
    (term (car value) (map (lambda (item) (json-expression item span)) (cdr value)) span))
   (else (literal value span))))

(define (evaluate-rule-expression
         body
         expression
         bindings
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:fact
         (fact absent-fact)
         #:sort
         (sort "value")
         #:binding-resolver
         (binding-resolver (lambda (name value) value))
         #:origin
         (origin (hash 'kind "program" 'path "<input>" 'line 1)))
  (define (evaluate value)
    (cond
     ((literal? value) (literal-value value))
     ((capture? value)
      (define bound
        (hash-ref
         (merge-captures captures bindings)
         (capture-name value)
         (lambda ()
           (declaration-fail
            "capture"
            (capture-name value)
            "The expression uses an unbound capture"
            (capture-span value)))))
      (define received (binding-resolver (capture-name value) bound))
      (if (list? received) (evaluate (json-expression received (capture-span value))) received))
     ((term? value)
      (define name (term-constructor value))
      (define arguments (map evaluate (term-arguments value)))
      (cond
       ((equal? name "context") (context-value inherited context (car arguments) origin))
       ((equal? name "inherited") (inherited-values inherited context (car arguments) origin))
       ((hash-has-key? library-arities name) (library-call name arguments (term-span value)))
       ((member name fact-names) (query-fact fact name arguments origin (equal? sort "list")))
       ((member name state-predicate-names) (fact name arguments origin))
       (else
        (declaration-fail "reference" name "The expression names no evaluator" (term-span value)))))
     (else
      (declaration-fail
       "syntax"
       (term-data value)
       "The value is not an expression"
       (language-span body)))))
  (evaluate expression))

(define (positional-bindings arguments)
  (for/hash
   ((value (in-list arguments)) (index (in-naturals 1)))
   (values (number->string index) value)))

(define (program-guard-observation
         body
         terms
         #:facts
         (fact absent-fact)
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:origins
         (origins
          (for/list ((value (in-list terms))) (hash 'kind "program" 'path "<input>" 'line 1)))
         #:spans
         (spans #f)
         #:resolve
         (resolve identity-resolver))
  (unless (= (length terms) (length origins))
    (raise-arguments-error 'program-guard-observation "terms and origins must align"))
  (define model (language-model body))
  (define exact-spans (producing-spans body terms spans))
  (for/or
   ((value (in-list terms)) (origin (in-list origins)) (span (in-list exact-spans)))
   (parameterize
    ((current-program-span span))
    (define name (car value))
    (define guard (hash-ref (compiled-model-guards model) name #f))
    (and guard
         (let* ((arguments (cdr value))
                (item (hash-ref (language-constructors body) name))
                (scope (model-scope model name))
                (fact-name (if (string? guard) guard (term-constructor guard))))
           (define (receive index argument)
             (define evaluated
               (evaluate-rule-expression
                body
                (json-expression argument span)
                (hash)
                #:context
                context
                #:inherited
                inherited
                #:captures
                captures
                #:fact
                fact
                #:origin
                origin))
             (define class-name (list-ref (constructor-signature item) index))
             (define-values
              (received receiving-class)
              (resolve body name index class-name evaluated origin scope name))
             (unless (and
                      (string? received)
                      (class-accepts? (hash-ref (language-classes body) receiving-class) received))
               (fail
                "E_TERM_CLASS"
                "A guard key fails its receiving string class"
                span
                (hash 'kind "result" 'constructor name 'class receiving-class 'actual received)))
             received)
           (define key
             (if (string? guard)
               (let* ((write (hash-ref (compiled-model-writes model) name #f))
                      (width
                       (if (constructor-primitive? item)
                         (if write (model-write-width write) 0)
                         (min 1 (length arguments))))
                      (partitioned?
                       (and write
                            (positive? width)
                            (pair? (language-template-names body))
                            (model-cell-stacking?
                             (hash-ref (compiled-model-cells model) (model-write-cell write))))))
                 (for/list
                  ((index (in-list (if partitioned? (list (sub1 width)) (range width)))))
                  (receive index (list-ref arguments index))))
               (for/list
                ((argument (in-list (term-arguments guard))))
                (evaluate-rule-expression
                 body
                 argument
                 (positional-bindings arguments)
                 #:context
                 context
                 #:inherited
                 inherited
                 #:captures
                 captures
                 #:fact
                 fact
                 #:origin
                 origin
                 #:binding-resolver
                 (lambda (position bound)
                   (define number (string->number position))
                   (if (and (exact-positive-integer? number) (<= number (length arguments)))
                     (receive (sub1 number) bound)
                     bound))))))
           (define result (query-fact fact fact-name key origin #f))
           (unless (boolean? result)
             (fail
              "E_TERM_CLASS"
              "A guard requires predicate sort"
              span
              (hash 'kind "sort" 'constructor name 'class "predicate" 'actual "list")))
           (and (not result)
                (hash
                 'constructor
                 name
                 'fact
                 fact-name
                 'args
                 key
                 'reason
                 "guard"
                 'origin
                 origin)))))))

(define (program-guarded?
         body
         terms
         #:facts
         (fact absent-fact)
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:origins
         (origins
          (for/list ((value (in-list terms))) (hash 'kind "program" 'path "<input>" 'line 1)))
         #:spans
         (spans #f)
         #:resolve
         (resolve identity-resolver))
  (and (program-guard-observation
        body
        terms
        #:facts
        fact
        #:context
        context
        #:inherited
        inherited
        #:captures
        captures
        #:origins
        origins
        #:spans
        spans
        #:resolve
        resolve)
       #t))

(define (language-needs-facts? body)
  (define (external? expression)
    (and (term? expression)
         (or (and (equal? (term-constructor expression) "if")
                  (pair? (term-arguments expression))
                  (term? (car (term-arguments expression)))
                  (member (term-constructor (car (term-arguments expression))) fact-names))
             (ormap external? (term-arguments expression)))))
  (define model (language-model body))
  (or (positive? (hash-count (compiled-model-guards model)))
      (for/or ((reads (in-hash-values (compiled-model-reads model)))) (pair? reads))
      (for/or
       ((item (in-list (ordered-constructors (language-constructors body)))))
       (and (constructor-rule item) (ormap external? (cdr (constructor-rule item))) #t))))

(define (primitive-events
         body
         terms
         origins
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:fact
         (fact absent-fact)
         #:spans
         (spans #f)
         #:resolve
         (resolve identity-resolver))
  (unless (= (length terms) (length origins))
    (raise-arguments-error 'primitive-events "terms and origins must have equal lengths"))
  (define model (language-model body))
  (define events '())
  (define ensures '())
  (define skips '())
  (define normal-forms '())
  (define (expression-value expression bindings origin #:sort (sort "value"))
    (evaluate-rule-expression
     body
     expression
     bindings
     #:context
     context
     #:inherited
     inherited
     #:captures
     captures
     #:fact
     fact
     #:origin
     origin
     #:sort
     sort))
  (define (expand-effect expression bindings carrier fuel origin scope scope-constructor)
    (define name (term-constructor expression))
    (define args (term-arguments expression))
    (cond
     ((equal? name "if")
      (define condition (car args))
      (define fact-name (term-constructor condition))
      (define fact-args
        (map
         (lambda (argument) (expression-value argument bindings origin))
         (term-arguments condition)))
      (define predicate
        (if (member fact-name library-predicate-names)
          (library-call fact-name fact-args (term-span condition))
          (query-fact fact fact-name fact-args origin #f)))
      (unless (boolean? predicate)
        (fail
         "E_TERM_CLASS"
         "if requires predicate sort"
         (term-span (car args))
         (hash 'kind "sort" 'constructor "if" 'class "predicate" 'actual "list")))
      (if predicate
        (expand-effect (cadr args) bindings carrier fuel origin scope scope-constructor)
        (set! skips
          (cons
           (hash 'constructor carrier 'fact fact-name 'args fact-args 'reason "if" 'origin origin)
           skips))))
     ((equal? name "each")
      (define observed #f)
      (define observe (current-lookup-observer))
      (define values
        (parameterize
         ((current-lookup-observer (lambda (result) (set! observed result) (observe result))))
         (expression-value (caddr args) bindings origin #:sort "list")))
      (unless (and (list? values) (andmap string? values))
        (fail
         "E_TERM_CLASS"
         "each requires a list of strings"
         (term-span (caddr args))
         (hash
          'kind
          "sort"
          'constructor
          "each"
          'class
          "list"
          'actual
          (if (boolean? values) "predicate" "string"))))
      (when (> (length values) fuel)
        (raise-typify
         "E_RULE_FUEL"
         "The loop exceeds its carrying constructor's fuel cap"
         #:span
         (term-span expression)
         #:related
         (if (and (equal? (term-constructor (caddr args)) "inherited")
                  observed
                  (hash? (list-ref (lookup-observation-origins observed) fuel)))
           (list (list-ref (lookup-observation-origins observed) fuel))
           '())
         #:data
         (hash 'constructor carrier 'limit fuel 'actual (length values))))
      (for
       ((value (in-list values)))
       (expand-effect
        (cadddr args)
        (hash-set bindings (capture-name (car args)) value)
        carrier
        fuel
        origin
        scope
        scope-constructor)))
     (else
      (expand-value
       (cons name (map (lambda (argument) (expression-value argument bindings origin)) args))
       origin
       scope
       scope-constructor
       (term-span expression)))))
  (define (expand-value value origin inherited-scope inherited-constructor (use-span #f))
    (unless (and (list? value) (pair? value) (string? (car value)))
      (declaration-fail
       "syntax"
       value
       "A normalized value must be a constructor term"
       (language-span body)))
    (define name (car value))
    (define supplied-args (cdr value))
    (define item (hash-ref (language-constructors body) name #f))
    (unless item
      (declaration-fail "reference" name "The constructor is undeclared" (language-span body)))
    (unless (= (length supplied-args) (length (constructor-signature item)))
      (fail
       "E_TERM_CLASS"
       "The constructor has the wrong number of arguments"
       (or use-span (constructor-span item))
       (hash
        'kind
        "arity"
        'constructor
        name
        'class
        (string-join (constructor-signature item) " ")
        'actual
        (length supplied-args))))
    (define explicit-scope? (hash-has-key? (compiled-model-scopes model) name))
    (define scope (if explicit-scope? (model-scope model name) inherited-scope))
    (define scope-constructor (if explicit-scope? name inherited-constructor))
    (define args
      (for/list
       ((argument (in-list supplied-args))
        (class-name (in-list (constructor-signature item)))
        (index (in-naturals)))
       (define evaluated
         (if (list? argument)
           (expression-value
            (json-expression argument (or use-span (constructor-span item)))
            captures
            origin)
           argument))
       (define-values
        (received receiving-class)
        (resolve body name index class-name evaluated origin scope scope-constructor))
       (unless (and
                (string? received)
                (class-accepts? (hash-ref (language-classes body) receiving-class) received))
         (fail
          "E_TERM_CLASS"
          "The expanded argument fails its receiving string class"
          (or use-span (constructor-span item))
          (hash 'kind "result" 'constructor name 'class receiving-class 'actual received)))
       received))
    (define normalized-value (cons name args))
    (define bindings (merge-captures captures (positional-bindings args)))
    (for
     ((ensure (in-list (hash-ref (compiled-model-ensures model) name '()))))
     (set! ensures
       (cons
        (hash
         'ensure
         ensure
         'bindings
         bindings
         'origin
         origin
         'body
         body
         'scope
         scope
         'scope-constructor
         scope-constructor
         'arguments-resolved?
         #t)
        ensures)))
    (cond
     ((constructor-rule item)
      (define rule-bindings
        (for/fold
         ((bindings captures))
         ((name (in-list (car (constructor-rule item)))) (argument (in-list args)))
         (hash-set bindings name argument)))
      (for
       ((right (in-list (cdr (constructor-rule item)))))
       (expand-effect
        right
        rule-bindings
        name
        (model-fuel model name)
        origin
        scope
        scope-constructor)))
     (else
      (set! normal-forms (cons normalized-value normal-forms))
      (when (constructor-primitive? item)
        (set! events
          (cons
           (hash
            'term
            normalized-value
            'origin
            origin
            'body
            body
            'stage
            (model-stage model name)
            'bindings
            bindings
            'scope
            scope
            'scope-constructor
            scope-constructor)
           events))))))
  (define diagnostics '())
  (parameterize
   ((current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics))))
    (current-lookup-observer
     (lambda (observed) (set! skips (append (reverse (lookup-observation-skips observed)) skips)))))
   (for
    ((value (in-list terms))
     (origin (in-list origins))
     (span (in-list (producing-spans body terms spans))))
    (parameterize
     ((current-program-span span))
     (recover
      (lambda ()
        (expand-value
         value
         origin
         "program"
         (car value)
         (hash-ref value-meaning-spans value #f)))))))
  (raise-diagnostics (diagnostics-sort (reverse diagnostics)))
  (hash
   'events
   (reverse events)
   'ensures
   (reverse ensures)
   'skips
   (reverse skips)
   'normal-forms
   (reverse normal-forms)))

(define (term-value
         expression
         captures
         body
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:origin
         (origin (hash 'kind "program" 'path "<input>" 'line 1)))
  (define (evaluate item)
    (if (list? item)
      (cond
       ((equal? (car item) "context") (context-value inherited context (cadr item) origin))
       ((equal? (car item) "inherited") (inherited-values inherited context (cadr item) origin))
       ((hash-has-key? library-arities (car item))
        (library-call (car item) (map evaluate (cdr item)) (term-span expression)))
       (else (cons (car item) (map evaluate (cdr item)))))
      item))
  (define value (evaluate (instantiate-term expression captures)))
  (define (check item)
    (when (pair? item)
      (define signature (hash-ref (language-constructors body) (car item) #f))
      (when signature
        (for
         ((class-name (in-list (constructor-signature signature))) (argument (in-list (cdr item))))
         (when (or
                (not (string? argument))
                (not (class-accepts? (hash-ref (language-classes body) class-name) argument)))
           (fail
            "E_TERM_CLASS"
            "A term argument fails its declared class"
            (term-span expression)
            (hash 'kind "result" 'constructor (car item) 'class class-name 'actual argument)))))))
  (check value)
  (hash-set! value-meaning-spans value (term-span expression))
  value)

(define (identity-resolver body name index class value origin scope scope-constructor)
  (values value class))

(define (check-program
         text
         body
         #:file
         (file "<input>")
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:facts
         (facts #f)
         #:guard
         (guard #f)
         #:resolve
         (resolve identity-resolver)
         #:guard-resolver
         (guard-resolver #f))
  (define result
    (check-program/diagnostics
     text
     body
     #:file
     file
     #:context
     context
     #:inherited
     inherited
     #:captures
     captures
     #:facts
     facts
     #:guard
     guard
     #:resolve
     resolve
     #:guard-resolver
     guard-resolver))
  (raise-diagnostics (hash-ref result 'diagnostics))
  (hash-remove result 'diagnostics))

(define (check-program/diagnostics
         text
         body
         #:file
         (file "<input>")
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:facts
         (facts #f)
         #:guard
         (guard #f)
         #:resolve
         (resolve identity-resolver)
         #:guard-resolver
         (guard-resolver #f))
  (define diagnostics '())
  (define raw-terms '())
  (define raw-origins '())
  (define raw-spans '())
  (define entries '())
  (define terms '())
  (define origins '())
  (define term-spans '())
  (define quantities (hash))
  (define last-index -1)
  (define guarded? #f)
  (define skips '())
  (define records (read-lines text #:file file #:comment-prefix (language-comment body)))
  (define line-spans
    (for/hash
     ((record (in-list records)))
     (values (src-span-start-line (line-record-span record)) (line-record-span record))))
  (define (add-term! hit span kind)
    (define value (instantiate-term (form-meaning (car hit)) (merge-captures captures (cdr hit))))
    (define origin (hash 'kind kind 'path (src-span-file span) 'line (src-span-start-line span)))
    (hash-set! value-origin-cache value span)
    (hash-set! value-meaning-spans value (term-span (form-meaning (car hit))))
    (set! quantities (hash-update quantities (car value) add1 0))
    (set! raw-terms (append raw-terms (list value)))
    (set! raw-origins (append raw-origins (list origin)))
    (set! raw-spans (append raw-spans (list span)))
    (set! entries (append entries (list hit))))
  (parameterize
   ((current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics))))
    (current-lookup-observer
     (lambda (observed) (set! skips (append skips (lookup-observation-skips observed)))))
    (current-source-text-lines
     (for/hash
      ((record (in-list records)))
      (values
       (cons
        (src-span-file (line-record-span record))
        (src-span-start-line (line-record-span record)))
       (line-record-raw record)))))
   (for
    ((record (in-list records)))
    (unless (memq (line-record-kind record) '(blank comment))
      (parameterize
       ((current-program-span (line-record-span record)))
       (recover
        (lambda ()
          (cond
           ((question-line? record)
            (unless (valid-question? record)
              (fail
               "E_QUESTION"
               "A question requires one verb word"
               (line-record-span record)
               (hash 'actual (line-record-raw record)))))
           (else
            (define-values (hit failure) (scan-forms body record))
            (unless hit (refuse-form! record failure))
            (define candidate (car hit))
            (when (and
                   (equal? (language-order body) "strict")
                   (< (form-ordinal candidate) last-index))
              (recover
               (lambda ()
                 (fail
                  "E_ORDER"
                  "The line violates the declared form order"
                  (line-record-span record)
                  (hash
                   'form
                   (form-source candidate)
                   'expected
                   last-index
                   'actual
                   (form-ordinal candidate))))))
            (set! last-index (max last-index (form-ordinal candidate)))
            (add-term! hit (line-record-span record) "program"))))))))
   (define explicit-constructors quantities)
   (for*
    ((candidate (in-list (language-forms body)))
     #:unless
     (hash-has-key? explicit-constructors (term-constructor (form-meaning candidate)))
     (default (in-list (form-defaults candidate))))
    (parameterize
     ((current-program-span (cdr default)))
     (recover
      (lambda ()
        (define record
          (car
           (read-lines
            (car default)
            #:file
            (src-span-file (cdr default))
            #:comment-prefix
            (language-comment body))))
        (define hit (find-form body record))
        (unless hit
          (fail
           "E_FORM_NONE"
           "The default matches no form with valid captures"
           (cdr default)
           (hash 'actual (car default))))
        (add-term! hit (cdr default) "default")))))
   (for
    ((name (in-list (sort (hash-keys (language-counts body)) string<?))))
    (define multiplicity (hash-ref (language-counts body) name))
    (define quantity (hash-ref quantities name 0))
    (unless (cond
             ((equal? multiplicity "1") (= quantity 1))
             ((equal? multiplicity "0..1") (<= quantity 1))
             ((equal? multiplicity "1..*") (>= quantity 1))
             (else (equal? multiplicity "*")))
      (define matching
        (for/list
         ((value (in-list raw-terms))
          (origin (in-list raw-origins))
          (span (in-list raw-spans))
          #:when
          (equal? (car value) name))
         (cons origin span)))
      (define anchor
        (cond
         ((and (> quantity 1) (member multiplicity '("1" "0..1"))) (cdr (cadr matching)))
         ((pair? records) (line-record-span (car records)))
         (else (src-span file 1 1 1 1 0 0))))
      (recover
       (lambda ()
         (raise-typify
          "E_PROGRAM_COUNT"
          "The constructor count violates its multiplicity"
          #:span
          anchor
          #:related
          (map car matching)
          #:data
          (hash 'constructor name 'expected multiplicity 'actual quantity 'key '()))))))
   (define actual-resolve (if guard-resolver (guard-resolver raw-terms raw-origins) resolve))
   (define failed-guard (gensym 'failed-guard))
   (define guard-observation
     (recover
      (lambda ()
        (and facts
             (program-guard-observation
              body
              raw-terms
              #:facts
              facts
              #:context
              context
              #:inherited
              inherited
              #:captures
              captures
              #:origins
              raw-origins
              #:spans
              raw-spans
              #:resolve
              actual-resolve)))
      failed-guard))
   (set! guarded?
     (and (not (eq? guard-observation failed-guard))
          (or guard-observation (and guard (guard raw-terms raw-origins)))
          #t))
   (when (hash? guard-observation) (set! skips (append skips (list guard-observation))))
   (unless (or guarded? (eq? guard-observation failed-guard))
     (for
      ((entry (in-list entries)) (origin (in-list raw-origins)) (span (in-list raw-spans)))
      (parameterize
       ((current-program-span span))
       (recover
        (lambda ()
          (define value
            (term-value
             (form-meaning (car entry))
             (merge-captures captures (cdr entry))
             body
             #:context
             context
             #:inherited
             inherited
             #:origin
             origin))
          (hash-set! value-origin-cache value span)
          (set! terms (append terms (list value)))
          (set! origins (append origins (list origin)))
          (set! term-spans (append term-spans (list span)))))))))
  (parameterize
   ((current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics)))))
   (for
    (((name constraint) (in-hash (language-keyed-counts body))))
    (define width (hash-ref constraint 'width))
    (define multiplicity (hash-ref constraint 'multiplicity))
    (define groups (make-hash))
    (define keys '())
    (for
     ((value (in-list terms))
      (origin (in-list origins))
      (span (in-list term-spans))
      #:when
      (equal? (car value) name))
     (define key (take (cdr value) width))
     (unless (hash-has-key? groups key) (set! keys (append keys (list key))))
     (hash-update! groups key (lambda (entries) (append entries (list (cons origin span)))) '()))
    (for
     ((key (in-list keys)))
     (define matching (hash-ref groups key))
     (define quantity (length matching))
     (unless (cond
              ((equal? multiplicity "1") (= quantity 1))
              ((equal? multiplicity "0..1") (<= quantity 1))
              ((equal? multiplicity "1..*") (>= quantity 1))
              (else (equal? multiplicity "*")))
       (recover
        (lambda ()
          (raise-typify
           "E_PROGRAM_COUNT"
           "The constructor's key group violates its multiplicity"
           #:span
           (if (and (> quantity 1) (member multiplicity '("1" "0..1")))
             (cdr (cadr matching))
             (cdar matching))
           #:related
           (map car matching)
           #:data
           (hash 'constructor name 'expected multiplicity 'actual quantity 'key key))))))))
  (define ordered (diagnostics-sort (reverse diagnostics)))
  (hash
   'valid
   (not (diagnostics-error? ordered))
   'language
   (language-name body)
   'terms
   terms
   'origins
   origins
   'diagnostics
   ordered
   'raw-terms
   raw-terms
   'raw-origins
   raw-origins
   'effective-lines
   (length raw-terms)
   'guarded?
   guarded?
   'skips
   skips
   '_term-spans
   term-spans
   '_raw-term-spans
   raw-spans
   'line-spans
   line-spans))

(define library-domain-classes (delay (compile-classes (hash "allowed" "/.*/"))))

(define (library-pattern-accepts? name text)
  (class-pattern-accepts? (hash-ref (force library-domain-classes) name) text))

(define (library-call name args span)
  (define (strings!)
    (unless (andmap string? args)
      (fail
       "E_LIBRARY_DOMAIN"
       "The library function requires string arguments"
       span
       (hash
        'constructor
        name
        'expected
        "string"
        'actual
        (for/first ((value (in-list args)) #:unless (string? value)) value)))))
  (strings!)
  (define (domain! value index domain valid?)
    (unless valid?
      (fail
       "E_LIBRARY_DOMAIN"
       "The library argument is outside its specified domain"
       span
       (hash 'constructor name 'expected domain 'actual value 'argument index))))
  (case (string->symbol name)
    ((parent split ancestors)
     (domain! (car args) 1 "canonical-path" (library-pattern-accepts? "canonical-path" (car args))))
    ((join)
     (domain! (car args) 1 "canonical-path" (library-pattern-accepts? "canonical-path" (car args)))
     (define relative (cadr args))
     (domain!
      relative
      2
      "relative-components"
      (and (positive? (string-length relative))
           (not (string-prefix? relative "/"))
           (library-pattern-accepts? "canonical-path" (string-append "/" relative)))))
    (else
     (for
      ((value (in-list args)) (index (in-naturals 1)))
      (domain! value index "allowed-string" (library-pattern-accepts? "allowed" value)))))
  (case (string->symbol name)
    ((concat) (apply string-append args))
    ((equal) (equal? (car args) (cadr args)))
    ((prefix) (string-prefix? (cadr args) (car args)))
    ((suffix) (string-suffix? (cadr args) (car args)))
    ((split)
     (if (equal? (car args) "/") '() (string-split (substring (car args) 1) "/" #:trim? #f)))
    ((join) (string-append (if (equal? (car args) "/") "" (car args)) "/" (cadr args)))
    ((parent)
     (define value (car args))
     (when (equal? value "/")
       (fail "E_LIBRARY_PARENT" "The root has no parent" span (hash 'path value)))
     (define positions
       (for/list
        ((character (in-string value)) (index (in-naturals)) #:when (char=? character #\/))
        index))
     (if (zero? (last positions)) "/" (substring value 0 (last positions))))
    ((ancestors)
     (let loop ((value (car args)) (result '()))
       (if (equal? value "/")
         result
         (let ((next (library-call "parent" (list value) span)))
           (loop next (cons next result))))))))

(module+ test-support (provide library-call))

(define (normalize-terms
         body
         terms
         #:context
         (context missing-context)
         #:inherited
         (inherited #f)
         #:captures
         (captures (hash))
         #:fact
         (fact absent-fact)
         #:spans
         (spans #f)
         #:resolve
         (resolve identity-resolver))
  (define producing-spans
    (or spans
        (for/list
         ((value (in-list terms)))
         (hash-ref value-origin-cache value (language-span body)))))
  (define origins
    (for/list
     ((span (in-list producing-spans)))
     (hash 'kind "program" 'path (src-span-file span) 'line (src-span-start-line span))))
  (hash-ref
   (primitive-events
    body
    terms
    origins
    #:context
    context
    #:inherited
    inherited
    #:captures
    captures
    #:fact
    fact
    #:spans
    producing-spans
    #:resolve
    resolve)
   'normal-forms))

(define (declaration-bound body)
  (define model (language-model body))
  (define memo (make-hash))
  (define (expression-cost expression fuel)
    (define args (term-arguments expression))
    (cond
     ((equal? (term-constructor expression) "if") (expression-cost (cadr args) fuel))
     ((equal? (term-constructor expression) "each") (* fuel (expression-cost (cadddr args) fuel)))
     (else (cost (term-constructor expression)))))
  (define (cost name)
    (hash-ref!
     memo
     name
     (lambda ()
       (define item (hash-ref (language-constructors body) name))
       (if (constructor-rule item)
         (for/sum
          ((right (in-list (cdr (constructor-rule item)))))
          (expression-cost right (model-fuel model name)))
         1))))
  (hash
   'c
   (for/fold
    ((maximum 0))
    ((candidate (in-list (language-forms body))))
    (max maximum (cost (term-constructor (form-meaning candidate)))))
   'k
   1))

(define (emit-declarations bodies)
  (string-join
   (append
    '("#lang typify" "")
    (append-map
     (lambda (body)
       (append
        (list (string-append "declare : " (language-name body)))
        (map
         (lambda (row) (string-trim (line-record-raw (hash-ref row 'record))))
         (language-rows body))
        '("end!" "")))
     (sort (hash-values bodies) string<? #:key language-name)))
   "\n"))

(define (declaration->jsexpr body #:origins? (origins? #t))
  (define (expression value)
    (cond
     ((term? value) (cons (term-constructor value) (map expression (term-arguments value))))
     ((capture? value) (hash 'capture (capture-name value)))
     ((literal? value) (literal-value value))
     (else value)))
  (define (located value span)
    (if origins?
      (hash-set value 'origin (hash 'file (src-span-file span) 'range (src-span->jsexpr span)))
      value))
  (located
   (hash
    'name
    (language-name body)
    'version
    (language-version body)
    'comment
    (language-comment body)
    'order
    (language-order body)
    'classes
    (language-class-sources body)
    'constructors
    (for/list
     ((item
       (in-list (sort (hash-values (language-constructors body)) < #:key constructor-ordinal))))
     (located
      (hash
       'name
       (constructor-name item)
       'signature
       (constructor-signature item)
       'primitive
       (constructor-primitive? item)
       'fact
       (constructor-fact? item)
       'rule
       (if (constructor-rule item)
         (hash
          'variables
          (car (constructor-rule item))
          'right
          (map expression (cdr (constructor-rule item))))
         'null))
      (constructor-span item)))
    'forms
    (for/list
     ((candidate (in-list (language-forms body))))
     (located
      (hash
       'source
       (form-source candidate)
       'restricts
       (form-restricting? candidate)
       'meaning
       (expression (form-meaning candidate))
       'examples
       (map car (form-examples candidate))
       'defaults
       (map car (form-defaults candidate)))
      (form-span candidate)))
    'counts
    (language-counts body)
    'keyed-counts
    (language-keyed-counts body)
    'file
    (or (language-file body) 'null)
    'required
    (language-required? body)
    'sees
    (language-sees body)
    'notes
    (language-notes body)
    'caps
    (language-caps body)
    'checks
    (declaration-check-bounds body)
    'declarations
    (for/list
     ((row (in-list (language-rows body))))
     (located
      (hash
       'kind
       (symbol->string (hash-ref row 'kind))
       'line
       (string-trim (line-record-raw (hash-ref row 'record)))
       'captures
       (hash-ref row 'captures))
      (row-span row))))
   (language-span body)))

(define meta-language-cache (make-hash))

(define (meta-language)
  (hash-ref!
   meta-language-cache
   (list (current-meta-classes) (current-meta-forms))
   (lambda () (hash-ref (read-declarations (meta-source) #:file "typify.typify") "Typify"))))
