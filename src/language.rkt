#lang racket/base

(require racket/list
         racket/match
         racket/promise
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "meta.rkt"
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
         form-automata)

(struct constructor (name signature primitive? fact? rule span ordinal) #:transparent)

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

(define form-restriction-spans (make-weak-hasheq))

(define current-program-span (make-parameter #f))

(define current-source-lines (make-parameter (hash)))

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
  (if selected (struct-copy diagnostic item (file (src-span-file selected)) (range selected)) item))

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
          (values "E_DECLARATION" (hash 'kind "class-cycle" 'actual (hash-ref data 'chain))))
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

(define (check-rule-cycles! constructors)
  (define colors (make-hash))
  (define cyclic (make-hash))
  (define (visit item path)
    (define name (constructor-name item))
    (define next-path (append path (list name)))
    (hash-set! colors name 'active)
    (for
     ((right (in-list (cdr (constructor-rule item)))))
     (define next (hash-ref constructors (term-constructor right) #f))
     (when (and next (constructor-rule next))
       (case (hash-ref colors (constructor-name next) #f)
         ((active)
          (for
           ((member (in-list (member (constructor-name next) next-path))))
           (hash-set! cyclic member #t))
          (recover
           (lambda ()
             (fail
              "E_RULE_CYCLE"
              "Rewrite dependencies form a cycle"
              (term-span right)
              (hash 'constructor (constructor-name next))))))
         ((done) (void))
         (else (visit next next-path)))))
    (hash-set! colors name 'done))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (when (and (constructor-rule item) (not (hash-has-key? colors (constructor-name item))))
     (visit item '())))
  cyclic)

(struct declaration-automata (fields whole checks) #:transparent)

(define declaration-automata-cache (make-weak-hasheq))

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
  (define model
    (parameterize
     ((current-reader-meta meta)
      (current-bootstrap? (equal? text (meta-source)))
      (current-source-lines source-lines)
      (current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics)))))
     (read-declarations/internal text file)))
  (values model (diagnostics-sort (reverse diagnostics))))

(define (read-declarations/internal text file)
  (define lines (read-lines text #:file file))
  (recover
   (lambda ()
     (unless (and
              (pair? lines)
              (not (regexp-match? #rx"\r" (line-record-raw (car lines))))
              (equal? (string-trim (line-record-raw (car lines)) #px"[ \t]") "#lang typify"))
       (fail
        "E_DECLARATION"
        "A declaration file must open with #lang typify"
        (if (pair? lines) (line-record-span (car lines)) (src-span file 1 1 1 1 0 0))
        (hash 'kind "header" 'actual (if (pair? lines) (line-record-raw (car lines)) ""))))))
  (define bodies '())
  (define current #f)
  (define opening #f)
  (define rows '())
  (for
   ((record (in-list (if (pair? lines) (cdr lines) '()))))
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
              (define body (recover (lambda () (compile-body current rows opening))))
              (when body (set! bodies (append bodies (list body)))))
            (set! current (capture-value row "name"))
            (set! opening (row-span row))
            (set! rows '()))
           ((end)
            (unless current
              (declaration-fail "body" (row-source row) "No language is open" (row-span row)))
            (define body-name current)
            (set! current #f)
            (define body (recover (lambda () (compile-body body-name rows opening))))
            (when body (set! bodies (append bodies (list body)))))
           (else
            (unless current
              (declaration-fail
               "body"
               (row-source row)
               "Declare a language before its contents"
               (row-span row)))
            (set! rows (append rows (list row))))))))))
  (when current
    (recover (lambda () (declaration-fail "body" "EOF" "The language has no end!" opening)))
    (define body (recover (lambda () (compile-body current rows opening))))
    (when body (set! bodies (append bodies (list body)))))
  (define result
    (for/fold
     ((result (hash)))
     ((body (in-list bodies)))
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
   ((body (in-list bodies)) (name (in-list (language-sees body))))
   (recover
    (lambda ()
      (unless (hash-has-key? result name)
        (fail
         "E_DECLARATION"
         "The referenced language is not declared"
         (language-span body)
         (hash 'kind "reference" 'actual name))))))
  result)

(define (compile-body name rows opening)
  (class-run (lambda () (compile-body/internal name rows opening)) opening))

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
  (define rules '())
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
        ((count)
         (define key (capture-value row "name"))
         (when (hash-has-key? counts key)
           (declaration-fail "duplicate" key "The constructor has two counts" span))
         (set! counts (hash-set counts key (capture-value row "multiplicity"))))
        ((rule) (set! rules (append rules (list row))))
        ((sees) (set! sees (append sees (list (capture-value row "name")))))
        ((note) (set! notes (append notes (list (capture-value row "name")))))
        ((cell)
         (define key (capture-value row "name"))
         (when (or (reserved-name? key) (hash-has-key? properties (cons 'cell key)))
           (declaration-fail "duplicate" key "The cell is declared twice or reserved" span))
         (set! properties (hash-set properties (cons 'cell key) #t)))
        ((writes reads ensure join effect stage guard scope fuel) (void))
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
      (define sides (string-split source "->" #:trim? #f))
      (unless (= (length sides) 2)
        (declaration-fail "syntax" source "A rule has one arrow" (row-span row)))
      (define value-span (line-right-span (hash-ref row 'record)))
      (define arrow-position (caar (regexp-match-positions #rx"->" source)))
      (define left-text (car sides))
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
      (define right-source (cadr sides))
      (define right-start (+ arrow-position 2))
      (define right
        (let loop ((remaining (string-split right-source ";" #:trim? #f)) (offset right-start))
          (if (null? remaining)
            '()
            (cons
             (parse-source-term
              (car remaining)
              (span-slice value-span source offset (+ offset (string-length (car remaining)))))
             (loop (cdr remaining) (+ offset (string-length (car remaining)) 1))))))
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
  (define cyclic-constructors (check-rule-cycles! constructors))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (recover
    (lambda ()
      (unless (or (constructor-primitive? item) (constructor-fact? item) (constructor-rule item))
        (fail
         "E_CONSTRUCTOR_UNBOUND"
         "A constructor must be primitive or have one rule"
         (constructor-span item)
         (hash 'constructor (constructor-name item))))))
   (when (constructor-rule item)
     (for
      ((right (in-list (cdr (constructor-rule item)))))
      (recover
       (lambda ()
         (unless (hash-has-key? constructors (term-constructor right))
           (fail
            "E_DECLARATION"
            "A rule emits terms; library calls are arguments of those terms"
            (term-span right)
            (hash 'kind "reference" 'actual (term-constructor right))))
         (when (andmap
                (lambda (class-name) (hash-has-key? classes class-name))
                (constructor-signature item))
           (validate-term!
            right
            constructors
            classes
            (for/hash
             ((name (in-list (car (constructor-rule item))))
              (class-name (in-list (constructor-signature item))))
             (values name (hash-ref classes class-name)))
            (and (not (hash-ref cyclic-constructors (constructor-name item) #f))
                 (constructor-ordinal item))
            #:record-check
            (lambda (entry)
              (record-signature!
               (hash 'kind "rewrite" 'ordinal (constructor-ordinal item))
               entry)))))))))
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
       (when (for/or
              ((placeholder (in-list slots)))
              (and (not (hash-has-key? classes (slot-name placeholder)))
                   (hash-has-key? class-sources (slot-name placeholder))))
         (skip-form #f))
       (when (check-duplicates (map slot-name slots))
         (declaration-fail
          "duplicate"
          (string-append "<" (check-duplicates (map slot-name slots)) ">")
          "A placeholder occurs on both sides of the line"
          span))
       (define missing-placeholder? #f)
       (for
        ((placeholder (in-list slots)))
        (unless (hash-has-key? classes (slot-name placeholder))
          (set! missing-placeholder? #t)
          (recover
           (lambda ()
             (fail
              "E_DECLARATION"
              "A form placeholder has no class"
              span
              (hash 'kind "capture" 'actual (string-append "<" (slot-name placeholder) ">")))))))
       (when missing-placeholder? (skip-form #f))
       (for
        ((side (in-list (filter values (list left right)))))
        (for
         ((a (in-list side)) (b (in-list (if (pair? side) (append (cdr side) (list #f)) '()))))
         (when (and (slot? a) (string? b) (positive? (string-length b)))
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
       (when (and
              meaning-constructor
              (ormap
               (lambda (class-name) (not (hash-has-key? classes class-name)))
               (constructor-signature meaning-constructor)))
         (skip-form #f))
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
              ((member (term-constructor meaning) '("ancestors" "split")) "list")
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
        (for/hash
         ((part (in-list slots)))
         (values (slot-name part) (hash-ref classes (slot-name part))))
        #f
        #:record-check
        (lambda (entry) (record-signature! (hash 'kind "form" 'ordinal index) entry)))
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
   ((key (in-list (sort (hash-keys counts) string<?))))
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
            (and (eq? (hash-ref row 'kind) 'count) (equal? (capture-value row "name") key)))
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
   ((key (in-list (sort (hash-keys counts) string<?)))
    #:when
    (member (hash-ref counts key) '("1" "0..1")))
   (define declared-defaults
     (append-map
      form-defaults
      (filter (lambda (candidate) (equal? (term-constructor (form-meaning candidate)) key)) forms)))
   (when (> (length declared-defaults) 1)
     (define excessive (cadr declared-defaults))
     (define row
       (for/first
        ((row (in-list rows))
         #:when
         (and (eq? (hash-ref row 'kind) 'default) (equal? (row-span row) (cdr excessive))))
        row))
     (recover
      (lambda ()
        (declaration-fail
         "conflict"
         (if row (row-source row) (string-append "default : " (car excessive)))
         "The constructor's defaults exceed its multiplicity"
         (cdr excessive))))))
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
      (term-value (form-meaning candidate) (cdr hit) body))))
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
  (unless (or item (hash-has-key? library-arities name))
    (fail
     "E_DECLARATION"
     "The term constructor is undeclared"
     (term-span expression)
     (hash 'kind "reference" 'actual name)))
  (when (and upper item (not (constructor-primitive? item)) (>= (constructor-ordinal item) upper))
    (fail
     "E_DECLARATION"
     "A rule may refer only to earlier constructors, primitives and library functions"
     (term-span expression)
     (hash 'kind "stratification" 'actual name)))
  (define arities
    (if item (list (length (constructor-signature item))) (hash-ref library-arities name)))
  (unless (member (length args) arities)
    (declaration-fail
     "arity"
     (term-source expression)
     "The term has the wrong number of arguments"
     (term-span expression)
     (hash 'expected arities 'count (length args))))
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
     (unless (hash-has-key? library-arities (term-constructor argument))
       (fail
        "E_TERM_NESTED"
        "A string-class position permits a library call, not a nested user constructor"
        (term-span argument)
        (hash 'constructor name 'actual (term-source argument))))
     (when (member (term-constructor argument) '("ancestors" "split"))
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
     (when (and (capture? argument) (hash? captures))
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
            (string-trim (line-record-raw record) #px"[ \t]")))))

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

(define (term-value expression captures body)
  (define (evaluate item)
    (if (list? item)
      (if (hash-has-key? library-arities (car item))
        (library-call (car item) (map evaluate (cdr item)) (term-span expression))
        (cons (car item) (map evaluate (cdr item))))
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
  value)

(define (check-program text body #:file (file "<input>"))
  (define result (check-program/diagnostics text body #:file file))
  (raise-diagnostics (hash-ref result 'diagnostics))
  (hash-remove result 'diagnostics))

(define (check-program/diagnostics text body #:file (file "<input>"))
  (define diagnostics '())
  (define terms '())
  (define origins '())
  (define term-spans '())
  (define quantities (hash))
  (define last-index -1)
  (define records (read-lines text #:file file #:comment-prefix (language-comment body)))
  (define (add-term! value span kind)
    (hash-set! value-origin-cache value span)
    (set! quantities (hash-update quantities (car value) add1 0))
    (set! terms (append terms (list value)))
    (set! origins
      (append
       origins
       (list (hash 'kind kind 'path (src-span-file span) 'line (src-span-start-line span)))))
    (set! term-spans (append term-spans (list span))))
  (parameterize
   ((current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics)))))
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
            (add-term!
             (term-value (form-meaning candidate) (cdr hit) body)
             (line-record-span record)
             "program"))))))))
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
        (add-term! (term-value (form-meaning (car hit)) (cdr hit) body) (cdr default) "default")))))
   (for
    ((name (in-list (sort (hash-keys (language-counts body)) string<?))))
    (define multiplicity (hash-ref (language-counts body) name))
    (define quantity (hash-ref quantities name 0))
    (unless (cond
             ((equal? multiplicity "1") (= quantity 1))
             ((equal? multiplicity "0..1") (<= quantity 1))
             ((equal? multiplicity "1..*") (>= quantity 1))
             ((equal? multiplicity "*") #t)
             (else #f))
      (define matching
        (for/list
         ((value (in-list terms))
          (origin (in-list origins))
          (span (in-list term-spans))
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
          (hash 'constructor name 'expected multiplicity 'actual quantity)))))))
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
   ordered))

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

(define (normalize-terms body terms)
  (define diagnostics '())
  (define (evaluate value)
    (cond
     ((not (list? value)) value)
     ((hash-has-key? library-arities (car value))
      (library-call (car value) (map evaluate (cdr value)) (language-span body)))
     (else (cons (car value) (map evaluate (cdr value))))))
  (define (normalize value)
    (define evaluated (evaluate value))
    (unless (and (list? evaluated) (pair? evaluated))
      (declaration-fail
       "syntax"
       evaluated
       "A top-level meaning must be a term"
       (language-span body)))
    (define item (hash-ref (language-constructors body) (car evaluated) #f))
    (unless item
      (declaration-fail
       "reference"
       (car evaluated)
       "The term constructor is undeclared"
       (language-span body)))
    (for
     ((argument (in-list (cdr evaluated))) (class-name (in-list (constructor-signature item))))
     (unless (and
              (string? argument)
              (class-accepts? (hash-ref (language-classes body) class-name) argument))
       (fail
        "E_TERM_CLASS"
        "The normalized argument fails its declared string class"
        (language-span body)
        (hash
         'kind
         "result"
         'constructor
         (constructor-name item)
         'class
         class-name
         'actual
         argument))))
    (if (or (constructor-primitive? item)
            (and (constructor-fact? item) (not (constructor-rule item))))
      (list evaluated)
      (let ((bindings
             (for/hash
              ((name (in-list (car (constructor-rule item)))) (argument (in-list (cdr evaluated))))
              (values name argument))))
        (append-map
         (lambda (right) (normalize (instantiate-term right bindings)))
         (cdr (constructor-rule item))))))
  (define result
    (parameterize
     ((current-diagnostic-sink (lambda (item) (set! diagnostics (cons item diagnostics)))))
     (append-map
      (lambda (value)
        (parameterize
         ((current-program-span (hash-ref value-origin-cache value #f)))
         (recover (lambda () (normalize value)) '())))
      terms)))
  (raise-diagnostics (diagnostics-sort (reverse diagnostics)))
  result)

(define (declaration-bound body)
  (define memo (make-hash))
  (define (cost name)
    (hash-ref!
     memo
     name
     (lambda ()
       (define item (hash-ref (language-constructors body) name))
       (if (constructor-rule item)
         (for/sum ((right (in-list (cdr (constructor-rule item))))) (cost (term-constructor right)))
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
