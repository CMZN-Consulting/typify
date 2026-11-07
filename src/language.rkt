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
         check-program
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

(define (words text) (string-split text))

(define (class-run thunk span)
  (with-handlers
   ((class-error?
     (lambda (error)
       (fail (class-error-code error) (exn-message error) span (class-error-data error)))))
   (thunk)))

(define meta-class-cache (make-hash))

(define (compiled-meta-classes)
  (hash-ref!
   meta-class-cache
   (current-meta-classes)
   (lambda () (compile-classes (current-meta-classes)))))

(define current-reader-meta (make-parameter #f))

(define (parts production span)
  (define record (car (read-lines production #:file (src-span-file span) #:comment-prefix "")))
  (when (memq (line-record-kind record) '(blank comment unclassified))
    (fail "E_FORM" "A form must have a bang, colon or arrow shape" span))
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

(define (meta-row record)
  (define from-language
    (and (current-reader-meta)
         (let ((hit (find-form (current-reader-meta) record)))
           (and hit
                (let* ((value (term-value (form-meaning (car hit)) (cdr hit) (current-reader-meta)))
                       (normalized (normalize-terms (current-reader-meta) (list value))))
                  (unless (= (length normalized) 1)
                    (fail
                     "E_META_ACTION"
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
                    (fail
                     "E_META_ACTION"
                     "The meta primitive is not in the kernel"
                     (line-record-span record)))
                  (unless (and
                           (= (length (cdr action)) (length (hash-ref entry 'arguments)))
                           (for/and
                            ((value (in-list (cdr action)))
                             (class-name (in-list (hash-ref entry 'arguments))))
                            (and (string? value)
                                 (class-accepts?
                                  (hash-ref (compiled-meta-classes) class-name)
                                  value))))
                    (fail
                     "E_META_SIGNATURE"
                     "The meta action violates the kernel primitive contract"
                     (line-record-span record)))
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
            ((entry (in-list (current-meta-forms))))
            (define-values
             (kind left right)
             (parts (hash-ref entry 'production) (line-record-span record)))
            (define captures (form-captures kind left right record))
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
                     (fail
                      "E_META_ACTION"
                      "The meta primitive is not in the kernel"
                      (line-record-span record)))
                   (unless (and
                            (= (length arguments) (length signature))
                            (for/and
                             ((argument (in-list arguments)) (class-name (in-list signature)))
                             (and (hash-has-key? (compiled-meta-classes) class-name)
                                  (class-accepts?
                                   (hash-ref (compiled-meta-classes) class-name)
                                   argument))))
                     (fail
                      "E_META_SIGNATURE"
                      "The meta primitive arguments fail its signature"
                      (line-record-span record)))
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
       "E_DECLARATION_FORM"
       "The line is not a declared meta form"
       (line-record-span record)
       (hash 'line (line-record-raw record)))))

(define (capture-value row key) (hash-ref (hash-ref row 'captures) key))

(define (row-span row) (line-record-span (hash-ref row 'record)))

(define (ordered-constructors constructors)
  (sort (hash-values constructors) < #:key constructor-ordinal))

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
         (define restricted
           (for/fold
            ((machine trimmed))
            ((literal (in-list forbidden)))
            (class-without-literal
             machine
             (if (symbol? literal) (symbol->string literal) literal))))
         (if (eq? (form-kind candidate) 'bang)
           (class-intersection
            restricted
            (hash-ref
             (compile-classes (hash "nonempty" "/.+/") #:caps (compiled-class-caps restricted))
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
  (parameterize ((current-reader-meta meta)) (read-declarations/internal text file)))

(define (read-declarations/internal text file)
  (define lines (read-lines text #:file file))
  (unless (and (pair? lines) (equal? (string-trim (line-record-raw (car lines))) "#lang typify"))
    (fail
     "E_HEADER"
     "A declaration file must open with #lang typify"
     (if (pair? lines) (line-record-span (car lines)) (src-span file 1 1 1 1 0 0))))
  (define bodies '())
  (define current #f)
  (define opening #f)
  (define rows '())
  (for
   ((record (in-list (cdr lines))))
   (case (line-record-kind record)
     ((blank) (void))
     ((comment)
      (fail
       "E_DECLARATION_COMMENT"
       "Declaration prose belongs outside the declaration"
       (line-record-span record)))
     (else
      (define row (meta-row record))
      (case (hash-ref row 'kind)
        ((declare)
         (when current
           (fail
            "E_BODY_NESTED"
            "Close the current language before declaring another"
            (row-span row)))
         (set! current (capture-value row "name"))
         (set! opening (row-span row))
         (set! rows '()))
        ((end)
         (unless current (fail "E_BODY_END" "No language is open" (row-span row)))
         (set! bodies (append bodies (list (compile-body current rows opening))))
         (set! current #f))
        (else
         (unless current
           (fail "E_BODY_MISSING" "Declare a language before its contents" (row-span row)))
         (set! rows (append rows (list row))))))))
  (when current (fail "E_BODY_UNCLOSED" "The language has no end!" opening))
  (define result
    (for/fold
     ((result (hash)))
     ((body (in-list bodies)))
     (when (hash-has-key? result (language-name body))
       (fail "E_LANGUAGE_DUPLICATE" "The language is declared twice" (language-span body)))
     (hash-set result (language-name body) body)))
  (for*
   ((body (in-list bodies)) (name (in-list (language-sees body))))
   (unless (hash-has-key? result name)
     (fail
      "E_LANGUAGE_REFERENCE"
      "The referenced language is not declared"
      (language-span body)
      (hash 'name name))))
  result)

(define (compile-body name rows opening)
  (define sources (hash))
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
       "E_PROPERTY_DUPLICATE"
       "The property is declared twice"
       (row-span row)
       (hash 'property (symbol->string key))))
    (set! properties (hash-set properties key value)))
  (define (finish-form!)
    (when current-form
      (unless (hash-ref current-form 'meaning #f)
        (fail "E_MEANING_MISSING" "Every form requires does" (hash-ref current-form 'span)))
      (set! pending-forms (append pending-forms (list current-form)))
      (set! current-form #f)))
  (for
   ((row (in-list rows)))
   (define kind (hash-ref row 'kind))
   (define span (row-span row))
   (when (and
          current-form
          (not (hash-ref current-form 'meaning #f))
          (not (memq kind '(does restricts))))
     (fail
      "E_MEANING_MISSING"
      "does must immediately follow its form"
      (hash-ref current-form 'span)))
   (case kind
     ((class)
      (define key (capture-value row "name"))
      (when (or (member key '("letter" "canonical-path")) (hash-has-key? sources key))
        (fail "E_CLASS_DUPLICATE" "The class is declared twice or reserved" span (hash 'name key)))
      (set! sources (hash-set sources key (capture-value row "value"))))
     ((cap)
      (define key (capture-value row "limit"))
      (when (hash-has-key? caps key) (fail "E_CAP_DUPLICATE" "The cap is declared twice" span))
      (define value (string->number (capture-value row "number")))
      (unless (and
               (hash-has-key? meta-cap-defaults key)
               (<= value (hash-ref meta-cap-defaults key)))
        (fail
         "E_CAP_MAXIMUM"
         "A body may lower a cap, never raise it"
         span
         (hash 'cap key 'value value)))
      (set! caps (hash-set caps key value)))
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
      (define kernel-library?
        (and (equal? name "Typify")
             (eq? kind 'primitive)
             (hash-has-key? library-arities key)
             (equal? signature (make-list (car (hash-ref library-arities key)) "value"))))
      (when (or
             (hash-has-key? constructors key)
             (and (hash-has-key? library-arities key) (not kernel-library?)))
        (fail
         "E_CONSTRUCTOR_DUPLICATE"
         "The constructor is declared twice or reserved"
         span
         (hash 'name key)))
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
      (when (hash-has-key? facts key) (fail "E_FACT_DUPLICATE" "The fact is declared twice" span))
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
      (unless current-form (fail "E_MEANING_ORPHAN" "does must follow a form" span))
      (when (hash-ref current-form 'meaning #f)
        (fail "E_MEANING_DUPLICATE" "The form has two meanings" span))
      (set! current-form
        (hash-set
         current-form
         'meaning
         (parse-term (capture-value row "value") #:span (line-right-span (hash-ref row 'record))))))
     ((restricts)
      (unless current-form (fail "E_RESTRICTION_ORPHAN" "restricts! must belong to a form" span))
      (when (hash-ref current-form 'restricting? #f)
        (fail "E_RESTRICTION_DUPLICATE" "The restriction is declared twice" span))
      (set! current-form (hash-set current-form 'restricting? #t)))
     ((example default)
      (unless current-form
        (fail "E_EXAMPLE_ORPHAN" "An example or default must follow a form" span))
      (define key (if (eq? kind 'example) 'examples 'defaults))
      (set! current-form
        (hash-set
         current-form
         key
         (append (hash-ref current-form key) (list (cons (capture-value row "value") span))))))
     ((count)
      (define key (capture-value row "name"))
      (when (hash-has-key? counts key)
        (fail "E_COUNT_DUPLICATE" "The constructor has two counts" span))
      (set! counts (hash-set counts key (capture-value row "multiplicity"))))
     ((rule) (set! rules (append rules (list row))))
     ((sees) (set! sees (append sees (list (capture-value row "name")))))
     ((note) (set! notes (append notes (list (capture-value row "name")))))
     ((cell writes reads ensure join effect stage guard scope fuel) (void))
     (else (fail "E_META_OPERATION" "The meta operation has no implementation" span))))
  (finish-form!)
  (define class-sources
    (for/fold
     ((merged (current-meta-classes)))
     (((key value) (in-hash sources)))
     (hash-set merged key value)))
  (define classes (class-run (lambda () (compile-classes class-sources #:caps caps)) opening))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (for
    ((class-name (in-list (constructor-signature item))))
    (unless (hash-has-key? classes class-name)
      (fail
       "E_CLASS_REFERENCE"
       "A constructor uses an undeclared class"
       (constructor-span item)
       (hash 'class class-name)))))
  (for
   ((row (in-list rules)))
   (define source (capture-value row "value"))
   (define sides (string-split source "->" #:trim? #f))
   (unless (= (length sides) 2) (fail "E_RULE" "A rule has one arrow" (row-span row)))
   (define left (parse-term (string-trim (car sides)) #:span (row-span row)))
   (define key (term-constructor left))
   (unless (hash-has-key? constructors key)
     (fail "E_RULE_CONSTRUCTOR" "A rule names an undeclared constructor" (row-span row)))
   (define item (hash-ref constructors key))
   (when (or (constructor-primitive? item) (constructor-rule item))
     (fail
      "E_RULE_DUPLICATE"
      "A primitive cannot be rewritten and a constructor has at most one rule"
      (row-span row)))
   (unless (and
            (= (length (term-arguments left)) (length (constructor-signature item)))
            (andmap capture? (term-arguments left)))
     (fail
      "E_RULE_LEFT"
      "A rule left side contains only its constructor and variables"
      (row-span row)))
   (define names (map capture-name (term-arguments left)))
   (when (check-duplicates names)
     (fail "E_RULE_VARIABLE" "A rule variable is repeated" (row-span row)))
   (define right
     (map
      (lambda (part) (parse-term (string-trim part) #:span (row-span row)))
      (string-split (cadr sides) ";" #:trim? #f)))
   (set! constructors
     (hash-set constructors key (struct-copy constructor item (rule (cons names right))))))
  (define effect-body? (ormap constructor-primitive? (hash-values constructors)))
  (for
   ((key (in-list (sort (hash-keys facts) string<?))))
   (define span (hash-ref facts key))
   (unless (hash-has-key? constructors key)
     (fail "E_FACT_CONSTRUCTOR" "The fact constructor is undeclared" span))
   (when (or
          (constructor-primitive? (hash-ref constructors key))
          (constructor-rule (hash-ref constructors key)))
     (fail "E_FACT_EFFECT" "A fact cannot also be a primitive or rewritten effect" span)))
  (set! constructors
    (for/hash
     (((key item) (in-hash constructors)))
     (values
      key
      (struct-copy constructor item (fact? (or (not effect-body?) (hash-has-key? facts key)))))))
  (for
   ((item (in-list (ordered-constructors constructors))))
   (unless (or (constructor-primitive? item) (constructor-fact? item) (constructor-rule item))
     (fail
      "E_CONSTRUCTOR_UNREDUCED"
      "A constructor must be primitive or have one rule"
      (constructor-span item)
      (hash 'constructor (constructor-name item))))
   (when (constructor-rule item)
     (for
      ((right (in-list (cdr (constructor-rule item)))))
      (unless (hash-has-key? constructors (term-constructor right))
        (fail
         "E_RULE_RESULT"
         "A rule emits terms; library calls are arguments of those terms"
         (term-span right)))
      (validate-term!
       right
       constructors
       classes
       (for/hash
        ((name (in-list (car (constructor-rule item))))
         (class-name (in-list (constructor-signature item))))
        (values name (hash-ref classes class-name)))
       (constructor-ordinal item)
       #:record-check
       (lambda (entry)
         (record-signature! (hash 'kind "rewrite" 'ordinal (constructor-ordinal item)) entry))))))
  (for
   ((pending (in-list pending-forms)) (index (in-naturals)))
   (define span (hash-ref pending 'span))
   (define-values
    (kind left right)
    (parts (hash-ref pending 'source) (hash-ref pending 'production-span)))
   (define slots (append (filter slot? left) (if right (filter slot? right) '())))
   (when (check-duplicates (map slot-name slots))
     (fail "E_TEMPLATE_DUPLICATE" "A placeholder occurs on both sides of the line" span))
   (for
    ((placeholder (in-list slots)))
    (unless (hash-has-key? classes (slot-name placeholder))
      (fail
       "E_CLASS_REFERENCE"
       "A form placeholder has no class"
       (slot-span placeholder)
       (hash 'class (slot-name placeholder)))))
   (for
    ((side (in-list (filter values (list left right)))))
    (for
     ((a (in-list side)) (b (in-list (append (cdr side) (list #f)))))
     (when (and (slot? a) (string? b) (positive? (string-length b)))
       (define class (hash-ref classes (slot-name a)))
       (define witness (class-delimiter-witness class b))
       (when witness
         (fail
          "E_DELIMITER_AMBIGUOUS"
          "A placeholder can contain its following delimiter"
          (slot-span a)
          (hash 'delimiter b 'witness witness)))
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
   (unless (and
            (hash-has-key? constructors (term-constructor meaning))
            (not (hash-has-key? library-arities (term-constructor meaning))))
     (fail
      "E_MEANING_CONSTRUCTOR"
      "does must build a declared constructor"
      (term-span meaning)
      (hash
       'constructor
       (term-constructor meaning)
       'expected
       "declared-constructor"
       'actual
       (cond
        ((member (term-constructor meaning) '("prefix" "suffix" "equal")) "predicate")
        ((member (term-constructor meaning) '("ancestors" "split")) "list")
        ((hash-has-key? library-arities (term-constructor meaning)) "string")
        (else "undeclared-constructor")))))
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
        (hash-ref pending 'restricting? #f))))))
  (for
   ((candidate (in-list forms)) #:when (form-restricting? candidate))
   (define multiplicity (hash-ref counts (term-constructor (form-meaning candidate)) "*"))
   (unless (or (member multiplicity '("1" "1..*")) (pair? (form-defaults candidate)))
     (fail
      "E_RESTRICTION_MISSING"
      "A restricting form needs a required multiplicity or a default"
      (form-span candidate))))
  (for
   ((key (in-list (sort (hash-keys counts) string<?))))
   (unless (hash-has-key? constructors key)
     (fail
      "E_COUNT_CONSTRUCTOR"
      "A count names an undeclared constructor"
      opening
      (hash 'constructor key))))
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
   (define left-machine (hash-ref machines earlier))
   (define right-machine (hash-ref machines later))
   (define witness (class-overlap-witness left-machine right-machine))
   (when witness
     (fail
      "E_FORM_OVERLAP"
      "Two forms accept the same line"
      (form-span later)
      (hash 'earlier (form-source earlier) 'later (form-source later) 'witness witness)))
   (set! overlap-checks
     (cons
      (hash
       'earlier
       (form-ordinal earlier)
       'later
       (form-ordinal later)
       'bounds
       (class-check-count-bounds left-machine right-machine))
      overlap-checks)))
  (for*
   ((candidate (in-list forms))
    (pair (in-list (append (form-examples candidate) (form-defaults candidate)))))
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
     (fail "E_EXAMPLE_EMPTY" "An example must dispatch to its intended form" (cdr pair)))
   (define hit (find-form body (car records)))
   (unless (and hit (= (form-ordinal (car hit)) (form-ordinal candidate)))
     (fail "E_EXAMPLE_DISPATCH" "An example does not reach its intended form" (cdr pair)))
   (term-value (form-meaning candidate) (cdr hit) body))
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
     "E_TERM_CONSTRUCTOR"
     "The term constructor is undeclared"
     (term-span expression)
     (hash 'constructor name)))
  (when (and upper item (not (constructor-primitive? item)) (>= (constructor-ordinal item) upper))
    (fail
     "E_RULE_STRATIFICATION"
     "A rule may refer only to earlier constructors, primitives and library functions"
     (term-span expression)))
  (define arities
    (if item (list (length (constructor-signature item))) (hash-ref library-arities name)))
  (unless (member (length args) arities)
    (fail "E_TERM_ARITY" "The term has the wrong number of arguments" (term-span expression)))
  (for
   ((argument (in-list args)))
   (cond
    ((capture? argument)
     (unless (if
              (hash? captures)
              (hash-has-key? captures (capture-name argument))
              (member (capture-name argument) captures))
       (fail
        "E_CAPTURE_UNBOUND"
        "The meaning refers to an unbound capture"
        (capture-span argument))))
    ((term? argument)
     (unless (hash-has-key? library-arities (term-constructor argument))
       (fail
        "E_TERM_NESTED"
        "A string-class position permits a library call, not a nested user constructor"
        (term-span argument)))
     (when (member (term-constructor argument) '("ancestors" "split"))
       (fail
        "E_TERM_CLASS"
        "A string-class position cannot receive a list-valued library call"
        (term-span argument)
        (hash 'constructor (term-constructor argument) 'expected "string" 'actual "list")))
     (when (member (term-constructor argument) '("prefix" "suffix" "equal"))
       (fail
        "E_TERM_CLASS"
        "A predicate is permitted only in if or ensure, not a string-class position"
        (term-span argument)
        (hash
         'constructor
         (term-constructor argument)
         'expected
         "string"
         'actual
         "predicate"
         'allowed-contexts
         '("if" "ensure"))))
     (validate-term! argument constructors classes captures upper #:record-check record-check))
    ((literal? argument) (void))
    (else (fail "E_TERM" "The term contains an invalid argument" (term-span expression)))))
  (when item
    (for
     ((argument (in-list args))
      (class-name (in-list (constructor-signature item)))
      (index (in-naturals)))
     (when (and
            (literal? argument)
            (not (class-accepts? (hash-ref classes class-name) (literal-value argument))))
       (fail
        "E_TERM_CLASS"
        "A literal term argument fails its declared class"
        (literal-span argument)
        (hash 'class class-name 'value (literal-value argument))))
     (when (and (capture? argument) (hash? captures))
       (define source-class (hash-ref captures (capture-name argument)))
       (define target-class (hash-ref classes class-name))
       (define witness (class-difference-witness source-class target-class))
       (when witness
         (fail
          "E_TERM_CLASS"
          "The capture class is not contained in the receiving argument class"
          (capture-span argument)
          (hash 'capture (capture-name argument) 'class class-name 'witness witness)))
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

(define (find-form body record)
  (for/or
   ((candidate (in-list (language-forms body))))
   (define captures
     (form-captures (form-kind candidate) (form-left candidate) (form-right candidate) record))
   (and captures (captures-fit? captures (language-classes body)) (cons candidate captures))))

(define (line-accepts? text body)
  (and (not (regexp-match? #rx"[\r\n]" text))
       (let ((record (car (read-lines text #:comment-prefix (language-comment body)))))
         (if (memq (line-record-kind record) '(blank comment))
           #t
           (and (find-form body record) #t)))))

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
            (hash 'class class-name 'value argument)))))))
  (check value)
  value)

(define (check-program text body #:file (file "<input>"))
  (define terms '())
  (define origins '())
  (define quantities (hash))
  (define last-index -1)
  (define (add-term! value origin)
    (set! quantities (hash-update quantities (car value) add1 0))
    (set! terms (append terms (list value)))
    (set! origins (append origins (list origin))))
  (for
   ((record (in-list (read-lines text #:file file #:comment-prefix (language-comment body)))))
   (unless (memq (line-record-kind record) '(blank comment))
     (define hit (find-form body record))
     (unless hit
       (fail
        "E_PROGRAM_FORM"
        "The line matches no form with valid captures"
        (line-record-span record)
        (hash 'line (line-record-raw record))))
     (define candidate (car hit))
     (when (and (equal? (language-order body) "strict") (< (form-ordinal candidate) last-index))
       (fail
        "E_PROGRAM_ORDER"
        "The line violates the declared form order"
        (line-record-span record)))
     (set! last-index (max last-index (form-ordinal candidate)))
     (define value (term-value (form-meaning candidate) (cdr hit) body))
     (add-term! value (line-record-span record))))
  (define explicit-constructors quantities)
  (for*
   ((candidate (in-list (language-forms body)))
    #:unless
    (hash-has-key? explicit-constructors (term-constructor (form-meaning candidate)))
    (default (in-list (form-defaults candidate))))
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
     (fail "E_PROGRAM_FORM" "The default matches no form with valid captures" (cdr default)))
   (add-term!
    (term-value (form-meaning (car hit)) (cdr hit) body)
    (hash
     'kind
     "default"
     'file
     (src-span-file (cdr default))
     'range
     (src-span->jsexpr (cdr default))
     'source
     (car default))))
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
     (fail
      "E_PROGRAM_COUNT"
      "The constructor count violates its multiplicity"
      (language-span body)
      (hash 'constructor name 'multiplicity multiplicity 'actual quantity))))
  (hash 'valid #t 'language (language-name body) 'terms terms 'origins origins))

(define library-domain-classes (delay (compile-classes (hash "allowed" "/.*/"))))

(define (library-pattern-accepts? name text)
  (class-pattern-accepts? (hash-ref (force library-domain-classes) name) text))

(define (library-call name args span)
  (define (strings!)
    (unless (andmap string? args)
      (fail
       "E_LIBRARY_ARGUMENT"
       "The library function requires string arguments"
       span
       (hash 'function name))))
  (strings!)
  (define (domain! value index domain valid?)
    (unless valid?
      (fail
       "E_LIBRARY_DOMAIN"
       "The library argument is outside its specified domain"
       span
       (hash 'function name 'argument index 'domain domain))))
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
     (when (equal? value "/") (fail "E_LIBRARY_PARENT" "The root has no parent" span))
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
  (define (evaluate value)
    (cond
     ((not (list? value)) value)
     ((hash-has-key? library-arities (car value))
      (library-call (car value) (map evaluate (cdr value)) (language-span body)))
     (else (cons (car value) (map evaluate (cdr value))))))
  (define (normalize value)
    (define evaluated (evaluate value))
    (unless (and (list? evaluated) (pair? evaluated))
      (fail "E_NORMAL_FORM" "A top-level meaning must be a term" (language-span body)))
    (define item (hash-ref (language-constructors body) (car evaluated) #f))
    (unless item
      (fail "E_NORMAL_CONSTRUCTOR" "The term constructor is undeclared" (language-span body)))
    (for
     ((argument (in-list (cdr evaluated))) (class-name (in-list (constructor-signature item))))
     (unless (and
              (string? argument)
              (class-accepts? (hash-ref (language-classes body) class-name) argument))
       (fail
        "E_TERM_CLASS"
        "The normalized argument fails its declared string class"
        (language-span body)
        (hash 'constructor (constructor-name item) 'class class-name 'value argument))))
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
  (append-map normalize terms))

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
  (string-append
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
    "\n")
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
