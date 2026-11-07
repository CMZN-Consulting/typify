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
         emit-declarations
         declaration->jsexpr
         normalize-terms
         meta-language
         form-automata)

(struct constructor (name signature primitive? rule span ordinal) #:transparent)

(struct form (source kind left right meaning examples defaults span ordinal) #:transparent)

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

(define (trim-angle word)
  (if (and (> (string-length word) 2) (string-prefix? word "<") (string-suffix? word ">"))
    (substring word 1 (sub1 (string-length word)))
    word))

(define (words text) (map trim-angle (string-split text)))

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
  (define record (car (read-lines production #:file (src-span-file span))))
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
         (for/fold
          ((machine trimmed))
          ((literal (in-list forbidden)))
          (class-without-literal
           machine
           (if (symbol? literal) (symbol->string literal) literal))))))

(define (form-automata body candidate)
  (values
   (field-automaton (language-classes body) candidate #f)
   (field-automaton (language-classes body) candidate #t)))

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
  (define properties (hash))
  (define defaults '())
  (define sees '())
  (define notes '())
  (define current-form #f)
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
   (when (and current-form (not (hash-ref current-form 'meaning #f)) (not (eq? kind 'does)))
     (fail
      "E_MEANING_MISSING"
      "does must immediately follow its form"
      (hash-ref current-form 'span)))
   (case kind
     ((class)
      (define key (capture-value row "name"))
      (when (or (equal? key "letter") (hash-has-key? sources key))
        (fail "E_CLASS_DUPLICATE" "The class is declared twice or reserved" span (hash 'name key)))
      (set! sources (hash-set sources key (capture-value row "value"))))
     ((cap)
      (define key (capture-value row "name"))
      (when (hash-has-key? caps key) (fail "E_CAP_DUPLICATE" "The cap is declared twice" span))
      (set! caps (hash-set caps key (string->number (capture-value row "number")))))
     ((version) (property! 'version (capture-value row "number") row))
     ((comment) (property! 'comment (capture-value row "value") row))
     ((order) (property! 'order (capture-value row "ordering") row))
     ((file) (property! 'file (capture-value row "value") row))
     ((required) (property! 'required #t row))
     ((optional) (property! 'required #f row))
     ((term term-zero primitive primitive-zero)
      (define key (capture-value row "name"))
      (when (or (hash-has-key? constructors key) (hash-has-key? library-arities key))
        (fail
         "E_CONSTRUCTOR_DUPLICATE"
         "The constructor is declared twice or reserved"
         span
         (hash 'name key)))
      (define signature
        (if (memq kind '(term-zero primitive-zero)) '() (words (capture-value row "value"))))
      (set! constructors
        (hash-set
         constructors
         key
         (constructor
          key
          signature
          (and (memq kind '(primitive primitive-zero)) #t)
          #f
          span
          (length constructor-order))))
      (set! constructor-order (append constructor-order (list key))))
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
     ((cell writes reads ensure) (void))
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
  (for
   ((item (in-list (ordered-constructors constructors))))
   (unless (or (constructor-primitive? item) (constructor-rule item))
     (fail
      "E_CONSTRUCTOR_UNREDUCED"
      "A constructor must be primitive or have one rule"
      (constructor-span item)
      (hash 'constructor (constructor-name item))))
   (when (constructor-rule item)
     (for
      ((right (in-list (cdr (constructor-rule item)))))
      (validate-term!
       right
       constructors
       classes
       (for/hash
        ((name (in-list (car (constructor-rule item))))
         (class-name (in-list (constructor-signature item))))
        (values name (hash-ref classes class-name)))
       (constructor-ordinal item)))))
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
       (define witness (class-delimiter-witness (hash-ref classes (slot-name a)) b))
       (when witness
         (fail
          "E_DELIMITER_AMBIGUOUS"
          "A placeholder can contain its following delimiter"
          (slot-span a)
          (hash 'delimiter b 'witness witness))))))
   (define meaning (hash-ref pending 'meaning))
   (unless (hash-has-key? constructors (term-constructor meaning))
     (fail "E_MEANING_CONSTRUCTOR" "does must build a declared constructor" (term-span meaning)))
   (validate-term!
    meaning
    constructors
    classes
    (for/hash
     ((part (in-list slots)))
     (values (slot-name part) (hash-ref classes (slot-name part))))
    #f)
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
        index)))))
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
  (define (form-language candidate)
    (class-sequence
     (append
      (list (field-automaton classes candidate #f))
      (case (form-kind candidate)
        ((colon) (list " : "))
        ((arrow) (list " -> "))
        ((bang) (list "!")))
      (if (eq? (form-kind candidate) 'bang) '() (list (field-automaton classes candidate #t))))
     #:name
     "form"))
  (define machines
    (for/hash
     ((candidate (in-list forms)))
     (values (form-ordinal candidate) (form-language candidate))))
  (for*
   ((earlier (in-list forms))
    (later (in-list forms))
    #:when
    (and (< (form-ordinal earlier) (form-ordinal later))
         (eq? (form-kind earlier) (form-kind later))))
   (define witness
     (class-overlap-witness
      (hash-ref machines (form-ordinal earlier))
      (hash-ref machines (form-ordinal later))))
   (when witness
     (fail
      "E_FORM_OVERLAP"
      "Two forms accept the same line"
      (form-span later)
      (hash 'earlier (form-source earlier) 'later (form-source later) 'witness witness))))
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
  body)

(define (validate-term! expression constructors classes captures upper)
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
     (validate-term! argument constructors classes captures upper))
    ((literal? argument) (void))
    (else (fail "E_TERM" "The term contains an invalid argument" (term-span expression)))))
  (when item
    (for
     ((argument (in-list args)) (class-name (in-list (constructor-signature item))))
     (when (and
            (literal? argument)
            (not (class-accepts? (hash-ref classes class-name) (literal-value argument))))
       (fail
        "E_TERM_CLASS"
        "A literal term argument fails its declared class"
        (literal-span argument)
        (hash 'class class-name 'value (literal-value argument))))
     (when (and (capture? argument) (hash? captures))
       (define witness
         (class-difference-witness
          (hash-ref captures (capture-name argument))
          (hash-ref classes class-name)))
       (when witness
         (fail
          "E_TERM_CLASS"
          "The capture class is not contained in the receiving argument class"
          (capture-span argument)
          (hash 'capture (capture-name argument) 'class class-name 'witness witness)))))))

(define (find-form body record)
  (for/or
   ((candidate (in-list (language-forms body))))
   (define captures
     (form-captures (form-kind candidate) (form-left candidate) (form-right candidate) record))
   (and captures (captures-fit? captures (language-classes body)) (cons candidate captures))))

(define (term-value expression captures body)
  (define (evaluate item)
    (if (list? item)
      (if (hash-has-key? library-arities (car item))
        (library-call (car item) (map evaluate (cdr item)) (term-span expression))
        (cons (car item) (map evaluate (cdr item))))
      item))
  (define value (evaluate (instantiate-term expression captures)))
  (define (check item)
    (when (list? item)
      (define signature (hash-ref (language-constructors body) (car item) #f))
      (for ((argument (in-list (cdr item)))) (check argument))
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
     (set! quantities (hash-update quantities (car value) add1 0))
     (set! terms (append terms (list value)))
     (set! origins (append origins (list (line-record-span record))))))
  (for
   ((name (in-list (sort (hash-keys (language-counts body)) string<?))))
   (define multiplicity (hash-ref (language-counts body) name))
   (define quantity (hash-ref quantities name 0))
   (unless (case
            (string->symbol multiplicity)
            ((one) (= quantity 1))
            ((at-most-one) (<= quantity 1))
            ((at-least-one) (>= quantity 1))
            ((any) #t))
     (fail
      "E_PROGRAM_COUNT"
      "The constructor count violates its multiplicity"
      (language-span body)
      (hash 'constructor name 'multiplicity multiplicity 'actual quantity))))
  (hash 'valid #t 'language (language-name body) 'terms terms 'origins origins))

(define (library-call name args span)
  (define (strings!)
    (unless (andmap string? args)
      (fail
       "E_LIBRARY_ARGUMENT"
       "The library function requires string arguments"
       span
       (hash 'function name))))
  (strings!)
  (case (string->symbol name)
    ((concat) (apply string-append args))
    ((equal) (if (equal? (car args) (cadr args)) "true" "false"))
    ((prefix) (if (string-prefix? (cadr args) (car args)) "true" "false"))
    ((suffix) (if (string-suffix? (cadr args) (car args)) "true" "false"))
    ((split)
     (when (equal? (cadr args) "")
       (fail "E_LIBRARY_SEPARATOR" "The split separator cannot be empty" span))
     (string-split (car args) (cadr args) #:trim? #f))
    ((join)
     (string-append
      (string-trim (car args) "/" #:left? #f)
      "/"
      (string-trim (cadr args) "/" #:right? #f)))
    ((parent)
     (define value (car args))
     (when (or (equal? value "/") (equal? value ""))
       (fail "E_LIBRARY_PARENT" "The root or empty string has no parent" span))
     (define positions
       (for/list
        ((character (in-string value)) (index (in-naturals)) #:when (char=? character #\/))
        index))
     (cond
      ((null? positions) ".")
      ((zero? (last positions)) "/")
      (else (substring value 0 (last positions)))))
    ((ancestors)
     (let loop ((value (car args)) (result '()))
       (if (or (equal? value "/") (equal? value ".") (equal? value ""))
         result
         (let ((next (library-call "parent" (list value) span)))
           (loop next (append result (list next)))))))))

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
    (if (constructor-primitive? item)
      (list evaluated)
      (let ((bindings
             (for/hash
              ((name (in-list (car (constructor-rule item)))) (argument (in-list (cdr evaluated))))
              (values name argument))))
        (append-map
         (lambda (right) (normalize (instantiate-term right bindings)))
         (cdr (constructor-rule item))))))
  (append-map normalize terms))

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
