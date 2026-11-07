#lang racket/base

(require racket/list
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "facts.rkt"
         "language.rkt"
         "model.rkt"
         "syntax.rkt")

(provide plan-programs
         plan-programs/details
         plan-program
         plan-program/details
         plan-needs-facts?
         program-resolver)

(define (origin-span origin program body)
  (define path (hash-ref origin 'path))
  (define line (hash-ref origin 'line))
  (define stored (hash-ref (hash-ref program '_source-spans (hash)) (cons path line) #f))
  (define program-span
    (and (equal? path (hash-ref program 'path))
         (hash-ref (hash-ref program 'line-spans (hash)) line #f)))
  (define declaration-span
    (and (not program-span)
         (for/first
          ((row (in-list (language-rows body)))
           #:when
           (let ((span (line-record-span (hash-ref row 'record))))
             (and (equal? (src-span-file span) path) (= (src-span-start-line span) line))))
          (line-record-span (hash-ref row 'record)))))
  (or stored
      program-span
      declaration-span
      (raise-arguments-error
       'plan
       "the producing program line's private source span is unavailable"
       "path"
       path
       "line"
       line)))

(define (origin-only origin)
  (hash
   'kind
   (hash-ref origin 'kind)
   'path
   (string-replace (hash-ref origin 'path) "\\" "/")
   'line
   (hash-ref origin 'line)))

(define (fact-callback data) (lambda (name arguments origin) (fact-query data name arguments)))

(define (inherited-callback program)
  (lambda (constructor origin)
    (for*/list
     ((record (in-list (hash-ref program 'enclosing '())))
      #:unless
      (equal? (hash-ref record 'status "present") "guarded")
      (value (in-list (hash-ref record 'terms '())))
      #:when
      (and (equal? (car value) constructor) (pair? (cdr value))))
     (cadr value))))

(define (context-callback program)
  (lambda (constructor origin)
    (define found ((inherited-callback program) constructor origin))
    (when (null? found)
      (raise-typify
       "E_CONTEXT"
       "No enclosing program provides this constructor"
       #:span
       (origin-span origin program (hash-ref program 'body))
       #:data
       (hash 'constructor constructor 'key "")))
    (car found)))

(define (directory-depth program)
  (max
   0
   (sub1
    (length
     (filter
      (lambda (part) (not (equal? part "")))
      (string-split (hash-ref program 'path) "/" #:trim? #f))))))

(define (scope-programs program scope constructor key origin)
  (define enclosing (hash-ref program 'enclosing '()))
  (case (string->symbol scope)
    ((program) (cons program enclosing))
    ((parent)
     (when (null? enclosing)
       (raise-typify
        "E_CONTEXT"
        "The parent scope has no enclosing program"
        #:span
        (origin-span origin program (hash-ref program 'body))
        #:data
        (hash 'constructor constructor 'key key)))
     enclosing)
    ((root)
     (define candidates (cons program enclosing))
     (define minimum (apply min (map directory-depth candidates)))
     (filter (lambda (record) (= (directory-depth record) minimum)) candidates))
    (else (raise-argument-error 'scope-programs "program, parent or root scope" scope))))

(define (resolve-key body program arguments scope constructor origin)
  (for/list
   ((key (in-list arguments)))
   (if (string-prefix? key "/")
     key
     (let* ((records (scope-programs program scope constructor key origin))
            (workdir
             (for*/first
              ((record (in-list records))
               (value (in-list (hash-ref record 'terms '())))
               #:when
               (and (equal? (car value) "workdir") (pair? (cdr value))))
              (cadr value))))
       (unless workdir
         (raise-typify
          "E_CONTEXT"
          "A relative key has no workdir in scope"
          #:span
          (origin-span origin program (hash-ref program 'body))
          #:data
          (hash 'constructor constructor 'key key)))
       (evaluate-rule-expression
        body
        (term
         "join"
         (list
          (literal workdir (origin-span origin program (hash-ref program 'body)))
          (literal key (origin-span origin program (hash-ref program 'body))))
         (origin-span origin program (hash-ref program 'body)))
        (hash)
        #:context
        (context-callback program)
        #:inherited
        (inherited-callback program)
        #:captures
        (hash-ref program 'placeholders (hash))
        #:fact
        (fact-callback #f)
        #:origin
        origin)))))

(define (program-resolver program)
  (lambda (body constructor index class-name value origin scope scope-constructor)
    (if (relative-path-role? (language-classes body) class-name)
      (begin
        (unless (class-accepts? (hash-ref (language-classes body) class-name) value)
          (raise-typify
           "E_TERM_CLASS"
           "The relative argument fails its source class"
           #:span
           (origin-span origin program (hash-ref program 'body))
           #:data
           (hash 'kind "result" 'constructor constructor 'class class-name 'actual value)))
        (values
         (car (resolve-key body program (list value) scope scope-constructor origin))
         "canonical-path"))
      (values value class-name))))

(define (ensure-bindings clause)
  (define initial (hash-ref clause 'bindings))
  (if (hash-ref clause 'arguments-resolved? #f)
    initial
    (let* ((invariant (hash-ref clause 'ensure))
           (body (hash-ref clause 'body))
           (name (model-ensure-constructor invariant))
           (signature (constructor-signature (hash-ref (language-constructors body) name))))
      (for/fold
       ((bindings initial))
       ((class-name (in-list signature)) (index (in-naturals 1)))
       (define key (number->string index))
       (if (and (hash-has-key? bindings key)
                (relative-path-role? (language-classes body) class-name))
         (let-values (((value receiving)
                       ((program-resolver (hash-ref clause 'program))
                        body
                        name
                        (sub1 index)
                        class-name
                        (hash-ref bindings key)
                        (hash-ref clause 'origin)
                        (hash-ref clause 'scope "program")
                        (hash-ref clause 'scope-constructor name))))
           (hash-set bindings key value))
         bindings)))))

(define (tuple-prefix? prefix key)
  (and (<= (length prefix) (length key)) (equal? prefix (take key (length prefix)))))

(define (path-ancestor? ancestor path)
  (and (not (equal? ancestor path))
       (or (equal? ancestor "/") (string-prefix? path (string-append ancestor "/")))))

(define (state-query state cells name arguments)
  (unless (and (pair? arguments) (hash-has-key? cells (car arguments)))
    (raise-arguments-error
     'state-query
     "state predicate names no compiled cell"
     "arguments"
     arguments))
  (define cell-name (car arguments))
  (define key (cdr arguments))
  (define entries (hash-ref state cell-name (hash)))
  (define entry (hash-ref entries key #f))
  (define bound? (and entry (pair? (hash-ref entry 'value)) #t))
  (case (string->symbol name)
    ((absent) (not entry))
    ((masked) (and entry (null? (hash-ref entry 'value)) #t))
    ((bound) bound?)
    ((visible)
     (and bound?
          (or (not (model-cell-stacking? (hash-ref cells cell-name)))
              (not
               (for/or
                (((other-key other-entry) (in-hash entries)))
                (and (equal? (drop-right other-key 1) (drop-right key 1))
                     (path-ancestor? (last other-key) (last key))
                     (> (hash-ref other-entry 'sequence) (hash-ref entry 'sequence))))))))
    (else (raise-argument-error 'state-query "bound, masked, visible or absent" name))))

(define (cell-semantics model cell)
  (list
   (model-cell-law cell)
   (model-cell-stacking? cell)
   (and (eq? (model-cell-law cell) 'join)
        (hash-ref (compiled-model-joins model) (model-cell-table cell)))))

(define (collect-cells programs)
  (define cells (hash))
  (define meanings (hash))
  (define widths (hash))
  (for
   ((program (in-list programs)))
   (define model (language-model (hash-ref program 'body)))
   (for
    ((name (in-list (sort (hash-keys (compiled-model-cells model)) string<?))))
    (define cell (hash-ref (compiled-model-cells model) name))
    (define meaning (cell-semantics model cell))
    (when (and (hash-has-key? meanings name) (not (equal? (hash-ref meanings name) meaning)))
      (raise-typify
       "E_DECLARATION"
       "The shared cell has conflicting declarations"
       #:span
       (model-cell-span cell)
       #:data
       (hash 'kind "conflict" 'actual name)))
    (set! meanings (hash-set meanings name meaning))
    (set! cells (hash-set cells name cell))
    (define width (model-key-width model name))
    (when width
      (when (and (hash-has-key? widths name) (not (= (hash-ref widths name) width)))
        (raise-typify
         "E_DECLARATION"
         "The shared cell has conflicting key widths"
         #:span
         (model-cell-span cell)
         #:data
         (hash 'kind "conflict" 'actual name)))
      (set! widths (hash-set widths name width)))))
  (for
   ((name (in-list (sort (hash-keys cells) string<?))))
   (unless (hash-has-key? widths name)
     (raise-typify
      "E_DECLARATION"
      "The root cell has no put writer to determine its key width"
      #:span
      (model-cell-span (hash-ref cells name))
      #:data
      (hash 'kind "reference" 'actual name))))
  (for
   ((program (in-list programs)))
   (define model (language-model (hash-ref program 'body)))
   (for
    ((write (in-hash-values (compiled-model-writes model)))
     #:when
     (eq? (model-write-operation write) 'delete))
    (when (> (model-write-width write) (hash-ref widths (model-write-cell write)))
      (raise-typify
       "E_DECLARATION"
       "A delete prefix is wider than the root cell's key"
       #:span
       (model-write-span write)
       #:data
       (hash 'kind "conflict" 'actual (model-write-constructor write))))
    (model-check-stacking-write! model write (hash-ref widths (model-write-cell write)))))
  (values cells widths))

(define (state->jsexpr state)
  (for/hash
   (((cell entries) (in-hash state)))
   (values
    cell
    (for/list
     ((key (in-list (sort (hash-keys entries) string<? #:key canonical-json))))
     (define entry (hash-ref entries key))
     (hash
      'key
      key
      'value
      (hash-ref entry 'value)
      'masked
      (null? (hash-ref entry 'value))
      'origin
      (hash-ref entry 'origin))))))

(define (plan-needs-facts? programs)
  (ormap (lambda (program) (language-needs-facts? (hash-ref program 'body))) programs))

(define (plan-programs/details
         programs
         #:facts
         (facts #f)
         #:facts-hash
         (facts-hash "none")
         #:on-error
         (on-error #f))
  (plan-records programs facts facts-hash #f on-error))

(define (plan-programs
         programs
         #:facts
         (facts #f)
         #:facts-hash
         (facts-hash "none")
         #:on-error
         (on-error #f))
  (define-values
   (report events)
   (plan-programs/details programs #:facts facts #:facts-hash facts-hash #:on-error on-error))
  report)

(define (plan-program/details
         body
         text
         #:path
         (path "<input>")
         #:facts
         (facts #f)
         #:facts-hash
         (facts-hash "none")
         #:on-error
         (on-error #f))
  (define initial
    (hash
     'body
     body
     'path
     path
     'status
     "present"
     'placeholders
     (hash)
     'enclosing
     '()
     'line-spans
     (for/hash
      ((record (in-list (read-lines text #:file path))))
      (values (src-span-start-line (line-record-span record)) (line-record-span record)))))
  (define checked
    (check-program
     text
     body
     #:file
     path
     #:context
     (context-callback initial)
     #:inherited
     (inherited-callback initial)
     #:captures
     (hash-ref initial 'placeholders (hash))
     #:guard-resolver
     (lambda (raw-terms raw-origins) (program-resolver (hash-set initial 'terms raw-terms)))
     #:facts
     (fact-callback facts)))
  (define program
    (hash-set*
     initial
     'status
     (if (hash-ref checked 'guarded? #f) "guarded" "present")
     'terms
     (hash-ref checked 'terms)
     'origins
     (hash-ref checked 'origins)
     'skips
     (hash-ref checked 'skips '())
     'effective-lines
     (hash-ref checked 'effective-lines)
     '_term-spans
     (hash-ref checked '_term-spans)))
  (plan-records (list program) facts facts-hash #t on-error))

(define (plan-program
         body
         text
         #:path
         (path "<input>")
         #:facts
         (facts #f)
         #:facts-hash
         (facts-hash "none")
         #:on-error
         (on-error #f))
  (define-values
   (report events)
   (plan-program/details
    body
    text
    #:path
    path
    #:facts
    facts
    #:facts-hash
    facts-hash
    #:on-error
    on-error))
  report)

(define (plan-records programs facts facts-hash single? on-error)
  (unless (or (not on-error) (and (procedure? on-error) (procedure-arity-includes? on-error 1)))
    (raise-argument-error 'plan "one-argument diagnostic handler or #f" on-error))
  (define-values (cells widths) (collect-cells programs))
  (define query-fact (fact-callback facts))
  (define events '())
  (define ensures '())
  (define skips '())
  (define maximum-c 0)
  (define effective-lines 0)
  (for
   ((program (in-list programs)) (program-index (in-naturals)))
   (define body (hash-ref program 'body))
   (when (equal? (language-name body) "Typify")
     (raise-typify
      "E_USAGE"
      "The meta body is not an executable plan"
      #:data
      (hash 'actual "Typify")))
   (set! maximum-c (max maximum-c (hash-ref (declaration-bound body) 'c)))
   (set! effective-lines (+ effective-lines (hash-ref program 'effective-lines 0)))
   (for
    ((skip (in-list (hash-ref program 'skips '()))))
    (set! skips (cons (hash-set skip 'program-index program-index) skips)))
   (unless (equal? (hash-ref program 'status "present") "guarded")
     (define expanded
       (primitive-events
        body
        (hash-ref program 'terms '())
        (hash-ref program 'origins '())
        #:context
        (context-callback program)
        #:inherited
        (inherited-callback program)
        #:captures
        (hash-ref program 'placeholders (hash))
        #:fact
        query-fact
        #:spans
        (hash-ref
         program
         '_term-spans
         (lambda ()
           (map
            (lambda (origin) (origin-span origin program body))
            (hash-ref program 'origins '()))))
        #:resolve
        (program-resolver program)))
     (for
      ((skip (in-list (hash-ref expanded 'skips '()))))
      (set! skips (cons (hash-set skip 'program-index program-index) skips)))
     (for
      ((event (in-list (hash-ref expanded 'events))))
      (set! events (cons (hash-set* event 'program program 'program-index program-index) events)))
     (for
      ((clause (in-list (hash-ref expanded 'ensures))))
      (set! ensures
        (cons (hash-set* clause 'program program 'program-index program-index) ensures)))))
  (define ordered (sort (reverse events) < #:key (lambda (event) (hash-ref event 'stage))))
  (define state (for/hash ((name (in-hash-keys cells))) (values name (hash))))
  (define applied '())
  (define modelled 0)
  (for
   ((event (in-list ordered)) (sequence (in-naturals)))
   (define body (hash-ref event 'body))
   (define model (language-model body))
   (define original (hash-ref event 'term))
   (define name (car original))
   (define origin (origin-only (hash-ref event 'origin)))
   (define program (hash-ref event 'program))
   (define scope (hash-ref event 'scope "program"))
   (define scope-constructor (hash-ref event 'scope-constructor name))
   (define write (hash-ref (compiled-model-writes model) name #f))
   (define key (if write (take (cdr original) (model-write-width write)) '()))
   (define bindings
     (for/hash
      ((value (in-list (cdr original))) (position (in-naturals 1)))
      (values (number->string position) value)))
   (define skip? #f)
   (for
    ((read (in-list (hash-ref (compiled-model-reads model) name '()))) #:break skip?)
    (define expression (model-read-fact read))
    (define tested-key key)
    (define (query-read fact arguments source-origin)
      (set! tested-key arguments)
      (query-fact fact arguments source-origin))
    (define present?
      (if (string? expression)
        (let-values (((present? value) (query-fact expression key origin))) present?)
        (evaluate-rule-expression
         body
         expression
         bindings
         #:context
         (context-callback program)
         #:inherited
         (inherited-callback program)
         #:captures
         (hash-ref program 'placeholders (hash))
         #:fact
         query-read
         #:origin
         origin)))
    (unless present?
      (if (eq? (model-read-behavior read) 'skip)
        (begin
          (set! skip? #t)
          (set! skips
            (cons
             (hash
              'constructor
              name
              'fact
              (if (string? expression) expression (term-constructor expression))
              'args
              tested-key
              'reason
              "read"
              'origin
              origin
              'program-index
              (hash-ref event 'program-index))
             skips)))
        (raise-typify
         "E_FACT_REJECT"
         "A required primitive fact is absent"
         #:span
         (origin-span origin program (hash-ref program 'body))
         #:data
         (hash
          'fact
          (if (string? expression) expression (term-constructor expression))
          'key
          tested-key
          'origins
          (list origin))))))
   (unless skip?
     (define value (if write (drop (cdr original) (model-write-width write)) '()))
     (define planned (if write (cons name (append key value)) original))
     (set! applied
       (cons (hash-set* event 'term planned 'origin origin 'modelled? (and write #t)) applied))
     (when write
       (set! modelled (add1 modelled))
       (define cell-name (model-write-cell write))
       (define cell (hash-ref (compiled-model-cells model) cell-name))
       (define entries (hash-ref state cell-name))
       (case (model-write-operation write)
         ((delete)
          (set! state
            (hash-set
             state
             cell-name
             (for/hash
              (((prior-key entry) (in-hash entries)) #:unless (tuple-prefix? key prior-key))
              (values prior-key entry)))))
         ((put)
          (define prior (hash-ref entries key #f))
          (unless (and prior (eq? (model-cell-law cell) 'first))
            (define joined
              (if (and prior (eq? (model-cell-law cell) 'join))
                (list
                 (hash-ref
                  (hash-ref (compiled-model-joins model) (model-cell-table cell))
                  (cons (car (hash-ref prior 'value)) (car value))))
                value))
            (set! state
              (hash-set
               state
               cell-name
               (hash-set entries key (hash 'value joined 'origin origin 'sequence sequence))))))))))
  (define ensure-results '())
  (for
   ((clause (in-list (reverse ensures))))
   (define invariant (hash-ref clause 'ensure))
   (define origin (origin-only (hash-ref clause 'origin)))
   (define body (hash-ref clause 'body))
   (define program (hash-ref clause 'program))
   (define last-key '())
   (define (query name arguments source-origin)
     (if (member name '("bound" "masked" "visible" "absent"))
       (let ((key (cdr arguments)))
         (unless (= (length key) (hash-ref widths (car arguments)))
           (define row
             (for/first
              ((row (in-list (language-rows body)))
               #:when
               (and (eq? (hash-ref row 'kind) 'ensure)
                    (equal?
                     (hash-ref (hash-ref row 'captures) "constructor")
                     (model-ensure-constructor invariant))
                    (equal?
                     (hash-ref (hash-ref row 'captures) "name")
                     (model-ensure-name invariant))))
              row))
           (raise-typify
            "E_DECLARATION"
            "The invariant key arity disagrees with the root cell"
            #:span
            (model-ensure-span invariant)
            #:data
            (hash 'kind "arity" 'actual (hash-ref (hash-ref row 'captures) "value"))))
         (set! last-key key)
         (state-query state cells name (cons (car arguments) key)))
       (query-fact name arguments source-origin)))
   (define passed?
     (evaluate-rule-expression
      body
      (model-ensure-expression invariant)
      (ensure-bindings clause)
      #:context
      (context-callback program)
      #:inherited
      (inherited-callback program)
      #:captures
      (hash-ref program 'placeholders (hash))
      #:fact
      query
      #:origin
      origin))
   (set! ensure-results
     (cons
      (hash
       'ensure
       (model-ensure-name invariant)
       'ok
       (and passed? #t)
       'origin
       origin
       'program-index
       (hash-ref clause 'program-index))
      ensure-results))
   (unless passed?
     (define diagnostic
       (make-diagnostic
        "E_ENSURE"
        "The predicted state fails its invariant"
        #:span
        (origin-span origin program (hash-ref program 'body))
        #:data
        (hash 'ensure (model-ensure-name invariant) 'key last-key 'origins (list origin))))
     (if on-error (on-error diagnostic) (raise-diagnostics (list diagnostic)))))
  (define final-events (reverse applied))
  (define final-ensures (reverse ensure-results))
  (define final-skips (reverse skips))
  (define bound (hash 'c maximum-c 'n effective-lines 'limit (* maximum-c effective-lines)))
  (when (> (length ordered) (hash-ref bound 'limit))
    (raise-arguments-error
     'plan
     "the emitted primitive count exceeds the calculated declaration bound"
     "actual"
     (length ordered)
     "limit"
     (hash-ref bound 'limit)))
  (define report
    (hash
     'state
     (state->jsexpr state)
     'bound
     bound
     'modelled
     (format "~a of ~a" modelled (length final-events))
     'status
     (if (= modelled (length final-events)) "ok" "unchecked")
     'ensure
     (map (lambda (item) (hash-remove item 'program-index)) final-ensures)
     'skips
     (map skip->jsexpr final-skips)
     'facts
     (if (plan-needs-facts? programs) facts-hash "none")
     'origins
     (map (lambda (event) (hash-ref event 'origin)) final-events)))
  (define result
    (if single?
      (hash-set*
       report
       'plan
       (map event->jsexpr final-events)
       'ensure
       (map (lambda (item) (hash-remove item 'program-index)) final-ensures))
      (hash-set
       report
       'programs
       (for/list
        ((program (in-list programs)) (index (in-naturals)))
        (hash
         'path
         (hash-ref program 'path)
         'status
         (hash-ref program 'status "present")
         'plan
         (for/list
          ((event (in-list final-events)) #:when (= (hash-ref event 'program-index) index))
          (event->jsexpr event))
         'ensure
         (for/list
          ((item (in-list final-ensures)) #:when (= (hash-ref item 'program-index) index))
          (hash-remove item 'program-index))
         'skips
         (for/list
          ((item (in-list final-skips)) #:when (= (hash-ref item 'program-index) index))
          (skip->jsexpr item)))))))
  (values result final-events))

(define (event->jsexpr event)
  (if (hash-ref event 'modelled?)
    (hash-ref event 'term)
    (hash 'term (hash-ref event 'term) 'unchecked #t)))

(define (skip->jsexpr item)
  (for/hash
   ((name '(constructor fact args reason origin)))
   (values name (if (eq? name 'origin) (origin-only (hash-ref item name)) (hash-ref item name)))))
