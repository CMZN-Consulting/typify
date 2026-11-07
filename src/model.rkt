#lang racket/base

(require racket/list
         racket/match
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "syntax.rkt"
         "meta.rkt")

(provide compile-model
         model-needs-facts?
         model-stage
         model-scope
         model-fuel
         model-key-widths
         model-key-width
         model-key-classes
         model-write-key-classes
         model-state?
         model-check-notes
         model-check-stacking-write!
         model-host-guards
         (struct-out compiled-model)
         (struct-out model-cell)
         (struct-out model-write)
         (struct-out model-read)
         (struct-out model-ensure))

(struct model-cell (name law stacking? table span) #:transparent)

(struct model-write (constructor cell operation width span literals) #:transparent)

(struct model-read (constructor fact behavior span) #:transparent)

(struct model-ensure (constructor name expression span) #:transparent)

(struct
 compiled-model
 (cells
  writes
  reads
  ensures
  joins
  external
  stages
  guards
  scopes
  fuels
  constructors
  classes
  span
  states)
 #:transparent)

(define model-host-guards (hash "join-law-checks" 1048576))

(define known-facts '("exists" "directory" "symlink" "mode" "owner" "clean"))

(define (decl-fail kind actual message span)
  (raise-typify "E_DECLARATION" message #:span span #:data (hash 'kind kind 'actual actual)))

(define (row-span row) (line-record-span (hash-ref row 'record)))

(define (row-text row) (line-record-raw (hash-ref row 'record)))

(define (capture-value row name)
  (define captures (hash-ref row 'captures))
  (hash-ref
   captures
   name
   (lambda () (if (equal? name "value") (hash-ref captures "law") (hash-ref captures name)))))

(define (model-stage model constructor) (hash-ref (compiled-model-stages model) constructor 0))

(define (model-scope model constructor)
  (hash-ref (compiled-model-scopes model) constructor "program"))

(define (model-fuel model constructor) (hash-ref (compiled-model-fuels model) constructor #f))

(define (model-key-widths model cell)
  (sort
   (remove-duplicates
    (for/list
     ((write (in-hash-values (compiled-model-writes model)))
      #:when
      (and (equal? (model-write-cell write) cell) (eq? (model-write-operation write) 'put)))
     (model-write-width write)))
   <))

(define (model-key-width model cell)
  (define widths (model-key-widths model cell))
  (and (pair? widths) (car widths)))

(define (model-write-key-classes model write)
  (take
   (hash-ref
    (hash-ref (compiled-model-constructors model) (model-write-constructor write))
    'signature)
   (model-write-width write)))

(define (model-key-classes model cell)
  (define first-put
    (for/first
     ((write
       (in-list
        (sort
         (hash-values (compiled-model-writes model))
         <
         #:key
         (lambda (write) (src-span-start-byte (model-write-span write))))))
      #:when
      (and (equal? (model-write-cell write) cell) (eq? (model-write-operation write) 'put)))
     write))
  (and first-put (model-write-key-classes model first-put)))

(define (model-state? model constructor) (hash-ref (compiled-model-states model) constructor #f))

(define (model-check-notes model)
  (define writes
    (sort
     (hash-values (compiled-model-writes model))
     <
     #:key
     (lambda (write) (src-span-start-byte (model-write-span write)))))
  (for/list
   ((name (in-list (sort (hash-keys (compiled-model-cells model)) string<?)))
    #:unless
    (model-key-width model name)
    #:when
    (ormap (lambda (write) (equal? (model-write-cell write) name)) writes))
   (define first-write
     (for/first ((write (in-list writes)) #:when (equal? (model-write-cell write) name)) write))
   (make-diagnostic
    "N_CELL_WIDTH"
    "The cell's key width must be resolved by a root put writer"
    #:severity
    'note
    #:span
    (model-write-span first-write)
    #:data
    (hash 'cell name))))

(define (state-predicate? name)
  (member name '("bound" "masked" "visible" "absent" "covered" "uncovered")))

(define (library-predicate? name) (member name '("prefix" "suffix" "equal")))

(define (expression-needs-facts? expression)
  (and (term? expression)
       (or (and (not (state-predicate? (term-constructor expression)))
                (not (library-predicate? (term-constructor expression)))
                (not
                 (member
                  (term-constructor expression)
                  '("parent" "join" "split" "ancestors" "concat" "context" "inherited"))))
           (ormap expression-needs-facts? (term-arguments expression)))))

(define (model-needs-facts? model)
  (or (positive? (hash-count (compiled-model-reads model)))
      (positive? (hash-count (compiled-model-guards model)))
      (for/or
       ((clauses (in-hash-values (compiled-model-ensures model))))
       (ormap
        (lambda (clause) (expression-needs-facts? (model-ensure-expression clause)))
        clauses))))

(define (quoted-pair source classes span)
  (define size (string-length source))
  (define position 0)
  (define (skip!)
    (let loop ()
      (when (and (< position size) (char-whitespace? (string-ref source position)))
        (set! position (add1 position))
        (loop))))
  (define (literal!)
    (skip!)
    (unless (and (< position size) (char=? (string-ref source position) #\"))
      (decl-fail "syntax" source "A join entry requires quoted literals" span))
    (define start position)
    (set! position (add1 position))
    (let loop ((escaped? #f))
      (when (>= position size) (decl-fail "syntax" source "A join literal is unclosed" span))
      (define character (string-ref source position))
      (set! position (add1 position))
      (cond
       (escaped? (loop #f))
       ((char=? character #\\) (loop #t))
       ((char=? character #\") (void))
       (else (loop #f))))
    (define fragment (substring source start position))
    (define value
      (with-handlers
       ((class-error? (lambda (error) (decl-fail "syntax" source (exn-message error) span))))
       (decode-quoted-literal fragment)))
    (define alphabet (compiled-class-alphabet (hash-ref classes "letter")))
    (unless (for/and
             ((character (in-string value)))
             (for/or
              ((interval (in-list alphabet)))
              (<= (car interval) (char->integer character) (cdr interval))))
      (decl-fail "syntax" fragment "A join literal must use allowed code points" span))
    value)
  (define left (literal!))
  (define right (literal!))
  (skip!)
  (unless (and (< position size) (char=? (string-ref source position) #\=))
    (decl-fail "syntax" source "A join entry requires an equals sign" span))
  (set! position (add1 position))
  (define result (literal!))
  (skip!)
  (unless (= position size) (decl-fail "syntax" source "Unexpected input after a join entry" span))
  (list left right result))

(define (join-fail cell expected actual span)
  (raise-typify
   "E_JOIN_LAW"
   "The join table fails its declared law"
   #:span
   span
   #:data
   (hash
    'cell
    cell
    'expected
    expected
    'actual
    (if (list? actual) (string-join actual " ") actual))))

(define (validate-join name table domain span #:complete? (complete? #t))
  (define size (length domain))
  (define work (+ size (* size size) (* size size size)))
  (when (> work (hash-ref model-host-guards "join-law-checks"))
    (raise-typify
     "E_CAP"
     "The join law check exceeds its finite work guard"
     #:span
     span
     #:data
     (hash
      'cap
      "join-law-checks"
      'limit
      (hash-ref model-host-guards "join-law-checks")
      'actual
      work)))
  (define completed
    (for/fold
     ((result table))
     ((value (in-list domain)))
     (when (and
            (hash-has-key? result (cons value value))
            (not (equal? (hash-ref result (cons value value)) value)))
       (join-fail name "idempotent" value span))
     (hash-set result (cons value value) value)))
  (when complete?
    (for*
     ((left (in-list domain)) (right (in-list domain)))
     (unless (hash-has-key? completed (cons left right))
       (join-fail name "complete" (list left right) span))))
  (for*
   ((left (in-list domain)) (middle (in-list domain)) (right (in-list domain)))
   (when (and
          (hash-has-key? completed (cons left middle))
          (hash-has-key? completed (cons middle right))
          (hash-has-key? completed (cons (hash-ref completed (cons left middle)) right))
          (hash-has-key? completed (cons left (hash-ref completed (cons middle right)))))
     (unless (equal?
              (hash-ref completed (cons (hash-ref completed (cons left middle)) right))
              (hash-ref completed (cons left (hash-ref completed (cons middle right)))))
       (join-fail name "associative" (list left middle right) span))))
  completed)

(define (table-domain table)
  (define domain
    (sort
     (remove-duplicates
      (append
       (append-map (lambda (pair) (list (car pair) (cdr pair))) (hash-keys table))
       (hash-values table)))
     string<?))
  domain)

(define (literal-domain name classes span (seen '()))
  (when (member name seen) (decl-fail "class-cycle" name "The literal class is cyclic" span))
  (define description (class->description (hash-ref classes name)))
  (cond
   ((hash-has-key? description 'literals)
    (filter
     (lambda (value) (class-accepts? (hash-ref classes name) value))
     (hash-ref description 'literals)))
   ((hash-has-key? description 'ref)
    (literal-domain (hash-ref description 'ref) classes span (cons name seen)))
   (else (decl-fail "syntax" name "A join value requires a literal class" span))))

(define (fact-expression row value)
  (define expression (if (string-prefix? value "(") (parse-term value #:span (row-span row)) value))
  (define name (if (term? expression) (term-constructor expression) expression))
  (unless (member name known-facts)
    (decl-fail "reference" name "The clause names an undeclared fact" (row-span row)))
  expression)

(define (expression-identity value)
  (cond
   ((term? value) (cons (term-constructor value) (map expression-identity (term-arguments value))))
   ((capture? value) (hash 'capture (capture-name value)))
   ((literal? value) (literal-value value))
   (else value)))

(define (check-stacking-key! cell write width classes constructors)
  (when (and (model-cell-stacking? cell) (positive? width) (= (model-write-width write) width))
    (define signature (hash-ref (hash-ref constructors (model-write-constructor write)) 'signature))
    (define class-name (list-ref signature (sub1 width)))
    (when (hash-has-key? classes class-name)
      (define witness
        (and (not (relative-path-role? classes class-name))
             (class-difference-witness
              (hash-ref classes class-name)
              (hash-ref classes "canonical-path"))))
      (when witness
        (raise-typify
         "E_KEY_CANONICAL"
         "The last stacking key component is not canonical"
         #:span
         (model-write-span write)
         #:data
         (hash 'cell (model-cell-name cell) 'class class-name 'witness witness))))))

(define (model-check-stacking-write! model write width)
  (check-stacking-key!
   (hash-ref (compiled-model-cells model) (model-write-cell write))
   write
   width
   (compiled-model-classes model)
   (compiled-model-constructors model)))

(define (compile-model
         rows
         constructors
         classes
         span
         #:on-error
         (on-error #f)
         #:template-names
         (template-names '()))
  (unless (or (not on-error) (and (procedure? on-error) (procedure-arity-includes? on-error 1)))
    (raise-argument-error 'compile-model "one-argument diagnostic handler or #f" on-error))
  (define failed (make-hash))
  (define (attempt thunk (row #f))
    (with-handlers
     ((exn:fail:typify?
       (lambda (error)
         (when row
           (define captures (hash-ref row 'captures))
           (define name (hash-ref captures "name" (lambda () (hash-ref captures "constructor" #f))))
           (hash-set! failed (cons (hash-ref row 'kind) name) #t))
         (if on-error
           (begin (for-each on-error (exn:fail:typify-diagnostics error)) #f)
           (raise error)))))
     (thunk)))
  (define (failed? kind name) (hash-ref failed (cons kind name) #f))
  (define cells (hash))
  (define writes (hash))
  (define write-rows (hash))
  (define reads (hash))
  (define ensures (hash))
  (define joins (hash))
  (define join-spans (hash))
  (define join-entries (hash))
  (define join-domains (hash))
  (define table-domains (hash))
  (define used-tables (make-hash))
  (define external (hash))
  (define states (hash))
  (define external-spans (hash))
  (define stages (hash))
  (define guards (hash))
  (define scopes (hash))
  (define fuels (hash))
  (define (constructor! name row #:primitive? (primitive? #f))
    (unless (hash-has-key? constructors name)
      (decl-fail "reference" name "The clause names an undeclared constructor" (row-span row)))
    (define descriptor (hash-ref constructors name))
    (when (and primitive? (not (hash-ref descriptor 'primitive?)))
      (decl-fail "conflict" name "The clause requires a primitive constructor" (row-span row)))
    descriptor)
  (define (duplicate! table name row)
    (when (hash-has-key? table name)
      (decl-fail "duplicate" name "The model clause is declared twice" (row-span row))))
  (for
   ((row (in-list rows)))
   (attempt
    (lambda ()
      (case (hash-ref row 'kind)
        ((cell)
         (define name (capture-value row "name"))
         (when (or
                (member name builtin-class-names)
                (hash-has-key? library-arities name)
                (hash-has-key? (current-meta-classes) name))
           (decl-fail "duplicate" name "The cell name is reserved" (row-span row)))
         (duplicate! cells name row)
         (define parts (string-split (capture-value row "value")))
         (define stacking? (and (pair? parts) (equal? (last parts) "stacking")))
         (define base (if stacking? (drop-right parts 1) parts))
         (define-values
          (law table)
          (match
           base
           ((list "shadow") (values 'shadow #f))
           ((list "first") (values 'first #f))
           ((list "join" table) (values 'join table))
           (_ (decl-fail "syntax" (row-text row) "The cell law is malformed" (row-span row)))))
         (set! cells (hash-set cells name (model-cell name law stacking? table (row-span row)))))
        ((writes)
         (define name (capture-value row "name"))
         (define descriptor (constructor! name row #:primitive? #t))
         (duplicate! writes name row)
         (define match
           (regexp-match
            #px"^([^ =]+) *= *(put|delete) +([0-9]+)(?: +(.*))?$"
            (capture-value row "value")))
         (unless match
           (decl-fail "syntax" (row-text row) "The write clause is malformed" (row-span row)))
         (define width (string->number (list-ref match 3)))
         (define literal-source (list-ref match 4))
         (define literals
           (if literal-source
             (begin
               (unless (equal? (list-ref match 2) "put")
                 (decl-fail "syntax" (row-text row) "A delete has no literal value" (row-span row)))
               (list
                (with-handlers
                 ((class-error?
                   (lambda (error)
                     (decl-fail "syntax" (row-text row) (exn-message error) (row-span row)))))
                 (decode-quoted-literal literal-source))))
             '()))
         (for
          ((literal (in-list literals)))
          (unless (class-accepts? (hash-ref classes "value") literal)
            (decl-fail
             "syntax"
             (row-text row)
             "A write literal requires allowed code points within the length cap"
             (row-span row))))
         (unless (<=
                  (if (equal? (list-ref match 2) "delete") 0 1)
                  width
                  (length (hash-ref descriptor 'signature)))
           (decl-fail
            "arity"
            (row-text row)
            "The write width exceeds its signature"
            (row-span row)))
         (set! writes
           (hash-set
            writes
            name
            (model-write
             name
             (list-ref match 1)
             (string->symbol (list-ref match 2))
             width
             (row-span row)
             literals)))
         (set! write-rows (hash-set write-rows name row)))
        ((reads)
         (define name (capture-value row "name"))
         (constructor! name row #:primitive? #t)
         (define match (regexp-match #px"^(.+) *= *(skip|reject)$" (capture-value row "value")))
         (unless match
           (decl-fail "syntax" (row-text row) "The read clause is malformed" (row-span row)))
         (define prior (hash-ref reads name '()))
         (define expression (fact-expression row (string-trim (cadr match))))
         (when (ormap
                (lambda (read)
                  (equal?
                   (expression-identity (model-read-fact read))
                   (expression-identity expression)))
                prior)
           (decl-fail
            "duplicate"
            (row-text row)
            "The primitive reads this fact twice"
            (row-span row)))
         (set! reads
           (hash-set
            reads
            name
            (append
             prior
             (list (model-read name expression (string->symbol (caddr match)) (row-span row)))))))
        ((ensure)
         (define name (capture-value row "constructor"))
         (constructor! name row)
         (define label (capture-value row "name"))
         (define prior (hash-ref ensures name '()))
         (when (ormap (lambda (clause) (equal? (model-ensure-name clause) label)) prior)
           (decl-fail "duplicate" label "The invariant is declared twice" (row-span row)))
         (set! ensures
           (hash-set
            ensures
            name
            (append
             prior
             (list
              (model-ensure
               name
               label
               (parse-term (capture-value row "value") #:span (row-span row))
               (row-span row)))))))
        ((join)
         (define name (capture-value row "name"))
         (unless (failed? 'join name)
           (define triple (quoted-pair (capture-value row "value") classes (row-span row)))
           (define left (car triple))
           (define right (cadr triple))
           (define result (caddr triple))
           (define table (hash-ref joins name (hash)))
           (define key (cons left right))
           (unless (hash-has-key? join-spans name)
             (set! join-spans (hash-set join-spans name (row-span row))))
           (set! join-entries
             (hash-update join-entries name (lambda (prior) (append prior (list triple))) '()))
           (cond
            ((equal? left right)
             (unless (equal? result left)
               (join-fail name "idempotent" left (hash-ref join-spans name)))
             (set! joins (hash-set joins name table)))
            (else
             (when (and (hash-has-key? table key) (not (equal? (hash-ref table key) result)))
               (join-fail name "once" (list left right) (row-span row)))
             (set! joins
               (hash-set joins name (hash-set* table key result (cons right left) result)))))))
        ((effect effect-state)
         (define name (capture-value row "name"))
         (constructor! name row #:primitive? #t)
         (when (or (hash-has-key? external name) (hash-has-key? states name))
           (decl-fail "duplicate" name "The primitive effect is declared twice" (row-span row)))
         (if (eq? (hash-ref row 'kind) 'effect-state)
           (set! states (hash-set states name #t))
           (set! external (hash-set external name #t)))
         (set! external-spans (hash-set external-spans name (row-span row))))
        ((stage)
         (define name (capture-value row "name"))
         (constructor! name row #:primitive? #t)
         (duplicate! stages name row)
         (set! stages (hash-set stages name (string->number (capture-value row "number")))))
        ((guard)
         (define name (capture-value row "name"))
         (constructor! name row)
         (duplicate! guards name row)
         (set! guards (hash-set guards name (fact-expression row (capture-value row "value")))))
        ((scope)
         (define name (capture-value row "name"))
         (constructor! name row)
         (duplicate! scopes name row)
         (set! scopes (hash-set scopes name (capture-value row "scope-mode"))))
        ((fuel)
         (define name (capture-value row "name"))
         (constructor! name row)
         (duplicate! fuels name row)
         (define amount (string->number (capture-value row "number")))
         (unless (positive? amount)
           (decl-fail "syntax" (row-text row) "A fuel cap must be positive" (row-span row)))
         (set! fuels (hash-set fuels name amount)))
        (else (void))))
    row))
  (for
   ((name
     (in-list
      (sort
       (hash-keys constructors)
       <
       #:key
       (lambda (name) (hash-ref (hash-ref constructors name) 'ordinal)))))
    #:when
    (hash-ref (hash-ref constructors name) 'primitive?))
   (attempt
    (lambda ()
      (unless (or
               (hash-has-key? writes name)
               (hash-has-key? external name)
               (failed? 'writes name)
               (failed? 'effect name))
        (define declaring-row
          (for/first
           ((row (in-list rows))
            #:when
            (and (memq (hash-ref row 'kind) '(primitive primitive-zero))
                 (equal? (capture-value row "name") name)))
           row))
        (decl-fail
         "effect"
         name
         "A primitive requires writes or an explicit external effect"
         (if declaring-row (row-span declaring-row) span))))))
  (for
   (((name clauses) (in-hash reads)))
   (for
    ((clause (in-list clauses)))
    (attempt
     (lambda ()
       (when (and
              (string? (model-read-fact clause))
              (not (hash-has-key? writes name))
              (not (failed? 'writes name)))
         (decl-fail
          "effect"
          name
          "A bare read requires a declared write key"
          (model-read-span clause)))))))
  (define key-widths (hash))
  (define key-classes (hash))
  (for
   ((write
     (in-list
      (sort
       (hash-values writes)
       <
       #:key
       (lambda (item) (src-span-start-byte (model-write-span item))))))
    #:when
    (eq? (model-write-operation write) 'put))
   (attempt
    (lambda ()
      (define cell-name (model-write-cell write))
      (define signature
        (take
         (hash-ref (hash-ref constructors (model-write-constructor write)) 'signature)
         (model-write-width write)))
      (when (and
             (hash-has-key? key-widths cell-name)
             (not (= (hash-ref key-widths cell-name) (model-write-width write))))
        (decl-fail
         "conflict"
         cell-name
         "A cell's put writers disagree on key width"
         (model-write-span write)))
      (when (and
             (hash-has-key? key-classes cell-name)
             (not (equal? (hash-ref key-classes cell-name) signature)))
        (decl-fail
         "conflict"
         cell-name
         "A cell's put writers disagree on key class names"
         (model-write-span write)))
      (set! key-classes (hash-set key-classes cell-name signature))
      (set! key-widths (hash-set key-widths cell-name (model-write-width write))))))
  (for
   (((name write) (in-hash writes)))
   (attempt
    (lambda ()
      (let/ec
       skip-dependent
       (unless (hash-has-key? cells (model-write-cell write))
         (when (failed? 'cell (model-write-cell write)) (skip-dependent #f))
         (decl-fail
          "reference"
          (model-write-cell write)
          "The write names an undeclared cell"
          (model-write-span write)))
       (when (hash-has-key? external name)
         (decl-fail
          "conflict"
          name
          "An external primitive also declares a modelled write"
          (if (>
               (src-span-start-byte (hash-ref external-spans name))
               (src-span-start-byte (model-write-span write)))
            (hash-ref external-spans name)
            (model-write-span write))))
       (define cell (hash-ref cells (model-write-cell write)))
       (when (and
              (model-cell-stacking? cell)
              (eq? (model-write-operation write) 'delete)
              (< (model-write-width write) (length template-names)))
         (decl-fail
          "partition"
          (row-text (hash-ref write-rows name))
          "A delete prefix must include every holding file-template partition"
          (model-write-span write)))
       (when (and
              (eq? (model-write-operation write) 'delete)
              (hash-has-key? key-classes (model-write-cell write))
              (<=
               (model-write-width write)
               (length (hash-ref key-classes (model-write-cell write)))))
         (define actual
           (take (hash-ref (hash-ref constructors name) 'signature) (model-write-width write)))
         (unless (equal?
                  actual
                  (take (hash-ref key-classes (model-write-cell write)) (model-write-width write)))
           (decl-fail
            "conflict"
            name
            "A delete prefix disagrees on key class names"
            (model-write-span write))))
       (when (and
              (eq? (model-write-operation write) 'delete)
              (hash-has-key? key-widths (model-write-cell write))
              (> (model-write-width write) (hash-ref key-widths (model-write-cell write))))
         (decl-fail
          "conflict"
          name
          "A delete prefix is wider than its cell key"
          (model-write-span write)))
       (when (model-cell-stacking? cell)
         (define width
           (if (eq? (model-write-operation write) 'put)
             (model-write-width write)
             (hash-ref key-widths (model-write-cell write) #f)))
         (when width (check-stacking-key! cell write width classes constructors)))
       (when (and (eq? (model-cell-law cell) 'join) (eq? (model-write-operation write) 'put))
         (define signature (hash-ref (hash-ref constructors name) 'signature))
         (define remaining (- (length signature) (model-write-width write)))
         (unless (= (+ remaining (length (model-write-literals write))) 1)
           (decl-fail
            "arity"
            name
            "A join cell has one literal value argument"
            (model-write-span write)))
         (define domain
           (if (zero? remaining)
             (model-write-literals write)
             (and (hash-has-key? classes (list-ref signature (model-write-width write)))
                  (literal-domain
                   (list-ref signature (model-write-width write))
                   classes
                   (model-write-span write)))))
         (when domain
           (define normalized (remove-duplicates domain))
           (when (and
                  (hash-has-key? join-domains (model-cell-name cell))
                  (not (equal? (hash-ref join-domains (model-cell-name cell)) normalized)))
             (decl-fail
              "conflict"
              (model-cell-name cell)
              "A cell's writers disagree on its value class"
              (model-write-span write)))
           (set! join-domains (hash-set join-domains (model-cell-name cell) normalized))))))))
  (for
   ((cell
     (in-list
      (sort
       (hash-values cells)
       <
       #:key
       (lambda (cell) (src-span-start-byte (model-cell-span cell))))))
    #:when
    (eq? (model-cell-law cell) 'join))
   (define table-name (model-cell-table cell))
   (hash-set! used-tables table-name #t)
   (define domain (hash-ref join-domains (model-cell-name cell) #f))
   (when domain
     (attempt
      (lambda ()
        (when (and
               (hash-has-key? table-domains table-name)
               (not (equal? (hash-ref table-domains table-name) domain)))
          (decl-fail
           "conflict"
           (model-cell-name cell)
           "Cells sharing a table require one value class"
           (model-cell-span cell)))
        (set! table-domains (hash-set table-domains table-name domain)))))
   (unless (or (hash-has-key? joins table-name) (failed? 'join table-name))
     (attempt
      (lambda ()
        (decl-fail
         "reference"
         table-name
         "The cell names an undeclared join table"
         (model-cell-span cell))))))
  (for
   (((name table) (in-hash joins)) #:unless (failed? 'join name))
   (attempt
    (lambda ()
      (define domain
        (hash-ref
         table-domains
         name
         (lambda () (remove-duplicates (append-map values (hash-ref join-entries name '()))))))
      (when (hash-has-key? table-domains name)
        (for*
         ((triple (in-list (hash-ref join-entries name '()))) (literal (in-list triple)))
         (unless (member literal domain)
           (join-fail name "closed" literal (hash-ref join-spans name)))))
      (set! joins
        (hash-set
         joins
         name
         (validate-join
          name
          table
          domain
          (hash-ref join-spans name)
          #:complete?
          (hash-ref used-tables name #f)))))))
  (compiled-model
   cells
   writes
   reads
   ensures
   joins
   external
   stages
   guards
   scopes
   fuels
   constructors
   classes
   span
   states))
