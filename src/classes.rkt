#lang racket/base

(require racket/list racket/match racket/set racket/string "automata-minimize.rkt")

(provide compile-classes
         class-accepts?
         class-pattern-accepts?
         class-empty?
         class->description
         class->references
         class-witnesses
         class-overlap-witness
         class-difference-witness
         class-sequence
         class-intersection
         class-without-literal
         class-trimmed
         class-contains-literal-witness
         class-delimiter-witness
         class->gbnf
         class->gbnf-rules
         class-work-bound
         class-check-bound
         class-count-bounds
         class-check-count-bounds
         class-host-guards
         class-error-message
         builtin-class-names
         default-class-caps
         unicode-version
         unicode-source
         unicode-source-sha256
         (struct-out class-error)
         (struct-out compiled-class)
         (struct-out automaton)
         (struct-out edge))

(struct class-error exn:fail (code data) #:transparent)

(struct edge (ranges target) #:transparent)

(struct automaton (start accepting transitions) #:transparent)

(struct compiled-class (name source machine caps alphabet) #:transparent)

(struct boundary-choice (distance point target))

(define class-error-message exn-message)

(define builtin-class-names '("letter" "canonical-path" "relative-path"))

(define default-class-caps
  (hash 'states 4096 'repetition 256 'reference-depth 8 'token-length 4096 'allow-ascii-space #t))

(define automaton-work-limit 1048576)

(define class-host-guards
  (hash "temporary-states" automaton-work-limit "reference-nodes" automaton-work-limit))

(define bounded-machines (make-weak-hasheq))

(define bounded-class-cache (make-weak-hasheq))

(define official-machines (make-weak-hasheq))

(define count-bounds-cache (make-weak-hasheq))

(define class-original-trees (make-weak-hasheq))

(define (register-machine! machine)
  (when (and
         (immutable? (automaton-accepting machine))
         (immutable? (automaton-transitions machine)))
    (hash-set! official-machines machine #t))
  machine)

(define (bounded? machine) (hash-ref bounded-machines machine #f))

(define (mark-bounded! machine) (hash-set! bounded-machines machine #t) machine)

(define (work-fail phase actual (bounds #f) #:limit (limit automaton-work-limit))
  (define resource (if (equal? phase "references") "reference-nodes" "temporary-states"))
  (fail
   "class-work-cap"
   (if (equal? resource "reference-nodes")
     "expanded class reference nodes exceed the construction budget"
     "temporary automaton state count exceeds the construction or search budget")
   (if bounds
     (hash 'limit limit 'actual actual 'phase phase 'resource resource 'bounds bounds)
     (hash 'limit limit 'actual actual 'phase phase 'resource resource))))

(define (class-work-bound class)
  (define machine (compiled-class-machine class))
  (define controls (vector-length (automaton-transitions machine)))
  (if (bounded? machine)
    controls
    (* controls (add1 (caps-ref (compiled-class-caps class) 'token-length)))))

(define (class-check-bound left (right #f) #:literal (literal #f))
  (cond
   (literal (* (class-work-bound left) (add1 (string-length literal))))
   (right (* (class-work-bound left) (add1 (class-work-bound right))))
   (else (class-work-bound left))))

(define (class-count-bounds class)
  (define machine (compiled-class-machine class))
  (if (and (hash-ref official-machines machine #f)
           (immutable? (automaton-accepting machine))
           (immutable? (automaton-transitions machine))
           (immutable? (compiled-class-caps class)))
    (hash-ref! count-bounds-cache class (lambda () (uncached-class-count-bounds class)))
    (uncached-class-count-bounds class)))

(define (uncached-class-count-bounds class)
  (define machine (compiled-class-machine class))
  (define rows (automaton-transitions machine))
  (define embedded? (bounded? machine))
  (define length-limit (caps-ref (compiled-class-caps class) 'token-length))
  (define edge-groups (for/sum ((row (in-vector rows))) (length row)))
  (define interval-pieces
    (for*/sum
     ((row (in-vector rows)) (transition (in-list row)))
     (length (edge-ranges transition))))
  (hash
   'representation
   (if embedded? "length-bounded" "compact-pattern")
   'stored-states
   (vector-length rows)
   'pattern-control-states
   (if embedded? 'null (vector-length rows))
   'length-limit
   length-limit
   'length-counter-positions
   (add1 length-limit)
   'bounded-states-upper-bound
   (class-work-bound class)
   'bounded-edge-groups-upper-bound
   (if embedded? edge-groups (* length-limit edge-groups))
   'bounded-interval-pieces-upper-bound
   (if embedded? interval-pieces (* length-limit interval-pieces))
   'temporary-states-limit
   automaton-work-limit))

(define (class-check-count-bounds left (right #f) #:literal (literal #f))
  (define base
    (hash
     'kind
     (cond (literal "literal-prefix-product") (right "class-product") (else "bounded-view"))
     'states-upper-bound
     (class-check-bound left right #:literal literal)
     'left
     (class-count-bounds left)))
  (cond
   (literal (hash-set base 'literal-code-points (string-length literal)))
   (right (hash-set base 'right (class-count-bounds right)))
   (else base)))

(define (with-count-bounds bounds thunk)
  (with-handlers
   ((class-error?
     (lambda (failure)
       (if (equal? (class-error-code failure) "class-work-cap")
         (raise
          (struct-copy
           class-error
           failure
           (data (hash-set (class-error-data failure) 'bounds bounds))))
         (raise failure)))))
   (thunk)))

(define (bounded-class-machine class)
  (define machine (compiled-class-machine class))
  (if (bounded? machine)
    machine
    (hash-ref!
     bounded-class-cache
     class
     (lambda ()
       (with-count-bounds
        (class-count-bounds class)
        (lambda () (bound-machine machine (compiled-class-caps class))))))))

(define (minimize-machine machine)
  (define-values
   (start accepting transitions)
   (minimize-dfa
    (automaton-start machine)
    (automaton-accepting machine)
    (for/vector
     ((row (in-vector (automaton-transitions machine))))
     (for/list
      ((transition (in-list row)))
      (cons (edge-ranges transition) (edge-target transition))))))
  (register-machine!
   (automaton
    start
    accepting
    (vector->immutable-vector
     (for/vector
      ((row (in-vector transitions)))
      (for/list ((transition (in-list row))) (edge (car transition) (cdr transition))))))))

(define scalar-intervals '((0 . 55295) (57344 . 1114111)))

(define (fail code message (data (hash)))
  (raise (class-error message (current-continuation-marks) code data)))

(define (normalize-intervals ranges)
  (reverse
   (for/fold
    ((result '()))
    ((range (in-list (sort ranges < #:key car))))
    (cond
     ((> (car range) (cdr range)) result)
     ((and (pair? result) (<= (car range) (add1 (cdar result))))
      (cons (cons (caar result) (max (cdar result) (cdr range))) (cdr result)))
     (else (cons range result))))))

(define (intersect-intervals left right)
  (let loop ((left left) (right right) (result '()))
    (cond
     ((or (null? left) (null? right)) (reverse result))
     (else
      (define lower (max (caar left) (caar right)))
      (define upper (min (cdar left) (cdar right)))
      (define next (if (<= lower upper) (cons (cons lower upper) result) result))
      (cond
       ((< (cdar left) (cdar right)) (loop (cdr left) right next))
       (else (loop left (cdr right) next)))))))

(define (subtract-intervals left right)
  (for/fold
   ((result left))
   ((cut (in-list right)))
   (append-map
    (lambda (range)
      (cond
       ((or (< (cdr range) (car cut)) (> (car range) (cdr cut))) (list range))
       (else
        (append
         (if (< (car range) (car cut)) (list (cons (car range) (sub1 (car cut)))) '())
         (if (> (cdr range) (cdr cut)) (list (cons (add1 (cdr cut)) (cdr range))) '())))))
    result)))

(define (interval-member? ranges point)
  (for/or ((range (in-list ranges))) (<= (car range) point (cdr range))))

(define (caps-ref caps key) (hash-ref caps key (lambda () (hash-ref default-class-caps key))))

(define (normalize-caps raw)
  (unless (hash? raw) (fail "class-caps" "class caps must be a map"))
  (define aliases
    (hash
     'max-states
     'states
     'max-repetition
     'repetition
     'depth
     'reference-depth
     'max-reference-depth
     'reference-depth
     'length
     'token-length
     'max-token-length
     'token-length))
  (define caps
    (for/fold
     ((result default-class-caps))
     (((key value) (in-hash raw)))
     (define name (if (string? key) (string->symbol key) key))
     (define canonical (hash-ref aliases name name))
     (unless (hash-has-key? default-class-caps canonical)
       (fail "class-caps" "unknown class cap" (hash 'cap key)))
     (unless (if
              (eq? canonical 'allow-ascii-space)
              (boolean? value)
              (exact-positive-integer? value))
       (fail "class-caps" "invalid class cap" (hash 'cap key 'value value)))
     (hash-set result canonical value)))
  caps)

(define (safe-alphabet caps)
  (subtract-intervals
   scalar-intervals
   (if (caps-ref caps 'allow-ascii-space)
     (subtract-intervals unicode-unsafe-intervals '((32 . 32)))
     unicode-unsafe-intervals)))

(define (scalar? value)
  (and (exact-nonnegative-integer? value) (<= value 1114111) (not (<= 55296 value 57343))))

(define (rx-sequence parts)
  (cond ((null? parts) 'epsilon) ((null? (cdr parts)) (car parts)) (else (list 'sequence parts))))

(define (rx-alternation parts)
  (cond ((null? parts) 'empty) ((null? (cdr parts)) (car parts)) (else (list 'alternation parts))))

(define (parse-pattern source known caps #:pattern? (pattern? #f))
  (define at 0)
  (define size (string-length source))
  (define (syntax-error message)
    (fail "class-syntax" message (hash 'source source 'column (add1 at))))
  (define (peek) (and (< at size) (string-ref source at)))
  (define (take!) (define value (peek)) (set! at (add1 at)) value)
  (define (blank? value) (and value (memv value '(#\space #\tab #\newline #\return))))
  (define (skip!) (let loop () (when (blank? (peek)) (take!) (loop))))
  (define (except?)
    (and (not pattern?)
         (<= (+ at 6) size)
         (string=? (substring source at (+ at 6)) "except")
         (or (= (+ at 6) size)
             (blank? (string-ref source (+ at 6)))
             (char=? (string-ref source (+ at 6)) #\"))))
  (define (hex! width)
    (unless (<= (+ at width) size) (syntax-error "incomplete code-point escape"))
    (unless (for/and
             ((point (in-string source at (+ at width))))
             (or (char<=? #\0 point #\9) (char<=? #\a point #\f) (char<=? #\A point #\F)))
      (syntax-error "invalid hexadecimal escape"))
    (define value (string->number (substring source at (+ at width)) 16))
    (unless (and value (scalar? value)) (syntax-error "invalid Unicode scalar escape"))
    (set! at (+ at width))
    value)
  (define (braced-hex!)
    (take!)
    (define begin at)
    (let loop ()
      (when (and (peek) (not (char=? (peek) #\})))
        (unless (or (char<=? #\0 (peek) #\9) (char<=? #\a (peek) #\f) (char<=? #\A (peek) #\F))
          (syntax-error "invalid braced code-point escape"))
        (take!)
        (loop)))
    (unless (and (> at begin) (peek)) (syntax-error "unclosed or empty code-point escape"))
    (define value (string->number (substring source begin at) 16))
    (unless (and value (scalar? value)) (syntax-error "invalid Unicode scalar escape"))
    (take!)
    value)
  (define (escaped!)
    (unless (peek) (syntax-error "incomplete escape"))
    (define value (take!))
    (case value
      ((#\u) (if (and (peek) (char=? (peek) #\{)) (braced-hex!) (hex! 4)))
      ((#\U) (hex! 8))
      ((#\x) (hex! 2))
      ((#\n) 10)
      ((#\r) 13)
      ((#\t) 9)
      ((#\b) 8)
      ((#\f) 12)
      ((#\d) '((48 . 57)))
      ((#\w) '((48 . 57) (65 . 90) (95 . 95) (97 . 122)))
      ((#\s) '((9 . 13) (32 . 32)))
      (else
       (unless (memv
                value
                '(#\\ #\" #\/ #\[ #\] #\( #\) #\{ #\} #\^ #\- #\. #\? #\* #\+ #\| #\< #\> #\space))
         (syntax-error "unsupported escape"))
       (char->integer value))))
  (define (quoted!)
    (take!)
    (let loop ((result '()))
      (unless (peek) (syntax-error "unclosed quoted literal"))
      (define value (take!))
      (cond
       ((char=? value #\") (list 'text (list->string (reverse result))))
       ((char=? value #\\)
        (define point (escaped!))
        (unless (exact-integer? point)
          (syntax-error "a quoted literal requires a single code point"))
        (loop (cons (integer->char point) result)))
       (else (loop (cons value result))))))
  (define (range-point!)
    (unless (peek) (syntax-error "unclosed range"))
    (define value (take!))
    (if (char=? value #\\) (escaped!) (char->integer value)))
  (define (range!)
    (take!)
    (define negate? (and (peek) (char=? (peek) #\^)))
    (when negate? (take!))
    (define ranges
      (let loop ((result '()))
        (unless (peek) (syntax-error "unclosed range"))
        (cond
         ((char=? (peek) #\]) (take!) (reverse result))
         (else
          (define lower (range-point!))
          (define interval?
            (and (peek)
                 (char=? (peek) #\-)
                 (< (add1 at) size)
                 (not (char=? (string-ref source (add1 at)) #\]))))
          (cond
           (interval?
            (take!)
            (define upper (range-point!))
            (unless (and (exact-integer? lower) (exact-integer? upper))
              (syntax-error "range endpoints must be single code points"))
            (when (> lower upper) (syntax-error "reversed range"))
            (loop (cons (cons lower upper) result)))
           ((exact-integer? lower) (loop (cons (cons lower lower) result)))
           (else (loop (append lower result))))))))
    (when (null? ranges) (syntax-error "empty range"))
    (define normalized (intersect-intervals (normalize-intervals ranges) scalar-intervals))
    (list 'characters (if negate? (subtract-intervals scalar-intervals normalized) normalized)))
  (define (slash!)
    (take!)
    (define begin at)
    (let loop ((escaped? #f))
      (unless (peek) (syntax-error "unclosed slash pattern"))
      (define value (take!))
      (cond
       (escaped? (loop #f))
       ((char=? value #\\) (loop #t))
       ((char=? value #\/)
        (define raw (substring source begin (sub1 at)))
        (list 'pattern raw (parse-pattern raw known caps #:pattern? #t)))
       (else (loop #f)))))
  (define (number!)
    (define begin at)
    (let loop () (when (and (peek) (char<=? #\0 (peek) #\9)) (take!) (loop)))
    (when (= at begin) (syntax-error "expected repetition count"))
    (define result (string->number (substring source begin at)))
    (when (> result (caps-ref caps 'repetition))
      (fail
       "class-repetition-cap"
       "repetition exceeds the declared cap"
       (hash 'count result 'cap (caps-ref caps 'repetition))))
    result)
  (define (atom!)
    (skip!)
    (unless (peek) (syntax-error "expected a class expression"))
    (case (peek)
      ((#\") (quoted!))
      ((#\< #\>) (syntax-error "class references are bare names, not angle placeholders"))
      ((#\[) (unless pattern? (syntax-error "ranges must appear inside slash patterns")) (range!))
      ((#\.)
       (unless pattern? (syntax-error "wildcards must appear inside slash patterns"))
       (take!)
       (list 'characters scalar-intervals))
      ((#\\)
       (unless pattern? (syntax-error "escape a literal inside quotes or a slash pattern"))
       (take!)
       (define point (escaped!))
       (list 'characters (if (exact-integer? point) (list (cons point point)) point)))
      ((#\/) (if pattern? (syntax-error "escape a literal slash") (slash!)))
      ((#\()
       (take!)
       (define result (expression!))
       (skip!)
       (unless (and (peek) (char=? (take!) #\))) (syntax-error "unclosed group"))
       result)
      ((#\? #\* #\+ #\{ #\} #\) #\| #\@ #\^) (syntax-error "unexpected class operator"))
      (else
       (cond
        (pattern? (list 'text (string (take!))))
        (else
         (define begin at)
         (let loop ()
           (when (and
                  (peek)
                  (not
                   (memv
                    (peek)
                    '(#\space
                      #\tab
                      #\newline
                      #\return
                      #\"
                      #\[
                      #\]
                      #\<
                      #\>
                      #\.
                      #\\
                      #\/
                      #\(
                      #\)
                      #\?
                      #\*
                      #\+
                      #\{
                      #\}
                      #\|))))
             (take!)
             (loop)))
         (when (= begin at) (syntax-error "unexpected class token"))
         (define name (substring source begin at))
         (unless (set-member? known name)
           (fail
            "class-reference"
            "undeclared class reference"
            (hash 'class name 'column (add1 begin))))
         (list 'reference name))))))
  (define (repeated!)
    (define result (atom!))
    (skip!)
    (cond
     ((and (peek) (memv (peek) '(#\? #\* #\+)))
      (case (take!)
        ((#\?) (list 'repeat result 0 1))
        ((#\*) (list 'repeat result 0 #f))
        (else (list 'repeat result 1 #f))))
     ((and (peek) (char=? (peek) #\{))
      (take!)
      (define minimum (number!))
      (define maximum (cond ((and (peek) (char=? (peek) #\,)) (take!) (number!)) (else minimum)))
      (unless (and (peek) (char=? (take!) #\})) (syntax-error "unclosed repetition"))
      (when (> minimum maximum) (syntax-error "reversed repetition bounds"))
      (list 'repeat result minimum maximum))
     (else result)))
  (define (sequence!)
    (let loop ((result '()))
      (skip!)
      (if (or (not (peek)) (memv (peek) '(#\) #\|)) (except?))
        (begin
          (when (and (not pattern?) (null? result)) (syntax-error "expected a class expression"))
          (rx-sequence (reverse result)))
        (loop (cons (repeated!) result)))))
  (define (alternation!)
    (let loop ((result (list (sequence!))))
      (skip!)
      (if (and (peek) (char=? (peek) #\|))
        (begin (take!) (loop (cons (sequence!) result)))
        (rx-alternation (reverse result)))))
  (define (expression!)
    (define body (alternation!))
    (skip!)
    (cond
     ((except?)
      (set! at (+ at 6))
      (define literals
        (let loop ((result '()))
          (skip!)
          (unless (and (peek) (char=? (peek) #\"))
            (fail
             "class-except"
             "except accepts quoted literal values only"
             (hash 'source source 'column (add1 at))))
          (define value (cadr (quoted!)))
          (skip!)
          (if (and (peek) (char=? (peek) #\|))
            (begin (take!) (loop (cons value result)))
            (reverse (cons value result)))))
      (list 'except body literals))
     (else body)))
  (define result (expression!))
  (skip!)
  (unless (= at size) (syntax-error "unexpected closing operator"))
  result)

(define (read-class-source source known caps)
  (unless (and (string? source) (positive? (string-length (string-trim source))))
    (fail "class-syntax" "class source must be nonempty text" (hash 'source source)))
  (define trimmed (string-trim source))
  (when (string-prefix? trimmed "@")
    (fail
     "class-host-predicate"
     "host predicates are not part of the class language"
     (hash 'source source)))
  (values (parse-pattern trimmed known caps) '()))

(define (make-nfa tree resolve caps alphabet)
  (define next 0)
  (define epsilon (make-hash))
  (define transitions (make-hash))
  (define (state!)
    (when (>= next automaton-work-limit) (work-fail "nfa" (add1 next)))
    (begin0 next (set! next (add1 next))))
  (define (epsilon! from to) (hash-update! epsilon from (lambda (values) (cons to values)) '()))
  (define (edge! from to ranges)
    (define safe (intersect-intervals ranges alphabet))
    (unless (null? safe)
      (hash-update! transitions from (lambda (values) (cons (edge safe to) values)) '())))
  (define (build node stack)
    (match
     node
     ('empty (values (state!) (state!)))
     ('epsilon
      (define start (state!))
      (define end (state!))
      (epsilon! start end)
      (values start end))
     ((list 'characters ranges)
      (define start (state!))
      (define end (state!))
      (edge! start end ranges)
      (values start end))
     ((list 'pattern _ child) (build child stack))
     ((list 'text text)
      (build
       (rx-sequence
        (for/list
         ((character (in-string text)))
         (list 'characters (list (cons (char->integer character) (char->integer character))))))
       stack))
     ((list 'reference name)
      (when (member name stack)
        (fail
         "class-reference-cycle"
         "class references must be acyclic"
         (hash 'class name 'chain (reverse (cons name stack)))))
      (when (>= (length stack) (caps-ref caps 'reference-depth))
        (fail
         "class-reference-cap"
         "class reference depth exceeds the declared cap"
         (hash 'class name 'cap (caps-ref caps 'reference-depth) 'actual (add1 (length stack)))))
      (build (resolve name) (cons name stack)))
     ((list 'except child literals)
      (define-values
       (inner-start inner-end inner-epsilon inner-transitions)
       (make-nfa child resolve caps alphabet))
      (define inner
        (exclude-literals
         (determinize inner-start inner-end inner-epsilon inner-transitions caps)
         literals
         caps))
      (define copies
        (build-vector (vector-length (automaton-transitions inner)) (lambda (_) (state!))))
      (define end (state!))
      (for
       ((index (in-range (vector-length copies))))
       (when (vector-ref (automaton-accepting inner) index)
         (epsilon! (vector-ref copies index) end))
       (for
        ((transition (in-list (vector-ref (automaton-transitions inner) index))))
        (edge!
         (vector-ref copies index)
         (vector-ref copies (edge-target transition))
         (edge-ranges transition))))
      (values (vector-ref copies (automaton-start inner)) end))
     ((list 'sequence children)
      (define start (state!))
      (define end
        (for/fold
         ((last start))
         ((child (in-list children)))
         (define-values (first final) (build child stack))
         (epsilon! last first)
         final))
      (values start end))
     ((list 'alternation children)
      (define start (state!))
      (define end (state!))
      (for
       ((child (in-list children)))
       (define-values (first final) (build child stack))
       (epsilon! start first)
       (epsilon! final end))
      (values start end))
     ((list 'repeat child minimum maximum)
      (define start (state!))
      (define last
        (for/fold
         ((last start))
         ((_ (in-range minimum)))
         (define-values (first final) (build child stack))
         (epsilon! last first)
         final))
      (define end (state!))
      (cond
       (maximum
        (define final
          (for/fold
           ((last last))
           ((_ (in-range (- maximum minimum))))
           (define-values (first final) (build child stack))
           (epsilon! last end)
           (epsilon! last first)
           final))
        (epsilon! final end))
       (else
        (define-values (first final) (build child stack))
        (epsilon! last end)
        (epsilon! last first)
        (epsilon! final first)
        (epsilon! final end)))
      (values start end))))
  (define-values (start end) (build tree '()))
  (values start end epsilon transitions))

(define (epsilon-closure states epsilon)
  (let loop ((pending states) (seen (set)))
    (cond
     ((null? pending) (sort (set->list seen) <))
     ((set-member? seen (car pending)) (loop (cdr pending) seen))
     (else
      (loop
       (append (hash-ref epsilon (car pending) '()) (cdr pending))
       (set-add seen (car pending)))))))

(define (group-edges transitions)
  (define grouped (make-hash))
  (for
   ((transition (in-list transitions)) #:unless (null? (edge-ranges transition)))
   (hash-update!
    grouped
    (edge-target transition)
    (lambda (labels) (cons (edge-ranges transition) labels))
    '()))
  (sort
   (for/list
    (((target labels) (in-hash grouped)))
    (edge
     (if (null? (cdr labels)) (car labels) (normalize-intervals (append-map values labels)))
     target))
   <
   #:key
   (lambda (transition) (caar (edge-ranges transition)))))

(define (disjoint-outgoing options close)
  (define disjoint?
    (let loop ((remaining options))
      (or (null? remaining)
          (and (for/and
                ((other (in-list (cdr remaining))))
                (null? (intersect-intervals (edge-ranges (car remaining)) (edge-ranges other))))
               (loop (cdr remaining))))))
  (and disjoint?
       (for/list
        ((transition (in-list options)))
        (edge (edge-ranges transition) (close (list (edge-target transition)))))))

(define (build-machine start-key accepting? outgoing caps)
  (define identities (make-hash))
  (define acceptance (make-hash))
  (define transitions (make-hash))
  (define pending '())
  (define next 0)
  (define (intern! key)
    (hash-ref
     identities
     key
     (lambda ()
       (when (>= next automaton-work-limit) (work-fail "dfa" (add1 next)))
       (define identity next)
       (set! next (add1 next))
       (hash-set! identities key identity)
       (set! pending (cons key pending))
       identity)))
  (define start (intern! start-key))
  (let loop ((front '()))
    (cond
     ((and (null? front) (null? pending)) (void))
     ((null? front) (define ready (reverse pending)) (set! pending '()) (loop ready))
     (else
      (define key (car front))
      (define identity (hash-ref identities key))
      (hash-set! acceptance identity (accepting? key))
      (hash-set!
       transitions
       identity
       (group-edges
        (for/list
         ((transition (in-list (outgoing key))))
         (edge (edge-ranges transition) (intern! (edge-target transition))))))
      (loop (cdr front)))))
  (register-machine!
   (automaton
    start
    (vector->immutable-vector (build-vector next (lambda (index) (hash-ref acceptance index))))
    (vector->immutable-vector (build-vector next (lambda (index) (hash-ref transitions index)))))))

(define (determinize start end epsilon transitions caps)
  (define closures (make-hash))
  (define (close states)
    (define key (sort (remove-duplicates states) <))
    (hash-ref! closures key (lambda () (epsilon-closure key epsilon))))
  (define (outgoing states)
    (define options (append-map (lambda (state) (hash-ref transitions state '())) states))
    (define direct (disjoint-outgoing options close))
    (if direct
      direct
      (let ()
        (define events (make-hash))
        (for
         ((transition (in-list options)))
         (for
          ((range (in-list (edge-ranges transition))))
          (hash-update!
           events
           (car range)
           (lambda (changes) (cons (cons (edge-target transition) 1) changes))
           '())
          (hash-update!
           events
           (add1 (cdr range))
           (lambda (changes) (cons (cons (edge-target transition) -1) changes))
           '())))
        (define boundaries (sort (hash-keys events) <))
        (define active (make-hash))
        (if (null? boundaries)
          '()
          (filter-map
           (lambda (lower upper)
             (for
              ((change (in-list (hash-ref events lower))))
              (define count (+ (hash-ref active (car change) 0) (cdr change)))
              (if (zero? count)
                (hash-remove! active (car change))
                (hash-set! active (car change) count)))
             (define targets (hash-keys active))
             (and (pair? targets) (edge (list (cons lower (sub1 upper))) (close targets))))
           (drop-right boundaries 1)
           (cdr boundaries))))))
  (build-machine (close (list start)) (lambda (states) (and (member end states) #t)) outgoing caps))

(define (machine-step machine state point)
  (and state
       (for/first
        ((transition (in-list (vector-ref (automaton-transitions machine) state)))
         #:when
         (interval-member? (edge-ranges transition) point))
        (edge-target transition))))

(define (exclude-literals machine exclusions caps)
  (if (null? exclusions)
    machine
    (let ()
      (define nodes (make-hash))
      (define next 1)
      (hash-set! nodes 0 (make-hash))
      (for
       ((text (in-list exclusions)))
       (define last
         (for/fold
          ((state 0))
          ((character (in-string text)))
          (define point (char->integer character))
          (hash-ref
           (hash-ref nodes state)
           point
           (lambda ()
             (when (>= next automaton-work-limit) (work-fail "exclusion-trie" (add1 next)))
             (define identity next)
             (set! next (add1 next))
             (hash-set! nodes identity (make-hash))
             (hash-set! (hash-ref nodes state) point identity)
             identity))))
       (hash-set! (hash-ref nodes last) 'terminal #t))
      (define (outgoing key)
        (define current (car key))
        (define trie (and (cdr key) (hash-ref nodes (cdr key))))
        (define points (if trie (filter exact-nonnegative-integer? (hash-keys trie)) '()))
        (append-map
         (lambda (transition)
           (append-map
            (lambda (range)
              (define boundaries
                (sort
                 (remove-duplicates
                  (append
                   (list (car range) (add1 (cdr range)))
                   (append-map
                    (lambda (point)
                      (if (<= (car range) point (cdr range)) (list point (add1 point)) '()))
                    points)))
                 <))
              (for/list
               ((lower (in-list boundaries)) (upper (in-list (cdr boundaries))))
               (edge
                (list (cons lower (sub1 upper)))
                (cons (edge-target transition) (and trie (hash-ref trie lower #f))))))
            (edge-ranges transition)))
         (vector-ref (automaton-transitions machine) current)))
      (build-machine
       (cons (automaton-start machine) 0)
       (lambda (key)
         (and (vector-ref (automaton-accepting machine) (car key))
              (not (and (cdr key) (hash-ref (hash-ref nodes (cdr key)) 'terminal #f)))))
       outgoing
       caps))))

(define (bound-machine machine caps)
  (define limit (caps-ref caps 'token-length))
  (mark-bounded!
   (build-machine
    (cons (automaton-start machine) 0)
    (lambda (key) (vector-ref (automaton-accepting machine) (car key)))
    (lambda (key)
      (if (= (cdr key) limit)
        '()
        (for/list
         ((transition (in-list (vector-ref (automaton-transitions machine) (car key)))))
         (edge (edge-ranges transition) (cons (edge-target transition) (add1 (cdr key)))))))
    caps)))

(define (relative-path-pattern)
  (define component
    (list
     'except
     (list 'repeat (list 'characters (subtract-intervals scalar-intervals '((47 . 47)))) 1 #f)
     '("." "..")))
  (rx-sequence
   (list component (list 'repeat (rx-sequence (list (list 'text "/") component)) 0 #f))))

(define (canonical-path-pattern)
  (rx-alternation
   (list (list 'text "/") (rx-sequence (list (list 'text "/") (relative-path-pattern))))))

(define (compile-classes
         sources
         #:caps
         (raw-caps (hash))
         #:order
         (source-order #f)
         #:on-error
         (on-error #f))
  (unless (hash? sources) (fail "class-source" "classes must be a map"))
  (unless (or (not on-error) (and (procedure? on-error) (procedure-arity-includes? on-error 1)))
    (fail "class-source" "the class error callback accepts one error"))
  (unless (or
           (not source-order)
           (and (list? source-order)
                (andmap (lambda (name) (or (string? name) (symbol? name))) source-order)))
    (fail "class-source" "class order must be a list of names"))
  (define caps (normalize-caps raw-caps))
  (define alphabet (safe-alphabet caps))
  (define raw
    (for/hash
     (((name source) (in-hash sources)))
     (unless (or (string? name) (symbol? name)) (fail "class-name" "class name must be text"))
     (values (if (symbol? name) (symbol->string name) name) source)))
  (define failed (make-hash))
  (define (attempt name thunk)
    (with-handlers
     ((class-error?
       (lambda (error)
         (define detailed
           (struct-copy
            class-error
            error
            (data
             (hash-set*
              (class-error-data error)
              'definition-name
              name
              'definition-source
              (hash-ref raw name name)))))
         (if on-error (begin (hash-set! failed name #t) (on-error detailed) #f) (raise detailed)))))
     (thunk)))
  (for
   ((name (in-list builtin-class-names)))
   (when (hash-has-key? raw name)
     (attempt
      name
      (lambda ()
        (fail "class-reserved" "a built-in class cannot be redeclared" (hash 'class name))))
     (hash-remove! failed name)
     (set! raw (hash-remove raw name))))
  (define ordered
    (remove-duplicates
     (append
      (if source-order
        (filter
         (lambda (name) (hash-has-key? raw name))
         (map (lambda (name) (if (symbol? name) (symbol->string name) name)) source-order))
        '())
      (sort (hash-keys raw) string<?))))
  (define known (list->set (append builtin-class-names (hash-keys raw))))
  (define trees
    (make-hash
     (list
      (cons "letter" (list 'characters unicode-letter-intervals))
      (cons "canonical-path" (canonical-path-pattern))
      (cons "relative-path" (relative-path-pattern)))))
  (define exclusions (make-hash))
  (for
   ((name (in-list ordered)))
   (define source (hash-ref raw name))
   (define parsed
     (attempt
      name
      (lambda ()
        (define-values (tree excluded) (read-class-source source known caps))
        (cons tree excluded))))
   (when parsed (hash-set! trees name (car parsed)) (hash-set! exclusions name (cdr parsed))))
  (define (blocked? node (seen (make-hash)))
    (match
     node
     ((list 'reference name)
      (or (hash-ref failed name #f)
          (and (not (hash-has-key? seen name))
               (begin (hash-set! seen name #t) (blocked? (hash-ref trees name) seen)))))
     ((list (or 'sequence 'alternation) children)
      (ormap (lambda (child) (blocked? child seen)) children))
     ((list 'repeat child _ _) (blocked? child seen))
     ((list 'except child _) (blocked? child seen))
     ((list 'pattern _ child) (blocked? child seen))
     (_ #f)))
  (define (resolve name)
    (hash-ref
     trees
     name
     (lambda () (fail "class-reference" "undeclared class reference" (hash 'class name)))))
  (define expanded-nodes 0)
  (define (expand node stack)
    (set! expanded-nodes (add1 expanded-nodes))
    (when (> expanded-nodes automaton-work-limit) (work-fail "references" expanded-nodes))
    (match
     node
     ((list 'reference name)
      (when (member name stack)
        (fail
         "class-reference-cycle"
         "class references must be acyclic"
         (hash 'class name 'chain (reverse (cons name stack)))))
      (when (>= (length stack) (caps-ref caps 'reference-depth))
        (fail
         "class-reference-cap"
         "class reference depth exceeds the declared cap"
         (hash 'class name 'cap (caps-ref caps 'reference-depth) 'actual (add1 (length stack)))))
      (define body (expand (resolve name) (cons name stack)))
      (define literals (hash-ref exclusions name '()))
      (if (null? literals) body (list 'except body literals)))
     ((list 'sequence children) (rx-sequence (map (lambda (child) (expand child stack)) children)))
     ((list 'alternation children)
      (rx-alternation (map (lambda (child) (expand child stack)) children)))
     ((list 'repeat child minimum maximum) (list 'repeat (expand child stack) minimum maximum))
     ((list 'except child literals) (list 'except (expand child stack) literals))
     ((list 'pattern source child) (list 'pattern source (expand child stack)))
     (_ node)))
  (for/fold
   ((result (hash)))
   ((name
     (in-list
      (if source-order
        (append
         ordered
         (sort (filter (lambda (name) (not (member name ordered))) (hash-keys trees)) string<?))
        (sort (hash-keys trees) string<?))))
    #:unless
    (or (hash-ref failed name #f) (blocked? (hash-ref trees name))))
   (define class
     (attempt
      name
      (lambda ()
        (set! expanded-nodes 0)
        (define-values
         (start end epsilon transitions)
         (make-nfa (expand (list 'reference name) '()) resolve caps alphabet))
        (define machine (minimize-machine (determinize start end epsilon transitions caps)))
        (define controls (vector-length (automaton-transitions machine)))
        (when (> controls (caps-ref caps 'states))
          (fail
           "class-state-cap"
           "minimal pattern automaton exceeds the declared state cap"
           (hash 'cap (caps-ref caps 'states) 'states controls 'phase "minimal-pattern")))
        (define compiled (compiled-class name (hash-ref raw name name) machine caps alphabet))
        (hash-set!
         class-original-trees
         compiled
         (if (hash-has-key? raw name) (hash-ref trees name) (list 'builtin name)))
        compiled)))
   (if class (hash-set result name class) result)))

(define (original-class-tree class)
  (unless (compiled-class? class)
    (fail "class-description" "a class description requires a compiled class"))
  (hash-ref
   class-original-trees
   class
   (lambda ()
     (fail
      "class-description"
      "a source description is unavailable for this constructed class"
      (hash 'class (compiled-class-name class))))))

(define (class->description class #:qualify (qualify values))
  (unless (and (procedure? qualify) (procedure-arity-includes? qualify 1))
    (fail "class-description" "a class qualifier must accept one reference name"))
  (define (describe node)
    (match
     node
     ('epsilon (hash 'literals (list "")))
     ('empty (hash 'literals '()))
     ((list 'text text) (hash 'literals (list (string->immutable-string text))))
     ((list 'pattern source _) (hash 'pattern (string->immutable-string source)))
     ((list 'reference name)
      (define qualified (qualify (string->immutable-string name)))
      (unless (string? qualified)
        (fail "class-description" "a qualified class reference must be text" (hash 'class name)))
      (hash 'ref (string->immutable-string qualified)))
     ((list 'sequence children) (hash 'seq (map describe children)))
     ((list 'alternation children)
      (define alternatives (map describe children))
      (if (andmap (lambda (value) (hash-has-key? value 'literals)) alternatives)
        (hash 'literals (append-map (lambda (value) (hash-ref value 'literals)) alternatives))
        (hash 'alt alternatives)))
     ((list 'repeat child minimum maximum)
      (hash 'repeat (hash 'node (describe child) 'min minimum 'max (or maximum 'null))))
     ((list 'except child literals)
      (hash
       'except
       (hash 'node (describe child) 'literals (map string->immutable-string literals))))
     ((list 'builtin name) (hash 'builtin (string->immutable-string name)))
     (_ (fail "class-description" "the retained class node has no public description"))))
  (describe (original-class-tree class)))

(define (class->references class)
  (define seen (make-hash))
  (define result '())
  (define (visit node)
    (match
     node
     ((list 'reference name)
      (unless (hash-has-key? seen name)
        (hash-set! seen name #t)
        (set! result (cons (string->immutable-string name) result))))
     ((list (or 'sequence 'alternation) children) (for-each visit children))
     ((list 'repeat child _ _) (visit child))
     ((list 'except child _) (visit child))
     (_ (void))))
  (visit (original-class-tree class))
  (reverse result))

(define (class-accepts? class text)
  (and (string? text)
       (<= (string-length text) (caps-ref (compiled-class-caps class) 'token-length))
       (class-pattern-accepts? class text)))

(define (class-pattern-accepts? class text)
  (and (string? text)
       (let ((machine (compiled-class-machine class)))
         (define final
           (for/fold
            ((state (automaton-start machine)))
            ((character (in-string text)))
            (machine-step machine state (char->integer character))))
         (and final (vector-ref (automaton-accepting machine) final)))))

(define (class-empty? class)
  (define machine (compiled-class-machine class))
  (define limit (caps-ref (compiled-class-caps class) 'token-length))
  (define seen (make-hasheqv))
  (hash-set! seen (automaton-start machine) #t)
  (let loop ((front (list (cons (automaton-start machine) 0))) (back '()))
    (cond
     ((and (null? front) (null? back)) #t)
     ((null? front) (loop (reverse back) '()))
     (else
      (define state (caar front))
      (define length (cdar front))
      (cond
       ((vector-ref (automaton-accepting machine) state) #f)
       (else
        (define next back)
        (when (< length limit)
          (for
           ((transition (in-list (vector-ref (automaton-transitions machine) state))))
           (define target (edge-target transition))
           (unless (hash-has-key? seen target)
             (hash-set! seen target #t)
             (set! next (cons (cons target (add1 length)) next)))))
        (loop (cdr front) next)))))))

(define (search-witness initial accepting? outgoing cap (bounds #f))
  (define seen (make-hash))
  (hash-set! seen initial #t)
  (let loop ((front (list (cons initial '()))) (back '()))
    (cond
     ((and (null? front) (null? back)) #f)
     ((null? front) (loop (reverse back) '()))
     (else
      (define item (car front))
      (define state (car item))
      (define reversed (cdr item))
      (if (accepting? state)
        (list->string (map integer->char (reverse reversed)))
        (let ((next back))
          (for
           ((transition (in-list (outgoing state))))
           (define target (edge-target transition))
           (unless (hash-has-key? seen target)
             (when (>= (hash-count seen) cap)
               (work-fail "product" (add1 (hash-count seen)) bounds #:limit cap))
             (hash-set! seen target #t)
             (set! next (cons (cons target (cons (caar (edge-ranges transition)) reversed)) next))))
          (loop (cdr front) next)))))))

(define (class-overlap-witness left right)
  (define a (bounded-class-machine left))
  (define b (bounded-class-machine right))
  (search-witness
   (cons (automaton-start a) (automaton-start b))
   (lambda (state)
     (and (vector-ref (automaton-accepting a) (car state))
          (vector-ref (automaton-accepting b) (cdr state))))
   (lambda (state)
     (sort
      (append-map
       (lambda (first)
         (filter-map
          (lambda (second)
            (define ranges (intersect-intervals (edge-ranges first) (edge-ranges second)))
            (and (pair? ranges) (edge ranges (cons (edge-target first) (edge-target second)))))
          (vector-ref (automaton-transitions b) (cdr state))))
       (vector-ref (automaton-transitions a) (car state)))
      <
      #:key
      (lambda (transition) (caar (edge-ranges transition)))))
   automaton-work-limit
   (class-check-count-bounds left right)))

(define (complete-outgoing machine state alphabet)
  (define transitions (if state (vector-ref (automaton-transitions machine) state) '()))
  (define covered (normalize-intervals (append-map edge-ranges transitions)))
  (define missing (subtract-intervals alphabet covered))
  (append transitions (if (null? missing) '() (list (edge missing #f)))))

(define (class-difference-witness left right)
  (if (and (compiled-class? left) (eq? left right)) #f (class-difference-search left right)))

(define (class-difference-search left right)
  (define a (bounded-class-machine left))
  (define b (bounded-class-machine right))
  (search-witness
   (cons (automaton-start a) (automaton-start b))
   (lambda (state)
     (and (vector-ref (automaton-accepting a) (car state))
          (not (and (cdr state) (vector-ref (automaton-accepting b) (cdr state))))))
   (lambda (state)
     (sort
      (append-map
       (lambda (first)
         (filter-map
          (lambda (second)
            (define ranges (intersect-intervals (edge-ranges first) (edge-ranges second)))
            (and (pair? ranges) (edge ranges (cons (edge-target first) (edge-target second)))))
          (complete-outgoing b (cdr state) (compiled-class-alphabet left))))
       (vector-ref (automaton-transitions a) (car state)))
      <
      #:key
      (lambda (transition) (caar (edge-ranges transition)))))
   automaton-work-limit
   (class-check-count-bounds left right)))

(define (intersection-caps left right)
  (for/hash
   (((key fallback) (in-hash default-class-caps)))
   (values
    key
    (if (eq? key 'allow-ascii-space)
      (and (caps-ref left key) (caps-ref right key))
      (min (caps-ref left key) (caps-ref right key))))))

(define (product-outgoing left right state (intersection intersect-intervals))
  (append-map
   (lambda (first)
     (filter-map
      (lambda (second)
        (define ranges (intersection (edge-ranges first) (edge-ranges second)))
        (and (pair? ranges) (edge ranges (cons (edge-target first) (edge-target second)))))
      (vector-ref right (cdr state))))
   (vector-ref left (car state))))

(define (class-intersection left right #:name (name "intersection"))
  (define a (compiled-class-machine left))
  (define b (compiled-class-machine right))
  (define caps (intersection-caps (compiled-class-caps left) (compiled-class-caps right)))
  (define intersections (make-hasheq))
  (define (intersection left-ranges right-ranges)
    (define against (hash-ref! intersections left-ranges make-hasheq))
    (hash-ref! against right-ranges (lambda () (intersect-intervals left-ranges right-ranges))))
  (define machine
    (build-machine
     (cons (automaton-start a) (automaton-start b))
     (lambda (state)
       (and (vector-ref (automaton-accepting a) (car state))
            (vector-ref (automaton-accepting b) (cdr state))))
     (lambda (state)
       (product-outgoing (automaton-transitions a) (automaton-transitions b) state intersection))
     caps))
  (when (or
         (and (bounded? a)
              (<=
               (caps-ref (compiled-class-caps left) 'token-length)
               (caps-ref (compiled-class-caps right) 'token-length)))
         (and (bounded? b)
              (<=
               (caps-ref (compiled-class-caps right) 'token-length)
               (caps-ref (compiled-class-caps left) 'token-length))))
    (mark-bounded! machine))
  (compiled-class
   name
   ""
   machine
   caps
   (intersect-intervals (compiled-class-alphabet left) (compiled-class-alphabet right))))

(define (class-without-literal class literal #:name (name "without-literal"))
  (unless (string? literal) (fail "class-literal" "excluded literal must be text"))
  (define size (string-length literal))
  (define original (compiled-class-machine class))
  (define caps (compiled-class-caps class))
  (define alphabet (compiled-class-alphabet class))
  (define machine
    (cond
     ((zero? size) (build-machine 0 (lambda (_) #f) (lambda (_) '()) caps))
     (else
      (define-values
       (_advance prefix-edges)
       (literal-prefix-machine literal alphabet automaton-work-limit))
      (build-machine
       (cons (automaton-start original) 0)
       (lambda (state) (vector-ref (automaton-accepting original) (car state)))
       (lambda (state)
         (filter
          (lambda (transition) (< (cdr (edge-target transition)) size))
          (product-outgoing (automaton-transitions original) prefix-edges state)))
       caps))))
  (when (or (zero? size) (bounded? original)) (mark-bounded! machine))
  (compiled-class name "" machine caps alphabet))

(define (class-trimmed class #:name (name "trimmed"))
  (define alphabet (compiled-class-alphabet class))
  (define space (intersect-intervals alphabet '((32 . 32))))
  (define nonspace (subtract-intervals alphabet '((32 . 32))))
  (define nonspace-edge (if (null? nonspace) '() (list (edge nonspace 1))))
  (define following (append nonspace-edge (if (null? space) '() (list (edge space 2)))))
  (define boundaries
    (automaton
     0
     '#(#t #t #f)
     (vector->immutable-vector (vector nonspace-edge following following))))
  (class-intersection
   class
   (compiled-class "field-boundaries" "" boundaries (compiled-class-caps class) alphabet)
   #:name
   name))

(define (class-sequence parts #:caps (raw-caps (hash)) #:name (name "sequence"))
  (unless (and
           (list? parts)
           (andmap (lambda (part) (or (string? part) (compiled-class? part))) parts))
    (fail "class-sequence" "a class sequence contains literal strings or compiled classes"))
  (define inherited
    (for/first ((part (in-list parts)) #:when (compiled-class? part)) (compiled-class-caps part)))
  (define combined-length
    (for/sum
     ((part (in-list parts)))
     (if (string? part) (string-length part) (caps-ref (compiled-class-caps part) 'token-length))))
  (define base-caps (or inherited default-class-caps))
  (define selected
    (for/fold
     ((result base-caps))
     (((key value) (in-hash raw-caps)))
     (define name (if (string? key) (string->symbol key) key))
     (define canonical
       (case name
         ((max-states) 'states)
         ((max-repetition) 'repetition)
         ((depth max-reference-depth) 'reference-depth)
         ((length max-token-length) 'token-length)
         (else name)))
     (hash-set result canonical value)))
  (define explicit-length?
    (for/or
     ((key (in-list (hash-keys raw-caps))))
     (member
      key
      '(length token-length max-token-length "length" "token-length" "max-token-length"))))
  (define caps
    (normalize-caps
     (if explicit-length? selected (hash-set selected 'token-length (max 1 combined-length)))))
  (define alphabet (safe-alphabet caps))
  (define next 0)
  (define epsilon (make-hash))
  (define transitions (make-hash))
  (define (state!)
    (when (>= next automaton-work-limit) (work-fail "sequence" (add1 next)))
    (begin0 next (set! next (add1 next))))
  (define (link! from to) (hash-update! epsilon from (lambda (targets) (cons to targets)) '()))
  (define initial (state!))
  (define final
    (for/fold
     ((last initial))
     ((part (in-list parts)))
     (cond
      ((string? part)
       (for/fold
        ((last last))
        ((character (in-string part)))
        (define target (state!))
        (define point (char->integer character))
        (when (interval-member? alphabet point)
          (hash-update!
           transitions
           last
           (lambda (options) (cons (edge (list (cons point point)) target) options))
           '()))
        target))
      (else
       (define machine (bounded-class-machine part))
       (define same-alphabet? (equal? alphabet (compiled-class-alphabet part)))
       (define copies
         (build-vector (vector-length (automaton-transitions machine)) (lambda (_) (state!))))
       (define end (state!))
       (link! last (vector-ref copies (automaton-start machine)))
       (for
        ((index (in-range (vector-length copies))))
        (when (vector-ref (automaton-accepting machine) index)
          (link! (vector-ref copies index) end))
        (for
         ((transition (in-list (vector-ref (automaton-transitions machine) index))))
         (hash-update!
          transitions
          (vector-ref copies index)
          (lambda (options)
            (cons
             (edge
              (if same-alphabet?
                (edge-ranges transition)
                (intersect-intervals alphabet (edge-ranges transition)))
              (vector-ref copies (edge-target transition)))
             options))
          '())))
       end))))
  (define concatenated (determinize initial final epsilon transitions caps))
  (define machine
    (if (< (caps-ref caps 'token-length) combined-length)
      (bound-machine concatenated caps)
      concatenated))
  (compiled-class name "" (mark-bounded! machine) caps alphabet))

(define (class-contains-literal-witness class literal)
  (unless (string? literal) (fail "class-literal" "delimiter must be text"))
  (define caps (compiled-class-caps class))
  (define alphabet (compiled-class-alphabet class))
  (define tree
    (rx-sequence
     (list
      (list 'repeat (list 'characters alphabet) 0 #f)
      (list 'text literal)
      (list 'repeat (list 'characters alphabet) 0 #f))))
  (define-values
   (start end epsilon transitions)
   (make-nfa
    tree
    (lambda (name) (fail "class-reference" "unexpected internal reference"))
    caps
    alphabet))
  (define machine (determinize start end epsilon transitions caps))
  (class-overlap-witness class (compiled-class "delimiter" literal machine caps alphabet)))

(define (literal-prefix-machine literal alphabet cap)
  (define size (string-length literal))
  (when (zero? size) (fail "class-literal" "literal prefix must be nonempty text"))
  (when (> (add1 size) cap) (work-fail "delimiter" (add1 size) #:limit cap))
  (define points (list->vector (map char->integer (string->list literal))))
  (define failures (make-vector size 0))
  (for
   ((index (in-range 1 size)))
   (define previous
     (let loop ((previous (vector-ref failures (sub1 index))))
       (if (and (positive? previous)
                (not (= (vector-ref points previous) (vector-ref points index))))
         (loop (vector-ref failures (sub1 previous)))
         previous)))
   (vector-set!
    failures
    index
    (if (= (vector-ref points previous) (vector-ref points index)) (add1 previous) previous)))
  (define (advance prefix point)
    (cond
     ((= prefix size) size)
     (else
      (define next
        (let loop ((next prefix))
          (if (and (positive? next) (not (= point (vector-ref points next))))
            (loop (vector-ref failures (sub1 next)))
            next)))
      (if (= point (vector-ref points next)) (add1 next) next))))
  (define relevant (sort (remove-duplicates (vector->list points)) <))
  (define rest
    (subtract-intervals alphabet (for/list ((point (in-list relevant))) (cons point point))))
  (define prefix-edges
    (build-vector
     (add1 size)
     (lambda (prefix)
       (group-edges
        (append
         (if (null? rest) '() (list (edge rest (advance prefix -1))))
         (for/list
          ((point (in-list relevant)) #:when (interval-member? alphabet point))
          (edge (list (cons point point)) (advance prefix point))))))))
  (values advance prefix-edges))

(define (class-delimiter-witness class literal)
  (unless (string? literal) (fail "class-literal" "delimiter must be text"))
  (define size (string-length literal))
  (define machine (bounded-class-machine class))
  (define alphabet (compiled-class-alphabet class))
  (define cap automaton-work-limit)
  (cond
   ((zero? size) #f)
   (else
    (define-values (advance prefix-edges) (literal-prefix-machine literal alphabet cap))
    (define points (list->vector (map char->integer (string->list literal))))
    (define unsafe-prefix
      (build-vector
       (add1 size)
       (lambda (prefix)
         (=
          (for/fold
           ((state prefix))
           ((index (in-range (sub1 size))))
           (advance state (vector-ref points index)))
          size))))
    (search-witness
     (cons (automaton-start machine) 0)
     (lambda (state)
       (and (vector-ref (automaton-accepting machine) (car state))
            (vector-ref unsafe-prefix (cdr state))))
     (lambda (state)
       (sort
        (append-map
         (lambda (first)
           (filter-map
            (lambda (second)
              (define ranges (intersect-intervals (edge-ranges first) (edge-ranges second)))
              (and (pair? ranges) (edge ranges (cons (edge-target first) (edge-target second)))))
            (vector-ref prefix-edges (cdr state))))
         (vector-ref (automaton-transitions machine) (car state)))
        <
        #:key
        (lambda (transition) (caar (edge-ranges transition)))))
     cap
     (class-check-count-bounds class #:literal literal)))))

(define (class-witnesses class)
  (if (class-empty? class)
    (hash 'empty #t 'bounds (class-count-bounds class))
    (nonempty-class-witnesses class)))

(define (nonempty-class-witnesses class)
  (define machine (bounded-class-machine class))
  (define accepting (automaton-accepting machine))
  (define transitions (automaton-transitions machine))
  (define cap automaton-work-limit)
  (define shortest
    (search-witness
     (automaton-start machine)
     (lambda (state) (vector-ref accepting state))
     (lambda (state) (vector-ref transitions state))
     cap
     (class-check-count-bounds class)))
  (define memo (make-hash))
  (define (longest state)
    (hash-ref!
     memo
     state
     (lambda ()
       (for/fold
        ((best (boundary-choice (if (vector-ref accepting state) 0 -1) #f #f)))
        ((transition (in-list (vector-ref transitions state))))
        (define suffix (longest (edge-target transition)))
        (define distance (add1 (boundary-choice-distance suffix)))
        (define point (cdr (last (edge-ranges transition))))
        (if (and (>= (boundary-choice-distance suffix) 0)
                 (or (> distance (boundary-choice-distance best))
                     (and (= distance (boundary-choice-distance best))
                          (> point (boundary-choice-point best)))))
          (boundary-choice distance point (edge-target transition))
          best)))))
  (define initial (longest (automaton-start machine)))
  (define boundary
    (and (>= (boundary-choice-distance initial) 0)
         (let ((text (make-string (boundary-choice-distance initial))))
           (let loop ((state (automaton-start machine)) (index 0))
             (when (< index (string-length text))
               (define choice (hash-ref memo state))
               (string-set! text index (integer->char (boundary-choice-point choice)))
               (loop (boundary-choice-target choice) (add1 index))))
           text)))
  (define near (if (not (vector-ref accepting (automaton-start machine))) "" (string #\nul)))
  (hash 'shortest shortest 'boundary boundary 'near-miss near 'bounds (class-count-bounds class)))

(define (gbnf-point point)
  (define width (if (<= point 65535) 4 8))
  (define digits (string-upcase (number->string point 16)))
  (string-append
   (if (= width 4) "\\u" "\\U")
   (make-string (- width (string-length digits)) #\0)
   digits))

(define (gbnf-range ranges)
  (string-append
   "["
   (apply
    string-append
    (for/list
     ((range (in-list ranges)))
     (if (= (car range) (cdr range))
       (gbnf-point (car range))
       (string-append (gbnf-point (car range)) "-" (gbnf-point (cdr range))))))
   "]"))

(define (class->gbnf-rules class #:name (name "class"))
  (unless (and
           (string? name)
           (positive? (string-length name))
           (for/and
            ((character (in-string name)))
            (or (char<=? #\a character #\z)
                (char<=? #\A character #\Z)
                (char<=? #\0 character #\9)
                (char=? character #\-))))
    (fail "class-gbnf-name" "GBNF rule names contain letters, digits and hyphens only"))
  (define machine (bounded-class-machine class))
  (define rows (automaton-transitions machine))
  (define accepts (automaton-accepting machine))
  (define productive (make-hash))
  (define (productive? state)
    (hash-ref!
     productive
     state
     (lambda ()
       (or (vector-ref accepts state)
           (for/or
            ((transition (in-list (vector-ref rows state))))
            (productive? (edge-target transition)))))))
  (define (state-name state) (format "~a-s~a" name state))
  (define rules (make-hash))
  (define charsets (make-hash))
  (define (charset-name ranges)
    (hash-ref!
     charsets
     ranges
     (lambda ()
       (define rule (format "~a-c~a" name (hash-count charsets)))
       (hash-set! rules rule (gbnf-range ranges))
       rule)))
  (for
   ((state (in-range (vector-length rows))) #:when (productive? state))
   (define alternatives
     (append
      (if (vector-ref accepts state) (list "\"\"") '())
      (for/list
       ((transition (in-list (vector-ref rows state)))
        #:when
        (productive? (edge-target transition)))
       (string-append
        (charset-name (edge-ranges transition))
        " "
        (state-name (edge-target transition))))))
   (hash-set! rules (state-name state) (string-join alternatives " | ")))
  (hash-set!
   rules
   name
   (if (productive? (automaton-start machine))
     (state-name (automaton-start machine))
     "[^\\u0000-\\U0010FFFF]"))
  (hash 'start name 'rules (make-immutable-hash (hash->list rules))))

(define (class->gbnf class #:name (name "class"))
  (define generated (class->gbnf-rules class #:name name))
  (define rules (hash-ref generated 'rules))
  (string-append
   (string-join
    (for/list
     ((key (in-list (cons name (remove name (sort (hash-keys rules) string<?))))))
     (string-append key " ::= " (hash-ref rules key)))
    "\n")
   "\n"))

(define unicode-version "17.0.0")

(define unicode-source
  "https://www.unicode.org/Public/17.0.0/ucd/extracted/DerivedGeneralCategory.txt")

(define unicode-source-sha256 "d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e")

(define unicode-letter-intervals
  '((65 . 90)
    (97 . 122)
    (170 . 170)
    (181 . 181)
    (186 . 186)
    (192 . 214)
    (216 . 246)
    (248 . 705)
    (710 . 721)
    (736 . 740)
    (748 . 748)
    (750 . 750)
    (880 . 884)
    (886 . 887)
    (890 . 893)
    (895 . 895)
    (902 . 902)
    (904 . 906)
    (908 . 908)
    (910 . 929)
    (931 . 1013)
    (1015 . 1153)
    (1162 . 1327)
    (1329 . 1366)
    (1369 . 1369)
    (1376 . 1416)
    (1488 . 1514)
    (1519 . 1522)
    (1568 . 1610)
    (1646 . 1647)
    (1649 . 1747)
    (1749 . 1749)
    (1765 . 1766)
    (1774 . 1775)
    (1786 . 1788)
    (1791 . 1791)
    (1808 . 1808)
    (1810 . 1839)
    (1869 . 1957)
    (1969 . 1969)
    (1994 . 2026)
    (2036 . 2037)
    (2042 . 2042)
    (2048 . 2069)
    (2074 . 2074)
    (2084 . 2084)
    (2088 . 2088)
    (2112 . 2136)
    (2144 . 2154)
    (2160 . 2183)
    (2185 . 2191)
    (2208 . 2249)
    (2308 . 2361)
    (2365 . 2365)
    (2384 . 2384)
    (2392 . 2401)
    (2417 . 2432)
    (2437 . 2444)
    (2447 . 2448)
    (2451 . 2472)
    (2474 . 2480)
    (2482 . 2482)
    (2486 . 2489)
    (2493 . 2493)
    (2510 . 2510)
    (2524 . 2525)
    (2527 . 2529)
    (2544 . 2545)
    (2556 . 2556)
    (2565 . 2570)
    (2575 . 2576)
    (2579 . 2600)
    (2602 . 2608)
    (2610 . 2611)
    (2613 . 2614)
    (2616 . 2617)
    (2649 . 2652)
    (2654 . 2654)
    (2674 . 2676)
    (2693 . 2701)
    (2703 . 2705)
    (2707 . 2728)
    (2730 . 2736)
    (2738 . 2739)
    (2741 . 2745)
    (2749 . 2749)
    (2768 . 2768)
    (2784 . 2785)
    (2809 . 2809)
    (2821 . 2828)
    (2831 . 2832)
    (2835 . 2856)
    (2858 . 2864)
    (2866 . 2867)
    (2869 . 2873)
    (2877 . 2877)
    (2908 . 2909)
    (2911 . 2913)
    (2929 . 2929)
    (2947 . 2947)
    (2949 . 2954)
    (2958 . 2960)
    (2962 . 2965)
    (2969 . 2970)
    (2972 . 2972)
    (2974 . 2975)
    (2979 . 2980)
    (2984 . 2986)
    (2990 . 3001)
    (3024 . 3024)
    (3077 . 3084)
    (3086 . 3088)
    (3090 . 3112)
    (3114 . 3129)
    (3133 . 3133)
    (3160 . 3162)
    (3164 . 3165)
    (3168 . 3169)
    (3200 . 3200)
    (3205 . 3212)
    (3214 . 3216)
    (3218 . 3240)
    (3242 . 3251)
    (3253 . 3257)
    (3261 . 3261)
    (3292 . 3294)
    (3296 . 3297)
    (3313 . 3314)
    (3332 . 3340)
    (3342 . 3344)
    (3346 . 3386)
    (3389 . 3389)
    (3406 . 3406)
    (3412 . 3414)
    (3423 . 3425)
    (3450 . 3455)
    (3461 . 3478)
    (3482 . 3505)
    (3507 . 3515)
    (3517 . 3517)
    (3520 . 3526)
    (3585 . 3632)
    (3634 . 3635)
    (3648 . 3654)
    (3713 . 3714)
    (3716 . 3716)
    (3718 . 3722)
    (3724 . 3747)
    (3749 . 3749)
    (3751 . 3760)
    (3762 . 3763)
    (3773 . 3773)
    (3776 . 3780)
    (3782 . 3782)
    (3804 . 3807)
    (3840 . 3840)
    (3904 . 3911)
    (3913 . 3948)
    (3976 . 3980)
    (4096 . 4138)
    (4159 . 4159)
    (4176 . 4181)
    (4186 . 4189)
    (4193 . 4193)
    (4197 . 4198)
    (4206 . 4208)
    (4213 . 4225)
    (4238 . 4238)
    (4256 . 4293)
    (4295 . 4295)
    (4301 . 4301)
    (4304 . 4346)
    (4348 . 4680)
    (4682 . 4685)
    (4688 . 4694)
    (4696 . 4696)
    (4698 . 4701)
    (4704 . 4744)
    (4746 . 4749)
    (4752 . 4784)
    (4786 . 4789)
    (4792 . 4798)
    (4800 . 4800)
    (4802 . 4805)
    (4808 . 4822)
    (4824 . 4880)
    (4882 . 4885)
    (4888 . 4954)
    (4992 . 5007)
    (5024 . 5109)
    (5112 . 5117)
    (5121 . 5740)
    (5743 . 5759)
    (5761 . 5786)
    (5792 . 5866)
    (5873 . 5880)
    (5888 . 5905)
    (5919 . 5937)
    (5952 . 5969)
    (5984 . 5996)
    (5998 . 6000)
    (6016 . 6067)
    (6103 . 6103)
    (6108 . 6108)
    (6176 . 6264)
    (6272 . 6276)
    (6279 . 6312)
    (6314 . 6314)
    (6320 . 6389)
    (6400 . 6430)
    (6480 . 6509)
    (6512 . 6516)
    (6528 . 6571)
    (6576 . 6601)
    (6656 . 6678)
    (6688 . 6740)
    (6823 . 6823)
    (6917 . 6963)
    (6981 . 6988)
    (7043 . 7072)
    (7086 . 7087)
    (7098 . 7141)
    (7168 . 7203)
    (7245 . 7247)
    (7258 . 7293)
    (7296 . 7306)
    (7312 . 7354)
    (7357 . 7359)
    (7401 . 7404)
    (7406 . 7411)
    (7413 . 7414)
    (7418 . 7418)
    (7424 . 7615)
    (7680 . 7957)
    (7960 . 7965)
    (7968 . 8005)
    (8008 . 8013)
    (8016 . 8023)
    (8025 . 8025)
    (8027 . 8027)
    (8029 . 8029)
    (8031 . 8061)
    (8064 . 8116)
    (8118 . 8124)
    (8126 . 8126)
    (8130 . 8132)
    (8134 . 8140)
    (8144 . 8147)
    (8150 . 8155)
    (8160 . 8172)
    (8178 . 8180)
    (8182 . 8188)
    (8305 . 8305)
    (8319 . 8319)
    (8336 . 8348)
    (8450 . 8450)
    (8455 . 8455)
    (8458 . 8467)
    (8469 . 8469)
    (8473 . 8477)
    (8484 . 8484)
    (8486 . 8486)
    (8488 . 8488)
    (8490 . 8493)
    (8495 . 8505)
    (8508 . 8511)
    (8517 . 8521)
    (8526 . 8526)
    (8579 . 8580)
    (11264 . 11492)
    (11499 . 11502)
    (11506 . 11507)
    (11520 . 11557)
    (11559 . 11559)
    (11565 . 11565)
    (11568 . 11623)
    (11631 . 11631)
    (11648 . 11670)
    (11680 . 11686)
    (11688 . 11694)
    (11696 . 11702)
    (11704 . 11710)
    (11712 . 11718)
    (11720 . 11726)
    (11728 . 11734)
    (11736 . 11742)
    (11823 . 11823)
    (12293 . 12294)
    (12337 . 12341)
    (12347 . 12348)
    (12353 . 12438)
    (12445 . 12447)
    (12449 . 12538)
    (12540 . 12543)
    (12549 . 12591)
    (12593 . 12686)
    (12704 . 12735)
    (12784 . 12799)
    (13312 . 19903)
    (19968 . 42124)
    (42192 . 42237)
    (42240 . 42508)
    (42512 . 42527)
    (42538 . 42539)
    (42560 . 42606)
    (42623 . 42653)
    (42656 . 42725)
    (42775 . 42783)
    (42786 . 42888)
    (42891 . 42972)
    (42993 . 43009)
    (43011 . 43013)
    (43015 . 43018)
    (43020 . 43042)
    (43072 . 43123)
    (43138 . 43187)
    (43250 . 43255)
    (43259 . 43259)
    (43261 . 43262)
    (43274 . 43301)
    (43312 . 43334)
    (43360 . 43388)
    (43396 . 43442)
    (43471 . 43471)
    (43488 . 43492)
    (43494 . 43503)
    (43514 . 43518)
    (43520 . 43560)
    (43584 . 43586)
    (43588 . 43595)
    (43616 . 43638)
    (43642 . 43642)
    (43646 . 43695)
    (43697 . 43697)
    (43701 . 43702)
    (43705 . 43709)
    (43712 . 43712)
    (43714 . 43714)
    (43739 . 43741)
    (43744 . 43754)
    (43762 . 43764)
    (43777 . 43782)
    (43785 . 43790)
    (43793 . 43798)
    (43808 . 43814)
    (43816 . 43822)
    (43824 . 43866)
    (43868 . 43881)
    (43888 . 44002)
    (44032 . 55203)
    (55216 . 55238)
    (55243 . 55291)
    (63744 . 64109)
    (64112 . 64217)
    (64256 . 64262)
    (64275 . 64279)
    (64285 . 64285)
    (64287 . 64296)
    (64298 . 64310)
    (64312 . 64316)
    (64318 . 64318)
    (64320 . 64321)
    (64323 . 64324)
    (64326 . 64433)
    (64467 . 64829)
    (64848 . 64911)
    (64914 . 64967)
    (65008 . 65019)
    (65136 . 65140)
    (65142 . 65276)
    (65313 . 65338)
    (65345 . 65370)
    (65382 . 65470)
    (65474 . 65479)
    (65482 . 65487)
    (65490 . 65495)
    (65498 . 65500)
    (65536 . 65547)
    (65549 . 65574)
    (65576 . 65594)
    (65596 . 65597)
    (65599 . 65613)
    (65616 . 65629)
    (65664 . 65786)
    (66176 . 66204)
    (66208 . 66256)
    (66304 . 66335)
    (66349 . 66368)
    (66370 . 66377)
    (66384 . 66421)
    (66432 . 66461)
    (66464 . 66499)
    (66504 . 66511)
    (66560 . 66717)
    (66736 . 66771)
    (66776 . 66811)
    (66816 . 66855)
    (66864 . 66915)
    (66928 . 66938)
    (66940 . 66954)
    (66956 . 66962)
    (66964 . 66965)
    (66967 . 66977)
    (66979 . 66993)
    (66995 . 67001)
    (67003 . 67004)
    (67008 . 67059)
    (67072 . 67382)
    (67392 . 67413)
    (67424 . 67431)
    (67456 . 67461)
    (67463 . 67504)
    (67506 . 67514)
    (67584 . 67589)
    (67592 . 67592)
    (67594 . 67637)
    (67639 . 67640)
    (67644 . 67644)
    (67647 . 67669)
    (67680 . 67702)
    (67712 . 67742)
    (67808 . 67826)
    (67828 . 67829)
    (67840 . 67861)
    (67872 . 67897)
    (67904 . 67929)
    (67968 . 68023)
    (68030 . 68031)
    (68096 . 68096)
    (68112 . 68115)
    (68117 . 68119)
    (68121 . 68149)
    (68192 . 68220)
    (68224 . 68252)
    (68288 . 68295)
    (68297 . 68324)
    (68352 . 68405)
    (68416 . 68437)
    (68448 . 68466)
    (68480 . 68497)
    (68608 . 68680)
    (68736 . 68786)
    (68800 . 68850)
    (68864 . 68899)
    (68938 . 68965)
    (68975 . 68997)
    (69248 . 69289)
    (69296 . 69297)
    (69314 . 69319)
    (69376 . 69404)
    (69415 . 69415)
    (69424 . 69445)
    (69488 . 69505)
    (69552 . 69572)
    (69600 . 69622)
    (69635 . 69687)
    (69745 . 69746)
    (69749 . 69749)
    (69763 . 69807)
    (69840 . 69864)
    (69891 . 69926)
    (69956 . 69956)
    (69959 . 69959)
    (69968 . 70002)
    (70006 . 70006)
    (70019 . 70066)
    (70081 . 70084)
    (70106 . 70106)
    (70108 . 70108)
    (70144 . 70161)
    (70163 . 70187)
    (70207 . 70208)
    (70272 . 70278)
    (70280 . 70280)
    (70282 . 70285)
    (70287 . 70301)
    (70303 . 70312)
    (70320 . 70366)
    (70405 . 70412)
    (70415 . 70416)
    (70419 . 70440)
    (70442 . 70448)
    (70450 . 70451)
    (70453 . 70457)
    (70461 . 70461)
    (70480 . 70480)
    (70493 . 70497)
    (70528 . 70537)
    (70539 . 70539)
    (70542 . 70542)
    (70544 . 70581)
    (70583 . 70583)
    (70609 . 70609)
    (70611 . 70611)
    (70656 . 70708)
    (70727 . 70730)
    (70751 . 70753)
    (70784 . 70831)
    (70852 . 70853)
    (70855 . 70855)
    (71040 . 71086)
    (71128 . 71131)
    (71168 . 71215)
    (71236 . 71236)
    (71296 . 71338)
    (71352 . 71352)
    (71424 . 71450)
    (71488 . 71494)
    (71680 . 71723)
    (71840 . 71903)
    (71935 . 71942)
    (71945 . 71945)
    (71948 . 71955)
    (71957 . 71958)
    (71960 . 71983)
    (71999 . 71999)
    (72001 . 72001)
    (72096 . 72103)
    (72106 . 72144)
    (72161 . 72161)
    (72163 . 72163)
    (72192 . 72192)
    (72203 . 72242)
    (72250 . 72250)
    (72272 . 72272)
    (72284 . 72329)
    (72349 . 72349)
    (72368 . 72440)
    (72640 . 72672)
    (72704 . 72712)
    (72714 . 72750)
    (72768 . 72768)
    (72818 . 72847)
    (72960 . 72966)
    (72968 . 72969)
    (72971 . 73008)
    (73030 . 73030)
    (73056 . 73061)
    (73063 . 73064)
    (73066 . 73097)
    (73112 . 73112)
    (73136 . 73179)
    (73440 . 73458)
    (73474 . 73474)
    (73476 . 73488)
    (73490 . 73523)
    (73648 . 73648)
    (73728 . 74649)
    (74880 . 75075)
    (77712 . 77808)
    (77824 . 78895)
    (78913 . 78918)
    (78944 . 82938)
    (82944 . 83526)
    (90368 . 90397)
    (92160 . 92728)
    (92736 . 92766)
    (92784 . 92862)
    (92880 . 92909)
    (92928 . 92975)
    (92992 . 92995)
    (93027 . 93047)
    (93053 . 93071)
    (93504 . 93548)
    (93760 . 93823)
    (93856 . 93880)
    (93883 . 93907)
    (93952 . 94026)
    (94032 . 94032)
    (94099 . 94111)
    (94176 . 94177)
    (94179 . 94179)
    (94194 . 94195)
    (94208 . 101589)
    (101631 . 101662)
    (101760 . 101874)
    (110576 . 110579)
    (110581 . 110587)
    (110589 . 110590)
    (110592 . 110882)
    (110898 . 110898)
    (110928 . 110930)
    (110933 . 110933)
    (110948 . 110951)
    (110960 . 111355)
    (113664 . 113770)
    (113776 . 113788)
    (113792 . 113800)
    (113808 . 113817)
    (119808 . 119892)
    (119894 . 119964)
    (119966 . 119967)
    (119970 . 119970)
    (119973 . 119974)
    (119977 . 119980)
    (119982 . 119993)
    (119995 . 119995)
    (119997 . 120003)
    (120005 . 120069)
    (120071 . 120074)
    (120077 . 120084)
    (120086 . 120092)
    (120094 . 120121)
    (120123 . 120126)
    (120128 . 120132)
    (120134 . 120134)
    (120138 . 120144)
    (120146 . 120485)
    (120488 . 120512)
    (120514 . 120538)
    (120540 . 120570)
    (120572 . 120596)
    (120598 . 120628)
    (120630 . 120654)
    (120656 . 120686)
    (120688 . 120712)
    (120714 . 120744)
    (120746 . 120770)
    (120772 . 120779)
    (122624 . 122654)
    (122661 . 122666)
    (122928 . 122989)
    (123136 . 123180)
    (123191 . 123197)
    (123214 . 123214)
    (123536 . 123565)
    (123584 . 123627)
    (124112 . 124139)
    (124368 . 124397)
    (124400 . 124400)
    (124608 . 124638)
    (124640 . 124642)
    (124644 . 124645)
    (124647 . 124653)
    (124656 . 124660)
    (124670 . 124671)
    (124896 . 124902)
    (124904 . 124907)
    (124909 . 124910)
    (124912 . 124926)
    (124928 . 125124)
    (125184 . 125251)
    (125259 . 125259)
    (126464 . 126467)
    (126469 . 126495)
    (126497 . 126498)
    (126500 . 126500)
    (126503 . 126503)
    (126505 . 126514)
    (126516 . 126519)
    (126521 . 126521)
    (126523 . 126523)
    (126530 . 126530)
    (126535 . 126535)
    (126537 . 126537)
    (126539 . 126539)
    (126541 . 126543)
    (126545 . 126546)
    (126548 . 126548)
    (126551 . 126551)
    (126553 . 126553)
    (126555 . 126555)
    (126557 . 126557)
    (126559 . 126559)
    (126561 . 126562)
    (126564 . 126564)
    (126567 . 126570)
    (126572 . 126578)
    (126580 . 126583)
    (126585 . 126588)
    (126590 . 126590)
    (126592 . 126601)
    (126603 . 126619)
    (126625 . 126627)
    (126629 . 126633)
    (126635 . 126651)
    (131072 . 173791)
    (173824 . 178205)
    (178208 . 183981)
    (183984 . 191456)
    (191472 . 192093)
    (194560 . 195101)
    (196608 . 201546)
    (201552 . 210041)))

(define unicode-unsafe-intervals
  '((0 . 32)
    (127 . 160)
    (173 . 173)
    (1536 . 1541)
    (1564 . 1564)
    (1757 . 1757)
    (1807 . 1807)
    (2192 . 2193)
    (2274 . 2274)
    (5760 . 5760)
    (6158 . 6158)
    (8192 . 8207)
    (8232 . 8239)
    (8287 . 8292)
    (8294 . 8303)
    (12288 . 12288)
    (65279 . 65279)
    (65529 . 65531)
    (69821 . 69821)
    (69837 . 69837)
    (78896 . 78911)
    (113824 . 113827)
    (119155 . 119162)
    (917505 . 917505)
    (917536 . 917631)))
