#lang racket/base

(require racket/list)

(provide minimize-dfa)

(struct segment (lower upper target) #:transparent)

(define (merge-segments segments)
  (reverse
   (for/fold
    ((result '()))
    ((current (in-list segments)))
    (cond
     ((and (pair? result)
           (= (segment-target (car result)) (segment-target current))
           (= (add1 (segment-upper (car result))) (segment-lower current)))
      (cons
       (segment (segment-lower (car result)) (segment-upper current) (segment-target current))
       (cdr result)))
     (else (cons current result))))))

(define (segments->edges segments)
  (define targets (make-hasheqv))
  (for
   ((current (in-list segments)))
   (hash-update!
    targets
    (segment-target current)
    (lambda (ranges) (cons (cons (segment-lower current) (segment-upper current)) ranges))
    '()))
  (sort
   (for/list (((target ranges) (in-hash targets))) (cons (reverse ranges) target))
   <
   #:key
   (lambda (edge) (car (car (car edge))))))

(define (minimize-dfa start accepting transitions)
  (unless (and
           (vector? accepting)
           (vector? transitions)
           (positive? (vector-length accepting))
           (= (vector-length accepting) (vector-length transitions)))
    (raise-arguments-error
     'minimize-dfa
     "matching nonempty accepting and transition vectors required"))
  (define count (vector-length accepting))
  (define (state? value) (and (exact-nonnegative-integer? value) (< value count)))
  (unless (state? start) (raise-argument-error 'minimize-dfa "valid start state" start))
  (unless (for/and ((value (in-vector accepting))) (boolean? value))
    (raise-argument-error 'minimize-dfa "vector of booleans" accepting))
  (define rows
    (for/vector
     ((edges (in-vector transitions)))
     (unless (list? edges)
       (raise-argument-error 'minimize-dfa "list of interval-labelled edges" edges))
     (define flattened
       (append-map
        (lambda (edge)
          (unless (and (pair? edge) (list? (car edge)) (state? (cdr edge)))
            (raise-argument-error 'minimize-dfa "edge containing ranges and a valid target" edge))
          (for/list
           ((range (in-list (car edge))))
           (unless (and
                    (pair? range)
                    (exact-integer? (car range))
                    (exact-integer? (cdr range))
                    (<= (car range) (cdr range)))
             (raise-argument-error 'minimize-dfa "inclusive integer range" range))
           (segment (car range) (cdr range) (cdr edge))))
        edges))
     (define sorted (sort flattened < #:key segment-lower))
     (for
      ((left (in-list sorted)) (right (in-list (if (null? sorted) '() (cdr sorted)))))
      (when (>= (segment-upper left) (segment-lower right))
        (raise-arguments-error 'minimize-dfa "transition intervals must be disjoint")))
     (merge-segments sorted)))
  (define reachable (make-hasheqv))
  (let visit ((pending (list start)))
    (unless (null? pending)
      (define state (car pending))
      (if (hash-has-key? reachable state)
        (visit (cdr pending))
        (begin
          (hash-set! reachable state #t)
          (visit (append (map segment-target (vector-ref rows state)) (cdr pending)))))))
  (define predecessors (make-vector count '()))
  (for
   ((state (in-hash-keys reachable)))
   (for
    ((current (in-list (vector-ref rows state))))
    (define target (segment-target current))
    (vector-set! predecessors target (cons state (vector-ref predecessors target)))))
  (define productive (make-hasheqv))
  (let visit ((pending
               (for/list
                ((state (in-hash-keys reachable)) #:when (vector-ref accepting state))
                state)))
    (unless (null? pending)
      (define state (car pending))
      (if (hash-has-key? productive state)
        (visit (cdr pending))
        (begin
          (hash-set! productive state #t)
          (visit (append (vector-ref predecessors state) (cdr pending)))))))
  (cond
   ((not (hash-has-key? productive start)) (values 0 (vector-immutable #f) (vector-immutable '())))
   (else
    (define states (sort (hash-keys productive) <))
    (define (labelled state blocks)
      (merge-segments
       (for/list
        ((current (in-list (vector-ref rows state)))
         #:when
         (hash-has-key? productive (segment-target current)))
        (segment
         (segment-lower current)
         (segment-upper current)
         (hash-ref blocks (segment-target current))))))
    (define initial
      (for/hash ((state (in-list states))) (values state (if (vector-ref accepting state) 1 0))))
    (define blocks
      (let refine ((previous initial))
        (define signatures (make-hash))
        (define next
          (for/hash
           ((state (in-list states)))
           (define signature (cons (vector-ref accepting state) (labelled state previous)))
           (values state (hash-ref! signatures signature (lambda () (hash-count signatures))))))
        (if (equal? previous next) next (refine next))))
    (define representatives (make-hasheqv))
    (for
     ((state (in-list states)))
     (hash-ref! representatives (hash-ref blocks state) (lambda () state)))
    (define queue (make-vector (hash-count representatives) #f))
    (define identifiers (make-hasheqv))
    (define start-block (hash-ref blocks start))
    (hash-set! identifiers start-block 0)
    (vector-set! queue 0 start-block)
    (let discover ((head 0))
      (when (< head (hash-count identifiers))
        (define state (hash-ref representatives (vector-ref queue head)))
        (for
         ((current (in-list (labelled state blocks))))
         (define target (segment-target current))
         (unless (hash-has-key? identifiers target)
           (define identifier (hash-count identifiers))
           (hash-set! identifiers target identifier)
           (vector-set! queue identifier target)))
        (discover (add1 head))))
    (define result-count (hash-count identifiers))
    (define result-accepting
      (build-vector
       result-count
       (lambda (index) (vector-ref accepting (hash-ref representatives (vector-ref queue index))))))
    (define result-transitions
      (build-vector
       result-count
       (lambda (index)
         (define state (hash-ref representatives (vector-ref queue index)))
         (segments->edges
          (for/list
           ((current (in-list (labelled state blocks))))
           (segment
            (segment-lower current)
            (segment-upper current)
            (hash-ref identifiers (segment-target current))))))))
    (values
     0
     (vector->immutable-vector result-accepting)
     (vector->immutable-vector result-transitions)))))
