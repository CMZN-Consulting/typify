#lang racket/base

(require racket/list "../src/automata-minimize.rkt")

(provide run-tests)

(struct machine (start accepting transitions) #:transparent)

(define (edge lower upper target) (cons (list (cons lower upper)) target))

(define (minimum input)
  (call-with-values
   (lambda ()
     (minimize-dfa (machine-start input) (machine-accepting input) (machine-transitions input)))
   machine))

(define (row input state) (if state (vector-ref (machine-transitions input) state) '()))

(define (accepting? input state) (and state (vector-ref (machine-accepting input) state)))

(define (step input state point)
  (for*/first
   ((outgoing (in-list (row input state)))
    (range (in-list (car outgoing)))
    #:when
    (<= (car range) point (cdr range)))
   (cdr outgoing)))

(define (accepts? input word)
  (accepting?
   input
   (for/fold ((state (machine-start input))) ((point (in-list word))) (step input state point))))

(define (difference-witness left right)
  (define seen (make-hash))
  (let loop ((pending (list (list (machine-start left) (machine-start right) '()))))
    (cond
     ((null? pending) #f)
     (else
      (define current (car pending))
      (define a (car current))
      (define b (cadr current))
      (define word (caddr current))
      (define key (cons a b))
      (cond
       ((hash-has-key? seen key) (loop (cdr pending)))
       ((not (equal? (accepting? left a) (accepting? right b))) (reverse word))
       (else
        (hash-set! seen key #t)
        (define points
          (sort
           (remove-duplicates
            (append-map
             (lambda (outgoing)
               (append-map (lambda (range) (list (car range) (add1 (cdr range)))) (car outgoing)))
             (append (row left a) (row right b))))
           <))
        (loop
         (append
          (for/list
           ((point (in-list points)))
           (list (step left a point) (step right b point) (cons point word)))
          (cdr pending)))))))))

(define (renumber input mapping)
  (define count (vector-length mapping))
  (define accepting (make-vector count #f))
  (define transitions (make-vector count '()))
  (for
   ((old (in-range count)))
   (define next (vector-ref mapping old))
   (vector-set! accepting next (vector-ref (machine-accepting input) old))
   (vector-set!
    transitions
    next
    (for/list
     ((outgoing (in-list (reverse (vector-ref (machine-transitions input) old)))))
     (cons (reverse (car outgoing)) (vector-ref mapping (cdr outgoing))))))
  (machine (vector-ref mapping (machine-start input)) accepting transitions))

(define (run-tests)
  (define checks 0)
  (define models 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-automata-minimize "~a: expected ~s; received ~s" context expected actual)))
  (define (checked input expected-count context)
    (define before
      (list (vector->list (machine-accepting input)) (vector->list (machine-transitions input))))
    (define result (minimum input))
    (set! models (add1 models))
    (check (machine-start result) 0 context)
    (check (vector-length (machine-accepting result)) expected-count context)
    (check (vector-length (machine-transitions result)) expected-count context)
    (check (immutable? (machine-accepting result)) #t context)
    (check (immutable? (machine-transitions result)) #t context)
    (check (difference-witness input result) #f context)
    (check (minimum result) result context)
    (check
     (list (vector->list (machine-accepting input)) (vector->list (machine-transitions input)))
     before
     context)
    result)
  (define redundant
    (machine
     0
     (vector #f #t #t #f)
     (vector (list (edge 10 12 1) (edge 13 15 2)) '() '() (list (edge -1000 1000 3)))))
  (define redundant-result
    (checked redundant 2 "equivalent accepting leaves and unreachable control"))
  (check
   (vector-ref (machine-transitions redundant-result) 0)
   (list (edge 10 15 1))
   "adjacent equivalent intervals are coalesced")
  (define permuted (renumber redundant '#(2 3 1 0)))
  (check
   (checked permuted 2 "renumbered equivalent input")
   redundant-result
   "canonical BFS order does not depend on input state identifiers")
  (define reject-equivalence
    (machine
     0
     (vector #f #f #f #t #f)
     (vector
      (list (edge 1 1 1) (edge 2 2 2))
      (list (edge 3 3 3))
      (list (edge 3 3 3) (edge 4 4 4))
      '()
      (list (edge 0 100 4)))))
  (define reject-result
    (checked reject-equivalence 3 "explicit dead transitions equal missing transitions"))
  (check
   (vector-ref (machine-transitions reject-result) 0)
   (list (edge 1 2 1))
   "productive controls merge after reject sink omission")
  (check (accepts? reject-result '(1 3)) #t "first equivalent path remains accepted")
  (check (accepts? reject-result '(2 3)) #t "second equivalent path remains accepted")
  (check (accepts? reject-result '(2 4)) #f "discarded sink transition remains rejected")
  (define future-sensitive
    (machine 0 '#(#f #t #t) (vector (list (edge 1 1 1) (edge 2 2 2)) (list (edge 3 3 1)) '())))
  (define future-result
    (checked future-sensitive 3 "accepting controls with distinct suffix languages"))
  (check (accepts? future-result '(1 3)) #t "accepting loop suffix")
  (check (accepts? future-result '(2 3)) #f "missing suffix is implicit rejection")
  (define gaps (machine 0 '#(#f #t) (vector (list (edge 3 3 1) (edge 1 1 1)) '())))
  (define gap-result (checked gaps 2 "unsorted ranges retain a rejecting gap"))
  (check
   (vector-ref (machine-transitions gap-result) 0)
   (list (cons (list (cons 1 1) (cons 3 3)) 1))
   "nonadjacent equal-target ranges stay separate")
  (check (accepts? gap-result '(2)) #f "a gap is never filled by interval merging")
  (define cycle (machine 0 '#(#t #t) (vector (list (edge 10 20 1)) (list (edge 10 20 1)))))
  (define cycle-result (checked cycle 1 "equivalent productive cyclic accepting controls"))
  (check
   (vector-ref (machine-transitions cycle-result) 0)
   (list (edge 10 20 0))
   "canonical self loop")
  (check (accepts? cycle-result '()) #t "empty word remains accepted")
  (define empty-language
    (machine 0 '#(#f #f #t) (vector (list (edge 0 100 1)) (list (edge 0 100 0)) '())))
  (define empty-result
    (checked empty-language 1 "reachable nonproductive graph with unreachable accepting control"))
  (check (machine-accepting empty-result) '#(#f) "empty language has one rejecting start")
  (check (machine-transitions empty-result) '#(()) "empty language has no explicit reject loop")
  (define epsilon (machine 2 '#(#f #t #t) (vector (list (edge 0 100 1)) '() '())))
  (define epsilon-result (checked epsilon 1 "nonzero start with epsilon-only language"))
  (check (machine-accepting epsilon-result) '#(#t) "epsilon acceptance preserved")
  (check (machine-transitions epsilon-result) '#(()) "epsilon-only language omits sink")
  (define large-interval
    (machine
     0
     '#(#f #t #t)
     (vector (list (edge 0 499999999999 1) (edge 500000000000 1000000000000 2)) '() '())))
  (define large-result
    (checked large-interval 2 "interval arithmetic never enumerates scalar values"))
  (check
   (vector-ref (machine-transitions large-result) 0)
   (list (edge 0 1000000000000 1))
   "large adjacent intervals coalesce symbolically")
  (define chain
    (machine
     0
     '#(#f #f #f #t)
     (vector (list (edge 1 1 1)) (list (edge 1 1 2)) (list (edge 1 1 3)) '())))
  (define chain-result (checked chain 4 "distinct finite accepted suffix lengths"))
  (check (accepts? chain-result '(1 1)) #f "short chain word rejected")
  (check (accepts? chain-result '(1 1 1)) #t "exact chain word accepted")
  (check (accepts? chain-result '(1 1 1 1)) #f "long chain word rejected")
  (define (rejected thunk) (with-handlers ((exn:fail? (lambda (_) #t))) (thunk) #f))
  (check (rejected (lambda () (minimize-dfa 0 '#() '#()))) #t "empty input vectors rejected")
  (check (rejected (lambda () (minimize-dfa 1 '#(#f) '#(())))) #t "invalid start rejected")
  (check
   (rejected (lambda () (minimize-dfa 0 '#(#f) (vector (list (edge 0 1 1))))))
   #t
   "invalid target rejected")
  (check
   (rejected (lambda () (minimize-dfa 0 '#(#f #t) (vector (list (edge 0 5 1) (edge 5 10 1)) '()))))
   #t
   "nondeterministic overlapping intervals rejected")
  (check
   (rejected (lambda () (minimize-dfa 0 '#(#f #t) (vector (list (edge 5 0 1)) '()))))
   #t
   "reversed range rejected")
  (hash
   'suite
   "automata-minimize"
   'checks
   checks
   'status
   "passed"
   'models
   models
   'equivalence
   "independent product over interval boundaries"))

(module+
 test
 (define result (run-tests))
 (unless (or
          (equal? (hash-ref result 'status #f) "passed")
          (and (hash-has-key? result 'fail) (equal? (hash-ref result 'fail) 0)))
   (error 'test "Checks failed" result)))
