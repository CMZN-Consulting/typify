#lang racket/base

(require racket/list racket/string)

(provide optimal-string-alignment-distance near-words)

(define (optimal-string-alignment-distance left right)
  (unless (string? left) (raise-argument-error 'optimal-string-alignment-distance "string?" left))
  (unless (string? right) (raise-argument-error 'optimal-string-alignment-distance "string?" right))
  (define-values
   (rows columns)
   (if (< (string-length left) (string-length right)) (values right left) (values left right)))
  (define width (string-length columns))
  (for/fold
   ((older (make-vector (add1 width) 0))
    (previous (for/vector ((column (in-range (add1 width)))) column))
    (current (make-vector (add1 width) 0))
    #:result
    (vector-ref previous width))
   ((row (in-range 1 (add1 (string-length rows)))))
   (vector-set! current 0 row)
   (for
    ((column (in-range 1 (add1 width))))
    (define cost (if (char=? (string-ref rows (sub1 row)) (string-ref columns (sub1 column))) 0 1))
    (define distance
      (min
       (add1 (vector-ref previous column))
       (add1 (vector-ref current (sub1 column)))
       (+ (vector-ref previous (sub1 column)) cost)))
    (vector-set!
     current
     column
     (if (and (> row 1)
              (> column 1)
              (char=? (string-ref rows (sub1 row)) (string-ref columns (- column 2)))
              (char=? (string-ref rows (- row 2)) (string-ref columns (sub1 column))))
       (min distance (add1 (vector-ref older (- column 2))))
       distance)))
   (values previous current older)))

(define (hyphen-parts text)
  (filter (lambda (part) (not (string=? part ""))) (string-split text "-" #:trim? #f)))

(define (near-words asked candidates)
  (unless (string? asked) (raise-argument-error 'near-words "string?" asked))
  (unless (and (list? candidates) (andmap string? candidates))
    (raise-argument-error 'near-words "list of strings" candidates))
  (define words (remove-duplicates candidates))
  (define limit (min 2 (quotient (max (string-length asked) 3) 3)))
  (define nearest
    (for/fold
     ((best (add1 limit)) (selected '()) #:result selected)
     ((word (in-list words)) #:when (<= (abs (- (string-length asked) (string-length word))) limit))
     (define distance (optimal-string-alignment-distance asked word))
     (cond
      ((> distance limit) (values best selected))
      ((< distance best) (values distance (list word)))
      ((= distance best) (values best (cons word selected)))
      (else (values best selected)))))
  (sort
   (if (pair? nearest)
     nearest
     (let ((parts (hyphen-parts asked)))
       (for/list
        ((word (in-list words))
         #:when
         (ormap (lambda (part) (member part parts)) (hyphen-parts word)))
        word)))
   string<?))
