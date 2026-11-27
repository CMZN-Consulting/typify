#lang racket/base

(require "syntax.rkt")

(provide canonical-bytes first-different-line)

(define (decode bytes) (bytes->string/utf-8 bytes (integer->char 65533)))

(define (blank-byte? byte) (or (= byte 32) (= byte 9)))

(define (trim-bytes bytes)
  (define size (bytes-length bytes))
  (define start
    (let loop ((index 0))
      (if (and (< index size) (blank-byte? (bytes-ref bytes index))) (loop (add1 index)) index)))
  (define end
    (let loop ((index size))
      (if (and (> index start) (blank-byte? (bytes-ref bytes (sub1 index))))
        (loop (sub1 index))
        index)))
  (subbytes bytes start end))

(define (find-bytes bytes needle)
  (define width (bytes-length needle))
  (for/first
   ((index (in-range (add1 (- (bytes-length bytes) width))))
    #:when
    (equal? (subbytes bytes index (+ index width)) needle))
   index))

(define (line-ending bytes start)
  (for/first
   ((index (in-range start (bytes-length bytes))) #:when (= (bytes-ref bytes index) 10))
   index))

(define (text-lines bytes #:ends? (ends? #f))
  (define size (bytes-length bytes))
  (let loop ((start 0) (lines '()))
    (define ending (line-ending bytes start))
    (cond
     (ending
      (define end
        (cond
         (ends? (add1 ending))
         ((and (> ending start) (= (bytes-ref bytes (sub1 ending)) 13)) (sub1 ending))
         (else ending)))
      (loop (add1 ending) (cons (subbytes bytes start end) lines)))
     ((= start size) (reverse lines))
     (else (reverse (cons (subbytes bytes start size) lines))))))

(define (separated body separator spelling record)
  (define at (find-bytes body separator))
  (define left (trim-bytes (subbytes body 0 at)))
  (define right (trim-bytes (subbytes body (+ at (bytes-length separator)))))
  (unless (and
           (equal? (decode left) (line-record-left record))
           (equal? (decode right) (line-record-right record)))
    (error 'fmt "a payload differs from the reader's reading of its line"))
  (if (zero? (bytes-length right))
    (bytes-append left spelling)
    (bytes-append left spelling #" " right)))

(define (canonical-line line prefix)
  (define record (car (read-lines (decode line) #:comment-prefix prefix)))
  (define body (trim-bytes line))
  (case (line-record-kind record)
    ((blank) #f)
    ((colon) (separated body #":" #" :" record))
    ((arrow) (separated body #"->" #" ->" record))
    (else body)))

(define (canonical-bytes text prefix)
  (define lines
    (let loop ((remaining (text-lines text)) (kept '()) (gap? #f))
      (cond
       ((null? remaining) (reverse kept))
       (else
        (define line (canonical-line (car remaining) prefix))
        (if line
          (loop (cdr remaining) (cons line (if gap? (cons #"" kept) kept)) #f)
          (loop (cdr remaining) kept (pair? kept)))))))
  (apply bytes-append (for/list ((line (in-list lines))) (bytes-append line #"\n"))))

(define (first-different-line original canonical)
  (let loop ((left (text-lines original #:ends? #t))
             (right (text-lines canonical #:ends? #t))
             (line 1))
    (cond
     ((and (null? left) (null? right)) #f)
     ((or (null? left) (null? right) (not (equal? (car left) (car right)))) line)
     (else (loop (cdr left) (cdr right) (add1 line))))))
