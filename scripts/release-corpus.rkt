#lang racket/base

(require json net/base64 racket/cmdline racket/file racket/list "../test/blind-corpus.rkt")

(define (check-native-corpus corpus transcript)
  (define rows (file->list transcript read-json #:mode 'text))
  (unless (and (= (length rows) 1) (list? (car rows)))
    (raise-user-error 'release-corpus "expected a single transcript array"))
  (define inventory (blind-case-inventory corpus))
  (define ids (map (lambda (row) (hash-ref row 'id)) inventory))
  (define captured (car rows))
  (unless (and
           (pair? ids)
           (>= (length captured) (length ids))
           (equal? ids (map (lambda (row) (hash-ref row 'id)) (take captured (length ids)))))
    (raise-user-error 'release-corpus "native transcript prefix differs from the frozen inventory"))
  (define by-id (for/hash ((id (in-list ids)) (row (in-list captured))) (values id row)))
  (define (observe entry)
    (define row (hash-ref by-id (hash-ref entry 'id)))
    (define status (hash-ref row 'exit))
    (define (decode key)
      (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 (hash-ref row key)))))
    (define out (decode 'stdout))
    (define parsed (with-handlers ((exn:fail:read? (lambda (_) #f))) (string->jsexpr out)))
    (define verdict
      (case status
        ((0)
         (if (and (hash? parsed) (equal? (hash-ref parsed 'status #f) "unchecked"))
           "unchecked"
           "accepted"))
        ((1) "refused")
        ((2) "usage_or_io_error")
        (else (raise-user-error 'release-corpus "unexpected reader exit: ~a" status))))
    (hash
     'exit_code
     status
     'verdict
     verdict
     'stdout_json
     parsed
     'stdout_gbnf
     out
     'stdout
     out
     'stderr
     (decode 'stderr)
     'diagnostics
     (if (hash? parsed) (hash-ref parsed 'diagnostics '()) '())))
  (define report (run-blind-corpus #:root corpus #:wire 'stage4 #:observe observe))
  (write-json report)
  (newline)
  (and (stable-v12-replay? report)
       (hash-ref report 'complete)
       (equal? (hash-ref report 'total) (length ids))
       (zero? (hash-ref report 'failures))))

(module+
 main
 (command-line #:args (corpus transcript) (exit (if (check-native-corpus corpus transcript) 0 1))))
