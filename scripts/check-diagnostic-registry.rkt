#lang racket/base

(require json racket/list racket/runtime-path racket/string)

(provide compare-diagnostic-registries)

(define-runtime-path candidate-root "..")

(define (read-codes checkout)
  (define path (build-path (path->complete-path checkout) "src" "diagnostics.rkt"))
  (define codes (dynamic-require path 'diagnostic-codes))
  (unless (and
           (pair? codes)
           (list? codes)
           (andmap non-empty-string? codes)
           (not (check-duplicates codes)))
    (error 'check-diagnostic-registry "invalid diagnostic-codes in ~a" path))
  (sort codes string<?))

(define (compare-diagnostic-registries previous-checkout (candidate-checkout candidate-root))
  (define previous (read-codes previous-checkout))
  (define current (read-codes candidate-checkout))
  (define removed (remove* current previous))
  (hasheq
   'pass
   (null? removed)
   'previousCount
   (length previous)
   'currentCount
   (length current)
   'removed
   removed
   'added
   (remove* previous current)))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (unless (<= 1 (length arguments) 2)
   (displayln
    "usage: racket scripts/check-diagnostic-registry.rkt PREVIOUS_CHECKOUT [CANDIDATE_CHECKOUT]"
    (current-error-port))
   (exit 2))
 (with-handlers
  ((exn:fail?
    (lambda (failure)
      (eprintf "Cannot compare diagnostic registries: ~a\n" (exn-message failure))
      (exit 2))))
  (define result (apply compare-diagnostic-registries arguments))
  (write-json result)
  (newline)
  (exit (if (hash-ref result 'pass) 0 1))))
