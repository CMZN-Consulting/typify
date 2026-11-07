#lang racket/base

(require racket/list racket/string "../src/diagnostics.rkt" "../src/language.rkt")

(provide run-tests)

(define (source clauses (defaults "") (extra ""))
  (string-append
   "#lang typify\ndeclare : Grants\ncap : length = 64\n"
   "is : path = /\\/[a-z]+/\nis : mode = \"ro\" | \"rw\"\n"
   "term : vault : path mode\nform : vault : <path> = <mode>\ndoes : vault <path> <mode>\n"
   defaults
   "\n"
   extra
   "\n"
   clauses
   "\nend!\n"))

(define (read-body text) (hash-ref (read-declarations text #:file "counts.typify") "Grants"))

(define (diagnostics thunk)
  (with-handlers ((exn:fail:typify? exn:fail:typify-diagnostics)) (thunk) '()))

(define (run-tests)
  (define checks 0)
  (define failed 0)
  (define details '())
  (define (check name thunk expected)
    (set! checks (add1 checks))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! failed (add1 failed))
         (set! details (cons (hash 'name name 'error (exn-message failure)) details)))))
     (define actual (thunk))
     (unless (equal? actual expected)
       (set! failed (add1 failed))
       (set! details (cons (hash 'name name 'expected expected 'actual actual) details)))))
  (define keyed (read-body (source "count : vault = 0..1 by 1")))
  (define (checked body text) (check-program/diagnostics text body #:file "grants.data"))
  (check
   "distinct-paths-are-separate-groups"
   (lambda () (hash-ref (checked keyed "vault : /a = ro\nvault : /b = rw") 'valid))
   #t)
  (check
   "same-path-mixed-mode-is-one-group"
   (lambda ()
     (define report (checked keyed "vault : /a = ro\nvault : /a = rw"))
     (define item (car (hash-ref report 'diagnostics)))
     (list
      (diagnostic-code item)
      (diagnostic-data item)
      (src-span-start-line (diagnostic-range item))
      (map (lambda (origin) (hash-ref origin 'line)) (diagnostic-related item))))
   (list
    "E_PROGRAM_COUNT"
    (hash 'constructor "vault" 'expected "0..1" 'actual 2 'key '("/a"))
    2
    '(1 2)))
  (check
   "different-surface-forms-share-the-constructor-group"
   (lambda ()
     (define body
       (read-body
        (source "count : vault = 0..1 by 1" "" "form : readonly : <path>\ndoes : vault <path> ro")))
     (map diagnostic-code (hash-ref (checked body "vault : /a = rw\nreadonly : /a") 'diagnostics)))
   '("E_PROGRAM_COUNT"))
  (check
   "one-global-and-one-keyed-constraint-coexist"
   (lambda ()
     (define body (read-body (source "count : vault = 1..*\ncount : vault = 0..1 by 1")))
     (list
      (language-counts body)
      (language-keyed-counts body)
      (hash-ref (checked body "vault : /a = ro\nvault : /b = rw") 'valid)
      (map diagnostic-code (hash-ref (checked body "") 'diagnostics))))
   (list
    (hash "vault" "1..*")
    (hash "vault" (hash 'multiplicity "0..1" 'width 1))
    #t
    '("E_PROGRAM_COUNT")))
  (check
   "keyed-required-count-does-not-invent-a-group"
   (lambda () (hash-ref (checked (read-body (source "count : vault = 1 by 1")) "") 'valid))
   #t)
  (check
   "explicit-zero-key-width-is-unkeyed"
   (lambda ()
     (define body (read-body (source "count : vault = 1 by 0")))
     (list
      (language-counts body)
      (language-keyed-counts body)
      (map diagnostic-code (hash-ref (checked body "") 'diagnostics))))
   (list (hash "vault" "1") (hash) '("E_PROGRAM_COUNT")))
  (for
   ((clauses
     (in-list
      '("count : vault = 1\ncount : vault = 0..1 by 0"
        "count : vault = 0..1 by 1\ncount : vault = * by 2")))
    (index (in-naturals)))
   (check
    (format "second-count-of-same-kind-~a-is-duplicate" index)
    (lambda ()
      (map
       (lambda (item) (hash-ref (diagnostic-data item) 'kind))
       (diagnostics (lambda () (read-body (source clauses))))))
    '("duplicate")))
  (check
   "key-width-cannot-exceed-signature"
   (lambda ()
     (define item (car (diagnostics (lambda () (read-body (source "count : vault = 1 by 3"))))))
     (diagnostic-data item))
   (hash 'kind "syntax" 'actual "count : vault = 1 by 3"))
  (check
   "keyed-only-count-does-not-close-a-restricting-form"
   (lambda ()
     (map
      diagnostic-code
      (diagnostics (lambda () (read-body (source "count : vault = 1 by 1" "restricts!"))))))
   '("E_RESTRICTS"))
  (check
   "defaults-of-distinct-keys-are-legal-and-counted"
   (lambda ()
     (define body
       (read-body
        (source "count : vault = 1 by 1" "default : vault : /a = ro\ndefault : vault : /b = rw")))
     (define report (checked body ""))
     (list
      (hash-ref report 'valid)
      (hash-ref report 'terms)
      (map (lambda (origin) (hash-ref origin 'kind)) (hash-ref report 'origins))))
   '(#t (("vault" "/a" "ro") ("vault" "/b" "rw")) ("default" "default")))
  (check
   "same-default-key-conflicts-at-declaration-time"
   (lambda ()
     (define item
       (car
        (diagnostics
         (lambda ()
           (read-body
            (source
             "count : vault = 0..1 by 1"
             "default : vault : /a = ro\ndefault : vault : /a = rw"))))))
     (diagnostic-data item))
   (hash 'kind "conflict" 'actual "default : vault : /a = rw"))
  (check
   "computed-equal-keys-are-compared-after-evaluation"
   (lambda ()
     (define body
       (read-body
        (source
         "count : vault = 0..1 by 1"
         ""
         "form : nested : <mode>\ndoes : vault (join / a) <mode>")))
     (map diagnostic-data (hash-ref (checked body "vault : /a = ro\nnested : rw") 'diagnostics)))
   (list (hash 'constructor "vault" 'expected "0..1" 'actual 2 'key '("/a"))))
  (check
   "false-guard-precedes-computed-key-context-evaluation"
   (lambda ()
     (define body
       (read-body
        (source
         "count : vault = 0..1 by 1\nguard : vault = (exists /absent)"
         ""
         "term : workdir : path\nform : computed!\ndoes : vault (context workdir) rw")))
     (define report
       (check-program/diagnostics
        "computed!"
        body
        #:facts
        (lambda (name args origin) (values #f #f))))
     (list (hash-ref report 'valid) (hash-ref report 'guarded?) (hash-ref report 'terms)))
   '(#t #t ()))
  (check
   "library-valued-default-arguments-are-not-literals"
   (lambda ()
     (map
      diagnostic-data
      (diagnostics
       (lambda ()
         (read-body
          (source
           "count : vault = 0..1 by 1"
           ""
           "form : nested : <mode>\ndoes : vault (join / a) <mode>\ndefault : nested : ro"))))))
   (list (hash 'kind "syntax" 'actual "default : nested : ro")))
  (define template-source
    (string-append
     "#lang typify\ndeclare : Template\ncap : length = 64\n"
     "is : seat = /[A-Za-z]+/\nis : token = /[A-Za-z]+/\n"
     "primitive : record : token\neffect : record = external\n"
     "form : record : <token>\ndoes : record <token>\n"
     "ensure : record : identified = (equal <seat> <1>)\nfile : S<seat>/data\nend!\n"))
  (check
   "template-captures-reach-ensure-bindings"
   (lambda ()
     (define body (hash-ref (read-declarations template-source) "Template"))
     (define checked (check-program "record : Alpha" body #:captures (hash "seat" "Alpha")))
     (define expanded
       (primitive-events
        body
        (hash-ref checked 'terms)
        (hash-ref checked 'origins)
        #:captures
        (hash "seat" "Alpha")
        #:spans
        (hash-ref checked '_term-spans)))
     (hash-ref (hash-ref (car (hash-ref expanded 'ensures)) 'bindings) "seat"))
   "Alpha")
  (check
   "a-rule-variable-cannot-shadow-a-template-capture"
   (lambda ()
     (define text
       (string-replace
        template-source
        "form : record : <token>"
        "term : relay : token\nrule : relay <seat> -> record <seat>\nform : record : <token>"))
     (map diagnostic-data (diagnostics (lambda () (read-declarations text)))))
   (list (hash 'kind "duplicate" 'actual "<seat>")))
  (check
   "count-key-is-required-and-empty-for-plain-counts"
   (lambda ()
     (list
      (diagnostic-data-projection
       "E_PROGRAM_COUNT"
       (hash 'constructor "vault" 'expected "0..1" 'actual 2 'key '("/a")))
      (diagnostic-data-projection
       "E_PROGRAM_COUNT"
       (hash 'constructor "vault" 'expected "1" 'actual 0 'key '()))))
   (list
    (hash 'constructor "vault" 'expected "0..1" 'actual 2 'key '("/a"))
    (hash 'constructor "vault" 'expected "1" 'actual 0 'key '())))
  (hash
   'suite
   "keyed-counts"
   'count
   checks
   'pass
   (- checks failed)
   'fail
   failed
   'details
   (reverse details)))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail)) (error 'keyed-counts "~s" result)))
