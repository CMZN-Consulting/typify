#lang racket/base

(require racket/list racket/string "../src/diagnostics.rkt" "../src/language.rkt" "../src/plan.rkt")

(provide run-tests)

(define requested 10000)

(define (join-words values) (string-join values " "))

(define (quoted value) (string-append "\"" value "\""))

(define (capture name) (string-append "<" name ">"))

(define (declaration name rows)
  (string-append "#lang typify\ndeclare : " name "\n" (string-join rows "\n") "\nend!\n"))

(define (parameters index)
  (define width (add1 (modulo (quotient index 3) 3)))
  (hash
   'index
   index
   'name
   (format "Generated~a" index)
   'law
   (list-ref '(shadow first join) (modulo index 3))
   'width
   width
   'rule-depth
   (modulo (quotient index 9) 4)
   'fuel
   (add1 (modulo (quotient index 36) 4))
   'loop-depth
   (add1 (modulo (quotient index 144) 2))
   'delete-width
   (modulo (quotient index 288) (add1 width))
   'alias-depth
   (modulo (quotient index 432) 4)
   'put-stage
   (modulo (quotient index 864) 3)
   'delete-stage
   (modulo (quotient index 2592) 3)
   'emit-stage
   (modulo (quotient index 7776) 3)
   'stacking?
   (odd? (quotient index 27))))

(define (case-data params)
  (define index (hash-ref params 'index))
  (define width (hash-ref params 'width))
  (define delete-width (hash-ref params 'delete-width))
  (define aliases (hash-ref params 'alias-depth))
  (define depth (hash-ref params 'rule-depth))
  (define fuel (hash-ref params 'fuel))
  (define loops (hash-ref params 'loop-depth))
  (define stacking? (hash-ref params 'stacking?))
  (define left (format "g~aa" index))
  (define right (format "g~ab" index))
  (define path-left (format "/g~a/child" index))
  (define path-right (format "/g~a" index))
  (define key-a
    (if stacking? (append (make-list (sub1 width) left) (list path-left)) (make-list width left)))
  (define key-b
    (if stacking?
      (append (if (= width 1) '() (cons right (make-list (- width 2) left))) (list path-right))
      (cons right (make-list (sub1 width) left))))
  (define slot-names (for/list ((i (in-range width))) (format "p~a" i)))
  (define clear-names (for/list ((i (in-range delete-width))) (format "q~a" i)))
  (define key-class (if (zero? aliases) "key-base" (format "key~a" aliases)))
  (define variables (append (map capture slot-names) '("<v>")))
  (define signature (append slot-names '("payload")))
  (define iteration-count (add1 (modulo index fuel)))
  (define components
    (cons (format "g~a" index) (for/list ((i (in-range (sub1 iteration-count)))) (format "c~a" i))))
  (define path (string-append "/" (string-join components "/")))
  (define rows
    (append
     (list
      "cap : states = 64"
      "cap : length = 32"
      (format "cap : depth = ~a" (+ aliases 2))
      (format "is : key-base = ~a | ~a" (quoted left) (quoted right)))
     (for/list
      ((i (in-range 1 (add1 aliases))))
      (format "is : key~a = ~a" i (if (= i 1) "key-base" (format "key~a" (sub1 i)))))
     (if stacking?
       (list (format "is : path-base = ~a | ~a" (quoted path-left) (quoted path-right)))
       '())
     (for/list
      ((name (in-list slot-names)) (position (in-naturals 1)))
      (format "is : ~a = ~a" name (if (and stacking? (= position width)) "path-base" key-class)))
     (for/list
      ((name (in-list clear-names)) (position (in-naturals 1)))
      (format "is : ~a = ~a" name (if (and stacking? (= position width)) "path-base" key-class)))
     (list
      "is : payload = \"ro\" | \"rw\""
      "is : tick-path = canonical-path"
      (string-append
       "cell : entries = "
       (if (eq? (hash-ref params 'law) 'join) "join modes" (symbol->string (hash-ref params 'law)))
       (if stacking? " stacking" "")))
     (if (eq? (hash-ref params 'law) 'join) '("join : modes : \"ro\" \"rw\" = \"rw\"") '())
     (list
      (string-append "primitive : put : " (join-words signature))
      (if (zero? delete-width)
        "primitive : clear"
        (string-append "primitive : clear : " (join-words clear-names)))
      "primitive : emit : value"
      "effect : emit = external"
      (format "writes : put : entries = put ~a" width)
      (format "writes : clear : entries = delete ~a" delete-width)
      (format "stage : put = ~a" (hash-ref params 'put-stage))
      (format "stage : clear = ~a" (hash-ref params 'delete-stage))
      (format "stage : emit = ~a" (hash-ref params 'emit-stage)))
     (for/list
      ((i (in-range 1 (add1 depth))))
      (format "term : step~a : ~a" i (join-words signature)))
     (for/list
      ((i (in-range 1 (add1 depth))))
      (format
       "rule : step~a ~a -> ~a ~a"
       i
       (join-words variables)
       (if (= i 1) "put" (format "step~a" (sub1 i)))
       (join-words variables)))
     (list
      "term : batch : tick-path"
      (format "fuel : batch = ~a" fuel)
      (if (= loops 1)
        "rule : batch <path> -> (each <x> in (split <path>) (emit <x>))"
        "rule : batch <path> -> (each <x> in (split <path>) (each <y> in (split <path>) (emit (concat <x> <y>))))")
      (format "form : set : ~a = <payload>" (string-join (map capture slot-names) " / "))
      (format
       "does : ~a ~a <payload>"
       (if (zero? depth) "put" (format "step~a" depth))
       (join-words (map capture slot-names)))
      (if (zero? delete-width)
        "form : clear!"
        (format "form : clear : ~a" (string-join (map capture clear-names) " / ")))
      (if (zero? delete-width)
        "does : clear"
        (format "does : clear ~a" (join-words (map capture clear-names)))))
     '("form : tick : <tick-path>" "does : batch <tick-path>")))
  (define program
    (string-join
     (list
      (format "set : ~a = ro" (string-join key-a " / "))
      (format "set : ~a = rw" (string-join key-a " / "))
      (format "set : ~a = ro" (string-join key-b " / "))
      (if (zero? delete-width)
        "clear!"
        (format "clear : ~a" (string-join (take key-a delete-width) " / ")))
      (string-append "tick : " path))
     "\n"))
  (values (declaration (hash-ref params 'name) rows) program key-a key-b components))

(define (oracle params key-a key-b components)
  (define events
    (append
     (list
      (list (hash-ref params 'put-stage) 1 (append '("put") key-a '("ro")))
      (list (hash-ref params 'put-stage) 2 (append '("put") key-a '("rw")))
      (list (hash-ref params 'put-stage) 3 (append '("put") key-b '("ro")))
      (list
       (hash-ref params 'delete-stage)
       4
       (cons "clear" (take key-a (hash-ref params 'delete-width)))))
     (for/list
      ((value
        (in-list
         (if (= (hash-ref params 'loop-depth) 1)
           components
           (for*/list ((x (in-list components)) (y (in-list components))) (string-append x y))))))
      (list (hash-ref params 'emit-stage) 5 (list "emit" value)))))
  (define ordered
    (append-map (lambda (stage) (filter (lambda (event) (= (car event) stage)) events)) '(0 1 2)))
  (define state (hash))
  (define (origin line) (hash 'kind "program" 'path "generated-program.typify" 'line line))
  (for
   ((event (in-list ordered)))
   (define term (caddr event))
   (cond
    ((equal? (car term) "clear")
     (define prefix (cdr term))
     (set! state
       (for/hash
        (((key entry) (in-hash state))
         #:unless
         (for/and ((x (in-list prefix)) (y (in-list key))) (equal? x y)))
        (values key entry))))
    ((equal? (car term) "put")
     (define key (take (cdr term) (hash-ref params 'width)))
     (define value (last term))
     (define prior (hash-ref state key #f))
     (unless (and prior (eq? (hash-ref params 'law) 'first))
       (define stored
         (if (and prior (eq? (hash-ref params 'law) 'join))
           (if (or (equal? value "rw") (equal? (car (hash-ref prior 'value)) "rw")) "rw" "ro")
           value))
       (set! state
         (hash-set
          state
          key
          (hash 'key key 'value (list stored) 'masked #f 'origin (origin (cadr event)))))))))
  (values
   (for/list
    ((event (in-list ordered)))
    (define term (caddr event))
    (if (equal? (car term) "emit") (hash 'term term 'unchecked #t) term))
   (hash
    "entries"
    (for/list
     ((key (in-list (sort (hash-keys state) string<? #:key (lambda (key) (string-join key "~"))))))
     (hash-ref state key)))
   (map (lambda (event) (origin (cadr event))) ordered)))

(define (run-tests)
  (define checks 0)
  (define failures 0)
  (define attempted 0)
  (define failed-cases 0)
  (define details '())
  (define hashes (make-hash))
  (define bound-counts (make-hash))
  (define schema-counts (make-hash))
  (define maximum-plan 0)
  (define maximum-state 0)
  (define plan-violations 0)
  (define state-violations 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (set! failures (add1 failures))
      (when (< (length details) 20)
        (set! details (cons (hash 'context context 'expected expected 'actual actual) details)))))
  (for
   ((index (in-range requested)) #:break (>= failed-cases 20))
   (set! attempted (add1 attempted))
   (define before failures)
   (define params (parameters index))
   (define-values (source program key-a key-b components) (case-data params))
   (hash-set! hashes (content-hash source) #t)
   (with-handlers
    ((exn:fail?
      (lambda (error)
        (check
         (exn-message error)
         "no exception"
         (hash 'parameters params 'declaration source 'program program)))))
    (define language
      (hash-ref
       (read-declarations source #:file "generated-language.typify")
       (hash-ref params 'name)))
    (define report (plan-program language program #:path "generated-program.typify"))
    (define-values
     (expected-plan expected-state expected-origins)
     (oracle params key-a key-b components))
    (define expected-c (expt (hash-ref params 'fuel) (hash-ref params 'loop-depth)))
    (define plan (hash-ref report 'plan))
    (define state-size (length (hash-ref (hash-ref report 'state) "entries")))
    (set! maximum-plan (max maximum-plan (length plan)))
    (set! maximum-state (max maximum-state state-size))
    (when (> (length plan) (hash-ref (hash-ref report 'bound) 'limit))
      (set! plan-violations (add1 plan-violations)))
    (when (> state-size (length plan)) (set! state-violations (add1 state-violations)))
    (hash-update! bound-counts (number->string expected-c) add1 0)
    (hash-update!
     schema-counts
     (format
      "~a/~a/~a/~a/~a/~a"
      (hash-ref params 'law)
      (hash-ref params 'width)
      (hash-ref params 'rule-depth)
      (hash-ref params 'fuel)
      (hash-ref params 'loop-depth)
      (hash-ref params 'stacking?))
     add1
     0)
    (check plan expected-plan (hash 'index index 'field "plan"))
    (check (hash-ref report 'state) expected-state (hash 'index index 'field "state"))
    (check (hash-ref report 'origins) expected-origins (hash 'index index 'field "origins"))
    (check
     (declaration-bound language)
     (hash 'c expected-c 'k 1)
     (hash 'index index 'field "declaration-bound"))
    (check
     (hash-ref report 'bound)
     (hash 'c expected-c 'n 5 'limit (* 5 expected-c))
     (hash 'index index 'field "program-bound"))
    (check
     (hash-ref report 'modelled)
     (format "4 of ~a" (length expected-plan))
     (hash 'index index 'field "modelled"))
    (check
     (<= (length plan) (hash-ref (hash-ref report 'bound) 'limit))
     #t
     (hash 'index index 'field "plan-within-bound"))
    (check (<= state-size (length plan)) #t (hash 'index index 'field "state-within-bound")))
   (when (> failures before) (set! failed-cases (add1 failed-cases))))
  (define canary #f)
  (with-handlers
   ((exn:fail? (lambda (error) (check (exn-message error) "no canary exception" "canary"))))
   (define language
     (hash-ref
      (read-declarations
       (declaration
        "Canary"
        '("primitive : emit : value"
          "effect : emit = external"
          "term : pair : value"
          "rule : pair <x> -> emit <x> ; emit <x>"
          "form : run : <value>"
          "does : pair <value>")))
      "Canary"))
   (define report (plan-program language "run : sample" #:path "canary-program.typify"))
   (define actual (length (hash-ref report 'plan)))
   (set! canary
     (hash
      'kind
      "understated-c-per-line"
      'actual
      actual
      'false-limit
      1
      'printed-bound
      (hash-ref report 'bound)
      'found
      (> actual 1)))
   (check
    (hash-ref report 'plan)
    (list
     (hash 'term '("emit" "sample") 'unchecked #t)
     (hash 'term '("emit" "sample") 'unchecked #t))
    "hand-derived canary plan")
   (check (hash-ref report 'bound) (hash 'c 2 'n 1 'limit 2) "canary true bound")
   (check
    (hash-ref canary 'found)
    #t
    "a false one-step bound has an actual two-step counterexample"))
  (check attempted requested "all generated declarations were attempted")
  (check (hash-count hashes) requested "generated declaration texts are distinct")
  (hash
   'suite
   "plan-generated"
   'count
   checks
   'pass
   (- checks failures)
   'fail
   failures
   'details
   (reverse details)
   'gates
   (hash
    'plan-state-bounds
    (hash
     'requested
     requested
     'total
     attempted
     'failures
     failed-cases
     'complete
     (= attempted requested)
     'distinct-declarations
     (hash-count hashes)
     'plan-violations
     plan-violations
     'state-violations
     state-violations
     'canary-found
     (and canary (hash-ref canary 'found))
     'scope
     "Distinct names and literal key domains; no renaming-only declaration count. Independent staged plan and state oracle."
     'independent-oracle
     "Hand-built primitive events; finite shadow/first/max-join fold; formula fuel^loop-depth; no reader-derived expected values."
     'bound-counts
     bound-counts
     'schema-counts
     schema-counts
     'maximum-plan
     maximum-plan
     'maximum-state
     maximum-state
     'canary
     canary))))

(module+ test (run-tests))
