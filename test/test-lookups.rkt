#lang racket/base

(require racket/list racket/string "../src/diagnostics.rkt" "../src/language.rkt")

(provide run-tests)

(define (target name signature)
  (string-append
   "declare : "
   name
   "\ncap : length = 64\nis : word = /[A-Za-z]+/\n"
   "term : entry : "
   signature
   "\nend!\n"))

(define (consumer reference (example ""))
  (string-append
   "declare : Consumer\ncap : length = 64\nis : word = /[A-Za-z]+/\n"
   "primitive : use : word\neffect : use = external\nform : use!\n"
   "does : use (context "
   reference
   ")\n"
   example
   "\nend!\n"))

(define (read-model . bodies)
  (read-declarations
   (string-append "#lang typify\n" (string-join bodies "\n"))
   #:file
   "lookups.typify"))

(define (run-tests)
  (define count 0)
  (define failures '())
  (define (check name thunk expected)
    (set! count (add1 count))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! failures (cons (hash 'name name 'error (exn-message failure)) failures)))))
     (define actual (thunk))
     (unless (equal? actual expected)
       (set! failures (cons (hash 'name name 'expected expected 'actual actual) failures)))))
  (define (refusal thunk)
    (with-handlers ((exn:fail:typify? exn:fail:typify-diagnostics)) (thunk) '()))
  (check
   "a-bare-name-resolves-only-one-declaring-body"
   (lambda ()
     (define model (read-model (target "Only" "word") (consumer "entry")))
     (lookup-reference (hash-ref model "Consumer") "entry"))
   "Only.entry")
  (check
   "ambiguous-bare-names-report-the-declaring-bodies"
   (lambda ()
     (define item
       (car
        (refusal
         (lambda ()
           (read-model (target "First" "word") (target "Second" "word") (consumer "entry"))))))
     (list
      (diagnostic-data item)
      (sort
       (map (lambda (related) (hash-ref related 'language)) (diagnostic-related item))
       string<?)))
   (list (hash 'kind "reference" 'actual "entry") '("First" "Second")))
  (check
   "qualified-lookups-select-a-body-despite-shared-constructor-spelling"
   (lambda ()
     (define model
       (read-model
        (target "First" "word")
        (target "Second" "word")
        (consumer "Second.entry" "example : use!")))
     (lookup-reference (hash-ref model "Consumer") "Second.entry"))
   "Second.entry")
  (for
   ((signature '("word word" "")) (index (in-naturals)))
   (check
    (format "lookup-arity-~a-is-exactly-unary" index)
    (lambda ()
      (define body
        (if (equal? signature "")
          "declare : Target\nterm : entry\nend!\n"
          (target "Target" signature)))
      (map
       (lambda (item) (hash-ref (diagnostic-data item) 'kind))
       (refusal (lambda () (read-model body (consumer "Target.entry"))))))
    '("arity")))
  (check
   "examples-execute-pure-library-subtrees"
   (lambda ()
     (map
      diagnostic-code
      (refusal
       (lambda ()
         (read-model
          (string-append
           "declare : Example\nterm : output : canonical-path\n"
           "form : parent : <canonical-path>\ndoes : output (parent <canonical-path>)\n"
           "example : parent : /\nend!\n"))))))
   '("E_LIBRARY_PARENT"))
  (check
   "examples-bind-template-shortest-witnesses-before-library-evaluation"
   (lambda ()
     (define items
       (refusal
        (lambda ()
          (read-model
           (string-append
            "declare : Witness\ncap : length = 64\nis : seat = \"bad\"\n"
            "is : result = \"/good\"\nprimitive : output : result\neffect : output = external\n"
            "form : ready!\ndoes : output (parent (concat / <seat>))\n"
            "example : ready!\nfile : Room<seat>/data\nend!\n")))))
     (map diagnostic-data items))
   (list (hash 'kind "result" 'constructor "output" 'class "result" 'actual "/")))
  (check
   "notes-are-associated-with-a-declared-constructor"
   (lambda ()
     (define model
       (read-model
        "declare : Notes\nterm : ready\nform : ready!\ndoes : ready\nnote : ready = host check\nend!\n"))
     (language-notes (hash-ref model "Notes")))
   '("host check"))
  (define partition-source
    (string-append
     "declare : Partition\ncap : length = 64\nis : seat = \"A\"\n"
     "primitive : bind : seat canonical-path\ncell : mounts = shadow stacking\n"
     "writes : bind : mounts = put 2\nform : bind : <canonical-path>\n"
     "does : bind <seat> <canonical-path>\nfile : Seat<seat>/data\nend!\n"))
  (check
   "stacking-partitions-follow-the-holding-template-order"
   (lambda () (language-template-names (hash-ref (read-model partition-source) "Partition")))
   '("seat"))
  (check
   "equal-literal-values-do-not-replace-template-capture-provenance"
   (lambda ()
     (define item
       (car
        (refusal
         (lambda ()
           (read-model (string-replace partition-source "does : bind <seat>" "does : bind A"))))))
     (list (diagnostic-data item) (src-span-start-column (diagnostic-range item))))
   (list (hash 'kind "partition" 'actual "bind A <canonical-path>") 1))
  (check
   "partitioned-delete-cannot-omit-its-partition-components"
   (lambda ()
     (define text
       (string-replace
        partition-source
        "file : Seat<seat>/data"
        (string-append
         "primitive : clear\nwrites : clear : mounts = delete 0\n"
         "form : clear!\ndoes : clear\nfile : Seat<seat>/data")))
     (map diagnostic-data (refusal (lambda () (read-model text)))))
   (list (hash 'kind "partition" 'actual "clear")))
  (hash
   'suite
   "lookups"
   'count
   count
   'pass
   (- count (length failures))
   'fail
   (length failures)
   'details
   (reverse failures)))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail)) (error 'lookups "~s" result)))
