#lang racket/base

(require racket/list racket/string)

(provide meta-classes
         meta-forms
         current-meta-classes
         current-meta-forms
         meta-source
         library-arities)

(define meta-classes
  (hash
   "name"
   "/<letter>(<letter>|[0-9_-])*/"
   "value"
   "/.*/"
   "number"
   "/0|[1-9][0-9]*/"
   "ordering"
   "strict | free"
   "multiplicity"
   "one | at-most-one | at-least-one | any"))

(define (entry kind production constructor arguments example)
  (hash
   'kind
   kind
   'production
   production
   'constructor
   constructor
   'arguments
   arguments
   'example
   example))

(define meta-forms
  (list
   (entry 'declare "declare : <name>" "declare-language" '("name") "declare : Catalog")
   (entry 'end "end!" "close-language" '() "end!")
   (entry 'version "version : <number>" "set-version" '("number") "version : 1")
   (entry 'comment "comment : <value>" "set-comment" '("value") "comment : #")
   (entry
    'class
    "is : <name> = <value>"
    "declare-class"
    '("name" "value")
    "is : category = book | journal")
   (entry 'form "form : <value>" "declare-form" '("value") "form : item : <category>")
   (entry 'does "does : <value>" "declare-meaning" '("value") "does : item <category>")
   (entry 'example "example : <value>" "declare-example" '("value") "example : item : book")
   (entry
    'term
    "term : <name> : <value>"
    "declare-constructor"
    '("name" "value")
    "term : item : category")
   (entry 'term-zero "term : <name>" "declare-nullary-constructor" '("name") "term : stop")
   (entry
    'primitive
    "primitive : <name> : <value>"
    "declare-primitive"
    '("name" "value")
    "primitive : output : category")
   (entry
    'primitive-zero
    "primitive : <name>"
    "declare-nullary-primitive"
    '("name")
    "primitive : stop")
   (entry 'rule "rule : <value>" "declare-rule" '("value") "rule : item <x> -> output <x>")
   (entry
    'count
    "count : <name> = <multiplicity>"
    "declare-count"
    '("name" "multiplicity")
    "count : item = at-least-one")
   (entry 'order "order : <ordering>" "set-order" '("ordering") "order : free")
   (entry 'file "file : <value>" "set-file" '("value") "file : catalog.typify")
   (entry 'required "required!" "require-file" '() "required!")
   (entry 'optional "optional!" "allow-missing-file" '() "optional!")
   (entry 'default "default : <value>" "declare-default" '("value") "default : item : book")
   (entry 'sees "sees : <name>" "declare-reference" '("name") "sees : Catalog")
   (entry 'note "note : <name>" "declare-note" '("name") "note : quoting")
   (entry 'cap "cap : <name> = <number>" "declare-cap" '("name" "number") "cap : states = 4096")
   (entry 'cell "cell : <name> = <value>" "declare-cell" '("name" "value") "cell : mounts = shadow")
   (entry
    'writes
    "writes : <name> : <value>"
    "declare-write"
    '("name" "value")
    "writes : bind : mounts = put 1")
   (entry
    'reads
    "reads : <name> : <value>"
    "declare-read"
    '("name" "value")
    "reads : bind : exists = reject")
   (entry
    'ensure
    "ensure : <name> : <value>"
    "declare-invariant"
    '("name" "value")
    "ensure : visible : (bound path)")))

(define library-arities
  (hash
   "parent"
   '(1)
   "join"
   '(2)
   "split"
   '(2)
   "ancestors"
   '(1)
   "prefix"
   '(2)
   "suffix"
   '(2)
   "equal"
   '(2)
   "concat"
   '(2)))

(define current-meta-classes (make-parameter meta-classes))

(define current-meta-forms (make-parameter meta-forms))

(define (signature-lines row)
  (define constructor (hash-ref row 'constructor))
  (define arguments (hash-ref row 'arguments))
  (define slots
    (let loop ((remaining (hash-ref row 'production)) (names '()))
      (define start (string-index remaining #\<))
      (if start
        (let ((finish (string-index remaining #\>)))
          (loop
           (substring remaining (add1 finish))
           (append names (list (substring remaining (add1 start) finish)))))
        names)))
  (list
   (string-append
    "primitive : "
    constructor
    (if (null? arguments) "" (string-append " : " (string-join arguments " "))))
   (string-append "form : " (hash-ref row 'production))
   (string-append
    "does : "
    constructor
    (if (null? slots)
      ""
      (string-append
       " "
       (string-join (map (lambda (name) (string-append "<" name ">")) slots) " "))))
   (string-append "example : " (hash-ref row 'example))))

(define (string-index text character)
  (for/first
   ((index (in-range (string-length text))) #:when (char=? (string-ref text index) character))
   index))

(define (meta-source)
  (string-append
   (string-join
    (append
     '("#lang typify" "" "declare : Typify" "version : 1")
     (for/list
      ((name (in-list (sort (hash-keys (current-meta-classes)) string<?))))
      (string-append "is : " name " = " (hash-ref (current-meta-classes) name)))
     (append-map signature-lines (current-meta-forms))
     '("end!"))
    "\n")
   "\n"))
