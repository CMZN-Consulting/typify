#lang racket/base

(require racket/list racket/string "classes.rkt")

(provide meta-classes
         meta-forms
         current-meta-classes
         current-meta-forms
         meta-source
         meta-cap-defaults
         library-arities)

(define meta-classes
  (hash
   "name"
   "letter (letter | /[0-9_-]/)*"
   "constructor"
   "name"
   "value"
   "/.*/"
   "text"
   "/.*/"
   "number"
   "/0|[1-9][0-9]*/"
   "ordering"
   "\"strict\" | \"free\""
   "order-mode"
   "\"strict\" | \"free\""
   "prefix"
   "/.*/"
   "production"
   "/.*/"
   "term"
   "/.*/"
   "pattern"
   "/.*/"
   "law"
   "/(shadow|first|join[ ][A-Za-z][A-Za-z0-9_-]*)([ ]stacking)?/"
   "width"
   "/0|[1-9][0-9]*/"
   "limit"
   "\"states\" | \"repetition\" | \"depth\" | \"length\""
   "scope-mode"
   "\"program\" | \"parent\" | \"root\""
   "multiplicity"
   "\"1\" | \"0..1\" | \"1..*\" | \"*\""))

(define meta-cap-defaults
  (hash
   "states"
   (hash-ref default-class-caps 'states)
   "repetition"
   (hash-ref default-class-caps 'repetition)
   "depth"
   (hash-ref default-class-caps 'reference-depth)
   "length"
   (hash-ref default-class-caps 'token-length)))

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
   (entry 'comment "comment : <prefix>" "set-comment" '("prefix") "comment : #")
   (entry
    'class
    "is : <name> = <pattern>"
    "declare-class"
    '("name" "pattern")
    "is : category = \"book\" | \"journal\"")
   (entry 'form "form : <production>" "declare-form" '("production") "form : item : <category>")
   (entry 'does "does : <term>" "declare-meaning" '("term") "does : item <category>")
   (entry 'example "example : <value>" "declare-example" '("value") "example : item : book")
   (entry
    'term
    "term : <name> : <value>"
    "declare-constructor"
    '("name" "value")
    "term : item : category")
   (entry 'term-zero "term : <name>" "declare-nullary-constructor" '("name") "term : stop")
   (entry 'fact "fact : <name>" "declare-fact" '("name") "fact : item")
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
   (entry 'rule "rule : <term>" "declare-rule" '("term") "rule : item <x> -> output <x>")
   (entry
    'count
    "count : <name> = <multiplicity>"
    "declare-count"
    '("name" "multiplicity")
    "count : item = 1..*")
   (entry
    'count-keyed
    "count : <name> = <multiplicity> by <width>"
    "declare-keyed-count"
    '("name" "multiplicity" "width")
    "count : item = 0..1 by 1")
   (entry 'order "order : <order-mode>" "set-order" '("order-mode") "order : free")
   (entry 'file "file : <value>" "set-file" '("value") "file : catalog.typify")
   (entry 'required "required!" "require-file" '() "required!")
   (entry 'optional "optional!" "allow-missing-file" '() "optional!")
   (entry 'restricts "restricts!" "mark-restriction" '() "restricts!")
   (entry 'default "default : <value>" "declare-default" '("value") "default : item : book")
   (entry 'sees "sees : <name>" "declare-reference" '("name") "sees : Catalog")
   (entry
    'note
    "note : <constructor> = <text>"
    "declare-note"
    '("constructor" "text")
    "note : item = quoting")
   (entry 'cap "cap : <limit> = <number>" "declare-cap" '("limit" "number") "cap : states = 4096")
   (entry 'cell "cell : <name> = <law>" "declare-cell" '("name" "law") "cell : mounts = shadow")
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
    "ensure : <constructor> : <name> = <value>"
    "declare-invariant"
    '("constructor" "name" "value")
    "ensure : item : visible = (bound path)")
   (entry
    'join
    "join : <name> : <value>"
    "declare-join"
    '("name" "value")
    "join : modes : \"ro\" \"rw\" = \"rw\"")
   (entry
    'effect
    "effect : <name> = external"
    "declare-external"
    '("name")
    "effect : output = external")
   (entry
    'effect-state
    "effect : <name> = state"
    "declare-state"
    '("name")
    "effect : remember = state")
   (entry
    'stage
    "stage : <name> = <number>"
    "declare-stage"
    '("name" "number")
    "stage : output = 0")
   (entry
    'guard
    "guard : <name> = <value>"
    "declare-guard"
    '("name" "value")
    "guard : item = exists")
   (entry
    'scope
    "scope : <name> = <scope-mode>"
    "declare-scope"
    '("name" "scope-mode")
    "scope : item = program")
   (entry 'fuel "fuel : <name> = <number>" "declare-fuel" '("name" "number") "fuel : item = 32")))

(define library-arities
  (hash
   "parent"
   '(1)
   "join"
   '(2)
   "split"
   '(1)
   "ancestors"
   '(1)
   "prefix"
   '(2)
   "suffix"
   '(2)
   "equal"
   '(2)
   "concat"
   '(2)
   "inherited"
   '(1)))

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
   (string-append "effect : " constructor " = external")
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
      ((name (in-list '("states" "repetition" "depth" "length"))))
      (string-append "cap : " name " = " (number->string (hash-ref meta-cap-defaults name))))
     (for/list
      ((name (in-list (sort (hash-keys (current-meta-classes)) string<?))))
      (string-append "is : " name " = " (hash-ref (current-meta-classes) name)))
     (append-map
      (lambda (name)
        (list
         (string-append
          "primitive : "
          name
          " : "
          (string-join (make-list (car (hash-ref library-arities name)) "value") " "))
         (string-append "effect : " name " = external")))
      (sort (hash-keys library-arities) string<?))
     (append-map signature-lines (current-meta-forms))
     '("end!"))
    "\n")
   "\n"))
