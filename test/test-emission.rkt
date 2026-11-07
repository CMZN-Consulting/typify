#lang racket/base

(require json
         racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/emission.rkt"
         "../src/language.rkt"
         (only-in "test-decoder.rkt" parse-gbnf gbnf-accepts?))

(provide run-tests)

(define (body name lines #:version (version "1") #:caps (caps '("length = 32")))
  (define source
    (string-append
     (string-join
      (append
       (list "#lang typify" (string-append "declare : " name) (string-append "version : " version))
       (map (lambda (cap) (string-append "cap : " cap)) caps)
       lines
       '("end!"))
      "\n")
     "\n"))
  (hash-ref (read-declarations source) name))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-emission "~a: expected ~s; received ~s" context expected actual)))
  (define catalog
    (body
     "Catalog"
     '("is : category = \"book\" | \"journal\""
       "is : title = /[^=]+/"
       "is : unused = \"unused\""
       "term : item : category title"
       "form : item : <category> = <title>"
       "does : item <category> <title>")))
  (define catalog-wire (language->grammar catalog))
  (check
   catalog-wire
   (hash
    'language
    "Catalog"
    'version
    1
    'caps
    (hash 'states 4096 'repetition 256 'depth 8 'length 32)
    'classes
    (hash
     "Catalog.category"
     (hash 'literals '("book" "journal"))
     "Catalog.title"
     (hash 'pattern "[^=]+"))
    'forms
    (list (hash 'verb "item" 'shape "colon" 'production "item : <category> = <title>")))
   "Catalog has the qualified grammar wire shape and no unreachable classes")
  (check
   (canonical-json (string->jsexpr (canonical-json catalog-wire)))
   (canonical-json catalog-wire)
   "the grammar is JSON data with stable canonical serialization")
  (define nodes
    (body
     "Nodes"
     (append
      '("is : literal = \"a\" | \"b\""
        "is : suffix = /[0-9]/"
        "is : aliased = literal"
        "is : sequence = literal suffix"
        "is : alternative = literal | suffix"
        "is : bounded = literal{1,2}"
        "is : unbounded = literal*"
        "is : excluded = literal except \"a\""
        "is : named = name"
        "is : path = canonical-path"
        "term : record : value")
      (append-map
       (lambda (name)
         (list
          (string-append "form : " name " : <" name ">")
          (string-append "does : record <" name ">")))
       '("aliased" "sequence" "alternative" "bounded" "unbounded" "excluded" "named" "path")))))
  (define node-classes (hash-ref (language->grammar nodes) 'classes))
  (define (ref name) (hash 'ref (string-append "Nodes." name)))
  (for
   ((case (in-list
           (list
            (cons "Nodes.literal" (hash 'literals '("a" "b")))
            (cons "Nodes.suffix" (hash 'pattern "[0-9]"))
            (cons "Nodes.aliased" (ref "literal"))
            (cons "Nodes.sequence" (hash 'seq (list (ref "literal") (ref "suffix"))))
            (cons "Nodes.alternative" (hash 'alt (list (ref "literal") (ref "suffix"))))
            (cons "Nodes.bounded" (hash 'repeat (hash 'node (ref "literal") 'min 1 'max 2)))
            (cons "Nodes.unbounded" (hash 'repeat (hash 'node (ref "literal") 'min 0 'max 'null)))
            (cons "Nodes.excluded" (hash 'except (hash 'node (ref "literal") 'literals '("a"))))
            (cons "Nodes.named" (hash 'ref "Typify.name"))
            (cons "Nodes.path" (hash 'ref "Typify.canonical-path"))
            (cons "Typify.letter" (hash 'builtin "letter"))
            (cons "Typify.canonical-path" (hash 'builtin "canonical-path"))))))
   (check (hash-ref node-classes (car case) #f) (cdr case) "each class node keeps its syntax"))
  (check
   (sort (hash-keys node-classes) string<?)
   (sort
    '("Nodes.literal"
      "Nodes.suffix"
      "Nodes.aliased"
      "Nodes.sequence"
      "Nodes.alternative"
      "Nodes.bounded"
      "Nodes.unbounded"
      "Nodes.excluded"
      "Nodes.named"
      "Nodes.path"
      "Typify.name"
      "Typify.letter"
      "Typify.canonical-path")
    string<?)
   "all and only transitively reachable definitions are emitted")
  (define shadow
    (body
     "Shadow"
     '("is : name = \"local\""
       "is : inherited = constructor"
       "term : record : value"
       "form : inherited : <inherited>"
       "does : record <inherited>")))
  (define shadow-classes (hash-ref (language->grammar shadow) 'classes))
  (check
   (hash-ref shadow-classes "Typify.constructor")
   (hash 'ref "Shadow.name")
   "inherited references retain the reader's flat local override binding")
  (check
   (hash-ref shadow-classes "Shadow.name")
   (hash 'literals '("local"))
   "the actual local override definition is reachable")
  (check
   (hash-has-key? shadow-classes "Typify.name")
   #f
   "a shadowed inherited definition is not substituted into the wire")
  (define shapes
    (body
     "Shapes"
     '("is : word = /[a-z]+/"
       "term : record : value"
       "form : literal : <word>"
       "does : record <word>"
       "form : two words : <word>"
       "does : record <word>"
       "form : <word> : 123"
       "does : record <word>"
       "form : done!"
       "does : record done"
       "form : send -> <word>"
       "does : record <word>")
     #:version
     "7"
     #:caps
     '("states = 512" "repetition = 16" "depth = 4" "length = 32")))
  (define shape-wire (language->grammar shapes))
  (check (hash-ref shape-wire 'version) 7 "the wire version is numeric")
  (check
   (hash-ref shape-wire 'caps)
   (hash 'states 512 'repetition 16 'depth 4 'length 32)
   "all four effective caps use public names")
  (check
   (hash-ref shape-wire 'forms)
   (list
    (hash 'shape "colon" 'production "literal : <word>" 'verb "literal")
    (hash 'shape "colon" 'production "two words : <word>" 'verb 'null)
    (hash 'shape "colon" 'production "<word> : 123" 'verb 'null)
    (hash 'shape "bang" 'production "done!" 'verb "done")
    (hash 'shape "arrow" 'production "send -> <word>" 'verb 'null 'left "send" 'right "<word>"))
   "forms retain order, arrow productions and the defined verb cases")
  (define grammar (parse-gbnf (language->gbnf catalog)))
  (for
   ((case (in-list
           (list
            (cons "item:book = Algebra" #t)
            (cons " \titem\t:\tjournal = Market Research\t " #t)
            (cons "item : book = a=b" #f)
            (cons (string-append "item : book = " (string #\nul)) #f)
            (cons (string-append "item : book = " (make-string 33 #\x)) #f)))))
   (check
    (gbnf-accepts? grammar (car case))
    (cdr case)
    "the GBNF keeps exact spacing, global refusals and finite captures"))
  (hash 'suite "emission" 'count checks 'pass checks 'fail 0))

(module+
 test
 (define result (run-tests))
 (unless (equal? (hash-ref result 'fail) 0) (error 'test "Checks failed" result)))
