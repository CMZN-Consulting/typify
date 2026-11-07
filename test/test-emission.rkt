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
    'comment
    "#"
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
        "is : digit-tail = /[0-9]/"
        "is : aliased = literal"
        "is : sequence = literal digit-tail"
        "is : alternative = literal | digit-tail"
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
            (cons "Nodes.digit-tail" (hash 'pattern "[0-9]"))
            (cons "Nodes.aliased" (ref "literal"))
            (cons "Nodes.sequence" (hash 'seq (list (ref "literal") (ref "digit-tail"))))
            (cons "Nodes.alternative" (hash 'alt (list (ref "literal") (ref "digit-tail"))))
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
      "Nodes.digit-tail"
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
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostic))
     (body "Shadow" '("is : name = \"local\""))))
  (check
   (diagnostic-code shadow)
   "E_DECLARATION"
   "meta class names cannot acquire a local override")
  (check
   (diagnostic-data-projection (diagnostic-code shadow) (diagnostic-data shadow))
   (hash 'kind "duplicate" 'actual "name")
   "a forbidden override identifies the reserved name")
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
  (define paths
    (body
     "PathWords"
     '("is : mode = \"ro\" | \"rw\""
       "term : bind : canonical-path mode"
       "form : bind : <canonical-path> <mode>"
       "does : bind <canonical-path> <mode>"
       "term : relative : relative-path mode"
       "form : relative : <relative-path> <mode>"
       "does : relative <relative-path> <mode>")))
  (define path-grammar (parse-gbnf (language->gbnf paths)))
  (for
   ((case (in-list
           (list
            (cons "bind : /a rw" #t)
            (cons "bind : / rw" #t)
            (cons "bind : /a b rw" #f)
            (cons "bind : /a  rw" #f)
            (cons "relative : a/b ro" #t)
            (cons "relative : a b ro" #f)))))
   (check
    (line-accepts? (car case) paths)
    (cdr case)
    "2026-11-07T15:45 path components exclude blanks and preserve their separator")
   (check
    (gbnf-accepts? path-grammar (car case))
    (cdr case)
    "emitted path automata enforce the same no-blank component rule"))
  (check
   (hash-ref (hash-ref (language->grammar paths) 'classes) "Typify.canonical-path")
   (hash 'builtin "canonical-path")
   "path semantics retain the canonical builtin JSON node")
  (define comment-models
    (for/list
     ((prefix '("#" "//" "")))
     (define item
       (body
        "Comments"
        (list
         (string-append "comment : " prefix)
         "term : record : value"
         "form : go!"
         "does : record go")))
     (list prefix item (language->grammar item) (parse-gbnf (language->gbnf item)))))
  (for
   ((entry (in-list comment-models)))
   (define prefix (car entry))
   (define model (cadr entry))
   (define wire (caddr entry))
   (define oracle (cadddr entry))
   (check (hash-ref wire 'comment) prefix "the comment field is present even when disabled")
   (check (hash-has-key? wire 'checks) #f "derived check bounds do not enter grammar JSON")
   (for
    ((text '("# note" "// note" " \t# note\t " " \t// note\t " "go!" "")))
    (define stripped (string-trim text))
    (define expected
      (or (member stripped '("go!" ""))
          (and (positive? (string-length prefix)) (string-prefix? stripped prefix))))
    (define accepted? (and expected #t))
    (check (line-accepts? text model) accepted? "the reader uses this body's comment prefix")
    (check (gbnf-accepts? oracle text) accepted? "GBNF uses the same enabled or disabled prefix")))
  (for*
   ((left (in-list comment-models))
    (right (in-list comment-models))
    #:when
    (string<? (car left) (car right)))
   (check
    (equal? (caddr left) (caddr right))
    #f
    "different comment meanings cannot collapse to identical grammar objects")
   (check
    (hash-remove (caddr left) 'comment)
    (hash-remove (caddr right) 'comment)
    "the regression varies only the comment meaning"))
  (hash 'suite "emission" 'count checks 'pass checks 'fail 0))

(module+
 test
 (define result (run-tests))
 (unless (equal? (hash-ref result 'fail) 0) (error 'test "Checks failed" result)))
