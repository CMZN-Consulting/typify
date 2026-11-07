#lang racket/base

(require racket/list
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (source rows (name "Example"))
  (string-append "#lang typify\ndeclare : " name "\n" (string-join rows "\n") "\nend!\n"))

(define basic-rows
  '("is : word = /[a-z]+/"
    "primitive : output : word"
    "form : item : [<word>]"
    "does : output <word>"))

(define (body rows)
  (hash-ref (read-declarations (source rows) #:file "declarations.typify") "Example"))

(define (native-errors rows)
  (define-values
   (model diagnostics)
   (read-declarations/diagnostics (source rows) #:file "declarations.typify"))
  (filter (lambda (item) (equal? (diagnostic-severity item) "error")) diagnostics))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-reader-routed "~a: expected ~s; received ~s" context expected actual)))
  (define (one-error rows code data)
    (define diagnostics (native-errors rows))
    (define selected
      (for/first ((item (in-list diagnostics)) #:when (equal? (diagnostic-code item) code)) item))
    (check
     (and selected (diagnostic-code selected))
     code
     (hash 'rows rows 'observed-codes (map diagnostic-code diagnostics)))
    (check (diagnostic-data-projection code (diagnostic-data selected)) data "refusal data")
    selected)
  (define simple (body basic-rows))
  (define dynamic-bang
    (body
     '("is : text = /.*/" "primitive : output : text" "form : <text>!" "does : output <text>")))
  (check
   (hash-ref (check-program "123-name!" dynamic-bang) 'terms)
   '(("output" "123-name"))
   "a declared bang production may contain a placeholder")
  (define-values
   (dynamic-left dynamic-right)
   (form-automata dynamic-bang (car (language-forms dynamic-bang))))
  (for
   ((verb (in-list '("abc" "123-name"))))
   (check (class-accepts? dynamic-left verb) #t "emitted dynamic bang accepts verb words")
   (check
    (line-accepts? (string-append verb "!") dynamic-bang)
    #t
    "dynamic bang reader accepts verb words"))
  (for
   ((verb (in-list '("_bad" "two words" "-bad" ""))))
   (check (class-accepts? dynamic-left verb) #f "emitted dynamic bang rejects invalid words")
   (check
    (line-accepts? (string-append verb "!") dynamic-bang)
    #f
    "dynamic bang reader rejects invalid words"))
  (define empty-argument (body '("primitive : clear" "form : clear :" "does : clear")))
  (check
   (hash-ref (check-program "clear :" empty-argument) 'terms)
   '(("clear"))
   "an empty colon argument is a legal form")
  (check (line-accepts? "clear :" empty-argument) #t "formation accepts an empty colon argument")
  (define-values
   (empty-left empty-right)
   (form-automata empty-argument (car (language-forms empty-argument))))
  (check (class-accepts? empty-left "clear") #t "emitted empty-argument verb")
  (check (class-accepts? empty-right "") #t "emitted empty-argument field")
  (check
   (hash-ref (check-program "? : item\nitem : [a]\n? : 123-name\n" simple) 'terms)
   '(("output" "a"))
   "questions produce no term")
  (for
   ((text (in-list '("?" "?!" "? :" "? : bad_word" "? : two words" "? : -bad"))))
   (define diagnostics
     (hash-ref (check-program/diagnostics text simple #:file "program.typify") 'diagnostics))
   (check (map diagnostic-code diagnostics) '("E_QUESTION") "malformed reserved question")
   (check (diagnostic-data (car diagnostics)) (hash 'actual text) "question source data")
   (check (line-accepts? text simple) #f "malformed question formation"))
  (for
   ((text (in-list '("? : item" "? : 123" "? : a-b"))))
   (check (line-accepts? text simple) #t "valid question formation"))
  (define reserved
    (body
     '("is : word = /[a-z_]+/"
       "primitive : output : word"
       "form : ?x : <word>"
       "does : output <word>")))
  (define-values
   (reserved-left reserved-right)
   (form-automata reserved (car (language-forms reserved))))
  (check
   (class-accepts? reserved-left "?x")
   #t
   "question-prefixed ordinary verbs remain ordinary forms")
  (check (line-accepts? "?x : _bad" reserved) #t "reader accepts ordinary question-prefixed verbs")
  (define reserved-only
    (body
     '("is : word = /[a-z_]+/"
       "primitive : output : word"
       "form : ? : <word>"
       "does : output <word>")))
  (define-values
   (question-left question-right)
   (form-automata reserved-only (car (language-forms reserved-only))))
  (check
   (class-accepts? question-left "?")
   #f
   "ordinary emitted forms cannot bypass reserved question dispatch")
  (check
   (line-accepts? "? : _bad" reserved-only)
   #f
   "reader refuses malformed reserved word despite an ordinary form")
  (define counted (body (append basic-rows '("count : output = 1"))))
  (check
   (map diagnostic-code (hash-ref (check-program/diagnostics "? : item" counted) 'diagnostics))
   '("E_PROGRAM_COUNT")
   "questions do not satisfy counts")
  (define excess
    (car
     (hash-ref
      (check-program/diagnostics "item:[a]\nitem:[b]\n" counted #:file "program.typify")
      'diagnostics)))
  (check (diagnostic-file excess) "program.typify" "count excess is in program")
  (check
   (diagnostic-range excess)
   (src-span "program.typify" 2 1 2 9 9 17)
   "first term past multiplicity")
  (check
   (diagnostic-related excess)
   (list
    (hash 'kind "program" 'path "program.typify" 'line 1)
    (hash 'kind "program" 'path "program.typify" 'line 2))
   "all counted origins are related")
  (define missing
    (car (hash-ref (check-program/diagnostics "" counted #:file "empty.typify") 'diagnostics)))
  (check
   (diagnostic-range missing)
   (src-span "empty.typify" 1 1 1 1 0 0)
   "empty program count anchor")
  (one-error
   (append basic-rows '("default : item : [a]" "default : item : [b]" "count : output = 1"))
   "E_DECLARATION"
   (hash 'kind "conflict" 'actual "default : item : [b]"))
  (define one-default (body (append basic-rows '("default : item : [a]" "count : output = 1"))))
  (check
   (hash-ref (check-program "item:[b]" one-default) 'terms)
   '(("output" "b"))
   "explicit terms suppress a well-formed default set")
  (define ranged
    (hash-ref
     (check-program/diagnostics
      "  unknown!  \nitem:[123]\n? : bad_word\n"
      simple
      #:file
      "program.typify")
     'diagnostics))
  (check
   (map diagnostic-code ranged)
   '("E_FORM_NONE" "E_CLASS" "E_QUESTION")
   "independent program errors are all collected")
  (check
   (diagnostic-range (car ranged))
   (src-span "program.typify" 1 3 1 11 2 10)
   "form refusal covers trimmed whole line")
  (check
   (diagnostic-range (cadr ranged))
   (src-span "program.typify" 2 7 2 10 19 22)
   "class refusal covers capture only")
  (define aggregate
    (with-handlers ((exn:fail:typify? values)) (check-program "unknown!\nitem:[123]" simple)))
  (check
   (map diagnostic-code (exn:fail:typify-diagnostics aggregate))
   '("E_FORM_NONE" "E_CLASS")
   "legacy API exposes all diagnostics")
  (check
   (diagnostic-code (exn:fail:typify-diagnostic aggregate))
   "E_FORM_NONE"
   "legacy singular accessor points to first error")
  (define short (body (cons "cap : length = 2" basic-rows)))
  (define too-long (car (hash-ref (check-program/diagnostics "item:[abc]" short) 'diagnostics)))
  (check (diagnostic-code too-long) "E_CAP" "length gets a cap diagnostic")
  (check (diagnostic-data too-long) (hash 'cap "length" 'limit 2 'actual 3) "capture length data")
  (one-error '("term : ready :") "E_DECLARATION" (hash 'kind "syntax" 'actual "term : ready :"))
  (one-error
   '("primitive : ready :")
   "E_DECLARATION"
   (hash 'kind "syntax" 'actual "primitive : ready :"))
  (check
   (hash-ref
    (check-program "ready!" (body '("primitive : ready" "form : ready!" "does : ready")))
    'terms)
   '(("ready"))
   "nullary constructors omit trailing colon")
  (one-error
   '("is : word = /[a-z]+/"
     "term : pair : word word"
     "form : pair : <word> = <word>"
     "does : pair <word> <word>")
   "E_DECLARATION"
   (hash 'kind "duplicate" 'actual "<word>"))
  (one-error
   '("is : word = /[a-z]+/"
     "term : pair : word word"
     "form : <word> : <word>"
     "does : pair <word> <word>")
   "E_DECLARATION"
   (hash 'kind "duplicate" 'actual "<word>"))
  (define restriction-line " \trestricts! \t")
  (define restriction-source (source (append basic-rows (list restriction-line))))
  (define-values
   (restricted restriction-diagnostics)
   (read-declarations/diagnostics restriction-source #:file "restricted.typify"))
  (check
   (map diagnostic-code restriction-diagnostics)
   '("E_RESTRICTS")
   "an ungraded restriction is refused")
  (check
   (diagnostic-range (car restriction-diagnostics))
   (line-record-span
    (for/first
     ((record (in-list (read-lines restriction-source #:file "restricted.typify")))
      #:when
      (equal? (line-record-raw record) restriction-line))
     record))
   "the restriction diagnostic covers its restricts line")
  (define (check-body-actual text expected-actual source-line)
    (define-values (model diagnostics) (read-declarations/diagnostics text #:file "bodies.typify"))
    (define body-errors
      (filter
       (lambda (item)
         (and (equal? (diagnostic-code item) "E_DECLARATION")
              (equal? (hash-ref (diagnostic-data item) 'kind #f) "body")))
       diagnostics))
    (check (length body-errors) 1 "one body structure refusal")
    (check
     (diagnostic-data-projection "E_DECLARATION" (diagnostic-data (car body-errors)))
     (hash 'kind "body" 'actual expected-actual)
     "body actual follows its source kind")
    (check
     (diagnostic-range (car body-errors))
     (line-record-span
      (for/first
       ((record (in-list (read-lines text #:file "bodies.typify")))
        #:when
        (equal? (line-record-raw record) source-line))
       record))
     "body refusal covers the ruled declaring or offending line"))
  (define unclosed-line "  declare : Open  ")
  (check-body-actual (string-append "#lang typify\n" unclosed-line "\n") "EOF" unclosed-line)
  (define inner-line " \tdeclare : Inner \t")
  (check-body-actual
   (string-append "#lang typify\ndeclare : Outer\n" inner-line "\nend!\n")
   inner-line
   inner-line)
  (define outside-line "  version : 1 \t")
  (check-body-actual (string-append "#lang typify\n" outside-line "\n") outside-line outside-line)
  (define absent-class
    (one-error
     '("primitive : output" "form : item : <unknown>" "does : output")
     "E_DECLARATION"
     (hash 'kind "capture" 'actual "<unknown>")))
  (check
   (src-span-start-column (diagnostic-range absent-class))
   1
   "unknown placeholder covers form line")
  (one-error '("primitive : split") "E_DECLARATION" (hash 'kind "duplicate" 'actual "split"))
  (one-error '("is : number = /[0-9]+/") "E_DECLARATION" (hash 'kind "duplicate" 'actual "number"))
  (one-error
   '("cell : canonical-path = shadow")
   "E_DECLARATION"
   (hash 'kind "duplicate" 'actual "canonical-path"))
  (define arity
    (one-error
     '("is : word = /[a-z]+/"
       "primitive : output : word word"
       "form : item : <word>"
       "does : output <word>")
     "E_DECLARATION"
     (hash 'kind "arity" 'actual "output <word>")))
  (check (src-span-start-column (diagnostic-range arity)) 1 "arity covers declaring line")
  (define nested
    (one-error
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "term : inner : word"
       "fact : inner"
       "form : item : <word>"
       "does : output (inner <word>)")
     "E_TERM_NESTED"
     (hash 'constructor "output" 'actual "(inner <word>)")))
  (check
   (src-span-start-column (diagnostic-range nested))
   15
   "nested term starts within does field")
  (define rule-nested
    (one-error
     '("is : word = /[a-z]+/"
       "primitive : output : word"
       "term : inner : word"
       "fact : inner"
       "term : wrapper : word"
       "rule : wrapper <x> -> output (inner <x>)")
     "E_TERM_NESTED"
     (hash 'constructor "output" 'actual "(inner <x>)")))
  (check
   (src-span-start-column (diagnostic-range rule-nested))
   30
   "nested rule term retains real RHS offset")
  (define-values
   (partial declarations-errors)
   (read-declarations/diagnostics
    (source '("bad line" "version : bad" "order : unknown" "count : missing = any"))
    #:file
    "declarations.typify"))
  (check
   (map diagnostic-code declarations-errors)
   '("E_LINE_SHAPE" "E_CLASS" "E_CLASS" "E_CLASS")
   "all independent meta line errors are collected")
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'class #f)) (cdr declarations-errors))
   '("number" "order-mode" "multiplicity")
   "meta class report names")
  (define-values
   (bad-classes class-errors)
   (read-declarations/diagnostics
    (source '("is : first = missing" "is : second = othermissing"))
    #:file
    "declarations.typify"))
  (check
   (map diagnostic-code class-errors)
   '("E_CLASS_UNDECLARED" "E_CLASS_UNDECLARED")
   "class compiler reports independent errors")
  (check
   (map (lambda (item) (src-span-start-line (diagnostic-range item))) class-errors)
   '(3 4)
   "class errors point to their is lines")
  (define-values
   (empty-model empty-notes)
   (read-declarations/diagnostics (source '("is : empty = /\\x00/")) #:file "declarations.typify"))
  (check (map diagnostic-code empty-notes) '("N_CLASS_EMPTY") "empty class is a note")
  (check (diagnostics-error? empty-notes) #f "empty class note does not refuse declaration")
  (check
   (hash-has-key? (read-declarations (meta-source)) "Typify")
   #t
   "exact canonical bootstrap is permitted")
  (define canonical-model (read-declarations (meta-source)))
  (define canonical-emitted (emit-declarations canonical-model))
  (check canonical-emitted (meta-source) "canonical grammar emits byte-identically")
  (check
   (emit-declarations (read-declarations canonical-emitted))
   canonical-emitted
   "canonical emission remains a strict-bootstrap fixed point")
  (define-values
   (spoofed spoof-errors)
   (read-declarations/diagnostics (source '("is : number = /[0-9]+/") "Typify")))
  (check
   (map diagnostic-code spoof-errors)
   '("E_DECLARATION")
   "body name does not grant bootstrap exemption")
  (hash 'suite "reader-routed" 'checks checks 'status "passed"))
