#lang racket/base

(require racket/list
         racket/string
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/meta.rkt"
         "../src/model.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (source rows (name "Local"))
  (string-append
   (string-join
    (append (list "#lang typify" (string-append "declare : " name)) rows '("end!"))
    "\n")
   "\n"))

(define (errors text)
  (define-values
   (model diagnostics)
   (read-declarations/diagnostics text #:file "declarations.typify"))
  (filter (lambda (item) (equal? (diagnostic-severity item) "error")) diagnostics))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-stage3-meta "~a: expected ~s; received ~s" context expected actual)))
  (define canonical (meta-source))
  (define canonical-lines (string-split canonical "\n"))
  (for
   ((line
     (in-list
      '("is : verb = /[A-Za-z0-9][A-Za-z0-9-]*/"
        "form : ? : <verb>"
        "does : question <verb>"
        "primitive : question : verb"
        "effect : question = external"
        "example : ? : form"))))
   (check (count (lambda (candidate) (string=? candidate line)) canonical-lines) 1 line))
  (define question-entry (findf (lambda (row) (eq? (hash-ref row 'kind) 'question)) meta-forms))
  (check
   (hash-ref question-entry 'production)
   "? : <verb>"
   "the question comes from the meta table")
  (check (hash-ref question-entry 'constructor) "question" "the table gives the question primitive")
  (check (hash-ref question-entry 'arguments) '("verb") "the table gives the question signature")
  (define-values
   (canonical-declarations canonical-diagnostics)
   (read-declarations/diagnostics canonical #:file "generated.typify"))
  (check canonical-diagnostics '() "the exact canonical declaration has no exemption diagnostics")
  (define meta (hash-ref canonical-declarations "Typify"))
  (check
   (emit-declarations canonical-declarations)
   canonical
   "the canonical source remains byte stable")
  (check
   (emit-declarations (read-declarations (emit-declarations canonical-declarations)))
   canonical
   "the canonical source remains a strict-bootstrap fixed point")
  (define question (hash-ref (language-constructors meta) "question"))
  (check (constructor-primitive? question) #t "question is primitive")
  (check (constructor-signature question) '("verb") "question takes the verb class")
  (check
   (hash-ref (compiled-model-external (language-model meta)) "question" #f)
   #t
   "question is an external primitive")
  (define question-form
    (findf
     (lambda (candidate) (equal? (form-source candidate) "? : <verb>"))
     (language-forms meta)))
  (check (term-constructor (form-meaning question-form)) "question" "the form builds question")
  (check (map car (form-examples question-form)) '("? : form") "the question's example is authored")
  (define-values (question-left question-right) (form-automata meta question-form))
  (check (class-accepts? question-left "?") #t "the meta question retains its literal left field")
  (check (class-accepts? question-left "??") #f "the meta question's left field stays exact")
  (define verb-class (hash-ref (language-classes meta) "verb"))
  (for
   ((word (in-list '("form" "A" "9" "a-B-9" "9-"))))
   (check (class-accepts? verb-class word) #t "the meta verb class follows D5")
   (check (class-accepts? question-right word) #t "the emitted question field follows D5"))
  (for
   ((word (in-list '("" "?" "-bad" "bad_word" "two words" "é" "a\nb"))))
   (check (class-accepts? verb-class word) #f "the meta verb class refuses non-D5 words")
   (check (class-accepts? question-right word) #f "the emitted question refuses non-D5 words"))
  (check
   (hash-ref (check-program "? : form\n? : 123-name\n" meta) 'terms)
   '()
   "the declared meta question is never built by program checking")
  (check
   (map diagnostic-code (hash-ref (check-program/diagnostics "? : ?" meta) 'diagnostics))
   '("E_QUESTION")
   "a program question still uses D5 even when ask can name the reserved verb")
  (define reserved-form-errors
    (errors
     (source
      '("is : word = /[a-z]+/" "term : record : word" "form : ? : <word>" "does : record <word>"))))
  (check (map diagnostic-code reserved-form-errors) '("E_DECLARATION") "user question form refusal")
  (check
   (diagnostic-data (car reserved-form-errors))
   (hash 'kind "duplicate" 'actual "?")
   "the user question form reports the reserved verb")
  (check
   (src-span-start-line (diagnostic-range (car reserved-form-errors)))
   5
   "the reserved verb refusal is anchored at the form line")
  (define reserved-body-errors (errors (source '() "Typify")))
  (check (map diagnostic-code reserved-body-errors) '("E_DECLARATION") "user meta body refusal")
  (check
   (diagnostic-data (car reserved-body-errors))
   (hash 'kind "duplicate" 'actual "Typify")
   "the user meta body reports the reserved name")
  (check
   (src-span-start-line (diagnostic-range (car reserved-body-errors)))
   2
   "the reserved body refusal is anchored at the declare line")
  (check
   (ormap
    (lambda (item)
      (and (equal? (diagnostic-code item) "E_DECLARATION")
           (equal? (diagnostic-data item) (hash 'kind "duplicate" 'actual "Typify"))))
    (errors (string-append canonical "? : form\n")))
   #t
   "an ignored question does not broaden the exact-byte bootstrap exception")
  (check
   (hash-has-key? (read-declarations (source '() "typify")) "typify")
   #t
   "the reserved body name is case significant")
  (for
   ((name
     (in-list
      '("letter" "canonical-path" "relative-path" "concat" "name" "constructor" "number" "verb"))))
   (define diagnostics (errors (source (list (string-append "is : " name " = \"local\"")))))
   (check (map diagnostic-code diagnostics) '("E_DECLARATION") "reserved class name refusal")
   (check
    (diagnostic-data (car diagnostics))
    (hash 'kind "duplicate" 'actual name)
    "built-in, library and meta class names remain reserved"))
  (define counted
    (hash-ref
     (read-declarations
      (source
       '("is : word = /[a-z]+/"
         "order : strict"
         "term : record : word"
         "form : first : <word>"
         "does : record <word>"
         "form : last!"
         "does : record last"
         "count : record = 1")))
     "Local"))
  (define counted-check (check-program/diagnostics "last!\n? : first\n" counted))
  (check (hash-ref counted-check 'terms) '(("record" "last")) "questions do not add terms")
  (check
   (hash-ref counted-check 'diagnostics)
   '()
   "questions do not reorder forms or increase counts")
  (check
   (map diagnostic-code (hash-ref (check-program/diagnostics "? : first" counted) 'diagnostics))
   '("E_PROGRAM_COUNT")
   "questions do not satisfy a constructor's required count")
  (define declaration-rows
    '("declare : Local"
      "is : word = /[a-z]+/"
      "term : record : word"
      "form : item : <word>"
      "does : record <word>"
      "example : item : a"
      "default : item : b"
      "count : record = 1"
      "end!"
      "declare : Other"
      "term : halt"
      "form : halt!"
      "does : halt"
      "example : halt!"
      "end!"))
  (define control-source (string-append "#lang typify\n" (string-join declaration-rows "\n") "\n"))
  (define questioned-source
    (string-append
     "#lang typify\n? : outside-before\n"
     (string-join (append-map (lambda (row) (list row "? : gap")) declaration-rows) "\n")
     "\n? : outside-after\n"))
  (define output (open-output-string))
  (define-values
   (questioned questioned-diagnostics)
   (parameterize
    ((current-output-port output))
    (read-declarations/diagnostics questioned-source #:file "questions.typify")))
  (define control (read-declarations control-source #:file "control.typify"))
  (check questioned-diagnostics '() "valid questions are accepted in every declaration-file gap")
  (check (get-output-string output) "" "the declaration reader does not answer question lines")
  (check
   (sort (hash-keys questioned) string<?)
   '("Local" "Other")
   "questions outside bodies declare nothing")
  (for
   ((name (in-list '("Local" "Other"))))
   (define actual (hash-ref questioned name))
   (define expected (hash-ref control name))
   (check
    (declaration->jsexpr actual #:origins? #f)
    (declaration->jsexpr expected #:origins? #f)
    "questions preserve form, does, example and default attachments")
   (check
    (ormap (lambda (row) (eq? (hash-ref row 'kind) 'question)) (language-rows actual))
    #f
    "passed-over questions never enter declaration rows"))
  (check
   (emit-declarations questioned)
   (emit-declarations control)
   "passed-over questions are absent from emitted declarations")
  (check
   (hash-ref (check-program "item : a" (hash-ref questioned "Local")) 'terms)
   '(("record" "a"))
   "the form around an intervening question still builds its meaning")
  (define-values
   (through-meta through-meta-diagnostics)
   (read-declarations/diagnostics questioned-source #:meta meta))
  (check through-meta-diagnostics '() "questions are ignored with an explicit meta reader too")
  (check
   (declaration->jsexpr (hash-ref through-meta "Local") #:origins? #f)
   (declaration->jsexpr (hash-ref control "Local") #:origins? #f)
   "passing questions does not bypass the explicit meta reader's ordinary rows")
  (define malformed-source
    (string-append
     (string-join
      '("#lang typify"
        "  ? : bad_word\t "
        "declare : Local"
        "is : word = /[a-z]+/"
        "term : record : word"
        "form : item : <word>"
        "?"
        "does : record <word>"
        "example : item : a"
        "?!"
        "default : item : b"
        "count : record = 1"
        "end!"
        "? :"
        "declare : Other"
        "term : halt"
        "form : halt!"
        "? -> ready"
        "does : halt"
        "example : halt!"
        "end!"
        "? : two words"
        "? : -bad")
      "\n")
     "\n"))
  (define-values
   (malformed malformed-diagnostics)
   (read-declarations/diagnostics malformed-source #:file "declarations.typify"))
  (check
   (map diagnostic-code malformed-diagnostics)
   (make-list 7 "E_QUESTION")
   "malformed questions are declaration errors in and out of bodies")
  (check
   (map diagnostic-data malformed-diagnostics)
   (map
    (lambda (actual) (hash 'actual actual))
    '("? : bad_word" "?" "?!" "? :" "? -> ready" "? : two words" "? : -bad"))
   "malformed questions carry the exact covered text")
  (check
   (map (lambda (item) (src-span-start-line (diagnostic-range item))) malformed-diagnostics)
   '(2 7 10 14 18 22 23)
   "every malformed question keeps its source line")
  (check
   (diagnostic-range (car malformed-diagnostics))
   (src-span "declarations.typify" 2 3 2 15 15 27)
   "a padded malformed question covers its nonblank text with byte offsets")
  (for
   ((name (in-list '("Local" "Other"))))
   (check
    (declaration->jsexpr (hash-ref malformed name) #:origins? #f)
    (declaration->jsexpr (hash-ref control name) #:origins? #f)
    "question errors recover without disturbing declaration attachments"))
  (define recovery-errors
    (errors
     (string-append
      "#lang typify\n? : bad_word\nnonsense\ndeclare : Recovery\n"
      "is : missing = unbound\nterm : halt\nform : halt!\n? :\ndoes : halt\nend!\n")))
  (check
   (map diagnostic-code recovery-errors)
   '("E_QUESTION" "E_LINE_SHAPE" "E_CLASS_UNDECLARED" "E_QUESTION")
   "D1 retains independent errors around malformed questions")
  (define header-errors
    (errors "? : ready\ndeclare : MissingHeader\nterm : halt\nform : halt!\ndoes : halt\nend!\n"))
  (check
   (map diagnostic-code header-errors)
   '("E_DECLARATION")
   "a question cannot replace the header")
  (check
   (hash-ref (diagnostic-data (car header-errors)) 'kind)
   "header"
   "question handling leaves the first-line header requirement intact")
  (define double-header-errors
    (errors
     "? : bad_word\ndeclare : MissingHeader\nterm : halt\nform : halt!\ndoes : halt\nend!\n"))
  (check
   (map diagnostic-code double-header-errors)
   '("E_DECLARATION" "E_QUESTION")
   "a malformed first-line question retains both independent refusals")
  (for
   ((false-question (in-list '("?x : ready" "?? : ready" "?-x : ready"))))
   (check
    (map diagnostic-code (errors (string-append "#lang typify\n" false-question "\n")))
    '("E_FORM_NONE")
    "question-like ordinary verbs are not silently passed over"))
  (define prefixed
    (hash-ref
     (read-declarations
      (source
       '("is : word = /[a-z]+/"
         "term : record : word"
         "form : ?x : <word>"
         "does : record <word>")))
     "Local"))
  (check
   (hash-ref (check-program "?x : a" prefixed) 'terms)
   '(("record" "a"))
   "ordinary question-prefixed forms keep their ordinary behavior")
  (hash 'suite "stage3-meta" 'checks checks 'status "passed"))

(module+ test (run-tests))
