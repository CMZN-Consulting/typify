#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/questions.rkt")

(provide run-tests)

(define (source lines (name "Questions"))
  (string-append
   (string-join
    (append (list "#lang typify" (string-append "declare : " name)) lines '("end!"))
    "\n")
   "\n"))

(define (body lines (name "Questions"))
  (hash-ref (read-declarations (source lines name) #:file "questions.typify") name))

(define (answer-lines body word) (hash-ref (ask-language body word) 'lines))

(define (run-tests)
  (define checks 0)
  (define (check actual expected description)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'questions "~a: expected ~s, got ~s" description expected actual)))
  (define (refuse thunk)
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostics))
     (thunk)
     (error 'questions "Expected a native refusal")))
  (define catalog
    (body
     '("file : entries.typify"
       "required!"
       "is : atom = /[a-z]+/"
       "is : category = \"a\" | \"b\""
       "is : title = atom"
       "term : item : category title"
       "form : view : <category> = <title>"
       "does : item <category> <title>"
       "example : view : a = oak"
       "form : archive : <title>"
       "does : item b <title>"
       "example : archive : elm"
       "count : item = 1..*"
       "note : item = catalog-items")))
  (define item-lines
    '("? : form : view : <category> = <title>"
      "? : file : entries.typify"
      "? : does : item <category> <title>"
      "? : form : archive : <title>"
      "? : does : item b <title>"
      "? : term : item : category title"
      "? : is : category = \"a\" | \"b\""
      "? : is : title = atom"
      "? : example : view : a = oak"
      "? : example : archive : elm"
      "? : count : item = 1..*"
      "? : missing : refused"
      "? : note : item = catalog-items"))
  (check (answer-lines catalog "item") item-lines "constructor selects every building form")
  (check
   (answer-lines catalog "view")
   '("? : form : view : <category> = <title>"
     "? : file : entries.typify"
     "? : does : item <category> <title>"
     "? : term : item : category title"
     "? : is : category = \"a\" | \"b\""
     "? : is : title = atom"
     "? : example : view : a = oak"
     "? : count : item = 1..*"
     "? : missing : refused"
     "? : note : item = catalog-items")
   "verb selects its own form and that form's constructor")
  (check
   (question-candidates catalog)
   '("archive" "item" "view")
   "only verbs and constructors are near candidates")
  (check (answer-lines catalog "title") '("? : is : title = atom") "class answer is direct")
  (check
   (answer-lines catalog "atom")
   '("? : is : atom = /[a-z]+/")
   "referenced class can be asked separately")
  (for
   ((word '("letter" "canonical-path" "relative-path")))
   (check
    (hash-ref (ask-language catalog word) 'status)
    "undeclared"
    "a built-in class is not declared by a body"))
  (check
   (hash-ref (ask-language catalog "value") 'status)
   "undeclared"
   "inherited meta classes are not declared locally")
  (check
   (ask-language catalog "itemm")
   (hash
    'word
    "itemm"
    'status
    "undeclared"
    'lines
    '("? : itemm = undeclared" "? : near : item")
    'near
    '("item")
    'accepts
    '()
    'diagnostics
    '())
   "an undeclared word gets the nearest candidate without a diagnostic")
  (check
   (answer-lines catalog "zzzzz")
   '("? : zzzzz = undeclared")
   "nothing resembling a candidate gives only the status line")
  (for
   ((word '("" "bad_name" "two words" "?" "é")))
   (define refused (refuse (lambda () (ask-language catalog word))))
   (check (map diagnostic-code refused) '("E_USAGE") "an invalid undeclared spelling is usage")
   (check (diagnostic-data (car refused)) (hash 'actual word) "usage preserves the exact word")
   (check (diagnostic-file (car refused)) "" "usage has no file")
   (check (diagnostic-range (car refused)) #f "usage has no range"))
  (define unusual (body '("is : private_name = \"one\"" "is : Élan = \"two\"")))
  (check
   (answer-lines unusual "private_name")
   '("? : is : private_name = \"one\"")
   "a declared underscore spelling bypasses the verb test")
  (check
   (answer-lines unusual "Élan")
   '("? : is : Élan = \"two\"")
   "a declared Unicode spelling bypasses the verb test")
  (define defaults
    (body
     '("required!"
       "is : mode = \"on\" | \"off\""
       "term : setting : mode"
       "form : view : <mode>"
       "does : setting <mode>"
       "form : quiet!"
       "does : setting off"
       "default : quiet!"
       "count : setting = 0..1")))
  (check
   (answer-lines defaults "view")
   '("? : form : view : <mode>"
     "? : does : setting <mode>"
     "? : term : setting : mode"
     "? : is : mode = \"on\" | \"off\""
     "? : count : setting = 0..1"
     "? : missing : default"
     "? : default : quiet!")
   "a constructor's defaults include its other forms exactly once")
  (check
   (hash-ref (ask-language defaults "quiet") 'status)
   "declared"
   "a bang form is asked by its verb without the bang")
  (check
   (map diagnostic-code (refuse (lambda () (ask-language defaults "quiet!"))))
   '("E_USAGE")
   "a bang suffix is not part of a question word")
  (define restricted
    (body
     '("term : datum : value"
       "form : guarded : <value>"
       "does : datum <value>"
       "restricts!"
       "form : plain : <value>"
       "does : datum <value>"
       "count : datum = 1")))
  (check
   (answer-lines restricted "datum")
   '("? : form : guarded : <value>"
     "? : does : datum <value>"
     "? : restricts!"
     "? : form : plain : <value>"
     "? : does : datum <value>"
     "? : term : datum : value"
     "? : is : value = /.*/"
     "? : count : datum = 1"
     "? : missing : refused")
   "restricts prints with its form immediately after does")
  (check
   (member "? : restricts!" (answer-lines restricted "plain"))
   #f
   "an unselected form contributes no restricts line")
  (define optional (body '("required!" "term : note-item : value" "count : note-item = 1 by 1")))
  (check
   (last (answer-lines optional "note-item"))
   "? : missing : empty"
   "a required file and keyed count do not require a missing constructor")
  (check
   (answer-lines optional "note-item")
   '("? : term : note-item : value"
     "? : is : value = /.*/"
     "? : count : note-item = *"
     "? : count : note-item = 1 by 1"
     "? : missing : empty")
   "a keyed count follows the explicit default unkeyed bound")
  (define ordered-counts
    (body '("term : datum : value" "count : datum = 0..1 by 1" "count : datum = 1..*")))
  (check
   (answer-lines ordered-counts "datum")
   '("? : term : datum : value"
     "? : is : value = /.*/"
     "? : count : datum = 1..*"
     "? : count : datum = 0..1 by 1"
     "? : missing : refused")
   "the unkeyed count prints first even when authored after the keyed count")
  (define signature-only
    (body
     '("is : first-slot = \"a\""
       "is : second-slot = \"b\""
       "term : tuple : second-slot first-slot second-slot")))
  (check
   (answer-lines signature-only "tuple")
   '("? : term : tuple : second-slot first-slot second-slot"
     "? : is : second-slot = \"b\""
     "? : is : first-slot = \"a\""
     "? : count : tuple = *"
     "? : missing : empty")
   "a formless signature names direct classes once in signature order")
  (define templated
    (body
     '("file : <section>/entry.typify"
       "is : section = /[a-z]+/"
       "is : mode = \"read\" | \"write\""
       "term : point : mode section"
       "form : point : <mode>"
       "does : point <mode> <section>")))
  (check
   (answer-lines templated "point")
   '("? : form : point : <mode>"
     "? : file : <section>/entry.typify"
     "? : does : point <mode> <section>"
     "? : term : point : mode section"
     "? : is : mode = \"read\" | \"write\""
     "? : is : section = /[a-z]+/"
     "? : count : point = *"
     "? : missing : empty")
   "template captures follow their first mention in does")
  (define literal-capture
    (body '("term : datum : value" "form : put : <value>" "does : datum <value>")))
  (check
   (answer-lines literal-capture "put")
   '("? : form : put : <value>"
     "? : does : datum <value>"
     "? : term : datum : value"
     "? : is : value = /.*/"
     "? : count : datum = *"
     "? : missing : empty")
   "a capture can use the canonical meta class definition")
  (define multiple
    (body
     '("is : a-kind = \"a\""
       "is : b-kind = \"b\""
       "term : second : b-kind"
       "term : first : a-kind"
       "form : pick : <a-kind>"
       "does : first <a-kind>"
       "form : pick : <b-kind>"
       "does : second <b-kind>"
       "count : first = 1..*"
       "count : second = 0..1"
       "note : first = first-note"
       "note : second = second-note")))
  (check
   (answer-lines multiple "pick")
   '("? : form : pick : <a-kind>"
     "? : does : first <a-kind>"
     "? : form : pick : <b-kind>"
     "? : does : second <b-kind>"
     "? : term : second : b-kind"
     "? : is : b-kind = \"b\""
     "? : term : first : a-kind"
     "? : is : a-kind = \"a\""
     "? : count : second = 0..1"
     "? : missing : empty"
     "? : count : first = 1..*"
     "? : missing : refused"
     "? : note : second = second-note"
     "? : note : first = first-note")
   "multiple constructors group their lines in constructor declaration order")
  (define attributed
    (body
     '("file : operations.typify"
       "cell : ledger = shadow"
       "is : target = canonical-path"
       "primitive : publish : target value"
       "writes : publish : ledger = put 1"
       "reads : publish : exists = skip"
       "guard : publish = (exists <1>)"
       "effect : publish = state"
       "stage : publish = 2"
       "scope : publish = program"
       "fuel : publish = 3"
       "ensure : publish : present = (bound ledger <1>)")))
  (check
   (answer-lines attributed "publish")
   '("? : primitive : publish : target value"
     "? : file : operations.typify"
     "? : is : target = canonical-path"
     "? : is : value = /.*/"
     "? : writes : publish : ledger = put 1"
     "? : reads : publish : exists = skip"
     "? : guard : publish = (exists <1>)"
     "? : effect : publish = state"
     "? : stage : publish = 2"
     "? : scope : publish = program"
     "? : fuel : publish = 3"
     "? : ensure : publish : present = (bound ledger <1>)"
     "? : count : publish = *"
     "? : missing : empty")
   "a primitive answers all its attributes without its cell's declaration")
  (define joined
    (body
     '("is : mode = \"cold\" | \"warm\""
       "cell : registry = join heat"
       "primitive : store : value mode"
       "writes : store : registry = put 1"
       "join : heat : \"cold\" \"warm\" = \"warm\"")))
  (check
   (answer-lines joined "registry")
   '("? : cell : registry = join heat" "? : join : heat : \"cold\" \"warm\" = \"warm\"")
   "a cell answers its declaration and its table, with no constructor tail")
  (check
   (question-candidates joined)
   '("store")
   "neither a cell nor its table or value class is a candidate")
  (define built-in-verbs
    (body
     '("term : datum : value"
       "form : letter : <value>"
       "does : datum <value>"
       "form : canonical-path : <value>"
       "does : datum <value>"
       "form : relative-path : <value>"
       "does : datum <value>")))
  (check
   (question-candidates built-in-verbs)
   '("canonical-path" "datum" "letter" "relative-path")
   "literal form verbs remain candidates when spelled like built-in classes")
  (for
   ((word '("letter" "canonical-path" "relative-path")))
   (check
    (hash-ref (ask-language built-in-verbs word) 'status)
    "declared"
    "a built-in spelling is declared by a literal form verb"))
  (check
   (answer-lines built-in-verbs "letter")
   '("? : form : letter : <value>"
     "? : does : datum <value>"
     "? : term : datum : value"
     "? : is : value = /.*/"
     "? : count : datum = *"
     "? : missing : empty")
   "a built-in-spelled form contributes no fabricated built-in declaration")
  (define all-kinds
    (body
     '("file : records.typify"
       "is : payload = /[a-z]+/"
       "is : mode = \"lo\" | \"hi\""
       "is : shared = \"a\""
       "cell : shared = join heat"
       "primitive : commit : value mode"
       "writes : commit : shared = put 1"
       "join : heat : \"lo\" \"hi\" = \"hi\""
       "term : shared : payload"
       "fact : shared"
       "form : shared : <payload>"
       "does : shared <payload>"
       "example : shared : oak"
       "count : shared = 0..1"
       "note : shared = shared-note")))
  (check
   (answer-lines all-kinds "shared")
   '("? : form : shared : <payload>"
     "? : file : records.typify"
     "? : does : shared <payload>"
     "? : term : shared : payload"
     "? : is : payload = /[a-z]+/"
     "? : fact : shared"
     "? : example : shared : oak"
     "? : count : shared = 0..1"
     "? : missing : empty"
     "? : note : shared = shared-note"
     "? : is : shared = \"a\""
     "? : cell : shared = join heat"
     "? : join : heat : \"lo\" \"hi\" = \"hi\"")
   "a spelling shared by every namespace answers their ordered union")
  (define grouped-shared-class
    (body
     '("is : shared = \"x\""
       "cell : shared = shadow"
       "term : alpha : shared"
       "term : beta : shared"
       "form : shared : a <shared>"
       "does : alpha <shared>"
       "form : shared : b <shared>"
       "does : beta <shared>")))
  (check
   (answer-lines grouped-shared-class "shared")
   '("? : form : shared : a <shared>"
     "? : does : alpha <shared>"
     "? : form : shared : b <shared>"
     "? : does : beta <shared>"
     "? : term : alpha : shared"
     "? : is : shared = \"x\""
     "? : term : beta : shared"
     "? : is : shared = \"x\""
     "? : count : alpha = *"
     "? : missing : empty"
     "? : count : beta = *"
     "? : missing : empty"
     "? : cell : shared = shadow")
   "a class appendage is deduplicated while per-constructor copies remain")
  (define class-and-cell
    (body '("file : ignored.typify" "is : shared = \"x\"" "cell : shared = shadow")))
  (check
   (answer-lines class-and-cell "shared")
   '("? : is : shared = \"x\"" "? : cell : shared = shadow")
   "a class and cell combine without acquiring a constructor tail or file line")
  (define rewritten
    (body
     '("primitive : output : value"
       "effect : output = external"
       "term : decorate : value"
       "rule : decorate <x> -> output <x>"
       "form : transform : <value>"
       "does : decorate <value>")))
  (check
   (answer-lines rewritten "transform")
   '("? : form : transform : <value>"
     "? : does : decorate <value>"
     "? : term : decorate : value"
     "? : is : value = /.*/"
     "? : rule : decorate <x> -> output <x>"
     "? : count : decorate = *"
     "? : missing : empty")
   "rewritten constructor answers include the authored rule")
  (define fact
    (body
     '("primitive : output : value"
       "effect : output = external"
       "term : datum : value"
       "fact : datum"
       "form : remember : <value>"
       "does : datum <value>")))
  (check
   (answer-lines fact "datum")
   '("? : form : remember : <value>"
     "? : does : datum <value>"
     "? : term : datum : value"
     "? : is : value = /.*/"
     "? : fact : datum"
     "? : count : datum = *"
     "? : missing : empty")
   "fact attributes are retained in their constructor block")
  (define scoped
    (read-declarations
     (string-append (source '("term : alpha") "First") "declare : Second\nterm : beta\nend!\n")))
  (check
   (hash-ref (ask-language (hash-ref scoped "First") "beta") 'status)
   "undeclared"
   "other bodies do not declare words in the asked language")
  (define note
    (make-diagnostic
     "N_CLASS_EMPTY"
     "Synthetic declaration note"
     #:severity
     'note
     #:data
     (hash 'class "empty")))
  (define noted (ask-language catalog "item" #:diagnostics (list note)))
  (check
   (hash-ref noted 'diagnostics)
   (list (diagnostic->jsexpr note))
   "declaration notes remain in the answer's diagnostics")
  (check
   (map
    diagnostic-code
    (refuse (lambda () (ask-language catalog "bad_word" #:diagnostics (list note)))))
   '("E_USAGE" "N_CLASS_EMPTY")
   "usage retains declaration notes under D1 ordering")
  (define meta (meta-language))
  (check
   (answer-lines meta "?")
   '("? : form : ? : <verb>"
     "? : does : question <verb>"
     "? : primitive : question : verb"
     "? : is : verb = /[A-Za-z0-9][A-Za-z0-9-]*/"
     "? : effect : question = external"
     "? : example : ? : form"
     "? : count : question = *"
     "? : missing : empty")
   "the meta body's question answers from its canonical declaration")
  (define untrimmed
    (body
     '("is : code = \"x\""
       "term : datum : code"
       "  form : show : [<code>]  "
       "does : datum   <code>")))
  (check
   (take (answer-lines untrimmed "show") 2)
   '("? : form : show : [<code>]" "? : does : datum   <code>")
   "answers strip only outer blanks and preserve authored inner spelling")
  (for
   ((answer
     (in-list
      (list
       (ask-language catalog "item")
       (ask-language catalog "itemm")
       (ask-language catalog "zzzzz")
       (ask-language meta "?")
       noted))))
   (check (question-answer-roundtrip? answer) #t "answer lines pass the structural round trip"))
  (define unknown (ask-language catalog "itemm"))
  (check
   (question-answer-roundtrip? (hash-set unknown 'near '("view")))
   #f
   "round trip detects a near-list mismatch")
  (check
   (question-answer-roundtrip?
    (hash-set unknown 'lines '("? : itemm = undeclared\n? : near : item")))
   #f
   "every answer string must contain exactly one line")
  (check
   (question-answer-roundtrip? (hash-set unknown 'accepts '("view : <category> = <title>")))
   #f
   "v1 round trip retains an empty accepts menu")
  (check
   (question-answer-roundtrip? (hash-set (ask-language catalog "item") 'lines '("item : a = oak")))
   #f
   "a declaration answer cannot escape the reserved question verb")
  (hash 'suite "questions" 'checks checks 'status "passed"))

(module+ test (run-tests))
