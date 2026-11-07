#lang racket/base

(require racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (source rows)
  (string-append "#lang typify\ndeclare : Example\n" (string-join rows "\n") "\nend!\n"))

(define (errors text)
  (define-values (model diagnostics) (read-declarations/diagnostics text #:file "rules.typify"))
  (filter (lambda (item) (equal? (diagnostic-severity item) "error")) diagnostics))

(define (record-at text line) (list-ref (read-lines text #:file "rules.typify") (sub1 line)))

(define (term-at text line term-text)
  (define record (record-at text line))
  (define raw (line-record-raw record))
  (define position (car (regexp-match-positions (regexp (regexp-quote term-text)) raw)))
  (define span (line-record-span record))
  (span-slice
   (src-span
    "rules.typify"
    line
    1
    line
    1
    (- (src-span-start-byte span) (sub1 (src-span-start-column span)))
    (- (src-span-start-byte span) (sub1 (src-span-start-column span))))
   raw
   (car position)
   (cdr position)))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-recovery-rulings "~a: expected ~s; received ~s" context expected actual)))
  (define (project item) (diagnostic-data-projection (diagnostic-code item) (diagnostic-data item)))
  (define missing-header "  declare : Retry  \nterm : ready\nform : ready!\ndoes : ready\nend!\n")
  (define-values
   (retried header-diagnostics)
   (read-declarations/diagnostics missing-header #:file "rules.typify"))
  (check
   (map diagnostic-code header-diagnostics)
   '("E_DECLARATION")
   "header recovery retries the actual first line")
  (check
   (project (car header-diagnostics))
   (hash 'kind "header" 'actual "declare : Retry")
   "header payload follows covered text")
  (check (hash-has-key? retried "Retry") #t "the retried opener creates its body")
  (check
   (map form-source (language-forms (hash-ref retried "Retry")))
   '("ready!")
   "the body remains available for recovery")
  (define-values
   (padded-model padded-diagnostics)
   (read-declarations/diagnostics
    "  \t#lang typify\t  \ndeclare : Padded\nterm : ready\nform : ready!\ndoes : ready\nend!\n"
    #:file
    "rules.typify"))
  (check padded-diagnostics '() "all outer header blanks are stripped")
  (check (hash-has-key? padded-model "Padded") #t "padded valid header needs no recovery")
  (define tied-errors (errors "nonsense\n"))
  (check
   (map diagnostic-code tied-errors)
   '("E_DECLARATION" "E_LINE_SHAPE")
   "same-position diagnostics retain discovery order")
  (check
   (map diagnostic-range tied-errors)
   (list (src-span "rules.typify" 1 1 1 9 0 8) (src-span "rules.typify" 1 1 1 9 0 8))
   "header retry diagnostics share their exact position")
  (define nested
    (string-join
     '("#lang typify"
       "declare : Outside"
       "version : 2"
       "  declare : Inside  "
       "version : 3"
       "term : inner"
       "form : inner!"
       "does : inner"
       "end!"
       "term : outer"
       "form : outer!"
       "does : outer"
       "end!")
     "\n"))
  (define-values
   (nested-model nested-diagnostics)
   (read-declarations/diagnostics nested #:file "rules.typify"))
  (check
   (map diagnostic-code nested-diagnostics)
   '("E_DECLARATION")
   "a nested body does not create an unmatched outer end")
  (check
   (project (car nested-diagnostics))
   (hash 'kind "body" 'actual "declare : Inside")
   "nested opener payload is stripped")
  (check (language-version (hash-ref nested-model "Outside")) "2" "outer properties survive")
  (check (language-version (hash-ref nested-model "Inside")) "3" "inner properties are local")
  (check
   (hash-keys (language-constructors (hash-ref nested-model "Outside")))
   '("outer")
   "outer body resumes after inner closure")
  (check
   (hash-keys (language-constructors (hash-ref nested-model "Inside")))
   '("inner")
   "inner rows do not leak into outer body")
  (define unclosed "#lang typify\n  declare : Pending  \n")
  (check
   (map project (errors unclosed))
   (list (hash 'kind "body" 'actual "EOF"))
   "unclosed body keeps the explicit EOF sentinel")
  (define unknown-slot
    (source
     '("is : token = /[a-z]+/"
       "term : output : token"
       "form : item : <unknown>"
       "does : output <other>")))
  (define unknown-errors (errors unknown-slot))
  (check
   (map project unknown-errors)
   (list (hash 'kind "capture" 'actual "<unknown>") (hash 'kind "capture" 'actual "<other>"))
   "unknown slot classes still bind their own names for does checking")
  (define bound-unknown
    (source
     '("is : token = /[a-z]+/"
       "term : output : token"
       "form : item : <unknown>"
       "does : output <unknown>")))
  (check
   (map project (errors bound-unknown))
   (list (hash 'kind "capture" 'actual "<unknown>"))
   "a bound unknown-class slot is not spuriously unbound")
  (define missing-constructor (source '("form : item : <unknown>" "does : missing <unknown>")))
  (check
   (map project (errors missing-constructor))
   (list (hash 'kind "capture" 'actual "<unknown>") (hash 'kind "reference" 'actual "missing"))
   "independent does constructor checks do not need a compiled slot class")
  (define malformed-rule
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : action : token"
       "rule : action literal -> output literal"
       "form : item : <token>"
       "does : action <token>")))
  (check
   (map project (errors malformed-rule))
   (list (hash 'kind "syntax" 'actual "rule : action literal -> output literal"))
   "a malformed rule with a known left constructor suppresses unbound cascades")
  (define-values
   (malformed-model malformed-diagnostics)
   (read-declarations/diagnostics malformed-rule #:file "rules.typify"))
  (check
   (constructor-rule
    (hash-ref (language-constructors (hash-ref malformed-model "Example")) "action"))
   #f
   "a suppressed unbound cascade never installs a malformed executable rule")
  (define malformed-quote
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : action : token"
       "form : item : <token>"
       "does : action <token>"
       "rule : action <x> -> output \"unfinished")))
  (define-values
   (quote-model quote-diagnostics)
   (read-declarations/diagnostics malformed-quote #:file "rules.typify"))
  (check
   (map diagnostic-code quote-diagnostics)
   '("E_DECLARATION")
   "malformed quoting does not create a second unbound cascade")
  (check
   (constructor-rule (hash-ref (language-constructors (hash-ref quote-model "Example")) "action"))
   #f
   "malformed quoted rule data is absent from executable rule storage")
  (define unrecognized-rule
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : action : token"
       "form : item : <token>"
       "does : action <token>"
       "rule :")))
  (check
   (map diagnostic-code (errors unrecognized-rule))
   '("E_CONSTRUCTOR_UNBOUND" "E_DECLARATION")
   "an unreadable left side does not guess an unrelated constructor")
  (define sees-source (source '("term : ready" "sees : Missing" "form : ready!" "does : ready")))
  (define sees-error (car (errors sees-source)))
  (check (project sees-error) (hash 'kind "reference" 'actual "Missing") "sees reference data")
  (check
   (diagnostic-range sees-error)
   (line-record-span (record-at sees-source 4))
   "sees errors cover the offending sees row")
  (define unbound-source
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : waiting : token"
       "form : item : <token>"
       "does : waiting <token>")))
  (define unbound-error (car (errors unbound-source)))
  (check (diagnostic-code unbound-error) "E_CONSTRUCTOR_UNBOUND" "missing action is refused")
  (check
   (diagnostic-range unbound-error)
   (term-at unbound-source 7 "waiting <token>")
   "unbound errors cover the required does term")
  (define rule-use
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : missing : token"
       "term : caller : token"
       "rule : caller <x> -> missing <x>")))
  (define rule-use-error (car (errors rule-use)))
  (check
   (diagnostic-code rule-use-error)
   "E_CONSTRUCTOR_UNBOUND"
   "a missing rule target is refused")
  (check
   (diagnostic-range rule-use-error)
   (term-at rule-use 7 "missing <x>")
   "unbound errors can also cover a rule right-side term")
  (define cycle-source
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : earlier : token"
       "term : later : token"
       "rule : later <x> -> earlier <x>"
       "rule : earlier <x> -> later <x>")))
  (define cycle-errors (errors cycle-source))
  (check
   (map diagnostic-code cycle-errors)
   '("E_RULE_CYCLE" "E_DECLARATION")
   "one SCC cycle and an independent forward violation are both reported")
  (check
   (project (car cycle-errors))
   (hash 'constructor "later")
   "cycle identity follows earliest rule line rather than constructor order")
  (check
   (diagnostic-range (car cycle-errors))
   (term-at cycle-source 7 "later <x>")
   "cycle range covers earliest rule left term")
  (check
   (project (cadr cycle-errors))
   (hash 'kind "stratification" 'actual "later")
   "cycle membership does not suppress stratification")
  (define self-cycle
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : loop : token"
       "rule : loop <x> -> loop <x>")))
  (check
   (map diagnostic-code (errors self-cycle))
   '("E_RULE_CYCLE")
   "self cycles are reported once without a fictitious later declaration")
  (define multiple-cycles
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : first : token"
       "term : second : token"
       "rule : first <x> -> first <x>"
       "rule : second <x> -> second <x>")))
  (check
   (map project (errors multiple-cycles))
   (list (hash 'constructor "first") (hash 'constructor "second"))
   "independent SCCs each get one cycle diagnostic")
  (define arity-source
    (source
     '("is : token = /[a-z]+/"
       "term : pair : token token"
       "form : item : <token>"
       "does : pair <token>")))
  (define arity-error (car (errors arity-source)))
  (check (diagnostic-code arity-error) "E_TERM_CLASS" "does arity uses the use-site code")
  (check
   (project arity-error)
   (hash 'kind "arity" 'constructor "pair" 'class "token token" 'actual 1)
   "use-site arity carries signature and received count")
  (check
   (diagnostic-range arity-error)
   (term-at arity-source 6 "pair <token>")
   "does arity covers only the term")
  (define right-arity
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token token"
       "term : caller : token"
       "rule : caller <x> -> output <x>")))
  (define right-error (car (errors right-arity)))
  (check
   (project right-error)
   (hash 'kind "arity" 'constructor "output" 'class "token token" 'actual 1)
   "rule right-side arity is a use-site error")
  (check
   (diagnostic-range right-error)
   (term-at right-arity 6 "output <x>")
   "right-side arity retains its actual offset")
  (define left-arity
    (source
     '("is : token = /[a-z]+/"
       "primitive : output : token"
       "term : caller : token"
       "rule : caller <x> <y> -> output <x>")))
  (define left-error (car (errors left-arity)))
  (check
   (project left-error)
   (hash 'kind "arity" 'actual "caller <x> <y>")
   "rule left-side arity keeps declaration code and source text")
  (check
   (diagnostic-range left-error)
   (line-record-span (record-at left-arity 6))
   "left-side arity covers its whole rule row")
  (define missing-does (source '("term : ready" "  form : ready!  ")))
  (check
   (map project (errors missing-does))
   (list (hash 'kind "syntax" 'actual "form : ready!"))
   "missing does syntax payload includes the declaring introducer")
  (define bad-pattern (source '("  is : token = /(?=b)b/  ")))
  (check
   (map project (errors bad-pattern))
   (list (hash 'kind "syntax" 'actual "is : token = /(?=b)b/"))
   "class syntax errors project the full stripped is line")
  (define simple
    (hash-ref
     (read-declarations (source '("term : ready" "form : ready!" "does : ready")))
     "Example"))
  (define program-errors
    (hash-ref
     (check-program/diagnostics
      "  ???  \n  unknown : item  \n  ? : bad_word  "
      simple
      #:file
      "rules.typify")
     'diagnostics))
  (check
   (map diagnostic-code program-errors)
   '("E_LINE_SHAPE" "E_FORM_NONE" "E_QUESTION")
   "program line payload projection preserves refusal categories")
  (check
   (map project program-errors)
   (list (hash 'actual "???") (hash 'actual "unknown : item") (hash 'actual "? : bad_word"))
   "program actual text matches its covered range")
  (define quoted-rule
    (source
     '("is : text = /.*/"
       "primitive : output : text"
       "term : action : text"
       "rule : action <x> -> output \"a;b -> c\" ; output (concat \"x;y\" \" -> z\")"
       "form : item : <text>"
       "does : action <text>")))
  (define quoted-body (hash-ref (read-declarations quoted-rule) "Example"))
  (check
   (normalize-terms quoted-body (hash-ref (check-program "item:seed" quoted-body) 'terms))
   '(("output" "a;b -> c") ("output" "x;y -> z"))
   "quoted arrows and semicolons are data rather than rule separators")
  (hash 'suite "recovery-rulings" 'checks checks 'status "passed"))
