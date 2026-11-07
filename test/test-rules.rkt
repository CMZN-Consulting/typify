#lang racket/base

(require racket/list
         racket/string
         "../src/language.rkt"
         "../src/model.rkt"
         "../src/classes.rkt"
         "../src/diagnostics.rkt"
         "../src/syntax.rkt")

(provide run-tests)

(define (body rows)
  (hash-ref
   (read-declarations
    (string-append
     (string-join
      (append '("#lang typify" "declare : Rules" "cap : length = 64") rows '("end!"))
      "\n")
     "\n")
    #:file
    "rules.typify")
   "Rules"))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-rules "~a: expected ~s, got ~s" context expected actual)))
  (define (reject thunk code)
    (define item
      (with-handlers
       ((exn:fail:typify? exn:fail:typify-diagnostic))
       (thunk)
       (error 'test-rules "The invalid operation was accepted")))
    (check (diagnostic-code item) code "native refusal")
    item)
  (define (expand
           language
           source
           #:context
           (context #f)
           #:fact
           (fact (lambda (name args origin) #f)))
    (define checked (check-program source language #:file "program.typify"))
    (define options
      (if context
        (primitive-events
         language
         (hash-ref checked 'terms)
         (hash-ref checked 'origins)
         #:context
         context
         #:fact
         fact
         #:spans
         (hash-ref checked '_term-spans))
        (primitive-events
         language
         (hash-ref checked 'terms)
         (hash-ref checked 'origins)
         #:fact
         fact
         #:spans
         (hash-ref checked '_term-spans))))
    (check
     (<=
      (length (hash-ref options 'events))
      (* (hash-ref (declaration-bound language) 'c) (hash-ref checked 'effective-lines)))
     #t
     "primitive count stays within the effective-line bound")
    options)
  (define sequence
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : pair : value"
       "rule : pair <x> -> emit (concat <x> -a) ; emit (concat <x> -b)"
       "form : run : <value>"
       "does : pair <value>")))
  (define sequential (expand sequence "run : seed"))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref sequential 'events))
   '(("emit" "seed-a") ("emit" "seed-b"))
   "rule terms expand left to right")
  (check
   (hash-ref (car (hash-ref sequential 'events)) 'origin)
   (hash 'kind "program" 'path "program.typify" 'line 1)
   "primitive origins are unified objects")
  (check (declaration-bound sequence) (hash 'c 2 'k 1) "sequence bound sums emitted effects")
  (check
   (normalize-terms sequence '(("pair" "seed")))
   '(("emit" "seed-a") ("emit" "seed-b"))
   "normalization uses the same expansion kernel")
  (define conditional
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : maybe : value"
       "rule : maybe <x> -> (if (prefix a <x>) (emit <x>))"
       "form : run : <value>"
       "does : maybe <value>")))
  (check
   (length (hash-ref (expand conditional "run : abc") 'events))
   1
   "true predicates emit the effect")
  (define skipped (expand conditional "run : z"))
  (check (hash-ref skipped 'events) '() "false predicates emit nothing")
  (check
   (hash-ref (car (hash-ref skipped 'skips)) 'origin)
   (hash 'kind "program" 'path "program.typify" 'line 1)
   "skips retain origin")
  (define fact-conditional
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : maybe : value"
       "rule : maybe <x> -> (if (exists <x>) (emit <x>))"
       "form : run : <value>"
       "does : maybe <value>")))
  (check (language-needs-facts? fact-conditional) #t "external predicates require facts")
  (check (language-needs-facts? conditional) #f "pure predicates do not require facts")
  (check
   (length
    (hash-ref
     (expand
      fact-conditional
      "run : entry"
      #:fact
      (lambda (name args origin) (values #t "metadata")))
     'events))
   1
   "a present fact is a predicate even when its value is text")
  (reject
   (lambda ()
     (body
      '("primitive : emit : value"
        "effect : emit = external"
        "term : maybe : value"
        "rule : maybe <x> -> (if exists (emit <x>))")))
   "E_DECLARATION")
  (define each
    (body
     '("is : path = canonical-path"
       "primitive : emit : value"
       "effect : emit = external"
       "term : batch : path"
       "fuel : batch = 3"
       "rule : batch <p> -> (each <x> in (split <p>) (emit <x>))"
       "form : run : <path>"
       "does : batch <path>")))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref (expand each "run : /a/b/c") 'events))
   '(("emit" "a") ("emit" "b") ("emit" "c"))
   "each follows component order")
  (check (hash-ref (expand each "run : /") 'events) '() "an empty component list emits nothing")
  (define fuel-error (reject (lambda () (expand each "run : /a/b/c/d")) "E_RULE_FUEL"))
  (check
   (diagnostic-data-projection (diagnostic-code fuel-error) (diagnostic-data fuel-error))
   (hash 'constructor "batch" 'limit 3 'actual 4)
   "fuel reports the carrying constructor and list length")
  (define nested
    (body
     '("is : path = canonical-path"
       "primitive : emit : value"
       "effect : emit = external"
       "term : batch : path"
       "fuel : batch = 3"
       "rule : batch <p> -> (each <x> in (split <p>) (each <y> in (split <p>) (emit (concat <x> <y>))))"
       "form : run : <path>"
       "does : batch <path>")))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref (expand nested "run : /a/b") 'events))
   '(("emit" "aa") ("emit" "ab") ("emit" "ba") ("emit" "bb"))
   "nested loops preserve lexical bindings and order")
  (check
   (length (hash-ref (expand nested "run : /a/b/c") 'events))
   9
   "fuel applies independently to every loop")
  (check (declaration-bound nested) (hash 'c 9 'k 1) "nested loop bound multiplies fuel caps")
  (define contextual
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : base : value"
       "fact : base"
       "term : wrapper : value"
       "rule : wrapper <x> -> emit (context base)"
       "form : run : <value>"
       "does : wrapper <value>")))
  (check
   (map
    (lambda (event) (hash-ref event 'term))
    (hash-ref
     (expand
      contextual
      "run : item"
      #:context
      (lambda (name origin) (check name "base" "context names constructor") "/root"))
     'events))
   '(("emit" "/root"))
   "context uses the enclosing-program callback")
  (define context-error (reject (lambda () (expand contextual "run : item")) "E_CONTEXT"))
  (check
   (diagnostic-data-projection "E_CONTEXT" (diagnostic-data context-error))
   (hash 'constructor "base" 'key "")
   "missing context has the canonical data")
  (define guarded
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : missing : value"
       "fact : missing"
       "term : guarded : value value"
       "guard : guarded = exists"
       "rule : guarded <key> <x> -> emit (parent /)"
       "form : run : <value>"
       "does : guarded <value> (context missing)")))
  (define guarded-result
    (check-program
     "run : key"
     guarded
     #:file
     "program.typify"
     #:facts
     (lambda (name args origin)
       (check args '("key") "bare rewritten guard uses only its first argument")
       (values #f #f))))
  (check (hash-ref guarded-result 'guarded?) #t "a false guard marks the program guarded")
  (check (hash-ref guarded-result 'terms) '() "guarded terms are empty")
  (check (hash-ref guarded-result 'origins) '() "guarded origins are not public context")
  (check
   (hash-ref guarded-result 'raw-terms)
   '(("guarded" "key" ("context" "missing")))
   "guard preflight retains unevaluated audit data")
  (check
   (hash-ref guarded-result 'skips)
   (list
    (hash
     'constructor
     "guarded"
     'fact
     "exists"
     'args
     '("key")
     'reason
     "guard"
     'origin
     (hash 'kind "program" 'path "program.typify" 'line 1)))
   "the guard records exactly the fact arguments it evaluated")
  (reject
   (lambda ()
     (check-program "run : key" guarded #:facts (lambda (name args origin) (values #t #t))))
   "E_CONTEXT")
  (define scoped
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "stage : emit = 4"
       "term : inner : value"
       "scope : inner = parent"
       "rule : inner <x> -> emit <x>"
       "term : outer : value"
       "scope : outer = root"
       "rule : outer <x> -> emit <x> ; inner <x>"
       "form : run : <value>"
       "does : outer <value>")))
  (define scoped-events (hash-ref (expand scoped "run : key") 'events))
  (check
   (map (lambda (event) (hash-ref event 'scope)) scoped-events)
   '("root" "parent")
   "scope is inherited until an inner constructor overrides it")
  (check
   (map (lambda (event) (hash-ref event 'scope-constructor)) scoped-events)
   '("outer" "inner")
   "events retain the constructor that selected scope")
  (check
   (map (lambda (event) (hash-ref event 'stage)) scoped-events)
   '(4 4)
   "events carry primitive stages")
  (define ensured
    (body
     '("cell : items = shadow"
       "primitive : put : value"
       "writes : put : items = put 1"
       "ensure : put : primitive-mask = (masked items <1>)"
       "term : carry : value"
       "scope : carry = root"
       "rule : carry <x> -> put <x>"
       "ensure : carry : carried-mask = (masked items <1>)"
       "form : run : <value>"
       "does : carry <value>")))
  (define ensure-tasks (hash-ref (expand ensured "run : alpha") 'ensures))
  (check (length ensure-tasks) 2 "every carrying term records its invariant once")
  (check
   (map (lambda (task) (hash-ref task 'arguments-resolved?)) ensure-tasks)
   '(#t #t)
   "invariant bindings are recorded after their constructor's receiving resolution")
  (check
   (map (lambda (task) (hash-ref task 'scope)) ensure-tasks)
   '("root" "root")
   "invariants inherit the same scope as their carrying terms")
  (for
   ((task (in-list ensure-tasks)))
   (check
    (evaluate-rule-expression
     ensured
     (model-ensure-expression (hash-ref task 'ensure))
     (hash-ref task 'bindings)
     #:origin
     (hash-ref task 'origin)
     #:fact
     (lambda (name args origin)
       (check name "masked" "the state predicate name is retained")
       (check args '("items" "alpha") "invariants bind positional arguments and explicit cells")
       #t))
    #t
    "invariants use the shared expression evaluator"))
  (reject
   (lambda ()
     (body
      '("primitive : emit : value"
        "effect : emit = external"
        "term : batch : value"
        "fuel : batch = 3"
        "rule : batch <x> -> (each <item> in (exists <x>) (emit <item>))")))
   "E_DECLARATION")
  (define relative
    (body
     '("is : local = relative-path"
       "primitive : emit : canonical-path"
       "effect : emit = external"
       "term : request : local"
       "rule : request <p> -> emit <p>"
       "form : run : <local>"
       "does : request <local>")))
  (define relative-checked (check-program "run : cache" relative #:file "program.typify"))
  (define resolved
    (primitive-events
     relative
     (hash-ref relative-checked 'terms)
     (hash-ref relative-checked 'origins)
     #:resolve
     (lambda (body name index class value origin scope scope-constructor)
       (if (relative-path-role? (language-classes body) class)
         (values (string-append "/root/" value) "canonical-path")
         (values value class)))))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref resolved 'events))
   '(("emit" "/root/cache"))
   "a relative receiving term resolves before its rule's canonical recipient")
  (reject
   (lambda ()
     (body
      '("is : local = relative-path"
        "primitive : emit : canonical-path"
        "effect : emit = external"
        "form : run : <local>"
        "does : emit <local>")))
   "E_TERM_CLASS")
  (define defaulted
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : pair : value"
       "rule : pair <x> -> emit <x> ; emit <x>"
       "form : run : <value>"
       "does : pair <value>"
       "default : run : first"
       "default : run : second")))
  (define checked-defaults (check-program "" defaulted))
  (check (hash-ref checked-defaults 'effective-lines) 2 "active defaults count as effective lines")
  (define default-events
    (primitive-events
     defaulted
     (hash-ref checked-defaults 'terms)
     (hash-ref checked-defaults 'origins)))
  (check (length (hash-ref default-events 'events)) 4 "default line expansion uses the same bound")
  (check
   (map
    (lambda (event) (hash-ref (hash-ref event 'origin) 'kind))
    (hash-ref default-events 'events))
   '("default" "default" "default" "default")
   "expanded defaults retain default origins")
  (define exact-source "# note\n  run : 😀  \n run : again\n")
  (define exact-checked (check-program exact-source contextual #:file "precise.typify"))
  (define exact-records (read-lines exact-source #:file "precise.typify"))
  (check
   (hash-ref exact-checked '_term-spans)
   (list (line-record-span (cadr exact-records)) (line-record-span (caddr exact-records)))
   "private spans preserve Unicode columns and UTF-8 offsets")
  (define exact-errors
    (with-handlers
     ((exn:fail:typify? exn:fail:typify-diagnostics))
     (primitive-events
      contextual
      (hash-ref exact-checked 'terms)
      (hash-ref exact-checked 'origins)
      #:spans
      (hash-ref exact-checked '_term-spans))
     (error 'test-rules "missing contexts were accepted")))
  (check
   (map diagnostic-code exact-errors)
   '("E_CONTEXT" "E_CONTEXT")
   "independent source terms each report their first runtime error")
  (check
   (map diagnostic-range exact-errors)
   (hash-ref exact-checked '_term-spans)
   "runtime context errors use exact producing lines")
  (define controlled-cycle
    (string-join
     '("#lang typify"
       "declare : Cycle"
       "primitive : emit : value"
       "effect : emit = external"
       "term : earlier : value"
       "term : later : value"
       "rule : earlier <x> -> (if (equal <x> <x>) (later <x>))"
       "rule : later <x> -> earlier <x>"
       "end!")
     "\n"))
  (define-values
   (cycle-model cycle-diagnostics)
   (read-declarations/diagnostics controlled-cycle #:file "cycle.typify"))
  (check
   (map diagnostic-code cycle-diagnostics)
   '("E_DECLARATION" "E_RULE_CYCLE")
   "control bodies preserve SCC detection and independent stratification")
  (define unknown-form
    (string-join
     '("#lang typify"
       "declare : Unknown"
       "primitive : emit : value"
       "effect : emit = external"
       "form : run : <missing>"
       "does : emit <other>"
       "end!")
     "\n"))
  (define-values
   (unknown-model unknown-diagnostics)
   (read-declarations/diagnostics unknown-form #:file "unknown.typify"))
  (check
   (map (lambda (item) (hash-ref (diagnostic-data item) 'actual)) unknown-diagnostics)
   '("<missing>" "<other>")
   "stage-two compilation preserves D14 capture recovery")
  (define relative-guard
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : hidden : value"
       "fact : hidden"
       "term : request : relative-path value"
       "guard : request = (exists <1>)"
       "rule : request <p> <x> -> emit <x>"
       "form : run : <relative-path>"
       "does : request <relative-path> (context hidden)")))
  (define resolver-factories 0)
  (define relative-guarded
    (check-program
     "run : cache"
     relative-guard
     #:file
     "guard.typify"
     #:facts
     (lambda (name args origin)
       (check name "exists" "guard uses its declared fact")
       (check args '("/work/cache") "guard resolves nominal relative arguments")
       #f)
     #:guard-resolver
     (lambda (terms origins)
       (set! resolver-factories (add1 resolver-factories))
       (check
        (car terms)
        '("request" "cache" ("context" "hidden"))
        "factory sees raw arguments before unrelated evaluation")
       (lambda (language name index class value origin scope carrier)
         (if (relative-path-role? (language-classes language) class)
           (values (string-append "/work/" value) "canonical-path")
           (values value class))))))
  (check resolver-factories 1 "guard resolver is built once after raw collection")
  (check (hash-ref relative-guarded 'guarded?) #t "false relative guard skips the whole program")
  (check (hash-ref relative-guarded 'terms) '() "guarded data is never context-visible")
  (check
   (hash-ref relative-guarded '_term-spans)
   '()
   "guarded public terms have no private term spans")
  (check
   (length (hash-ref relative-guarded '_raw-term-spans))
   1
   "guarded raw evidence keeps its producing source span")
  (check
   (hash-ref (car (hash-ref relative-guarded 'skips)) 'args)
   '("/work/cache")
   "skip evidence records the exact resolved guard key")
  (define inherited-body
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : source : value"
       "fact : source"
       "term : collect : value"
       "fuel : collect = 3"
       "rule : collect <x> -> (each <item> in (inherited source) (emit <item>))"
       "form : collect : <value>"
       "does : collect <value>"
       "example : collect : sample")))
  (define inherited-checked (check-program "collect : sample" inherited-body))
  (define inherited-values '("near-first" "near-second" "far-first"))
  (define (inherit name origin)
    (check name "source" "inherited names the declared constructor")
    inherited-values)
  (define inherited-events
    (primitive-events
     inherited-body
     (hash-ref inherited-checked 'terms)
     (hash-ref inherited-checked 'origins)
     #:spans
     (hash-ref inherited-checked '_term-spans)
     #:inherited
     inherit))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref inherited-events 'events))
   '(("emit" "near-first") ("emit" "near-second") ("emit" "far-first"))
   "inherited expands every ordered first argument")
  (check
   (declaration-bound inherited-body)
   (hash 'c 3 'k 1)
   "inherited loops contribute their declared fuel bound")
  (check
   (hash-ref
    (primitive-events
     inherited-body
     (hash-ref inherited-checked 'terms)
     (hash-ref inherited-checked 'origins)
     #:spans
     (hash-ref inherited-checked '_term-spans)
     #:inherited
     (lambda (name origin) '()))
    'events)
   '()
   "an empty inheritance list emits no terms")
  (reject
   (lambda ()
     (primitive-events
      inherited-body
      (hash-ref inherited-checked 'terms)
      (hash-ref inherited-checked 'origins)
      #:spans
      (hash-ref inherited-checked '_term-spans)
      #:inherited
      (lambda (name origin) '("a" "b" "c" "d"))))
   "E_RULE_FUEL")
  (define nearest
    (primitive-events
     contextual
     '(("wrapper" "unused"))
     (list (hash 'kind "program" 'path "near.typify" 'line 1))
     #:inherited
     (lambda (name origin) '("nearest" "later"))))
  (check
   (map (lambda (event) (hash-ref event 'term)) (hash-ref nearest 'events))
   '(("emit" "nearest"))
   "context is the first element of inherited")
  (define declared-example
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "term : source : value"
       "fact : source"
       "form : run!"
       "does : emit (context source)"
       "example : run!")))
  (check
   (length (language-forms declared-example))
   1
   "examples validate formation without executing context lookups")
  (check
   (hash-ref
    (check-program "run!" declared-example #:inherited (lambda (name origin) '("ancestor")))
    'terms)
   '(("emit" "ancestor"))
   "located programs execute the deferred lookup")
  (for
   ((name (in-list '("context" "inherited"))))
   (define reference
     (reject
      (lambda ()
        (body
         (list
          "primitive : emit : value"
          "effect : emit = external"
          "term : collect : value"
          "fuel : collect = 2"
          (if (equal? name "context")
            "rule : collect <x> -> emit (context unknown)"
            "rule : collect <x> -> (each <item> in (inherited unknown) (emit <item>))"))))
      "E_DECLARATION"))
   (check
    (diagnostic-data-projection "E_DECLARATION" (diagnostic-data reference))
    (hash 'kind "reference" 'actual "unknown")
    "lookup constructors must be declared before any program is read"))
  (define cross-language
    (read-declarations
     (string-join
      '("#lang typify"
        "declare : Child"
        "primitive : emit : value"
        "effect : emit = external"
        "form : run!"
        "does : emit (context global-source)"
        "example : run!"
        "end!"
        "declare : Parent"
        "term : global-source : value"
        "form : source : <value>"
        "does : global-source <value>"
        "end!")
      "\n")))
  (check
   (sort (hash-keys cross-language) string<?)
   '("Child" "Parent")
   "lookup references may name constructors declared in another body")
  (define template-body
    (body
     '("is : slot = /[0-9]+/"
       "file : SEAT[<slot>]/SEAT.typify"
       "primitive : emit : value value"
       "effect : emit = external"
       "term : request : value"
       "rule : request <x> -> emit <slot> <x>"
       "form : direct : <value>"
       "does : emit <slot> <value>"
       "example : direct : sample"
       "form : rule : <value>"
       "does : request <value>")))
  (define template-values (hash "slot" "4"))
  (define direct-template
    (check-program "direct : sample" template-body #:captures template-values))
  (check
   (hash-ref direct-template 'terms)
   '(("emit" "4" "sample"))
   "a template placeholder is bound in does")
  (define rule-template (check-program "rule : sample" template-body #:captures template-values))
  (check
   (normalize-terms template-body (hash-ref rule-template 'terms) #:captures template-values)
   '(("emit" "4" "sample"))
   "a template placeholder is bound in a rule right side")
  (define duplicate-template
    (reject
     (lambda ()
       (body
        '("is : slot = /[0-9]+/"
          "file : SEAT[<slot>]/SEAT.typify"
          "term : item : slot"
          "form : item : <slot>"
          "does : item <slot>")))
     "E_DECLARATION"))
  (check
   (diagnostic-data-projection "E_DECLARATION" (diagnostic-data duplicate-template))
   (hash 'kind "duplicate" 'actual "<slot>")
   "a form cannot shadow a template placeholder")
  (reject
   (lambda () (normalize-terms sequence '(("emit" ("concat" ("parent" "/") ("join" "/a" "/b"))))))
   "E_LIBRARY_PARENT")
  (reject
   (lambda () (normalize-terms sequence '(("emit" ("concat" ("join" "/a" "/b") ("parent" "/"))))))
   "E_LIBRARY_DOMAIN")
  (check
   (language-needs-facts? inherited-body)
   #f
   "inheritance alone does not require a facts file")
  (define ensure-only-fact
    (body
     '("primitive : emit : value"
       "effect : emit = external"
       "ensure : emit : present = (exists <1>)"
       "form : run : <value>"
       "does : emit <value>")))
  (check
   (language-needs-facts? ensure-only-fact)
   #f
   "only guard, reads and fact calls inside if require facts")
  (hash 'suite "rules" 'checks checks 'status "passed"))

(module+ main (displayln (canonical-json (run-tests))))

(module+ test (run-tests))
