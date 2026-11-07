#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/string
         "../src/diagnostics.rkt"
         "../src/language.rkt"
         "../src/plan.rkt"
         "../src/tree.rkt")

(provide run-tests)

(define (declarations . bodies)
  (read-declarations
   (string-append "#lang typify\n" (string-join bodies "\n"))
   #:file
   "layout.typify"))

(define (body name file (extra "") #:required? (required? #f))
  (string-append
   "declare : "
   name
   "\ncap : length = 64\n"
   "is : area = /[0-9]+/\nis : document = /[A-Za-z]+/\n"
   "is : word = /[A-Za-z]+/\nprimitive : mark : word\neffect : mark = external\n"
   "form : mark : <word>\ndoes : mark <word>\n"
   extra
   "\nfile : "
   file
   "\n"
   (if required? "required!\n" "optional!\n")
   "end!\n"))

(define (write-file root relative text)
  (define path (build-path root relative))
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'truncate/replace))

(define (diagnostic-of thunk)
  (with-handlers
   ((exn:fail:typify? (lambda (failure) (exn:fail:typify-diagnostic failure))))
   (thunk)
   #f))

(define (run-tests)
  (define count 0)
  (define passed 0)
  (define failures '())
  (define (check name thunk expected)
    (set! count (add1 count))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! failures (cons (hash 'name name 'error (exn-message failure)) failures)))))
     (define actual (thunk))
     (if (equal? actual expected)
       (set! passed (add1 passed))
       (set! failures (cons (hash 'name name 'mismatch #t) failures)))))
  (define temporary (make-temporary-file "tree-model~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define root (build-path temporary "root"))
     (make-directory root)
     (write-file root "z-root.data" "mark : Root\n")
     (write-file root "a-root.data" "mark : Other\n")
     (write-file root "A2/child.data" "mark : Second\n")
     (write-file root "A1/child.data" "mark : First\n")
     (write-file root "Ax/child.data" "mark : Ignored\n")
     (write-file root "A2/default.data" "")
     (define model
       (declarations
        (body "FirstRoot" "z-root.data")
        (body "SecondRoot" "a-root.data")
        (body "Child" "A<area>/child.data")
        (body "Defaulted" "A<area>/default.data" "default : mark : Default\ncount : mark = 1")
        (body "Optional" "A<area>/optional.data" "count : mark = 1")))
     (define programs (read-tree model root))
     (check
      "global-depth-path-language-order"
      (lambda () (map (lambda (program) (hash-ref program 'path)) programs))
      '("a-root.data"
        "z-root.data"
        "A1/child.data"
        "A1/default.data"
        "A1/optional.data"
        "A2/child.data"
        "A2/default.data"
        "A2/optional.data"))
     (check
      "directory-bindings-are-class-checked"
      (lambda ()
        (for/list
         ((program (in-list programs))
          #:when
          (equal? (language-name (hash-ref program 'body)) "Child"))
         (hash-ref program 'placeholders)))
      (list (hash "area" "1") (hash "area" "2")))
     (check
      "optional-and-default-statuses"
      (lambda () (map (lambda (program) (hash-ref program 'status)) programs))
      '("present"
        "present"
        "present"
        "defaulted"
        "absent-empty"
        "present"
        "present"
        "absent-empty"))
     (check
      "default-origin-is-declaration-and-own-origin-is-program"
      (lambda ()
        (list
         (hash-ref (car (hash-ref (third programs) 'origins)) 'kind)
         (hash-ref (car (hash-ref (third programs) 'origins)) 'path)
         (hash-ref (car (hash-ref (fourth programs) 'origins)) 'kind)
         (hash-ref (car (hash-ref (fourth programs) 'origins)) 'path)))
      '("program" "A1/child.data" "default" "layout.typify"))
     (check
      "private-producing-spans-cover-own-and-default-lines"
      (lambda ()
        (for/list
         ((program (in-list (list (third programs) (fourth programs)))))
         (define origin (car (hash-ref program 'origins)))
         (define span (car (hash-ref program '_term-spans)))
         (list
          (src-span-file span)
          (src-span-start-line span)
          (equal?
           span
           (hash-ref
            (hash-ref program '_source-spans)
            (cons (hash-ref origin 'path) (hash-ref origin 'line)))))))
      '(("A1/child.data" 1 #t) ("layout.typify" 53 #t)))
     (check
      "proper-ancestor-nesting-and-declaration-order-ties"
      (lambda ()
        (list
         (hash-ref (car programs) 'enclosing)
         (map
          (lambda (program) (language-name (hash-ref program 'body)))
          (hash-ref (third programs) 'enclosing))))
      (list '() '("FirstRoot" "SecondRoot")))
     (check
      "wire-carries-language-without-host-paths-or-internal-audit-fields"
      (lambda ()
        (define wire (tree->jsexpr programs))
        (list (hash-ref (car wire) 'language) (sort (hash-keys (car wire)) symbol<?)))
      (list "SecondRoot" '(language origins path placeholders status terms)))
     (check
      "same-file-located-by-two-languages-stays-adjacent"
      (lambda ()
        (map
         (lambda (program) (language-name (hash-ref program 'body)))
         (read-tree
          (declarations (body "Earlier" "z-root.data") (body "Later" "z-root.data"))
          root)))
      '("Earlier" "Later"))
     (check
      "literal-required-missing-file"
      (lambda ()
        (define item
          (diagnostic-of
           (lambda ()
             (read-tree (declarations (body "Required" "missing.data" #:required? #t)) root))))
        (list (diagnostic-code item) (diagnostic-data item) (diagnostic-range item)))
      (list "E_REQUIRED" (hash 'path "missing.data") #f))
     (define invalid-root (build-path temporary "invalid-programs"))
     (make-directory invalid-root)
     (write-file invalid-root "a.data" "unshaped\n")
     (write-file invalid-root "b.data" "mark : 123\n")
     (check
      "tree-aggregates-independent-program-and-required-file-errors"
      (lambda ()
        (define report
          (read-tree/diagnostics
           (declarations
            (body "BadShape" "a.data")
            (body "BadClass" "b.data")
            (body "Missing" "c.data" #:required? #t))
           invalid-root))
        (list
         (hash-ref report 'ok)
         (map
          (lambda (item) (list (diagnostic-file item) (diagnostic-code item)))
          (hash-ref report 'diagnostics))
         (map (lambda (program) (hash-ref program 'status)) (hash-ref report 'programs))))
      '(#f
        (("a.data" "E_LINE_SHAPE") ("b.data" "E_CLASS") ("c.data" "E_REQUIRED"))
        ("present" "present" "missing")))
     (make-directory* (build-path root "D1"))
     (write-file root "D1/item-Alpha.data" "mark : Alpha\n")
     (write-file root "D1/item-123.data" "mark : Ignored\n")
     (make-directory* (build-path root "D1" "item-Directory.data"))
     (check
      "fresh-filename-capture-matches-existing-files-only"
      (lambda ()
        (map
         (lambda (program) (list (hash-ref program 'path) (hash-ref program 'placeholders)))
         (read-tree (declarations (body "Files" "D<area>/item-<document>.data")) root)))
      (list (list "D1/item-Alpha.data" (hash "area" "1" "document" "Alpha"))))
     (check
      "required-filename-capture-does-not-invent-missing-files"
      (lambda ()
        (define item
          (diagnostic-of
           (lambda ()
             (discover-tree
              (declarations (body "None" "D<area>/absent-<document>.data" #:required? #t))
              root))))
        (list (diagnostic-code item) (diagnostic-data item)))
      (list "E_REQUIRED" (hash 'path "D<area>/absent-<document>.data")))
     (write-file root "R1/S1/value.data" "mark : Match\n")
     (write-file root "R1/S2/value.data" "mark : Ignored\n")
     (check
      "repeated-capture-across-components-is-one-binding"
      (lambda ()
        (map
         (lambda (program) (hash-ref program 'path))
         (read-tree (declarations (body "Repeat" "R<area>/S<area>/value.data")) root)))
      '("R1/S1/value.data"))
     (check
      "out-of-root-template-is-refused"
      (lambda ()
        (diagnostic-code
         (diagnostic-of
          (lambda () (discover-tree (declarations (body "Escape" "../outside.data")) root)))))
      "E_DECLARATION")
     (define context-root (build-path temporary "contexts"))
     (make-directory context-root)
     (write-file context-root "base.data" "base : Parent\n")
     (write-file context-root "C1/use.data" "use!\n")
     (define parent-body
       (string-append
        "declare : Parent\ncap : length = 64\nis : word = /[A-Za-z]+/\n"
        "primitive : workdir : word\neffect : workdir = external\n"
        "form : base : <word>\ndoes : workdir <word>\nfile : base.data\nend!\n"))
     (define (context-body file (guard ""))
       (string-append
        "declare : ChildContext\ncap : length = 64\nis : area = /[0-9]+/\n"
        "is : word = /[A-Za-z]+/\nprimitive : use : word\neffect : use = external\n"
        "form : use!\ndoes : use (context workdir)\n"
        guard
        "\nfile : "
        file
        "\nend!\n"))
     (check
      "context-uses-checked-proper-ancestor-terms"
      (lambda ()
        (hash-ref
         (second
          (read-tree (declarations parent-body (context-body "C<area>/use.data")) context-root))
         'terms))
      '(("use" "Parent")))
     (write-file context-root "same.data" "use!\n")
     (check
      "same-directory-programs-do-not-supply-context"
      (lambda ()
        (diagnostic-code
         (diagnostic-of
          (lambda ()
            (read-tree (declarations parent-body (context-body "same.data")) context-root)))))
      "E_CONTEXT")
     (write-file context-root "same.data" "  # résumé\r\n\tuse!  \r\n")
     (check
      "context-error-retains-the-producing-line-and-utf8-offsets"
      (lambda ()
        (diagnostic-range
         (diagnostic-of
          (lambda ()
            (read-tree (declarations parent-body (context-body "same.data")) context-root)))))
      (src-span "same.data" 2 2 2 6 15 19))
     (check
      "false-guard-precedes-a-missing-context"
      (lambda ()
        (define programs
          (read-tree
           (declarations parent-body (context-body "same.data" "guard : use = (exists /absent)"))
           context-root
           #:facts
           (hash 'collector "typify 1.0" 'root "/" 'facts '())))
        (define child (second programs))
        (list
         (hash-ref child 'status)
         (hash-ref child 'terms)
         (hash-ref child 'origins)
         (hash-ref child 'raw-terms)
         (length (hash-ref child '_raw-term-spans))
         (hash-ref (car (hash-ref child 'skips)) 'reason)))
      '("guarded" () () (("use" ("context" "workdir"))) 1 "guard"))
     (define relative-root (build-path temporary "relative-guards"))
     (make-directory relative-root)
     (write-file relative-root "root.data" "dir : /vaults\n")
     (write-file relative-root "C1/use.data" "act : item\n")
     (define relative-model
       (declarations
        (string-append
         "declare : Base\ncap : length = 64\n"
         "primitive : workdir : canonical-path\neffect : workdir = external\n"
         "form : dir : <canonical-path>\ndoes : workdir <canonical-path>\n"
         "file : root.data\nend!\n")
        (string-append
         "declare : RelativeGuard\ncap : length = 64\nis : area = /[0-9]+/\n"
         "primitive : consume : canonical-path\neffect : consume = external\n"
         "term : act : relative-path\nrule : act <path> -> consume <path>\n"
         "scope : act = parent\nguard : act = exists\n"
         "form : act : <relative-path>\ndoes : act <relative-path>\n"
         "file : C<area>/use.data\nend!\n")))
     (define (relative-records entries)
       (read-tree
        relative-model
        relative-root
        #:facts
        (hash 'collector "typify 1.0" 'root "/" 'facts entries)
        #:guard-resolver
        (lambda (program terms origins)
          (program-resolver (hash-set* program 'terms terms 'origins origins)))))
     (check
      "rewritten-bare-guard-resolves-nominal-relative-key-in-parent-scope"
      (lambda ()
        (hash-ref
         (second (relative-records (list (hash 'fact "exists" 'args '("/vaults/item")))))
         'status))
      "present")
     (check
      "unmatched-resolved-guard-keeps-raw-terms-out-of-active-context"
      (lambda ()
        (define child (second (relative-records '())))
        (list
         (hash-ref child 'status)
         (hash-ref child 'terms)
         (hash-ref child 'raw-terms)
         (hash-ref (car (hash-ref child 'skips)) 'args)))
      '("guarded" () (("act" "item")) ("/vaults/item")))
     (write-file relative-root "C1/identity.data" "identify : Alpha\n")
     (define capture-body
       (string-append
        "declare : Captured\ncap : length = 64\nis : area = /[0-9]+/\nis : word = /[A-Za-z]+/\n"
        "primitive : identify : area word\neffect : identify = external\n"
        "form : identify : <word>\ndoes : identify <area> <word>\n"
        "file : C<area>/identity.data\nend!\n"))
     (check
      "file-template-captures-are-bound-in-does"
      (lambda () (hash-ref (car (read-tree (declarations capture-body) relative-root)) 'terms))
      '(("identify" "1" "Alpha")))
     (check
      "file-template-captures-reach-rule-right-sides-at-plan-time"
      (lambda ()
        (define source
          (string-replace
           capture-body
           "form : identify : <word>\ndoes : identify <area> <word>"
           (string-append
            "term : announce : word\nrule : announce <entry> -> identify <area> <entry>\n"
            "form : identify : <word>\ndoes : announce <word>")))
        (define-values
         (report events)
         (plan-programs/details (read-tree (declarations source) relative-root)))
        (map (lambda (event) (hash-ref event 'term)) events))
      '(("identify" "1" "Alpha")))
     (define inherited-root (build-path temporary "inheritance"))
     (make-directory inherited-root)
     (write-file inherited-root "root.data" "deny : Far\ndeny : FarLast\n")
     (write-file inherited-root "nested/settings.data" "deny : Near\ndeny : NearLast\n")
     (write-file inherited-root "nested/deep/use.data" "gather!\n")
     (define (denials name file (guard ""))
       (string-append
        "declare : "
        name
        "\ncap : length = 64\nis : word = /[A-Za-z]+/\n"
        "term : deny : word\nform : deny : <word>\ndoes : deny <word>\n"
        guard
        "\nfile : "
        file
        "\nend!\n"))
     (define gather-body
       (string-append
        "declare : Gather\ncap : length = 64\nis : word = /[A-Za-z]+/\n"
        "primitive : use : word\neffect : use = external\nterm : deny : word\nfact : deny\n"
        "term : gather\nfuel : gather = 4\n"
        "rule : gather -> (each <entry> in (inherited deny) (use <entry>))\n"
        "form : gather!\ndoes : gather\nfile : nested/deep/use.data\nend!\n"))
     (define (inherited-programs (guard ""))
       (read-tree
        (declarations
         (denials "Far" "root.data")
         (denials "Near" "nested/settings.data" guard)
         gather-body)
        inherited-root
        #:facts
        (hash 'collector "typify 1.0" 'root "/" 'facts '())))
     (check
      "inherited-terms-preserve-nearest-program-and-own-line-order"
      (lambda ()
        (define-values (report events) (plan-programs/details (inherited-programs)))
        (map (lambda (event) (hash-ref event 'term)) events))
      '(("use" "Near") ("use" "NearLast") ("use" "Far") ("use" "FarLast")))
     (check
      "guarded-ancestor-terms-never-enter-inherited-lists"
      (lambda ()
        (define programs (inherited-programs "guard : deny = (exists /absent)"))
        (define-values (report events) (plan-programs/details programs))
        (list
         (hash-ref (second programs) 'status)
         (map (lambda (event) (hash-ref event 'term)) events)))
      '("guarded" (("use" "Far") ("use" "FarLast"))))
     (unless (eq? (system-type 'os) 'windows)
       (define outside (build-path temporary "outside"))
       (make-directory outside)
       (write-file outside "value.data" "mark : Outside\n")
       (make-file-or-directory-link (path->directory-path outside) (build-path root "linked"))
       (check
        "symlink-directory-is-never-walked"
        (lambda ()
          (diagnostic-code
           (diagnostic-of
            (lambda () (discover-tree (declarations (body "Link" "linked/value.data")) root)))))
        "E_FILE")
       (make-file-or-directory-link
        (build-path outside "value.data")
        (build-path root "linked.data"))
       (check
        "symlink-program-is-never-read"
        (lambda ()
          (diagnostic-code
           (diagnostic-of
            (lambda () (read-tree (declarations (body "LinkFile" "linked.data")) root)))))
        "E_FILE")))
   (lambda () (delete-directory/files temporary)))
  (hash 'suite "tree" 'count count 'pass passed 'fail (- count passed) 'details (reverse failures)))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail)) (error 'tree "tree checks failed: ~s" result)))
