#lang racket/base

(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../src/diagnostics.rkt"
         "../src/facts.rkt"
         "../src/language.rkt"
         "../src/plan.rkt"
         "../src/tree.rkt")

(provide run-tests)

(define-runtime-path shell-collector "facts-collector.sh")

(define (portable path)
  (define text (path->string path))
  (if (eq? (system-type 'os) 'windows) (string-replace text "\\" "/") text))

(define (query data name arguments)
  (call-with-values (lambda () (fact-query data name arguments)) list))

(define (shell-facts root)
  (define shell (find-executable-path "sh"))
  (unless shell (error 'facts "A POSIX shell is required for the independent collector gate"))
  (define-values (process output input errors) (subprocess #f #f #f shell shell-collector root))
  (close-output-port input)
  (define text (port->string output))
  (define error-text (port->string errors))
  (close-input-port output)
  (close-input-port errors)
  (subprocess-wait process)
  (unless (zero? (subprocess-status process))
    (error 'facts "The independent collector failed: ~a" error-text))
  (string->jsexpr text))

(define (run-tests)
  (define count 0)
  (define passed 0)
  (define failures '())
  (define independent-total 0)
  (define independent-disagreements 0)
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
  (define (compare-collectors root)
    (set! independent-total (add1 independent-total))
    (with-handlers
     ((exn:fail?
       (lambda (failure)
         (set! independent-disagreements (add1 independent-disagreements))
         (raise failure))))
     (define same?
       (equal? (canonical-json (collect-facts root)) (canonical-json (shell-facts root))))
     (unless same? (set! independent-disagreements (add1 independent-disagreements)))
     same?))
  (define temporary (make-temporary-file "facts-model~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define root (build-path temporary "root"))
     (make-directory root)
     (define folder (build-path root "nested space"))
     (make-directory folder)
     (define file
       (build-path folder (if (eq? (system-type 'os) 'windows) "café.data" "café\".data")))
     (display-to-file "contents-never-enter-facts" file)
     (define outside (build-path temporary "outside"))
     (make-directory outside)
     (display-to-file "outside" (build-path outside "not-collected.data"))
     (unless (eq? (system-type 'os) 'windows)
       (file-or-directory-permissions root 488)
       (file-or-directory-permissions folder 448)
       (file-or-directory-permissions file 0)
       (display-to-file "opaque" (build-path folder "back\\slash.data"))
       (make-file-or-directory-link outside (build-path root "directory-link"))
       (make-file-or-directory-link "missing-target" (build-path root "dangling-link"))
       (make-file-or-directory-link "missing-target\n\n" (build-path root "newline-target-link")))
     (define data (collect-facts root))
     (define root-name (portable root))
     (check
      "collector-includes-root-and-normalizes-absolute-paths"
      (lambda ()
        (list
         (hash-ref data 'collector)
         (hash-ref data 'root)
         (query data "exists" (list root-name))
         (query data "directory" (list root-name))))
      (list "typify 1.0" root-name '(#t #t) '(#t #t)))
     (check
      "exact-argument-list-query-and-absence"
      (lambda ()
        (list
         (query data "exists" (list root-name "extra"))
         (query data "exists" (list (string-append root-name "/missing")))))
      '((#f #f) (#f #f)))
     (check
      "only-supported-metadata-enters-facts"
      (lambda ()
        (and (andmap
              (lambda (entry)
                (member (hash-ref entry 'fact) '("exists" "directory" "symlink" "mode" "owner")))
              (hash-ref data 'facts))
             (not (string-contains? (canonical-json data) "contents-never-enter-facts"))))
      #t)
     (check
      "owner-is-decimal-text-and-mode-is-four-octal-digits"
      (lambda ()
        (and (for/and
              ((entry (in-list (hash-ref data 'facts)))
               #:when
               (equal? (hash-ref entry 'fact) "owner"))
              (regexp-match? #px"^[0-9]+$" (hash-ref entry 'value)))
             (for/and
              ((entry (in-list (hash-ref data 'facts)))
               #:when
               (equal? (hash-ref entry 'fact) "mode"))
              (regexp-match? #px"^[0-7]{4}$" (hash-ref entry 'value)))))
      #t)
     (check
      "deterministic-canonical-fact-order"
      (lambda ()
        (equal? (hash-ref data 'facts) (sort (hash-ref data 'facts) string<? #:key canonical-json)))
      #t)
     (check "validated-data-is-preserved" (lambda () (equal? (validate-facts data) data)) #t)
     (define markers
       (validate-facts
        (hash
         'collector
         "typify 1.0"
         'root
         "/"
         'facts
         (list
          (hash 'fact "exists" 'args '("/vault"))
          (hash 'fact "directory" 'args '("/vault/nested/.obsidian"))))))
     (check
      "clean-is-derived-with-component-boundaries-and-case-sensitive-names"
      (lambda ()
        (list
         (query markers "clean" '("/vault" ".obsidian"))
         (query markers "clean" '("/vaul" ".obsidian"))
         (query markers "clean" '("/vault" ".Obsidian"))
         (query markers "clean" '("/vault/nested/.obsidian" ".obsidian"))
         (query #f "clean" '("/vault" ".obsidian"))))
      '((#f #f) (#t #t) (#t #t) (#t #t) (#f #f)))
     (check
      "clean-is-never-accepted-as-a-collector-wire-fact"
      (lambda ()
        (with-handlers
         ((exn:fail:typify?
           (lambda (failure) (diagnostic-code (exn:fail:typify-diagnostic failure)))))
         (validate-facts
          (hash
           'collector
           "typify 1.0"
           'root
           "/"
           'facts
           (list (hash 'fact "clean" 'args '("/vault" ".obsidian")))))
         "accepted"))
      "E_FILE")
     (define (clean-body clauses (direct? #f))
       (hash-ref
        (read-declarations
         (string-append
          "#lang typify\ndeclare : Clean\ncap : length = 64\n"
          "primitive : output : canonical-path\neffect : output = external\n"
          "term : act : canonical-path\n"
          "rule : act <p> -> (if (clean <p> \".obsidian\") (output <p>))\n"
          "form : output : <canonical-path>\ndoes : "
          (if direct? "output" "act")
          " <canonical-path>\n"
          clauses
          "\nend!\n"))
        "Clean"))
     (define conditional (clean-body ""))
     (check
      "clean-if-is-a-fact-dependency-only-for-plan"
      (lambda ()
        (list
         (language-needs-facts? conditional)
         (tree-facts-required? (hash "Clean" conditional))))
      '(#t #f))
     (check
      "clean-conditional-controls-primitive-expansion"
      (lambda ()
        (list
         (length (hash-ref (plan-program conditional "output : /vault" #:facts markers) 'plan))
         (length (hash-ref (plan-program conditional "output : /safe" #:facts markers) 'plan))))
      '(0 1))
     (check
      "clean-guard-is-evaluated-before-an-active-program"
      (lambda ()
        (define guarded (clean-body "guard : act = (clean <1> \".obsidian\")"))
        (define report
          (check-program/diagnostics
           "output : /vault"
           guarded
           #:facts
           (lambda (name args origin) (fact-query markers name args))))
        (list (hash-ref report 'valid) (hash-ref report 'guarded?) (hash-ref report 'terms)))
      '(#t #t ()))
     (for
      ((behavior '("skip" "reject")))
      (check
       (string-append "clean-read-" behavior)
       (lambda ()
         (define body
           (clean-body (string-append "reads : output : (clean <1> \".obsidian\") = " behavior) #t))
         (with-handlers
          ((exn:fail:typify?
            (lambda (failure) (diagnostic-code (exn:fail:typify-diagnostic failure)))))
          (define report (plan-program body "output : /vault" #:facts markers))
          (length (hash-ref report 'plan))))
       (if (equal? behavior "reject") "E_FACT_REJECT" 0)))
     (check
      "duplicate-fact-key-is-refused"
      (lambda ()
        (with-handlers
         ((exn:fail:typify?
           (lambda (failure) (diagnostic-code (exn:fail:typify-diagnostic failure)))))
         (validate-facts
          (hash-set data 'facts (list (car (hash-ref data 'facts)) (car (hash-ref data 'facts)))))
         "accepted"))
      "E_FILE")
     (check
      "list-valued-facts-are-outside-v1"
      (lambda ()
        (with-handlers
         ((exn:fail:typify?
           (lambda (failure) (diagnostic-code (exn:fail:typify-diagnostic failure)))))
         (validate-facts
          (hash
           'collector
           "typify 1.0"
           'root
           "/"
           'facts
           (list (hash 'fact "mode" 'args '("/example") 'value '("a" "b")))))
         "accepted"))
      "E_FILE")
     (check
      "present-false-value-and-absence-remain-distinct"
      (lambda ()
        (define input
          (validate-facts
           (hash
            'collector
            "typify 1.0"
            'root
            "/"
            'facts
            (list (hash 'fact "exists" 'args '("/example") 'value #f)))))
        (list (query input "exists" '("/example")) (query input "exists" '("/absent"))))
      '((#t #f) (#f #f)))
     (for
      ((update
        (in-list
         (list
          (hash-set data 'collector "other")
          (hash-set data 'root "/not/../canonical")
          (hash-set data 'facts (list (hash 'fact "unknown" 'args '("/example"))))
          (hash-set data 'facts (list (hash 'fact "exists" 'args '("relative"))))
          (hash-set data 'facts (list (hash 'fact "exists" 'args '("/a" "/b"))))
          (hash-set data 'facts (list (hash 'fact "mode" 'args '("/example") 'value "700")))
          (hash-set data 'facts (list (hash 'fact "owner" 'args '("/example") 'value 42))))))
       (index (in-naturals)))
      (check
       (format "malformed-facts-wire-~a-is-file-error" index)
       (lambda ()
         (with-handlers
          ((exn:fail:typify?
            (lambda (failure)
              (define diagnostic (exn:fail:typify-diagnostic failure))
              (list
               (diagnostic-code diagnostic)
               (diagnostic-file diagnostic)
               (diagnostic-range diagnostic)
               (diagnostic-data diagnostic)))))
          (validate-facts update #:path "supplied-facts.json")
          "accepted"))
       (list "E_FILE" "supplied-facts.json" #f (hash 'path "supplied-facts.json"))))
     (unless (eq? (system-type 'os) 'windows)
       (check
        "unreadable-file-needs-no-content-read"
        (lambda () (query data "mode" (list (portable file))))
        '(#t "0000"))
       (check
        "dangling-link-is-an-entry-and-retains-target-text"
        (lambda ()
          (define name (portable (build-path root "dangling-link")))
          (list
           (query data "exists" (list name))
           (query data "symlink" (list name))
           (query data "directory" (list name))))
        '((#t #t) (#t "missing-target") (#f #f)))
       (check
        "directory-link-is-not-followed-or-classified-as-directory"
        (lambda ()
          (define name (portable (build-path root "directory-link")))
          (list
           (query data "directory" (list name))
           (query data "exists" (list (string-append name "/not-collected.data")))
           (query data "exists" (list (portable (build-path outside "not-collected.data"))))))
        '((#f #f) (#f #f) (#f #f)))
       (check
        "independent-posix-collector-has-zero-disagreements"
        (lambda () (compare-collectors root))
        #t)
       (check
        "literal-backslash-is-not-a-posix-directory-separator"
        (lambda () (query data "exists" (list (portable (build-path folder "back\\slash.data")))))
        '(#t #t))
       (check
        "symlink-target-preserves-trailing-newlines"
        (lambda () (query data "symlink" (list (portable (build-path root "newline-target-link")))))
        '(#t "missing-target\n\n"))
       (check
        "collectors-lexically-normalize-relative-root"
        (lambda ()
          (parameterize
           ((current-directory temporary))
           (compare-collectors "root/nested space/..")))
        #t)
       (define root-link (build-path temporary "root-link"))
       (make-file-or-directory-link root root-link)
       (check
        "symlink-root-is-described-without-following-it"
        (lambda ()
          (define linked (collect-facts root-link))
          (list
           (query linked "exists" (list (portable root-link)))
           (query linked "directory" (list (portable root-link)))
           (length (hash-ref linked 'facts))))
        '((#t #t) (#f #f) 4))
       (check
        "independent-posix-collector-agrees-on-symlink-root"
        (lambda () (compare-collectors root-link))
        #t)
       (file-or-directory-permissions file 384)))
   (lambda () (delete-directory/files temporary)))
  (hash
   'suite
   "facts"
   'count
   count
   'pass
   passed
   'fail
   (- count passed)
   'details
   (reverse failures)
   'gates
   (hash
    'facts-collector
    (hash
     'total
     independent-total
     'failures
     independent-disagreements
     'independent
     (positive? independent-total)
     'scope
     (if (eq? (system-type 'os) 'windows)
       "POSIX comparison not run on Windows"
       "Independent POSIX shell collector over controlled filesystem metadata")))))

(module+
 test
 (define result (run-tests))
 (unless (zero? (hash-ref result 'fail)) (error 'facts "facts checks failed: ~s" result)))
