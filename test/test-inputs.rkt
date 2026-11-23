#lang racket/base

(require json
         racket/file
         racket/list
         racket/string
         "../src/diagnostics.rkt"
         "../src/facts.rkt"
         "../src/physical-path.rkt"
         "../src/main.rkt")

(provide run-tests)

(define declarations
  (string-join
   '("#lang typify"
     "declare : Segment"
     "file : area/<section>/note.typify"
     "is : section = /[a-z]+/"
     "primitive : mark : canonical-path"
     "effect : mark = external"
     "form : mark : <canonical-path>"
     "does : mark <canonical-path>"
     "guard : mark = (exists <1>)"
     "reads : mark : (clean <1> \"lock\") = skip"
     "end!"
     "declare : Blank"
     "file : absent/blank.typify"
     "optional!"
     "end!"
     "declare : Defaults"
     "file : defaults.typify"
     "optional!"
     "primitive : saved : canonical-path"
     "effect : saved = external"
     "form : saved : <canonical-path>"
     "does : saved <canonical-path>"
     "default : saved : /known"
     "end!")
   "\n"))

(define (run-tests)
  (define checks 0)
  (define (check label actual expected)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-inputs "~a: expected ~s, received ~s" label expected actual)))
  (define (write-text path text) (display-to-file text path #:exists 'truncate/replace))
  (define (write-facts . extra)
    (write-text
     "facts.json"
     (canonical-json
      (hash
       'collector
       "typify 1.0"
       'root
       "/"
       'facts
       (cons (hash 'fact "exists" 'args '("/zone")) extra)))))
  (define (run which)
    (define-values
     (result json? gbnf?)
     (run-command (list which "decls" "walk" "--facts" "facts.json" "--json")))
    result)
  (define (paths read-record collection)
    (map (lambda (row) (hash-ref row 'path)) (hash-ref read-record collection)))
  (define (tree-response root)
    (define output (open-output-string))
    (define-values
     (code diagnostics)
     (parameterize
      ((current-output-port output))
      (execute-command (list "tree" "decls" root "--facts" "facts.json" "--json"))))
    (values code (string->jsexpr (get-output-string output))))
  (define (windows-name native)
    (physical-path-name (bytes->path (string->bytes/utf-8 native) 'windows)))
  (for
   ((fixture
     (in-list
      '(("c:\\" "/Volumes/C")
        ("Z:/a/b/" "/Volumes/Z/a/b")
        ("v:\\α\\β" "/Volumes/V/α/β")
        ("\\\\Server\\Share\\" "/Volumes/UNC/Server/Share")
        ("//Server/C/foo" "/Volumes/UNC/Server/C/foo")))))
   (check 'windows-physical-name (windows-name (car fixture)) (cadr fixture)))
  (for
   ((native
     (in-list
      '("\\\\?\\Z:\\a"
        "\\\\.\\PhysicalDrive0"
        "\\??\\C:\\a"
        "\\Device\\Harddisk0"
        "\\\\server\\pipe\\port"
        "\\\\server"
        "C:relative"))))
   (check
    'unsupported-windows-name
    (with-handlers
     ((exn:fail:typify? (lambda (e) (diagnostic-code (exn:fail:typify-diagnostic e)))))
     (windows-name native))
    "E_USAGE"))
  (define win-facts
    (validate-facts
     (hash
      'collector
      "typify 1.0"
      'root
      (windows-name "c:\\")
      'facts
      (list (hash 'fact "directory" 'args (list (windows-name "c:\\one\\lock")))))))
  (define (clean? physical)
    (define-values
     (held? value)
     (fact-query win-facts "clean" (list (windows-name physical) "lock")))
    held?)
  (check 'drive-root-separator (clean? "C:\\") #f)
  (check 'empty-subdirectory (clean? "C:\\one") #t)
  (check 'different-drive (clean? "D:\\one") #f)
  (check 'unc-is-not-a-drive (clean? "\\\\host\\C") #f)
  (define scratch (make-temporary-file "typify-inputs~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (parameterize
      ((current-directory scratch))
      (make-directory* "walk/area/a")
      (make-directory* "walk/area/b")
      (write-text "decls" declarations)
      (write-text "walk/area/a/note.typify" "mark : /zone\nmark : /zone\n")
      (write-text "walk/area/b/note.typify" "mark : /missing\n")
      (write-text "walk/area/ø" "ignored")
      (write-facts)
      (define plan (run "plan"))
      (define tree (run "tree"))
      (define reads (hash-ref plan 'inputs))
      (define tree-reads (hash-ref tree 'inputs))
      (define native (path->string (build-path scratch "walk")))
      (define where
        (if (eq? (system-type 'os) 'windows)
          (let ((components (string-split native #px"[/\\\\]+")))
            (if (regexp-match? #px"^[A-Za-z]:" native)
              (string-append
               "/Volumes/"
               (string-upcase (substring (car components) 0 1))
               "/"
               (string-join (cdr components) "/"))
              (string-append "/Volumes/UNC/" (string-join components "/"))))
          native))
      (check 'root (hash-ref reads 'root) where)
      (define collected (collect-facts "walk"))
      (check 'native-collector-coordinate (hash-ref collected 'root) where)
      (check
       'native-collector-directory
       (call-with-values
        (lambda () (fact-query collected "directory" (list (string-append where "/area"))))
        list)
       '(#t #t))
      (define-values
       (auto-plan auto-json? auto-gbnf?)
       (run-command '("plan" "decls" "walk" "--json")))
      (check 'native-auto-collector (hash-ref (hash-ref auto-plan 'inputs) 'factsRoot) where)
      (check 'facts-root (hash-ref reads 'factsRoot) "/")
      (check 'guard-fact-digest (hash-ref tree 'facts) (hash-ref plan 'facts))
      (check 'declared (paths reads 'declarations) '("decls"))
      (check 'present-bytes (paths reads 'programs) '("area/a/note.typify" "area/b/note.typify"))
      (check
       'program-digest
       (hash-ref (car (hash-ref reads 'programs)) 'sha256)
       "c0a9eb1c5297396c1d70d92d94788155651536d04e847ec3a5970626d5fbf841")
      (check
       'status
       (map (lambda (p) (hash-ref p 'status)) (hash-ref plan 'programs))
       '("defaulted" "absent-empty" "present" "guarded"))
      (check
       'absent-fixed
       (for/list
        ((row (in-list (hash-ref reads 'paths))) #:when (equal? "absent" (hash-ref row 'kind)))
        (hash-ref row 'path))
       '("absent" "absent/blank.typify" "defaults.typify"))
      (check 'excluded-unlisted-siblings (paths reads 'directories) '("area"))
      (check
       'listing
       (hash-ref (car (hash-ref reads 'directories)) 'entries)
       (list
        (hash 'kind "directory" 'name "a")
        (hash 'kind "directory" 'name "b")
        (hash 'kind "file" 'name "ø")))
      (check
       'listing-digest
       (hash-ref (car (hash-ref reads 'directories)) 'sha256)
       "b011bd96b669bd76cf45412fed1917a53e5a04d50c85cfc18ed32ce1bbf7bea0")
      (define point-reads
        (list
         (hash 'fact "exists" 'args '("/zone") 'held #t)
         (hash 'fact "exists" 'args '("/missing") 'held #f)))
      (define clean-read (hash 'fact "clean" 'args '("/zone" "lock") 'held #t))
      (check 'tree-guards-once (hash-ref tree-reads 'facts) point-reads)
      (check 'plan-reads-once (hash-ref reads 'facts) (append point-reads (list clean-read)))
      (write-facts (hash 'fact "exists" 'args '("/unused")))
      (define other (run "plan"))
      (check 'unread-fact-does-not-invalidate (hash-ref other 'inputs) reads)
      (check 'whole-file-digest-changes (equal? (hash-ref other 'facts) (hash-ref plan 'facts)) #f)
      (write-facts (hash 'fact "directory" 'args '("/zone/new/lock")))
      (check
       'new-marker
       (last (hash-ref (hash-ref (run "plan") 'inputs) 'facts))
       (hash-set clean-read 'held #f))
      (write-facts)
      (write-text "walk/area/ø" "unread other bytes")
      (check 'unopened-file (hash-ref (run "plan") 'inputs) reads)
      (delete-file "walk/area/ø")
      (make-directory "walk/area/ø")
      (check
       'membership-kind-observed
       (equal?
        (hash-ref (hash-ref (run "plan") 'inputs) 'directories)
        (hash-ref reads 'directories))
       #f)
      (write-text "walk/area/b/note.typify" "mark : /missing")
      (check
       'guarded-program-lf-change
       (equal? (hash-ref (hash-ref (run "tree") 'inputs) 'programs) (hash-ref reads 'programs))
       #f)
      (write-text "decls" (string-append declarations "\n"))
      (check
       'declaration-lf-change
       (equal?
        (hash-ref (hash-ref (run "tree") 'inputs) 'declarations)
        (hash-ref reads 'declarations))
       #f)
      (unless (eq? (system-type 'os) 'windows)
        (make-file-or-directory-link "walk" "linked-root")
        (define link-data (collect-facts "linked-root/."))
        (check
         'collector-keeps-link-root
         (hash-ref link-data 'facts)
         (hash-ref (collect-facts "linked-root") 'facts))
        (define-values (link-code link-report) (tree-response "linked-root/"))
        (check 'tree-refuses-link-root link-code 2)
        (check
         'link-root-kind
         (hash-ref (hash-ref link-report 'inputs) 'paths)
         (list (hasheq 'path "." 'kind "link"))))
      (define-values (failure-code failed) (tree-response "missing-root"))
      (check 'root-failure failure-code 2)
      (check
       'failed-root-kind
       (hash-ref (hash-ref failed 'inputs) 'paths)
       (list (hasheq 'path "." 'kind "absent")))
      (check 'failure-accepted-facts (hash-ref failed 'facts) (hash-ref plan 'facts))
      (check 'failure-no-programs (hash-ref (hash-ref failed 'inputs) 'programs) '())
      (write-text "decls" "#lang typify\ndeclare : Blank\nfile : blank.typify\noptional!\nend!\n")
      (delete-file "facts.json")
      (define no-queries (run "plan"))
      (check 'unused-facts-digest (hash-ref no-queries 'facts) "none")
      (check 'unused-facts-root (hash-ref (hash-ref no-queries 'inputs) 'factsRoot) 'null)
      (check 'unused-facts (hash-ref (hash-ref no-queries 'inputs) 'facts) '())))
   (lambda () (delete-directory/files scratch)))
  (hash 'suite "root-inputs" 'checks checks 'status "passed"))

(module+ test (run-tests))

(module+ main (displayln (canonical-json (run-tests))))
