#lang racket/base

(require file/sha1
         json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "../scripts/license-check.rkt")

(provide run-tests)

(define-runtime-path repository-root "..")

(define (sha256-text text) (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

(define (write-text! root name text)
  (define path (build-path root name))
  (make-directory* (path-only path))
  (call-with-output-file
   path
   (lambda (output) (write-string text output))
   #:exists
   'truncate/replace))

(define license-text
  (string-append
   "MIT License\n\nUNICODE LICENSE V3\n\nApache License\n\nChez Scheme Version 10.5.0\n"
   "Jean-loup Gailly and Mark Adler\nLZ4 Library\n"
   "Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell\n"
   "Mozilla Public License, version 2.0\n"))

(define unicode-text
  (string-append
   "0000..001F    ; Cc # [32] <control-0000>..<control-001F>\n"
   "0020          ; Zs #       SPACE\n"
   "0041..005A    ; Lu #  [26] LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER Z\n"
   "0061..007A    ; Ll #  [26] LATIN SMALL LETTER A..LATIN SMALL LETTER Z\n"
   "00AD          ; Cf #       SOFT HYPHEN\n"
   "D800..DFFF    ; Cs # [2048] <surrogates>\n"))

(define classes-text
  (string-append
   "#lang racket/base\n"
   "(define unicode-source-sha256 \"PIN\")\n"
   "(define unicode-source \"https://example.test/DGC.txt\")\n"
   "(define unicode-letter-intervals '((65 . 90) (97 . 122)))\n"
   "(define unicode-unsafe-intervals '((0 . 32) (173 . 173)))\n"))

(define python-text
  (string-append
   "UNICODE_SOURCE = \"https://example.test/DGC.txt\"\n"
   "UNICODE_SOURCE_SHA256 = \"PIN\"\n"
   "UNSAFE = (\n    (0, 32),\n    (173, 173),\n    (55296, 57343),\n)\n"))

(define lock-text
  (string-append
   "{\n  \"packages\": {\n"
   "    \"left\": [\"left@1.0.0\", \"\", { }, \"sha512-LEFT\"],\n"
   "    \"right\": [\"right@2.0.0\", \"\", { \"dependencies\": { \"left\": \"^1\" } }, \"sha512-RIGHT\"],\n"
   "  }\n}\n"))

(define (upstream-entry sha)
  (hasheq 'url "https://example.test/DGC.txt" 'version "17.0.0" 'sha256 sha))

(define (inventory pin)
  (hasheq
   'licenses
   (hasheq
    'MIT
    (hasheq 'notice "MIT License")
    'Unicode-3.0
    (hasheq 'notice "UNICODE LICENSE V3")
    'Apache-2.0
    (hasheq 'notice "Apache License")
    'Chez-Scheme-NOTICE
    (hasheq 'notice "Chez Scheme Version 10.5.0")
    'Zlib
    (hasheq 'notice "Jean-loup Gailly and Mark Adler")
    'MPL-2.0
    (hasheq 'notice "Mozilla Public License, version 2.0"))
   'groups
   (list
    (hasheq
     'id
     "own"
     'match
     '("src/own.rkt" "scripts/**" "LICENSE.md" "mcp/**")
     'origin
     "original"
     'license
     "MIT")
    (hasheq
     'id
     "tables"
     'match
     '("src/classes.rkt" "test/facts-path.py")
     'origin
     "original"
     'license
     "MIT")
    (hasheq
     'id
     "handoff"
     'match
     '("test/handoff/**")
     'origin
     "contributed"
     'license
     "MIT"
     'authorship
     "an independent author"
     'confirmed
     #f))
   'data
   (list
    (hasheq
     'path
     "test/gate.json"
     'origin
     "generated"
     'license
     "MIT"
     'sha256
     (sha256-text "{\"gate\":1}\n")))
   'embedded
   (list
    (hasheq
     'id
     "unicode-general-category"
     'files
     '("src/classes.rkt" "test/facts-path.py")
     'license
     "Unicode-3.0"
     'upstream
     (upstream-entry pin)))
   'runtime-notices
   (list
    (hasheq
     'component
     "runtime"
     'license
     "Apache-2.0"
     'upstream
     (hasheq 'url "https://example.test/LICENSE" 'version "v9.3" 'sha256 (make-string 64 #\a))))
   'lockfile
   "mcp/bun.lock"
   'allowed-dependency-licenses
   '("MIT" "ISC")
   'dependencies
   (list
    (hasheq 'name "left" 'version "1.0.0" 'license "MIT" 'integrity "sha512-LEFT")
    (hasheq 'name "right" 'version "2.0.0" 'license "ISC" 'integrity "sha512-RIGHT"))))

(define (build-root parent name pin)
  (define root (build-path parent name))
  (write-text! root "LICENSE.md" license-text)
  (write-text! root "src/own.rkt" "#lang racket/base\n")
  (write-text! root "src/classes.rkt" (string-replace classes-text "PIN" pin))
  (write-text! root "test/facts-path.py" (string-replace python-text "PIN" pin))
  (write-text! root "test/gate.json" "{\"gate\":1}\n")
  (write-text! root "test/handoff/case.txt" "case\n")
  (write-text! root "mcp/bun.lock" lock-text)
  root)

(define (kinds result)
  (sort (map (lambda (item) (hash-ref item 'kind)) (hash-ref result 'issues)) string<?))

(define (run-tests)
  (define checks 0)
  (define (check actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-license-check "~a: expected ~s; received ~s" context expected actual)))
  (define parent (make-temporary-file "license-check~a" 'directory))
  (define pin (sha256-text unicode-text))
  (define unicode-file (build-path parent "DGC.txt"))
  (write-text! parent "DGC.txt" unicode-text)
  (define counter 0)
  (define (fresh) (set! counter (add1 counter)) (build-root parent (format "root~a" counter) pin))
  (define (run
           root
           (inv (inventory pin))
           #:release?
           (release? #f)
           #:unicode-source
           (source #f)
           #:archive
           (archive #f))
    (check-licenses root inv #:release? release? #:unicode-source source #:archive archive))
  (define (changed! root name text) (write-text! root name text))
  (check
   (regexp-match? (glob->regexp "src/**") "src/lang/reader.rkt")
   #t
   "a double star crosses directories")
  (check
   (regexp-match? (glob->regexp "test/*.rkt") "test/a/b.rkt")
   #f
   "a single star stays in one directory")
  (check (regexp-match? (glob->regexp "a.b") "axb") #f "a dot is literal")
  (check (regexp-match? (glob->regexp "x?") "xy") #t "a question mark is one character")
  (define clean (fresh))
  (check (kinds (run clean)) '() "a consistent root passes")
  (check
   (kinds (run clean #:unicode-source unicode-file))
   '()
   "the derived tables match their upstream file")
  (check (hash-ref (run clean) 'files) 7 "every file is counted")
  (write-text! clean ".git" "gitdir: ../separate-index\n")
  (check (kinds (run clean)) '() "a linked worktree has the same distribution inventory")
  (define unlisted (fresh))
  (changed! unlisted "test/new-data.json" "{}\n")
  (check (kinds (run unlisted)) '("unlisted") "a new file that no entry covers fails")
  (define nested (fresh))
  (changed! nested "src/lang/extra.rkt" "#lang racket/base\n")
  (check (kinds (run nested)) '("unlisted") "a file outside every group fails")
  (define altered (fresh))
  (changed! altered "test/gate.json" "{\"gate\":2}\n")
  (check
   (kinds (run altered))
   '("data-changed")
   "a data file that changed fails until the inventory is reviewed")
  (define absent (fresh))
  (delete-file (build-path absent "test/gate.json"))
  (check (kinds (run absent)) '("data-missing") "a listed data file that is gone fails")
  (define ambiguous (fresh))
  (check
   (kinds
    (run
     ambiguous
     (hash-set
      (inventory pin)
      'groups
      (append
       (hash-ref (inventory pin) 'groups)
       (list (hasheq 'id "again" 'match '("src/own.rkt") 'origin "original" 'license "MIT"))))))
   '("ambiguous")
   "a file in two groups fails")
  (define without-notice (fresh))
  (changed!
   without-notice
   "LICENSE.md"
   (string-replace license-text "UNICODE LICENSE V3" "removed"))
  (check
   (sort (remove-duplicates (kinds (run without-notice))) string<?)
   '("notice-missing")
   "a license whose notice text is gone from LICENSE.md fails")
  (define no-license-file (fresh))
  (delete-file (build-path no-license-file "LICENSE.md"))
  (check
   (and (member "license-file-missing" (kinds (run no-license-file))) #t)
   #t
   "a missing LICENSE.md fails")
  (check
   (and (member
         "license-unknown"
         (kinds
          (run
           (fresh)
           (hash-set
            (inventory pin)
            'groups
            (list (hasheq 'id "x" 'match '("**") 'origin "original" 'license "GPL-3.0"))))))
        #t)
   #t
   "a license that the inventory does not define fails")
  (define (with-upstream upstream)
    (hash-set
     (inventory pin)
     'embedded
     (list
      (hasheq
       'id
       "unicode-general-category"
       'files
       '("src/classes.rkt" "test/facts-path.py")
       'license
       "Unicode-3.0"
       'upstream
       upstream))))
  (check
   (and (member
         "provenance-incomplete"
         (kinds
          (run
           (fresh)
           (with-upstream (hasheq 'url "http://example.test/x" 'version "1" 'sha256 pin)))))
        #t)
   #t
   "an upstream that is not https fails")
  (check
   (and (member
         "provenance-incomplete"
         (kinds
          (run
           (fresh)
           (with-upstream (hasheq 'url "https://example.test/x" 'version "1" 'sha256 "abc")))))
        #t)
   #t
   "an upstream without a sha256 fails")
  (check
   (and (member
         "provenance-incomplete"
         (kinds
          (run
           (fresh)
           (with-upstream (hasheq 'url "https://example.test/x" 'version "" 'sha256 pin)))))
        #t)
   #t
   "an upstream without a version fails")
  (define repinned (fresh))
  (changed! repinned "src/classes.rkt" (string-replace classes-text "PIN" (make-string 64 #\b)))
  (check
   (kinds (run repinned))
   '("pin-missing")
   "a table whose pin differs from the inventory fails")
  (define resourced (fresh))
  (changed!
   resourced
   "test/facts-path.py"
   (string-replace (string-replace python-text "PIN" pin) "example.test" "elsewhere.test"))
  (check (kinds (run resourced)) '("source-missing") "a table that names another source fails")
  (check
   (kinds
    (run
     (fresh)
     (inventory pin)
     #:unicode-source
     (let ((other (build-path parent "other.txt"))) (write-text! parent "other.txt" "x\n") other)))
   '("upstream-hash")
   "an upstream file that is not the pinned one fails")
  (define wrong-letters (fresh))
  (changed!
   wrong-letters
   "src/classes.rkt"
   (string-replace (string-replace classes-text "PIN" pin) "(97 . 122)" "(97 . 121)"))
  (check
   (kinds (run wrong-letters #:unicode-source unicode-file))
   '("table-differs")
   "a letter table that differs from upstream fails")
  (define wrong-unsafe (fresh))
  (changed!
   wrong-unsafe
   "src/classes.rkt"
   (string-replace (string-replace classes-text "PIN" pin) "(173 . 173)" "(174 . 174)"))
  (check
   (kinds (run wrong-unsafe #:unicode-source unicode-file))
   '("table-differs")
   "an unsafe table that differs from upstream fails")
  (define wrong-python (fresh))
  (changed!
   wrong-python
   "test/facts-path.py"
   (string-replace (string-replace python-text "PIN" pin) "(55296, 57343)" "(55296, 57342)"))
  (check
   (kinds (run wrong-python #:unicode-source unicode-file))
   '("table-differs")
   "the Python table is compared with upstream, surrogates included")
  (check
   (kinds (run (fresh) (inventory pin) #:release? #t))
   '("unconfirmed")
   "a contributed group without the owner's confirmation fails a release")
  (define (with-confirmation extra)
    (hash-set
     (inventory pin)
     'groups
     (append
      (list (car (hash-ref (inventory pin) 'groups)) (cadr (hash-ref (inventory pin) 'groups)))
      (list
       (for/fold
        ((group (caddr (hash-ref (inventory pin) 'groups))))
        (((key value) (in-hash extra)))
        (hash-set group key value))))))
  (check
   (kinds
    (run
     (fresh)
     (with-confirmation
      (hasheq 'confirmed #t 'confirmed-by "the maintainer" 'confirmed-on "2026-12-01"))
     #:release?
     #t))
   '()
   "a confirmation with its author and date lets a release pass")
  (check
   (kinds (run (fresh) (with-confirmation (hasheq 'confirmed #t)) #:release? #t))
   '("confirmation-incomplete")
   "a confirmation without its author and date fails")
  (check
   (kinds (run (fresh) (with-confirmation (hasheq 'confirmed "yes")) #:release? #t))
   '("confirmation-malformed")
   "a confirmation that is not a boolean fails")
  (check
   (kinds (run (fresh) (with-confirmation (hasheq 'confirmed #f))))
   '()
   "an unconfirmed group does not fail outside a release")
  (define new-dependency (fresh))
  (changed!
   new-dependency
   "mcp/bun.lock"
   (string-replace
    lock-text
    "  }\n}\n"
    "    \"extra\": [\"extra@3.0.0\", \"\", { }, \"sha512-EXTRA\"],\n  }\n}\n"))
  (check
   (kinds (run new-dependency))
   '("dependency-unlisted")
   "a locked package that the inventory does not list fails")
  (define moved-integrity (fresh))
  (changed! moved-integrity "mcp/bun.lock" (string-replace lock-text "sha512-LEFT" "sha512-OTHER"))
  (check
   (kinds (run moved-integrity))
   '("dependency-integrity")
   "a locked package whose integrity moved fails")
  (define dropped (fresh))
  (changed!
   dropped
   "mcp/bun.lock"
   (string-replace
    lock-text
    "    \"right\": [\"right@2.0.0\", \"\", { \"dependencies\": { \"left\": \"^1\" } }, \"sha512-RIGHT\"],\n"
    ""))
  (check (kinds (run dropped)) '("dependency-stale") "a listed package that the lock dropped fails")
  (check
   (kinds (run (fresh) (hash-set (inventory pin) 'allowed-dependency-licenses '("MIT"))))
   '("dependency-license")
   "a dependency whose license is not allowed fails")
  (define (has? kind result) (and (member kind (kinds result)) #t))
  (define base-embedded (car (hash-ref (inventory pin) 'embedded)))
  (check
   (has?
    "provenance-missing"
    (run (fresh) (hash-set (inventory pin) 'embedded (list (hash-remove base-embedded 'upstream)))))
   #t
   "an embedded table with no upstream record fails")
  (check
   (has?
    "entry-malformed"
    (run (fresh) (hash-set (inventory pin) 'data (list (hasheq 'origin "generated")))))
   #t
   "a data entry without a path fails")
  (define gate-entry (car (hash-ref (inventory pin) 'data)))
  (check
   (has?
    "duplicate-entry"
    (run (fresh) (hash-set (inventory pin) 'data (list gate-entry gate-entry))))
   #t
   "two data entries for one path fail")
  (check
   (has?
    "group-origin"
    (run
     (fresh)
     (hash-set
      (inventory pin)
      'groups
      (list (hasheq 'id "x" 'match '("**") 'origin "mystery" 'license "MIT")))))
   #t
   "a group of an unknown origin fails")
  (check
   (has?
    "entry-origin"
    (run (fresh) (hash-set (inventory pin) 'data (list (hash-set gate-entry 'origin "mystery")))))
   #t
   "a data entry of an unknown origin fails")
  (check
   (has?
    "ambiguous"
    (run
     (fresh)
     (hash-set
      (inventory pin)
      'groups
      (cons
       (hasheq 'id "covers-data" 'match '("test/gate.json") 'origin "original" 'license "MIT")
       (hash-ref (inventory pin) 'groups)))))
   #t
   "a file listed as data and also matched by a group fails")
  (define without-table (fresh))
  (delete-file (build-path without-table "src/classes.rkt"))
  (check
   (kinds (run without-table))
   '("embedded-missing")
   "an embedded table whose file is gone fails")
  (define without-lock (fresh))
  (delete-file (build-path without-lock "mcp/bun.lock"))
  (check
   (kinds (run without-lock))
   '("lockfile-missing")
   "a missing lockfile fails when dependencies are listed")
  (define archive (build-path parent "archive"))
  (write-text! archive "typify/LICENSE.md" license-text)
  (write-text!
   archive
   "typify/MANIFEST.txt"
   (string-append (sha256-text license-text) "  0644  typify/LICENSE.md\n"))
  (check
   (kinds (run (fresh) (inventory pin) #:archive archive))
   '()
   "an archive that carries the license passes")
  (define bare (build-path parent "bare"))
  (make-directory* (build-path bare "typify"))
  (check
   (kinds (run (fresh) (inventory pin) #:archive bare))
   '("archive-license-missing" "archive-manifest-missing")
   "an archive with no license and no manifest fails")
  (define different (build-path parent "different"))
  (write-text! different "typify/LICENSE.md" "MIT License\n")
  (write-text!
   different
   "typify/MANIFEST.txt"
   (string-append (sha256-text "MIT License\n") "  0644  typify/LICENSE.md\n"))
  (check
   (kinds (run (fresh) (inventory pin) #:archive different))
   '("archive-license-differs")
   "an archive with another license text fails")
  (define unlisted-archive (build-path parent "unlisted-archive"))
  (write-text! unlisted-archive "typify/LICENSE.md" license-text)
  (write-text!
   unlisted-archive
   "typify/MANIFEST.txt"
   (string-append (make-string 64 #\0) "  0644  typify/bin/typify\n"))
  (check
   (kinds (run (fresh) (inventory pin) #:archive unlisted-archive))
   '("archive-license-unlisted")
   "an archive whose manifest omits the license fails")
  (define stale-hash (build-path parent "stale-hash"))
  (write-text! stale-hash "typify/LICENSE.md" license-text)
  (write-text!
   stale-hash
   "typify/MANIFEST.txt"
   (string-append (make-string 64 #\0) "  0644  typify/LICENSE.md\n"))
  (check
   (kinds (run (fresh) (inventory pin) #:archive stale-hash))
   '("archive-license-manifest-hash")
   "an archive whose manifest holds another hash fails")
  (define real-inventory
    (call-with-input-file
     (build-path repository-root "scripts" "license-inventory.json")
     read-json))
  (define real (check-licenses repository-root real-inventory))
  (check (hash-ref real 'issues) '() "the repository agrees with its own inventory")
  (check (positive? (hash-ref real 'files)) #t "the repository check looks at files")
  (for
   ((entry (in-list (hash-ref real-inventory 'embedded))))
   (check
    (hash-ref (hash-ref entry 'upstream) 'sha256)
    "d62e5bab70ca74f099343f71224fa051cb1fdd61a1ab45c0488c44cfc0b6102e"
    "the inventory pins Unicode 17.0.0's DerivedGeneralCategory.txt"))
  (delete-directory/files parent)
  (hasheq 'suite "license-check" 'checks checks 'status "passed"))

(module+ main (write-json (run-tests)) (newline))

(module+ test (run-tests))
