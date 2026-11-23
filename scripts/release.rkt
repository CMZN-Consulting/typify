#lang racket/base

(require compiler/cm
         compiler/distribute
         compiler/embed
         file/gunzip
         file/gzip
         file/sha1
         file/tar
         file/untar
         file/unzip
         file/zip
         json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         syntax/modcode
         syntax/modresolve)

(provide platforms
         platform-entry
         host-platform
         manifest-path
         staged-entries
         manifest-text
         with-manifest
         write-archive
         read-archive
         tree-problems
         verify-archive
         executable-format
         architecture-report
         canary-variants
         canary-matrix
         compare-builds)

(define platforms
  (list
   (hasheq
    'name
    "linux-x86_64"
    'os
    'linux
    'arch
    'x86_64
    'asset
    "typify-linux-x86_64.tar.gz"
    'program
    "typify"
    'kind
    'tgz)
   (hasheq
    'name
    "linux-aarch64"
    'os
    'linux
    'arch
    'aarch64
    'asset
    "typify-linux-aarch64.tar.gz"
    'program
    "typify"
    'kind
    'tgz)
   (hasheq
    'name
    "macos-aarch64"
    'os
    'macosx
    'arch
    'aarch64
    'asset
    "typify-macos-aarch64.tar.gz"
    'program
    "typify"
    'kind
    'tgz)
   (hasheq
    'name
    "windows-x86_64"
    'os
    'windows
    'arch
    'x86_64
    'asset
    "typify-windows-x86_64.zip"
    'program
    "typify.exe"
    'kind
    'zip)))

(define manifest-path "typify/MANIFEST.txt")

(define grammar-path "typify/share/typify.typify")

(define racket-version "9.3")

(define (platform-entry name)
  (or (for/first ((entry (in-list platforms)) #:when (equal? (hash-ref entry 'name) name)) entry)
      (raise-user-error 'release "unknown platform: ~a" name)))

(define (host-platform)
  (for/first
   ((entry (in-list platforms))
    #:when
    (and (eq? (hash-ref entry 'os) (system-type 'os*))
         (eq? (hash-ref entry 'arch) (system-type 'arch))))
   (hash-ref entry 'name)))

(define (sha256-hex value) (bytes->hex-string (sha256-bytes value)))

(define (file-sha256 path) (call-with-input-file path (lambda (input) (sha256-hex input))))

(define (mode-text mode) (string-append "0" (number->string mode 8)))

(define (slash-path path) (string-join (map path->string (explode-path path)) "/"))

(define (entry-key entry) (string->bytes/utf-8 (hash-ref entry 'path)))

(define (sort-entries entries) (sort entries bytes<? #:key entry-key))

(define (file-entry path mode content) (hasheq 'path path 'kind 'file 'mode mode 'content content))

(define (directory-entry path) (hasheq 'path path 'kind 'directory 'mode 493))

(define (staged-entries parent windows?)
  (define top (build-path parent "typify"))
  (sort-entries
   (cons
    (directory-entry "typify")
    (for/list
     ((path (in-directory top)))
     (define name (slash-path (find-relative-path parent path)))
     (cond
      ((link-exists? path) (raise-user-error 'release "the staged tree holds a link: ~a" name))
      ((directory-exists? path) (directory-entry name))
      (else
       (file-entry
        name
        (if (or windows? (zero? (bitwise-and (file-or-directory-permissions path 'bits) 73)))
          420
          493)
        (file->bytes path))))))))

(define (listed-files entries)
  (filter
   (lambda (entry)
     (and (eq? (hash-ref entry 'kind) 'file) (not (equal? (hash-ref entry 'path) manifest-path))))
   entries))

(define (manifest-text entries)
  (string-append*
   (for/list
    ((entry (in-list (sort-entries (listed-files entries)))))
    (format
     "~a  ~a  ~a\n"
     (sha256-hex (hash-ref entry 'content))
     (mode-text (hash-ref entry 'mode))
     (hash-ref entry 'path)))))

(define (with-manifest entries)
  (define others
    (filter (lambda (entry) (not (equal? (hash-ref entry 'path) manifest-path))) entries))
  (sort-entries
   (cons (file-entry manifest-path 420 (string->bytes/utf-8 (manifest-text others))) others)))

(define (tar-entry-of entry epoch)
  (define directory? (eq? (hash-ref entry 'kind) 'directory))
  (define content (hash-ref entry 'content #f))
  (tar-entry
   (if directory? 'directory 'file)
   (hash-ref entry 'path)
   (and content (lambda () (open-input-bytes content)))
   (if content (bytes-length content) 0)
   (hasheq
    'permissions
    (hash-ref entry 'mode)
    'owner
    0
    'group
    0
    'owner-bytes
    #"root"
    'group-bytes
    #"root"
    'modify-seconds
    epoch)))

(define (zip-entry-of entry epoch)
  (define directory? (eq? (hash-ref entry 'kind) 'directory))
  (define content (hash-ref entry 'content #f))
  (make-zip-entry
   (if directory? 'directory 'file)
   (hash-ref entry 'path)
   content
   (if content (bytes-length content) 0)
   (hasheq 'permissions (if directory? 493 420) 'modify-seconds epoch)
   6))

(define (write-archive entries kind epoch destination)
  (call-with-output-file
   destination
   #:exists
   'truncate/replace
   (lambda (output)
     (case kind
       ((tgz)
        (define archive
          (call-with-output-bytes
           (lambda (tar-output)
             (tar->output
              (map (lambda (entry) (tar-entry-of entry epoch)) entries)
              tar-output
              #:format
              'ustar))))
        (gzip-through-ports (open-input-bytes archive) output #f epoch))
       ((zip)
        (zip->output
         (map (lambda (entry) (zip-entry-of entry epoch)) entries)
         output
         #:utc-timestamps?
         #t
         #:system-type
         'windows))
       (else (raise-argument-error 'write-archive "tgz or zip" kind))))))

(define (read-archive path kind)
  (define entries '())
  (define (add! name kind mode content)
    (set! entries
      (cons
       (if (eq? kind 'file)
         (file-entry name mode content)
         (hasheq 'path name 'kind kind 'mode mode))
       entries)))
  (case kind
    ((tgz)
     (define archive
       (call-with-output-bytes
        (lambda (output)
          (call-with-input-file path (lambda (input) (gunzip-through-ports input output))))))
     (untar
      (open-input-bytes archive)
      #:handle-entry
      (lambda (kind name content size attributes)
        (add!
         (slash-path name)
         kind
         (bitwise-and (hash-ref attributes 'permissions 0) 4095)
         (and (eq? kind 'file) (input-port? content) (read-bytes-exactly content size)))
        '())))
    ((zip)
     (call-with-input-file
      path
      (lambda (input)
        (unzip
         input
         (lambda (name directory? content)
           (define text (regexp-replace #rx"/$" (bytes->string/utf-8 name #\�) ""))
           (if directory?
             (add! text 'directory 493 #f)
             (add! text 'file 420 (port->bytes content))))))))
    (else (raise-argument-error 'read-archive "tgz or zip" kind)))
  (reverse entries))

(define (read-bytes-exactly input size)
  (define content (if (zero? size) #"" (read-bytes size input)))
  (unless (and (bytes? content) (= (bytes-length content) size))
    (raise-user-error 'release "the archive ends inside an entry"))
  content)

(define (input-archive-kind path)
  (call-with-input-file
   path
   (lambda (input)
     (case (read-bytes 2 input)
       ((#"\37\213") 'tgz)
       ((#"PK") 'zip)
       (else (raise-user-error 'release "unrecognized archive header"))))))

(define (tree-problems entries manifest-sha256)
  (define problems '())
  (define (problem! text) (set! problems (cons text problems)))
  (define seen (make-hash))
  (for
   ((entry (in-list entries)))
   (define name (hash-ref entry 'path))
   (when (hash-ref seen name #f) (problem! (format "duplicate: ~a" name)))
   (hash-set! seen name entry)
   (unless (or (equal? name "typify") (string-prefix? name "typify/"))
     (problem! (format "outside: ~a" name)))
   (unless (memq (hash-ref entry 'kind) '(file directory)) (problem! (format "kind: ~a" name))))
  (define manifest (hash-ref seen manifest-path #f))
  (cond
   ((not (and manifest (eq? (hash-ref manifest 'kind) 'file))) (problem! "no manifest"))
   (else
    (define content (hash-ref manifest 'content))
    (unless (equal? (sha256-hex content) manifest-sha256) (problem! "manifest hash"))
    (define text (bytes->string/utf-8 content #\�))
    (define lines (if (string-suffix? text "\n") (string-split text "\n" #:trim? #f) (list text)))
    (define rows (if (and (pair? lines) (equal? (last lines) "")) (drop-right lines 1) lines))
    (when (null? rows) (problem! "empty manifest"))
    (define listed (make-hash))
    (for
     ((row (in-list rows)) (index (in-naturals 1)))
     (define match (regexp-match #px"^([0-9a-f]{64})  (0755|0644)  (typify/[^\n]+)$" row))
     (cond
      ((not match) (problem! (format "malformed line ~a" index)))
      (else
       (define name (cadddr match))
       (when (hash-ref listed name #f) (problem! (format "listed twice: ~a" name)))
       (hash-set! listed name #t)
       (define entry (hash-ref seen name #f))
       (cond
        ((not (and entry (eq? (hash-ref entry 'kind) 'file)))
         (problem! (format "missing: ~a" name)))
        (else
         (unless (equal? (sha256-hex (hash-ref entry 'content)) (cadr match))
           (problem! (format "content: ~a" name)))
         (unless (equal? (mode-text (bitwise-and (hash-ref entry 'mode) 511)) (caddr match))
           (problem! (format "mode: ~a" name))))))))
    (define names
      (for/list
       ((row (in-list rows)))
       (define match (regexp-match #px"^[0-9a-f]{64}  [0-9]{4}  (.*)$" row))
       (if match (string->bytes/utf-8 (cadr match)) #"")))
    (unless (equal? names (sort names bytes<?)) (problem! "unsorted manifest"))
    (for
     ((entry (in-list (listed-files entries))))
     (unless (hash-ref listed (hash-ref entry 'path) #f)
       (problem! (format "unlisted: ~a" (hash-ref entry 'path)))))))
  (reverse problems))

(define (little-endian content start size)
  (for/sum
   ((index (in-range size)))
   (arithmetic-shift (bytes-ref content (+ start index)) (* 8 index))))

(define (executable-format content)
  (define size (bytes-length content))
  (define (at-least? count) (>= size count))
  (cond
   ((and (at-least? 20) (equal? (subbytes content 0 4) #"\177ELF"))
    (define bits (if (= (bytes-ref content 4) 2) 64 32))
    (define machine (and (= (bytes-ref content 5) 1) (little-endian content 18 2)))
    (cons
     'elf
     (cond
      ((and (= bits 64) (eqv? machine 62)) 'x86_64)
      ((and (= bits 64) (eqv? machine 183)) 'aarch64)
      (else (string->symbol (format "elf-~a-~a" bits machine))))))
   ((and (at-least? 8) (equal? (subbytes content 0 4) #"\317\372\355\376"))
    (define cpu (little-endian content 4 4))
    (cons
     'mach-o
     (cond
      ((= cpu 16777223) 'x86_64)
      ((= cpu 16777228) 'aarch64)
      (else (string->symbol (format "mach-o-~a" cpu))))))
   ((and (at-least? 8)
         (member (subbytes content 0 4) (list #"\312\376\272\276" #"\276\272\376\312")))
    (cons 'mach-o 'universal))
   ((and (at-least? 64) (equal? (subbytes content 0 2) #"MZ"))
    (define offset (little-endian content 60 4))
    (if (and (at-least? (+ offset 6)) (equal? (subbytes content offset (+ offset 4)) #"PE\0\0"))
      (let ((machine (little-endian content (+ offset 4) 2)))
        (cons
         'pe
         (cond
          ((= machine 34404) 'x86_64)
          ((= machine 43620) 'aarch64)
          (else (string->symbol (format "pe-~a" machine))))))
      (cons 'mz 'unknown)))
   ((and (at-least? 2) (equal? (subbytes content 0 2) #"#!")) (cons 'script 'none))
   (else #f)))

(define (platform-format platform)
  (case (hash-ref platform 'os) ((linux) 'elf) ((macosx) 'mach-o) ((windows) 'pe)))

(define (architecture-report entries platform)
  (define program (program-path platform))
  (define rows
    (for/list
     ((entry (in-list entries))
      #:when
      (and (eq? (hash-ref entry 'kind) 'file)
           (or (equal? (hash-ref entry 'path) "typify/bin/typify")
               (regexp-match? #rx"(?i:[.](exe|dll))$" (hash-ref entry 'path))
               (positive? (bitwise-and (hash-ref entry 'mode) 73)))))
     (define found (executable-format (hash-ref entry 'content)))
     (hasheq
      'path
      (hash-ref entry 'path)
      'sha256
      (sha256-hex (hash-ref entry 'content))
      'format
      (if found (symbol->string (car found)) "none")
      'architecture
      (if found (symbol->string (cdr found)) "none")
      'native
      (and found
           (eq? (car found) (platform-format platform))
           (eq? (cdr found) (hash-ref platform 'arch))
           #t))))
  (define program-row
    (for/first ((row (in-list rows)) #:when (equal? (hash-ref row 'path) program)) row))
  (hasheq
   'platform
   (hash-ref platform 'name)
   'expected
   (format "~a ~a" (platform-format platform) (hash-ref platform 'arch))
   'executables
   rows
   'pass
   (and program-row (andmap (lambda (row) (hash-ref row 'native)) rows) #t)))

(define (verify-archive path platform #:sha256 archive-sha256 #:manifest-sha256 manifest-sha256)
  (define actual (file-sha256 path))
  (define entries
    (with-handlers
     ((exn:fail? (lambda (error) error)))
     (read-archive path (hash-ref platform 'kind))))
  (define problems
    (if (exn? entries)
      (list (format "unreadable: ~a" (exn-message entries)))
      (tree-problems entries manifest-sha256)))
  (define manifest
    (and (list? entries)
         (for/first
          ((entry (in-list entries))
           #:when
           (and (eq? (hash-ref entry 'kind) 'file) (equal? (hash-ref entry 'path) manifest-path)))
          (sha256-hex (hash-ref entry 'content)))))
  (define architecture (and (not (exn? entries)) (architecture-report entries platform)))
  (hasheq
   'archive
   (if (equal? actual archive-sha256) "ok" "mismatch")
   'archive_sha256
   actual
   'manifest_sha256
   (or manifest 'null)
   'tree
   (if (null? problems) "ok" "mismatch")
   'tree_problems
   problems
   'architecture
   (if (and architecture (hash-ref architecture 'pass)) "ok" "mismatch")
   'executables
   (if architecture (hash-ref architecture 'executables) '())))

(define (program-path platform) (string-append "typify/bin/" (hash-ref platform 'program)))

(define (flip-last content)
  (define copy (bytes-copy content))
  (define index (sub1 (bytes-length copy)))
  (bytes-set! copy index (bitwise-xor (bytes-ref copy index) 1))
  copy)

(define (replace-content entries name change)
  (for/list
   ((entry (in-list entries)))
   (if (equal? (hash-ref entry 'path) name)
     (hash-set entry 'content (change (hash-ref entry 'content)))
     entry)))

(define (canary-variants entries platform)
  (define program (program-path platform))
  (define flipped (replace-content entries program flip-last))
  (list
   (cons "flipped-byte" flipped)
   (cons
    "extra-file"
    (sort-entries (cons (file-entry "typify/share/extra.typify" 420 #"extra\n") entries)))
   (cons
    "missing-file"
    (filter (lambda (entry) (not (equal? (hash-ref entry 'path) grammar-path))) entries))
   (cons "rewritten-manifest" (with-manifest flipped))))

(define (canary-matrix
         archive
         platform
         epoch
         destination
         #:sha256
         archive-sha256
         #:manifest-sha256
         manifest-sha256)
  (make-directory* destination)
  (define kind (hash-ref platform 'kind))
  (define genuine
    (verify-archive archive platform #:sha256 archive-sha256 #:manifest-sha256 manifest-sha256))
  (define entries (read-archive archive kind))
  (define rows
    (cons
     (hasheq
      'name
      "genuine"
      'file
      (path->string (file-name-from-path archive))
      'expected
      "both checks accept"
      'archive
      (hash-ref genuine 'archive)
      'tree
      (hash-ref genuine 'tree)
      'tree_problems
      (hash-ref genuine 'tree_problems)
      'as_designed
      (and (equal? (hash-ref genuine 'archive) "ok") (equal? (hash-ref genuine 'tree) "ok")))
     (for/list
      ((variant (in-list (canary-variants entries platform))))
      (define name (string-append (car variant) "-" (hash-ref platform 'asset)))
      (define path (build-path destination name))
      (write-archive (cdr variant) kind epoch path)
      (define result
        (verify-archive path platform #:sha256 archive-sha256 #:manifest-sha256 manifest-sha256))
      (hasheq
       'name
       (car variant)
       'file
       name
       'expected
       "both checks refuse"
       'archive
       (hash-ref result 'archive)
       'tree
       (hash-ref result 'tree)
       'tree_problems
       (hash-ref result 'tree_problems)
       'as_designed
       (and (equal? (hash-ref result 'archive) "mismatch")
            (equal? (hash-ref result 'tree) "mismatch"))))))
  (hasheq
   'platform
   (hash-ref platform 'name)
   'rows
   rows
   'as_designed
   (andmap (lambda (row) (hash-ref row 'as_designed)) rows)))

(define (compare-builds first second platform)
  (define kind (hash-ref platform 'kind))
  (define (contents path)
    (for/hash
     ((entry (in-list (read-archive path kind))) #:when (eq? (hash-ref entry 'kind) 'file))
     (values (hash-ref entry 'path) (sha256-hex (hash-ref entry 'content)))))
  (define left (contents first))
  (define right (contents second))
  (define names (sort (remove-duplicates (append (hash-keys left) (hash-keys right))) string<?))
  (define differing
    (for/list
     ((name (in-list names)) #:unless (equal? (hash-ref left name #f) (hash-ref right name #f)))
     (hasheq
      'path
      name
      'first
      (or (hash-ref left name #f) 'null)
      'second
      (or (hash-ref right name #f) 'null))))
  (define first-sha256 (file-sha256 first))
  (define second-sha256 (file-sha256 second))
  (hasheq
   'platform
   (hash-ref platform 'name)
   'archive_identical
   (equal? first-sha256 second-sha256)
   'first_archive_sha256
   first-sha256
   'second_archive_sha256
   second-sha256
   'manifest_identical
   (equal? (hash-ref left manifest-path #f) (hash-ref right manifest-path #f))
   'differing_files
   differing))

(define (module-graph main)
  (define seen (make-hash))
  (define (visit! path)
    (define key (simplify-path path))
    (unless (hash-ref seen key #f) (hash-set! seen key #t) (visit-code! (get-module-code key) key)))
  (define (visit-code! code relative-to)
    (for*
     ((phase+imports (in-list (module-compiled-imports code)))
      (index (in-list (cdr phase+imports))))
     (define resolved (resolve-module-path-index index relative-to))
     (cond
      ((path? resolved) (visit! resolved))
      ((and (pair? resolved) (eq? (car resolved) 'submod) (path? (cadr resolved)))
       (visit! (cadr resolved)))))
    (for
     ((submodule
       (in-list
        (append (module-compiled-submodules code #t) (module-compiled-submodules code #f)))))
     (visit-code! submodule relative-to)))
  (visit! main)
  (sort (hash-keys seen) string<? #:key path->string))

(define (occurrences haystack needle)
  (if (zero? (bytes-length needle))
    0
    (let loop ((start 0) (total 0))
      (define found (regexp-match-positions (byte-regexp (regexp-quote needle)) haystack start))
      (if found (loop (cdar found) (add1 total)) total))))

(define (path-needles path)
  (define native (path->bytes (simplify-path (path->complete-path path))))
  (remove-duplicates (list native (regexp-replace* #rx#"\\\\" native #"/"))))

(define (scan-paths entries root checkout)
  (define root-needles (path-needles root))
  (define home-needles (path-needles (find-system-path 'home-dir)))
  (define checkout-needles (if checkout (path-needles checkout) '()))
  (define (total needles)
    (for*/sum
     ((entry (in-list entries))
      #:when
      (eq? (hash-ref entry 'kind) 'file)
      (needle (in-list needles)))
     (occurrences (hash-ref entry 'content) needle)))
  (define root-count (total root-needles))
  (define home-count (total home-needles))
  (define root-under-home?
    (for*/or
     ((root-needle (in-list root-needles)) (home-needle (in-list home-needles)))
     (regexp-match?
      (regexp (string-append "^" (regexp-quote (bytes->string/utf-8 home-needle #\?))))
      (bytes->string/utf-8 root-needle #\?))))
  (define home-outside-root (if root-under-home? (max 0 (- home-count root-count)) home-count))
  (define checkout-count (total checkout-needles))
  (hasheq
   'build_root
   root-count
   'home_outside_build_root
   home-outside-root
   'checkout
   checkout-count
   'pass
   (and (zero? home-outside-root) (zero? checkout-count))))

(define (embed! main executable windows?)
  (define name
    (string->symbol
     (format "#%mzc:~a" (path->string (path-replace-extension (file-name-from-path main) #"")))))
  (define (submodule part) (string->symbol (format "~a(~a)" name part)))
  (create-embedding-executable
   executable
   #:variant
   'cs
   #:expand-namespace
   (make-base-namespace)
   #:modules
   (list (list '#%mzc: (list 'file (path->string main)) '(main configure-runtime)))
   #:configure-via-first-module?
   #t
   #:early-literal-expressions
   (parameterize
    ((current-namespace (make-base-namespace)))
    (list
     (compile
      `(when (module-declared? '',(submodule "configure-runtime"))
         (dynamic-require '',(submodule "configure-runtime") #f)))))
   #:literal-expression
   (parameterize
    ((current-namespace (make-base-namespace)))
    (compile
     `(begin
        (namespace-require '',name)
        (when (module-declared? '',(submodule "main"))
          (dynamic-require '',(submodule "main") #f)))))
   #:cmdline
   '("-U" "--")
   #:collects-path
   '()
   #:aux
   (append '((config-dir . "etc")) (if windows? '((embed-dlls? . #t)) '()))))

(define (run-program program arguments)
  (define-values (process output input errors) (apply subprocess #f #f #f program arguments))
  (close-output-port input)
  (define out-bytes #f)
  (define err-bytes #f)
  (define readers
    (list
     (thread (lambda () (set! out-bytes (port->bytes output))))
     (thread (lambda () (set! err-bytes (port->bytes errors))))))
  (subprocess-wait process)
  (for-each thread-wait readers)
  (close-input-port output)
  (close-input-port errors)
  (values (subprocess-status process) out-bytes err-bytes))

(define (self-description! top source program)
  (define text ((dynamic-require (build-path source "meta.rkt") 'meta-source)))
  (define shared (build-path top "share" "typify.typify"))
  (make-directory* (build-path top "share"))
  (define bytes (string->bytes/utf-8 text))
  (call-with-output-file shared (lambda (output) (write-bytes bytes output)))
  (unless (equal? (file->bytes shared) bytes)
    (raise-user-error 'release "the packaged self-declaration differs from its source bytes"))
  (define-values
   (check-status check-output check-errors)
   (run-program program (list "check" (path->string shared) "--json")))
  (define parsed (with-handlers ((exn:fail? (lambda (error) #f))) (bytes->jsexpr check-output)))
  (unless (and
           (eqv? check-status 0)
           (hash? parsed)
           (eq? (hash-ref parsed 'ok #f) #t)
           (null? (hash-ref parsed 'diagnostics '(missing))))
    (raise-user-error
     'release
     "the packaged program does not check share/typify.typify clean: ~a ~a"
     check-output
     check-errors))
  (define-values
   (file-status file-grammar file-errors)
   (run-program program (list "emit" (path->string shared) "Typify")))
  (define-values (meta-status meta-grammar meta-errors) (run-program program '("emit" "Typify")))
  (unless (and
           (eqv? file-status 0)
           (eqv? meta-status 0)
           (positive? (bytes-length meta-grammar))
           (equal? file-grammar meta-grammar))
    (raise-user-error
     'release
     "share/typify.typify does not emit the packaged grammar: ~a ~a"
     file-errors
     meta-errors))
  (hasheq
   'path
   "typify/share/typify.typify"
   'sha256
   (sha256-hex (string->bytes/utf-8 text))
   'check
   "ok"
   'emitted_grammar_sha256
   (sha256-hex meta-grammar)
   'emit_from_file_equal
   #t))

(define (layout! distribution stage platform)
  (define windows? (eq? (hash-ref platform 'os) 'windows))
  (define program (hash-ref platform 'program))
  (define top (build-path stage "typify"))
  (when (directory-exists? stage) (delete-directory/files stage))
  (make-directory* (build-path top "bin"))
  (define built
    (if windows? (build-path distribution program) (build-path distribution "bin" program)))
  (unless (file-exists? built) (raise-user-error 'release "the distribution lacks ~a" program))
  (copy-file built (build-path top "bin" program))
  (define library (build-path distribution "lib"))
  (when (and (directory-exists? library) (pair? (directory-list library)))
    (copy-directory/files
     library
     (if windows? (build-path top "bin" "lib") (build-path top "lib"))))
  (define expected
    (append
     (list (if windows? (string->path program) (string->path "bin")))
     (if (directory-exists? library) (list (string->path "lib")) '())))
  (for
   ((name (in-list (directory-list distribution))))
   (unless (member name expected)
     (raise-user-error 'release "the distribution holds an unexpected entry: ~a" name)))
  top)

(define (build!
         #:root
         root
         #:platform
         name
         #:output
         output
         #:commit
         commit
         #:tree
         tree
         #:epoch
         epoch
         #:racket-installer
         racket-installer
         #:racket-sha256
         racket-sha256
         #:checkout
         checkout)
  (define platform (platform-entry name))
  (unless (equal? (host-platform) name)
    (raise-user-error 'release "the host is ~a, not ~a" (or (host-platform) "unsupported") name))
  (unless (and (equal? (version) racket-version) (eq? (system-type 'vm) 'chez-scheme))
    (raise-user-error
     'release
     "Racket ~a CS is required, not ~a ~a"
     racket-version
     (version)
     (system-type 'vm)))
  (define windows? (eq? (hash-ref platform 'os) 'windows))
  (define source (build-path root "typify"))
  (define main (build-path source "src" "main.rkt"))
  (parameterize ((current-namespace (make-base-namespace))) (managed-compile-zo main))
  (define modules (module-graph main))
  (define module-names
    (for/list
     ((path (in-list modules)))
     (slash-path (find-relative-path (simplify-path root) path))))
  (define solver (filter (lambda (name) (regexp-match? #rx"(?i:rosette|z3)" name)) module-names))
  (unless (null? solver) (raise-user-error 'release "the module graph holds a solver: ~a" solver))
  (define executable-directory (build-path root "executable"))
  (make-directory* executable-directory)
  (define executable (build-path executable-directory (hash-ref platform 'program)))
  (embed! main executable windows?)
  (define distribution (build-path root "distribution"))
  (assemble-distribution distribution (list executable))
  (define stage (build-path root "stage"))
  (define top (layout! distribution stage platform))
  (define self-description
    (self-description!
     top
     (build-path source "src")
     (build-path top "bin" (hash-ref platform 'program))))
  (copy-file (build-path source "LICENSE.md") (build-path top "LICENSE.md"))
  (define entries (with-manifest (staged-entries stage windows?)))
  (define scan (scan-paths entries root checkout))
  (unless (hash-ref scan 'pass)
    (raise-user-error 'release "the archive names the checkout or the home directory: ~a" scan))
  (make-directory* output)
  (define asset (hash-ref platform 'asset))
  (define archive (build-path output asset))
  (write-archive entries (hash-ref platform 'kind) epoch archive)
  (define archive-sha256 (file-sha256 archive))
  (define manifest
    (hash-ref
     (for/first
      ((entry (in-list entries)) #:when (equal? (hash-ref entry 'path) manifest-path))
      entry)
     'content))
  (define manifest-sha256 (sha256-hex manifest))
  (define check
    (verify-archive archive platform #:sha256 archive-sha256 #:manifest-sha256 manifest-sha256))
  (unless (and
           (equal? (hash-ref check 'archive) "ok")
           (equal? (hash-ref check 'tree) "ok")
           (equal? (hash-ref check 'architecture) "ok"))
    (raise-user-error 'release "the written archive fails its own check: ~a" check))
  (call-with-output-file
   (build-path output (string-append asset ".sha256"))
   #:exists
   'truncate/replace
   (lambda (port) (fprintf port "~a  ~a\n" archive-sha256 asset)))
  (call-with-output-file
   (build-path output (string-append name ".MANIFEST.txt"))
   #:exists
   'truncate/replace
   (lambda (port) (write-bytes manifest port)))
  (define record
    (hasheq
     'platform
     name
     'asset
     asset
     'archive_sha256
     archive-sha256
     'manifest_sha256
     manifest-sha256
     'files
     (length (listed-files entries))
     'bytes
     (for/sum ((entry (in-list (listed-files entries)))) (bytes-length (hash-ref entry 'content)))
     'source
     (hasheq 'commit commit 'tree tree 'source_date_epoch epoch)
     'racket
     (hasheq
      'version
      (version)
      'vm
      (symbol->string (system-type 'vm))
      'installer
      racket-installer
      'installer_sha256
      racket-sha256)
     'build_root
     (path->string (simplify-path root))
     'modules
     (length module-names)
     'solver_modules
     solver
     'path_scan
     scan
     'self_description
     self-description
     'architecture
     (hasheq
      'pass
      (equal? (hash-ref check 'architecture) "ok")
      'executables
      (hash-ref check 'executables))))
  (call-with-output-file
   (build-path output (string-append name ".build.json"))
   #:exists
   'truncate/replace
   (lambda (port) (write-json record port) (newline port)))
  record)

(define (option arguments name (default #f))
  (define tail (member name arguments))
  (cond
   ((and tail (pair? (cdr tail))) (cadr tail))
   (default default)
   (else (raise-user-error 'release "missing ~a" name))))

(define (print-json value) (write-json value) (newline))

(define (usage)
  (displayln
   "usage: racket scripts/release.rkt build --root DIR --platform NAME --output DIR --commit SHA --tree SHA --epoch SECONDS --racket-installer NAME --racket-sha256 HEX [--checkout DIR] | verify ARCHIVE --platform NAME --sha256 HEX --manifest-sha256 HEX | canaries ARCHIVE --platform NAME --sha256 HEX --manifest-sha256 HEX --epoch SECONDS --output DIR | architecture ARCHIVE --platform NAME | epoch BUILD_JSON | compare FIRST SECOND --platform NAME | platform"
   (current-error-port))
  (exit 2))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (when (null? arguments) (usage))
 (case (car arguments)
   (("build")
    (print-json
     (build!
      #:root
      (path->complete-path (option arguments "--root"))
      #:platform
      (option arguments "--platform")
      #:output
      (path->complete-path (option arguments "--output"))
      #:commit
      (option arguments "--commit")
      #:tree
      (option arguments "--tree")
      #:epoch
      (string->number (option arguments "--epoch"))
      #:racket-installer
      (option arguments "--racket-installer")
      #:racket-sha256
      (option arguments "--racket-sha256")
      #:checkout
      (let ((given (option arguments "--checkout" "")))
        (and (positive? (string-length given)) given)))))
   (("verify")
    (when (< (length arguments) 2) (usage))
    (define result
      (verify-archive
       (cadr arguments)
       (platform-entry (option arguments "--platform"))
       #:sha256
       (option arguments "--sha256")
       #:manifest-sha256
       (option arguments "--manifest-sha256")))
    (print-json result)
    (exit
     (if (and (equal? (hash-ref result 'archive) "ok")
              (equal? (hash-ref result 'tree) "ok")
              (equal? (hash-ref result 'architecture) "ok"))
       0
       1)))
   (("architecture")
    (when (< (length arguments) 2) (usage))
    (define platform (platform-entry (option arguments "--platform")))
    (define result
      (with-handlers
       ((exn:fail?
         (lambda (error)
           (hasheq 'platform (hash-ref platform 'name) 'pass #f 'error (exn-message error)))))
       (architecture-report (read-archive (cadr arguments) (input-archive-kind (cadr arguments))) platform)))
    (print-json result)
    (exit (if (hash-ref result 'pass) 0 1)))
   (("epoch")
    (when (< (length arguments) 2) (usage))
    (define record (call-with-input-file (cadr arguments) read-json))
    (displayln (hash-ref (hash-ref record 'source) 'source_date_epoch)))
   (("canaries")
    (when (< (length arguments) 2) (usage))
    (define result
      (canary-matrix
       (cadr arguments)
       (platform-entry (option arguments "--platform"))
       (string->number (option arguments "--epoch"))
       (option arguments "--output")
       #:sha256
       (option arguments "--sha256")
       #:manifest-sha256
       (option arguments "--manifest-sha256")))
    (print-json result)
    (exit (if (hash-ref result 'as_designed) 0 1)))
   (("compare")
    (when (< (length arguments) 3) (usage))
    (print-json
     (compare-builds
      (cadr arguments)
      (caddr arguments)
      (platform-entry (option arguments "--platform")))))
   (("platform")
    (define name (host-platform))
    (unless name (raise-user-error 'release "this host has no release platform"))
    (displayln name))
   (else (usage))))
