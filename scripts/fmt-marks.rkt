#lang racket/base

(require file/untgz
         json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "../src/main.rkt"
         (only-in "../src/meta.rkt" meta-source)
         (only-in "../src/fmt.rkt" canonical-bytes)
         (only-in "../src/language.rkt" read-declarations language-comment)
         (only-in
          "../src/syntax.rkt"
          read-lines
          line-record-kind
          line-record-left
          line-record-right
          line-record-raw))

(define-runtime-path repository "..")

(define (decode bytes) (bytes->string/utf-8 bytes (integer->char 65533)))

(define (encode text) (string->bytes/utf-8 text))

(define (blank-byte? byte) (or (= byte 32) (= byte 9)))

(define (trim-bytes bytes)
  (define size (bytes-length bytes))
  (define start
    (let loop ((index 0))
      (if (and (< index size) (blank-byte? (bytes-ref bytes index))) (loop (add1 index)) index)))
  (define end
    (let loop ((index size))
      (if (and (> index start) (blank-byte? (bytes-ref bytes (sub1 index))))
        (loop (sub1 index))
        index)))
  (subbytes bytes start end))

(define (find-bytes bytes needle)
  (define width (bytes-length needle))
  (for/first
   ((index (in-range (add1 (- (bytes-length bytes) width))))
    #:when
    (equal? (subbytes bytes index (+ index width)) needle))
   index))

(define (split-text bytes ends?)
  (define size (bytes-length bytes))
  (let loop ((start 0) (lines '()))
    (define ending
      (for/first ((index (in-range start size)) #:when (= (bytes-ref bytes index) 10)) index))
    (cond
     (ending
      (define end
        (cond
         (ends? (add1 ending))
         ((and (> ending start) (= (bytes-ref bytes (sub1 ending)) 13)) (sub1 ending))
         (else ending)))
      (loop (add1 ending) (cons (subbytes bytes start end) lines)))
     ((= start size) (reverse lines))
     (else (reverse (cons (subbytes bytes start size) lines))))))

(define (ends-with-newline? bytes)
  (and (positive? (bytes-length bytes)) (= (bytes-ref bytes (sub1 (bytes-length bytes))) 10)))

(define (join-lines lines ending final?)
  (cond
   ((null? lines) (if final? ending #""))
   (else
    (define body (apply bytes-append (add-between lines ending)))
    (if final? (bytes-append body ending) body))))

(define (expected-check-line current canonical)
  (let loop ((left (split-text current #t)) (right (split-text canonical #t)) (line 1))
    (cond
     ((and (null? left) (null? right)) #f)
     ((or (null? left) (null? right) (not (equal? (car left) (car right)))) line)
     (else (loop (cdr left) (cdr right) (add1 line))))))

(struct observation (status stdout stderr) #:transparent)

(define (run execute directory arguments)
  (define output (open-output-bytes))
  (define errors (open-output-bytes))
  (define status
    (with-handlers
     ((exn:fail? (lambda (failure) (string-append "crash: " (exn-message failure)))))
     (parameterize
      ((current-directory directory) (current-output-port output) (current-error-port errors))
      (define-values (status diagnostics) (execute arguments))
      status)))
  (observation status (get-output-bytes output) (get-output-bytes errors)))

(define current-table (make-parameter #f))

(define (tally! key ok? detail)
  (define table (current-table))
  (define entry (hash-ref table key (lambda () (vector 0 0 '()))))
  (hash-set!
   table
   key
   (vector
    (add1 (vector-ref entry 0))
    (if ok? (add1 (vector-ref entry 1)) (vector-ref entry 1))
    (if (or ok? (>= (length (vector-ref entry 2)) 5))
      (vector-ref entry 2)
      (append (vector-ref entry 2) (list (detail)))))))

(define current-notes (make-parameter #f))

(define current-cases (make-parameter #f))

(define (m1-totals)
  (for/fold
   ((run 0) (held 0))
   (((key entry) (in-hash (current-table))) #:when (string-prefix? key "M1 "))
   (values (+ run (vector-ref entry 0)) (+ held (vector-ref entry 1)))))

(define (print-cases cases)
  (printf "M1 by case: texts read, texts accepted, every invocation exits 0, M1 run and held\n")
  (for
   ((entry (in-list cases)))
   (printf
    "  ~a  ~a: texts ~a, accepted ~a, every invocation exits 0: ~a, M1 run ~a, held ~a\n"
    (if (= (list-ref entry 4) (list-ref entry 5)) "pass" "FAIL")
    (list-ref entry 0)
    (list-ref entry 1)
    (list-ref entry 2)
    (if (list-ref entry 3) "yes" "no")
    (list-ref entry 4)
    (list-ref entry 5)))
  (define corpus
    (filter
     (lambda (entry)
       (not (or (string-prefix? (car entry) "control-") (equal? (car entry) "ask-gate"))))
     cases))
  (printf "  corpus cases: ~a\n" (length corpus))
  (printf
   "  corpus cases whose every invocation exits 0: ~a\n"
   (count (lambda (entry) (list-ref entry 3)) corpus))
  (printf
   "  corpus cases with a text that check accepts: ~a\n"
   (count (lambda (entry) (positive? (list-ref entry 2))) corpus))
  (printf
   "  corpus cases measured under M1, run above 0: ~a\n"
   (count (lambda (entry) (positive? (list-ref entry 4))) corpus))
  (andmap (lambda (entry) (= (list-ref entry 4) (list-ref entry 5))) cases))

(define (note! key (amount 1)) (hash-update! (current-notes) key (lambda (n) (+ n amount)) 0))

(define position-keys '(range line column byteOffset origin origins related inputs fingerprints))

(define (strip value)
  (cond
   ((hash? value)
    (for/hash
     (((key item) (in-hash value)) #:unless (memq key position-keys))
     (values key (strip item))))
   ((list? value) (map strip value))
   (else value)))

(define (echo-reading line)
  (define outer (car (read-lines line #:comment-prefix "")))
  (if (and (eq? (line-record-kind outer) 'colon) (equal? (line-record-left outer) "?"))
    (let ((inner (car (read-lines (line-record-right outer) #:comment-prefix ""))))
      (list "?" (line-record-kind inner) (line-record-left inner) (line-record-right inner)))
    (list (line-record-kind outer) (line-record-left outer) (line-record-right outer))))

(define (positionals arguments)
  (let loop ((remaining arguments) (kept '()))
    (cond
     ((null? remaining) (reverse kept))
     ((equal? (car remaining) "--facts")
      (loop (if (pair? (cdr remaining)) (cddr remaining) '()) kept))
     ((string-prefix? (car remaining) "--") (loop (cdr remaining) kept))
     (else (loop (cdr remaining) (cons (car remaining) kept))))))

(define (facts-arguments arguments)
  (define tail (member "--facts" arguments))
  (if (and tail (pair? (cdr tail))) (list "--facts" (cadr tail)) '()))

(define (projection arguments observed)
  (define stdout (observation-stdout observed))
  (define command (let ((items (positionals arguments))) (and (pair? items) (car items))))
  (define parsed
    (and (member "--json" arguments)
         (with-handlers ((exn:fail? (lambda (_) #f))) (bytes->jsexpr stdout))))
  (list
   (observation-status observed)
   (cond
    ((not parsed) stdout)
    (else
     (define stripped (strip parsed))
     (if (and (equal? command "ask") (hash? stripped) (list? (hash-ref stripped 'lines #f)))
       (hash-set stripped 'lines (map echo-reading (hash-ref stripped 'lines)))
       stripped)))
   (observation-stderr observed)))

(define (line-reading record)
  (define kind (line-record-kind record))
  (if (memq kind '(colon arrow))
    (list kind (line-record-left record) (line-record-right record))
    (list kind (string-trim (line-record-raw record) #px"[ \t]+"))))

(define (readings bytes prefix)
  (for/list
   ((record (in-list (read-lines (decode bytes) #:comment-prefix prefix)))
    #:unless
    (eq? (line-record-kind record) 'blank))
   (line-reading record)))

(define (layout bytes prefix)
  (let loop ((records (read-lines (decode bytes) #:comment-prefix prefix))
             (seen? #f)
             (gap? #f)
             (kept '()))
    (cond
     ((null? records) (reverse kept))
     ((eq? (line-record-kind (car records)) 'blank) (loop (cdr records) seen? seen? kept))
     (else
      (loop
       (cdr records)
       #t
       #f
       (cons (line-reading (car records)) (if gap? (cons 'gap kept) kept)))))))

(define (reads-alike? one other prefix) (equal? (layout one prefix) (layout other prefix)))

(define (line-shape line prefix)
  (define record (car (read-lines (decode line) #:comment-prefix prefix)))
  (define kind (line-record-kind record))
  (define body (trim-bytes line))
  (cond
   ((memq kind '(colon arrow))
    (define separator (if (eq? kind 'colon) #":" #"->"))
    (define at (find-bytes body separator))
    (list
     kind
     (trim-bytes (subbytes body 0 at))
     separator
     (trim-bytes (subbytes body (+ at (bytes-length separator))))))
   (else (list kind body))))

(define (spell shape before after)
  (if (memq (car shape) '(colon arrow))
    (bytes-append (list-ref shape 1) before (list-ref shape 2) after (list-ref shape 3))
    (list-ref shape 1)))

(define (variants original prefix program?)
  (define lines (split-text original #f))
  (define shapes (for/list ((line (in-list lines))) (line-shape line prefix)))
  (define final? (ends-with-newline? original))
  (define (blank? shape) (eq? (car shape) 'blank))
  (define (render before after) (for/list ((shape (in-list shapes))) (spell shape before after)))
  (define (runs rendered)
    (append
     (if program? (list #" " #"") '())
     (append*
      (for/list
       ((line (in-list rendered)) (shape (in-list shapes)))
       (if (blank? shape) (list line #" " #"\t") (list line))))
     (list #"" #"  ")))
  (define (indent rendered)
    (for/list
     ((line (in-list rendered)) (shape (in-list shapes)) (index (in-naturals)))
     (if (blank? shape) line (bytes-append (if (even? index) #"  " #"\t") line #" \t"))))
  (list
   (cons "crlf" (join-lines lines #"\r\n" final?))
   (cons "last newline" (join-lines lines #"\n" (not final?)))
   (cons "blank runs" (join-lines (runs lines) #"\n" final?))
   (cons "indentation" (join-lines (indent lines) #"\n" final?))
   (cons "loose separators" (join-lines (render #"   " #"  ") #"\n" final?))
   (cons "tab separators" (join-lines (render #"\t" #"\t") #"\n" final?))
   (cons "tight separators" (join-lines (render #"" #"") #"\n" final?))
   (cons "all at once" (join-lines (runs (indent (render #" \t " #"\t  "))) #"\r\n" (not final?)))))

(struct trial (id source invocations) #:transparent)

(define (working-copy trial work)
  (define directory (build-path work (trial-id trial)))
  (when (directory-exists? directory) (delete-directory/files directory))
  (make-directory* work)
  (define source (trial-source trial))
  (cond
   ((hash? source)
    (make-directory* directory)
    (for
     (((name bytes) (in-hash source)))
     (define path (build-path directory name))
     (make-directory* (path-only path))
     (call-with-output-file path (lambda (output) (void (write-bytes bytes output))))))
   (else (copy-directory/files source directory)))
  directory)

(define (corpus-trials)
  (define root (build-path repository "test" "corpus"))
  (define receipt (call-with-input-file (build-path root "import.json") read-json))
  (define deferred (map (lambda (entry) (hash-ref entry 'id)) (hash-ref receipt 'deferred '())))
  (for/list
   ((name (in-list (hash-ref receipt 'cases))) #:unless (member name deferred))
   (define data (call-with-input-file (build-path root name "expected.json") read-json))
   (define invocations
     (append
      (if (hash-has-key? data 'invocation) (list (hash-ref data 'invocation)) '())
      (for/list
       ((subcase (in-list (hash-ref data 'subcases '())))
        #:when
        (hash-has-key? subcase 'invocation))
       (hash-ref subcase 'invocation))))
   (trial
    name
    (build-path root name)
    (for/list
     ((invocation (in-list invocations)))
     (if (and (pair? invocation) (equal? (car invocation) "typify"))
       (cdr invocation)
       invocation)))))

(define (ask-gate-trial all?)
  (define root (build-path repository "test" "ask-gate"))
  (define data
    (car (hash-ref (call-with-input-file (build-path root "gate.json") read-json) 'declarations)))
  (define path (hash-ref data 'path))
  (trial
   "ask-gate"
   (hash path (file->bytes (build-path root path)))
   (for/list
    ((entry (in-list (hash-ref data 'words)))
     #:when
     (or all? (member (hash-ref entry 'set) '("witness" "unknown"))))
    (list "ask" path (hash-ref data 'language) (hash-ref entry 'word) "--json"))))

(define (fenced-typify-blocks path)
  (let loop ((lines (file->lines path)) (inside? #f) (current '()) (blocks '()))
    (cond
     ((null? lines) (reverse blocks))
     ((string-prefix? (car lines) "```")
      (cond
       (inside?
        (loop
         (cdr lines)
         #f
         '()
         (cons (string-append (string-join (reverse current) "\n") "\n") blocks)))
       ((equal? (car lines) "```typify") (loop (cdr lines) #t '() blocks))
       (else (loop (cdr lines) #f '() blocks))))
     (inside? (loop (cdr lines) #t (cons (car lines) current) blocks))
     (else (loop (cdr lines) #f current blocks)))))

(define (catalog-invocations)
  (list
   (list "check" "catalog-language.typify" "--json")
   (list "check" "catalog-language.typify" "Catalog" "catalog.typify" "--json")
   (list "ask" "catalog-language.typify" "Catalog" "item" "--json")
   (list "ask" "catalog-language.typify" "Catalog" "category" "--json")
   (list "emit" "catalog-language.typify" "Catalog" "--json")
   (list "emit" "catalog-language.typify" "Catalog" "--gbnf")))

(define (lines->bytes . lines) (encode (string-append (string-join lines "\n") "\n")))

(define simple-language
  (lines->bytes
   "#lang typify"
   "declare : Notes"
   "is : word = /[^ ]+/"
   "is : rest = /.*/"
   "term : x : word"
   "form : x : <word>"
   "does : x <word>"
   "term : y : rest"
   "form : y -> <rest>"
   "does : y <rest>"
   "end!"))

(define (control-trials)
  (define blocks (fenced-typify-blocks (build-path repository "README.md")))
  (define nbsp (encode (string (integer->char 160))))
  (list
   (trial
    "control-specimen"
    (hash
     "catalog-language.typify"
     (lines->bytes
      "#lang typify"
      ""
      "declare:Catalog"
      "  file : catalog.typify"
      "  is   : category = \"book\" | \"journal\""
      "  is   : title = /[^=]+/"
      "  term : item : category title"
      "  form : item : <category> = <title>"
      "  does : item <category> <title>"
      "  example : item : book = Algebra"
      ""
      ""
      "  count : item = 1..*"
      "end!")
     "catalog.typify"
     #"\n   item:journal = Market Research")
    (catalog-invocations))
   (trial
    "control-readme"
    (hash "catalog-language.typify" (encode (car blocks)) "catalog.typify" (encode (cadr blocks)))
    (catalog-invocations))
   (trial
    "control-note"
    (hash
     "notes.typify"
     (lines->bytes
      "#lang typify"
      "declare:Empty"
      "  is : none = \"a\" except \"a\""
      "  is :\tword = /[a-z]+/"
      "  term : x : word"
      "  form : x : <word>"
      "  does : x <word>"
      "end!")
     "program.typify"
     #"x:y  \n")
    (list
     (list "check" "notes.typify" "--json")
     (list "check" "notes.typify" "Empty" "program.typify" "--json")))
   (trial
    "control-empty"
    (hash
     "notes.typify"
     simple-language
     "empty.typify"
     #""
     "blank.typify"
     #"\n \t\n\r\n"
     "comment.typify"
     #"\n  # only a comment  \n\n")
    (list
     (list "check" "notes.typify" "Notes" "empty.typify" "--json")
     (list "check" "notes.typify" "Notes" "blank.typify" "--json")
     (list "check" "notes.typify" "Notes" "comment.typify" "--json")))
   (trial
    "control-bytes"
    (hash
     "notes.typify"
     simple-language
     "program.typify"
     #"  x:a\377b\t\ny->\300 c\n#\tcomment \351 \n")
    (list (list "check" "notes.typify" "Notes" "program.typify" "--json")))
   (trial
    "control-nbsp"
    (hash
     "notes.typify"
     simple-language
     "program.typify"
     (bytes-append #"# a comment that ends in U+00A0" nbsp #"\nx : a\n"))
    (list (list "check" "notes.typify" "Notes" "program.typify" "--json")))
   (glued-trial "control-glued-colon" "Glued" "a:" "a : <word>" #"a : b\n" #"a:b\n")
   (glued-trial "control-glued-arrow" "Dash" "a-" "a -> <word>" #"a -> b\n" #"a->b\n")))

(define (glued-trial id language prefix form separated joined)
  (trial
   id
   (hash
    "glued.typify"
    (lines->bytes
     "#lang typify"
     (string-append "declare : " language)
     (string-append "comment : " prefix)
     "is : word = /[^ ]+/"
     "term : a : word"
     (string-append "form : " form)
     "does : a <word>"
     "end!")
    "separated.typify"
    separated
    "joined.typify"
    joined)
   (list
    (list "check" "glued.typify" "--json")
    (list "check" "glued.typify" language "separated.typify" "--json")
    (list "check" "glued.typify" language "joined.typify" "--json"))))

(define (blank-prefix-trials)
  (list
   (trial
    "control-prefix-colon"
    (hash
     "spaced.typify"
     (lines->bytes
      "#lang typify"
      "declare : Spaced"
      "comment : x :"
      "is : word = /[^ ]+/"
      "term : x : word"
      "form : x : <word>"
      "does : x <word>"
      "end!")
     "program.typify"
     #"x:y\n"
     "accepted.typify"
     #"x : a\n"
     "refused.typify"
     #"x:a b\n")
    (list
     (list "check" "spaced.typify" "--json")
     (list "check" "spaced.typify" "Spaced" "program.typify" "--json")
     (list "check" "spaced.typify" "Spaced" "accepted.typify" "--json")
     (list "check" "spaced.typify" "Spaced" "refused.typify" "--json")))
   (trial
    "control-prefix-arrow"
    (hash
     "dash.typify"
     (lines->bytes
      "#lang typify"
      "declare : Dash"
      "comment : a -"
      "is : word = /[^ ]+/"
      "term : a : word"
      "form : a -> <word>"
      "does : a <word>"
      "end!")
     "program.typify"
     #"a->b\n")
    (list
     (list "check" "dash.typify" "--json")
     (list "check" "dash.typify" "Dash" "program.typify" "--json")))))

(define (program-path root path) (if (equal? root ".") path (string-append root "/" path)))

(define (tree-texts execute directory declarations root facts)
  (define observed (run execute directory (append (list "tree" declarations root "--json") facts)))
  (define data
    (with-handlers ((exn:fail? (lambda (_) #f))) (bytes->jsexpr (observation-stdout observed))))
  (for/list
   ((entry (in-list (if (hash? data) (hash-ref data 'programs '()) '())))
    #:when
    (file-exists? (build-path directory (program-path root (hash-ref entry 'path)))))
   (list declarations (hash-ref entry 'language) (program-path root (hash-ref entry 'path)))))

(define reading-commands '("check" "tree" "plan" "ask" "emit"))

(define (trial-texts execute directory invocations)
  (remove-duplicates
   (append*
    (for/list
     ((arguments (in-list invocations)))
     (define items (positionals arguments))
     (define command (and (pair? items) (car items)))
     (cond
      ((or (< (length items) 2)
           (not (member command reading-commands))
           (equal? (cadr items) "Typify"))
       '())
      (else
       (define declarations (cadr items))
       (cons
        (list declarations)
        (cond
         ((and (member command '("check" "plan")) (= (length items) 4))
          (list (list declarations (caddr items) (cadddr items))))
         ((and (member command '("tree" "plan")) (= (length items) 3))
          (tree-texts execute directory declarations (caddr items) (facts-arguments arguments)))
         (else '())))))))))

(define (reads? arguments text)
  (define items (positionals arguments))
  (and (>= (length items) 2)
       (member (car items) reading-commands)
       (equal? (cadr items) (car text))
       (or (= (length text) 1)
           (and (= (length items) 4)
                (member (car items) '("check" "plan"))
                (equal? (cadddr items) (caddr text)))
           (and (= (length items) 3) (member (car items) '("tree" "plan"))))))

(define (text-path directory text) (build-path directory (last text)))

(define (read-text directory text) (file->bytes (text-path directory text)))

(define (write-text! directory text bytes)
  (call-with-output-file
   (text-path directory text)
   (lambda (output) (void (write-bytes bytes output)))
   #:exists
   'truncate))

(define (text-prefix directory text)
  (if (= (length text) 1)
    "#"
    (language-comment
     (hash-ref
      (read-declarations (decode (file->bytes (build-path directory (car text)))) #:file (car text))
      (cadr text)))))

(define (snapshot directory)
  (sort
   (for/list
    ((path (in-directory directory)))
    (cons
     (path->string (find-relative-path directory path))
     (if (file-exists? path) (file->bytes path) 'directory)))
   string<?
   #:key
   car))

(define (describe trial text . more)
  (string-join (append (list (trial-id trial) (format "~s" text)) more) " "))

(struct prepared (directory indexed baseline texts checks accepted formatted))

(define (prepare execute trial work)
  (define directory (working-copy trial work))
  (define indexed
    (for/list
     ((arguments (in-list (trial-invocations trial))) (index (in-naturals)))
     (cons index arguments)))
  (define baseline
    (for/hash
     ((item (in-list indexed)))
     (values (car item) (projection (cdr item) (run execute directory (cdr item))))))
  (define texts (trial-texts execute directory (trial-invocations trial)))
  (define checks
    (for/hash ((text (in-list texts))) (values text (run execute directory (cons "check" text)))))
  (define accepted
    (filter (lambda (text) (equal? 0 (observation-status (hash-ref checks text)))) texts))
  (define formatted
    (for/hash
     ((text (in-list accepted)))
     (define observed (run execute directory (cons "fmt" text)))
     (values
      text
      (and (equal? 0 (observation-status observed))
           (equal? #"" (observation-stderr observed))
           (observation-stdout observed)))))
  (prepared directory indexed baseline texts checks accepted formatted))

(define (observe-reading execute state text)
  (for/hash
   ((item (in-list (prepared-indexed state))) #:when (reads? (cdr item) text))
   (values (car item) (projection (cdr item) (run execute (prepared-directory state) (cdr item))))))

(define (same! key reference observed describe-item)
  (for
   (((index seen) (in-hash observed)))
   (tally! key (equal? seen (hash-ref reference index)) (lambda () (describe-item index)))))

(define (measure-trial execute trial work #:variants? (variants? #t))
  (define state (prepare execute trial work))
  (define directory (prepared-directory state))
  (define (invocation index) (cdr (assv index (prepared-indexed state))))
  (define (run-fmt text . flags) (run execute directory (append (cons "fmt" text) flags)))
  (define-values (run-before held-before) (m1-totals))
  (define every-zero?
    (and (pair? (prepared-indexed state))
         (for/and ((value (in-hash-values (prepared-baseline state)))) (equal? (car value) 0))))
  (note! "texts read by the invocations" (length (prepared-texts state)))
  (note! "texts that check accepts" (length (prepared-accepted state)))
  (when every-zero? (note! "trials whose every invocation exits 0"))
  (for
   ((text (in-list (prepared-accepted state))))
   (define original (read-text directory text))
   (define canonical (hash-ref (prepared-formatted state) text))
   (tally!
    "M1 fmt formats each accepted text, exit 0 and nothing on standard error"
    (and canonical #t)
    (lambda () (describe trial text)))
   (when canonical
     (unless (equal? canonical original) (note! "accepted texts that fmt changes"))
     (write-text! directory text canonical)
     (define reference (observe-reading execute state text))
     (same!
      "M1 invocations on each formatted text, equal under the projection"
      (prepared-baseline state)
      reference
      (lambda (index) (describe trial text (format "~s" (invocation index)))))
     (define prefix (text-prefix directory text))
     (tally!
      "M2 rule 8: the same payloads, comment lines, order and counts"
      (equal? (readings original prefix) (readings canonical prefix))
      (lambda () (describe trial text)))
     (tally!
      "M2 L2: fmt of the canonical form is itself"
      (equal? (run-fmt text) (observation 0 canonical #""))
      (lambda () (describe trial text)))
     (tally!
      "M2 --check on the canonical form: no output, exit 0"
      (equal? (run-fmt text "--check") (observation 0 #"" #""))
      (lambda () (describe trial text)))
     (tally!
      "M2 --write on the canonical form: silent, exit 0, the file unchanged"
      (and (equal? (run-fmt text "--write") (observation 0 #"" #""))
           (equal? (read-text directory text) canonical))
      (lambda () (describe trial text)))
     (when variants?
       (for
        ((variant (in-list (variants original prefix (= (length text) 3)))))
        (define label (car variant))
        (define bytes (cdr variant))
        (define alike? (reads-alike? bytes original prefix))
        (write-text! directory text bytes)
        (unless (string-prefix? (trial-id trial) "control-")
          (tally!
           "M2 each variant of a corpus or ask-gate text reads alike its original, none left out"
           alike?
           (lambda () (describe trial text label))))
        (cond
         ((not alike?) (note! "variants left out: they read otherwise than their original"))
         ((not (equal? 0 (observation-status (run execute directory (cons "check" text)))))
          (tally! "M2 every variant is accepted" #f (lambda () (describe trial text label))))
         (else
          (note! "variants measured")
          (tally!
           "M2 L3: each variant's form is the text's form"
           (equal? (run-fmt text) (observation 0 canonical #""))
           (lambda () (describe trial text label)))
          (define line (expected-check-line bytes canonical))
          (tally!
           "M2 --check on each variant: the first differing line, exit 1"
           (equal?
            (run-fmt text "--check")
            (if line
              (observation
               1
               (encode (format "~a:~a: not in canonical form\n" (last text) line))
               #"")
              (observation 0 #"" #"")))
           (lambda () (describe trial text label)))
          (same!
           "M1 invocations on each variant, equal to those on its form"
           reference
           (observe-reading execute state text)
           (lambda (index) (describe trial text label (format "~s" (invocation index)))))
          (tally!
           "M2 --write on each variant: silent, exit 0, the file becomes the form"
           (and (equal? (run-fmt text "--write") (observation 0 #"" #""))
                (equal? (read-text directory text) canonical))
           (lambda () (describe trial text label))))))
       (define loose (cdr (list-ref (variants original prefix (= (length text) 3)) 1)))
       (unless (equal? loose canonical)
         (define path (text-path directory text))
         (define mode (file-or-directory-permissions path 'bits))
         (write-text! directory text loose)
         (file-or-directory-permissions path 292)
         (define refused (run-fmt text "--write"))
         (file-or-directory-permissions path mode)
         (tally!
          "M2 --write on a file it cannot write: E_FILE, exit 2, the file unchanged"
          (and (equal?
                refused
                (observation
                 2
                 #""
                 (encode
                  (format
                   "~a:0:0: error: The requested file cannot be written [E_FILE]\n"
                   (last text)))))
               (equal? (read-text directory text) loose))
          (lambda () (describe trial text)))
         (write-text! directory text canonical)
         (file-or-directory-permissions path 292)
         (define untouched (run-fmt text "--write"))
         (file-or-directory-permissions path mode)
         (tally!
          "M2 --write writes only when the form differs (a read-only canonical file passes)"
          (equal? untouched (observation 0 #"" #""))
          (lambda () (describe trial text)))))
     (write-text! directory text original)))
  (define changed
    (for/list
     ((text (in-list (prepared-accepted state))) #:when (hash-ref (prepared-formatted state) text))
     text))
  (for
   ((text (in-list changed)))
   (write-text! directory text (hash-ref (prepared-formatted state) text)))
  (for
   ((item (in-list (prepared-indexed state)))
    #:when
    (ormap (lambda (text) (reads? (cdr item) text)) changed))
   (tally!
    "M1 invocations with every accepted text formatted at once, equal under the projection"
    (equal?
     (projection (cdr item) (run execute directory (cdr item)))
     (hash-ref (prepared-baseline state) (car item)))
    (lambda () (describe trial '() (format "~s" (cdr item))))))
  (for
   ((text (in-list (prepared-texts state))))
   (define observed (hash-ref (prepared-checks state) text))
   (unless (equal? 0 (observation-status observed))
     (note! "texts that check refuses")
     (define before (snapshot directory))
     (for
      ((flags (in-list '(() ("--write") ("--check")))))
      (tally!
       "M3 refused texts: check's standard error and status, nothing on standard output, nothing written"
       (and (equal?
             (apply run-fmt text flags)
             (observation (observation-status observed) #"" (observation-stderr observed)))
            (equal? (snapshot directory) before))
       (lambda () (describe trial text (format "~s" flags)))))))
  (define-values (run-after held-after) (m1-totals))
  (when (current-cases)
    (set-box!
     (current-cases)
     (cons
      (list
       (trial-id trial)
       (length (prepared-texts state))
       (length (prepared-accepted state))
       every-zero?
       (- run-after run-before)
       (- held-after held-before))
      (unbox (current-cases)))))
  state)

(define fmt-usage
  #":0:0: error: usage: typify fmt DECLARATIONS [LANGUAGE PROGRAM] [--write | --check] [E_USAGE]\n")

(define (measure-refusals execute work)
  (for
   ((trial (in-list (blank-prefix-trials))))
   (define state (prepare execute trial work))
   (define directory (prepared-directory state))
   (for
    ((text (in-list (prepared-accepted state))))
    (define before (snapshot directory))
    (cond
     ((= (length text) 1)
      (tally!
       "M3 a declarations file that declares a prefix with a blank is itself formatted"
       (and (bytes? (hash-ref (prepared-formatted state) text)) #t)
       (lambda () (describe trial text))))
     (else
      (for
       ((flags (in-list '(() ("--write") ("--check")))))
       (tally!
        "M3 a prefix with a blank: the message, exit 2, nothing on standard output, nothing written"
        (and (equal?
              (run execute directory (append (cons "fmt" text) flags))
              (observation
               2
               #""
               #":0:0: error: fmt does not format a text whose comment prefix holds a blank [E_USAGE]\n"))
             (equal? (snapshot directory) before))
        (lambda () (describe trial text (format "~s" flags))))))))
   (for
    ((text (in-list (prepared-texts state))) #:unless (member text (prepared-accepted state)))
    (define observed (hash-ref (prepared-checks state) text))
    (define before (snapshot directory))
    (for
     ((flags (in-list '(() ("--write") ("--check")))))
     (tally!
      "M3 section 12: a refused program under a prefix with a blank gets check's E_CLASS line and status, nothing on standard output, nothing written"
      (and (regexp-match?
            #rx#"^refused[.]typify:1:3: error: The captured token fails its class \\[E_CLASS\\]\n$"
            (observation-stderr observed))
           (equal? (observation-status observed) 1)
           (equal?
            (run execute directory (append (cons "fmt" text) flags))
            (observation 1 #"" (observation-stderr observed)))
           (equal? (snapshot directory) before))
      (lambda () (describe trial text (format "~s ~s" flags observed)))))))
  (define state (prepare execute (car (control-trials)) work))
  (define directory (prepared-directory state))
  (define before (snapshot directory))
  (define declarations "catalog-language.typify")
  (define program (list declarations "Catalog" "catalog.typify"))
  (define (message text) (encode (format ":0:0: error: ~a [E_USAGE]\n" text)))
  (for
   ((entry
     (in-list
      (list
       (cons '("fmt") fmt-usage)
       (cons (list "fmt" declarations "Catalog") fmt-usage)
       (cons (append (cons "fmt" program) '("extra")) fmt-usage)
       (cons (list "fmt" declarations "--write" "--check") fmt-usage)
       (cons (append (cons "fmt" program) '("--check" "--write")) fmt-usage)
       (cons (list "fmt" declarations "--json") fmt-usage)
       (cons (list "fmt" declarations "--sarif") fmt-usage)
       (cons (list "fmt" declarations "--lsp") fmt-usage)
       (cons (list "fmt" declarations "--github-annotations") fmt-usage)
       (cons (append (cons "fmt" program) '("--json" "--write")) fmt-usage)
       (cons
        (list "fmt" declarations "--facts" "facts.json")
        (message "--facts is available only with check, tree or plan"))
       (cons (list "fmt" declarations "--bounds") (message "--bounds is available only with check"))
       (cons (list "fmt" declarations "--gbnf") (message "--gbnf is available only with emit"))
       (cons
        (list "fmt" declarations "--write" "--write")
        (message "A command-line flag was repeated"))
       (cons
        (list "fmt" declarations "--bogus")
        (message "The command-line flag is not recognized"))
       (cons (list "fmt" "missing.typify" "--json") fmt-usage)
       (cons (list "fmt" "missing.typify" "--write" "--check") fmt-usage)))))
   (tally!
    "M3 fmt's command line: the ruled message, exit 2, nothing on standard output, nothing written"
    (and (equal? (run execute directory (car entry)) (observation 2 #"" (cdr entry)))
         (equal? (snapshot directory) before))
    (lambda () (format "~s" (car entry)))))
  (define unread (run execute directory '("check" "missing.typify")))
  (for
   ((flags (in-list '(() ("--write") ("--check")))))
   (tally!
    "M3 section 12: a file that check cannot read gets check's line and status once the command line is sound"
    (and (not (equal? (observation-status unread) 0))
         (equal?
          (run execute directory (append '("fmt" "missing.typify") flags))
          (observation (observation-status unread) #"" (observation-stderr unread)))
         (equal? (snapshot directory) before))
    (lambda () (format "~s ~s" flags unread)))))

(define (measure-controls execute work)
  (define (control id) (findf (lambda (trial) (equal? (trial-id trial) id)) (control-trials)))
  (define notes (working-copy (control "control-note") work))
  (define noted (run execute notes '("check" "notes.typify")))
  (tally!
   "M2 the note control: check accepts it and prints N_CLASS_EMPTY on standard error"
   (and (equal? (observation-status noted) 0)
        (regexp-match? #rx#"note: .*\\[N_CLASS_EMPTY\\]" (observation-stderr noted)))
   (lambda () (format "~s" noted)))
  (for
   ((flags (in-list '(() ("--check") ("--write")))))
   (define observed (run execute notes (append '("fmt" "notes.typify") flags)))
   (tally!
    "M2 the note control: fmt prints no note, in all three modes"
    (and (equal? (observation-stderr observed) #"")
         (equal? (observation-status observed) (if (equal? flags '("--check")) 1 0)))
    (lambda () (format "~s ~s" flags observed))))
  (define empty (working-copy (control "control-empty") work))
  (for
   ((name (in-list '("empty.typify" "blank.typify"))))
   (tally!
    "M2 the empty control: a text with no content and no comment line has zero bytes as its form"
    (equal? (run execute empty (list "fmt" "notes.typify" "Notes" name)) (observation 0 #"" #""))
    (lambda () name)))
  (define raw (working-copy (control "control-bytes") work))
  (define formatted (run execute raw '("fmt" "notes.typify" "Notes" "program.typify")))
  (tally!
   "M2 the byte control: invalid UTF-8 kept byte for byte, U+FFFD never written"
   (equal? formatted (observation 0 #"x : a\377b\ny -> \300 c\n#\tcomment \351\n" #""))
   (lambda () (format "~s" formatted)))
  (for
   ((pair (in-list '(("control-glued-colon" "Glued") ("control-glued-arrow" "Dash")))))
   (define glued (working-copy (control (car pair)) work))
   (define (program name) (list "glued.typify" (cadr pair) name))
   (define (terms name)
     (define observed (run execute glued (append (cons "check" (program name)) '("--json"))))
     (with-handlers
      ((exn:fail? (lambda (_) 'unreadable)))
      (hash-ref (bytes->jsexpr (observation-stdout observed)) 'terms)))
   (define forms
     (for/list
      ((name (in-list '("separated.typify" "joined.typify"))))
      (define text (file->bytes (build-path glued name)))
      (define accepted (run execute glued (cons "check" (program name))))
      (define formatted (run execute glued (cons "fmt" (program name))))
      (define checked (run execute glued (append (cons "fmt" (program name)) '("--check"))))
      (tally!
       "M2 the pairs of section 11: each text is accepted and is its own canonical form"
       (and (equal? (observation-status accepted) 0)
            (equal? formatted (observation 0 text #""))
            (equal? checked (observation 0 #"" #"")))
       (lambda () (format "~a ~a ~s ~s" (car pair) name formatted checked)))
      (observation-stdout formatted)))
   (define seen (map terms '("separated.typify" "joined.typify")))
   (tally!
    "M2 the pairs of section 11: the two forms stay different, and the reader reads them otherwise"
    (and (not (equal? (car forms) (cadr forms)))
         (not (equal? (car seen) (cadr seen)))
         (not (memq 'unreadable seen)))
    (lambda () (format "~a ~s ~s" (car pair) forms seen)))
   (note! (format "the pairs of section 11, terms of ~a: ~s" (car pair) seen))))

(define (measure-self execute work archive)
  (define directory (build-path work "self"))
  (when (directory-exists? directory) (delete-directory/files directory))
  (make-directory* directory)
  (define source (encode (meta-source)))
  (call-with-output-file
   (build-path directory "typify.typify")
   (lambda (output) (void (write-bytes source output))))
  (tally!
   "M4 L5: fmt maps the reader's self-declaration to itself, byte for byte"
   (equal? (run execute directory '("fmt" "typify.typify")) (observation 0 source #""))
   (lambda () "typify.typify"))
  (tally!
   "M4 L5: fmt --check on the self-declaration: no output, exit 0"
   (equal? (run execute directory '("fmt" "typify.typify" "--check")) (observation 0 #"" #""))
   (lambda () "typify.typify"))
  (cond
   (archive
    (define unpacked (build-path work "archive"))
    (when (directory-exists? unpacked) (delete-directory/files unpacked))
    (make-directory* unpacked)
    (call-with-input-file archive (lambda (input) (untgz input #:dest unpacked)))
    (define shared "typify/share/typify.typify")
    (tally!
     "M4 the archive's share/typify.typify is the reader's self-declaration, byte for byte"
     (and (file-exists? (build-path unpacked shared))
          (equal? (file->bytes (build-path unpacked shared)) source))
     (lambda () shared))
    (tally!
     "M4 fmt --check on the archive's share/typify.typify: no output, exit 0"
     (equal? (run execute unpacked (list "fmt" shared "--check")) (observation 0 #"" #""))
     (lambda () shared)))
   (else (note! "M4 archive not given, its two checks not run"))))

(define (mutant-lines canonical prefix change)
  (and canonical
       (join-lines
        (for/list
         ((line (in-list (split-text canonical #f))))
         (change line (car (read-lines (decode line) #:comment-prefix prefix))))
        #"\n"
        (positive? (bytes-length canonical)))))

(define (rebuild line record left right)
  (if (and (equal? left (line-record-left record)) (equal? right (line-record-right record)))
    line
    (encode
     (string-append
      left
      (if (eq? (line-record-kind record) 'colon) " :" " ->")
      (if (equal? right "") "" " ")
      right))))

(define (strip-payload-blanks original canonical prefix)
  (mutant-lines
   canonical
   prefix
   (lambda (line record)
     (if (memq (line-record-kind record) '(colon arrow))
       (rebuild
        line
        record
        (regexp-replace* #px"[ \t]" (line-record-left record) "")
        (regexp-replace* #px"[ \t]" (line-record-right record) ""))
       line))))

(define (blank-before-bang original canonical prefix)
  (mutant-lines
   canonical
   prefix
   (lambda (line record)
     (if (eq? (line-record-kind record) 'bang)
       (bytes-append (subbytes line 0 (sub1 (bytes-length line))) #" !")
       line))))

(define nbsp-blanks (pregexp (string-append "[ \t" (string (integer->char 160)) "]+")))

(define (nbsp-as-blank original canonical prefix)
  (define (trim text) (string-trim text nbsp-blanks))
  (mutant-lines
   canonical
   prefix
   (lambda (line record)
     (cond
      ((not (regexp-match? #rx#"\302\240" line)) line)
      ((memq (line-record-kind record) '(colon arrow))
       (rebuild line record (trim (line-record-left record)) (trim (line-record-right record))))
      (else (encode (trim (decode line))))))))

(define (sort-is-lines original canonical prefix)
  (and canonical
       (let loop ((lines (split-text canonical #f)) (kept '()))
         (cond
          ((null? lines) (join-lines (reverse kept) #"\n" (positive? (bytes-length canonical))))
          ((regexp-match? #rx#"^is : " (car lines))
           (define-values
            (run rest)
            (splitf-at lines (lambda (line) (regexp-match? #rx#"^is : " line))))
           (loop rest (append (reverse (sort run bytes<?)) kept)))
          (else (loop (cdr lines) (cons (car lines) kept)))))))

(define (rule-five-everywhere original canonical prefix) (canonical-bytes original prefix))

(define mutants
  (list
   (cons "strips blanks inside a payload" strip-payload-blanks)
   (cons "puts a blank before !" blank-before-bang)
   (cons "treats U+00A0 as a blank" nbsp-as-blank)
   (cons "sorts is lines" sort-is-lines)
   (cons "applies rule 5 under a prefix with a blank" rule-five-everywhere)))

(define (measure-mutant execute mutate trials work)
  (define caught '())
  (for
   ((trial (in-list trials)))
   (define state (prepare execute trial work))
   (define directory (prepared-directory state))
   (for
    ((text (in-list (prepared-accepted state))))
    (define original (read-text directory text))
    (define canonical (hash-ref (prepared-formatted state) text))
    (define prefix (text-prefix directory text))
    (define mutated (mutate original canonical prefix))
    (when (and mutated (not (equal? mutated canonical)))
      (note! "texts the control changes")
      (define payloads-kept? (equal? (readings original prefix) (readings mutated prefix)))
      (tally! "rule 8 on its output" payloads-kept? (lambda () (describe trial text)))
      (write-text! directory text mutated)
      (define observed (observe-reading execute state text))
      (define meaning-kept?
        (for/and
         (((index seen) (in-hash observed)))
         (equal? seen (hash-ref (prepared-baseline state) index))))
      (same!
       "L1 on its output"
       (prepared-baseline state)
       observed
       (lambda (index) (describe trial text)))
      (unless (and payloads-kept? meaning-kept?) (set! caught (cons (trial-id trial) caught)))
      (write-text! directory text original))))
  (remove-duplicates (reverse caught)))

(define (measure-baseline-identity execute-base execute trials work)
  (define shapes
    (list
     (lambda (arguments) arguments)
     (lambda (arguments) (remove "--json" arguments))
     (lambda (arguments) (append (remove "--json" arguments) '("--sarif")))
     (lambda (arguments) (append (remove "--json" arguments) '("--lsp")))
     (lambda (arguments) (append (remove "--json" arguments) '("--github-annotations")))))
  (for
   ((trial (in-list trials)))
   (define directory (working-copy trial work))
   (define all-shapes? (not (equal? (trial-id trial) "ask-gate")))
   (for*
    ((arguments (in-list (trial-invocations trial)))
     (shape (in-list (if all-shapes? shapes (list (car shapes))))))
    (define given (shape arguments))
    (tally!
     "M7 check, tree, plan, ask and emit: the same bytes and status as the base"
     (equal? (run execute-base directory given) (run execute directory given))
     (lambda () (describe trial '() (format "~s" given))))))
  (define directory (working-copy (car (control-trials)) work))
  (for
   ((given
     (in-list
      (list
       '()
       '("check")
       '("check" "catalog-language.typify" "Catalog")
       '("foo")
       '("format" "catalog-language.typify")
       '("check" "catalog-language.typify" "--write")
       '("check" "catalog-language.typify" "--check")
       '("--write" "check" "catalog-language.typify")
       '("check" "catalog-language.typify" "--write" "--bogus")
       '("check" "catalog-language.typify" "--json" "--write")
       '("check" "catalog-language.typify" "--sarif" "--check")
       '("tree" "catalog-language.typify" "." "--write")
       '("plan" "catalog-language.typify" "Catalog" "catalog.typify" "--check")
       '("ask" "catalog-language.typify" "Catalog" "item" "--write")
       '("emit" "Typify" "--check")
       '("emit" "Typify")
       '("check" "--" "catalog-language.typify")
       '("check" "catalog-language.typify" "--facts")
       '("check" "catalog-language.typify" "--json" "--sarif")
       '("check" "missing.typify" "--json")
       '("check" "missing.typify")))))
   (tally!
    "M7 command lines outside fmt: the same bytes and status as the base"
    (equal? (run execute-base directory given) (run execute directory given))
    (lambda () (format "~s" given))))
  (define base-version (run execute-base directory '("--version")))
  (define version (run execute directory '("--version")))
  (note!
   (format
    "M7 --version, which carries the source digest, base ~s and branch ~s"
    (string-trim (bytes->string/utf-8 (observation-stdout base-version)))
    (string-trim (bytes->string/utf-8 (observation-stdout version))))))

(define (print-table title table notes)
  (printf "~a\n" title)
  (for
   ((key (in-list (sort (hash-keys notes) string<?))))
   (printf "  note  ~a: ~a\n" key (hash-ref notes key)))
  (define pass?
    (for/and
     ((key (in-list (sort (hash-keys table) string<?))))
     (define entry (hash-ref table key))
     (define held? (= (vector-ref entry 0) (vector-ref entry 1)))
     (printf
      "  ~a  ~a: run ~a, held ~a\n"
      (if held? "pass" "FAIL")
      key
      (vector-ref entry 0)
      (vector-ref entry 1))
     (for ((detail (in-list (vector-ref entry 2)))) (printf "        ~a\n" detail))
     held?))
  (and pass? (positive? (hash-count table))))

(define (measure-in title thunk)
  (define table (make-hash))
  (define notes (make-hash))
  (parameterize ((current-table table) (current-notes notes)) (thunk))
  (print-table title table notes))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (define (option name)
   (define tail (member name arguments))
   (and tail (pair? (cdr tail)) (cadr tail)))
 (unless (for/and
          ((item (in-list arguments)) (index (in-naturals)))
          (or (member item '("--work" "--base" "--archive" "--only" "--mark" "--all-ask-words"))
              (and (positive? index)
                   (member
                    (list-ref arguments (sub1 index))
                    '("--work" "--base" "--archive" "--only" "--mark"))
                   (or (not (equal? (list-ref arguments (sub1 index)) "--mark"))
                       (member item '("M1" "M4" "M6" "M7"))))))
   (displayln
    "usage: racket scripts/fmt-marks.rkt [--work DIR] [--base CHECKOUT] [--archive FILE] [--all-ask-words] [--only PREFIX]... [--mark M1|M4|M6|M7]..."
    (current-error-port))
   (exit 2))
 (define work
   (path->complete-path (or (option "--work") (make-temporary-directory "typify-fmt-marks-~a"))))
 (define base (option "--base"))
 (define archive (option "--archive"))
 (define all-ask-words? (and (member "--all-ask-words" arguments) #t))
 (define prefixes
   (for/list
    ((item (in-list arguments)) (index (in-naturals)) #:when (equal? item "--only"))
    (list-ref arguments (add1 index))))
 (define marks-chosen
   (for/list
    ((item (in-list arguments)) (index (in-naturals)) #:when (equal? item "--mark"))
    (list-ref arguments (add1 index))))
 (define (mark? name) (or (null? marks-chosen) (and (member name marks-chosen) #t)))
 (define (not-chosen title) (printf "~a: not run, not chosen\n" title) #t)
 (define (chosen trials)
   (if (null? prefixes)
     trials
     (filter
      (lambda (trial) (ormap (lambda (prefix) (string-prefix? (trial-id trial) prefix)) prefixes))
      trials)))
 (define corpus (chosen (corpus-trials)))
 (define controls (chosen (control-trials)))
 (define refusal-controls (chosen (blank-prefix-trials)))
 (define gate (chosen (list (ask-gate-trial all-ask-words?))))
 (printf
  "ask gate: ~a\n"
  (if all-ask-words?
    "all words, the text as it stands"
    "the witness and unknown words, the text as it stands and its variants"))
 (printf "work: ~a\n" work)
 (define marks-pass
   (list
    (if (not (mark? "M1"))
      (not-chosen "M1 to M3")
      (let ((cases (box '())))
        (define measured
          (parameterize
           ((current-cases cases))
           (measure-in
            "M1 to M3: the corpus, the ask gate and the controls"
            (lambda ()
              (for
               ((trial (in-list (append corpus controls))))
               (measure-trial execute-command trial (build-path work "m1")))
              (for
               ((trial (in-list gate)))
               (measure-trial
                execute-command
                trial
                (build-path work "m1")
                #:variants?
                (not all-ask-words?)))
              (measure-controls execute-command (build-path work "m2"))
              (measure-refusals execute-command (build-path work "m3"))))))
        (and (print-cases (reverse (unbox cases))) measured)))
    (if (mark? "M4")
      (measure-in
       "M4: the self-declaration"
       (lambda () (measure-self execute-command (build-path work "m4") archive)))
      (not-chosen "M4"))
    (if (not (mark? "M6"))
      (not-chosen "M6")
      (andmap
       values
       (for/list
        ((mutant (in-list mutants)))
        (define caught #f)
        (define table (make-hash))
        (define notes (make-hash))
        (parameterize
         ((current-table table) (current-notes notes))
         (set! caught
           (measure-mutant
            execute-command
            (cdr mutant)
            (append corpus controls refusal-controls)
            (build-path work "m6"))))
        (printf "M6 negative control, a formatter that ~a\n" (car mutant))
        (for
         ((key (in-list (sort (hash-keys notes) string<?))))
         (printf "  note  ~a: ~a\n" key (hash-ref notes key)))
        (for
         ((key (in-list (sort (hash-keys table) string<?))))
         (define entry (hash-ref table key))
         (printf "        ~a: run ~a, held ~a\n" key (vector-ref entry 0) (vector-ref entry 1)))
        (printf
         "  ~a  caught in: ~a\n"
         (if (pair? caught) "pass" "FAIL")
         (if (pair? caught) (string-join caught ", ") "nothing"))
        (pair? caught))))
    (cond
     ((not (mark? "M7")) (not-chosen "M7"))
     (base
      (define execute-base
        (dynamic-require (path->complete-path (build-path base "src" "main.rkt")) 'execute-command))
      (measure-in
       "M7: the five commands against the base"
       (lambda ()
         (measure-baseline-identity
          execute-base
          execute-command
          (append corpus controls gate)
          (build-path work "m7")))))
     (else (printf "M7: not run, no --base\n") #t))))
 (define pass? (andmap values marks-pass))
 (printf
  "fmt marks: ~a~a\n"
  (if pass? "pass" "FAIL")
  (if (null? marks-chosen) "" (format ", chosen marks only: ~a" (string-join marks-chosen " "))))
 (exit (if pass? 0 1)))
