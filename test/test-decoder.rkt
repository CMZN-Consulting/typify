#lang racket/base

(require json
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         "../src/diagnostics.rkt"
         "../src/emission.rkt"
         "../src/language.rkt")

(provide run-tests parse-gbnf gbnf-accepts?)

(struct reference (name) #:transparent)

(struct terminal (text) #:transparent)

(struct character-set (ranges negated?) #:transparent)

(struct production (name symbols) #:transparent)

(struct grammar (productions by-name nullable start) #:transparent)

(struct item (production dot origin) #:transparent)

(define-runtime-path corpus-path "decoder-corpus.json")

(define decoder-revision "fd27b2034905f8dadd8bd6d2cc44595f4929d391")

(define (oracle-error message) (error 'independent-gbnf "~a" message))

(define (parse-expression source)
  (define index 0)
  (define size (string-length source))
  (define (peek) (and (< index size) (string-ref source index)))
  (define (take!)
    (unless (peek) (oracle-error "unexpected end of grammar expression"))
    (begin0 (peek) (set! index (add1 index))))
  (define (skip!)
    (let loop ()
      (cond
       ((and (peek) (char-whitespace? (peek))) (take!) (loop))
       ((equal? (peek) #\#) (set! index size)))))
  (define (hex! width)
    (unless (<= (+ index width) size) (oracle-error "truncated hexadecimal escape"))
    (define digits (substring source index (+ index width)))
    (unless (for/and
             ((char (in-string digits)))
             (or (char<=? #\0 char #\9) (char<=? #\a char #\f) (char<=? #\A char #\F)))
      (oracle-error "invalid hexadecimal escape"))
    (set! index (+ index width))
    (string->number digits 16))
  (define (point!)
    (define char (take!))
    (if (char=? char #\\)
      (case (take!)
        ((#\n) 10)
        ((#\r) 13)
        ((#\t) 9)
        ((#\x) (hex! 2))
        ((#\u) (hex! 4))
        ((#\U) (hex! 8))
        ((#\\) 92)
        ((#\") 34)
        ((#\[) 91)
        ((#\]) 93)
        ((#\-) 45)
        ((#\^) 94)
        (else (oracle-error "unsupported escaped character")))
      (char->integer char)))
  (define (quoted!)
    (take!)
    (define out (open-output-string))
    (let loop ()
      (cond
       ((not (peek)) (oracle-error "unclosed quoted literal"))
       ((char=? (peek) #\") (take!) (terminal (get-output-string out)))
       (else
        (define point (point!))
        (unless (or (<= 0 point 55295) (<= 57344 point 1114111))
          (oracle-error "quoted literal is not a Unicode scalar value"))
        (write-char (integer->char point) out)
        (loop)))))
  (define (charset!)
    (take!)
    (define negated? (equal? (peek) #\^))
    (when negated? (take!))
    (let loop ((ranges '()))
      (cond
       ((not (peek)) (oracle-error "unclosed character set"))
       ((equal? (peek) #\])
        (take!)
        (when (null? ranges) (oracle-error "empty character set"))
        (character-set (reverse ranges) negated?))
       (else
        (define lower (point!))
        (define upper
          (if (and (equal? (peek) #\-)
                   (< (add1 index) size)
                   (not (char=? (string-ref source (add1 index)) #\])))
            (begin (take!) (point!))
            lower))
        (unless (<= 0 lower upper 1114111) (oracle-error "invalid character range"))
        (loop (cons (cons lower upper) ranges))))))
  (define (name-char? char)
    (and char
         (or (char<=? #\a char #\z)
             (char<=? #\A char #\Z)
             (char<=? #\0 char #\9)
             (char=? char #\-))))
  (define (number!)
    (define start index)
    (let loop () (when (and (peek) (char<=? #\0 (peek) #\9)) (take!) (loop)))
    (when (= start index) (oracle-error "expected repetition bound"))
    (string->number (substring source start index)))
  (define (atom!)
    (skip!)
    (define base
      (cond
       ((equal? (peek) #\") (quoted!))
       ((equal? (peek) #\[) (charset!))
       ((equal? (peek) #\.) (take!) (character-set '((0 . 1114111)) #f))
       ((equal? (peek) #\()
        (take!)
        (define expression (choice!))
        (skip!)
        (unless (equal? (take!) #\)) (oracle-error "expected closing group"))
        expression)
       ((name-char? (peek))
        (define start index)
        (let loop () (when (name-char? (peek)) (take!) (loop)))
        (reference (substring source start index)))
       (else (oracle-error "unsupported GBNF atom"))))
    (skip!)
    (case (peek)
      ((#\?) (take!) (list 'repeat base 0 1))
      ((#\*) (take!) (list 'repeat base 0 #f))
      ((#\+) (take!) (list 'repeat base 1 #f))
      ((#\{)
       (take!)
       (skip!)
       (define lower (number!))
       (skip!)
       (define upper
         (if (equal? (peek) #\,)
           (begin (take!) (skip!) (if (equal? (peek) #\}) #f (number!)))
           lower))
       (skip!)
       (unless (equal? (take!) #\}) (oracle-error "expected closing repetition bound"))
       (when (or (> lower 1024) (and upper (or (< upper lower) (> upper 1024))))
         (oracle-error "repetition outside the internal oracle's declared bound1024"))
       (list 'repeat base lower upper))
      (else base)))
  (define (sequence!)
    (let loop ((atoms '()))
      (skip!)
      (if (or (not (peek)) (memv (peek) '(#\| #\))))
        (list 'sequence (reverse atoms))
        (loop (cons (atom!) atoms)))))
  (define (choice!)
    (let loop ((alternatives (list (sequence!))))
      (skip!)
      (if (equal? (peek) #\|)
        (begin (take!) (loop (cons (sequence!) alternatives)))
        (list 'choice (reverse alternatives)))))
  (define expression (choice!))
  (skip!)
  (when (peek) (oracle-error "unconsumed grammar expression"))
  expression)

(define (parse-gbnf source)
  (define definitions (make-hash))
  (for
   ((raw (in-list (string-split source "\n" #:trim? #f))))
   (define line (string-trim raw))
   (unless (or (equal? line "") (string-prefix? line "#"))
     (define header (regexp-match #px"^([A-Za-z0-9-]+)[ \t]*::=[ \t]*(.*)$" line))
     (unless header (oracle-error "expected one complete rule definition per line"))
     (define name (cadr header))
     (when (hash-has-key? definitions name) (oracle-error "duplicate grammar rule"))
     (hash-set! definitions name (parse-expression (caddr header)))))
  (unless (hash-has-key? definitions "root") (oracle-error "missing root rule"))
  (define rows '())
  (define next-name 0)
  (define (fresh) (set! next-name (add1 next-name)) (format "@oracle-~a" next-name))
  (define (add! name symbols) (set! rows (cons (production name (list->vector symbols)) rows)))
  (define (symbol! expression)
    (cond
     ((and (terminal? expression) (equal? (terminal-text expression) "")) #f)
     ((or (reference? expression) (terminal? expression) (character-set? expression)) expression)
     (else (define name (fresh)) (expand! name expression) (reference name))))
  (define (rhs! expression)
    (if (and (pair? expression) (eq? (car expression) 'sequence))
      (filter values (map symbol! (cadr expression)))
      (filter values (list (symbol! expression)))))
  (define (expand! name expression)
    (cond
     ((and (pair? expression) (eq? (car expression) 'choice))
      (for ((alternative (in-list (cadr expression)))) (add! name (rhs! alternative))))
     ((and (pair? expression) (eq? (car expression) 'repeat))
      (define unit (symbol! (cadr expression)))
      (define lower (caddr expression))
      (define upper (cadddr expression))
      (cond
       ((not unit) (add! name '()))
       (upper (for ((width (in-range lower (add1 upper)))) (add! name (make-list width unit))))
       (else
        (define tail (fresh))
        (add! tail '())
        (add! tail (list unit (reference tail)))
        (add! name (append (make-list lower unit) (list (reference tail)))))))
     (else (add! name (rhs! expression)))))
  (for
   ((name (in-list (sort (hash-keys definitions) string<?))))
   (expand! name (hash-ref definitions name)))
  (define accepted-name (fresh))
  (add! accepted-name (list (reference "root")))
  (define productions (list->vector (reverse rows)))
  (define by-name (make-hash))
  (for
   ((row (in-vector productions)) (index (in-naturals)))
   (hash-update! by-name (production-name row) (lambda (old) (cons index old)) '()))
  (for*
   ((row (in-vector productions)) (symbol (in-vector (production-symbols row))))
   (when (and (reference? symbol) (not (hash-has-key? by-name (reference-name symbol))))
     (oracle-error "undeclared grammar reference")))
  (define nullable (make-hash))
  (let loop ()
    (define changed? #f)
    (for
     ((row (in-vector productions)))
     (when (and
            (not (hash-has-key? nullable (production-name row)))
            (for/and
             ((symbol (in-vector (production-symbols row))))
             (and (reference? symbol) (hash-has-key? nullable (reference-name symbol)))))
       (hash-set! nullable (production-name row) #t)
       (set! changed? #t)))
    (when changed? (loop)))
  (grammar productions by-name nullable (sub1 (vector-length productions))))

(define (oracle-prefix? source text index)
  (and (<= (+ index (string-length text)) (string-length source))
       (for/and
        ((char (in-string text)) (position (in-naturals index)))
        (char=? char (string-ref source position)))))

(define (gbnf-accepts? compiled source)
  (define width (string-length source))
  (define charts (build-vector (add1 width) (lambda (_) (make-hash))))
  (define queues (make-vector (add1 width) '()))
  (define rows (grammar-productions compiled))
  (define (add! position state)
    (unless (hash-has-key? (vector-ref charts position) state)
      (hash-set! (vector-ref charts position) state #t)
      (vector-set! queues position (cons state (vector-ref queues position)))))
  (define (waiting state)
    (define symbols (production-symbols (vector-ref rows (item-production state))))
    (and (< (item-dot state) (vector-length symbols)) (vector-ref symbols (item-dot state))))
  (define (advance state)
    (item (item-production state) (add1 (item-dot state)) (item-origin state)))
  (add! 0 (item (grammar-start compiled) 0 0))
  (for
   ((position (in-range (add1 width))))
   (let loop ()
     (unless (null? (vector-ref queues position))
       (define state (car (vector-ref queues position)))
       (vector-set! queues position (cdr (vector-ref queues position)))
       (define symbol (waiting state))
       (cond
        ((not symbol)
         (define name (production-name (vector-ref rows (item-production state))))
         (for
          ((parent (in-list (hash-keys (vector-ref charts (item-origin state))))))
          (define wanted (waiting parent))
          (when (and (reference? wanted) (equal? (reference-name wanted) name))
            (add! position (advance parent)))))
        ((reference? symbol)
         (define name (reference-name symbol))
         (for
          ((rule (in-list (hash-ref (grammar-by-name compiled) name))))
          (add! position (item rule 0 position)))
         (when (hash-has-key? (grammar-nullable compiled) name) (add! position (advance state))))
        ((terminal? symbol)
         (when (oracle-prefix? source (terminal-text symbol) position)
           (add! (+ position (string-length (terminal-text symbol))) (advance state))))
        ((character-set? symbol)
         (when (< position width)
           (define point (char->integer (string-ref source position)))
           (define member?
             (for/or
              ((range (in-list (character-set-ranges symbol))))
              (<= (car range) point (cdr range))))
           (when (if (character-set-negated? symbol) (not member?) member?)
             (add! (add1 position) (advance state))))))
       (loop))))
  (hash-has-key? (vector-ref charts width) (item (grammar-start compiled) 1 0)))

(define (declaration family)
  (define slots (hash-ref family 'slots))
  (string-append
   (string-join
    (append
     (list
      "#lang typify"
      (string-append "declare : " (hash-ref family 'language))
      "version : 1"
      "cap : token-length = 32")
     (for/list
      ((entry (in-list (hash-ref family 'classes))))
      (string-append "is : " (car entry) " = " (cadr entry)))
     (list
      (string-append "primitive : record : " (string-join slots " "))
      (string-append "form : " (hash-ref family 'production))
      (string-append
       "does : record "
       (string-join (map (lambda (name) (string-append "<" name ">")) slots) " ")))
     (map (lambda (example) (string-append "example : " example)) (hash-ref family 'examples))
     (list "end!"))
    "\n")
   "\n"))

(define (generated-cases family generation)
  (define cases (make-hash))
  (define (add! text kind) (hash-update! cases text (lambda (previous) (cons kind previous)) '()))
  (for
   ((example (in-list (hash-ref family 'examples))))
   (add! example 'example)
   (add! (string-append " \t" example "\t ") 'outer-blanks)
   (for
    ((index (in-range (string-length example))))
    (add!
     (string-append (substring example 0 index) (substring example (add1 index)))
     'single-character-deletion)
    (for
     ((replacement (in-list (hash-ref generation 'single-character-replacements))))
     (add!
      (string-append (substring example 0 index) replacement (substring example (add1 index)))
      'single-character-replacement)))
   (for*
    ((index (in-range (add1 (string-length example))))
     (delimiter (in-list (hash-ref generation 'delimiter-insertions))))
    (add!
     (string-append (substring example 0 index) delimiter (substring example index))
     'delimiter-insertion)))
  (for ((text (in-list (hash-ref family 'refusals)))) (add! text 'refusal))
  (for ((text (in-list '("" "  " "# internal comment")))) (add! text 'ignored-line))
  cases)

(define (external-verdict executable grammar-path input-path text)
  (call-with-output-file input-path #:exists 'truncate (lambda (out) (display text out)))
  (define-values
   (process output input errors)
   (subprocess #f #f #f executable grammar-path input-path))
  (close-output-port input)
  (define output-result (make-channel))
  (define error-result (make-channel))
  (thread (lambda () (channel-put output-result (port->string output))))
  (thread (lambda () (channel-put error-result (port->string errors))))
  (define finished (sync/timeout 15 process))
  (unless finished (subprocess-kill process #t) (subprocess-wait process))
  (define stdout (channel-get output-result))
  (define stderr (channel-get error-result))
  (close-input-port output)
  (close-input-port errors)
  (unless finished (oracle-error "external validator timed out"))
  (unless (zero? (subprocess-status process))
    (oracle-error (format "external validator infrastructure failure: ~a" stderr)))
  (cond
   ((string-prefix? stdout "Input string is valid according to the grammar.\n") #t)
   ((string-prefix? stdout "Input string is invalid according to the grammar.\n") #f)
   (else (oracle-error "external validator returned no recognized semantic verdict"))))

(define (run-tests)
  (define checks 0)
  (define failures 0)
  (define total 0)
  (define delimiter-count 0)
  (define decoder-total 0)
  (define disagreements '())
  (define (check! actual expected context)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (set! failures (add1 failures))
      (when (< (length disagreements) 20)
        (set! disagreements
          (cons (hash 'context context 'actual actual 'expected expected) disagreements)))))
  (for
   ((canary
     (in-list
      (list
       (list "root ::= \"\"\n" "" #t)
       (list "root ::= \"\"\n" "x" #f)
       (list "root ::= \"a\" root | \"\"\n" "aaa" #t)
       (list "root ::= \"a\" root | \"\"\n" "aab" #f)
       (list "root ::= (\"a\"?)* \"b\"\n" "b" #t)
       (list "root ::= (\"a\"?)* \"b\"\n" "aab" #t)
       (list "root ::= [a-c]{2,3}\n" "abc" #t)
       (list "root ::= [a-c]{2,3}\n" "ad" #f)
       (list "root ::= \"\\U0001F600\" [\\u03B1-\\u03B3]+\n" "😀αβ" #t)
       (list "root ::= \"\\U0001F600\" [\\u03B1-\\u03B3]+\n" "😀δ" #f)
       (list "root ::= [^\\u0000-\\U0010FFFF]\n" "x" #f)
       (list "root ::= \"#\" # literal comment\n" "#" #t)))))
   (check!
    (gbnf-accepts? (parse-gbnf (car canary)) (cadr canary))
    (caddr canary)
    "independent oracle canary"))
  (define corpus (call-with-input-file corpus-path read-json))
  (define families (hash-ref corpus 'families))
  (define executable (getenv "TYPIFY_GBNF_VALIDATOR"))
  (define revision (getenv "TYPIFY_GBNF_VALIDATOR_REVISION"))
  (define available? (and executable (file-exists? executable) (equal? revision decoder-revision)))
  (unless available? (set! failures (add1 failures)))
  (define directory (make-temporary-file "typify-decoder-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (for
      ((family (in-list families)))
      (unless (and
               (>= (length (hash-ref family 'slots)) 2)
               (string-prefix? (hash-ref (hash-ref family 'reference) 'url) "https://"))
        (oracle-error "outside format lacks slots or official provenance"))
      (define body (hash-ref (read-declarations (declaration family)) (hash-ref family 'language)))
      (define emitted (language->gbnf body))
      (define oracle (parse-gbnf emitted))
      (define grammar-path (build-path directory "grammar.gbnf"))
      (define input-path (build-path directory "input.txt"))
      (call-with-output-file grammar-path #:exists 'truncate (lambda (out) (display emitted out)))
      (define cases (generated-cases family (hash-ref corpus 'generation)))
      (for
       ((text (in-list (sort (hash-keys cases) string<?))))
       (set! total (add1 total))
       (when (member 'delimiter-insertion (hash-ref cases text))
         (set! delimiter-count (add1 delimiter-count)))
       (define reader-accepts?
         (with-handlers ((exn:fail:typify? (lambda (_) #f))) (check-program text body) #t))
       (define oracle-accepts? (gbnf-accepts? oracle text))
       (define context (hash 'family (hash-ref family 'id) 'input text))
       (check! oracle-accepts? reader-accepts? context)
       (when (member 'example (hash-ref cases text)) (check! reader-accepts? #t context))
       (when (member 'refusal (hash-ref cases text)) (check! reader-accepts? #f context))
       (when available?
         (set! decoder-total (add1 decoder-total))
         (check!
          (external-verdict executable grammar-path input-path text)
          reader-accepts?
          context)))))
   (lambda () (delete-directory/files directory)))
  (define status (cond ((not available?) "pending") ((zero? failures) "passed") (else "failed")))
  (hash
   'suite
   "decoder"
   'checks
   checks
   'status
   status
   'gates
   (hash
    'decoder-differential
    (hash
     'status
     status
     'total
     total
     'failures
     failures
     'outside-formats
     (length families)
     'delimiter-insertions
     delimiter-count
     'external-total
     decoder-total
     'decoder-revision
     (or revision "unavailable")
     'required-decoder-revision
     decoder-revision))
   'details
   (reverse disagreements)))

(module+ main (displayln (canonical-json (run-tests))))
