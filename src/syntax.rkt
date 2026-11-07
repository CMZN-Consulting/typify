#lang racket/base

(require racket/list racket/string "diagnostics.rkt")

(provide (all-from-out "diagnostics.rkt")
         (struct-out line-record)
         (struct-out slot)
         (struct-out term)
         (struct-out capture)
         (struct-out literal)
         read-lines
         span-slice
         line-left-span
         line-right-span
         parse-template
         match-template
         parse-term
         instantiate-term)

(struct line-record (kind left right span raw) #:transparent)

(struct slot (name span) #:transparent)

(struct term (constructor arguments span) #:transparent)

(struct capture (name span) #:transparent)

(struct literal (value span) #:transparent)

(define (blank? char) (or (char=? char #\space) (char=? char #\tab)))

(define (trim-bounds text (start 0) (end (string-length text)))
  (define left
    (let loop ((index start))
      (if (and (< index end) (blank? (string-ref text index))) (loop (add1 index)) index)))
  (define right
    (let loop ((index end))
      (if (and (> index left) (blank? (string-ref text (sub1 index)))) (loop (sub1 index)) index)))
  (values left right))

(define (trim text) (define-values (start end) (trim-bounds text)) (substring text start end))

(define (span-slice span text start (end (string-length text)))
  (unless (<= 0 start end (string-length text))
    (raise-arguments-error 'span-slice "invalid source slice" "start" start "end" end))
  (define (position limit)
    (for/fold
     ((line (src-span-start-line span))
      (column (src-span-start-column span))
      (byte (src-span-start-byte span))
      (after-cr? #f)
      #:result
      (list line column byte))
     ((char (in-string text 0 limit)))
     (define next-byte (+ byte (bytes-length (string->bytes/utf-8 (string char)))))
     (cond
      ((char=? char #\return) (values (add1 line) 1 next-byte #t))
      ((char=? char #\newline) (values (if after-cr? line (add1 line)) 1 next-byte #f))
      (else (values line (add1 column) next-byte #f)))))
  (define left (position start))
  (define right (position end))
  (src-span
   (src-span-file span)
   (car left)
   (cadr left)
   (car right)
   (cadr right)
   (caddr left)
   (caddr right)))

(define (base-span source file span)
  (or span (src-span file 1 1 1 1 0 (bytes-length (string->bytes/utf-8 source)))))

(define (starts-at? source literal start)
  (and (<= (+ start (string-length literal)) (string-length source))
       (for/and
        ((char (in-string literal)) (index (in-naturals start)))
        (char=? char (string-ref source index)))))

(define (find-literal source literal (start 0))
  (for/first
   ((index (in-range start (add1 (- (string-length source) (string-length literal)))))
    #:when
    (starts-at? source literal index))
   index))

(define (classify raw line byte file prefix)
  (define-values (start end) (trim-bounds raw))
  (define source (substring raw start end))
  (define anchor (src-span file line 1 line 1 byte byte))
  (define span (span-slice anchor raw start end))
  (define colon (find-literal source ":"))
  (define arrow (find-literal source "->"))
  (cond
   ((string=? source "") (line-record 'blank "" "" span raw))
   ((and prefix (not (string=? prefix "")) (string-prefix? source prefix))
    (line-record 'comment "" "" span raw))
   (colon
    (line-record
     'colon
     (trim (substring source 0 colon))
     (trim (substring source (add1 colon)))
     span
     raw))
   (arrow
    (line-record
     'arrow
     (trim (substring source 0 arrow))
     (trim (substring source (+ arrow 2)))
     span
     raw))
   ((and (string-suffix? source "!")
         (> (string-length source) 1)
         (for/and
          ((char (in-string source 0 (sub1 (string-length source)))))
          (not (or (char-whitespace? char) (char=? char #\!)))))
    (line-record 'bang (substring source 0 (sub1 (string-length source))) "" span raw))
   (else (line-record 'unclassified source "" span raw))))

(define (read-lines source #:file (file "<input>") #:comment-prefix (prefix "#"))
  (define normalized-file (string-replace (if (path? file) (path->string file) file) "\\" "/"))
  (let loop ((start 0) (line 1) (byte 0) (records '()))
    (define ending
      (for/first
       ((index (in-range start (string-length source)))
        #:when
        (memv (string-ref source index) '(#\return #\newline)))
       index))
    (define end (or ending (string-length source)))
    (define raw (substring source start end))
    (define entry (classify raw line byte normalized-file prefix))
    (if ending
      (let ((next
             (if (and (char=? (string-ref source end) #\return)
                      (< (add1 end) (string-length source))
                      (char=? (string-ref source (add1 end)) #\newline))
               (+ end 2)
               (add1 end))))
        (loop
         next
         (add1 line)
         (+ byte (bytes-length (string->bytes/utf-8 (substring source start next))))
         (cons entry records)))
      (reverse (cons entry records)))))

(define (line-part-span record right?)
  (define raw (trim (line-record-raw record)))
  (define kind (line-record-kind record))
  (define split
    (case kind ((colon) (find-literal raw ":")) ((arrow) (find-literal raw "->")) (else #f)))
  (define cut (if split (+ split (if (eq? kind 'arrow) 2 1)) (string-length raw)))
  (define-values
   (start end)
   (trim-bounds
    raw
    (if right? cut 0)
    (if right?
      (string-length raw)
      (or split (if (eq? kind 'bang) (sub1 (string-length raw)) (string-length raw))))))
  (span-slice (line-record-span record) raw start end))

(define (line-left-span record) (line-part-span record #f))

(define (line-right-span record) (line-part-span record #t))

(define (valid-name? name)
  (and (positive? (string-length name))
       (for/and
        ((char (in-string name)))
        (not (or (char-whitespace? char) (memv char '(#\< #\> #\( #\))))))))

(define (parse-template source #:file (file "<input>") #:span (provided-span #f))
  (define span (base-span source file provided-span))
  (define (failure code message start end)
    (raise-typify code message #:span (span-slice span source start end) #:content source))
  (let loop ((index 0) (literal-start 0) (parts '()) (names '()))
    (cond
     ((= index (string-length source))
      (reverse
       (if (< literal-start index) (cons (substring source literal-start index) parts) parts)))
     ((char=? (string-ref source index) #\<)
      (define close (find-literal source ">" (add1 index)))
      (unless close (failure "E_TEMPLATE" "Unclosed placeholder" index (string-length source)))
      (define name (substring source (add1 index) close))
      (unless (valid-name? name)
        (failure "E_TEMPLATE" "Invalid placeholder name" index (add1 close)))
      (when (member name names)
        (failure
         "E_TEMPLATE_DUPLICATE"
         "A placeholder name occurs more than once"
         index
         (add1 close)))
      (when (and (= literal-start index) (pair? parts) (slot? (car parts)))
        (failure
         "E_TEMPLATE_ADJACENT"
         "Adjacent placeholders have no literal boundary"
         index
         (add1 close)))
      (define before
        (if (< literal-start index) (cons (substring source literal-start index) parts) parts))
      (loop
       (add1 close)
       (add1 close)
       (cons (slot name (span-slice span source index (add1 close))) before)
       (cons name names)))
     (else (loop (add1 index) literal-start parts names)))))

(define (match-template parts source)
  (let loop ((remaining parts) (index 0) (captures (hash)))
    (cond
     ((null? remaining) (and (= index (string-length source)) captures))
     ((string? (car remaining))
      (and (starts-at? source (car remaining) index)
           (loop (cdr remaining) (+ index (string-length (car remaining))) captures)))
     ((slot? (car remaining))
      (define following (cdr remaining))
      (define end
        (cond
         ((null? following) (string-length source))
         ((string? (car following)) (find-literal source (car following) index))
         (else #f)))
      (and end
           (loop
            following
            end
            (hash-set captures (slot-name (car remaining)) (substring source index end)))))
     (else (raise-argument-error 'match-template "list of literal strings and slots" parts)))))

(define (parse-term source #:file (file "<input>") #:span (provided-span #f))
  (define span (base-span source file provided-span))
  (define index 0)
  (define size (string-length source))
  (define (at-end?) (= index size))
  (define (failure message (start index) (end (min size (add1 start))))
    (raise-typify "E_TERM" message #:span (span-slice span source start end) #:content source))
  (define (skip!)
    (define before index)
    (let loop ()
      (when (and (not (at-end?)) (blank? (string-ref source index)))
        (set! index (add1 index))
        (loop)))
    (> index before))
  (define (word!)
    (define start index)
    (let loop ()
      (when (and
             (not (at-end?))
             (not
              (or (char-whitespace? (string-ref source index))
                  (memv (string-ref source index) '(#\( #\) #\< #\>)))))
        (set! index (add1 index))
        (loop)))
    (when (= start index) (failure "Expected a literal word"))
    (values (substring source start index) start index))
  (define (argument!)
    (define start index)
    (case (string-ref source index)
      ((#\<)
       (define close (find-literal source ">" (add1 index)))
       (unless close (failure "Unclosed capture" index size))
       (define name (substring source (add1 index) close))
       (unless (valid-name? name) (failure "Invalid capture name" index (add1 close)))
       (set! index (add1 close))
       (capture name (span-slice span source start index)))
      ((#\() (parenthesized!))
      (else
       (define-values (value begin end) (word!))
       (literal value (span-slice span source begin end)))))
  (define (body! nested? start)
    (skip!)
    (when (at-end?) (failure "Expected a term constructor"))
    (define-values (name begin end) (word!))
    (let loop ((arguments '()))
      (define token-end index)
      (define separated? (skip!))
      (cond
       ((at-end?)
        (when nested? (failure "Unclosed parenthesized term" start size))
        (term name (reverse arguments) (span-slice span source start token-end)))
       ((char=? (string-ref source index) #\))
        (unless nested? (failure "Unexpected closing parenthesis"))
        (set! index (add1 index))
        (term name (reverse arguments) (span-slice span source start index)))
       (else
        (unless separated? (failure "Term arguments must be separated by blanks"))
        (loop (cons (argument!) arguments))))))
  (define (parenthesized!) (define start index) (set! index (add1 index)) (body! #t start))
  (skip!)
  (when (at-end?) (failure "Expected a term"))
  (define result (if (char=? (string-ref source index) #\() (parenthesized!) (body! #f index)))
  (skip!)
  (unless (at-end?) (failure "Unexpected input after term"))
  result)

(define (instantiate-term expression captures)
  (define (argument item)
    (cond
     ((term? item) (instantiate-term item captures))
     ((literal? item) (literal-value item))
     ((capture? item)
      (hash-ref
       captures
       (capture-name item)
       (lambda ()
         (raise-typify
          "E_CAPTURE"
          "No value was supplied for a capture"
          #:span
          (capture-span item)
          #:data
          (hasheq 'capture (capture-name item))))))
     (else (raise-argument-error 'instantiate-term "term, literal or capture" item))))
  (cons (term-constructor expression) (map argument (term-arguments expression))))
