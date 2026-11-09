#lang racket/base

(require racket/list
         racket/string
         "diagnostics.rkt"
         "language.rkt"
         "meta.rkt"
         "model.rkt"
         "near.rkt"
         "syntax.rkt")

(provide ask-language question-candidates question-answer-roundtrip?)

(define (row-kind row) (hash-ref row 'kind))

(define (row-captures row) (hash-ref row 'captures))

(define (row-name row)
  (define captures (row-captures row))
  (hash-ref captures "constructor" (lambda () (hash-ref captures "name" #f))))

(define (row-text row)
  (string-trim (line-record-raw (hash-ref row 'record)) #px"[ \t]" #:repeat? #t))

(define (row-byte row) (src-span-start-byte (line-record-span (hash-ref row 'record))))

(define (form-byte candidate) (src-span-start-byte (form-span candidate)))

(define (form-verb candidate)
  (and (memq (form-kind candidate) '(colon bang))
       (andmap string? (form-left candidate))
       (let ((verb (apply string-append (form-left candidate))))
         (and (positive? (string-length verb))
              (for/and ((character (in-string verb))) (not (char-whitespace? character)))
              verb))))

(define (ordered-constructors body)
  (sort (hash-values (language-constructors body)) < #:key constructor-ordinal))

(define (question-candidates body)
  (sort
   (remove-duplicates
    (append
     (filter values (map form-verb (language-forms body)))
     (map constructor-name (ordered-constructors body))))
   string<?))

(define (selected-forms body word)
  (filter
   (lambda (candidate)
     (or (equal? (form-verb candidate) word)
         (equal? (term-constructor (form-meaning candidate)) word)))
   (language-forms body)))

(define (selected-constructors body word forms)
  (define names (map (lambda (candidate) (term-constructor (form-meaning candidate))) forms))
  (filter
   (lambda (item) (or (equal? (constructor-name item) word) (member (constructor-name item) names)))
   (ordered-constructors body)))

(define (form-rows body)
  (define result (make-hash))
  (define current #f)
  (for
   ((row (in-list (language-rows body))))
   (when (eq? (row-kind row) 'form) (set! current (row-byte row)))
   (when current (hash-update! result current (lambda (rows) (append rows (list row))) '())))
  result)

(define (own-row body kind name)
  (for/first
   ((row (in-list (language-rows body)))
    #:when
    (and (eq? (row-kind row) kind) (equal? (row-name row) name)))
   row))

(define (direct-class-line body name)
  (define authored (own-row body 'class name))
  (cond
   (authored (row-text authored))
   ((hash-has-key? (current-meta-classes) name)
    (string-append "is : " name " = " (hash-ref (current-meta-classes) name)))
   (else #f)))

(define (expression-captures expression)
  (cond
   ((capture? expression) (list (capture-name expression)))
   ((term? expression) (append-map expression-captures (term-arguments expression)))
   (else '())))

(define (class-lines body names)
  (filter values (map (lambda (name) (direct-class-line body name)) (remove-duplicates names))))

(define (form-block candidate groups)
  (define rows (hash-ref groups (form-byte candidate)))
  (append
   (for/list ((row (in-list rows)) #:when (eq? (row-kind row) 'form)) (row-text row))
   (for/list ((row (in-list rows)) #:when (eq? (row-kind row) 'does)) (row-text row))
   (for/list ((row (in-list rows)) #:when (eq? (row-kind row) 'restricts)) (row-text row))))

(define (form-pairs body forms groups)
  (define file
    (for/first ((row (in-list (language-rows body))) #:when (eq? (row-kind row) 'file)) row))
  (for/fold
   ((lines '()))
   ((candidate (in-list forms)) (index (in-naturals)))
   (define pair (form-block candidate groups))
   (append
    lines
    (if (and (zero? index) file) (append (list (car pair) (row-text file)) (cdr pair)) pair))))

(define (forms-building body name)
  (filter
   (lambda (candidate) (equal? (term-constructor (form-meaning candidate)) name))
   (language-forms body)))

(define (form-value-rows body forms selector kind)
  (define bytes
    (append-map
     (lambda (candidate)
       (map (lambda (entry) (src-span-start-byte (cdr entry))) (selector candidate)))
     forms))
  (for/list
   ((row (in-list (language-rows body)))
    #:when
    (and (eq? (row-kind row) kind) (member (row-byte row) bytes)))
   (row-text row)))

(define (constructor-attributes body name)
  (for/list
   ((row (in-list (language-rows body)))
    #:when
    (and (memq (row-kind row) '(writes reads guard effect effect-state stage scope fuel fact))
         (equal? (row-name row) name)))
   (row-text row)))

(define (rule-name row)
  (define text (hash-ref (row-captures row) "term"))
  (car (string-split text)))

(define (constructor-block body item forms)
  (define name (constructor-name item))
  (define rows (language-rows body))
  (define declaration
    (for/first
     ((row (in-list rows))
      #:when
      (and (memq (row-kind row) '(term term-zero primitive primitive-zero))
           (equal? (row-name row) name)))
     (row-text row)))
  (define building
    (filter (lambda (candidate) (equal? (term-constructor (form-meaning candidate)) name)) forms))
  (append
   (list declaration)
   (class-lines
    body
    (if (null? building)
      (constructor-signature item)
      (append-map (lambda (candidate) (expression-captures (form-meaning candidate))) building)))
   (constructor-attributes body name)
   (for/list
    ((row (in-list rows)) #:when (and (eq? (row-kind row) 'rule) (equal? (rule-name row) name)))
    (row-text row))
   (for/list
    ((row (in-list rows)) #:when (and (eq? (row-kind row) 'ensure) (equal? (row-name row) name)))
    (row-text row))))

(define (constructor-tail body item)
  (define name (constructor-name item))
  (define count-rows
    (for/list
     ((row (in-list (language-rows body)))
      #:when
      (and (memq (row-kind row) '(count count-keyed)) (equal? (row-name row) name)))
     row))
  (define (unkeyed? row)
    (or (eq? (row-kind row) 'count) (zero? (string->number (hash-ref (row-captures row) "width")))))
  (define unkeyed (filter unkeyed? count-rows))
  (define keyed (filter (lambda (row) (not (unkeyed? row))) count-rows))
  (define defaults (form-value-rows body (forms-building body name) form-defaults 'default))
  (define missing
    (cond
     ((pair? defaults) "default")
     ((member (hash-ref (language-counts body) name "*") '("1" "1..*")) "refused")
     (else "empty")))
  (append
   (if (null? unkeyed) (list (string-append "count : " name " = *")) (map row-text unkeyed))
   (map row-text keyed)
   (list (string-append "missing : " missing))
   defaults))

(define (constructor-answer body forms items)
  (define groups (form-rows body))
  (define pairs (form-pairs body forms groups))
  (define blocks (append-map (lambda (item) (constructor-block body item forms)) items))
  (define file
    (and (null? forms)
         (for/first
          ((row (in-list (language-rows body))) #:when (eq? (row-kind row) 'file))
          (row-text row))))
  (append
   pairs
   (if file (append (list (car blocks) file) (cdr blocks)) blocks)
   (form-value-rows body forms form-examples 'example)
   (append-map (lambda (item) (constructor-tail body item)) items)
   (append-map
    (lambda (item)
      (for/list
       ((row (in-list (language-rows body)))
        #:when
        (and (eq? (row-kind row) 'note) (equal? (row-name row) (constructor-name item))))
       (row-text row)))
    items)))

(define (cell-answer body row)
  (define name (row-name row))
  (define cell (hash-ref (compiled-model-cells (language-model body)) name))
  (define table (model-cell-table cell))
  (cons
   (row-text row)
   (if table
     (for/list
      ((entry (in-list (language-rows body)))
       #:when
       (and (eq? (row-kind entry) 'join) (equal? (row-name entry) table)))
      (row-text entry))
     '())))

(define (declared-answer body word forms items)
  (define class-row (own-row body 'class word))
  (define cell-row (own-row body 'cell word))
  (and (or (pair? items) class-row cell-row)
       (for/fold
        ((lines (if (pair? items) (constructor-answer body forms items) '())))
        ((line
          (in-list
           (append
            (if class-row (list (row-text class-row)) '())
            (if cell-row (cell-answer body cell-row) '())))))
        (if (member line lines) lines (append lines (list line))))))

(define (ask-language body word #:diagnostics (diagnostics '()))
  (raise-diagnostics diagnostics)
  (define forms (selected-forms body word))
  (define items (selected-constructors body word forms))
  (define declared (declared-answer body word forms items))
  (unless (or declared (verb-word? word))
    (raise-diagnostics
     (append
      diagnostics
      (list
       (make-diagnostic
        "E_USAGE"
        "The asked word must be declared or have a verb's spelling"
        #:data
        (hash 'actual word))))))
  (define near (if declared '() (near-words word (question-candidates body))))
  (hash
   'word
   word
   'status
   (if declared "declared" "undeclared")
   'lines
   (if declared
     (map (lambda (line) (string-append "? : " line)) declared)
     (cons
      (string-append "? : " word " = undeclared")
      (map (lambda (candidate) (string-append "? : near : " candidate)) near)))
   'near
   near
   'accepts
   '()
   'diagnostics
   (map diagnostic->jsexpr (diagnostics-sort diagnostics))))

(define (question-answer-roundtrip? answer)
  (with-handlers
   ((exn:fail? (lambda (_) #f)))
   (define word (hash-ref answer 'word))
   (define status (hash-ref answer 'status))
   (define lines (hash-ref answer 'lines))
   (define near (hash-ref answer 'near))
   (and (string? word)
        (member status '("declared" "undeclared"))
        (list? lines)
        (andmap string? lines)
        (list? near)
        (andmap string? near)
        (equal? (hash-ref answer 'accepts) '())
        (list? (hash-ref answer 'diagnostics))
        (for/and
         ((line (in-list lines)))
         (define records (read-lines line #:comment-prefix ""))
         (and (= (length records) 1)
              (eq? (line-record-kind (car records)) 'colon)
              (equal? (line-record-left (car records)) "?")))
        (if (equal? status "declared")
          (and (pair? lines) (null? near))
          (equal?
           lines
           (cons
            (string-append "? : " word " = undeclared")
            (map (lambda (candidate) (string-append "? : near : " candidate)) near)))))))
