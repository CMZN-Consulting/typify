#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/string
         "classes.rkt"
         "diagnostics.rkt"
         "facts.rkt"
         "language.rkt"
         "syntax.rkt")

(provide discover-tree read-tree read-tree/diagnostics tree->jsexpr tree-facts-required?)

(define (portable path)
  (define text (path->string path))
  (if (eq? (system-type 'os) 'windows) (string-replace text "\\" "/") text))

(define (file-error path)
  (define name (if (path? path) (portable path) path))
  (raise-typify
   "E_FILE"
   "The tree input cannot be read without following a link"
   #:span
   (src-span name 1 1 1 1 0 0)
   #:data
   (hash 'path name)))

(define (safe-component? text)
  (and (positive? (string-length text))
       (not (member text '("." "..")))
       (not (string-contains? text "/"))
       (not (string-contains? text "\\"))
       (relative-path? (string->path text))))

(define (template-error body actual)
  (define record
    (for/first
     ((row (in-list (language-rows body))) #:when (eq? (hash-ref row 'kind) 'file))
     (hash-ref row 'record)))
  (raise-typify
   "E_DECLARATION"
   "A tree file template must be a relative path with portable components"
   #:span
   (if record (line-record-span record) (language-span body))
   #:data
   (hash 'kind "syntax" 'actual (if record (string-trim (line-record-raw record)) actual))))

(define (body-order left right)
  (define a (language-span left))
  (define b (language-span right))
  (or (< (src-span-start-line a) (src-span-start-line b))
      (and (= (src-span-start-line a) (src-span-start-line b))
           (string<? (language-name left) (language-name right)))))

(define (tree-facts-required? declarations)
  (and (for/or
        ((body (in-hash-values declarations)))
        (for/or ((row (in-list (language-rows body)))) (memq (hash-ref row 'kind) '(guard reads))))
       #t))

(define (discover-tree declarations given-root)
  (define root (simplify-path (path->complete-path given-root) #f))
  (unless (and (directory-exists? root) (not (link-exists? root))) (file-error root))
  (define root-parts (explode-path root))
  (define records '())
  (define bodies (sort (filter language-file (hash-values declarations)) body-order))
  (define ordinals (for/hasheq ((body (in-list bodies)) (index (in-naturals))) (values body index)))
  (define (located components)
    (define path (apply build-path root components))
    (define parts (explode-path (simplify-path path #f)))
    (unless (and
             (<= (length root-parts) (length parts))
             (equal? root-parts (take parts (length root-parts))))
      (file-error path))
    path)
  (define (path-type path)
    (with-handlers
     ((exn:fail:filesystem? (lambda (_) (file-error path))))
     (when (link-exists? path) (file-error path))
     (file-or-directory-type path #f)))
  (define (entries components)
    (define path (located components))
    (if (eq? (path-type path) 'directory)
      (with-handlers
       ((exn:fail:filesystem? (lambda (_) (file-error path))))
       (sort (directory-list path) string<? #:key path->string))
      '()))
  (for
   ((body (in-list bodies)))
   (define before (length records))
   (define source (language-file body))
   (define components (string-split source "/" #:trim? #f))
   (unless (and (relative-path? (string->path source)) (andmap safe-component? components))
     (template-error body source))
   (define patterns
     (for/list
      ((component (in-list components)))
      (define pattern (parse-template component #:span (language-span body)))
      (for
       ((part (in-list pattern)) #:when (slot? part))
       (unless (hash-has-key? (language-classes body) (slot-name part))
         (raise-typify
          "E_DECLARATION"
          "A file template placeholder has no class"
          #:span
          (slot-span part)
          #:data
          (hash 'kind "capture" 'actual (string-append "<" (slot-name part) ">")))))
      pattern))
   (define (render pattern bindings)
     (and (for/and
           ((part (in-list pattern)))
           (or (string? part) (hash-has-key? bindings (slot-name part))))
          (apply
           string-append
           (for/list
            ((part (in-list pattern)))
            (if (string? part) part (hash-ref bindings (slot-name part)))))))
   (define (match-component pattern name bindings)
     (define found (match-template pattern name))
     (and found
          (for/and
           (((key value) (in-hash found)))
           (and (class-accepts? (hash-ref (language-classes body) key) value)
                (or (not (hash-has-key? bindings key)) (equal? (hash-ref bindings key) value))))
          (for/fold
           ((result bindings))
           (((key value) (in-hash found)))
           (hash-set result key value))))
   (define (add! components bindings)
     (unless (andmap safe-component? components) (template-error body source))
     (define path (located components))
     (define type (path-type path))
     (when (and type (not (eq? type 'file))) (file-error path))
     (when type
       (unless (= (bitwise-and (hash-ref (file-or-directory-stat path #t) 'mode) 61440) 32768)
         (file-error path)))
     (set! records
       (cons
        (hash
         'body
         body
         'path
         (string-join components "/")
         'placeholders
         bindings
         'status
         (if type "present" (if (language-required? body) "missing" "absent-empty"))
         'terms
         '()
         'origins
         '()
         'enclosing
         '()
         'absolute-path
         path
         'directory
         (drop-right components 1)
         'language-order
         (hash-ref ordinals body)
         'effective-lines
         0)
        records)))
   (define (walk remaining prefix bindings)
     (define pattern (car remaining))
     (define fixed (render pattern bindings))
     (define last? (null? (cdr remaining)))
     (cond
      (fixed
       (unless (safe-component? fixed) (template-error body source))
       (define next (append prefix (list fixed)))
       (if last?
         (add! next bindings)
         (let ((type (path-type (located next))))
           (if (or (not type) (eq? type 'directory))
             (walk (cdr remaining) next bindings)
             (file-error (located next))))))
      (else
       (for
        ((entry (in-list (entries prefix))))
        (define name (path->string entry))
        (define captures (and (safe-component? name) (match-component pattern name bindings)))
        (when captures
          (define next (append prefix (list name)))
          (if last?
            (when (eq? (path-type (located next)) 'file) (add! next captures))
            (when (eq? (path-type (located next)) 'directory)
              (walk (cdr remaining) next captures))))))))
   (walk patterns '() (hash))
   (when (and (language-required? body) (= before (length records)))
     (raise-typify
      "E_REQUIRED"
      "A required file template locates no programs"
      #:span
      (language-span body)
      #:data
      (hash 'path source))))
  (define ordered
    (sort
     records
     (lambda (left right)
       (define a (hash-ref left 'directory))
       (define b (hash-ref right 'directory))
       (or (< (length a) (length b))
           (and (= (length a) (length b))
                (or (string<? (hash-ref left 'path) (hash-ref right 'path))
                    (and (equal? (hash-ref left 'path) (hash-ref right 'path))
                         (< (hash-ref left 'language-order) (hash-ref right 'language-order)))))))))
  ordered)

(define (proper-prefix? prefix path)
  (and (< (length prefix) (length path)) (equal? prefix (take path (length prefix)))))

(define (enclosing-order left right)
  (define a (length (hash-ref left 'directory)))
  (define b (length (hash-ref right 'directory)))
  (or (> a b)
      (and (= a b)
           (or (< (hash-ref left 'language-order) (hash-ref right 'language-order))
               (and (= (hash-ref left 'language-order) (hash-ref right 'language-order))
                    (string<? (hash-ref left 'path) (hash-ref right 'path)))))))

(define (read-tree/diagnostics
         declarations
         root
         #:facts
         (given-facts #f)
         #:guard-resolver
         (guard-resolver #f))
  (define facts
    (if (tree-facts-required? declarations)
      (validate-facts (or given-facts (collect-facts root)))
      #f))
  (define (query name arguments origin) (fact-query facts name arguments))
  (define checked '())
  (define diagnostics '())
  (for
   ((record (in-list (discover-tree declarations root))))
   (define body (hash-ref record 'body))
   (define path (hash-ref record 'path))
   (define enclosing
     (sort
      (filter
       (lambda (candidate)
         (proper-prefix? (hash-ref candidate 'directory) (hash-ref record 'directory)))
       checked)
      enclosing-order))
   (define present? (equal? (hash-ref record 'status) "present"))
   (define text
     (if present?
       (with-handlers
        ((exn:fail:filesystem? (lambda (_) (file-error path))))
        (when (link-exists? (hash-ref record 'absolute-path)) (file-error path))
        (file->string (hash-ref record 'absolute-path)))
       ""))
   (define line-spans
     (for/hash
      ((line (in-list (read-lines text #:file path #:comment-prefix (language-comment body)))))
      (values (src-span-start-line (line-record-span line)) (line-record-span line))))
   (define source-spans
     (for/fold
      ((spans (for/hash (((line span) (in-hash line-spans))) (values (cons path line) span))))
      ((row (in-list (language-rows body))))
      (define span (line-record-span (hash-ref row 'record)))
      (hash-set spans (cons (src-span-file span) (src-span-start-line span)) span)))
   (define initial
     (hash-set* record 'enclosing enclosing 'line-spans line-spans '_source-spans source-spans))
   (define (context constructor origin)
     (define found
       (for*/first
        ((program (in-list enclosing))
         (term (in-list (hash-ref program 'terms)))
         #:when
         (and (equal? (car term) constructor) (pair? (cdr term))))
        (cadr term)))
     (unless found
       (define source (hash-ref origin 'path path))
       (define line (hash-ref origin 'line 1))
       (define span
         (hash-ref
          source-spans
          (cons source line)
          (lambda ()
            (raise-arguments-error
             'read-tree
             "the producing line's source span is unavailable"
             "path"
             source
             "line"
             line))))
       (raise-typify
        "E_CONTEXT"
        "No enclosing program provides the requested context"
        #:span
        span
        #:data
        (hash 'constructor constructor 'key "")))
     found)
   (define missing? (equal? (hash-ref record 'status) "missing"))
   (when missing?
     (set! diagnostics
       (cons
        (make-diagnostic
         "E_REQUIRED"
         "A required program is missing"
         #:span
         (src-span path 1 1 1 1 0 0)
         #:data
         (hash 'path path))
        diagnostics)))
   (define has-defaults? (ormap (lambda (form) (pair? (form-defaults form))) (language-forms body)))
   (define result
     (if (and (not missing?) (or present? has-defaults?))
       (check-program/diagnostics
        text
        body
        #:file
        path
        #:context
        context
        #:facts
        query
        #:guard-resolver
        (and guard-resolver (lambda (terms origins) (guard-resolver initial terms origins))))
       (hash 'terms '() 'origins '() 'effective-lines 0)))
   (set! diagnostics (append (hash-ref result 'diagnostics '()) diagnostics))
   (define terms (hash-ref result 'terms))
   (define origins (hash-ref result 'origins))
   (define guarded? (hash-ref result 'guarded? #f))
   (define updated
     (hash-set*
      initial
      'status
      (cond
       (missing? "missing")
       (guarded? "guarded")
       (present? "present")
       (has-defaults? "defaulted")
       (else "absent-empty"))
      'terms
      (if guarded? '() terms)
      'origins
      (if guarded? '() origins)
      'raw-terms
      (hash-ref result 'raw-terms terms)
      'raw-origins
      (hash-ref result 'raw-origins origins)
      '_term-spans
      (hash-ref result '_term-spans '())
      '_raw-term-spans
      (hash-ref result '_raw-term-spans '())
      'skips
      (hash-ref result 'skips '())
      'effective-lines
      (hash-ref result 'effective-lines (length terms))
      'enclosing
      enclosing))
   (set! checked (append checked (list updated))))
  (define ordered (diagnostics-sort diagnostics))
  (hash 'ok (not (diagnostics-error? ordered)) 'programs checked 'diagnostics ordered))

(define (read-tree declarations root #:facts (facts #f) #:guard-resolver (guard-resolver #f))
  (define result
    (read-tree/diagnostics declarations root #:facts facts #:guard-resolver guard-resolver))
  (raise-diagnostics (hash-ref result 'diagnostics))
  (hash-ref result 'programs))

(define (tree->jsexpr programs)
  (for/list
   ((program (in-list programs)))
   (hash
    'path
    (hash-ref program 'path)
    'language
    (language-name (hash-ref program 'body))
    'placeholders
    (hash-ref program 'placeholders)
    'status
    (hash-ref program 'status)
    'terms
    (hash-ref program 'terms)
    'origins
    (hash-ref program 'origins))))
