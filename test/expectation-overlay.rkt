#lang racket/base

(require file/sha1 json racket/file racket/list racket/match racket/path racket/string)

(provide apply-frozen-overlay)

(define (require-overlay condition message) (unless condition (error 'frozen-overlay "~a" message)))

(define (exact-fields? object fields)
  (and (hash? object) (equal? (sort (hash-keys object) symbol<?) (sort fields symbol<?))))

(define (fixture-root entry marker)
  (define args (hash-ref (hash-ref entry 'expected) 'invocation))
  (match-define (list* "typify" (or "tree" "plan") _ operand flags) args)
  (require-overlay
   (and (pair? flags)
        (string-prefix? (car flags) "--")
        (or (equal? marker "root") (equal? marker operand))
        (or (equal? operand ".")
            (and (relative-path? operand)
                 (not (string-contains? operand "\\"))
                 (andmap
                  (lambda (s) (not (member s '("" "." ".."))))
                  (string-split operand "/" #:trim? #f)))))
   "The fixture root marker must select this frozen root invocation's relative operand")
  (define base (path->complete-path (hash-ref entry 'directory)))
  (define path (simplify-path (path->complete-path operand base) #f))
  (define name (path->string (apply build-path (explode-path path))))
  (cond
   ((not (eq? (system-type 'os) 'windows)) name)
   ((regexp-match #px"^([A-Za-z]):[/\\\\]" name)
    =>
    (lambda (m)
      (string-append
       "/Volumes/"
       (string-upcase (cadr m))
       (string-trim (string-replace (substring name 2) "\\" "/") "/" #:left? #f))))
   ((regexp-match #px"^[/\\\\]{2}([^/\\\\]+)[/\\\\]([^/\\\\]+)" name)
    (string-append
     "/Volumes/UNC/"
     (string-trim (string-replace (substring name 2) "\\" "/") "/" #:left? #f)))
   (else (error 'frozen-overlay "Expected an ordinary drive or UNC path for this frozen fixture"))))

(define (expand-markers value path entry)
  (cond
   ((and (hash? value) (hash-has-key? value '$fixtureRoot))
    (require-overlay
     (and (exact-fields? value '($fixtureRoot)) (equal? path '("stdout_json" "inputs" "root")))
     "A fixture root marker can appear only at stdout_json.inputs.root")
    (fixture-root entry (hash-ref value '$fixtureRoot)))
   ((hash? value)
    (for/hasheq
     (((k v) (in-hash value)))
     (values k (expand-markers v (append path (list (symbol->string k))) entry))))
   ((list? value)
    (for/list
     ((v (in-list value)) (i (in-naturals)))
     (expand-markers v (append path (list i)) entry)))
   (else value)))

(define (add-member parent path value)
  (define step (car path))
  (cond
   ((string? step)
    (define key (string->symbol step))
    (require-overlay (hash? parent) "A named overlay path step requires an existing object")
    (if (null? (cdr path))
      (begin
        (require-overlay
         (not (hash-has-key? parent key))
         "An addition cannot replace an expected member")
        (hash-set parent key value))
      (begin
        (require-overlay (hash-has-key? parent key) "Missing parent in an overlay path")
        (hash-set parent key (add-member (hash-ref parent key) (cdr path) value)))))
   (else
    (require-overlay
     (and (list? parent)
          (exact-nonnegative-integer? step)
          (< step (length parent))
          (pair? (cdr path)))
     "An overlay array step must address an existing element")
    (list-update parent step (lambda (item) (add-member item (cdr path) value))))))

(define (allowed-addition? path)
  (match
   path
   ((list "stdout_json" (or "inputs" "facts" "schedule")) #t)
   ((list "stdout_json" "state" (? string?) (? exact-nonnegative-integer?) "sequence") #t)
   (_ #f)))

(define (apply-frozen-overlay file pinned-sha baseline inventory)
  (require-overlay
   (and (string? pinned-sha) (regexp-match? #px"^[0-9a-f]{64}$" pinned-sha))
   "The independent Stage 4 expectation overlay has no custody pin")
  (require-overlay
   (and (file-exists? file) (not (link-exists? file)))
   "The independently frozen Stage 4 expectation overlay is missing")
  (define bytes (file->bytes file))
  (require-overlay
   (equal? (bytes->hex-string (sha256-bytes bytes)) pinned-sha)
   "Stage 4 expectation overlay bytes differ from their custody pin")
  (define input (open-input-bytes bytes))
  (define data (read-json input))
  (require-overlay (eof-object? (read-json input)) "Trailing content after the overlay document")
  (require-overlay
   (and (exact-fields?
         data
         '(schema baselineArchiveSha256 baselineTreeSha256 baselineReceiptSha256 entries))
        (equal? (hash-ref data 'schema) "typify-v12-stage4-overlay-v1")
        (for/and (((key pin) (in-hash baseline))) (equal? (hash-ref data key) pin))
        (list? (hash-ref data 'entries))
        (pair? (hash-ref data 'entries)))
   "Invalid overlay schema or historical corpus custody")
  (define by-id (for/hash ((entry (in-list inventory))) (values (hash-ref entry 'id) entry)))
  (require-overlay (= (hash-count by-id) (length inventory)) "Duplicate frozen invocation ID")
  (define updated (make-hash))
  (for
   ((row (in-list (hash-ref data 'entries))))
   (require-overlay (exact-fields? row '(id invocation add)) "Invalid overlay entry")
   (define id (hash-ref row 'id))
   (define entry (hash-ref by-id id #f))
   (require-overlay
    (and (string? id) entry (not (hash-has-key? updated id)))
    "An overlay invocation ID is unknown or repeated")
   (define expected (hash-ref entry 'expected))
   (require-overlay
    (equal? (hash-ref row 'invocation) (hash-ref expected 'invocation))
    "The overlay arguments differ from the frozen invocation")
   (define additions (hash-ref row 'add))
   (require-overlay
    (and (list? additions) (pair? additions))
    "An overlay entry must add at least one named member")
   (define revised
     (for/fold
      ((result expected))
      ((item (in-list additions)))
      (require-overlay (exact-fields? item '(path value)) "Invalid overlay addition")
      (define path (hash-ref item 'path))
      (require-overlay (allowed-addition? path) "Path outside the additive Stage 4 wire")
      (add-member result path (expand-markers (hash-ref item 'value) path entry))))
   (hash-set! updated id (hash-set entry 'expected revised)))
  (for/list ((entry (in-list inventory))) (hash-ref updated (hash-ref entry 'id) entry)))
