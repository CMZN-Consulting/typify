#lang racket/base

(require net/uri-codec
         net/url
         racket/list
         racket/path
         racket/promise
         racket/string
         "diagnostics.rkt")

(provide record-source-path record-source-text with-projection-sources write-diagnostic-projection)

(struct document (path lines) #:transparent)

(struct source-index (by-id by-name) #:transparent)

(define current-sources (make-parameter #f))

(define (with-projection-sources thunk)
  (parameterize ((current-sources (source-index (make-hasheq) (make-hash)))) (thunk)))

(define (portable path) (string-replace (if (path? path) (path->string path) path) "\\" "/"))

(define (record-source! display physical lines)
  (define index (current-sources))
  (when index
    (define file (document (simplify-path (path->complete-path physical) #f) lines))
    (hash-set! (source-index-by-id index) display file)
    (hash-set! (source-index-by-name index) (portable display) file)))

(define (record-source-path display physical) (record-source! display physical #f))

(define (record-source-text display physical text)
  (when (current-sources)
    (record-source! display physical (delay (list->vector (regexp-split #rx"\r?\n" text)))))
  text)

(define (source name)
  (define index (current-sources))
  (and index
       (or (hash-ref (source-index-by-id index) name #f)
           (hash-ref (source-index-by-name index) (portable name) #f))))

(define (physical-path name)
  (define file (source name))
  (if file (document-path file) (simplify-path (path->complete-path name) #f)))

(define (document-uri name) (url->string (path->url (physical-path name))))

(define (display-path name)
  (portable (find-relative-path (current-directory) (physical-path name))))

(define (artifact-uri name)
  (define path (find-relative-path (current-directory) (physical-path name)))
  (if (absolute-path? path)
    (document-uri name)
    (string-join (map uri-encode (string-split (portable path) "/")) "/")))

(define (sarif-region range)
  (define start (hash-ref range 'start))
  (define end (hash-ref range 'end))
  (hash
   'startLine
   (hash-ref start 'line)
   'startColumn
   (hash-ref start 'column)
   'endLine
   (hash-ref end 'line)
   'endColumn
   (hash-ref end 'column)
   'byteOffset
   (hash-ref start 'byteOffset)
   'byteLength
   (- (hash-ref end 'byteOffset) (hash-ref start 'byteOffset))))

(define (sarif-location file region)
  (define physical (hash 'artifactLocation (hash 'uri (artifact-uri file))))
  (hash 'physicalLocation (if region (hash-set physical 'region region) physical)))

(define (related-file item) (hash-ref item 'file (lambda () (hash-ref item 'path))))

(define (related-message item)
  (hash-ref
   item
   'message
   (lambda () (hash-ref item 'language (lambda () (hash-ref item 'kind "Related location"))))))

(define (sarif-related item id)
  (define range (hash-ref item 'range #f))
  (define region
    (cond
     ((hash? range) (sarif-region range))
     ((hash-has-key? item 'line) (hash 'startLine (hash-ref item 'line)))
     (else #f)))
  (hash-set*
   (sarif-location (related-file item) region)
   'id
   id
   'message
   (hash 'text (related-message item))
   'properties
   item))

(define (sarif-result item)
  (define file (hash-ref item 'file))
  (define range (hash-ref item 'range))
  (define fingerprints (hash-ref item 'fingerprints))
  (define data (hash-ref item 'data))
  (define properties
    (hash-set data 'typify (hash 'fingerprints fingerprints 'fix (hash-ref item 'fix))))
  (hash
   'ruleId
   (hash-ref item 'code)
   'level
   (hash-ref item 'severity)
   'message
   (hash 'text (hash-ref item 'message))
   'locations
   (if (equal? file "") '() (list (sarif-location file (and (hash? range) (sarif-region range)))))
   'relatedLocations
   (for/list
    ((related (in-list (hash-ref item 'related))) (id (in-naturals 1)))
    (sarif-related related id))
   'partialFingerprints
   (hash 'typify/v1 (content-hash (canonical-json fingerprints)))
   'properties
   properties))

(define (sarif-report items)
  (define rules (for/list ((code (in-list diagnostic-codes))) (hash 'id code)))
  (hash
   'version
   "2.1.0"
   'runs
   (list
    (hash
     'tool
     (hash 'driver (hash 'name "Typify" 'rules rules))
     'columnKind
     "unicodeCodePoints"
     'results
     (map sarif-result items)))))

(define (lsp-position name pos)
  (define index (sub1 (hash-ref pos 'line)))
  (define column (sub1 (hash-ref pos 'column)))
  (define character
    (if (zero? column)
      0
      (let* ((file (source name)) (line (vector-ref (force (document-lines file)) index)))
        (for/sum ((ch (in-string line 0 column))) (if (> (char->integer ch) 65535) 2 1)))))
  (hash 'line index 'character character))

(define (lsp-range file range)
  (hash
   'start
   (lsp-position file (hash-ref range 'start))
   'end
   (lsp-position file (hash-ref range 'end))))

(define (lsp-related item)
  (define file (related-file item))
  (define span (hash-ref item 'range #f))
  (define range
    (cond
     ((hash? span) (lsp-range file span))
     ((hash-has-key? item 'line)
      (define at (hash 'line (sub1 (hash-ref item 'line)) 'character 0))
      (hash 'start at 'end at))
     (else #f)))
  (and range
       (hash
        'location
        (hash 'uri (document-uri file) 'range range)
        'message
        (related-message item))))

(define (lsp-diagnostic item)
  (hash
   'range
   (lsp-range (hash-ref item 'file) (hash-ref item 'range))
   'severity
   (if (equal? (hash-ref item 'severity) "error") 1 3)
   'code
   (hash-ref item 'code)
   'source
   "Typify"
   'message
   (hash-ref item 'message)
   'relatedInformation
   (filter-map lsp-related (hash-ref item 'related))
   'data
   (hash
    'data
    (hash-ref item 'data)
    'fix
    (hash-ref item 'fix)
    'related
    (hash-ref item 'related)
    'fingerprints
    (hash-ref item 'fingerprints))))

(define (lsp-report items)
  (define documents (make-hash))
  (define unlocated '())
  (for
   ((file (in-hash-values (source-index-by-id (current-sources)))) #:when (document-lines file))
   (hash-set! documents (url->string (path->url (document-path file))) '()))
  (for
   ((item (in-list items)))
   (if (hash? (hash-ref item 'range))
     (hash-update!
      documents
      (document-uri (hash-ref item 'file))
      (lambda (group) (cons (lsp-diagnostic item) group))
      '())
     (set! unlocated (cons item unlocated))))
  (hash
   'documents
   (for/list
    ((uri (in-list (sort (hash-keys documents) string<?))))
    (hash 'uri uri 'diagnostics (reverse (hash-ref documents uri))))
   'unlocated
   (reverse unlocated)))

(define (escape-message text)
  (string-replace (string-replace (string-replace text "%" "%25") "\r" "%0D") "\n" "%0A"))

(define (escape-property text)
  (string-replace (string-replace (escape-message text) ":" "%3A") "," "%2C"))

(define (annotation item)
  (define file (hash-ref item 'file))
  (define range (hash-ref item 'range))
  (define properties
    (append
     (if (equal? file "") '() (list (string-append "file=" (escape-property (display-path file)))))
     (if (hash? range)
       (let ((start (hash-ref range 'start)) (end (hash-ref range 'end)))
         (define single-line? (= (hash-ref start 'line) (hash-ref end 'line)))
         (define last-line
           (if (and (not single-line?) (= (hash-ref end 'column) 1))
             (sub1 (hash-ref end 'line))
             (hash-ref end 'line)))
         (append
          (list (format "line=~a" (hash-ref start 'line)) (format "endLine=~a" last-line))
          (if single-line?
            (list
             (format "col=~a" (hash-ref start 'column))
             (format "endColumn=~a" (max (hash-ref start 'column) (sub1 (hash-ref end 'column)))))
            '())))
       '())
     (list (string-append "title=" (escape-property (hash-ref item 'code))))))
  (format
   "::~a ~a::~a"
   (if (equal? (hash-ref item 'severity) "error") "error" "notice")
   (string-join properties ",")
   (escape-message (hash-ref item 'message))))

(define (write-diagnostic-projection format items)
  (cond
   ((equal? format "--sarif") (displayln (canonical-json (sarif-report items))))
   ((equal? format "--lsp") (displayln (canonical-json (lsp-report items))))
   ((equal? format "--github-annotations") (for-each displayln (map annotation items)))))
