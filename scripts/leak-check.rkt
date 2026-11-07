#lang racket/base

(require file/sha1 json racket/file racket/list racket/path racket/port racket/string)

(provide check-repository source-comment-offset)

(define top-level '("README.md" "LICENSE.md" "src" "test" "scripts" ".github"))

(define corpus-network-addresses
  (for/list
   ((parts '((10 0 0 0) (192 168 1 1) (172 16 0 0) (10 0 0 1))))
   (string-join (map number->string parts) ".")))

(define corpus-network-admissions
  (hash
   "test/corpus/047-multislot-4-network-route/declarations.typify"
   "815fe198f5f75e0ae1164473a9b432eea9de19c6c24a6e76af808c8b8516c6e0"
   "test/corpus/047-multislot-4-network-route/program.typify"
   "79b453736b6d986f7fe4074b449f38e7f56d4d99c948ef8b0350b1a72bafdb53"
   "test/corpus/047-multislot-4-network-route/expected.json"
   "392e00c6a2bee382b42861417848f266dca606196427c2397a3ad73bf6eb8cf7"))

(define signatures
  (list
   (cons 'private-key #px"-----BEGIN (?:[A-Z ]+ )?PRIVATE KEY-----")
   (cons
    'credential
    #px"gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,}|(?:AKIA|ASIA)[A-Z0-9]{16}")
   (cons 'credential #px"(?:sk|hf)_[A-Za-z0-9_-]{24,}")
   (cons
    'home-path
    #px"/(?:home|Users)/[^/[:space:]\"']+|[A-Za-z]:[\\\\/]+Users[\\\\/]+[^\\\\/[:space:]\"']+")
   (cons 'key-file #px"[[:alnum:]_.-]+[.]pem\\b")))

(define (source-comment-offset text)
  (define size (string-length text))
  (let loop ((index 0) (quoted? #f))
    (cond
     ((>= index size) #f)
     (quoted?
      (case (string-ref text index)
        ((#\\) (loop (+ index 2) #t))
        ((#\") (loop (add1 index) #f))
        (else (loop (add1 index) #t))))
     ((char=? (string-ref text index) #\") (loop (add1 index) #t))
     ((and (< (+ index 2) size)
           (char=? (string-ref text index) #\#)
           (char=? (string-ref text (add1 index)) #\\))
      (let skip ((next (+ index 3)))
        (if (and (< next size) (char-alphabetic? (string-ref text next)))
          (skip (add1 next))
          (loop next #f))))
     ((or (char=? (string-ref text index) #\;)
          (and (< (add1 index) size)
               (char=? (string-ref text index) #\#)
               (memv (string-ref text (add1 index)) '(#\| #\;))))
      index)
     (else (loop (add1 index) #f)))))

(define (relative root path)
  (string-replace (path->string (find-relative-path root path)) "\\" "/"))

(define (network-literal? line admitted)
  (for/or
   ((candidate (regexp-match* #px"(?<![0-9])(?:[0-9]{1,3}[.]){3}[0-9]{1,3}(?![0-9])" line)))
   (define parts (map string->number (string-split candidate ".")))
   (and (andmap (lambda (part) (<= 0 part 255)) parts)
        (not (member candidate admitted))
        (not
         (or (= (car parts) 127) (member (take parts 3) '((192 0 2) (198 51 100) (203 0 113))))))))

(define (source-files root)
  (for/list
   ((path
     (in-directory
      root
      (lambda (path)
        (and (not (link-exists? path))
             (not (member (path->string (file-name-from-path path)) '(".git" "compiled")))))))
    #:unless
    (or (directory-exists? path) (equal? (file-name-from-path path) (string->path ".git"))))
   path))

(define (check-repository (given-root (current-directory)) #:deny (denied '()) #:extra (extra '()))
  (define root (simplify-path (path->complete-path given-root)))
  (define issues '())
  (define admissions '())
  (define (issue! path kind (line 1))
    (set! issues (cons (hasheq 'file path 'line line 'kind (symbol->string kind)) issues)))
  (for
   ((entry (directory-list root)))
   (define name (path->string entry))
   (unless (or (equal? name ".git") (member name top-level)) (issue! name 'layout)))
  (for
   ((required top-level))
   (unless (or
            (file-exists? (build-path root required))
            (directory-exists? (build-path root required)))
     (issue! required 'missing)))
  (define files (source-files root))
  (for
   ((path files))
   (define name (relative root path))
   (when (link-exists? path) (issue! name 'symlink))
   (when (and (regexp-match? #rx"[.]typify$" name) (not (string-prefix? name "test/corpus/")))
     (issue! name 'generated-source))
   (when (and
          (regexp-match? #rx"[.]md$" name)
          (not (member name '("README.md" "LICENSE.md" "test/corpus/MANIFEST.md"))))
     (issue! name 'markdown)))
  (for
   ((path (append files (map path->complete-path extra))))
   (define name (if (member path files) (relative root path) "<publication-input>"))
   (with-handlers
    ((exn:fail? (lambda (_) (issue! name 'unreadable))))
    (cond
     ((link-exists? path) (void))
     ((> (file-size path) (* 16 1024 1024)) (issue! name 'size))
     (else
      (define bytes (file->bytes path))
      (define text (bytes->string/utf-8 bytes #f))
      (define authorized-hash (hash-ref corpus-network-admissions name #f))
      (define admitted
        (if (and authorized-hash (equal? (bytes->hex-string (sha256-bytes bytes)) authorized-hash))
          corpus-network-addresses
          '()))
      (unless (null? admitted)
        (set! admissions
          (cons
           (hasheq
            'file
            name
            'sha256
            authorized-hash
            'allowed-addresses
            admitted
            'authority
            "Hash-bound admission for generic private-range corpus examples")
           admissions)))
      (when (regexp-match? #rx"[.]rkt$" (path->string path))
        (define offset (source-comment-offset text))
        (when offset
          (issue!
           name
           'source-comment
           (add1
            (for/sum
             ((char (in-string (substring text 0 offset))))
             (if (char=? char #\newline) 1 0))))))
      (for
       ((line (in-list (string-split text "\n" #:trim? #f))) (number (in-naturals 1)))
       (when (network-literal? line admitted) (issue! name 'network-address number))
       (for
        ((signature signatures) #:when (regexp-match? (cdr signature) line))
        (issue! name (car signature) number))
       (for
        ((word denied) #:when (string-contains? line word))
        (issue! name 'private-vocabulary number)))))))
  (hasheq
   'pass
   (null? issues)
   'files
   (length files)
   'extra
   (length extra)
   'admissions
   (sort admissions string<? #:key (lambda (item) (hash-ref item 'file)))
   'issues
   (reverse issues)))

(module+
 main
 (define arguments (vector->list (current-command-line-arguments)))
 (define denied '())
 (define extra '())
 (let loop ((remaining arguments))
   (cond
    ((null? remaining) (void))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--deny-file"))
     (set! denied (filter (lambda (line) (not (string=? line ""))) (file->lines (cadr remaining))))
     (loop (cddr remaining)))
    ((and (pair? (cdr remaining)) (equal? (car remaining) "--extra"))
     (set! extra (cons (cadr remaining) extra))
     (loop (cddr remaining)))
    (else
     (displayln
      "usage: racket scripts/leak-check.rkt [--deny-file FILE] [--extra FILE]"
      (current-error-port))
     (exit 2))))
 (define result (check-repository #:deny denied #:extra extra))
 (write-json result)
 (newline)
 (exit (if (hash-ref result 'pass) 0 1)))
