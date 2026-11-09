#lang racket/base

(require "../src/near.rkt")

(provide run-tests)

(define (run-tests)
  (define checks 0)
  (define (check actual expected description)
    (set! checks (add1 checks))
    (unless (equal? actual expected)
      (error 'test-near "~a: expected ~s; received ~s" description expected actual)))
  (for
   ((row
     (in-list
      '(("" "" 0 "empty strings agree")
        ("abc" "abc" 0 "identical strings have distance zero")
        ("" "abc" 3 "inserting three code points costs three")
        ("abc" "" 3 "deleting three code points costs three")
        ("cat" "cart" 1 "an insertion costs one")
        ("cart" "cat" 1 "a deletion costs one")
        ("cat" "cut" 1 "a substitution costs one")
        ("ab" "ba" 1 "an adjacent exchange costs one")
        ("aab" "aba" 1 "an exchange beside a repeated character costs one")
        ("aaaa" "aaa" 1 "one repeated character can be deleted")
        ("abab" "baba" 2 "two disjoint adjacent exchanges cost two")
        ("CA" "AC" 1 "the first leg of the overlapping-edit witness is one exchange")
        ("AC" "ABC" 1 "the second leg of the overlapping-edit witness is one insertion")
        ("CA" "ABC" 3 "OSA refuses the overlapping edits unrestricted Damerau counts as two")
        ("ABC" "CA" 3 "the overlapping-edit witness has the same reverse distance")
        ("a" "A" 1 "case changes cost one substitution")
        ("abcdef" "ABCDEF" 6 "case is significant at every position")
        ("" "😀" 1 "a non-BMP code point is one insertion")
        ("😀" "" 1 "a non-BMP code point is one deletion")
        ("😀" "😁" 1 "different non-BMP code points cost one substitution")
        ("😀a" "a😀" 1 "a non-BMP code point participates in an adjacent exchange")
        ("😀😁" "😁😀" 1 "two non-BMP code points exchange at cost one")
        ("é" "é" 2 "distance uses code points without Unicode normalization")))))
   (check (optimal-string-alignment-distance (car row) (cadr row)) (caddr row) (cadddr row)))
  (define original "abcdef")
  (for
   ((index (in-range (add1 (string-length original)))))
   (define changed (string-append (substring original 0 index) "x" (substring original index)))
   (check
    (optimal-string-alignment-distance original changed)
    1
    "each insertion position costs one")
   (check
    (optimal-string-alignment-distance changed original)
    1
    "each insertion reverses to one deletion"))
  (for
   ((index (in-range (string-length original))))
   (define deleted (string-append (substring original 0 index) (substring original (add1 index))))
   (define replaced
     (string-append (substring original 0 index) "x" (substring original (add1 index))))
   (check (optimal-string-alignment-distance original deleted) 1 "each deletion position costs one")
   (check
    (optimal-string-alignment-distance original replaced)
    1
    "each substitution position costs one"))
  (for
   ((index (in-range (sub1 (string-length original)))))
   (define exchanged
     (string-append
      (substring original 0 index)
      (substring original (add1 index) (+ index 2))
      (substring original index (add1 index))
      (substring original (+ index 2))))
   (check
    (optimal-string-alignment-distance original exchanged)
    1
    "each adjacent exchange position costs one"))
  (for
   ((row
     (in-list
      '(("x" ("y") ("y") "a one-code-point word admits distance one")
        ("abcde" ("abc") () "five code points admit only distance one")
        ("abcdef" ("abcd") ("abcd") "six code points admit distance two from the asked length")
        ("abcdefghij" ("abcxyzghij") () "the bound stays capped at two for longer words")
        ("abcdefghij" ("abcdghij") ("abcdghij") "the capped bound still admits two deletions")
        ("abcdef"
         ("abxyef" "abcxef" "abcd" "abcde")
         ("abcde" "abcxef")
         "only minimum-distance candidates survive within the bound")
        ("cat"
         ("hat" "cats" "cart" "bat" "dog")
         ("bat" "cart" "cats" "hat")
         "all least-distance ties are sorted by code point")
        ("cat" ("hat" "bat" "hat" "bat") ("bat" "hat") "each tied candidate appears once")
        ("cat" () () "an empty candidate set has no suggestion")
        ("Abc" ("abc" "Abd") ("Abd" "abc") "distance and tie ordering preserve case")
        ("abcdef" ("ABCDEF") () "case folding cannot create a suggestion")
        ("😀a"
         ("😀😀" "😀b" "a😀")
         ("a😀" "😀b" "😀😀")
         "non-BMP ties use code-point distance and code-point ordering")
        ("red-blue"
         ("red-xxxxxxxx" "red-glue")
         ("red-glue")
         "a distance match suppresses the hyphen-part fallback")
        ("red-blue"
         ("redwood" "bluegreen" "red-black" "infra-blue-zone" "blue-green" "-red--gold-")
         ("-red--gold-" "blue-green" "infra-blue-zone" "red-black")
         "fallback keeps every exact shared hyphen part in code-point order")
        ("--red--blue--"
         ("--" "-" "---" "red-black")
         ("red-black")
         "empty hyphen parts do not become shared parts")
        ("alpha"
         ("alphabet" "gamma-alpha" "alpha-long")
         ("alpha-long" "gamma-alpha")
         "an unhyphenated word has one exact fallback part")
        ("alpha"
         ("alphabet-soup" "zzalpha-long")
         ()
         "prefixes and substrings are not shared hyphen parts")
        ("RED-BLUE"
         ("red-xxxx" "zzzz-BLUE")
         ("zzzz-BLUE")
         "hyphen-part equality is case significant")
        ("alpha"
         ("alpha-long" "alpha-long" "gamma-alpha")
         ("alpha-long" "gamma-alpha")
         "fallback candidates are deduplicated")
        ("alpha"
         ("alpha-long" "alpha" "alpht")
         ("alpha")
         "an exact candidate is the unique least-distance match")))))
   (check (near-words (car row) (cadr row)) (caddr row) (cadddr row)))
  (hash 'suite "near" 'checks checks 'status "passed"))

(module+ test (run-tests))
