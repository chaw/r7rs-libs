;; Calculate the set of unique abbreviations for a given set of 
;; words.  (Borrowed from Ruby's Abbrev.)

;; Written by Peter Lane, 2017

(define-library
  (robin abbrev)
  (export abbrev)
  (import (scheme base)
          (scheme case-lambda)
          (only (srfi 1) remove))
  (cond-expand
    ((library (chibi string))
      (import (only (chibi string) string-prefix?)))
    ((library (srfi 13))
      (import (only (srfi 13) string-prefix?))))

  (begin

    ;; input: a list of strings
    ;;        and an optional string prefix
    ;; output: list of (abbrev . string) dotted pairs
    (define abbrev
      (case-lambda
        ((strings) (abbrev strings #f))
        ((strings prefix) 
         (let loop ((abbrevs '())
                    (seen '())
                    (words strings)
                    (idx 1))
           (cond ((null? words) ; done
                  abbrevs)
                 ((or (and prefix (not (string-prefix? prefix (car words)))) ; does not match prefix
                      (> idx (string-length (car words)))) ; or at end of word, so go to next word
                  (loop abbrevs
                        seen
                        (cdr words)
                        1))
                 (else 
                   (let ((new-abbrev (string-copy (car words) 0 idx)))
                     (if (member new-abbrev seen string=?) ; if seen abbrev before
                       (loop (remove (lambda (w) (string=? new-abbrev (car w))) abbrevs) ; delete from abbrevs
                             seen ; but keep in seen
                             words 
                             (+ 1 idx))
                       (loop (cons (cons new-abbrev (car words)) abbrevs) ; not seen before, so add to abbrevs
                             (cons new-abbrev seen) ; and store in seen
                             words
                             (+ 1 idx))))))))))

    ))

