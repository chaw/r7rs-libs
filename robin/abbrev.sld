;; Abbrev library for R7RS Scheme.

;; Calculate the set of unique abbreviations for a given set of 
;; words.  (Borrowed from Ruby's Abbrev.)

;; Written by Peter Lane, 2017

;; # Open Works License
;; 
;; This is version 0.9.4 of the Open Works License
;; 
;; ## Terms
;; 
;; Permission is hereby granted by the holder(s) of copyright or other legal
;; privileges, author(s) or assembler(s), and contributor(s) of this work, to any
;; person who obtains a copy of this work in any form, to reproduce, modify,
;; distribute, publish, sell, sublicense, use, and/or otherwise deal in the
;; licensed material without restriction, provided the following conditions are
;; met:
;; 
;; Redistributions, modified or unmodified, in whole or in part, must retain
;; applicable copyright and other legal privilege notices, the above license
;; notice, these conditions, and the following disclaimer.
;; 
;; NO WARRANTY OF ANY KIND IS IMPLIED BY, OR SHOULD BE INFERRED FROM, THIS LICENSE
;; OR THE ACT OF DISTRIBUTION UNDER THE TERMS OF THIS LICENSE, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
;; AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS, ASSEMBLERS, OR HOLDERS OF
;; COPYRIGHT OR OTHER LEGAL PRIVILEGE BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER
;; LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT, OR OTHERWISE ARISING FROM, OUT
;; OF, OR IN CONNECTION WITH THE WORK OR THE USE OF OR OTHER DEALINGS IN THE WORK.

(define-library
  (robin abbrev)
  (export abbrev)
  (import (scheme base)
          (scheme case-lambda)
          (only (scheme list) remove))
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

