;; Text library for R7RS Scheme

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
  (robin text)
  (export word-wrap
          words->with-commas)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (only (srfi 13) string-join string-tokenize))

  (begin

    ;; word wrap, using greedy algorithm with minimum lines
    ;; returns a list of lines
    (define (word-wrap str width)
      (let loop ((words (string-tokenize str))
                 (line-length 0)
                 (line "")
                 (lines '()))
        (cond ((null? words)
               (reverse (cons line lines)))
              ((> (+ line-length (string-length (car words)))
                  width)
               (if (zero? (string-length line))
                 (loop (cdr words) ; case where word exceeds line length
                       0
                       "" 
                       (cons (car words) lines))
                 (loop words ; word must go to next line, so finish current line
                       0
                       ""
                       (cons line lines))))
              (else
                (loop (cdr words) ; else, add word to current line
                      (+ 1 line-length (string-length (car words)))
                      (if (zero? (string-length line))
                        (car words)
                        (string-join (list line (car words))))
                      lines)))))

    ;; given a list of strings, each string representing a word, 
    ;; return a string with each word separated by commas, 
    ;; but last separated by 'and'.
    ;; Optional flag is used to include the comma before 'and', if required.
    (define words->with-commas 
      (case-lambda
        ((words) ; default to no optional comma
         (words->with-commas words #f))
        ((words add-comma?)
         (parameterize ((current-output-port (open-output-string)))
                       (do ((rem words (cdr rem)))
                         ((null? rem) )
                         (display (car rem))
                         (cond ((= 1 (length rem)) )
                               ((= 2 (length rem))
                                (when add-comma? (display ","))
                                (display " and "))
                               (else
                                 (display ", "))))
                       (get-output-string (current-output-port))))))

    ))

