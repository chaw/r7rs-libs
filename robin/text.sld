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
          list->with-commas)
  (import (scheme base)
          (scheme write)
          (only (srfi 13) string-tokenize))

  (begin

    ;; word wrap, using greedy algorithm with minimum lines
    ;; returns a list of lines
    (define (word-wrap str width)
      (let loop ((words (string-tokenize str))
                 (line-length 0)
                 (line '())
                 (lines '()))
        (cond ((null? words)
               (reverse (cons (reverse line) lines)))
              ((> (+ line-length (string-length (car words)))
                  width)
               (if (null? line) 
                 (loop (cdr words) ; case where word exceeds line length
                       0
                       '()
                       (cons (list (car words)) lines))
                 (loop words ; word must go to next line, so finish current line
                       0
                       '()
                       (cons (reverse line) lines))))
              (else
                (loop (cdr words) ; else, add word to current line
                      (+ 1 line-length (string-length (car words)))
                      (cons (car words) line)
                      lines)))))

    ;; given a list of strings, 
    ;; return a string with list separated by commas, 
    ;; with last separated by 'and'.
    (define (list->with-commas strings)
      (parameterize ((current-output-port (open-output-string)))
                    (display "{")
                    (do ((rem strings (cdr rem)))
                      ((null? rem) (display "}\n"))
                      (display (car rem))
                      (cond ((= 1 (length rem)) )
                            ((= 2 (length rem))
                             (display " and "))
                            (else
                              (display ", "))))
                    (get-output-string (current-output-port))))

    ))

