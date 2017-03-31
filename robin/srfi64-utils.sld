;; SRFI 64 helper functions
;; written by Peter Lane, 2017

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
  (robin srfi64-utils)
  (export test-all-equal
          test-approx-same
          test-compare
          test-for-error
          test-no-error)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 64))

  (begin

    ;; Uses test-equal on all pairs in given association list
    (define (test-all-equal fn pairs)
      (for-each (lambda (pair)
                  (test-equal (cdr pair) (fn (car pair))))
                pairs))

    ;; Test if two inexact numbers are within a given tolerance: default is 0.001
    (define test-approx-same 
      (case-lambda 
        ((x y)
         (test-approx-same x y 0.001))
        ((x y tolerance)
         (test-assert (< (abs (- x y)) tolerance)))))

    ;; Test if two given items satisfy the given comparison procedure
    (define (test-compare proc l1 l2)
      (test-assert (proc l1 l2)))

    ;; Test fails if no error is raised
    (define-syntax test-for-error
      (syntax-rules ()
                    ((test-no-error code)
                     (guard (err
                              (else (test-assert #t)))
                            code
                            (test-assert #f)))))

    ;; Test fails if an error is raised
    (define-syntax test-no-error
      (syntax-rules ()
                    ((test-no-error code)
                     (guard (err
                              (else (test-assert #f)))
                            code
                            (test-assert #t)))))

    ))

