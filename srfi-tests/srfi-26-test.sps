; CONFIDENCE TEST FOR IMPLEMENTATION OF SRFI-26
; =============================================
;
; Sebastian.Egner@philips.com, 3-Jun-2002.
;
; This file checks a few assertions about the implementation.
; If you run it and no error message is issued, the implementation
; is correct on the cases that have been tested.
;

;; SRFI 64 tests by Peter Lane

(import (scheme base)
        (srfi 26)
        (srfi 64))

(test-begin "srfi-26-cut")

(test-equal ((cut list)) '())
(test-equal ((cut list <...>)) '())
(test-equal ((cut list 1)) '(1))
(test-equal ((cut list <>) 1) '(1))
(test-equal ((cut list <...>) 1) '(1))
(test-equal ((cut list 1 2)) '(1 2))
(test-equal ((cut list 1 <>) 2) '(1 2))
(test-equal ((cut list 1 <...>) 2) '(1 2))
(test-equal ((cut list 1 <...>) 2 3 4) '(1 2 3 4))
(test-equal ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4))
(test-equal ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
(test-equal (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)) '(ok))
(test-equal 
  (let ((a 0))
    (map (cut + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  2)
; cutes
(test-equal ((cute list)) '())
(test-equal ((cute list <...>)) '())
(test-equal ((cute list 1)) '(1))
(test-equal ((cute list <>) 1) '(1))
(test-equal ((cute list <...>) 1) '(1))
(test-equal ((cute list 1 2)) '(1 2))
(test-equal ((cute list 1 <>) 2) '(1 2))
(test-equal ((cute list 1 <...>) 2) '(1 2))
(test-equal ((cute list 1 <...>) 2 3 4) '(1 2 3 4))
(test-equal ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4))
(test-equal ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6))
(test-equal 
  (let ((a 0))
    (map (cute + (begin (set! a (+ a 1)) a) <>)
         '(1 2))
    a)
  1)

(test-end)

