;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995, 2001, 2003 Aubrey Jaffer.
; Copyright (C) 2000 Colin Walters
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; A small selection packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib common-list-functions)
  (export adjoin
          fold-left
          fold-right
          position
          reduce)
  (import (scheme base))

  (begin

    ;;> Returns list with object added, unless object already in the list
    (define (adjoin obj lst) (if (memv obj lst) lst (cons obj lst)))

    ;;> fold-left takes a function whose arguments are (accumulated-value next-item)
    ;;> this agrees with fold-left from R6RS, but is different to fold in SRFI 1
    ;;> e.g. (fold-left f i '(a b c)) computes (f (f (f i a) b) c)
    (define (fold-left f i l)
      (if (null? l)
        i
        (fold-left f (f i (car l)) (cdr l))))

    ;;> (fold-right f i '(a b c)) computes (f a (f b (f c i)))
    (define (fold-right f i l)
      (define (iter rest)
        (if (null? rest)
          i
          (f (car rest)
             (iter (cdr rest)))))
      (iter l))

    ;;> a convenient version of fold-left taking its initial 
    ;;> argument from the start of the list.
    (define (reduce pred? lst)
      (cond ((null? lst) lst)
            ((null? (cdr lst)) (car lst))
            (else (fold-left pred? (car lst) (cdr lst)))))

    ;;> Returns the left-most index of obj in lst, or #f if it is not present.
    ;;> Note, list-index in srfi-1 uses a procedure to test for the object, so
    ;;> this is effectively (list-index (lambda (o) (eqv? o obj)) lst)
    (define (position obj lst)
      (define pos (lambda (n lst)
                    (cond ((null? lst) #f)
                          ((eqv? obj (car lst)) n)
                          (else (pos (+ 1 n) (cdr lst))))))
      (pos 0 lst))

    ))

