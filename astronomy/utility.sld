
(define-library
  (astronomy utility)
  (export
    divisible?
    deg-in-range
    deg-to-rad
    rad-to-deg
    dsin
    dasin
    dcos
    neg
    group-by
    )
  (import (scheme base)
          (scheme inexact)
          (robin constants))

  (begin

    ;; Return #t if p divides exactly into n
    (define (divisible? n p)
      (zero? (modulo n p)))

    ;; Return negative of given number
    (define (neg n)
      (* -1 n))

    ;; Ensures a given degree is in the range [0,360)
    (define (deg-in-range d)
      (floor-remainder d 360))

    ;; Return radians equivalent of given angle in degrees
    (define (deg-to-rad d)
      (/ (* PI d) 180))

    ;; Return degrees equivalent of given angle in radians
    (define (rad-to-deg r)
      (/ (* 180 r) PI))

    ;; Compute sin of an angle given in degrees
    (define (dsin deg)
      (sin (deg-to-rad deg)))

    ;; Compute asin of a number and return angle in degrees
    (define (dasin r)
      (rad-to-deg (asin r)))

    ;; Compute cos of an angle given in degrees
    (define (dcos deg)
      (cos (deg-to-rad deg)))

    ;; Rearrange items in lst into lists of size n, padding last list with #f
    (define (group-by lst n)
      (let loop ((rem lst)
                 (result '())
                 (row '()))
        (cond ((null? rem) ; finished
               (if (null? row)
                 (reverse result)
                 (reverse 
                   (cons 
                     (append (reverse row) (make-list (- n (length row)) #f))
                     result))))
              ((= n (length row)) ; finished row
               (loop rem
                     (cons (reverse row) result)
                     '()))
              (else ; add item to row
                (loop (cdr rem)
                      result 
                      (cons (car rem) row))))))

    ))

