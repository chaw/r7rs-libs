
(define-library
  (astrocalc utility)
  (export
    divisible?
    deg-to-rad
    rad-to-deg
    dsin
    dasin
    dcos
    mod
    )
  (import (scheme base)
          (scheme inexact)
          (robin constants))

  (begin

    ;; Return #t if p divides exactly into n
    (define (divisible? n p)
      (zero? (modulo n p)))


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

    ))

