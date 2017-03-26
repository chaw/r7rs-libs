;;; bitwise.sls --- Bitwise Arithmetic Utilities

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library 
  (pfds bitwise)
  (export bitwise-bit-set
          bitwise-bit-set?
          bitwise-bit-unset
          bitwise-arithmetic-shift-right
          )
  (import (scheme base)
          (srfi 60))

  (begin

    (define (bitwise-bit-set bits i)
      (bitwise-ior bits (arithmetic-shift 1 i)))

    (define (bitwise-bit-set? bits i)
      (not (zero?
             (bitwise-and
               (arithmetic-shift 1 i)
               bits))))

    (define (bitwise-bit-unset bits i)
      (bitwise-and bits (bitwise-not (arithmetic-shift 1 i))))

    (define (bitwise-arithmetic-shift-right n r)
      (if (negative? r)
        (error "arithmetic shift right only valid for positive shifts")
        (exact (floor (/ n (expt 2 r))))))

    ))

