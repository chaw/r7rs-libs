;;; vectors.sls --- Vector Utilities

;; Copyright (C) 2014 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library 
  (pfds vector)
  (export vector-set
          vector-insert
          vector-remove
          vector-fold
          )
  (import (except (scheme base) vector-copy!))

  (begin

    (define (vector-set h i x)
      (let ((v* (vector-copy h)))
        (vector-set! v* i x)
        v*))

    (define (vector-remove v i)
      (define len (vector-length v))
      (unless (and (<= 0 i) (< i len)) (error "index out of bounds in vector-remove"))
      (let ((newvec (make-vector (- len 1))))
        (vector-copy! v 0 newvec 0 i)
        (vector-copy! v (+ i 1) newvec i (- len i 1))
        newvec))

    (define (vector-insert v i x)
      (define len (vector-length v))
      (unless (<= 0 i len) (error "index out of bounds in vector-insert"))
      (let* ((newvec (make-vector (+ len 1))))
        (vector-set! newvec i x)
        (vector-copy! v 0 newvec 0 i)
        (vector-copy! v i newvec (+ 1 i) (- len i))
        newvec))

    ;; note, different interface to vector-copy! in R7RS
    ;; only used internally
    (define (vector-copy! source source-start target target-start k)
      (do ((i 0 (+ 1 i)))
        ((>= i k))
        (vector-set! target
                     (+ target-start i)
                     (vector-ref source (+ source-start i)))))

    ;; vector-fold is left to right
    (define (vector-fold combine initial vector)
      (define len (vector-length vector))
      (let loop ((index 0) (accum initial))
        (if (>= index len)
          accum
          (loop (+ index 1)
                (combine (vector-ref vector index) accum)))))
    ))

