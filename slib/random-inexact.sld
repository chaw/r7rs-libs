;;;"randinex.scm" Pseudo-Random inexact real numbers for scheme.
;;; Copyright (C) 1991, 1993, 1999 Aubrey Jaffer
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

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Rewritten to use SRFI 27 and random-real 

;;; Sphere and normal functions corrections from: Harald Hanche-Olsen

(define-library
  (slib random-inexact)
  (export random:uniform
          random:exp
          random:normal
          random:normal-vector!
          random:hollow-sphere!
          random:solid-sphere!)
  (import (scheme base)
          (scheme inexact)
          (srfi 27))

  (begin

    ;;@code{(require 'random-inexact)}
    ;;@ftindex random-inexact

    ;;@args
    ;;@args state
    ;;Returns an uniformly distributed inexact real random number in the
    ;;range between 0 and 1.
    (define (random:uniform . args)
      (unless (null? args)
        (random-source-state-set! default-random-source (car args)))
      (random-real))

    ;;@args
    ;;@args state
    ;;Returns an inexact real in an exponential distribution with mean 1.  For
    ;;an exponential distribution with mean @var{u} use
    ;;@w{@code{(* @var{u} (random:exp))}}.
    (define (random:exp . args)
      (unless (null? args)
        (random-source-state-set! default-random-source (car args)))
      (- (log (random-real))))


    ;;@args
    ;;@args state
    ;;Returns an inexact real in a normal distribution with mean 0 and
    ;;standard deviation 1.  For a normal distribution with mean @var{m} and
    ;;standard deviation @var{d} use
    ;;@w{@code{(+ @var{m} (* @var{d} (random:normal)))}}.
    (define (random:normal . args)
      (let ((vect (make-vector 1)))
        (apply random:normal-vector! vect args)
        (vector-ref vect 0)))


    ;;; If x and y are independent standard normal variables, then with
    ;;; x=r*cos(t), y=r*sin(t), we find that t is uniformly distributed
    ;;; over [0,2*pi] and the cumulative distribution of r is
    ;;; 1-exp(-r^2/2).  This latter means that u=exp(-r^2/2) is uniformly
    ;;; distributed on [0,1], so r=sqrt(-2 log u) can be used to generate r.

    ;;@args vect
    ;;@args vect state
    ;;Fills @1 with inexact real random numbers which are independent
    ;;and standard normally distributed (i.e., with mean 0 and variance 1).
    (define random:normal-vector!
      (let ((*2pi (* 8 (atan 1))))
        (lambda (vect . args)
          (unless (null? args)
            (random-source-state-set! default-random-source (car args)))
          (let ((sum2 0))
            (let ((do! (lambda (k x)
                         (vector-set! vect k x)
                         (set! sum2 (+ sum2 (* x x))))))
              (do ((n (- (vector-length vect) 1) (- n 2)))
                ((negative? n) sum2)
                (let ((t (* *2pi (random-real)))
                      (r (sqrt (* -2 (log (random-real))))))
                  (do! n (* r (cos t)))
                  (if (positive? n) (do! (- n 1) (* r (sin t)))))))))))


    ;;; For the uniform distibution on the hollow sphere, pick a normal
    ;;; family and scale.

    ;;@args vect
    ;;@args vect state
    ;;Fills @1 with inexact real random numbers the sum of whose
    ;;squares is equal to 1.0.  Thinking of @1 as coordinates in space
    ;;of dimension n = @code{(vector-length @1)}, the coordinates are
    ;;uniformly distributed over the surface of the unit n-shere.
    (define (random:hollow-sphere! vect . args)
      (let ((ms (sqrt (apply random:normal-vector! vect args))))
        (do ((n (- (vector-length vect) 1) (- n 1)))
          ((negative? n))
          (vector-set! vect n (/ (vector-ref vect n) ms)))))


    ;;; For the uniform distribution on the solid sphere, note that in
    ;;; this distribution the length r of the vector has cumulative
    ;;; distribution r^n; i.e., u=r^n is uniform [0,1], so r can be
    ;;; generated as r=u^(1/n).

    ;;@args vect
    ;;@args vect state
    ;;Fills @1 with inexact real random numbers the sum of whose
    ;;squares is less than 1.0.  Thinking of @1 as coordinates in
    ;;space of dimension @var{n} = @code{(vector-length @1)}, the
    ;;coordinates are uniformly distributed within the unit @var{n}-shere.
    ;;The sum of the squares of the numbers is returned.
    (define (random:solid-sphere! vect . args)
      (unless (null? args)
        (random-source-state-set! default-random-source (car args)))
      (apply random:hollow-sphere! vect args)
      (let ((r (expt (random-real)
                     (/ (vector-length vect)))))
        (do ((n (- (vector-length vect) 1) (- n 1)))
          ((negative? n) r)
          (vector-set! vect n (* r (vector-ref vect n))))))

    ))

