;;; Port of quaternion.scm to R7RS Scheme, by Peter Lane, 2017.

;;; Original version Copyright (c) 2001-2002, Dorai Sitaram.
;;; All rights reserved.

;;; Permission to distribute and use this work for any
;;; purpose is hereby granted provided this copyright
;;; notice is included in the copy.  This work is provided
;;; as is, with no warranty of any kind.

(define-library
  (rebottled quaternion)
  (export ; note: renames standard procedures for numbers to handle quaternions as well
    (rename q-make-rectangular make-rectangular)
    (rename q-make-polar make-polar)
    (rename q-imag-part imag-part)
    (rename q-magnitude magnitude)
    (rename q-angle angle)
    (rename q-number? number?)
    (rename q-quaternion? quaternion?)
    (rename q= =)
    (rename q+ +)
    (rename q- -)
    (rename q* *)
    (rename q/ /)
    (rename qexp exp)
    (rename qlog log)
    (rename qexpt expt)
    (rename qsqrt sqrt)
    (rename qsin sin)
    (rename qcos cos)
    (rename qtan tan)
    (rename qasin asin)
    (rename qacos acos)
    (rename qatan atan)
    jmag-part
    kmag-part
    vector-part
    colatitude
    longitude
    conjugate
    unit-vector
    dot-product
    cross-product
    )
  (import (scheme base)
          (scheme complex)
          (scheme inexact))

  (begin

    (define-record-type <quaternion>
                        (make-quaternion w x y z)
                        quaternion?
                        (w quaternion-w)
                        (x quaternion-x)
                        (y quaternion-y)
                        (z quaternion-z))

    ;make-rectangular returns a plain real or
    ;a plain complex if some arguments are zero
    (define q-make-rectangular
      (lambda args
        (let ((w 0) (x 0) (y 0) (z 0)
                    (n (length args)))
          (when (> n 0)
            (set! w (list-ref args 0)))
          (when (> n 1)
            (set! x (list-ref args 1)))
          (when (> n 2)
            (set! y (list-ref args 2)))
          (when (> n 3)
            (set! z (list-ref args 3)))
          (when (> n 4)
            (error 'make-rectangular "more than 4 args supplied"))
          (if (= y z 0) 
            (make-rectangular w x)
            (make-quaternion w x y z)))))

    ;real-part, etc, operate on reals and complexes too
    (define q-real-part 
      (lambda (q)
        (cond ((complex? q) (real-part q))
              ((quaternion? q) (quaternion-w q))
              (else (error 'real-part "~a not a quaternion" q)))))

    (define q-imag-part
      (lambda (q)
        (cond ((complex? q) (imag-part q))
              ((quaternion? q) (quaternion-x q))
              (else (error 'imag-part "~a not a quaternion" q)))))

    (define jmag-part
      (lambda (q)
        (cond ((complex? q) 0)
              ((quaternion? q) (quaternion-y q))
              (else (error 'jmag-part "~a not a quaternion" q)))))

    (define kmag-part
      (lambda (q)
        (cond ((complex? q) 0)
              ((quaternion? q) (quaternion-z q))
              (else (error 'kmag-part "~a not a quaternion" q)))))

    ;vector part is quaternion minus its real part
    (define vector-part 
      (lambda (q)
        (q-make-rectangular 0 (q-imag-part q) (jmag-part q) (kmag-part q))))

    ;make-polar doesn't need colatitude (phi) and
    ;longitude (psi), in which case it returns plain
    ;complex
    (define q-make-polar
      (lambda (mu theta . phipsi)
        (let ((phi 0) (psi 0) (n (length phipsi)))
          (when (> n 0) (set! phi (list-ref phipsi 0)))
          (when (> n 1) (set! psi (list-ref phipsi 1)))
          (when (> n 2) (error 'make-polar
                               "more than 4 args supplied"))
          (q-make-rectangular
            (* mu (cos theta))
            (* mu (sin theta) (cos phi))
            (* mu (sin theta) (sin phi) (cos psi))
            (* mu (sin theta) (sin phi) (sin psi))))))

    (define q-magnitude
      (lambda (q)
        (sqrt (+ (expt (q-real-part q) 2)
                 (expt (q-imag-part q) 2)
                 (expt (jmag-part q) 2)
                 (expt (kmag-part q) 2)))))

    (define q-angle
      (lambda (q)
        (atan (sqrt (+ (expt (q-imag-part q) 2)
                       (expt (jmag-part q) 2)
                       (expt (kmag-part q) 2)))
              (q-real-part q))))

    (define colatitude
      (lambda (q)
        (atan (sqrt (+ (expt (jmag-part q) 2)
                       (expt (kmag-part q) 2)))
              (q-imag-part q))))

    (define longitude
      (lambda (q)
        (atan (kmag-part q)
              (jmag-part q))))

    ;the new number? must succeed on quaternions too
    (define q-number?
      (lambda (q)
        (or (complex? q)
            (quaternion? q))))

    ;in the quaternion language, quaternion?
    ;should return #t for all numbers, not just
    ;the quaternion structs
    (define q-quaternion? q-number?)

    (define q=
      (lambda (q . qq)
        (let ((w1 (q-real-part q))
              (x1 (q-imag-part q))
              (y1 (jmag-part q))
              (z1 (kmag-part q)))
          (let loop ((qq qq))
            (or (null? qq) 
                (let ((q (car qq)))
                  (and (= (q-real-part q) w1)
                       (= (q-imag-part q) x1)
                       (= (jmag-part q) y1)
                       (= (kmag-part q) z1)
                       (loop (cdr qq)))))))))

    (define q+
      (lambda qq 
        (let loop ((qq qq) (w1 0) (x1 0) (y1 0) (z1 0)) 
          (if (null? qq) (q-make-rectangular w1 x1 y1 z1)
            (let ((q (car qq)))
              (loop (cdr qq)
                    (+ w1 (q-real-part q))
                    (+ x1 (q-imag-part q))
                    (+ y1 (jmag-part q))
                    (+ z1 (kmag-part q))))))))

    (define q-
      (lambda (q . qq)
        (let ((w1 (q-real-part q))
              (x1 (q-imag-part q))
              (y1 (jmag-part q))
              (z1 (kmag-part q)))
          (if (null? qq)
            (q-make-rectangular (- w1) (- x1) (- y1) (- z1))
            (let loop ((qq qq) (w1 w1) (x1 x1) (y1 y1) (z1 z1)) 
              (if (null? qq)
                (q-make-rectangular w1 x1 y1 z1)
                (let ((q (car qq)))
                  (loop (cdr qq)
                        (- w1 (q-real-part q))
                        (- x1 (q-imag-part q))
                        (- y1 (jmag-part q))
                        (- z1 (kmag-part q))))))))))

    (define q*
      (lambda qq
        (let loop ((qq qq) (w1 1) (x1 0) (y1 0) (z1 0))
          (if (null? qq)
            (q-make-rectangular w1 x1 y1 z1)
            (let ((q (car qq)))
              (let ((w2 (q-real-part q))
                    (x2 (q-imag-part q))
                    (y2 (jmag-part q))
                    (z2 (kmag-part q)))
                (loop (cdr qq)
                      (- (* w1 w2) (* x1 x2) (* y1 y2) (* z1 z2))
                      (+ (* w1 x2) (* x1 w2) (* y1 z2) (- (* z1 y2)))
                      (+ (* w1 y2) (* y1 w2) (* z1 x2) (- (* x1 z2)))
                      (+ (* w1 z2) (* z1 w2) (* x1 y2) (- (* y1 x2))))))))))

    ;this will work on complexes too
    (define conjugate
      (lambda (q)
        (q-make-rectangular (q-real-part q)
                            (- (q-imag-part q))
                            (- (jmag-part q))
                            (- (kmag-part q)))))

    ;note we use "right" division 
    (define q/
      (lambda (q . qq)
        (if (null? qq)
          (cond ((complex? q) (/ q))
                ((quaternion? q) 
                 (let ((n (let ((m (q-magnitude q))) (* m m))))
                   (q-make-rectangular
                     (/ (q-real-part q) n)
                     (- (/ (q-imag-part q) n))
                     (- (/ (jmag-part q) n))
                     (- (/ (kmag-part q) n)))))
                (else (error '/ "~a not a quaternion" q)))
          (let loop ((qq qq) (r q)) 
            (if (null? qq) 
              r
              (let ((q (car qq)))
                (loop (cdr qq)
                      (q* r (q/ q)))))))))

    (define unit-vector
      (lambda (q)
        (if (real? q) +i
          (let ((v (vector-part q)))
            (q* v 
                (q/ (q-magnitude v)))))))

    ;dot- and cross-product only accept 
    ;vector quaternion arguments
    (define dot-product
      (lambda (q1 q2)
        (unless (and (= (q-real-part q1) 0)
                     (= (q-real-part q2) 0))
          (error 'dot-product "arguments ~a, ~a, are not vector quaternions"
                 q1 q2))
        (- (q-real-part (q* q1 q2)))))

    (define cross-product
      (lambda (q1 q2)
        (unless (and (= (q-real-part q1) 0)
                     (= (q-real-part q2) 0))
          (error 'dot-product "arguments ~a, ~a, are not vector quaternions"
                 q1 q2))
        (vector-part (q* q1 q2))))

    ;the following are all based on Maclaurin's series
    (define qexp
      (lambda (q)
        (let ((w (q-real-part q))
              (u (unit-vector q))
              (v (q-magnitude (vector-part q))))
          (q* (exp w)
              (q+ (cos v) (q* u (sin v)))))))

    (define qlog
      (lambda (q)
        (let ((w (q-real-part q))
              (u (unit-vector q))
              (v (q-magnitude (vector-part q))))
          (q+ (* 1/2 (log (+ (expt w 2) (expt v 2)))) 
              (q* u (atan v w))))))

    (define qexpt
      (lambda (q1 q2)
        (qexp (q* (qlog q1) q2))))

    (define qsqrt
      (lambda (q)
        (qexpt q 1/2)))

    ;real-valued sinh and cosh, which aren't in standard Scheme,
    ;are needed to define quat trig
    (define sinh
      (lambda (x)
        (* 1/2 (- (exp x) (exp (- x))))))

    (define cosh
      (lambda (x)
        (* 1/2 (+ (exp x) (exp (- x))))))

    (define qsin
      (lambda (q)
        (let ((w (q-real-part q))
              (u (unit-vector q))
              (v (q-magnitude (vector-part q))))
          (q+ (* (sin w) (cosh v))
              (q* u (* (cos w) (sinh v)))))))

    (define qcos
      (lambda (q)
        (let ((w (q-real-part q))
              (u (unit-vector q))
              (v (q-magnitude (vector-part q))))
          (q- (* (cos w) (cosh v))
              (q* u (* (sin w) (sinh v)))))))

    (define qtan
      (lambda (q)
        (q* (qsin q) (q/ (qcos q)))))

    (define qasin
      (lambda (q)
        (let ((u (unit-vector q)))
          (q- (q* u (qlog (q+ (q* u q)
                              (qsqrt (q- 1 (q* q q))))))))))

    (define qacos
      (lambda (q)
        (let ((u (unit-vector q)))
          (q- (q* u (qlog (q+ q
                              (qsqrt (q- (q* q q) 1)))))))))

    ;2-argument atan remains same as before 
    (define qatan
      (lambda (q . qq)
        (let ((u (unit-vector q)))
          (if (null? qq)
            (q* 1/2 u (qlog (q* (q+ u q)
                                (q/ (q- u q)))))
            (atan 
              ;note: args must be real here!
              q (car qq))))))

    )) ; end of library

