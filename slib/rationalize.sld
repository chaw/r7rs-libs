;;;; "ratize.scm" Find simplest number ratios

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Note: Rationalize is in R7RS

(define-library
  (slib rationalize)
  (export find-ratio
          find-ratio-between)
  (import (scheme base))

  (begin

    ;;The procedure @dfn{rationalize} is interesting because most programming
    ;;languages do not provide anything analogous to it.  Thanks to Alan
    ;;Bawden for contributing this algorithm.  (Note: now in R7RS, so not provided here.)

    ;;@code{Rationalize} has limited use in implementations lacking exact
    ;;(non-integer) rational numbers.  The following procedures return a list
    ;;of the numerator and denominator.

    ;;@body
    ;;@0 returns the list of the @emph{simplest}
    ;;numerator and denominator whose quotient differs from @1 by no more
    ;;than @2.
    ;;
    ;;@format
    ;;@t{(find-ratio 3/97 .0001)             @result{} (3 97)
    ;;(find-ratio 3/97 .001)              @result{} (1 32)
    ;;}
    ;;@end format
    (define (find-ratio x e) (find-ratio-between (- x e) (+ x e)))

    ;;@body
    ;;@0 returns the list of the @emph{simplest}
    ;;numerator and denominator between @1 and @2.
    ;;
    ;;@format
    ;;@t{(find-ratio-between 2/7 3/5)        @result{} (1 2)
    ;;(find-ratio-between -3/5 -2/7)      @result{} (-1 2)
    ;;}
    ;;@end format
    (define (find-ratio-between x y)
      (define (sr x y)
        (let ((fx (exact (floor x))) (fy (exact (floor y))))
          (cond ((>= fx x) (list fx 1))
                ((= fx fy) (let ((rat (sr (/ (- y fy)) (/ (- x fx)))))
                             (list (+ (cadr rat) (* fx (car rat))) (car rat))))
                (else (list (+ 1 fx) 1)))))
      (cond ((< y x) (find-ratio-between y x))
            ((>= x y) (list x 1))
            ((positive? x) (sr x y))
            ((negative? y) (let ((rat (sr (- y) (- x))))
                             (list (- (car rat)) (cadr rat))))
            (else '(0 1))))

    ))

