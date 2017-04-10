;; Some examples of AD
;; Examples from https://www.sintef.no/globalassets/project/evitameeting/2010/ad2010.pdf

(import (except (scheme base) + - * / expt square = < > <= >= zero? positive? negative? real?)
        (scheme write)
        (autodiff AD))

(define (f x) (square x))

(display "(f 3) = ") (display (f 3))
(display "  ; (f' 3) = ") (display ((derivative-F f) 3)) (newline)

(define (g x) (/ (* (+ x 1) (- x 2))
                 (+ x 3)))

(display "(g 3) = ") (display (g 3))
(display "  ; (g' 3) = ") (display ((derivative-F g) 3)) (newline)

(define (h x) (- x (/ (+ (* 4 x) 2)
                      (+ x 3))))

(display "(h 3) = ") (display (h 3))
(display "  ; (h' 3) = ") (display ((derivative-F h) 3)) (newline)

(display "(f x) = (1+x+e^x).sin x ; f'(0) = ") 
(display ((derivative-F (lambda (x) (* (+ 1 x (exp x)) (sin x))))
          0)) 
(newline)

