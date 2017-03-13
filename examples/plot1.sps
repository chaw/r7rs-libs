;; Example of using charplot

(import (scheme base)
        (scheme inexact)
        (slib charplot))

(charplot:dimensions-set! '(20 55))

(define (make-points n)
  (if (zero? n)
    '()
    (cons (list (/ n 6) (sin (/ n 6))) (make-points (- n 1)))))

(plot (make-points 40) "x" "Sin(x)")
(newline)
