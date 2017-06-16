
(import (scheme base)
        (slib minimize)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-minimize")

(test-approx-same 0.816497
                  (car
                    (golden-section-search (lambda (x) (+ (* x x x) (* -2 x) -5))
                                           0
                                           1
                                           (/ 10000)))
                  (/ 10000))

(test-approx-same 0
                  (car 
                    (golden-section-search square -10 10 -1000)) ;; run for 1000 iterations
                  (/ 10000))

(test-end)

