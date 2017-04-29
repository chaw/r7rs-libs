
(import (scheme base)
        (robin constants)
        (slib rationalize)
        (srfi 64))

(test-begin "slib-rationalize")

(test-equal '(22 7) (find-ratio PI 0.01))
(test-equal '(201 64) (find-ratio PI 0.001))
(test-equal '(333 106) (find-ratio PI 0.0001))
(test-equal '(355 113) (find-ratio PI 0.00001))

(test-equal '(3 97) (find-ratio 3/97 0.0001))
(test-equal '(1 32) (find-ratio 3/97 0.001))

(test-equal '(1 2) (find-ratio-between 2/7 3/5))

(test-end)

