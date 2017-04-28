
(import (scheme base)
        (astronomy calendar)
        (astronomy jupiter)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "astronomy-jupiter")

;(let-values (((w1 w2 De Ds P) (jupiter-data (make-date 16 12 1992))))
;            (test-approx-same 268.06 w1 0.01)
;            (test-approx-same 72.74 w2 0.01)
;            (test-approx-same -2.48 De 0.01)
;            (test-approx-same -2.20 Ds 0.01)
;            (test-approx-same 24.80 P 0.01))

(let-values (((w1 w2 De Ds) (jupiter-data-low-accuracy (make-date 16 12 1992))))
            (test-approx-same 267.69 w1 0.01)
            (test-approx-same 72.36 w2 0.01)
            (test-approx-same -2.50 De 0.1)
            (test-approx-same -2.194 Ds 0.01))

(test-end)

