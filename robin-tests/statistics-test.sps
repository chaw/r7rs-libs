;; Test file for (robin statistics)

(import (scheme base)
        (scheme case-lambda)
        (robin statistics)
        (srfi 1)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "robin-statistics")

(test-equal 3 (mean '(1 2 3 4 5)))
(test-equal 7/2 (arithmetic-mean '(1 2 3 4 5 6)))
(test-approx-same 2.605 (geometric-mean '(1 2 3 4 5)))
(test-equal 300/137 (harmonic-mean '(1 2 3 4 5)))
(test-equal 3 (median '(1 2 3 4 5)))
(let-values (((modes count) (mode '(1 2 2 3 4 4 5 6))))
            (test-equal '(2 4) modes)
            (test-equal 2 count))
(test-equal 5/2 (variance '(1 2 3 4 5)))
(test-equal 2 (population-variance '(1 2 3 4 5)))
(test-approx-same 52.705 (coefficient-of-variation '(1 2 3 4 5)))
(test-approx-same 0.707 (standard-error-of-the-mean '(1 2 3 4 5)))

(test-equal 1 (sign 3))
(test-equal 0 (sign 0))
(test-equal -1 (sign -3))

(test-end)

