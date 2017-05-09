(import (scheme base)
        (slib factor)
        (srfi 64)
        (srfi 132))

(test-begin "slib-factor")

(test-equal 1 (jacobi-symbol 15 7))
(test-equal 0 (jacobi-symbol 7 7))
(test-equal -1 (jacobi-symbol 7 15))
(test-equal 1 (jacobi-symbol 64 7))
(test-equal 0 (jacobi-symbol 63 7))

(test-assert (prime? 17))
(test-assert (not (prime? 16)))

(test-equal '(13 17) (list-sort < (primes< 18 2)))
(test-equal '(19 23) (list-sort < (primes> 18 2)))

(test-equal '(2 2 5 5) (list-sort < (factor 100)))
(test-equal '(5) (list-sort < (factor 5)))

(test-end)

