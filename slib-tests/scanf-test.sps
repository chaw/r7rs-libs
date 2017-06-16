;; some tests for scanf - implementation is failing in some cases

(import (scheme base) (scheme write)
        (slib scanf)
        (srfi 64))

(test-begin "slib-scanf")

(test-equal '(1) (scanf-read-list "%d" "1"))
(test-equal '(1 23 456) (scanf-read-list "%d %d %d" "1 23 456"))

(let-values (((n x) (scanf-read-values "%d" "2")))
            (test-equal 1 n)
            (test-equal 2 x))

(let-values (((n x y z) (scanf-read-values "%d %d %d" "2 3 4")))
            (test-equal 3 n)
            (test-equal 2 x)
            (test-equal 3 y)
            (test-equal 4 z))

(test-end)

