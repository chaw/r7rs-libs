
(import (scheme base)
        (scheme list)
        (srfi 8)
        (srfi 64))

(test-begin "srfi-8-test")

(define x 22) ; variables are new locations

(receive (x y z) (values 1 2 3) ;; 1. one variable per value
         (test-equal 1 x)
         (test-equal 2 y)
         (test-equal 3 z))

(test-equal 22 x) ; so x should be unchanged

(receive x (values 1 2 3) ;; 2. all values as list in one variable
         (test-equal '(1 2 3) x))

(receive (x y . z) (values 1 2 3 4) ;; 3. excess values into list
         (test-equal 1 x)
         (test-equal 2 y)
         (test-equal '(3 4) z))

(receive (odds evens) (partition odd? (iota 10))
         (test-equal '(1 3 5 7 9) odds)
         (test-equal '(0 2 4 6 8) evens))

(test-end)

