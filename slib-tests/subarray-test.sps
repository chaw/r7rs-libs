
(import (except (scheme base) equal?)  (scheme write)
        (slib subarray)
        (srfi 63)
        (srfi 64))

(test-begin "slib-subarray") ; note, we must use srfi 63's equal?

;; trimming vectors removes items from one side or other
(test-assert (equal? #(1 2 3 4) (array-trim #(0 1 2 3 4) 1)))
(test-assert (equal? #(0 1 2) (array-trim #(0 1 2 3 4) -2)))


(test-end)

