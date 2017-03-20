(import (scheme base)
        (slib common-list-functions)
        (srfi 64))

(test-begin "slib-common-list-functions")

(test-equal 2 (position 3 '(1 2 3 4 5)))
(test-equal 1 (position #\e (string->list "peter")))
(test-equal #f (position 6 '(1 2 3 4 5)))

(test-end)

