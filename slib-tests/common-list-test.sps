(import (scheme base)
        (slib common-list-functions)
        (srfi 64))

(test-begin "slib-common-list-functions")

(test-equal '(a b c) (adjoin 'a '(b c)))
(test-equal '(a b c) (adjoin 'b '(a b c)))

(test-equal '(((i a) b) c) (fold-left list 'i '(a b c)))
(test-equal '(a (b (c i))) (fold-right list 'i '(a b c)))
(test-equal '(((i a) b) c) (reduce list '(i a b c)))

(test-equal 2 (position 3 '(1 2 3 4 5)))
(test-equal 1 (position #\e (string->list "peter")))
(test-equal #f (position 6 '(1 2 3 4 5)))

(test-end)

