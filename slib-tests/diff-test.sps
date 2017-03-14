
(import (scheme base)
        (slib diff)
        (only (srfi 63) array->list)
        (srfi 64))

(test-begin "(slib diff)")

(test-equal (diff:longest-common-subsequence "fghiejcklm" "fgehijkpqrlm")
            "fghijklm")
(test-equal (diff:edit-length "fghiejcklm" "fgehijkpqrlm")
            6)
(test-equal (array->list (diff:edits "fghiejcklm" "fgehijkpqrlm"))
            '(3 -5 -7 8 9 10))

(test-end)

