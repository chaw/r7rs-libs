
(import (scheme base)
        (slib chapter-order)
        (srfi 64))

(test-begin "(slib chapter-order)")

(test-assert (chap:string<? "a.9" "a.10"))
(test-assert (chap:string>? "4aa" "4c"))
(test-assert (chap:string<? "Revised^{3.99}" "Revised^{4}"))

(test-equal (chap:next-string "a.9") "a.10")
(test-equal (chap:next-string "4c") "4d")
(test-equal (chap:next-string "4z") "4aa")
(test-equal (chap:next-string "Revised^{4}") "Revised^{5}")

(test-end)

