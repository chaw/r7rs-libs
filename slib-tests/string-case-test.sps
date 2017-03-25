(import (scheme base)
        (slib string-case)
        (srfi 64))

(test-begin "slib-string-case")

(test-equal "String" (string-capitalize "string"))
(test-equal "String Words" (string-capitalize "stRInG WoRDs"))
(test-equal 'string (string-ci->symbol "StRiNg"))
(test-equal 'x11 (symbol-append 'x 1 "1"))
(for-each (lambda (args res)
            (test-equal res
                        (apply StudlyCapsExpand args)))
          '(("aX" " ") ("aX" "..") ("AX") ("Ax") ("AXLE") ("aAXACz")
                       ("AaXACz") ("AAaXACz") ("AAaXAC"))
          '("a X" "a..X" "AX" "Ax" "AXLE" "a-AXA-Cz"
            "Aa-XA-Cz" "A-Aa-XA-Cz" "A-Aa-XAC"))

(test-end)
