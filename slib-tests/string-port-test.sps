
(import (scheme base)
        (slib string-port)
        (srfi 64))

(test-begin "slib-string-port")

(test-equal "abc" (call-with-output-string (lambda () (display "abc"))))
(test-equal 'abc (call-with-input-string "abc def" read))

(test-end)

