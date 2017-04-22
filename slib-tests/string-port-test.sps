
(import (scheme base)
        (scheme read)
        (scheme write)
        (slib string-port)
        (srfi 64))

(test-begin "slib-string-port")

(test-equal "abc" (call-with-output-string (lambda (port) (display "abc" port))))
(test-equal 'abc (call-with-input-string "abc def" read))

(test-end)

