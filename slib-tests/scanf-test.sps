;; some tests for scanf - implementation is failing in some cases

(import (scheme base) (scheme eval)
        (slib scanf)
        (srfi 64))

(test-begin "slib-scanf")

(test-equal '(1) (scanf-read-list "%d" "1"))
(test-equal '(1 23 456) (scanf-read-list "%d %d %d" "1 23 456"))

(test-end)

