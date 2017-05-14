;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds sequence)
        (srfi 64))

(test-begin "pfds-sequence")

;; Note: at the moment, sequences are a trivial instantiation of
;; fingertrees, and so are pretty much covered by the fingertrees
;; tests.

;; sequences-bugs
(let ((s (sequence 'zero 'one 'two)))
  (test-equal 'zero (sequence-ref s 0))
  (test-equal 'two (sequence-ref s 2))
  (test-error (sequence-ref s -1))
  (test-error (sequence-ref s 3)))

(test-end)

