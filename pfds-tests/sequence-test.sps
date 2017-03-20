;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds sequence)
        (srfi 64)
        (srfi 95))

(test-begin "pfds-sequence")

;; check that given code raises an error when turned to tokens
(define-syntax check-error
  (syntax-rules ()
    ((check-error code)
     (guard (err
              (else (test-assert #t)))
            code
            (test-assert #f)))))

;; Note: at the moment, sequences are a trivial instantiation of
;; fingertrees, and so are pretty much covered by the fingertrees
;; tests.

;; sequences-bugs
(let ((s (sequence 'zero 'one 'two)))
  (test-equal 'zero (sequence-ref s 0))
  (test-equal 'two (sequence-ref s 2))
  (check-error (sequence-ref s -1))
  (check-error (sequence-ref s 3)))

