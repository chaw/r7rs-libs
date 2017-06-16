
(import (scheme base)
        (slib dynamic)
        (srfi 64))

(test-begin "slib-dynamic")

(let ((x (make-dynamic 'a))
      (y (make-dynamic 'b)))
  (test-assert (dynamic? x))
  (test-assert (not (dynamic? 'a)))
  (test-equal 'a (dynamic-ref x))
  (test-equal 'b (dynamic-ref y))
  (dynamic-set! x 'c)
  (test-equal 'c (dynamic-ref x))
  (test-equal 'b (dynamic-ref y))
  (call-with-dynamic-binding x 'd (lambda () (test-equal 'd (dynamic-ref x))))
  (test-equal 'c (dynamic-ref x)))

(test-end)

