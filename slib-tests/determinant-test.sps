
(import (except (scheme base) equal?)
        (slib determinant)
        (srfi 63)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-determinant")

(test-equal '((1 2) (3 4)) (matrix->lists '((1 2) (3 4))))
(let ((arr (make-array #(1) 2 2)))
  (array-set! arr 2 0 1)
  (array-set! arr 3 1 0)
  (array-set! arr 4 1 1)
  (test-equal '((1 2) (3 4)) (matrix->lists arr))
  (test-assert (equal? arr (matrix->array '((1 2) (3 4))))))

(test-equal -2 (determinant '((1 2) (3 4))))
(test-equal 0 (determinant '((1 2 3) (4 5 6) (7 8 9))))

(test-equal '((1 3) (2 4)) (transpose '((1 2) (3 4))))
(test-equal '((1 4 7) (2 5 8) (3 6 9)) (transpose '((1 2 3) (4 5 6) (7 8 9))))

(test-equal '((6 8) (10 12)) (matrix:sum '((1 2) (3 4)) '((5 6) (7 8))))
(test-equal '((-4 -4) (-4 -4)) (matrix:difference '((1 2) (3 4)) '((5 6) (7 8))))
(test-equal '((19 22) (43 50)) (matrix:product '((1 2) (3 4)) '((5 6) (7 8))))
(test-equal '((2 4) (6 8)) (matrix:product '((1 2) (3 4)) 2))
(test-equal '((2 4) (6 8)) (matrix:product 2 '((1 2) (3 4))))
(test-equal '((-2 1) (3/2 -1/2)) (matrix:inverse '((1 2) (3 4))))
(test-equal #f (matrix:inverse '((0 0) (0 0))))

(test-end)

