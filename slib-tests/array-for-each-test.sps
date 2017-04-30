(import (scheme base)
        (slib array-for-each)
        (slib determinant)
        (srfi 63)
        (srfi 64))

(test-begin "slib-array-for-each")

(let ((x (list->array 2 #() '((1 2) (3 4))))
      (y (list->array 2 #() '((5 6) (7 8)))))
  (test-assert (array-map! x + x y))
  (test-equal '((6 8) (10 12))
              (matrix->lists x)))

(test-equal '((6 8) (10 12))
            (matrix->lists (array-map #()
                                      +
                                      (list->array 2 #() '((1 2) (3 4)))
                                      (list->array 2 #() '((5 6) (7 8))))))

(test-equal '(((0 0) (0 1)) ((1 0) (1 1)))
            (matrix->lists (array-indexes (list->array 2 #() '((1 2) (3 4))))))

(let ((a (vector 0 0 0)))
  (test-assert (array-index-map! a (lambda args (apply + args))))
  (test-equal #(0 1 2) a))

(let ((v (vector 1 2 3))
      (w (vector 4 5 6)))
  (test-assert (array:copy! v w))
  (test-equal #(4 5 6) v)
  (test-equal #(4 5 6) w))

(test-end)

