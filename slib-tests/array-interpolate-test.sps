(import (except (scheme base) equal?)
        (slib array-interpolate)
        (srfi 63)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-array-interpolate")

(test-approx-same 4.1 (interpolate-array-ref (list->array 2 #() '((1 2 3) (4 5 6))) 1 0.1) 0.1)
(test-approx-same 2.75 (interpolate-array-ref (list->array 2 #() '((1 2 3) (4 5 6))) 0.5 0.25) 0.01)
(test-approx-same 5 (interpolate-array-ref (list->array 2 #() '((1 2 3) (4 5 6))) 1 1) 0.01)

(let ((arr1 (list->array 2 #() '((1 2 3 4) (5 6 7 8))))
      (arr2 (make-array #() 2 3)))
  (resample-array! arr2 arr1)
  (test-equal '((1 5/2 4) (5 13/2 8)) (array->list arr2)))

(test-end)

