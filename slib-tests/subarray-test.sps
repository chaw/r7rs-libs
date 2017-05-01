
(import (except (scheme base) equal?)  (scheme write)
        (slib subarray)
        (srfi 63)
        (srfi 64))

(test-begin "slib-subarray") ; note, we must use srfi 63's equal?

(let ((arr (list->array 2 #() '((1 2 3) (4 5 6) (7 8 9)))))
  (test-equal '(4 5 6) (array->list (subarray arr 1 #f))) ; select a single row
  (test-equal '(2 5 8) (array->list (subarray arr #f 1))) ; select a single column
  (test-equal '((1 2) (4 5) (7 8)) (array->list (subarray arr #f '(0 1)))) ; select first two columns
  (test-equal '((7 8 9) (4 5 6)) (array->list (subarray arr '(2 1) #f))) ; select last two rows, in reverse order
  (test-equal '((7 8) (4 5)) (array->list (subarray arr '(2 1) '(0 1)))) ; select first two columns of last two rows in reverse order
  )

;; trimming vectors removes items from one side or other
(test-assert (equal? #(1 2 3 4) (array-trim #(0 1 2 3 4) 1)))
(test-assert (equal? #(0 1 2) (array-trim #(0 1 2 3 4) -2)))

(let ((arr (list->array 2 #() '((1 2 3) (4 5 6) (7 8 9)))))
  (test-equal '((1 2 3) (4 5 6) (7 8 9)) (array->list arr))
  (test-equal '((4 5 6) (7 8 9)) (array->list (array-trim arr 1 0))) ; trim first row
  (test-equal '((2 3) (5 6) (8 9)) (array->list (array-trim arr 0 1))) ; trim first column
  (test-equal '((1 2 3) (4 5 6)) (array->list (array-trim arr -1 0))) ; trim last row
  (test-equal '((1 2) (4 5) (7 8)) (array->list (array-trim arr 0 -1))) ; trim last column
  (test-equal '((4 5) (7 8)) (array->list (array-trim arr 1 -1))) ; trim first row and last column
  (test-equal '((7)) (array->list (array-trim arr 2 -2)))) ; trim first two rows and last two columns


(test-end)

