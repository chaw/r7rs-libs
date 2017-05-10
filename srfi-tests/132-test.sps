;; Test suite from SRFI 132 put into SRFI 64 style

(import (scheme base)
        (srfi 132)
        (srfi 64))

(test-begin "srfi-132")

(test-assert (list-sorted? > '()))

(test-assert (list-sorted? > '(987)))

(test-assert (list-sorted? > '(9 8 7)))

(test-assert (vector-sorted? > '#()))

(test-assert (vector-sorted? > '#(987)))

(test-assert (vector-sorted? > '#(9 8 7 6 5)))

(test-assert (vector-sorted? > '#() 0))

(test-assert (vector-sorted? > '#(987) 1))

(test-assert (vector-sorted? > '#(9 8 7 6 5) 1))

(test-assert (vector-sorted? > '#() 0 0))

(test-assert (vector-sorted? > '#(987) 1 1))

(test-assert (vector-sorted? > '#(9 8 7 6 5) 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (list-sort > (list))
            '())

(test-equal (list-sort > (list 987))
            '(987))

(test-equal (list-sort > (list 987 654))
            '(987 654))

(test-equal (list-sort > (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 7 6 5 4 3 2 1 0))

(test-equal (list-stable-sort > (list))
            '())

(test-equal (list-stable-sort > (list 987))
            '(987))

(test-equal (list-stable-sort > (list 987 654))
            '(987 654))

(test-equal (list-stable-sort > (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 7 6 5 4 3 2 1 0))

(test-equal (list-stable-sort (lambda (x y)
                                 (> (quotient x 2)
                                    (quotient y 2)))
                               (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 6 7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort > v))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort > (vector 987)))
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-sort > v))
            '#(987 654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort > v))
            '#(9 8 7 6 5 4 3 2 1 0))

(test-equal (let ((v (vector)))
              (vector-stable-sort > v))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort > (vector 987)))
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort > v))
            '#(987 654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort > v))
            '#(9 8 7 6 5 4 3 2 1 0))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v))
            '#(9 8 6 7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort > v 0))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort > (vector 987) 1))
            '#())

#;(test-equal (let ((v (vector 987 654)))
              (vector-sort > v 1))
            '#(654))

#;(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort > v 3))
            '#(7 5 4 3 2 1 0))

(test-equal (let ((v (vector)))
              (vector-stable-sort > v 0))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort > (vector 987) 1))
            '#())

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort < v 0 2))
            '#(654 987))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort > v 3))
            '#(7 5 4 3 2 1 0))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v
                                   3))
            '#(7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort > v 0 0))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort > (vector 987) 1 1))
            '#())

(test-equal (let ((v (vector 987 654)))
              (vector-sort > v 1 2))
            '#(654))

#;(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort > v 4 8))
            '#(5 4 2 0))

(test-equal (let ((v (vector)))
              (vector-stable-sort > v 0 0))
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort > (vector 987) 1 1))
            '#())

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort > v 1 2))
            '#(654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort > v 2 6))
            '#(6 4 3 0))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v
                                   1
                                   8))
            '#(8 6 4 5 3 2 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (list-sort! > (list))
            '())

(test-equal (list-sort! > (list 987))
            '(987))

(test-equal (list-sort! > (list 987 654))
            '(987 654))

(test-equal (list-sort! > (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 7 6 5 4 3 2 1 0))

(test-equal (list-stable-sort! > (list))
            '())

(test-equal (list-stable-sort! > (list 987))
            '(987))

(test-equal (list-stable-sort! > (list 987 654))
            '(987 654))

(test-equal (list-stable-sort! > (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 7 6 5 4 3 2 1 0))

(test-equal (list-stable-sort! (lambda (x y)
                                 (> (quotient x 2)
                                    (quotient y 2)))
                               (list 9 8 6 3 0 4 2 5 7 1))
            '(9 8 6 7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort! > v)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort! > (vector 987))
              v)
            '#(987))

#;(test-equal (let ((v (vector 987 654)))
              (vector-sort! > v)
              v)
            '#(987 654))

#;(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort! > v)
              v)
            '#(9 8 7 6 5 4 3 2 1 0))

(test-equal (let ((v (vector)))
              (vector-stable-sort! > v)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort! > (vector 987))
              v)
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort! > v)
              v)
            '#(987 654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! > v)
              v)
            '#(9 8 7 6 5 4 3 2 1 0))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v)
              v)
            '#(9 8 6 7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort! > v 0)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort! > (vector 987) 1)
              v)
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-sort! > v 1)
              v)
            '#(987 654))

#;(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort! > v 3)
              v)
            '#(9 8 6 7 5 4 3 2 1 0))

#;(test-equal (let ((v (vector)))
              (vector-stable-sort! > v 0)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort! > (vector 987) 1)
              v)
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort! < v 0 2)
              v)
            '#(654 987))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! > v 3)
              v)
            '#(9 8 6 7 5 4 3 2 1 0))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v
                                   3)
              v)
            '#(9 8 6 7 4 5 3 2 0 1))

(test-equal (let ((v (vector)))
              (vector-sort! > v 0 0)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-sort! > (vector 987) 1 1)
              v)
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-sort! > v 1 2)
              v)
            '#(987 654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-sort! > v 4 8)
              v)
            '#(9 8 6 3 5 4 2 0 7 1))

(test-equal (let ((v (vector)))
              (vector-stable-sort! > v 0 0)
              v)
            '#())

(test-equal (let ((v (vector 987)))
              (vector-stable-sort! > (vector 987) 1 1)
              v)
            '#(987))

(test-equal (let ((v (vector 987 654)))
              (vector-stable-sort! > v 1 2)
              v)
            '#(987 654))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! > v 2 6)
              v)
            '#(9 8 6 4 3 0 2 5 7 1))

(test-equal (let ((v (vector 9 8 6 3 0 4 2 5 7 1)))
              (vector-stable-sort! (lambda (x y)
                                     (> (quotient x 2)
                                        (quotient y 2)))
                                   v
                                   1
                                   8)
              v)
            '#(9 8 6 4 5 3 2 0 7 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (list-merge > (list) (list))
            '())

(test-equal (list-merge > (list) (list 9 6 3 0))
            '(9 6 3 0))

(test-equal (list-merge > (list 9 7 5 3 1) (list))
            '(9 7 5 3 1))

(test-equal (list-merge > (list 9 7 5 3 1) (list 9 6 3 0))
            '(9 9 7 6 5 3 3 1 0))

(test-equal (list-merge! > (list) (list))
            '())

(test-equal (list-merge! > (list) (list 9 6 3 0))
            '(9 6 3 0))

(test-equal (list-merge! > (list 9 7 5 3 1) (list))
            '(9 7 5 3 1))

(test-equal (list-merge! > (list 9 7 5 3 1) (list 9 6 3 0))
            '(9 9 7 6 5 3 3 1 0))

(test-equal (vector-merge > (vector) (vector))
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0))
            '#(9 6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector))
            '#(9 7 5 3 1))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0))
            '#(9 9 7 6 5 3 3 1 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector))
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0))
              v)
            '#( 9  6  3  0 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector))
              v)
            '#( 9  7  5  3  1 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0))
              v)
            '#( 9  9  7  6  5  3  3  1  0 #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 0)
              v)
            '#( 9  6  3  0 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 0)
              v)
            '#( 9  7  5  3  1 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 0)
              v)
            '#( 9  9  7  6  5  3  3  1  0 #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2)
              v)
            '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2)
              v)
            '#(#f #f  9  7  5  3  1 #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
              v)
            '#(#f #f 9  9  7  6  5  3  3  1  0 #f))

(test-equal (vector-merge > (vector) (vector) 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0)
            '#(9 6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2)
            '#(5 3 1))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2)
            '#(9 6 5 3 3 1 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0)
              v)
            '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2)
              v)
            '#(#f #f 5  3  1 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2)
              v)
            '#(#f #f  9   6  5  3  3  1  0 #f #f #f))

(test-equal (vector-merge > (vector) (vector) 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0)
            '#(9 6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 5)
            '#(5 3 1))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 5)
            '#(9 6 5 3 3 1 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
              v)
            '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 5)
              v)
            '#(#f #f 5  3  1 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 5)
              v)
            '#(#f #f  9  6  5  3  3  1  0 #f #f #f))

;;; Some tests are duplicated to make the pattern easier to discern.

(test-equal (vector-merge > (vector) (vector) 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0)
            '#(9 6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 4)
            '#(5 3))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4)
            '#(9 6 5 3 3 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0)
              v)
            '#(#f #f 9  6  3  0 #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4)
              v)
            '#(#f #f 5  3 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4)
              v)
            '#(#f #f  9  6  5  3  3  0 #f #f #f #f))

(test-equal (vector-merge > (vector) (vector) 0 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0 0)
            '#(9 6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
            '#(5 3))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 0)
            '#(9 6 5 3 3 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 0)
              v)
            '#(#f #f  9  6  3  0 #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
              v)
            '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 0)
              v)
            '#(#f #f  9  6  5  3  3  0 #f #f #f #f))

(test-equal (vector-merge > (vector) (vector) 0 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0 1)
            '#(6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0)
            '#(5 3))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1)
            '#(6 5 3 3 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1)
              v)
            '#(#f #f  6  3  0 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0)
              v)
            '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1)
              v)
            '#(#f #f  6  5  3  3  0 #f #f #f #f #f))

(test-equal (vector-merge > (vector) (vector) 0 0 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 4)
            '#(6 3 0))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
            '#(5 3))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 4)
            '#(6 5 3 3 0))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 4)
              v)
            '#(#f #f  6  3  0 #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
              v)
            '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 4)
              v)
            '#(#f #f  6  5  3  3  0 #f #f #f #f #f))

(test-equal (vector-merge > (vector) (vector) 0 0 0 0)
            '#())

(test-equal (vector-merge > (vector) (vector 9 6 3 0) 0 0 1 2)
            '#(6))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector) 2 4 0 0)
            '#(5 3))

(test-equal (vector-merge > (vector 9 7 5 3 1) (vector 9 6 3 0) 2 4 1 2)
            '#(6 5 3))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector) 2 0 0 0 0)
              v)
            '#(#f #f #f #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector) (vector 9 6 3 0) 2 0 0 1 2)
              v)
            '#(#f #f  6 #f #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector) 2 2 4 0 0)
              v)
            '#(#f #f  5  3 #f #f #f #f #f #f #f #f))

(test-equal (let ((v (make-vector 12 #f)))
              (vector-merge! > v (vector 9 7 5 3 1) (vector 9 6 3 0) 2 2 4 1 2)
              v)
            '#(#f #f  6  5  3 #f #f #f #f #f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (list-delete-neighbor-dups char=? (list))
            '())

(test-equal (list-delete-neighbor-dups char=? (list #\a))
            '(#\a))

(test-equal (list-delete-neighbor-dups char=? (list #\a #\a #\a #\b #\b #\a))
            '(#\a #\b #\a))

(test-equal (list-delete-neighbor-dups! char=? (list))
            '())

(test-equal (list-delete-neighbor-dups! char=? (list #\a))
            '(#\a))

(test-equal (list-delete-neighbor-dups! char=? (list #\a #\a #\a #\b #\b #\a))
            '(#\a #\b #\a))

(test-equal (let ((v (vector)))
              (vector-delete-neighbor-dups char=? v))
            '#())

(test-equal (let ((v (vector #\a)))
              (vector-delete-neighbor-dups char=? v))
            '#(#\a))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (vector-delete-neighbor-dups char=? v))
            '#(#\a #\b #\a))

(test-equal (let ((v (vector)))
              (list (vector-delete-neighbor-dups! char=? v) v))
            '(0 #()))

(test-equal (let ((v (vector #\a)))
              (list (vector-delete-neighbor-dups! char=? v) v))
            '(1 #(#\a)))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (list (vector-delete-neighbor-dups! char=? v) v))
            '(3 #(#\a #\b #\a #\b #\b #\a)))

(test-equal (let ((v (vector)))
              (vector-delete-neighbor-dups char=? v 0))
            '#())

(test-equal (let ((v (vector #\a)))
              (vector-delete-neighbor-dups char=? v 0))
            '#(#\a))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (vector-delete-neighbor-dups char=? v 0))
            '#(#\a #\b #\a))

(test-equal (let ((v (vector)))
              (list (vector-delete-neighbor-dups! char=? v 0) v))
            '(0 #()))

(test-equal (let ((v (vector #\a)))
              (list (vector-delete-neighbor-dups! char=? v 0) v))
            '(1 #(#\a)))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (list (vector-delete-neighbor-dups! char=? v 0) v))
            '(3 #(#\a #\b #\a #\b #\b #\a)))

(test-equal (let ((v (vector)))
              (vector-delete-neighbor-dups char=? v 0))
            '#())

(test-equal (let ((v (vector #\a)))
              (vector-delete-neighbor-dups char=? v 1))
            '#())

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (vector-delete-neighbor-dups char=? v 3))
            '#(#\b #\a))

(test-equal (let ((v (vector)))
              (list (vector-delete-neighbor-dups! char=? v 0) v))
            '(0 #()))

(test-equal (let ((v (vector #\a)))
              (list (vector-delete-neighbor-dups! char=? v 1) v))
            '(1 #(#\a)))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (list (vector-delete-neighbor-dups! char=? v 3) v))
            '(5 #(#\a #\a #\a #\b #\a #\a)))

(test-equal (let ((v (vector)))
              (vector-delete-neighbor-dups char=? v 0 0))
            '#())

(test-equal (let ((v (vector #\a)))
              (vector-delete-neighbor-dups char=? v 1 1))
            '#())

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (vector-delete-neighbor-dups char=? v 3 5))
            '#(#\b))

(test-equal (let ((v (vector)))
              (list (vector-delete-neighbor-dups! char=? v 0 0) v))
            '(0 #()))

(test-equal (let ((v (vector #\a)))
              (list (vector-delete-neighbor-dups! char=? v 0 1) v))
            '(1 #(#\a)))

(test-equal (let ((v (vector #\a)))
              (list (vector-delete-neighbor-dups! char=? v 1 1) v))
            '(1 #(#\a)))

(test-equal (let ((v (vector #\a #\a #\a #\b #\b #\a)))
              (list (vector-delete-neighbor-dups! char=? v 3 5) v))
            '(4 #(#\a #\a #\a #\b #\b #\a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (vector-find-median < (vector) "knil")
            "knil")

(test-equal (vector-find-median < (vector 17) "knil")
            17)

(test-equal (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil")
            12)

(test-equal (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil")
            23/2)

(test-equal (vector-find-median < (vector 18 1 12 14 12 5 18 2) "knil" list)
            (list 12 12))

(test-equal (vector-find-median < (vector 18 1 11 14 12 5 18 2) "knil" list)
            (list 11 12))

(test-equal (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil")
            7)

(test-equal (vector-find-median < (vector 7 6 9 3 1 18 15 7 8) "knil" list)
            7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (let ((v (vector 19)))
              (vector-select! < v 0))
            19)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0))
            3)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2))
            9)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 8))
            22)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 9))
            23)

(test-equal (let ((v (vector 19)))
              (vector-select! < v 0 0))
            19)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0 0))
            3)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2 0))
            9)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 8 0))
            22)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 9 0))
            23)

(test-equal (let ((v (vector 19)))
              (vector-select! < v 0 0 1))
            19)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0 0 10))
            3)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2 0 10))
            9)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 8 0 10))
            22)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 9 0 10))
            23)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0 4 10))
            3)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2 4 10))
            13)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 4 4 10))
            21)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 5 4 10))
            23)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0 4 10))
            3)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2 4 10))
            13)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 3 4 10))
            13)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 4 4 10))
            21)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 5 4 10))
            23)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 0 4 8))
            9)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 1 4 8))
            13)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 2 4 8))
            13)

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-select! < v 3 4 8))
            21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-equal (let ((v (vector)))
              (vector-separate! < v 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 1)
              (vector-sort < (vector-copy v 0 1)))
            '#(19))

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 3)
              (vector-sort < (vector-copy v 0 3)))
            '#(3 8 9))

(test-equal (let ((v (vector)))
              (vector-separate! < v 0 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 0 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 1 0)
              (vector-sort < (vector-copy v 0 1)))
            '#(19))

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 0 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 3 0)
              (vector-sort < (vector-copy v 0 3)))
            '#(3 8 9))

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 0 1)
              (vector-sort < (vector-copy v 1 1)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 0 2)
              (vector-sort < (vector-copy v 2 2)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 3 2)
              (vector-sort < (vector-copy v 2 5)))
            '#(3 9 13))

(test-equal (let ((v (vector)))
              (vector-separate! < v 0 0 0)
              (vector-sort < (vector-copy v 0 0)))
            '#())

(test-equal (let ((v (vector 19)))
              (vector-separate! < v 0 1 1)
              (vector-sort < (vector-copy v 1 1)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 0 2 8)
              (vector-sort < (vector-copy v 2 2)))
            '#())

(test-equal (let ((v (vector 8 22 19 19 13 9 21 13 3 23)))
              (vector-separate! < v 3 2 8)
              (vector-sort < (vector-copy v 2 5)))
            '#(9 13 13))

(test-end)

