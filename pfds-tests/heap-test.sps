;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds heap)
        (srfi 64)
        (scheme sort))

(test-begin "pfds-heap")

;; empty-heap
(test-assert (heap? (make-heap <)))
(test-assert (heap-empty? (make-heap <)))
(test-equal 0 (heap-size (heap <)))

;; heap-insertion
(let ((h1 (heap < 7 1 13 9 5 3 11))
      (h2 (heap < 4 2 8 10 6 0 12)))
  (test-equal (+ 1 (heap-size h1))
              (heap-size (heap-insert h1 0)))
  (test-equal (+ 1 (heap-size h1))
              (heap-size (heap-insert h1 1)))
  (test-equal '(1 2 3 5 7 9 11 13)
              (heap->list (heap-insert h1 2)))
  (test-equal '(1 3 4 5 7 9 11 13)
              (heap->list (heap-insert h1 4)))
  (test-equal '(1 3 5 7 9 11 12 13)
              (heap->list (heap-insert h1 12)))
  (test-equal '(1 3 5 7 9 11 13 100)
              (heap->list (heap-insert h1 100)))
  (test-equal '(-2 0 2 4 6 8 10 12)
              (heap->list (heap-insert h2 -2)))
  (test-equal '(0 0 2 4 6 8 10 12)
              (heap->list (heap-insert h2 0)))
  (test-equal '(0 2 4 6 8 8 10 12)
              (heap->list (heap-insert h2 8))))

;; heap-deletion
(let ((h1 (heap < 7 1 13 9 5 3 11))
      (h2 (heap < 4 2 8 6 0)))
  (test-equal (- (heap-size h1) 1)
              (heap-size (heap-delete-min h1)))
  (test-equal 1 (heap-min h1))
  (test-equal 0 (heap-min h2))
  (test-equal 1 (heap-min (heap-delete-min (heap-insert h1 -10))))
  (test-equal 3 (heap-size (heap-delete-min (heap-delete-min h2))))
  (test-equal 4 (heap-min (heap-delete-min (heap-delete-min h2))))
  (test-equal '(7 9 11 13)
              (heap->list
                (heap-delete-min (heap-delete-min (heap-delete-min h1)))))
  (test-error (heap-pop (make-heap <)))
  (test-error (heap-delete-min (make-heap <)))
  (test-error (heap-min (make-heap <))))

;; sorting
(let ((l1 '(129 109 146 175 229 48 225 239 129 41
            38 13 187 15 207 70 64 198 79 125))
      (l2 '(72 17 220 158 164 133 20 78 96 230 25
            19 13 17 58 223 37 214 94 195 93 174)))
             (test-equal '() (heap-sort < '()))
             (test-equal (list-sort < l1)
                         (heap-sort < l1))
             (test-equal (list-sort < l2)
                         (heap-sort < l2)))

(test-end)

