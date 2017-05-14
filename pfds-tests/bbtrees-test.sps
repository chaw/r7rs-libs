;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds bounded-balance-tree)
        (srfi 64)
        (robin srfi64-utils)
        (srfi 132))

(test-begin "pfds-bounded-balance-tree")

;; empty tree
(test-assert (bbtree? (make-bbtree <)))
(test-equal 0 (bbtree-size (make-bbtree <)))

;; bbtree-set
(let* ((tree1 (bbtree-set (make-bbtree <) 1 'a))
       (tree2 (bbtree-set tree1 2 'b))
       (tree3 (bbtree-set tree2 1 'c )))
  (test-equal 1 (bbtree-size tree1))
  (test-equal 'a (bbtree-ref tree1 1))
  (test-equal 2 (bbtree-size tree2))
  (test-equal 'b (bbtree-ref tree2 2))
  (test-equal 2 (bbtree-size tree3))
  (test-equal 'c (bbtree-ref tree3 1))
  (test-equal #f (bbtree-ref tree1 #xdeadbeef #f))
  (test-equal 'not-in (bbtree-ref tree1 #xdeadbeef 'not-in))
  (test-error (bbtree-ref tree3 20)))

;; bbtree-update
(let ((bb (alist->bbtree '(("foo" . 10) ("bar" . 12)) string<?))
      (add1 (lambda (x) (+ x 1))))
  (test-equal 11 (bbtree-ref (bbtree-update bb "foo" add1 0) "foo"))
  (test-equal 13 (bbtree-ref (bbtree-update bb "bar" add1 0) "bar"))
  (test-equal  1 (bbtree-ref (bbtree-update bb "baz" add1 0) "baz")))

;; bbtree-delete
(let* ((tree1 (bbtree-set (bbtree-set (bbtree-set (make-bbtree string<?) "a" 3)
                                      "b"
                                      8)
                          "c"
                          19))
       (tree2 (bbtree-delete tree1 "b"))
       (tree3 (bbtree-delete tree2 "a")))
  (test-equal 3 (bbtree-size tree1))
  (test-equal #t (bbtree-contains? tree1 "b"))
  (test-equal #t (bbtree-contains? tree1 "a"))
  (test-equal 2 (bbtree-size tree2))
  (test-equal #f (bbtree-contains? tree2 "b"))
  (test-equal #t (bbtree-contains? tree2 "a"))
  (test-equal 1 (bbtree-size tree3))
  (test-equal #f (bbtree-contains? tree3 "a"))
  (test-no-error (bbtree-delete (bbtree-delete tree3 "a") "a")))

;; bbtree-folds
(let ((bb (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
  ;; empty case
  (test-equal #t (bbtree-fold (lambda args #f) #t (make-bbtree >)))      
  (test-equal #t (bbtree-fold-right (lambda args #f) #t (make-bbtree >)))
  ;; associative operations
  (test-equal 20 (bbtree-fold (lambda (key value accum) (+ value accum)) 0 bb))
  (test-equal 20 (bbtree-fold-right (lambda (key value accum) (+ value accum)) 0 bb))
  ;; non-associative operations
  (test-equal '("foo" "baz" "bar")
              (bbtree-fold (lambda (key value accum) (cons key accum)) '() bb))
  (test-equal '("bar" "baz" "foo")
              (bbtree-fold-right (lambda (key value accum) (cons key accum)) '() bb)))

;; bbtree-map
(let ((empty (make-bbtree <))
      (bb (alist->bbtree '((#\a . foo) (#\b . bar) (#\c . baz) (#\d . quux))
                         char<?)))
  (test-equal 0 (bbtree-size (bbtree-map (lambda (x) 'foo) empty)))
  (test-equal '((#\a foo . foo) (#\b bar . bar) (#\c baz . baz) (#\d quux . quux))
              (bbtree->alist (bbtree-map (lambda (x) (cons x x)) bb)))
  (test-equal '((#\a . "foo") (#\b . "bar") (#\c . "baz") (#\d . "quux"))
              (bbtree->alist (bbtree-map symbol->string bb))))

;; conversion
(test-equal '() (bbtree->alist (make-bbtree <)))
(test-equal 0 (bbtree-size (alist->bbtree '() <)))
(test-equal '(("bar" . 12) ("baz" . 7) ("foo" . 1))
            (bbtree->alist (alist->bbtree '(("foo" . 1) ("bar" . 12) ("baz" . 7)) string<?)))
(let ((l '(48 2 89 23 7 11 78))
      (tree-sort  (lambda (< l)
                    (map car
                         (bbtree->alist
                           (alist->bbtree (map (lambda (x) (cons x 'dummy))
                                               l)
                                          <))))))
  (test-equal (list-sort < l) (tree-sort < l)))

;; bbtree-union
(let ((empty (make-bbtree char<?))
      (bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                              char<?))
      (bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                              char<?)))
  (test-equal 0 (bbtree-size (bbtree-union empty empty)))
  (test-equal (bbtree-size bbtree1)
              (bbtree-size (bbtree-union empty bbtree1)))
  (test-equal (bbtree-size bbtree1)
              (bbtree-size (bbtree-union bbtree1 empty)))
  (test-equal (bbtree-size bbtree1)
              (bbtree-size (bbtree-union bbtree1 bbtree1)))
  (test-equal '(#\e #\g #\i #\l #\p #\s #\u)
              (bbtree-keys (bbtree-union bbtree1 bbtree2)))
  ;; union favours values in first argument when key exists in both
  (let ((union (bbtree-union bbtree1 bbtree2)))
    (test-equal 105 (bbtree-ref union #\i))
    (test-equal 108 (bbtree-ref union #\l)))
  ;; check this holds on larger bbtrees
  (let* ((l (string->list "abcdefghijlmnopqrstuvwxyz"))
         (b1 (map (lambda (x) (cons x (char->integer x))) l))
         (b2 (map (lambda (x) (cons x #f)) l)))
    (test-equal b1
                (bbtree->alist (bbtree-union (alist->bbtree b1 char<?)
                                             (alist->bbtree b2 char<?))))))

;; bbtree-intersection
(let ((empty (make-bbtree char<?))
      (bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                              char<?))
      (bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                              char<?)))
  (test-equal 0 (bbtree-size (bbtree-intersection empty empty)))
  (test-equal 0 (bbtree-size (bbtree-intersection bbtree1 empty)))
  (test-equal 0 (bbtree-size (bbtree-intersection empty bbtree1)))
  (test-equal (bbtree-size bbtree1)
              (bbtree-size (bbtree-intersection bbtree1 bbtree1)))
  ;; intersection favours values in first set
  (test-equal '((#\i . 105) (#\l . 108))
              (bbtree->alist (bbtree-intersection bbtree1 bbtree2)))
  ;; check this holds on larger bbtrees
  (let* ((l (string->list "abcdefghijlmnopqrstuvwxyz"))
         (b1 (map (lambda (x) (cons x (char->integer x))) l))
         (b2 (map (lambda (x) (cons x #f)) l)))
    (test-equal b1
                (bbtree->alist (bbtree-intersection (alist->bbtree b1 char<?)
                                                    (alist->bbtree b2 char<?)))))
  ;; definition of intersection is equivalent to two differences
  (test-equal (bbtree->alist (bbtree-intersection bbtree1 bbtree2))
              (bbtree->alist
                (bbtree-difference bbtree1
                                   (bbtree-difference bbtree1 bbtree2)))))

;; bbtree-difference
(let ((empty (make-bbtree char<?))
      (bbtree1 (alist->bbtree '((#\g . 103) (#\u . 117) (#\i . 105) (#\l . 108) (#\e . 101))
                              char<?))
      (bbtree2 (alist->bbtree '((#\l . 8) (#\i . 5) (#\s . 15) (#\p . 12))
                              char<?)))
  (test-equal 0 (bbtree-size (bbtree-difference empty empty)))
  (test-equal 5 (bbtree-size (bbtree-difference bbtree1 empty)))
  (test-equal 0 (bbtree-size (bbtree-difference empty bbtree1)))
  (test-equal 0 (bbtree-size (bbtree-difference bbtree1 bbtree1)))
  (test-equal '((#\e . 101) (#\g . 103) (#\u . 117))
              (bbtree->alist (bbtree-difference bbtree1 bbtree2)))
  (test-equal '((#\p . 12) (#\s . 15))
              (bbtree->alist (bbtree-difference bbtree2 bbtree1))))

;; bbtree-indexing
(let* ((l (string->list "abcdefghijklmno"))
       (bb (alist->bbtree (map (lambda (x) (cons x #f)) l) char<?)))
  "tnerfgxukscjmwhaod yz"
  (test-equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
              (map (lambda (x) (bbtree-index bb x)) l))
  (test-error (bbtree-index bb #\z))
  (test-equal l
              (map (lambda (x)
                     (let-values (((k v) (bbtree-ref/index bb x)))
                                 k))
                   '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
  (test-error (bbtree-ref/index bb -1))
  (test-error (bbtree-ref/index bb 15)))

(test-end)

