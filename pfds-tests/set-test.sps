;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds set)
        (only (pfds list-helpers) fold-left)
        (only (srfi 1) any every)
        (srfi 64)
        (robin srfi64-utils)
        (srfi 95))

(test-begin "pfds-set")

;; set-basics
(let ((empty (make-set string<?))
      (set (fold-left set-insert
                      (make-set string<?)
                      (list "foo" "bar" "baz" "quux" "zot"))))
  (test-assert (set? empty))
  (test-assert (set? set))
  (test-equal 0 (set-size empty))
  (test-equal 5 (set-size set))
  (test-equal #f (set-member? empty "foo"))
  (test-equal #t (set-member? (set-insert empty "foo") "foo"))
  (test-equal #t (set-member? set "foo"))
  (test-equal #f (set-member? (set-remove set "foo") "foo"))
  (test-no-error (set-remove empty "anything"))
  (test-no-error (set-insert set "quux"))
  (test-equal (set-size (set-insert empty "foo"))
              (set-size (set-insert (set-insert empty "foo") "foo")))
  (test-equal (set-size (set-remove set "foo"))
              (set-size (set-remove (set-remove set "foo") "foo"))))

;; set-equality
(let* ((empty (make-set string<?))
       (set1  (list->set '("foo" "bar" "baz") string<?))
       (set2  (list->set '("foo" "bar" "baz" "quux" "zot") string<?))
       (sets  (list empty set1 set2)))
  (test-assert (every (lambda (x) (set=? x x)) sets))
  (test-assert (every (lambda (x) (subset? x x)) sets))
  (test-assert (not (any (lambda (x) (proper-subset? x x)) sets)))
  (test-assert (set<? empty set1))
  (test-assert (set<? set1 set2))
  (test-assert (set=? (set-insert set1 "quux")
               (set-remove set2 "zot"))))

;; set-operations
(let* ((empty (make-set <))
       (set1 (list->set '(0 2 5 7 12 2 3 62 5) <))
       (set2 (list->set '(94 33 44 2 73 55 48 92 98 29
                          28 98 55 20 69 5 33 53 89 50)
                        <))
       (sets (list empty set1 set2)))
  (test-assert (every (lambda (x) (set=? x (set-union x x))) sets))
  (test-assert (every (lambda (x) (set=? x (set-intersection x x))) sets))
  (test-assert (every (lambda (x) (set=? empty (set-difference x x))) sets))
  (test-assert (every (lambda (x) (set=? x (set-union empty x))) sets))
  (test-assert (every (lambda (x) (set=? empty (set-intersection empty x))) sets))
  (test-assert (every (lambda (x) (set=? x (set-difference x empty))) sets))
  (test-assert (every (lambda (x) (set=? empty (set-difference empty x))) sets))

  (test-assert (set=? (set-union set1 set2) (set-union set2 set1)))
  (test-assert (set=? (set-union set1 set2)
               (list->set '(0 2 3 69 7 73 12 20 89 28
                            29 94 5 33 98 92 44 48 50 53
                            55 62)
                          <)))

  (test-assert (set=? (set-intersection set1 set2) (set-intersection set2 set1)))
  (test-assert (set=? (set-intersection set1 set2)
               (list->set '(2 5) <)))
  (test-assert (set=? (set-difference set1 set2)
               (list->set '(0 3 12 62 7) <)))
  (test-assert (set=? (set-difference set2 set1)
               (list->set '(33 98 69 73 44 48 92 50 20 53
                            55 89 28 29 94)
                          <))))

;; set-conversion 
(test-equal '() (set->list (make-set <)))
(test-equal 0 (set-size (list->set '() <)))
(test-equal (string->list "abcdefghijklmno")
            (sort (set->list
                    (list->set (string->list "abcdefghijklmno") char<?)) char<?))
(test-equal '(0) (set->list (fold-left set-insert (make-set <) '(0 0 0 0))))

;; set-iterators
(test-equal 0 (set-fold + 0 (list->set '() <)))
(test-equal 84 (set-fold + 0 (list->set '(3 12 62 7) <)))
(test-equal 499968 (set-fold * 1 (list->set '(3 12 62 7 8 4) <)))

(test-end)

