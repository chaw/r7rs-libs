
(import (scheme base)
        (robin disjoint-set)
        (srfi 64)
        (scheme comparator))

(test-begin "robin-disjoint-set")

(let ((ds (make-disjoint-set eq? default-hash)))
  (test-equal 0 (disjoint-set:size ds)) ; starts empty
  (disjoint-set:make ds 'a)
  (test-equal 1 (disjoint-set:size ds))
  (disjoint-set:make ds 'b)
  (test-equal 2 (disjoint-set:size ds))
  (disjoint-set:make ds 'c)
  (test-equal 3 (disjoint-set:size ds)) ; added 3 disjoint sets
  ;; merge (a b), so we are left with 2 disjoint sets
  (disjoint-set:union ds (disjoint-set:find ds 'a) (disjoint-set:find ds 'b))
  (test-equal 2 (disjoint-set:size ds))
  (test-assert (or (eq? 'a (disjoint-set:find ds 'a))
                   (eq? 'b (disjoint-set:find ds 'a)))) ; representative item for (a b) either a or b
  (test-assert (or (eq? 'a (disjoint-set:find ds 'b))
                   (eq? 'b (disjoint-set:find ds 'b)))) ; representative item for (a b) either a or b
  (test-equal 'c (disjoint-set:find ds 'c)) ; only 1 option for representative item for (c)
  (test-equal 'item-not-found (disjoint-set:find ds 'd)) ; item not found
  (test-equal #f (disjoint-set:find ds 'd #f)) ; item not found with default

  )


(test-end)

