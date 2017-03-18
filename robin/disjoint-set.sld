;;; Disjoint-set data structure:
;;;   a data structure to hold sets of items in disjoint sets, providing 
;;;   efficient procedures for finding a representative of the set any 
;;;   item is contained in, and also for joining two sets together
;;; Uses disjoint-set forest data structure described at 
;;; http://en.wikipedia.org/wiki/Disjoint-set_data_structure

;;; The user must provide a hash procedure and equality check procedure for 
;;; the items to be stored in the data structure

;;; Exported procedures:
;;;
;;; make-disjoint-set
;;;    Input: two procedures, a hash function and equality test for the items
;;;           to hold within the disjoint set
;;;    Output: a reference to a disjoint-set object
;;; disjoint-set:make
;;;    Input: a disjoint set and an item
;;;    Effect: converts item into a disjoint set item and adds to the disjoint-set
;;;    Output: unspecified
;;; disjoint-set:find
;;;    Input: a disjoint set and an item
;;;    Output: a reference to the representative item of set that given item appears in
;;; disjoint-set:union
;;;    Input: a disjoint set and two representative items
;;;    Effect: modifies disjoint set, merging the sets represented by the given items
;;;    Output: unspecified
;;;
;;; Errors:
;;;
;;; An error condition is raised if a disjoint set or procedure
;;; is not provided as input to a procedure where required.

(define-library
  (disjoint-set)
  (export make-disjoint-set
          disjoint-set:make 
          disjoint-set:find 
          disjoint-set:union
          disjoint-set:size)
  (import (scheme base) (scheme write)
          (srfi 1)
          (srfi 69))

  (begin

    (define-record-type <disjoint-set>
                        (create-dj-set item-equals? items)
                        disjoint-set?
                        (item-equals? disjoint-set:item-equals?)
                        (items disjoint-set:items))

    (define (make-disjoint-set equality? hash-fn)
      (unless (and (procedure? hash-fn) 
                   (procedure? equality?))
        (error "make-disjoint-set expects two procedures as arguments"))
      (create-dj-set equality? (make-hash-table equality? hash-fn)))

    ;; define a record type for an item in the set: includes a parent reference and a rank
    ;; - parent reference is used to link back up to a representative of the item's set
    ;; - rank is used to record how many links away the representative is

    (define-record-type <disjoint-set-item>
                        (make-item parent item rank)
                        disjoint-set-item?
                        (parent parent parent-set!)
                        (item item)
                        (rank rank rank-set!))

    ;; input: a disjoint-set and an item
    ;; effect: include the item in the disjoint-set as its own group
    ;; output: undefined
    (define (disjoint-set:make set item)
      (hash-table-set! (disjoint-set:items set)
                       item 
                       (make-item item item 0)))

    ;; input: a disjoint-set and an item
    ;; effect: none - but path compression is used to optimise future searches
    ;; output: a representative item in set 
    (define (disjoint-set:find set item)
      (unless (disjoint-set? set)
        (error "disjoint-set:find Expected a disjoint-set for set" set))
      (let ((repn (hash-table-ref (disjoint-set:items set) item (lambda () #f))))
        (if repn
          (if ((disjoint-set:item-equals? set) item (parent repn))
            (parent repn)
            (let ((res (disjoint-set:find set (parent repn))))
              (parent-set! repn res) ; path compression 
              res))
          'item-not-found)))

    ;; input: a disjoint-set and representatives of two sets
    ;; effect: modify disjoint-set so the two represented sets are equivalent
    ;; output: undefined
    (define (disjoint-set:union set item-1 item-2)
      (unless (disjoint-set? set)
        (error "disjoint-set:union Expected a disjoint-set for set" set))
      (let ((root-1 (hash-table-ref (disjoint-set:items set) (disjoint-set:find set item-1) (lambda () #f)))
            (root-2 (hash-table-ref (disjoint-set:items set) (disjoint-set:find set item-2) (lambda () #f))))
        (unless root-1 
          (error "disjoint-set:union Item not found in disjoint-set items" item-1))
        (unless root-2
          (error "disjoint-set:union Item not found in disjoint-set items" item-2))
        ; perform union by rank - keeping tree as shallow as possible
        (cond ((> (rank root-1) (rank root-2))
               (parent-set! root-2 (parent root-1)))
              ((< (rank root-1) (rank root-2))
               (parent-set! root-1 (parent root-2)))
              (else
                (parent-set! root-2 (parent root-1))
                (rank-set! root-1 (+ 1 (rank root-1)))))))

    ;; input: a disjoint-set
    ;; output: returns the number of sets in the disjoint-set
    (define (disjoint-set:size set)
      (let ((parents '()))
        (hash-table-walk (disjoint-set:items set)
                         (lambda (k v)
                           (let ((repn (disjoint-set:find set k)))
                             (unless (member repn parents (disjoint-set:item-equals? set))
                               (set! parents (cons repn parents))))))
        (length parents)))

    ))

