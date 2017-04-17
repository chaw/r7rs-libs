;;; Disjoint-set data structure:
;;;   a data structure to hold sets of items in disjoint sets, providing 
;;;   efficient procedures for finding a representative of the set any 
;;;   item is contained in, and also for joining two sets together

;; Written by Peter Lane, 2017
;; Modifications by Sudarshan S Chawathe, 2017.

;; # Open Works License
;; 
;; This is version 0.9.4 of the Open Works License
;; 
;; ## Terms
;; 
;; Permission is hereby granted by the holder(s) of copyright or other legal
;; privileges, author(s) or assembler(s), and contributor(s) of this work, to any
;; person who obtains a copy of this work in any form, to reproduce, modify,
;; distribute, publish, sell, sublicense, use, and/or otherwise deal in the
;; licensed material without restriction, provided the following conditions are
;; met:
;; 
;; Redistributions, modified or unmodified, in whole or in part, must retain
;; applicable copyright and other legal privilege notices, the above license
;; notice, these conditions, and the following disclaimer.
;; 
;; NO WARRANTY OF ANY KIND IS IMPLIED BY, OR SHOULD BE INFERRED FROM, THIS LICENSE
;; OR THE ACT OF DISTRIBUTION UNDER THE TERMS OF THIS LICENSE, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
;; AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS, ASSEMBLERS, OR HOLDERS OF
;; COPYRIGHT OR OTHER LEGAL PRIVILEGE BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER
;; LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT, OR OTHERWISE ARISING FROM, OUT
;; OF, OR IN CONNECTION WITH THE WORK OR THE USE OF OR OTHER DEALINGS IN THE WORK.

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
;;;    Input: a disjoint set, an item, and an optional default value (for if not found)
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
  (robin disjoint-set)
  (export make-disjoint-set
          disjoint-set?
          disjoint-set:make 
          disjoint-set:find 
          disjoint-set:union
          disjoint-set:size)
  (import (scheme base)
          (scheme case-lambda)
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

    ;; input: a disjoint-set, an item and an optional value to return if not found
    ;; effect: none - but path compression is used to optimise future searches
    ;; output: a representative item in set 
    (define disjoint-set:find
      (case-lambda
        ((set item)
         (disjoint-set:find set item 'item-not-found))
        ((set item default)
         (unless (disjoint-set? set)
           (error "disjoint-set:find Expected a disjoint-set for set" set))
         (let ((repn (hash-table-ref/default (disjoint-set:items set) item #f)))
           (if repn
             (if ((disjoint-set:item-equals? set) item (parent repn))
               (parent repn)
               (let ((res (disjoint-set:find set (parent repn))))
                 (parent-set! repn res) ; path compression 
                 res))
             default)))))

    ;; input: a disjoint-set and representatives of two sets
    ;; effect: modify disjoint-set so the two represented sets are equivalent
    ;; output: undefined
    (define (disjoint-set:union set item-1 item-2)
      (unless (disjoint-set? set)
        (error "disjoint-set:union Expected a disjoint-set for set" set))
      (let ((root-1 (hash-table-ref/default (disjoint-set:items set) (disjoint-set:find set item-1) #f))
            (root-2 (hash-table-ref/default (disjoint-set:items set) (disjoint-set:find set item-2) #f)))
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
      (let ((roots-ht (make-hash-table (disjoint-set:item-equals? set)
                                       (hash-table-hash-function 
                                         (disjoint-set:items set)))))
        (hash-table-walk (disjoint-set:items set)
                         (lambda (k v)
                           (hash-table-set! roots-ht
                                            (disjoint-set:find set k)
                                            #t)))
        (hash-table-size roots-ht)))

    ))

