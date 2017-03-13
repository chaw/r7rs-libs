;;;; "priorque.scm" priority queues for Scheme.
;;; Copyright (C) 1992, 1993, 1994, 1995, 1997 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017

;;@code{(require 'priority-queue)}
;;@ftindex priority-queue
;;
;;@noindent
;;This algorithm for priority queues is due to
;;@cite{Introduction to Algorithms}
;;by T. Cormen, C. Leiserson, R. Rivest.
;;1989 MIT Press.

(define-library
  (slib priority-queue)
  (export make-heap
          priority-queue? ; extra export
          heap-length
          heap-insert!
          heap-extract-max!)
  (import (scheme base)
          (slib common))

  (begin

    (define-record-type <priority-queue>
                        (priority-queue array size pred<?)
                        priority-queue?
                        (array pqueue-array pqueue-array-set!)
                        (size pqueue-size pqueue-size-set!)
                        (pred<? pqueue-pred<?))

    (define (heap:set-size! pqueue size)
      (let ((ra (pqueue-array pqueue)))
        (when (> size (vector-length ra))
          (let ((nra (make-vector (+ size (quotient size 2)))))
            (do ((i (- (vector-length ra) 1) (- i 1)))
              ((negative? i) (pqueue-array-set! pqueue nra))
              (vector-set! nra i (vector-ref ra i))))))
      (pqueue-size-set! pqueue size))

    ;; Reference an element.
    (define (heap:ref pqueue idx)
      (vector-ref (pqueue-array pqueue) (- idx 1)))

    ;; Set an element.
    (define (heap:set! pqueue idx value)
      (vector-set! (pqueue-array pqueue) (- idx 1) value))

    ;; Retain binary heap property
    (define (heap:heapify a i)
      (let* ((l (heap:left i))
             (r (heap:right i))
             (largest (if (and (<= l (pqueue-size a))
                               ((pqueue-pred<? a) (heap:ref a i) (heap:ref a l)))
                        l
                        i)))
        (cond ((and (<= r (pqueue-size a))
                    ((pqueue-pred<? a) (heap:ref a largest) (heap:ref a r)))
               (set! largest r)))
        (cond ((not (= largest i))
               (heap:exchange a i largest)
               (heap:heapify a largest)))))

    ;; Exchange two elements.
    (define (heap:exchange pqueue idx-i idx-j)
      (let* ((i (- idx-i 1))
            (j (- idx-j 1))
            (arr (pqueue-array pqueue))
            (tmp (vector-ref arr i)))
        (vector-set! arr i (vector-ref arr j))
        (vector-set! arr j tmp)))

    (define (heap:parent i) (quotient i 2))
    (define (heap:left i) (* 2 i))
    (define (heap:right i) (+ 1 (* 2 i)))

    ;;;; Externals

    ;;@body
    ;;Returns a binary heap suitable for priority queue operations.
    (define (make-heap pred<?)
      (priority-queue (make-vector 4) 0 pred<?))

    ;;@args heap
    ;;Returns the number of elements in @1.
    (define heap-length pqueue-size)

    ;;@args heap item
    ;;Inserts @2 into @1.  @2 can be inserted multiple
    ;;times.  The value returned is unspecified.
    (define (heap-insert! pqueue item)
      (let ((i (+ 1 (pqueue-size pqueue))))
        (heap:set-size! pqueue i)
        (do ()
          ((not (and (> i 1)
                     ((pqueue-pred<? pqueue) (heap:ref pqueue (heap:parent i))
                                             item))))
          (heap:set! pqueue i (heap:ref pqueue (heap:parent i)))
          (set! i (heap:parent i)))
        (heap:set! pqueue i item)))


    ;;@args heap
    ;;Returns the item which is larger than all others according to the
    ;;@var{pred<?} argument to @code{make-heap}.  If there are no items in
    ;;@1, an error is signaled.
    (define (heap-extract-max! pqueue)
      (if (< (pqueue-size pqueue) 1)
        (slib:error "heap underflow" pqueue)
        (let ((max (heap:ref pqueue 1)))
          (heap:set! pqueue 1 (heap:ref pqueue (pqueue-size pqueue)))
          (heap:set-size! pqueue (+ -1 (pqueue-size pqueue)))
          (heap:heapify pqueue 1)
          max)))

    ))

