;;; queues.sls --- Purely functional queues

;; Copyright (C) 2011,2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Packaged for R7RS Scheme by Peter Lane, 2017

;; ------------------------------------------------------------------

;;; Commentary:
;;
;; A scheme translation of "Simple and Efficient Purely Functional
;; Queues and Deques" by Chris Okazaki
;;
(define-library 
  (pfds queue)
  (export make-queue
          queue?
          queue-length
          queue-empty?
          enqueue
          dequeue
          list->queue
          queue->list
          )
  (import (scheme base)
          (only (pfds list-helpers) fold-left)
          (pfds lazy-list))

  (begin

    (define (rotate l r a)
      (if (empty? l)
        (cons* (head r) a)
        (cons* (head l)
               (rotate (tail l)
                       (tail r)
                       (cons* (head r) a)))))


    ;;; Implementation

    ;;> queue? : any -> boolean
    ;;> tests if an object is a queue
    ;;>
    ;;> queue-length : queue -> non-negative integer
    ;;> returns the number of items in the queue
    (define-record-type <queue>
                        (%make-queue len l r l^)
                        queue?
                        (len queue-length)
                        (l queue-l)
                        (r queue-r)
                        (l^ queue-l^))

    ;;> make-queue : () -> queue
    ;;> returns a queue containing no items
    (define (make-queue)
      (%make-queue 0 '() '() '()))

    ;;> enqueue : queue any -> queue
    ;;> returns a new queue with the enqueued item at the end
    (define (enqueue queue item)
      (let ((len (queue-length queue))
            (l (queue-l queue))
            (r (queue-r queue))
            (l^ (queue-l^ queue)))
        (makeq (+ len 1) l (cons* item r) l^)))

    ;;> dequeue : queue -> value queue
    ;;> returns two values, the item at the front of the queue, and a new
    ;;> queue containing the all the other items
    ;;> raises an error if the queue is empty
    (define (dequeue queue)
      (when (queue-empty? queue)
        (error 'dequeue  "Can't dequeue empty queue"))
      (let ((len (queue-length queue))
            (l (queue-l queue))
            (r (queue-r queue))
            (l^ (queue-l^ queue)))
        (values (head l)
                (makeq (- len 1) (tail l) r l^))))

    (define (makeq length l r l^)
      (if (empty? l^)
        (let ((l* (rotate l r '())))
          (%make-queue length l* '() l*))
        (%make-queue length l r (tail l^))))

    ;;> queue-empty? : queue -> boolean
    ;;> returns true if there are no items in the queue, false otherwise
    (define (queue-empty? queue)
      (zero? (queue-length queue)))

    ;;> list->queue : listof(any) -> queue
    ;;> returns a list containing all the items in the queue. The order of
    ;;> the items in the list is the same as the order in the queue.
    ;;> For any list l, (equal? (queue->list (list->queue l)) l) is #t.
    (define (list->queue list)
      (fold-left enqueue (make-queue) list))

    ;;> queue->list : queue -> listof(any)
    ;;> returns a queue containing all the items in the list. The order of
    ;;> the elements in the queue is the same as the order of the elements
    ;;> in the list.
    (define (queue->list queue)
      (let loop ((rev-list '()) (queue queue))
        (if (queue-empty? queue)
          (reverse rev-list)
          (let-values (((val queue) (dequeue queue)))
                      (loop (cons val rev-list)
                            queue)))))

    ))

