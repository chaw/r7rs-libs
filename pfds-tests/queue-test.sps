;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds queue)
        (only (srfi 1) fold)
        (srfi 64))

(test-begin "pfds-queue")

;; empty-queue
(test-assert (queue? (make-queue)))
(test-assert (queue-empty? (make-queue)))
(test-equal 0 (queue-length (make-queue)))

;; enqueue
(let ((queue (enqueue (make-queue) 'foo)))
  (test-assert (queue? queue))
  (test-equal #t (not (queue-empty? queue)))
  (test-equal 1 (queue-length queue))
  (test-equal 10 (queue-length
                   (fold (lambda (val queue)
                           (enqueue queue val))
                         (make-queue)
                         '(0 1 2 3 4 5 6 7 8 9)))))

;; dequeue
(let ((empty (make-queue))
      (queue1 (enqueue (make-queue) 'foo))
      (queue2 (enqueue (enqueue (make-queue) 'foo) 'bar)))
  (let-values (((item queue) (dequeue queue1)))
              (test-equal 'foo item)
              (test-assert (queue? queue))
              (test-assert (queue-empty? queue)))
  (let*-values (((first queue*) (dequeue queue2))
                ((second queue) (dequeue queue*)))
               (test-equal 'foo first)
               (test-equal 'bar second)
               (test-equal 1 (queue-length queue*))
               (test-equal 0 (queue-length queue)))
  (test-error (dequeue empty)))

;; queue-ordering
(let* ((list '(bar quux foo zot baz))
       (queue (list->queue list)))
  (test-equal 5 (queue-length queue))
  (test-equal list (queue->list queue)))

(test-end)
