(import (scheme base)
        (slib queue)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-queue")

(define q (make-queue))

(test-assert (queue? q))
(test-assert (not (queue? 1)))
(test-assert (queue-empty? q))

(queue-push! q 2)

(test-assert (not (queue-empty? q)))
(test-equal 2 (queue-front q))
(test-equal 2 (queue-front q))
(test-equal 2 (queue-rear q))

(queue-push! q 4)

(test-assert (not (queue-empty? q)))
(test-equal 4 (queue-front q))
(test-equal 2 (queue-rear q))

(queue-push! q 6)

(test-assert (not (queue-empty? q)))
(test-equal 6 (queue-front q))
(test-equal 2 (queue-rear q))

(test-equal 6 (queue-pop! q))

(test-assert (not (queue-empty? q)))
(test-equal 4 (queue-front q))
(test-equal 2 (queue-rear q))

(enqueue! q 8)

(test-assert (not (queue-empty? q)))
(test-equal 4 (queue-front q))
(test-equal 8 (queue-rear q))

(test-equal '(4 2 8) (dequeue-all! q))

(test-assert (queue-empty? q))
(test-for-error (dequeue! q))
(test-for-error (queue-front q))
(test-for-error (queue-rear q))

(test-end)

