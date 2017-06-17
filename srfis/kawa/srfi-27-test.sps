
(import (scheme base)
        (scheme list)
        (srfi 27)
        (srfi 64))

(define (n-ints n)
  (do ((i 0 (+ 1 i))
       (res '() (cons (random-integer 10) res)))
    ((= i n) res)))

(test-begin "kawa-srfi-27")

;; check integers created within range
(test-assert (every (lambda (i) (<= 0 i 10)) 
                    (n-ints 100)))

;; check adjusting state regenerates the same list of numbers
(let ((initial-state (random-source-state-ref default-random-source))
      (original-n (n-ints 100)))
  (random-source-state-set! default-random-source initial-state)
  (test-assert (every = 
                      original-n
                      (n-ints 100))))


(test-end)

