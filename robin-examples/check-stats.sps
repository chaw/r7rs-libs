;; simple program to visually check the random-normal function 

(import (scheme base)
        (robin statistics)
        (slib charplot))

(define mean 50)
(define sd 10)
(define num 100000)

(define (tests n res)
  (do ((i 0 (+ 1 i))
       (res '() (cons (round (random-normal mean sd)) res)))
    ((= i n) res)))

(define res (tests num '()))
(histograph res "random-normal")
