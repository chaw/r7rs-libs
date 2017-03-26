;; Test cases for Series library

(import (scheme base)
        (scheme case-lambda)
        (scheme inexact)
        (robin series)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "robin-series")

;; examples with series
(test-equal '(a b a b a b) (collect (subseries (series 'a 'b) 0 6)))
(test-equal '(a b c d) (collect (make-series 'a 'b 'c 'd)))

;; testing all functions with lists

;; *** scanners
(test-equal '(3 2 1 0) (collect (scan-fn (lambda () 3) (lambda (n) (- n 1)) negative?)))
(test-equal '(3 2 1 0 -1) (collect (scan-fn-inclusive (lambda () 3) (lambda (n) (- n 1)) negative?)))

(test-equal '((a b c) (b c) (c)) (collect (scan-sublists '(a b c))))

(let-values (((keys vals) (scan-alist '((a . 2) (b . 4) (c . 8)))))
            (test-equal '(a b c) (collect keys))
            (test-equal '(2 4 8) (collect vals)))

;; *** transducers
(test-equal '(0 2 3) (collect (positions (scan '(a #f b c #f #f)))))
(test-equal '(1 3) (collect (positions (scan '(#f a #f b #f)))))
(test-equal '(2 3) (collect (choose (scan '(#f #t #t #f)) (scan '(1 2 3 4)))))
(test-equal 4 (collect-sum (choose-if positive? (scan '(1 -2 3 -4)))))
(for-each test-approx-same '(2 3 4) (collect (map-fn sqrt (scan '(4 9 16)))))

(test-equal '(1 2) (collect (until (scan '(#f #f #t #f #t)) (scan '(1 2 -3 4 -5)))))
(test-equal '(1 2) (collect (until-if negative? (scan '(1 2 -3 4 -5)))))
;(let-values (((fst snd) (cotruncate (scan '(1 2 -3 4 -5)) (scan '(10)))))
;            (test-equal '(1) (collect fst))
;            (test-equal '(10) (collect snd)))

(test-equal '(#f a b) (collect (previous (scan '(a b c)))))
(test-equal '(z a b) (collect (previous (scan '(a b c)) 'z)))
(test-equal '(z z a) (collect (previous (scan '(a b c)) 'z 2)))
(test-equal '() (collect (previous (scan '()))))

(test-equal '(-1 2 -1 4) (collect (spread (scan '(1 1)) (scan '(2 4)) -1)))
(test-equal '(a #f #f b) (collect (spread (scan '(0 2 4)) (scan '(a b)))))
(test-equal '(#f a) (collect (spread (scan '(1)) (scan '(a b)))))

(test-equal '(#f a #f b c) (collect (expand (scan '(#f #t #f #t #t)) (scan '(a b c)))))
(test-equal '(#f a #f) (collect (expand (scan '(#f #t #f #t #t)) (scan '(a)))))
(test-equal '(z a) (collect (expand (scan '(#f #t)) (scan '(a b c)) 'z)))
(test-equal '(#f) (collect (expand (scan '(#f #t #f #t #t)) (scan '()))))

(test-equal '(b c d) (collect (catenate (scan '(b c)) (scan '()) (scan '(d)))))
(test-equal '() (collect (catenate (scan '()) (scan '()))))

(test-equal '(b c d) (collect (subseries (scan '(a b c d)) 1)))
(test-equal '(b c) (collect (subseries (scan '(a b c d)) 1 3)))

(test-equal '(#f #f #f #f) (collect (subseries (mask (scan '())) 0 4)))
(test-equal '(#t #f #t #t #f #f) (collect (subseries (mask (scan '(0 2 3))) 0 6)))
(test-equal '(#f #t #f #t #f #f) (collect (subseries (mask (positions (scan '(#f a #f b #f)))) 0 6)))

(test-equal '(1 3 4 5 7 8 9) (collect (mingle (scan '(1 3 7 9)) (scan '(4 5 8)) <)))
(test-equal '(1 4 5 7 3 8 9) (collect (mingle (scan '(1 7 3 9)) (scan '(4 5 8)) <)))

;; *** collectors

;; -- collect-last
(test-equal 'c (collect-last (scan '(a b c))))
(test-equal #f (collect-last (scan '())))
(test-equal 'z (collect-last (scan '()) 'z))

;; -- collect-first
(test-equal 'a (collect-first (scan '(a b c))))
(test-equal #f (collect-first (scan '())))
(test-equal 'z (collect-first (scan '()) 'z))

;; -- collect-nth
(test-equal 'b (collect-nth 1 (scan '(a b c))))
(test-equal #f (collect-nth 1 (scan '())))
(test-equal 'z (collect-nth 1 (scan '()) 'z))

;; -- collect-alist
(test-equal '() (collect-alist (scan '(a b)) (scan '())))
(test-equal '((b . 2) (a . 1)) (collect-alist (scan '(a b)) (scan '(1 2))))
(test-equal '((a . 3) (b . 2) (a . 1)) (collect-alist (scan '(a b a)) (scan '(1 2 3 4))))

;; -- collect-length
(test-equal 4 (collect-length (scan '(1 2 3 4))))

;; -- collect-max
(test-equal #f (collect-max (scan '(2 1 4 3)) (scan '())))
(test-equal 0 (collect-max (scan '()) (scan '()) 0))
(test-equal 4 (collect-max (scan '(2 1 4 3))))
(test-equal 'c (collect-max (scan '(1.2 1.1 1.4 1.3)) (scan '(a b c d))))

;; -- collect-min
(test-equal #f (collect-min (scan '(2 1 4 3)) (scan '())))
(test-equal 0 (collect-min (scan '()) (scan '()) 0))
(test-equal 1 (collect-min (scan '(2 1 4 3))))
(test-equal 'b (collect-min (scan '(1.2 1.1 1.4 1.3)) (scan '(a b c d))))

;; -- collect-sum
(test-equal 10 (collect-sum (scan '(1 2 3 4))))

;; -- collect-and
(test-equal #t (collect-and (scan '())))
(test-equal 'c (collect-and (scan '(a b c))))
(test-equal #f (collect-and (scan '(a #f c))))

;; -- collect-or
(test-equal #f (collect-or (scan '())))
(test-equal 'a (collect-or (scan '(a b c))))
(test-equal 'a (collect-or (scan '(a #f c))))
(test-equal 'b (collect-or (scan '(#f b c))))
(test-equal #f (collect-or (scan '(#f #f #f))))

;; -- collect-fn
(test-equal 9 (collect-fn (lambda () 3) + (scan '(1 2 3))))



;; tests with vectors
(test-equal 4 (collect-sum (choose-if positive? (scan #(1 -2 3 -4)))))

;; tests with strings
(test-equal 2 (collect-length (choose-if (lambda (c) (memv c '(#\a #\e #\i #\o #\u)))
                                         (scan "scheme"))))

(test-end)

