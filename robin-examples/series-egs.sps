;; EXPERIMENTAL - only runs in Chibi Scheme currently

(import (scheme base)
        (scheme eval)
        (robin series)
        (slib format))

(define (try-it expr)
  (format #t "~a~&=> ~a~&" expr (eval expr (environment '(scheme base)
                                                        '(scheme inexact)
                                                        '(robin series)))))

;; Examples from Waters - Part 1

(try-it '(collect-sum (scan '(1 2 3 4))))


;; -- try first with lists

(try-it '(collect-sum (choose-if positive? (scan '(1 -2 3 -4)))))

; TODO Requires x to be operated on by multiple collectors
;(try-it '(let ((x (subseries (scan-range 100 0 2) 0 5)))
;           (values (collect x) (collect-sum x))))

(try-it '(collect (map-fn sqrt (scan '(4 9 16)))))

(try-it '(collect (scan-fn (lambda () 3) (lambda (n) (- n 1)) negative?)))

(try-it '(collect-fn (lambda () 3) + (scan '(1 2 3))))

(try-it '(collect (positions (scan '(a #f b c #f #f)))))

(try-it '(collect (choose (scan '(#f #t #t #f)) (scan '(1 2 3 4)))))

;; -- try with vectors

(try-it '(collect-sum (choose-if positive? (scan #(1 -2 3 -4)))))