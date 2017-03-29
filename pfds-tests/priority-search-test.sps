;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base)
        (pfds priority-search-queue)
        (only (srfi 1) fold filter)
        (srfi 64)
        (srfi 95))

(define (alist->psq alist key<? priority<?)
  (fold (lambda (kv psq)
           (psq-set psq (car kv) (cdr kv)))
         (make-psq key<? priority<?)
         alist))

(define (add1 n) (+ 1 n))

(test-begin "pfds-priority-search-queue")

(test-assert (psq? (make-psq string<? <)))
(test-assert (psq-empty? (make-psq string<? <)))
(test-assert (zero? (psq-size (make-psq string<? <))))

;; psq-set
(let* ((empty (make-psq char<? <))
       (psq1  (psq-set empty #\a 10))
       (psq2  (psq-set psq1 #\b 33))
       (psq3  (psq-set psq2 #\c 3))
       (psq4  (psq-set psq3 #\a 12)))
  (test-equal 10 (psq-ref psq1 #\a))
  (test-error (psq-ref psq1 #\b))
  (test-equal 1 (psq-size psq1))

  (test-equal 10 (psq-ref psq2 #\a))
  (test-equal 33 (psq-ref psq2 #\b))
  (test-assert (not (psq-contains? psq2 #\c)))
  (test-equal 2 (psq-size psq2))

  (test-equal 10 (psq-ref psq3 #\a))
  (test-equal 33 (psq-ref psq3 #\b))
  (test-equal 3  (psq-ref psq3 #\c))
  (test-equal 3 (psq-size psq3))

  (test-equal 12 (psq-ref psq4 #\a))
  (test-equal 33 (psq-ref psq4 #\b))
  (test-equal 3  (psq-ref psq4 #\c))
  (test-equal 3 (psq-size psq4)))

;; psq-delete
(let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3))
                         char<?
                         <))
       (psq2 (psq-delete psq1 #\c))
       (psq3 (psq-delete psq2 #\b))
       (psq4 (psq-delete psq3 #\a))
       (psq5 (psq-delete psq1 #\d)))
  (test-equal #t (psq-contains? psq1 #\c))
  (test-assert (not (psq-contains? psq2 #\c)))
  (test-equal #t (psq-contains? psq2 #\b))
  (test-assert (not (psq-contains? psq3 #\b)))
  (test-equal #t (psq-contains? psq3 #\a))
  (test-assert (psq-empty? psq4))
  (test-equal (psq-size psq1)
              (psq-size psq5)))

;; psq-update
(let* ((empty (make-psq char<? <))
       (psq1  (psq-update empty #\a add1 10))
       (psq2  (psq-update psq1 #\b add1 33))
       (psq3  (psq-update psq2 #\c add1 3))
       (psq4  (psq-update psq3 #\a add1 0))
       (psq5  (psq-update psq3 #\c add1 0)))
  (test-equal 11 (psq-ref psq3 #\a))
  (test-equal 34 (psq-ref psq3 #\b))
  (test-equal 4  (psq-ref psq3 #\c))

  (test-equal 12 (psq-ref psq4 #\a))
  (test-equal 34 (psq-ref psq4 #\b))
  (test-equal 4  (psq-ref psq4 #\c))
  (test-equal 3  (psq-size psq4))

  (test-equal 11 (psq-ref psq5 #\a))
  (test-equal 34 (psq-ref psq5 #\b))
  (test-equal 5  (psq-ref psq5 #\c))
  (test-equal 3  (psq-size psq5)))

;; priority-queue-functions
(let* ((psq1 (alist->psq '((#\a . 10) (#\b . 33) (#\c . 3) (#\d . 23) (#\e . 7))
                         char<?
                         <))
       (psq2 (psq-delete-min psq1))
       (psq3 (psq-delete-min (psq-set psq2 #\b 9)))
       (psq4 (make-psq < <)))
  (test-equal #\c (psq-min psq1))
  (test-equal #\e (psq-min psq2))
  (test-error (psq-delete-min psq4))
  (test-equal #\a (psq-min (psq-set psq1 #\a 0)))
  (call-with-values
    (lambda ()
      (psq-pop psq3))
    (lambda (min rest)
      (test-equal #\b min)
      (test-equal #\a (psq-min rest)))))

;; ranged-functions
(let* ((alist '((#\f . 24) (#\u . 42) (#\p . 16) (#\s . 34) (#\e . 17)
                           (#\x . 45) (#\l . 14) (#\z . 5) (#\t . 45) (#\r . 41)
                           (#\k . 32) (#\w . 14) (#\d . 12) (#\c . 16) (#\m . 20) (#\j . 25)))
       (alist-sorted (sort alist (lambda (x y)
                             (char<? (car x) (car y)))))
       (psq  (alist->psq alist char<? <)))
  (test-equal alist-sorted
              (psq-at-most psq +inf.0))
  (test-equal '() (psq-at-most psq 0))
  (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                           (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
              (psq-at-most psq 20))
  (test-equal alist-sorted
              (psq-at-most-range psq +inf.0 #\x00 #\xFF))
  ;; with bounds outwith range in psq, is the same as psq-at-most
  (test-equal '() (psq-at-most-range psq 0 #\x00 #\xFF))
  (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                           (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
              (psq-at-most-range psq 20 #\x00 #\xFF))
  (test-equal '((#\c . 16) (#\d . 12) (#\e . 17) (#\l . 14)
                           (#\m . 20) (#\p . 16) (#\w . 14) (#\z . 5))
              (psq-at-most psq 20))
  (test-equal (filter (lambda (x) (char<=? #\e (car x) #\u)) alist-sorted)
              (psq-at-most-range psq +inf.0 #\e #\u))
  (test-equal '() (psq-at-most-range psq 0 #\e #\u))
  (test-equal '((#\e . 17) (#\l . 14) (#\m . 20) (#\p . 16))
              (psq-at-most-range psq 20 #\e #\u))
  ;; inclusiveness check
  (test-equal '((#\t . 45))
              (psq-at-most-range psq 80 #\t #\t))
  ;; if lower bound is higher than upper, then nothing
  (test-equal '() (psq-at-most-range psq 80 #\t #\r)))

(test-end)

