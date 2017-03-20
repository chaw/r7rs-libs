;;; filename: test-ngrams.scm
;;;
;;; Time-stamp: <2010-02-28 01:15:00 dcavar>
;;; encoding: UTF-8
;;;
;;; Copyright (C) 2010 by Damir Ćavar. 
;;;
;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the Creative Commons
;;; GNU Lesser General Public License as published by the
;;; Free Software Foundation; either version 2.1 of the License,
;;; or (at your option) any later version.
;;;
;;; The Scheme NLTK is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the Creative Commons GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;; Author: Damir Ćavar <dcavar@unizd.hr>
;;
;;
;; Commentary:
;; 
;; TODO:

(import (scheme base)
        (scheme write)
        (nltk n-grams))


(define test1
  (lambda ()
    (ngrams->dot-digraph (token-sequence->ngrams '(1 2 3 4 5 6 7 8) 2))))


(define test2
  (lambda ()
    (vector-for-each
     (lambda (item)
       (display (vector-ref item 0))(display "\t")
       (display (vector-ref item 1))(newline))
     (frequency-profile-decreasing (token-sequence->ngrams
                                    '("John" "saw" "the" "new" "house" "of" "the" "new" "landlords"
                                             "who" "live" "in" "the" "new" "part" "of" "the" "city")
                                    2)))))


(define test3
  (lambda (n)
    (display "n = ")(display n)(newline)
    (let ((result (ngrams-sort
                   (ngrams->bigrams
                    (token-sequence->ngrams '(1 2 3 4 5 6 7 8 9 10 11 1 2 3 4 5 6 7 8 9 10 11) n)) #t #t)))
          (display (vector-map
                    (lambda (item)
                      ;if val > (n - 2)
                      ;   val = val / (n - 1)
                      (display item)(newline)
                      (let ((val (if (> (vector-ref item 1) (- n 2))
                                     (/ (vector-ref item 1) (- n 1))
                                     (vector-ref item 1))))
                        (vector (vector-ref item 0) val)))
                    result))(newline)
          result)))



(display "Running test1: DOT")(newline)
(display (test1))
(newline)
(display "Running test2: HTML table")(newline)
(test2)
(newline)
(display "Running test3: Decreasing frequency profile")(newline)
(newline)
(display (test3 2))
(newline)

