;;; filename: test-sequences.scm
;;;
;;; Time-stamp: <2010-02-28 01:15:00 dcavar>
;;; encoding: UTF-8
;;;
;;; Copyright (C) 2010 by Damir Ćavar. 
;;;
;;; testing sequence procedures
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

;; Written using SRFI 64 tests by Peter Lane, 2017

(import (scheme base)
        (nltk sequence)
        (srfi 64))

(test-begin "nltk-sequence")

;; test1: returns vector of token permutations from list of strings
;; -- test allows different orders of the permutations
(let ((expected '(#("John" "loves" "Mary" "not") #("John" "loves" "not" "Mary")
              #("John" "Mary" "loves" "not") #("John" "Mary" "not" "loves")
              #("John" "not" "Mary" "loves") #("John" "not" "loves" "Mary")
              #("loves" "John" "Mary" "not") #("loves" "John" "not" "Mary")
              #("loves" "Mary" "John" "not") #("loves" "Mary" "not" "John")
              #("loves" "not" "Mary" "John") #("loves" "not" "John" "Mary")
              #("Mary" "loves" "John" "not") #("Mary" "loves" "not" "John")
              #("Mary" "John" "loves" "not") #("Mary" "John" "not" "loves")
              #("Mary" "not" "John" "loves") #("Mary" "not" "loves" "John")
              #("not" "Mary" "loves" "John") #("not" "Mary" "John" "loves")
              #("not" "loves" "Mary" "John") #("not" "loves" "John" "Mary")
              #("not" "John" "loves" "Mary") #("not" "John" "Mary" "loves")))
      (actual (vector->list (sequence->permutations-vector '("John" "loves" "Mary" "not")))))
  (test-assert (= (length expected) (length actual)))
  (for-each (lambda (act) (test-assert (member act expected equal?)))
            actual))

;; test1: returns vector of token permutations from vector of symbols
;; -- test allows different orders of the permutations
(let ((expected '(#(John loves Mary not) #(John loves not Mary) #(John Mary loves not)
              #(John Mary not loves) #(John not Mary loves) #(John not loves Mary)
              #(loves John Mary not) #(loves John not Mary) #(loves Mary John not)
              #(loves Mary not John) #(loves not Mary John) #(loves not John Mary)
              #(Mary loves John not) #(Mary loves not John) #(Mary John loves not)
              #(Mary John not loves) #(Mary not John loves) #(Mary not loves John)
              #(not Mary loves John) #(not Mary John loves) #(not loves Mary John)
              #(not loves John Mary) #(not John loves Mary) #(not John Mary loves)))
      (actual (vector->list (sequence->permutations-vector (vector 'John 'loves 'Mary 'not)))))
  (test-assert (= (length expected) (length actual)))
  (for-each (lambda (act) (test-assert (member act expected equal?)))
            actual))

;; test1: returns vector of character permutations from string
(test-equal
  #(#(#\J #\o #\h #\n) #(#\J #\o #\n #\h) #(#\J #\h #\o #\n) #(#\J #\h #\n #\o)
  #(#\J #\n #\h #\o) #(#\J #\n #\o #\h) #(#\o #\J #\h #\n) #(#\o #\J #\n #\h)
  #(#\o #\h #\J #\n) #(#\o #\h #\n #\J) #(#\o #\n #\h #\J) #(#\o #\n #\J #\h)
  #(#\h #\o #\J #\n) #(#\h #\o #\n #\J) #(#\h #\J #\o #\n) #(#\h #\J #\n #\o)
  #(#\h #\n #\J #\o) #(#\h #\n #\o #\J) #(#\n #\h #\o #\J) #(#\n #\h #\J #\o)
  #(#\n #\o #\h #\J) #(#\n #\o #\J #\h) #(#\n #\J #\o #\h) #(#\n #\J #\h #\o))
  (sequence->permutations-vector "John"))

;; test2: returns list of token permutations from list of strings
(test-equal 
  '(("John" "loves" "Mary" "not") ("John" "loves" "not" "Mary")
                                  ("John" "Mary" "loves" "not") ("John" "Mary" "not" "loves")
                                  ("John" "not" "Mary" "loves") ("John" "not" "loves" "Mary")
                                  ("loves" "John" "Mary" "not") ("loves" "John" "not" "Mary")
                                  ("loves" "Mary" "John" "not") ("loves" "Mary" "not" "John")
                                  ("loves" "not" "Mary" "John") ("loves" "not" "John" "Mary")
                                  ("Mary" "loves" "John" "not") ("Mary" "loves" "not" "John")
                                  ("Mary" "John" "loves" "not") ("Mary" "John" "not" "loves")
                                  ("Mary" "not" "John" "loves") ("Mary" "not" "loves" "John")
                                  ("not" "Mary" "loves" "John") ("not" "Mary" "John" "loves")
                                  ("not" "loves" "Mary" "John") ("not" "loves" "John" "Mary")
                                  ("not" "John" "loves" "Mary") ("not" "John" "Mary" "loves"))
  (sequence->permutations-list '("John" "loves" "Mary" "not")))

;; test2: returns list of character permutations from string
(test-equal 
  '((#\J #\o #\h #\n) (#\J #\o #\n #\h) (#\J #\h #\o #\n) (#\J #\h #\n #\o)
                      (#\J #\n #\h #\o) (#\J #\n #\o #\h) (#\o #\J #\h #\n) (#\o #\J #\n #\h)
                      (#\o #\h #\J #\n) (#\o #\h #\n #\J) (#\o #\n #\h #\J) (#\o #\n #\J #\h)
                      (#\h #\o #\J #\n) (#\h #\o #\n #\J) (#\h #\J #\o #\n) (#\h #\J #\n #\o)
                      (#\h #\n #\J #\o) (#\h #\n #\o #\J) (#\n #\h #\o #\J) (#\n #\h #\J #\o)
                      (#\n #\o #\h #\J) (#\n #\o #\J #\h) (#\n #\J #\o #\h) (#\n #\J #\h #\o))
  (sequence->permutations-list "John"))

;; test2: returns list of number permutations from vector of numbers
(test-equal '((1 7 6 5 3) (1 7 6 3 5) (1 7 5 6 3) (1 7 5 3 6) (1 7 3 5 6) (1 7 3 6 5)
                          (1 6 7 5 3) (1 6 7 3 5) (1 6 5 7 3) (1 6 5 3 7) (1 6 3 5 7) (1 6 3 7 5)
                          (1 5 6 7 3) (1 5 6 3 7) (1 5 7 6 3) (1 5 7 3 6) (1 5 3 7 6) (1 5 3 6 7)
                          (1 3 5 6 7) (1 3 5 7 6) (1 3 6 5 7) (1 3 6 7 5) (1 3 7 6 5) (1 3 7 5 6)
                          (7 1 6 5 3) (7 1 6 3 5) (7 1 5 6 3) (7 1 5 3 6) (7 1 3 5 6) (7 1 3 6 5)
                          (7 6 1 5 3) (7 6 1 3 5) (7 6 5 1 3) (7 6 5 3 1) (7 6 3 5 1) (7 6 3 1 5)
                          (7 5 6 1 3) (7 5 6 3 1) (7 5 1 6 3) (7 5 1 3 6) (7 5 3 1 6) (7 5 3 6 1)
                          (7 3 5 6 1) (7 3 5 1 6) (7 3 6 5 1) (7 3 6 1 5) (7 3 1 6 5) (7 3 1 5 6)
                          (6 7 1 5 3) (6 7 1 3 5) (6 7 5 1 3) (6 7 5 3 1) (6 7 3 5 1) (6 7 3 1 5)
                          (6 1 7 5 3) (6 1 7 3 5) (6 1 5 7 3) (6 1 5 3 7) (6 1 3 5 7) (6 1 3 7 5)
                          (6 5 1 7 3) (6 5 1 3 7) (6 5 7 1 3) (6 5 7 3 1) (6 5 3 7 1) (6 5 3 1 7)
                          (6 3 5 1 7) (6 3 5 7 1) (6 3 1 5 7) (6 3 1 7 5) (6 3 7 1 5) (6 3 7 5 1)
                          (5 6 7 1 3) (5 6 7 3 1) (5 6 1 7 3) (5 6 1 3 7) (5 6 3 1 7) (5 6 3 7 1)
                          (5 7 6 1 3) (5 7 6 3 1) (5 7 1 6 3) (5 7 1 3 6) (5 7 3 1 6) (5 7 3 6 1)
                          (5 1 7 6 3) (5 1 7 3 6) (5 1 6 7 3) (5 1 6 3 7) (5 1 3 6 7) (5 1 3 7 6)
                          (5 3 1 7 6) (5 3 1 6 7) (5 3 7 1 6) (5 3 7 6 1) (5 3 6 7 1) (5 3 6 1 7)
                          (3 5 6 7 1) (3 5 6 1 7) (3 5 7 6 1) (3 5 7 1 6) (3 5 1 7 6) (3 5 1 6 7)
                          (3 6 5 7 1) (3 6 5 1 7) (3 6 7 5 1) (3 6 7 1 5) (3 6 1 7 5) (3 6 1 5 7)
                          (3 7 6 5 1) (3 7 6 1 5) (3 7 5 6 1) (3 7 5 1 6) (3 7 1 5 6) (3 7 1 6 5)
                          (3 1 7 6 5) (3 1 7 5 6) (3 1 6 7 5) (3 1 6 5 7) (3 1 5 6 7) (3 1 5 7 6))
            (sequence->permutations-list '(1 7 6 5 3)))

;; test3: returns empty list for negative sequence lengths
(test-equal '() (enum-list -1))

;; test4: returns vector #(0 1 2 3)
(test-equal #(0 1 2 3) (enum-vector (length '("Peter" "lives" "in" "Paris"))))

(test-end)
