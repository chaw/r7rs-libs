;;; filename: sequences.sls
;;;
;;; Time-stamp: <2010-02-28 01:15:00 dcavar>
;;; encoding: UTF-8
;;;
;;; Copyright (C) 2010 by Damir Ćavar. 
;;;
;;; sequence procedures
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

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
 (nltk sequence)
 (export sequence->permutations-list
         sequence->permutations-vector
         enum-list
         enum-vector
         sequence->hashtable
         remove-duplicates
         sequence->list
         str-split)
 (import (scheme base)
         (scheme case-lambda)
         (srfi 69))
 
 (begin

   (define sequence->list
     (lambda (seq)
       (cond ((vector? seq)
              (vector->list seq))
             ((list? seq)
              seq)
             ((string? seq)
              (string->list seq))
             (else '()))))

   (define rec-seq-gen
     (lambda (mylist)
       (let ((rev (reverse mylist)))
         (map
           (lambda (token index)
             (let ((rest (rec-seq-gen (append (list-tail rev (- (length rev) index))
                                              (list-tail mylist (+ index 1))))))
               (if (null? rest)
                 (list token)
                 (let loop ((result '())
                            (rest   rest))
                   (if (null? rest)
                     result
                     (begin
                       (loop (append result
                                     (map
                                       (lambda (a)
                                         (if (list? a)
                                           (append (list token) a)
                                           (append (list token) (list a))))
                                       (list-ref rest 0))) (list-tail rest 1))))))))
           mylist (enum-list (length mylist))))))


   (define enum-list
     (case-lambda
       ((len)      (enum-list len 0))
       ((len from) (enum-list len from 1))
       ((len from step)
        (let loop ((result '())
                   (count from))
          (if (<= len count)
            result
            (loop (append result (list count)) (+ count step)))))))


   (define enum-vector
     (case-lambda
       ((len)      (enum-vector len 0))
       ((len from) (enum-vector len from 1))
       ((len from step)
        (list->vector (enum-list len from step)))))


   (define sequence->permutations-list
     (lambda (mylist)
       (cond ((vector? mylist)
              (sequence->permutations-list (vector->list mylist)))
             ((string? mylist)
              (sequence->permutations-list (string->list mylist)))
             ((list? mylist)
              (let ((ts (rec-seq-gen mylist)))
                (let loop ((res '())
                           (seq ts))
                  (if (null? seq)
                    res
                    (loop (append res (list-ref seq 0)) (list-tail seq 1))))))
             (else
               '()))))


   (define sequence->permutations-vector
     (lambda (mylist)
       (list->vector (map list->vector (sequence->permutations-list mylist)))))


   (define sequence->hashtable
     (case-lambda
       ((seq) (sequence->hashtable seq (make-hash-table equal?)))
       ((seq wlhash)
        (begin
          (for-each
            (lambda (token)
              (hash-table-set! wlhash token (+ (hash-table-ref/default wlhash token 0) 1)))
            (cond ((list? seq) seq)
                  ((vector? seq) (vector->list seq))
                  ((string? seq) (string->list seq))
                  (else '())))
          wlhash))))


   (define remove-duplicates
     (lambda (somelist)
       (let loop ((mylist somelist)
                  (result '()))
         (if (null? mylist)
           result
           (if (member (car mylist) result)
             (loop (cdr mylist) result)
             (loop (cdr mylist) (append result (list (car mylist)))))))))


   ;;; str-split : Apr 2006 Doug Hoyte, hcsw.org.
   ;;; ----
   ;;; Splits a string 'str into a list of strings
   ;;; that were separated by the delimiter character 'ch
   ;;; ----
   ;;; Efficient as possible given that we can't count on
   ;;; 'str being an immutable string.
   (define (str-split str ch)
     (let ((len (string-length str)))
       (letrec
         ((split
            (lambda (a b)
              (cond
                ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
                ((char=? ch (string-ref str b)) (if (= a b)
                                                  (split (+ 1 a) (+ 1 b))
                                                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
         (split 0 0))))


   )) ; end nltk sequences - sequences.sls

