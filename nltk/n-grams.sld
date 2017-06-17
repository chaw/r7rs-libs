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

;;; The Scheme NLTK is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the Creative Commons GNU Lesser
;;; General Public License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Damir Ćavar <dcavar@unizd.hr>
;;
;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Commentary:
;; 
;; TODO:
;; - serialization and deserialization procedures
;; - 


(define-library
 (nltk n-grams)
 (export filter-ngrams
         filter-ngrams!
         filter-ngrams-counts
         filter-ngrams-counts!
         frequency-profile
         frequency-profile-decreasing
         frequency-profile-increasing
         ngrams->bigrams
         ngrams->dot
         ngrams->dot-digraph
         ngrams->dot-graph
         ngrams->html-table
         ngrams-sort
         relativize-ngrams
         relativize-ngrams!
         token-sequence->ngrams
         ngram-counts)
 (import (scheme base)
         (scheme case-lambda)
         (scheme write)
         (nltk vectorspace)
         (nltk sequence)
         (scheme list)
         (scheme comparator)
         (scheme hash-table)
         (scheme sort))
 
 (begin

   ;; Special version to return vectors, like in R6RS
   (define (hash-table-ventries ht)
     (values (apply vector (hash-table-keys ht))
             (apply vector (hash-table-values ht))))

   ; TODO
   ; serialization and deserialization procedures

   (define ngrams->bigrams
     (lambda (ngrams)
       (let ((bigrams (make-hash-table (make-default-comparator))))
         (let-values (((keyv valuev) (hash-table-ventries ngrams)))
                     (cond ((> (vector-length keyv) 0)
                            (cond ((= (vector-length (vector-ref keyv 0)) 2)
                                   ngrams)
                                  ((> (vector-length (vector-ref keyv 0)) 2)
                                   (vector-for-each
                                     (lambda (key value)
                                       (for-each
                                         (lambda (index)
                                           (let ((bigram (vector
                                                           (vector-ref key index)
                                                           (vector-ref key (+ index 1)))))
                                             (hash-table-set! bigrams bigram
                                                              (+ (hash-table-ref/default bigrams bigram 0) value))))
                                         (enum-list (- (vector-length key) 1))))
                                     keyv valuev)))))
                     ; repair bigram statistics here?
                     ; TODO?
                     bigrams))))

   (define filter-ngrams
     (case-lambda 
       ((ngrams filter-tokens) (filter-ngrams ngrams filter-tokens #t))
       ((ngrams filter-tokens remove)
        (let ((new-ngrams (make-hash-table (make-default-comparator))))
          (let-values (((keys vals) (hash-table-entries ngrams)))
                      ; check for length ngrams > 0
                      (unless (null? keys)
                             (for-each
                               (lambda (key value)
                                 (cond ((let loop ((tokens (vector->list key)))
                                          (cond ((null? tokens) remove)
                                                ((member (car tokens) filter-tokens) (not remove))
                                                (else (loop (cdr tokens)))))
                                        (hash-table-set! new-ngrams key value))))
                               keys vals))))
          new-ngrams)))

   (define filter-ngrams!
     (case-lambda 
       ((ngrams filter-tokens) (filter-ngrams ngrams filter-tokens #t))
       ((ngrams filter-tokens remove)
        (let-values (((keys vals) (hash-table-entries ngrams)))
                    (for-each
                      (lambda (key value)
                        (cond ((let loop ((tokens (vector->list key)))
                                 (cond ((null? tokens) (not remove))
                                       ((member (car tokens) filter-tokens) remove)
                                       (else (loop (cdr tokens)))))
                               (hash-table-delete! ngrams key))))
                      keys vals)
                    ngrams))))

   (define filter-pos-ngrams
     (case-lambda 
       ((ngrams filter-tokens) (filter-ngrams ngrams filter-tokens 0))
       ((ngrams filter-tokens pos) (filter-ngrams ngrams filter-tokens pos #t))
       ((ngrams filter-tokens pos remove)
        (let ((new-ngrams (make-hash-table (make-default-comparator))))
          (let-values (((keys vals) (hash-table-entries ngrams)))
                      (unless (null? keys)
                        (for-each
                          (lambda (key value)
                            (cond ((and (>= pos 0)
                                        (< pos (vector-length key)))
                                   (cond ((if (member (vector-ref key pos) filter-tokens)
                                            (not remove)
                                            remove)
                                          (hash-table-set! new-ngrams key value))))))
                          keys vals)))
          new-ngrams))))


   (define filter-ngrams-counts
     (case-lambda 
       ((ngrams threshold) (filter-ngrams ngrams threshold #t))
       ((ngrams threshold larger)
        (let ((new-ngrams (make-hash-table (make-default-comparator)))
              (comparison (if larger > <)))
          (let-values (((keys vals) (hash-table-entries ngrams)))
                      (for-each
                        (lambda (key value)
                          (cond ((comparison value threshold)
                                 (hash-table-set! new-ngrams key value))))
                        keys vals))
          new-ngrams))))

   (define filter-ngrams-counts!
     (case-lambda 
       ((ngrams threshold) (filter-ngrams ngrams threshold #t))
       ((ngrams threshold larger)
        (let ((comparison (if larger > <)))
          (let-values (((keys vals) (hash-table-entries ngrams)))
                      (for-each
                        (lambda (key value)
                          (cond ((not (comparison value threshold))
                                 (hash-table-delete! ngrams key))))
                        keys vals))
          ngrams))))


   (define ngrams->dot-digraph
     (lambda (ngrams)
       (ngrams->dot ngrams 'digraph)))

   (define ngrams->dot-graph
     (lambda (ngrams)
       (ngrams->dot ngrams 'graph)))


   ; change:
   ; link thickness based on cooccurence frequency
   ; node and text thickness based on token frequency
   ; distance corresponds to cooccurence frequency
   (define ngrams->dot
     (case-lambda
       ((ngrams)                           (ngrams->dot ngrams 'digraph))
       ((ngrams graph-type)                (ngrams->dot ngrams 'digraph #f))
       ((ngrams graph-type line-thickness) (ngrams->dot ngrams 'digraph #f #f))
       ((ngrams graph-type line-thickness node-size)
        ; TODO display line-thickness and node-size
        (let ((port (open-output-string)))
          (call-with-port
            port
            (lambda (p)
              (let-values (((label connector) (if (eq? graph-type 'digraph)
                                                (values "digraph" " -> ")
                                                (values "graph" " -- "))))
                          (display label p)
                          (display " G {" p)(newline p)
                          (for-each 
                            (lambda (bkey)
                              (let ((tokens (vector->list bkey)))
                                (display "\t" p)
                                (display (car tokens) p)
                                (for-each
                                  (lambda (item)
                                    (display connector p)
                                    (display item p))
                                  (cdr tokens))
                                (display " ;" p)(newline p)))
                            (hash-table-keys ngrams))
                          (display "}" p)(newline p))
              (get-output-string port)))))))


   ; ngrams->html-table-string
   ; Description: Returns a HTML table from an n-gram hash-table.
   ; Parameters: n-grams hash-table
   ; Return: String
   (define ngrams->html-table
     (case-lambda
       ((n-grams)               (ngrams->html-table n-grams '()))
       ((n-grams column-titles) (ngrams->html-table n-grams column-titles #t))
       ((n-grams column-titles sorted) (ngrams->html-table n-grams column-titles sorted 'frequency))
       ((n-grams column-titles sorted sort-field)
        (let ((fields (cond (sorted
                              (ngrams-sort n-grams
                                           (if (eq? sort-field 'frequency)
                                             #t
                                             #f)
                                           (if (eq? sort-field 'frequency)
                                             #t
                                             #f)))
                            (else
                              (error "ngrams not sorted")))))
          (let ((port (open-output-string)))
            (call-with-port
              port
              (lambda (p)
                (display "<table class=\"table-style\">" p)(newline p)
                ; write out header
                (cond ((not (null? column-titles))
                       (begin
                         (display "<thead class=\"table-header-style\">" p)(newline p)
                         (display "<tr>" p)(newline p)
                         (for-each
                           (lambda (token)
                             (display "<td>" p)(display token p)(display "</td>" p))
                           column-titles)
                         (display "</tr>" p)(newline p)
                         (display "</thead>" p)(newline p))))
                ; write the body
                (display "<tbody class=\"table-body-style\">" p)(newline p)
                (vector-for-each 
                  (lambda (bkeyval)
                    (display "<tr>" p)(newline p)
                    (vector-for-each
                      (lambda (skey)
                        (display "<td>" p)(display skey p)(display "</td>" p))
                      (vector-ref bkeyval 0))
                    (display "<td>" p)(display (vector-ref bkeyval 1) p)(display "</td>" p)(newline p)
                    (display "</tr>" p)(newline p))
                  fields)
                (display "</tbody>" p)(newline p)
                (display "</table>" p)(newline p)))
            (get-output-string port))))))


   (define relativize-ngrams
     (lambda (ngrams)
       (relativize-ngrams! ngrams (make-hash-table (make-default-comparator)))))


   (define relativize-ngrams!
     (case-lambda
       ((from-ngrams) (relativize-ngrams! from-ngrams from-ngrams))
       ((from-ngrams to-ngrams)
        (begin
          (let-values (((keys vals) (hash-table-entries from-ngrams)))
                      (let ((total-count (fold + 0 vals)))
                        (for-each (lambda (key)
                                    (hash-table-set! to-ngrams key (/ (hash-table-ref/default from-ngrams key 0) total-count)))
                                  keys)))
          to-ngrams))))


   (define ngram-counts
     (lambda (ngrams)
       (fold + 0 (hash-table-values ngrams))))

   (define ngrams-sort
     (case-lambda
       ((ngrams) (ngrams-sort ngrams #t))
       ((ngrams decreasing) (ngrams-sort ngrams decreasing #t))
       ((ngrams decreasing by-value)
        (let-values (((keys values) (hash-table-ventries ngrams)))
                    (let ((pick (if by-value
                                  (lambda (elem)
                                    (vector-ref elem 1))
                                  (lambda (elem)
                                    (vector-ref (vector-ref elem 0) 0))))
                          (compare (cond ((not by-value)
                                          (let ((gram (vector-ref (vector-ref keys 0) 0)))
                                            (cond ((number? gram)
                                                   (if decreasing > <))
                                                  ((string? gram)
                                                   (if decreasing string>? string<?))
                                                  ((char? gram)
                                                   (if decreasing char>? char<?))
                                                  (else
                                                    (error "Unknown type for comparison")))))
                                         (else
                                           (if decreasing > <)))))
                      (vector-sort 
                        (lambda (a b)
                          (compare (pick a) (pick b)))
                        (vector-map
                          (lambda (key value)
                            (vector key value))
                          keys values)))))))


   (define frequency-profile-decreasing
     (lambda (ngrams)
       (ngrams-sort ngrams #t #t)))

   (define frequency-profile-increasing
     (lambda (ngrams)
       (ngrams-sort ngrams #f #t)))

   (define frequency-profile
     (lambda (ngrams) (ngrams-sort ngrams #t #t)))


   ; token-sequence->ngrams
   ; parameters: tokens - list, vector, string
   ;             n      - length of ngram
   ; optional parameter: hash-table
   ;                     key:   ngram
   ;                     value: count
   ; return: hash-table
   ;         keys:  vector of tokens
   ;         value: count of tokens in list
   (define token-sequence->ngrams
     (case-lambda
       ((tokens)   (token-sequence->ngrams tokens 2))
       ((tokens n) (token-sequence->ngrams tokens n (make-hash-table (make-default-comparator))))
       ((tokens n ngrams)
        (begin
          (cond ((and (integer? n)
                      (> n 0))
                 (begin
                   (cond ((vector? tokens) ; token vector -> token list
                          (set! tokens (vector->list tokens)))
                         ((string? tokens) ; token string -> chracter list
                          (set! tokens (string->list tokens))))
                   (cond ((list? tokens)
                          (let ((num-tokens (length tokens)))
                            (when (>= num-tokens n)
                              (for-each
                                (lambda (index)
                                  (let* ((rest-list (list-tail tokens index))
                                         (n-gram    (list->vector (drop-right rest-list (- (length rest-list) n) ))))
                                    (hash-table-set! ngrams n-gram
                                                     (+ (hash-table-ref/default ngrams n-gram 0) 1))))
                                (enum-list (- num-tokens (- n 1)))))))))))
          ngrams))))

   )) ; library end

