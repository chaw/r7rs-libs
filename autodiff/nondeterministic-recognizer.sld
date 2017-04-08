;Thursday 20 May 2010
;
;Jeffrey Mark Siskind
;School of Electrical and Computer Engineering
;Purdue University
;465 Northwestern Avenue
;West Lafayette IN 47907-2035 USA
;voice: 765/496-3197
;fax: 765/494-6440
;qobi@purdue.edu
;http://engineering.purdue.edu/~qobi
;
;The entire contents of this directory copyright 2010 Purdue University. All
;rights reserved.
;
;These are some AD (Automatic Differentiation) tools for both forward
;and reverse mode written for R6RS Scheme.  They run under Ikarus and
;PLT Scheme.  Also included are some experimental packages to support
;nondeterministic and stochastic programming, including constraint
;satisfaction problems.
;
;This code is completely unsupported.
;
;It is licensed under the GPL v2 or later, see the enclosed file COPYING.
;
;This material is based upon work supported by the National Science
;Foundation under Grant No. 0438806.
;Any opinions, findings, and conclusions or recommendations expressed in
;this material are those of the author(s) and do not necessarily reflect
;the views of the National Science Foundation.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
 (autodiff nondeterministic-recognizer)
 (export make-production a-tree yield phrase1? yield-equal? phrase2?
	 a-tree-yield-equal? phrase3?)
 (import (scheme base)
	 (autodiff QobiScheme)
	 (autodiff nondeterministic-scheme)
	 (autodiff nondeterministic-memoization))

 (begin

 (define-record-type <production>
                     (make-production lhs rhs1 rhs2)
                     production?
                     (lhs production-lhs)
                     (rhs1 production-rhs1)
                     (rhs2 production-rhs2))

 (define-record-type <tree>
                     (make-tree category left right)
                     tree?
                     (category tree-category)
                     (left tree-left) 
                     (right tree-right))

 (define (a-tree category productions)
  (let ((matching-productions
	 (remove-if-not
	  (lambda (production) (eq? (production-lhs production) category))
	  productions)))
   (if (null? matching-productions)
       category
       (let ((production (a-member-of matching-productions)))
	(make-tree category
		   (a-tree (production-rhs1 production) productions)
		   (a-tree (production-rhs2 production) productions))))))

 (define (yield tree)
  (if (symbol? tree)
      (list tree)
      (append (yield (tree-left tree)) (yield (tree-right tree)))))

 (define (phrase1? categories category productions)
  (possibly?
   (domain (equal? categories (yield (a-tree category productions))))))

 (define (yield-equal? tree categories)
  (or (and (symbol? tree)
	   (null? (rest categories))
	   (eq? tree (first categories)))
      (and (tree? tree)
	   (some-n
	    (lambda (i)
	     (and
	      (yield-equal? (tree-left tree) (sublist categories 0 (+ i 1)))
	      (yield-equal? (tree-right tree)
			    (sublist categories (+ i 1) (length categories)))))
	    (- (length categories) 1)))))

 (define (phrase2? categories category productions)
  (possibly? (domain (yield-equal? (a-tree category productions) categories))))

 (define a-tree-yield-equal?
  (memoize-nondeterministic-removing-duplicates
   (lambda (categories category productions)
    (let ((matching-productions
	   (remove-if-not
	    (lambda (production) (eq? (production-lhs production) category))
	    productions)))
     (or (and (null? matching-productions)
	      (null? (rest categories))
	      (eq? category (first categories)))
	 (let ((production (a-member-of matching-productions)))
	  (some-n (lambda (i)
		   (and (a-tree-yield-equal?
			 (sublist categories 0 (+ i 1))
			 (production-rhs1 production)
			 productions)
			(a-tree-yield-equal?
			 (sublist categories (+ i 1) (length categories))
			 (production-rhs2 production)
			 productions)))
		  (- (length categories) 1))))))))

 (define (phrase3? categories category productions)
  (possibly? (domain (a-tree-yield-equal? categories category productions))))
 
 ))

