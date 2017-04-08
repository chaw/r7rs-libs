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
  (autodiff stochastic-recognizer)
  (export make-production a-tree yield phrase-probability1 yield-equal?
          phrase-probability2 a-tree-yield-equal? phrase-probability3)
  (import (scheme base)
          (autodiff QobiScheme)
          (autodiff stochastic-scheme)
          (autodiff stochastic-memoization))

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



    (define (a-tree category production-distributions)
      (let ((matching-production-distribution
              (remove-if-not
                (lambda (production-pair)
                  (eq? (production-lhs (car production-pair)) category))
                production-distributions)))
        (if (null? matching-production-distribution)
          category
          (let ((production (draw matching-production-distribution)))
            (make-tree
              category
              (a-tree (production-rhs1 production) production-distributions)
              (a-tree (production-rhs2 production) production-distributions))))))

    (define (yield tree)
      (if (symbol? tree)
        (list tree)
        (append (yield (tree-left tree)) (yield (tree-right tree)))))

    (define (phrase-probability1 categories category production-distributions)
      (probability
        (distribution (equal? categories
                              (yield (a-tree category production-distributions))))))

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

    (define (phrase-probability2 categories category production-distributions)
      (probability
        (distribution
          (yield-equal? (a-tree category production-distributions) categories))))

    (define a-tree-yield-equal?
      (memoize-stochastic-coalescing-duplicates
        (lambda (categories category production-distributions)
          (let ((matching-production-distribution
                  (remove-if-not
                    (lambda (production-pair)
                      (eq? (production-lhs (car production-pair)) category))
                    production-distributions)))
            (or (and (null? matching-production-distribution)
                     (null? (rest categories))
                     (eq? category (first categories)))
                (let ((production (draw matching-production-distribution)))
                  (some-n (lambda (i)
                            (and (a-tree-yield-equal?
                                   (sublist categories 0 (+ i 1))
                                   (production-rhs1 production)
                                   production-distributions)
                                 (a-tree-yield-equal?
                                   (sublist categories (+ i 1) (length categories))
                                   (production-rhs2 production)
                                   production-distributions)))
                          (- (length categories) 1))))))))

    (define (phrase-probability3 categories category production-distributions)
      (probability
        (distribution
          (a-tree-yield-equal? categories category production-distributions))))

    ))

