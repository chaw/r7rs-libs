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
 (autodiff stochastic-montague-grammar)
 (export generate understand)
 (import (scheme base)
	 (autodiff QobiScheme)
	 (autodiff nondeterministic-scheme)
	 (autodiff stochastic-scheme)
	 (autodiff deterministic-memoization)
	 (autodiff nondeterministic-memoization)
	 (autodiff reduced-gradient))

 (begin

   (define-record-type <typed-meaning>
                       (make-typed-meaning type meaning)
                       typed-meaning?
                       (type typed-meaning-type)
                       (meaning typed-meaning-meaning))

   (define-record-type <position>
                       (make-position position)
                       position?
                       (position position-position))

   (define-record-type <position-state>
                       (make-position-state position state)
                       position-state?
                       (position position-state-position)
                       (state position-state-state))

   (define-record-type <leftward-arrow-type>
                       (make-leftward-arrow-type result argument)
                       leftward-arrow-type?
                       (result leftward-arrow-type-result)
                       (argument leftward-arrow-type-argument))

   (define-record-type <rightward-arrow-type>
                       (make-rightward-arrow-type argument result)
                       rightward-arrow-type?
                       (argument rightward-arrow-type-argument)
                       (result rightward-arrow-type-result))

   (define (equal-type? type1 type2)
     (or (eq? type1 type2)
         (and (leftward-arrow-type? type1)
              (leftward-arrow-type? type2)
              (equal-type? (leftward-arrow-type-argument type1)
                           (leftward-arrow-type-argument type2))
              (equal-type? (leftward-arrow-type-result type1)
                           (leftward-arrow-type-result type2)))
         (and (rightward-arrow-type? type1)
              (rightward-arrow-type? type2)
              (equal-type? (rightward-arrow-type-argument type1)
                           (rightward-arrow-type-argument type2))
              (equal-type? (rightward-arrow-type-result type1)
                           (rightward-arrow-type-result type2)))))

   (define (lexicon game-state)
     (let ((things (append (map-n make-position 9)
                           (map-n (lambda (position)
                                    (make-position-state
                                      position (list-ref game-state position)))
                                  9))))
       (list
         (cons
           'the
           (make-typed-meaning
             (make-rightward-arrow-type
               (make-rightward-arrow-type 'thing 'bool)
               (make-rightward-arrow-type
                 (make-rightward-arrow-type 'thing 'bool) 'bool))
             (lambda (noun1)
               (lambda (noun2)
                 ;; Montague semantics of "the" is wrong
                 (and (one noun1 things) (noun2 (find-if noun1 things)))))))
         (cons
           'x
           (make-typed-meaning
             (make-rightward-arrow-type 'thing 'bool)
             (lambda (thing)
               (and (position-state? thing) (eq? (position-state-state thing) 'x)))))
         (cons
           'is-on
           (make-typed-meaning
             (make-rightward-arrow-type
               (make-rightward-arrow-type
                 (make-rightward-arrow-type 'thing 'bool) 'bool)
               (make-leftward-arrow-type
                 'bool
                 (make-rightward-arrow-type
                   (make-rightward-arrow-type 'thing 'bool) 'bool)))
             (lambda (np2)
               (lambda (np1)
                 (np2 (lambda (thing2)
                        (np1 (lambda (thing1)
                               (and (position-state? thing1)
                                    (position? thing2)
                                    (= (position-state-position thing1)
                                       (position-position thing2)))))))))))
         (cons
           'center
           (make-typed-meaning
             (make-rightward-arrow-type 'thing 'bool)
             (lambda (thing)
               (and (position? thing) (= (position-position thing) 4))))))))

   (define (a-typed-apply left right)
     (amb (if (and (rightward-arrow-type? (typed-meaning-type left))
                   (equal-type?
                     (typed-meaning-type right)
                     (rightward-arrow-type-argument (typed-meaning-type left))))
            (make-typed-meaning
              (rightward-arrow-type-result (typed-meaning-type left))
              ((typed-meaning-meaning left) (typed-meaning-meaning right)))
            (fail))
          (if (and (leftward-arrow-type? (typed-meaning-type right))
                   (equal-type?
                     (typed-meaning-type left)
                     (leftward-arrow-type-argument (typed-meaning-type right))))
            (make-typed-meaning
              (leftward-arrow-type-result (typed-meaning-type right))
              ((typed-meaning-meaning right) (typed-meaning-meaning left)))
            (fail))))

   (define (possibly-true? game-state)
     (let ((lexicon (lexicon game-state)))
       (lambda (words)
         (letrec ((an-interpretation
                    (memoize-nondeterministic
                      (lambda (words)
                        (if (= (length words) 1)
                          (cdr (assq (first words) lexicon))
                          (let ((i (+ (a-member-of (enumerate (- (length words) 1)))
                                      1)))
                            (a-typed-apply (an-interpretation
                                             (sublist words 0 i))
                                           (an-interpretation
                                             (sublist words i (length words))))))))))
           (possibly? (domain (let ((typed-meaning (an-interpretation words)))
                                (and (eq? (typed-meaning-type typed-meaning) 'bool)
                                     (typed-meaning-meaning typed-meaning)))))))))

   (define (position-state-draw mixture)
     (draw (map cons '(empty x o) (vector->list mixture))))

   (define (word-draw mixture)
     (draw (map cons '(the x is-on center) (vector->list mixture))))

   (define (generate)
     (argmax
       (lambda (mixtures)
         (probability
           (distribution
             ((possibly-true? '(empty empty empty empty x empty empty empty empty))
              (map word-draw (vector->list mixtures))))))
       (map-n-vector (lambda (i) (uniform 4)) 5)))

   (define (understand)
     (argmax
       (lambda (mixtures)
         (probability
           (distribution
             ((possibly-true? (map position-state-draw (vector->list mixtures)))
              '(the x is-on the center)))))
       (map-n-vector (lambda (i) (uniform 3)) 9)))

   ))

