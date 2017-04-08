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
 (autodiff stochastic-constraints)
 (export set-stochastic-strategy! create-distribution-variable
	 distribution-variable-distribution stochastic-solution
	 assert-stochastic-constraint!)
 (import (scheme base) 
         (autodiff QobiScheme) 
         (autodiff stochastic-scheme))

 (begin

   (define *strategy* #f)

   (define-record-type <distribution-variable>
                       (make-distribution-variable distribution demons)
                       distribution-variable?
                       (distribution distribution-variable-distribution distribution-variable-distribution-set!) 
                       (demons distribution-variable-demons distribution-variable-demons-set!))

   (define (set-stochastic-strategy! strategy) (set! *strategy* strategy))

   (define (distribution-variable-distribution-local-set!
             distribution-variable distribution)
     (let ((distribution
             (distribution-variable-distribution distribution-variable)))
       (upon-bottom (distribution-variable-distribution-set!
                      distribution-variable distribution)))
     (distribution-variable-distribution-set! distribution-variable distribution))

   (define (distribution-variable-demons-local-set! distribution-variable demons)
     (let ((demons (distribution-variable-demons distribution-variable)))
       (upon-bottom
         (distribution-variable-demons-set! distribution-variable demons)))
     (distribution-variable-demons-set! distribution-variable demons))

   (define (create-distribution-variable distribution)
     (let ((distribution (coalesce-duplicates distribution)))
       (when (null? distribution) (bottom))
       (make-distribution-variable distribution '())))

   (define (restrict-distribution! d distribution)
     (when (null? distribution) (bottom))
     (when (< (length distribution)
              (length (distribution-variable-distribution d)))
       (distribution-variable-distribution-local-set! d distribution)
       (for-each (lambda (demon) (demon)) (distribution-variable-demons d))))

   (define (bound? d) (null? (rest (distribution-variable-distribution d))))

   (define (binding d) (car (first (distribution-variable-distribution d))))

   (define (stochastic-solution ds)
     (let loop ((ds ds) (xs '()))
       (if (null? ds)
         (reverse xs)
         (let ((pair
                 (draw-pair (distribution-variable-distribution (first ds)))))
           (restrict-distribution! (first ds) (list pair))
           (loop (rest ds) (cons (first pair) xs))))))

   (define (some-element predicate d)
     (some (lambda (pair) (predicate (car pair)))
           (distribution-variable-distribution d)))

   (define (one-element predicate d)
     (one (lambda (pair) (predicate (car pair)))
          (distribution-variable-distribution d)))

   (define (the-element predicate d)
     (list (find-if (lambda (pair) (predicate (car pair)))
                    (distribution-variable-distribution d))))

   (define (the-elements predicate d)
     (remove-if-not (lambda (pair) (predicate (car pair)))
                    (distribution-variable-distribution d)))

   (define (attach-demon! demon d)
     (distribution-variable-demons-local-set!
       d (cons demon (distribution-variable-demons d)))
     (demon))

   (define (assert-constraint-efd! constraint ds)
     (for-each
       (lambda (d)
         (attach-demon! (lambda ()
                          (when (every bound? ds)
                            (unless (apply constraint (map binding ds)) (bottom))))
                        d))
       ds))

   (define (assert-constraint-fc! constraint ds)
     (for-each
       (lambda (d)
         (attach-demon!
           (lambda ()
             (when (one (lambda (d) (not (bound? d))) ds)
               (let* ((i (position-if (lambda (d) (not (bound? d))) ds))
                      (d (list-ref ds i)))
                 (unless (some-element
                           (lambda (x)
                             (apply
                               constraint
                               (map-indexed (lambda (d j) (if (= j i) x (binding d))) ds)))
                           d)
                   (bottom)))))
           d))
       ds))

   (define (assert-constraint-vp! constraint ds)
     (for-each
       (lambda (d)
         (attach-demon!
           (lambda ()
             (when (one (lambda (d) (not (bound? d))) ds)
               (let* ((i (position-if (lambda (d) (not (bound? d))) ds))
                      (d (list-ref ds i)))
                 (when (one-element
                         (lambda (x)
                           (apply
                             constraint
                             (map-indexed (lambda (d j) (if (= j i) x (binding d))) ds)))
                         d)
                   (restrict-distribution!
                     d
                     (the-element
                       (lambda (x)
                         (apply constraint
                                (map-indexed (lambda (d j) (if (= j i) x (binding d))) ds)))
                       d))))))
           d))
       ds))

   (define (assert-constraint-gfc! constraint ds)
     (for-each
       (lambda (d)
         (attach-demon!
           (lambda ()
             (when (one (lambda (d) (not (bound? d))) ds)
               (let* ((i (position-if (lambda (d) (not (bound? d))) ds))
                      (d (list-ref ds i)))
                 (restrict-distribution!
                   d
                   (the-elements
                     (lambda (x)
                       (apply constraint
                              (map-indexed (lambda (d j) (if (= j i) x (binding d))) ds)))
                     d)))))
           d))
       ds))

   (define (assert-constraint-ac! constraint ds)
     (for-each
       (lambda (d)
         (attach-demon!
           (lambda ()
             (for-each-indexed
               (lambda (d i)
                 (restrict-distribution!
                   d
                   (the-elements
                     (lambda (x)
                       (let loop ((ds ds) (xs '()) (j 0))
                         (if (null? ds)
                           (apply constraint (reverse xs))
                           (if (= j i)
                             (loop (rest ds) (cons x xs) (+ j 1))
                             (some-element
                               (lambda (pair) (loop (rest ds) (cons (car pair) xs) (+ j 1)))
                               (first ds))))))
                     d)))
               ds))
           d))
       ds))

   (define (assert-stochastic-constraint! constraint . ds)
     (case *strategy*
       ((efd) (assert-constraint-efd! constraint ds))
       ((fc) (assert-constraint-efd! constraint ds)
             (assert-constraint-fc! constraint ds))
       ((vp) (assert-constraint-efd! constraint ds)
             (assert-constraint-fc! constraint ds)
             (assert-constraint-vp! constraint ds))
       ((gfc) (assert-constraint-efd! constraint ds)
              (assert-constraint-gfc! constraint ds))
       ((ac) (assert-constraint-ac! constraint ds))
       (else (error #f "Unrecognized strategy"))))

   ))

