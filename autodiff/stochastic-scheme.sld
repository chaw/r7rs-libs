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
 (autodiff stochastic-scheme)
 (export flip bottom distribution coalesce-duplicates draw-pair draw
	 probability support expected-value entropy upon-bottom)
 (import (except (scheme base) + - * / = < > <= >= zero? expt positive? negative? real?)
	 (autodiff AD)
	 (autodiff QobiScheme))

 (begin

   (define (*flip* alpha) (error #f "Top-level flip"))

   (define (flip alpha) (*flip* alpha))

   (define (*bottom*) (error #f "Top-level bottom"))

   (define (bottom) (*bottom*))

   (define (distribution-thunk thunk)
     (call-with-current-continuation
       (lambda (c)
         (let ((distribution '()) (saved-flip *flip*) (saved-bottom *bottom*) (p 1))
           (set! *flip*
             (lambda (alpha)
               (unless (<= 0 alpha 1) (error #f "Alpha not probability"))
               (call-with-current-continuation
                 (lambda (c)
                   (let ((saved-bottom *bottom*) (saved-p p))
                     (set! p (* alpha p))
                     (set! *bottom*
                       (lambda ()
                         (set! p (* (- 1 alpha) saved-p))
                         (set! *bottom* saved-bottom)
                         (c #f)))
                     #t)))))
           (set! *bottom*
             (lambda ()
               (set! *flip* saved-flip)
               (set! *bottom* saved-bottom)
               (c (reverse distribution))))
           (let ((value (thunk)))
             (set! distribution (cons (cons value p) distribution)))
           (bottom)))))

   (define-syntax distribution
     (syntax-rules () ((distribution e) (distribution-thunk (lambda () e)))))

   (define (coalesce-duplicates distribution)
     (let loop ((distribution distribution) (new-distribution '()))
       (cond ((null? distribution) (reverse new-distribution))
             ((zero? (cdr (first distribution)))
              (loop (rest distribution) new-distribution))
             ((position-if
                (lambda (pair) (equal? (car pair) (car (first distribution))))
                new-distribution)
              => (lambda (i)
                   (loop (rest distribution)
                         (map-indexed
                           (lambda (pair j)
                             (if (= i j)
                               (cons (car pair)
                                     (+ (cdr (first distribution)) (cdr pair)))
                               pair))
                           new-distribution))))
             (else (loop (rest distribution)
                         (cons (first distribution) new-distribution))))))

   (define (draw-pair distribution)
     (define (min x1 x2) (if (< x1 x2) x1 x2))
     (define (max x1 x2) (if (> x1 x2) x1 x2))
     (let loop ((distribution distribution) (p 1))
       (cond
         ((or (zero? p) (null? distribution)) (bottom))
         ((flip (min (/ (cdr (first distribution)) p) 1)) (first distribution))
         (else
           (loop (rest distribution) (max (- p (cdr (first distribution))) 0))))))

   (define (draw distribution) (car (draw-pair distribution)))

   (define (probability distribution)
     (fold + 0 (map cdr (remove-if-not car distribution))))

   (define (support distribution) (map car distribution))

   (define (expected-value distribution)
     (fold + 0 (map (lambda (pair) (* (car pair) (cdr pair))) distribution)))

   (define (entropy distribution)
     (fold
       + 0 (map (lambda (pair) (* (cdr pair) (log (cdr pair)))) distribution)))

   (define (upon-bottom-thunk thunk)
     (let ((saved-bottom *bottom*))
       (set! *bottom* (lambda () (thunk) (saved-bottom)))))

   (define-syntax upon-bottom
     (syntax-rules () ((upon-bottom e) (upon-bottom-thunk (lambda () e)))))

   ))

