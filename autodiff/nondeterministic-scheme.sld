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
 (autodiff nondeterministic-scheme)
 (export a-boolean fail domain remove-duplicates a-member-of possibly? amb
	 upon-fail)
 (import (scheme base) 
         (autodiff QobiScheme))

 (begin

   (define (*a-boolean*) (error #f "Top-level a-boolean"))

   (define (a-boolean) (*a-boolean*))

   (define (*fail*) (error #f "Top-level fail"))

   (define (fail) (*fail*))

   (define (domain-thunk thunk)
     (call-with-current-continuation
       (lambda (c)
         (let ((domain '()) (saved-a-boolean *a-boolean*) (saved-fail *fail*))
           (set! *a-boolean*
             (lambda ()
               (call-with-current-continuation
                 (lambda (c)
                   (let ((saved-fail *fail*))
                     (set! *fail*
                       (lambda ()
                         (set! *fail* saved-fail)
                         (c #f))))
                   #t))))
           (set! *fail*
             (lambda ()
               (set! *a-boolean* saved-a-boolean)
               (set! *fail* saved-fail)
               (c (reverse domain))))
           (let ((value (thunk)))
             (set! domain (cons value domain)))
           (fail)))))

   (define-syntax domain
     (syntax-rules () ((domain e) (domain-thunk (lambda () e)))))

   (define (remove-duplicates domain)
     (let loop ((domain domain) (new-domain '()))
       (cond ((null? domain) (reverse new-domain))
             ((member (first domain) new-domain) (loop (rest domain) new-domain))
             (else (loop (rest domain) (cons (first domain) new-domain))))))

   (define (a-member-of domain)
     (let loop ((domain domain))
       (cond ((null? domain) (fail))
             ((a-boolean) (first domain))
             (else (loop (rest domain))))))

   (define (possibly? domain) (some (lambda (x) x) domain))

   (define-syntax amb (syntax-rules () ((amb e1 e2) (if (a-boolean) e1 e2))))

   (define (upon-fail-thunk thunk)
     (let ((saved-fail *fail*)) (set! *fail* (lambda () (thunk) (saved-fail)))))

   (define-syntax upon-fail
     (syntax-rules () ((upon-fail e) (upon-fail-thunk (lambda () e)))))

   ))

