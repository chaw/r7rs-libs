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
 (autodiff deterministic-memoization)
 (export memoize)
 (import (scheme base))

 (begin

   (define (memoize f)
     (let ((cache '()))
       (lambda arguments
         (let ((entry (assoc arguments cache)))
           (if entry
             (cdr entry)
             (let ((result (apply f arguments)))
               (set! cache (cons (cons arguments result) cache))
               result))))))

   ))

