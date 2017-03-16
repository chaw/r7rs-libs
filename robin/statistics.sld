;; Statistics library for R7RS Scheme

;; Written by Peter Lane, 2017

;; # Open Works License
;; 
;; This is version 0.9.4 of the Open Works License
;; 
;; ## Terms
;; 
;; Permission is hereby granted by the holder(s) of copyright or other legal
;; privileges, author(s) or assembler(s), and contributor(s) of this work, to any
;; person who obtains a copy of this work in any form, to reproduce, modify,
;; distribute, publish, sell, sublicense, use, and/or otherwise deal in the
;; licensed material without restriction, provided the following conditions are
;; met:
;; 
;; Redistributions, modified or unmodified, in whole or in part, must retain
;; applicable copyright and other legal privilege notices, the above license
;; notice, these conditions, and the following disclaimer.
;; 
;; NO WARRANTY OF ANY KIND IS IMPLIED BY, OR SHOULD BE INFERRED FROM, THIS LICENSE
;; OR THE ACT OF DISTRIBUTION UNDER THE TERMS OF THIS LICENSE, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
;; AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS, ASSEMBLERS, OR HOLDERS OF
;; COPYRIGHT OR OTHER LEGAL PRIVILEGE BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER
;; LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT, OR OTHERWISE ARISING FROM, OUT
;; OF, OR IN CONNECTION WITH THE WORK OR THE USE OF OR OTHER DEALINGS IN THE WORK.

(define-library
  (robin statistics)
  (export mean
          arithmetic-mean
          geometric-mean
          harmonic-mean
          median
          mode
          percentile
          population-standard-deviation
          population-variance
          standard-deviation
          variance
          coefficient-of-variation
          standard-error-of-the-mean
          )
  (import (scheme base)
          (scheme case-lambda)
          (scheme inexact)
          (srfi 1)
          (srfi 69)
          (srfi 95))

  (begin

    (define (arithmetic-mean lst)
      (if (null? lst)
        0
        (/ (fold + 0 lst) (length lst))))

    (define mean arithmetic-mean) ;; make popular synonym

    (define (geometric-mean lst)
      (expt (fold * 1 lst) (/ 1 (length lst))))

    (define (harmonic-mean lst)
      (if (null? lst)
        0
        (/ 1 (* (/ 1 (length lst))
                (fold (lambda (n v) (+ (/ 1 n) v)) 0 lst)))))

    (define (median lst)
      (percentile lst 50))

    (define (mode lst)
      (if (null? lst)
        (error "Mode: List must not be null")
        (let ((count-table (make-hash-table eqv?))
              (modes '())
              (mode-count 0))
          (for-each 
            (lambda (item) 
              (hash-table-set! count-table
                               item
                               (+ 1 (hash-table-ref/default count-table item 0))))
            lst)
          (for-each 
            (lambda (key) 
              (let ((val (hash-table-ref/default count-table key #f)))
                (cond ((> val mode-count) ; keep mode
                       (set! modes (list key))
                       (set! mode-count val))
                      ((= val mode-count) ; store multiple modes
                       (set! modes (cons key modes))))))
            (hash-table-keys count-table))
          (cond ((every number? modes) (set! modes (sort modes <)))
                ((every string? modes) (set! modes (sort modes string<?)))
                )
          (values modes mode-count))))

  (define (percentile lst percent)
    (if (null? lst)
      (error "Percentile: List must not be null")
      (let* ((sorted-vec (apply vector (sort lst <)))
             (n (vector-length sorted-vec))
             (k (* n (/ percent 100)))
             (floor-k (floor k)))
        (if (= k floor-k)
          (/ (+ (vector-ref sorted-vec k)
                (vector-ref sorted-vec (- k 1)))
             2)
          (vector-ref sorted-vec floor-k)))))

  (define general-variance
    (case-lambda
      ((type lst)
       (general-variance type lst (mean lst)))
      ((type lst mean1)
       (if (< (length lst) 2)
         (error "for variances, List must contain at least 2 elements")
         (/ (fold + 0 (map (lambda (x) (square (- mean1 x))) lst))
            (case type
              ((sample)
                (- (length lst) 1))
              ((population)
                (length lst))
              (else
                (error "Internal error: unknown type for general-variance"))))))))

  (define general-standard-deviation
    (case-lambda 
      ((type lst)
       (general-standard-deviation type lst (mean lst)))
      ((type lst mean1)
       (sqrt (general-variance type lst mean1)))))

  (define (variance . args)
    (apply general-variance (cons 'sample args)))

  (define (standard-deviation . args)
    (apply general-standard-deviation (cons 'sample args)))

  (define (population-variance . args)
    (apply general-variance (cons 'population args)))

  (define (population-standard-deviation . args)
    (apply general-standard-deviation (cons 'population args)))

  (define (coefficient-of-variation lst)
    (* 100
       (/ (standard-deviation lst)
          (mean lst))))

  (define (standard-error-of-the-mean lst)
    (/ (standard-deviation lst)
       (sqrt (length lst))))

  ))

