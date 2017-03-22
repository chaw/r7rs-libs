;; EXPERIMENTAL

;; Exploring Lisp's Series, as described by Richard Waters, 1989.

(define-library
  (robin series)
  (export 
    ; scanners 
    scan
    scan-range
    ; transducers
    choose-if
    subseries
    ; collectors
    collect-sum
    collect
    collect-first
    collect-length
    ; hof
    map-fn
    scan-fn
    collect-fn
    )
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1))

  (begin

    (define-record-type <series>
                        (make-series lst)
                        series?
                        (lst series-lst))

    ;; Scanners

    (define (scan seq)
      (cond ((list? seq)
             (make-series seq))
            ((vector? seq)
             (make-series (vector->list seq)))))

    (define scan-range
      (case-lambda
        ((total)
         (scan-range total 0 1))
        ((total start)
         (scan-range total start 1))
        ((total start step)
         (make-series (iota total start step)))))

    ;; Transducers

    (define (choose-if proc series)
      (make-series (filter proc (series-lst series))))

    (define (subseries series start end)
      (make-series (take (drop (series-lst series) start)
                         (- end start) )))

    ;; Collectors

    (define (collect-sum series)
      (apply + (series-lst series)))

    (define (collect series)
      (series-lst series))

    (define (collect-first series)
      (car (series-lst series)))

    (define (collect-length series)
      (length (series-lst series)))

    ;; Higher Order Functions

    (define (map-fn proc series)
      (make-series (map proc (series-lst series))))

    (define (scan-fn init-proc next-proc end-proc?)
      (do ((res (list (init-proc)) (cons (next-proc (car res)) res)))
        ((end-proc? (car res)) 
         (make-series (reverse (cdr res))))))

    (define (collect-fn init-proc accum-proc series)
      (fold accum-proc (init-proc) (series-lst series)))

    ))

