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
    positions
    choose
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
          (scheme write)        ; for development
          (srfi 1))

  (begin

    ;; gen: lambda expression, (gen 'more?) -> #t/#f 
    ;;                         (gen 'next) -> next value
    (define-record-type <series>
                        (make-series gen data)
                        series?
                        (gen series-gen)
                        (data series-data series-data-set!))

    ;; Scanners

    (define (scan seq)
      (cond ((list? seq)
             (make-series (let ((lst seq))
                            (lambda (query)
                              (case query
                                ((more?) (not (null? lst)))
                                ((next) (let ((next (car lst)))
                                          (set! lst (cdr lst))
                                          next))
                                (else
                                  (error "unknown query to series")))))
                          '()))
            ((vector? seq)
             (make-series (let ((posn 0))
                            (lambda (query)
                              (case query
                                ((more?) (< posn (vector-length seq)))
                                ((next) (set! posn (+ 1 posn))
                                        (vector-ref seq (- posn 1)))
                                (else
                                  (error "unknown query to series")))))
                          '()))))

    (define scan-range ;; TODO - generators
      (case-lambda
        ((total)
         (scan-range total 0 1))
        ((total start)
         (scan-range total start 1))
        ((total start step)
         (scan (iota total start step)))))

    ;; Transducers

    (define (choose-if proc series)
      (make-series (let* ((item #f)        ; the next item in series matching proc
                          (has-item? #f)   ; flag to indicate if there is a next item
                          (seek            ; function to find next matching item
                            (lambda () 
                              (let loop ()
                                (if ((series-gen series) 'more?)
                                  (let ((next ((series-gen series) 'next)))
                                    (if (proc next)
                                      (begin (set! item next)
                                             (set! has-item? #t))
                                      (loop)))
                                  (set! has-item? #f))))))
                     (lambda (query)
                       (case query
                         ((more?) 
                          ; look forward to see if there is an item matching proc
                          (seek)
                          has-item?)
                         ((next) 
                          (if has-item? ; we have a stored item
                            (begin ; so unmark it, and return
                              (set! has-item? #f)
                              item)
                            (begin ; otherwise, find the next one
                              (seek)
                              (if has-item?
                                item)
                              (error "no next item in series")))))))
                   '()))

  (define (subseries series start end) ;; TODO
    (make-series (take (drop (series-lst series) start)
                       (- end start) )))

  ;; return a new series of positions of non-#f items (in Lisp, nil)
  (define (positions series)
      (make-series (let* ((posn -1)        ; current position in series
                          (has-item? #f)   ; flag to indicate if there is a next item
                          (seek            ; function to find next matching item
                            (lambda () 
                              (let loop ()
                                (if ((series-gen series) 'more?)
                                  (let ((next ((series-gen series) 'next)))
                                    (set! posn (+ 1 posn))
                                    (if (not (eq? #f next))
                                      (set! has-item? #t)
                                      (loop)))
                                  (set! has-item? #f))))))
                     (lambda (query)
                       (case query
                         ((more?) 
                          ; look forward to see if there is an item matching proc
                          (seek)
                          has-item?)
                         ((next) 
                          (if has-item? ; we have a stored item
                            (begin ; so unmark it, and return
                              (set! has-item? #f)
                              posn)
                            (begin ; otherwise, find the next one
                              (seek)
                              (if has-item?
                                posn)
                              (error "no next item in series")))))))
                   '()))

  ;; return a new series with members of second series only where first is not #f
  (define (choose flags series)
    (let loop ((res '()))
      (if (and ((series-gen flags) 'more?)
               ((series-gen series) 'more?))
        (let ((next-flag ((series-gen flags) 'next))
              (next-item ((series-gen series) 'next)))
          (if next-flag
            (loop (cons next-item res))
            (loop res)))
        (scan (reverse res)))))

  ;; Collectors

  (define (collect-sum series)
    (do ((sum 0 (+ sum ((series-gen series) 'next))))
      ((not ((series-gen series) 'more?)) sum)))

  ;; TODO: collect to a designated type?
  (define (collect series)
    (do ((res '() (cons ((series-gen series) 'next) res)))
      ((not ((series-gen series) 'more?)) (reverse res))))

  (define (collect-first series)
    ((series-gen series) 'next))

  (define (collect-length series)
    (do ((len 0 (+ len 1)))
      ((not ((series-gen series) 'more?)) len)
      ((series-gen series) 'next))) ; read next value, but discard it

  ;; Higher Order Functions

  (define (map-fn proc series)
    (make-series (lambda (query)
                   (case query
                     ((more?) ((series-gen series) 'more?))
                     ((next) (proc ((series-gen series) 'next)))))
                 '()))

  (define (scan-fn init-proc next-proc end-proc?)
    (make-series (let ((val (init-proc)))
                   (lambda (query)
                     (case query
                       ((more?) (not (end-proc? val)))
                       ((next) (let ((res val))
                                 (set! val (next-proc val))
                                 res)))))
                 '()))

  (define (collect-fn init-proc accum-proc series)
    (do ((res (init-proc) (accum-proc ((series-gen series) 'next) res)))
      ((not ((series-gen series) 'more?)) res)))

  ))

