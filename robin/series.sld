;; Series library for R7RS Scheme.

;; Exploring Lisp's Series, as described by Richard Waters, 1989.
;; Function names and definitions are taken from the Series 'Basic Reference Manual', pp.10-35

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


;; TODO
;; 1. Complete remaining functions, where appropriate
;; 2. Allow series to be used more than once (add a 'make-gen' function, and use the data field?)
;; 3. Include X-hash functions if (srfi 69) present
;; 4. Better support Kawa - i.e. allow Java collections to be scanned
;; 5. Error reporting?
;; 6. Use Lisp implementation to help program an efficient version (?)

(define-library
  (robin series)
  (export 
    ; series datatype
    series
    series?
    make-series
    ; scanners 
    scan
    scan-list
    scan-vector
    scan-string
    scan-range
    scan-sublists
    scan-alist
    scan-file
    scan-fn
    scan-fn-inclusive
    ; transducers
    ;; -- mapping
    map-fn
    ; -- truncation
    until
    until-if
    ; -- other on-line transducers
    previous
    ; -- choosing and expanding
    choose
    choose-if
    choose-if-not
    spread
    expand
    ; -- other off-line transducers
    catenate
    subseries
    positions
    mask
    mingle
    ; collectors
    collect-last
    collect-first
    collect-nth
    collect
    collect-string
    collect-append
    collect-alist
    collect-file
    collect-length
    collect-sum
    collect-max
    collect-min
    collect-and
    collect-or
    collect-fn
    )
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (only (scheme list) iota))

  (begin

    ;; gen: lambda expression, (gen 'more?) -> #t/#f 
    ;;                         (gen 'next) -> next value
    ;; TODO: when 'data' in use, allow a 'peek' operation
    (define-record-type <series>
                        (create-series gen data size posn)
                        series?
                        (gen series-gen)
                        (data series-data series-data-set!)
                        (size series-data-size series-data-size-set!)
                        (posn series-posn series-posn-set!))

    (define (new-series gen) (create-series gen (make-vector 2) 2 0))
    (define (more? series) ((series-gen series) 'more?))
    (define (next series) ; stores next item in 'data' and returns the value
      (let ((val ((series-gen series) 'next)))
        ; if data store is too small, expand it
        (when (= (series-posn series) (series-data-size series))
          (let ((new-vec (make-vector (* 2 (series-data-size series)))))
            (for-each (lambda (i) (vector-set! new-vec i (vector-ref (series-data series) i)))
                      (iota (series-data-size series)))
            (series-data-set! series new-vec)
            (series-data-size-set! series (vector-length new-vec))))
        (vector-set! (series-data series) (series-posn series) val)
        (series-posn-set! series (+ 1 (series-posn series)))
        val))

    ;; An unbounded series of the given items
    (define series 
      (lambda args
        (new-series
          (let ((rem args))
            (lambda (query)
              (case query
                ((more?) #t) ; always more, as unbounded
                ((next) (when (null? rem)
                          (set! rem args))
                        (let ((item (car rem)))
                          (set! rem (cdr rem))
                          item))))))))

    ;; A bounded series of the given items
    (define make-series (lambda args (scan args)))

    ;;  ***** Scanners *****

    (define (scan seq) 
      (cond ((list? seq) (scan-list seq))
            ((vector? seq) (scan-vector seq))
            ((string? seq) (scan-string seq))))

    (define (scan-list seq)
      (new-series (let ((lst seq))
                    (lambda (query)
                      (case query
                        ((more?) (not (null? lst)))
                        ((next) (let ((next (car lst)))
                                  (set! lst (cdr lst))
                                  next))
                        (else
                          (error "unknown query to series")))))))

    (define (scan-vector seq)
      (new-series (let ((posn 0))
                    (lambda (query)
                      (case query
                        ((more?) (< posn (vector-length seq)))
                        ((next) (set! posn (+ 1 posn))
                                (vector-ref seq (- posn 1)))
                        (else
                          (error "unknown query to series")))))))

    (define (scan-string seq)
      (new-series (let ((posn 0))
                    (lambda (query)
                      (case query
                        ((more?) (< posn (string-length seq)))
                        ((next) (set! posn (+ 1 posn))
                                (string-ref seq (- posn 1)))
                        (else
                          (error "unknown query to series")))))))

    ; scan-multiple

    ;; as we don't have keywords, just allow [start step stop] parameters
    (define scan-range 
      (case-lambda
        ((start)
         (scan-range start 1 #f))
        ((start step)
         (scan-range start step #f))
        ((start step stop)
         (new-series (let ((n start))
                       (lambda (query)
                         (case query
                           ((more?) (cond ((eq? stop #f)   ; we never stop
                                           #t)
                                          ((> start stop)  ; counting down
                                           (<= n stop))
                                          (else            ; counting up
                                            (> n stop))))
                           ((next) (let ((curr n))
                                     (set! n (+ n step))
                                     curr))
                           (else
                             (error "unknown query to series")))))))))

    ;; Creates a series containing the successive sublists of lst
    (define (scan-sublists lst)
      (new-series (let ((rem lst))
                    (lambda (query)
                      (case query
                        ((more?) (not (null? rem)))
                        ((next) (let ((curr rem))
                                  (set! rem (cdr rem))
                                  curr)))))))

    ;; Returns two series as values: one for keys, one for values
    (define (scan-alist alist)
      (values (new-series (let ((rem alist))
                            (lambda (query)
                              (case query
                                ((more?) (not (null? rem)))
                                ((next) (let ((curr (caar rem)))
                                          (set! rem (cdr rem))
                                          curr))))))
              (new-series (let ((rem alist))
                            (lambda (query)
                              (case query
                                ((more?) (not (null? rem)))
                                ((next) (let ((curr (cdar rem)))
                                          (set! rem (cdr rem))
                                          curr))))))))

    ; scan-plist - not relevant to Scheme
    ; scan-hash
    ; scan-lists-of-lists
    ; scan-lists-of-lists-fringe
    ; scan-symbols - not relevant to Scheme

    ;; Opens given filename and uses reader repeatedly until end of file
    (define scan-file
      (case-lambda
        ((filename)
         (scan-file filename read-line))
        ((filename reader)
         (if (file-exists? filename)
           (new-series
             (let ((port (open-input-file filename))
                   (done #f))
               (lambda (query)
                 (case query
                   ((more?) 
                    (if done
                      #f
                      (if (eof-object? (peek-char port))
                        (begin (close-input-port port)
                               (set! done #t)
                               #f)
                        #t)))
                   ((next) (if done
                             (error "eof reached")
                             (guard (condition ; silently catch and ignore any reader errors
                                      (else (close-input-port port)
                                            (set! done #t)
                                            #f))
                                    (let ((res (reader port)))
                                      (when (eof-object? res)
                                        (close-input-port port)
                                        (set! done #t))
                                      res))))))))
           (error "File does not exist")))))

    ;; Higher-order function supports generic concept of scanning
    (define (scan-fn init-proc next-proc end-proc?)
      (new-series (let ((val (init-proc)))
                    (lambda (query)
                      (case query
                        ((more?) (not (end-proc? val)))
                        ((next) (let ((res val))
                                  (set! val (next-proc val))
                                  res)))))))

    ;; same as above, except includes the first end-proc? passing item
    (define (scan-fn-inclusive init-proc next-proc end-proc?)
      (new-series (let ((val (init-proc))
                        (done? #f))
                    (lambda (query)
                      (case query
                        ((more?) (not done?))
                        ((next) (let ((res val))
                                  (set! val (next-proc val))
                                  (set! done? (end-proc? res))
                                  res)))))))
    ;; ***** Mapping *****

    (define (map-fn proc series)
      (new-series (lambda (query)
                    (case query
                      ((more?) (more? series))
                      ((next) (proc (next series)))))))

    ; mapping
    ; iterate 

    ;;  ***** Truncation *****

    ;; returns items up to, but not including, first non-false element in bools
    ;; TODO: make this work with multiple series
    (define (until bools series-1)
      (new-series (lambda (query)
                    (case query
                      ((more?) (and (more? bools)
                                    (not (next bools))
                                    (more? series-1)))
                      ((next) (next series-1)))) ; TODO: this assumes more?
                  ))

    ;; Returns items up to, but not including, first non-false result from test
    ;; TODO: make this work with multiple series
    (define (until-if pred? series-1)
      (new-series (let ((item #f)
                        (has-item? #f))
                    (lambda (query)
                      (case query
                        ((more?) (when (and (not has-item?)
                                            (more? series-1))
                                   (set! item (next series-1))
                                   (set! has-item? #t))                                   
                                 (and (not (pred? item))
                                      (more? series-1)))
                        ((next) (when (and (not has-item?)
                                           (more? series-1))
                                  (set! item (next series-1))
                                  (set! has-item? #t))
                                (set! has-item? #f)
                                item)))))) ; TODO: this assumes more?

    ;; Inputs and outputs are all series: output series are all the length of shortest input series
    ;; TODO: requires option to revisit series - i.e. to visit all in parallel
    ;    (define cotruncate 
    ;      (lambda series
    ;        (when (zero? (length series)) (error "Cotruncate requires at least one input series"))
    ;        (apply values
    ;               (map (lambda (a-series) 
    ;                      (new-series
    ;                        (lambda (query)
    ;                          (case query
    ;                            ((more?) (every more? series))
    ;                            ((next) (next a-series))))
    ;                        ))
    ;                    series))))


    ;; ***** Other On-Line Transducers *****

    (define (bounded-queue n) ; Only for use by 'previous', not a complete implementation
      (let ((queue (make-vector n))
            (top 0)
            (last 0))
        (lambda (op)
          (case op
            ((push)
             (lambda (item)
               (vector-set! queue top item)
               (set! top (modulo (+ 1 top) n))))
            ((pop)
             (let ((item (vector-ref queue last)))
               (set! last (modulo (+ 1 last) n))
               item))))))

    (define (queue-push q obj) ((q 'push) obj))
    (define (queue-pop q) (q 'pop))

    ;; Creates a series shifted right by 'amount' elements
    ;; and the last 'amount' elements of series are lost
    (define previous
      (case-lambda 
        ((series)
         (previous series #f 1))
        ((series default)
         (previous series default 1))
        ((series default amount)
         (let ((q (bounded-queue amount)))
           ; fill queue with default
           (do ((i 0 (+ 1 i)))
             ((= i amount) )
             (queue-push q default))
           ; define series
           (new-series (lambda (query)
                         (case query
                           ((more?) (more? series))
                           ((next) (let ((res (queue-pop q)))
                                     (queue-push q (next series))
                                     res)))))))))


    ; latch ; - difficult without keywords (use an association list?)
    ; collecting-fn

    ;; ***** Choosing and Expanding *****

    ;; return a new series with members of second series only where first is not #f
    (define (choose flags series)
      (let loop ((res '()))
        (if (and (more? flags)
                 (more? series))
          (let ((next-flag (next flags))
                (next-item (next series)))
            (if next-flag
              (loop (cons next-item res))
              (loop res)))
          (scan (reverse res)))))

    (define (choose-if proc series)
      (new-series (let* ((item #f)        ; the next item in series matching proc
                         (has-item? #f)   ; flag to indicate if there is a next item
                         (seek            ; function to find next matching item
                           (lambda () 
                             (let loop ()
                               (if (more? series)
                                 (let ((next (next series)))
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
                             (error "no next item in series")))))))))

    (define (choose-if-not proc series)
      (choose-if (lambda (obj) (not (proc obj))) series))

    ;; Output elements of series spread out by default value in positions of 'gaps' 
    (define spread
      (case-lambda
        ((gaps series)
         (spread gaps series #f))
        ((gaps series default)
         (new-series (let ((defs (if (more? gaps) ; remaining number of defs to output
                                   (next gaps)    ; initially the first value of gaps, if exists
                                   0))
                           (last-was-value? #f)) 
                       (lambda (query)
                         (case query
                           ((more?) (if last-was-value? ; if we just output from series, we must have defs to output and more in series
                                      (and (> defs 0) (more? series))
                                      (more? series))) ; otherwise need more in series
                           ((next) (cond ((> defs 0) ; outputting the defaults
                                          (set! defs (- defs 1))
                                          (set! last-was-value? #f)
                                          default)
                                         ((zero? defs) ; finished defaults, so output value
                                          (let ((val (next series)))
                                            (when (more? gaps)
                                              (set! defs (next gaps)))
                                            (set! last-was-value? #t)
                                            val)))))))))))

    ;; Places series items into #t slots in booleans
    (define expand
      (case-lambda 
        ((bools series)
         (expand bools series #f))
        ((bools series default)
         (new-series (let ((next-bool #f)
                           (has-next? #f))
                       (lambda (query)
                         (case query
                           ((more?) 
                            (if (and (not has-next?)      ; no more bools, so end
                                     (not (more? bools)))
                              #f
                              (begin ; must be some more bools, so make sure we can see it
                                (unless has-next?
                                  (set! next-bool (next bools))
                                  (set! has-next? #t))
                                (or (not next-bool)    ; either #f
                                    (more? series))))) ; or there are more in series
                           ((next) (unless has-next? ; make sure we have the next one ready
                                     (set! next-bool (next bools)))
                                   (set! has-next? #f) ; we will consume the next bool, so flag to #f
                                   (if next-bool
                                     (next series)
                                     default)))))))))

    ; split
    ; split-if

    ;; ***** Other Off-Line Transducers *****

    ;; Creates a series by joining together two or more series
    (define (catenate series1 series2 . series)
      (new-series (let ((curr series1)
                        (rem (cons series2 series)))
                    (lambda (query)
                      (case query
                        ((more?) (if (more? curr)
                                   #t
                                   (let loop ()
                                     (if (null? rem)
                                       #f
                                       (begin (set! curr (car rem))
                                              (set! rem (cdr rem))
                                              (if (more? curr) 
                                                #t
                                                (loop)))))))
                        ((next) (unless (more? curr) ; curr is finished, so try next
                                  (when (null? rem) (error "no more series left in catenate/next"))
                                  (set! curr (car rem))
                                  (set! rem (cdr rem)))
                                (next curr)))))))

    ;; Creates a series containing an indexed subseries of given series.
    (define subseries
      (case-lambda 
        ((series start)
         (subseries series start #f))
        ((series start end)
         ; skip start elements
         (do ((i 0 (+ i 1)))
           ((= i start) )
           (next series))
         ; make series to return next (end-start) elements
         (new-series
           (let ((posn start))
             (lambda (query)
               (case query
                 ((more?) (and (or (not (number? end)) 
                                   (< posn end))
                               (more? series)))
                 ((next) (set! posn (+ 1 posn))
                         (next series)))))))))

    ;; return a new series of positions of non-#f items (in Lisp, nil)
    (define (positions series)
      (new-series (let* ((posn -1)        ; current position in series
                         (has-item? #f)   ; flag to indicate if there is a next item
                         (seek            ; function to find next matching item
                           (lambda () 
                             (let loop ()
                               (if (more? series)
                                 (let ((next (next series)))
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
                             (error "no next item in series")))))))))

    ;; given a monotonically increasing series of non-negative integers,
    ;; returns a series of booleans with #t at the given integer positions
    (define (mask series)
      (new-series (let* ((rem (if (more? series) series #f))
                         (index (if rem (next rem) rem))
                         (posn -1))
                    (lambda (query)
                      (case query
                        ((more?) #t)  ; unbounded series
                        ((next) 
                         (set! posn (+ 1 posn))
                         (cond ((eq? #f rem) ; if rem #f, we just keep outputting #f
                                #f)
                               ((and (number? index) ; if we have a next number
                                     (< posn index)) ; and posn not at next
                                #f)
                               ((and (number? index)
                                     (= posn index))
                                (if (more? rem)
                                  (set! index (next rem))
                                  (begin (set! rem #f)
                                         (set! index #f)))
                                #t)
                               (else ; else, there is no next number, get it from rem
                                 (if (more? rem)
                                   (set! index (next rem))
                                   (begin (set! rem #f)
                                          (set! index #f)))
                                 (= posn index)))))))))

    (define (mingle series-1 series-2 comparator<?)
      (new-series (let ((has-item-1 #f)
                        (has-item-2 #f)
                        (peek-1 #f)
                        (peek-2 #f)
                        )
                    (lambda (query)
                      (case query
                        ((more?) (or has-item-1
                                     has-item-2
                                     (more? series-1)
                                     (more? series-2)))
                        ((next) 
                         ; fill in peeks
                         (when (and (not has-item-1)
                                    (more? series-1))
                           (set! peek-1 (next series-1))
                           (set! has-item-1 #t))
                         (when (and (not has-item-2)
                                    (more? series-2))
                           (set! peek-2 (next series-2))
                           (set! has-item-2 #t))
                         (cond ((and has-item-1 has-item-2) ; both present
                                (if (comparator<? peek-2 peek-1)
                                  (begin (set! has-item-2 #f)
                                         peek-2)
                                  (begin (set! has-item-1 #f)
                                         peek-1)))
                               (has-item-1
                                 (set! has-item-1 #f)
                                 peek-1)
                               (has-item-2
                                 (set! has-item-2 #f)
                                 peek-2)
                               (else
                                 (error "no items remaining")))))))))

    ; chunk

    ;; ***** Collectors *****

    ;; returns the last element in the series
    (define collect-last
      (case-lambda 
        ((series)
         (collect-last series #f))
        ((series default)
         (let loop ((last default))
           (if (more? series)
             (loop (next series))
             last)))))

    ;; returns the first element in the series
    (define collect-first
      (case-lambda 
        ((series)
         (collect-first series #f))
        ((series default)
         (if (more? series)
           (next series)
           default))))

    ;; returns the nth element in the series
    (define collect-nth
      (case-lambda 
        ((n series) 
         (collect-nth n series #f))
        ((n series default)
         (let loop ((last default)
                    (rem n))
           (if (and (more? series)
                    (>= rem 0))
             (loop (next series)
                   (- rem 1))
             last)))))

    ;; returns series collected into a list
    ;; TODO: collect to a designated type?  or write collect-vector/collect-string
    (define (collect series)
      (do ((res '() (cons (next series) res)))
        ((not (more? series)) (reverse res))))

    ;; Returns a string from appending given series
    (define (collect-string series)
      (do ((res "" (string-append res (next series))))
        ((not (more? series)) res)))

    ;; Given a series of series, return a list with all appended together
    (define (collect-append serieses)
      (if (more? serieses)
        (let loop ((series (next serieses))
                   (res '()))
          (cond ((more? series)
                 (loop series (cons (next series) res)))
                ((more? serieses)
                 (loop (next serieses) res))
                (else
                  (reverse res))))
        '()))

    ;; collect-nconc

    ;; returns an association list, pairing the given keys and values
    (define (collect-alist keys values)
      (do ((res '() (cons (cons (next keys)
                                (next values))
                          res)))
        ((or (not (more? keys))
             (not (more? values)))
         res)))

    ;; collect-plist - not relevant to Scheme
    ;; collect-hash

    ;; Creates filename and writes elements of series into it using writer function
    (define collect-file
      (case-lambda
        ((filename series)
         (collect-file filename series write))
        ((filename series writer)
         (with-output-to-file filename
                              (lambda ()
                                (do ()
                                  ((not (more? series)) #t)
                                  (writer (next series))))))))

    (define (collect-length series)
      (do ((len 0 (+ len 1)))
        ((not (more? series)) len)
        (next series))) ; read next value, but discard it

    (define (collect-sum series)
      (do ((sum 0 (+ sum (next series))))
        ((not (more? series)) sum)))

    ;; Returns the element of series corresponding to the maximum number
    ;; (or the number itself, if no series provided)
    (define collect-max 
      (case-lambda
        ((numbers)
         (let loop ((num #f))
           (if (more? numbers)
             (let ((next-num (next numbers)))
               (if (or (eq? num #f)
                       (> next-num num))
                 (loop next-num)
                 (loop num)))
             num)))
        ((numbers series)
         (collect-max numbers series #f))
        ((numbers series default)
         (if (or (not (more? numbers))
                 (not (more? series)))
           default
           (let loop ((num (next numbers))
                      (obj (next series)))
             (if (or (not (more? numbers))
                     (not (more? series)))
               obj
               (let ((next-num (next numbers))
                     (next-obj (next series)))
                 (if (> next-num num)
                   (loop next-num next-obj)
                   (loop num obj)))))))))

    ;; Returns the element of series corresponding to the minimum number
    ;; (or the number itself, if no series provided)
    (define collect-min
      (case-lambda
        ((numbers)
         (let loop ((num #f))
           (if (more? numbers)
             (let ((next-num (next numbers)))
               (if (or (eq? num #f)
                       (< next-num num))
                 (loop next-num)
                 (loop num)))
             num)))
        ((numbers series)
         (collect-min numbers series #f))
        ((numbers series default)
         (if (or (not (more? numbers))
                 (not (more? series)))
           default
           (let loop ((num (next numbers))
                      (obj (next series)))
             (if (or (not (more? numbers))
                     (not (more? series)))
               obj
               (let ((next-num (next numbers))
                     (next-obj (next series)))
                 (if (< next-num num)
                   (loop next-num next-obj)
                   (loop num obj)))))))))

    ;; Returns the and of elements in series
    (define (collect-and series)
      (do ((res #t (let ((next (next series)))
                     (if next
                       next
                       #f))))
        ((or (not res) (not (more? series))) res)))

    ;; Returns the or of elements in series
    (define (collect-or series)
      (do ((res #f (or (next series) res)))
        ((or res (not (more? series))) res)))

    ;; Higher order function to create collectors
    (define (collect-fn init-proc accum-proc series)
      (do ((res (init-proc) (accum-proc (next series) res)))
        ((not (more? series)) res)))

    ))

