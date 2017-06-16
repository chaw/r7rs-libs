(define-library 
  (srfi 151) 
  (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
          bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
          bitwise-orc1 bitwise-orc2
          arithmetic-shift bit-count integer-length bitwise-if 
          bit-set? copy-bit bit-swap any-bit-set? every-bit-set?  first-set-bit
          bit-field bit-field-any? bit-field-every?  bit-field-clear bit-field-set
          bit-field-replace  bit-field-replace-same
          bit-field-rotate bit-field-reverse
          bits->list list->bits bits->vector vector->bits bits
          bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)
  (import (scheme base)
          (scheme case-lambda))

  (cond-expand
    (chibi
      (include-shared "bit")
      (begin
        (define (bitwise-not i) (- -1 i))

        (define (make-nary proc2 default)
          (lambda args
            (if (null? args)
              default
              (let lp ((i (car args)) (ls (cdr args)))
                (if (null? ls)
                  i
                  (lp (proc2 i (car ls)) (cdr ls)))))))

        (define bitwise-and  (make-nary bit-and  -1))
        (define bitwise-ior  (make-nary bit-ior   0))
        (define bitwise-xor  (make-nary bit-xor   0))))

    ((library (rnrs arithmetic bitwise))
     (import (only (rnrs arithmetic bitwise)
                   bitwise-not bitwise-and bitwise-ior bitwise-xor
                   bitwise-bit-count bitwise-length))
     (import (rename (only (rnrs arithmetic bitwise)
                           bitwise-arithmetic-shift bitwise-length)
                     (bitwise-arithmetic-shift arithmetic-shift)
                     (bitwise-length integer-length)))
     (begin 
       (define (bit-count i) ; Note, -ve case different to R6RS bitwise-bit-count
         (if (>= i 0) 
           (bitwise-bit-count i)
           (bitwise-bit-count (bitwise-not i))))))

    (gauche
      (import (only (gauche base)
                    integer-length))
      (import (rename (only (gauche base)
                            lognot logand logcount logior logxor ash)
                      (lognot bitwise-not)
                      (logcount bit-count)
                      (logand bitwise-and)
                      (logior bitwise-ior)
                      (logxor bitwise-xor)
                      (ash arithmetic-shift))))

    (else 
      (begin
        ;;;; bitwise-core, core bitwise operations
        ;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
        ;;; This implementation of the seven core functions required by SRFI 33
        ;;; (bitwise-not, bitwise-and, bitwise-ior, bitwise-xor, arithmetic-shift,
        ;;; bit-count, integer-length) is drawn from the SRFI 60 implementation.
        ;;; Here is Shivers's comment on this code in SRFI 33:

        ;;; The [following] implementations of these functions use [only] R4RS
        ;;; arithmetic, so a simple-minded implementation again doesn't need to
        ;;; do much to support them -- however, [these] general implementations
        ;;; are terribly inefficient relative to native support and should *not*
        ;;; be used except in case of dire emergency. (It's quite clever code,
        ;;; nonetheless, to provide the semantics with such little support.)

        ;Permission to copy this software, to modify it, to redistribute it,
        ;to distribute modified versions, and to use it for any purpose is
        ;granted, subject to the following restrictions and understandings.
        ;
        ;1.  Any copy made of this software must include this copyright notice
        ;in full.
        ;
        ;2.  I have made no warranty or representation that the operation of
        ;this software will be error-free, and I am under no obligation to
        ;provide any services, by way of maintenance, update, or otherwise.
        ;
        ;3.  In conjunction with products arising from the use of this
        ;material, there shall be no use of my name in any advertising,
        ;promotional, or sales literature without prior written consent in
        ;each case.

        (define (bitwise-not n) (- -1 n))

        (define logical:boole-xor
          '#(#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
             #(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14)
             #(2 3 0 1 6 7 4 5 10 11 8 9 14 15 12 13)
             #(3 2 1 0 7 6 5 4 11 10 9 8 15 14 13 12)
             #(4 5 6 7 0 1 2 3 12 13 14 15 8 9 10 11)
             #(5 4 7 6 1 0 3 2 13 12 15 14 9 8 11 10)
             #(6 7 4 5 2 3 0 1 14 15 12 13 10 11 8 9)
             #(7 6 5 4 3 2 1 0 15 14 13 12 11 10 9 8)
             #(8 9 10 11 12 13 14 15 0 1 2 3 4 5 6 7)
             #(9 8 11 10 13 12 15 14 1 0 3 2 5 4 7 6)
             #(10 11 8 9 14 15 12 13 2 3 0 1 6 7 4 5)
             #(11 10 9 8 15 14 13 12 3 2 1 0 7 6 5 4)
             #(12 13 14 15 8 9 10 11 4 5 6 7 0 1 2 3)
             #(13 12 15 14 9 8 11 10 5 4 7 6 1 0 3 2)
             #(14 15 12 13 10 11 8 9 6 7 4 5 2 3 0 1)
             #(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)))

        (define logical:boole-and
          '#(#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
             #(0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1)
             #(0 0 2 2 0 0 2 2 0 0 2 2 0 0 2 2)
             #(0 1 2 3 0 1 2 3 0 1 2 3 0 1 2 3)
             #(0 0 0 0 4 4 4 4 0 0 0 0 4 4 4 4)
             #(0 1 0 1 4 5 4 5 0 1 0 1 4 5 4 5)
             #(0 0 2 2 4 4 6 6 0 0 2 2 4 4 6 6)
             #(0 1 2 3 4 5 6 7 0 1 2 3 4 5 6 7)
             #(0 0 0 0 0 0 0 0 8 8 8 8 8 8 8 8)
             #(0 1 0 1 0 1 0 1 8 9 8 9 8 9 8 9)
             #(0 0 2 2 0 0 2 2 8 8 10 10 8 8 10 10)
             #(0 1 2 3 0 1 2 3 8 9 10 11 8 9 10 11)
             #(0 0 0 0 4 4 4 4 8 8 8 8 12 12 12 12)
             #(0 1 0 1 4 5 4 5 8 9 8 9 12 13 12 13)
             #(0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14)
             #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

        (define (logical:ash-4 x)
          (if (negative? x)
            (+ -1 (quotient (+ 1 x) 16))
            (quotient x 16)))

        (define (logical:reduce op4 ident)
          (lambda args
            (do ((res ident (op4 res (car rgs) 1 0))
                 (rgs args (cdr rgs)))
              ((null? rgs) res))))


        (define bitwise-and
          (letrec
            ((lgand
               (lambda (n2 n1 scl acc)
                 (cond ((= n1 n2) (+ acc (* scl n1)))
                       ((zero? n2) acc)
                       ((zero? n1) acc)
                       (else (lgand (logical:ash-4 n2)
                                    (logical:ash-4 n1)
                                    (* 16 scl)
                                    (+ (* (vector-ref (vector-ref logical:boole-and
                                                                  (modulo n1 16))
                                                      (modulo n2 16))
                                          scl)
                                       acc)))))))
            (logical:reduce lgand -1)))

        (define bitwise-ior
          (letrec
            ((lgior
               (lambda (n2 n1 scl acc)
                 (cond ((= n1 n2) (+ acc (* scl n1)))
                       ((zero? n2) (+ acc (* scl n1)))
                       ((zero? n1) (+ acc (* scl n2)))
                       (else (lgior (logical:ash-4 n2)
                                    (logical:ash-4 n1)
                                    (* 16 scl)
                                    (+ (* (- 15 (vector-ref
                                                  (vector-ref logical:boole-and
                                                              (- 15 (modulo n1 16)))
                                                  (- 15 (modulo n2 16))))
                                          scl)
                                       acc)))))))
            (logical:reduce lgior 0)))

        (define bitwise-xor
          (letrec
            ((lgxor
               (lambda (n2 n1 scl acc)
                 (cond ((= n1 n2) acc)
                       ((zero? n2) (+ acc (* scl n1)))
                       ((zero? n1) (+ acc (* scl n2)))
                       (else (lgxor (logical:ash-4 n2)
                                    (logical:ash-4 n1)
                                    (* 16 scl)
                                    (+ (* (vector-ref (vector-ref logical:boole-xor
                                                                  (modulo n1 16))
                                                      (modulo n2 16))
                                          scl)
                                       acc)))))))
            (logical:reduce lgxor 0)))

        (define (arithmetic-shift n count)
          (if (negative? count)
            (let ((k (expt 2 (- count))))
              (if (negative? n)
                (+ -1 (quotient (+ 1 n) k))
                (quotient n k)))
            (* (expt 2 count) n)))
        ;@
        (define integer-length
          (letrec ((intlen (lambda (n tot)
                             (case n
                               ((0 -1) (+ 0 tot))
                               ((1 -2) (+ 1 tot))
                               ((2 3 -3 -4) (+ 2 tot))
                               ((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
                               (else (intlen (logical:ash-4 n) (+ 4 tot)))))))
            (lambda (n) (intlen n 0))))

        (define bit-count
          (letrec ((logcnt (lambda (n tot)
                             (if (zero? n)
                               tot
                               (logcnt (quotient n 16)
                                       (+ (vector-ref
                                            '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                            (modulo n 16))
                                          tot))))))
            (lambda (n)
              (cond ((negative? n) (logcnt (bitwise-not n) 0))
                    ((positive? n) (logcnt n 0))
                    (else 0))))))))

  (begin

    ;;;; bitwise-33 - Olin Shivers's code from SRFI-33 with modified names

    ;;; Olin Shivers is the sole author of this code, and he has placed it in
    ;;; the public domain.
    ;;; 
    ;;; A good implementation might choose to provide direct compiler/interpreter
    ;;; support for these derived functions, or might simply define them to be
    ;;; integrable -- i.e., inline-expanded.
    ;;; 
    ;;; The seven non-trivial boolean functions in terms
    ;;; of not, and, or & xor.

    (define (bitwise-nand  i j)  (bitwise-not (bitwise-and i j)))
    (define (bitwise-nor   i j)  (bitwise-not (bitwise-ior i j)))
    (define (bitwise-andc1 i j)  (bitwise-and (bitwise-not i) j))	
    (define (bitwise-andc2 i j)  (bitwise-and i (bitwise-not j)))	
    (define (bitwise-orc1  i j)  (bitwise-ior (bitwise-not i) j))
    (define (bitwise-orc2  i j)  (bitwise-ior i (bitwise-not j)))

    ;;; This is a general definition, but less than efficient.  It should also
    ;;; receive primitive compiler/interpreter support so that the expensive
    ;;; n-ary mechanism is not invoked in the standard cases -- that is,
    ;;; an application of BITWISE-EQV should be rewritten into an equivalent
    ;;; tree applying some two-argument primitive to the arguments, in the
    ;;; same manner that statically-known n-ary applications of associative
    ;;; operations such as + and * are handled efficiently:
    ;;;   (bitwise-eqv)         => -1
    ;;;   (bitwise-eqv i)       => i
    ;;;   (bitwise-eqv i j)     => (%bitwise-eqv i j)
    ;;;   (bitwise-eqv i j k)   => (%bitwise-eqv (%bitwise-eqv i j) k)
    ;;;   (bitwise-eqv i j k l) => (%bitwise-eqv (%bitwise-eqv (%bitwise-eqv i j) k) l)

    (define (bitwise-eqv . args)
      (let lp ((args args) (ans -1))
        (if (pair? args)
          (lp (cdr args) (bitwise-not (bitwise-xor ans (car args))))
          ans)))

    ;;; Helper function -- make a mask of SIZE 1-bits, e.g. (%MASK 3) = #b111.
    ;;; Suppose your Scheme's fixnums are N bits wide (counting the sign bit,
    ;;; not counting any tag bits). This version, due to Marc Feeley, will 
    ;;; handle SIZE in the range [0,N-1] without overflowing to bignums. 
    ;;; (For SIZE >= N, the correct bignum value is also produced.)

    (define (mask start end) (bitwise-not (arithmetic-shift -1 (- end start))))

    ;;; This alternate, mathematically-equivalent expression
    ;;;     (- (arithmetic-shift 1 size) 1)
    ;;; is not as good -- it only handles SIZE in the range [0,N-2] without
    ;;; overflowing to bignums.
    ;;;
    ;;; Finally, note that even Feeley's expression can't build an N-bit mask
    ;;; without bignum help. This is fundamental, since the interpretation
    ;;; of fixed-size fixnum bit patterns as semi-infinite-bit-strings is that
    ;;; you replicate the high bit out to infinity. So you have to have a
    ;;; zero "stop bit" appearing after that highest one bit to turn off the
    ;;; replication of the ones.

    (define (bit-set? index n) 
      (not (zero? (bitwise-and (arithmetic-shift 1 index) n))))

    (define (any-bit-set? test-bits n) (not (zero? (bitwise-and test-bits n))))

    (define (every-bit-set? test-bits n) (= test-bits (bitwise-and test-bits n)))

    ;;; Bit-field ops

    (define (bit-field n start end)
      (bitwise-and (mask start end) (arithmetic-shift n (- start))))

    (define (bit-field-any? n start end)
      (not (zero? (bitwise-and (arithmetic-shift n (- start)) (mask start end)))))

    ;; Part of Olin's late revisions; code by John Cowan; public domain.
    (define (bit-field-every? n start end)
      (let ((m (mask start end)))
        (eqv? m (bitwise-and (arithmetic-shift n (- start)) m))))

    ;; Integrating i-b-f reduces nicely.
    (define (bit-field-clear n start end)
      (bit-field-replace n 0 start end))

    ;; Counterpart to above, not in SRFI 33, written by John Cowan, public domain
    (define (bit-field-set n start end)
      (bit-field-replace n -1 start end))

    ;;; Oops -- intermediate ARITHMETIC-SHIFT can fixnum-overflow on fixnum args.
    ;(define (bit-field-replace newfield n start end)
    ;  (bit-field-replace-same (arithmetic-shift newfield start) n start end))

    ;;; This three-line version won't fixnum-overflow on fixnum args.
    (define (bit-field-replace n newfield start end)
      (let ((m (mask start end)))
        (bitwise-ior (bitwise-and n (bitwise-not (arithmetic-shift m start)))
                     (arithmetic-shift (bitwise-and newfield m) start))))

    (define (bit-field-replace-same to from start end)
      (bitwise-if (arithmetic-shift (mask start end) start) from to))

    ;; Simple definition
    ;(define (first-set-bit i)
    ;  (and (not (zero? i))
    ;       (let lp ((j 0) (i start))
    ;         (if (bit-set? i 0) j
    ;             (lp (+ j 1) (arithmetic-shift i 1))))))

    ;;; Clever definition, assuming you have a fast BIT-COUNT.
    (define (first-set-bit i) (- (bit-count (bitwise-xor i (- i 1))) 1))

    ;;;; bitwise-60 - SRFI-60 procedures without SRFI-33 analogues, renamed
    ;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer
    ;
    ;Permission to copy this software, to modify it, to redistribute it,
    ;to distribute modified versions, and to use it for any purpose is
    ;granted, subject to the following restrictions and understandings.
    ;
    ;1.  Any copy made of this software must include this copyright notice
    ;in full.
    ;
    ;2.  I have made no warranty or representation that the operation of
    ;this software will be error-free, and I am under no obligation to
    ;provide any services, by way of maintenance, update, or otherwise.
    ;
    ;3.  In conjunction with products arising from the use of this
    ;material, there shall be no use of my name in any advertising,
    ;promotional, or sales literature without prior written consent in
    ;each case.

    (define (bit-field-rotate n count start end)
      (define width (- end start))
      (set! count (modulo count width))
      (let ((mask (bitwise-not (arithmetic-shift -1 width))))
        (define zn (bitwise-and mask (arithmetic-shift n (- start))))
        (bitwise-ior (arithmetic-shift
                       (bitwise-ior (bitwise-and mask (arithmetic-shift zn count))
                                    (arithmetic-shift zn (- count width)))
                       start)
                     (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))

    (define (bit-reverse k n)
      (do ((m (if (negative? n) (bitwise-not n) n) (arithmetic-shift m -1))
           (k (+ -1 k) (+ -1 k))
           (rvs 0 (bitwise-ior (arithmetic-shift rvs 1) (bitwise-and 1 m))))
        ((negative? k) (if (negative? n) (bitwise-not rvs) rvs))))


    (define (bit-field-reverse n start end)
      (define width (- end start))
      (let ((mask (bitwise-not (arithmetic-shift -1 width))))
        (define zn (bitwise-and mask (arithmetic-shift n (- start))))
        (bitwise-ior (arithmetic-shift (bit-reverse width zn) start)
                     (bitwise-and (bitwise-not (arithmetic-shift mask start)) n))))

    (define (copy-bit index to bool)
      (if bool
        (bitwise-ior to (arithmetic-shift 1 index))
        (bitwise-and to (bitwise-not (arithmetic-shift 1 index)))))

    (define (bits->list k . len)
      (if (null? len)
        (do ((k k (arithmetic-shift k -1))
             (lst '() (cons (odd? k) lst)))
          ((<= k 0) (reverse lst)))
        (do ((idx (+ -1 (car len)) (+ -1 idx))
             (k k (arithmetic-shift k -1))
             (lst '() (cons (odd? k) lst)))
          ((negative? idx) (reverse lst)))))

    (define (list->bits bools)
      (do ((bs (reverse bools) (cdr bs))
           (acc 0 (+ acc acc (if (car bs) 1 0))))
        ((null? bs) acc)))

    (define (bits . bools)
      (list->bits bools))

    (define (bitwise-if mask n0 n1)
      (bitwise-ior (bitwise-and mask n0)
                   (bitwise-and (bitwise-not mask) n1)))

    ;;;; bitwise-other - functions not from SRFI 33 or SRFI 60
    ;;; Copyright John Cowan 2017

    (define bits->vector
      (case-lambda
        ((i) (list->vector (bits->list i)))
        ((i len) (list->vector (bits->list i len)))))

    (define (vector->bits vector) (list->bits (vector->list vector)))

    (define (bit-swap n1 n2 i)
      (let ((n1-bit (bit-set? n1 i))
            (n2-bit (bit-set? n2 i)))
        (copy-bit n2 (copy-bit n1 i n2-bit) n1-bit)))

    (define (bitwise-fold proc seed i)
      (let ((len (integer-length i)))
        (let loop ((n 0) (r seed))
          (if (= n len)
            r
            (loop (+ n 1) (proc (bit-set? n i) r))))))

    (define (bitwise-for-each proc i)
      (let ((len (integer-length i)))
        (let loop ((n 0))
          (when (< n len)
            (proc (bit-set? n i))
            (loop (+ n 1))))))

    (define (bitwise-unfold stop? mapper successor seed)
      (let loop ((n 0) (result 0) (state seed))
        (if (stop? state)
          result
          (loop (+ n 1)
                (copy-bit n result (mapper state))
                (successor state)))))

    (define (make-bitwise-generator i)
      (lambda ()
        (let ((bit (bit-set? 0 i)))
          (set! i (arithmetic-shift i -1))
          bit)))

    ))

