;; SRFI 27 Random Bits
;; implementation for Kawa, based around java.util.Random

(define-library 
  (srfi 27)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  (import (scheme base)
          (class java.lang
                 System)
          (class java.math
                 BigInteger)
          (class java.util
                 Random)
          (srfi 60))

  (begin

    ;; -- private method

    (define MAX-INTEGER 2147483647) ; ??? java.lang.Integer:MAX_VALUE)
    (define MAX-LONG 9223372036854775807) ; ??? java.lang.Long:MAX_VALUE)

    ;; Uses BigInteger to permit arbitrarily large n
    (define (random-integer-from-source source n)
      (if (<= n MAX-LONG)
        (invoke source 'nextInt n)
        (let* ((input (BigInteger (number->string n)))
               (bits (invoke input 'bitLength)))
          (do ((r (BigInteger bits source)
                  (BigInteger bits source)))
            ((< r input) r)))))

    ;; -- exported methods

    (define default-random-source (make-random-source))

    (define (random-integer n)
      (random-integer-from-source default-random-source n))

    (define (random-real)
      (invoke default-random-source 'nextFloat))

    (define (make-random-source)
      (Random 0))

    (define (random-source? source)
      (Random? source))

    ;; Use reflection to retrieve value of seed in source
    (define (random-source-state-ref source)
      (let ((field (invoke (invoke source 'getClass) 'getDeclaredField "seed")))
        (invoke field 'setAccessible #t)
        (logxor (invoke (invoke field 'get source) 'longValue)
                #x5DEECE66D)))

    (define (random-source-state-set! source seed)
      (invoke source 'setSeed seed))

    (define (random-source-randomize! source)
      (invoke source 'setSeed (invoke-static System 'currentTimeMillis)))

    ;; constructs a long from i j in a simple but deterministic manner
    (define (random-source-pseudo-randomize! source i j)
      (random-source-state-set! source 
                                (modulo (+ i (* j (+ MAX-INTEGER 1))) 
                                        MAX-LONG)))

    (define (random-source-make-integers source)
      (lambda (n) (random-integer-from-source source n)))

    (define (random-source-make-reals source)
      (lambda () (invoke source 'nextFloat)))

    ))



