;; Copyright © 2009, 2010, 2012 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;; Packaged for R7RS Scheme by Peter Lane, 2017

;; Byte-oriented SHA-224/256 and SHA-384/512 from FIPS 180-3

;; RFC3874 SHA-224

;; TODO: give an error if more than 2^64 / 2^128 bits are processed?
;; TODO: Optimize. Should be simple enough with the help of a profiler.

(define-library 
  (weinholt sha-2)
  (export make-sha-224 sha-224-update! sha-224-finish! sha-224-clear!
          sha-224 sha-224-copy sha-224-finish sha-224-length
          sha-224-copy-hash! sha-224-128-copy-hash!
          sha-224->bytevector sha-224->string
          sha-224-hash=? sha-224-128-hash=?
          hmac-sha-224

          make-sha-256 sha-256-update! sha-256-finish! sha-256-clear!
          sha-256 sha-256-copy sha-256-finish sha-256-length
          sha-256-copy-hash! sha-256-128-copy-hash!
          sha-256->bytevector sha-256->string
          sha-256-hash=? sha-256-128-hash=?
          hmac-sha-256

          make-sha-384 sha-384-update! sha-384-finish! sha-384-clear!
          sha-384 sha-384-copy sha-384-finish sha-384-length
          sha-384-copy-hash! sha-384-128-copy-hash!
          sha-384->bytevector sha-384->string
          sha-384-hash=? sha-384-128-hash=?
          hmac-sha-384

          make-sha-512 sha-512-update! sha-512-finish! sha-512-clear!
          sha-512 sha-512-copy sha-512-finish sha-512-length
          sha-512-copy-hash! sha-512-128-copy-hash!
          sha-512->bytevector sha-512->string
          sha-512-hash=? sha-512-128-hash=?
          hmac-sha-512)
  (import (except (scheme base) bytevector-copy!)
          (scheme case-lambda)
          (r6rs bytevectors)
          (r6rs fixnums)
          (only (srfi 151) arithmetic-shift bitwise-and bitwise-ior bitwise-not bitwise-xor)
          (weinholt hmac))

  (begin

    (define (sha-224-length) 224/8)
    (define (sha-256-length) 256/8)
    (define (sha-384-length) 384/8)
    (define (sha-512-length) 512/8)

    (define (ror32 n count)
      (let ((field1 (bitwise-and #xffffffff (arithmetic-shift n (- 32 count))))
            (field2 (arithmetic-shift n (- count))))
        (bitwise-ior field1 field2)))

    (define (ror64 n count)
      (let ((field1 (bitwise-and #xffffffffffffffff
                                 (arithmetic-shift n (- 64 count))))
            (field2 (arithmetic-shift n (- count))))
        (bitwise-ior field1 field2)))

    (define-record-type <sha-state>
                        (make-sha-state H init W m pending processed)
                        sha-state?
                        (H sha-state-H)               ;Hash
                        (init sha-state-init)         ;initial hash
                        (W sha-state-W)               ;temporary data
                        (m sha-state-m)               ;unprocessed data
                        (pending sha-state-pending sha-state-pending-set!)           ;length of unprocessed data
                        (processed sha-state-processed sha-state-processed-set!))    ;length of processed data

    (define (make-sha-2 initial-hash)
      (let ((W (make-vector 80 #f))
            (m (make-bytevector (* 4 32))))
        (make-sha-state (list->vector initial-hash)
                        initial-hash
                        W m 0 0)))

    (define (make-sha-224) (make-sha-2 initial-hash224))
    (define (make-sha-256) (make-sha-2 initial-hash256))
    (define (make-sha-384) (make-sha-2 initial-hash384))
    (define (make-sha-512) (make-sha-2 initial-hash512))

    (define (sha-2-copy state)
      (let ((H (vector-copy (sha-state-H state)))
            (W (make-vector 80 #f))
            (m (bytevector-copy (sha-state-m state))))
        (make-sha-state H
                        (sha-state-init state)
                        W m
                        (sha-state-pending state)
                        (sha-state-processed state))))

    (define (sha-224-copy x) (sha-2-copy x))
    (define (sha-256-copy x) (sha-2-copy x))
    (define (sha-384-copy x) (sha-2-copy x))
    (define (sha-512-copy x) (sha-2-copy x))

    (define (sha-2-clear! state)
      (do ((init (sha-state-init state) (cdr init))
           (i 0 (+ i 1)))
        ((null? init))
        (vector-set! (sha-state-H state) i (car init)))
      (vector-fill! (sha-state-W state) #f)
      (bytevector-fill! (sha-state-m state) 0)
      (sha-state-pending-set! state 0)
      (sha-state-processed-set! state 0))

    (define (sha-224-clear! state) (sha-2-clear! state))
    (define (sha-256-clear! state) (sha-2-clear! state))
    (define (sha-384-clear! state) (sha-2-clear! state))
    (define (sha-512-clear! state) (sha-2-clear! state))


    (define initial-hash224 '(#xc1059ed8 #x367cd507 #x3070dd17 #xf70e5939
                              #xffc00b31 #x68581511 #x64f98fa7 #xbefa4fa4))

    (define initial-hash256 '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                              #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

    (define initial-hash384 '(#xcbbb9d5dc1059ed8 #x629a292a367cd507
                              #x9159015a3070dd17 #x152fecd8f70e5939
                              #x67332667ffc00b31 #x8eb44a8768581511
                              #xdb0c2e0d64f98fa7 #x47b5481dbefa4fa4))

    (define initial-hash512 '(#x6a09e667f3bcc908 #xbb67ae8584caa73b
                              #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
                              #x510e527fade682d1 #x9b05688c2b3e6c1f
                              #x1f83d9abfb41bd6b #x5be0cd19137e2179))


    (define (Ch x y z)
      (bitwise-xor (bitwise-and x y)
                   (bitwise-and (bitwise-not x) z)))

    (define Parity bitwise-xor)

    (define (Maj x y z)
      (bitwise-xor (bitwise-and x y)
                   (bitwise-and x z)
                   (bitwise-and y z)))



    (define (Sigma0-256 x)
      (bitwise-xor (ror32 x 2)
                   (ror32 x 13)
                   (ror32 x 22)))

    (define (Sigma1-256 x)
      (bitwise-xor (ror32 x 6)
                   (ror32 x 11)
                   (ror32 x 25)))

    (define (sigma0-256 x)
      (bitwise-xor (ror32 x 7)
                   (ror32 x 18)
                   (arithmetic-shift x -3)))

    (define (sigma1-256 x)
      (bitwise-xor (ror32 x 17)
                   (ror32 x 19)
                   (arithmetic-shift x -10)))


    (define (Sigma0-512 x)
      (bitwise-xor (ror64 x 28)
                   (ror64 x 34)
                   (ror64 x 39)))

    (define (Sigma1-512 x)
      (bitwise-xor (ror64 x 14)
                   (ror64 x 18)
                   (ror64 x 41)))

    (define (sigma0-512 x)
      (bitwise-xor (ror64 x 1)
                   (ror64 x 8)
                   (arithmetic-shift x -7)))

    (define (sigma1-512 x)
      (bitwise-xor (ror64 x 19)
                   (ror64 x 61)
                   (arithmetic-shift x -6)))

    (define k-256
      '#(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
         #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
         #xd807aa98 #x12835b01 #x243185be #x550c7dc3
         #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
         #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
         #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
         #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
         #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
         #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
         #x650a7354 #x766a0abb #x81c2c92e #x92722c85
         #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
         #xd192e819 #xd6990624 #xf40e3585 #x106aa070
         #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
         #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
         #x748f82ee #x78a5636f #x84c87814 #x8cc70208
         #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

    (define k-512
      '#(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
         #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
         #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
         #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
         #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
         #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
         #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
         #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
         #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
         #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
         #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
         #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
         #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
         #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
         #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
         #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
         #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
         #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
         #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
         #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817))

    ;; This function transforms a whole 512 bit block.
    (define (sha-256-transform! H* W m offset)
      ;; Copy the message block
      (do ((t 0 (+ t 1)))
        ((= t 16))
        (vector-set! W t (bytevector-u32-ref m (+ (* t 4) offset) (endianness big))))
      ;; Initialize W[16..63]
      (do ((t 16 (+ t 1)))
        ((= t 64))
        (vector-set! W t (bitwise-and (+ (sigma1-256 (vector-ref W (- t 2)))
                                         (vector-ref W (- t 7))
                                         (sigma0-256 (vector-ref W (- t 15)))
                                         (vector-ref W (- t 16)))
                                      #xffffffff)))
      ;; Do the hokey pokey
      (let lp ((A (vector-ref H* 0))
               (B (vector-ref H* 1))
               (C (vector-ref H* 2))
               (D (vector-ref H* 3))
               (E (vector-ref H* 4))
               (F (vector-ref H* 5))
               (G (vector-ref H* 6))
               (H (vector-ref H* 7))
               (t 0))
        (cond ((= t 64)
               (vector-set! H* 0 (bitwise-and #xffffffff (+ A (vector-ref H* 0))))
               (vector-set! H* 1 (bitwise-and #xffffffff (+ B (vector-ref H* 1))))
               (vector-set! H* 2 (bitwise-and #xffffffff (+ C (vector-ref H* 2))))
               (vector-set! H* 3 (bitwise-and #xffffffff (+ D (vector-ref H* 3))))
               (vector-set! H* 4 (bitwise-and #xffffffff (+ E (vector-ref H* 4))))
               (vector-set! H* 5 (bitwise-and #xffffffff (+ F (vector-ref H* 5))))
               (vector-set! H* 6 (bitwise-and #xffffffff (+ G (vector-ref H* 6))))
               (vector-set! H* 7 (bitwise-and #xffffffff (+ H (vector-ref H* 7)))))
              (else
                (let ((T1 (+ H (Sigma1-256 E) (Ch E F G)
                             (vector-ref k-256 t) (vector-ref W t)))
                      (T2 (+ (Sigma0-256 A) (Maj A B C))))
                  (lp (bitwise-and #xffffffff (+ T1 T2))
                      A B C
                      (bitwise-and #xffffffff (+ D T1))
                      E F G
                      (+ t 1)))))))

    ;; This function transforms a whole 1024 bit block.
    (define (sha-512-transform! H* W m offset)
      ;; Copy the message block
      (do ((t 0 (+ t 1)))
        ((= t 16))
        (vector-set! W t (bytevector-u64-ref m (+ (* t 8) offset) (endianness big))))
      ;; Initialize W[16..63]
      (do ((t 16 (+ t 1)))
        ((= t 80))
        (vector-set! W t (bitwise-and (+ (sigma1-512 (vector-ref W (- t 2)))
                                         (vector-ref W (- t 7))
                                         (sigma0-512 (vector-ref W (- t 15)))
                                         (vector-ref W (- t 16)))
                                      #xffffffffffffffff)))
      ;; Do the hokey pokey
      (let lp ((A (vector-ref H* 0))
               (B (vector-ref H* 1))
               (C (vector-ref H* 2))
               (D (vector-ref H* 3))
               (E (vector-ref H* 4))
               (F (vector-ref H* 5))
               (G (vector-ref H* 6))
               (H (vector-ref H* 7))
               (t 0))
        (cond ((= t 80)
               (vector-set! H* 0 (bitwise-and #xffffffffffffffff (+ A (vector-ref H* 0))))
               (vector-set! H* 1 (bitwise-and #xffffffffffffffff (+ B (vector-ref H* 1))))
               (vector-set! H* 2 (bitwise-and #xffffffffffffffff (+ C (vector-ref H* 2))))
               (vector-set! H* 3 (bitwise-and #xffffffffffffffff (+ D (vector-ref H* 3))))
               (vector-set! H* 4 (bitwise-and #xffffffffffffffff (+ E (vector-ref H* 4))))
               (vector-set! H* 5 (bitwise-and #xffffffffffffffff (+ F (vector-ref H* 5))))
               (vector-set! H* 6 (bitwise-and #xffffffffffffffff (+ G (vector-ref H* 6))))
               (vector-set! H* 7 (bitwise-and #xffffffffffffffff (+ H (vector-ref H* 7)))))
              (else
                (let ((T1 (+ H (Sigma1-512 E) (Ch E F G)
                             (vector-ref k-512 t) (vector-ref W t)))
                      (T2 (+ (Sigma0-512 A) (Maj A B C))))
                  (lp (bitwise-and #xffffffffffffffff (+ T1 T2))
                      A B C
                      (bitwise-and #xffffffffffffffff (+ D T1))
                      E F G
                      (+ t 1)))))))

    (define (sha-224-update! . x) (apply sha-256-update! x))

    ;; Add a bytevector to the state. Align your data to whole blocks if
    ;; you want this to go a little faster.
    (define sha-256-update!
      (case-lambda
        ((state data start end)
         (let ((m (sha-state-m state))    ;unprocessed data
               (H (sha-state-H state))
               (W (sha-state-W state)))
           (let lp ((offset start))
             (cond ((= (sha-state-pending state) 64)
                    ;; A whole block is pending
                    (sha-256-transform! H W m 0)
                    (sha-state-pending-set! state 0)
                    (sha-state-processed-set! state (+ 64 (sha-state-processed state)))
                    (lp offset))
                   ((= offset end)
                    (values))
                   ((or (> (sha-state-pending state) 0)
                        (> (+ offset 64) end))
                    ;; Pending data exists or less than a block remains.
                    ;; Add more pending data.
                    (let ((added (min (- 64 (sha-state-pending state))
                                      (- end offset))))
                      (bytevector-copy! data offset
                                        m (sha-state-pending state)
                                        added)
                      (sha-state-pending-set! state (+ added (sha-state-pending state)))
                      (lp (+ offset added))))
                   (else
                     ;; Consume a whole block
                     (sha-256-transform! H W data offset)
                     (sha-state-processed-set! state (+ 64 (sha-state-processed state)))
                     (lp (+ offset 64)))))))
        ((state data)
         (sha-256-update! state data 0 (bytevector-length data)))))

    (define (sha-384-update! . x) (apply sha-512-update! x))

    (define sha-512-update!
      (case-lambda
        ((state data start end)
         (let ((m (sha-state-m state))    ;unprocessed data
               (H (sha-state-H state))
               (W (sha-state-W state)))
           (let lp ((offset start))
             (cond ((= (sha-state-pending state) 128)
                    ;; A whole block is pending
                    (sha-512-transform! H W m 0)
                    (sha-state-pending-set! state 0)
                    (sha-state-processed-set! state (+ 128 (sha-state-processed state)))
                    (lp offset))
                   ((= offset end)
                    (values))
                   ((or (> (sha-state-pending state) 0)
                        (> (+ offset 128) end))
                    ;; Pending data exists or less than a block remains.
                    ;; Add more pending data.
                    (let ((added (min (- 128 (sha-state-pending state))
                                      (- end offset))))
                      (bytevector-copy! data offset
                                        m (sha-state-pending state)
                                        added)
                      (sha-state-pending-set! state (+ added (sha-state-pending state)))
                      (lp (+ offset added))))
                   (else
                     ;; Consume a whole block
                     (sha-512-transform! H W data offset)
                     (sha-state-processed-set! state (+ 128 (sha-state-processed state)))
                     (lp (+ offset 128)))))))
        ((state data)
         (sha-512-update! state data 0 (bytevector-length data)))))


    (define zero-block (make-bytevector 128 0))

    (define (sha-224-finish! state) (sha-256-finish! state))

    ;; Finish the state by adding a 1, zeros and the counter.
    (define (sha-256-finish! state)
      (let ((m (sha-state-m state))
            (pending (+ (sha-state-pending state) 1)))
        (bytevector-u8-set! m (sha-state-pending state) #x80)
        (cond ((> pending 56)
               (bytevector-copy! zero-block 0
                                 m pending
                                 (- 64 pending))
               (sha-256-transform! (sha-state-H state)
                                   (sha-state-W state)
                                   m
                                   0)
               (bytevector-fill! m 0))
              (else
                (bytevector-copy! zero-block 0
                                  m pending
                                  (- 64 pending))))
        ;; Number of bits in the data
        (bytevector-u64-set! m 56
                             (* (+ (sha-state-processed state)
                                   (- pending 1))
                                8)
                             (endianness big))
        (sha-256-transform! (sha-state-H state)
                            (sha-state-W state)
                            m
                            0)))

    (define (sha-384-finish! state) (sha-512-finish! state))

    (define (sha-512-finish! state)
      (let ((m (sha-state-m state))
            (pending (+ (sha-state-pending state) 1)))
        (bytevector-u8-set! m (sha-state-pending state) #x80)
        (cond ((> pending 112)
               (bytevector-copy! zero-block 0
                                 m pending
                                 (- 128 pending))
               (sha-512-transform! (sha-state-H state)
                                   (sha-state-W state)
                                   m
                                   0)
               (bytevector-fill! m 0))
              (else
                (bytevector-copy! zero-block 0
                                  m pending
                                  (- 128 pending))))
        ;; Number of bits in the data
        (bytevector-uint-set! m 112
                              (* (+ (sha-state-processed state)
                                    (- pending 1))
                                 8)
                              (endianness big)
                              16)
        (sha-512-transform! (sha-state-H state)
                            (sha-state-W state)
                            m
                            0)))

    (define (sha-2-finish copy finish!)
      (lambda (state)
        (let ((copy (copy state)))
          (finish! copy)
          copy)))

    (define sha-224-finish (sha-2-finish sha-224-copy sha-224-finish!))
    (define sha-256-finish (sha-2-finish sha-256-copy sha-256-finish!))
    (define sha-384-finish (sha-2-finish sha-384-copy sha-384-finish!))
    (define sha-512-finish (sha-2-finish sha-512-copy sha-512-finish!))

    ;; Find the message digest of the concatenation of the given bytevectors.
    (define (sha-2 make update! finish!)
      (lambda data
        (let ((state (make)))
          (for-each (lambda (d) (update! state d))
                    data)
          (finish! state)
          state)))

    (define sha-224 (sha-2 make-sha-224 sha-224-update! sha-224-finish!))
    (define sha-256 (sha-2 make-sha-256 sha-256-update! sha-256-finish!))
    (define sha-384 (sha-2 make-sha-384 sha-384-update! sha-384-finish!))
    (define sha-512 (sha-2 make-sha-512 sha-512-update! sha-512-finish!))

    (define (sha-2/32-copy-hash! len)
      (lambda (state bv off)
        (do ((i 0 (+ i 1)))
          ((= i len))
          (bytevector-u32-set! bv (+ off (* 4 i))
                               (vector-ref (sha-state-H state) i)
                               (endianness big)))))

    (define sha-224-copy-hash! (sha-2/32-copy-hash! 224/32))
    (define sha-256-copy-hash! (sha-2/32-copy-hash! 256/32))
    (define sha-224-128-copy-hash! (sha-2/32-copy-hash! 128/32))
    (define sha-256-128-copy-hash! (sha-2/32-copy-hash! 128/32))

    (define (sha-2/64-copy-hash! len)
      (lambda (state bv off)
        (do ((i 0 (+ i 1)))
          ((= i len))
          (bytevector-u64-set! bv (+ off (* 8 i))
                               (vector-ref (sha-state-H state) i)
                               (endianness big)))))

    (define sha-384-copy-hash! (sha-2/64-copy-hash! 384/64))
    (define sha-512-copy-hash! (sha-2/64-copy-hash! 512/64))
    (define sha-384-128-copy-hash! (sha-2/64-copy-hash! 128/64))
    (define sha-512-128-copy-hash! (sha-2/64-copy-hash! 128/64))

    (define (sha-2->bytevector copy! len)
      (lambda (state)
        (let ((ret (make-bytevector (* 4 len))))
          (copy! state ret 0)
          ret)))

    (define sha-224->bytevector (sha-2->bytevector sha-224-copy-hash! 224/32))
    (define sha-256->bytevector (sha-2->bytevector sha-256-copy-hash! 256/32))
    (define sha-384->bytevector (sha-2->bytevector sha-384-copy-hash! 384/32))
    (define sha-512->bytevector (sha-2->bytevector sha-512-copy-hash! 512/32))

    (define (sha-2->string ->bytevector)
      (lambda (state)
        (apply string-append
               (map (lambda (x)
                      (if (< x #x10)
                        (string-append "0" (number->string x 16))
                        (number->string x 16)))
                    (bytevector->u8-list (->bytevector state))))))

    (define sha-224->string (sha-2->string sha-224->bytevector))
    (define sha-256->string (sha-2->string sha-256->bytevector))
    (define sha-384->string (sha-2->string sha-384->bytevector))
    (define sha-512->string (sha-2->string sha-512->bytevector))

    (define (cmp/32 state bv len)
      (do ((i 0 (fx+ i 1))
           (diff 0 (+ diff
                      (bitwise-xor
                        (bytevector-u32-ref bv (* 4 i) (endianness big))
                        (vector-ref (sha-state-H state) i)))))
        ((fx=? i len)
         (zero? diff))))

    (define (sha-224-hash=? state bv) (cmp/32 state bv 224/32))
    (define (sha-256-hash=? state bv) (cmp/32 state bv 256/32))
    (define (sha-384-hash=? state bv) (cmp/64 state bv 384/64))
    (define (sha-512-hash=? state bv) (cmp/64 state bv 512/64))

    (define (cmp/64 state bv len)
      (do ((i 0 (fx+ i 1))
           (diff 0 (+ diff
                      (bitwise-xor
                        (bytevector-u64-ref bv (* 8 i) (endianness big))
                        (vector-ref (sha-state-H state) i)))))
        ((fx=? i len)
         (zero? diff))))

    (define (sha-224-128-hash=? state bv) (cmp/32 state bv 128/32))
    (define (sha-256-128-hash=? state bv) (cmp/32 state bv 128/32))
    (define (sha-384-128-hash=? state bv) (cmp/64 state bv 128/64))
    (define (sha-512-128-hash=? state bv) (cmp/64 state bv 128/64))

    (define hmac-sha-224
      (make-hmac 64 sha-224 sha-224->bytevector make-sha-224 sha-224-update! sha-224-finish! sha-224-clear!))

    (define hmac-sha-256
      (make-hmac 64 sha-256 sha-256->bytevector make-sha-256 sha-256-update! sha-256-finish! sha-256-clear!))

    (define hmac-sha-384
      (make-hmac 128 sha-384 sha-384->bytevector make-sha-384 sha-384-update! sha-384-finish! sha-384-clear!))

    (define hmac-sha-512
      (make-hmac 128 sha-512 sha-512->bytevector make-sha-512 sha-512-update! sha-512-finish! sha-512-clear!))

    ))

