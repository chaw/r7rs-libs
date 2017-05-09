;; Copyright © 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Mark Adler's Adler-32 checksum (used by zlib). Provides the same
;; procedures as (weinholt crypto crc), but Adler-32 is not a CRC.

;; (adler-32 bytevector)
;;     returns the final Adler-32 checksum of the entire bytevector
;; (adler-32-init)
;;     returns an initial Adler-32 state
;; (adler-32-update state bv)
;; (adler-32-update state bv start)
;; (adler-32-update state bv start end)
;;     returns a new state which includes the checksum on the given bytes
;; (adler-32-finish state)
;;     returns the final checksum
;; (adler-32-width)
;;     returns the bit-width of the checksum, i.e. 32

(define-library 
  (weinholt adler-32)
  (export adler-32 adler-32-init adler-32-update
          adler-32-finish adler-32-width)
  (import (scheme base)
          (scheme case-lambda)
          (r6rs fixnums)
          (only (srfi 151) arithmetic-shift bit-field))

  (begin

    ;;>     returns the final Adler-32 checksum of the entire bytevector
    (define (adler-32 bv)
      (adler-32-finish (adler-32-update (adler-32-init) bv)))

    ;;>     returns an initial Adler-32 state
    (define (adler-32-init) 1)

    ;;>     returns a new state which includes the checksum on the given bytes
    (define adler-32-update
      (case-lambda
        ((state bv)
         (adler-32-update state bv 0 (bytevector-length bv)))
        ((state bv start)
         (adler-32-update state bv start (bytevector-length bv)))
        ((state bv start end)
         ;; This is the simple approach. Based on the example in
         ;; RFC1950. TODO: A more clever approach will probably unroll
         ;; the loop and avoid fxmod?
         (let lp ((i start)
                  (s1 (bit-field state 0 16))
                  (s2 (bit-field state 16 32)))
           (if (= i end)
             (+ s1 (arithmetic-shift s2 16))
             (let* ((s1 (fxmod (fx+ s1 (bytevector-u8-ref bv i)) 65521))
                    (s2 (fxmod (fx+ s1 s2) 65521)))
               (lp (+ i 1) s1 s2)))))))

    ;;>     returns the final checksum
    (define (adler-32-finish state) state)

    ;;>     returns the bit-width of the checksum, i.e. 32
    (define (adler-32-width) 32)

    ))


