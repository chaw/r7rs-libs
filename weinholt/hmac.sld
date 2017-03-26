;; Copyright © 2009, 2012 Göran Weinholt <goran@weinholt.se>

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

;; RFC 2104, FIPS-198-1.

(define-library 
  (weinholt hmac)
  (export make-hmac)
  (import (scheme base)
          (r6rs fixnums))

  (begin

    ;; Returns a procedure that calculates the HMAC given a secret and
    ;; data (both of which are bytevectors).
    (define (make-hmac block-length hash ->bytevector make-hash update! finish! clear!)
      (lambda (secret . data)
        (let lp ((secret secret))
          (if (> (bytevector-length secret) block-length)
            (lp (->bytevector (hash secret)))
            (let ((k-ipad (make-bytevector block-length 0))
                  (k-opad (make-bytevector block-length 0)))
              (bytevector-copy! secret 0 k-ipad 0 (bytevector-length secret))
              (bytevector-copy! secret 0 k-opad 0 (bytevector-length secret))
              (do ((i 0 (fx+ i 1)))
                ((fx=? i block-length))
                (bytevector-u8-set! k-ipad i (fxxor #x36 (bytevector-u8-ref k-ipad i)))
                (bytevector-u8-set! k-opad i (fxxor #x5c (bytevector-u8-ref k-opad i))))
              (let ((state (make-hash)))
                (update! state k-ipad)
                (for-each (lambda (d) (update! state d)) data)
                (finish! state)
                (let ((digest (->bytevector state)))
                  (clear! state)
                  (update! state k-opad)
                  (update! state digest)
                  (finish! state)
                  state)))))))

    ))

