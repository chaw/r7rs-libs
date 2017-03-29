;; Copyright © 2009, 2010, 2011 Göran Weinholt <goran@weinholt.se>

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

(import (weinholt des)
        (srfi 64)
        (scheme base)
        (scheme write))

(test-begin "weinholt-des")

(define (print . x) (for-each display x) (newline))

(define (rivest)
  ;; TESTING IMPLEMENTATIONS OF DES, Ronald L. Rivest
  ;; http://people.csail.mit.edu/rivest/Destest.txt
  (do ((x (bytevector-copy '#u8(#x94 #x74 #xB8 #xE8 #xC7 #x3B #xCA #x7D)))
       (i 0 (+ i 1)))
    ((= i 16) x)
    (print i ": " x)
    (des! x (if (even? i)
              (permute-key x)            ;encipher
              (reverse (permute-key x)))))) ;decipher

(test-equal (rivest)  '#u8(#x1B #x1A #x2D #xDB #x4C #x64 #x24 #x38))


(define (test-tdea plaintext k1 k2 k3)
  ;; Returns the ciphertext and the deciphered ciphertext, which
  ;; should match the plaintext.
  (let ((bv (bytevector-copy plaintext))
        (key (tdea-permute-key k1 k2 k3)))
    (tdea-encipher! bv 0 key)
    (let ((enciphered (bytevector-copy bv)))
      (tdea-decipher! bv 0 key)
      (list enciphered bv))))

(test-equal (test-tdea (string->utf8 "The Msg.")
                       (string->utf8 "01234567")
                       (string->utf8 "abcdefgh")
                       (string->utf8 "qwertyui"))
            (list #u8(243 85 37 68 185 248 44 83)
                  (string->utf8 "The Msg.")))

;; From NIST Special Publication 800-67 version 1.1,
;; revised 19 may 2008.
(define k1 #u8(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF))
(define k2 #u8(#x23 #x45 #x67 #x89 #xAB #xCD #xEF #x01))
(define k3 #u8(#x45 #x67 #x89 #xAB #xCD #xEF #x01 #x23))

(test-equal (test-tdea (string->utf8 "The qufc") ;sic
                       k1 k2 k3)
            (list #u8(#xA8 #x26 #xFD #x8C #xE5 #x3B #x85 #x5F)
                  (string->utf8 "The qufc")))

(test-equal (test-tdea (string->utf8 "k brown ")
                       k1 k2 k3)
            (list #u8(#xCC #xE2 #x1C #x81 #x12 #x25 #x6F #xE6)
                  (string->utf8 "k brown ")))

(test-equal (test-tdea (string->utf8 "fox jump")
                       k1 k2 k3)
            (list #u8(#x68 #xD5 #xC0 #x5D #xD9 #xB6 #xB9 #x00)
                  (string->utf8 "fox jump")))

(test-end)
