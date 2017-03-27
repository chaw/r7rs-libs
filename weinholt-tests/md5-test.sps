;; Copyright © 2009, 2010 Göran Weinholt <goran@weinholt.se>

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

(import (scheme base)
        (scheme char)
        (weinholt bytevector)
        (weinholt md5)
        (srfi 64)) 

(test-begin "weinholt-md5")

(define (m str) (string-downcase (md5->string (md5 (string->utf8 str)))))

(test-equal (m (make-string 100000 #\A))    "5793f7e3037448b250ae716b43ece2c2")
(test-equal (m (make-string 1000000 #\A))    "48fcdb8b87ce8ef779774199a856091d")

;;; From RFC 1321
(test-equal (m "")
            "d41d8cd98f00b204e9800998ecf8427e")
(test-equal (m "a")
            "0cc175b9c0f1b6a831c399e269772661")
(test-equal (m "abc")
            "900150983cd24fb0d6963f7d28e17f72")
(test-equal (m "message digest")
            "f96b697d7cb7938d525a2f31aaf161d0")
(test-equal (m "abcdefghijklmnopqrstuvwxyz")
            "c3fcd3d76192e4007dfb496cca67e13b")
(test-equal (m "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
            "d174ab98d277d9f5a5611c2c9f419d9f")
(test-equal (m "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
            "57edf4a22be3c955ac49da2e2107b67a")

;;; From RFC 2104/2202
(define (h key data) (string-downcase (md5->string (hmac-md5 key data))))

(test-equal (h (make-bytevector 16 #x0b)
               (string->utf8 "Hi There"))
            "9294727a3638bb1c13f48ef8158bfc9d")

(test-equal (h (string->utf8 "Jefe")
               (string->utf8 "what do ya want for nothing?"))
            "750c783e6ab0b503eaa86e310a5db738")

(test-equal (h (make-bytevector 16 #xAA)
               (make-bytevector 50 #xDD))
            "56be34521d144c88dbb8c733f0e8b3f6")

(test-equal (h #u8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c
                   #x0d #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19)
               (make-bytevector 50 #xcd))
            "697eaf0aca3a3aea3a75164746ffaa79")

(test-equal (h (make-bytevector 16 #x0c)
               (string->utf8 "Test With Truncation"))
            "56461ef2342edc00f9bab995690efd4c") ; not testing truncation...
(test-equal (md5-hash=?
              (hmac-md5 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                        (string->utf8 "Test With Truncation"))
              (uint->bytevector #x56461ef2342edc00f9bab995690efd4c))
            #t)
(test-equal (md5-96-hash=?
              (hmac-md5 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                        (string->utf8 "Test With Truncation"))
              (uint->bytevector #x56461ef2342edc00f9bab995))
            #t)
(test-equal (md5-96-hash=?
              (hmac-md5 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                        (string->utf8 "Test With Truncation"))
              (uint->bytevector #x56461ef2342edc00f9bab990))
            #f)                           ;bad mac

(test-equal (h (make-bytevector 80 #xaa)
               (string->utf8 "Test Using Larger Than Block-Size Key - Hash Key First"))
            "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd")

(test-equal (h (make-bytevector 80 #xaa)
               (string->utf8 "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"))
            "6f630fad67cda0ee1fb1f562db3aa53e")

(test-end)

