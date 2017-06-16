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

;; Packaged for R7RS Scheme by Peter Lane, 2017

(import (scheme base)
        (scheme char)
        (weinholt sha-2)
        (srfi 64)
        )

(test-begin "weinholt-sha-256")

(define (sha256 data)
  (string-downcase (sha-256->string (sha-256 data))))

(test-equal (sha256 #u8())    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

;; http://csrc.nist.gov/groups/ST/toolkit/examples.html

(test-equal (sha256 (string->utf8 "abc"))   
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

(test-equal (sha256 (string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")

(test-equal (sha256 #u8(#xbd))
            "68325720aabd7c82f30f554b313d0570c95accbb7dc4b5aae11204c08ffe732b")

(test-equal (sha256 #u8(#xc9 #x8c #x8e #x55))
            "7abc22c0ae5af26ce93dbb94433a0e0b2e119d014f8e7f65bd56c61ccccd9504")

(test-equal (sha256 (make-bytevector 55 0))
            "02779466cdec163811d078815c633f21901413081449002f24aa3e80f0b88ef7")

(test-equal (sha256 (make-bytevector 56 0))
            "d4817aa5497628e7c77e6b606107042bbba3130888c5f47a375e6179be789fbb")

(test-equal (sha256 (make-bytevector 57 0))
            "65a16cb7861335d5ace3c60718b5052e44660726da4cd13bb745381b235a1785")

(test-equal (sha256 (make-bytevector 64 0))
            "f5a5fd42d16a20302798ef6ed309979b43003d2320d9f0e8ea9831a92759fb4b")

(test-equal (sha256 (make-bytevector 1000 0))
            "541b3e9daa09b20bf85fa273e5cbd3e80185aa4ec298e765db87742b70138a53")

(test-equal (sha256 (make-bytevector 1000 #x41))
            "c2e686823489ced2017f6059b8b239318b6364f6dcd835d0a519105a1eadd6e4")

(test-equal (sha256 (make-bytevector 1005 #x55))
            "f4d62ddec0f3dd90ea1380fa16a5ff8dc4c54b21740650f24afc4120903552b0")

;; Let's not run these now. When the sha-256 library is fast enough,
;; these might be test-equaled, but without allocating gigantic
;; bytevectors.

(test-equal (sha256 (make-bytevector 1000000 0))
            "d29751f2649b32ff572b5e0a9f541ea660a50f94ff0beedfb0b692b924cc8025")

;; (test-equal (sha256 (make-bytevector #x20000000 #x5a))
;;           "15a1868c12cc53951e182344277447cd0979536badcc512ad24c67e9b2d4f3dd")

;; (test-equal (sha256 (make-bytevector #x41000000 0))
;;           "461c19a93bd4344f9215f5ec64357090342bc66b15a148317d276e31cbc20b53")

;; (test-equal (sha256 (make-bytevector #x6000003e #x42))
;;           "c23ce8a7895f4b21ec0daf37920ac0a262a220045a03eb2dfed48ef9b05aabea")



(define (sha224 data)
  (string-downcase (sha-224->string (sha-224 data))))

(test-equal (sha224 (string->utf8 "abc"))
            "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")

(test-equal (sha224 (string->utf8
                      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))
            "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525")

;;; HMAC

(test-equal (string-downcase
              (sha-256->string
                (hmac-sha-256
                  (bytevector #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d
                      #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1a
                      #x1b #x1c #x1d #x1e #x1f #x20 #x21 #x22 #x23 #x24 #x25 #x26 #x27 
                      #x28 #x29 #x2a #x2b #x2c #x2d #x2e #x2f #x30 #x31 #x32 #x33 #x34
                      #x35 #x36 #x37 #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f)
                  (string->utf8 "Sample message for keylen=blocklen"))))
            "8bb9a1db9806f20df7f77b82138c7914d174d59e13dc4d0169c9057b133e1d62")

(test-end)
