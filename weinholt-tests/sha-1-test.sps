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

;; Packaged for R7RS Scheme by Peter Lane, 2017

(import (scheme base)
        (scheme char)
        (only (srfi 1) iota)
        (srfi 64)
        (weinholt bytevector)
        (weinholt sha-1))

(define (test/s expect . data)
  (let ((result (sha-1->string (apply sha-1 (map string->utf8 data)))))
    (unless (string-ci=? result expect)
      (error 'test "Bad result" result))))

(define (s . data) (string-downcase (sha-1->string (apply sha-1 (map string->utf8 data)))))

;; Run test-equals for "", "A", "AA", etc
(define test-equal-A*
  (lambda args
    (for-each (lambda (i d)
                (test-equal (s (make-string i #\A))
                            d))
              (iota (length args))
              args)))

(test-begin "weinholt-sha-1")

(test-equal-A*
 "da39a3ee5e6b4b0d3255bfef95601890afd80709"
 "6dcd4ce23d88e2ee9568ba546c007c63d9131c1b"
 "801c34269f74ed383fc97de33604b8a905adb635"
 "606ec6e9bd8a8ff2ad14e5fade3f264471e82251"
 "e2512172abf8cc9f67fdd49eb6cacf2df71bbad3"
 "c1fe3a7b487f66a6ac8c7e4794bc55c31b0ef403"
 "2d2929e0f1bca99d9652924ce73b7969d33ff429"
 "f9b2869de6cc9226b990d83f805ec83915cc9c85"
 "c08598945e566e4e53cf3654c922fa98003bf2f9"
 "1cbbd7d768f77d4d3f24de43238979aa9fa1cd2f"
 "c71613a7386fd67995708464bf0223c0d78225c4"
 "004537f3b1fd67347489185a1c4b55da58f6edca"
 "2b52d47ab698ccce79ab6d0552e98f87f8a3aebc"
 "91dd8a106d38bd458250b80314a3b4837acfa85b"
 "9108c1fc03ff53527f9d9de94d9c151e697e154d"
 "343ad63c4d45b81d945360c080b065c98c7a8351"
 "19b1928d58a2030d08023f3d7054516dbc186f20"
 "9ee276acbf8a1257a58a5bad22bef8907e49cbf2"
 "3a8262b7c3b43877389d300986b0c0b1eedfdfbf"
 "1a6372d15d776f9879d300e51ec145363cd63667"
 "ebd3d4adf97066c84b8ed17d6bd1e270818763e0"
 "29ad0c6384182c5c2d4c953e200eed245467e503"
 "d088f3b187a0957d72b5d5645939bfc4302dffb8"
 "293efde746444af8e7aff0ad1a57c874cdc50966"
 "4f130f23896bd6d0e95f2a42b2cb83d17ac8f1a2"
 "a92b995e293d295c4bbab7043cccb030bef47488"
 "ed641f05795d5ee712d1e6ddc2d5146079db9dee"
 "82e757683db0b0417976c1661f7b020ae5225b80"
 "7b92fac2f01809101168d085e9f1ef059b131be4"
 "41be845b8e19da10e18a6bd3105793484d22bd53"
 "2a22d32e957a9de69c50e8f52872e2dbf1d0745c"
 "ca75e66a01a2b5b24f825f569d5ddeead3e50e4d"
 "43d83b2e816a89cac876f16530b0b625585c8160"
 "e04976c6e1ce44aa1840b07b57021c158a11eafc"
 "609b3f4ee88fd429c53d51dca7ace87711e7d48f"
 "4c911f83e9b42c92b8ea62135fa1bc0e727ce367"
 "3c8a34351337e8f5376092d3f329767c8035344d"
 "0b314daa55be9ff60f4337a25fef266036aed20c"
 "35309ec13ef8d90aaae172e4cf437eb16ddbf6d5"
 "6784f01a2b317aeef2ac03660dafa3270f4d420e"
 "5cdbb64242d8551a7cf583903fd7d5b72b277537"
 "0e477417eecfe482fd137e4a038fb5cf6dc7be76"
 "880b405e8e5059e3aa1797f662ff4a0cfcbce20b"
 "885dd07854409bf8cf5443652fd6835c23423338"
 "a7da128970268478e46f9585d0fb6297349b9675"
 "06bf9b84f2cffdb4b343ef9b3ddd1847f9b6ce3c"
 "4683b63a087f88e7ada2f6e3eceb4a0e9f7195a1"
 "b459efc276e7c1e39f997ed6c9b4f692dafd30b5"
 "8b2177f39b224cab2fb4df5ee4827fbe7115ce44"
 "d52bcfb557dd3ed70968f8835ccff3c924885631"
 "080316afb4e11d98120b29d1070ce749f1f0a32c"
 "4456f6c537924b7d47e430050d92bf6949a1fba8"
 "defc08198e86f88a007ca10f10d8af0d402ffdc3"
 "55066b480654e5846549494b863e3cd34bae76eb"
 "18b837ae2f9a204a7fea6d6a2ae5174365137861"
 "5021b3d42aa093bffc34eedd7a1455f3624bc552"
 "6b45e3cf1eb3324b9fd4df3b83d89c4c2c4ca896"
 "e8d6ea5c627fc8676fa662677b028640844dc35c"
 "e0ed6b6f61dae4219379cf9fe19565150c8e6046"
 "ba83959b9f4a8b3ca082d501e7b75ce73992e35f"
 "c9c4571630054c5466d19b5ea28069dc71c72b68"
 "fc202c022fdc439b99892020e04fc93b4ee8448a"
 "0dc94299f2d293a48173f9c78a882f8a9bffe3b0"
 "0ec86b3f3ac34ad860fa8da56bcca03a54018049"
 "30b86e44e6001403827a62c58b08893e77cf121f"
 "826b7e7a7af8a529ae1c7443c23bf185c0ad440c"
 "eddee92010936db2c45d2c9f5fdd2726fcd28789"
 "d0c9def032806d32bc485ea5493e34217d5091c9"
 "01ae707f5f6574b061a4643f59c98277da6544a3"
 "4b4e4f859b006e5b0afe2dc2806bae2ab3cb55b0"
 "049dbd0c7c40ce1a9a322531c994778cae8f3f0f"
 "d0929751861c93c786335ead7d5b5c066b3a8cb7"
 "41f9070504f9c81abfbb614daaec3b26a2f9237e"
 "4fced99ee1b5cb0dd68a5c5a194b79dc70841d43"
 "1268a031cf339eb68968e87334574862a95c4d48"
 "b17654dfc615ef4a8dd86d53f5dee434bec61143"
 "e106fa6de4ce5177f0d2fd4b7bae8478456dc25c"
 "ccf93ced5c9a95c23ae36936b7ebff088c991919"
 "e5e8a4e450be9938b318a96a5f95b12733cb39be"
 "c958795890d309b7add6d6432b510c297375e5d7"
 "2d5d85dfd3361150e8bebe7cb730c08258206ba6"
 "259c4c06d026726ced06b9d81cd3abcd5e936393"
 "736545c3e47672f832d54171c88b213789160c8d"
 "3da1e7b5188c2fdc84aa4e3b0b2c05c93f246e2f"
 "dbf2a20a9e1ebe314e8da8a678fdc6949750b9c4"
 "bcc9fe3fff88ff66df70c1e53401a28c5873bd63"
 "54c4ba90ae95dd2dda25ce8eaec645ac56052845"
 "1e34b919b2b449c51c72c4922e7d4841405857f1"
 "0ac5a64edb54535a9d71ccc853a1073a5f2001e6"
 "792534345f64f4d8cd1457ce8edf3e067cb5666f"
 "9a2f2b83877c65c955ab6a6c239357fac93609a5"
 "5cd33462b7a8ffbf17dda2b61911377658a96f26"
 "768bbe547a68238aecbbdddf78f517227e6ea98b"
 "b70f0297e92d4b1f5ae01618d7ed6aafc2dd8404"
 "6675ab9c5ca21f903e070ea1a217ac655584cf55"
 "84e94d95ec69d965d0b36ca3a9ce5dcd4ec84bab"
 "4e3872621039b359e7371bb9810430a5a2c78195"
 "81ad592e1a48b35db67cf02705566315f2c149d1"
 "39ba29cf0bc73595d1476abaa413ac968cdf8fa2"
 "436bab78a2b10f04528d408c922fcdfba069419c"
 "7a4a9ae537ebbbb826b1060e704490ad0f365ead")

(test-equal (s "AAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          "30b86e44e6001403827a62c58b08893e77cf121f")
(test-equal (s "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAAAAAAA")
          "30b86e44e6001403827a62c58b08893e77cf121f")
(test-equal (s "AAAAAAAAAAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          "30b86e44e6001403827a62c58b08893e77cf121f")
(test-equal (s "" "" "" "" "" "AAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAA" "AAAA")
          "30b86e44e6001403827a62c58b08893e77cf121f")
(test-equal (s "" "" "" "AAAAAAAAAAAAAAAAAAAAA" "" "AAAAAAAAAAAAAAAAAAAAA" "AAAAAAAAAAAAAAAAAA" "AAAA" "")
          "30b86e44e6001403827a62c58b08893e77cf121f")
(test-equal (s "" "" "AAAAAAAAAAAAAAAAAAAAA" "" "AAAAAAAAAAAAAAAAAAAA" "A" "AAAAAAAAAAAAAAAAAA" "AAAA" "")
          "30b86e44e6001403827a62c58b08893e77cf121f")

(define (o string start end)
  (let ((state (make-sha-1)))
    (sha-1-update! state (string->utf8 string) start end)
    (sha-1-finish! state)
    (string-downcase (sha-1->string state))))

(test-equal (o "xAAAAAAAAAAAAAAAAAAyy" 1 19)    "3a8262b7c3b43877389d300986b0c0b1eedfdfbf")
(test-equal (o "xxAAAAAAAAAAAAAAAAAAyy" 2 20)    "3a8262b7c3b43877389d300986b0c0b1eedfdfbf")

;; From RFC 3174

(define (r repeat data)
  (let ((state (make-sha-1)))
    (sha-1-clear! state)
    (do ((data (string->utf8 data))
         (i 0 (+ i 1)))
        ((= i repeat))
      (sha-1-update! state data))
    (sha-1-finish! state)
    (string-downcase (sha-1->string state))))

(test-equal (r 1 "abc")
          "a9993e364706816aba3e25717850c26c9cd0d89d")

(test-equal (r 1 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
          "84983e441c3bd26ebaae4aa1f95129e5e54670f1")

(test-equal (r 1000000 "a")
          "34aa973cd4c4daa4f61eeb2bdbad27316534016f")

(test-equal (r 10 "0123456701234567012345670123456701234567012345670123456701234567")
          "dea356a2cddd90c7a7ecedc5ebb563934f460452" )

;; From RFC 2202, (in which some tests are repeated due to some weird
;; typesetting error)

(define (h key data) (string-downcase (sha-1->string (hmac-sha-1 key data))))

(test-equal (h (make-bytevector 20 #x0b)
          (string->utf8 "Hi There"))
          "b617318655057264e28bc0b6fb378c8ef146be00")

(test-equal (h (string->utf8 "Jefe")
          (string->utf8 "what do ya want for nothing?"))
          "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79")

(test-equal (h (make-bytevector 20 #xaa)
          (make-bytevector 50 #xdd))
          "125d7342b9ac11cd91a39af48aa17b4f63f175d3")

(test-equal (h #u8(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c
                    #x0d #x0e #x0f #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19)
          (make-bytevector 50 #xcd))
          "4c9007f4026250c6bc8414f9bf50c86c2d7235da")

(test-equal (h (make-bytevector 20 #x0c)
          (string->utf8 "Test With Truncation"))
          "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04")
(test-equal (sha-1-hash=?
        (hmac-sha-1 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                    (string->utf8 "Test With Truncation"))
        (uint->bytevector #x4c1a03424b55e07fe7f27be1d58bb9324a9a5a04))
          #t)
(test-equal (sha-1-96-hash=?
        (hmac-sha-1 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                    (string->utf8 "Test With Truncation"))
        (uint->bytevector #x4c1a03424b55e07fe7f27be1))
          #t)
(test-equal (sha-1-96-hash=?
        (hmac-sha-1 (uint->bytevector #x0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c)
                    (string->utf8 "Test With Truncation"))
        (uint->bytevector #x4c1a03424b55e07fe7f27be0))
          #f)                           ;bad mac


(test-equal (h (make-bytevector 80 #xaa)
          (string->utf8 "Test Using Larger Than Block-Size Key - Hash Key First"))
          "aa4ae5e15272d00e95705637ce8a3b55ed402112")

(test-equal (h (make-bytevector 80 #xaa)
          (string->utf8 "Test Using Larger Than Block-Size Key and Larger Than One Block-Size Data"))
      "e8e99d0f45237d786d6bbaa7965c7808bbff1a91")

(test-end)
