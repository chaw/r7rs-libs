;; Copyright © 2010, 2011 Göran Weinholt <goran@weinholt.se>

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

(import (weinholt dh)
        (srfi 60)
        (srfi 64)
        (scheme base))

(define (check-dh g p)
  (let-values (((y Y) (make-dh-secret g p (integer-length p)))
                      ((x X) (make-dh-secret g p (integer-length p))))
           (test-equal (expt-mod X y p) (expt-mod Y x p))
           #f))

(test-begin "dh-test")

(check-dh modp-group1-g modp-group1-p)
(check-dh modp-group2-g modp-group2-p)
(check-dh modp-group5-g modp-group5-p)
(check-dh modp-group14-g modp-group14-p)
;; These take too long to test on slower systems, and will probably
;; pass anyway:
;; (check-dh modp-group15-g modp-group15-p)
;; (check-dh modp-group16-g modp-group16-p)
;; (check-dh modp-group17-g modp-group17-p)
;; (check-dh modp-group18-g modp-group18-p)

(test-end)
