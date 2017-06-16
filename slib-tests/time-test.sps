;; Time library tests
;;
;; Need to specify timezone explicity

(import (scheme base)           (scheme write)
        (slib common-lisp-time)
        (slib posix-time)
        (slib time-core)
        (slib time-zone)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-time")

(let-values (((a b c d e f g h i) (decode-universal-time 1493474448.28725 0)))
            (test-equal 48 a)
            (test-equal 0 b)
            (test-equal 14 c)
            (test-equal 30 d)
            (test-equal 4 e)
            (test-equal 1947 f)
            (test-equal 2 g)
            (test-equal #f h)
            (test-equal 0 i))

(let-values (((a b c d e f g h i) (decode-universal-time 1493474448.28725 -1)))
            (test-equal 48 a)
            (test-equal 0 b)
            (test-equal 15 c)
            (test-equal 30 d)
            (test-equal 4 e)
            (test-equal 1947 f)
            (test-equal 2 g)
            (test-equal #f h)
            (test-equal -1 i))

(test-assert (leap-year? 2000))
(test-assert (leap-year? 2004))
(test-assert (not (leap-year? 1900)))
(test-assert (not (leap-year? 2001)))

(test-equal #(48 0 14 29 3 117 6 118 0 0 "GMT")
            (time:gmtime 1493474448.28725))

(test-equal "Sat Apr 29 14:00:48 2017\n"
            (asctime (gmtime 1493474448.28725)))

;; Note: written like this as Sagittarius does not like the usual (test-equal ... ) version
(let ((res (equal?  #(tz:rule "CST6CDT,M3.2.0/2:00:00,M11.1.0/2:00:00" "CST" "CDT" 21600 18000 (3 2 0 7200) (11 1 0 7200))
            (time-zone "CST6CDT,M3.2.0/2:00:00,M11.1.0/2:00:00"))))
  (test-assert res))

(test-end)

