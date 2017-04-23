
(import (scheme base)   
        (astrocalc calendar)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "astrocalc-calendar")

(test-equal 2 (day-of-year (make-date 2 1 2013)))
(test-equal 32 (day-of-year (make-date 1 2 2013)))

(test-equal 3 (day-of-week-as-fixnum (make-date 30 6 1954)))
(test-equal 5 (day-of-week-as-fixnum (make-date 19 4 2013)))
(test-equal "Friday" (day-of-week (make-date 19 4 2013)))

(test-approx-same 2451544.5 (julian-day (make-date 1 1 2000)) 0.01)
(test-approx-same 2451179.5 (julian-day (make-date 1 1 1999)) 0.01)
(test-approx-same 2436115.5 (julian-day (make-date 4 10 1957)) 0.01)

(test-assert (gregorian-date? (make-date 1 1 1751)))
(test-assert (not (gregorian-date? (make-date 1 1 1752))))
(test-assert (not (julian-date? (make-date 1 1 1751))))
(test-assert (julian-date? (make-date 1 1 1752)))

(test-assert (leap-year? 2000))
(test-assert (not (leap-year? 1900)))
(test-assert (leap-year? 2012))
(test-assert (not (leap-year? 2013)))

(test-end)
