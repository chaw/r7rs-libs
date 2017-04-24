
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

(test-assert (and (= 31 (date-day (easter-day 1991)))
                  (= 3 (date-month (easter-day 1991)))))

(test-assert (and (= 19 (date-day (easter-day 1992)))
                  (= 4 (date-month (easter-day 1992)))))

(test-assert (and (= 11 (date-day (easter-day 1993)))
                  (= 4 (date-month (easter-day 1993)))))

(test-assert (and (= 18 (date-day (easter-day 1954)))
                  (= 4 (date-month (easter-day 1954)))))

(test-assert (and (= 23 (date-day (easter-day 2000)))
                  (= 4 (date-month (easter-day 2000)))))

(test-assert (and (= 22 (date-day (easter-day 1818)))
                  (= 3 (date-month (easter-day 1818)))))

(test-end)
