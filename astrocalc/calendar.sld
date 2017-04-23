;; Calculation of calendar data
;; from Meeus, Astronomical Algorithms

(define-library
  (astrocalc calendar)
  (export 
    all-dates
    date->string
    date-day
    date-month
    date-year
    date?
    day-of-week
    day-of-week-as-fixnum
    day-of-year
    gregorian-date?
    julian-date?
    julian-day
    julian-ephemeris-day
    leap-year?
    make-date
    )
  (import (scheme base)
          (astrocalc utility)
          (slib format)
          (srfi 1))

  (begin

    (define *week-days*
      '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

    (define *month-names* 
      '("January" "February" "March" "April" "May" "June" 
        "July" "August" "September" "October" "November" "December"))
    
    ;; Record for date
    (define-record-type <date>
                        (make-date day month year)
                        date?
                        (day date-day)
                        (month date-month)
                        (year date-year))

    ;; Return #t if two dates represent identical dates
    (define (date-equal? date1 date2)
      (and (= (date-day date1) (date-day date2))
           (= (date-month date1) (date-month date2))
           (= (date-year date1) (date-year date2))))

    ;; Return the given date in a string format
    (define (date->string date)
      (format #f "~a ~d ~a ~d"
              (day-of-week date)
              (date-day date)
              (list-ref *month-names* (- (date-month date) 1))
              (date-year date)))

    ;; Return the Julian Day (JD) for given date
    (define (julian-day date)
      (let ((m (date-month date))
            (y (date-year date)))
        (when (or (= 1 m) (= 2 m))
          (set! m (+ m 12))
          (set! y (- y 1)))
        (let* ((a (floor (/ y 100)))
               (b (if (gregorian-date? date)
                    0
                    (+ 2 (* -1 a) (floor (/ a 4))))))
          (+ (floor (* 36525/100 (+ y 4716)))
             (floor (* 306001/10000 (+ 1 m)))
             (date-day date)
             b
             -15245/10))))

    ;; Return the Julian Ephemeris Day (JDE) for given date
    (define (julian-ephemeris-day date)
      (+ (julian-day date)
         67/100000)) ; delta-T for 2013

    ;; Return the day of year of given date
    (define (day-of-year date)
      (let ((i (list-index (lambda (d) (date-equal? date d)) 
                           (all-dates (date-year date)))))
        (if (number? i)
          (+ 1 i)
          (error "Could not find date in calendar - is the date valid?"))))

    ;; Return the day of week of given date as a fixnum
    (define (day-of-week-as-fixnum date)
      (floor (modulo (+ 3/2 (julian-day date)) 7)))

    ;; Return the day of week of given date
    (define (day-of-week date)
      (list-ref *week-days* (day-of-week-as-fixnum date)))

    ;; Return #t if date is within Gregorian Calendar for England,
    ;; which started 1/1/1752
    (define (gregorian-date? date)
      (< (date-year date) 1752))

    ;; Return #t if date is after Gregorian Calendar start in England
    (define (julian-date? date)
      (not (gregorian-date? date)))

    ;; Return a list of all dates in the given year
    (define (all-dates year)
      (apply append
             (map (lambda (md) 
                    (let ((month (car md)))
                      (map (lambda (d) (make-date (+ 1 d) month year))
                           (iota (cadr md)))))
                  (list '(1 31) (list 2 (if (leap-year? year) 29 28)) 
                        '(3 31) '(4 30) '(5 31) '(6 30) 
                        '(7 31) '(8 31) '(9 30) '(10 31) 
                        '(11 30) '(12 31)))))

    ;; Return #t if given year is a leap year
    (define (leap-year? year)
      (or (and (divisible? year 4)
               (not (divisible? year 100)))
          (divisible? year 400)))

    ))
