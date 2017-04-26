;; Calculation of calendar data
;; from Meeus, Astronomical Algorithms

;; Peter Lane, 2017

(define-library
  (astronomy calendar)
  (export 
    all-dates
    date->decimal
    date->string
    date-day
    date-equal?
    date-month
    date-year
    date?
    day-of-week
    day-of-week-as-fixnum
    day-of-year
    easter-day
    gregorian-date?
    julian-date?
    julian-day
    julian-day->date
    julian-day->date+time
    julian-ephemeris-day
    leap-year?
    make-date
    month-name
    month-names
    time->string
    time?
    time-hour
    time-minute
    time-second
    week-days
    )
  (import (scheme base)
          (astronomy utility)
          (slib format)
          (srfi 1))

  (begin

    (define *week-days*
      '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

    (define (week-days) *week-days*)

    (define *month-names* 
      '("January" "February" "March" "April" "May" "June" 
        "July" "August" "September" "October" "November" "December"))

    (define (month-names) *month-names*)

    ;; Return month name for m a month number in [1, 12]
    (define (month-name m)
      (list-ref *month-names* (- m 1)))
    
    ;; Record for date
    (define-record-type <date>
                        (make-date day month year)
                        date?
                        (day date-day)
                        (month date-month)
                        (year date-year))

    ;; Record for time
    (define-record-type <time>
                        (make-time hour minute second)
                        time?
                        (hour time-hour)
                        (minute time-minute)
                        (second time-second))

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

    ;; Return the given time in a string format
    (define (time->string time)
      (format #f "~dh ~dm ~ds"
              (time-hour time)
              (time-minute time)
              (time-second time)))

    ;; Convert the given date as a decimal, e.g. March 31st is year.25
    (define (date->decimal date)
      (+ (date-year date)
         (/ (day-of-year date)
            (if (leap-year? (date-year date))
              366
              365))))

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
          (+ (floor (* 365.25 (+ y 4716)))
             (floor (* 30.6001 (+ 1 m)))
             (date-day date)
             b
             -15245/10))))

    ;; Return the date for a given Julian Day
    (define (julian-day->date jd)
      (let* ((adj-jd (+ jd 0.5))
             (Z (floor adj-jd))
             (F (- adj-jd Z))
             (A (if (< Z 2299161)
                  Z
                  (let ((alpha (floor (/ (- Z 1867216.25) 36524.25))))
                    (+ Z 1 alpha (- (floor (/ alpha 4)))))))
             (B (+ A 1524))
             (C (floor (/ (- B 122.1) 365.25)))
             (D (floor (* 365.25 C)))
             (E (floor (/ (- B D) 30.6001)))

             (d (+ B (- D) (- (floor (* 30.6001 E))) F))
             (m (if (< E 14) (- E 1) (- E 13)))
             (y (if (> m 2) (- C 4716) (- C 4715))))
        (make-date (exact (floor d)) (exact (floor m)) (exact (floor y)))))

    ;; Return the date and time for a given Julian Day as values
    (define (julian-day->date+time jd)
      (let* ((date (julian-day->date jd))
             (rem-secs (exact (floor (* 86400 (- jd (julian-day date))))))
             (h (exact (floor (/ rem-secs 3600))))
             (m (exact (floor (/ (- rem-secs (* 3600 h)) 60))))
             (s (- rem-secs (* 3600 h) (* 60 m))))
        (values date (make-time h m s))))

    ;; Return the Julian Ephemeris Day (JDE) for given date
    (define (julian-ephemeris-day date)
      (+ (julian-day date)
         0.00067)) ; delta-T for 2013

    ;; Return the day of year of given date
    (define (day-of-year date)
      (let ((i (list-index (lambda (d) (date-equal? date d)) 
                           (all-dates (date-year date)))))
        (if (number? i)
          (+ 1 i)
          (error "Could not find date in calendar - is the date valid?"))))

    ;; Return the day of week of given date as a fixnum
    (define (day-of-week-as-fixnum date)
      (modulo (exact (floor (+ 3/2 (julian-day date)))) 7))

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

    ;; Return date of Easter for given year
    ;; -- year must be in Gregorian calendar
    (define (easter-day year)
      (let*-values (((t a) (floor/ year 19))
                    ((b c) (floor/ year 100))
                    ((d e) (floor/ b 4))
                    ((f u) (floor/ (+ 8 b) 25))
                    ((g v) (floor/ (+ b (- f) 1) 3))
                    ((w h) (floor/ (+ (* 19 a) b (- d) (- g) 15) 30))
                    ((i k) (floor/ c 4))
                    ((x l) (floor/ (+ 32 (* 2 e) (* 2 i) (- h) (- k)) 7))
                    ((m y) (floor/ (+ a (* 11 h) (* 22 l)) 451))
                    ((n p) (floor/ (+ h l (* -7 m) 114) 31)))
                   (make-date (+ 1 p) n year)))

    ))
