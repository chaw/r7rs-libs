
(import (scheme base)
        (astronomy calendar)
        (astronomy moon)
        (srfi 64))

(test-begin "astronomy-moon")

(test-equal 18 (date-day (julian-day->date (lunar-phase (make-date 14 2 1977) 'new-moon))))
(test-equal 2 (date-month (julian-day->date (lunar-phase (make-date 14 2 1977) 'new-moon))))
(test-equal 1977 (date-year (julian-day->date (lunar-phase (make-date 14 2 1977) 'new-moon))))

(test-equal 21 (date-day (julian-day->date (lunar-phase (make-date 1 1 2044) 'last-quarter))))
(test-equal 1 (date-month (julian-day->date (lunar-phase (make-date 1 1 2044) 'last-quarter))))
(test-equal 2044 (date-year (julian-day->date (lunar-phase (make-date 1 1 2044) 'last-quarter))))

(test-end)
