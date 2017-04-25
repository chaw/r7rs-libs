;; Show some moon phases 

(import (scheme base)
        (astronomy calendar)
        (astronomy moon)
        (slib format)
        (srfi 1))

;; Find and show all moons of given phase
(define (show-all-moons year phase)
  (format #t "~%      ~a 2017      ~%~%~{~a~&~}"
          phase
          (map date->string
               (filter (lambda (d) (= (date-year d) 2017))
                       (delete-duplicates
                         (map (lambda (d) (julian-day->date (lunar-phase d phase)))
                              (all-dates 2017))
                         date-equal?)))))

(show-all-moons 2017 'new-moon)
(show-all-moons 2017 'first-quarter)
(show-all-moons 2017 'full-moon)
(show-all-moons 2017 'last-quarter)

;(let ((res (lunar-phase (make-date 14 2 1977) 'new-moon)))
;  (format #t "~a ~a~&" res (date->string (julian-day->date res))))
;(let ((res (lunar-phase (make-date 1 1 2044) 'last-quarter)))
;  (format #t "~a ~a~&" res (date->string (julian-day->date res))))

