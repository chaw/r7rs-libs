;; Show some moon phases 

(import (scheme base)
        (astronomy calendar)
        (astronomy moon)
        (slib format)
        (srfi 1))

;; Find and show all moons of given phase
(define (show-all-moons year phase)
  (format #t "~%      ~a 2017      ~%~%" phase)

  (for-each
    (lambda (dt)
      (format #t "~a ~a~&" (date->string (car dt)) (time->string (cadr dt))))
    (filter (lambda (dt) (= (date-year (car dt)) 2017))
            (delete-duplicates
              (map (lambda (d) 
                     (let-values (((d t) (julian-day->date+time (lunar-phase d phase))))
                                 (list d t)))
                   (all-dates 2017))
              (lambda (a b) (date-equal? (car a) (car b)))))))

(show-all-moons 2017 'new-moon)
(show-all-moons 2017 'first-quarter)
(show-all-moons 2017 'full-moon)
(show-all-moons 2017 'last-quarter)

