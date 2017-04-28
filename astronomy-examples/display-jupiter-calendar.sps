;; Show Calendar for a given year
;; of Jupiter's Central Meridian in System I and II

(import (scheme base)
        (astronomy calendar)
        (astronomy jupiter)
        (slib format))

;; Display calendar for given year, s = I or II
;; -- low accuracy
(define (calendar year s)
  (format #t "Calendar for year ~a system ~a~&" year s)
  (for-each (lambda (date)
              (let-values (((w1 w2 De Ds) (jupiter-data-low-accuracy date)))
                          (format #t "~27a ~5,1f~&"
                                  (date->string date)
                                  (if (eq? s 'I)
                                    w1
                                    w2))))
            (all-dates year)))

;; -- high accuracy
(define (calendar2 year s)
  (format #t "Calendar for year ~a system ~a~&" year s)
  (for-each (lambda (date)
              (let-values (((w1 w2 De Ds P) (jupiter-data date)))
                          (format #t "~27a ~5,1f~&"
                                  (date->string date)
                                  (if (eq? s 'I)
                                    w1
                                    w2))))
            (all-dates year)))

(calendar2 2017 'I)
(calendar2 2017 'II)
