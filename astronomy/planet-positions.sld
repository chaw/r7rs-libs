;; Planet Positions
;; Implements chapter 32 of Meeus

(define-library
  (astronomy planet-positions)
  (export
    heliocentric-longitude-earth
    heliocentric-latitude-earth
    heliocentric-radius-earth
    heliocentric-longitude-jupiter
    heliocentric-latitude-jupiter
    heliocentric-radius-jupiter
    )
  (import (scheme base)
          (scheme cxr)
          (scheme inexact)
          (astronomy calendar)
          (astronomy utility)
          (srfi 1)
          (srfi 69))

  (begin

    ;; Routines specialised to each planet and result type
    ;; -- outer planets have possible shortcut for 1998-2025

    (define (heliocentric-longitude-earth date)
      (heliocentric-coordinate date *earth-l*))

    (define (heliocentric-latitude-earth date)
      (heliocentric-coordinate date *earth-b*))

    (define (heliocentric-radius-earth date)
      (heliocentric-coordinate date *earth-r*))

    (define (heliocentric-longitude-jupiter date)
      (heliocentric-coordinate-outer-planet date *jupiter-l-coeffs* *jupiter-l*))

    (define (heliocentric-latitude-jupiter date)
      (heliocentric-coordinate-outer-planet date *jupiter-b-coeffs* *jupiter-b*))

    (define (heliocentric-radius-jupiter date)
      (heliocentric-coordinate-outer-planet date *jupiter-r-coeffs* *jupiter-r*))

    ;; Generic routines

    ;; -- used for output planets, which have a shortcut-table for years 1998-2025
    (define (heliocentric-coordinate-outer-planet date shortcut-table list-nums)
      (let* ((year (date-year date)) ; (/ (day-of-year date) 365)))
             (coeffs (hash-table-ref/default shortcut-table year #f)))
        (if (list? coeffs)
          (sum-polynomial coeffs (time-in-julian-millennia date))
          (error "Main tables not yet present"))))

    ;; -- used for all planets, based on provided list of lists of ABC numbers
    (define (heliocentric-coordinate date list-nums)
      (define (sum-series abc-lst tau) ; sum of A cos (B+CT)
        (define (abc-value abc) ; computes A cos (B+CT)
          (* (car abc)
             (cos (+ (cadr abc)
                     (* (caddr abc) tau)))))
        ; 
        (fold + 0 (map abc-value abc-lst)))
      ;
      (let* ((tau (time-in-julian-millennia date))
             (ls (map (lambda (lst) (sum-series lst tau))
                      list-nums)))
        (/ (sum-polynomial ls tau)
           (expt 10 8))))

    ;; Follows Data from Meeus Appendix III for each planet's periodic terms

    (define *earth-l*
      '(; L0
        ((175347046 0 0)
         (3341656 4.6692568 62830758500)
         (34894 4.6261 12566.1517)
         (3497 2.7441 5753.3849)
         (3418 2.8289 3.5231)
         (3136 3.6277 77713.7715)
         (2676 4.4181 7860.4194)
         (2343 6.1352 3930.2097)
         (1324 .7425 11506.7698)
         (1273 2.0371 529.691)
         (1199 1.1096 1577.3435)
         (990 5.233 5884.927)
         (902 2.045 26.298)
         (857 3.508 398.149)
         (780 1.179 5223.694)
         (753 2.533 5507.553)
         (505 4.583 18849.228)
         (492 4.205 775.523)
         (357 2.92 0.067)
         (317 5.849 11790.629)
         (284 1.899 796.298)
         (271 0.315 10977.079)
         (243 0.345 5486.778)
         (206 4.806 2544.314)
         (205 1.869 5573.143)
         (202 2.458 6069.777)
         (156 0.833 213.299)
         (132 3.411 2942.463)
         (126 1.083 20.775)
         (115 0.645 0.98)
         (103 0.636 4694.003)
         (102 0.976 15720.839)
         (102 4.267 7.114)
         (99 6.21 2146.17)
         (98 0.68 155.42)
         (86 5.98 161000.69)
         (85 1.3 6275.96)
         (85 3.67 71430.70)
         (80 1.81 17260.15)
         (79 3.04 12036.46)
         (75 1.76 5088.63)
         (74 3.5 3154.69)
         (74 4.68 801.82)
         (70 0.83 9437.76)
         (62 3.98 8827.39)
         (61 1.82 7084.9)
         (57 2.78 6286.60)
         (56 4.39 14143.50)
         (56 3.47 6279.55)
         (52 0.19 12139.55)
         (52 1.33 1748.02)
         (51 0.28 5856.48)
         (49 0.49 1194.45)
         (41 5.37 8429.24)
         (41 2.4 19651.05)
         (39 6.17 10447.39)
         (37 6.04 10213.29)
         (37 2.57 1059.38)
         (36 1.71 2352.87)
         (36 1.78 6812.77)
         (33 0.59 17789.85)
         (30 0.44 83996.85)
         (30 2.74 1349.87)
         (25 3.16 4690.48))
        ; L1
        ((628331966747 0 0)
         (206059 2.678235 6283.075850)
         (4303 2.6351 12566.1517)
         (425 1.59 3.523)
         (119 5.796 26.298)
         (109 2.966 1577.344)
         (93 2.59 18849.23)
         (72 1.14 529.69)
         (68 1.87 398.15)
         (67 4.41 5507.55)
         (59 2.89 5223.69)
         (56 2.17 155.42)
         (45 0.4 796.30)
         (36 0.47 775.52)
         (29 2.65 7.11)
         (21 5.34 0.98)
         (19 1.85 5486.78)
         (19 4.97 213.30)
         (17 2.99 6275.96)
         (16 0.03 2544.31)
         (16 1.43 2146.17)
         (15 1.21 10977.08)
         (12 2.83 1748.02)
         (12 3.26 5088.63)
         (12 5.27 1194.45)
         (12 2.08 4694)
         (11 .77 553.57)
         (10 1.3 6286.6)
         (10 4.24 1349.87)
         (9 2.7 242.73)
         (9 5.64 951.72)
         (8 5.3 2352.87)
         (6 2.65 9437.76)
         (6 4.67 4690.48))
        ; L2
        ((52919 0 0)
         (8720 1.0721 6283.0758)
         (309 0.867 12566.152)
         (27 0.05 3.52)
         (16 5.19 26.30)
         (10 0.76 18849.23)
         (9 2.06 77713.77)
         (7 0.83 775.52)
         (5 4.66 1577.34)
         (4 1.03 7.11)
         (4 3.44 5573.14)
         (3 5.14 796.3)
         (3 6.05 5507.55)
         (3 1.19 242.73)
         (3 6.12 529.69)
         (3 0.31 398.15)
         (3 2.28 553.57)
         (2 4.38 5223.69)
         (2 3.75 0.98))
        ; L3
        ((289 5.844 6283.076)
         (35 0 0)
         (17 5.49 12566.15)
         (3 5.2 155.42)
         (1 4.72 3.52)
         (1 5.30 18849.23)
         (1 5.97 242.73))
        ; L4
        ((114 3.142 0)
         (8 4.13 6283.08)
         (1 3.84 12566.15))
        ; L5
        ((1 3.14 0))))

    (define *earth-b*
      '(; B0
        ((280 3.199 84334.662)
         (102 5.422 5507.553)
         (80 3.88 5223.69)
         (44 3.70 2352.87)
         (32 4.00 1577.34))
        ; B1
        ((9 3.90 5507.55)
         (6 1.73 5223.69))))

    (define *earth-r*
      '(; R0
        ((100013989 0 0)
         (1670700 3.0984635 6283.0758500)
         (13956 3.05525 12566.15170)
         (3084 5.1985 77713.7715)
         (1628 1.1739 5753.3849)
         (1576 2.8469 7860.4194)
         (925 5.453 11506.770)
         (542 4.564 3930.210)
         (472 3.661 5884.927)
         (346 0.964 5507.553)
         (329 5.900 5223.694)
         (307 0.299 5573.143)
         (243 4.273 11790.629)
         (212 5.847 1577.344)
         (186 5.022 10977.079)
         (175 3.012 18849.228)
         (110 5.055 5486.778)
         (98 0.89 6069.78)
         (86 5.69 15720.84)
         (86 1.27 161000.69)
         (65 0.27 17260.15)
         (63 0.92 529.69)
         (57 2.01 83996.85)
         (56 5.24 71430.70)
         (49 3.25 2544.31)
         (47 2.58 775.52)
         (45 5.54 9437.76)
         (43 6.01 6275.95)
         (39 5.36 4694.00)
         (38 2.39 8827.39)
         (37 0.83 19651.05)
         (37 4.90 12139.55)
         (36 1.67 12036.46)
         (35 1.84 2942.46)
         (33 0.24 7084.90)
         (32 0.18 5088.63)
         (32 1.78 398.15)
         (28 1.21 6286.60)
         (28 1.90 6279.55)
         (26 4.59 10447.39))
        ; R1
        ((103019 1.107490 6283.075850)
         (1721 1.0644 12566.1517)
         (702 3.142 0)
         (32 1.02 18849.23)
         (31 2.84 5507.55)
         (25 1.32 5223.69)
         (18 1.42 1577.34)
         (10 5.91 10977.08)
         (9 1.42 6275.96)
         (9 0.27 5486.78))
        ; R2
        ((4359 5.7846 6283.0758)
         (124 5.579 12566.152)
         (12 3.14 0)
         (9 3.63 77713.77)
         (6 1.87 5573.14)
         (3 5.47 18849.23))
        ; R3
        ((145 4.273 6283.076)
         (7 3.92 12566.15))
        ; R4
        ((4 2.56 6283.08))))

    (define *jupiter-l*
      '())

    (define *jupiter-b*
      '())

    (define *jupiter-r*
      '())

    (define *jupiter-l-coeffs*
      (let ((ht (make-hash-table eqv?)))
        (for-each (lambda (data) (hash-table-set! ht (car data) (cdr data)))
                  '(
                    (2017 190.8614472 27.5648846 -0.0376795 0.0950355 0.0125018 -0.0044412)
                    ))
        ht))

    (define *jupiter-b-coeffs*
      (let ((ht (make-hash-table eqv?)))
        (for-each (lambda (data) (hash-table-set! ht (car data) (cdr data)))
                  '(
                    (2017 1.3030989 -0.0022487 -0.1508309 0.0009584 0.0013231 0.0001389)
                    ))
        ht))

    (define *jupiter-r-coeffs*
      (let ((ht (make-hash-table eqv?)))
        (for-each (lambda (data) (hash-table-set! ht (car data) (cdr data)))
                  '(
                    (2017 5.4559510 0.0085834 -0.0322787 -0.0010422 0.0016684 -0.0004893)
                    ))
        ht))

    ))

