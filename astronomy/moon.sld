;; Calculations about the Moon
;; Peter Lane, 2017

;; TODO: Still has some errors

(define-library
  (astronomy moon)
  (export lunar-phase)
  (import (scheme base)
          (astronomy calendar)
          (astronomy utility))

  (begin

    ;; Given a date and a phase ('new-moon 'first-quarter 'full-moon 'last-quarter)
    ;; Returns the JDE for that phase
    (define (lunar-phase date phase)
      (define new-moon-constants
        (map (lambda (n) (/ n 100000))
             '(-40720 17241 1608 1039 739 -514 208)))
      (define full-moon-constants
        (map (lambda (n) (/ n 100000))
             '(-40614 17302 1614 1043 734 -515 209)))
      (define (adjust-whole JDE E M MM F T constants)
        (+ JDE
           (* (list-ref constants 0) (dsin MM))
           (* (list-ref constants 1) E (dsin M))
           (* (list-ref constants 2) (dsin (* 2 MM)))
           (* (list-ref constants 3) (dsin (* 2 F)))
           (* (list-ref constants 4) E (dsin (- MM M)))
           (* (list-ref constants 5) E (dsin (+ MM M)))
           (* (list-ref constants 6) E E (dsin (* 2 M)))
           (* -111/100000 (dsin (- MM (* 2 F))))
           (* -57/100000 (dsin (+ MM (* 2 F))))
           (* 56/100000 E (dsin (+ (* 2 MM) M)))
           (* -42/100000 (dsin (* 3 MM)))
           (* 42/100000 E (dsin (+ M (* 2 F))))
           (* 38/100000 E (dsin (- M (* 2 F))))
           (* -24/100000 E (dsin (- (* 2 MM) M)))
           (* -17/100000 (dsin T))
           (* -7/100000 (dsin (+ MM (* 2 M))))
           (* 4/100000 (dsin (- (* 2 MM) (* 2 F))))
           (* 4/100000 (dsin (* 3 M)))
           (* 3/100000 (dsin (+ MM M (neg (* 2 F)))))
           (* 3/100000 (dsin (+ (* 2 MM) (* 2 F))))
           (* 3/100000 (dsin (+ MM M (* 2 F))))
           (* 3/100000 (dsin (+ MM (neg M) (* 2 F))))
           (* 2/100000 (dsin (+ MM (neg M) (neg (* 2 F)))))
           (* 2/100000 (dsin (+ (* 3 MM) M)))
           (* 2/100000 (dsin (* 4 MM)))))
      (define (adjust-quarter JDE E M MM F T)
        (+ JDE
           (* -62801/100000 (dsin MM))
           (* 17172/100000 E (dsin M))
           (* 1183/100000 E (dsin (+ MM M)))
           (* 862/100000 (dsin (* 2 MM)))
           (* 804/100000 (dsin (* 2 F)))
           (* 454/100000 E (dsin (- MM M)))
           (* 204/100000 E E (dsin (* 2 M)))
           (* -180/100000 (dsin (- MM (* 2 F))))
           (* -70/100000 (dsin (+ MM (* 2 F))))
           (* -40/100000 (dsin (* 3 MM)))
           (* -34/100000 E (dsin (- (* 2 MM) M)))
           (* 32/100000 E (dsin (+ M (* 2 F))))
           (* 32/100000 E (dsin (- M (* 2 F))))
           (* -28/100000 E E (dsin (+ MM (* 2 M))))
           (* 27/100000 E (dsin (+ (* 2 MM) M)))
           (* -17/100000 (dsin T))
           (* -5/100000 (dsin (+ MM (neg M) (neg (* 2 F)))))
           (* 4/100000 (dsin (+ (* 2 MM) (* 2 F))))
           (* -4/100000 (dsin (+ MM M (* 2 F))))
           (* 4/100000 (dsin (- MM (* 2 M))))
           (* 3/100000 (dsin (+ MM M (neg (* 2 F)))))
           (* 3/100000 (dsin (* 3 M)))
           (* 2/100000 (dsin (- (* 2 MM) (* 2 F))))
           (* 2/100000 (dsin (+ MM (neg M) (* 2 F))))
           (* -2/100000 (dsin (+ (* 3 MM) M)))))
      (define (adjust-all JDE k T)
        (let ((A1 (deg-in-range
                    (+ 29977/100 
                       (* k 107408/1000000)
                       (neg (* T T 9173/1000000)))))
              (A2 (deg-in-range
                    (+ 25188/100
                       (* k 16321/1000000))))
              (A3 (deg-in-range
                    (+ 25183/100
                       (* k 26651886/1000000))))
              (A4 (deg-in-range
                    (+ 34942/100 
                       (* k 36412478/1000000))))
              (A5 (deg-in-range
                    (+ 8466/100
                       (* k 18206239/1000000))))
              (A6 (deg-in-range
                    (+ 14174/100
                       (* k 53303771/1000000))))
              (A7 (deg-in-range
                    (+ 20714/100
                       (* k 2453732/1000000))))
              (A8 (deg-in-range
                    (+ 15484/100
                       (* k 730686/100000))))
              (A9 (deg-in-range
                    (+ 3452/100
                       (* k 27261239/1000000))))
              (A10 (deg-in-range 
                     (+ 20719/100
                        (* k 121824/1000000))))
              (A11 (deg-in-range
                     (+ 29134/100 
                        (* k 1844379/1000000))))
              (A12 (deg-in-range
                     (+ 16172/100
                        (* k 24198154/1000000))))
              (A13 (deg-in-range
                     (+ 23956/100
                        (* k 25513099/1000000))))
              (A14 (deg-in-range
                     (+ 33155/100
                        (* k 3592518/1000000)))))
          (+ JDE 
             (* 325/1000000 (dsin A1))
             (* 165/1000000 (dsin A2))
             (* 164/1000000 (dsin A3))
             (* 126/1000000 (dsin A4))
             (* 110/1000000 (dsin A5))
             (* 62/1000000 (dsin A6))
             (* 60/1000000 (dsin A7))
             (* 56/1000000 (dsin A8))
             (* 47/1000000 (dsin A9))
             (* 42/1000000 (dsin A10))
             (* 40/1000000 (dsin A11))
             (* 37/1000000 (dsin A12))
             (* 35/1000000 (dsin A13))
             (* 23/1000000 (dsin A14)))))
      ;
      (let* ((k (+ (floor (* (- (date->decimal date) 2000)
                             123685/10000))
                   (case phase
                     ((new-moon) 0.0)
                     ((first-quarter) 0.25)
                     ((full-moon) 0.5)
                     ((last-quarter) 0.75)
                     (else (error "Unknown phase in lunar-phase")))))
             (T (/ k 123685/100))
             (JDE (+ 245155009766/100000
                     (* k 29530588861/1000000000)
                     (* T T 15437/100000000)
                     (neg (* T T T 15/100000000))
                     (* T T T T 73/100000000000)))
             (E (+ 1 (neg (* T 2516/1000000)) (neg (* T T 74/10000000))))
             (sun-anomaly (deg-in-range
                            (+ 25534/10000
                               (* k 291053567/10000000)
                               (neg (* T T 14/10000000))
                               (neg (* T T T 11/100000000)))))
             (moon-anomaly (deg-in-range
                             (+ 2015643/10000
                                (* k 38581693528/100000000)
                                (* T T 107582/10000000)
                                (* T T T 1238/100000000)
                                (neg (* T T T T 58/1000000000)))))
             (moon-arg-lat (deg-in-range
                             (+ 1607108/10000
                                (* k 39067050284/100000000)
                                (neg (* T T 16118/10000000))
                                (neg (* T T T 227/100000000))
                                (* T T T T 11/1000000000))))
             (moon-asc-long (deg-in-range
                              (+ 1247746/10000
                                 (neg (* k 156375588/100000000))
                                 (* T T 20672/10000000)
                                 (* T T T 215/100000000))))
             (W (+ 306/100000
                   (neg (* 38/100000 E (dcos sun-anomaly)))
                   (* 26/100000 (dcos moon-anomaly))
                   (neg (* 2/100000 (dcos (- moon-anomaly sun-anomaly))))
                   (* 2/100000 (dcos (+ moon-anomaly sun-anomaly)))
                   (* 2/10000 (dcos (* 2 moon-asc-long)))))
             (phase-adj-JDE 
               (case phase ; adjust JDE according to phase
                 ((new-moon)
                  (adjust-whole JDE E sun-anomaly moon-anomaly moon-arg-lat moon-asc-long new-moon-constants))
                 ((full-moon)
                  (adjust-whole JDE E sun-anomaly moon-anomaly moon-arg-lat moon-asc-long full-moon-constants))
                 ((first-quarter)
                  (+ W
                     (adjust-quarter JDE E sun-anomaly moon-anomaly moon-arg-lat moon-asc-long)))
                 ((last-quarter)
                  (+ (neg W)
                     (adjust-quarter JDE E sun-anomaly moon-anomaly moon-arg-lat moon-asc-long))))))
        (adjust-all phase-adj-JDE k T)))

    ))

