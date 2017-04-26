;; Calculations about the Moon
;; Peter Lane, 2017

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
             '(-0.40720 0.17241 0.01608 0.01039 0.00739 -0.00514 0.00208))
      (define full-moon-constants
             '(-0.40614 0.17302 0.01614 0.01043 0.00734 -0.00515 0.00209))
      (define (adjust-whole JDE E M MM F T constants)
        (+ JDE
           (* (list-ref constants 0) (dsin MM))
           (* (list-ref constants 1) E (dsin M))
           (* (list-ref constants 2) (dsin (* 2 MM)))
           (* (list-ref constants 3) (dsin (* 2 F)))
           (* (list-ref constants 4) E (dsin (- MM M)))
           (* (list-ref constants 5) E (dsin (+ MM M)))
           (* (list-ref constants 6) E E (dsin (* 2 M)))
           (* -0.00111 (dsin (- MM (* 2 F))))
           (* -0.00057 (dsin (+ MM (* 2 F))))
           (* 0.00056 E (dsin (+ (* 2 MM) M)))
           (* -0.00042 (dsin (* 3 MM)))
           (* 0.00042 E (dsin (+ M (* 2 F))))
           (* 0.00038 E (dsin (- M (* 2 F))))
           (* -0.00024 E (dsin (- (* 2 MM) M)))
           (* -0.00017 (dsin T))
           (* -0.00007 (dsin (+ MM (* 2 M))))
           (* 0.00004 (dsin (- (* 2 MM) (* 2 F))))
           (* 0.00004 (dsin (* 3 M)))
           (* 0.00003 (dsin (+ MM M (- (* 2 F)))))
           (* 0.00003 (dsin (+ (* 2 MM) (* 2 F))))
           (* -0.00003 (dsin (+ MM M (* 2 F))))
           (* 0.00003 (dsin (+ MM (- M) (* 2 F))))
           (* -0.00002 (dsin (+ MM (- M) (- (* 2 F)))))
           (* -0.00002 (dsin (+ (* 3 MM) M)))
           (* 0.00002 (dsin (* 4 MM)))))
      (define (adjust-quarter JDE E M MM F T)
        (+ JDE
           (* -0.62801 (dsin MM))
           (* 0.17172 E (dsin M))
           (* -0.01183 E (dsin (+ MM M)))
           (* 0.00862 (dsin (* 2 MM)))
           (* 0.00804 (dsin (* 2 F)))
           (* 0.00454 E (dsin (- MM M)))
           (* 0.00204 E E (dsin (* 2 M)))
           (* -0.00180 (dsin (- MM (* 2 F))))
           (* -0.00070 (dsin (+ MM (* 2 F))))
           (* -0.00040 (dsin (* 3 MM)))
           (* -0.00034 E (dsin (- (* 2 MM) M)))
           (* 0.00032 E (dsin (+ M (* 2 F))))
           (* 0.00032 E (dsin (- M (* 2 F))))
           (* -0.00028 E E (dsin (+ MM (* 2 M))))
           (* 0.00027 E (dsin (+ (* 2 MM) M)))
           (* -0.00017 (dsin T))
           (* -0.00005 (dsin (+ MM (- M) (- (* 2 F)))))
           (* 0.00004 (dsin (+ (* 2 MM) (* 2 F))))
           (* -0.00004 (dsin (+ MM M (* 2 F))))
           (* 0.00004 (dsin (- MM (* 2 M))))
           (* 0.00003 (dsin (+ MM M (- (* 2 F)))))
           (* 0.00003 (dsin (* 3 M)))
           (* 0.00002 (dsin (- (* 2 MM) (* 2 F))))
           (* 0.00002 (dsin (+ MM (- M) (* 2 F))))
           (* -0.00002 (dsin (+ (* 3 MM) M)))))
      (define (adjust-all JDE k T)
        (let ((A1 (deg-in-range
                    (+ 299.77 
                       (* k 0.107408)
                       (* T T -0.009173))))
              (A2 (deg-in-range
                    (+ 251.88
                       (* k 0.016321))))
              (A3 (deg-in-range
                    (+ 251.83
                       (* k 26.651886))))
              (A4 (deg-in-range
                    (+ 349.42 
                       (* k 36.412478))))
              (A5 (deg-in-range
                    (+ 84.66
                       (* k 18.206239))))
              (A6 (deg-in-range
                    (+ 141.74
                       (* k 53.303771))))
              (A7 (deg-in-range
                    (+ 207.14
                       (* k 2.453732))))
              (A8 (deg-in-range
                    (+ 154.84
                       (* k 7.30686))))
              (A9 (deg-in-range
                    (+ 34.52
                       (* k 27.261239))))
              (A10 (deg-in-range 
                     (+ 207.19
                        (* k 0.121824))))
              (A11 (deg-in-range
                     (+ 291.34 
                        (* k 1.844379))))
              (A12 (deg-in-range
                     (+ 161.72
                        (* k 24.198154))))
              (A13 (deg-in-range
                     (+ 239.56
                        (* k 25.513099))))
              (A14 (deg-in-range
                     (+ 331.55
                        (* k 3.592518)))))
          (+ JDE 
             (* 0.000325 (dsin A1))
             (* 0.000165 (dsin A2))
             (* 0.000164 (dsin A3))
             (* 0.000126 (dsin A4))
             (* 0.000110 (dsin A5))
             (* 0.000062 (dsin A6))
             (* 0.000060 (dsin A7))
             (* 0.000056 (dsin A8))
             (* 0.000047 (dsin A9))
             (* 0.000042 (dsin A10))
             (* 0.000040 (dsin A11))
             (* 0.000037 (dsin A12))
             (* 0.000035 (dsin A13))
             (* 0.000023 (dsin A14)))))
      ;
      (let* ((k (+ (floor (* (- (date->decimal date) 2000)
                             12.3685))
                   (case phase
                     ((new-moon) 0.0)
                     ((first-quarter) 0.25)
                     ((full-moon) 0.5)
                     ((last-quarter) 0.75)
                     (else (error "Unknown phase in lunar-phase")))))
             (T (/ k 1236.85))
             (JDE (+ 2451550.09766
                     (* k 29.530588861)
                     (* T T 0.00015437)
                     (* T T T -0.00000015)
                     (* T T T T 0.00000000073)))
             (E (+ 1 (* T -0.002516) (* T T -0.0000074)))
             (sun-anomaly (deg-in-range
                            (+ 2.5534
                               (* k 29.1053567)
                               (* T T -0.0000014)
                               (* T T T -0.00000011))))
             (moon-anomaly (deg-in-range
                             (+ 201.5643
                                (* k 385.81693528)
                                (* T T 0.0107582)
                                (* T T T 0.00001238)
                                (* T T T T -0.000000058))))
             (moon-arg-lat (deg-in-range
                             (+ 160.7108
                                (* k 390.67050284)
                                (* T T -0.0016118)
                                (* T T T -0.00000227)
                                (* T T T T 0.000000011))))
             (moon-asc-long (deg-in-range
                              (+ 124.7746
                                 (* k -1.56375588)
                                 (* T T 0.0020672)
                                 (* T T T 0.00000215))))
             (W (+ 0.00306
                   (* -0.00038 E (dcos sun-anomaly))
                   (* 0.00026 (dcos moon-anomaly))
                   (* -0.00002 (dcos (- moon-anomaly sun-anomaly)))
                   (* 0.00002 (dcos (+ moon-anomaly sun-anomaly)))
                   (* 0.00002 (dcos (* 2 moon-asc-long)))))
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
                  (+ (- W)
                     (adjust-quarter JDE E sun-anomaly moon-anomaly moon-arg-lat moon-asc-long)))
                 (else
                   (error "Unknown phase in lunar-phase")))))
        (adjust-all phase-adj-JDE k T)))

    ))

