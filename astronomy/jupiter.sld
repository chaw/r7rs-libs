;;;
;;; Physical Ephemeris of Jupiter
;;; following Meeus pp. 297-299
;;;

;; Peter Lane, 2017

(define-library
  (astronomy jupiter)
  (export
    jupiter-data
    jupiter-data-low-accuracy
    )
  (import (scheme base)
          (scheme inexact)
          (astronomy calendar)
          (astronomy planet-positions)
          (astronomy utility)
          (robin statistics))

  (begin

    ;; TODO: NEEDS CHECKING
    ;; Compute some physical ephemeris of Jupiter
    ;; Input: a date
    ;; Output: the longitudes of central meridian for Jupiter System I and II as values
    ;;         the planetocentric declinations of the Earth and Sun, respectively
    ;;         the position angle of Jupiter's northern rotation pole
    (define (jupiter-data date)
      (let* ((d (- (julian-ephemeris-day date) 2433282.5))
             (T1 (/ d 36525))
             (a0 (+ 268.00 (* 0.1061 T1)))
             (d0 (- 64.50 (* 0.0164 T1)))
             (W1 (deg-in-range
                   (+ 17.710 (* 877.90003539 d))))
             (W2 (deg-in-range
                   (+ 16.838 (* 870.27003539 d))))
             (l0 (heliocentric-longitude-earth date))
             (b0 (heliocentric-latitude-earth date))
             (r0 (heliocentric-radius-earth date))
             (l-1 (heliocentric-longitude-jupiter date))
             (b (heliocentric-latitude-jupiter date))
             (r (heliocentric-radius-jupiter date))
             (x-1 (- (* r (dcos b) (dcos l-1))
                    (* r0 (dcos l0))))
             (y-1 (- (* r (dcos b) (dsin l-1))
                     (* r0 (dsin l0))))
             (z-1 (- (* r (dsin b))
                     (* r0 (dsin b0))))
             (delta-1 (sqrt (+ (square x-1) (square y-1) (square z-1))))
             (l (- l-1 (/ (* 0.012990 delta-1) (square r))))
             (x (- (* r (dcos b) (dcos l))
                    (* r0 (dcos l0))))
             (y (- (* r (dcos b) (dsin l))
                     (* r0 (dsin l0))))
             (z (- (* r (dsin b))
                     (* r0 (dsin b0))))
             (delta (sqrt (+ (square x) (square y) (square z))))
             (e0 (mean-obliquity-of-ecliptic (time-in-julian-centuries date)))
             (as (datan  (- (* (dcos e0) (dsin l)) (* (dsin e0) (dtan b)))  
                         (dcos l))) ; converted to 2 arg form
             (ds (dasin (+ (* (dcos e0) (dsin b))
                           (* (dsin e0) (dcos b) (dsin l)))))
             (Dsun (dasin (+ (- (* (dsin d0) (dsin ds))) (- (* (dcos d0) (dcos ds) (dcos (- a0 as)))))))
             (u (- (* y (dcos e0)) (* z (dsin e0))))
             (v (+ (* y (dsin e0)) (* z (dcos e0))))
             (alpha (datan u x)) ; converted to 2 arg form
             (del (datan v (sqrt (+ (square x) (square u))))) ; converted to 2 arg form
             (zeta (datan (- (* (dsin d0) (dcos del) (dcos (- a0 alpha))) (* (dsin del) (dcos d0)))
                          (* (dcos del) (dsin (- a0 alpha))))) ; converted to 2 arg form
             (Dearth (dasin (+ (- (* (dsin d0) (dsin del))) (- (* (dcos d0) (dcos del) (dcos (- a0 alpha)))))))
             (w1 (deg-in-range (+ W1 (- zeta) (* -5.07033 delta))))
             (w2 (deg-in-range (+ W2 (- zeta) (* -5.02626 delta))))
             (corr (* (sign (dsin (- l l0))) 
                      57.2958
                      (/ (+ (* 2 r delta) (square r0) (- (square r)) (- (square delta)))
                         (* 4 r delta))))
             (P (datan (* (dcos d0) (dsin (- a0 alpha))); converted to 2 arg form
                       (- (* (dsin d0) (dcos del)) (* (dcos d0) (dsin del) (dcos (- a0 alpha)))))))
        (values (+ w1 corr) (+ w2 corr) Dearth Dsun P)))

    ;; Low accuracy method to compute some physical ephemeris of Jupiter
    ;; Input: a date
    ;; Output: the longitudes of central meridian for Jupiter System I and II as values
    ;;         the planetocentric declinations of the Earth and Sun, respectively
    ;; -- w1 w2 out by around 0.2 of true value
    (define (jupiter-data-low-accuracy date)
      (let* ((jde (julian-ephemeris-day date))
             (d (- jde 2451545))
             (V (deg-in-range
                  (+ 172.74 (* 0.00111588 d))))
             (M (deg-in-range 
                  (+ 357.529 (* 0.9856003 d))))
             (N (deg-in-range
                  (+ 20.02 (* 0.0830853 d) (* 0.329 (dsin V)))))
             (J (deg-in-range
                  (+ 66.115 (* 0.9025179 d) (* -1 0.329 (dsin V)))))
             (A (+ (* 1.915 (dsin M)) (* 0.020 (dsin (* 2 M)))))
             (B (+ (* 5.555 (dsin N)) (* 0.168 (dsin (* 2 N)))))
             (K (- (+ J A) B))
             (Re (+ 1.00014 (* -0.01671 (dcos M)) (* -0.00014 (dcos (* 2 M)))))
             (rj (+ 5.20872 (* -0.25208 (dcos N)) (* -0.00611 (dcos (* 2 N)))))
             (delta (sqrt (+ (* Re Re) (* rj rj) (* -2 Re rj (dcos K)))))
             (sin-psi (* (/ Re delta) (dsin K)))
             (psi (dasin sin-psi))
             (w1 (deg-in-range
                   (+ 210.98 (* 877.8169088 (- d (/ delta 173))) psi (- B))))
             (w2 (deg-in-range
                   (+ 187.23 (* 870.1869088 (- d (/ delta 173))) psi (- B))))
             (hl (+ 34.35 
                    (* 0.083091 d)
                    (* 0.329 (dsin V))
                    B))
             (Ds (* 3.12 (dsin (+ hl 42.8))))
             (De (+ Ds 
                    (* -2.22 sin-psi (dcos (+ hl 22)))
                    (* -1.30 (/ (- rj delta) delta) (dsin (- hl 100.5))))))
        (values w1 w2 De Ds)))

    ))

