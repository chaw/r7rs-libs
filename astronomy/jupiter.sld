;;;
;;; Physical Ephemeris of Jupiter
;;; following Meeus pp. 297-299
;;;

;; Peter Lane, 2017

(define-library
  (astronomy jupiter)
  (export
    jupiter-central-meridian-low-accuracy
    )
  (import (scheme base)
          (scheme inexact)
          (astronomy calendar)
          (astronomy earth)
          (astronomy utility)
          (robin statistics))

  (begin

    ;; Return the longitudes of central meridian for 
    ;; Jupiter System I and II as values
    ;; -- values out by around 0.2 of true value
    (define (jupiter-central-meridian-low-accuracy date)
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
                   (+ 187.23 (* 870.1869088 (- d (/ delta 173))) psi (- B)))))
        (values w1 w2)))

    ))

