;;;
;;; Physical Ephemeris of Jupiter
;;; following Meeus pp. 297-299
;;;

(define-library
  (astrocalc jupiter)
  (export
    jupiter-central-meridian-low-accuracy
    )
  (import (scheme base)
          (scheme inexact)
          (astrocalc calendar)
          (astrocalc utility))

  (begin

    ;; Return the longitudes of central meridian for 
    ;; Jupiter System I and II as values
    (define (jupiter-central-meridian-low-accuracy date)
      (let* ((jde (julian-ephemeris-day date))
             (d (- jde 2451545))
             (V (floor-remainder
                  (+ 17274/100 (* 111588/100000000 d))
                  360))
             (M (floor-remainder 
                  (+ 357529/1000 (* 9856003/10000000 d))
                  360))
             (N (floor-remainder
                  (+ 2002/100 (* 830853/10000000 d) (* 329/1000 (dsin V)))
                  360))
             (J (floor-remainder
                  (+ 66115/1000 (* 9025179/10000000 d) (* -1 329/1000 (dsin V)))
                  360))
             (A (+ (* 1915/1000 (dsin M)) (* 20/1000 (dsin (* 2 M)))))
             (B (+ (* 5555/1000 (dsin N)) (* 168/1000 (dsin (* 2 N)))))
             (K (- (+ J A) B))
             (Re (- 100014/100000 (* 1671/100000 (dcos M)) (* 14/100000 (dcos (* 2 M)))))
             (rj (- 520872/100000 (* 25208/100000 (dcos N)) (* 611/100000 (dcos (* 2 N)))))
             (delta (sqrt (- (+ (* Re Re) (* rj rj)) (* 2 Re rj (dcos K)))))
             (sin-psi (* (/ Re delta) (dsin K)))
             (psi (dasin sin-psi))
             (w1 (floor-remainder
                   (+ 21098/100 (* 8778169088/10000000 (- d (/ delta 173))) psi (* -1 B))
                   360))
             (w2 (floor-remainder
                   (+ 18723/100 (* 8701869088/10000000 (- d (/ delta 173))) psi (* -1 B))
                   360))
             (corr (* 573/10 (dsin (/ psi 2)) (dsin (/ psi 2))))
             (l (+ 3435/100 (* 83091/1000000 d) (* 329/1000 (dsin V)) B))
             (Ds (* 312/100 (dsin (+ l 428/10))))
             (De (- Ds 
                    (* 222/100 sin-psi (dcos (+ l 22))) 
                    (* 13/10 (/ (- rj delta) delta) (dsin (- l 1005/10)))))
             )
        (values w1 w2)))

    ))

