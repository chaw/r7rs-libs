
(import (scheme base)
        (slib format)
        (slib hilbert-fill)
        (slib peano-fill))

(define (show-peano)
  ; generates points in Figure 11: Peano
  ; (0 0) in bottom left, positive coordinates
  (do ((i 0 (+ 1 i)))
    ((= i 81) )
    (format #t "~a~&" (natural->peano-coordinates i 2)))

  ;; position of (2 2) in above list
  (format #t "~a~&" (peano-coordinates->natural '(2 2)))

  ; generates points with centre (0 0) and negative coordinates
  (do ((i 0 (+ 1 i)))
    ((= i 81) )
    (format #t "~a~&" (integer->peano-coordinates i 2)))

  ;; position of (2 2) in above list
  (format #t "~a~&" (peano-coordinates->integer '(2 2))))


;; Hilbert space filling curve fills an arbitrarily large n-dimensional cube
;; from left-corner as origin, coordinates non-negative
(do ((i 0 (+ 1 i)))
  ((= i 27) )
  (format #t "~a~&" (integer->hilbert-coordinates i 3)))
