;; test only for Kawa

(import (scheme base)
        (scheme write)
        (srfi 27))

(define r (make-random-source))
(random-source-pseudo-randomize! r 1 100)
(define s (random-source-make-integers r))
(do ((i 0 (+ 1 i)))
  ((= i 10) )
  (format #t "Seed ~a Value ~a~&"
          (random-source-state-ref r)
          (s 10)))
          
(newline) (newline)
(random-source-state-set! r 205769375804441)
(do ((i 0 (+ 1 i)))
  ((= i 10) )
  (format #t "Seed ~a Value ~a~&"
          (random-source-state-ref r)
          (s 10)))
          
(random-source-randomize! default-random-source)
(do ((i 0 (+ 1 i)))
  ((= i 10) )
  (format #t "~a -> ~a~&"
          i (random-integer 10000000000000000000000)))
