(import (scheme base)
        (scheme write)
        (autodiff stochastic-scheme))

(write (distribution (draw '((1 . 0.25) (2 . 0.25) (3 . 0.25)))))
(newline)
(write
  (probability (distribution (draw '((#t . 0.25) (#f . 0.25) (a . 0.25))))))
(newline)


