
(import (except (scheme base) abs)
        (scheme complex)
        (slib math-real)
        (srfi 64))

(test-begin "slib-math-real")

(test-error (real-sqrt -2))
(test-error (real-expt 2+i 3+i))
(test-error (real-expt 2+i 3))
(test-error (real-expt 2 3+i))
(test-equal 8 (real-expt 2 3))
(test-error (real-sin 3+2i))
(test-error (make-rectangular 0+i 2+i))

(test-end)

