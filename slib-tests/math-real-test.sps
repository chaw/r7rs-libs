
(import (except (scheme base) abs)
        (scheme complex)
        (slib math-real)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "slib-math-real")

(test-for-error (real-sqrt -2))
(test-for-error (real-expt 2+i 3+i))
(test-for-error (real-expt 2+i 3))
(test-for-error (real-expt 2 3+i))
(test-equal 8 (real-expt 2 3))
(test-for-error (real-sin 3+2i))
(test-for-error (make-rectangular 0+i 2+i))

(test-end)

