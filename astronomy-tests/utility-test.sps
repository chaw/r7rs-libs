
(import (scheme base) 
        (astronomy calendar)
        (astronomy utility)
        (srfi 64)
        (robin srfi64-utils))

(test-begin "astronomy-utility")

(test-approx-same 23.4402069 
                  (mean-obliquity-of-ecliptic (time-in-julian-centuries (make-date 16 12 1992)))
                  0.000001)

(test-end)
