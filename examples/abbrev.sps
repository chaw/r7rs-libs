(import (scheme base)
        (scheme write)
        (robin abbrev))

(display (abbrev '("car" "cone")))
(newline)
(display (abbrev '("ruby" "rules")))
(newline)

;; show use of prefix
(display (abbrev '("car" "cat" "candy" "cone") "ca"))
(newline)

;; case matters
(display (abbrev '("Car" "cat" "candy" "COne") "ca"))
(newline)
