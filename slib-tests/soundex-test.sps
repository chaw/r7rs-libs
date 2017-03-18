
(import (scheme base)
        (slib soundex)
        (srfi 64))

(test-begin "slib-soundex")

(for-each (lambda (str target)
            (test-equal (soundex str) target))
          '("Euler" "Gauss" "Hilbert" "Knuth"
                                 "Lloyd" "Lukasiewicz")
          '("E460" "G200" "H416" "K530" "L300" "L222"))

(test-end)

