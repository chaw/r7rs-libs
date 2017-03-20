(import (scheme base)
        (scheme write)
        (rebottled pregexp))

(display (pregexp-split "[[:digit:]]" "abcd3a32asdf22.  .")) (newline)

