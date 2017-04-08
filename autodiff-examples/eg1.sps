(import (scheme base) 
        (scheme write)
        (autodiff nondeterministic-scheme))

(write (domain (a-member-of '(1 2 3))))
(newline)
