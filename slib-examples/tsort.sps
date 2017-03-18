(import (scheme base)
        (scheme write)
        (slib topological-sort))

(display
  (topological-sort '((shirt tie belt)
                      (tie jacket)
                      (belt jacket)
                      (watch)
                      (pants shoes belt)
                      (undershorts pants shoes)
                      (socks shoes))
                    eq?))
(newline)

