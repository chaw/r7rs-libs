(import (scheme base)
        (slib topological-sort)
        (srfi 64))

(test-begin "slib-topological-sort")

(let* ((data '((shirt tie belt)
               (tie jacket)
               (belt jacket)
               (watch)
               (pants shoes belt)
               (undershorts pants shoes)
               (socks shoes)))
       (result (topological-sort data eq?)))
  (test-equal '(socks undershorts pants shoes watch shirt belt tie jacket)
              result))

(test-end)

