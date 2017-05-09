;; Files from SRFI 132 collected and simplified somewhat

(define-library
  (srfi 132)
  (export list-sorted?               vector-sorted?
          list-sort                  vector-sort
          list-stable-sort           vector-stable-sort
          list-sort!                 vector-sort!
          list-stable-sort!          vector-stable-sort!
          list-merge                 vector-merge
          list-merge!                vector-merge!
          list-delete-neighbor-dups  vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median         vector-find-median!
          vector-select!             vector-separate!
          )

  (import (scheme base)
          (scheme cxr)
          (only (srfi 27) random-integer))

  (begin

    (define (assert x)
      (when (not x)
        (error "assertion failure"))))

    (include "sort/delndups.scm")     ; list-delete-neighbor-dups etc
    (include "sort/lmsort.scm")       ; list-merge, list-merge!
    (include "sort/sortp.scm")        ; list-sorted?, vector-sorted?
    (include "sort/vector-util.scm")
    (include "sort/vhsort.scm")
    (include "sort/vmsort.scm")       ; vector-merge, vector-merge!
    (include "sort/vqsort2.scm")
    (include "sort/sort.scm")

    (include "sort/select.scm")

    )

