;; As most of library uses srfi 95 sorting, adapt Sagittarius' srfi 132

(define-library
  (srfi 95) ; limited only
  (export sort 
          sort!)
  (import (scheme base)
          (srfi 132))

  (begin
    (define (sort lst proc) (list-sort proc lst))
    (define (sort! lst proc) (list-sort! proc lst))

    ))

