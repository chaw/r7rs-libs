
(define-library
  (slib common-list-functions)
  (export position)
  (import (scheme base))

  (begin

    ;@
    (define (position obj lst)
      (define pos (lambda (n lst)
                    (cond ((null? lst) #f)
                          ((eqv? obj (car lst)) n)
                          (else (pos (+ 1 n) (cdr lst))))))
      (pos 0 lst))

    ))

