
(define-library
  (slib common-list-functions)
  (export position)
  (import (scheme base))

  (begin

    ;@
    ;; note, list-index in srfi-1 uses a procedure to test for the object
    ;; this is effectively (list-index (lambda (o) (eqv? o obj)) lst)
    (define (position obj lst)
      (define pos (lambda (n lst)
                    (cond ((null? lst) #f)
                          ((eqv? obj (car lst)) n)
                          (else (pos (+ 1 n) (cdr lst))))))
      (pos 0 lst))

    ))

