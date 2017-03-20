
(define-library
  (slib common-list-functions)
  (export fold-left
          fold-right
          position)
  (import (scheme base))

  (begin

    ;; note, R6RS fold-left has different order of args in f to SRFI 1
    (define (fold-left f i l)
      (if (null? l)
        i
        (fold-left f (f i (car l)) (cdr l))))

    (define (fold-right f i l)
      (define (iter rest)
        (if (null? rest)
          i
          (f (car rest)
             (iter (cdr rest)))))
      (iter l))

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

