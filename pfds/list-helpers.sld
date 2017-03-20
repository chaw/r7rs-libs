;;; List helpers - extracted from fingertrees.sld

(define-library
  (pfds list-helpers)
  (export snoc
          take
          last
          but-last
          map-reverse
          fold-left
          fold-right)
  (import (scheme base))

  (begin

    (define (snoc l val)
      (append l (list val)))

    (define (take l n)
      (if (or (null? l) (zero? n))
        '()
        (cons (car l)
              (take (cdr l) (- n 1)))))

    (define (last list)
      (if (null? (cdr list))
        (car list)
        (last (cdr list))))

    (define (but-last list)
      (if (null? (cdr list))
        '()
        (cons (car list)
              (but-last (cdr list)))))

    (define (map-reverse f l)
      (fold-left (lambda (o n) (cons (f n) o)) '() l))

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

    ))

