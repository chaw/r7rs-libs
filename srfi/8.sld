;;; SRFI 8 syntax for receiving multiple values

(define-library
  (srfi 8 )
  (export receive)
  (import (scheme base))

  (begin

    (define-syntax receive
      (syntax-rules ()
                    ((receive formals expression body ...)
                     (call-with-values (lambda () expression)
                                       (lambda formals body ...)))))

    ))

