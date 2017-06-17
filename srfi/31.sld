;; Reference implementation

(define-library
  (srfi 31)
  (export rec)
  (import (scheme base))

  (begin 

    (define-syntax rec
      (syntax-rules ()
                    ((rec (NAME . VARIABLES) . BODY)
                     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
                    ((rec NAME EXPRESSION)
                     (letrec ( (NAME EXPRESSION) ) NAME))))))


