;"pp.scm" Pretty-Print

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib pretty-print)
  (export pretty-print
          pretty-print->string)
  (import (scheme base)
          (scheme write)
          (slib common)
          (slib generic-write))

  (begin

    ;@
    (define (pretty-print obj . opt)
      (let ((port (if (pair? opt) (car opt) (current-output-port))))
        (generic-write obj #f (output-port-width port)
                       (lambda (s) (display s port) #t))))

    ;@
    (define (pretty-print->string obj . width)
      (define result '())
      (generic-write obj #f (if (null? width) (output-port-width) (car width))
                     (lambda (str) (set! result (cons str result)) #t))
      (reverse-string-append result))

    ))
