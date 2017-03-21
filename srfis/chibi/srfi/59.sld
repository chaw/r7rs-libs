;; Partial implementation - to support SLIB

(define-library 
  (srfi 59)
  (export pathname->vicinity
          in-vicinity)
  (import (scheme base)
          (chibi pathname))

  (begin

    (define (pathname->vicinity str)
      (string-append (path-directory str)
                     "/"))

    (define in-vicinity string-append)

    ))
