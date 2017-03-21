;; Partial implementation - to support SLIB

(define-library 
  (srfi 59)
  (export pathname->vicinity
          in-vicinity)
  (import (scheme base))

  (begin

    (define (pathname->vicinity str)
      (let* ((path (path-directory str))
               (chars (reverse (string->list path))))
          (if (char=? #\. (car chars)) ; Kawa adds a 'dot' to end, so remove it
            (list->string (reverse (cdr chars)))
            path)))

    (define in-vicinity string-append)

    ))

