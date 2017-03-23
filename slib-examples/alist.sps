(import (scheme base)
        (scheme char)
        (scheme write)
        (slib alist))

(define put (alist-associator string-ci=?))
(define alist '())
(set! alist (put alist "Adam" 9))
(set! alist (put alist "Ben" 19))
(set! alist (put alist "Charles" 19))

(display alist) (newline)
(alist-for-each (lambda (k v)
                  (display (string-append k
                                          " => "
                                          (number->string v)
                                          "\n")))
                alist)

(display "---------------\n")

(define rem (alist-remover string-ci=?))
(set! alist (rem alist "ben"))

(alist-for-each (lambda (k v)
                  (display (string-append k
                                          " => "
                                          (number->string v)
                                          "\n")))
                alist)


