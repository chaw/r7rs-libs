;; Features from Template, usually defined in init

(define-library
  (slib common)
  (export call-with-open-ports
          char-code-limit
          current-directory
          gentemp
          identity
          make-exchanger
          most-positive-fixnum
          output-port-height
          output-port-width
          provided?
          slib:error
          slib:warn
          software-type
          tmpnam
          with-load-pathname)
  (import (scheme base)
          (scheme write))

  (begin

    ;;@ CHAR-CODE-LIMIT is one greater than the largest integer which can
    ;;; be returned by CHAR->INTEGER.
    (define char-code-limit 256)

    ;;@ (software-type) should be set to the generic operating system type.
    ;;; unix, vms, macos, amiga and ms-dos are supported.
    (define (software-type) 'unix)

    (define (current-directory) ".") ;; TODO: port dirs.scm ??

    (define gentemp
      (let ((c 100))
        (lambda ()
          (string->symbol (string-append "slib_" (number->string c))))))

    (define (identity x) x)

    (define (make-exchanger obj)
      (lambda (rep) (let ((old obj)) (set! obj rep) old)))

    ;; TODO: use cond-expand to make this implementation specific?
    (define most-positive-fixnum #xFFFFFFFF)

    (define (output-port-height . arg) 24) ; value used in all the .init files
    (define (output-port-width . arg) 79) ; value used in all the .init files

    (define (call-with-open-ports . ports)
      (define proc (car ports))
      (cond ((procedure? proc) (set! ports (cdr ports)))
            (else (set! ports (reverse ports))
                  (set! proc (car ports))
                  (set! ports (reverse (cdr ports)))))
      (let ((ans (apply proc ports)))
        (for-each close-port ports)
        ans))

    (define (provided? feature)
      (case feature
        ((array) #t)
        ((inexact) #t)
        (else
          (error "unknown feature " feature))))

    ;@
    (define slib:warn
      (lambda args
        (let ((cep (current-error-port)))
          (display "Warn: " cep)
          (for-each (lambda (x) (display #\space cep) (write x cep)) args)
          (newline cep))))

    ;;@ define an error procedure for the library
    (define slib:error
      (let ((error error))
        (lambda args
          (apply error args))))

    (define tmpnam (let ((cntr 100))
                     (lambda () (set! cntr (+ 1 cntr))
                       (string-append "slib_" (number->string cntr)))))

    (define *load-pathname* #f)

    (define with-load-pathname
      (let ((exchange
              (lambda (new)
                (let ((old *load-pathname*))
                  (set! *load-pathname* new)
                  old))))
        (lambda (path thunk)
          (let ((old #f))
            (dynamic-wind
              (lambda () (set! old (exchange path)))
              thunk
              (lambda () (exchange old)))))))

    ))

