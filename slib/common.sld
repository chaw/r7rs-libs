;; Features from Template, usually defined in init

(define-library
  (slib common)
  (export call-with-open-ports
          char-code-limit
          gentemp
          identity
          make-exchanger
          most-positive-fixnum
          open-file
          output-port-height
          output-port-width
          provided?
          system
          slib:warn
          software-type
          tmpnam
          with-load-pathname
          base-table-implementations
          add-base-table-implementation
          slib:version)
  (import (scheme base)
          (scheme file)
          (scheme write))
  
    ;; Use underlying 'system' implementation, if it exists
    (cond-expand
      (kawa 
        (import (kawa lib system)))
      (larceny
        (import (primitives system)))
      ((library (chibi process))  
       ;; chibi cannot use 'system' directly for redirecting to files (execvp)
       ;; hence calls out to bash  TODO: Clearly this is Linux specific
       (import (prefix (chibi process) chibi:))
       (begin
         (define (system cmd)
           (guard (exc (else 1))
                  (let-values (((e s) (chibi:system "/bin/bash" "-c" cmd)))
                              s)))))
      ((library (sagittarius process))
       ;; to avoid execvp errors, Sagittarius also calls out to bash  TODO: Clearly this is Linux specific
       (import (sagittarius process))
       (begin
         (define (system cmd)
           (run "/bin/bash" "-c" cmd))))
      (else ; else, set system to return an 'unsupported' error
        (begin
          (define (system . args) (error "Implementation does not support 'system' calls")))))

  (begin

    (define (slib:version) "3b5 (ported to R7RS)") ;; return the ported version of SLIB

    ;;@ CHAR-CODE-LIMIT is one greater than the largest integer which can
    ;;; be returned by CHAR->INTEGER.
    (define char-code-limit 256)

    ;;@ (software-type) should be set to the generic operating system type.
    ;;; unix, vms, macos, amiga and ms-dos are supported.
    (define (software-type) 
      (cond ((memv 'windows (features))
             'windows)
            ((memv 'posix (features))
             'posix)
            ((or (memv 'unix (features))
                 (memv 'linux (features)))
             'unix)
            (else
              'unknown)))

    (define gentemp
      (let ((c 100))
        (lambda ()
          (string->symbol (string-append "slib_" (number->string c))))))

    (define (identity x) x)

    (define (make-exchanger obj-in)
      (let ((obj obj-in))
        (lambda (rep) (let ((old obj)) (set! obj rep) old))))

    (define most-positive-fixnum #xFFFFFFFF)

    ;; This replicates SCM's 'open-file' command which returns #f on failure
    ;; -- R7RS throws an exception
    ;; -- rewrote modes so b means binary file, as in C
    (define (open-file filename modes)
      (case modes
        ((r) (if (file-exists? filename)
               (open-input-file filename)
               #f))
        ((rb) (if (file-exists? filename)
                (open-binary-input-file filename)
                #f))
        ((w) (open-output-file filename))
        ((wb) (open-binary-output-file filename))
        (else (error 'open-file "invalid mode" modes))))

    (define (output-port-height . arg) 24) ; value used in all the .init files
    (define (output-port-width . arg) 79) ; value used in all the .init files

    (define (call-with-open-ports . ports-in)
      (let ((proc (car ports-in))
            (ports ports-in))
        (cond ((procedure? proc) (set! ports (cdr ports)))
              (else (set! ports (reverse ports))
                    (set! proc (car ports))
                    (set! ports (reverse (cdr ports)))))
        (let ((ans (apply proc ports)))
          (for-each close-port ports)
          ans)))

    ;; TODO: These features must be varied based on implementation - use cond-expand
    (define (provided? feature)
      (case feature
        ((array) #t) ;; if srfi-63 is available, type is 'array'
        ((bignum) #t)
        ((complex) #t)
        ((inexact) #t)
        ((object-hash) #f)
        ((real) #t)  ;; ?? same as inexact?
        (else
          (error "unknown feature " feature))))

    ;@
    (define slib:warn
      (lambda args
        (let ((cep (current-error-port)))
          (display "Warn: " cep)
          (for-each (lambda (x) (display #\space cep) (write x cep)) args)
          (newline cep))))

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

    ;; support for database tables
    (define base-table-implementations (make-parameter '()))
    (define (add-base-table-implementation impl)
      (base-table-implementations (cons impl (base-table-implementations))))

    ))

