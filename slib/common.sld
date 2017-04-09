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
          slib:error
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
;      (kawa ; TODO including this fails to compile as package does not implement Externalizable
;        (import (only (kawa base) as invoke invoke-static))
;        (begin
;          (define (system str)
;            (let ((runtime (invoke-static java.lang.Runtime 'getRuntime))
;                  (process (invoke runtime 'exec (as String str))))
;              (invoke p 'waitFor)))))
      (larceny
        (import (primitives system)))
      ((library (chibi process))  ; Chibi relies on execvp, which calls commands directly without redirection
       ;; hence chibi cannot use 'system' for redirecting to files
       (import (prefix (chibi process) chibi:))
       (begin
         (define (system cmd)
           (let-values (((e s) (chibi:system cmd)))
                       s))))
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
    (define (open-file filename modes)
      (guard (exc (else #f)) ; return #f on failure
             (case modes
               ((r) (open-input-file filename))
               ((rb) (open-binary-input-file filename))
               ((w) (open-output-file filename))
               ((wb) (open-binary-output-file filename))
               (else (slib:error 'open-file "invalid mode" modes)))))

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

    ;; support for database tables
    (define base-table-implementations (make-parameter '()))
    (define (add-base-table-implementation impl)
      (base-table-implementations (cons impl (base-table-implementations))))

    ))

