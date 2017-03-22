
(define-library 
  (srfi 59)
  (export program-vicinity 
          library-vicinity 
          implementation-vicinity
          user-vicinity 
          home-vicinity 
          in-vicinity 
          sub-vicinity
          make-vicinity 
          pathname->vicinity 
          vicinity:suffix?)
  (import (scheme base)
          (scheme process-context))

  (begin

    (define program-vicinity home-vicinity)

    ;; (This function probably means the location of SLIB)
    ;; If SCHEME_LIBRARY_PATH not defined, return current directory
    (define (library-vicinity)
      (let ((env (get-environment-variable "SCHEME_LIBRARY_PATH")))
        (if (string? env)
          env
          "")))

    ;; Unclear what this should return
    (define (implementation-vicinity)
      "")

    (define (user-vicinity)
      (if (memv 'vms (features))
        "[.]"
        ""))

    (define (home-vicinity)
      (let ((env (get-environment-variable "HOME")))
        (if (string? env)
          env
          ".")))

    (define (in-vicinity . args)
      (apply string-append args))

    (define sub-vicinity
      (let ((*vicinity-suffix* (cond ((memv 'windows (features))
                                      "\\")
                                     ((memv 'macos (features))
                                      ":")
                                     (else ; unix
                                       "/"))))
        (lambda (vic name)
          (string-append vic name *vicinity-suffix*))))

    (define (make-vicinity pathname) pathname)

    (define (pathname->vicinity str)
      (let* ((path (path-directory str))
             (chars (reverse (string->list path))))
        (if (char=? #\. (car chars)) ; Kawa adds a 'dot' to end, so remove it
          (list->string (reverse (cdr chars)))
          path)))

    (define vicinity:suffix?
      (let ((suffi
              (cond ((memv 'macos (features)) 
                     '(#\:))
                    ((or (memv 'ms-dos (features))
                         (memv 'windows (features)))
                     '(#\\ #\/))
                    ((or (memv 'unix (features))
                         (memv 'posix (features)))
                     '(#\/))
                    (else 
                      '(#\/)))))
        (lambda (chr) (and (memv chr suffi) #t))))


    ))

