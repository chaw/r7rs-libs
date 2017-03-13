; "dynamic.scm", DYNAMIC data type for Scheme
; Copyright 1992 Andrew Wilcox.
;
; You may freely copy, redistribute and modify this package.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib dynamic)
  (export make-dynamic
          dynamic?
          dynamic-ref
          dynamic-set!
          call-with-dynamic-binding)
  (import (scheme base)
          (slib common))

  (begin

    ;; maintains a global dynamic environment

    (define-record-type <dynamic-environment>
                        (make-dynamic-environment dynamic value parent)
                        dynamic-environment?
                        (dynamic dynamic-environment:dynamic)
                        (value dynamic-environment:value dynamic-environment:set-value!)
                        (parent dynamic-environment:parent))

    (define *current-dynamic-environment* #f)

    (define (extend-current-dynamic-environment dynamic obj) ; adds old env as parent
      (set! *current-dynamic-environment*
        (make-dynamic-environment dynamic obj
                                  *current-dynamic-environment*)))

    (define-record-type <dynamic>
                        (new-dynamic-entry )
                        dynamic?)

    ;@
    (define (make-dynamic obj)
      (let ((dynamic (new-dynamic-entry)))
        (extend-current-dynamic-environment dynamic obj)
        dynamic))

    ;@
    (define (dynamic-ref dynamic)
      (guarantee-dynamic dynamic)
      (let loop ((env *current-dynamic-environment*))
        (cond ((not env)
               (slib:error dynamic:errmsg dynamic))
              ((eq? (dynamic-environment:dynamic env) dynamic)
               (dynamic-environment:value env))
              (else
                (loop (dynamic-environment:parent env))))))
    ;@
    (define (dynamic-set! dynamic obj)
      (guarantee-dynamic dynamic)
      (let loop ((env *current-dynamic-environment*))
        (cond ((not env)
               (slib:error dynamic:errmsg dynamic))
              ((eq? (dynamic-environment:dynamic env) dynamic)
               (dynamic-environment:set-value! env obj))
              (else
                (loop (dynamic-environment:parent env))))))
    ;@
    (define (call-with-dynamic-binding dynamic obj thunk)
      (let ((out-thunk-env #f)
            (in-thunk-env (make-dynamic-environment
                            dynamic obj
                            *current-dynamic-environment*)))
        (dynamic-wind (lambda ()
                        (set! out-thunk-env *current-dynamic-environment*)
                        (set! *current-dynamic-environment* in-thunk-env))
                      thunk
                      (lambda ()
                        (set! in-thunk-env *current-dynamic-environment*)
                        (set! *current-dynamic-environment* out-thunk-env)))))

    (define (guarantee-dynamic dynamic)
      (or (dynamic? dynamic)
          (slib:error "Not a dynamic" dynamic)))

    (define dynamic:errmsg
      "No value defined for this dynamic in the current dynamic environment")

    ))

