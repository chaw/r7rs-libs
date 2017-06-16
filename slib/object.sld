;;; "object.scm" Macroless Object System
;;; Author: Wade Humeniuk <humeniuw@cadvision.com>
;;;
;;; This code is in the public domain.

;;;Date:  February 15, 1994

;; Packaged for R7RS Scheme, Peter Lane 2017

;; Object Construction:
;;       0           1          2             3              4
;; #(object-tag get-method make-method! unmake-method! get-all-methods)

(define-library
  (slib object)
  (export make-object
          object?
          make-generic-method
          make-generic-predicate
          make-method!
          make-predicate!
          unmake-method!
          get-method)
  (import (scheme base))

  (begin

    (define object:tag "object")

    ;;; This might be better done using COMLIST:DELETE-IF.
    (define (object:removeq obj alist)
      (if (null? alist)
        alist
        (if (eq? (caar alist) obj)
          (cdr alist)
          (cons (car alist) (object:removeq obj (cdr alist))))))

    (define (get-all-methods obj)
      (if (object? obj)
        ((vector-ref obj 4))
        (error "Cannot get methods on non-object: " obj)))
    ;@
    (define (object? obj)
      (and (vector? obj)
           (eq? object:tag (vector-ref obj 0))))
    ;@
    (define (make-method! obj generic-method method)
      (if (object? obj)
        (if (procedure? method)
          (begin
            ((vector-ref obj 2) generic-method method)
            method)
          (error "Method must be a procedure: " method))
        (error "Cannot make method on non-object: " obj)))
    ;@
    (define (get-method obj generic-method)
      (if (object? obj)
        ((vector-ref obj 1) generic-method)
        (error "Cannot get method on non-object: " obj)))
    ;@
    (define (unmake-method! obj generic-method)
      (if (object? obj)
        ((vector-ref obj 3) generic-method)
        (error "Cannot unmake method on non-object: " obj)))
    ;@
    (define (make-predicate! obj generic-predicate)
      (if (object? obj)
        ((vector-ref obj 2) generic-predicate (lambda (self) #t))
        (error "Cannot make predicate on non-object: " obj)))
    ;@
    (define (make-generic-method . exception-procedure-in)
      (let ((exception-procedure exception-procedure-in))
        (define generic-method
          (lambda (obj . operands)
            (if (object? obj)
              (let ((object-method ((vector-ref obj 1) generic-method)))
                (if object-method
                  (apply object-method (cons obj operands))
                  (error "Method not supported: " obj)))
              (apply exception-procedure (cons obj operands)))))
        ;
        (if (not (null? exception-procedure))
          (if (procedure? (car exception-procedure))
            (set! exception-procedure (car exception-procedure))
            (error "Exception Handler Not Procedure:"))
          (set! exception-procedure
            (lambda (obj . params)
              (error "Operation not supported: " obj))))
        generic-method))

    ;@
    (define (make-generic-predicate)
      (define generic-predicate
        (lambda (obj)
          (if (object? obj)
            (and ((vector-ref obj 1) generic-predicate) #t)
            #f)))
      generic-predicate)
    ;@
    (define (make-object . ancestors)
      (define method-list
        (apply append (map (lambda (obj) (get-all-methods obj)) ancestors)))
      (define (make-method! generic-method method)
        (set! method-list (cons (cons generic-method method) method-list))
        method)
      (define (unmake-method! generic-method)
        (set! method-list (object:removeq generic-method method-list))
        #t)
      (define (all-methods) method-list)
      (define (get-method generic-method)
        (let ((method-def (assq generic-method method-list)))
          (if method-def (cdr method-def) #f)))
      (vector object:tag get-method make-method! unmake-method! all-methods))

    ))

