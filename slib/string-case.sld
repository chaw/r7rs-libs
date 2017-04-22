;;; "strcase.scm" String casing functions.
; Written 1992 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;
; This code is in the public domain.

; Modified by Aubrey Jaffer Nov 1992.
; SYMBOL-APPEND and StudlyCapsExpand added by A. Jaffer 2001.
; Authors of the original version were Ken Dickey and Aubrey Jaffer.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;; Changes to original:
;; 1. (scheme char) contains string-upcase / string-downcase
;; 2. SRFI 13 contains string-titlecase, equivalent to string-capitalize
;;    so use if present to define string-capitalize

(define-library
  (slib string-case)
  (export string-capitalize
          string-capitalize!
          string-ci->symbol
          symbol-append
          StudlyCapsExpand)
  (import (scheme base)
          (scheme char)
          (slib common))

  (cond-expand 
    ((library (srfi 13))
     (import (only (srfi 13) string-titlecase string-titlecase!))
     (begin
       (define string-capitalize string-titlecase)
       (define string-capitalize! string-titlecase!)))
    (else
      (begin
        ;@
        (define (string-capitalize! str)	; "hello" -> "Hello"
          (let ((non-first-alpha #f)		; "hELLO" -> "Hello"
                (str-len (string-length str)))	; "*hello" -> "*Hello"
            (do ((i 0 (+ i 1)))			; "hello you" -> "Hello You"
              ((= i str-len) str)
              (let ((c (string-ref str i)))
                (if (char-alphabetic? c)
                  (if non-first-alpha
                    (string-set! str i (char-downcase c))
                    (begin
                      (set! non-first-alpha #t)
                      (string-set! str i (char-upcase c))))
                  (set! non-first-alpha #f))))))
        ;@
        (define (string-capitalize str)
          (string-capitalize! (string-copy str))))))

  (begin

    ;@
    (define string-ci->symbol
      (let ((s2cis (if (equal? "x" (symbol->string 'x))
                     string-downcase string-upcase)))
        (lambda (str) (string->symbol (s2cis str)))))
    ;@
    (define symbol-append
      (let ((s2cis (cond ((equal? "x" (symbol->string 'X)) string-downcase)
                         ((equal? "X" (symbol->string 'x)) string-upcase)
                         (else identity))))
        (lambda args
          (string->symbol
            (apply string-append
                   (map
                     (lambda (obj)
                       (cond ((string? obj) (s2cis obj))
                             ((number? obj) (s2cis (number->string obj)))
                             ((symbol? obj) (symbol->string obj))
                             ((not obj) "")
                             (else (error 'wrong-type-to 'symbol-append obj))))
                     args))))))
    ;@
    (define (StudlyCapsExpand nstr . delimitr-in)
      (let ((delimitr
              (cond ((null? delimitr-in) "-")
                    ((char? (car delimitr-in)) (string (car delimitr-in)))
                    (else (car delimitr-in)))))
        (do ((idx (+ -1 (string-length nstr)) (+ -1 idx)))
          ((> 1 idx) nstr)
          (cond ((and (> idx 1)
                      (char-upper-case? (string-ref nstr (+ -1 idx)))
                      (char-lower-case? (string-ref nstr idx)))
                 (set! nstr
                   (string-append (string-copy nstr 0 (+ -1 idx))
                                  delimitr
                                  (string-copy nstr (+ -1 idx)
                                             (string-length nstr)))))
                ((and (char-lower-case? (string-ref nstr (+ -1 idx)))
                      (char-upper-case? (string-ref nstr idx)))
                 (set! nstr
                   (string-append (string-copy nstr 0 idx)
                                  delimitr
                                  (string-copy nstr idx
                                             (string-length nstr)))))))))

    ))

