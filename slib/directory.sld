;;; "dirs.scm" Directories.
; Copyright 1998, 2002 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;; -- adapted for R7RS implementations
;;    1. Chibi Scheme
;;    2. Kawa Scheme

(define-library
  (slib directory)
  (export current-directory
          make-directory
          directory-for-each
          directory*-for-each)
  (import (scheme base) (scheme write)
          (scheme case-lambda)
          (slib common)
          (slib filename))

  ;; functions must be defined in platform specific ways
  (cond-expand 
    ((library (chibi filesystem))
     (import (chibi filesystem))
     (begin
       ; current-directory exported
       (define make-directory create-directory*)
       (define list-directory-files directory-files)))
    (kawa
      (begin
        (define current-directory current-path)
        (define make-directory create-directory)
        (define (list-directory-files dir)
          (map (lambda (file) (invoke file 'toString)) 
               (invoke (java.io.File dir) 'listFiles)))))
    (else
      (error "directory not supported for current R7RS Scheme implementation")))

  (cond-expand
    ((library (chibi pathname))
     (import (chibi pathname))
     (begin
       (define pathname->vicinity path-directory)))
    (kawa
      (define (pathname->vicinity str)
        (let* ((path (path-directory str))
               (chars (reverse (string->list path))))
          (if (char=? #\. (car chars)) ; Kawa adds a 'dot' to end, so remove it
            (list->string (reverse (cdr chars)))
            path))))
    (else
      (error "directory not supported for current R7RS Scheme implementation")))

  (begin

    (define directory-for-each
      (case-lambda 
        ((proc dir) (directory-for-each proc dir identity))
        ((proc dir given-selector)
         (let ((selector (cond ((null? given-selector) identity)
                               ((procedure? given-selector) given-selector)
                               ((string? given-selector) (filename:match?? given-selector))
                               (else (error "Invalid selector for directory-for-each")))))
           (for-each (lambda (filename) (when (selector filename) (proc filename)))
                     (list-directory-files dir))))))

    (define (directory*-for-each proc path-glob)
      (let* ((dir (pathname->vicinity path-glob))
             (glob (string-copy path-glob (+ 1 (string-length dir)))))
        (directory-for-each proc
                            (if (equal? "" dir) "." dir)
                            glob)))

    ))

