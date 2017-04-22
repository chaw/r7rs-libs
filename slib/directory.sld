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
;;
;; Added pathname->dirname as a new function to replace pathname->vicinity
;; -- this removes the need for SRFI 59

(define-library
  (slib directory)
  (export current-directory
          directory-for-each
          directory*-for-each
          make-directory
          pathname->dirname ;; New function added to remove need for SRFI 59
          )
  (import (scheme base) 
          (scheme case-lambda)
          (slib common) 
          (slib filename))

  ;; functions must be defined in platform specific ways
  (cond-expand 
    ((library (chibi filesystem))
     (import (chibi filesystem)
             (chibi pathname))
     (begin
       ; current-directory exported
       (define make-directory create-directory*)
       (define (pathname->dirname path)
         (string-append (path-directory path) "/"))
       (define list-directory-files directory-files)))
    (kawa
      (import (only (kawa lib files) create-directory path-directory)
              (only (kawa lib ports) current-path)
              (only (kawa base) as invoke))
      (begin
        (define current-directory current-path)
        (define make-directory create-directory)
        (define (pathname->dirname path)
          (let* ((dir (path-directory path))
                 (chars (reverse (string->list dir))))
            (if (and (not (null? chars))
                     (char=? #\. (car chars))) ; Kawa sometimes adds a 'dot' to end, so remove it
              (list->string (reverse (cdr chars)))
              dir)))
        (define (list-directory-files dir)
          (map (lambda (file) ; list-directory-files must return just the filenames
                 (let ((path (invoke file 'toString)))
                   (string-copy path (string-length (pathname->dirname path)))))
               (invoke (java.io.File (as String dir)) 'listFiles)))))
    (larceny
      (import (primitives current-directory list-directory)
              (only (srfi 59) pathname->vicinity))
      (begin 
        ; current-directory exported
        (define (make-directory str) (system (string-append "mkdir " str)))
        (define pathname->dirname pathname->vicinity)
        (define list-directory-files list-directory)))
    (sagittarius
      (import (sagittarius)
              (util file)
              (only (srfi 1) filter))
      (begin
        ; current-directory exported
        (define (make-directory str) (create-directory str))
        (define (pathname->dirname path)
          (let-values (((dir file ext) (decompose-path path)))
                      (if (string? dir)
                        dir
                        "")))
        (define (list-directory-files dir)
          (filter file-regular? (read-directory dir)))))
    (else
      (error "(slib directory) not supported for current R7RS Scheme implementation")))

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
      (let* ((dir (pathname->dirname path-glob))
             (glob (string-copy path-glob (string-length dir))))
        (directory-for-each proc
                            (if (equal? "" dir) "." dir)
                            glob)))

    ))

