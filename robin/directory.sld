

(define-library
  (robin directory)
  (export change-directory 
          current-directory
          list-directory-files
          list-directory-paths
          make-directory
          )
  (import (scheme base))

  ;; functions must be defined in platform specific ways
  (cond-expand 
    ((library (chibi filesystem))
     (import (chibi filesystem))
     (begin
       ; change-directory exported
       ; current-directory exported
       (define make-directory create-directory*)
       (define list-directory-files directory-files)))
    (kawa
      (import (only (kawa lib files) create-directory)
              (only (kawa lib ports) current-path)
              (only (kawa base) as invoke)
              (only (srfi 59) pathname->vicinity))
      (begin
        (define (change-directory str) (system (string-append "cd " str)))
        (define (current-directory) (as String current-path))
        (define make-directory create-directory)
        (define (list-directory-files dir)
          (map (lambda (file) ; list-directory-files must return just the filenames
                 (let ((path (invoke file 'toString)))
                   (string-copy path (string-length (pathname->vicinity path)))))
               (invoke (java.io.File (as String dir)) 'listFiles)))))
    (larceny
      (import (primitives current-directory list-directory system))
      (begin 
        (define (change-directory str) (system (string-append "cd " str)))
        ; current-directory exported
        (define (make-directory str) (system (string-append "mkdir " str)))
        (define list-directory-files list-directory)))
    (sagittarius
      (import (sagittarius)
              (only (srfi 1) filter))
      (begin
        (define (change-directory str) (current-directory str))
        ; current-directory exported
        (define (make-directory str) (create-directory str))
        (define (list-directory-files dir)
          (filter file-regular? (read-directory dir)))))
    (else
      (error "(robin directory) not supported for current R7RS Scheme implementation")))

  (begin

    (define (list-directory-paths dir)
      (map (lambda (filename) (string-append dir "/" filename))
           (list-directory-files dir)))

    ))
