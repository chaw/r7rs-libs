

(define-library
  (robin directory)
  (export current-directory
          make-directory
          list-directory-files
          list-directory-paths)
  (import (scheme base))

  ;; functions must be defined in platform specific ways
  (cond-expand 
    ((library (chibi filesystem))
     (import (chibi filesystem))
     (begin
       ; current-directory exported
       (define make-directory create-directory*)
       (define list-directory-files directory-files)))
    (kawa
      (import (only (kawa lib files) create-directory)
              (only (kawa lib ports) current-path)
              (only (kawa base) as invoke)
              (only (srfi 59) pathname->vicinity))
      (begin
        (define current-directory current-path)
        (define make-directory create-directory)
        (define (list-directory-files dir)
          (map (lambda (file) ; list-directory-files must return just the filenames
                 (let ((path (invoke file 'toString)))
                   (string-copy path (string-length (pathname->vicinity path)))))
               (invoke (java.io.File (as String dir)) 'listFiles)))))
    (larceny
      (import (primitives current-directory list-directory system))
      (begin 
        ; current-directory exported
        (define (make-directory str) (system (string-append "mkdir " str)))
        (define list-directory-files list-directory)))
    (else
      (error "(robin directory) not supported for current R7RS Scheme implementation")))

  (begin

    (define (list-directory-paths dir)
      (map (lambda (filename) (string-append dir "/" filename))
           (list-directory-files dir)))

    ))
