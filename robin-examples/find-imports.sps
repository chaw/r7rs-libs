;; This program illustrates using the Series library 
;; and creates two text files: 
;; libraries.txt - list of each library and its imports
;; imported.txt - list of each import and the libraries using it

(import (scheme base)
        (scheme read)
        (scheme write)
        (rebottled pregexp)
        (robin directory)
        (robin series)
        (slib format)
        (scheme list)
        (scheme sort))

(define *search-paths* '("nltk" "pfds" "rebottled" "robin" "slib" "weinholt"))

;; Given an sexpr, assumed to be the text of a library definition
;; Return a cons of the library name and its imports
(define (extract-libraries expr)
  (let ((library-name (cadr expr))
        (imports (cdar (filter (lambda (x) (eq? 'import (car x)))
                               (cdr expr)))))
    (cons library-name (map (lambda (imp) (if (and (> (length imp) 2)
                                                   (list? (cadr imp)))
                                            (cadr imp)
                                            imp))
                            imports))))

(define (library-path? path)
  (pregexp-match "[:alnum:]*\[:alnum:]*.sld$" path))

(define (library-defn? expr)
  (and (not (eof-object? expr))
       (list? expr)
       (eq? 'define-library (car expr))))

;; Extract a list of library defn-imports for given file
(define (process-file pathname)
  (map-fn extract-libraries
          (choose-if library-defn?
                     (scan-file pathname read))))

;; Create sorted association list of library name -> library imports
(define (library<? lib-1 lib-2)
  (string<? (format #f "~a" (car lib-1))
            (format #f "~a" (car lib-2))))

(define (list-directory-paths filename)
  (list-directory-files filename #t))

(define *dataset* 
  (list-sort
    library<?
    (collect-append 
      (map-fn process-file
              (choose-if library-path?
                         (scan ; scan over list of directory paths
                           (apply append (map list-directory-paths *search-paths*))))))))

;; Write to file all imports for each library name 

(collect-file "libraries.txt"
              (map-fn
                (lambda (library)
                  (format #f 
                          "Library: ~a~&~{-- ~a ~&~}~%" 
                          (car library) 
                          (list-sort library<? (cdr library))))
                (scan *dataset*))
              display)

;; Write to file, for each library name, which libraries use it

(define (describe-imported-by imp)
  (collect-string
    (map-fn (lambda (library)
              (if (member imp (cdr library) equal?)
                (format #f "-- ~a~&" (car library))
                ""))
            (scan *dataset*))))

(collect-file "imported.txt"
              (map-fn 
                (lambda (imp) (format #f "Importing: ~a~&~a~%" imp (describe-imported-by imp)))
                (scan (list-sort
                        library<?
                        (delete-duplicates
                          (apply append (map cdr *dataset*))))))
              display)

