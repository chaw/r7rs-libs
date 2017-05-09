;; This program creates the tables of information on library imports/imported by
;; used in the documentation
;; -- run from root directory of r7rs-libs

(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme read)
        (scheme write)
        (rebottled pregexp)
        (robin directory)
        (robin series)
        (only (slib common) identity)
        (slib format)
        (srfi 1)
        (srfi 132))

(define *search-paths* '("astronomy" "autodiff" "nltk" "pfds" "rebottled" "robin" "slib" "weinholt"))

;; Given an sexpr, assumed to be the text of a library definition
;; Return a cons of the library name and its imports
(define (extract-libraries expr)
  (let ((library-name (cadr expr))
        (exports (cdar (filter (lambda (x) (eq? 'export (car x)))
                               (cdr expr))))
        (imports (cdar (filter (lambda (x) (eq? 'import (car x)))
                               (cdr expr)))))
    (list library-name 
          (map (lambda (imp) (if (and (> (length imp) 2)
                                                   (list? (cadr imp)))
                                            (cadr imp)
                                            imp))
                            imports)
          exports)))

(define (library-path? path)
  (pregexp-match "[:alnum:]*.sld$" path))

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

(define *dataset* 
  (list-sort
    library<?
    (collect-append 
      (map-fn process-file
              (choose-if library-path?
                         (scan ; scan over list of directory paths
                           (apply append 
                                  (map (lambda (path) 
                                         (list-directory-files path #t))
                                       *search-paths*))))))))

;; return list of libraries which this assn imports
(define (imports assn) 
  (cadr assn))
;; return list of libraries which import this assn
(define (imported-by assn) 
  (let ((library (car assn)))
    (collect 
      (choose-if identity
                 (map-fn (lambda (assn)
                           (if (member library (cadr assn) equal?)
                             (car assn)
                             #f))
                         (scan *dataset*))))))

;; Overall statistics
(when (file-exists? "doc/summary.txt")
  (delete-file "doc/summary.txt"))

(with-output-to-file
  "doc/summary.txt"
  (lambda ()
    (format #t "There are ~a subcollections and ~a libraries.~&~&"
            (length *search-paths*) 
            (length *dataset*))))

;; Write table of statistics
(when (file-exists? "doc/subdata.txt")
  (delete-file "doc/subdata.txt"))

(with-output-to-file 
  "doc/subdata.txt"
  (lambda ()
    (let ((total-libraries 0)
          (total-exports 0))
      (format #t ".Information on Subcollections~&")
      (format #t "[width=\"50%\",frame=\"all\",options=\"header\"]~&")
      (format #t "|================================================~&")
      (format #t "| Name | Libraries | Exports ~&")
      (for-each
        (lambda (subcollection)
          (let* ((libraries (filter (lambda (x) (memv subcollection (car x)))
                                    *dataset*))
                 (num-libraries (length libraries))
                 (num-exports (fold (lambda (v e) (+ (length (caddr v)) e)) 0 libraries)))
            (set! total-libraries (+ total-libraries num-libraries))
            (set! total-exports (+ total-exports num-exports))
            (format #t "| ~a | ~a | ~a~&"
                    subcollection
                    num-libraries
                    num-exports)))
        (map string->symbol *search-paths*))
      (format #t "| Total | ~a | ~a~&" total-libraries total-exports)
      (format #t "|================================================~&"))))

;; Write the table of import/imported-by data for each library in turn

(when (file-exists? "doc/alldata.txt")
  (delete-file "doc/alldata.txt"))

(with-output-to-file
  "doc/alldata.txt"
  (lambda ()
    (define (write-table library)
      (format #t ".Library (~a indexterm2:[~a])~&" (caar library) (cadar library))
      (format #t "[width=\"80%\",frame=\"all\",options=\"header\"]~&")
      (format #t "|================================================~&")
      (format #t "| Imports | Imported by ~&")
      (let* ((lib-imports (remove (lambda (lib) (eq? (car lib) 'scheme)) (imports library)))
             (lib-imported-by (remove (lambda (lib) (eq? (car lib) 'scheme)) (imported-by library)))
             (diff (- (length lib-imports) (length lib-imported-by)))
             (filler (make-list (abs diff) "")))
        (cond ((zero? diff) )
              ((negative? diff)
               (set! lib-imports (append lib-imports filler)))
              ((positive? diff)
               (set! lib-imported-by (append lib-imported-by filler))))
        (if (and (null? lib-imports)
                 (null? lib-imported-by))
          (format #t "| only (scheme NNNN) |  ~&")
          (for-each (lambda (im by)
                      (format #t "| ~a | ~a~&" im by))
                    lib-imports
                    lib-imported-by)))
      (format #t "|================================================~&~%"))

    (for-each write-table *dataset*)))

