;; Directory library: some cross-implementation directory functions

;; Written by Peter Lane, 2017

;; # Open Works License
;; 
;; This is version 0.9.4 of the Open Works License
;; 
;; ## Terms
;; 
;; Permission is hereby granted by the holder(s) of copyright or other legal
;; privileges, author(s) or assembler(s), and contributor(s) of this work, to any
;; person who obtains a copy of this work in any form, to reproduce, modify,
;; distribute, publish, sell, sublicense, use, and/or otherwise deal in the
;; licensed material without restriction, provided the following conditions are
;; met:
;; 
;; Redistributions, modified or unmodified, in whole or in part, must retain
;; applicable copyright and other legal privilege notices, the above license
;; notice, these conditions, and the following disclaimer.
;; 
;; NO WARRANTY OF ANY KIND IS IMPLIED BY, OR SHOULD BE INFERRED FROM, THIS LICENSE
;; OR THE ACT OF DISTRIBUTION UNDER THE TERMS OF THIS LICENSE, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
;; AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS, ASSEMBLERS, OR HOLDERS OF
;; COPYRIGHT OR OTHER LEGAL PRIVILEGE BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER
;; LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT, OR OTHERWISE ARISING FROM, OUT
;; OF, OR IN CONNECTION WITH THE WORK OR THE USE OF OR OTHER DEALINGS IN THE WORK.

;; TODO: Larceny calls out to system
;; -- check works on Windows
;; -- other Linux versions?

(define-library
  (robin directory)
  (export change-directory 
          current-directory
          delete-directory
          list-directory       ; everything (like Ruby Dir#entries)
          list-directory-files ; just files, flag for full paths
          list-glob            ; returns glob of files (like Dir#glob)
          make-directory
          )
  (import (scheme base)
          (scheme case-lambda)        (slib format)
          (slib filename))

  ;; functions must be defined in platform specific ways
  (cond-expand 
    ;;
    ((library (chibi filesystem))
     (import (chibi filesystem)
             (scheme list))
     (begin ; good support
       ; change-directory exported
       ; current-directory exported
       ; delete-directory exported
       (define (list-directory dir)
         (delete ".." (delete "." (directory-files))))

       (define list-directory-files 
         (case-lambda
           ((dir)
            (list-directory-files dir #f))
           ((dir flag)
            (with-directory ; makes dir temporarily the current directory
              dir
              (lambda ()
                (let ((filenames (filter file-regular? (directory-files ".")))
                      (curr (current-directory)))
                  (if flag
                    (map (lambda (filename) (string-append curr "/" filename))
                         filenames)
                    filenames)))))))

       (define list-glob 
         (case-lambda 
           ((glob) ; return result as a list
            (let ((match-glob? (lambda (f) ((filename:match?? glob) f))))
              (directory-fold-tree (current-directory) #f #f 
                                   (lambda (f acc)
                                     (if (match-glob? f)
                                       (cons f acc)
                                       acc))
                                   '())))
           ((glob proc) ; apply proc to each result in list, returns #f
            (let ((match-glob? (lambda (f) ((filename:match?? glob) f))))
              (directory-fold-tree (current-directory) #f #f 
                                   (lambda (f acc)
                                     (when (match-glob? f)
                                       (proc f)))
                                   '()))
            #f)))

       (define make-directory create-directory*)))
    ;;
    (gauche
      (import (file util)
              (scheme list))
      (begin
        (define (change-directory str) (current-directory str))
        
        ; current-directory exported
        
        (define (delete-directory str) (delete-directory* str))
        
        (define list-directory directory-list)

        (define list-directory-files
          (case-lambda 
            ((dir)
             (list-directory-files dir #f))
            ((dir flag)
             (let ((curr (current-directory)))
               (let-values (((subdirs files) (directory-list2 dir)))
                           (if flag
                             (map (lambda (file) (string-append curr "/" dir "/" file))
                                  files)
                             files))))))

        (define list-glob 
          (case-lambda 
            ((glob) ; return result as a list
             (define glob-match? (filename:match?? glob))
             (define (collect-files dir) ; return list of all files, recursively, in dir
               (let-values (((subdirs files) (directory-list2 dir)))
                           (fold append (filter glob-match? files)
                                 (map collect-files 
                                      (map (lambda (d) (string-append dir "/" d))
                                          (remove (lambda (d) (member d '("." "..") string=?))
                                                  subdirs))))))
             ;
             (collect-files (current-directory)))
            ((glob proc) ; apply proc to each result in list, returns #f
             (define glob-match? (filename:match?? glob))
             (define (process-files dir) 
               (let-values (((subdirs files) (directory-list2 dir)))
               (format #t "Collect ~a ~a ~a~&" dir files (filter glob-match? files))
                           (for-each proc (filter glob-match? files))
                           (for-each process-files 
                                     (map (lambda (d) (string-append dir "/" d))
                                          (remove (lambda (d) (member d '("." "..") string=?))
                                                  subdirs)))))
             ;
             (process-files (current-directory))
             #f)))

        (define (make-directory str) (current-directory str))))
    ;;
    (kawa ; good support through JVM, except for cd
      (import (only (kawa lib files) create-directory)
              (only (kawa lib ports) current-path)
              (kawa lib system)
              (only (kawa base) as invoke invoke-static)
              (only (scheme list) fold remove))
      (begin
        (define (is-directory? filename)
          (invoke-static java.nio.file.Files 'isDirectory 
                         (as java.nio.file.Path (str->path filename))))

        (define (change-directory str) (system (string-append "cd " str))) ; TODO: Not working on Kawa

        (define (current-directory) (as String (current-path)))

        (define (delete-directory dir)
          (let ((obj (java.io.File (as String dir))))
            (and (invoke obj 'isDirectory)
                 (invoke obj 'delete))))

        (define (list-directory dir) ; error if dir not a directory
          (map (lambda (file) (invoke file 'toString))
               (invoke (java.io.File (as String dir)) 'list)))

        (define (str->path str)
          (invoke (as java.io.File 
                      (java.io.File (as String str)))
                  'toPath))

        (define list-directory-files 
          (case-lambda 
            ((dir)
             (list-directory-files dir #f))
            ((dir flag) 
             (map (lambda (file) ; list-directory-files must return just the filenames
                    (if flag ; on true, return complete path, else just filename part
                      (invoke (invoke (str->path file) 'toAbsolutePath)
                              'toString)
                      (invoke (invoke (str->path file) 'getFileName)
                              'toString)))
                  (remove is-directory? (map (lambda (p) (string-append dir "/" p)) (list-directory dir)))))))

        (define list-glob
          (case-lambda 
            ((glob)
             (define glob-match? (filename:match?? glob))
             (define (collect-files dir) ; return list of all files, recursively, in dir
               (cond ((is-directory? dir)
                      (let ((current (current-directory))
                            (res (fold append '()
                                       (map collect-files (map (lambda (p) (string-append dir "/" p)) (list-directory dir))))))
                        (change-directory current)
                        res))
                     ((glob-match? dir)
                      (list dir))
                     (else ; ignore file
                       '())))
             ;
             (collect-files (current-directory)))
            ((glob proc)
             (define glob-match? (filename:match?? glob))
             (define (process-files dir) ; return list of all files, recursively, in dir
               (cond ((is-directory? dir)
                      (let ((current (current-directory)))
                        (for-each process-files (map (lambda (p) (string-append dir "/" p)) (list-directory dir)))
                        (change-directory current)
                        #f))
                     ((glob-match? dir) ; run proc on the function
                      (proc dir))
                     (else ; ignore file
                       #f)))
             ;
             (process-files (current-directory))
             #f)))

        (define make-directory create-directory)))
    ;;
    (larceny ; poor support: calls out to system
      (import (primitives current-directory list-directory system)
              (scheme list))
      (begin 
        (define (is-directory? str) (zero? (system (string-append "test -d " str))))

        (define (change-directory str) (system (string-append "cd " str))) ; TODO: Not working on Larceny

        ; current-directory exported

        (define (delete-directory dir) (system (string-append "rmdir " dir)))

        ; list-directory exported

        (define list-directory-files 
          (case-lambda 
            ((dir)
             (list-directory-files dir #f))
            ((dir flag)
             (let ((files (remove is-directory? (list-directory dir)))
                   (curr (current-directory)))
               (if flag
                 (map (lambda (file) (string-append curr "/" dir "/" file))
                      files)
                 files)))))

        (define list-glob
          (case-lambda 
            ((glob)
             (define glob-match? (filename:match?? glob))
             (define (collect-files dir) ; return list of all files, recursively, in dir
               (cond ((is-directory? dir)
                      (fold append '()
                            (map collect-files 
                                 (map (lambda (p) (string-append dir "/" p))
                                      (list-directory dir)))))
                     ((glob-match? dir)
                      (list dir))
                     (else ; ignore file
                       '())))
             ;
             (collect-files (current-directory)))
            ((glob proc)
             (define glob-match? (filename:match?? glob))
             (define (process-files dir) ; visit list of all files, recursively, in dir
               (cond ((is-directory? dir)
                      (for-each process-files 
                                (map (lambda (p) (string-append dir "/" p)) 
                                     (list-directory dir))))
                     ((glob-match? dir) ; run proc on the function
                      (proc dir))
                     (else ; ignore file
                       #f)))
             ;
             (process-files (current-directory))
             #f)))

        (define (make-directory str) (system (string-append "mkdir " str)))))
    ;;
    (sagittarius ; good support for functions
      (import (sagittarius)
              (util file)
              (only (scheme list) filter))
      (begin

        (define (change-directory str) (set-current-directory str))

        ; current-directory exported
        ; delete-directory exported

        (define list-directory read-directory)

        (define list-directory-files 
          (case-lambda
            ((dir)
             (list-directory-files dir #f))
            ((dir flag)
             (let ((filenames (filter file-regular? (read-directory dir)))
                   (curr (current-directory)))
               (if flag
                 (map (lambda (filename) (build-path (build-path curr dir) filename))
                      filenames)
                 filenames)))))

        (define list-glob 
          (case-lambda 
            ((glob) ; return result as a list
             (let ((match-glob? (filename:match?? glob)))
               (filter match-glob? (find-files (current-directory)))))
            ((glob proc) ; apply proc to each result in list, returns #f
             (for-each proc (list-glob glob))
             #f)))

        (define (make-directory str) (create-directory str))))
    (else
      (error "(robin directory) not supported for current R7RS Scheme implementation")))

  )
