;; scmcount: counts lines of code in scheme files
;;
;; Example of using R7RS libraries
;; Written by Peter Lane, 2017

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (robin directory)
        (robin series)
        (slib format)
        (slib string-search)
        (only (srfi 1) fold)
        (only (srfi 132) list-sort))

(cond-expand
  ((library (srfi 13))
   (import (only (srfi 13) string-null? string-pad string-trim)))
  ((library (chibi string))
   (import (only (chibi string) string-null? string-trim))
   (begin
     (define (string-pad str len)
       (let ((diff (- len (string-length str))))
         (if (> len 0)
           (string-append (make-string diff #\space) str)
           str)))))
  (else
    (error "Scheme implementation does not support suitable string library")))

(define *extns* '("scm" "sld" "sls" "sps" "ss"))
(define *files* '())

;; retrieve information on lines of code in current file
;; note, caller puts us into correct directory
(define (count-lines-of-code filename)
  (define (non-comment-line? str)
    (not (char=? (string-ref str 0) #\;)))
  ;
  (collect-length
    (choose-if non-comment-line? ; keep all non-comment lines
               (choose-if-not string-null? ; reject null strings
                              (map-fn string-trim ; remove leading white space
                                      (scan-file filename))))))

(define (update-information filename)
  (define (scheme-filename? filename)
    (let ((extn-start (string-reverse-index filename #\.)))
      (and (number? extn-start)
           (member (string-copy filename (+ 1 extn-start)) *extns* string=?))))
  ;
  (when (scheme-filename? filename)
    (set! *files* (cons (cons filename (count-lines-of-code filename))
                        *files*))))

(define (collect-information directory)
  (let ((current (current-directory)))
    (guard (err (else (display err))) ; ignore any errors - e.g. permission errors
           (change-directory directory)
           (let ((chop (+ 1 (string-length (current-directory)))))
             (list-glob "**/*.s?*" 
                        (lambda (filename) ; chop starts directory from filename
                          (update-information (string-copy filename chop))))
             (change-directory current)))))

(define (output-information)
  (let* ((maxwidth (fold max 0 (map (lambda (l) (string-length (car l)))
                                    *files*)))
         (divider (make-string (+ maxwidth 10) #\-)))
    (format #t "~a~&" divider)
    (for-each (lambda (file) 
                (format #t "~a  ~5d~&" 
                        (string-pad (car file) maxwidth) 
                        (cdr file)))
              (list-sort (lambda (a b) (string<? (car a) (car b))) *files*))
    (format #t "~a~&" divider)
    (format #t "Total files: ~a~&" (length *files*))
    (format #t "Total lines: ~a~&" (fold + 0 (map cdr *files*)))))

;; run the script
(let* ((args (command-line))
       (initial-path (if (= 2 (length args))
                       (cadr args)
                       ".")))
  (collect-information initial-path)
  (output-information))

