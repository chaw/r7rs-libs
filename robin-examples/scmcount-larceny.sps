;; scmcount: counts lines of code in scheme files
;;
;; Example of using R7RS libraries
;; Written by Peter Lane, 2017

;; Separate version as Larceny:
;; 1. does not like cond-expand in program files
;; 2. trips over (slib format) and ends in an infinite loop of unhandled exceptions

(import (scheme base)
        (scheme process-context)
        (scheme write)
        (robin directory)
        (robin series)
        (slib string-search)
        (only (scheme list) fold)
        (only (srfi 13) string-null? string-pad string-trim)
        (only (scheme sort) list-sort))

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
                        (lambda (filename) ; chop start directory from filename
                          (update-information (string-copy filename chop))))
             (change-directory current)))))

(define (output-information)
  (let* ((maxwidth (fold max 0 (map (lambda (l) (string-length (car l)))
                                    *files*)))
         (divider (make-string (+ maxwidth 10) #\-)))
    (display divider) (newline)
    (for-each (lambda (file) 
                (display (string-pad (car file) maxwidth)) 
                (display "  ") 
                (display (string-pad (number->string (cdr file)) 5)) 
                (newline))
              (list-sort (lambda (a b) (string<? (car a) (car b))) *files*))
    (display divider) (newline)
    (display "Total files: ") (display (length *files*)) (newline)
    (display "Total lines: ") (display (fold + 0 (map cdr *files*))) (newline)))

;; run the script
(let* ((args (command-line))
       (initial-path (if (= 2 (length args))
                       (cadr args)
                       ".")))
  (collect-information initial-path)
  (output-information))

