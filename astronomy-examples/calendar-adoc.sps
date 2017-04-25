;; Write a calendar in Asciidoc format
;; Peter Lane, 2017

(import (scheme base)
        (scheme case-lambda)
        (scheme file)
        (scheme process-context)
        (scheme read)
        (slib format)
        (slib scanf)
        (astronomy calendar)
        (srfi 1))

;; -- utilities to write asciidoc 

;; Write given table of data to the given filename, overwriting any previous file
(define write-table-to-file 
  (case-lambda 
    ((filename data)
     (write-table-to-file filename data "Table"))
    ((filename data title) 
     (when (file-exists? filename) (delete-file filename))
     (with-output-to-file 
       filename 
       (lambda ()
         (write-table data title))))))

;; Write given table of data to the current output port
(define (write-table data title)
  (format #t ".~a~&" title)
  (format #t "[cols=\"~{~a~^,~}\"]~&"
          (make-list (length (car data)) "1^a"))
  (format #t "|==================================~&")
  (for-each (lambda (row)
              (format #t "~{|~a~}~&" 
                      (map (lambda (item) (if (string? item) item ""))
                           row)))
            data)
  (format #t "|==================================~&")
  )

;; -- write calendar for given year

;; Given a date, return an asciidoc table with required information.  
;; Calendar may have empty slots to pad out table, which are represented 
;; by null, so represent null with an empty string.
(define (date-information date)
  (if (date? date)
    (format #f 
            "~%~%~
            [grid=\"none\",frame=\"none\",cols=\"1^\"]~&~
            !==================================~&~
            ! ~a ! Day of year: ~a~&~
            !==================================~%~%" 
            (date-day date) 
            (day-of-year date))
            ""))

;; Write given month number 
(define (write-month-to-stream month year)
  (let* ((month-dates ; select all the dates for given month, assume ordered by day
          (filter (lambda (d) (= month (date-month d))) (all-dates year)))
         (spaced-dates (append (make-list (day-of-week-as-fixnum (car month-dates)) #f)
                               month-dates)))
    (write-table 
      ; table for month with week names as first row, to form heading
      (cons (week-days)
            (group-by (map date-information spaced-dates)
                      7))
      ; title for table
      (month-name month))))

;; Open given file and write out each month in turn separated by a blank line"
(define (write-calendar-to-file filename year)
  (when (file-exists? filename) (delete-file filename))
  (with-output-to-file 
    filename 
    (lambda ()
      (format #t "= Calendar ~a = ~%" year)
      (do ((month 1 (+ 1 month)))
        ((> month 12) )
        (format #t "~%")
        (write-month-to-stream month year)))))

;; -- top-level to run the program

(let ((args (command-line)))
  (if (= 3 (length args))
    (let ((filename (list-ref args 1))
          (year (scanf-read-list "%d" (list-ref args 2))))
      (cond ((= 1 (length year))
             (write-calendar-to-file filename (car year))
             (format #t "Now use: a2x -L --dblatex-opts \"-P latex.output.revhistory=0\" ~a~&" filename)
             (format #t "     or: asciidoc ~a~&" filename)
             (format #t " [ These operations take some time ]~&"))
            (else
              (format #t "Error: ~a could not be read as a year~&" (list-ref args 2))))))
  (format #t "Error: call with filename year"))
    
