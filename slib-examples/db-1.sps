;; Simple database example
;; -- create the database
;; -- add a table
;; -- put some data in
;; -- retrieve some data

;; Note: Chibi leaves a .#mydb lock file

(import (scheme base)
        (scheme file)
        (scheme write)
        (slib databases)
        (slib alist-table))

(define *filename* "mydb")

;; ensure it is not already there
(when (file-exists? *filename*) (delete-file *filename*))
;; nor the lock files, in case of errors
(when (file-exists? (string-append "\.#" *filename*)) (delete-file (string-append "\.#" *filename*)))
(when (file-exists? (string-append "~\$" *filename*)) (delete-file (string-append "~\$" *filename*)))

;; create database only needs to run once
(define db (create-database *filename* 'alist-table))

(define rdb (open-database! db)) 
(define-tables rdb '(testit ((id number)) ((first string) (second string))
                            ((1 "Peter" "Lane")
                             (2 "Joe" "Smith"))))

(display (mdbm:report)) (newline)

(close-database rdb)

(display "After closing:") (newline)
(display (mdbm:report)) (newline)

(define new-rdb (open-database! *filename*))

(display "After re-opening:") (newline)
(display (mdbm:report)) (newline)
(display "Table definition: ")
(display (list-table-definition new-rdb 'testit)) (newline)

(define table (open-table! new-rdb 'testit))

(display "Retrieving 1: ") (display ((table 'row:retrieve) 1)) (newline)
(display "Retrieving 2: ") (display ((table 'row:retrieve) 2)) (newline)
(display "Retrieving 3: ") (display ((table 'row:retrieve) 3)) (newline)

((table 'row:insert) '(3 "Jane" "Wheeler"))
(display "Retrieving 3: ") (display ((table 'row:retrieve) 3)) (newline)

((table 'for-each-row) (lambda (row) (display "Row: ") (display row) (newline)))

(close-database new-rdb)

