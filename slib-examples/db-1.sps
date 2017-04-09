;; Simple database example
;; -- create the database
;; -- add a table
;; -- put some data in
;; -- retrieve some data

;; Currently, only works with Larceny

(import (scheme base)
        (scheme file)
        (scheme write)
        (slib databases)
        (slib alist-table))

(define *filename* "mydb")

;; ensure it is not already there
(when (file-exists? *filename*) (delete-file *filename*))

;; create database only needs to run once
(define db (create-database *filename* 'alist-table))

(define rdb (open-database! db 'alist-table)) 
(define-tables rdb '(testit ((id number)) ((first string) (second string))
                            ((1 "Peter" "Lane")
                             (2 "Joe" "Smith"))))

(display (mdbm:report)) (newline)

(close-database *filename*)

