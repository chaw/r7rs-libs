;; Examples of using Series package
;; -- see tests for main examples

(import (scheme base)
        (scheme eval)
        (scheme file)
        (scheme write)
        (robin series)
        (slib line-io))

(define *test-file* "test-series.txt")

;; write a test file
(collect-file *test-file* (scan '("a line" "a second line")) write-line)

;; read from a file, and display each line
(let ((file-scanner (scan-file *test-file*)))
  (collect 
    (map-fn
      (lambda (line) 
        (display (string-append "> " line "\n")))
      file-scanner)))

(delete-file *test-file*)

