;; Some test cases for the (rebottled json) library testing read/write of JSON format
;; -- this needs some more tests

(import (scheme base)
        (rebottled json)
        (srfi 64))

(define (test-write val res)
  (test-equal res
              (parameterize ((current-output-port (open-output-string)))
                            (json-write val)
                            (get-output-string (current-output-port)))))

(define (test-read val res)
  (test-equal res
              (json-read (open-input-string val))))

(test-begin "rebottled-json")

(for-each (lambda (res-val)
            (test-write (car res-val) (cdr res-val))
            (test-read (cdr res-val) (car res-val)))
          (list ; Scheme value -> JSON string
            (cons 3 "3") 
            (cons (list 1 2 3) "[1, 2, 3]")
            (cons #f "false")
            (cons #t "true")
            (cons (vector (cons "a" 4) (cons "b" 5) (cons "c" 6)) "{\"a\": 4, \"b\": 5, \"c\": 6}")
            ))
(test-assert (and (symbol? (json-read (open-input-string "null")))
                  (eq? 'null (json-read (open-input-string "null")))))
(test-write 'null "\"null\"")
(parameterize ((current-output-port (open-output-string)))
              (test-error (json-write (vector 4 5 6)))
              (test-error (json-write (vector (list 4 'a))))
              )

(test-end)

