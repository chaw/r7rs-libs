
(import (scheme base)
        (scheme process-context) 
        (scheme write)
        (slib getopt))

(define argv (command-line))
(define opts ":a:b:cd")

(define (do-arguments opt)
  (case opt
    ((#\a) (display "option a: ") (display (option-arg)) (newline))
    ((#\b) (display "option b: ") (display (option-arg)) (newline))
    ((#\c) (display "option c\n"))
    ((#\d) (display "option d\n"))
    ((#\?) (display "error ") (display (option-name)) (newline))
    ((#\:) (display "missing arg ") (display (option-name)) (newline))
    ((#f) (when (< (option-index) (length argv))
            (display "argv[") (display (option-index))
            (display "]=") (display (list-ref argv (option-index)))
            (newline))
          (option-index (+ 1 (option-index)))))
  ;
  (when (< (option-index) (length argv))
    (do-arguments (getopt opts))))

(do-arguments (getopt opts))



