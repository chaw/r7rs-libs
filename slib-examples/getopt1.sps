
(import (scheme base)
        (scheme process-context) (scheme write)
        (slib format)
        (slib getopt))

(define argv (command-line))
(define opts ":a:b:cd")

(define (do-arguments opt)
  (case opt
    ((#\a) (format #t "option a: ~a~&" (option-arg)))
    ((#\b) (format #t "option b: ~a~&" (option-arg)))
    ((#\c) (format #t "option c~&"))
    ((#\d) (format #t "option d~&"))
    ((#\?) (format #t "error ~a~&" (option-name)))
    ((#\:) (format #t "missing arg ~a~&" (option-name)))
    ((#f) (when (< (option-index) (length argv))
            (format #t "argv[~a]=~a~&" (option-index)
                   (list-ref argv (option-index))))
          (option-index (+ 1 (option-index)))))
  ;
  (when (< (option-index) (length argv))
    (do-arguments (getopt opts))))

(do-arguments (getopt opts))
