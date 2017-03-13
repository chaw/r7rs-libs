
(import (scheme base)
        (scheme process-context)
        (slib format)
        (slib getopt))

(define argv (command-line))
(define opts ":a:b:cd")
(let loop ((opt (getopt opts)))
  (case opt
    ((#\a) (format #t "option a: ~a~&" (option-value)))
    ((#\b) (format #t "option b: ~a~&" (option-value)))
    ((#\c) (format #t "option c~&"))
    ((#\d) (format #t "option d~&"))
    ((#\?) (format #t "error ~a~&" (option-name)))
    ((#\:) (format #t "missing arg ~a~&" (option-name)))
    ((#f) (if (< (option-index) (length argv))
            (format #t "argv[~a]=~a~&" (option-index)
                   (list-ref argv (option-index))))
          (next-option!)))
  (if (< (option-index) (length argv))
    (loop (getopt opts))))


