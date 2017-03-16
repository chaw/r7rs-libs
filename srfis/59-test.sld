;; IN PROGRESS 
;; SRFI 59 Vicinities

(define-library
  (srfi 59)
  (export pathname->vicinity
          )
  (import (scheme base))

  (begin

    ;; Checks if given character is a directory separator
    (define vicinity:suffix?
      (let ((suffi
              (case (software-type)
                ((amiga)			'(#\: #\/))
                ((macos thinkc)			'(#\:))
                ((ms-dos windows atarist os/2)	'(#\\ #\/))
                ((nosve)			'(#\: #\.))
                ((unix coherent plan9)		'(#\/))
                ((vms)				'(#\: #\]))
                (else
                  (slib:warn "SRFI 59" 'unknown 'software-type (software-type))
                  "/"))))
        (lambda (chr) (and (memv chr suffi) #t))))

    ;@
    (define (pathname->vicinity pathname)
      (let loop ((i (- (string-length pathname) 1)))
        (cond ((negative? i) "")
              ((vicinity:suffix? (string-ref pathname i))
               (substring pathname 0 (+ i 1)))
              (else (loop (- i 1))))))


    ))

