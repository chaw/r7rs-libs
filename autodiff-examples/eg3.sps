(import (scheme base)
        (scheme write)
        (autodiff nondeterministic-recognizer))

(define (with-a-telescope n)
  (if (zero? n) '() (append '(p det n) (with-a-telescope (- n 1)))))

(define (grammatical-hard-sentence n)
  `(det n v det n ,@(with-a-telescope n)))

(define (ungrammatical-hard-sentence n)
  `(det n v det n ,@(with-a-telescope n) v))

(define *productions*
  (list (make-production 's 'np 'vp)
        (make-production 'vp 'v 'np)
        (make-production 'vp 'v 's)
        (make-production 'vp 'vp 'pp)
        (make-production 'np 'np 'pp)
        (make-production 'np 'det 'n)
        (make-production 'pp 'p 'np)))

(write (phrase3? (grammatical-hard-sentence 13) 's *productions*))
(newline)
(write (phrase3? (ungrammatical-hard-sentence 13) 's *productions*))
(newline)

