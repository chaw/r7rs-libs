(import (scheme base)
        (scheme write)
        (autodiff stochastic-recognizer))

(define (with-a-telescope n)
  (if (zero? n) '() (append '(p det n) (with-a-telescope (- n 1)))))

(define (grammatical-hard-sentence n)
  `(det n v det n ,@(with-a-telescope n)))

(define (ungrammatical-hard-sentence n)
  `(det n v det n ,@(with-a-telescope n) v))

(define *production-distributions*
  (list (cons (make-production 's 'np 'vp) 1.0)
        (cons (make-production 'vp 'v 'np) 0.5)
        (cons (make-production 'vp 'v 's) 0.25)
        (cons (make-production 'vp 'vp 'pp) 0.25)
        (cons (make-production 'np 'np 'pp) 0.5)
        (cons (make-production 'np 'det 'n) 0.5)
        (cons (make-production 'pp 'p 'np) 1.0)))

(write (phrase-probability3
         (grammatical-hard-sentence 13) 's *production-distributions*))
(newline)
(write (phrase-probability3
         (ungrammatical-hard-sentence 13) 's *production-distributions*))
(newline)

