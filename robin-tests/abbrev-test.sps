;; Test file for (robin abbrev)

(import (scheme base)
        (robin abbrev)
        (scheme list)
        (srfi 64))

(define (equal-assocs? alist1 alist2)
  (define (in-alist? association alist)
    (and (assoc (car association) alist)
         (equal? (cdr (assoc (car association) alist))
                 (cdr association))))
  ;
  (and (= (length alist1) (length alist2))
       (every (lambda (association) (in-alist? association alist2)) alist1)
       (every (lambda (association) (in-alist? association alist1)) alist2)))

(test-begin "robin-abbrev")

(test-assert 
  (equal-assocs? (abbrev '("car" "cat"))
                 '(("cat" . "cat") ("car" . "car"))))

(test-assert
  (equal-assocs? (abbrev '("car" "cone"))
                 '(("ca" . "car") ("car" . "car") ("co" . "cone") 
                                  ("con" . "cone") ("cone" . "cone"))))

(test-assert ; test with a prefix
  (equal-assocs? (abbrev '("car" "cat" "dog") "c")
                 '(("cat" . "cat") ("car" . "car"))))

(test-end)
