;; Tests of fector - rewritten for srfi 64 by Peter Lane

;;; test-vicare.sps --- Functional Vectors: tests with Vicare Scheme

;; Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Marco Maggi <marco.maggi-ipsu@poste.it>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

(import (scheme base)
        (scheme case-lambda)
        (pfds fector)
        (srfi 64))

(test-begin "pfds-fector")

;;;; helpers

(define fector=?
  (case-lambda
   ((obj1 obj2)
    (fector=? obj1 obj2 eqv?))
   ((obj1 obj2 item=)
    (and (fector? obj1)
	 (fector? obj2)
	 (or (eq? obj1 obj2)
	     (let ((len1 (fector-length obj1)))
	       (and (= len1 (fector-length obj2))
		    (let loop ((i 0))
		      (or (= i len1)
			  (and (item= (fector-ref obj1 i)
				      (fector-ref obj2 i))
			       (loop (+ 1 i))))))))))))

;;;; building fectors

(test-equal
    (let ((F (make-fector 5 #\a)))
      (and (fector? F)
	   (fector->list F)))
   '(#\a #\a #\a #\a #\a))

(test-equal
    (let ((F (fector 1 2 3)))
      (and (fector? F)
	   (fector->list F)))
   '(1 2 3))

(test-equal
    (let ((F (build-fector 5 (lambda (idx) (* 10 idx)))))
      (and (fector? F)
	   (fector->list F)))
   '(0 10 20 30 40))

;;;; inspecting

(test-equal
    (let ((F (fector)))
      (fector-length F))
   0)

(test-equal
    (let ((F (fector 1)))
      (fector-length F))
   1)

(test-equal
    (let ((F (fector 1 2 3)))
      (fector-length F))
   3)

;;;; accessing and mutating

(test-equal
    (let ((F (fector 1 2 3)))
      (list (fector-ref F 0)
	    (fector-ref F 1)
	    (fector-ref F 2)))
   '(1 2 3))

(test-equal
    (let* ((F (fector 1 2 3))
	   (F (fector-set F 0 'a))
	   (F (fector-set F 1 'b))
	   (F (fector-set F 2 'c)))
      (list (fector-ref F 0)
	    (fector-ref F 1)
	    (fector-ref F 2)))
   '(a b c))

;;;; comparison

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector 1 2 3)))
      (fector=? F G))
   #t)

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector 10 2 3)))
      (fector=? F G))
   #f)

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector 1 20 3)))
      (fector=? F G))
   #f)

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector 1 2 30)))
      (fector=? F G))
   #f)

;;; --------------------------------------------------------------------

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector 1 2 3 4)))
      (fector=? F G))
   #f)

(test-equal
    (let ((F (fector 1 2 3 4))
	  (G (fector 1 2 3)))
      (fector=? F G))
   #f)

(test-equal
    (let ((F (fector))
	  (G (fector 1 2 3)))
      (fector=? F G))
   #f)

(test-equal
    (let ((F (fector 1 2 3))
	  (G (fector)))
      (fector=? F G))
   #f)

;;;; conversion

(test-equal
    (let ((F (fector 1 2 3)))
      (fector->list F))
   '(1 2 3))

(test-assert (fector=?
               (list->fector '(1 2 3))
               (fector 1 2 3)))

;;;; done

(test-end)

