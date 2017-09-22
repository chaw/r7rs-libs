;"sc2.scm" Implementation of rev2 procedures eliminated in subsequent versions.
; Copyright (C) 1991, 1993 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Note, append! is in srfi 1

(define-library
  (slib rev2-procedures)
  (export substring-move-left!
          substring-move-right!
          substring-fill!
          string-null?
          <?
          <=?
          =?
          >?
          >=?)
  (import (scheme base))
  (cond-expand
    ((library (scheme list))
     (import (only (scheme list) last-pair)))
    (else
     (import (only (srfi 1) last-pair))))

  (begin

    ;@
    (define (substring-move-left! string1 start1 end1 string2 start2)
      (do ((i start1 (+ i 1))
           (j start2 (+ j 1))
           (l (- end1 start1) (- l 1)))
        ((<= l 0))
        (string-set! string2 j (string-ref string1 i))))
    ;@
    (define (substring-move-right! string1 start1 end1 string2 start2)
      (do ((i (+ start1 (- end1 start1) -1) (- i 1))
           (j (+ start2 (- end1 start1) -1) (- j 1))
           (l (- end1 start1) (- l 1)))
        ((<= l 0))
        (string-set! string2 j (string-ref string1 i))))
    ;@
    (define (substring-fill! string start end char)
      (do ((i start (+ i 1))
           (l (- end start) (- l 1)))
        ((<= l 0))
        (string-set! string i char)))
    ;@
    (define (string-null? str)
      (= 0 (string-length str)))

    ;;;; need to add code for OBJECT-HASH and OBJECT-UNHASH
    ;@
    ;; These two methods upset 'read' in chibi-scheme
;    (define 1+
;      (let ((+ +))
;        (lambda (n) (+ n 1))))
;    (define -1+
;      (let ((+ +))
;        (lambda (n) (+ n -1))))
    ;@
    (define <? <)
    (define <=? <=)
    (define =? =)
    (define >? >)
    (define >=? >=)

    ))

