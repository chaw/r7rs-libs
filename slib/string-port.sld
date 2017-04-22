;;;;"strport.scm" Portable string ports for Scheme
;;;Copyright 1993 Dorai Sitaram and Aubrey Jaffer.
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
;; Rewritten to use string-ports and not temporary files

(define-library
  (slib string-port)
  (export call-with-output-string
          call-with-input-string)
  (import (scheme base))

  (begin

    ;@
    (define (call-with-output-string f)
      (let ((sport (open-output-string)))
        (f sport)
        (get-output-string sport)))

    ;@
    (define (call-with-input-string s f)
      (call-with-port (open-input-string s) f))

    ))

