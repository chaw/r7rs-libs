;"saturate.txt" Saturated-Colors Dictionary.
;Copyright 2002, 2010 Aubrey Jaffer
;
;Permission to copy this dictionary, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this dictionary must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that this data is
;error-free, and I am under no obligation to provide any services, by
;way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this material,
;there shall be no use of my name in any advertising, promotional, or
;sales literature without prior written consent in each case.

;Saturated colors from "Approximate Colors on CIE Chromaticity Diagram"
;Francis S. Hill, "Computer Graphics" Macmillan, 1990, pg. 572

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib saturate)
  (export saturate)
  (import (scheme base)
          (scheme char)
          (slib color))

  (begin

    ;; Given a colour name, return its colour
    (define (saturate colour-name)
      (let ((res (assoc colour-name *saturate* string-ci=?)))
        (if res
          (cdr res)
          #f)))

    (define *saturate*
      (list
        (cons "red"			(CIEXYZ->color '(0.735484 0.264516 0)))
        (cons "reddish orange"		(CIEXYZ->color '(0.647123 0.3525 0.00037723)))
        (cons "orange"			(CIEXYZ->color '(0.586144 0.413257 0.000598302)))
        (cons "yellowish orange"	(CIEXYZ->color '(0.531897 0.467256 0.000847751)))
        (cons "yellow"			(CIEXYZ->color '(0.499122 0.499899 0.000979355)))
        (cons "greenish yellow"		(CIEXYZ->color '(0.465098 0.5338 0.00110199)))
        (cons "yellow green"		(CIEXYZ->color '(0.373102 0.624451 0.0024476)))
        (cons "yellowish green"		(CIEXYZ->color '(0.22962 0.754329 0.0160512)))
        (cons "green"			(CIEXYZ->color '(0.0138702 0.750186 0.235943)))
        (cons "bluish green"		(CIEXYZ->color '(0.0453772 0.294952 0.659671)))
        (cons "blue green"		(CIEXYZ->color '(0.0687611 0.200711 0.730528)))
        (cons "greenish blue"		(CIEXYZ->color '(0.0912562 0.132684 0.77606)))
        (cons "blue"			(CIEXYZ->color '(0.0995467 0.11196 0.788494)))
        (cons "purplish blue"		(CIEXYZ->color '(0.14396 0.029703 0.826337)))
        (cons "bluish purple"		(CIEXYZ->color '(0.200614 0.0242655 0.77512)))
        (cons "purple"			(CIEXYZ->color '(0.263205 0.0523799 0.684415)))
        (cons "reddish purple"		(CIEXYZ->color '(0.354247 0.0932736 0.552479)))
        (cons "red purple"		(CIEXYZ->color '(0.479429 0.149503 0.371068)))
        (cons "purplish red"		(CIEXYZ->color '(0.604612 0.205731 0.189657)))
        ))

    ))

