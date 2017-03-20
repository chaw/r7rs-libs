;;;; "table.scm" Bluestone table on patio.

(import (scheme base)
        (scheme inexact)
        (robin constants)
        (slib solid))

(define inch 25.4e-3)                   ; 25.4.mm

; First create the surfaces for bluestone and wood from monochromatic intensity maps (pictures).

(define bluestone
  (solid:texture "greystone.jpg" (solid:color '(.3 .3 .5) .65)))
(define douglas-fir
  (solid:texture "wood_g.jpg" (solid:color '(.7 .6 .2) .5)))

; The bluestone top is simple:

(define slab (solid:box '(20.5 2 12) bluestone))

; Next make the legs, rails, and styles. The rails are created vertically, then
; rotated so that the woodgrain runs along the length rather than across the
; width.

(define leg (solid:box '(1.25 22 1.25) douglas-fir))
(define long-rail
  (solid:rotation '(0 0 1) 90
                  (solid:box '(.75 15.5 .75) douglas-fir)))
(define short-rail
  (solid:rotation '(1 0 0) 90
                  (solid:box '(.75 9 .75) douglas-fir)))
(define style (solid:cylinder .375 -4 douglas-fir))

; With the pieces already oriented, translation and repetition are all that is
; needed to finish assembly of the table. The overall upward translation by 11
; inches puts the leg tips at 0.

(define table
  (solid:translation
    '(0 11 0)
    (solid:translation '(0 12 0) slab)
    (solid:translation
      '(0 3 0)
      (solid:center-array-of 2 2 short-rail '(15.5 0 0) '(0 4 0))
      (solid:center-array-of 2 2 style '(15.5 0 0) '(0 0 1.5)))
    (solid:translation
      '(0 5 0)
      (solid:center-array-of 2 2 long-rail '(0 4 0) '(0 0 9))
      (solid:center-array-of 3 2 style '(1.5 0 0) '(0 0 9)))
    (solid:center-array-of 2 2 leg '(15.5 0 0) '(0 0 9))))

; Create a patio from a photograph of one cell of the paver pattern.

(define patio
  (let ((repeat 9))
    (solid:translation
      '(0 -1.5 0)
      (solid:box `(,(* 15 repeat) 3 ,(* 15 repeat))
                 (solid:texture "paver.jpg"
                                (solid:color '(1 .9 .95) .4)
                                repeat)))))

; Here are two objects on the table (at 24 inch altitude).

(define curios
  (solid:translation
    '(0 24 0)
    (solid:translation
      '(-7 2 4)
      (solid:pyramid 4 4 (solid:color '#(1 .8 0) .4 '#(1 .9 .5) .8)))
    (solid:translation
      '(0 0.95 0)
      (solid:rotation '(0 0 1)
                      (+ -90 (/ (asin (/ .036 .32)) (/ PI 180)))
                      (solid:scale (/ inch) (solid:arrow))))))

; The rest creates viewpoints, backdrop, and daylight.

(vrml-to-file
  "table.wrl"
  (world:info "Bluestone Table")
  "NavigationInfo {headlight FALSE avatarSize [0.01, 0.01, 0.01]}"

  (solid:translation '(0 .35 0) (scene:viewpoint "Photo" 1.3 0 -15.1))
  (solid:translation '(0 .5 0) (scene:viewpoints 1.3))
  (scene:sky-and-dirt)
  (scene:sun 42 340 9 2 1.5)
  (solid:scale inch
               (solid:rotation '(0 1 0) 21
                               curios
                               table)
               patio))

