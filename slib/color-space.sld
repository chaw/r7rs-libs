;;; "colorspc.scm" color-space conversions
;Copyright 2001, 2002 Aubrey Jaffer
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

(define-library
  (slib color-space)
  (export read-cie-illuminant
          read-normalized-illuminant
          illuminant-map
          illuminant-map->XYZ
          spectrum->XYZ
          spectrum->chromaticity
          wavelength->XYZ
          wavelength->chromaticity
          blackbody-spectrum
          temperature->XYZ
          temperature->chromaticity
          XYZ->chromaticity
          chromaticity->CIEXYZ
          chromaticity->whitepoint
          XYZ->xyY
          xyY->XYZ
          xyY:normalize-colors
          L*a*b*:DE*
          L*a*b*:DE*94
          CMC-DE
          CIEXYZ:D65
          CIEXYZ:D50
          CIEXYZ:A
          CIEXYZ:B
          CIEXYZ:C
          CIEXYZ:E
          color:linear-transform
          CIEXYZ->RGB709
          RGB709->CIEXYZ
          CIEXYZ->L*u*v*
          L*u*v*->CIEXYZ
          CIEXYZ->L*a*b*
          L*a*b*->CIEXYZ
          L*a*b*->L*C*h
          L*C*h->L*a*b*
          CIEXYZ->sRGB
          sRGB->CIEXYZ
          CIEXYZ->xRGB
          xRGB->CIEXYZ
          sRGB->xRGB
          xRGB->sRGB
          CIEXYZ->e-sRGB
          e-sRGB->CIEXYZ
          sRGB->e-sRGB
          e-sRGB->sRGB
          e-sRGB->e-sRGB)
  (import (scheme base)
          (scheme cxr)
          (scheme file)
          (scheme inexact)
          (scheme read)
          (slib common)
          (srfi 60)
          (srfi 95))

  (begin

    ;@
    (define (color:linear-transform matrix row)
      (map (lambda (mrow) (apply + (map * mrow row)))
           matrix))

    (define RGB709:into-matrix
      '((  3.240479 -1.537150 -0.498535 )
        ( -0.969256  1.875992  0.041556 )
        (  0.055648 -0.204043  1.057311 )))

    ;;; http://www.pima.net/standards/it10/PIMA7667/PIMA7667-2001.PDF gives
    ;;; matrix identical to sRGB:from-matrix, but colors drift under
    ;;; repeated conversions to and from CIEXYZ.  Instead use RGB709.

    (define RGB709:from-matrix
      '((  0.412453  0.357580  0.180423 )
        (  0.212671  0.715160  0.072169 )
        (  0.019334  0.119193  0.950227 )))

    ;; From http://www.cs.rit.edu/~ncs/color/t_convert.html
    ;@
    (define (CIEXYZ->RGB709 XYZ)
      (color:linear-transform RGB709:into-matrix XYZ))
    (define (RGB709->CIEXYZ rgb)
      (color:linear-transform RGB709:from-matrix rgb))

    ;;; From http://www.w3.org/Graphics/Color/sRGB.html

    (define sRGB-log
      (lambda (sv)
        (if (<= sv 0.00304)
          (* 12.92 sv)
          (+ -0.055 (* 1.055 (expt sv 10/24))))))
    (define sRGB-exp
      (lambda (x)
        (if (<= x 0.03928)
          (/ x 12.92)
          (expt (/ (+ 0.055 x) 1.055) 2.4))))

    ;; Clipping as recommended by sRGB spec.
    ;@
    (define (CIEXYZ->sRGB XYZ)
      (map (lambda (sv)
             (exact (round (* 255 (sRGB-log (max 0 (min 1 sv)))))))
           (color:linear-transform RGB709:into-matrix XYZ)))
    (define (sRGB->CIEXYZ sRGB)
      (color:linear-transform
        RGB709:from-matrix
        (map sRGB-exp
             (map (lambda (b8v) (/ b8v 255.0)) sRGB))))

    ;;; sRGB values are sometimes written as 24-bit integers 0xRRGGBB
    ;@
    (define (xRGB->sRGB xRGB)
      (list (ash xRGB -16)
            (logand (ash xRGB -8) 255)
            (logand xRGB 255)))
    (define (sRGB->xRGB sRGB)
      (apply + (map * sRGB '(#x10000 #x100 #x1))))
    ;@
    (define (xRGB->CIEXYZ xRGB) (sRGB->CIEXYZ (xRGB->sRGB xRGB)))
    (define (CIEXYZ->xRGB xyz)   (sRGB->xRGB (CIEXYZ->sRGB xyz)))

    ;;;  http://www.pima.net/standards/it10/PIMA7667/PIMA7667-2001.PDF
    ;;;	    Photography - Electronic still picture imaging -
    ;;;		 Extended sRGB color encoding - e-sRGB

    (define e-sRGB-log
      (lambda (sv)
        (cond ((< sv -0.0031308)
               (- 0.055 (* 1.055 (expt (- sv) 10/24))))
              ((<= sv 0.0031308)
               (* 12.92 sv))
              (else (+ -0.055 (* 1.055 (expt sv 10/24)))))))
    (define e-sRGB-exp
      (lambda (x)
        (cond ((< x -0.04045)
               (- (expt (/ (- 0.055 x) 1.055) 2.4)))
              ((<= x 0.04045)
               (/ x 12.92))
              (else (expt (/ (+ 0.055 x) 1.055) 2.4)))))
    ;@
    (define (CIEXYZ->e-sRGB n XYZ)
      (define two^n-9 (ash 1 (- n 9)))
      (define offset (* 3 (ash 1 (- n 3))))
      (map (lambda (x)
             (+ (exact (round (* x 255 two^n-9))) offset))
           (map e-sRGB-log
                (color:linear-transform
                  RGB709:into-matrix
                  XYZ))))
    ;@
    (define (e-sRGB->CIEXYZ n rgb)
      (define two^n-9 (ash 1 (- n 9)))
      (define offset (* 3 (ash 1 (- n 3))))
      (color:linear-transform
        RGB709:from-matrix
        (map e-sRGB-exp
             (map (lambda (b8v) (/ (- b8v offset) 255.0 two^n-9))
                  rgb))))
    ;@
    (define (sRGB->e-sRGB n sRGB)
      (define two^n-9 (ash 1 (- n 9)))
      (define offset (* 3 (ash 1 (- n 3))))
      (map (lambda (x) (+ offset (* two^n-9 x))) sRGB))
    ;@
    (define (e-sRGB->sRGB n rgb)
      (define two^n-9 (ash 1 (- n 9)))
      (define offset (* 3 (ash 1 (- n 3))))
      (map (lambda (x) (/ (- x offset) two^n-9)) rgb))
    ;@
    (define (e-sRGB->e-sRGB n rgb m)
      (define shft (- m n))
      (cond ((zero? shft) rgb)
            (else (map (lambda (x) (ash x shft)) rgb))))

    ;;; From http://www.cs.rit.edu/~ncs/color/t_convert.html

    ;;; CIE 1976 L*a*b* is based directly on CIE XYZ and is an attampt to
    ;;; linearize the perceptibility of color differences. The non-linear
    ;;; relations for L*, a*, and b* are intended to mimic the logarithmic
    ;;; response of the eye. Coloring information is referred to the color
    ;;; of the white point of the system, subscript n.

    ;;;; L* is CIE lightness
    ;;;     L* = 116 * (Y/Yn)^1/3 - 16    for Y/Yn > 0.008856
    ;;;     L* = 903.3 * Y/Yn             otherwise

    (define (CIE:Y/Yn->L* Y/Yn)
      (if (> Y/Yn 0.008856)
        (+ -16 (* 116 (expt Y/Yn 1/3)))
        (* 903.3 Y/Yn)))
    (define (CIE:L*->Y/Yn L*)
      (cond ((<= L* (* 903.3 0.008856))
             (/ L* 903.3))
            ((<= L* 100.)
             (expt (/ (+ L* 16) 116) 3))
            (else 1)))

    ;;; a* = 500 * ( f(X/Xn) - f(Y/Yn) )
    ;;; b* = 200 * ( f(Y/Yn) - f(Z/Zn) )
    ;;;     where f(t) = t^1/3              for t > 0.008856
    ;;;           f(t) = 7.787 * t + 16/116 otherwise

    (define (ab-log t)
      (if (> t 0.008856)
        (expt t 1/3)
        (+ 16/116 (* t 7.787))))
    (define (ab-exp f)
      (define f3 (expt f 3))
      (if (> f3 0.008856)
        f3
        (/ (- f 16/116) 7.787)))
    ;@
    (define (CIEXYZ->L*a*b* XYZ . white-point)
      (apply (lambda (X/Xn Y/Yn Z/Zn)
               (list (CIE:Y/Yn->L* Y/Yn)
                     (* 500 (- (ab-log X/Xn) (ab-log Y/Yn)))
                     (* 200 (- (ab-log Y/Yn) (ab-log Z/Zn)))))
             (map / XYZ (if (null? white-point)
                          CIEXYZ:D65
                          (car white-point)))))

    ;;; Here Xn, Yn and Zn are the tristimulus values of the reference white.
    ;@
    (define (L*a*b*->CIEXYZ L*a*b* . white-point)
      (apply (lambda (Xn Yn Zn)
               (apply (lambda (L* a* b*)
                        (let* ((Y/Yn (CIE:L*->Y/Yn L*))
                               (fY/Yn (ab-log Y/Yn)))
                          (list (* Xn (ab-exp (+ fY/Yn (/ a* 500))))
                                (* Yn Y/Yn)
                                (* Zn (ab-exp (+ fY/Yn (/ b* -200)))))))
                      L*a*b*))
             (if (null? white-point)
               CIEXYZ:D65
               (car white-point))))

    ;;; XYZ to CIELUV

    ;;; CIE 1976 L*u*u* (CIELUV) is based directly on CIE XYZ and is another
    ;;; attampt to linearize the perceptibility of color differences.  L* is
    ;;; CIE lightness as for L*a*b* above.  The non-linear relations for u*
    ;;; and v* are:

    ;;;     u* =  13 L* ( u' - un' )
    ;;;     v* =  13 L* ( v' - vn' )

    ;;; The quantities un' and vn' refer to the reference white or the light
    ;;; source; for the 2.o observer and illuminant C, un' = 0.2009, vn' =
    ;;; 0.4610. Equations for u' and v' are given below:

    ;;;     u' = 4 X / (X + 15 Y + 3 Z)
    ;;;     v' = 9 Y / (X + 15 Y + 3 Z)

    (define (XYZ->uv XYZ)
      (apply (lambda (X Y Z)
               (define denom (+ X (* 15 Y) (* 3 Z)))
               (if (zero? denom)
                 '(4. 9.)
                 (list (/ (* 4 X) denom)
                       (/ (* 9 Y) denom))))
             XYZ))
    ;@
    (define (CIEXYZ->L*u*v* XYZ . white-point)
      (set! white-point (if (null? white-point)
                          CIEXYZ:D65
                          (car white-point)))
      (let* ((Y/Yn (/ (cadr XYZ) (cadr white-point)))
             (L* (CIE:Y/Yn->L* Y/Yn)))
        (cons L* (map (lambda (q) (* 13 L* q))
                      (map - (XYZ->uv XYZ) (XYZ->uv white-point))))))

    ;;; CIELUV to XYZ

    ;;; The transformation from CIELUV to XYZ is performed as following:

    ;;;     u' = u / ( 13 L* ) + un
    ;;;     v' = v / ( 13 L* ) + vn
    ;;;     X = 9 Y u' / 4 v'
    ;;;     Z = ( 12 Y - 3 Y u' - 20 Y v' ) / 4 v'
    ;@
    (define (L*u*v*->CIEXYZ L*u*v* . white-point)
      (set! white-point (if (null? white-point)
                          CIEXYZ:D65
                          (car white-point)))
      (apply (lambda (un vn)
               (apply (lambda (L* u* v*)
                        (if (not (positive? L*))
                          '(0. 0. 0.)
                          (let* ((up (+ (/ u* 13 L*) un))
                                 (vp (+ (/ v* 13 L*) vn))
                                 (Y (* (CIE:L*->Y/Yn L*) (cadr white-point))))
                            (list (/ (* 9 Y up) 4 vp)
                                  Y
                                  (/ (* Y (+ 12 (* -3 up) (* -20 vp))) 4 vp)))))
                      L*u*v*))
             (XYZ->uv white-point)))

    ;;; http://www.inforamp.net/~poynton/PDFs/coloureq.pdf

    (define pi (* 4 (atan 1)))
    (define pi/180 (/ pi 180))
    ;@
    (define (L*a*b*->L*C*h lab)
      (define h (/ (atan (caddr lab) (cadr lab)) pi/180))
      (list (car lab)
            (sqrt (apply + (map * (cdr lab) (cdr lab))))
            (if (negative? h) (+ 360 h) h)))
    ;@
    (define (L*C*h->L*a*b* lch)
      (apply (lambda (L* C* h)
               (set! h (* h pi/180))
               (list L*
                     (* C* (cos h))
                     (* C* (sin h))))
             lch))
    ;@
    (define (L*a*b*:DE* lab1 lab2)
      (sqrt (apply + (map (lambda (x) (* x x)) (map - lab1 lab2)))))

    ;;; http://www.colorpro.com/info/data/cie94.html

    (define (color:process-params parametric-factors)
      (define ans
        (case (length parametric-factors)
          ((0) #f)
          ((1) (if (list? parametric-factors)
                 (apply color:process-params parametric-factors)
                 (append parametric-factors '(1 1))))
          ((2) (append parametric-factors '(1)))
          ((3) parametric-factors)
          (else (slib:error 'parametric-factors 'too-many parametric-factors))))
      (and ans
           (for-each (lambda (obj)
                       (if (not (number? obj))
                         (slib:error 'parametric-factors 'not 'number? obj)))
                     ans))
      ans)
    ;;; http://www.brucelindbloom.com/index.html?Eqn_DeltaE_CIE94.html
    ;@
    (define (L*a*b*:DE*94 lab1 lab2 . parametric-factors)
      (define (square x) (* x x))
      (let ((C1 (sqrt (apply + (map square (cdr lab1)))))
            (C2 (sqrt (apply + (map square (cdr lab2))))))
        (define dC^2 (square (- C1 C2)))
        (sqrt (apply + (map /
                            (list (square (- (car lab1) (car lab2)))
                                  dC^2
                                  (- (apply + (map square
                                                   (map - (cdr lab1) (cdr lab2))))
                                     dC^2))
                            (list 1			 ; S_l
                                  (+ 1 (* .045 C1))	 ; S_c
                                  (+ 1 (* .015 C1))) ; S_h
                            (or (color:process-params parametric-factors)
                                '(1 1 1)))))))

    ;;; CMC-DE is designed only for small color-differences.  But try to do
    ;;; something reasonable for large differences.  Use bisector (h*) of
    ;;; the hue angles if separated by less than 90.o; otherwise, pick h of
    ;;; the color with larger C*.
    ;@
    (define (CMC-DE lch1 lch2 . parametric-factors)
      (apply (lambda (L* C* h_)		;Geometric means
               (let ((ang1 (* pi/180 (caddr lch1)))
                     (ang2 (* pi/180 (caddr lch2))))
                 (cond ((>= 90 (abs (/ (atan (sin (- ang1 ang2))
                                             (cos (- ang1 ang2)))
                                       pi/180)))
                        (set! h_ (/ (atan (+ (sin ang1) (sin ang2))
                                          (+ (cos ang1) (cos ang2)))
                                    pi/180)))
                       ((>= (cadr lch1) (cadr lch2)) (caddr lch1))
                       (else (caddr lch2))))
               (let* ((C*^4 (expt C* 4))
                      (f    (sqrt (/ C*^4 (+ C*^4 1900))))
                      (T    (if (and (> h_ 164) (< h_ 345))
                              (+ 0.56 (abs (* 0.2 (cos (* (+ h_ 168) pi/180)))))
                              (+ 0.36 (abs (* 0.4 (cos (* (+ h_ 35) pi/180)))))))
                      (S_l  (if (< L* 16)
                              0.511
                              (/ (* 0.040975 L*) (+ 1 (* 0.01765 L*)))))
                      (S_c  (+ (/ (* 0.0638 C*) (+ 1 (* 0.0131 C*))) 0.638))
                      (S_h  (* S_c (+ (* (+ -1 T) f) 1))))
                 (sqrt (apply
                         + (map /
                                (map (lambda (x) (* x x)) (map - lch1 lch2))
                                (list S_l S_c S_h)
                                (or (color:process-params parametric-factors)
                                    '(2 1 1)))))))
             (map sqrt (map * lch1 lch2))))

    ;;; Chromaticity
    ;@
    (define (XYZ->chromaticity XYZ)
      (define sum (apply + XYZ))
      (list (/ (car XYZ) sum) (/ (cadr XYZ) sum)))
    ;@
    (define (chromaticity->CIEXYZ x y)
      (list x y (- 1 x y)))
    (define (chromaticity->whitepoint x y)
      (list (/ x y) 1 (/ (- 1 x y) y)))
    ;@
    (define (XYZ->xyY XYZ)
      (define sum (apply + XYZ))
      (if (zero? sum)
        '(0 0 0)
        (list (/ (car XYZ) sum) (/ (cadr XYZ) sum) (cadr XYZ))))
    ;@
    (define (xyY->XYZ xyY)
      (define x (car xyY))
      (define y (cadr xyY))
      (if (zero? y)
        '(0 0 0)
        (let ((Y/y (/ (caddr xyY) y)))
          (list (* Y/y x) (caddr xyY) (* Y/y (- 1 x y))))))
    ;@
    (define (xyY:normalize-colors lst . n-in)
      (define (nthcdr n lst) (if (zero? n) lst (nthcdr (+ -1 n) (cdr lst))))
      (define Ys (map caddr lst))
      (let ((n (if (null? n-in) 1 (car n-in))))
        (let ((max-Y (if (positive? n)
                       (* n (apply max Ys))
                       (let ()
                         (apply max (nthcdr (- n) (sort Ys >=)))))))
          (map (lambda (xyY)
                 (let ((x (max 0 (car xyY)))
                       (y (max 0 (cadr xyY))))
                   (define sum (max 1 (+ x y)))
                   (list (/ x sum)
                         (/ y sum)
                         (max 0 (min 1 (/ (caddr xyY) max-Y))))))
               lst))))

    ;;;  http://www.aim-dtp.net/aim/technology/cie_xyz/cie_xyz.htm:
    ;;;  Illuminant D65                           0.312713 0.329016
    ;; (define CIEXYZ:D65 (chromaticity->whitepoint 0.312713 0.329016))
    ;; (define CIEXYZ:D65 (chromaticity->whitepoint 0.3127 0.3290))
    ;@
    (define CIEXYZ:D50 (chromaticity->whitepoint 0.3457 0.3585))

    ;;; With its 16-bit resolution, e-sRGB-16 is extremely sensitive to
    ;;; whitepoint.  Even the 6 digits of precision specified above is
    ;;; insufficient to make (color->e-srgb 16 d65) ==> (57216 57216 57216)
    ;@
    (define CIEXYZ:D65 (e-sRGB->CIEXYZ 16 '(57216 57216 57216)))

    ;;; http://www.efg2.com/Lab/Graphics/Colors/Chromaticity.htm CIE 1931:
    ;@
    (define CIEXYZ:A (chromaticity->whitepoint 0.44757 0.40745)) ; 2856.K
    (define CIEXYZ:B (chromaticity->whitepoint 0.34842 0.35161)) ; 4874.K
    (define CIEXYZ:C (chromaticity->whitepoint 0.31006 0.31616)) ; 6774.K
    (define CIEXYZ:E (chromaticity->whitepoint 1/3 1/3)) ; 5400.K

    ;;; Converting spectra
    (define cie:x-bar #f)
    (define cie:y-bar #f)
    (define cie:z-bar #f)
    ;@
    (define (setup-ciexyz)
      (set! cie:x-bar (make-vector 80))
      (set! cie:y-bar (make-vector 80))
      (set! cie:z-bar (make-vector 80))
      (do ((wlen 380 (+ 5 wlen))
           (idx 0 (+ 1 idx))
           (data *cie1931* (cdr data)))
        ((>= wlen 780) )
        (when (not (eqv? wlen (caar data)))
          (slib:error *cie1931* 'expected wlen 'not (caar data)))
        (vector-set! cie:x-bar idx (list-ref 1 (car data)))
        (vector-set! cie:y-bar idx (list-ref 2 (car data)))
        (vector-set! cie:z-bar idx (list-ref 3 (car data)))))
  
    ;@
    (define (read-cie-illuminant path)
      (define siv (make-vector 107))
      (call-with-input-file path
                            (lambda (iprt)
                              (do ((idx 0 (+ 1 idx)))
                                ((>= idx 107) siv)
                                (vector-set! siv idx (read iprt))))))
    ;@
    (define (read-normalized-illuminant path)
      (define siv (read-cie-illuminant path))
      (let ((yw (/ (cadr (spectrum->XYZ siv 300e-9 830e-9)))))
        (illuminant-map (lambda (w x) (* x yw)) siv)))
    ;@
    (define (illuminant-map proc siv)
      (define prod (make-vector 107))
      (do ((idx 106 (+ -1 idx))
           (w 830e-9 (+ -5e-9 w)))
        ((negative? idx) prod)
        (vector-set! prod idx (proc w (vector-ref siv idx)))))
    ;@
    (define (illuminant-map->XYZ proc siv)
      (spectrum->XYZ (illuminant-map proc siv) 300e-9 830e-9))
    ;@
    (define (wavelength->XYZ wl)
      (if (not cie:y-bar) (setup-ciexyz))
      (set! wl (- (/ wl 5.e-9) 380/5))
      (if (<= 0 wl (+ -1 400/5))
        (let* ((wlf (exact (floor wl)))
               (res (- wl wlf)))
          (define (interpolate vect idx res)
            (+ (* (- 1 res) (vector-ref vect idx))
               (* res (vector-ref vect (+ 1 idx)))))
          (list (interpolate cie:x-bar wlf res)
                (interpolate cie:y-bar wlf res)
                (interpolate cie:z-bar wlf res)))
        (slib:error 'wavelength->XYZ 'out-of-range wl)))
    (define (wavelength->chromaticity wl)
      (XYZ->chromaticity (wavelength->XYZ wl)))
    ;@
    (define (spectrum->XYZ . args)
      (define x 0)
      (define y 0)
      (define z 0)
      (if (not cie:y-bar) (setup-ciexyz))
      (case (length args)
        ((1)
         (set! args (car args))
         (do ((wvln 380.e-9 (+ 5.e-9 wvln))
              (idx 0 (+ 1 idx)))
           ((>= idx 80) (map (lambda (x) (/ x 80)) (list x y z)))
           (let ((inten (args wvln)))
             (set! x (+ x (* (vector-ref cie:x-bar idx) inten)))
             (set! y (+ y (* (vector-ref cie:y-bar idx) inten)))
             (set! z (+ z (* (vector-ref cie:z-bar idx) inten))))))
        ((3)
         (let* ((vect (if (list? (car args)) (list->vector (car args)) (car args)))
                (vlen (vector-length vect))
                (x1 (cadr args))
                (x2 (caddr args))
                (xinc (/ (- x2 x1) (+ -1 vlen)))
                (x->j (lambda (x) (exact (round (/ (- x x1) xinc)))))
                (x->k (lambda (x) (exact (round (/ (- x 380.e-9) 5.e-9)))))
                (j->x (lambda (j) (+ x1 (* j xinc))))
                (k->x (lambda (k) (+ 380.e-9 (* k 5.e-9))))
                (xlo (max (min x1 x2) 380.e-9))
                (xhi (min (max x1 x2) 780.e-9))
                (jhi (x->j xhi))
                (khi (x->k xhi))
                (jinc (if (negative? xinc) -1 1)))
           (if (<= (abs xinc) 5.e-9)
             (do ((wvln (j->x (x->j xlo)) (+ wvln (abs xinc)))
                  (jdx (x->j xlo) (+ jdx jinc)))
               ((>= jdx jhi)
                (let ((nsmps (abs (- jhi (x->j xlo)))))
                  (map (lambda (x) (/ x nsmps)) (list x y z))))
               (let ((ciedex (min 79 (x->k wvln)))
                     (inten (vector-ref vect jdx)))
                 (set! x (+ x (* (vector-ref cie:x-bar ciedex) inten)))
                 (set! y (+ y (* (vector-ref cie:y-bar ciedex) inten)))
                 (set! z (+ z (* (vector-ref cie:z-bar ciedex) inten)))))
             (do ((wvln (k->x (x->k xlo)) (+ wvln 5.e-9))
                  (kdx (x->k xlo) (+ kdx 1)))
               ((>= kdx khi)
                (let ((nsmps (abs (- khi (x->k xlo)))))
                  (map (lambda (x) (/ x nsmps)) (list x y z))))
               (let ((inten (vector-ref vect (x->j wvln))))
                 (set! x (+ x (* (vector-ref cie:x-bar kdx) inten)))
                 (set! y (+ y (* (vector-ref cie:y-bar kdx) inten)))
                 (set! z (+ z (* (vector-ref cie:z-bar kdx) inten))))))))
        (else (slib:error 'spectrum->XYZ 'wna args))))
    (define (spectrum->chromaticity . args)
      (XYZ->chromaticity (apply spectrum->XYZ args)))
    ;@
    (define blackbody-spectrum
      (let* ((c 2.998e8)
             (h 6.626e-34)
             (h*c (* h c))
             (k 1.381e-23)
             (pi*2*h*c*c (* 2 pi h*c c)))
        (lambda (temp . span)
          (define h*c/kT (/ h*c k temp))
          (define pi*2*h*c*c*span
            (* pi*2*h*c*c (if (null? span) 1.e-9 (car span))))
          (lambda (x)
            (/ pi*2*h*c*c*span
               (expt x 5)
               (- (exp (/ h*c/kT x)) 1))))))
    ;@
    (define (temperature->XYZ temp . span)
      (spectrum->XYZ (apply blackbody-spectrum temp span)))	;was .5e-9
    (define (temperature->chromaticity temp)
      (XYZ->chromaticity (temperature->XYZ temp)))

;;; "cie1931.xyz" CIE XYZ(1931) Spectra from 380.nm to 780.nm.
(define *cie1931* '((380 0.0014 0.0000 0.0065)
                    (385 0.0022 0.0001 0.0105)
                    (390 0.0042 0.0001 0.0201)
                    (395 0.0076 0.0002 0.0362)
                    (400 0.0143 0.0004 0.0679)
                    (405 0.0232 0.0006 0.1102)
                    (410 0.0435 0.0012 0.2074)
                    (415 0.0776 0.0022 0.3713)
                    (420 0.1344 0.0040 0.6456)
                    (425 0.2148 0.0073 1.0391)
                    (430 0.2839 0.0116 1.3856)
                    (435 0.3285 0.0168 1.6230)
                    (440 0.3483 0.0230 1.7471)
                    (445 0.3481 0.0298 1.7826)
                    (450 0.3362 0.0380 1.7721)
                    (455 0.3187 0.0480 1.7441)
                    (460 0.2908 0.0600 1.6692)
                    (465 0.2511 0.0739 1.5281)
                    (470 0.1954 0.0910 1.2876)
                    (475 0.1421 0.1126 1.0419)
                    (480 0.0956 0.1390 0.8130)
                    (485 0.0580 0.1693 0.6162)
                    (490 0.0320 0.2080 0.4652)
                    (495 0.0147 0.2586 0.3533)
                    (500 0.0049 0.3230 0.2720)
                    (505 0.0024 0.4073 0.2123)
                    (510 0.0093 0.5030 0.1582)
                    (515 0.0291 0.6082 0.1117)
                    (520 0.0633 0.7100 0.0782)
                    (525 0.1096 0.7932 0.0573)
                    (530 0.1655 0.8620 0.0422)
                    (535 0.2257 0.9149 0.0298)
                    (540 0.2904 0.9540 0.0203)
                    (545 0.3597 0.9803 0.0134)
                    (550 0.4334 0.9950 0.0087)
                    (555 0.5121 1.0000 0.0057)
                    (560 0.5945 0.9950 0.0039)
                    (565 0.6784 0.9786 0.0027)
                    (570 0.7621 0.9520 0.0021)
                    (575 0.8425 0.9154 0.0018)
                    (580 0.9163 0.8700 0.0017)
                    (585 0.9786 0.8163 0.0014)
                    (590 1.0263 0.7570 0.0011)
                    (595 1.0567 0.6949 0.0010)
                    (600 1.0622 0.6310 0.0008)
                    (605 1.0456 0.5668 0.0006)
                    (610 1.0026 0.5030 0.0003)
                    (615 0.9384 0.4412 0.0002)
                    (620 0.8544 0.3810 0.0002)
                    (625 0.7514 0.3210 0.0001)
                    (630 0.6424 0.2650 0.0000)
                    (635 0.5419 0.2170 0.0000)
                    (640 0.4479 0.1750 0.0000)
                    (645 0.3608 0.1382 0.0000)
                    (650 0.2835 0.1070 0.0000)
                    (655 0.2187 0.0816 0.0000)
                    (660 0.1649 0.0610 0.0000)
                    (665 0.1212 0.0446 0.0000)
                    (670 0.0874 0.0320 0.0000)
                    (675 0.0636 0.0232 0.0000)
                    (680 0.0468 0.0170 0.0000)
                    (685 0.0329 0.0119 0.0000)
                    (690 0.0227 0.0082 0.0000)
                    (695 0.0158 0.0057 0.0000)
                    (700 0.0114 0.0041 0.0000)
                    (705 0.0081 0.0029 0.0000)
                    (710 0.0058 0.0021 0.0000)
                    (715 0.0041 0.0015 0.0000)
                    (720 0.0029 0.0010 0.0000)
                    (725 0.0020 0.0007 0.0000)
                    (730 0.0014 0.0005 0.0000)
                    (735 0.0010 0.0004 0.0000)
                    (740 0.0007 0.0002 0.0000)
                    (745 0.0005 0.0002 0.0000)
                    (750 0.0003 0.0001 0.0000)
                    (755 0.0002 0.0001 0.0000)
                    (760 0.0002 0.0001 0.0000)
                    (765 0.0001 0.0000 0.0000)
                    (770 0.0001 0.0000 0.0000)
                    (775 0.0001 0.0000 0.0000)
                    (780 0.0000 0.0000 0.0000)))

  ;;; "cie1964.xyz" CIE XYZ(1964) Spectra from 380.nm to 780.nm.
  (define *cie1964* '((380 0.0002 0.0000 0.0007)
                      (385 0.0007 0.0001 0.0029)
                      (390 0.0024 0.0003 0.0105)
                      (395 0.0072 0.0008 0.0323)
                      (400 0.0191 0.0020 0.0860)
                      (405 0.0434 0.0045 0.1971)
                      (410 0.0847 0.0088 0.3894)
                      (415 0.1406 0.0145 0.6568)
                      (420 0.2045 0.0214 0.9725)
                      (425 0.2647 0.0295 1.2825)
                      (430 0.3147 0.0387 1.5535)
                      (435 0.3577 0.0496 1.7985)
                      (440 0.3837 0.0621 1.9673)
                      (445 0.3867 0.0747 2.0273)
                      (450 0.3707 0.0895 1.9948)
                      (455 0.3430 0.1063 1.9007)
                      (460 0.3023 0.1282 1.7454)
                      (465 0.2541 0.1528 1.5549)
                      (470 0.1956 0.1852 1.3176)
                      (475 0.1323 0.2199 1.0302)
                      (480 0.0805 0.2536 0.7721)
                      (485 0.0411 0.2977 0.5701)
                      (490 0.0162 0.3391 0.4153)
                      (495 0.0051 0.3954 0.3024)
                      (500 0.0038 0.4608 0.2185)
                      (505 0.0154 0.5314 0.1592)
                      (510 0.0375 0.6067 0.1120)
                      (515 0.0714 0.6857 0.0822)
                      (520 0.1177 0.7618 0.0607)
                      (525 0.1730 0.8233 0.0431)
                      (530 0.2365 0.8752 0.0305)
                      (535 0.3042 0.9238 0.0206)
                      (540 0.3768 0.9620 0.0137)
                      (545 0.4516 0.9822 0.0079)
                      (550 0.5298 0.9918 0.0040)
                      (555 0.6161 0.9991 0.0011)
                      (560 0.7052 0.9973 0.0000)
                      (565 0.7938 0.9824 0.0000)
                      (570 0.8787 0.9556 0.0000)
                      (575 0.9512 0.9152 0.0000)
                      (580 1.0142 0.8689 0.0000)
                      (585 1.0743 0.8256 0.0000)
                      (590 1.1185 0.7774 0.0000)
                      (595 1.1343 0.7204 0.0000)
                      (600 1.1240 0.6583 0.0000)
                      (605 1.0891 0.5939 0.0000)
                      (610 1.0305 0.5280 0.0000)
                      (615 0.9507 0.4618 0.0000)
                      (620 0.8563 0.3981 0.0000)
                      (625 0.7549 0.3396 0.0000)
                      (630 0.6475 0.2835 0.0000)
                      (635 0.5351 0.2283 0.0000)
                      (640 0.4316 0.1798 0.0000)
                      (645 0.3437 0.1402 0.0000)
                      (650 0.2683 0.1076 0.0000)
                      (655 0.2043 0.0812 0.0000)
                      (660 0.1526 0.0603 0.0000)
                      (665 0.1122 0.0441 0.0000)
                      (670 0.0813 0.0318 0.0000)
                      (675 0.0579 0.0226 0.0000)
                      (680 0.0409 0.0159 0.0000)
                      (685 0.0286 0.0111 0.0000)
                      (690 0.0199 0.0077 0.0000)
                      (695 0.0138 0.0054 0.0000)
                      (700 0.0096 0.0037 0.0000)
                      (705 0.0066 0.0026 0.0000)
                      (710 0.0046 0.0018 0.0000)
                      (715 0.0031 0.0012 0.0000)
                      (720 0.0022 0.0008 0.0000)
                      (725 0.0015 0.0006 0.0000)
                      (730 0.0010 0.0004 0.0000)
                      (735 0.0007 0.0003 0.0000)
                      (740 0.0005 0.0002 0.0000)
                      (745 0.0004 0.0001 0.0000)
                      (750 0.0003 0.0001 0.0000)
                      (755 0.0002 0.0001 0.0000)
                      (760 0.0001 0.0000 0.0000)
                      (765 0.0001 0.0000 0.0000)
                      (770 0.0001 0.0000 0.0000)
                      (775 0.0000 0.0000 0.0000)
                      (780 0.0000 0.0000 0.0000)))

    ))

