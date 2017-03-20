;;;
;;; pdf-util.ss -- PDF drawing primitives
;;;
;;; Copy of the original licence from Marc Battyani:
;;;
;;;
;;;  cl-pdf is a Common Lisp library for generating PDF files.
;;;
;;;  It is distributed under a FreeBSD style license
;;;  (if you want another license contact me) marc.battyani@fractalconcept.com
;;;
;;;  Copyright (c) 2002 Marc Battyani. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without modification, are
;;;  permitted provided that the following conditions are met:
;;;
;;;  Redistributions of source code must retain the above copyright notice, this list of
;;;  conditions and the following disclaimer.
;;;
;;;  Redistributions in binary form must reproduce the above copyright notice, this list of
;;;  conditions and the following disclaimer in the documentation and/or other materials 
;;;  provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE MARC BATTYANI ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
;;;  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARC BATTYANI OR
;;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;;;  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;;  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  The latest version is at http://www.fractalconcept.com/asp/html/cl-pdf.html
;;;  You can contact me at marc.battyani@fractalconcept.com or marc@battyani.net

;;;
;;; Author: Bruce Butterfield <bab@entricom.com>
;;;
;;; Commentary:
;;;
;;; The port from Common Lisp was done as "Scheme-ishly" as possible; most of the changes 
;;; from the original code involved mapping CLOS objects to structures and associated
;;; functions. I would have used the PLT class library but I wanted to be able to use this
;;; code in other Scheme implementations; structures/records are a bit more universal.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (rebottled cl-pdf-utils)
  (export 
    *page-stream* ; this is a parameter object
    set-page-stream
    in-text-mode
    set-font
    move-to-next-line
    draw-text
    move-text
    draw-text-on-next-line
    set-text-rendering-mode
    set-char-spacing
    set-text-x-scale
    set-text-leading
    set-text-rise
    set-text-matrix
    draw-and-adjust-string
    escape
    with-saved-state
    rotate
    translate
    scale
    set-line-width
    set-line-cap
    set-line-join
    set-dash-pattern
    set-mitter-limit
    move-to
    line-to
    bezier-to
    bezier2-to
    bezier3-to
    close-path
    basic-rect
    stroke
    close-and-stroke
    fill-path
    close-and-fill
    even-odd-fill
    fill-and-stroke
    even-odd-fill-and-stroke
    close-fill-and-stroke
    close-even-odd-fill-and-stroke
    end-path-no-op
    clip-path
    even-odd-clip-path
    set-gray-stroke
    set-gray-fill
    set-rgb-stroke
    set-rgb-fill
    set-cymk-stroke
    set-cymk-fill
    arc
    pie
    circle
    ellipse
    rectangle
    regular-polygon
    star
    )
  (import (scheme base) 
          (scheme case-lambda)
          (scheme inexact)
          (rebottled pregexp)
          (slib common-list-functions)
          (slib format)
          (only (robin statistics) sign))

  (begin

    (define *page-stream* (make-parameter #f))

    (define (set-page-stream port)
      (*page-stream* port))

    ;; text functions

    (define-syntax in-text-mode
      (syntax-rules ()
                    ((_ arg ...)
                     (begin
                       (format (*page-stream*) "BT~%")
                       arg ...
                       (format (*page-stream*) "ET~%")))))

    (define (set-font font size)
      (format (*page-stream*) "~a ~,2f Tf~%" font size)) 

    (define-syntax define-pdf-op
      (syntax-rules ()
                    ((_ name tmpl)
                     (define name
                       (lambda ()
                         (format (*page-stream*) tmpl)
                         (format (*page-stream*) "~%"))))
                    ((_ name (arg ...) tmpl)
                     (define name
                       (lambda (arg ...)
                         (format (*page-stream*) tmpl arg ...))))))

    (define-pdf-op move-to-next-line " T*~%")
    (define-pdf-op draw-text (str) "(~a) Tj~%")
    (define-pdf-op move-text (dx dy) "~,3f ~,3f Td~%") 
    (define-pdf-op draw-text-on-next-line (string) "(~a) '~%")
    (define-pdf-op set-text-rendering-mode (mode) "~d Tr~%")
    (define-pdf-op set-char-spacing (space) "~,3f Tc~%")
    (define-pdf-op set-text-x-scale (scale) "~,3f Tz~%")
    (define-pdf-op set-text-leading (space) "~,3f TL~%")
    (define-pdf-op set-text-rise (rise) "~,3f Ts~%")
    (define-pdf-op set-text-matrix (a b c d e f) "~,3f ~,3f ~,3f ~,3f ~,3f ~,3f Tm~%")

    (define (draw-and-adjust-string strings)
      (format (*page-stream*) "[ ")
      (for-each
        (lambda (str)
          (if (number? str)
            (format (*page-stream*) "~,3f " str)
            (format (*page-stream*) "(~a) " str)))
        strings)
      (format (*page-stream*) "] TJ"))


    ;; escape special characters in strings
    (define escape-table 
      (map (lambda (x y)
             (cons (pregexp x) y))
           '("\\(" "\\)" "\\\\")
           '("\\\\(" "\\\\)" "\\\\\\")))

    (define (escape str)
      (fold-right (lambda (esc str)
                    (pregexp-replace* (car esc) str (cdr esc)))
                  str
                  escape-table))

    ;; graphic functions

    (define-syntax with-saved-state
      (syntax-rules ()
                    ((_ arg ...)
                     (begin
                       (format (*page-stream*) "q~%")
                       arg ...
                       (format (*page-stream*) "Q~%")))))

    (define (rotate deg)
      (let* ((angle (/ (* PI deg) 180.0))
             (s (sin angle))
             (c (cos angle)))
        (format (*page-stream*) "~,3f ~,3f ~,3f ~,3f 0.0 0.0 cm~%" c s (- s) c)))

    (define-pdf-op translate (dx dy) "1.0 0.0 0.0 1.0 ~,3f ~,3f cm~%")
    (define-pdf-op scale (sx sy) " ~,3f 0.0 0.0 ~,3f 0.0 0.0 cm~%")
    (define-pdf-op set-line-width (width) "~,3f w~%")
    (define-pdf-op set-line-cap (mode) "~d J~%")
    (define-pdf-op set-line-join (mode) "~d j~%")
    (define-pdf-op set-dash-pattern (dash-array phase) "[~{~d~^ ~}] ~d~%")
    (define-pdf-op set-mitter-limit (limit) "~,3f M~%")
    (define-pdf-op move-to (x y) "~,3f ~,3f m~%")
    (define-pdf-op line-to (x y) "~,3f ~,3f l~%")
    (define-pdf-op bezier-to (x1 y1 x2 y2 x3 y3) "~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c~%")
    (define-pdf-op bezier2-to (x2 y2 x3 y3) "~,3f ~,3f ~,3f ~,3f v~%")
    (define-pdf-op bezier3-to (x1 y1 x3 y3) "~,3f ~,3f ~,3f ~,3f y~%")
    (define-pdf-op close-path " h")
    (define-pdf-op basic-rect (x y dx dy) "~,3f ~,3f ~,3f ~,3f re~%")
    (define-pdf-op stroke " S")
    (define-pdf-op close-and-stroke " s")
    (define-pdf-op fill-path " f")
    (define-pdf-op close-and-fill " h f")
    (define-pdf-op even-odd-fill " f*")
    (define-pdf-op fill-and-stroke " B")
    (define-pdf-op even-odd-fill-and-stroke " B*")
    (define-pdf-op close-fill-and-stroke " b")
    (define-pdf-op close-even-odd-fill-and-stroke " b*")
    (define-pdf-op end-path-no-op " n")
    (define-pdf-op clip-path " W")
    (define-pdf-op even-odd-clip-path " W*")
    (define-pdf-op set-gray-stroke (gray) "~,3f G~%")
    (define-pdf-op set-gray-fill (gray) "~,3f g~%")
    (define-pdf-op set-rgb-stroke (r g b) "~,3f ~,3f ~,3f RG~%")
    (define-pdf-op set-rgb-fill (r g b) "~,3f ~,3f ~,3f rg~%")
    (define-pdf-op set-cymk-stroke (c y m k) "~,3f ~,3f ~,3f ~,3f K~%")
    (define-pdf-op set-cymk-fill (c y m k) "~,3f ~,3f ~,3f ~,3f k~%")

    ;; geometry

    (define PI 3.141527)
    (define *2pi* (* 2 PI))
    (define *pi/2* (/ PI 2))

    (define (arc center-x center-y radius start extent)
      (move-to (+ center-x (* radius (cos start)))
               (+ center-y (* radius (sin start))))
      (arc-to center-x center-y radius start extent)
      (line-to center-x center-y))

    (define (pie center-x center-y radius start extent)
      (move-to center-x center-y)
      (line-to (+ center-x (* radius (cos start)))
               (+ center-y (* radius (sin start))))
      (arc-to center-x center-y radius start extent)
      (line-to center-x center-y))

    (define (circle center-x center-y radius)
      (move-to (+ center-x radius) center-y)
      (arc-to center-x center-y radius 0 *2pi*))

    (define (ellipse center-x center-y radius-a radius-b)
      (move-to (+ center-x radius-a) center-y)
      (let ((kappa (* 4 (/ (- (sqrt 2) 1) 3.0))))
        (bezier-to (+ center-x radius-a) (+ center-y (* kappa radius-b))
                   (+ center-x (* kappa radius-a)) (+ center-y radius-b)
                   center-x (+ center-y radius-b))
        (bezier-to (- center-x (* kappa radius-a)) (+ center-y radius-b)
                   (- center-x radius-a) (+ center-y (* kappa radius-b))
                   (- center-x radius-a) center-y)
        (bezier-to (- center-x radius-a) (- center-y (* kappa radius-b))
                   (- center-x (* kappa radius-a)) (- center-y radius-b)
                   center-x (- center-y radius-b))
        (bezier-to (+ center-x (* kappa radius-a)) (- center-y radius-b)
                   (+ center-x radius-a) (- center-y (* kappa radius-b))
                   (+ center-x radius-a) center-y)))

    (define (rectangle x y dx dy radius)
      (if (zero? radius)
        (basic-rect x y dx dy)
        (begin
          (move-to (+ x dx) (- (+ y dy) radius))
          (polyline (list (list x y) (list (+ x dx) y)
                          (list (+ x dx) (+ y dy)) (list x (+ y dy)))
                    radius #t))))

    (define-syntax dotimes
      (syntax-rules ()
                    ((_ (index maxval) body ...)
                     (do ((index 0 (+ index 1)))
                       ((= index maxval))
                       body ...))))

    (define (polyline points radius closed)
      (define (last-pair items)
        (list (car (reverse items))))
      (let ((x-coord (lambda (pt) (car (car pt))))
            (y-coord (lambda (pt) (cadr (car pt)))))
        (if (zero? radius)
          (let ((x1 (x-coord points))
                (y1 (y-coord points)))
            (move-to x1 y1)
            (let loop ((point (cdr points)))
              (if (not (null? point))
                (begin
                  (line-to (x-coord point) (y-coord point))
                  (loop (cdr point)))))
            (if closed
              (line-to x1 y1))))
        (begin
          (if closed
            (let ((break-point (midpoint (car points) (car (last-pair points)) 0.5)))
              (set! points `(,break-point ,@points ,break-point))))
          (move-to (x-coord points) (y-coord points))
          (dotimes (i (- (length points) 2))
            (let ((p1 (list-ref points i))
                  (p2 (list-ref points (+ 1 i)))
                  (p3 (list-ref points (+ 2 i))))
              (fillet p2 p1 p3 radius)))
          (line-to (x-coord (last-pair points))
                   (y-coord (last-pair points))))))

    (define regular-polygon
      (case-lambda
        ((center-x center-y radius sides fillet-radius)
         (polyline
           (let ((step-angle (/ *2pi* sides)))
             (do ((current-angle *2pi* (+ current-angle step-angle))
                  (side 0 (+ side 1))
                  (lst '()))
               ((> side sides) lst)
               (set! lst (cons (list (+ center-x (* radius (cos current-angle)))
                                     (+ center-y (* radius (sin current-angle))))
                               lst))))
           fillet-radius #t))
        ((center-x center-y radius sides)
         (regular-polygon center-x center-y radius sides 0))))

    (define star
      (case-lambda
        ((center-x center-y ext-radius int-radius sides fillet-radius)
         (let* ((current-angle *pi/2*)
                (step-angle (/ *2pi* sides))
                (half-step (/ step-angle 2.0))
                (points '()))
           (dotimes (i sides)
             (set! points 
               (cons (list (+ center-x (* ext-radius (cos current-angle)))
                           (+ center-y (* ext-radius (sin current-angle))))
                     points))
             (set! points
               (cons (list (+ center-x (* int-radius (cos (+ current-angle half-step))))
                           (+ center-y (* int-radius (sin (+ current-angle half-step)))))
                     points))
             (set! current-angle (+ current-angle step-angle)))
           (polyline points fillet-radius #t)))
        ((center-x center-y ext-radius int-radius sides)
         (star center-x center-y ext-radius int-radius sides 0))))


    ;;; Non exported functions

    (define (arc-to center-x center-y radius start extent)
      ;; An arc of extent zero will generate an error at bezarc (divide by zero).
      ;; This case may be given by two aligned points in a polyline.
      ;; Better do nothing.
      (unless (zero? extent)
        (if (<= (abs extent) (/ PI 2.0))
          (let-values (((x1 y1 x2 y2 x3 y3)
                        (bezarc center-x center-y radius start extent)))
                      (bezier-to x1 y1 x2 y2 x3 y3))
          (let ((half-extent (/ extent 2.0)))
            (arc-to center-x center-y radius start half-extent)
            (arc-to center-x center-y radius (+ start half-extent) half-extent)))))

    (define (bezarc center-x center-y radius start extent)
      ;; start and extent should be in radians.
      ;; Returns first-control-point-x first-control-point-y
      ;;         second-control-point-x second-control-point-y
      ;;         end-point-x end-point-y
      (let* ((end (+ start extent))
             (s-start (sin start)) (c-start (cos start))
             (s-end (sin end)) (c-end (cos end))
             (ang/2 (/ extent 2.0))
             (kappa (* (/ 4.0 3.0)
                       (/ (- 1 (cos ang/2))
                          (sin ang/2))))
             (x1 (- c-start (* kappa s-start)))
             (y1 (+ s-start (* kappa c-start)))
             (x2 (+ c-end   (* kappa s-end)))
             (y2 (- s-end   (* kappa c-end))))
        (values (+ (* x1 radius) center-x) (+ (* y1 radius) center-y)
                (+ (* x2 radius) center-x) (+ (* y2 radius) center-y)
                (+ (* c-end radius) center-x) (+ (* s-end radius) center-y))))


    (define (distance p1 p2)
      (sqrt (+ (expt (- (car p2)  (car p1))  2)
               (expt (- (cadr p2) (cadr p1)) 2))))

    (define (angle2 p1 p2)
      (if (zero? (distance p1 p2))
        0.0
        (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1)))))

    ;;;============================================================================;
    ;;;
    ;;; (angle-3points <point> <point> <point>)
    ;;;
    ;;; Devuelve el angulo en radianes entre tres puntos.  Se considera el punto
    ;;; 'pt1' como vertice del angulo.  El rango del angulo de salida es [+Pi -Pi)
    ;;;

    (define (angle-3points pt1 pt2 pt3)
      (let* ((ang (- (angle2 pt1 pt3) (angle2 pt1 pt2))))
        (if (or (> ang PI) (<= ang (- PI)))
          (- ang (* (sign ang) *2pi*))
          ang)))


    ;;;============================================================================;
    ;;;
    ;;; (midpoint <point> <point> <real>)
    ;;;
    ;;; Devuelve un punto situado entre los dos que se dan como argumento. El
    ;;; factor de posición indica la relación de las distancias entre los puntos
    ;;; de entrada y el de salida.
    ;;;

    (define (midpoint pt1 pt2 ratio)
      (let ((x1 (car pt1))(y1 (cadr pt1))
            (x2 (car pt2))(y2 (cadr pt2)))
        (list (+ x1 (* ratio (- x2 x1)))
              (+ y1 (* ratio (- y2 y1))))))

    ;; This function is the support to create rounded polylines
    ;;
    ;; p1 = corner
    ;; p2 = start
    ;; p3 = end
    ;; -> no useful return value
    (define (fillet p1 p2 p3 radius)
      (let* ((gamma (/ (abs (angle-3points p1 p2 p3)) 2))
             (dist-p1-t (/ radius (tan gamma)))
             (dist-p1-s (/ (sqrt (+ (expt radius 2) (expt dist-p1-t 2)))
                           (cos gamma)))
             (dist-p1-p2 (distance p1 p2))
             (dist-p1-p3 (distance p1 p3)))
        (if (or (< dist-p1-p2 dist-p1-t)
                (< dist-p1-p3 dist-p1-t))
          ;; Radius is too large, so we aren't going to draw the arc.
          (line-to (car p1) (cadr p1))
          ;; Else, draw the arc.
          (let ((t2 (midpoint p1 p2 (/ dist-p1-t dist-p1-p2)))
                (t3 (midpoint p1 p3 (/ dist-p1-t dist-p1-p3)))
                (center (midpoint (midpoint p1 p2 (/ dist-p1-s dist-p1-p2))
                                  (midpoint p1 p3 (/ dist-p1-s dist-p1-p3))
                                  0.5)))
            (line-to (car t2) (cadr t2))
            (arc-to (car center) (cadr center) radius
                    (angle2 center t2) (angle-3points center t2 t3))))))

    )) ; end of library

