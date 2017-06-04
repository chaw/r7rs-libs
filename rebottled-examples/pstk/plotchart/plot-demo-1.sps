;; Port of tklib examples/plotchart/plotdemos1.tcl

(import (scheme base)   
        (scheme inexact)
        (rebottled pstk)
        (rebottled pstk-plotchart)
        (robin constants)
        (scheme list)
        (srfi 27))

(define tk (tk-start)) 
(import-plotchart)

;; Create the plots displayed in the root frame
(define (example-1)
  (define (xy-plot)
    (let* ((c (tk 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-xy-plot c '(0.0 100.0 10.0) '(0.0 100.0 20.0)))
           (r (create-right-axis c '(0.0 0.1 0.01)))
           (xd 5.0)
           (yd 20.0)
           (xold 0.0)
           (yold 50.0))
      (s 'dataconfig "series1" 'colour: "red")
      (s 'dataconfig "series2" 'colour: "blue")
      (s 'dataconfig "series3" 'colour: "magenta")
      ;; add some data for the three series
      (do ((i 0 (+ i 1))
           (xnew (+ xold xd) (+ xnew xd))
           (ynew (+ yold (* yd (- (random-real) 0.5))) (+ ynew (* yd (- (random-real) 0.5))))
           (ynew2 (+ yold (* 2.0 yd (- (random-real) 0.5))) (+ ynew (* 2.0 yd (- (random-real) 0.5)))))
        ((= i 20) )
        (s 'plot "series1" xnew ynew)
        (s 'plot "series2" xnew ynew2)
        (s 'trend "series3" xnew ynew2))
      ;; tweaks and labels
      (s 'interval "series2" 50.0 40.0 60.0 52.0) ; point with error bar
      (s 'interval "series2" 60.0 40.0 60.0) ; error bar
      (s 'xtext "X-coordinate")
      (s 'ytext "Y-coordinate")
      (r 'ytext "Right axis")
      (s 'title "Aha!")
      ;; Data for the right axis
      (r 'dataconfig 'right 'type: 'both 'symbol: 'circle 'colour: 'green)
      (r 'plot 'right 10.0 0.01)
      (r 'plot 'right 30.0 0.03)
      (r 'plot 'right 40.0 0.02)
      c))
  ;
  (define (pie-chart)
    (let* ((c (tk 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-pie-chart c)))
      (s 'plot '("Long names" 10 "Short names" 30 "Average" 40 "Ultra-short names" 5))
      (s 'title "Okay - this works")
      c))
  ;
  (define (polar-plot)
    (let* ((c (tk 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-polar-plot c '(3.0 1.0))))
      (do ((angle 0 (+ angle 10.0)))
        ((>= angle 360) )
        (s 'plot "cardiod" (+ 1.0 (cos (* angle PI (/ 180)))) angle))
      (s 'title "Cardiod")
      c))
  ;
  (define (tx-plot)
    (let* ((c (tk 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-tx-plot c '("2006-01-01" "2007-01-01" 120) '(0.0 100.0 20.0))))
      (s 'dataconfig 'series1 'colour: "red")
      (s 'dataconfig 'series2 'colour: "blue")
      (s 'xtext "Time")
      (s 'ytext "Data")
      (s 'xticklines)
      ; add series 1
      (s 'plot 'series1 "2006-02-01" 10.0)
      (s 'plot 'series1 "2006-02-11" 50.0)
      (s 'plot 'series1 "2006-03-01" 50.0)
      (s 'plot 'series1 "2006-07-01" 40.0)
      (s 'plot 'series1 "2006-08-21" 20.0)
      (s 'plot 'series1 "2006-08-22"  1.0)
      (s 'plot 'series1 "2006-12-11" 78.0)
      ; add series 2
      (s 'plot 'series2 "2006-03-01" 110.0)
      (s 'plot 'series2 "2006-04-11"  50.0)
      (s 'plot 'series2 "2006-07-28"  20.0)
      (s 'plot 'series2 "2006-10-21"  99.0)
      (s 'plot 'series2 "2006-11-22"   1.0)
      (s 'plot 'series2 "2006-12-31"  78.0)
      ; finished, return canvas
      c))
  ;
  (tk/pack (xy-plot) (pie-chart) (polar-plot) (tx-plot)
           'fill: 'both 'side: 'top))

(define (example-2)
  (define frame (tk 'create-widget 'toplevel))
  ;
  (define (bar-chart-1)
    (let* ((c (frame 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-bar-chart c '(A B C D E) '(0.0 10.0 2.0) 2.5)))
      (s 'plot 'series1 '(1.0 4.0 6.0 1.0 7.0) 'red)
      (s 'plot 'series2 '(0.0 3.0 7.0 9.3 2.0) 'green)
      (s 'title "Arbitrary Data")
      (s 'legend 'series1 "Series 1")
      (s 'legend 'series2 "Series 2" 20) ; extra spacing
      c))
  ;
  (define (bar-chart-2)
    (let* ((c (frame 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-bar-chart c '(A B C D E) '(0.0 20.0 5.0) 'stacked)))
      (s 'plot 'series1 '(1.0 4.0 6.0 1.0 7.0) 'red)
      (s 'plot 'series2 '(0.0 3.0 7.0 9.3 2.0) 'green)
      (s 'title "Stacked Diagram")
      c))
  ;
  (tk/wm 'title frame "Bar Charts")
  (tk/pack (bar-chart-1) (bar-chart-2)))

(define (example-3)
    (define frame (tk 'create-widget 'toplevel))
  ;
  (define (bar-chart-1)
    (let* ((c (frame 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-horizontal-bar-chart c '(0.0 10.0 2.0) '(A B C D E) 2)))
      (s 'plot 'series1 '(1.0 4.0 6.0 1.0 7.0) 'red)
      (s 'plot 'series2 '(0.0 3.0 7.0 9.3 2.0) 'green)
      (s 'title "Arbitrary Data")
      c))
  ;
  (define (bar-chart-2)
    (let* ((c (frame 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-horizontal-bar-chart c '(0.0 20.0 5.0) '(A B C D E) 'stacked)))
      (s 'plot 'series1 '(1.0 4.0 6.0 1.0 7.0) 'red)
      (s 'plot 'series2 '(0.0 3.0 7.0 9.3 2.0) 'green)
      (s 'title "Stacked Diagram")
      c))
  ;
  (define (time-chart)
    (let* ((c (frame 'create-widget 'canvas 'background: 'white 'width: 400 'height: 200))
           (s (create-time-chart c "1 january 2004" "31 december 2004" 4)))
      (s 'period "Spring" "1 march 2004" "1 june 2004" 'green)
      (s 'period "Summer" "1 june 2004" "1 september 2004" 'yellow)
      (s 'vertline "1 jan" "1 january 2004")
      (s 'vertline "1 apr" "1 april 2004")
      (s 'vertline "1 jul" "1 july 2004")
      (s 'vertline "1 oct" "1 october 2004")
      (s 'milestone "Longest day" "21 july 2004")
      (s 'title "Seasons (northern hemisphere)")
      c))
  ;
  (tk/wm 'title frame "More Charts")
  (tk/pack (bar-chart-1) (bar-chart-2) (time-chart)))

(define (example-4)
  (define frame (tk 'create-widget 'toplevel))
  ;
  (define (threed-plot-1)
    (define (cowboyhat x y)
      (let ((x1 (/ x 9.0))
            (y1 (/ y 9.0)))
        (* 3.0 
           (- 1.0 (+ (square x1) (square y1)))
           (- 1.0 (+ (square x1) (square y1))))))
    ;
    (let* ((c (frame 'create-widget 'canvas 'bg: 'white 'width: 400 'height: 300))
           (s (create-3d-plot c '(0 10 3) '(-10 10 10) '(0 10 2.5))))
      (s 'title "3D plot")
      ;; (s 'plotfunc cowboyhat) ;; TODO: plotfunc does not call back to the Scheme function
      ; -- so create the data explicitly
      (let ((xs (iota 11 0 1))
            (ys (iota 11 -10 2)))
        (s 'plotdata (map (lambda (r) 
                            (map (lambda (c) (cowboyhat r c))
                                 xs))
                          ys)))
      c))
  
  (define (threed-plot-2)
    (let* ((c (frame 'create-widget 'canvas 'bg: 'white 'width: 400 'height: 250))
           (s (create-3d-plot c '(0 10 3) '(-10 10 10) '(0 10 2.5))))
      (s 'title "3D plot - data")
      (s 'colour 'green 'black)
      (s 'plotdata '((1.0 2.0 1.0 0.0) 
                     (1.1 3.0 1.1 -0.5) 
                     (3.0 1.0 4.0 5.0)))
      c))
  ;
  (tk/wm 'title frame "3D Plots")
  (tk/pack (threed-plot-1) (threed-plot-2)))

;; run the examples
(example-1)
(example-2)
(example-3)
(example-4)

;; don't forget to start the loop to display everything
(tk-event-loop tk)

