;; Port of tklib examples/plotchart/test-histogram.tcl

(import (scheme base)
        (rebottled pstk)
        (rebottled pstk-plotchart))

(define tk (tk-start))
(import-plotchart)

(tk/wm 'title tk "Test Histogram")

(define canvas (tk 'create-widget 'canvas
                   'width: 400 'height: 300
                   'bg: 'white))
(tk/pack canvas)

(define p (create-histogram canvas '(0 100 20) '(0 50 10)))
(p 'dataconfig 'data
   'style: 'filled
   'fillcolour: 'cyan
   'width: 2
   'colour: 'blue)
(p 'plot 'data 0.0 10.0)
(p 'plot 'data 20.0 10.0)
(p 'plot 'data 40.0  3.0)
(p 'plot 'data 45.0  6.0)
(p 'plot 'data 55.0 26.0)
(p 'plot 'data 67.0 24.0)

(tk-event-loop tk)
