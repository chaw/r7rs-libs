;; Simple xy plot example: scatter plot
;; -- data from http://www.inconstantmoon.com/not_cat1.htm

(import (scheme base)
        (rebottled pstk)
        (rebottled pstk-plotchart))

(define tk (tk-start))
(import-plotchart)

(define canvas (tk 'create-widget 'canvas
                   'background: 'white
                   'width: 400
                   'height: 400))

(define graph (create-xy-plot canvas
                              '(0 20000 4000)
                              '(0 100 20)))
;; label the graph and axes
(graph 'title "Crater Diameter vs Depth")
(graph 'xtext "Depth (feet)")
(graph 'ytext "Diameter (miles)")
;;
(graph 'dataconfig "values" 'type: 'symbol 'symbol: 'upfilled 'colour: "blue")
(graph 'dataconfig "relation" 'colour: "green")
;; add data
(for-each (lambda (pair)
            (graph 'plot "values" (car pair) (cdr pair))
            (graph 'trend "relation" (car pair) (cdr pair)))
          '((14500 . 81) (2000 . 2) (10500 . 73) (7000 . 51) (16000 . 77)))

(tk/pack canvas)

;; finally save the plot
(graph 'saveplot "craters.ps")

(tk-event-loop tk)

