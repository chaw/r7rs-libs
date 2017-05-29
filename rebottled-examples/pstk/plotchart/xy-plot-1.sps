;; Simple xy plot example: shows two functions

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
                              '(-10 10 2)
                              '(-100 100 20)))
;; label the graph and axes
(graph 'title "Two Functions")
(graph 'xtext "input x")
(graph 'ytext "output y")
;; colour the two lines
(graph 'dataconfig "square" 'colour: 'blue)
(graph 'dataconfig "cube" 'colour: 'green)
;; line labels in the legend
(graph 'legendconfig 'position: 'bottom-right)
(graph 'legend "square" "x*x")
(graph 'legend "cube" "x*x*x")
;; add data
(do ((i -10 (+ i 0.1)))
  ((> i 10) )
  (graph 'plot "square" i (square i))
  (graph 'plot "cube" i (* i i i)))

(tk/pack canvas)

(tk-event-loop tk)

