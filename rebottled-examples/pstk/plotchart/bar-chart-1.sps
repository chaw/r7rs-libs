;; Simple bar chart example

(import (scheme base)
        (rebottled pstk)
        (rebottled pstk-plotchart))

(define tk (tk-start))
(import-plotchart)

(define canvas (tk 'create-widget 'canvas
                   'background: 'white
                   'width: 400
                   'height: 400))

(define bar-chart (create-bar-chart canvas 
                                    '(Venus Earth Mars Pluto) 
                                    '(0 6 2) 
                                    1))
(bar-chart 'plot 'mydata '(0 1 2 5) 'green)
(bar-chart 'title "Number of Moons")
(bar-chart 'legend 'mydata "Moons")

(tk/pack canvas)

(tk-event-loop tk)

