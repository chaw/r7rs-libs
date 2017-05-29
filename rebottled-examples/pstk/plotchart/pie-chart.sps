;; Simple pie chart example

(import (scheme base)
        (rebottled pstk)
        (rebottled pstk-plotchart))

(define tk (tk-start))
(import-plotchart)

(define canvas (tk 'create-widget 'canvas
                   'background: 'white
                   'width: 400
                   'height: 400))

(define pie-chart (create-pie-chart canvas)) 
(pie-chart 'plot '("Earth" 1 "Mars" 2 "Jupiter" 53 "Saturn" 53 "Uranus" 27 "Neptune" 13 "Pluto" 5))
(pie-chart 'title "Number of Moons")

(tk/pack canvas)

(tk-event-loop tk)

