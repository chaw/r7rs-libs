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
                                    '(Q1 Q2 Q3 Q4) 
                                    '(0 30 2) 
                                    'stacked))
(bar-chart 'plot 'yr2014 '(4 2 3 5) 'green)
(bar-chart 'plot 'yr2015 '(7 6 3 8) 'red)
(bar-chart 'plot 'yr2016 '(6 4 7 9) 'blue)
(bar-chart 'title "Quarterly Sales")
(bar-chart 'legend 'yr2014 "2014")
(bar-chart 'legend 'yr2015 "2015")
(bar-chart 'legend 'yr2016 "2016")

(tk/pack canvas)

(tk-event-loop tk)

