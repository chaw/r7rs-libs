;; Timer example
;; from http://www.linuxplanet.com/linuxplanet/tutorials/6708/1
;; Listing 4 translated into Scheme

(import (scheme base)
        (scheme process-context)
        (rebottled pstk))

(let ((tk (tk-start)))
  (tk/grid (tk 'create-widget
               'label 'text: "Minutes:")
           'row: 0 'column: 0)
  (tk/grid (tk 'create-widget
                'scale 'from: 1 'to: 45
                'orient: 'horizontal)
           'row: 0 'column: 1)
  (tk/grid (tk 'create-widget
               'button 'text: "Start timing"
               'command: (lambda () (exit)))
           'row: 1 'column: 1 'pady: 5 'sticky: 'E)
  (tk-event-loop tk))

