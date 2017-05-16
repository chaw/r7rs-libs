;; Timer example
;; from http://www.linuxplanet.com/linuxplanet/tutorials/6708/2
;; Listing 5 translated into Scheme

(import (scheme base)
        (scheme process-context)
        (rebottled pstk))

(let* ((tk (tk-start))
       (scale (tk 'create-widget 'scale 'from: 1 'to: 45
                'orient: 'horizontal))
       (show-alert (lambda () 
                     (tk/bell)
                     (tk/message-box 'title: "Ready!"
                                     'message: "DING DING DING!")
                     (exit))))
  (tk/grid (tk 'create-widget
               'label 'text: "Minutes:")
           'row: 0 'column: 0)
  (tk/grid scale 'row: 0 'column: 1)
  (tk/grid (tk 'create-widget
               'button 'text: "Start timing"
               'command: (lambda () (tk/after (* (string->number (scale 'get)) 6000) 
                                              show-alert)))
           'row: 1 'column: 1 'pady: 5 'sticky: 'E)
  (tk-event-loop tk))

