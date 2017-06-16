;; Timer example
;; from http://www.linuxplanet.com/linuxplanet/tutorials/6708/1
;; Listing 3 translated into Scheme

(import (scheme base)
        (scheme process-context)
        (rebottled pstk))

(let ((tk (tk-start)))
  (tk/pack (tk 'create-widget
               'label 'text: "Minutes:")
           'side: 'left)
  (tk/pack (tk 'create-widget
                'scale 'from: 1 'to: 45
                'orient: 'horizontal))
  (tk/pack (tk 'create-widget
               'button 'text: "Start timing"
               'command: (lambda () (exit))))
  (tk-event-loop tk))

