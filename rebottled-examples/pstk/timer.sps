;; Timer example
;; from http://www.linuxplanet.com/linuxplanet/tutorials/6708/1
;; translated into Scheme

(import (scheme base)
        (scheme process-context)
        (rebottled pstk))

(let ((tk (tk-start)))
  (tk/pack (tk 'create-widget
               'button 'text: "Start timing"
               'command: (lambda () (exit)))
           'side: "left")
  (tk-event-loop tk))

