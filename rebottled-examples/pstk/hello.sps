(import (scheme base)
        (scheme write)
        (rebottled pstk))

(let ((tk (tk-start)))
  (tk/pack (tk 'create-widget 
               'button 'text: "Hello"
               'command: (lambda () (display "Hello world") (newline)))
           'padx: 20 'pady: 20)
  (tk-event-loop tk))
