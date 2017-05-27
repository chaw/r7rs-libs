;; Timer example
;; from http://www.linuxplanet.com/linuxplanet/tutorials/6708/3
;; Listing 6 translated into Scheme

(import (scheme base)
        (scheme process-context)
        (rebottled pstk))

(let* ((tk (tk-start))
       (scale (tk 'create-widget 'scale 'from: 1 'to: 45
                'orient: 'horizontal))
       (message-window (lambda ()
                         (let* ((win (tk 'create-widget 'toplevel))
                                (b (win 'create-widget 'button
                                       'text: "DING DING DING"
                                       'bg: "blue"
                                       'fg: "yellow"
                                       'activebackground: "red"
                                       'activeforeground: "white"
                                       'padx: (/ (string->number (tk/winfo 'screenwidth tk)) 3)
                                       'pady: (/ (string->number (tk/winfo 'screenheight tk)) 3)
                                       'command: exit)))
                           (tk/pack b)
                           (tk-event-loop tk))))
       (show-alert (lambda () 
                     (tk/bell)
                     (message-window)
                     (exit))))
  (tk/grid (tk 'create-widget
               'label 'text: "Minutes:")
           'row: 0 'column: 0)
  (tk/grid scale 'row: 0 'column: 1)
  (tk/grid (tk 'create-widget
               'button 'text: "Start timing"
               'command: (lambda () (tk/after (* (string->number (scale 'get)) 600) 
                                              show-alert)))
           'row: 1 'column: 1 'pady: 5 'sticky: 'E)
  (tk-event-loop tk))

