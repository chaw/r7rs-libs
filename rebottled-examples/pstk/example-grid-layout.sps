;; PS-TK example: display frame with buttons in grid

(import (scheme base) 
	(srfi 42)
        (rebottled pstk)
	(slib format))

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Grid layout")

(do-ec (: i 5)
       (: j 5)
       (tk/grid 
	 (tk 'create-widget 'button 
	     'text: (format #f "Button ~d, ~d" i j)
	     'command: (lambda () 
			 (format #t "Clicked button ~d, ~d~&" i j)))
	 'column: i 'row: j
	 'padx: 5 'pady: 2))

(tk-event-loop tk)

