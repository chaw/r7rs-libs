
;; PS-TK example: display frame + button

(import (scheme base) 
        (scheme write)
        (rebottled pstk))

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Button")
(tk 'configure 'width: 300 'height: 200)

(let ((button (tk 'create-widget 'button 'text: "Click me"
		 'command: (lambda () (display "Clicked ") (flush-output-port)))))
  (tk/place button 'height: 20 'width: 70 'x: 10 'y: 20))

(tk-event-loop tk)

