
;; PS-Tk example: Temperature conversion (no 'sticky: ???)

(import (scheme base) 
	(rebottled pstk)) 


(define (celsius->fahrenheit item)
  (let ((number (string->number item)))
    (if (number? number)
      (inexact (+ (* number 9/5) 32))
      0.0)))

(define tk (tk-start)) 
(tk/wm 'title tk "Celsius to Fahrenheit")

(let* ((celsius (tk 'create-widget 'entry))
       (label (tk 'create-widget 'label))
       (button (tk 'create-widget 'button
		   'text: 'Calculate
		   'command: (lambda () 
			       (label 'configure 
				      'text: (number->string (celsius->fahrenheit (celsius 'get))))))))
  ; layout widgets in a grid
  (tk/grid celsius 'column: 2 'row: 1 'padx: 5 'pady: 5)
  (tk/grid label 'column: 2 'row: 2 'padx: 5 'pady: 5)
  (tk/grid button 'column: 2 'row: 3 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "celsius") 
	   'column: 3 'row: 1 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "is") 
	   'column: 1 'row: 2 'padx: 5 'pady: 5)
  (tk/grid (tk 'create-widget 'label 'text: "fahrenheit") 
	   'column: 3 'row: 2 'padx: 5 'pady: 5)

  (tk-event-loop tk))

