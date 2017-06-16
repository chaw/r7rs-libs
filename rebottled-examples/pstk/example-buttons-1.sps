
;;; Example of buttons from Welch, p.386
;;; Note: Scheme scoping rules compared with 'trouble' of tcl's
;;; But: can a scheme variable be used/updated like a tcl variable???

(import (scheme base)
	(rebottled pstk))

(define (run-example vals)
  (let* ((x 1)
	 (tk (tk-start))
	 (label (tk 'create-widget 'label 'text: (number->string x)))
	 (update-x! (lambda (val) 
		      (set! x (* x val))
		      (label 'configure 'text: (number->string x)))))
    (tk/wm 'title tk "PS-Tk Example: Buttons 1")

    (tk/pack label 'side: 'top 'pady: 10)

    (for-each (lambda (val)
		(tk/pack
		  (tk 'create-widget 'button 'text: (number->string val)
		      'command: (lambda () (update-x! val)))
		  'side: 'left 'padx: 10 'pady: 10))
	      vals)

    (tk-event-loop tk)))

(define (run-example-tk vals)
  (tk-var 'x)
  (tk-set-var! 'x 1)
  (let ((tk (tk-start)))
    (tk/wm 'title tk "PS-Tk Example: Buttons 1")
    (tk/pack (tk 'create-widget 'label 'textvariable: (tk-var 'x))
	     'side: 'top 'pady: 10)
    (for-each (lambda (val)
		(tk/pack
		  (tk 'create-widget 'button 'text: val
		      'command: (lambda () (tk-set-var! 'x val)))
		  'side: 'left 'padx: 10 'pady: 10))
	      vals)
    (tk-event-loop tk)))

(run-example '(2 4 6 8))

