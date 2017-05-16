;; PS-TK example: display frame + menu

(import (scheme base) 
        (rebottled pstk))

(define tk (tk-start))
(ttk-map-widgets 'all) ; make sure we are using tile widget set

(tk/wm 'title tk "PS-Tk Example: Menu")
(tk 'configure 'width: 300 'height: 200)

(let* ((menubar (tk 'create-widget 'menu))
       (main-menu (tk 'create-widget 'menu 'tearoff: 0 ))
       (second-menu (tk 'create-widget 'menu 'tearoff: 0))
       (sub-menu (tk 'create-widget 'menu 'tearoff: 0)))
  (menubar 'add 'cascade 'menu: main-menu 'label: "Main" 'underline: 0)
  (menubar 'add 'cascade 'menu: second-menu 'label: "Second" 'underline: 0)

  (main-menu 'add 'command 'label: "About" 'underline: 0
	     'command: (lambda () (tk/message-box 'title: "About" 
						  'message: "About me"
						  'type: 'ok)))
  (main-menu 'add 'separator)
  (main-menu 'add 'command 'label: "Exit" 'underline: 1
	     'command: tk-end)

  (tk-var 'check-value)
  (second-menu 'add 'command 'label: "Show value"
	       'command: (lambda () (tk/message-box 'title: "Menu Item"
						    'message: (if (string=? "1" (tk-get-var 'check-value))
								"Menu item checked"
								"Menu item not checked")
						    'type: 'ok)))
  (second-menu 'add 'checkbutton 'label: "Checkable" 
	       'variable: (tk-var 'check-value))

  (second-menu 'add 'separator)
  (second-menu 'add 'cascade 'menu: sub-menu 'label: "Sub")
  (second-menu 'add 'separator)

  (tk-var 'radio-value)
  (second-menu 'add 'command 'label: "Show value"
		 'command: (lambda ()
			     (tk/message-box 'title: "Radio value"
					     'message: (tk-get-var 'radio-value)
					     'type: 'ok)))
    (second-menu 'add 'radiobutton 'label: "Red" 
		 'underline: 0 'value: "Red"
		 'variable: (tk-var 'radio-value))
    (second-menu 'add 'radiobutton 'label: "Green"
		 'underline: 0 'value: "Green"
		 'variable: (tk-var 'radio-value))
    (second-menu 'add 'radiobutton 'label: "Blue" 
		 'underline: 0 'value: "Blue"
		 'variable: (tk-var 'radio-value))
    (tk-set-var! 'radio-value "Red")

  (sub-menu 'add 'command 'label: "Sub menu item"
	    'command: (lambda () (tk/message-box 'title: "sub menu item"
						 'message: "Sub menu item"
						 'type: 'ok)))

  (sub-menu 'add 'separator)

  (let ((subradio "None selected"))
    (sub-menu 'add 'radiobutton 'label: "Mercury" 'variable: "y"
	      'underline: 0
	      'command: (lambda () (set! subradio "Mercury")))
    (sub-menu 'add 'radiobutton 'label: "Venus" 'variable: "y"
	      'underline: 0
	      'command: (lambda () (set! subradio "Venus"))))

  (tk 'configure 'menu: menubar))


(tk-event-loop tk)

