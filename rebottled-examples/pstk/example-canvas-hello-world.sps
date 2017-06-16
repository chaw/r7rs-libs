
(import (scheme base)
	(rebottled pstk))

(let ((tk (tk-start)))
  (ttk-map-widgets 'all)
  (tk/wm 'title tk "PS-Tk Example: Canvas Hello World")

  ;; create canvas with two scroll bars
  (let ((canvas (tk 'create-widget 'canvas))
	(hscroll (tk 'create-widget 'scrollbar 'orient: 'horizontal))
	(vscroll (tk 'create-widget 'scrollbar 'orient: 'vertical)))
    (tk/grid canvas 'column: 0 'row: 0 'sticky: 'news)
    (tk/grid hscroll 'column: 0 'row: 1 'sticky: 'we)
    (tk/grid vscroll 'column: 1 'row: 0 'sticky: 'ns)
    ; register canvas and scrollbars with each other
    (hscroll 'configure 'command: (list canvas 'xview))
    (vscroll 'configure 'command: (list canvas 'yview))
    (canvas 'configure 'xscrollcommand: (list hscroll 'set))
    (canvas 'configure 'yscrollcommand: (list vscroll 'set))
    ; ensure grid fills the frame
    (tk/grid 'columnconfigure tk 0 'weight: 1)
    (tk/grid 'rowconfigure tk 0 'weight: 1)

    ; set size of canvas 
    (canvas 'configure 'scrollregion: (list 0 0 500 500))

    ; place draggable item on canvas
    (canvas 'configure 'background: 'white)
    (canvas 'create 'text 50 50 'text: "Hello World" 'tag: 'movable)
    (canvas 'create 'text 100 50 'text: "Hi" 'tag: 'movable2)
    (canvas 'bind 'movable "<Button-1>" (lambda args (display "click") 
					  (display args) (newline)))
    (canvas 'bind 'movable2 "<Button-1>" (lambda args (display "click 2")))
    (tk-event-loop tk)))

