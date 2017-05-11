
(import (scheme base)
	(srfi 42)
	(rebottled pstk))

(let ((tk (tk-start)))
  (tk/wm 'title tk "PS/Tk Example: Label relief")
  (tk 'configure 'borderwidth: 10)

  (do-ec (: relief '("raised" "sunken" "flat" "ridge" "groove" "solid"))
	 (tk/pack 
	   (tk 'create-widget 'label 'text: relief
	       'relief: relief
	       'bd: 2   ; width of the relief
	       'padx: 10 ; inner padding, between label and its relief
;	       'width: 10 ; hardcode desired size of label
;	       'height: 5
;	       'anchor: "nw" ; indicate position for text of label
	       )
	   'side: 'left 'padx: 4))

  (tk-event-loop tk))

