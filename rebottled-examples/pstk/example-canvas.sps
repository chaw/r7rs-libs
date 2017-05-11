
;; PS-TK example: display frame + canvas

(import (scheme base) 
        (rebottled pstk))

(define tk (tk-start))
(ttk-map-widgets 'all) ; make sure we are using tile widget set

(tk/wm 'title tk "PS-Tk Example: Canvas")

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
  ; draw onto canvas
  (canvas 'configure 'background: 'white)
  (canvas 'create 'line 0 0 20 50)
  (canvas 'create 'line 20 50 150 100 'dash: "2 4")
  (canvas 'create 'oval 100 200 200 250 'fill: 'blue)
  (canvas 'create 'polygon 250 200 280 200 300 230 300 280 250 240 250 200)
  (canvas 'create 'rectangle 200 300 250 370 'fill: 'green 'outline: 'red)
  (canvas 'create 'text 300 20 'text: "Some text" 'anchor: 'w))

(tk-event-loop tk)

