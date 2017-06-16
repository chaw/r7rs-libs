
;; PS-TK example: display frame + label

(import (scheme base) 
        (rebottled pstk))

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Label")

(let ((label (tk 'create-widget 'label 'text: "Hello")))
  (tk/place label 'height: 20 'width: 50 'x: 10 'y: 20))

(tk-event-loop tk)

