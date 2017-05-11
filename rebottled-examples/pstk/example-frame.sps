
;; PS-TK example: display simple frame

(import (scheme base) 
        (rebottled pstk))

(define tk (tk-start))
(tk/wm 'title tk "PS-Tk Example: Frame")

(tk-event-loop tk)

