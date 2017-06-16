
(import (scheme base)
        (srfi 42)
        (rebottled pstk))

;; set up the graphical display
(let ((tk (tk-start)))
  (tk/wm 'title tk "Example")
  (tk 'configure 'width: 300 'height: 200)
  (let ((canvas (tk 'create-widget 'canvas 'width: 300 'height: 200))
        (image (tk/image 'create 'photo 'width: 300 'height: 200)))
    ; clear the image
    (tk-eval (string-append image " blank"))
    ; draw a line across the centre
    (do-ec (: i 300)
           (tk-eval (string-append image 
                                   " put red -to " 
                                   (number->string i) 
                                   " 120")))
    ; place image in canvas
    (canvas 'create 'image 0 0 'image: image 'anchor: 'nw)
    ; put canvas on window
    (tk/pack canvas))

  (tk-event-loop tk))


