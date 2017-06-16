;; An analog clock, translating the Tcl example at http://wiki.tcl.tk/1011
;; (Note: due to the time-coding used, this only displays GMT)

(import (scheme base)
        (scheme inexact)
        (scheme time)
        (rebottled pstk)
        (only (robin constants) PI)
        (slib time-core))

;; Draws the hands on the canvas using the current time, and repeats each second
(define (hands canvas)
  (canvas 'delete 'withtag "hands")

  (let* ((time (time:gmtime (current-second)))
         (second-angle (* (vector-ref time 0) (* 2 PI 1/60)))
         (minute-angle (* (vector-ref time 1) (* 2 PI 1/60)))
         (hour-angle (* (vector-ref time 2) (* 2 PI 1/12))))
    (canvas 'create 'line ; second hand
            100 100 
            (+ 100 (* 90 (sin second-angle)))
            (- 100 (* 90 (cos second-angle)))
            'width: 1 'tags: "hands")
    (canvas 'create 'line ; minute hand
            100 100 
            (+ 100 (* 85 (sin minute-angle)))
            (- 100 (* 85 (cos minute-angle)))
            'width: 3 
            'capstyle: "projecting"
            'tags: "hands")
    (canvas 'create 'line ; hour hand
            100 100 
            (+ 100 (* 60 (sin hour-angle)))
            (- 100 (* 60 (cos hour-angle)))
            'width: 7 
            'capstyle: "projecting"
            'tags: "hands"))
  (tk/after 1000 (lambda () (hands canvas))))

;; Create the initial frame, clock frame and hours
(let ((tk (tk-start)))
  (tk/wm 'title tk "GMT Clock")

  (let ((canvas (tk 'create-widget 'canvas)))
    (tk/pack canvas)
    (canvas 'configure 'height: 200 'width: 200)
    (canvas 'create 'oval 2 2 198 198 'fill: "white" 'outline: "black")
    (do ((h 1 (+ 1 h)))
      ((> h 12) )
      (let ((angle (- (/ PI 2) (* h PI 1/6))))
        (canvas 'create 'text 
                (+ 100 (* 90 (cos angle)))
                (- 100 (* 90 (sin angle)))
                'text: (number->string h)
                'font: "{Helvetica -12}")))

    (hands canvas))
  (tk-event-loop tk))
