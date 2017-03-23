(import (except (scheme base) equal?)
        (scheme char)
        (scheme file)
        (scheme read)
        (slib common)
        (slib eps-graph)
        (slib line-io)
        (slib string-port)
        (srfi 63))

(define *data*
  (string-append
    "0.3050	9.2	0.05	0.0000	9.5	0.05	0.0000\n"
    "0.3100	40.8	0.25	0.0003	42.3	0.26	0.0003\n"
    "0.3150	103.9	0.77	0.0008	107.8	0.80	0.0008\n"
    "0.3200	174.4	1.64	0.0017	181.0	1.70	0.0017\n"
    "0.3250	237.9	2.83	0.0029	246.9	2.94	0.0029\n"
    "0.3300	381.0	4.74	0.0049	395.4	4.92	0.0049\n"
    "0.3350	376.0	6.62	0.0069	390.2	6.87	0.0069\n"
    "0.3400	419.5	8.71	0.0090	435.4	9.04	0.0090\n"
    "0.3450	423.0	10.83	0.0112	439.0	11.24	0.0112\n"
    "0.3500	466.2	104.33	0.0149	483.8	14.87	0.0149\n"
    "0.8500	46.2	246.33	0.0149	483.8	14.87	0.0149\n"
    "1.3500	6.2	544.33	0.0149	483.8	14.87	0.0149\n"
    "2.3500	3.1	714.33	0.0149	483.8	14.87	0.0149\n"
    ))

(define (read->list line)
  (define elts '())
  (call-with-input-string line
                          (lambda (iprt) (do ((elt (read iprt) (read iprt)))
                                           ((eof-object? elt) elts)
                                           (set! elts (cons elt elts))))))
(define irradiance
  (call-with-input-string *data*
                          (lambda (iprt)
                            (define lines '())
                            (do ((line (read-line iprt) (read-line iprt)))
                              ((eof-object? line)
                               (let ((nra (make-array (A:floR64b)
                                                      (length lines)
                                                      (length (car lines)))))
                                 (do ((lns lines (cdr lns))
                                      (idx (+ -1 (length lines)) (+ -1 idx)))
                                   ((null? lns) nra)
                                   (do ((kdx (+ -1 (length (car lines))) (+ -1 kdx))
                                        (lst (car lns) (cdr lst)))
                                     ((null? lst))
                                     (array-set! nra (car lst) idx kdx)))))
                              (if (and (positive? (string-length line))
                                       (char-numeric? (string-ref line 0)))
                                (set! lines (cons (read->list line) lines)))))))

(let ((xrange '(.25 2.5)))
  (create-postscript-graph
    "solarad.eps" '(600 300)
    (whole-page)
    (setup-plot xrange (column-range irradiance 1))
    (title-top
      "Solar Irradiance   (Invented Data)")
    (in-graphic-context
      (set-font "Helvetica-Oblique" 12)
      (title-top
        ""
        "Key Centre for Photovoltaic Engineering UNSW - Air Mass 1.5 Global Spectrum"))
    (outline-rect 'plotrect)
    (rule-vertical 'leftedge "W/(m^2.um)" 10)
    (in-graphic-context (clip-to-rect 'plotrect)
                        (plot-column irradiance 0 1 'line)
                        (set-color "Bright Sun")
                        (plot-column irradiance 0 1 'mountain)
                        )
    (rule-horizontal 'bottomedge "Wavelength in .um" 5)
    (set-color "sea green")

    (setup-plot xrange '(0 1000) 'graphrect)
    (in-graphic-context (clip-to-rect 'plotrect)
                        (set-linedash 5 2)
                        (plot-column irradiance 0 2 'line))
    (rule-vertical 'rightedge "Integrated .W/(m^2)" -10)
    ))
