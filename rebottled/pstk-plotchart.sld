;; Experiment in using tklib's plotchart with a simple wrapper
;; Documentation http://core.tcl.tk/tklib/doc/trunk/embedded/www/tklib/files/modules/plotchart/plotchart.html

;; Written by Peter Lane, 2017

(define-library
  (rebottled pstk-plotchart)
  (export 
    import-plotchart
    ;
    create-xy-plot
    create-strip-chart
    create-tx-plot
    create-x-logy-plot
    create-logx-y-plot
    create-logx-logy-plot
    create-polar-plot
    create-windrose
    create-isometric-plot
    create-histogram
    create-3d-plot
    create-3d-ribbon-plot
    create-pie-chart
    create-spiral-pie
    create-radial-chart
    create-bar-chart
    create-horizontal-bar-chart
    create-3d-bar-chart
    create-3d-ribbon-chart
    create-box-plot
    create-time-chart
    create-gantt-chart
    create-right-axis
    create-table-chart
    create-ternary-diagram
    create-status-timeline
    )
  (import (scheme base)
          (scheme cxr)
          (rebottled pstk))

  (begin

    ;; TODO: report error if not found
    (define (import-plotchart)
      (tk-eval "package require Plotchart"))

    (define (create-generic-plot tkname)
      (lambda (canvas . args)
        (let ((plot (tk-eval (string-append
                               tkname
                               " "
                               (canvas 'get-id)
                               (scheme-arglist->tk-argstring args)))))
          (lambda (command . args)
            (cond ((eq? 'plotfunc command)
                   '())
                  (else
                    (tk-eval (string-append 
                               plot
                               " "
                               (scheme-arglist->tk-argstring (cons command args))))))))))

    (define create-xy-plot (create-generic-plot "::Plotchart::createXYPlot"))
    (define create-strip-chart (create-generic-plot "::Plotchart::createStripchart"))
    (define create-tx-plot (create-generic-plot "::Plotchart::createTXPlot"))
    (define create-x-logy-plot (create-generic-plot "::Plotchart::createXLogYPlot"))
    (define create-logx-y-plot (create-generic-plot "::Plotchart::createLogXYPlot"))
    (define create-logx-logy-plot (create-generic-plot "::Plotchart::createLogXLogYPlot"))
    (define create-polar-plot (create-generic-plot "::Plotchart::createPolarplot"))
    (define create-windrose (create-generic-plot "::Plotchart::createWindrose"))
    (define create-isometric-plot (create-generic-plot "::Plotchart::createIsometricPlot"))
    (define create-histogram (create-generic-plot "::Plotchart::createHistogram"))
    (define create-3d-plot (create-generic-plot "::Plotchart::create3DPlot"))
    (define create-3d-ribbon-plot (create-generic-plot "::Plotchart::create3DRibbonPlot"))
    (define create-pie-chart (create-generic-plot "::Plotchart::createPiechart"))
    (define create-spiral-pie (create-generic-plot "::Plotchart::createSpiralPie"))
    (define create-radial-chart (create-generic-plot "::Plotchart::createRadialchart"))
    (define create-bar-chart (create-generic-plot "::Plotchart::createBarchart"))
    (define create-horizontal-bar-chart (create-generic-plot "::Plotchart::createHorizontalBarchart"))
    (define create-3d-bar-chart (create-generic-plot "::Plotchart::create3DBarchart"))
    (define create-3d-ribbon-chart (create-generic-plot "::Plotchart::create3DRibbonChart"))
    (define create-box-plot (create-generic-plot "::Plotchart::createBoxplot"))
    (define create-time-chart (create-generic-plot "::Plotchart::createTimechart"))
    (define create-gantt-chart (create-generic-plot "::Plotchart::createGanttchart"))
    (define create-right-axis (create-generic-plot "::Plotchart::createRightAxis"))
    (define create-table-chart (create-generic-plot "::Plotchart::createTableChart"))
    (define create-ternary-diagram (create-generic-plot "::Plotchart::createTernaryDiagram"))
    (define create-status-timeline (create-generic-plot "::Plotchart::createStatusTimeline"))

    ))

