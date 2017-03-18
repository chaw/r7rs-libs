;;;; "grapheps.scm", Create PostScript Graphs
;;; Copyright (C) 2003, 2004, 2005, 2006, 2008, 2010, 2011 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib eps-graph)
  (export create-postscript-graph
          whole-page
          ;
          import-array
          column-range
          pad-range
          snap-range
          combine-ranges
          setup-plot
          plot-column
          plot-text-column
          in-graphic-context
          set-color
          set-font
          set-linewidth
          set-linedash
          set-glyphsize
          partition-page
          fill-rect
          outline-rect
          clip-to-rect
          title-top
          title-bottom
          set-margin-templates
          rule-vertical
          rule-horizontal
          x-axis
          y-axis
          grid-verticals
          grid-horizontals
          graph:plot
          functions->array
          graph:plot-function
          plot
          )
  (import (except (scheme base) equal?)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (scheme write)
          (slib array-for-each)
          (slib color)
          (slib common)
          (slib filename)
          (slib line-io)
          (slib resene)
          (slib saturate)
          (srfi 63))

  (begin

    ;;
    ;;@noindent
    ;;This is a graphing package creating encapsulated-PostScript files.
    ;;Its motivations and design choice are described in
    ;;@url{http://people.csail.mit.edu/jaffer/Docupage/grapheps}
    ;;
    ;;@noindent
    ;;A dataset to be plotted is taken from a 2-dimensional array.
    ;;Corresponding coordinates are in rows.  Coordinates from any
    ;;pair of columns can be plotted.

    ;;; String append which accepts numbers, symbols, vectors, and lists.
    (define (scheme->ps . args)
      (apply string-append
             (map (lambda (arg)
                    (cond ((number? arg) (number->string arg))
                          ((symbol? arg) (symbol->string arg))
                          ((or (vector? arg) (list? arg))
                           (string-append
                             "[ "
                             (apply string-append
                                    (map (lambda (x) (scheme->ps x " "))
                                         (if (vector? arg) (vector->list arg) arg)))
                             "]"))
                          (else arg)))
                  args)))

    (define (data->ps . args)
      (apply string-append
             (map (lambda (arg)
                    (cond ((number? arg) (number->string arg))
                          ((symbol? arg) (string-append "/" (symbol->string arg)))
                          ((string? arg) (string-append "(" arg ")"))
                          ((or (vector? arg) (list? arg))
                           (string-append
                             "[ "
                             (apply string-append
                                    (map (lambda (x) (data->ps x " "))
                                         (if (vector? arg) (vector->list arg) arg)))
                             "]"))
                          (else arg)))
                  args)))

    ;;; Capture for %%Title
    (define *plot-title* #f)

    ;; Remember arrays so each is output only once.
    (define *plot-arrays* '())

    ;;@args filename.eps size elt1 ...
    ;;@1 should be a string naming an output file to be created.  @2
    ;;should be an exact integer, a list of two exact integers, or #f.
    ;;@3, ... are values returned by graphing primitives described here.
    ;;
    ;;@0 creates an @dfn{Encapsulated-PostScript} file named @1 containing
    ;;graphs as directed by the @3, ... arguments.
    ;;
    ;;The size of the graph is determined by the @2 argument.  If a list
    ;;of two integers, they specify the width and height.  If one integer,
    ;;then that integer is the width and the height is 3/4 of the width.
    ;;If #f, the graph will be 800 by 600.
    (define (create-postscript-graph filename size . args)
      (define xsize (cond ((pair? size) (car size))
                          ((number? size) size)
                          (else 800)))
      (let ((ysize (if (and (pair? size) (pair? (cdr size)))
                     (cadr size)
                     (quotient (* 3 xsize) 4))))
        (cond ((provided? 'inexact)
               (set! xsize (exact (round xsize)))
               (set! ysize (exact (round ysize)))))
        (call-with-output-file filename
                               (lambda (oprt)
                                 (define (write-lines lines)
                                   (for-each (lambda (line) (if (list? line)
                                                              (write-lines line)
                                                              (write-line line oprt)))
                                             lines))
                                 (write-line "%!PS-Adobe-3.0 EPSF-3.0" oprt)
                                 (write-line (scheme->ps "%%BoundingBox: 0 0 " xsize " " ysize) oprt)
                                 (write-line (scheme->ps "%%Title: " (or *plot-title* filename)) oprt)
                                 (write-line (scheme->ps "%%EndComments: ") oprt)
                                 (write-line (scheme->ps "0 0 " xsize " " ysize) oprt)
                                 (write-graph-library oprt)
                                 (for-each (lambda (pair) (write-array-def (cdr pair) (car pair) oprt))
                                           *plot-arrays*)
                                 (write-lines args)
                                 (newline oprt)
                                 (write-line "grestore" oprt)
                                 (write-line "end" oprt)
                                 (write-line "showpage" oprt)))
        (set! *plot-title* #f)
        (set! *plot-arrays* '())))

    (define (write-array-def name array oprt)
      (define row-length (cadr (array-dimensions array)))
      (define idx 0)
      (set! idx row-length)
      (write-line (scheme->ps "/" name) oprt)
      (write-line "[" oprt)
      (display " [" oprt)
      (array-for-each
        (lambda (elt)
          (cond ((zero? idx)
                 (write-line "]" oprt)
                 (display " [" oprt)))
          (display  "	" oprt)
          (display (data->ps elt) oprt)
          (set! idx (modulo (+ 1 idx) row-length)))
        array)
      (write-line "]" oprt)
      (write-line "] def" oprt))

    ;;; Arrays are named and cached in *plot-arrays*.
    (define (import-array array)
      (cond ((assq array *plot-arrays*) => cdr)
            (else
              (let ((name (gentemp)))
                (set! *plot-arrays* (cons (cons array name) *plot-arrays*))
                name))))

    ;;@noindent
    ;;These graphing procedures should be called as arguments to
    ;;@code{create-postscript-graph}.  The order of these arguments is
    ;;significant; PostScript graphics state is affected serially from the
    ;;first @var{elt} argument to the last.

    ;;@menu
    ;;* Column Ranges::
    ;;* Drawing the Graph::
    ;;* Graphics Context::
    ;;* Rectangles::
    ;;* Legending::
    ;;* Legacy Plotting::
    ;;* Example Graph::
    ;;@end menu
    ;;
    ;;@node Column Ranges, Drawing the Graph, PostScript Graphing, PostScript Graphing
    ;;@subsubsection Column Ranges

    ;;@noindent
    ;;A @dfn{range} is a list of two numbers, the minimum and the maximum.
    ;;@cindex range
    ;;Ranges can be given explicity or computed in PostScript by
    ;;@code{column-range}.

    ;;@body
    ;;Returns the range of values in 2-dimensional @1 column @2.
    (define (column-range array k)
      (set! array (import-array array))
      (scheme->ps array " " k " column-range"))

    ;;@body
    ;;Expands @1 by @2/100 on each end.
    (define (pad-range range p) (scheme->ps range " " p " pad-range"))

    ;;@body
    ;;Expands @1 to round number of ticks.
    (define (snap-range range) (scheme->ps range " snap-range"))

    ;;@args range1 range2 ...
    ;;Returns the minimal range covering all @1, @2, ...
    (define (combine-ranges rng1 . rngs)
      (define (loop rngs)
        (cond ((null? rngs) "")
              (else (scheme->ps " " (car rngs) (loop (cdr rngs))
                                " combine-ranges"))))
      (scheme->ps rng1 (loop rngs)))

    ;;@args x-range y-range pagerect
    ;;@args x-range y-range
    ;;@1 and @2 should each be a list of two numbers or the value returned
    ;;by @code{pad-range}, @code{snap-range}, or @code{combine-range}.
    ;;@3 is the rectangle bounding the graph to be drawn; if missing, the
    ;;rectangle from the top of the PostScript stack is popped and used.
    ;;
    ;;Based on the given ranges, @0 sets up scaling and margins for making
    ;;a graph.  The margins are sized proportional to the @var{fontheight}
    ;;value at the time of the call to setup-plot.  @0 sets two variables:
    ;;
    ;;@table @var
    ;;@item plotrect
    ;;The region where data points will be plotted.
    ;;@item graphrect
    ;;The @3 argument to @0.  Includes plotrect, legends, etc.
    ;;@end table
    (define (setup-plot xrange yrange . pagerect)
      (if (null? pagerect)
        (scheme->ps xrange " " yrange " setup-plot")
        (scheme->ps (car pagerect) " " xrange " " yrange " setup-plot")))

    ;;@node Drawing the Graph, Graphics Context, Column Ranges, PostScript Graphing
    ;;@subsubsection Drawing the Graph

    ;;@body
    ;;Plots points with x coordinate in @2 of @1 and y coordinate @3 of
    ;;@1.  The symbol @4 specifies the type of glyph or drawing style for
    ;;presenting these coordinates.
    (define (plot-column array x-column y-column proc3s)
      (set! array (import-array array))
      (scheme->ps "[ " array " " x-column " " y-column " ] "  proc3s
                  " plot-column"))

    ;;@noindent
    ;;The glyphs and drawing styles available are:
    ;;
    ;;@table @code
    ;;@item line
    ;;Draws line connecting points in order.
    ;;@item mountain
    ;;Fill area below line connecting points.
    ;;@item cloud
    ;;Fill area above line connecting points.
    ;;@item impulse
    ;;Draw line from x-axis to each point.
    ;;@item bargraph
    ;;Draw rectangle from x-axis to each point.
    ;;@item disc
    ;;Solid round dot.
    ;;@item point
    ;;Minimal point -- invisible if linewidth is 0.
    ;;@item square
    ;;Square box.
    ;;@item diamond
    ;;Square box at 45.o
    ;;@item plus
    ;;Plus sign.
    ;;@item cross
    ;;X sign.
    ;;@item triup
    ;;Triangle pointing upward
    ;;@item tridown
    ;;Triangle pointing downward
    ;;@item pentagon
    ;;Five sided polygon
    ;;@item circle
    ;;Hollow circle
    ;;@end table

    ;;@body
    ;;Plots text in @4 of @1 at x coordinate in @2 of @1 and y coordinate
    ;;@3 of @1.  The symbol @5 specifies the offset of the text from the
    ;;specified coordinates.
    (define (plot-text-column array x-column y-column t-column proc3s)
      (set! array (import-array array))
      (scheme->ps "[ " array " " x-column " " y-column " " t-column " ] "  proc3s
                  " plot-text-column"))

    ;;@noindent
    ;;The offsets available are:
    ;;
    ;;@table @code
    ;;@item above
    ;;Draws the text centered above at the point.
    ;;@item center
    ;;Draws the text centered at the point.
    ;;@item below
    ;;Draws the text centered below the point.
    ;;@item left
    ;;Draws the text to the left of the point.
    ;;@item right
    ;;Draws the text to the right of the point.
    ;;@end table
    ;;
    ;;All the offsets other than @code{center} are calculated to keep the
    ;;text clear of a glyph drawn at the same coordinates.  If you need
    ;;more or less clearance, use @code{set-glyphsize}.


    ;;@node Graphics Context, Rectangles, Drawing the Graph, PostScript Graphing
    ;;@subsubsection Graphics Context

    ;;@body
    ;;Saves the current graphics state, executes @1, then restores
    ;;to saved graphics state.
    (define (in-graphic-context . args)
      (append '("gpush") args '("gpop")))

    ;;@args color
    ;;@1 should be a string naming a Resene color, a saturate color, or a
    ;;number between 0 and 100.
    ;;
    ;;@0 sets the PostScript color to the color of the given string, or a
    ;;grey value between black (0) and white (100).
    (define (set-color clrn)
      (define clr
        (cond ((color? clrn) clrn)
              ((number? clrn) (* 255/100 clrn))
              ((or (eq? 'black clrn)
                   (and (string? clrn) (string-ci=? "black" clrn))) 0)
              ((or (eq? 'white clrn)
                   (and (string? clrn) (string-ci=? "white" clrn))) 255)
              (else (or (saturate clrn) (resene clrn)
                        (string->color (if (symbol? clrn)
                                         (symbol->string clrn)
                                         clrn))))))
      (define (num->str x)
        (define num (exact (round (+ 1000 (* x 999/255)))))
        (scheme->ps "." (substring (number->string num) 1 4) " "))
      (cond ((number? clr) (string-append (num->str clr) " setgray"))
            (clr (apply scheme->ps
                        (append (map num->str (color->sRGB clr)) '(setrgbcolor))))
            (else "")))

    ;;@args font height
    ;;@args font encoding height
    ;;@1 should be a (case-sensitive) string naming a PostScript font.
    ;;@var{height} should be a positive real number.
    ;;@var{encoding} should name a PostScript encoding such as
    ;;@samp{ISOLatin1Encoding}.
    ;;
    ;;@0 Changes the current PostScript font to @1 with the @var{encoding}
    ;;encoding, and height equal to @var{height}.  The default font is
    ;;@samp{Helvetica} (12pt).  The default encoding is
    ;;@samp{StandardEncoding}.
    (define (set-font name arg2 . args)
      (define fontheight (if (null? args) arg2 (car args)))
      (define encoding (and (not (null? args)) arg2))
      (scheme->ps
        (if (null? args)
          ""
          (string-append " /" name " " encoding " /" name "-" encoding
                         " combine-font-encoding"))
        " /fontsize " fontheight " def /"
        (if encoding (string-append name "-" encoding) name)
        " fontsize selectfont"))

    ;;@noindent
    ;;The base set of PostScript fonts is:
    ;;
    ;;@multitable @columnfractions .20 .25 .25 .30
    ;;@item Times @tab Times-Italic @tab Times-Bold @tab Times-BoldItalic
    ;;@item Helvetica @tab Helvetica-Oblique @tab Helvetica-Bold @tab Helvetica-BoldOblique
    ;;@item Courier @tab Courier-Oblique @tab Courier-Bold @tab Courier-BoldOblique
    ;;@item Symbol
    ;;@end multitable

    ;;@noindent
    ;;The base set of PostScript encodings is:
    ;;
    ;;@multitable @columnfractions .33 .33 .33
    ;;@item StandardEncoding @tab ISOLatin1Encoding @tab ExpertEncoding
    ;;@item ExpertSubsetEncoding @tab SymbolEncoding
    ;;@end multitable

    ;;@noindent
    ;;Line parameters do no affect fonts; they do effect glyphs.

    ;;@body
    ;;The default linewidth is 1.  Setting it to 0 makes the lines drawn
    ;;as skinny as possible.  Linewidth must be much smaller than
    ;;glyphsize for readable glyphs.
    (define (set-linewidth w) (scheme->ps w " setlinewidth"))

    ;;@args j k
    ;;Lines are drawn @1-on @2-off.
    ;;@args j
    ;;Lines are drawn @1-on @1-off.
    ;;@args
    ;;Turns off dashing.
    (define (set-linedash . args) (scheme->ps args " 0 setdash"))

    ;;@body
    ;;Sets the (PostScript) variable glyphsize to @1.  The default
    ;;glyphsize is 6.
    (define (set-glyphsize w) (scheme->ps "/glyphsize " w " def"))

    ;;@noindent
    ;;The effects of @code{clip-to-rect} are also part of the graphic
    ;;context.


    ;;@node Rectangles, Legending, Graphics Context, PostScript Graphing
    ;;@subsubsection Rectangles

    ;;@noindent
    ;;A @dfn{rectangle} is a list of 4 numbers; the first two elements are
    ;;the x and y coordinates of lower left corner of the rectangle.  The
    ;;other two elements are the width and height of the rectangle.

    ;;@body
    ;;Pushes a rectangle for the whole encapsulated page onto the
    ;;PostScript stack.  This pushed rectangle is an implicit argument to
    ;;@code{partition-page} or @code{setup-plot}.
    (define (whole-page) 'whole-page)

    ;;@body
    ;;Pops the rectangle currently on top of the stack and pushes @1 * @2
    ;;sub-rectangles onto the stack in decreasing y and increasing x order.
    ;;If you are drawing just one graph, then you don't need @0.
    (define (partition-page xparts yparts)
      (scheme->ps xparts " " yparts " partition-page"))

    ;;@body
    ;;The rectangle where data points should be plotted.  @0 is set by
    ;;@code{setup-plot}.
    (define plotrect 'plotrect)

    ;;@body
    ;;The @var{pagerect} argument of the most recent call to
    ;;@code{setup-plot}.  Includes plotrect, legends, etc.
    (define graphrect 'graphrect)

    ;;@body
    ;;fills @1 with the current color.
    (define (fill-rect rect) (scheme->ps rect " fill-rect"))

    ;;@body
    ;;Draws the perimiter of @1 in the current color.
    (define (outline-rect rect) (scheme->ps rect " outline-rect"))

    ;;@body
    ;;Modifies the current graphics-state so that nothing will be drawn
    ;;outside of the rectangle @1.  Use @code{in-graphic-context} to limit
    ;;the extent of @0.
    (define (clip-to-rect rect) (scheme->ps rect " clip-to-rect"))


    ;;@node Legending, Legacy Plotting, Rectangles, PostScript Graphing
    ;;@subsubsection Legending

    ;;@args title subtitle
    ;;@args title
    ;;Puts a @1 line and an optional @2 line above the @code{graphrect}.
    (define (title-top title . subtitle)
      (set! *plot-title* title)
      (scheme->ps "(" title ") ("
                  (if (null? subtitle) "" (car subtitle))
                  ") title-top"))

    ;;@args title subtitle
    ;;@args title
    ;;Puts a @1 line and an optional @2 line below the @code{graphrect}.
    (define (title-bottom title . subtitle)
      (set! *plot-title* title)
      (scheme->ps "(" title ") ("
                  (if (null? subtitle) "" (car subtitle))
                  ") title-bottom"))

    ;;@body
    ;;These edge coordinates of @code{graphrect} are suitable for passing
    ;;as the first argument to @code{rule-horizontal}.
    (define topedge 'topedge)
    (define bottomedge 'bottomedge)

    ;;@body
    ;;These edge coordinates of @code{graphrect} are suitable for passing
    ;;as the first argument to @code{rule-vertical}.
    (define leftedge 'leftedge)
    (define rightedge 'rightedge)

    ;;@body
    ;;The margin-templates are strings whose displayed width is used to
    ;;reserve space for the left and right side numerical legends.
    ;;The default values are "-.0123456789".
    (define (set-margin-templates left right)
      (scheme->ps "/lmargin-template (" left ") def "
                  "/rmargin-template (" right ") def"))

    ;;@body
    ;;Draws a vertical ruler with X coordinate @1 and labeled with string
    ;;@2.  If @3 is positive, then the ticks are @3 long on the right side
    ;;of @1; and @2 and numeric legends are on the left.  If @3 is
    ;;negative, then the ticks are -@3 long on the left side of @1; and @2
    ;;and numeric legends are on the right.
    (define (rule-vertical x-coord text tick-width)
      (scheme->ps x-coord " (" text ") " tick-width " rule-vertical"))

    ;;@body
    ;;Draws a horizontal ruler with Y coordinate @1 and labeled with
    ;;string @2.  If @3 is positive, then the ticks are @3 long on the top
    ;;side of @1; and @2 and numeric legends are on the bottom.  If @3 is
    ;;negative, then the ticks are -@3 long on the bottom side of @1; and
    ;;@2 and numeric legends are on the top.
    (define (rule-horizontal y-coord text tick-height)
      (scheme->ps y-coord " (" text ") " tick-height " rule-horizontal"))

    ;;@body
    ;;Draws the y-axis.
    (define (y-axis) 'y-axis)
    ;;@body
    ;;Draws the x-axis.
    (define (x-axis) 'x-axis)
    ;;@body
    ;;Draws vertical lines through @code{graphrect} at each tick on the
    ;;vertical ruler.
    (define (grid-verticals) 'grid-verticals)
    ;;@body
    ;;Draws horizontal lines through @code{graphrect} at each tick on the
    ;;horizontal ruler.
    (define (grid-horizontals) 'grid-horizontals)

    ;;@node Legacy Plotting, Example Graph, Legending, PostScript Graphing
    ;;@subsubsection Legacy Plotting

    (define (graph:plot tmp data xlabel ylabel . histogram-in?)
      (let ((histogram? (if (null? histogram-in?) #f (car histogram-in?))))
        (if (list? data)
          (let ((len (length data))
                (nra (make-array (A:floR64b) (length data) 2)))
            (do ((idx 0 (+ 1 idx))
                 (lst data (cdr lst)))
              ((>= idx len)
               (set! data nra))
              (array-set! nra (caar lst) idx 0)
              (array-set! nra (if (list? (cdar lst)) (cadar lst) (cdar lst))
                          idx 1))))
        (create-postscript-graph
          tmp (or graph:dimensions '(600 300))
          (whole-page)
          (setup-plot (column-range data 0)
                      (apply combine-ranges
                             (do ((idx (+ -1 (cadr (array-dimensions data))) (+ -1 idx))
                                  (lst '() (cons (column-range data idx) lst)))
                               ((< idx 1) lst))))
          (outline-rect plotrect)
          (x-axis) (y-axis)
          (do ((idx (+ -1 (cadr (array-dimensions data))) (+ -1 idx))
               (lst '() (cons 
                          (plot-column data 0 idx (if histogram? 'bargraph 'line))
                          lst)))
            ((< idx 1) lst))
          (rule-vertical leftedge ylabel 10)
          (rule-horizontal bottomedge xlabel 10))))

    (define (functions->array vlo vhi npts . funcs)
      (let ((dats (make-array (A:floR32b) npts (+ 1 (length funcs)))))
        (define jdx 1)
        (array-index-map! (make-shared-array dats
                                             (lambda (idx) (list idx 0))
                                             npts)
                          (lambda (idx)
                            (+ vlo (* (- vhi vlo) (/ idx (+ -1 npts))))))
        (for-each (lambda (func)
                    (array-map!
                      (make-shared-array dats (lambda (idx) (list idx jdx)) npts)
                      func
                      (make-shared-array dats (lambda (idx) (list idx 0)) npts))
                    (set! jdx (+ 1 jdx)))
                  funcs)
        dats))

    (define (graph:plot-function tmp func vlo vhi . npts-in)
      (let ((npts (if (null? npts-in) 200 (car npts-in))))
        (let ((dats (functions->array vlo vhi npts func)))
          (graph:plot tmp dats "" ""))))

    ;;@body
    ;;A list of the width and height of the graph to be plotted using
    ;;@code{plot}.
    (define graph:dimensions #f)

    ;;@args func x1 x2 npts
    ;;@args func x1 x2
    ;;Creates and displays using @code{(system "gv tmp.eps")} an
    ;;encapsulated PostScript graph of the function of one argument @1
    ;;over the range @2 to @3.  If the optional integer argument @4 is
    ;;supplied, it specifies the number of points to evaluate @1 at.
    ;;
    ;;@args x1 x2 npts func1 func2 ...
    ;;Creates and displays an encapsulated PostScript graph of the
    ;;one-argument functions @var{func1}, @var{func2}, ... over the range
    ;;@var{x1} to @var{x2} at @var{npts} points.
    ;;
    ;;@args coords x-label y-label
    ;;@var{coords} is a list or vector of coordinates, lists of x and y
    ;;coordinates.  @var{x-label} and @var{y-label} are strings with which
    ;;to label the x and y axes.
    (define (plot . args)
      (call-with-tmpnam
        (lambda (tmp)
          (cond ((procedure? (car args))
                 (apply graph:plot-function (cons tmp args)))
                ((or (array? (car args))
                     (and (pair? (car args))
                          (pair? (caar args))))
                 (apply graph:plot (cons tmp args)))
                (else (let ((dats (apply functions->array args)))
                        (graph:plot tmp dats "" ""))))
          (system (string-append "gv '" tmp "'")))
        ".eps"))

    ;; Writes the graph library:
    ;; added by Peter Lane to avoid having grapheps.ps as a separate file 
    (define (write-graph-library oprt)
      (for-each (lambda (line) (write-line line oprt))
                *graph-library*))

    (define *graph-library* ; contents of slib/grapheps.ps
      '(
        "% \"graph-eps\" library for creating PostScript graphs"
        "% http://people.csail.mit.edu/jaffer/Docupage/grapheps"
        "% Copyright (C) 1991, 2001, 2005, 2006, 2009, 2010, 2011 Aubrey Jaffer"
        "%"
        "% Permission to copy this software, to modify it, to redistribute it,"
        "% to distribute modified versions, and to use it for any purpose is"
        "% granted, subject to the following restrictions and understandings."
        "%"
        "% 1.  Any copy made of this software must include this copyright notice"
        "% in full."
        "%"
        "% 2.  I have made no warranty or representation that the operation of"
        "% this software will be error-free, and I am under no obligation to"
        "% provide any services, by way of maintenance, update, or otherwise."
        "%"
        "% 3.  In conjunction with products arising from the use of this"
        "% material, there shall be no use of my name in any advertising,"
        "% promotional, or sales literature without prior written consent in"
        "% each case."
        ""
        "/plotdict 100 dict def"
        "plotdict begin"
        ""
        "% Get dimensions"
        "4 array astore /whole-page exch def"
        ""
        "% Definitions so that internal assignments are bound before setting."
        "/DATA 0 def"
        "/DEN 0 def"
        "/DIAG 0 def"
        "/DIAG2 0 def"
        "/DLTA 0 def"
        "/EXPSN 0 def"
        "/GPROCS 0 def"
        "/GD 6 def"
        "/GR 3 def"
        "/IDX 0 def"
        "/ISIZ 0 def"
        "/MAX 0 def"
        "/MIN 0 def"
        "/NUM 0 def"
        "/PLOT-bmargin 0 def"
        "/PLOT-lmargin 0 def"
        "/PLOT-rmargin 0 def"
        "/PLOT-tmargin 0 def"
        "/PROC 0 def"
        "/ROW 0 def"
        "/TXT 0 def"
        "/WPAGE 0 def"
        "/X-COORD 0 def"
        "/XDX 0 def"
        "/XOFF 0 def"
        "/XPARTS 0 def"
        "/XRNG 0 def"
        "/XSCL 0 def"
        "/XSTEP 0 def"
        "/XSTEPH 0 def"
        "/XTSCL 0 def"
        "/XWID 0 def"
        "/Y-COORD 0 def"
        "/YDX 0 def"
        "/YHIT 0 def"
        "/YOFF 0 def"
        "/YPARTS 0 def"
        "/YRNG 0 def"
        "/YSCL 0 def"
        "/YSTEP 0 def"
        "/YSTEPH 0 def"
        "/YTSCL 0 def"
        "/STP3 0 def"
        "/STP2 0 def"
        "/SCL 0 def"
        "/Y0 0 def"
        "/NAME-ENCODING 0 def"
        "/ENCODING 0 def"
        "/NAME 0 def"
        "/graphrect 0 def"
        "/plotrect 0 def"
        ""
        "% ( TITLE ) ( SUBTITLE )"
        "/title-top"
        "{ dup stringwidth pop -2 div plotrect 0 get plotrect 2 get 2 div add add"
        "  plotrect 1 get plotrect 3 get add pointsize .4 mul add moveto show"
        "  dup stringwidth pop -2 div plotrect 0 get plotrect 2 get 2 div add add"
        "  plotrect 1 get plotrect 3 get add pointsize 1.4 mul add moveto show"
        "} bind def"
        ""
        "% ( TITLE ) ( SUBTITLE )"
        "/title-bottom"
        "{ dup stringwidth pop -2 div plotrect 0 get plotrect 2 get 2 div add add"
        "  plotrect 1 get pointsize -2 mul add moveto show"
        "  dup stringwidth pop -2 div plotrect 0 get plotrect 2 get 2 div add add"
        "  plotrect 1 get pointsize -1 mul add moveto show"
        "} bind def"
        ""
        "% Plots column K against column J of given two-dimensional ARRAY."
        "% The arguments are:"
        "%   [ ARRAY J K ] J and K are column-indexes into ARRAY"
        "%   [ PREAMBLE RENDER POSTAMBLE ] Plotting procedures:"
        "%       PREAMBLE  - Executed once before plotting row"
        "%       RENDER    - Called with each pair of coordinates to plot"
        "%       POSTAMBLE - Called once after plotting row (often does stroke)"
        "/plot-column"
        "{ /GPROCS exch def aload pop /YDX exch def /XDX exch def /DATA exch def"
        "  /GD glyphsize def"
        "  /GR GD .5 mul def"
        "  gsave"
        "    /ROW DATA 0 get def ROW XDX get ROW YDX get gtrans moveto"
        "    GPROCS 0 get exec % preamble"
        "    /PROC GPROCS 1 get def DATA {dup XDX get exch YDX get gtrans PROC} forall"
        "    GPROCS 2 get exec stroke % postamble"
        "  grestore"
        "} bind def"
        ""
        "% Here are the procedure-arrays for passing as the third argument to"
        "% plot-column.  Plot-column moves to the first coordinate before"
        "% calls to the first procedure.  Thus both line and scatter graphs are"
        "% supported.  Many additional glyph types can be produced as"
        "% combinations of these types.  This is best accomplished by calling"
        "% plot-column with each component."
        ""
        "% GD and GR are the graphic-glyph diameter and radius."
        "% DIAG and DIAG2, used in /cross are diagonal and twice diagonal."
        "% gtrans maps x, y coordinates on the stack to 72dpi page coordinates."
        ""
        "% Render line connecting points"
        "/line    [{} {lineto} {}] bind def"
        "/mountain [{currentpoint 2 copy pop bottomedge moveto lineto}"
        "	   {lineto}"
        "	   {currentpoint pop bottomedge lineto closepath fill}] bind def"
        "/cloud    [{currentpoint 2 copy pop topedge moveto lineto}"
        "	   {lineto}"
        "	   {currentpoint pop topedge lineto closepath fill}] bind def"
        "% Render lines from x-axis to points"
        "/impulse [{} {2 copy moveto pop Y0 lineto} {}] bind def"
        "/bargraph"
        "    [{}"
        "     {2 copy pop GR sub Y0"
        "      4 2 roll Y0 sub exch pop GD exch rectstroke}"
        "     {}] bind def"
        "/barfill"
        "    [{}"
        "     {2 copy pop GR sub Y0"
        "      4 2 roll Y0 sub exch pop GD exch rectfill}"
        "     {}] bind def"
        ""
        "% Solid round dot."
        "/disc    [{GD setlinewidth 1 setlinecap}"
        "			 {moveto 0 0 rlineto} {}] bind def"
        "% Minimal point -- invisible if linewidth is 0."
        "/point   [{1 setlinecap} {moveto 0 0 rlineto} {}] bind def"
        "% Square box."
        "/square  [{} {GR sub exch GR sub exch GD dup rectstroke} {}] bind def"
        "% Square box at 45.o"
        "/diamond [{}"
        "	  {2 copy GR add moveto"
        "	   GR neg GR neg rlineto GR GR neg rlineto"
        "		   GR GR rlineto GR neg GR rlineto"
        "	   closepath}"
        "	  {}] bind def"
        "% Plus Sign"
        "/plus	 [{}"
        "	  {       GR sub moveto  0 GD rlineto"
        "	   GR neg GR neg rmoveto GD 0 rlineto}"
        "	  {}] bind def"
        "% X Sign"
        "/cross   [{/DIAG GR .707 mul def /DIAG2 DIAG 2 mul def}"
        "	  {exch DIAG sub exch DIAG add moveto DIAG2 dup neg rlineto"
        "			  DIAG2 neg 0 rmoveto     DIAG2 dup rlineto}"
        "	  {}] bind def"
        "% Triangle pointing upward"
        "/triup   [{}"
        "	  {GR 1.12 mul add moveto GR neg GR -1.62 mul rlineto"
        "	   GR 2 mul 0 rlineto GR neg GR  1.62 mul rlineto"
        "	   closepath}"
        "	  {}] bind def"
        "% Triangle pointing downward"
        "/tridown [{}"
        "	  {GR 1.12 mul sub moveto GR neg GR  1.62 mul rlineto"
        "	   GR 2 mul 0 rlineto GR neg GR -1.62 mul rlineto"
        "	   closepath}"
        "	  {}] bind def"
        "/pentagon [{}"
        "	   {gsave translate 0 GR moveto 4 {72 rotate 0 GR lineto} repeat"
        "		  closepath stroke grestore}"
        "	   {}] bind def"
        "/circle   [{stroke} {GR 0 360 arc stroke} {}] bind def"
        ""
        "% Puts text in column-L at column-K vs column-J"
        "% The arguments are:"
        "%   [ ARRAY J K L ] J, K, and L are column-indexes into ARRAY"
        "%   [ PREAMBLE RENDER POSTAMBLE ] Plotting procedures:"
        "%       PREAMBLE  - Executed once before plotting row"
        "%       RENDER    - Called with each pair of coordinates and text"
        "%       POSTAMBLE - Called once after plotting row (often does stroke)"
        "/plot-text-column"
        "{ /GPROCS exch def aload pop"
        "    /TDX exch def /YDX exch def /XDX exch def /DATA exch def"
        "  /GD glyphsize def"
        "  /GR GD .5 mul def"
        "  gsave"
        "    /ROW DATA 0 get def ROW XDX get ROW YDX get gtrans moveto"
        "    GPROCS 0 get exec % preamble"
        "    /PROC GPROCS 1 get def"
        "    DATA"
        "    {/row exch def row XDX get row YDX get gtrans row TDX get PROC} forall"
        "    GPROCS 2 get exec stroke % postamble"
        "  grestore"
        "} bind def"
        ""
        "% Here are the procedure-arrays for passing as the third argument to"
        "% plot-text-column.  Plot-text-column moves to the first coordinate"
        "% before calls to the first procedure."
        ""
        "% GD and GR are the graphic-glyph diameter and radius."
        "% DIAG and DIAG2, used in /cross are diagonal and twice diagonal."
        "% gtrans maps x, y coordinates on the stack to 72dpi page coordinates."
        ""
        "/above [{}"
        "	 {/TXT exch def"
        "	 exch TXT stringwidth pop -2 div add exch pointsize 0.1 mul GR add add moveto"
        "	 TXT show}"
        "         {}] bind def"
        ""
        "/center [{}"
        "	 {/TXT exch def"
        "	 exch TXT stringwidth pop -2 div add exch pointsize -0.35 mul add moveto"
        "	 TXT show}"
        "         {}] bind def"
        ""
        "/below [{}"
        "	 {/TXT exch def"
        "	 exch TXT stringwidth pop -2 div add exch pointsize 0.7 mul GR add sub moveto"
        "	 TXT show}"
        "         {}] bind def"
        ""
        "/left [{}"
        "	 {/TXT exch def"
        "	 exch TXT stringwidth pop GR add sub exch pointsize -0.35 mul add moveto"
        "	 TXT show}"
        "         {}] bind def"
        ""
        "/right [{}"
        "	 {/TXT exch def"
        "	 exch GR add exch pointsize -0.35 mul add moveto"
        "	 TXT show}"
        "         {}] bind def"
        ""
        "/partition-page"
        "{ /YPARTS exch def /XPARTS exch def /WPAGE exch def"
        "  /XWID WPAGE 2 get XPARTS div def /YHIT WPAGE 3 get YPARTS div def"
        "  /Y-COORD WPAGE 1 get def"
        "  YPARTS"
        "  { /X-COORD WPAGE 0 get WPAGE 2 get add XWID sub def"
        "    XPARTS {[X-COORD Y-COORD XWID YHIT]"
        "	    /X-COORD X-COORD XWID sub def} repeat"
        "    /Y-COORD Y-COORD YHIT add def"
        "  } repeat"
        "} bind def"
        ""
        "/squelch-.0 % x"
        "{"
        "    dup dup cvi eq {cvi} if"
        "} bind def"
        ""
        "/fudge3 % SCL STP3 STP2"
        "{"
        "    /STP2 exch def /STP3 exch def /SCL exch def"
        "    SCL 3 mod 0 eq {STP3} {STP2} ifelse"
        "%% leads to range error in CVS."
        "%    SCL abs 3000 gt {STP2} {SCL 3 mod 0 eq {STP3} {STP2} ifelse} ifelse"
        "} bind def"
        ""
        "% The arguments are:"
        "%   [ MIN-X MIN-Y DELTA-X DELTA-Y ] whole graph rectangle"
        "%   [ MIN-COLJ MAX-COLJ ] Numerical range of plot data"
        "%   [ MIN-COLK MAX-COLK ] Numerical range of plot data"
        "% and the implicit current clippath"
        "/setup-plot"
        "{ /YRNG exch def /XRNG exch def /graphrect exch def"
        "  /PLOT-bmargin pointsize 2.4 mul def"
        "  /PLOT-tmargin pointsize 2.4 mul def"
        "  /PLOT-lmargin lmargin-template stringwidth pop pointsize 1.2 mul add def"
        "  /PLOT-rmargin rmargin-template stringwidth pop pointsize 1.2 mul add def"
        "  /plotrect [ graphrect 0 get PLOT-lmargin add"
        "	      graphrect 1 get PLOT-bmargin add"
        "	      graphrect 2 get PLOT-lmargin sub PLOT-rmargin sub"
        "	      graphrect 3 get PLOT-bmargin sub PLOT-tmargin sub ] def"
        "  /XSCL plotrect 2 get XRNG aload pop exch sub div def"
        "  /YSCL plotrect 3 get YRNG aload pop exch sub div def"
        "  /XOFF XRNG 0 get plotrect 0 get XSCL div sub def"
        "  /YOFF YRNG 0 get plotrect 1 get YSCL div sub def"
        "  /YTSCL plotrect 3 get YRNG aload pop exch sub abs find-tick-scale def"
        "  /YSTEP YTSCL 0 get 6 8 fudge3 5 mul yunttrans YSCL sign mul def"
        "  /XTSCL plotrect 2 get XRNG aload pop exch sub abs find-tick-scale def"
        "  /XSTEP XTSCL 0 get 12 10 fudge3 5 mul xunttrans XSCL sign mul def"
        "  /YSTEPH YSTEP 2 div def"
        "  /XSTEPH XSTEP 2 div def"
        "%  /Y0 0 YOFF sub YSCL mul def"
        "  /Y0 YRNG 0 get YOFF sub YSCL mul def"
        "} bind def"
        ""
        "% gtrans is the utility routine mapping data coordinates to view space."
        "% plot-column sets up XOFF, XSCL, and YSCL and uses it."
        "/gtrans {exch XOFF sub XSCL mul exch YOFF sub YSCL mul} bind def"
        "%/guntrans {exch XSCL div XOFF add exch YSCL div YOFF add} bind def"
        ""
        "/yunttrans {YTSCL aload pop exch div mul} bind def"
        "/xunttrans {XTSCL aload pop exch div mul} bind def"
        ""
        "/sign {dup 0 lt {pop -1} {0 gt {1} {0} ifelse} ifelse} bind def"
        ""
        "/zero-in-range? {dup 0 get 0 le exch 1 get 0 ge and} bind def"
        ""
        "/y-axis"
        "{ XRNG zero-in-range?"
        "    { 0 YRNG 0 get gtrans moveto 0 YRNG 1 get gtrans lineto stroke} if"
        "} bind def"
        "/x-axis"
        "{ YRNG zero-in-range?"
        "    {XRNG 0 get 0 gtrans moveto XRNG 1 get 0 gtrans lineto stroke} if"
        "} bind def"
        ""
        "% Find data range in column K of two-dimensional ARRAY."
        "%   ARRAY"
        "%   K  is the column-index into ARRAY"
        "/column-range"
        "{ /IDX exch def dup /MIN exch 0 get IDX get def /MAX MIN def"
        "  {IDX get dup dup MIN lt {/MIN exch def} {pop} ifelse"
        "	       dup MAX gt {/MAX exch def} {pop} ifelse} forall"
        "  [MIN MAX]"
        "} bind def"
        ""
        "/min {2 copy lt {pop} {exch pop} ifelse} bind def"
        "/max {2 copy gt {pop} {exch pop} ifelse} bind def"
        ""
        "/combine-ranges"
        "{ aload pop 3 2 roll aload pop exch 4 3 roll min 3 1 roll max 2 array astore}"
        "bind def"
        ""
        "/pad-range"
        "{ exch aload pop /MAX exch def /MIN exch def"
        "  /EXPSN exch 100 div MAX MIN sub mul def"
        "  [ MIN EXPSN sub  MAX EXPSN add ]"
        "} bind def"
        ""
        "/snap-range"
        "{dup aload pop exch sub 1 exch find-tick-scale aload pop"
        "     /DEN exch def /NUM exch def 1 NUM div DEN mul /DLTA exch def"
        "     aload pop /MAX exch def /MIN exch def"
        "     [ DLTA MAX MIN sub sub 2 div dup MIN exch sub exch MAX add ]"
        "} bind def"
        ""
        "% Given the width (or height) and the data-span, returns an array of"
        "% numerator and denominator [NUM DEN]"
        "%"
        "% NUM will be 1, 2, 3, 4, 5, 6, or 8 times a power of ten."
        "% DEN will be a power of ten."
        "%"
        "% NUM   ISIZ"
        "% === < ===="
        "% DEN   DLTA"
        "/find-tick-scale"
        "{/DLTA exch def /ISIZ exch def"
        " /DEN 1 def"
        " {DLTA abs ISIZ le {exit} if /DEN DEN 10 mul def /ISIZ ISIZ 10 mul def} loop"
        " /NUM 1 def"
        " {DLTA abs 10 mul ISIZ ge {exit} if /NUM NUM 10 mul def /DLTA DLTA 10 mul def} loop"
        " [[8 6 5 4 3 2 1] {/MAX exch def MAX DLTA mul ISIZ le {MAX exit} if} forall"
        "  NUM mul DEN]"
        "} bind def"
        ""
        "/rule-vertical"
        "{ /XWID exch def"
        "  /TXT exch def"
        "  /X-COORD exch def"
        "  X-COORD type [] type eq {/X-COORD X-COORD 0 get def} if"
        "  gsave"
        "    X-COORD plotrect 1 get plotrect 3 get 2 div add translate"
        "    TXT stringwidth pop -2 div"
        "    XWID 0 gt { 90 rotate PLOT-lmargin} {-90 rotate PLOT-rmargin} ifelse"
        "    pointsize 1.2 mul sub moveto TXT show"
        "  grestore"
        "  YRNG 0 get YSTEP div ceiling YSTEP mul  YSTEP  YRNG 1 get"
        "  { /YDX exch def 0 YDX gtrans /Y-COORD exch def pop"
        "    X-COORD Y-COORD moveto XWID 0 rlineto stroke"
        "    /TXT YDX squelch-.0 20 string cvs def"
        "    X-COORD"
        "    XWID 0 gt {TXT stringwidth pop sub ( ) stringwidth pop sub"
        "	       Y-COORD pointsize .3 mul sub moveto}"
        "	      {Y-COORD pointsize .3 mul sub moveto ( ) show} ifelse"
        "    TXT show} for"
        "  YRNG 0 get YSTEPH div ceiling YSTEPH mul  YSTEPH  YRNG 1 get"
        "  { /YDX exch def 0 YDX gtrans /Y-COORD exch def pop"
        "    X-COORD Y-COORD moveto XWID 2 div 0 rlineto stroke} for"
        "} bind def"
        ""
        "/rule-horizontal"
        "{ /YHIT exch def"
        "  /TXT exch def"
        "  /Y-COORD exch def"
        "  Y-COORD type [] type eq {/Y-COORD Y-COORD 1 get def} if"
        "  plotrect 0 get plotrect 2 get 2 div add TXT stringwidth pop -2 div add"
        "  Y-COORD"
        "  YHIT 0 gt {pointsize -2 mul} {pointsize 1.4 mul} ifelse add moveto TXT show"
        "  XRNG 0 get XSTEP div ceiling XSTEP mul  XSTEP  XRNG 1 get"
        "  { dup 0 gtrans pop /X-COORD exch def"
        "    X-COORD Y-COORD moveto 0 YHIT rlineto stroke"
        "    /TXT exch squelch-.0 10 string cvs def"
        "    X-COORD TXT stringwidth pop 2.0 div sub"
        "    Y-COORD YHIT 0 gt {pointsize sub} {pointsize .3 mul add} ifelse"
        "    moveto TXT show"
        "  } for"
        "  XRNG 0 get XSTEPH div ceiling XSTEPH mul  XSTEPH  XRNG 1 get"
        "  { 0 gtrans pop Y-COORD moveto 0 YHIT 2 div rlineto stroke} for"
        "} bind def"
        ""
        "/grid-verticals"
        "{ XRNG 0 get XSTEPH div ceiling XSTEPH mul  XSTEPH  XRNG 1 get"
        "  { 0 gtrans pop /X-COORD exch def"
        "    X-COORD plotrect 1 get moveto 0 plotrect 3 get rlineto} for"
        "  stroke"
        "} bind def"
        ""
        "/grid-horizontals"
        "{ YRNG 0 get YSTEPH div ceiling YSTEPH mul  YSTEPH  YRNG 1 get"
        "  { 0 exch gtrans /Y-COORD exch def pop"
        "    plotrect 0 get Y-COORD moveto plotrect 2 get 0 rlineto} for"
        "  stroke"
        "} bind def"
        ""
        "/leftedge {plotrect 0 get} bind def"
        "/rightedge {plotrect dup 0 get exch 2 get add} bind def"
        "/topedge {plotrect dup 1 get exch 3 get add} bind def"
        "/bottomedge {plotrect 1 get} bind def"
        ""
        "/outline-rect {aload pop rectstroke} bind def"
        "/fill-rect {aload pop rectfill} bind def"
        "/clip-to-rect {aload pop rectclip} bind def"
        ""
        "/gstack [] def"
        "/gpush {gsave /gstack [ gstack pointsize glyphsize ] def} bind def"
        "/gpop {/gstack gstack aload pop /glyphsize exch def /pointsize exch def def grestore} bind def"
        ""
        "/combine-font-encoding		% NAME ENCODING NAME-ENCODING"
        "{"
        "    /NAME-ENCODING exch def"
        "    /ENCODING exch def"
        "    findfont"
        "    dup length dict begin"
        "    {1 index /FID ne {def} {pop pop} ifelse} forall"
        "    /Encoding ENCODING def"
        "    currentdict"
        "    end"
        "    NAME-ENCODING exch definefont pop"
        "} bind def"
        ""
        "% Default parameters"
        ""
        "% The legend-templates are strings used to reserve horizontal space"
        "/lmargin-template (-.0123456789) def"
        "/rmargin-template (-.0123456789) def"
        ""
        "% glyphsize is the graphic-glyph size; GR, graphic radius, is"
        "% glyphsize/2.  Line width, set by \"setlinewidth\", must be much less"
        "% than glyphsize for readable glyphs."
        "/glyphsize 6 def"
        "% pointsize is the height of text characters in \"points\", 1/72 inch; 0.353.mm"
        "/pointsize 12 def"
        "% Set default font"
        "/Helvetica pointsize selectfont"
        ""
        "gsave"
        "% End of \"graph-eps\""
        ))


    ;;@node Example Graph,  , Legacy Plotting, PostScript Graphing
    ;;@subsubsection Example Graph

    ;;@noindent
    ;;The file @file{am1.5.html}, a table of solar irradiance, is fetched
    ;;with @samp{wget} if it isn't already in the working directory.  The
    ;;file is read and stored into an array, @var{irradiance}.
    ;;
    ;;@code{create-postscript-graph} is then called to create an
    ;;encapsulated-PostScript file, @file{solarad.eps}.  The size of the
    ;;page is set to 600 by 300.  @code{whole-page} is called and leaves
    ;;the rectangle on the PostScript stack.  @code{setup-plot} is called
    ;;with a literal range for x and computes the range for column 1.
    ;;
    ;;Two calls to @code{top-title} are made so a different font can be
    ;;used for the lower half.  @code{in-graphic-context} is used to limit
    ;;the scope of the font change.  The graphing area is outlined and a
    ;;rule drawn on the left side.
    ;;
    ;;Because the X range was intentionally reduced,
    ;;@code{in-graphic-context} is called and @code{clip-to-rect} limits
    ;;drawing to the plotting area.  A black line is drawn from data
    ;;column 1.  That line is then overlayed with a mountain plot of the
    ;;same column colored "Bright Sun".
    ;;
    ;;After returning from the @code{in-graphic-context}, the bottom ruler
    ;;is drawn.  Had it been drawn earlier, all its ticks would have been
    ;;painted over by the mountain plot.
    ;;
    ;;The color is then changed to @samp{seagreen} and the same graphrect
    ;;is setup again, this time with a different Y scale, 0 to 1000.  The
    ;;graphic context is again clipped to @var{plotrect}, linedash is set,
    ;;and column 2 is plotted as a dashed line.  Finally the rightedge is
    ;;ruled.  Having the line and its scale both in green helps
    ;;disambiguate the scales.

    ;;@example
    ;;(require 'eps-graph)
    ;;(require 'line-i/o)
    ;;(require 'string-port)
    ;;
    ;;(define irradiance
    ;;  (let ((url "http://www.pv.unsw.edu.au/am1.5.html")
    ;;        (file "am1.5.html"))
    ;;    (define (read->list line)
    ;;      (define elts '())
    ;;      (call-with-input-string line
    ;;        (lambda (iprt) (do ((elt (read iprt) (read iprt)))
    ;;                           ((eof-object? elt) elts)
    ;;                         (set! elts (cons elt elts))))))
    ;;    (if (not (file-exists? file))
    ;;        (system (string-append "wget -c -O" file " " url)))
    ;;    (call-with-input-file file
    ;;      (lambda (iprt)
    ;;        (define lines '())
    ;;        (do ((line (read-line iprt) (read-line iprt)))
    ;;            ((eof-object? line)
    ;;             (let ((nra (make-array (A:floR64b)
    ;;                                      (length lines)
    ;;                                      (length (car lines)))))
    ;;               (do ((lns lines (cdr lns))
    ;;                    (idx (+ -1 (length lines)) (+ -1 idx)))
    ;;                   ((null? lns) nra)
    ;;                 (do ((kdx (+ -1 (length (car lines))) (+ -1 kdx))
    ;;                      (lst (car lns) (cdr lst)))
    ;;                     ((null? lst))
    ;;                   (array-set! nra (car lst) idx kdx)))))
    ;;          (if (and (positive? (string-length line))
    ;;                   (char-numeric? (string-ref line 0)))
    ;;              (set! lines (cons (read->list line) lines))))))))
    ;;
    ;;(let ((xrange '(.25 2.5)))
    ;;  (create-postscript-graph
    ;;   "solarad.eps" '(600 300)
    ;;   (whole-page)
    ;;   (setup-plot xrange (column-range irradiance 1))
    ;;   (title-top
    ;;    "Solar Irradiance   http://www.pv.unsw.edu.au/am1.5.html")
    ;;   (in-graphic-context
    ;;    (set-font "Helvetica-Oblique" 12)
    ;;    (title-top
    ;;     ""
    ;;     "Key Centre for Photovoltaic Engineering UNSW - Air Mass 1.5 Global Spectrum"))
    ;;   (outline-rect plotrect)
    ;;   (rule-vertical leftedge "W/(m^2.um)" 10)
    ;;   (in-graphic-context (clip-to-rect plotrect)
    ;;                       (plot-column irradiance 0 1 'line)
    ;;                       (set-color "Bright Sun")
    ;;                       (plot-column irradiance 0 1 'mountain)
    ;;                       )
    ;;   (rule-horizontal bottomedge "Wavelength in .um" 5)
    ;;   (set-color 'seagreen)
    ;;
    ;;   (setup-plot xrange '(0 1000) graphrect)
    ;;   (in-graphic-context (clip-to-rect plotrect)
    ;;                       (set-linedash 5 2)
    ;;                       (plot-column irradiance 0 2 'line))
    ;;   (rule-vertical rightedge "Integrated .W/(m^2)" -10)
    ;;   ))
    ;;
    ;;(system "gv solarad.eps")
    ;;@end example

    ))

