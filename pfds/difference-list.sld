;;; dlists.sls --- Difference Lists

;; Copyright (C) 2012 Ian Price <ianprice90@googlemail.com>

;; Author: Ian Price <ianprice90@googlemail.com>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; Packaged for R7RS Scheme by Peter Lane, 2017

;; ---------------------------------------------------------------------

;;; Commentary:
;;
;;> Repeatedly appending to a list is a common, if inefficient pattern
;;> in functional programs. Usually the trick we use is to build up the
;;> list in reverse, and then to reverse it as the last action of a
;;> function.
;;
;;> Dlists are a representation of lists as functions that provide for
;;> constant time append to either the front or end of a dlist that may
;;> be used instead.

(define-library 
  (pfds difference-list)
  (export dlist
          dlist?
          dlist-cons
          dlist-snoc
          dlist-append
          dlist->list
          list->dlist
          )
  (import (scheme base))

  (begin

    ;;> dlist? : any -> boolean
    ;;> returns #t if its argument is a dlist, #f otherwise.

    (define-record-type <dlist>
                        (make-dlist proc)
                        dlist?
                        (proc undl))

    (define (%dlist . args)
      (list->dlist args))

    (define (compose f g)
      (lambda (x)
        (f (g x))))

    (define (singleton x)
      (list->dlist (list x)))

    ;;> dlist-append : dlist dlist -> dlist
    ;;> returns a new dlist consisting of all the elements of the first
    ;;> dlist, followed by all the items of the second dlist.
    (define (dlist-append dl1 dl2)
      (make-dlist (compose (undl dl1) (undl dl2))))

    ;;> dlist-cons : any dlist -> dlist
    ;;> returns a new dlist created by prepending the element to the head
    ;;> of the dlist argument.
    (define (dlist-cons element dlist)
      (dlist-append (singleton element) dlist))

    ;;> dlist-snoc : dlist any -> dlist
    ;;> returns a new dlist created by appending the element to the tail of
    ;;> the dlist argument.
    (define (dlist-snoc dlist element)
      (dlist-append dlist (singleton element)))

    ;;> dlist->list : dlist -> listof(any)
    ;;> returns a list consisting of all the elements of the dlist.
    (define (dlist->list dlist)
      ((undl dlist) '()))

    ;;> list->dlist : listof(any) -> dlist
    ;;> returns a dlist consisting of all the elements of the list.
    (define (list->dlist list)
      (make-dlist
        (lambda (rest)
          (append list rest))))

    ;;> dlist : any ... -> dlist
    ;;> returns a dlist containing all its arguments.
    (define dlist %dlist)

    ))

