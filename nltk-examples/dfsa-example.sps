;;; filename: test-dfsa.scm
;;;
;;; Time-stamp: <2010-02-28 01:15:00 dcavar>
;;; encoding: UTF-8
;;;
;;; Copyright (C) 2010 by Damir Ćavar. 
;;;
;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the Creative Commons
;;; GNU Lesser General Public License as published by the
;;; Free Software Foundation; either version 2.1 of the License,
;;; or (at your option) any later version.
;;;
;;; The Scheme NLTK is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the Creative Commons GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA
;;;
;;; Author: Damir Ćavar <dcavar@unizd.hr>
;;
;;
;; Commentary:
;; 
;; TODO:

(import (scheme base)
        (scheme write)
        (nltk dfsa)
        (nltk sequence)
        (srfi 1)
        (srfi 69))


(define dfsa-minimize2a!
  (lambda (automaton)
    (let ((goal-trans         (reverse-transitions automaton))
          (state-emit-symbols (state-symbol automaton))
          (trans              (dfsa-transitions automaton))
          (str-res (make-hash-table equal?)))
      ; merge all absolutely final states into one
      (let ((absolute-final-states (filter (lambda (el)
                                             (if (null? (hash-table-ref/default state-emit-symbols el '())) #t #f))
                                           (dfsa-finalstates automaton))))
        (dfsa-merge-states2! automaton
                             (car absolute-final-states)
                             (do ((x (cdr absolute-final-states) (cdr x))
                                  (res '() (append res (hash-table-ref/default goal-trans (car x) '()))))
                               ((null? x) res))
                             (cdr absolute-final-states)
                             '())
        (for-each ; change tranisitions to point to the new final state
         (lambda (state)
           (hash-table-set! goal-trans (car absolute-final-states)
                           (append (hash-table-ref/default goal-trans (car absolute-final-states) '())
                                   (hash-table-ref/default goal-trans state '())))
           (hash-table-delete! goal-trans state)
           (hash-table-delete! state-emit-symbols state))
         (cdr absolute-final-states))))
    automaton))


(define merge-absolute-final-states!
  (lambda (automaton)
    (let ((goal-trans         (reverse-transitions automaton))
          (state-emit-symbols (state-symbol automaton))
          (trans              (dfsa-transitions automaton))
          (str-res (make-hash-table equal?)))
      ; merge all absolutely final states into one
      (let ((absolute-final-states (filter (lambda (el)
                                             (if (null? (hash-table-ref/default state-emit-symbols el '())) #t #f))
                                           (dfsa-finalstates automaton))))
        (dfsa-merge-states2! automaton
                             (car absolute-final-states)
                             (do ((x (cdr absolute-final-states) (cdr x))
                                  (res '() (append res (hash-table-ref/default goal-trans (car x) '()))))
                               ((null? x) res))
                             (cdr absolute-final-states)
                             '())
        (for-each ; change tranisitions to point to the new final state
         (lambda (state)
           (hash-table-set! goal-trans (car absolute-final-states)
                           (append (hash-table-ref/default goal-trans (car absolute-final-states) '())
                                   (hash-table-ref/default goal-trans state '())))
           (hash-table-delete! goal-trans state)
           (hash-table-delete! state-emit-symbols state))
         (cdr absolute-final-states))
        (car absolute-final-states)))))


(define dfsa-minimize2!
  (lambda (automaton)
    (let ((goal-trans         (reverse-transitions automaton))
          (state-emit-symbols (state-symbol automaton))
          (trans              (dfsa-transitions automaton))
          (str-res            (make-hash-table equal?)))
      ; merge all absolutely final states into one
      (let ((absolute-final-state (merge-absolute-final-states! automaton)))
        ; start from the absolutely final state and try to merge states
        (let loop-agenda ((agenda (list absolute-final-state)))
          (unless (null? agenda)
            (let* ((current-final   (car agenda))
                   (entering-states (remove-duplicates (map (lambda (el)
                                                              (vector-ref el 0))
                                                            (hash-table-ref/default goal-trans current-final '())))))
              (for-each (lambda (key) (hash-table-delete! str-res key))
                        (hash-table-keys str-res))
              (for-each
               (lambda (elem)
                 ; str-res has strings as keys, list of states from to current-final as value
                 (let ((symbs (hash-table-ref/default state-emit-symbols elem '())))
                   (hash-table-set! str-res symbs (append (hash-table-ref/default str-res symbs '())
                                                         (list elem)))))
               entering-states)
              ;
              (vector-for-each ; find the str-res entry with more than one val
               (lambda (key)
                 (let ((val (hash-table-ref/default str-res key '())))
                   (when (> (length val) 1)
                     (let ((finals    (filter (lambda (el) (if (dfsa-is-final-state? automaton el) #t #f)) val))
                           (nonfinals (filter (lambda (el) (if (dfsa-is-final-state? automaton el) #f #t)) val)))
                       (for-each
                        (lambda (statelist)
                          (when (> (length statelist) 1) ; merge other states to the first
                            ; get all states that connect to any of the other states, but the first
                            ; make them all transite into the first
                            (dfsa-merge-states2! automaton (car statelist)
                                                 (do ((x (cdr statelist) (cdr x))
                                                      (res '() (append res (hash-table-ref/default goal-trans (car x) '()))))
                                                   ((null? x) res))
                                                 (cdr statelist)
                                                 (filter (lambda (elem)
                                                           (if (member (vector-ref elem 0) (cdr statelist)) #t #f))
                                                         (hash-table-ref/default goal-trans (car agenda) '())))
                            (display "Merged states: ")
                            (display statelist)(newline)
                            (for-each
                             (lambda (state) ; change goal-trans
                               (hash-table-set! goal-trans (car statelist)
                                               (append (hash-table-ref/default goal-trans (car statelist) '())
                                                       (hash-table-ref/default goal-trans state '())))
                               (hash-table-delete! goal-trans state)
                               (hash-table-delete! state-emit-symbols state))
                             (cdr statelist))
                            ; add first of statelist to agenda
                            (unless (member (car statelist) agenda)
                              (set! agenda (append agenda (list (car statelist)))))))
                        (list finals nonfinals))))))
               (apply vector (hash-table-keys str-res)))
              (loop-agenda (cdr agenda)))))))
    automaton))

(define dfsa-merge-states2!
  (lambda (aut to-state trans-list state-list remtrans)
    ;(display "in dfsa-merge-states!")(newline)
    ;(display "to-state: ")(display to-state)(newline)
    ;(display "trans-list: ")(display trans-list)(newline)
    ;(display "state-list: ")(display state-list)(newline)
    ;(display "remtrans: ")(display remtrans)(newline)
    ; remove from list of final states
    (dfsa-finalstates-set! aut (filter (lambda (el)
                                         (if (member el state-list) #f #t))
                                       (dfsa-finalstates aut)))
    ; remove from list of states
    (dfsa-states-set! aut (filter (lambda (el)
                                    (if (member el state-list) #f #t))
                                  (dfsa-states aut)))
    ; change tranisitions to point to the new final state
    (let ((trans (dfsa-transitions aut)))
      (for-each
       (lambda (transition)
         (hash-table-set! trans transition to-state))
       trans-list)
      (for-each
       (lambda (transition)
         (hash-table-delete! trans transition))
       remtrans))))

(define test1
  (lambda ()
    (let ((myfsa (sequence-list->dfsa '("there" "these" "those" "then" "that" "John" "saw" "the" "new" "house" "of" "the" "new" "landlords" "who" "live" "in" "the" "new" "part" "of" "the" "city" "test" "testing" "tested" "Lukas" "Lukasa" "Lukasu" "Lukasom"))))
      (dfsa->dot myfsa))))


(define test2
  (lambda ()
    (let ((myfsa (sequence-list->dfsa '("there" "these" "those" "then" "that" "John" "saw" "the" "new" "house" "of" "the" "new" "landlords" "who" "live" "in" "the" "new" "part" "of" "the" "city" "test" "testing" "tested" "Lukas" "Lukasa" "Lukasu" "Lukasom"))))
      (dfsa->dot (dfsa-minimize! myfsa)))))



(define test3
  (lambda (seqs)
    (let ((myfsa (sequence-list->dfsa seqs)))
      (display "States 1: ")(display (dfsa-states myfsa))(newline)
      (display "FinalStates 1: ")(display (dfsa-finalstates myfsa))(newline)
      (let ((minaut (dfsa-minimize2! myfsa)))
        (display "States: ")(display (dfsa-states minaut))(newline)
        (display "FinalStates: ")(display (dfsa-finalstates minaut))(newline)
        (dfsa->dot minaut)))))

(define test3b
  (lambda (seqs)
    (let ((myfsa (sequence-list->dfsa seqs)))
      (display "States 1: ")(display (dfsa-states myfsa))(newline)
      (display "FinalStates 1: ")(display (dfsa-finalstates myfsa))(newline)
      (dfsa->dot myfsa))))

(define test3c
  (lambda (seqs)
    (let ((myfsa (sequence-list->dfsa seqs)))
      (display "States 1: ")(display (dfsa-states myfsa))(newline)
      (display "FinalStates 1: ")(display (dfsa-finalstates myfsa))(newline)
      (dfsa->dot (dfsa-minimize2a! myfsa)))))


(define test4
  (lambda (seqs)
    (let ((myfsa (sequence-list->dfsa seqs)))
      (dfsa-accept? (dfsa-minimize! myfsa) "neradu"))))



;(display "Running test1: Wordlist to DOT, not Minimized")(newline)(newline)
;(display (test1))
;(newline)
;(newline)
;(newline)
;(display "Running test2: Wordlist to DOT, Minimized")(newline)(newline)
;(display (test2))
;(newline)
;(newline)
(let ((seqs '("piti" "pije" "pijem" "piješ" "pijemo" "pijete" "piju" "jesti" "jedem" "jedeš" "jede" "jedemo" "jedete" "jedu" "raditi" "radim" "radiš" "radi" "radimo" "radite" "rade" "radu")))
  (display "Running test3: Wordlist to DOT, Minimized")(newline)(newline)
  (display (test3 seqs))
  (newline)
  (newline)
  (display "Running test3b: Wordlist to DOT, Minimized")(newline)(newline)
  (display (test3b seqs))
  (newline)
  (newline)
  (display "Running test3c: Wordlist to DOT, Minimized")(newline)(newline)
  (display (test3c seqs))
)

