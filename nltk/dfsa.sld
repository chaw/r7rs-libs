;;; dfsa.sls
;;; Time-stamp: <2010-04-01 09:50:00 dcavar>
;;;
;;; Copyright (C) 2010 by Damir Ćavar and Peter Garžina
;;;
;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; The Scheme NLTK is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Damir Ćavar <dcavar@unizd.hr> and Petar Garžina <petar_garzina@yahoo.com>
;;
;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Commentary:
;; Bug: we do not eliminate merged state sub-automata completely in the minimization
;;      procedure, this needs to be fixed ASAP
;;      Example: Check the extended test-dfsa.scm script for the last DOT output, where
;;      the unnexxesary subtrees are still present!
;;

(define-library
 (nltk dfsa)
 (export reverse-transitions ; !
         state-symbol ; !
         dfsa-accept?
         dfsa-has-transitions?
         dfsa-transitions-count
         dfsa-states-count
         dfsa-final-states-count
         dfsa-is-final-state?
         dfsa?
         make-dfsa
         dfsa-finalstates
         dfsa-finalstates-set!
         dfsa-alphabet
         dfsa-alphabet-set!
         dfsa-states
         dfsa-states-set!
         dfsa-transitions
         dfsa-transitions-set!
         dfsa-startstate
         dfsa-startstate-set!
         dfsa->dot
         dfsa-add-state!
         dfsa-set-finalstate!
         dfsa-add-symbol!
         dfsa-add-transition!
         dfsa-get-transition
         dfsa-all-transitions
         sequence->dfsa
         sequence-list->dfsa
         dfsa-minimize!
         dfsa-union
         dfsa-concatenate)
 (import (scheme base)
         (scheme case-lambda)
         (scheme write)
         (nltk sequence)
         (srfi 1)
         (srfi 69))
 
 (begin

   (define dfsa-accept?
     (lambda (automaton seq)
       (let ((myseq (cond ((vector? seq) (vector->list seq))
                          ((string? seq) (string->list seq))
                          ((list? seq) seq)
                          (else (error "invalid seq"))))
             (trans (dfsa-transitions automaton)))
         (let loop ((state   (dfsa-startstate automaton))
                    (symbols myseq))
           (cond ((null? symbols)
                  (if (dfsa-is-final-state? automaton state)
                    #t
                    #f))
                 ((hash-table-exists? trans (vector state (car symbols)))
                  (loop (hash-table-ref/default trans (vector state (car symbols)) -1) (cdr symbols)))
                 (else
                   #f))))))


   (define dfsa-has-transitions?
     (lambda (automaton)
       (if (> (hash-table-size (dfsa-transitions automaton)) 0)
         #t
         #f)))


   (define dfsa-transitions-count
     (lambda (automaton)
       (hash-table-size (dfsa-transitions automaton))))


   (define dfsa-states-count
     (lambda (automaton)
       (length (dfsa-states automaton))))


   (define dfsa-final-states-count
     (lambda (automaton)
       (length (dfsa-finalstates automaton))))


   (define dfsa-is-final-state?
     (lambda (automaton state)
       (if (member state (dfsa-finalstates automaton))
         #t
         #f)))


   (define-record-type <dfsa>
                       (%dfsa finalstates alphabet states transitions startstate)
                       dfsa?
                       (finalstates dfsa-finalstates dfsa-finalstates-set!)
                       (alphabet    dfsa-alphabet    dfsa-alphabet-set!)
                       (states      dfsa-states      dfsa-states-set!)
                       (transitions dfsa-transitions dfsa-transitions-set!)
                       (startstate  dfsa-startstate  dfsa-startstate-set!))

   (define (make-dfsa) (%dfsa '() '() '( 0 ) (make-hash-table equal?) 0))

   ; returns a string for an automaton
   (define dfsa->dot
     (lambda (automaton) ; transitions start-state final-states states)
       (if (dfsa? automaton)
         (let ((port (open-output-string)))
           (call-with-port
             port
             (lambda (p)
               (display "digraph fsa {" p)(newline p)
               (display "rankdir=LR;" p)(newline p)
               (display "node ( shape=doublecircle )" p)(newline p)
               (for-each
                 (lambda (st)
                   (display st p)
                   (display " " p))
                 (dfsa-finalstates automaton))
               (display ";" p)(newline p)
               (display "node ( shape = circle );" p)(newline p)
               (let-values (((keys values) (dfsa-all-transitions automaton)))
                           (vector-for-each
                             (lambda (key value)
                               (display (vector-ref key 0) p)
                               (display " -> " p)
                               (display value p)
                               (display " ( label=" p)
                               (display (string (vector-ref key 1)) p)
                               (display " );" p)(newline p))
                             keys values))
               (display "}" p)(newline p)
               (get-output-string port))))
         "")))


   (define dfsa-add-state!
     (lambda (automaton state)
       (cond ((dfsa? automaton)
              (let ((states (dfsa-states automaton)))
                (cond ((not (member state states))
                       (dfsa-states-set! automaton (append states
                                                           (list state))))))))))


   (define dfsa-set-finalstate!
     (lambda (automaton state)
       (cond ((dfsa? automaton)
              (let ((states (dfsa-finalstates automaton)))
                (cond ((not (member state states))
                       (dfsa-finalstates-set! automaton (append states
                                                                (list state))))))))))


   (define dfsa-add-symbol!
     (lambda (automaton symb)
       (cond ((dfsa? automaton)
              (let ((symbols (dfsa-alphabet automaton)))
                (cond ((not (member symb symbols))
                       (dfsa-alphabet-set! automaton (append symbols
                                                             (list symb))))))))))


   (define dfsa-add-transition!
     (lambda (automaton from-state symb to-state . weight)
       (cond ((dfsa? automaton)
              (let ((transitions (dfsa-transitions automaton))
                    (state-symb  (vector from-state symb)))
                (let ((goal   (hash-table-ref/default transitions state-symb #f))
                      (new-goal (if (null? weight)
                                  to-state
                                  (append (list to-state) weight))))
                  (cond ((not goal)
                         (hash-table-set! transitions state-symb new-goal))
                        (else
                          (cond ((list? goal)
                                 (cond ((not (member to-state goal))
                                        (hash-table-set! transitions state-symb (append goal (list new-goal))))))))))
                (dfsa-transitions-set! automaton transitions))))))


   (define dfsa-get-transition
     (lambda (automaton from-state symb)
       (cond ((dfsa? automaton)
              (hash-table-ref/default (dfsa-transitions automaton) (vector from-state symb) -1)))))


   (define dfsa-all-transitions
     (lambda (automaton)
       (if (dfsa? automaton)
         (values (apply vector (hash-table-keys (dfsa-transitions automaton)))
                 (apply vector (hash-table-values (dfsa-transitions automaton))))
         (error "dfsa-all-transitions expected a dfsa"))))


   (define sequence->dfsa
     (case-lambda
       ((sequence)                      (sequence->dfsa sequence (make-dfsa)))
       ((sequence automaton)            (sequence->dfsa sequence automaton '() ))
       ((sequence automaton goalstates) (sequence->dfsa sequence automaton goalstates '() ))
       ((sequence automaton goalstates string-classes)
        (let ((sequence (cond ((string? sequence)
                               (list->vector (string->list sequence)))
                              ((list? sequence)
                               (list->vector sequence))
                              (else
                                (error "sequence must be a string or a list")))))
          (cond ((vector? sequence)
                 (let ((current-state (dfsa-startstate automaton)))
                   (for-each
                     (lambda (symbol-index)
                       (let* ((current-symbol (vector-ref sequence symbol-index))
                              (key            (cons current-state current-symbol))
                              (state          (dfsa-get-transition automaton current-state current-symbol)))
                         (cond ((= state -1)
                                (let ((newstate (if (> (length (dfsa-states automaton)) 1)
                                                  (+ (apply max (dfsa-states automaton)) 1)
                                                  (+ (car (dfsa-states automaton) ) 1))))
                                  ; add new transition
                                  (dfsa-add-transition! automaton current-state current-symbol newstate)
                                  ; add symbol to alphabet
                                  (dfsa-add-symbol! automaton current-symbol)
                                  ; add new state to automaton
                                  (dfsa-add-state! automaton newstate)
                                  ; make newstate the current-state
                                  (set! current-state newstate)
                                  ))
                               (else
                                 (begin
                                   (set! current-state state))))))
                     (enum-list (vector-length sequence)))
                   ; add current-state to FinalStates at the end of the word
                   (dfsa-set-finalstate! automaton current-state)))
                (else
                  (error "Sequence not a vector")))
          automaton))))


   (define sequence-list->dfsa
     (case-lambda
       ((sequence-list) (sequence-list->dfsa sequence-list (make-dfsa)))
       ((sequence-list automaton)
        (begin
          (for-each
            (lambda (token)
              (sequence->dfsa token automaton))
            sequence-list)
          automaton))))


   ; key = goal state
   ; value = list of transitions to key
   (define reverse-transitions
     (lambda (automaton)
       (let ((goals-transitions (make-hash-table equal?)))
         (let-values (((keys values) (values (apply vector (hash-table-keys (dfsa-transitions automaton)))
                                             (apply vector (hash-table-values (dfsa-transitions automaton))))))
                     (vector-for-each
                       (lambda (key value)
                         (let ((new-value (hash-table-ref/default goals-transitions value '())))
                           (hash-table-set! goals-transitions value (append new-value (list key)))))
                       keys values))
         goals-transitions)))


   (define state-symbol
     (lambda (automaton)
       (let ((stsym (make-hash-table equal?)))
         (vector-for-each
           (lambda (key)
             (hash-table-set! stsym (vector-ref key 0)
                             (append (hash-table-ref/default stsym (vector-ref key 0) '())
                                     (list (vector-ref key 1)))))
           (apply vector (hash-table-keys (dfsa-transitions automaton))))
         stsym)))



   (define dfsa-minimize!
     (lambda (automaton)
       (let ((goal-trans         (reverse-transitions automaton))
             (state-emit-symbols (state-symbol automaton))
             (trans              (dfsa-transitions automaton))
             (str-res (make-hash-table equal?)))
         ; merge all absolutely final states into one
         (let ((absolute-final-states (filter (lambda (el)
                                                (if (null? (hash-table-ref/default state-emit-symbols el '())) #t #f))
                                              (dfsa-finalstates automaton))))
           (dfsa-merge-states! automaton
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
           ; start from the absolutely final state and try to merge states
           (let loop1 ((agenda (list (car absolute-final-states))))
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
                                 (let ((remstates (cdr statelist)))
                                   (dfsa-merge-states! automaton (car statelist)
                                                       (do ((x (cdr statelist) (cdr x))
                                                            (res '() (append res (hash-table-ref/default goal-trans (car x) '()))))
                                                         ((null? x) res))
                                                       remstates
                                                       (filter (lambda (elem)
                                                                 (if (member (vector-ref elem 0) remstates) #t #f))
                                                               (hash-table-ref/default goal-trans (car agenda) '())))
                                   (display "Merged states: ")
                                   (display statelist)(newline))
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
                 (loop1 (cdr agenda)))))))
       automaton))


   (define dfsa-merge-states!
     (lambda (aut to-state trans-list state-list remtrans)
       (display "in dfsa-merge-states!")(newline)
       (display "to-state: ")(display to-state)(newline)
       (display "trans-list: ")(display trans-list)(newline)
       (display "state-list: ")(display state-list)(newline)
       (display "remtrans: ")(display remtrans)(newline)
       ; remove from list of final states
       (dfsa-finalstates-set! aut (filter (lambda (el)
                                            (if (member el state-list) #f #t))
                                          (dfsa-finalstates aut)))
       ; remove from list of states
       (dfsa-states-set! aut (filter (lambda (el)
                                       (if (member el state-list) #f #t))
                                     (dfsa-states aut)))
       ; change transitions to point to the new final state
       (let ((trans (dfsa-transitions aut)))
         (for-each
           (lambda (transition)
             (hash-table-set! trans transition to-state))
           trans-list)
         (for-each
           (lambda (transition)
             (hash-table-delete! trans transition))
           remtrans))))

;; Not implemented
;   (define dfsa-serialize
;     (lambda (automaton port)
;       #t))

;   (define dfsa-deserialize
;     (lambda (automaton-port)
;       #t))

;; TODO: these do nothing
   (define dfsa-union
     (lambda (fsa1 fsa2)
       fsa1))


   (define dfsa-concatenate
     (lambda (fsa1 fsa2)
       fsa1))

   ))

