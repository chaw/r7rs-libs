;;; text/json/tools.scm - JSON structure utilities
;;;
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; Repackaged for R7RS Scheme by Peter Lane, 2017

;; the APIs' name and behaviours are highly inspired by
;; SXPath and related libraries


;; JSON structure memo;
;; The structure must be the same as Chicken's json egg which is
;; ported to Sagittarius and Mosh. (I may add it to this package
;; to make this R6RS compatible).
;; 
;; {}: map      => vector
;; []: array    => list
;; true, false  => boolean
;; null         => 'null (this may not be the same per implementation...)
;; string       => string
;; number       => number

(define-library 
  (rebottled json-tools)
    (export <json:nodeset>
	    json:nodeset?
	    json:nodeset->list
	    json:nodeset
	    json:nodeset-set
	    json:empty-nodeset
	    json:empty-nodeset?
	    json:as-nodeset
	    json:union-nodeset

	    json:node?
	    json:node-value
	    json:node

	    <json:map>
	    json:map
	    json:map?
	    json:map-size
	    json:map-ref

	    <json:map-entry>
	    json:map-entry
	    json:map-entry?
	    json:map-entry-key
	    json:map-entry-value

	    <json:array>
	    json:array
	    json:array?
	    json:array-elements
	    json:array-ref
	    json:array-length
	    
	    <json:string>
	    json:string
	    json:string?
	    
	    <json:number>
	    json:number
	    json:number?

	    <json:boolean>
	    json:boolean
	    json:boolean?

	    <json:null>
	    json:null
	    json:null?

	    <json:binary>
	    json:binary
	    json:binary?
	    ;; common utilities
	    json:filter
	    json:child-nodes-as-list
	    json:child-nodes
	    json:child-as-list
	    ;; selectors
	    json:parent
	    json:child
	    json:ancestor
	    json:descendant
	    json:descendant-or-self
	    json:following-sibling
	    json:following-sibling-or-self
	    json:preceding-sibling
	    json:preceding-sibling-or-self
	    json:sibling		; for convenience
	    json:sibling-or-self
	    )
    (import (scheme base) 
	    (only (scheme list) append-map delete-duplicates))

    (begin

      ;; atoms

      (define-record-type <json:string> 
                          (new-json:string value)
                          json:string?
                          (value json:string-value))

      (define (json:string value)
        (unless (string? value) (error 'json:string "given value is not a string"))
        (new-json:string value))

      (define-record-type <json:number>
                          (new-json:number value)
                          json:number?
                          (value json:number-value))

      (define (json:number value)
        (unless (number? value) (error 'json:number "given value is not a number"))
        (new-json:number value))

      (define-record-type <json:boolean> 
                          (new-json:boolean value)
                          json:boolean?
                          (value json:boolean-value))

      (define (json:boolean value)
        (unless (boolean? value) (error 'json:boolean "given value is not a boolean"))
        (new-json:boolean value))

      (define-record-type <json:binary>
                          (new-json:binary value)
                          json:binary?
                          (value json:binary-value))

      (define (json:binary value)
        (unless (bytevector? value) (error 'json:binary "given value is not a bytevector"))
        (new-json:binary value))

      (define-record-type <json:null> 
                          (new-json:null value)
                          json:null?
                          (value json:null-value))

      (define (json:null value)
        (unless (eq? value 'null) (error 'json:null "given value is not null"))
        (new-json:null value))

      (define-record-type <json:array> 
                          (new-json:array value elements)
                          json:array?
                          (value json:array-value)
                          (elements json:array-elements))

      (define (json:array elements)
        (new-json:array elements (map json:node elements)))

      ;; map
      (define-record-type <json:map-entry> 
                          (new-json:map-entry value key val)
                          json:map-entry?
                          (value json:mapentry-value)
                          (key json:map-entry-key)
                          (val json:map-entry-value))

    (define (json:map-entry kv)
      (new-json:map-entry kv (json:node (car kv)) (json:node (cdr kv))))

      (define-record-type <json:map> 
                          (new-json:map value entries)
                          json:map?
                          (value json:map-value)
                          (entries json:map-entries))

      (define (json:map vec)
        (new-json:map vec (vector-map json:map-entry vec))) 

      (define (json:node o)
        (cond ((json:node? o)  o)
              ((vector? o) 	   (json:map o))
              ;; this makes a bit trouble with map-entry
              ((list? o)   	   (json:array o))
              ((string? o) 	   (json:string o))
              ((number? o) 	   (json:number o))
              ((boolean? o)    (json:boolean o))
              ((bytevector? o) (json:binary o))
              ((eq? o 'null)   (json:null o))
              (else (error 'json:node "unsupported value" o))))

      (define (json:node? o)
        (or (json:string? o)
            (json:number? o)
            (json:boolean? o)
            (json:binary? o)
            (json:null? o)
            (json:array? o)
            (json:map-entry? o)
            (json:map? o)))

      (define (json:node-value o)
        (cond ((json:string? o) (json:string-value o))
              ((json:number? o) (json:number-value o))
              ((json:boolean? o) (json:boolean-value o))
              ((json:binary? o) (json:binary-value o))
              ((json:null? o) (json:null-value o))
              ((json:array? o) (json:array-value o))
              ((json:map-entry? o) (json:mapentry-value o))
              ((json:map? o) (json:map-value o))))

      (define (json:map-size o) (vector-length o)) ; (json:map-entries (vector-length o)))
      (define (json:map-ref o key . default)
        (let ((len (json:map-size o))
              (value (json:node-value o)))
          (let loop ((i 0))
            (if (= i len)
              (if (pair? default)
                (car default)
                (error 'json:map-ref "no entry found" key))
              (let ((e (vector-ref value i)))
                (if (equal? key (json:map-entry-key e))
                  (json:map-entry-value e)
                  (loop (+ i 1))))))))

      (define (json:array-ref o n . default)
        (unless (json:array? o) (error 'json:array-ref "json:array required" o))
        (apply list-ref (json:array-elements o) n default))
      (define (json:array-length o)
        (unless (json:array? o) (error 'json:array-ref "json:array required" o))
        (length (json:node-value o)))

      ;; nodeset
      ;; introduce nodeset type. since the JSON structure doesn't have
      ;; any hint if it's nodeset or not. e.g) array can contain anything
      ;; to make sure it we introduce this
      (define-record-type <json:nodeset> 
                          (new-json:nodeset set)
                          json:nodeset?
                          (set json:nodeset-set))

      (define json:nodeset 
        (lambda set
          (new-json:nodeset (delete-duplicates 
                              (map json:node set)
                              (lambda (a b)
                                (or (eq? a b) ;; easy case
                                    ;; the same but checks inside
                                    (eq? (json:node-value a)
                                         (json:node-value b))))))))

      (define (json:nodeset->list nodeset)
        (let loop ((set (json:nodeset-set nodeset)) 
                   (r '()))
          (if (null? set)
            (reverse r)
            (loop (cdr set) (cons (json:node-value (car set)) r)))))

      (define *json:empty-nodeset* (json:nodeset))
      (define (json:empty-nodeset) *json:empty-nodeset*)
      (define (json:empty-nodeset? nodeset)
        (and (json:nodeset? nodeset) (null? (json:nodeset-set nodeset))))
      (define (json:as-nodeset node)
        (if (json:nodeset? node) node (json:nodeset (json:node node))))
      ;; shortcut
      (define (json:as-nodeset->list node)
        (if (json:nodeset? node) (json:nodeset-set node) (list (json:node node))))
      ;; utilities for nodeset
      (define (json:union-nodeset nodeset-list)
        (let ((sets (append-map json:nodeset-set nodeset-list)))
          (apply json:nodeset sets)))

      ;; misc utils
      ;; returns nodeset
      (define (json:map-union proc node-list)
        (let loop ((nodes node-list) (r '()))
          (if (null? nodes)
            (json:union-nodeset (reverse r))
            (loop (cdr nodes) (cons (proc (car nodes)) r)))))

      ;; tools
      (define (json:filter pred?)
        (lambda (node-list)
          (let loop ((lst (json:as-nodeset->list node-list)) 
                     (r '()))
            (if (null? lst)
              (apply json:nodeset (reverse r))
              (let ((result (pred? (car lst))))
                (loop (cdr lst) (if (and result (not (null? result)))
                                  (cons (car lst) r)
                                  r)))))))

      (define (json:child-nodes node)
        (let ((node (if (json:node? node) node (json:node node))))
          (cond ((json:map? node)
                 (apply json:nodeset (vector->list (json:map-entries node))))
                ((json:array? node)
                 (apply json:nodeset (json:array-elements node)))
                ((json:map-entry? node)
                 (json:nodeset (json:map-entry-value node)))
                (else *json:empty-nodeset*))))
      (define (json:child-nodes-as-list node)
        (json:nodeset-set (json:child-nodes node)))

      ;; selectors
      (define (json:child pred?)
        (lambda (node)
          (if (json:nodeset? node)
            (json:map-union (json:child pred?) (json:nodeset-set node))
            (let loop ((nodes (json:child-nodes-as-list node)) (r '()))
              (if (null? nodes)
                (apply json:nodeset (reverse r))
                (loop (cdr nodes)
                      (if (pred? (car nodes))
                        (cons (car nodes) r)
                        r)))))))

      (define (json:child-as-list pred?)
        (lambda (node)
          (json:nodeset-set ((json:child pred?) node))))

      (define (target? node node/raw)
        (let ((v (json:node-value node)))
          (or (eq? v node/raw)
              (and (json:node? node/raw)
                   (eq? v (json:node-value node/raw))))))

      (define (json:parent pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:parent pred?) root-node)
                              (json:nodeset-set node))
              ;; pairs ::= ((child . parent) ...)
              (let loop ((pairs (append-map
                                  (lambda (root-n)
                                    (map (lambda (arg) (cons arg root-n))
                                         (json:child-nodes-as-list root-n)))
                                  (json:as-nodeset->list root-node))))
                (if (null? pairs)
                  *json:empty-nodeset*
                  (let ((pair (car pairs)))
                    (if (target? (car pair) node)
                      ((json:filter pred?) (cdr pair))
                      (loop (append
                              (map (lambda (arg) (cons arg (car pair)))
                                   (json:child-nodes-as-list (car pair)))
                              (cdr pairs)))))))))))

      (define (json:ancestor pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:ancestor pred?) root-node)
                              (json:nodeset-set node))
              ;; paths ::= ((child parents ...) ...)
              (let loop ((paths (list (json:as-nodeset->list root-node))))
                (if (null? paths)
                  *json:empty-nodeset*
                  (let ((path (car paths)))
                    (if (target? (car path) node)
                      ((json:filter pred?) 
                       (apply json:nodeset (cdr path)))
                      (loop (append
                              (map (lambda (arg) (cons arg path))
                                   (json:child-nodes-as-list (car path)))
                              (cdr paths)))))))))))

      (define (json:descendant pred?)
        (lambda (node)
          (if (json:nodeset? node)
            (json:map-union (json:descendant pred?) (json:nodeset-set node))
            (let loop ((r '())
                       (more ((json:child-as-list json:node?) node)))
              (if (null? more)
                (apply json:nodeset (reverse r))
                (loop (if (pred? (car more)) (cons (car more) r) r)
                      (append ((json:child-as-list json:node?) (car more))
                              (cdr more))))))))
      (define (json:descendant-or-self pred?)
        (lambda (node)
          (if (json:nodeset? node)
            (json:map-union (json:descendant-or-self pred?)
                            (json:nodeset-set node))
            (let loop ((r '())
                       (more (list (json:node node))))
              (if (null? more)
                (apply json:nodeset (reverse r))
                (loop (if (pred? (car more)) (cons (car more) r) r)
                      (append ((json:child-as-list json:node?) (car more))
                              (cdr more))))))))

      (define (json:following-sibling pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:following-sibling pred?) root-node)
                              (json:nodeset-set node))
              ;; seqs ::= ((child siblings ...) ...)
              (let loop ((seqs (list (json:as-nodeset->list root-node))))
                (if (null? seqs)
                  *json:empty-nodeset*
                  (let rpt ((seq (car seqs)))
                    (cond ((null? seq)
                           (loop (append (map (json:child-as-list json:node?)
                                              (car seqs))
                                         (cdr seqs))))
                          ((target? (car seq) node)
                           ((json:filter pred?) 
                            (apply json:nodeset (cdr seq))))
                          (else (rpt (cdr seq)))))))))))

      (define (json:following-sibling-or-self pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:following-sibling-or-self pred?) root-node)
                              (json:nodeset-set node))
              ;; seqs ::= ((child siblings ...) ...)
              (let loop ((seqs (list (json:as-nodeset->list root-node))))
                (if (null? seqs)
                  *json:empty-nodeset*
                  (let rpt ((seq (car seqs)))
                    (cond ((null? seq)
                           (loop (append (map (json:child-as-list json:node?)
                                              (car seqs))
                                         (cdr seqs))))
                          ((target? (car seq) node)
                           ((json:filter pred?) (apply json:nodeset seq)))
                          (else (rpt (cdr seq)))))))))))

      (define (json:preceding-sibling pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:preceding-sibling pred?) root-node)
                              (json:nodeset-set node))
              ;; seqs ::= ((child siblings ...) ...)
              (let loop ((seqs (list (json:as-nodeset->list root-node))))
                (if (null? seqs)
                  *json:empty-nodeset*
                  (let rpt ((seq (car seqs)))
                    (cond ((null? seq)
                           (loop (append 
                                   (map (lambda (n) 
                                          (reverse
                                            ((json:child-as-list json:node?) n)))
                                        (car seqs))
                                   (cdr seqs))))
                          ((target? (car seq) node)
                           ((json:filter pred?) 
                            (apply json:nodeset (reverse (cdr seq)))))
                          (else (rpt (cdr seq)))))))))))

      (define (json:preceding-sibling-or-self pred?)
        (lambda (root-node)
          (lambda (node)
            (if (json:nodeset? node)
              (json:map-union ((json:preceding-sibling-or-self pred?) root-node)
                              (json:nodeset-set node))
              ;; seqs ::= ((child siblings ...) ...)
              (let loop ((seqs (list (json:as-nodeset->list root-node))))
                (if (null? seqs)
                  *json:empty-nodeset*
                  (let rpt ((seq (car seqs)))
                    (cond ((null? seq)
                           (loop (append 
                                   (map (lambda (n) 
                                          (reverse
                                            ((json:child-as-list json:node?) n)))
                                        (car seqs))
                                   (cdr seqs))))
                          ((target? (car seq) node)
                           ((json:filter pred?) 
                            (apply json:nodeset (reverse seq))))
                          (else (rpt (cdr seq)))))))))))

      (define (json:sibling pred?)
        (lambda (root-node)
          (lambda (node)
            (define ->list json:nodeset-set)
            (define (merge p f)
              (cond ((null? p) f)
                    ((null? f) p)
                    (else
                      `(,@p ,@f))))
            (apply json:nodeset
                   (merge
                     (->list (((json:preceding-sibling pred?) root-node) node))
                     (->list (((json:following-sibling pred?) root-node) node)))))))

      (define (json:sibling-or-self pred?)
        (lambda (root-node)
          (lambda (node)
            (define ->list json:nodeset-set)
            (define (merge p f)
              (cond ((null? p) f)
                    ((null? f) p)
                    (else
                      `(,@p ,@f))))
            (apply json:nodeset
                   (merge
                     (->list (((json:preceding-sibling pred?) root-node) node))
                     (->list (((json:following-sibling-or-self pred?) root-node)
                              node)))))))
      ))

