;; Example of using disjoint sets in implementing Kruskal's algorithm to 
;; find the minimum spanning tree of a graph.

(import (scheme base)
        (scheme cxr)
        (scheme comparator)
        (scheme list)
        (scheme hash-table)
        (scheme sort)
        (scheme write)
        (robin disjoint-set))

(define (kruskal graph)
  (let ((result '())
        (nodes (delete-duplicates
                 (append (map car graph) (map cadr graph))
                 eq?)))
    ; 1. make a disjoint set, with each node an item
    (let ((ds (make-disjoint-set (make-eq-comparator))))        ; <1>
      (for-each (lambda (node) 
                  (disjoint-set:make ds node))                  ; <2>
                nodes)
      ; 2. set 'links' holds all the links in graph, sorted
      (let loop ((links 
                   (list-sort (lambda (a b) (< (caddr a) (caddr b))) graph)))
        ; 3. if links non-empty and size > 1
        (when (and (not (null? links))
                   (> (disjoint-set:size ds) 1))                ; <3>
          (let ((link (car links)))
            (unless (eq? (disjoint-set:find ds (car link))      ; <4>
                         (disjoint-set:find ds (cadr link)))
              (set! result (cons link result))
              (disjoint-set:union ds (car link) (cadr link))))  ; <5>
          (loop (cdr links)))))
    (reverse result)))

(let* ((graph '((a b 3) (a e 1) (b c 5) (b e 4) (c d 2) (c e 6) (d e 7)))
       (res (kruskal graph)))
  (display (string-append "MST has "
                          (number->string (length res))
                          " links\n"))
  (for-each (lambda (link) (display "  : ") (display link) (newline))
            res)
  (display (string-append "Total length: "
                          (number->string (fold + 0 (map caddr res)))
                          "\n")))

