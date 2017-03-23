;; Original Test Suite from https://github.com/ijp/pfds
;; converted to use SRFI 64 tests by Peter Lane

(import (scheme base) (scheme write)
        (pfds hash-array-mapped-trie)
        (only (srfi 1) fold iota)
        (srfi 64)
        (only (srfi 69) string-hash)
        (srfi 95))

;; check that given code raises an error when turned to tokens
(define-syntax check-error
  (syntax-rules ()
    ((check-error code)
     (guard (err
              (else (test-assert #t)))
            code
            (test-assert #f)))))

(define (test-compare proc l1 l2)
  (test-assert (proc l1 l2)))

(define (make-string-hamt)
  (make-hamt string-hash string=?))

(define (compare-string-alist l1 l2)
  (lambda (l1 l2)
    (define (compare x y) (string<? (car x) (car y)))
    (equal? (sort compare l1)
            (sort compare l2))))

(define (bad-hash x) 0)

(define (add1 n) (+ 1 n))

(test-begin "pfds-hash-array-mapped-trie")

;; empty-hamt
(test-assert (hamt? (make-string-hamt)))
(test-equal 0 (hamt-size (make-string-hamt)))

;; hamt-ref/set
;; Referencing non-existent key
(test-equal #f (hamt-ref (make-string-hamt) "foo" #f))
;; Referencing a non-existent key (exception)
(check-error (hamt-ref (make-string-hamt) "bar"))
;; Referencing newly-added key
(test-equal "bar" (hamt-ref (hamt-set (make-string-hamt) "foo" "bar") "foo" #f))
(test-equal 1 (hamt-size (hamt-set (make-string-hamt) "foo" "bar")))
;; shadowing an existing key
(test-equal "baz" (hamt-ref (hamt-set (hamt-set (make-string-hamt) "foo" "bar") "foo" "baz") "foo" #f))
(test-equal 1 (hamt-size (hamt-set (hamt-set (make-string-hamt) "foo" "bar") "foo" "baz")))

;; hamt-contains 
(let ((h (hamt-set (make-string-hamt) "foo" 1)))
  (test-equal #t (hamt-contains? h "foo")))
(let ((h (hamt-set (make-string-hamt) "foo" 1)))
  (test-equal #f (hamt-contains? h "bar")))

;; hamt-conversion
;; alist->hamt / distinct keys
(let* ((l '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l string-hash string=?)))
  (test-equal (list 1 2 3)
              (map (lambda (x) (hamt-ref h x #f)) (list "a" "b" "c"))))
;; alist->hamt / overlapping keys (leftmost shadows)
(let* ((l '(("a" . 1) ("b" . 2) ("c" . 3) ("a" . 4)))
       (h (alist->hamt l string-hash string=?)))
  (test-equal (list 1 2 3)
              (map (lambda (x) (hamt-ref h x #f)) (list "a" "b" "c"))))
;; hamt->alist / distinct keys means left inverse
(let ((l '(("a" . 1) ("b" . 2) ("c" . 3))))
  (display (alist->hamt l string-hash string=?)) (newline)
  (hamt->alist (alist->hamt l string-hash string=?))
  (test-compare compare-string-alist l
                (hamt->alist (alist->hamt l string-hash string=?))))

;; hamt-folding 
;; count size
(let ((h (alist->hamt '(("a" . 1) ("b" . 2) ("c" . 3)) string-hash string=?))
      (increment (lambda (k v acc) (+ 1 acc))))
  (test-equal 3 (hamt-fold increment 0 h)))
;; copy hamt
(let* ((l '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l string-hash string=?))
       (add (lambda (k v acc) (hamt-set acc k v))))
  (test-compare compare-string-alist l
                (hamt->alist (hamt-fold add (make-string-hamt) h))))

;; hamt-removal
;; removed key exists
(let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l string-hash string=?)))
  (test-compare compare-string-alist '(("b" . 2) ("c" . 3)) (hamt-delete h "a"))
  (test-equal (- (hamt-size h) 1) (hamt-size (hamt-delete h "a"))))
;; removed key does not exist
(let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l string-hash string=?)))
  (test-compare compare-string-alist l (hamt-delete h "d"))
  (test-equal (hamt-size h) (hamt-size (hamt-delete h "d"))))

;; hamt-updates
;; update non-existent key
(test-equal 1 (hamt-ref (hamt-update (make-string-hamt) "foo" add1 0) "foo" #f))
;; update existing key
(let ((h (hamt-set (make-string-hamt) "foo" 12)))
  (test-equal 13 (hamt-ref (hamt-update h "foo" add1 0) "foo" #f)))

;; hamt-collisions
;; a bad hash function does not cause problems
(let* ((l  '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l bad-hash string=?)))
  (test-compare compare-string-alist l (hamt->alist h)))
;; stress test, since bigger amounts data usually finds bugs
(let ((insert (lambda (val hamt) (hamt-set hamt val val)))
      (hash   (lambda (n) (exact (floor (/ n 2))))))
  (test-equal 100 (hamt-size (fold insert (make-hamt hash =) (iota 100)))))
;; collision removal
(let* ((l '(("a" . 1) ("b" . 2) ("c" . 3) ("d" . 4)))
       (h (alist->hamt l bad-hash string=?)))
  (test-compare compare-string-alist '()
                (fold (lambda (str hamt) (hamt-delete hamt str))
                       h
                       '("b" "notexists" "d" "a" "c" "notexists"))))
;; stress test removal
(let* ((al (map (lambda (x) (cons x #t)) (iota 100)))
       (hash   (lambda (n) (exact (floor (/ n 2)))))
       (h (alist->hamt al hash =)))
  (test-equal 94 (hamt-size (fold (lambda (s h) (hamt-delete h s))
                                   h
                                   (list 1 93 72 6 24 48)))))
;; collision updates
(let* ((l '(("a" . 1) ("b" . 2) ("c" . 3)))
       (h (alist->hamt l bad-hash string=?)))
  (test-compare compare-string-alist
                '(("a" . 2) ("b" . 3) ("c" . 4))
                (fold (lambda (key hamt)
                         (hamt-update hamt key add1 0))
                       h
                       '("a" "b" "c"))))

;; hamt-mapping
(let* ((l '(("a" . 97) ("b" . 98) ("c" . 99)))
       (h (alist->hamt l string-hash string=?)))
  (test-compare compare-string-alist l
                (hamt->alist (hamt-map (lambda (x) x) h))))
(let* ((l '(("a" . 97) ("b" . 98) ("c" . 99)))
       (h (alist->hamt l string-hash string=?))
       (stringify (lambda (n) (string (integer->char n)))))
  (test-compare compare-string-alist
                '(("a". "a") ("b" . "b") ("c" . "c"))
                (hamt->alist (hamt-map stringify h))))
(let ((h (alist->hamt '(("a" . 97) ("b" . 98) ("c" . 99)) string-hash string=?)))
  (test-equal (hamt-size h) (hamt-size (hamt-map (lambda (x) x) h))))

(test-end)

