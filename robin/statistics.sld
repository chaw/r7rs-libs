;; Statistics library for R7RS Scheme

;; Written by Peter Lane, 2017
;; -- largely inspired by cl-stats.lisp, Statistical Functions in Common Lisp, by Larry Hunter

;; # Open Works License
;; 
;; This is version 0.9.4 of the Open Works License
;; 
;; ## Terms
;; 
;; Permission is hereby granted by the holder(s) of copyright or other legal
;; privileges, author(s) or assembler(s), and contributor(s) of this work, to any
;; person who obtains a copy of this work in any form, to reproduce, modify,
;; distribute, publish, sell, sublicense, use, and/or otherwise deal in the
;; licensed material without restriction, provided the following conditions are
;; met:
;; 
;; Redistributions, modified or unmodified, in whole or in part, must retain
;; applicable copyright and other legal privilege notices, the above license
;; notice, these conditions, and the following disclaimer.
;; 
;; NO WARRANTY OF ANY KIND IS IMPLIED BY, OR SHOULD BE INFERRED FROM, THIS LICENSE
;; OR THE ACT OF DISTRIBUTION UNDER THE TERMS OF THIS LICENSE, INCLUDING BUT NOT
;; LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
;; AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS, ASSEMBLERS, OR HOLDERS OF
;; COPYRIGHT OR OTHER LEGAL PRIVILEGE BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER
;; LIABILITY, WHETHER IN ACTION OF CONTRACT, TORT, OR OTHERWISE ARISING FROM, OUT
;; OF, OR IN CONNECTION WITH THE WORK OR THE USE OF OR OTHER DEALINGS IN THE WORK.

(define-library
  (robin statistics)
  (export mean
          arithmetic-geometric-mean
          arithmetic-mean
          geometric-mean
          harmonic-mean
          median
          mode
          percentile
          population-standard-deviation
          population-variance
          standard-deviation
          variance
          coefficient-of-variation
          standard-error-of-the-mean
          ;
          combinations
          permutations
          sign
          ;
          perlin-noise
          random-normal
          random-pick
          random-sample
          random-weighted-sample
          ;
          jaccard-index
          jaccard-distance
          sorenson-dice-index
          )
  (import (scheme base)
          (scheme case-lambda)
          (scheme inexact)       
          (srfi 1)
          (srfi 27)
          (srfi 69)
          (srfi 132)
          (only (srfi 151) bitwise-and))

  (begin

    (define (arithmetic-mean lst)
      (if (null? lst)
        0
        (/ (fold + 0 lst) (length lst))))

    (define mean arithmetic-mean) ;; make popular synonym

    (define arithmetic-geometric-mean
      (case-lambda
        ((a0 g0) ; call again with default value for tolerance
         (arithmetic-geometric-mean a0 g0 1e-8))
        ((a0 g0 tolerance) ; called with three arguments
         (do ((a a0 (* (+ a g) 1/2))
              (g g0 (sqrt (* a g))))
           ((< (abs (- a g)) tolerance) a)))))

    (define (geometric-mean lst)
      (if (null? lst)
        0
        (expt (fold * 1 lst) (/ 1 (length lst)))))

    (define (harmonic-mean lst)
      (if (null? lst)
        0
        (/ (length lst) 
           (fold (lambda (n v) (+ (/ 1 n) v)) 0 lst))))

    (define (median lst)
      (percentile lst 50))

    (define (mode lst)
      (if (null? lst)
        (error "Mode: List must not be null")
        (let ((count-table (make-hash-table eqv?))
              (modes '())
              (mode-count 0))
          (for-each 
            (lambda (item) 
              (hash-table-set! count-table
                               item
                               (+ 1 (hash-table-ref/default count-table item 0))))
            lst)
          (for-each 
            (lambda (key) 
              (let ((val (hash-table-ref/default count-table key #f)))
                (cond ((> val mode-count) ; keep mode
                       (set! modes (list key))
                       (set! mode-count val))
                      ((= val mode-count) ; store multiple modes
                       (set! modes (cons key modes))))))
            (hash-table-keys count-table))
          (cond ((every number? modes) (set! modes (list-sort < modes)))
                ((every string? modes) (set! modes (list-sort string<? modes)))
                )
          (values modes mode-count))))

    (define (percentile lst percent)
      (cond ((or (<= percent 0)
                 (>= percent 100))
             (error "Percentile: percent must be from 1 to 99, inclusive"))
            ((null? lst)
             (error "Percentile: List must not be null"))
            (else
              (let* ((sorted-vec (apply vector (list-sort < lst)))
                     (n (vector-length sorted-vec))
                     (k (* n (/ percent 100)))
                     (floor-k (floor k)))
                (if (= k floor-k)
                  (/ (+ (vector-ref sorted-vec k)
                        (vector-ref sorted-vec (- k 1)))
                     2)
                  (vector-ref sorted-vec floor-k))))))

    (define general-variance
      (case-lambda
        ((type lst)
         (general-variance type lst (mean lst)))
        ((type lst mean1)
         (if (< (length lst) 2)
           (error "for variances, List must contain at least 2 elements")
           (/ (fold + 0 (map (lambda (x) (square (- mean1 x))) lst))
              (case type
                ((sample)
                 (- (length lst) 1))
                ((population)
                 (length lst))
                (else
                  (error "Internal error: unknown type for general-variance"))))))))

    (define general-standard-deviation
      (case-lambda 
        ((type lst)
         (general-standard-deviation type lst (mean lst)))
        ((type lst mean1)
         (sqrt (general-variance type lst mean1)))))

    (define (variance . args)
      (apply general-variance (cons 'sample args)))

    (define (standard-deviation . args)
      (apply general-standard-deviation (cons 'sample args)))

    (define (population-variance . args)
      (apply general-variance (cons 'population args)))

    (define (population-standard-deviation . args)
      (apply general-standard-deviation (cons 'population args)))

    (define (coefficient-of-variation lst)
      (* 100
         (/ (standard-deviation lst)
            (mean lst))))

    (define (standard-error-of-the-mean lst)
      (/ (standard-deviation lst)
         (sqrt (length lst))))

    ;; Number of ways to take k things from n without replacement, when order does not matter
    (define (combinations n k)
      (do ((i 0 (+ 1 i))
           (res 1 (/ (* res (- n i))
                     (- k i))))
        ((= i k) res)))

    ;; Number of ways to take k things from n without replacement, when order matters
    (define (permutations n k)
      (do ((i 0 (+ 1 i))
           (res 1 (* res (- n i))))
        ((= i k) res)))

    (define (sign x)
      (cond ((negative? x) -1)
            ((positive? x) 1)
            ((zero? x) 0)
            (else '())))

    ;; perlin-noise:
    ;; Perlin noise provides a 'smoother' variation in noise, useful for natural simulations
    ;; Conversion of the Java implementation at http://mrl.nyu.edu/~perlin/noise/
    (define (perlin-noise x-in y-in z-in) 
      (define p #(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180 151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20 125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83 111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130 116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5 202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28 42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218 246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235 249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78 66 215 61 156 180))
      (define (fade t)
        (* t t t (+ (* t (- (* t 6) 15)) 10)))
      (define (lerp t a b)
        (+ a (* t (- b a))))
      (define (grad hash x y z)
        (let* ((h (bitwise-and hash 15)) ;; convert low 4 bits of hash code
               (u (if (< h 8) x y))
               (v (cond ((< h 4) y)
                        ((or (= 12 h) (= 14 h)) x)
                        (else z)))) ; into 12 gradient directions
          (+ (if (zero? (bitwise-and h 1)) u (- u))
             (if (zero? (bitwise-and h 2)) v (- v)))))
      ;
      (let* ((CX (bitwise-and (exact (floor x-in)) 255)) ; unit cube
             (CY (bitwise-and (exact (floor y-in)) 255))
             (CZ (bitwise-and (exact (floor z-in)) 255))
             (x (- x-in (floor x-in))) ; fractional parts
             (y (- y-in (floor y-in)))
             (z (- z-in (floor z-in)))
             (u (fade x))
             (v (fade y))
             (w (fade z))
             (A (+ (vector-ref p CX) CY))         ; hash coordinates of 
             (AA (+ (vector-ref p A) CZ))         ; the 8 cube corners
             (AB (+ (vector-ref p (+ A 1)) CZ))
             (B (+ (vector-ref p (+ CX 1)) CY))
             (BA (+ (vector-ref p B) CZ))
             (BB (+ (vector-ref p (+ B 1)) CZ)))
        ; finally add blended results from 8 corners of the cube
        (lerp w 
              (lerp v
                    (lerp u
                          (grad (vector-ref p AA) x y z)
                          (grad (vector-ref p BA) (- x 1) y z))
                    (lerp u 
                          (grad (vector-ref p AB) x (- y 1) z)
                          (grad (vector-ref p BB) (- x 1) (- y 1) z)))
              (lerp v 
                    (lerp u
                          (grad (vector-ref p (+ AA 1)) x y (- z 1))
                          (grad (vector-ref p (+ BA 1)) (- x 1) y (- z 1)))
                    (lerp u
                          (grad (vector-ref p (+ AB 1)) x (- y 1) (- z 1))
                          (grad (vector-ref p (+ BB 1)) (- x 1) (- y 1) (- z 1)))))))

    ;; random-normal:
    ;; returns a random number with mean and standard-distribution as specified.
    ;; Uses code from SRFI 27 documentation
    (define (random-source-make-normals s . unit)
      (let ((rand (apply random-source-make-reals s unit))
            (next #f))
        (lambda (mu sigma)
          (if next
            (let ((result next))
              (set! next #f)
              (+ mu (* sigma result)))
            (let loop ()
              (let* ((v1 (- (* 2 (rand)) 1))
                     (v2 (- (* 2 (rand)) 1))
                     (s (+ (* v1 v1) (* v2 v2))))
                (if (>= s 1)
                  (loop)
                  (let ((scale (sqrt (/ (* -2 (log s)) s))))
                    (set! next (* scale v2))
                    (+ mu (* sigma scale v1))))))))))

    (define random-normal
      (random-source-make-normals default-random-source))

    ;; random-pick:
    ;; random selection from list
    (define (random-pick items)
      (if (and (list? items) (not (null? items)))
        (list-ref items (random-integer (length items)))
        #f))

    ;; random-sample:
    ;; Return a random sample of size N from sequence, without replacement.  
    ;; If N is equal to or greater than the length of the sequence, return 
    ;; the entire sequence.
    (define (random-sample n items)
      (cond ((<= n 0) 
             '())
            ((>= n (length items)) 
             items)
            (else
              (let loop ((remaining items)
                         (kept '()))
                (if (= (length kept) n)
                  kept
                  (let ((one (random-pick remaining)))
                    (loop (delete one remaining)
                          (cons one kept))))))))

    ;; random-weighted-sample:
    ;; Return a random sample of size M from sequence of length N, 
    ;; without replacement, where each element has a defined 
    ;; probability of selection (weight) W.  If M is equal to
    ;; or greater to N, return the entire sequence.
    (define (random-weighted-sample m items weights)
      (let ((n (length items)))
        (cond ((<= m 0) '())
              ((>= m n) items)
              (else
                (let* ((keys (map (lambda (w) (expt (random-real) (/ 1 w))) weights))
                       (sorted-items (list-sort (lambda (x y) (> (car x) (car y)))
                                                (zip keys items))))
                  (map cadr (take sorted-items m)))))))

    ;; jaccard-index:
    (define jaccard-index
      (case-lambda 
        ((items-1 items-2)
         (jaccard-index items-1 items-2 equal?))
        ((items-1 items-2 eq-test?)
         (let* ((set-1 (delete-duplicates items-1 eq-test?))
                (set-2 (delete-duplicates items-2 eq-test?))
                (union (lset-union eq-test? set-1 set-2)))
           (if (null? union)
             1
             (/ (length (lset-intersection eq-test? set-1 set-2))
                (length union)))))))

    ;; jaccard-distance:
    (define jaccard-distance
      (case-lambda 
        ((items-1 items-2)
         (jaccard-distance items-1 items-2 equal?))
        ((items-1 items-2 eq-test?)
         (- 1 (jaccard-index items-1 items-2 eq-test?)))))

    ;; sorenson-dice-index:
    (define sorenson-dice-index
      (case-lambda
        ((items-1 items-2)
         (sorenson-dice-index items-1 items-2 equal?))
        ((items-1 items-2 eq-test?)
         (let ((set-1 (delete-duplicates items-1 eq-test?))
               (set-2 (delete-duplicates items-2 eq-test?)))
           (if (and (null? set-1) (null? set-2))
             1
             (/ (* (length (lset-intersection eq-test? set-1 set-2)) 2)
                (+ (length set-1) (length set-2))))))))

    ))

