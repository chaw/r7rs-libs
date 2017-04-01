;; Text library for R7RS Scheme

;; Written by Peter Lane, 2017

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
  (robin text)
  (export word-wrap
          words->with-commas
          string->n-grams
          ; metrics and similarity measures
          soundex ; straight from (slib soundex)
          daitch-mokotoff-soundex
          russell-soundex
          metaphone
          hamming-distance
          ;
          sorenson-dice-similarity
          porter-stem
          )
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (rebottled pregexp)
          (only (robin statistics) sorenson-dice-index)
          (slib soundex)                                      (slib format)
          (srfi 1)
          (srfi 69)
          (srfi 95))

  (cond-expand
    ((library (srfi 13))
     (import (only (srfi 13) string-join string-tokenize)))
    ((library (chibi string))
     (import (only (chibi string) string-join string-split))
     (begin
       (define string-tokenize string-split)))
    (else
      (error "Scheme implementation does not support a suitable string library")))

  (begin

    ;; word wrap, using greedy algorithm with minimum lines
    ;; returns a list of lines
    (define (word-wrap str width)
      (let loop ((words (string-tokenize str))
                 (line-length 0)
                 (line "")
                 (lines '()))
        (cond ((null? words)
               (reverse (cons line lines)))
              ((> (+ line-length (string-length (car words)))
                  width)
               (if (zero? (string-length line))
                 (loop (cdr words) ; case where word exceeds line length
                       0
                       "" 
                       (cons (car words) lines))
                 (loop words ; word must go to next line, so finish current line
                       0
                       ""
                       (cons line lines))))
              (else
                (loop (cdr words) ; else, add word to current line
                      (+ 1 line-length (string-length (car words)))
                      (if (zero? (string-length line))
                        (car words)
                        (string-join (list line (car words)) " "))
                      lines)))))

    ;; given a list of strings, each string representing a word, 
    ;; return a string with each word separated by commas, 
    ;; but last separated by 'and'.
    ;; Optional flag is used to include the comma before 'and', if required.
    (define words->with-commas 
      (case-lambda
        ((words) ; default to no optional comma
         (words->with-commas words #f))
        ((words add-comma?)
         (parameterize ((current-output-port (open-output-string)))
                       (do ((rem words (cdr rem)))
                         ((null? rem) )
                         (display (car rem))
                         (cond ((= 1 (length rem)) )
                               ((= 2 (length rem))
                                (when (and add-comma? 
                                           (> (length words) 2))
                                  (display ","))
                                (display " and "))
                               (else
                                 (display ", "))))
                       (get-output-string (current-output-port))))))

    ;; Convert a string to a list of letter n-grams
    (define (string->n-grams str n)
      (let ((len (string-length str)))
        (cond ((<= n 0)
               (error "string->n-grams needs n >= 1"))
              ((> n len)
               (list str))
              (else
                (do ((i 0 (+ 1 i))
                     (res '() (cons (string-copy str i (+ i n)) res)))
                  ((> (+ i n) len) (reverse res)))))))

    ;; Phonetic-based metrics

    ;; Convert given word into a coded string of length 6
    ;; Table from: http://www.jewishgen.org/InfoFiles/soundex.html
    (define daitch-mokotoff-soundex
      (let ((table 
              (sort
                '(("AI" "0" "1" "-") ; (letter(s) -> at-start  before-vowel  elsewhere)
                  ("AJ" "0" "1" "-")
                  ("AY" "0" "1" "-")
                  ("AU" "0" "7" "-")
                  ("A" "0" "-" "-")
                  ("Ą" "-" "-" ("6" "-")) ; Polish a-ogonek
                  ("B" "7" "7" "7")
                  ("C" ("4" "5") ("4" "5") ("4" "5")) 
                  ("CH" ("5" "4") ("5" "4") ("5" "4")) 
                  ("CK" ("5" "45") ("5" "45") ("5" "45"))
                  ("CHS" "5" "54" "54")
                  ("CK" "4" "4" "4")
                  ("CS" "4" "4" "4")
                  ("CSZ" "4" "4" "4")
                  ("CZS" "4" "4" "4")
                  ("CZ" "4" "4" "4")
                  ("DRZ" "4" "4" "4")
                  ("DRS" "4" "4" "4")
                  ("DS" "4" "4" "4")
                  ("DSH" "4" "4" "4")
                  ("DSZ" "4" "4" "4")
                  ("DZ" "4" "4" "4")
                  ("DZH" "4" "4" "4")
                  ("DZS" "4" "4" "4")
                  ("D" "3" "3" "3")
                  ("DT" "3" "3" "3")
                  ("Ę", "-" "-" ("6" "-")) ; Polish e-ogonek
                  ("EI" "0" "1" "-") 
                  ("EJ" "0" "1" "-")
                  ("EY" "0" "1" "-")
                  ("EU" "1" "1" "-")
                  ("E" "0" "-" "-")
                  ("FB" "7" "7" "7")
                  ("F" "7" "7" "7")
                  ("G" "5" "5" "5")
                  ("H" "5" "5" "-")
                  ("IA" "1" "-" "-")
                  ("IE" "1" "-" "-")
                  ("IO" "1" "-" "-")
                  ("IU" "1" "-" "-")
                  ("I" "0" "-" "-")
                  ("J" ("1" "4") ("-" "4") ("-" "4"))
                  ("KS" "5" "54" "54")
                  ("KH" "5" "5" "5")
                  ("K" "5" "5" "5")
                  ("L" "8" "8" "8")
                  ("MN" "-" "66" "66")
                  ("M" "6" "6" "6")
                  ("NM" "-" "66" "66")
                  ("N" "6" "6" "6")
                  ("OI" "0" "1" "-")
                  ("OJ" "0" "1" "-")
                  ("OY" "0" "1" "-")
                  ("O" "0" "-" "-")
                  ("P" "7" "7" "7")
                  ("PF" "7" "7" "7")
                  ("PH" "7" "7" "7")
                  ("Q" "5" "5" "5")
                  ("R" "9" "9" "9")
                  ("RS" ("4" "94") ("4" "94") ("4" "94"))
                  ("RZ" ("4" "94") ("4" "94") ("4" "94")) 
                  ("SCHTSCH" "2" "4" "4")
                  ("SCHTSH" "2" "4" "4")
                  ("SCHTCH" "2" "4" "4")
                  ("SCH" "4" "4" "4")
                  ("SHTCH" "2" "4" "4")
                  ("SHCH" "2" "4" "4")
                  ("SHTSH" "2" "4" "4")
                  ("SHT" "2" "43" "43")
                  ("SH" "4" "4" "4")
                  ("STCH" "2" "4" "4")
                  ("STSCH" "2" "4" "4")
                  ("SC" "2" "4" "4")
                  ("STRZ" "2" "4" "4")
                  ("STRS" "2" "4" "4")
                  ("STSH" "2" "4" "4")
                  ("ST" "2" "43" "43")
                  ("SZCZ" "2" "4" "4")
                  ("SZCS" "2" "4" "4")
                  ("SZT" "2" "43" "43")
                  ("SHD" "2" "43" "43")
                  ("SZD" "2" "43" "43")
                  ("SD" "2" "43" "43")
                  ("SZ" "4" "4" "4")
                  ("S" "4" "4" "4")
                  ("Ţ" ("3" "4") ("3" "4") ("3" "4")) ; Romanian t-cedilla 
                  ("TCH" "4" "4" "4")
                  ("TTCH" "4" "4" "4")
                  ("TTSCH" "4" "4" "4")
                  ("TH" "3" "3" "3")
                  ("TRZ" "4" "4" "4")
                  ("TRS" "4" "4" "4")
                  ("TSCH" "4" "4" "4")
                  ("TSH" "4" "4" "4")
                  ("TS" "4" "4" "4")
                  ("TTS" "4" "4" "4")
                  ("TTSZ" "4" "4" "4")
                  ("TC" "4" "4" "4")
                  ("TZ" "4" "4" "4")
                  ("TTZ" "4" "4" "4")
                  ("TZS" "4" "4" "4")
                  ("TSZ" "4" "4" "4")
                  ("T" "3" "3" "3")
                  ("UI" "0" "1" "-")
                  ("UJ" "0" "1" "-")
                  ("UY" "0" "1" "-")
                  ("U" "0" "-" "-")
                  ("UE" "0" "-" "-")
                  ("V" "7" "7" "7")
                  ("W" "7" "7" "7")
                  ("X" "5" "54" "54")
                  ("Y" "1" "-" "-")
                  ("ZDZ" "2" "4" "4")
                  ("ZDZH" "2" "4" "4")
                  ("ZHDZH" "2" "4" "4")
                  ("ZD" "2" "43" "43")
                  ("ZHD" "2" "43" "43")
                  ("ZH" "4" "4" "4")
                  ("ZS" "4" "4" "4")
                  ("ZSCH" "4" "4" "4")
                  ("ZSH" "4" "4" "4")
                  ("Z" "4" "4" "4"))
                (lambda (a b) (> (string-length (car a)) (string-length (car b)))))))
        (define (longest-match str) 
          (let loop ((words table))
            (cond ((null? words)
                   #f)
                  ((pregexp-match (string-append "^" (caar words)) str)
                   (caar words))
                  (else
                    (loop (cdr words))))))
        (define (match-result codes word posn next-posn)
          (cond ((zero? posn)
                 (list-ref codes 0))
                ((and (< (+ 1 next-posn) (string-length word))
                      (member (string-ref word (+ 1 next-posn))
                              (string->list "AEIOU")
                              char=?))
                 (list-ref codes 1))
                (else
                  (list-ref codes 2))))
        (define (valid-match? codes word posn next-posn)
          (let ((m (match-result codes word posn next-posn)))
            (or (list? m)
                 (not (string=? "-" m)))))
        (define (generate-results result) ; some of the terms may be lists, so make multiple terms for these
          (let ((first-list (list-index list? result)))
            (if (number? first-list)
              (let ((list-1 (list-copy result))
                    (list-2 (list-copy result))
                    (mix (list-ref result first-list)))
                (append
                  (generate-results (append (take list-1 first-list)
                                            (list (car mix))
                                            (drop list-1 (+ 1 first-list))))
                  (generate-results (append (take list-2 first-list)
                                            (list (cadr mix))
                                            (drop list-2 (+ 1 first-list))))))              
              (if (< (length result) 6)
                (list (apply string-append (append result (make-list (- 6 (length result)) "0"))))
                (list (apply string-append (take result 6)))))))
        ;
        (case-lambda
          ((input-word)
           (daitch-mokotoff-soundex input-word 'single))
          ((input-word option)
           (let ((word (string-upcase input-word)))
             (let loop ((posn 0)
                        (match (longest-match word))
                        (result '()))
               (cond ((>= posn (string-length word))
                      (let ((codes (generate-results (reverse result))))
                        (if (eq? option 'single)
                          (car codes)
                          codes)))
                     ((string? match)
                      (let ((codes (cdr (assoc match table string=?))))
                        (if (valid-match? codes word posn (+ posn (string-length match)))
                          (loop (+ posn (string-length match))
                                (longest-match (string-copy word (+ posn (string-length match))))
                                (cons (match-result codes word posn (+ posn (string-length match))) result))
                          (loop (+ posn (string-length match))
                                (longest-match (string-copy word (+ posn (string-length match))))
                                result))))
                     (else (loop (+ 1 posn)
                                 (longest-match (string-copy word (+ 1 posn)))
                                 result)))))))))

    (define russell-soundex soundex) ; from (slib soundex)
 
    ; metaphone: there is no consistent description of this algorithm
    ; these rules are applied from the description at http://aspell.net/metaphone/metaphone-kuhn.txt
    (define (metaphone word)
      ; remove duplicate letters in target alphabet (but leave 'c' alone)
      (define (remove-duplicates-but-c str) 
        (define target-group '(#\b #\f #\h #\j #\k #\l #\m #\n #\p #\r #\s #\t #\w #\x #\y))
        (define (update-res i res)
          (if (and (memv (string-ref str i) target-group)
                   (< i (- (string-length str) 1))
                   (char=? (string-ref str i) (string-ref str (+ i 1))))
            res
            (cons (string-ref str i) res)))
        (do ((i 0 (+ i 1))
             (res '() (update-res i res)))
          ((= i (string-length str)) (list->string (reverse res)))))
      ; adapt initial letters:
      ;   "ae-", "gn", "kn-", "pn-", "wr-" drop first letter
      ;   "x" -> "s"
      ;   "wh" -> "w"
      (define (adapt-initial-letters str)
        (cond ((member (substring str 0 2) '("ae" "gn" "kn" "pn" "wr") string=?)
               (string-copy str 1))
              ((char=? #\x (string-ref str 0))
               (string-append "s" (string-copy str 1)))
              ((string=? "wh" (substring str 0 2))
               (string-append "w" (string-copy str 2)))
              (else
                str)))
      ; remove b in mb$
      (define (remove-b str)
        (if (pregexp-match "mb$" str)
          (pregexp-replace "mb$" str "m")
          str))
      ; helper functions
      (define (vowel? c) (memq c '(#\a #\e #\i #\o #\u)))
      (define (one-of? grp) (lambda (c) (memq c grp)))
      (define (peek? rem c)
        (and (not (null? rem))
             (not (null? (cdr rem)))
             (if (char? c) ; if not a char, assume it's a proc
               (char=? c (cadr rem))
               (c (cadr rem)))))
      (define (peek-io/a? rem) ; is top char followed by io or ia
        (and (peek? rem #\i)
             (or (peek? (cdr rem) #\o)
                 (peek? (cdr rem) #\a))))
      ;
      (let* ((lword (pregexp-replace* "[^[:alpha:]]" (string-downcase word) ""))
             (dedupped (remove-duplicates-but-c lword))
             (initials (adapt-initial-letters dedupped))
             (no-mb (remove-b initials)))
        (let loop ((rem (string->list no-mb))
                   (last-done #f)             ; last actual character processed
                   (res '()))
          (if (null? rem)
            (string-upcase (list->string (reverse res)))
            (case (car rem)
              ((#\a #\e #\i #\o #\u) ; keep vowel only at front
               (if (null? res) 
                 (loop (cdr rem) (car rem) (cons (car rem) res))
                 (loop (cdr rem) last-done res)))
              ((#\b #\f #\j #\l #\m #\n #\r) ; retained
               (loop (cdr rem) (car rem) (cons (car rem) res)))
              ((#\c)
               (cond ((peek? rem #\h)
                      (loop (cdr rem)
                            (car rem)
                            (cons (if (eqv? last-done #\s) #\k #\x)
                                  res)))
                     ((and (peek? rem #\i)
                           (peek? (cdr rem) #\a))
                      (loop (cdr rem) (car rem) (cons #\x res)))
                     ((peek? rem (one-of? '(#\e #\i #\y)))
                      (loop (cdr rem)
                            (car rem)
                            (if (eqv? last-done #\s)
                              res ; silent if -sci- -sce- -scy-
                              (cons #\s res))))
                     (else
                       (loop (cdr rem) (car rem) (cons #\k res)))))
              ((#\d)
               (loop (cdr rem)
                     (car rem)
                     (cons 
                       (if (and (peek? rem #\g)
                                (peek? (cdr rem) (one-of? '(#\e #\i #\y))))
                         #\j
                         #\t)
                       res)))
              ((#\g)
               (cond ((peek? rem #\g) ; duplicate g
                      (loop (cdr rem) (car rem) res))
                     ((or (and (peek? rem #\h)
                               (> (length rem) 1) ; not at end
                               (not (peek? (cdr rem) vowel?))) ; not before a vowel
                          (peek? rem #\n)
                          (and (eqv? last-done #\d)
                               (peek? rem (one-of? '(#\e #\i #\y)))))
                      (loop (cdr rem) (car rem) res))
                     ((and (not (eqv? last-done #\g))
                           (or (peek? rem #\i)
                               (peek? rem #\e)
                               (peek? rem #\y)))
                      (loop (cdr rem) (car rem) (cons #\j res)))
                     (else
                       (loop (cdr rem) (car rem) (cons #\k res)))))
              ((#\h)
               (if (or (and (vowel? last-done) (not (peek? rem vowel?)))
                       (memv last-done '(#\c #\s #\p #\t #\g)))
                 (loop (cdr rem) (car rem) res)
                 (loop (cdr rem) (car rem) (cons #\h res))))
              ((#\k)
               (loop (cdr rem)
                     (car rem)
                     (if (eqv? last-done #\c)
                       res ; silent after c
                       (cons #\k res))))
              ((#\p)
               (loop (cdr rem)
                     (car rem)
                     (cons (if (peek? rem #\h) #\f #\p)
                           res)))
              ((#\q)
               (loop (cdr rem) (car rem) (cons #\k res)))
              ((#\s)
               (loop (cdr rem)
                     (car rem)
                     (cons (if (or (peek? rem #\h)
                                   (peek-io/a? rem))
                             #\x
                             #\s)
                           res)))
              ((#\t)
               (cond ((and (peek? rem #\c)        ; drop t if followed by ch
                           (peek? (cdr rem) #\h))
                      (loop (cdr rem) (car rem) res))
                     ((peek? rem #\h)             ; replace th with 0
                      (loop (cddr rem)
                            (car rem)
                            (cons #\0 res)))
                     ((peek-io/a? rem)            ; replace with x if followed by io or ia
                      (loop (cdr rem) (car rem) (cons #\x res)))
                     (else 
                       (loop (cdr rem) (car rem) (cons #\t res)))))
              ((#\v)
               (loop (cdr rem) (car rem) (cons #\f res)))
              ((#\w #\y)
               (loop (cdr rem)
                     (car rem)
                     (if (peek? rem vowel?)
                       (cons (car rem) res)
                       res)))
              ((#\x) ; TODO: x at start?
               (loop (cdr rem) (car rem) (cons #\s (cons #\k res))))
              ((#\z) 
               (loop (cdr rem) (car rem) (cons #\s res))))))))

    ; double-metaphone

    ;; Edit distance metrics
    ; levenshtein         -- insertion/deletion/substitution
    ; damerau-levenshtein -- insertion/deletion/substitution/transposition
    ; jaro-distance       -- transposition
    ; longest-common-subsequence  -- insertion/deletion

    ; hamming-distance    -- substitution
    ; -- adapted to work on different sequence types
    (define hamming-distance 
      (case-lambda
        ((item-1 item-2)
         (hamming-distance item-1 item-2 (cond ((string? item-1) char=?)
                                               ((bytevector? item-1) =)
                                               (else equal?))))
        ((item-1 item-2 equal-test?)
         ;
         (define (hamming-seq-distance seq-1 seq-2 seq-length seq-for-each)
           (if (= (seq-length seq-1) (seq-length seq-2))
             (let ((count 0))
               (seq-for-each (lambda (c1 c2)
                                  (unless (equal-test? c1 c2) (set! count (+ 1 count))))
                                seq-1
                                seq-2)
               count)
             (error "Hamming: Sequences are of different sizes")))
         (define (hamming-byte-distance vec-1 vec-2)
           (if (= (bytevector-length vec-1) (bytevector-length vec-2))
             (let loop ((count 0)
                        (i (bytevector-length vec-1)))
               (cond ((zero? i)
                      count)
                     ((= (bytevector-u8-ref vec-1 (- i 1))
                         (bytevector-u8-ref vec-2 (- i 1)))
                      (loop count
                            (- i 1)))
                     (else
                       (loop (+ 1 count)
                             (- i 1)))))
             (error "Hamming: Sequences are of different sizes")))
         ;
         (cond ((and (string? item-1) (string? item-2))
                (hamming-seq-distance item-1 item-2 string-length string-for-each))
               ((and (list? item-1) (list? item-2))
                (hamming-seq-distance item-1 item-2 length for-each))
               ((and (vector? item-1) (vector? item-2))
                (hamming-seq-distance item-1 item-2 vector-length vector-for-each))
               ((and (bytevector? item-1) (bytevector? item-2))
                (hamming-byte-distance item-1 item-2))
               (else
                 (error "Hamming: Unknown or mismatched types"))))))

    ;; Character group measure
    (define (sorenson-dice-similarity string-1 string-2)
      (sorenson-dice-index (string->n-grams string-1 2) 
                           (string->n-grams string-2 2) 
                           string-ci=?))

    ;; Porter stemming algorithm
    ;; https://tartarus.org/martin/PorterStemmer/
    (define (porter-stem initial-word)
      ; regexps - after Ruby version https://github.com/threedaymonk/text
      (define MGR0 "^([^aeiou][^aeiouy]*)?([aeiouy][aeiou]*)([^aeiou][^aeiouy]*)")
      (define MEQ1 "^([^aeiou][^aeiouy]*)?([aeiouy][aeiou]*)([^aeiou][^aeiouy]*)([aeiouy][aeiou]*)?")
      (define MGR1 "^([^aeiou][^aeiouy]*)?([aeiouy][aeiou]*)([^aeiou][^aeiouy]*)([aeiouy][aeiou]*)([^aeiou][^aeiouy]*)")
      (define VOWEL-STEM "^([^aeiou][^aeiouy]*)?[aeiouy]")
      ;
      (define (step-1a word) ; removes plurals 
        (cond ((pregexp-match "(ss|i)es$" word) ; kill the es
               (pregexp-replace "es$" word ""))
              ((pregexp-match "[^s]s$" word) ; kill the s
               (pregexp-replace "s$" word ""))
              (else
                word)))
      (define (step-1b word) ; remove -ed -ing
        (cond ((pregexp-match "eed$" word)
               (if (pregexp-match MGR0 (pregexp-replace "eed$" word ""))
                 (pregexp-replace "eed$" word "ee")
                 word))
              ((pregexp-match "(ed|ing)$" word)
               (let ((stem (pregexp-replace "(ed|ing)$" word "")))
                 (if (pregexp-match VOWEL-STEM stem)
                   (cond ((pregexp-match "(at|bl|iz)$" stem)
                          (string-append stem "e"))
                         ((pregexp-match "([^aeiouylsz])\\1$" stem)
                          (string-copy stem 0 (- (string-length stem) 1)))
                         ((pregexp-match "^([^aeiou][^aeiouy]*)[aeiouy][^aeiouwxy]$" stem)
                          (string-append stem "e"))
                         (else 
                           stem))
                   word)))
              (else
                word)))
      (define (step-1c word) ; turns terminal y to i when another vowel in stem
        (if (and (pregexp-match "y$" word)
                 (pregexp-match VOWEL-STEM (string-copy word 0 (- (string-length word) 1))))
          (pregexp-replace "y$" word "i")
          word))
      ;
      (define (remove-pairs word pairs)
        (if (null? pairs)
          word
          (let ((posn (pregexp-match-positions (caar pairs) word)))
            (if posn ; note, all matches will be at the end of the word
              (let ((stem (string-copy word 0 (caar posn))))
                (if (pregexp-match MGR0 stem)
                  (string-append stem (cdar pairs))
                  word))
              (remove-pairs word (cdr pairs))))))
      (define (remove-pairs-1 word words)
        (if (null? words)
          word
          (let ((posn (pregexp-match-positions (car words) word)))
            (if posn ; note, all matches will be at the end of the word
              (let ((stem (string-copy word 0 (caar posn))))
                (if (pregexp-match MGR1 stem)
                  stem
                  word))
              (remove-pairs-1 word (cdr words))))))

      (define (step-2 word) ; removes double suffices to single ones
        (remove-pairs word '(("ational$" . "ate")
                             ("tional$" . "tion")
                             ("enci$" . "ence")
                             ("anci$" . "ance")
                             ("izer$" . "ize")
                             ("bli$"   . "ble")
                             ("alli$"     . "al")
                             ("entli$"    . "ent")
                             ("eli$"      . "e")
                             ("ousli$"    . "ous")
                             ("ization$"  . "ize")
                             ("ation$"    . "ate")
                             ("ator$"     . "ate")
                             ("alism$"    . "al")
                             ("iveness$"  . "ive")
                             ("fulness$"  . "ful")
                             ("ousness$"  . "ous")
                             ("aliti$"    . "al")
                             ("iviti$"    . "ive")
                             ("biliti$"   . "ble")
                             ("logi$"     . "log"))))
      (define (step-3 word) ; removes -ic- -full -ness
        (remove-pairs word '(("icate$" . "ic")
                             ("ative$" . "")
                             ("alize$" . "al")
                             ("iciti$" . "ic")
                             ("ical$" . "ic")
                             ("ful$" . "")
                             ("ness$" . ""))))
      (define (step-4 word)
        (let ((rev-word (remove-pairs-1 word
                                        '("al$" "ance$" "ence$" "er$" "ic$" "able$" "ible$"
                                          "ant$" "ement$" "ment$" "ent$" "ou$" "ism$"
                                          "ate$" "iti$" "ous$" "ive$" "ize$"))))
          (if (string=? rev-word word)
            (if (pregexp-match "(s|t)ion$" word)
              (let ((stem (pregexp-replace "ion$" word "")))
                (if (pregexp-match MGR1 stem)
                  stem
                  word))
              word)
            rev-word)))
      ;
      (define (step-5 word)
        (if (pregexp-match "e$" word)
          (let ((stem (pregexp-replace "e$" word "")))
            (if (or (pregexp-match MGR1 stem)
                    (and (pregexp-match MEQ1 stem)
                         (not (pregexp-match "^([^aeiou][^aeiouy]*)[aeiouy][^aeiouwxy]$" stem))))
              stem
              word))
          word))
      (define (step-6 word) ; replace ll
        (if (and (pregexp-match "ll$" word)
                 (pregexp-match MGR1 word))
          (pregexp-replace "ll$" word "l")
          word))
      ;
      (if (< (string-length initial-word) 3)
        initial-word
        (string-downcase
          (step-6
            (step-5
              (step-4
                (step-3
                  (step-2
                    (step-1c
                      (step-1b 
                        (step-1a 
                          (let ((word (string-downcase initial-word)))
                            (if (char=? (string-ref word 0) #\y)
                              (string-append "Y" (string-copy word 1))
                              word)))))))))))))


    ))

