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
          ; metrics and similarity measures
          sorenson-dice-similarity
          soundex ; straight from (slib soundex)
          porter-stem
          ;
          string->n-grams
          )
  (import (scheme base)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (rebottled pregexp)
          (only (robin statistics) sorenson-dice-index)
          (slib soundex)                                      (slib format)
          (srfi 1))

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

    ;; Some metrics for text comparisons
    ;; -- borrowed list from https://github.com/threedaymonk/text

    ; russell-soundex (like slib soundex)
    ; daitch-mokotoff-soundex
    ; metaphone
    ; double-metaphone
    ; levenshtein

    (define (sorenson-dice-similarity string-1 string-2)
      (sorenson-dice-index 
        (string->n-grams string-1 2) 
        (string->n-grams string-2 2) 
        string-ci=?))

    ;; Porter stemming algorithm
    ;; https://tartarus.org/martin/PorterStemmer/
    (define (porter-stem initial-word)
      ; regexps - after Ruby version
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
        (cond ((null? pairs)
               word)
              ((pregexp-match (caar pairs) word)
               (let ((stem (pregexp-replace (caar pairs) word "")))
                 (if (pregexp-match MGR0 stem)
                   (string-append stem (cdar pairs))
                   word)))
              (else
                (remove-pairs word (cdr pairs)))))
      (define (remove-pairs-1 word words)
        (cond ((null? words)
               word)
              ((pregexp-match (car words) word)
               (let ((stem (pregexp-replace (car words) word "")))
                 (if (pregexp-match MGR1 stem)
                   stem
                   word)))
              (else
                (remove-pairs-1 word (cdr words)))))

      (define (step-2 word) ; removes double suffices to single ones
        (remove-pairs word '(("ational$" . "ate")
                             ("tional$" . "tion")
                             ("enci$" . "ence")
                             ("anci$" . "ance")
                             ("izer$" . "ize")
                             ("iser$" . "ise")
                             ("bli$"   . "ble")
                             ("alli$"     . "al")
                             ("entli$"    . "ent")
                             ("eli$"      . "e")
                             ("ousli$"    . "ous")
                             ("ization$"  . "ize")
                             ("isation$"  . "ise")
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
                             ("alise$" . "al")
                             ("iciti$" . "ic")
                             ("ical$" . "ic")
                             ("ful$" . "")
                             ("ness$" . ""))))
      (define (step-4 word)
        (let ((rev-word (remove-pairs-1 word
                                        '("al$" "ance$" "ence$" "er$" "ic$" "able$" "ible$"
                                          "ant$" "ement$" "ment$" "ent$" "ou$" "ism$"
                                          "ate$" "iti$" "ous$" "ive$" "ize$" "ise$"))))
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
        (step-6
          (step-5
            (step-4
              (step-3
                (step-2
                  (step-1c
                    (step-1b 
                      (step-1a 
                        initial-word))))))))))

    ;; reduce a string to a list of letter n-grams
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

    ))

