;;; "strsrch.scm" Search for string from port.
; Written 1995, 1996 by Oleg Kiselyov (oleg@acm.org)
; Modified 1996, 1997, 1998, 2001 by A. Jaffer (agj@alum.mit.edu)
; Modified 2003 by Steve VanDevender (stevev@hexadecimal.uoregon.edu)
; 2013-01 A. Jaffer replaced the skip-vector with an alist

; This code is in the public domain.

;; Packaged for R7RS Scheme by Peter Lane, 2017
;;
;; Implementation relies on SRFI-13 where possible

(define-library
  (slib string-search)
  (export string-index      ; from SRFI 13
          string-index-ci
          string-reverse-index
          string-reverse-index-ci
          substring?
          substring-ci?
          find-string-from-port?
          string-subst
          count-newlines)
  (import (scheme base)
          (scheme char)
          (slib alist))

  (cond-expand
    ((library (srfi 13))
     (import (only (srfi 13) string-index string-index-right string-contains
                   string-contains-ci string-count)))
    (else
      (error "No support for (srfi 13)")))

  (begin

    ;@
    (define (string-index-ci str chr)
      (string-index str (lambda (c) (char-ci=? chr c))))
    
    ;@
    (define (string-reverse-index str chr)
      (string-index-right str chr))
    
    ;@
    (define (string-reverse-index-ci str chr)
      (string-index-right str (lambda (c) (char-ci=? chr c))))

    ;@
    (define (substring? pat str)
      (string-contains str pat))

    ;@
    (define (substring-ci? pat str)
      (string-contains-ci str pat))

    ;@
    (define (find-string-from-port? str <input-port> . max-no-char-in)
      (let ((max-no-char (if (null? max-no-char-in) #f (car max-no-char-in))))
        (letrec
          ((no-chars-read 0)
           (peeked? #f)
           (my-peek-char			; Return a peeked char or #f
             (lambda () (and (or (not (number? max-no-char))
                                 (< no-chars-read max-no-char))
                             (let ((c (peek-char <input-port>)))
                               (cond (peeked? c)
                                     ((eof-object? c) #f)
                                     ((procedure? max-no-char)
                                      (set! peeked? #t)
                                      (if (max-no-char c) #f c))
                                     ((eqv? max-no-char c) #f)
                                     (else c))))))
           (next-char (lambda () (set! peeked? #f) (read-char <input-port>)
                        (set! no-chars-read  (+ 1 no-chars-read))))
           (match-1st-char			; of the string str
             (lambda ()
               (let ((c (my-peek-char)))
                 (and c
                      (begin (next-char)
                             (if (char=? c (string-ref str 0))
                               (match-other-chars 1)
                               (match-1st-char)))))))
           ;; There has been a partial match, up to the point pos-to-match
           ;; (for example, str[0] has been found in the stream)
           ;; Now look to see if str[pos-to-match] for would be found, too
           (match-other-chars
             (lambda (pos-to-match)
               (if (>= pos-to-match (string-length str))
                 no-chars-read	       ; the entire string has matched
                 (let ((c (my-peek-char)))
                   (and c
                        (if (not (char=? c (string-ref str pos-to-match)))
                          (backtrack 1 pos-to-match)
                          (begin (next-char)
                                 (match-other-chars (+ 1 pos-to-match)))))))))

           ;; There had been a partial match, but then a wrong char showed up.
           ;; Before discarding previously read (and matched) characters, we check
           ;; to see if there was some smaller partial match. Note, characters read
           ;; so far (which matter) are those of str[0..matched-substr-len - 1]
           ;; In other words, we will check to see if there is such i>0 that
           ;; substr(str,0,j) = substr(str,i,matched-substr-len)
           ;; where j=matched-substr-len - i
           (backtrack
             (lambda (i matched-substr-len)
               (let ((j (- matched-substr-len i)))
                 (if (<= j 0)
                   ;; backed off completely to the begining of str
                   (match-1st-char)
                   (let loop ((k 0))
                     (if (>= k j)
                       (match-other-chars j) ; there was indeed a shorter match
                       (if (char=? (string-ref str k)
                                   (string-ref str (+ i k)))
                         (loop (+ 1 k))
                         (backtrack (+ 1 i) matched-substr-len))))))))
           )
          (match-1st-char))))

    ;@
    (define (string-subst text old new . rest)
      (define sub
        (lambda (text)
          (set! text
            (cond ((equal? "" text) text)
                  ((substring? old text)
                   => (lambda (idx)
                        (string-append
                          (substring text 0 idx)
                          new
                          (sub (substring
                                 text (+ idx (string-length old))
                                 (string-length text))))))
                  (else text)))
          (if (null? rest)
            text
            (apply string-subst text rest))))
      (sub text))

    ;@
    (define (count-newlines str)
      (string-count str #\newline))

    ))

