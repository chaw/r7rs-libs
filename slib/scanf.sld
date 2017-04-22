;;;;"scanf.scm" implementation of formatted input
;Copyright (C) 1996, 1997, 2003, 2010 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Originally jjb@isye.gatech.edu (John Bartholdi) wrote some public
;;; domain code for a subset of scanf, but it was too difficult to
;;; extend to POSIX pattern compliance.  Jan 96, I rewrote the scanf
;;; functions starting from the POSIX man pages.

;; Packaged for R7Rs Scheme by Peter Lane, 2017
;; (Note: broke apart stdio:scan-and-set to avoid Kawa 2.3 problem; problem fixed in later version.)

(define-library
  (slib scanf)
  (export scanf-read-list
          scanf-read-values)
  (import (except (scheme base) read-string)
          (scheme case-lambda)
          (scheme char)
          (scheme cxr)
          (slib common)
          (slib string-port))

  (begin

    (define (char-non-numeric? c) (not (char-numeric? c)))

    (define (flush-whitespace port)
      (do ((c (peek-char port) (peek-char port))
           (i 0 (+ 1 i)))
        ((or (eof-object? c) (not (char-whitespace? c))) i)
        (read-char port)))

    (define (read-word width separator? input-port read-input-char)
      (let ((l (read-string width separator? input-port read-input-char)))
        (if (zero? (string-length l)) #f l)))

    (define (read-string width separator? input-port read-input-char)
      (cond (width
              (let ((str (make-string width)))
                (do ((i 0 (+ 1 i)))
                  ((>= i width)
                   str)
                  (let ((c (peek-char input-port)))
                    (cond ((eof-object? c)
                           (set! str (string-copy str 0 i))
                           (set! i width))
                          ((separator? c)
                           (set! str (if (zero? i) "" (string-copy str 0 i)))
                           (set! i width))
                          (else
                            (string-set! str i (read-input-char))))))))
            (else
              (do ((c (peek-char input-port) (peek-char input-port))
                   (l '() (cons c l)))
                ((or (eof-object? c) (separator? c))
                 (list->string (reverse l)))
                (read-input-char)))))

    (define (read-signed proc input-port read-input-char get-width width--)
      (case (peek-char input-port)
        ((#\-) (read-input-char) (width--)
               (let ((ret (proc input-port read-input-char get-width width--))) (and ret (- ret))))
        ((#\+) (read-input-char) (width--) (proc input-port read-input-char get-width width--))
        (else (proc input-port read-input-char get-width width--))))

    (define (read-radixed-unsigned input-port read-input-char get-width width--)
      (let ((c (peek-char input-port)))
        (case c
          ((#\0) (read-input-char) (width--)
                 (set! c (peek-char input-port))
                 (case c
                   ((#\x #\X) (read-input-char) (width--) (read-x input-port read-input-char get-width width--))
                   (else (read-o input-port read-input-char get-width width--))))
          (else (read-u input-port read-input-char get-width width--)))))

    (define (read-u input-port read-input-char get-width width--)
      (string->number (read-string (get-width) char-non-numeric? input-port read-input-char)))

    (define (read-o input-port read-input-char get-width width--)
      (string->number
        (read-string
          (get-width)
          (lambda (c)
            (not (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))))
          input-port
          read-input-char)
        8))

    (define (read-x input-port read-input-char get-width width--)
      (string->number
        (read-string
          (get-width)
          (lambda (c) (not (memv (char-downcase c)
                                 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
                                   #\9 #\a #\b #\c #\d #\e #\f))))
          input-port
          read-input-char)
        16))

    (define (read-ui input-port read-input-char get-width width--)
      (let* ((dot? #f)
             (mantissa
               (read-word
                 (get-width)
                 (lambda (c)
                   (not (or (char-numeric? c)
                            (cond (dot? #f)
                                  ((eqv? #\. c) (set! dot? #t) #t)
                                  (else #f)))))
                 input-port
                 read-input-char))
             (exponent
               (cond
                 ((not mantissa) #f)
                 ((and (or (not (get-width)) (> (get-width) 1))
                       (memv (peek-char input-port) '(#\E #\e)))
                  (read-input-char)
                  (width--)
                  (let* ((expsign
                           (case (peek-char input-port)
                             ((#\-) (read-input-char)
                                    (width--) "-")
                             ((#\+) (read-input-char)
                                    (width--) "+")
                             (else "")))
                         (expint
                           (and (or (not (get-width)) (positive? (get-width)))
                                (read-word (get-width) char-non-numeric? input-port read-input-char))))
                    (and expint (string-append "e" expsign expint))))
                 (else #f))))
        (and mantissa
             (string->number
               (string-append
                 "#i" (or mantissa "") (or exponent ""))))))


    (define (stdio:scan-and-set format-string input-port . args)
      (define setters (if (equal? '(#f) args) #f args))
      (define assigned-count 0)
      (define chars-scanned 0)
      (define items '())

      (define (read-input-char)
        (set! chars-scanned (+ 1 chars-scanned))
        (read-char input-port))

      (define (return)
        (cond ((and (zero? chars-scanned)
                    (eof-object? (peek-char input-port)))
               (peek-char input-port))
              (setters assigned-count)
              (else (reverse items))))

      (cond
        ((equal? "" format-string) (return))
        ((string? input-port)
         (call-with-input-string
           input-port
           (lambda (str-port)
             (apply stdio:scan-and-set format-string str-port args))))
        (else
          (call-with-input-string
            format-string
            (lambda (format-port)

              (define (add-item report-field? next-item)
                (cond (setters
                        (cond ((and report-field? (null? setters))
                               (error 'scanf "not enough variables for format"
                                      format-string))
                              ((not next-item) (return))
                              ((not report-field?) (loop1))
                              (else
                                (let ((suc ((car setters) next-item)))
                                  (unless (boolean? suc)
                                    (slib:warn 'scanf "setter returned non-boolean"
                                               suc))
                                  (set! setters (cdr setters))
                                  (cond ((not suc) (return))
                                        ((eqv? -1 report-field?) (loop1))
                                        (else
                                          (set! assigned-count (+ 1 assigned-count))
                                          (loop1)))))))
                      ((not next-item) (return))
                      (report-field? (set! items (cons next-item items))
                                     (loop1))
                      (else (loop1))))

              (define (loop1)
                (define fc (read-char format-port))
                (cond
                  ((eof-object? fc)
                   (return))
                  ((char-whitespace? fc)
                   (flush-whitespace format-port)
                   (set! chars-scanned (+ (flush-whitespace input-port) chars-scanned))
                   (loop1))
                  ((eqv? #\% fc)		; interpret next format
                   (set! fc (read-char format-port))
                   (let ((report-field? (not (eqv? #\* fc)))
                         (width #f))

                     (define (get-width) width)
                     (define (width--) (if width (set! width (+ -1 width))))

                     (when (not report-field?) (set! fc (read-char format-port)))
                     (when (char-numeric? fc) (set! width 0))
                     (do () ((or (eof-object? fc) (char-non-numeric? fc)))
                       (set! width (+ (* 10 width) (string->number (string fc))))
                       (set! fc (read-char format-port)))
                     (case fc			;ignore h,l,L modifiers.
                       ((#\h #\l #\L) (set! fc (read-char format-port))))
                     (case fc
                       ((#\n) (when (not report-field?)
                                (error 'scanf "not saving %n??"))
                              (add-item -1 chars-scanned)) ;-1 is special flag.
                       ((#\c #\C)
                        (when (not width) (set! width 1))
                        (let ((str (make-string width)))
                          (do ((i 0 (+ 1 i))
                               (c (peek-char input-port) (peek-char input-port)))
                            ((or (>= i width)
                                 (eof-object? c))
                             (add-item report-field? (string-copy str 0 i)))
                            (string-set! str i (read-input-char)))))
                       ((#\s #\S)
                        (add-item report-field? (read-word width char-whitespace? input-port read-input-char)))
                       ((#\[)
                        (set! fc (read-char format-port))
                        (let ((allbut #f))
                          (case fc
                            ((#\^) (set! allbut #t)
                                   (set! fc (read-char format-port))))

                          (let scanloop ((scanset (list fc)))
                            (set! fc (read-char format-port))
                            (case fc
                              ((#\-)
                               (set! fc (peek-char format-port))
                               (cond
                                 ((and (char<? (car scanset) fc)
                                       (not (eqv? #\] fc)))
                                  (set! fc (char->integer fc))
                                  (do ((i (char->integer (car scanset)) (+ 1 i)))
                                    ((> i fc) (scanloop scanset))
                                    (set! scanset (cons (integer->char i) scanset))))
                                 (else (scanloop (cons #\- scanset)))))
                              ((#\])
                               (add-item report-field?
                                         (read-word
                                           width
                                           (if allbut (lambda (c) (memv c scanset))
                                             (lambda (c) (not (memv c scanset))))
                                           input-port
                                           read-input-char)))
                              (else (if (eof-object? fc)
                                      (error 'scanf "unmatched [ in format"))
                                    (scanloop (cons fc scanset)))))))
                       ((#\o #\O)
                        (add-item report-field? (read-o input-port read-input-char get-width width--)))
                       ((#\u #\U)
                        (add-item report-field? (read-u input-port read-input-char get-width width--)))
                       ((#\d #\D)
                        (add-item report-field? (read-signed read-u input-port read-input-char get-width width--)))
                       ((#\x #\X)
                        (add-item report-field? (read-x input-port read-input-char get-width width--)))
                       ((#\e #\E #\f #\F #\g #\G)
                        (add-item report-field? (read-signed read-ui input-port read-input-char get-width width--)))
                       ((#\i)
                        (add-item report-field? (read-signed read-radixed-unsigned input-port read-input-char get-width width--)))
                       ((#\%)
                        (cond ((or width (not report-field?))
                               (error 'SCANF "%% has modifiers?"))
                              ((eqv? #\% (read-input-char))
                               (loop1))
                              (else (return))))
                       (else (error 'SCANF
                                    "Unknown format directive:" fc)))))
                  ((eqv? (peek-char input-port) fc)
                   (read-input-char)
                   (loop1))
                  (else (return))))
              (loop1))))))

    ;;;This implements a Scheme-oriented version of SCANF: returns a list of
    ;;;objects read (rather than set!-ing values).
    ;@
    (define scanf-read-list
      (case-lambda
        ((format-string)
         (scanf-read-list format-string (current-input-port)))
        ((format-string input-port)
         (cond ((input-port? input-port)
                (stdio:scan-and-set format-string input-port #f))
               ((string? input-port)
                (call-with-input-string
                  input-port (lambda (input-port)
                               (stdio:scan-and-set format-string input-port #f))))
               (else 
                 (error 'scanf-read-list "argument 2 not a port"
                        input-port))))
        (else
          (error 'scanf-read-list 'wrong-number-of-args))))

    ;; Returns the objects read as separate values, preceded by the number read
    (define scanf-read-values
      (case-lambda
        ((format-string)
         (scanf-read-values format-string (current-input-port)))
        ((format-string input-port)
         (let ((res (scanf-read-list format-string input-port)))
           (apply values (length res) res)))))

    ;; Not needed without scanf sscanf fscanf
    ;    (define (stdio:setter-procedure sexp)
    ;      (let ((v (gentemp)))
    ;        (cond ((symbol? sexp) `(lambda (,v) (set! ,sexp ,v) #t))
    ;              ((not (and (pair? sexp) (list? sexp)))
    ;               (error 'scanf "setter expression not understood" sexp))
    ;              (else
    ;                (case (car sexp)
    ;                  ((vector-ref) `(lambda (,v) (vector-set! ,@(cdr sexp) ,v) #t))
    ;                  ((array-ref) `(lambda (,v) (array-set! ,(cadr sexp) ,v ,@(cddr sexp)) #t))
    ;                  ((substring)
    ;                   `(lambda (,v) (substring-move-left!
    ;                                   ,v 0 (min (string-length ,v)
    ;                                             (- ,(cadddr sexp) ,(caddr sexp)))
    ;                                   ,(cadr sexp) ,(caddr sexp))
    ;                      #t))
    ;                  ((list-ref)
    ;                   `(lambda (,v) (set-car! (list-tail ,@(cdr sexp)) ,v) #t))
    ;                  ((car) `(lambda (,v) (set-car! ,@(cdr sexp) ,v) #t))
    ;                  ((cdr) `(lambda (,v) (set-cdr! ,@(cdr sexp) ,v) #t))
    ;                  (else (error 'scanf "setter not known" sexp)))))))

    ;; TODO: scanf sscanf fscanf not defined as no define-macro in R7RS

    ))

