;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/json/select/parser.scm - JSONSelect grammer parser library
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

;; Packaged for R7RS Scheme by Peter Lane, 2017


(define-library 
  (rebottled json-parser)
    (export json:parse-selector)
    (import (scheme base) 
            (scheme char)
            (rebottled packrat) 
            (only (srfi 1) cons*))

    (begin

      (define json:parse-selector
        (let ()
          (define parser
            (packrat-parser
              (begin
                (define (white results)
                  (let ((ch (parse-results-token-value results)))
                    (if (and ch (char-whitespace? ch))
                      (white (parse-results-next results))
                      (comment results)
                      )))
                (define (skip-comment-char results)
                  (comment-body (parse-results-next results)))
                (define (skip-to-newline results)
                  (if (memv (parse-results-token-value results)
                            '(#\newline #\return))
                    (white results)
                    (skip-to-newline (parse-results-next results))))
                (define (token str)
                  (lambda (starting-results)
                    (let loop ((pos 0) (results starting-results))
                      (if (= pos (string-length str))
                        (make-result str results)
                        (let ((ch (parse-results-token-value results)))
                          (if (and ch (char=? ch (string-ref str pos)))
                            (loop (+ pos 1) (parse-results-next results))
                            (make-expected-result 
                              (parse-results-position starting-results) str)))))))
                (define (interpret-string-unicode-escape results k)
                  (let loop ((i 0)
                             (acc '())
                             (results results))
                    (let ((ch (parse-results-token-value results)))
                      (cond ((= i 4)
                             (k
                               (integer->char (string->number (list->string (reverse acc)) 16))
                               results))
                            ((memv ch '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F))
                             (loop (+ i 1)
                                   (cons ch acc)
                                   (parse-results-next results)))
                            (else (error 'json-read "JSON Parse Error"))))))
                (define (interpret-string-escape results k)
                  (let ((ch (parse-results-token-value results)))
                    (cond
                      ((assv ch '((#\b . #\backspace)
                                  (#\n . #\newline)
                                  (#\f . 12) ; #\page)
                                  (#\r . #\return)
                                  (#\t . #\tab)))
                       => (lambda(x)(k (cdr x)
                                       (parse-results-next results))))
                      ((eqv? #\u ch)
                       (interpret-string-unicode-escape
                         (parse-results-next results)
                         k))
                      (else (k ch (parse-results-next results))))))
                (define (jstring-body results)
                  (let loop ((acc '()) (results results))
                    (let ((ch (parse-results-token-value results)))
                      (case ch
                        ((#\\) (interpret-string-escape (parse-results-next results)
                                                        (lambda (val results)
                                                          (loop (cons val acc) results))))
                        ((#\") (make-result (list->string (reverse acc)) results))
                        (else (loop (cons ch acc) (parse-results-next results)))))))
                (define (jnumber-body starting-results)
                  (let loop ((acc '()) (results starting-results))
                    (let ((ch (parse-results-token-value results)))
                      (if (memv ch '(#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E))
                        (loop (cons ch acc) (parse-results-next results))
                        (let ((n (string->number (list->string (reverse acc)))))
                          (if n
                            (make-result n results)
                            (make-expected-result 
                              (parse-results-position starting-results)
                              'number)))))))
                #|
                name
                : nmstart nmchar*
                ;
                nmstart
                : escape | [_a-zA-Z] | nonascii
                ;
                nmchar
                : [_a-zA-Z0-9-]
                | escape
                | nonascii
                ;
                escape
                : \\[^\r\n\f0-9a-fA-F]
                ;
                nonascii
                : [^\0-0177]
                ;
                |#
                (define (name results)
                 ; (define name-sets (char-set-union
                 ;                     char-set:letter+digit
                 ;                     (string->char-set "_-")
                 ;                     (char-set-complement char-set:ascii)))
                  (define (valid-char? ch) ; instead of srfi 14
                    (or (char-alphabetic? ch)
                        (char-numeric? ch)
                        (char=? #\_ ch)
                        (char=? #\- ch)
                        (> (char->integer ch) 127)))
                  (let loop ((acc '()) (results results))
                    (let ((ch (parse-results-token-value results)))
                      (cond ((and ch (char=? ch #\\)) ;; handle escape first
                             (interpret-string-escape (parse-results-next results)
                                                      (lambda (val results)
                                                        (loop (cons val acc) results))))
                            ((and ch (valid-char? ch)) ; (char-set-contains? name-sets ch))
                             (loop (cons ch acc) (parse-results-next results)))
                            (else
                              (make-result (list->string (reverse acc)) results))))))
                selectors-group)
              #|
              selectors_group
              : selector [ `,` selector ]*
              ;
              |#
              (selectors-group ((s <- selector g <- selector-group*)
                                ;; make 'or flat
                                (cond ((null? g) s )
                                      ((eq? (car g) 'or)
                                       (cons* 'or s (cdr g)))
                                      (else (list 'or s g)))))
              (or-select ((white '#\, white) 'or))
              (selector-group* ((or-select s <- selector g <- selector-group*) 
                                (if (null? g) s (list 'or s g)))
                               (() '()))
              #|
              selector
              : simple_selector_sequence [ combinator simple_selector_sequence ]*
              ;
              |#
              (selector ((s  <- simple-selector-sequence
                             s* <- simple-selector-sequence*) 
                         (cons s s*)))
              #|
              combinator
              : `>` | \s+
              ;
              |#
              (combinator ((white '#\> white) '>)  ;; parent
                          ((white '#\~ white) '~)  ;; siblings
                          ((white)            '>>));; ancesstor
              #|
              simple_selector_sequence
              : [ type_selector | universal ]
              [ class | pseudo ]*
              | [ class | pseudo ]+
              ;
              |#
              (simple-selector-sequence 
                ((tu <- type-select/universal cp* <- class/psuedo*) 
                 (if (null? cp*) tu (cons tu (list cp*))))
                ((cp+ <- class/psuedo+) cp+))
              ;; auxilities
              (type-select/universal ((t <- type-selector) t)
                                     ((u <- universal) u))
              (class/psuedo+
                ((cp <- class/psuedo cp* <- class/psuedo*)
                 (if (null? cp*) cp (list cp cp*))))
              (class/psuedo* ((cp* <- class/psuedo+) cp*)
                             (() '()))
              (class/psuedo ((c <- class)  c)
                            ((p <- pseudo) p))
              (simple-selector-sequence* 
                ((c <- combinator s* <- selector) (cons c s*))
                (() '()))

              #|
              type_selector
              : `object` | `array` | `number` | `string` | `boolean` | `null`
              ;
              |#
              (type-selector ((o <- (token "object"))  (string->symbol o))
                             ((a <- (token "array"))   (string->symbol a))
                             ((n <- (token "number"))  (string->symbol n))
                             ((s <- (token "string"))  (string->symbol s))
                             ((b <- (token "boolean")) (string->symbol b))
                             ((n <- (token "null"))    (string->symbol n))
                             ((b <- (token "binary"))  (string->symbol b)))
              #|
              universal
              : '*'
              ;
              |#
              (universal (('#\*) '*))
              #|
              class
              : `.` name
              | `.` json_string
              ;
              |#
              (class (('#\. n <- json-string) n)
                     (('#\. n <- name) n))
              #|
              pseudo
              : `:` pseudo_class_name
              | `:` nth_function_name `(` nth_expression `)`
              | `:has` `(`  selectors_group `)`
              | `:expr` `(`  expr `)`
              | `:contains` `(`  json_string `)`
              | `:val` `(` val `)`
              ;
              |#
              (pseudo
                (('#\: pc <- pseudo-class-name) pc)
                (('#\: nth <- nth-function-name '#\( n <- nth-expression '#\))
                 (cons nth (list n)))
                ;; TODO should we use keyword?
                (((token ":has") '#\( s <- selectors-group '#\)) (list 'has s))
                (((token ":expr") '#\( e <- expr '#\)) (list 'expr e))
                (((token ":contains") '#\( s <- json-string '#\)) (list 'contains s))
                (((token ":val") '#\( v <- val '#\))
                 ;; well BNF says val but it's actually string or number...
                 (unless (or (string? v) (number? v))
                   (error 'parse-json-selector ":val requires string or number"))
                 ;; may be too long but for safety
                 ;; we'll make S-expr compiler accept (("class" "value"))
                 ;; structor.
                 (list 'val v)))
              #|
              pseudo_class_name
              : `root` | `first-child` | `last-child` | `only-child` | `empty`
              |#
              (pseudo-class-name (((token "root"))        'root)
                                 (((token "first-child")) 'first-child)
                                 (((token "last-child"))  'last-child)
                                 (((token "only-child"))  'only-child)
                                 (((token "empty"))       'empty))
              #|
              nth_function_name
              : `nth-child` | `nth-last-child`
              |#
              (nth-function-name (((token "nth-child"))      'nth-child)
                                 (((token "nth-last-child")) 'nth-last-child))
              #|
              nth_expression
              : TODO ... wtf? for now only number
              ;
              |#
              (nth-expression ((n <- json-number) n))
              #|
              expr
              : expr binop expr
              | '(' expr ')'
              | val
              ;
              |#
              (expr ((e1 <- expr1 op <- binop e2 <- expr1) (list op e1 e2))
                    ((e <- expr1) e))
              (expr1((white '#\( e <- expr '#\) white) e)
                ((v <- val) v))
              #|
              binop
              : '*' | '/' | '%' | '+' | '-' | '<=' | '>=' | '$='
              | '^=' | '*=' | '>' | '<' | '=' | '!=' | '&&' | '||'
              ;
              |#
              (binop (((token "<=")) '<=)
                     (((token ">=")) '>=)
                     (((token "$=")) '$=)
                     (((token "^=")) '^=)
                     (((token "*=")) '*=)
                     (((token "!=")) '!=)
                     (((token "&&")) 'and) ;; for consistancy of 'or
                     (((token "||")) 'or) ;; to avoid escape symbol
                     (('#\*) '*)
                     (('#\/) '/)
                     (('#\%) '%)
                     (('#\+) '+)
                     (('#\-) '-)
                     (('#\>) '>)
                     (('#\<) '<)
                     (('#\=) '=))
              #|
              val
              : json_number | json_string | 'true' | 'false' | 'null' | 'x'
              ;
              |#
              (val ((white n <- json-number white) n)
                   ((white s <- json-string white) s)
                   ((white (token "true")  white)  #t)
                   ((white (token "false") white)  #f)
                   ((white (token "null")  white)  'null)
                   ((white '#\x white) 'x))
              ;; from (json)
              (json-string ((white '#\" body <- jstring-body '#\") body))
              ;; from (json)
              (json-number ((white body <- jnumber-body) body))
              ;; misc for my sake
              (comment (((token "/*") b <- comment-body) b)
                       (((token "//") b <- skip-to-newline) b)
                       (() 'whitespace))
              (comment-body (((token "*/") w <- white) w)
                            ((skip-comment-char) 'skipped-comment-char))
              ))


          (define (generator p)
            (let ((ateof #f) (pos (top-parse-position "<?>")))
              (lambda ()
                (if ateof
                  (values pos #f)
                  (let ((x (read-char p)))
                    (if (eof-object? x)
                      (begin
                        (set! ateof #t)
                        (values pos #f))
                      (let ((old-pos pos))
                        (set! pos (update-parse-position pos x))
                        (values old-pos (cons x x)))))))))
          (define (read-selector p)
            (let ((result (parser (base-generator->results (generator p)))))
              (if (parse-result-successful? result)
                (parse-result-semantic-value result)
                (error 'parse-json-selector "JSONSelector parse error"
                       (let ((e (parse-result-error result)))
                         (list 'parse-json-selector-error
                               (parse-position->string (parse-error-position e))
                               (parse-error-expected e)
                               (parse-error-messages e)))))))

          (lambda maybe-port
            (read-selector (if (pair? maybe-port) 
                             (car maybe-port)
                             (current-input-port))))))

      ))

