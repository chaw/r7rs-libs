;; Tests and related functions extracted from http://okmij.org/ftp/Scheme/lib/SSAX.scm
;; Converted to SRFI 64 form by Peter Lane, 2017

(import (scheme base)
        (slib string-port)
        (slib xml-parse)
        (only (scheme list) cons*)
        (srfi 64))

(define (string-join strs sep) ;; implemented here to avoid srfi 13 / not dependency
  (if (null? strs)
    ""
    (do ((rem (cdr strs) (cdr rem))
         (res (car strs) (string-append res sep (car rem))))
      ((null? rem) res))))

; The following function, which is often used in validation tests,
; lets us conveniently enter newline, CR and tab characters in a character
; string.
;	unesc-string: ESC-STRING -> STRING
; where ESC-STRING is a character string that may contain
;    %n  -- for #\newline
;    %r  -- for #\return
;    %t  -- for #\tab
;    %%  -- for #\%
;
; The result of unesc-string is a character string with all %-combinations
; above replaced with their character equivalents
(define (unesc-string str)
  (call-with-input-string str
                          (lambda (port)
                            (let loop ((frags '()))
                              (let* ((token (ssax:next-token '() '(#\% *eof*) "unesc-string" port))
                                     (cterm (read-char port))
                                     (frags (cons token frags)))
                                (if (eof-object? cterm) (string-join (reverse frags) "")
                                  (let ((cchar (read-char port)))  ; char after #\%
                                    (if (eof-object? cchar)
                                      (error "unexpected EOF after reading % in unesc-string:" str)
                                      (loop
                                        (cons
                                          (case cchar
                                            ((#\n) (string #\newline))
                                            ((#\r) (string (integer->char 13)))
                                            ((#\t) (string #\tab))
                                            ((#\%) "%")
                                            (else (error "bad %-char in unesc-string:" cchar)))
                                          frags))))))))))

(test-begin "slib-xml-parse")

;; Test cases from my documentation
(test-equal '(*TOP* (tag "content")) 
            (ssax:xml->sxml (open-input-string "<tag>content</tag>") '()))

(test-equal '(*TOP* (person (@ (gender "male")) (name "Peter") (number "1234")))
            (ssax:xml->sxml (open-input-string "<person gender=\"male\"><name>Peter</name><number>1234</number></person>") '()))


;; Tests from http://okmij.org/ftp/Scheme/lib/SSAX.scm 
(test-equal '_ (call-with-input-string "_" ssax:read-NCName))
(test-equal '_ (call-with-input-string "_" ssax:read-QName))
(test-equal (string->symbol "_abc_")
            (call-with-input-string "_abc_;" ssax:read-NCName))
(test-equal (string->symbol "_abc_")
            (call-with-input-string "_abc_;" ssax:read-QName))
(test-equal (string->symbol "_a.b")
            (call-with-input-string "_a.b " ssax:read-QName))
(test-equal (cons (string->symbol "_a.b") (string->symbol "d.1-ef-"))
            (call-with-input-string "_a.b:d.1-ef-;" ssax:read-QName))
(test-equal (cons (string->symbol "a") (string->symbol "b"))
            (call-with-input-string "a:b:c" ssax:read-QName))
(test-error (call-with-input-string ":abc" ssax:read-NCName))
(test-error (call-with-input-string "1:bc" ssax:read-NCName))

(test-equal "p1 content "
            (call-with-input-string "<?pi1  p1 content ?>"
                                    (lambda (port)
                                      (ssax:read-markup-token port)
                                      (ssax:read-pi-body-as-string port))))
(test-equal "pi2? content? ?"
            (call-with-input-string "<?pi2 pi2? content? ??>"
                                    (lambda (port)
                                      (ssax:read-markup-token port)
                                      (ssax:read-pi-body-as-string port))))

(letrec
  ((consumer (lambda (fragment foll-fragment seed)
               (cons* (if (equal? foll-fragment (string #\newline))
                        " NL" foll-fragment) fragment seed)))
   (test (lambda (str expected-result)
           (let ((result
                   (reverse 
                     (call-with-input-string (unesc-string str)
                                             (lambda (port) (ssax:read-cdata-body port consumer '()))
                                             ))))
             (test-equal expected-result result)))))
  (test "]]>" '())
  (test "abcd]]>" '("abcd" ""))
  (test "abcd]]]>" '("abcd" "" "]" ""))
  (test "abcd]]]]>" '("abcd" "" "]" "" "]" ""))
  (test "abcd]]]]]>" '("abcd" "" "]" "" "]" "" "]" ""))
  (test "abcd]]]a]]>" '("abcd" "" "]" "" "]]" "" "a" ""))
  (test "abc%r%ndef%n]]>" '("abc" " NL" "def" " NL"))     
  (test "%r%n%r%n]]>" '("" " NL" "" " NL"))
  (test "%r%n%r%na]]>" '("" " NL" "" " NL" "a" ""))
  (test "%r%r%r%na]]>" '("" " NL" "" " NL" "" " NL" "a" ""))
  (test "abc&!!!]]>" '("abc" "&" "" "" "!!!" ""))
  (test "abc]]&gt;&gt&amp;]]]&gt;and]]>"
        '("abc" "" "]]" "" "" ">" "" "&" "gt" "" "" "&" "amp" "" ";" "" "]" ""
          "]]" "" "" ">" "and" "")))

(letrec
  ((test (lambda (str decl-entities expected-res)
           (let ((result
                   (call-with-input-string (unesc-string str)
                                           (lambda (port)
                                             (ssax:read-attributes port decl-entities)))))
             (test-equal result expected-res)))))
  (test "" '() '())
  (test "href='http://a%tb%r%n%r%n%nc'" '()
        `((,(string->symbol "href") . "http://a b   c")))
  (test "href='http://a%tb%r%r%n%rc'" '()
        `((,(string->symbol "href") . "http://a b   c")))
  (test "_1 ='12&amp;' _2= \"%r%n%t12&#10;3\">" '()
        `((_1 . "12&") (_2 . ,(unesc-string "  12%n3"))))
  (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />" 
        '((ent . "&lt;xx&gt;"))
        `((,(string->symbol "Abc") . ,(unesc-string "<&>%n"))
          (,(string->symbol "Next") . "12<xx>34")))
  (test "%tAbc='&lt;&amp;&gt;&#x0d;'%nNext='12&ent;34' />" 
        '((ent . "&lt;xx&gt;"))
        `((,(string->symbol "Abc") . ,(unesc-string "<&>%r"))
          (,(string->symbol "Next") . "12<xx>34")))
  (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&en;34' />" 
        `((en . ,(lambda () (open-input-string "&quot;xx&apos;"))))
        `((,(string->symbol "Abc") . ,(unesc-string "<&>%n"))
          (,(string->symbol "Next") . "12\"xx'34")))
  (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />" 
        '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&amp;"))
        `((,(string->symbol "Abc") . ,(unesc-string "<&>%n"))
          (,(string->symbol "Next") . "12<&T;>34")))
  (test-error 
    (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />" 
          '((ent . "<&ent1;T;&gt;") (ent1 . "&amp;")) '()))
  (test-error
    (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />" 
          '((ent . "&lt;&ent;T;&gt;") (ent1 . "&amp;")) '()))
  (test-error
    (test "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />" 
          '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&ent;")) '()))
  (test "html:href='http://a%tb%r%n%r%n%nc'" '()
        `(((,(string->symbol "html") . ,(string->symbol "href"))
           . "http://a b   c")))
  (test "html:href='ref1' html:src='ref2'" '()
        `(((,(string->symbol "html") . ,(string->symbol "href"))
           . "ref1")
          ((,(string->symbol "html") . ,(string->symbol "src"))
           . "ref2")))
  (test "html:href='ref1' xml:html='ref2'" '()
        `(((,(string->symbol "html") . ,(string->symbol "href"))
           . "ref1")
          ((,ssax:Prefix-XML . ,(string->symbol "html"))
           . "ref2")))
  (test-error (test "html:href='ref1' html:href='ref2'" '() '()))
  (test-error (test "html:href='<' html:href='ref2'" '() '()))
  (test-error (test "html:href='ref1' html:href='&ref2;'" '() '()))
  )

(let* ((namespaces
         '((HTML UHTML . URN-HTML)
           (HTML UHTML-1 . URN-HTML)
           (A    UHTML . URN-HTML)))
       (namespaces-def
         (cons
           '(*DEFAULT* DEF . URN-DEF) namespaces))
       (namespaces-undef
         (cons
           '(*DEFAULT* #f . #f) namespaces-def))
       (port (current-input-port)))

  (test-equal 'ABC 
              (ssax:resolve-name port 'ABC namespaces #t))
  (test-equal '(DEF . ABC)
              (ssax:resolve-name port 'ABC namespaces-def #t))
  (test-equal 'ABC
              (ssax:resolve-name port 'ABC namespaces-def #f))
  (test-equal 'ABC
              (ssax:resolve-name port 'ABC namespaces-undef #t))
  (test-equal '(UHTML . ABC)
              (ssax:resolve-name port '(HTML . ABC) namespaces-def #t))
  (test-equal '(UHTML . ABC)
              (ssax:resolve-name port '(HTML . ABC) namespaces-def #f))
  (test-equal `(,ssax:Prefix-XML . space)
              (ssax:resolve-name port 
                                 `(,(string->symbol "xml") . space) namespaces-def #f))
  (test-error
    (ssax:resolve-name port '(XXX . ABC) namespaces-def #f))
  )

(let* ((urn-a (string->symbol "urn:a")) 
       (urn-b (string->symbol "urn:b"))
       (urn-html (string->symbol "http://w3c.org/html"))
       (namespaces
         `((#f '"UHTML" . ,urn-html)
           ('"A"  '"UA" . ,urn-a)))
       (test
         (lambda (tag-head-name elems str)
           (call-with-input-string str
                                   (lambda (port)
                                     (call-with-values
                                       (lambda ()
                                         (ssax:complete-start-tag
                                           (call-with-input-string tag-head-name
                                                                   (lambda (port) (ssax:read-QName port)))
                                           port
                                           elems '() namespaces))
                                       list))))))

  ; First test with no validation of elements
  (test-equal `(TAG1 () ,namespaces ANY)
              (test "TAG1" #f ">"))
  (test-equal `(TAG1 () ,namespaces EMPTY-TAG)
              (test "TAG1" #f "/>"))
  (test-equal `(TAG1 ((HREF . "a")) ,namespaces EMPTY-TAG)
              (test "TAG1" #f "HREF='a'/>"))
  (test-equal `(('"UA" . TAG1) ((HREF . "a"))
                               ,(cons `(*DEFAULT* '"UA" . ,urn-a) namespaces) ANY)
              (test "TAG1" #f "HREF='a' xmlns='urn:a'>"))
  (test-equal `(TAG1 ((HREF . "a"))
                     ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
              (test "TAG1" #f "HREF='a' xmlns=''>"))
  (test-error (test "UA:TAG1" #f "HREF='a' xmlns=''/>"))
  ;  (test-equal `(('"UA" . TAG1) ((('"UA" . HREF) . "a"))                        ;; TODO
  ;                                  ,(cons '(*DEFAULT* #f . #f) namespaces) ANY)
  ;              (test "A:TAG1" #f "A:HREF='a' xmlns=''>"))
  ;  (test-equal `(('"UA" . TAG1) ((('"UA" . HREF) . "a"))
  ;                                  ,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) ANY)
  ;              (test "A:TAG1" #f "A:HREF='a' xmlns='urn:b'>"))
  (test-error (test "B:TAG1" #f "A:HREF='a' xmlns:b=''/>"))
  #; (test-equal `((,urn-b . TAG1) ((('"UA" . HREF) . "a"))
  ,(cons `('"B" ,urn-b . ,urn-b) namespaces) ANY)
(test "B:TAG1" #f "A:HREF='a' xmlns:B='urn:b'>"))
#; (test-equal `((,urn-b . TAG1) ((('"UA" . HREF) . "a")
((,urn-b . '"SRC") . "b"))
,(cons `('"B" ,urn-b . ,urn-b) namespaces) ANY)
(test "B:TAG1" #f 
      "B:SRC='b' A:HREF='a' xmlns:B='urn:b'>"))
#; (test-equal `((,urn-b . TAG1) ((('"UA" . HREF) . "a")
((,urn-b . HREF) . "b"))
,(cons `('"B" ,urn-b . ,urn-b) namespaces) ANY)
(test "B:TAG1" #f 
      "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:b'>"))
; must be an error! Duplicate attr
(test-error (test "B:TAG1" #f
                  "HREF=\"b\" HREF='a' xmlns:B='urn:a'/>"))
; must be an error! Duplicate attr after ns expansion
(test-error (test "B:TAG1" #f 
                  "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:a'/>"))
#; (test-equal `(('"UA" . TAG1) ((HREF . "a")
(('"UA" . HREF) . "b"))
,(cons `(*DEFAULT* '"UA" . ,urn-a) namespaces) ANY)
(test "TAG1" #f 
      "A:HREF=\"b\" HREF='a' xmlns='urn:a'>"))
#;(test-equal `(TAG1 ((('"UHTML" . HREF) . "a")
((,urn-b . HREF) . "b"))
,(append `(
           ('"HTML" '"UHTML" . ,urn-html)
           ('"B" ,urn-b . ,urn-b))
         namespaces) ANY)
(test "TAG1" #f 
      "B:HREF=\"b\" xmlns:B='urn:b' xmlns:HTML='http://w3c.org/html' HTML:HREF='a' >"))

; Now test the validating parsing
; No decl for tag1
(test-error (test "TAG1" '((TAG2 ANY ()))
                  "B:HREF='b' xmlns:B='urn:b'>"))
(test-error
  (test "TAG1" '((TAG1 ANY (('"HREF1" CDATA IMPLIED #f))))
        "B:HREF='b' xmlns:B='urn:b'>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces EMPTY-TAG)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
                  "HREF='b'/>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
                  "HREF='b'>"))
; Req'd attribute not given error
(test-error 
  (test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f))))
        ">"))
; Wrong content-type of the attribute
(test-error 
  (test "TAG1" '((TAG1 PCDATA ((HREF ("c") REQUIRED #f))))
        "HREF='b'>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF ("c" "b") IMPLIED #f))))
                  "HREF='b'>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED "c"))))
                  "HREF='b'>"))
; Bad fixed attribute
(test-error 
  (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "c"))))
        "HREF='b'>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "b"))))
                  "HREF='b'>"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA FIXED "b")))) ">"))
(test-equal `(TAG1 ((HREF . "b")) ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED "b")))) ">"))
(test-equal `(TAG1 () ,namespaces PCDATA)
            (test "TAG1" '((TAG1 PCDATA ((HREF CDATA IMPLIED #f)))) ">"))
; Undeclared attr
(test-error 
  (test "TAG1"
        '((TAG1 PCDATA ((('"A" . HREF) CDATA IMPLIED "c"))))
        "HREF='b'>"))
#;(test-equal `(TAG1 ((HREF . "b") (('"UA" . HREF) . "c"))
,namespaces PCDATA)
(test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                             (('"A" . HREF) CDATA IMPLIED "c"))))
      "HREF='b'>"))
#;(test-equal `(('"UA" . TAG1)
((HREF . "b") (('"UA" . HREF) . "c"))
,namespaces PCDATA)
(test "A:TAG1" '((('"A" . TAG1) PCDATA
                                ((HREF NMTOKEN REQUIRED #f)
                                 (('"A" . HREF) CDATA IMPLIED "c"))))
      "HREF='b'>"))
#;(test-equal `((,urn-b . TAG1) ((HREF . "b"))
,(cons `('"B" ,urn-b . ,urn-b) namespaces) PCDATA)
(test "B:TAG1" '((('"B" . TAG1) PCDATA ((HREF CDATA REQUIRED #f)
                                        (('"xmlns" . '"B") CDATA IMPLIED "urn:b"))))
      "HREF='b'>"))
#;(test-equal `((,urn-b . TAG1) (((,urn-b . HREF) . "b"))
,(cons `('"B" ,urn-b . ,urn-b) namespaces) PCDATA)
(test "B:TAG1" '((('"B" . TAG1) PCDATA
                                ((('"B" . HREF) CDATA REQUIRED #f)
                                 (('"xmlns" . '"B") CDATA IMPLIED "urn:b"))))
      "B:HREF='b'>"))
#;(test-equal `((,urn-b . TAG1) ((HREF . "b"))
,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
(test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                             ('"xmlns" CDATA IMPLIED "urn:b"))))
      "HREF='b'>"))
; xmlns not declared
#;(test-equal `((,urn-b . TAG1) ((HREF . "b"))
,(cons `(*DEFAULT* ,urn-b . ,urn-b) namespaces) PCDATA)
(test "TAG1" '((TAG1 PCDATA ((HREF CDATA REQUIRED #f)
                             )))
      "HREF='b' xmlns='urn:b'>"))
; xmlns:B not declared
#;(test-equal `((,urn-b . TAG1) (((,urn-b . HREF) . "b"))
,(cons `('"B" ,urn-b . ,urn-b) namespaces) PCDATA)
(test "B:TAG1" '((('"B" . TAG1) PCDATA
                                ((('"B" . HREF) CDATA REQUIRED #f)
                                 )))
      "B:HREF='b' xmlns:B='urn:b'>"))
)

(letrec
  ((test (lambda (str namespace-assig expected-res)
           (let ((result
                   (call-with-input-string (unesc-string str)
                                           (lambda (port)
                                             (ssax:xml->sxml port namespace-assig)))))
             (test-equal result expected-res)))))

  (test " <BR/>" '() '(*TOP* (BR)))
  (test "<BR></BR>" '() '(*TOP* (BR)))
  (test " <BR CLEAR='ALL'%nCLASS='Class1'/>" '()
        '(*TOP* (BR (@ (CLEAR "ALL") (CLASS "Class1")))))
  (test "   <A HREF='URL'>  link <I>itlink </I> &amp;amp;</A>" '()
        '(*TOP* (A (@ (HREF "URL")) "  link " (I "itlink ") " &amp;")))
  (test "   <A HREF='URL' xml:space='preserve'>  link <I>itlink </I> &amp;amp;</A>" '()
        '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                   "  link " (I "itlink ") " &amp;")))
  (test "   <A HREF='URL' xml:space='preserve'>  link <I xml:space='default'>itlink </I> &amp;amp;</A>" '()
        '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                   "  link " (I (@ (xml:space "default"))
                                "itlink ") " &amp;")))
  ;  (test " <P><?pi1  p1 content ?>?<?pi2 pi2? content? ??></P>" '()  ;; TODO
  ;        '(*TOP* (P (*PI* pi1 "p1 content ") "?"
  ;                   (*PI* pi2 "pi2? content? ?"))))
  (test " <P>some text <![CDATA[<]]>1%n&quot;<B>strong</B>&quot;%r</P>"
        '()
        `(*TOP* (P ,(unesc-string "some text <1%n\"")
                   (B "strong") ,(unesc-string "\"%n"))))
  (test " <P><![CDATA[<BR>%n<![CDATA[<BR>]]&gt;]]></P>" '()
        `(*TOP* (P ,(unesc-string "<BR>%n<![CDATA[<BR>]]>"))))
  (test "<T1><T2>it&apos;s%r%nand   that%n</T2>%r%n%r%n%n</T1>" '()
        `(*TOP* (T1 (T2 ,(unesc-string "it's%nand   that%n")))))
  (test "<T1><T2>it&apos;s%rand   that%n</T2>%r%n%r%n%n</T1>" '()
        `(*TOP* (T1 (T2 ,(unesc-string "it's%nand   that%n")))))
  (test "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->%n<T/>" '()
        '(*TOP* (T)))
  ;  (test "<?xml version='1.0'?>%n<WEIGHT unit=\"pound\">%n<NET certified='certified'> 67 </NET>%n<GROSS> 95 </GROSS>%n</WEIGHT>" '()
  ;        '(*TOP* (*PI* xml "version='1.0'") (WEIGHT (@ (unit "pound"))
  ;                                                   (NET (@ (certified "certified")) " 67 ")
  ;                                                   (GROSS " 95 "))))
  (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '()
        '(*TOP* (URI1:DIV (@ (URI1:B "A") (B "B")) (URI1:P (BR)))))
  (test "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>" '((UA . "URI1"))
        '(*TOP* (@ (*NAMESPACES* (UA "URI1")))
                (UA:DIV (@ (UA:B "A") (B "B")) (UA:P (BR)))))

  ; A few tests from XML Namespaces Recommendation
  (test (string-append
          "<x xmlns:edi='http://ecommerce.org/schema'>"
          "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
          "<lineItem edi:taxClass='exempt'>Baby food</lineItem>" "\n"
          "</x>") '()
        '(*TOP* 
           (x (lineItem
                (@ (http://ecommerce.org/schema:taxClass "exempt"))
                "Baby food"))))
  (test (string-append 
          "<x xmlns:edi='http://ecommerce.org/schema'>"
          "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
          "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
          "</x>") '((EDI . "http://ecommerce.org/schema"))
        '(*TOP*
           (@ (*NAMESPACES* (EDI "http://ecommerce.org/schema")))
           (x (lineItem
                (@ (EDI:taxClass "exempt"))
                "Baby food"))))

  (test (string-append
          "<bk:book xmlns:bk='urn:loc.gov:books' "
          "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
          "<bk:title>Cheaper by the Dozen</bk:title>"
          "<isbn:number>1568491379</isbn:number></bk:book>")
        '()
        '(*TOP* (urn:loc.gov:books:book
                  (urn:loc.gov:books:title "Cheaper by the Dozen")
                  (urn:ISBN:0-395-36341-6:number "1568491379"))))

  (test (string-append
          "<!-- initially, the default namespace is 'books' -->"
          "<book xmlns='urn:loc.gov:books' "
          "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
          "<title>Cheaper by the Dozen</title>"
          "<isbn:number>1568491379</isbn:number>"
          "<notes>"
          "<!-- make HTML the default namespace for some commentary -->"
          "<p xmlns='urn:w3-org-ns:HTML'>"
          "This is a <i>funny</i> book!"
          "</p>"
          "</notes>"
          "</book>") '()
        '(*TOP* (urn:loc.gov:books:book
                  (urn:loc.gov:books:title "Cheaper by the Dozen")
                  (urn:ISBN:0-395-36341-6:number "1568491379")
                  (urn:loc.gov:books:notes
                    (urn:w3-org-ns:HTML:p 
                      "This is a " (urn:w3-org-ns:HTML:i "funny")
                      " book!")))))

  (test (string-append
          "<Beers>"
          "<!-- the default namespace is now that of HTML -->"
          "<table xmlns='http://www.w3.org/TR/REC-html40'>"
          "<th><td>Name</td><td>Origin</td><td>Description</td></th>"
          "<tr>"
          "<!-- no default namespace inside table cells -->"
          "<td><brandName xmlns=\"\">Huntsman</brandName></td>"
          "<td><origin xmlns=''>Bath, UK</origin></td>"
          "<td>"
          "<details xmlns=''><class>Bitter</class><hop>Fuggles</hop>"
          "<pro>Wonderful hop, light alcohol, good summer beer</pro>"
          "<con>Fragile; excessive variance pub to pub</con>"
          "</details>"
          "</td>"
          "</tr>"
          "</table>"
          "</Beers>")
        '((html . "http://www.w3.org/TR/REC-html40"))
        '(*TOP*
           (@ (*NAMESPACES* (html "http://www.w3.org/TR/REC-html40")))
           (Beers (html:table
                    (html:th (html:td "Name")
                             (html:td "Origin")
                             (html:td "Description"))
                    (html:tr (html:td (brandName "Huntsman"))
                             (html:td (origin "Bath, UK"))
                             (html:td 
                               (details 
                                 (class "Bitter")
                                 (hop "Fuggles")
                                 (pro "Wonderful hop, light alcohol, good summer beer")
                                 (con "Fragile; excessive variance pub to pub"))))))))

  (test (string-append
          "<!-- 1 --><RESERVATION xmlns:HTML='http://www.w3.org/TR/REC-html40'>"
          "<!-- 2 --><NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
          "<!-- 3 --><SEAT CLASS='Y' HTML:CLASS=\"largeMonotype\">33B</SEAT>"
          "<!-- 4 --><HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>"
          "<!-- 5 --><DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
        '((HTML . "http://www.w3.org/TR/REC-html40"))
        '(*TOP*
           (@ (*NAMESPACES* (HTML "http://www.w3.org/TR/REC-html40")))
           (RESERVATION
             (NAME (@ (HTML:CLASS "largeSansSerif")) "Layman, A")
             (SEAT (@ (HTML:CLASS "largeMonotype") (CLASS "Y")) "33B")
             (HTML:A (@ (HREF "/cgi-bin/ResStatus")) "Check Status")
             (DEPARTURE "1997-05-24T07:55:00+1"))))
  ; Part of RDF from the XML Infoset
  ;  (test (string-join '(
  ;                       "<?xml version='1.0' encoding='utf-8' standalone='yes'?>"
  ;                       "<!-- this can be decoded as US-ASCII or iso-8859-1 as well,"
  ;                       "  since it contains no characters outside the US-ASCII repertoire -->"
  ;                       "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'"
  ;                       "         xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'"
  ;                       "          xmlns='http://www.w3.org/2001/02/infoset#'>"
  ;                       "<rdfs:Class ID='Boolean'/>"
  ;                       "<Boolean ID='Boolean.true'/>"
  ;                       "<Boolean ID='Boolean.false'/>"
  ;                       "<!--Info item classes-->"
  ;                       "<rdfs:Class ID='InfoItem'/>"
  ;                       "<rdfs:Class ID='Document' rdfs:subClassOf='#InfoItem'/>"
  ;                       "<rdfs:Class ID='Element' rdfs:subClassOf='#InfoItem'/>"
  ;                       "<rdfs:Class ID='Attribute' rdfs:subClassOf='#InfoItem'/>"
  ;                       "<rdfs:Class ID='InfoItemSet'
  ;                       rdfs:subClassOf='http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag'/>"
  ;                       "<rdfs:Class ID='AttributeSet' rdfs:subClassOf='#InfoItemSet'/>"
  ;                       "<!--Info item properties-->"
  ;                       "<rdfs:Property ID='allDeclarationsProcessed'>"
  ;                       "<rdfs:domain resource='#Document'/>"
  ;                       "<rdfs:range resource='#Boolean'/></rdfs:Property>"
  ;                       "<rdfs:Property ID='attributes'>"
  ;                       "<rdfs:domain resource='#Element'/>"
  ;                       "<rdfs:range resource='#AttributeSet'/>"
  ;                       "</rdfs:Property>"
  ;                       "</rdf:RDF>")
  ;                     (string #\newline))
  ;        '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  ;          (RDFS . "http://www.w3.org/2000/01/rdf-schema#")
  ;          (ISET . "http://www.w3.org/2001/02/infoset#"))
  ;        '(*TOP* (@ (*NAMESPACES*
  ;                     (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  ;                     (RDFS "http://www.w3.org/2000/01/rdf-schema#")
  ;                     (ISET "http://www.w3.org/2001/02/infoset#")))
  ;                (*PI* xml "version='1.0' encoding='utf-8' standalone='yes'")
  ;                (RDF:RDF
  ;                  (RDFS:Class (@ (ID "Boolean")))
  ;                  (ISET:Boolean (@ (ID "Boolean.true")))
  ;                  (ISET:Boolean (@ (ID "Boolean.false")))
  ;                  (RDFS:Class (@ (ID "InfoItem")))
  ;                  (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Document")))
  ;                  (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Element")))
  ;                  (RDFS:Class (@ (RDFS:subClassOf "#InfoItem") (ID "Attribute")))
  ;                  (RDFS:Class
  ;                    (@ (RDFS:subClassOf
  ;                         "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
  ;                       (ID "InfoItemSet")))
  ;                  (RDFS:Class
  ;                    (@ (RDFS:subClassOf "#InfoItemSet") (ID "AttributeSet")))
  ;                  (RDFS:Property
  ;                    (@ (ID "allDeclarationsProcessed"))
  ;                    (RDFS:domain (@ (resource "#Document")))
  ;                    (RDFS:range (@ (resource "#Boolean"))))
  ;                  (RDFS:Property
  ;                    (@ (ID "attributes"))
  ;                    (RDFS:domain (@ (resource "#Element")))
  ;                    (RDFS:range (@ (resource "#AttributeSet")))))))

  ; Part of RDF from RSS of the Daemon News Mall
  ;  (test (string-join '(
  ;                       "<?xml version='1.0'?><rdf:RDF "
  ;                       "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' "
  ;                       "xmlns='http://my.netscape.com/rdf/simple/0.9/'>"
  ;                       "<channel>"
  ;                       "<title>Daemon News Mall</title>"
  ;                       "<link>http://mall.daemonnews.org/</link>"
  ;                       "<description>Central source for all your BSD needs</description>"
  ;                       "</channel>"
  ;                       "<item>"
  ;                       "<title>Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95</title>"
  ;                       "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=880</link>"
  ;                       "</item>"
  ;                       "<item>"
  ;                       "<title>The Design and Implementation of the 4.4BSD Operating System $54.95</title>"
  ;                       "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=912&amp;category_id=1761</link>"
  ;                       "</item>"
  ;                       "</rdf:RDF>")
  ;                     (string #\newline))
  ;        '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  ;          (RSS . "http://my.netscape.com/rdf/simple/0.9/")
  ;          (ISET . "http://www.w3.org/2001/02/infoset#"))
  ;        '(*TOP* (@ (*NAMESPACES*
  ;                     (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  ;                     (RSS "http://my.netscape.com/rdf/simple/0.9/")
  ;                     (ISET "http://www.w3.org/2001/02/infoset#")))
  ;                (*PI* xml "version='1.0'")
  ;                (RDF:RDF (RSS:channel
  ;                           (RSS:title "Daemon News Mall")
  ;                           (RSS:link "http://mall.daemonnews.org/")
  ;                           (RSS:description "Central source for all your BSD needs"))
  ;                         (RSS:item
  ;                           (RSS:title
  ;                             "Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95")
  ;                           (RSS:link
  ;                             "http://mall.daemonnews.org/?page=shop/flypage&product_id=880"))
  ;                         (RSS:item
  ;                           (RSS:title
  ;                             "The Design and Implementation of the 4.4BSD Operating System $54.95")
  ;                           (RSS:link
  ;                             "http://mall.daemonnews.org/?page=shop/flypage&product_id=912&category_id=1761")))))

  (test (string-join 
          '("<Forecasts TStamp='958082142'>"
            "<TAF TStamp='958066200' LatLon='36.583, -121.850' BId='724915'"
            "  SName='KMRY, MONTEREY PENINSULA'>"
            "<VALID TRange='958068000, 958154400'>111730Z 111818</VALID>"
            "<PERIOD TRange='958068000, 958078800'>"
            "<PREVAILING>31010KT P6SM FEW030</PREVAILING>"
            "</PERIOD>"
            "<PERIOD TRange='958078800, 958104000' Title='FM2100'>"
            "<PREVAILING>29016KT P6SM FEW040</PREVAILING>"
            "</PERIOD>"
            "<PERIOD TRange='958104000, 958154400' Title='FM0400'>"
            "<PREVAILING>29010KT P6SM SCT200</PREVAILING>"
            "<VAR Title='BECMG 0708' TRange='958114800, 958118400'>VRB05KT</VAR>"
            "</PERIOD></TAF>"
            "</Forecasts>")
          (string #\newline))
        '()
        '(*TOP* (Forecasts
                  (@ (TStamp "958082142"))
                  (TAF (@ (TStamp "958066200")
                          (SName "KMRY, MONTEREY PENINSULA")
                          (LatLon "36.583, -121.850")
                          (BId "724915"))
                       (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
                       (PERIOD (@ (TRange "958068000, 958078800"))
                               (PREVAILING "31010KT P6SM FEW030"))
                       (PERIOD (@ (Title "FM2100") (TRange "958078800, 958104000"))
                               (PREVAILING "29016KT P6SM FEW040"))
                       (PERIOD (@ (Title "FM0400") (TRange "958104000, 958154400"))
                               (PREVAILING "29010KT P6SM SCT200")
                               (VAR (@ (Title "BECMG 0708")
                                       (TRange "958114800, 958118400"))
                                    "VRB05KT"))))))
  )

(test-end)

