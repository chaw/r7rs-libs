;; Test file for Pregexp
;; https://github.com/ds26gte/pregexp/blob/master/pregexp-test.scm

;; Copyright (c) 1999-2015, Dorai Sitaram.
;; All rights reserved.
;; 
;; Permission to copy, modify, distribute, and use this work or
;; a modified copy of this work, for any purpose, is hereby
;; granted, provided that the copy includes this copyright
;; notice, and in the case of a modified copy, also includes a
;; notice of modification.  This work is provided as is, with
;; no warranty of any kind.

;last substantial change 2005-04-24
;last change 2008-04-12

;; Packaged for R7RS Scheme and SRFI 64 testing by Peter Lane, 2017

(import (scheme base)
        (rebottled pregexp)
        (srfi 64))

(test-begin "rebottled-pregexp")

(test-equal (pregexp "c.r") 
            '(:sub (:or (:seq #\c :any #\r))))
(test-equal (pregexp-match-positions "brain" "bird")
            #f)
(test-equal (pregexp-match-positions "needle" "hay needle stack")
            '((4 . 10)))
(test-equal
  (pregexp-match-positions "needle"
                           "his hay needle stack -- my hay needle stack -- her hay needle stack"
                           24 43)
  '((31 . 37)))
(test-equal
  (pregexp-match "brain" "bird")
  #f)
(test-equal
  (pregexp-match "needle" "hay needle stack")
  '("needle"))
(test-equal   
  (pregexp-split ":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
  '("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin"))
(test-equal
  (pregexp-split " " "pea soup")
  '("pea" "soup"))
(test-equal
  (pregexp-split "" "smithereens")
  '("s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s"))
(test-equal
  (pregexp-split " +" "split pea     soup")
  '("split" "pea" "soup"))
(test-equal
  (pregexp-split " *" "split pea     soup")
  '("s" "p" "l" "i" "t" "p" "e" "a" "s" "o" "u" "p"))
(test-equal
  (pregexp-replace "te" "liberte" "ty")
  "liberty")
(test-equal
  (pregexp-replace* "te" "liberte egalite fraternite" "ty")
  "liberty egality fratyrnity")
(test-equal
  (pregexp-match-positions "^contact" "first contact")
  #f)
(test-equal
  (pregexp-match-positions "laugh$" "laugh laugh laugh laugh")
  '((18 . 23)))
(test-equal
  (pregexp-match-positions "yack\\b" "yackety yack")
  '((8 . 12)))
(test-equal
  (pregexp-match-positions "an\\B" "an analysis")
  '((3 . 5)))
(test-equal
  (pregexp-match "p.t" "pet")
  '("pet"))
(test-equal   
  (pregexp-match "\\d\\d" "0 dear, 1 have to read catch 22 before 9")
  '("22"))
(test-equal
  (pregexp-match "[[:alpha:]_]" "--x--")
  '("x"))
(test-equal
  (pregexp-match "[[:alpha:]_]" "--_--")
  '("_"))
(test-equal
  (pregexp-match "[[:alpha:]_]" "--:--")
  #f)
(test-equal
  (pregexp-match "[:alpha:]" "--a--")
  '("a"))
(test-equal
  (pregexp-match "[:alpha:]" "--_--")
  #f)
(test-equal
  (pregexp-match-positions "c[ad]*r" "cadaddadddr")
  '((0 . 11)))
(test-equal
  (pregexp-match-positions "c[ad]*r" "cr")
  '((0 . 2)))
(test-equal
  (pregexp-match-positions "c[ad]+r" "cadaddadddr")
  '((0 . 11)))
(test-equal
  (pregexp-match-positions "c[ad]+r" "cr")
  #f)
(test-equal
  (pregexp-match-positions "c[ad]?r" "cadaddadddr")
  #f)
(test-equal
  (pregexp-match-positions "c[ad]?r" "cr")
  '((0 . 2)))
(test-equal
  (pregexp-match-positions "c[ad]?r" "car")
  '((0 . 3)))
(test-equal
  (pregexp-match "[aeiou]{3}" "vacuous")
  '("uou"))
(test-equal
  (pregexp-match "[aeiou]{3}" "evolve")
  #f)
(test-equal
  (pregexp-match "[aeiou]{2,3}" "evolve")
  #f)
(test-equal
  (pregexp-match "[aeiou]{2,3}" "zeugma")
  '("eu"))
(test-equal
  (pregexp-match "<.*>" "<tag1> <tag2> <tag3>")
  '("<tag1> <tag2> <tag3>"))
(test-equal
  (pregexp-match "<.*?>" "<tag1> <tag2> <tag3>")
  '("<tag1>"))
(test-equal
  (pregexp-match "([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")
  '("jan 1, 1970" "jan" "1" "1970"))
(test-equal  
  (pregexp-match "(poo )*" "poo poo platter")
  '("poo poo " "poo "))
(test-equal
  (pregexp-match "([a-z ]+;)*" "lather; rinse; repeat;")
  '("lather; rinse; repeat;" " repeat;"))


(define date-re
  ;match `month year' or `month day, year'.
  ;subpattern matches day, if present
  (pregexp "([a-z]+) +([0-9]+,)? *([0-9]+)"))

(test-equal
  (pregexp-match date-re "jan 1, 1970")
  '("jan 1, 1970" "jan" "1," "1970"))
(test-equal
  (pregexp-match date-re "jan 1970")
  '("jan 1970" "jan" #f "1970"))
(test-equal
  (pregexp-replace "_(.+?)_"
                   "the _nina_, the _pinta_, and the _santa maria_"
                   "*\\1*")
  "the *nina*, the _pinta_, and the _santa maria_")
(test-equal
  (pregexp-replace* "_(.+?)_"
                    "the _nina_, the _pinta_, and the _santa maria_"
                    "*\\1*")
  "the *nina*, the *pinta*, and the *santa maria*")
(test-equal
  (pregexp-replace "(\\S+) (\\S+) (\\S+)"
                   "eat to live"
                   "\\3 \\2 \\1")
  "live to eat")
(test-equal
  (pregexp-match "([a-z]+) and \\1"
                 "billions and billions")
  '("billions and billions" "billions"))
(test-equal
  (pregexp-match "([a-z]+) and \\1"
                 "billions and millions")
  #f)
(test-equal
  (pregexp-replace* "(\\S+) \\1"
                    "now is the the time for all good men to to come to the aid of of the party"
                    "\\1")
  "now is the time for all good men to come to the aid of the party")
(test-equal
  (pregexp-replace* "(\\d+)\\1"
                    "123340983242432420980980234"
                    "{\\1,\\1}")
  "12{3,3}40983{24,24}3242{098,098}0234")
(test-equal
  (pregexp-match "^(?:[a-z]*/)*([a-z]+)$" "/usr/local/bin/mzscheme")
  '("/usr/local/bin/mzscheme" "mzscheme"))
(test-equal
  (pregexp-match "(?i:hearth)" "HeartH")
  '("HeartH"))
(test-equal
  (pregexp-match "(?x: a   lot)" "alot")
  '("alot"))
(test-equal
  (pregexp-match "(?x: a  \\  lot)" "a lot")
  '("a lot"))
(test-equal
  (pregexp-match "(?x:
                    a \\ man  \\; \\   ; ignore
                    a \\ plan \\; \\   ; me
                    a \\ canal         ; completely
                    )"
                 "a man; a plan; a canal")
  '("a man; a plan; a canal"))
(test-equal
  (pregexp-match "(?ix:
                    a \\ man  \\; \\   ; ignore
                    a \\ plan \\; \\   ; me
                    a \\ canal         ; completely
                    )"
                 "A Man; a Plan; a Canal")
  '("A Man; a Plan; a Canal"))
(test-equal
  (pregexp-match "(?i:the (?-i:TeX)book)"
                 "The TeXbook")
  '("The TeXbook"))
(test-equal
  (pregexp-match "f(ee|i|o|um)" "a small, final fee")
  '("fi" "i"))
(test-equal
  (pregexp-replace* "([yi])s(e[sdr]?|ing|ation)"
                    "it is energising to analyse an organisation pulsing with noisy organisms"
                    "\\1z\\2")
  "it is energizing to analyze an organization pulsing with noisy organisms")
(test-equal
  (pregexp-match "f(?:ee|i|o|um)" "fun for all")
  '("fo"))
(test-equal
  (pregexp-match "call|call-with-current-continuation"
                 "call-with-current-continuation")
  '("call"))
(test-equal
  (pregexp-match "call-with-current-continuation|call"
                 "call-with-current-continuation")
  '("call-with-current-continuation"))
(test-equal
  (pregexp-match "(?:call|call-with-current-continuation) constrained"
                 "call-with-current-continuation constrained")
  '("call-with-current-continuation constrained"))
(test-equal
  (pregexp-match "(?>a+)." "aaaa")
  #f)
(test-equal
  (pregexp-match-positions "grey(?=hound)"
                           "i left my grey socks at the greyhound")
  '((28 . 32)))
(test-equal
  (pregexp-match-positions "grey(?!hound)"
                           "the gray greyhound ate the grey socks")
  '((27 . 31)))
(test-equal
  (pregexp-match-positions "(?<=grey)hound"
                           "the hound in the picture is not a greyhound")
  '((38 . 43)))
(test-equal
  (pregexp-match-positions "(?<!grey)hound"
                           "the greyhound in the picture is not a hound")
  '((38 . 43)))

(define n0-255
  "(?x:
     \\d          ;  0 through   9
     | \\d\\d     ; 00 through  99
     | [01]\\d\\d ;000 through 199
     | 2[0-4]\\d  ;200 through 249
     | 25[0-5]    ;250 through 255
     )")

(define ip-re1
  (string-append
    "^"        ;nothing before
    n0-255     ;the first n0-255,
    "(?x:"     ;then the subpattern of
    "\\."      ;a dot followed by
    n0-255     ;an n0-255,
    ")"        ;which is
    "{3}"      ;repeated exactly 3 times
    "$"        ;with nothing following
    ))

(test-equal
  (pregexp-match ip-re1
                 "1.2.3.4")
  '("1.2.3.4"))
(test-equal
  (pregexp-match ip-re1
                 "55.155.255.265")
  #f)
(test-equal
  (pregexp-match ip-re1
                 "0.00.000.00")
  '("0.00.000.00"))

(define ip-re
  (string-append
    "(?=[1-9])" ;ensure there's a non-0 digit
    ip-re1))

(test-equal
  (pregexp-match ip-re
                 "1.2.3.4")
  '("1.2.3.4"))
(test-equal
  (pregexp-match ip-re
                 "0.0.0.0")
  #f)

(set! ip-re
  (string-append
    "(?![0.]*$)" ;not just zeros and dots
    ;dot is not metachar inside []
    ip-re1))

(test-equal
  (pregexp-match ip-re
                 "1.2.3.4")
  '("1.2.3.4"))
(test-equal
  (pregexp-match ip-re
                 "0.0.0.0")
  #f)

;misc
(test-equal
  (pregexp-match "a[^a]*b" "glauber")
  '("aub"))
(test-equal
  (pregexp-match "a([^a]*)b" "glauber")
  '("aub" "u"))
(test-equal
  (pregexp-match "a([^a]*)b" "ababababab")
  '("ab" ""))
(test-equal
  (pregexp-match "(?x: s  e  * k )" "seeeeek")
  '("seeeeek"))
(test-equal
  (pregexp-match "(?x: t  ;matches t
                       h          ;   matches h
                       e           ;;;   matches e
                       \\              ; ; ; matches space
                       \\;          ;  matches ;
                       )"
                 "the ;")
  '("the ;"))
(test-equal
  (pregexp-replace* "^(.*)$" "foobar" "\\1abc")
  "foobarabc")
(test-equal
  (pregexp-replace* "^(.*)$" "foobar" "abc\\1")
  "abcfoobar")
(test-equal
  (pregexp-replace* "(.*)$" "foobar" "abc\\1")
  "abcfoobar")

;PLT bug 6095 from Neil W. Van Dyke
(test-equal
  (pregexp "[a-z-]")
  '(:sub (:or (:seq (:one-of-chars (:char-range #\a #\z) #\-)))))
(test-equal
  (pregexp "[-a-z]")
  '(:sub (:or (:seq (:one-of-chars #\- (:char-range #\a #\z))))))

;PLT bug 6442 from David T. Pierson
(test-equal
  (pregexp-match-positions "(a(b))?c" "abc")
  '((0 . 3) (0 . 2) (1 . 2)))
(test-equal
  (pregexp-match-positions "(a(b))?c" "c")
  '((0 . 1) #f #f))

;PLT bug 7233 from Edi Weitz
(test-equal
  (length (pregexp-match "(a)|(b)" "b"))
  3)

;PLT bug 7232 from Neil Van Dyke
(test-equal
  (pregexp "[-a]")
  '(:sub (:or (:seq (:one-of-chars #\- #\a)))))
(test-equal
  (pregexp "[a-]")
  '(:sub (:or (:seq (:one-of-chars #\a #\-)))))

(test-end)
