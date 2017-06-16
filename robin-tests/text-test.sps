;; Test file for (robin text)

(import (scheme base)
        (scheme char)
        (robin text)
        (robin series)
        (robin srfi64-utils)
        (srfi 64))

(test-begin "robin-text")

;; tests for word-wrap
(test-equal '("abc") (word-wrap "abc" 30))
(test-equal '("abc def ghi") (word-wrap "abc def ghi" 30))
(test-equal '("abc def" "ghi") (word-wrap "abc def ghi" 8))
(test-equal '("abc" "def" "ghi") (word-wrap "abc def ghi" 4))

;; tests for words->with-commas
(test-equal "" (words->with-commas '()))
(test-equal "a" (words->with-commas '("a")))
(test-equal "a and b" (words->with-commas '("a" "b")))
(test-equal "a and b" (words->with-commas '("a" "b") #t))
(test-equal "a, b and c" (words->with-commas '("a" "b" "c")))
(test-equal "a, b and c" (words->with-commas '("a" "b" "c") #f))
(test-equal "a, b, and c" (words->with-commas '("a" "b" "c") #t))

;;; Metric tests rewritten from ruby/text gem https://github.com/threedaymonk/text/
;; tests for soundex
(test-all-equal soundex '(("Euler" . "E460")
                          ("Ellery" . "E460")
                          ("Gauss" . "G200") 
                          ("Ghosh" . "G200")
                          ("Hilbert" . "H416")
                          ("Heilbronn" . "H416")
                          ("Knuth" . "K530")
                          ("Kant" . "K530")
                          ("Lloyd" . "L300")
                          ("Ladd" . "L300")
                          ("Lukasiewicz" . "L222")
                          ("Lissajous" . "L222")
                          ("SanFrancisco" . "S516")
                          ("\"SanFrancisco\"" . "S516")
                          ("" . "") 
                          ("!$982" . "")))

;; tests for sorenson-dice-similarity
(test-approx-same 0.8 (sorenson-dice-similarity "Healed" "Sealed") 0.01)
(test-approx-same 0.55 (sorenson-dice-similarity "Healed" "Healthy") 0.01)
(test-approx-same 0.44 (sorenson-dice-similarity "Healed" "Heard") 0.01)
(test-approx-same 0.40 (sorenson-dice-similarity "Healed" "Herded") 0.01)
(test-approx-same 0.25 (sorenson-dice-similarity "Healed" "Help") 0.01)
(test-approx-same 0.0 (sorenson-dice-similarity "Healed" "Sold") 0.01)
(test-approx-same 1.0 (sorenson-dice-similarity "abcde fghi" "abcde fghi") 0.01)
(test-approx-same 10/17 (sorenson-dice-similarity "sympathize" "sympthise"))
(test-approx-same 8/9 (sorenson-dice-similarity "sympathize" "sympthise" 1))

;; tests for Porter stemming algorithm: 
;; -- using lists from https://tartarus.org/martin/PorterStemmer/
(for-each (lambda (word result)
            (test-equal result (porter-stem word)))
          (collect (scan-file "robin-tests/voc.txt"))
          (collect (scan-file "robin-tests/output.txt")))
;; must run this from parent directory
;; -- 23531 words tested

;; tests for string->n-grams
(test-equal '("") (string->n-grams "" 2))
(test-error (string->n-grams "ABCDE" 0))
(test-error (string->n-grams "ABCDE" -10))
(test-equal '("A" "B" "C" "D" "E") (string->n-grams "ABCDE" 1))
(test-equal '("AB" "BC" "CD" "DE") (string->n-grams "ABCDE" 2))
(test-equal '("ABC" "BCD" "CDE") (string->n-grams "ABCDE" 3))
(test-equal '("ABCD" "BCDE") (string->n-grams "ABCDE" 4))
(test-equal '("ABCDE") (string->n-grams "ABCDE" 5))
(test-equal '("ABCDE") (string->n-grams "ABCDE" 6))

;; tests for Daitch-Mokotoff Soundex Algorithm
;; -- check for single returns
(test-all-equal daitch-mokotoff-soundex '(("MANHEIM" . "665600")
                                          ("MINTZ" . "664000")
                                          ("TOPF" . "370000")
                                          ("AUERBACH" . "097500")
                                          ("OHRBACH" . "097500")
                                          ("LIPSHITZ" . "874400")
                                          ("LIPPSZYC" . "877440")
                                          ("LEWINSKY" . "876450")
                                          ("LEVINSKI" . "876450")
                                          ("SZLAMAWICZ" . "486740")
                                          ("SHLAMOVITZ" . "486740")))
;; -- test for multiple returns
(test-equal '("097500" "097400") (daitch-mokotoff-soundex "AUERBACH" 'all))

;; Tests for Metaphone
;; -- using list from https://github.com/threedaymonk/text/blob/master/test/data/metaphone.yml
;; but modifying result as implementation here a little different
(test-all-equal metaphone '(("ANASTHA" . "ANS0")
                            ("DAVIS-CARTER" . "TFSKRTR")
                            ("ESCARMANT" . "ESKRMNT")
                            ("MCCALL" . "MKKL")     ; double c not removed
                            ("MCCROREY" . "MKKRR")
                            ("MERSEAL" . "MRSL")
                            ("PIEURISSAINT" . "PRSNT")
                            ("ROTMAN" . "RTMN")
                            ("SCHEVEL" . "SKFL")    ; c in sch transformed to k
                            ("SCHROM" . "SKRM")
                            ("SEAL" . "SL")
                            ("SPARR" . "SPR")
                            ("STARLEPER" . "STRLPR")
                            ("THRASH" . "0RX")
                            ("LOGGING" . "LKNK")
                            ("LOGIC" . "LJK")
                            ("JUDGES" . "JJS")
                            ("SHOOS" . "XS")
                            ("SHOES" . "XS")
                            ("CHUTE" . "XT")
                            ("SCHUSS" . "SKS")      ; c in sch transformed to k
                            ("OTTO" . "OT")
                            ("ERIC" . "ERK")
                            ("BUCK" . "BK")
                            ("COCK" . "KK")
                            ("DAVE" . "TF")
                            ("CATHERINE" . "K0RN")
                            ("KATHERINE" . "K0RN")
                            ("AUBREY" . "ABR")
                            ("BRYAN" . "BRYN")
                            ("BRYCE" . "BRS")
                            ("STEVEN" . "STFN")
                            ("RICHARD" . "RXRT")
                            ("HEIDI" . "HT")
                            ("AUTO" . "AT")
                            ("MAURICE" . "MRS")
                            ("RANDY" . "RNT")
                            ("CAMBRILLO" . "KMBRL")
                            ("BRIAN" . "BRN")
                            ("RAY" . "R")
                            ("GEOFF" . "JF")
                            ("BOB" . "BB")
                            ("AHA" . "AH")
                            ("AAH" . "A")
                            ("PAUL" . "PL")
                            ("BATTLEY" . "BTL")
                            ("WROTE" . "RT")
                            ("THIS" . "0S")))

;; Edit Distances
(test-equal 3 (hamming-distance "this string" "that strong"))
(test-equal 4 (hamming-distance "This string" "that strong"))
(test-equal 3 (hamming-distance "This string" "that strong" char-ci=?))
(test-error (hamming-distance "no" "not"))
(test-equal 1 (hamming-distance '(1 2 3 4) '(1 2 3 5)))
(test-error (hamming-distance "aa" '(a a)))
(test-error (hamming-distance '(a) '(a a)))
(test-equal 1 (hamming-distance #(1 2 3 4) #(1 2 3 5)))
(test-error (hamming-distance "aa" #(a a)))
(test-error (hamming-distance #(a) '(a a)))
(test-error (hamming-distance #(a) #(a a)))
(test-equal 2 (hamming-distance #u8(1 2 3 4) #u8(0 2 3 5)))

(test-equal 0 (levenshtein-distance "sitting" "sitting"))
(test-equal 7 (levenshtein-distance "sitting" ""))
(test-equal 6 (levenshtein-distance "" "kitten"))
(test-equal 3 (levenshtein-distance "sitting" "kitten"))
(test-equal 3 (levenshtein-distance "Sunday" "Saturday"))
(test-equal 4 (levenshtein-distance "Sunday" "saturday"))
(test-equal 3 (levenshtein-distance "Sunday" "saturday" char-ci=?))

(test-equal 3 (optimal-string-alignment-distance "kitten" "sitting"))
(test-equal 3 (optimal-string-alignment-distance "this string" "that strnig"))

(test-end)



