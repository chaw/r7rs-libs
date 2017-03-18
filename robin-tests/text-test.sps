;; Test file for (robin text)

(import (scheme base)
        (robin text)
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
(test-equal "a, b and c" (words->with-commas '("a" "b" "c")))
(test-equal "a, b and c" (words->with-commas '("a" "b" "c") #f))
(test-equal "a, b, and c" (words->with-commas '("a" "b" "c") #t))

(test-end)
      

