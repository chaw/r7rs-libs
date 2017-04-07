(import (scheme base)
        (rebottled json-parser)
        (srfi 64))

(test-begin "JSONSelect parser")
;; simple expressions from http://jsonselect.org/#tryit
(define-syntax test-parser
  (syntax-rules ()
    ((_ str expect)
     (test-equal str expect
		 (json:parse-selector (open-input-string str))))))
(test-parser ".languagesSpoken .lang" '("languagesSpoken" >> "lang"))
(test-parser ".drinkPreference :first-child" 
	     '("drinkPreference" >> first-child))
(test-parser ".seatingPreference :nth-child(1)"
	     '("seatingPreference" >> (nth-child 1)))
(test-parser ".\"weight\"" '("weight"))
(test-parser ".lang" '("lang"))
(test-parser "string.favoriteColor" '((string "favoriteColor")))
(test-parser "string:last-child" '((string last-child)))

(test-parser "string:nth-last-child(1)" '((string (nth-last-child 1))))

(test-parser ":root" '(root))

(test-parser ":empty" '(empty))

(test-parser "number" '(number))

(test-parser ":has(:root > .preferred)" '((has (root > "preferred"))))

(test-parser ".preferred ~ .lang" '("preferred" ~ "lang"))

(test-parser ":has(.lang:val(\"Spanish\")) > .level"
	     '((has (("lang" (val "Spanish")))) > "level"))
(test-parser ".lang:val(\"Bulgarian\") ~ .level"
	     '(("lang" (val "Bulgarian")) ~ "level"))

(test-parser ".weight:expr(x<160) ~ .name .first" 
	     '(("weight" (expr (< x 160))) ~ "name" >> "first"))

(test-parser "*" '(*))

(test-parser "string, :first-child" '(or (string) (first-child)))
(test-parser "string, :first-child, array" '(or (string) (first-child) (array)))
(test-end)

