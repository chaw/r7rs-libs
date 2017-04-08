(import (scheme base)
        (rebottled json-select)
        (rebottled json-tools)
        (srfi 64))

(test-begin "JSON-Select")

(define json1 '#(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
                 ("favoriteColor" . "yellow")
                 ("languagesSpoken"
                  #(("lang" . "Bulgarian") ("level" . "advanced"))
                  #(("lang" . "English")
                    ("level" . "native")
                    ("preferred" . #t))
                  #(("lang" . "Spanish") ("level" . "beginner")))
                 ("seatingPreference" "window" "aisle")
                 ("drinkPreference" "whiskey" "beer" "wine")
                 ("weight" . 156)))

(define json-small
  '#(("name" . #(("first" . "Lloyd")))
     ("favoriteColor" . "yellow")
     ("languagesSpoken" 
      #(("lang" . "Bulgarian"))
      #(("lang" . "Spanish")))
     ("drinkPreference" "whiskey" "beer" "wine")))

(test-equal "*"
            '(#(("name" . #(("first" . "Lloyd")))
                ("favoriteColor" . "yellow")
                ("languagesSpoken" 
                 #(("lang" . "Bulgarian"))
                 #(("lang" . "Spanish")))
                ("drinkPreference" "whiskey" "beer" "wine"))
              ("name" . #(("first" . "Lloyd")))
              #(("first" . "Lloyd"))
              ("first" . "Lloyd")
              "Lloyd"
              ("favoriteColor" . "yellow")
              "yellow"
              ("languagesSpoken" 
               #(("lang" . "Bulgarian"))
               #(("lang" . "Spanish")))
              (#(("lang" . "Bulgarian"))
               #(("lang" . "Spanish")))
              #(("lang" . "Bulgarian"))
              ("lang" . "Bulgarian")
              "Bulgarian"
              #(("lang" . "Spanish"))
              ("lang" . "Spanish")
              "Spanish"
              ("drinkPreference" "whiskey" "beer" "wine")
              ("whiskey" "beer" "wine")
              "whiskey" "beer" "wine")
            (json:nodeset->list ((json:select "*") json-small)))

(test-equal ".languagesSpoken"
            '(("languagesSpoken"
               #(("lang" . "Bulgarian") ("level" . "advanced"))
               #(("lang" . "English")
                 ("level" . "native")
                 ("preferred" . #t))
               #(("lang" . "Spanish") ("level" . "beginner"))))
            (json:nodeset->list ((json:select ".languagesSpoken") json1)))

(test-equal ".\"weight\""
            '(("weight" . 156))
            (json:nodeset->list ((json:select ".\"weight\"") json1)))
;; TODO add more descendants...
(test-equal ".languagesSpoken .lang"
            '(("lang" . "Bulgarian")
              ("lang" . "English")
              ("lang" . "Spanish"))
            (json:nodeset->list ((json:select ".languagesSpoken .lang") json1)))

(test-equal ".languagesSpoken > .lang"
            '(("lang" . "Bulgarian")
              ("lang" . "English")
              ("lang" . "Spanish"))
            (json:nodeset->list ((json:select ".languagesSpoken > .lang")
                                 json1)))

(test-equal "number" '(156)
            (json:nodeset->list ((json:select "number") json1)))
(test-equal "array" 
            '((#(("lang" . "Bulgarian") ("level" . "advanced"))
               #(("lang" . "English")
                 ("level" . "native")
                 ("preferred" . #t))
               #(("lang" . "Spanish") ("level" . "beginner")))
              ("window" "aisle")
              ("whiskey" "beer" "wine"))
            (json:nodeset->list ((json:select "array") json1)))

(test-equal "string.favoriteColor" 
            '(("favoriteColor" . "yellow"))
            (json:nodeset->list ((json:select "string.favoriteColor") json1)))

(test-equal ".preferred ~ .lang" '(("lang" . "English"))
            (json:nodeset->list ((json:select ".preferred ~ .lang") json1)))

(test-equal ".drinkPreference :first-child" 
            '("whiskey")
            (json:nodeset->list ((json:select ".drinkPreference :first-child")
                                 json1)))
(test-equal ".drinkPreference :last-child" 
            '("wine")
            (json:nodeset->list ((json:select ".drinkPreference :last-child")
                                 json1)))

;; for some reason it starts with 1
(test-equal ".drinkPreference :nth-child(2)" 
            '("beer")
            (json:nodeset->list ((json:select ".drinkPreference :nth-child(2)")
                                 json1)))
(test-equal ".drinkPreference :nth-last-child(1)" 
            '("wine")
            (json:nodeset->list 
              ((json:select ".drinkPreference :nth-last-child(1)")
               json1)))

;; root
(test-equal ":root" 
            (list json1)
            (json:nodeset->list ((json:select ":root") json1)))
(test-equal ":root .lang" 
            '(("lang" . "Bulgarian") ("lang" . "English") ("lang" . "Spanish"))
            (json:nodeset->list ((json:select ":root .lang") json1)))
(test-equal ".lang :root" '()
            (json:nodeset->list ((json:select ".lang :root") json1)))

(define json2 '#(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
                 ;; empty array
                 ("languagesSpoken")
                 ("weight" . 156)))

(test-equal ":empty" '(()) (json:nodeset->list ((json:select ":empty") json2)))

;; or
(test-equal ".lang, .level"
            '(("lang" . "Bulgarian")
              ("lang" . "English")
              ("lang" . "Spanish")
              ("level" . "advanced")
              ("level" . "native")
              ("level" . "beginner"))
            (json:nodeset->list ((json:select ".lang, .level") json1)))

;; combinations
(test-equal "string.lang" 
            '(("lang" . "Bulgarian") ("lang" . "English") ("lang" . "Spanish"))
            (json:nodeset->list ((json:select "string.lang") json1)))
(test-equal "string:nth-child(2)"
            '("aisle" "beer")
            (json:nodeset->list ((json:select "string:nth-child(2)") json1)))

;; has
(test-equal ":has(.languagesSpoken)"
            '(#(("name" . #(("first" . "Lloyd") ("last" . "Hilaiel")))
                ("favoriteColor" . "yellow")
                ("languagesSpoken"
                 #(("lang" . "Bulgarian") ("level" . "advanced"))
                 #(("lang" . "English")
                   ("level" . "native")
                   ("preferred" . #t))
                 #(("lang" . "Spanish") ("level" . "beginner")))
                ("seatingPreference" "window" "aisle")
                ("drinkPreference" "whiskey" "beer" "wine")
                ("weight" . 156))
              ("languagesSpoken"
               #(("lang" . "Bulgarian") ("level" . "advanced"))
               #(("lang" . "English")
                 ("level" . "native")
                 ("preferred" . #t))
               #(("lang" . "Spanish") ("level" . "beginner"))))
            (json:nodeset->list ((json:select ":has(.languagesSpoken)") json1)))

;; val
(test-equal ":val(156)"
            '(("weight" . 156) 
              156)
            (json:nodeset->list ((json:select ":val(156)") json1)))

(test-equal ".lang:val(\"Bulgarian\") ~ .level"
            '(("level" . "advanced"))
            (json:nodeset->list 
              ((json:select ".lang:val(\"Bulgarian\") ~ .level") json1)))

;; contains
(test-equal ".lang:contains(\"Bul\")"
            '(("lang" . "Bulgarian")
              "Bulgarian")
            (json:nodeset->list 
              ((json:select ".lang:contains(\"Bulgarian\")") json1)))

;; expr
(test-equal ":expr(x > 150)"
            '(("weight" . 156)
              156)
            (json:nodeset->list ((json:select ":expr(x > 150)") json1)))
(test-equal ":expr(x *= \"L\")"
            '(("first" . "Lloyd")
              "Lloyd")
            (json:nodeset->list ((json:select ":expr(x *= \"L\")") json1)))

(test-equal ":expr(x *= \"L\") ~ *"
            '(("first" . "Lloyd")
              "Lloyd"
              ("last" . "Hilaiel")
              "Hilaiel")
            (json:nodeset->list ((json:select ":expr(x *= \"L\") ~ *") json1)))

(define json3 '#(("data" . #u8(1 2 3 4 5))
                 ("name" . "name")))

(test-equal "binary"
            '(#u8(1 2 3 4 5))
            (json:nodeset->list ((json:select "binary") json3)))
(test-equal "binary - data"
            '(("data" . #u8(1 2 3 4 5)))
            (json:nodeset->list ((json:select ".data") json3)))
(test-equal "binary - data"
            '(#u8(1 2 3 4 5))
            (json:nodeset->list ((json:select ".data > binary") json3)))

(test-end)

