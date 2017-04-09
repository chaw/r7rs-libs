
(import (scheme base)
        (slib xml-parse)
        (srfi 64))

(define (run-test str res)
  (test-equal res
              (ssax:xml->sxml (open-input-string str) '())))

(test-begin "slib-xml-parse")

(run-test "<tag>content</tag>"
          '(*TOP* (tag "content")))

(run-test "<person gender=\"male\"><name>Peter</name><number>1234</number></person>"
          '(*TOP* (person (@ (gender "male")) (name "Peter") (number "1234"))))

(test-end)

