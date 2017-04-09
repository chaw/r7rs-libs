;; Exploring xml parser
;; Examples from: https://www.w3schools.com/xml/xml_attributes.asp

(import (scheme base)
        (scheme write)
        (slib xml-parse))

(define *str* "<person gender=\"male\"><name>Peter</name><number>1234</number></person>")

(display (ssax:xml->sxml (open-input-string *str*) '()))
(newline)

(define *str2* "
<messages>
  <note id=\"501\">
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget me this weekend!</body>
  </note>
  <note id=\"502\">
    <to>Jani</to>
    <from>Tove</from>
    <heading>Re: Reminder</heading>
    <body>I will not</body>
  </note>
</messages>")

(display (ssax:xml->sxml (open-input-string *str2*) '()))
(newline)



