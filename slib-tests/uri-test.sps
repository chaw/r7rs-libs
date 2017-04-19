;; Some illustrative examples of using the URI library

(import (scheme base)       (scheme write)
        (slib uri)
        (srfi 64))

(test-begin "slib-uri")

(test-assert (absolute-path? "/usr/local/bin/scheme"))
(test-assert (not (absolute-path? "../filename")))

(test-assert (absolute-uri? "ftp://example.org/resource.txt"))
(test-assert (not (absolute-uri? "resource.txt")))

(test-assert (glob-pattern? "/home/dir/*/file.txt"))
(test-assert (glob-pattern? "/home/dir/x/file?.txt"))
(test-assert (glob-pattern? "/home/dir/x/fil[e].txt"))
(test-assert (not (glob-pattern? "/home/dir/x/file.txt")))

(test-equal "<A NAME=\"location\"></A>" (html:anchor "location"))
(test-equal "<BASE HREF=\"http:peterlane.info\">" (html:base "http:peterlane.info"))
(test-equal "<ISINDEX PROMPT=\"Search term: \">" (html:isindex "Search term: "))
(test-equal "<A HREF=\"http://peterlane.info\">home page</A>" (html:link "http://peterlane.info" "home page"))

(test-assert (zero? (string-length (make-uri))))
(test-equal "#xyz" (make-uri "xyz"))
(test-equal "?query#xyz" (make-uri "query" "xyz"))
(test-equal "http://peterlane.info/files/location?query#xyz" (make-uri "http" "peterlane.info" "/files/location" "query" "xyz"))

(test-equal '("username" #f "ftp.someplace.site" #f) (parse-ftp-address "ftp://username@ftp.someplace.site/"))
(test-equal '("username" "password" "ftp.someplace.site" "dir") (parse-ftp-address "ftp://username:password@ftp.someplace.site/dir"))

(test-equal "file:/usr/local/filename.txt" (path->uri "/usr/local/filename.txt"))
(test-equal '(file #f ("" "usr" "local" "filename.txt") #f #f) (uri->tree "file:/usr/local/filename.txt"))

(test-equal '((date "32") (name "XXX")) (uri:decode-query "name=XXX&date=32"))
(test-equal "a/b/c" (uri:make-path '("a" "b" "c")))

(test-equal "xxx%20yyy" (uric:encode (uric:decode "xxx%20yyy")))

(test-equal '("some" "text" "to" "split") (uri:split-fields "some text to split" #\space))

(test-end)
