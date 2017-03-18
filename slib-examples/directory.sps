;; Works with Chibi Scheme and (mostly) Kawa

(import (scheme base)
        (scheme write)
        (slib directory))

(display (current-directory)) (newline)
(directory-for-each 
  (lambda (filename) 
    (display (string-append "Found file: " filename "\n")))
  ".")

(make-directory "test-me")
(display "\n../slib/t*.sld\n")
(directory*-for-each 
  (lambda (filename) 
    (display (string-append "Found: " filename "\n")))
  "../slib/t*.sld")
