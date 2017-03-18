;; PARTIAL IMPLEMENTATION
;; This version of srfi 64 is a wrapper around (chibi test)
;; created to run the tests in r7rs-libs.

(define-library
  (srfi 64)
  (export test-begin
          test-assert
          test-equal
          test-end)
  (import (scheme base)
          (prefix (chibi test) chibi:))

  (begin

    (define test-begin chibi:test-begin)
    (define (test-assert x)
      (chibi:test-assert x))
    (define (test-equal x y)
      (chibi:test-equal equal? x y))
    (define test-end chibi:test-end)

  ))
