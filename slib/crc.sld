;;;; "crc.scm" Compute Cyclic Checksums
;;; Copyright (C) 1995, 1996, 1997, 2001, 2002 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Packaged for R7RS Scheme by Peter Lane, 2017

(define-library
  (slib crc)
  (export crc-32-polynomial
          crc-ccitt-polynomial
          crc-16-polynomial
          crc-12-polynomial
          crc-10-polynomial
          crc-08-polynomial
          atm-hec-polynomial
          dowcrc-polynomial
          usb-token-polynomial
          crc:make-table
          cksum
          crc16
          crc5)
  (import (scheme base)
          (scheme file)
          (slib byte))

  (cond-expand
    ((library (srfi 151))
     (only (srfi 151) arithmetic-shift bit-set? bitwise-and bitwise-xor))
    ((library (srfi 60))
     (only (srfi 60) arithmetic-shift bit-set? bitwise-and bitwise-xor))
    (else
      (error "No suitable logical bits library")))

  (begin

    ;;@ (define CRC-32-polynomial    "100000100100000010001110110110111") ; IEEE-802, FDDI
    (define crc-32-polynomial    "100000100110000010001110110110111") ; IEEE-802, AAL5
    ;@
    (define crc-ccitt-polynomial "10001000000100001")	; X25
    ;@
    (define crc-16-polynomial    "11000000000000101")	; IBM Bisync, HDLC, SDLC, USB-Data

    ;;@ (define CRC-12-polynomial    "1100000001101")
    (define crc-12-polynomial    "1100000001111")

    ;;@ (define CRC-10-polynomial   "11000110001")
    (define crc-10-polynomial   "11000110011")
    ;@
    (define crc-08-polynomial    "100000111")
    ;@
    (define atm-hec-polynomial   "100000111")
    ;@
    (define dowcrc-polynomial    "100110001")
    ;@
    (define usb-token-polynomial "100101")

    ;;This procedure is careful not to use more than DEG bits in
    ;;computing (- (expt 2 DEG) 1).  It returns #f if the integer would
    ;;be larger than the implementation supports.
    (define (crc:make-mask deg)
      (string->number (make-string deg #\1) 2))
    ;@
    (define (crc:make-table str)
      (define deg (+ -1 (string-length str)))
      (define generator (string->number (string-copy str 1 (string-length str)) 2))
      (define crctab (make-vector 256))
      (if (not (eqv? #\1 (string-ref str 0)))
        (error 'crc:make-table 'first-digit-of-polynomial-must-be-1 str))
      (if (< deg 8)
        (error 'crc:make-table 'degree-must-be>7 deg str))
      (and
        generator
        (do ((i 0 (+ 1 i))
             (deg-1-mask (crc:make-mask (+ -1 deg)))
             (gen generator
                  (if (bit-set? (+ -1 deg) gen)
                    (bitwise-xor (arithmetic-shift (bitwise-and deg-1-mask gen) 1) generator)
                    (arithmetic-shift (bitwise-and deg-1-mask gen) 1)))
             (gens '() (cons gen gens)))
          ((>= i 8) (set! gens (reverse gens))
                    (do ((crc 0 0)
                         (m 0 (+ 1 m)))
                      ((> m 255) crctab)
                      (for-each (lambda (gen i)
                                  (set! crc (if (bit-set? i m) (bitwise-xor crc gen) crc)))
                                gens '(0 1 2 3 4 5 6 7))
                      (vector-set! crctab m crc))))))

    (define crc-32-table (crc:make-table crc-32-polynomial))

    ;;@ Computes the P1003.2/D11.2 (POSIX.2) 32-bit checksum.
    (define (cksum file)
      (cond ((not crc-32-table) #f)
            ((input-port? file) (cksum-port file))
            (else (call-with-input-file file cksum-port))))

    (define cksum-port
      (let ((mask-24 (crc:make-mask 24))
            (mask-32 (crc:make-mask 32)))
        (lambda (port)
          (define crc 0)
          (define (accumulate-crc byt)
            (set! crc
              (bitwise-xor (arithmetic-shift (bitwise-and mask-24 crc) 8)
                      (vector-ref crc-32-table (bitwise-xor (arithmetic-shift crc -24) byt)))))
          (do ((byt (read-byte port) (read-byte port))
               (byte-count 0 (+ 1 byte-count)))
            ((eof-object? byt)
             (do ((byte-count byte-count (arithmetic-shift byte-count -8)))
               ((zero? byte-count) (bitwise-xor mask-32 crc))
               (accumulate-crc (bitwise-and #xff byte-count))))
            (accumulate-crc byt)))))
    ;@
    (define (crc16 file)
      (cond ((not crc-16-table) #f)
            ((input-port? file) (crc16-port file))
            (else (call-with-input-file file crc16-port))))

    (define crc-16-table (crc:make-table crc-16-polynomial))

    (define crc16-port
      (let ((mask-8 (crc:make-mask 8))
            (mask-16 (crc:make-mask 16)))
        (lambda (port)
          (define crc mask-16)
          (define (accumulate-crc byt)
            (set! crc
              (bitwise-xor (arithmetic-shift (bitwise-and mask-8 crc) 8)
                      (vector-ref crc-16-table (bitwise-xor (arithmetic-shift crc -8) byt)))))
          (do ((byt (read-byte port) (read-byte port)))
            ((eof-object? byt) (bitwise-xor mask-16 crc))
            (accumulate-crc byt)))))
    ;@
    (define (crc5 file)
      (cond ((input-port? file) (crc5-port file))
            (else (call-with-input-file file crc5-port))))

    (define (crc5-port port)
      (define generator #b00101)
      (define crc #b11111)
      (do ((byt (read-byte port) (read-byte port)))
        ((eof-object? byt) (bitwise-xor #b11111 crc))
        (do ((data byt (arithmetic-shift data 1))
             (len (+ -1 8) (+ -1 len)))
          ((negative? len))
          (set! crc
            (bitwise-and #b11111
                    (if (eqv? (bit-set? 7 data) (bit-set? 4 crc))
                      (arithmetic-shift crc 1)
                      (bitwise-xor (arithmetic-shift crc 1) generator)))))))

    ))

