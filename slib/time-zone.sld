;;;; "timezone.scm" Compute timezones and DST from TZ environment variable.
;;; Copyright (C) 1994, 1996, 1997 Aubrey Jaffer
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

;; The C-library support for time in general and time-zones in particular
;; stands as a fine example of how *not* to create interfaces.
;;
;; Functions are not consistently named.  Support for GMT is offered in one
;; direction only; The localtime function returns some timezone data in the
;; structure which it returns, and some data in shared global variables.
;; The structure which localtime returns is overwritten with each
;; invocation.  There is no way to find local time in zones other than GMT
;; and the local timezone.
;;
;; The tzfile(5) format encodes only a single timezone per file.  There is
;; no dispatch on zone names, so multiple copies of a timezone file exist
;; under different names.  The TZ `:' specification is unix filesystem
;; specific.  The tzfile(5) format makes no provision for byte-order
;; differences; It mixes 32-bit integer data with characters; specifying
;; ASCII bytes, it is incompatible with different character sizes.  The
;; binary format makes it impossible to easily inspect a file for
;; corruption.
;;
;; I have corrected most of the failings of the C-library time interface in
;; SLIB while maintaining compatability.  I wrote support for Linux
;; timezone files because on a system where TZ is not set, there is no
;; other way to reveal this information.  HP-UX appears to have a more
;; sensible arrangement; I invite you to add support for it and other
;; platforms.
;;
;; Writing this was a long, tedious, and unenlightening process.  I hope it
;; is useful.
;;
;; Sat Nov 15 00:15:33 1997  Aubrey Jaffer

(define-library
  (slib time-zone)
  (export read-tzfile
          string->transition-day-time
          string->transition-time
          string->time-offset
          string->time-zone
          time-zone)
  (import (scheme base)
          (scheme char)
          (scheme cxr)
          (scheme file)
          (slib common)
          (slib scanf)
          (slib time-core)
          (slib tzfile))

  (begin

    ;;; This definition is here so that READ-TZFILE can verify the
    ;;; existence of these files before loading tzfile.scm to actually
    ;;; read them.
    (define tzfile:vicinity (if (file-exists? "/usr/share/zoneinfo/GMT")
                              "/usr/share/zoneinfo/"
                              "/usr/lib/zoneinfo/"))

    (define (read-tzfile path)
      (define (existing path) (and (file-exists? path) path))
      (let ((realpath
              (cond ((not path)
                     (or (existing (string-append tzfile:vicinity "localtime"))
                         (existing "/etc/localtime")))
                    ((or (char-alphabetic? (string-ref path 0))
                         (char-numeric? (string-ref path 0)))
                     (string-append tzfile:vicinity path))
                    (else path))))
        (or (and (file-exists? realpath) ; returns #f if no file found, e.g. on Windows
                 (let ((zone #f))
                   (set! zone (tzfile:read realpath))
                   (and zone (list->vector (cons 'tz:file zone)))))
            (error 'read-tzfile realpath))))

    ;;; Parse Posix TZ string.

    (define (string->transition-day-time str)
      (let ((month 0) (week 0) (day #f) (junk #f))
        (or (let ((scan (scanf-read-list "J%u%s" str)))
              (if (> (length scan) 0)
                (set! day (car scan))
                (if (> (length scan) 1)
                  (set! junk (cadr scan))))
              (case (length scan)
                ((1) (and (<= 1 day 365)
                          (list #f #f day)))
                (else #f))
              (let ((scan (scanf-read-list "J%u%s" str)))
                (if (> (length scan) 0)
                  (set! day (car scan))
                  (if (> (length scan) 1)
                    (set! junk (cadr scan))))
                (case (length scan)
                  ((1) (and (<= 0 day 365)
                            (list #f #t day)))
                  (else #f)))
              (let ((scan (scanf-read-list "M%u.%u.%u%s" str)))
                (set! month (car scan)) ;; assumes three values always retrieved
                (set! week (cadr scan))
                (set! day (caddr scan)) 
                (when (= 4 (length scan)) (set! junk (cadddr scan)))
                (case (length scan)
                  ((3) (and (<= 1 month 12)
                            (<= 1 week 5)
                            (<= 0 day 6)
                            (list month week day)))
                  (else #f)))))))

    (define (string->transition-time str)
      (define (mk-transition date time)
        (let ((day (string->transition-day-time date))
              (tim (string->time-offset time)))
          (and day tim (append day (list tim)))))
      ;
      (let ((case1 (scanf-read-list "%[JM.0-9]/%[:0-9]%s" str)))
        (if (= 2 (length case1))
          (mk-transition (car case1) (cadr case1))
          (let ((case2 (scanf-read-list "%[JM.0-9]" str)))
            (if (= 1 (length case2))
              (mk-transition (car case2) "2")
              #f)))))

    (define (string->time-offset str)
      (and str (string? str) (positive? (string-length str))
           (let ((hh #f) (mm 0) (ss 0) (junk #f))
             (let ((scan (scanf-read-list "%u:%u:%u%s" (if (memv (string-ref str 0) '(#\+ #\-))
                                                         (string-copy str 1 (string-length str))
                                                         str))))
               (when (> (length scan) 0) (set! hh (car scan)))
               (when (> (length scan) 1) (set! mm (cadr scan)))
               (when (> (length scan) 2) (set! ss (caddr scan)))
               (when (> (length scan) 3) (set! junk (cadddr scan)))
               (and (<= 1 (length scan)
                        3)
                    (number? hh) (<= 0 hh 23) (<= 0 mm 59) (<= 0 ss 59)
                    (* (if (char=? #\- (string-ref str 0)) -1 1)
                       (+ ss (* 60 (+ mm (* hh 60))))))))))

    (define (string->time-zone tz)
      (let* ((case1 (scanf-read-list "%[^0-9,+-]%[-:+0-9]%[^0-9,+-]%[-:+0-9],%[JM.0-9/:],%[JM.0-9/:]%s" tz))
             (found (length case1))
             (tzname (and (>= found 1) (list-ref case1 0)))
             (offset (and (>= found 2) (string->time-offset (list-ref case1 1))))
             (dtzname (and (>= found 3) (list-ref case1 2)))
             (doffset (and (>= found 4) (string->time-offset (list-ref case1 3))))
             (start-str (and (>= found 5) (list-ref case1 4)))
             (end-str (and (>= found 6) (list-ref case1 5)))
             (junk (and (>= found 7) (list-ref case1 6))))
        (when (and offset (= 3 found))
          (set! doffset (+ -3600 offset))
          (let* ((case2 (scanf-read-list "%[^0-9,+-]%[-:+0-9]%[^0-9,+-],%[JM.0-9/:],%[JM.0-9/:]%s" tz))
                 (found2 (length case2)))
            (set! found (+ 1 (length case2)))
            (when (>= found2 1) (set! tzname (list-ref case2 0)))
            (when (>= found2 2) (set! offset (list-ref case2 1)))
            (when (>= found2 3) (set! dtzname (list-ref case2 2)))
            (when (>= found2 4) (set! start-str (list-ref case2 3)))
            (when (>= found2 5) (set! end-str (list-ref case2 4)))
            (when (>= found2 6) (set! junk (list-ref case2 5)))
            (set! offset (string->time-offset offset))))
        (case found
          ((2) (vector 'tz:fixed tz tzname offset))
          ((4) (vector 'tz:rule tz tzname dtzname offset doffset
                       (list 4 1 0 7200) (list 10 5 0 7200)))
          ((6) (let ((start (string->transition-time start-str))
                     (end   (string->transition-time   end-str)))
                 (and
                   start end
                   (vector 'tz:rule tz tzname dtzname offset doffset start end))))
          (else #f))))
    ;@
    (define (time-zone tz)
      (cond ((vector? tz) tz)
            ((or (not tz)
                 (eqv? #\: (string-ref tz 0)))
             (let ()
               (read-tzfile (and tz (string-copy tz 1 (string-length tz))))))
            (else (string->time-zone tz))))

    ))

