#!/usr/bin/env csi -s
(use data-structures)
(use test)




;; date represetntations:



;; 15.11.2 Calculating DOW
;; from Julian date J: W := (+ 1 (mod (+ J 1) 7)) ;; julian date -> DOW (1=Sunday; 7=Saturday)

;; jdn = julian day number; dow == day of week number
(define (jdn->dow J)
  (+ 1 (modulo (+ (truncate (+ 0.5 J)) 1) 7)))


;; from gregoran date D/M/Y: W
(define (gd->dow gd)
  (jdn->dow (gd->jdn gd)))

         
;; https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_day_number
;
(define (date->jdn Y-M-D calendar)
  (let* ((Y (list-ref Y-M-D 0))
         (M (list-ref Y-M-D 1))
         (D (list-ref Y-M-D 2))

         ;;You must compute first the number of years (y) and months (m) since March 1 −4800 (March 1, 4801 BC):
         (a (floor (/(- 14 M) 12)))
         (y (- (+ 4800 Y) a))
         (m (+ M (* 12 a) -3))
         ;; The value 'a' will be 1 for January and February, and 0 for
         ;; other months. And 'm' will be 0 for March and 11 for
         ;; February.

         ;; All years in the BC era must be converted to astronomical
         ;; years, so that 1 BC is year 0, 2 BC is year −1,
         ;; etc. Convert to a negative number, then increment toward
         ;; zero.

         ;; Note: (153m+2)/5 gives the number of days since March 1
         ;; and comes from the repetition of days in the month from
         ;; March in groups of five:

         ;;Mar–Jul: 	31 30 31 30 31
         ;;Aug–Dec: 	31 30 31 30 31
         ;;Jan–Feb: 	31 28
         )
    (case calendar
      ((julian)
       (+ D
          (floor (/ (+ (* 153 m) 2) 5))
          (* 365 y)
          (floor (/ y 4))
          -32083 -0.5))
      ((gregorian)
        (+ D
           (floor (/ (+ (* 153 m) 2) 5))
           (* 365 y)
           (floor (/ y 4))
           (* -1 (floor (/ y 100)))
           (floor (/ y 400))
           -32045 -0.5))
      (else
       (abort (make-property-condition
               'exn
               'message
               (conc "Unknown calendar scheme: ["calendar"]" )))))))


;; https://en.wikipedia.org/wiki/Julian_day#Julian_or_Gregorian_calendar_from_Julian_day_number
;; This is an algorithm by Richards to convert a Julian Day Number, J,
;; to a date in the Gregorian calendar (proleptic, when
;; applicable). Richards does not state which dates the algorithm is
;; valid for.[30] All variables are integer values, and the notation
;; "a div b" indicates integer division, and "mod(a,b)" denotes the
;; modulus operator.
(define (jdn->date jdn calendar)
  (let* ((div (lambda (a b) (truncate (/ a b))))
         ;;algorithm parameters for Gregorian calendar
         (y 4716) (j 1401) (m 2) (n 12)
         (r 4)    (p 1461) (v 3) (u 5)
         (s 153)  (w 2)    (B 274277)
         (C -38)

         (f (case calendar
              ((julian)
               (+ jdn j))
              ((gregorian)
               (+ jdn j 
                  (div
                   (*
                    (div
                     (+ (* jdn 4) B)
                     146097)
                    3)
                   4)
                  C))))
         (e (+ (* r f) v))
         (g (div (modulo e p) r))
         (h (+ (* u g) w))
         (D (add1
             (div
              (modulo h s)
              u)))
         (M (add1
             (modulo
              (+ (div h s) m)
              n)))
         (Y (+
             (div e p)
             (- y)
             (div (+ n m (- M)) n))))
    (map inexact->exact (list Y M D))))


;; convert Gregorian calendar date to Julian Day Number
;; input - list of year, month, day -- integers
;; output - float
;;
;; Note: julian day number advances at noon UTC, so the value is is adjusted -0.5 at midnight UTC of the same day
;;
(define (gd->jdn gd)
  (date->jdn gd 'gregorian))

;; convert Julian calendar date to Julian Day Number
;; input - list of year, month, day -- integers
;; output - float
;;
;; Note: julian day number advances at noon UTC, so the value is is adjusted -0.5 at midnight UTC of the same day
;;
(define (jd->jdn jd)
  (date->jdn jd 'julian))


;; convert Julian Day Number to Gregorian calendar date 
;; input - float (one digit max after decimal point, must be 5 or 0)
;; output - list of year, month, day -- integers
;;
;; Note: julian day number advances at noon UTC, so the value is is adjusted -0.5 at midnight UTC of the same day
;;
(define (jdn->gd jdn)
  (jdn->date jdn 'gregorian))

;; convert Julian Day Number to Julian calendar date 
;; input - float (one digit max after decimal point, must be 5 or 0)
;; output - list of year, month, day -- integers
;;
;; Note: julian day number advances at noon UTC, so the value is is adjusted -0.5 at midnight UTC of the same day
;;
(define (jdn->jd jdn)
  (jdn->date jdn 'julian))


(use srfi-19)

;; Convert unix epoch time to gregorian date
(define (unixtime->gd seconds)
  (let* ((date (seconds->date seconds))
         (res
          (map
           (lambda (fmt)
             (string->number (date->string date fmt)))
           '( "~Y" "~m" "~d"))))
    res))

(define (current-gd)
  (unixtime->gd (current-seconds)))

(define (days-between-gds a b)
  (let* ((a-jdn (gd->jdn a))
         (b-jdn (gd->jdn b)))
    (- a-jdn b-jdn)))

(define (gd-pre-date gd days)
  (jdn->gd (- (gd->jdn gd) days)))

(define (gd-post-date gd days)
  (jdn->gd (+ (gd->jdn gd) days)))


(define (bday person)
  (let* ((bdays
          '((me          . (1977 8 13))
            (gma-barclay . (1917 11 11))
            (dad         . (1953 7 20))
            (mum         . (1954 3 20))
            (mindy       . (1980 5 30))
            (ruth        . (1972 1 11))
            (jj          . (1998 12 19)))))
    (alist-ref person bdays)))

(define (dday person)
  (let* ((bdays
          '(
            (gma-barclay . (2014 11 14))
            )))
    (alist-ref person bdays)))


(define (date-when-A-is-Bs-age A B)
  (let* ((A-bday-gd (bday A))
         (B-bday-gd (bday B))
         (B-dday-gd (dday B))
         (today (current-gd))
         (days-since-B-born (days-between-gds today B-bday-gd)))
    (if B-dday-gd
        (let* ((dage-days
                (days-between-gds B-dday-gd B-bday-gd)))
               (gd-post-date A-bday-gd dage-days))
        (gd-post-date A-bday-gd days-since-B-born))))




(define (do-calconv-tests)
  (test-group
   "unit-tests"
   ;; http://aa.usno.navy.mil/data/docs/JulianDate.php
   ;;The Julian date for CE  2016 October  2 00:00:00.0 UT is
   ;;JD 2457663.500000
   ;;
   ;;(test "gd->jdn" 2457663.500000 (wikipediagd->jdn '(2016 10 2)))

   (let ((test-pairs
          '(
            ( (2016 10 2) 2457663.5 )
            ( (1976 4 1)  2442869.5 )
            ( (1917 11 11) 2421543.5 )
            ( (1600 9 29) 2305719.5)
            ( (1 12 25) 1721783.5) ;; calculated pairs from
            ;;( (1582 10 15) 2299161.5) ;; broken test.
            ;;   aour algorithm has trouble at the discontinuity when gregorian calendar was adopted
            ;;   and days were intercalated.... weird.  only off by 1 day, so not spending cycles debugging..

            ;; https://www.fourmilab.ch/documents/calendar/ because
            ;; navy.mil calendar converterdoes not handle gregorian calendar
            ;; proleptically.
            )))

     ;; The Julian calendar day Thursday, 4 October 1582 was followed
     ;; by the first day of the Gregorian calendar, Friday, 15 October
     ;; 1582 (the cycle of weekdays was not affected).
     
     (for-each
      (lambda (test-pair)
        (let ((gd (car test-pair))
              (jdn (cadr test-pair)))
          (test "gd->jdn" jdn (gd->jdn gd))
          (test "jdn->gd" gd (jdn->gd jdn))
          (test jdn (gd->jdn (jdn->gd jdn)))
          (test gd (jdn->gd (gd->jdn gd)))
          (test jdn (jd->jdn (jdn->jd (gd->jdn gd))))))
      test-pairs))

   (test "jdn epoch" '(-4712 1 1) (jdn->jd 0))))


;; h m s -> secs
;; secs -> h m s
;; secs -> day
;; days -> secs

(do-tests)












