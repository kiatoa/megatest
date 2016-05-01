(use srfi-19)
(use test)
(use format)
(use regex)
(declare (unit inteldate))
;; utility procedures to convert among
;; different ways to express date (inteldate, seconds since epoch, isodate)
;;
;; samples:
;; isodate   -> "2016-01-01"
;; inteldate -> "16ww01.5"
;; seconds   -> 1451631600

;; procedures provided:
;; ====================
;; seconds->isodate
;; seconds->inteldate
;;
;; isodate->seconds
;; isodate->inteldate
;;
;; inteldate->seconds
;; inteldate->isodate

;; srfi-19 used extensively; this doc is better tha the eggref:
;; http://srfi.schemers.org/srfi-19/srfi-19.html

;; Author: brandon.j.barclay@intel.com 16ww18.6

(define (date->seconds date)
  (inexact->exact
   (string->number
    (date->string date "~s"))))

(define (seconds->isodate seconds)
  (let* ((date (seconds->date seconds))
         (result (date->string date "~Y-~m-~d")))
    result))

(define (isodate->seconds isodate)
  "Takes a string input of the form 'YY-MM-DD' or 'YYYY-MM-DD' and returns epoch time; for YY, assume after Y2K"
  (let* ((numlist (map string->number (string-split isodate "-")))
        (raw-year (car numlist))
        (year (if (< raw-year 100) (+ raw-year 2000) raw-year))
        (month (list-ref numlist 1))
        (day (list-ref numlist 2))
        (date (make-date 0 0 0 0 day month year))
        (seconds (date->seconds date)))

    seconds))

;; adapted from perl Intel::WorkWeek perl module
;; intel year consists of numbered weeks starting from week 1
;;   week 1 is the week containing jan 1 of the year
;;   days of week are numbered starting from 0 on sunday
;;   intel year does not match calendar year in workweek 1
;;     before jan1.
(define (seconds->inteldate-values seconds)
  (define (date-difference->seconds d1 d2)
    (- (date->seconds d1) (date->seconds d2)))

  (let* ((thisdate (seconds->date seconds))
         (thisdow (string->number (date->string thisdate "~w")))

         (year (date-year thisdate))
         ;; intel workweek 1 begins on sunday of week containing jan1
         (jan1 (make-date 0 0 0 0 1 1 year))
         (jan1dow (date-week-day jan1))
         (ww01 (date-subtract-duration jan1 (seconds->time (* 60 60 24 jan1dow))))

         (ww01_delta_seconds (date-difference->seconds thisdate ww01))
         (wwnum_initial (inexact->exact (add1 (floor (/ ww01_delta_seconds 24 3600 7) ))))
         
         ;; we could be in ww1 of next year
         (this-saturday (seconds->date
                         (+ seconds
                            (* 60 60 24 (- 6 thisdow)))))
         (this-week-ends-next-year?
          (> (date-year this-saturday) year))
         (intelyear
          (if this-week-ends-next-year?
              (add1 year)
              year))
         (intelweek
          (if this-week-ends-next-year?
              1
              wwnum_initial)))
   (values intelyear intelweek thisdow)))

(define (seconds->inteldate seconds)
  (define (string-leftpad in width pad-char)
    (let* ((unpadded-str (->string in))
           (padlen_temp (- width (string-length unpadded-str)))
           (padlen (if (< padlen_temp 0) 0 padlen_temp))
           (padding
            (fold conc ""
                  (map (lambda (x) (->string pad-char)) (iota padlen)))))
      (conc padding unpadded-str)))
  (define (zeropad num width)
    (string-leftpad num width #:0))

  (let-values (((intelyear intelweek day-of-week-num)
                (seconds->inteldate-values seconds)))
    (let ((intelyear-str
           (zeropad
            (->string
             (if (> intelyear 1999)
                 (- intelyear 2000) intelyear))
            2))
          (intelweek-str
           (zeropad (->string intelweek) 2))
          (dow-str (->string day-of-week-num)))
      (conc intelyear-str "ww" intelweek-str "." dow-str))))

(define (isodate->inteldate isodate)
  (seconds->inteldate
   (isodate->seconds isodate)))

(define (inteldate->seconds inteldate)
  (let ((match (string-match "^(\\d+)ww(\\d+).(\\d)$" inteldate)))
    (if
     (not match)
     #f
     (let* (
            (intelyear-raw (string->number (list-ref match 1)))
            (intelyear (if (< intelyear-raw 100)
                           (+ intelyear-raw 2000)
                           intelyear-raw))
            (intelww (string->number (list-ref match 2)))
            (dayofweek (string->number (list-ref match 3)))

            (day-of-seconds (* 60 60 24 ))
            (week-of-seconds (* day-of-seconds 7))
            

            ;; get seconds at ww1.0
            (new-years-date (make-date 0 0 0 0 1 1 intelyear))
            (new-years-seconds
             (date->seconds new-years-date))
            (new-years-dayofweek (date-week-day new-years-date))
            (ww1.0_seconds (- new-years-seconds
                              (* day-of-seconds
                                 new-years-dayofweek)))
            (workweek-adjustment (* week-of-seconds (sub1 intelww)))
            (weekday-adjustment (* dayofweek day-of-seconds))

            (result (+ ww1.0_seconds workweek-adjustment weekday-adjustment)))
       result))))

(define (inteldate->isodate inteldate)
  (seconds->isodate (inteldate->seconds inteldate)))

(define (inteldate-tests)
  (test-group
   "date conversion tests"
   (let ((test-table
          '(("16ww01.5" . "2016-01-01")
            ("16ww18.5" . "2016-04-29")
            ("1999ww33.5" . "1999-08-13")
            ("16ww18.4" . "2016-04-28")
            ("16ww18.3" . "2016-04-27")
            ("13ww01.0" . "2012-12-30")
            ("13ww52.6" . "2013-12-28")
            ("16ww53.3" . "2016-12-28"))))
     (for-each
      (lambda (test-pair)
        (let ((inteldate (car test-pair))
              (isodate (cdr test-pair)))
          (test
           (conc "(isodate->inteldate "isodate ") => "inteldate)
           inteldate
           (isodate->inteldate isodate))
          
          (test
           (conc "(inteldate->isodate "inteldate ")   => "isodate)
           isodate
           (inteldate->isodate inteldate))))
      test-table))))

;(inteldate-tests)
