
;;======================================================================
;; Event Processing and Simulator
;;======================================================================

;; The global event list
(define *event-list* '())
(define *start-time* 0)
(define *end-time*   (* 60 60 4)) ;; four hours
(define *now*        *start-time*)
(define *done*       #f)

(define (random-sort l)
  (sort l
        (lambda (x y)
          (equal? 0 (random 2))))) 
				    
;; Each item in the event list is a list of a scheduled time and the thunk
;; (time thunk). Sort the list so that the next event is the earliest.
;;
(define event-sort
  (lambda (@a @b)
    (< (car @a)(car @b))))

(define event
  (lambda ($time $thunk)  ;; add a sort based on scheduled time here -- improve later
                          ;; to use an insert algorythm.
    (set! *event-list* (sort (cons (list $time $thunk) *event-list*) event-sort))))

(define start
  (lambda ()
    (let ((next (car *event-list*)))
      (set! *event-list* (cdr *event-list*))
      (set! *now* (car next))      
      (if (not *done*) ;; note that the second item in the list is the thunk
	  ((car (cdr next)))))))

(define pause
  (lambda ()
    (call/cc 
     (lambda (k)
       (event (lambda () (k #f)))
       (start)))))

(define schedule
  (lambda ($time)
    (call/cc 
     (lambda (k)
       (event (+ *now* $time) (lambda () (k #f)))
       (start)))))

;; (event (lambda () (let f () (pause) (display "h") (f))))

(define years->seconds
  (lambda ($yrs)
    (* $yrs 365 24 3600)))

(define weeks->seconds
  (lambda ($wks)
    (* $wks 7 24 3600)))

(define days->seconds
  (lambda ($days)
    (* $days 24 3600)))

(define months->seconds
  (lambda ($months)
    (* $months (/ 365 12) 24 3600)))

(define seconds->date
  (lambda ($seconds)
    (posix-strftime "%D" (posix-localtime (inexact->exact $seconds)))))

(define (seconds->h:m:s seconds)
  (let* ((hours   (quotient seconds 3600))
	 (rem1    (- seconds (* hours 3600)))
	 (minutes (quotient rem1 60))
	 (rem-sec (- rem1 (* minutes 60))))
    (conc hours "h " minutes "m " rem-sec "s")))