(use foof-loop sql-de-lite posix)

(define beginning-2016 1451636435.0)
(define now (current-seconds))
(define one-year-ago (- now (* 365 24 60 60)))

(define db (open-database "example.db"))

(exec (sql db "CREATE TABLE IF NOT EXISTS alldat (event_time,var,val)"))
 
;; sin(time)
(with-transaction
 db
 (lambda ()
   (loop ((for m (up-from (/ one-year-ago 60) (to (/ now 60))))) ;; days of the year
	 (let ((thetime (* m 60))
	       (thehour (round (/ m 60))))
	   (let loop ((lastsec -1)
		      (sec     (random 60))
		      (count   0))
	     (if (> sec lastsec)
		 (exec (sql db "INSERT INTO alldat (event_time,var,val) VALUES (?,?,?)")
		       (+ thetime sec) ;; (* sec 60))
		       "stuff" 
		       (if (even? thehour)
			   (random 100)
			   (random 6))))
	     (if (< count 20)
		 (loop (max sec lastsec)(random 60)(+ count 1))))))))

(close-database db)


;; (with-transaction
;;  db
;;  (lambda ()
;;    (loop ((for d (up-from 0 (to 365)))) ;; days of the year
;; 	 (print "Day: " d)
;; 	 (loop ((for h (up-from 1 (to 24))))
;; 	     (loop ((for m (up-from 1 (to 60))))
;; 		   (let ((thetime (+ beginning-2016 (* 365 24 60 60)(* h 60 60)(* m 60))))
;; 		     (let loop ((lastsec -1)
;; 				(sec     (random 60))
;; 				(count   0))
;; 		       (if (> sec lastsec)
;; 			   (exec (sql db "INSERT INTO alldat (event_time,var,val) VALUES (?,?,?)")
;; 				 (+ thetime sec) ;; (* sec 60))
;; 				 "stuff" 
;; 				 (if (even? h)
;; 				     (random 100)
;; 				     (random 6))))
;; 		       (if (< count 20)
;; 			   (loop (max sec lastsec)(random 60)(+ count 1))))))))))
;; 
;; (close-database db)
