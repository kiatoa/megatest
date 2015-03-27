;; run sim for four hours
;;
(define *end-time* (* 60 40))

;; create the cpus
;;
(let loop ((count 200))
  (add-cpu (conc "cpu_" count) 1 1)
  (if (>= count 0)(loop (- count 1))))

(draw-cpus)

;; init the queues
;;
(hash-table-set! *queues* "normal" '())
(hash-table-set! *queues* "quick"  '())
(draw-queues)

;; user k adds 200 jobs at time zero
;;
(event *start-time*
       (lambda ()
	 (let loop ((count 300)) ;; add 500 jobs
	   (add-job "normal" "k" 600 1 1)
	   (if (>= count 0)(loop (- count 1))))))

;; one minute in user m runs ten jobs
;;
(event (+ 600 *start-time*)
       (lambda ()
	 (let loop ((count 300)) ;; add 100 jobs
	   (add-job "normal" "m" 600 1 1)
	   (if (> count 0)(loop (- count 1))))))

;; every minute user j runs ten jobs
;;
(define *user-j-jobs* 500)
(event (+ 600 *start-time*)
       (lambda ()
	 (let f ()
	   (schedule 60)
	   (if (> *user-j-jobs* 0)
	       (begin
		 (let loop ((count 10)) ;; add 100 jobs
		   (add-job "quick" "j" 600 1 1)
		   (if (> count 0)(loop (- count 1))))
		 (set! *user-j-jobs* (- *user-j-jobs* 10))))
	   (if (and (not *done*)
		    (> *user-j-jobs* 0))
	       (f))))) ;; Megatest user running 200 jobs
;; ;;
;; (event *start-time*
;;        (lambda ()
;; 	 (let f ((count 200))
;; 	   (schedule 10)
;; 	   (add-job "normal" "t" 60 1 1)
;; 	   (if (and (not *done*)
;; 		    (>= count 0))
;; 	       (f (- count 1))))))

;; every 3 seconds check for available machines and launch a job
;;
(event *start-time*
       (lambda ()
	 (let f ()
	   (schedule 3)
	   (let ((queue-names (hash-table-keys *queues*)))
	     (let loop ((cpu   (get-cpu))
			(count (+ (length queue-names) 4))
			(qname (car queue-names))
			(remq  (cdr queue-names)))
	       (if (and cpu
			(> count 0))
		   (begin
		     (if (peek-job qname) ;; any jobs to do in normal queue
			 (let ((job (take-job qname)))
			   (run-job cpu job)))
		     (loop (get-cpu)
			   (- count 1)
			   (if (null? remq)
			       (car queue-names)
			       (car remq))
			   (if (null? remq)
			       (cdr queue-names)
			       (cdr remq)))))))
	   (if (not *done*)(f)))))

;; screen updates
;;
(event *start-time* (lambda ()
		      (let f ()
			(schedule 60) ;; update the screen every 60 seconds of sim time
			(draw-cpus) ;; (print "Now: " *now* " queue: " (hash-table->alist *queues*))
			(wait-for-next-draw-time)
			(if (not *done*) (f)))))


;; end the simulation
;;
(event *end-time*
       (lambda () 
	 (set! *event-list* '())
	 (set! *done* #t)))

(start)
;; (exit 0)

