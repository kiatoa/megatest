;;======================================================================
;; Copyright 2015-2015, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;; Queue Feeder. Use a crude droop curve to limit feeding jobs into a queue
;;               to prevent slamming the queue

;;======================================================================
;; Methodology
;;
;;   Connect to the server, the server delays the appropriate time (if 
;;   any) and then launch the task.
;;

(use nanomsg posix regex)

;; (use trace)
;; (trace nn-bind nn-socket nn-assert nn-recv nn-send thread-terminate! nn-close )

(define port  22022)

;; get needed stuff from commandline
;;
(define cmd '()) ;; cmd is run to give a count of the queue length => returns number in queue

(let ((args (argv)))
  (if (> (length args) 2)
      (begin
	(set! port (cadr args))
	(set! cmd  (caddr args))) ;; no params supported
      (begin
	(print "Usage: queuefeeder-server port command")
	(print "       where the command gives an integer on stdout indicating the queue load")
	(exit))))

(print "Running queue feeder with port=" port ", command=" cmd)

(define rep   (nn-socket 'rep))

(print "connecting, got: " (nn-bind    rep  (conc "tcp://" "*" ":" port)))

(define *current-delay* 0)

(define (server soc)
  (print "server starting")
  (let loop ((msg-in (nn-recv soc))
	     (count  0))
    (if (eq? 0 (modulo count 1000))
	(print "server received: " msg-in ", count=" count))
    (cond
     ((equal? msg-in "quit")
      (nn-send soc "Ok, quitting"))
     ((and (>= (string-length msg-in) 4)
	   (equal? (substring msg-in 0 4) "ping"))
      (nn-send soc (conc (current-process-id)))
      (loop (nn-recv soc)(+ count 1)))
     (else
      (mutex-lock! *current-delay-mutex*)
      (let ((current-delay *current-delay*))
	(mutex-unlock! *current-delay-mutex*)
	(thread-sleep! current-delay)
	(nn-send soc (conc "hello " msg-in " you waited " current-delay " seconds"))
	(loop (nn-recv soc)(if (> count 20000000)
			       0
			       (+ count 1))))))))

(define (ping-self host port #!key (return-socket #t))
  ;; send a random number along with pid and check that we get it back
  (let* ((req     (nn-socket 'req))
	 (key     "ping")
	 (success #f)
	 (keepwaiting #t)
	 (ping    (make-thread
		   (lambda ()
		     (print "ping: sending string \"" key "\", expecting " (current-process-id))
		     (nn-send req key)
		     (let ((result  (nn-recv req)))
		       (if (equal? (conc (current-process-id)) result)
			   (begin
			     (print "ping, success: received \"" result "\"")
			     (set! success #t))
			   (begin
			     (print "ping, failed: received key \"" result "\"")
			     (set! keepwaiting #f)
			     (set! success #f)))))
		   "ping"))
	 (timeout (make-thread (lambda ()
				 (let loop ((count 0))
				   (thread-sleep! 1)
				   (print "still waiting after count seconds...")
				   (if (and keepwaiting (< count 10))
				       (loop (+ count 1))))
				 (if keepwaiting
				     (begin
				       (print "timeout waiting for ping")
				       (thread-terminate! ping))))
			       "timeout")))
    (nn-connect req (conc "tcp://" host ":" port))
    (handle-exceptions
     exn
     (begin
       (print-call-chain)
       (print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (print "exn=" (condition->list exn))
       (print "ping failed to connect to " host ":" port))
     (thread-start! timeout)
     (thread-start! ping)
     (thread-join! ping)
     (if success (thread-terminate! timeout)))
    (if return-socket
	(if success req #f)
	(begin
	  (nn-close req)
	  success))))

(define *current-delay-mutex* (make-mutex))

;; update the *current-delay* value every minute or QUEUE_CHK_DELAY seconds
(thread-start! (make-thread (lambda ()
			      (let ((delay-time (string->number (or (get-environment-variable "QUEUE_CHK_DELAY") "60"))))
				(let loop ()
				  (with-input-from-pipe 
				   cmd
				   (lambda ()
				     (let* ((val       (read))
					    (droop-val (if (number? val)(/ val 50) #f)))
				       ;; val is number of jobs in queue. Use a linear droop of val/50
				       (mutex-lock! *current-delay-mutex*)
				       (set! *current-delay* (/ (or droop-val 100) 50))
				       (mutex-unlock! *current-delay-mutex*)
				       (print "droop-val=" droop-val)
				       (thread-sleep! delay-time))))
				  (loop))))))

(let ((server-thread (make-thread (lambda ()(server rep)) "server")))
  (thread-start! server-thread)
  (if (ping-self (get-host-name) port)
      (begin
	(thread-join! server-thread)
	(nn-close rep))
      (print "ping failed")))

(exit)
