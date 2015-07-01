;; watch nanomsg's pipeline load-balancer in action.
(use nanomsg posix regex)

;; (use trace)
;; (trace nn-bind nn-socket nn-assert nn-recv nn-send thread-terminate! nn-close )

(define port  22022)
(define host  "127.0.0.1")

(define rep   (nn-socket 'rep))

(print "connecting, got: " (nn-bind    rep  (conc "tcp://" "*" ":" port)))

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
     ;;((and (>= (string-length msg-in)
     (else
      (let ((this-task  (/ (random 10) 200.0))
	    (start-time (current-milliseconds)))
	;; (thread-sleep! this-task)
	(nn-send soc (conc "hello " msg-in " this task took " this-task " seconds to complete"))
	;; (print "Actual send-receive time: " (- (current-milliseconds) start-time));
	(loop (nn-recv soc)(+ count 1)))))))

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

(let ((server-thread (make-thread (lambda ()(server rep)) "server")))
  (thread-start! server-thread)
  ;; (thread-sleep! 1)
  (if (ping-self host port)
      (begin
	(thread-join! server-thread)
	(nn-close rep))
      (print "ping failed")))

(exit)
