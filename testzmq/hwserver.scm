(use zmq srfi-18 posix)

(define th1 (make-thread 
	     (lambda ()
	       (let ((s (make-socket 'rep)))
		 (bind-socket s "tcp://*:5563")
		 (print "Start server...")
		 (let loop ()
		   (let* ((msg  (receive-message s))
			  (name (caddr (string-split msg " ")))
			  (resp (conc "World " name)))
		     (print "Received request: [" msg "]")
		     (thread-sleep! 0.0001)
		     (print "Sending response \"" resp "\"")
		     (send-message s resp)
		     (loop)))))))
(define th2 (make-thread
	     (lambda ()
	       (let loop ((count 0))
		 (print "count is " count)
		 (thread-sleep! 0.1)
		 (if (< count 10000)
		     (loop (+ count 1)))))))

(thread-start! th1)
(thread-start! th2)

(thread-join! th1)
