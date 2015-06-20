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
(use nanomsg posix regex message-digest md5)

(define req   (nn-socket 'req))

;; get needed stuff from commandline
;;
(define hostport #f)
(define cmd '())

(let ((args (argv)))
  (if (> (length args) 2)
      (begin
	(set! hostport (cadr args))
	(set! cmd      (cddr args)))
      (begin
	(print "Usage: queuefeeder host:port command params ....")
	(exit))))

(nn-connect req  (conc "tcp://" hostport)) ;; xena:22022")

(define (client-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))

;; Generate a unique signature for this client location
;;
(define (make-signature)
  (message-digest-string (md5-primitive) 
                         (with-output-to-string
                           (lambda ()
                             (write (current-directory))))))

;; (define ((talk-to-server soc))
;;   (let loop ((cnt 200000))
;;     (let ((name (list-ref '("Matt" "Tom" "Bob" "Jill" "James" "Jane")(random 6))))
;;       ;; (print "Sending " name)
;;       ;; (print
;;       (client-send-receive req name) ;; )
;;       (if (> cnt 0)(loop (- cnt 1)))))
;;   (print (client-send-receive req "quit"))
;;   (nn-close req)
;;   (exit))
;; 

(define (get-delay signature)
  (let* ((full-msg   (client-send-receive req (conc (current-user-name) "@" (get-host-name) ":" signature))))
    (print "Got " full-msg)
    (let* ((reply-msg  (string-match "^([\\d\\.]+)\\s+(.*)$" full-msg))
	   (delay-time (if (> (length reply-msg) 2)
			   (string->number (cadr reply-msg))
			   1)) ;; fall back to one sec delay
	   (msg        (if (> (length reply-msg) 2)
			   (caddr reply-msg)
			   full-msg)))
      (values delay-time msg))))


(let ((signature (make-signature)))
  
  (thread-start! (lambda ()
		   (thread-sleep! 60)
		   (print "Give up on waiting for the server")
		   (nn-close req)
		   ;; (exit)
		   ))
  (thread-join! (thread-start! (lambda ()
				 (let-values 
				  (((delay-time msg)(get-delay signature)))
				  (print "INFO: sleeping " delay-time " seconds per request of queuefeeder server")
				  (thread-sleep! delay-time)
				  (print "INFO: done waiting, now executing requested task."))))))

(process-execute (car cmd) (cdr cmd))


