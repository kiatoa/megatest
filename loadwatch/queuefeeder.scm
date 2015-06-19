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

(thread-start! (lambda ()
		 (thread-sleep! 20)
		 (print "Give up on waiting for the server")
		 (nn-close req)
		 (exit)))
(thread-join! (thread-start! (lambda ()
			       (print (client-send-receive req (conc (current-user-name) "@" (get-host-name)))))))

(process-execute (car cmd) (cdr cmd))


