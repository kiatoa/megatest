
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp rpc s11n)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo zmq md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

(define (server:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "tcp://" (car hostport) ":" (cadr hostport))))

(define (server:run hostn)
  (debug:print 0 "Attempting to start the server ...")
  (if (not *toppath*)(setup-for-run))
  (let* ((zmq-socket     #f)
	 (hostname       (if (string=? "-" hostn)
			     (get-host-name) 
			     hostn))
	 (ipaddrstr      (let ((ipstr (if (string=? "-" hostn)
					  (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					  #f)))
			   (if ipstr ipstr hostname))))
    (set! zmq-socket (server:find-free-port-and-open ipaddrstr zmq-socket 5555 0))
    (set! *cache-on* #t)
    
    ;; what to do when we quit
    ;;
    (on-exit (lambda ()
	       (open-run-close tasks:server-deregister-self tasks:open-db #f)
	       (let loop () 
		 (let ((queue-len 0))
		   (thread-sleep! (random 5))
		   (mutex-lock! *incoming-mutex*)
		   (set! queue-len (length *incoming-data*))
		   (mutex-unlock! *incoming-mutex*)
		   (if (> queue-len 0)
		       (begin
			 (debug:print-info 0 "Queue not flushed, waiting ...")
			 (loop)))))))

    ;; The heavy lifting
    ;;
    (let loop ()
      (let* ((rawmsg (receive-message* zmq-socket))
	     (params (db:string->obj rawmsg)) ;; (with-input-from-string rawmsg (lambda ()(deserialize))))
	     (res    #f))
	(debug:print-info 12 "server=> received params=" params)
	(set! res (cdb:cached-access params))
	(debug:print-info 12 "server=> processed res=" res)
	(send-message zmq-socket (db:obj->string res))
	(if (not *time-to-exit*)
	    (loop)
	    (begin
	      (open-run-close tasks:server-deregister-self tasks:open-db #f)
	      (db:write-cached-data)
	      (exit)
	      ))))))

;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 3) ;; no need to do this very often
    (db:write-cached-data)
    ;; (print "Server running, count is " count)
    (if (< count 2) ;; 3x3 = 9 secs aprox
	(loop (+ count 1))
	(let ((numrunning (open-run-close db:get-count-tests-running #f)))
	  (open-run-close tasks:server-update-heartbeat  tasks:open-db *server-id*)
	  (if (or (> numrunning 0) ;; stay alive for two days after last access
		  (> (+ *last-db-access* (* 48 60 60))(current-seconds)))
	      (begin
		(debug:print-info 2 "Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
		(loop 0))
	      (begin
		(debug:print-info 0 "Starting to shutdown the server.")
		;; need to delete only *my* server entry (future use)
		(set! *time-to-exit* #t)
		(open-run-close tasks:server-deregister-self tasks:open-db)
		(thread-sleep! 1)
		(debug:print-info 0 "Max cached queries was " *max-cache-size*)
		(debug:print-info 0 "Server shutdown complete. Exiting")
		(exit)))))))

(define (server:find-free-port-and-open host s port #!key (trynum 50))
  (let ((s (if s s (make-socket 'rep)))
	(p (if (number? port) port 5555)))
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "Failed to bind to port " p ", trying next port")
       (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
       (if (> trynum 0)
	   (server:find-free-port-and-open host s (+ p 1) trynum: (- trynum 1))
	   (debug:print-info 0 "Tried ports from " (- p trynum) " to " p 
			     " but all were in use. Please try a different port range by starting the server with parameter \" -port N\" where N is the starting port number to use")))
     (let ((zmq-url (conc "tcp://" host ":" p)))
       (print "Trying to start server on " zmq-url)
       (bind-socket s zmq-url)
       (set! *runremote* #f)
       (debug:print 0 "Server started on " zmq-url)
       (set! *server-id* (open-run-close tasks:server-register tasks:open-db (current-process-id) host p 0 'live))
       s))))

(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

(define (server:get-client-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; 
(define (server:client-connect host port #!key (context #f))
  (debug:print 3 "client-connect " host ":" port)
  (let ((connect-ok #f)
	(zmq-socket (if context 
			(make-socket 'req context)
			(make-socket 'req)))
	(conurl     (server:make-server-url (list host port))))
    (if (socket? zmq-socket)
	(begin
	  (connect-socket zmq-socket conurl)
	  zmq-socket)
	#f)))
  

(define (server:client-login zmq-socket)
  (cdb:login zmq-socket *toppath* (server:get-client-signature)))

(define (server:client-logout zmq-socket)
  (let ((ok (and (socket? zmq-socket)
		 (cdb:logout zmq-socket *toppath* (server:get-client-signature)))))
    ;; (close-socket zmq-socket)
    ok))

;; Do all the connection work, start a server if not already running
(define (server:client-setup #!key (numtries 10)(do-ping #f))
  (if (not *toppath*)(setup-for-run))
  (let ((hostinfo   (open-run-close tasks:get-best-server tasks:open-db do-ping: do-ping)))
    (if hostinfo
	(let ((host    (car hostinfo))
	      (port    (cadr hostinfo)))
	  ;; (zsocket (caddr hostinfo)))
	;; (set! *runremote* zsocket))
	  (let* ((host       (car hostinfo))
		 (port       (cadr hostinfo)))
	    (debug:print-info 2 "Setting up to connect to " hostinfo)
	    (handle-exceptions
	     exn
	     (begin
	       (debug:print 0 "ERROR: Failed to open a connection to the server at: " hostinfo)
	       (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
	       (debug:print 0 "   perhaps jobs killed with -9? Removing server records")
	       (open-run-close tasks:server-deregister tasks:open-db host port: port)
	       #f)
	     (let* ((zmq-socket (server:client-connect host port))
		    (login-res  (server:client-login zmq-socket))
		    (connect-ok (if (null? login-res) #f (car login-res)))
		    (conurl     (server:make-server-url hostinfo)))
	       (if connect-ok
		   (begin
		     (debug:print-info 2 "Logged in and connected to " conurl)
		     (set! *runremote* zmq-socket)
		     #t)
		   (begin
		     (debug:print-info 2 "Failed to login or connect to " conurl)
		     (set! *runremote* #f)
		     #f))))))
	(if (> numtries 0)
	    (let ((exe (car (argv))))
	      (debug:print-info 1 "No server available, attempting to start one...")
	      (process-run exe (list "-server" "-" "-debug" (conc *verbosity*)))
	      (sleep 2)
	      ;; not doing ping, assume the server started and registered itself
	      (server:client-setup numtries: (- numtries 1) do-ping: #f))
	    (debug:print-info 1 "Too many retries, giving up")))))

(define (server:launch)
  (let* ((toppath (setup-for-run)))
    (debug:print-info 0 "Starting the standalone server")
    (let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
      (if hostinfo
	  (debug:print-info 1 "NOT starting new server, one is already running on " (car hostinfo) ":" (cadr hostinfo))
	  (if *toppath* 
	      (let* ((th2 (make-thread (lambda ()
					 (server:run (args:get-arg "-server")))))
		     (th3 (make-thread (lambda ()
					 (server:keep-running)))))
		(thread-start! th2)
		(thread-start! th3)
		(set! *didsomething* #t)
		(thread-join! th3))
	      (debug:print 0 "ERROR: Failed to setup for megatest"))))))

(define (server:client-launch #!key (do-ping #f))
  (if (server:client-setup do-ping: do-ping)
      (debug:print-info 2 "connected as client")
      (begin
	(debug:print 0 "ERROR: Failed to connect as client")
	(exit))))

;; ping a server and return number of clients or #f (if no response)
(define (server:ping host port #!key (secs 10)(return-socket #f))
  (cdb:use-non-blocking-mode
   (lambda ()
     (let* ((res #f)
	    (th1 (make-thread
		  (lambda ()
		    (let* ((zmq-context (make-context 1))
			   (zmq-socket  (server:client-connect host port context: zmq-context)))
		      (if zmq-socket
			  (if (server:client-login zmq-socket)
			      (let ((numclients (cdb:num-clients zmq-socket)))
				(if (not return-socket)
				    (begin
				      (server:client-logout zmq-socket)
				      (close-socket  zmq-socket)))
				(set! res (list #t numclients (if return-socket zmq-socket #f))))
			      (begin
				;; (close-socket zmq-socket)
				(set! res (list #f "CAN'T LOGIN" #f))))
			  (set! res (list #f "CAN'T CONNECT" #f)))))))
	    (th2 (make-thread
		  (lambda ()
		    (let loop ((count 1))
		      (debug:print-info 1 "Ping " count " server on " host " at port " port)
		      (thread-sleep! 2)
		      (if (< count (/ secs 2))
			  (loop (+ count 1))))
		    ;; (thread-terminate! th1)
		    (set! res (list #f "TIMED OUT" #f))))))
       (thread-start! th2)
       (thread-start! th1)
       (handle-exceptions
	exn
	(set! res (list #f "TIMED OUT" #f))
	(thread-join! th1 secs))
       res))))