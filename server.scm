
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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(use zmq)

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

;; Transition to pub --> sub with pull <-- push
;;
;;   1. client sends request to server via push to the pull port
;;   2. server puts request in queue or processes immediately as appropriate
;;   3. server puts responses from completed requests into pub port 
;;
;; TODO
;;
;; Done Tested
;; [x]  [ ]    1. Add columns pullport pubport to servers table
;; [x]  [ ]    2. Add rm of monitor.db if older than 11/12/2012 
;; [x]  [ ]    3. Add create of pullport and pubport with finding of available ports
;; [ ]  [ ]    4. Add client compose of request
;; [ ]  [ ]        - name of client: testname/itempath-test_id-hostname 
;; [ ]  [ ]        - name of request: callname, params
;; [ ]  [ ]        - request key: f(clientname, callname, params)
;; [ ]  [ ]    5. Add processing of subscription hits
;; [ ]  [ ]        - done when get key 
;; [ ]  [ ]        - return results
;; [ ]  [ ]    6. Add timeout processing
;; [ ]  [ ]        - after 60 seconds
;; [ ]  [ ]            i. check server alive, connect to new if necessary
;; [ ]  [ ]           ii. resend request

(define (server:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "tcp://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

(define (server:self-ping iface port)
  (let ((zsocket (server:client-connect iface port)))
    (let loop ()
      (thread-sleep! 2)
      (cdb:client-call zsocket 'ping #t)
      (debug:print 4 "server:self-ping - I'm alive on " iface ":" port "!")
      (mutex-lock! *heartbeat-mutex*)
      (set! *server-loop-heart-beat* (current-seconds))
      (mutex-unlock! *heartbeat-mutex*)
      (loop))))
    
(define-inline (zmqsock:get-pub  dat)(vector-ref dat 0))
(define-inline (zmqsock:get-pull dat)(vector-ref dat 1))
(define-inline (zmqsock:set-pub! dat s)(vector-set! dat s 0))
(define-inline (zmqsock:set-pull! dat s)(vector-set! dat s 0))

(define (server:run hostn)
  (debug:print 0 "Attempting to start the server ...")
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, cannot start server, exiting")
	    (exit))))
  (let* ((zmq-sdat1       #f)
	 (zmq-sdat2       #f)
	 (zmq-socket1     #f)
	 (zmq-socket2     #f)
	 (p1              #f)
	 (p2              #f)
	 (zmq-sockets-dat #f)
	 (iface           (if (string=? "-" hostn)
			      "*" ;; (get-host-name) 
			      hostn))
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   #f)))
			    (if ipstr ipstr hostname))))
    (set! zmq-sockets-dat (server:setup-ports ipaddrstr (if (args:get-arg "-port")
							    (string->number (args:get-arg "-port"))
							    (+ 5000 (random 1001)))))

    (set! zmq-sdat1    (car   zmq-socket-dat))
    (set! zmq-socket1  (car   zmq-sdat1))
    (set! p1           (caddr zmq-sdat1))
    
    (set! zmq-sdat2    (cadr  zmq-socket-dat))
    (set! zmq-socket2  (car   zmq-sdat2))
    (set! p2           (caddr zmq-sdat2))

    (set! *cache-on* #t)

    ;; (set! th1 (make-thread (lambda ()
    ;;     		     (server:self-ping ipaddrstr actual-port))))
    ;; (thread-start! th1)
    
    ;; what to do when we quit
    ;;
    (on-exit (lambda ()
	       (if (and *toppath* *server-info*)
		   (begin
		     (open-run-close tasks:server-deregister-self tasks:open-db ipaddrstr p1 p2))
		   (let loop () 
		     (let ((queue-len 0))
		       (thread-sleep! (random 5))
		       (mutex-lock! *incoming-mutex*)
		       (set! queue-len (length *incoming-data*))
		       (mutex-unlock! *incoming-mutex*)
		       (if (> queue-len 0)
			   (begin
			     (debug:print-info 0 "Queue not flushed, waiting ...")
			     (loop))))))))

    ;; The heavy lifting
    ;;
    (let loop ()
      (let* ((rawmsg (receive-message* zmq-socket1))
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
	      ))))
    (thread-join! th1)))


;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 4) ;; no need to do this very often
    (db:write-cached-data)
    ;; (print "Server running, count is " count)
    (if (< count 1) ;; 3x3 = 9 secs aprox
	(loop (+ count 1))
	(let (;; (numrunning            (open-run-close db:get-count-tests-running #f))
	      (server-loop-heartbeat #f)
	      (server-info           #f)
	      (pulse                 0))
	  ;; BUG add a wait on server alive here!!
	  ;; ;; Ugly yuk. 
	  (mutex-lock! *heartbeat-mutex*)
	  (set! server-loop-heartbeat *server-loop-heart-beat*)
	  (set! server-info *server-info*)
	  (mutex-unlock! *heartbeat-mutex*)
	  ;; The logic here is that if the server loop gets stuck blocked in working
	  ;; we don't want to update our heartbeat
	  (set! pulse (- (current-seconds) server-loop-heartbeat))
	  (debug:print-info 2 "Heartbeat period is " pulse " seconds on " (cadr server-info) ":" (caddr server-info) ", last db access is " (- (current-seconds) *last-db-access*) " seconds ago")
	  (if (> pulse 15) ;; must stay less than 10 seconds 
	      (begin
		(open-run-close tasks:server-deregister tasks:open-db (cadr server-info) port: (caddr server-info))
		(debug:print 0 "ERROR: Heartbeat failed, committing servercide")
		(exit))
	      (open-run-close tasks:server-update-heartbeat tasks:open-db (car server-info)))
	  ;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
	  (if (> (+ *last-db-access* 
		    ;; (* 48 60 60)    ;; 48 hrs
		    ;; 60              ;; one minute
		    (* 60 60)       ;; one hour
		    )
		 (current-seconds))
	      (begin
		;; (debug:print-info 2 "Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
		(debug:print-info 2 "Server continuing, seconds since last db access: " (- (current-seconds) *last-db-access*))
		(loop 0))
	      (begin
		(debug:print-info 0 "Starting to shutdown the server.")
		;; need to delete only *my* server entry (future use)
		(set! *time-to-exit* #t)
		(open-run-close tasks:server-deregister-self tasks:open-db (get-host-name))
		(thread-sleep! 1)
		(debug:print-info 0 "Max cached queries was " *max-cache-size*)
		(debug:print-info 0 "Server shutdown complete. Exiting")
		(exit)))))))

(define (server:find-free-port-and-open iface s port stype #!key (trynum 50))
  (let ((s (if s s (make-socket stype)))
	(p (if (number? port) port 5555))
 	(old-handler (current-exception-handler)))
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "Failed to bind to port " p ", trying next port")
       (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
       ;; (old-handler)
       ;; (print-call-chain)
       (if (> trynum 0)
	   (server:find-free-port-and-open iface s (+ p 1) trynum: (- trynum 1))
	   (debug:print-info 0 "Tried ports up to " p 
			     " but all were in use. Please try a different port range by starting the server with parameter \" -port N\" where N is the starting port number to use"))
       (exit)) ;; To exit or not? That is the question.
     (let ((zmq-url (conc "tcp://" iface ":" p)))
       (debug:print 0 "Trying to start server on " zmq-url)
       (bind-socket s zmq-url)
       (list iface s port)))))

(define (server:setup-ports ipadrstr startport)
  (let* ((s1 (server:find-free-port-and-open ipadrstr #f startport 'pub))
	 (p1 (caddr s1))
	 (s2 (server:find-free-port-and-open ipadrstr #f (+ 1 (if p1 p1 (+ startport 1))) 'pull))
	 (p2 (caddr s2)))
    (set! *runremote* #f)
    (debug:print 0 "Server started on " ipaddrstr " ports " p1 " and p2")
    (mutex-lock! *heartbeat-mutex*)
    (set! *server-info* (open-run-close tasks:server-register tasks:open-db (current-process-id) iface p 0 'live))
    (mutex-unlock! *heartbeat-mutex*)
    (list s1 s2)))

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
(define (server:client-connect iface port #!key (context #f)(type 'req))
  (debug:print-info 3 "client-connect " iface ":" port)
  (let ((connect-ok #f)
	(zmq-socket (if context 
			(make-socket type context)
			(make-socket type)))
	(conurl     (server:make-server-url (list iface port))))
    (if (socket? zmq-socket)
	(begin
	  (connect-socket zmq-socket conurl)
	  zmq-socket)
	#f)))
  

(define (server:client-login zmq-sockets)
  (cdb:login zmq-sockets *toppath* (server:get-client-signature)))

(define (server:client-logout zmq-socket)
  (let ((ok (and (socket? zmq-socket)
		 (cdb:logout zmq-socket *toppath* (server:get-client-signature)))))
    ;; (close-socket zmq-socket)
    ok))

;; Do all the connection work, start a server if not already running
(define (server:client-setup #!key (numtries 50))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: failed to find megatest.config, exiting")
	    (exit))))
  (let ((hostinfo   (open-run-close tasks:get-best-server tasks:open-db)))
    (if hostinfo
	(let ((host     (list-ref hostinfo 0))
	      (iface    (list-ref hostinfo 1))
	      (pullport (list-ref hostinfo 2))
	      (pubport  (list-ref hostinfo 3)))
	  (debug:print-info 2 "Setting up to connect to " hostinfo)
	  (handle-exceptions
	   exn
	   (begin
	     ;; something went wrong in connecting to the server. In this scenario it is ok
	     ;; to try again
	     (debug:print 0 "ERROR: Failed to open a connection to the server at: " hostinfo)
	     (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 0 "   perhaps jobs killed with -9? Removing server records")
	     (open-run-close tasks:server-deregister tasks:open-db host port: port)
	     (server:client-setup (- numtries 1))
	     #f)
	   (let* ((push-socket (server:client-connect iface pullport 'push))
		  (sub-socket  (server:client-connect iface pubport  'sub))
		  (zmq-sockets (vector push-socket sub-socket))
		  (login-res   #f)
		  ;; (connect-ok 
		  (conurl     (server:make-server-url (list iface port))))
	     (socket-option-set! sub-socket 'subscribe  (server:get-client-signature))
	     (set! login-res (server:client-login zmq-sockets))
	     (if (and (not (null? login-res))
		      (car login-res))
		 (begin
		   (debug:print-info 2 "Logged in and connected to " conurl)
		   (set! *runremote* zmq-socket)
		   #t)
		 (begin
		   (debug:print-info 2 "Failed to login or connect to " conurl)
		   (set! *runremote* #f)
		   #f)))))
	(if (> numtries 0)
	    (let ((exe (car (argv))))
	      (debug:print-info 1 "No server available, attempting to start one...")
	      (process-run exe (list "-server" "-" "-debug" (conc *verbosity*)))
	      (sleep 5) ;; give server time to start
	      ;; we are starting a server, do not try again! That can lead to 
	      ;; recursively starting many processes!!!
	      (server:client-setup numtries: 0))
	    (debug:print-info 1 "Too many attempts, giving up")))))

;; all routes though here end in exit ...
(define (server:launch)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 1 "Starting the standalone server")
  (let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
    (if hostinfo
	(debug:print-info 1 "NOT starting new server, one is already running on " (car hostinfo) ":" (cadr hostinfo))
	(if *toppath* 
	    (let* ((th1 (make-thread (lambda ()
				       (let ((server-info #f))
					 ;; wait for the server to be online and available
					 (let loop ()
					   (debug:print-info 1 "Waiting for the server to come online before starting heartbeat")
					   (thread-sleep! 2)
					   (mutex-lock! *heartbeat-mutex*)
					   (set! server-info *server-info* )
					   (mutex-unlock! *heartbeat-mutex*)
					   (if (not server-info)(loop)))
					 (debug:print 1 "Server alive, starting self-ping")
					 (server:self-ping (cadr server-info)(caddr server-info)))) "Self ping"))
		   (th2 (make-thread (lambda ()
				       (server:run (args:get-arg "-server"))) "Server run"))
		   (th3 (make-thread (lambda ()
				       (server:keep-running)) "Keep running")))
	      (set! *client-non-blocking-mode* #t)
	      (thread-start! th1)
	      (thread-start! th2)
	      (thread-start! th3)
	      (set! *didsomething* #t)
	      (thread-join! th3))
	    (debug:print 0 "ERROR: Failed to setup for megatest")))
    (exit)))

(define (server:client-signal-handler signum)
  (handle-exceptions
   exn
   (debug:print " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     (if (not *received-response*)
				 (receive-message* *runremote*))) ;; flush out last call if applicable
			   "eat response"))
	 (th2 (make-thread (lambda ()
			     (debug:print 0 "ERROR: Received ^C, attempting clean exit.")
			     (thread-sleep! 3) ;; give the flush three seconds to do it's stuff
			     (debug:print 0 "       Done.")
			     (exit 4))
			   "exit on ^C timer")))
     (thread-start! th2)
     (thread-start! th1)
     (thread-join! th2))))

(define (server:client-launch)
  (set-signal-handler! signal/int server:client-signal-handler)
   (if (server:client-setup)
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
			  (set! res (list #f "CAN'T CONNECT" #f)))))
		  "Ping: th1"))
	    (th2 (make-thread
		  (lambda ()
		    (let loop ((count 1))
		      (debug:print-info 1 "Ping " count " server on " host " at port " port)
		      (thread-sleep! 2)
		      (if (< count (/ secs 2))
			  (loop (+ count 1))))
		    ;; (thread-terminate! th1)
		    (set! res (list #f "TIMED OUT" #f)))
		  "Ping: th2")))
       (thread-start! th2)
       (thread-start! th1)
       (handle-exceptions
	exn
	(set! res (list #f "TIMED OUT" #f))
	(thread-join! th1 secs))
       res))))
