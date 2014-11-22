
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(use nanomsg)

(declare (unit nmsg-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses server))

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
;; [x]  [ ]    4. Add client compose of request
;; [x]  [ ]        - name of client: testname/itempath-test_id-hostname 
;; [x]  [ ]        - name of request: callname, params
;; [x]  [ ]        - request key: f(clientname, callname, params)
;; [x]  [ ]    5. Add processing of subscription hits
;; [x]  [ ]        - done when get key 
;; [x]  [ ]        - return results
;; [x]  [ ]    6. Add timeout processing
;; [x]  [ ]        - after 60 seconds
;; [ ]  [ ]            i. check server alive, connect to new if necessary
;; [ ]  [ ]           ii. resend request
;; [ ]  [ ]    7. Turn self ping back on

(define (nmsg-transport:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "tcp://" (car hostport) ":" (cadr hostport))))

(define *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

(define-inline (nmsgsock:get-pub  dat)(vector-ref dat 0))
(define-inline (nmsgsock:get-pull dat)(vector-ref dat 1))
(define-inline (nmsgsock:set-pub! dat s)(vector-set! dat s 0))
(define-inline (nmsgsock:set-pull! dat s)(vector-set! dat s 0))

(define (nmsg-transport:run hostn)
  (debug:print 2 "Attempting to start the server ...")
  (if (not *toppath*)
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, cannot start server, exiting")
	    (exit))))
  (let* ((db              (open-db)) ;; here we *do not* want to be opening and closing the db
	 (nmsg-sdat1       #f)
	 (nmsg-sdat2       #f)
	 (pull-socket     #f)
	 (pub-socket      #f)
	 (p1              #f)
	 (p2              #f)
	 (nmsg-sockets-dat #f)
	 (iface           (if (string=? "-" hostn)
			      "*" ;; (get-host-name) 
			      hostn))
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   #f)))
			    (if ipstr ipstr hostname)))
	 (last-run       0))
    (set! nmsg-sockets-dat (nmsg-transport:setup-ports ipaddrstr (if (args:get-arg "-port")
			    (string->number (args:get-arg "-port"))
							    (+ 5000 (random 1001)))))

    (set! nmsg-sdat1    (car   nmsg-sockets-dat))
    (set! pull-socket  (cadr  nmsg-sdat1)) ;; (iface s  port)
    (set! p1           (caddr nmsg-sdat1))
    
    (set! nmsg-sdat2    (cadr  nmsg-sockets-dat))
    (set! pub-socket   (cadr  nmsg-sdat2))
    (set! p2           (caddr nmsg-sdat2))

    (set! *cache-on* #t)

    (set! *runremote* (vector pull-socket pub-socket)) ;; overloading the use of *runremote* BUG!?

    ;; what to do when we quit
    ;;
;;     (on-exit (lambda ()
;; 	       (if (and *toppath* *server-info*)
;; 		   (open-run-close tasks:server-deregister-self tasks:open-db (car *server-info*))
;; 		   (let loop () 
;; 		     (let ((queue-len 0))
;; 		       (thread-sleep! (random 5))
;; 		       (mutex-lock! *incoming-mutex*)
;; 		       (set! queue-len (length *incoming-data*))
;; 		       (mutex-unlock! *incoming-mutex*)
;; 		       (if (> queue-len 0)
;; 			   (begin
;; 			     (debug:print-info 0 "Queue not flushed, waiting ...")
;; 			     (loop))))))))

    ;; The heavy lifting
    ;;
    ;; make-vector-record cdb packet client-sig qtype immediate query-sig params qtime
    ;;
    (debug:print-info 11 "Server setup complete, start listening for messages")
    (let loop ((queue-lst '()))
      (let* ((rawmsg (receive-message* pull-socket))
	     (packet (db:string->obj rawmsg))
	     (qtype  (cdb:packet-get-qtype packet)))
	(debug:print-info 12 "server=> received packet=" packet)
	(if (not (member qtype '(sync ping)))
	    (begin
	      (mutex-lock! *heartbeat-mutex*)
	      (set! *last-db-access* (current-seconds))
	      (mutex-unlock! *heartbeat-mutex*)))
	(if #t ;; (cdb:packet-get-immediate packet) ;; process immediately or put in queue
	    (begin
	      (db:process-queue-item db packet)
	      ;; (open-run-close db:process-queue #f pub-socket (cons packet queue-lst))
	      
	      (loop '()))
	    (loop (cons packet queue-lst)))))))

;; run nmsg-transport:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (nmsg-transport:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (let* ((server-info (let loop ()
			(let ((sdat #f))
			  (mutex-lock! *heartbeat-mutex*)
			  (set! sdat *server-info*)
			  (mutex-unlock! *heartbeat-mutex*)
			  (if sdat sdat
			      (begin
				(debug:print 12 "WARNING: server not started yet, waiting few seconds before trying again")
				(sleep 4)
				(loop))))))
	 (iface       (cadr server-info))
	 (pullport    (caddr server-info))
	 (pubport     (cadddr server-info)) ;; id interface pullport pubport)
	 ;; (nmsg-sockets (nmsg-transport:client-connect iface pullport pubport))
	 (last-access 0))
    (debug:print-info 11 "heartbeat started for nmsg server on " iface " " pullport " " pubport)
    (let loop ((count 0))
      (thread-sleep! 4) ;; no need to do this very often
      ;; NB// sync currently does NOT return queue-length
      ;; GET REAL QUEUE LENGTH FROM THE VARIABLE
      (let ((queue-len 0)) ;; FOR NOW DO NOT DO THIS (cdb:client-call nmsg-sockets 'sync #t 1)))
      ;; (print "Server running, count is " count)
	(if (< count 1) ;; 3x3 = 9 secs aprox
	    (loop (+ count 1)))

	;; NOTE: Get rid of this mechanism! It really is not needed...
	(open-run-close tasks:server-update-heartbeat tasks:open-db (car server-info))

	;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
	(mutex-lock! *heartbeat-mutex*)
	(set! last-access *last-db-access*)
	(mutex-unlock! *heartbeat-mutex*)
	(if (> (+ last-access
		  ;; (* 50 60 60)    ;; 48 hrs
		  ;; 60              ;; one minute
		  ;; (* 60 60)       ;; one hour
		  (* 45 60)          ;; 45 minutes, until the db deletion bug is fixed.
		  )
	       (current-seconds))
	    (begin
	      (debug:print-info 2 "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
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

(define (nmsg-transport:find-free-port-and-open iface s port stype #!key (trynum 50))
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
           (nmsg-transport:find-free-port-and-open iface s (+ p 1) trynum: (- trynum 1))
           (debug:print-info 0 "Tried ports up to " p 
                             " but all were in use. Please try a different port range by starting the server with parameter \" -port N\" where N is the starting port number to use"))
       (exit)) ;; To exit or not? That is the question.
     (let ((nmsg-url (conc "tcp://" iface ":" p)))
       (debug:print 2 "Trying to start server on " nmsg-url)
       (bind-socket s nmsg-url)
       (list iface s port)))))

(define (nmsg-transport:setup-ports ipaddrstr startport)
  (let* ((s1 (nmsg-transport:find-free-port-and-open ipaddrstr #f startport 'pull))
         (p1 (caddr s1))
         (s2 (nmsg-transport:find-free-port-and-open ipaddrstr #f (+ 1 (if p1 p1 (+ startport 1))) 'pub))
         (p2 (caddr s2)))
    (set! *runremote* #f)
    (debug:print 0 "Server started on " ipaddrstr " ports " p1 " and " p2)
    (mutex-lock! *heartbeat-mutex*)
    (set! *server-info* (open-run-close tasks:server-register 
					tasks:open-db 
					(current-process-id) 
					ipaddrstr p1 
					0 
					'live
					'nmsg
					pubport: p2))
    (debug:print-info 11 "*server-info* set to " *server-info*)
    (mutex-unlock! *heartbeat-mutex*)
    (list s1 s2)))

(define (nmsg-transport:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;;======================================================================
;; C L I E N T S
;;======================================================================

;; 
(define (nmsg-transport:client-socket-connect iface port #!key (context #f)(type 'req)(subscriptions '()))
  (debug:print-info 3 "client-connect " iface ":" port ", type=" type ", subscriptions=" subscriptions)
  (let ((connect-ok #f)
	(nmsg-socket (if context 
			(make-socket type context)
			(make-socket type)))
	(conurl     (nmsg-transport:make-server-url (list iface port))))
    (if (socket? nmsg-socket)
     (begin
	  ;; first apply subscriptions
	  (for-each (lambda (subscription)
		      (debug:print 2 "Subscribing to " subscription)
		      (socket-option-set! nmsg-socket 'subscribe subscription))
		    subscriptions)
	  (connect-socket nmsg-socket conurl)
	  nmsg-socket)
	(begin
	  (debug:print 0 "ERROR: Failed to open socket to " conurl)
	  #f))))

(define (nmsg-transport:client-connect iface pullport pubport)
  (let* ((push-socket (nmsg-transport:client-socket-connect iface pullport type: 'push))
	 (sub-socket  (nmsg-transport:client-socket-connect iface pubport
						    type: 'sub
						    subscriptions: (list (client:get-signature) "all")))
	 (nmsg-sockets (vector push-socket sub-socket))
	 (login-res   #f))
    (debug:print-info 11 "nmsg-transport:client-connect started. Next is login")
    (set! login-res (client:login serverdat nmsg-sockets))
    (if (and (not (null? login-res))
	     (car login-res))
	(begin
	  (debug:print-info 2 "Logged in and connected to " iface ":" pullport "/" pubport ".")
	  (set! *runremote* nmsg-sockets)
	  nmsg-sockets)
	(begin
	  (debug:print-info 2 "Failed to login or connect to " conurl)
	  (set! *runremote* #f)
	  #f))))

;; run nmsg-transport:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (nmsg-transport:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (let* ((server-info (let loop ()
                        (let ((sdat #f))
                          (mutex-lock! *heartbeat-mutex*)
                          (set! sdat *runremote*)
                          (mutex-unlock! *heartbeat-mutex*)
                          (if sdat sdat
                              (begin
                                (sleep 4)
                                (loop))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0)
	 (tdb         (tasks:open-db))
	 (spid        (tasks:server-get-server-id tdb #f iface port #f)))
    (print "Keep-running got server pid " spid ", using iface " iface " and port " port)
    (let loop ((count 0))
      (thread-sleep! 4) ;; no need to do this very often
      ;; NB// sync currently does NOT return queue-length
      (let () ;; (queue-len (cdb:client-call server-info 'sync #t 1)))
      ;; (print "Server running, count is " count)
        (if (< count 1) ;; 3x3 = 9 secs aprox
            (loop (+ count 1)))
        
        ;; NOTE: Get rid of this mechanism! It really is not needed...
        (tasks:server-update-heartbeat tdb spid)
      
        ;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
        (mutex-lock! *heartbeat-mutex*)
        (set! last-access *last-db-access*)
        (mutex-unlock! *heartbeat-mutex*)
        (if (> (+ last-access
                  ;; (* 50 60 60)    ;; 48 hrs
                  ;; 60              ;; one minute
                  ;; (* 60 60)       ;; one hour
                  (* 45 60)          ;; 45 minutes, until the db deletion bug is fixed.
                  )
               (current-seconds))
            (begin
              (debug:print-info 2 "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
              (loop 0))
            (begin
              (debug:print-info 0 "Starting to shutdown the server.")
              ;; need to delete only *my* server entry (future use)
              (set! *time-to-exit* #t)
              (tasks:server-deregister-self tdb (get-host-name))
              (thread-sleep! 1)
              (debug:print-info 0 "Max cached queries was " *max-cache-size*)
              (debug:print-info 0 "Server shutdown complete. Exiting")
              (exit)))))))

;; all routes though here end in exit ...
(define (nmsg-transport:launch)
  (if (not *toppath*)
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting nmsg server")
  (if *toppath* 
      (let* (;; (th1 (make-thread (lambda ()
	     ;;      	       (let ((server-info #f))
	     ;;      		 ;; wait for the server to be online and available
	     ;;      		 (let loop ()
	     ;;			   (debug:print-info 2 "Waiting for the server to come online before starting heartbeat")
	     ;;      		   (thread-sleep! 2)
	     ;;      		   (mutex-lock! *heartbeat-mutex*)
	     ;;      		   (set! server-info *server-info* )
	     ;;      		   (mutex-unlock! *heartbeat-mutex*)
	     ;;      		   (if (not server-info)(loop)))
	     ;;			 (debug:print 2 "Server alive, starting self-ping")
	     ;;      		 (nmsg-transport:self-ping server-info)
	     ;;      		 ))
	     ;;      	     "Self ping"))
	     (th2 (make-thread (lambda ()
				 (nmsg-transport:run 
				  (if (args:get-arg "-server")
				      (args:get-arg "-server")
				      "-"))) "Server run"))
	     ;; (th3 (make-thread (lambda ()(nmsg-transport:keep-running)) "Keep running"))
	     )
	(set! *client-non-blocking-mode* #t)
	;; (thread-start! th1)
	(thread-start! th2)
	;; (thread-start! th3)
	(set! *didsomething* #t)
	;; (thread-join! th3)
	(thread-join! th2)
	)
      (debug:print 0 "ERROR: Failed to setup for megatest")))

(define (nmsg-transport:client-signal-handler signum)
  (handle-exceptions
   exn
   (debug:print " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     (if (not *received-response*)
				 (receive-message* *runremote*))) ;; flush out last call if applicable
			   "eat response"))
	 (th2 (make-thread (lambda ()
			     (debug:print 0 "ERROR: Received ^C, attempting clean exit. Please be patient and wait a few seconds before hitting ^C again.")
			     (thread-sleep! 3) ;; give the flush three seconds to do it's stuff
			     (debug:print 0 "       Done.")
			     (exit 4))
			   "exit on ^C timer")))
     (thread-start! th2)
     (thread-start! th1)
     (thread-join! th2))))

(define (nmsg-transport:client-launch)
  (set-signal-handler! signal/int nmsg-transport:client-signal-handler)
   (if (nmsg-transport:client-setup)
       (debug:print-info 2 "connected as client")
       (begin
	 (debug:print 0 "ERROR: Failed to connect as client")
	 (exit))))

;;======================================================================
;; Defunct functions
;;======================================================================

;; ping a server and return number of clients or #f (if no response)
;; NOT IN USE!
(define (nmsg-transport:ping host port #!key (secs 10)(return-socket #f))
  (cdb:use-non-blocking-mode
   (lambda ()
     (let* ((res #f)
	    (th1 (make-thread
		  (lambda ()
		    (let* ((nmsg-context (make-context 1))
			   (nmsg-socket  (nmsg-transport:client-connect host port context: nmsg-context)))
		      (if nmsg-socket
			  (if (nmsg-transport:client-login nmsg-socket)
			      (let ((numclients (cdb:num-clients nmsg-socket)))
				(if (not return-socket)
				    (begin
				      (nmsg-transport:client-logout nmsg-socket)
				      (close-socket  nmsg-socket)))
				(set! res (list #t numclients (if return-socket nmsg-socket #f))))
			      (begin
				;; (close-socket nmsg-socket)
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

;; (define (nmsg-transport:self-ping server-info)
;;   ;; server-info: server-id interface pullport pubport
;;   (let ((iface    (list-ref server-info 1))
;; 	(pullport (list-ref server-info 2))
;; 	(pubport  (list-ref server-info 3)))
;;     (nmsg-transport:client-connect iface pullport pubport)
;;     (let loop ()
;;       (thread-sleep! 2)
;;       (cdb:client-call *runremote* 'ping #t)
;;       (debug:print 4 "nmsg-transport:self-ping - I'm alive on " iface ":" pullport "/" pubport "!")
;;       (mutex-lock! *heartbeat-mutex*)
;;       (set! *server-loop-heart-beat* (current-seconds))
;;       (mutex-unlock! *heartbeat-mutex*)
;;       (loop))))
    
(define (nmsg-transport:reply pubsock target query-sig success/fail result)
  (debug:print-info 11 "nmsg-transport:reply target=" target ", result=" result)
  (send-message pubsock target send-more: #t)
  (send-message pubsock (db:obj->string (vector success/fail query-sig result))))

