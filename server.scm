
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

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(tcp-buffer-size 2048)

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
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))
	  (debug:print 0 "Server started on " host:port)

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

(define *db:process-queue-mutex* (make-mutex))

(define (server:run hostn)
  (debug:print 2 "Attempting to start the server ...")
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, cannot start server, exiting")
	    (exit))))
  (let* (;; (iface           (if (string=? "-" hostn)
	 ;;        	      #f ;; (get-host-name) 
	 ;;        	      hostn))
	 (db              #f) ;;        (open-db)) ;; we don't want the server to be opening and closing the db unnecesarily
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname)))
	 (start-port    (if (args:get-arg "-port")
			    (string->number (args:get-arg "-port"))
			    (+ 5000 (random 1001))))
	 (link-tree-path (config-lookup *configdat* "setup" "linktree")))
    (set! *cache-on* #t)
    (root-path     (if link-tree-path 
		       link-tree-path
		       (current-directory))) ;; WARNING: SECURITY HOLE. FIX ASAP!

    ;; Setup the web server and a /ctrl interface
    ;;
    (vhost-map `(((* any) . ,(lambda (continue)
			       ;; open the db on the first call 
			       (if (not db)(set! db (open-db)))
			       (let* (($   (request-vars source: 'both))
				      (dat ($ 'dat))
				      (res #f))
				 (cond
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ "hey"))
				   (send-response body: "hey there!\n"
						  headers: '((content-type text/plain))))
				  ;; This is the /ctrl path where data is handed to the server and
				  ;; responses 
				  ((equal? (uri-path (request-uri (current-request)))
					   '(/ "ctrl"))
				   (let* ((packet (db:string->obj dat))
					  (qtype  (cdb:packet-get-qtype packet)))
				     (debug:print-info 12 "server=> received packet=" packet)
				     (if (not (member qtype '(sync ping)))
					 (begin
					   (mutex-lock! *heartbeat-mutex*)
					   (set! *last-db-access* (current-seconds))
					   (mutex-unlock! *heartbeat-mutex*)))
				     ;; (mutex-lock! *db:process-queue-mutex*) ;; trying a mutex
				     ;; (set! res (open-run-close db:process-queue-item open-db packet))
				     (set! res (db:process-queue-item db packet))
				     ;; (mutex-unlock! *db:process-queue-mutex*)
				     (debug:print-info 11 "Return value from db:process-queue-item is " res)
				     (send-response body: (conc "<head>ctrl data</head>\n<body>"
								res
								"</body>")
						    headers: '((content-type text/plain)))))
				  (else (continue))))))))
    (server:try-start-server ipaddrstr start-port)
    ;; lite3:finalize! db)))
    ))



;; (define (server:main-loop)
;;   (print "INFO: Exectuing main server loop")
;;   (access-log "megatest-http.log")
;;   (server-bind-address #f)
;;   (define-page (main-page-path)
;;     (lambda ()
;;       (let ((dat ($ "dat")))
;;       ;; (with-request-variables (dat)
;;         (debug:print-info 12 "Got dat=" dat)
;; 	(let* ((packet (db:string->obj dat))
;; 	       (qtype  (cdb:packet-get-qtype packet)))
;; 	  (debug:print-info 12 "server=> received packet=" packet)
;; 	  (if (not (member qtype '(sync ping)))
;; 	      (begin
;; 		(mutex-lock! *heartbeat-mutex*)
;; 		(set! *last-db-access* (current-seconds))
;; 		(mutex-unlock! *heartbeat-mutex*)))
;; 	  (let ((res (open-run-close db:process-queue-item open-db packet)))
;; 	    (debug:print-info 11 "Return value from db:process-queue-item is " res)
;; 	    res))))))

;;; (use spiffy uri-common intarweb)
;;; 
;;; (root-path "/var/www")
;;; 
;;; (vhost-map `(((* any) . ,(lambda (continue)
;;;                            (if (equal? (uri-path (request-uri (current-request))) 
;;;                                        '(/ "hey"))
;;;                                (send-response body: "hey there!\n"
;;;                                               headers: '((content-type text/plain)))
;;;                                (continue))))))
;;; 
;;; (start-server port: 12345)

;; This is recursively run by server:run until sucessful
;;
(define (server:try-start-server ipaddrstr portnum)
  (handle-exceptions
   exn
   (begin
     (print-error-message exn)
     (if (< portnum 9000)
	 (begin 
	   (print "WARNING: failed to start on portnum: " portnum ", trying next port")
	   (thread-sleep! 0.1)
	   (open-run-close tasks:remove-server-records tasks:open-db)
	   (server:try-start-server ipaddrstr (+ portnum 1)))
	 (print "ERROR: Tried and tried but could not start the server")))
   (set! *runremote* (list ipaddrstr portnum))
   (open-run-close tasks:remove-server-records tasks:open-db)
   (open-run-close tasks:server-register 
		   tasks:open-db 
		   (current-process-id)
		   ipaddrstr portnum 0 'live)
   (print "INFO: Trying to start server on " ipaddrstr ":" portnum)
   ;; This starts the spiffy server
   (start-server port: portnum)
   (print "INFO: server has been stopped")))

(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;; When using zmq this would send the message back (two step process)
;; with spiffy or rpc this simply returns the return data to be returned
;; 
(define (server:reply return-addr query-sig success/fail result)
  (debug:print-info 11 "server:reply return-addr=" return-addr ", result=" result)
  ;; (send-message pubsock target send-more: #t)
  ;; (send-message pubsock 
  (db:obj->string (vector success/fail query-sig result)))

;;======================================================================
;; C L I E N T S
;;======================================================================

(define (server:get-client-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; <html>
;; <head></head>
;; <body>1 Hello, world! Goodbye Dolly</body></html>
;; Send msg to serverdat and receive result
(define (server:client-send-receive serverdat msg)
  (let* ((url        (server:make-server-url serverdat))
	 (fullurl    (conc url "/ctrl")) ;; (conc url "/?dat=" msg)))
	 (numretries 0))     
    (handle-exceptions
     exn
     (if (< numretries 200)
	 (server:client-send-receive serverdat msg))
     (begin
       (debug:print-info 11 "fullurl=" fullurl "\n")
       ;; set up the http-client here
       (max-retry-attempts 100)
       (retry-request? (lambda (request)
			 (thread-sleep! (/ (if (> numretries 100) 100 numretries) 10))
			 (set! numretries (+ numretries 1))
			 #t))
       ;; send the data and get the response
       ;; extract the needed info from the http data and 
       ;; process and return it.
       (let* ((res   (with-input-from-request fullurl 
					      ;; #f
					      ;; msg 
					      (list (cons 'dat msg)) 
					      read-string)))
	 (debug:print-info 11 "got res=" res)
	 (let ((match (string-search (regexp "<body>(.*)<.body>") res)))
	   (debug:print-info 11 "match=" match)
	   (let ((final (cadr match)))
	     (debug:print-info 11 "final=" final)
	     final)))))))

(define (server:client-login serverdat)
  (max-retry-attempts 100)
  (cdb:login serverdat *toppath* (server:get-client-signature)))

;; Not currently used! But, I think it *should* be used!!!
(define (server:client-logout serverdat)
  (let ((ok (and (socket? serverdat)
		 (cdb:logout serverdat *toppath* (server:get-client-signature)))))
    ;; (close-socket serverdat)
    ok))

(define (server:client-connect iface port)
  (let* ((login-res   #f)
	 (serverdat   (list iface port)))
    (set! login-res (server:client-login serverdat))
    (if (and (not (null? login-res))
	     (car login-res))
	(begin
	  (debug:print-info 2 "Logged in and connected to " iface ":" port)
	  (set! *runremote* serverdat)
	  serverdat)
	(begin
	  (debug:print-info 2 "Failed to login or connect to " iface ":" port)
	  (set! *runremote* #f)
	  #f))))

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
	      (port     (list-ref hostinfo 2))
	      (pid      (list-ref hostinfo 3)))
	  (debug:print-info 2 "Setting up to connect to " hostinfo)
	  (server:client-connect iface port)) ;; )
	(if (> numtries 0)
	    (let ((exe (car (argv)))
		  (pid #f))
	      (debug:print-info 0 "No server available, attempting to start one...")
	      (set! pid (process-run exe (list "-server" "-" "-debug" (if (list? *verbosity*)
	        							  (string-intersperse *verbosity* ",")
	        							  (conc *verbosity*)))))
	      ;; (set! pid (process-fork (lambda ()
	      ;;   			(current-input-port  (open-input-file  "/dev/null"))
	      ;;   			(current-output-port (open-output-file "/dev/null"))
	      ;;   			(current-error-port  (open-output-file "/dev/null"))
	      ;;   			(server:launch))))
	      (let loop ((count 0))
		(let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
		  (if (not hostinfo)
		      (begin
			(debug:print-info 0 "Waiting for server pid=" pid " to start")
			(sleep 2) ;; give server time to start
			(if (< count 5)
			    (loop (+ count 1)))))))
	      ;; we are starting a server, do not try again! That can lead to 
	      ;; recursively starting many processes!!!
	      (server:client-setup numtries: 0))
	    (debug:print-info 1 "Too many attempts, giving up")))))

;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running)
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
(define (server:launch)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting the standalone server")
  (let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
    (debug:print 11 "server:launch hostinfo=" hostinfo)
    (if hostinfo
	(debug:print-info 2 "NOT starting new server, one is already running on " (car hostinfo) ":" (cadr hostinfo))
	(if *toppath* 
	    (let* ((th2 (make-thread (lambda ()
				       (server:run 
					(if (args:get-arg "-server")
					    (args:get-arg "-server")
					    "-"))) "Server run"))
		   (th3 (make-thread (lambda ()(server:keep-running)) "Keep running"))
		   )
	      (thread-start! th2)
	      (thread-start! th3)
	      (set! *didsomething* #t)
	      (thread-join! th2)
	      )
	    (debug:print 0 "ERROR: Failed to setup for megatest")))
    (exit)))

(define (server:client-signal-handler signum)
  (handle-exceptions
   exn
   (debug:print " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     "") ;; do nothing for now (was flush out last call if applicable)
			   "eat response"))
	 (th2 (make-thread (lambda ()
			     (debug:print 0 "ERROR: Received ^C, attempting clean exit. Please be patient and wait a few seconds before hitting ^C again.")
			     (thread-sleep! 1) ;; give the flush one second to do it's stuff
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

