
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

(declare (unit http-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses server))

(include "common_records.scm")
(include "db_records.scm")

(define (http-transport:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

(define *db:process-queue-mutex* (make-mutex))

(define (http-transport:run hostn)
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
	 (start-port    (if (and (args:get-arg "-port")
				 (string->number (args:get-arg "-port")))
			    (string->number (args:get-arg "-port"))
			    (if (and (config-lookup  *configdat* "server" "port")
				     (string->number (config-lookup  *configdat* "server" "port")))
				(string->number (config-lookup  *configdat* "server" "port"))
				(+ 5000 (random 1001)))))
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
    (http-transport:try-start-server ipaddrstr start-port)
    ;; lite3:finalize! db)))
    ))

;; This is recursively run by http-transport:run until sucessful
;;
(define (http-transport:try-start-server ipaddrstr portnum)
  (handle-exceptions
   exn
   (begin
     (print-error-message exn)
     (if (< portnum 9000)
	 (begin 
	   (print "WARNING: failed to start on portnum: " portnum ", trying next port")
	   (thread-sleep! 0.1)
	   ;; (open-run-close tasks:remove-server-records tasks:open-db)
	   (http-transport:try-start-server ipaddrstr (+ portnum 1)))
	 (print "ERROR: Tried and tried but could not start the server")))
   (set! *runremote* (list ipaddrstr portnum))
   ;; (open-run-close tasks:remove-server-records tasks:open-db)
   (open-run-close tasks:server-register 
		   tasks:open-db 
		   (current-process-id)
		   ipaddrstr portnum 0 'live 'http)
   (print "INFO: Trying to start server on " ipaddrstr ":" portnum)
   ;; This starts the spiffy server
   (start-server port: portnum)
   (print "INFO: server has been stopped")))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;;======================================================================
;; C L I E N T S
;;======================================================================

;; <html>
;; <head></head>
;; <body>1 Hello, world! Goodbye Dolly</body></html>
;; Send msg to serverdat and receive result
(define (http-transport:client-send-receive serverdat msg)
  (let* ((url        (http-transport:make-server-url serverdat))
	 (fullurl    (conc url "/ctrl")) ;; (conc url "/?dat=" msg)))
	 (numretries 0))     
    (handle-exceptions
     exn
     (if (< numretries 200)
	 (http-transport:client-send-receive serverdat msg))
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

(define (http-transport:client-connect iface port)
  (let* ((login-res   #f)
	 (serverdat   (list iface port)))
    (set! login-res (client:login serverdat))
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


;; run http-transport:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (http-transport:keep-running)
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
	 (spid        (tasks:server-get-server-id tdb #f iface port #f))
	 (server-timeout (let ((tmo (config-lookup  *configdat* "server" "timeout")))
			   (if (and (string? tmo)
				    (string->number tmo))
			       (* 60 60 (string->number tmo))
			       ;; default to three days
			       (* 3 24 60)))))
    (debug:print-info 2 "server-timeout: " server-timeout ", server pid: " spid " on " iface ":" port)
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
	;; (debug:print 11 "last-access=" last-access ", server-timeout=" server-timeout)
        (if (> (+ last-access server-timeout)
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
              (debug:print-info 0 "Max cached queries was    " *max-cache-size*)
	      (debug:print-info 0 "Number of cached writes   " *number-of-writes*)
	      (debug:print-info 0 "Average cached write time " (/ *writes-total-delay* *number-of-writes*) " ms")
              (debug:print-info 0 "Server shutdown complete. Exiting")
              (exit)))))))

;; all routes though here end in exit ...
(define (http-transport:launch)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting the standalone server")
  (let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
    (debug:print 11 "http-transport:launch hostinfo=" hostinfo)
    ;; #(1 "143.182.207.24" 5736 -1 "http" 22771 "hostname")
    (if hostinfo
	(debug:print-info 2 "NOT starting new server, one is already running on " (vector-ref hostinfo 1) ":" (vector-ref hostinfo 2))
	(if *toppath* 
	    (let* ((th2 (make-thread (lambda ()
				       (http-transport:run 
					(if (args:get-arg "-server")
					    (args:get-arg "-server")
					    "-"))) "Server run"))
		   (th3 (make-thread http-transport:keep-running "Keep running"))
		   (th1 (make-thread server:write-queue-handler  "write queue")))
	      (thread-start! th2)
	      (thread-start! th3)
	      (thread-start! th1)
	      (set! *didsomething* #t)
	      (thread-join! th2))
	    (debug:print 0 "ERROR: Failed to setup for megatest")))
    (exit)))

(define (http-transport:server-signal-handler signum)
  (handle-exceptions
   exn
   (debug:print " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     (thread-sleep! 1))
			     ;; (if (not *received-response*)
			     ;;	 (receive-message* *runremote*))) ;; flush out last call if applicable
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
