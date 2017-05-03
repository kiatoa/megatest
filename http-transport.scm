
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use  srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest posix-extras) ;; sqlite3
;; (import (prefix sqlite3 sqlite3:))

(use spiffy uri-common intarweb http-client spiffy-request-vars intarweb spiffy-directory-listing)

;; Configurations for server
(tcp-buffer-size 2048)
(max-connections 2048) 

(declare (unit http-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses server))
(declare (uses daemon))
(declare (uses portlogger))
(declare (uses rmt))

(include "common_records.scm")
(include "db_records.scm")

(define (http-transport:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define *server-loop-heart-beat* (current-seconds))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

(define *db:process-queue-mutex* (make-mutex))

(define (http-transport:run hostn)
  (debug:print 2 *default-log-port* "Attempting to start the server ...")
  (let* ((db              #f) ;;        (open-db)) ;; we don't want the server to be opening and closing the db unnecesarily
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname))) 
	 (start-port      (portlogger:open-run-close portlogger:find-port))
	 (link-tree-path  (common:get-linktree))) ;; (configf:lookup *configdat* "setup" "linktree")))
    (debug:print-info 0 *default-log-port* "portlogger recommended port: " start-port)
    (root-path     (if link-tree-path 
		       link-tree-path
		       (current-directory))) ;; WARNING: SECURITY HOLE. FIX ASAP!
    (handle-directory spiffy-directory-listing)
    (handle-exception (lambda (exn chain)
			(signal (make-composite-condition
				 (make-property-condition 
				  'server
				  'message "server error")))))

    ;; http-transport:handle-directory) ;; simple-directory-handler)
    ;; Setup the web server and a /ctrl interface
    ;;
    (vhost-map `(((* any) . ,(lambda (continue)
			       ;; open the db on the first call 
				 ;; This is were we set up the database connections
			       (let* (($   (request-vars source: 'both))
				      (dat ($ 'dat))
				      (res #f))
				 (cond
				  ((equal? (uri-path (request-uri (current-request)))
					   '(/ "api"))
				   (send-response body:    (api:process-request *dbstruct-db* $) ;; the $ is the request vars proc
						  headers: '((content-type text/plain)))
				   (mutex-lock! *heartbeat-mutex*)
				   (set! *db-last-access* (current-seconds))
				   (mutex-unlock! *heartbeat-mutex*))
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ ""))
				   (send-response body: (http-transport:main-page)))
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ "json_api"))
				   (send-response body: (http-transport:main-page)))
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ "runs"))
				   (send-response body: (http-transport:main-page)))
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ any))
				   (send-response body: "hey there!\n"
						  headers: '((content-type text/plain))))
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ "hey"))
				   (send-response body: "hey there!\n"
						  headers: '((content-type text/plain))))
				  (else (continue))))))))
    (http-transport:try-start-server ipaddrstr start-port)))

;; This is recursively run by http-transport:run until sucessful
;;
(define (http-transport:try-start-server ipaddrstr portnum)
  (let ((config-hostname (configf:lookup *configdat* "server" "hostname"))
	(config-use-proxy (equal? (configf:lookup *configdat* "client" "use-http_proxy") "yes")))
    (if (not config-use-proxy)
	(determine-proxy (constantly #f)))
    (debug:print-info 0 *default-log-port* "http-transport:try-start-server time=" (seconds->time-string (current-seconds)) " ipaddrsstr=" ipaddrstr " portnum=" portnum " config-hostname=" config-hostname)
    (handle-exceptions
	exn
	(begin
	  (print-error-message exn)
	  (if (< portnum 64000)
	      (begin 
		(debug:print 0 *default-log-port* "WARNING: attempt to start server failed. Trying again ...")
		(debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
		(debug:print 0 *default-log-port* "exn=" (condition->list exn))
		(portlogger:open-run-close portlogger:set-failed portnum)
		(debug:print 0 *default-log-port* "WARNING: failed to start on portnum: " portnum ", trying next port")
		(thread-sleep! 0.1)
		
		;; get_next_port goes here
		(http-transport:try-start-server ipaddrstr
						 (portlogger:open-run-close portlogger:find-port)))
	      (begin
		(print "ERROR: Tried and tried but could not start the server"))))
      ;; any error in following steps will result in a retry
      (set! *server-info* (list ipaddrstr portnum))
      (debug:print 0 *default-log-port* "INFO: Trying to start server on " ipaddrstr ":" portnum)
      ;; This starts the spiffy server
      ;; NEED WAY TO SET IP TO #f TO BIND ALL
      ;; (start-server bind-address: ipaddrstr port: portnum)
      (if config-hostname ;; this is a hint to bind directly
	  (start-server port: portnum bind-address: (if (equal? config-hostname "-")
							ipaddrstr
							config-hostname))
	  (start-server port: portnum))
      (portlogger:open-run-close portlogger:set-port portnum "released")
      (debug:print 1 *default-log-port* "INFO: server has been stopped"))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;;======================================================================
;; C L I E N T S
;;======================================================================

(define *http-mutex* (make-mutex))

;; NOTE: Large block of code from 32436b426188080f72fceb6894af541fbad9921e removed here
;;       I'm pretty sure it is defunct.

;; This next block all imported en-mass from the api branch
(define *http-requests-in-progress* 0)
(define *http-connections-next-cleanup* (current-seconds))

(define (http-transport:get-time-to-cleanup)
  (let ((res #f))
    (mutex-lock! *http-mutex*)
    (set! res (> (current-seconds) *http-connections-next-cleanup*))
    (mutex-unlock! *http-mutex*)
    res))

(define (http-transport:inc-requests-count)
  (mutex-lock! *http-mutex*)
  (set! *http-requests-in-progress* (+ 1 *http-requests-in-progress*))
  ;; Use this opportunity to slow things down iff there are too many requests in flight
  (if (> *http-requests-in-progress* 5)
      (begin
	(debug:print-info 0 *default-log-port* "Whoa there buddy, ease up...")
	(thread-sleep! 1)))
  (mutex-unlock! *http-mutex*))

(define (http-transport:dec-requests-count proc) 
  (mutex-lock! *http-mutex*)
  (proc)
  (set! *http-requests-in-progress* (- *http-requests-in-progress* 1))
  (mutex-unlock! *http-mutex*))

(define (http-transport:dec-requests-count-and-close-all-connections)
  (set! *http-requests-in-progress* (- *http-requests-in-progress* 1))
  (let loop ((etime (+ (current-seconds) 5))) ;; give up in five seconds
    (if (> *http-requests-in-progress* 0)
	(if (> etime (current-seconds))
	    (begin
	      (thread-sleep! 0.05)
	      (loop etime))
	    (debug:print-error 0 *default-log-port* "requests still in progress after 5 seconds of waiting. I'm going to pass on cleaning up http connections"))
	(close-all-connections!)))
  (set! *http-connections-next-cleanup* (+ (current-seconds) 10))
  (mutex-unlock! *http-mutex*))

(define (http-transport:inc-requests-and-prep-to-close-all-connections)
  (mutex-lock! *http-mutex*)
  (set! *http-requests-in-progress* (+ 1 *http-requests-in-progress*)))

;; Send "cmd" with json payload "params" to serverdat and receive result
;;
(define (http-transport:client-api-send-receive run-id serverdat cmd params #!key (numretries 3)(area-dat #f))
  (let* ((fullurl    (if (vector? serverdat)
			 (http-transport:server-dat-get-api-req serverdat)
			 (begin
			   (debug:print 0 *default-log-port* "FATAL ERROR: http-transport:client-api-send-receive called with no server info")
			   (exit 1))))
	 (res        (vector #f "uninitialized"))
	 (success    #t)
	 (sparams    (db:obj->string params transport: 'http))
	 (runremote  (or area-dat *runremote*)))
       (debug:print-info 11 *default-log-port* "fullurl=" fullurl ", cmd=" cmd ", params=" params ", run-id=" run-id "\n")
       ;; set up the http-client here
       (max-retry-attempts 1)
       ;; consider all requests indempotent
       (retry-request? (lambda (request)
			 #f))
       ;; send the data and get the response
       ;; extract the needed info from the http data and 
       ;; process and return it.
       (let* ((send-recieve (lambda ()
			      (mutex-lock! *http-mutex*)
			      ;; (condition-case (with-input-from-request "http://localhost"; #f read-lines)
			      ;;					       ((exn http client-error) e (print e)))
			      (set! res (vector                ;;; DON'T FORGET - THIS IS THE CLIENT SIDE! NOTE: consider moving this to client.scm since we are only supporting http transport at this time.
					 success
					 (db:string->obj 
					  (handle-exceptions
					      exn
					      (let ((call-chain (get-call-chain))
						    (msg        ((condition-property-accessor 'exn 'message) exn)))
						(set! success #f)
						(debug:print 0 *default-log-port* "WARNING: failure in with-input-from-request to " fullurl ".")
						(debug:print 0 *default-log-port* " message: " msg)
						(debug:print 0 *default-log-port* " cmd: " cmd " params: " params)
						(if runremote
						    (remote-conndat-set! runremote #f))
						;; Killing associated server to allow clean retry.")
						;; (tasks:kill-server-run-id run-id)  ;; better to kill the server in the logic that called this routine?
						(mutex-unlock! *http-mutex*)
					     ;;; (signal (make-composite-condition
					     ;;;          (make-property-condition 'commfail 'message "failed to connect to server")))
					     ;;; "communications failed"
						(db:obj->string #f))
					    (with-input-from-request ;; was dat
					     fullurl 
					     (list (cons 'key "thekey")
						   (cons 'cmd cmd)
						   (cons 'params sparams))
					     read-string))
					  transport: 'http)
                                         0)) ;; added this speculatively
			      ;; Shouldn't this be a call to the managed call-all-connections stuff above?
			      (close-all-connections!)
			      (mutex-unlock! *http-mutex*)
			      ))
	      (time-out     (lambda ()
			      (thread-sleep! 45)
			      #f))
	      (th1 (make-thread send-recieve "with-input-from-request"))
	      (th2 (make-thread time-out     "time out")))
	 (thread-start! th1)
	 (thread-start! th2)
	 (thread-join! th1)
	 (thread-terminate! th2)
	 (debug:print-info 11 *default-log-port* "got res=" res)
	 (if (vector? res)
	     (if (vector-ref res 0) ;; this is the first flag or the second flag?
		 res ;; this is the *inner* vector? seriously? why?
                 (if (debug:debug-mode 11)
                     (let ((call-chain (get-call-chain))) ;; note: this code also called in nmsg-transport - consider consolidating it
                       (print-call-chain (current-error-port))
                       (debug:print-error 11 *default-log-port* "error above occured at server, res=" res " message: " ((condition-property-accessor 'exn 'message) exn))
                       (debug:print 11 *default-log-port* " server call chain:")
                       (pp (vector-ref res 1) (current-error-port))
                       (signal (vector-ref res 0)))
                     res))
	     (signal (make-composite-condition
		      (make-property-condition 
		       'timeout
		       'message "nmsg-transport:client-api-send-receive-raw timed out talking to server")))))))

;; careful closing of connections stored in *runremote*
;;
(define (http-transport:close-connections #!key (area-dat #f))
  (let* ((runremote  (or area-dat *runremote*))
	 (server-dat (if runremote
                         (remote-conndat runremote)
                         #f))) ;; (hash-table-ref/default *runremote* run-id #f)))
    (if (vector? server-dat)
	(let ((api-dat (http-transport:server-dat-get-api-uri server-dat)))
	  (close-connection! api-dat)
	  #t)
	#f)))


(define (make-http-transport:server-dat)(make-vector 6))
(define (http-transport:server-dat-get-iface         vec)    (vector-ref  vec 0))
(define (http-transport:server-dat-get-port          vec)    (vector-ref  vec 1))
(define (http-transport:server-dat-get-api-uri       vec)    (vector-ref  vec 2))
(define (http-transport:server-dat-get-api-url       vec)    (vector-ref  vec 3))
(define (http-transport:server-dat-get-api-req       vec)    (vector-ref  vec 4))
(define (http-transport:server-dat-get-last-access   vec)    (vector-ref  vec 5))
(define (http-transport:server-dat-get-socket        vec)    (vector-ref  vec 6))

(define (http-transport:server-dat-make-url vec)
  (if (and (http-transport:server-dat-get-iface vec)
	   (http-transport:server-dat-get-port  vec))
      (conc "http://" 
	    (http-transport:server-dat-get-iface vec)
	    ":"
	    (http-transport:server-dat-get-port  vec))
      #f))

(define (http-transport:server-dat-update-last-access vec)
  (if (vector? vec)
      (vector-set! vec 5 (current-seconds))
      (begin
	(print-call-chain (current-error-port))
	(debug:print-error 0 *default-log-port* "call to http-transport:server-dat-update-last-access with non-vector!!"))))

;;
;; connect
;;
(define (http-transport:client-connect iface port)
  (let* ((api-url      (conc "http://" iface ":" port "/api"))
	 (api-uri      (uri-reference (conc "http://" iface ":" port "/api")))
	 (api-req      (make-request method: 'POST uri: api-uri))
	 (server-dat   (vector iface port api-uri api-url api-req (current-seconds))))
    server-dat))

;; run http-transport:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (http-transport:keep-running) 
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (debug:print-info 0 *default-log-port* "Starting the sync-back, keep alive thread in server")
  (let* ((server-start-time (current-seconds))
	 (server-info (let loop ((start-time (current-seconds))
				 (changed    #t)
				 (last-sdat  "not this"))
                        (let ((sdat #f))
			  (thread-sleep! 0.01)
			  (debug:print-info 0 *default-log-port* "Waiting for server alive signature")
                          (mutex-lock! *heartbeat-mutex*)
                          (set! sdat *server-info*)
                          (mutex-unlock! *heartbeat-mutex*)
                          (if (and sdat
				   (not changed)
				   (> (- (current-seconds) start-time) 2))
			      (begin
				(debug:print-info 0 *default-log-port* "Received server alive signature")
				sdat)
                              (begin
				(debug:print-info 0 *default-log-port* "Still waiting, last-sdat=" last-sdat)
                                (sleep 4)
				(if (> (- (current-seconds) start-time) 120) ;; been waiting for two minutes
				    (begin
				      (debug:print-error 0 *default-log-port* "transport appears to have died, exiting server")
				      (exit))
				    (loop start-time
					  (equal? sdat last-sdat)
					  sdat)))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0)
	 (server-timeout (server:get-timeout))
	 (server-going  #f)
	 (server-log-file (args:get-arg "-log"))) ;; always set when we are a server
    (let loop ((count         0)
	       (server-state 'available)
	       (bad-sync-count 0)
	       (start-time     (current-milliseconds)))
      ;; Use this opportunity to sync the tmp db to megatest.db
      (if (not server-going) ;; *dbstruct-db* 
	  (begin
	    (debug:print 0 *default-log-port* "SERVER: dbprep")
	    (set! *dbstruct-db*  (db:setup #t)) ;;  run-id))
	    (set! server-going #t)
	    (debug:print 0 *default-log-port* "SERVER: running, megatest version: " (common:get-full-version)) ;; NOTE: the server is NOT yet marked as running in the log. We do that in the keep-running routine.
	    (thread-start! *watchdog*)))
      
      ;; when things go wrong we don't want to be doing the various queries too often
      ;; so we strive to run this stuff only every four seconds or so.
      (let* ((sync-time (- (current-milliseconds) start-time))
	    (rem-time  (quotient (- 4000 sync-time) 1000)))
	(if (and (<= rem-time 4)
		 (>  rem-time 0))
	    (thread-sleep! rem-time)))
      
      (if (< count 1) ;; 3x3 = 9 secs aprox
	  (loop (+ count 1) 'running bad-sync-count (current-milliseconds)))
      
      ;; Check that iface and port have not changed (can happen if server port collides)
      (mutex-lock! *heartbeat-mutex*)
      (set! sdat *server-info*)
      (mutex-unlock! *heartbeat-mutex*)
      
      (if (not (equal? sdat (list iface port)))
	  (let ((new-iface (car sdat))
		(new-port  (cadr sdat)))
	    (debug:print-info 0 *default-log-port* "WARNING: interface changed, refreshing iface and port info")
	    (set! iface new-iface)
	    (set! port  new-port)
	    (debug:print 0 *default-log-port* "SERVER STARTED: " iface ":" port " AT " (current-seconds))
	    (flush-output *default-log-port*)))
      
      ;; Transfer *db-last-access* to last-access to use in checking that we are still alive
      (mutex-lock! *heartbeat-mutex*)
      (set! last-access *db-last-access*)
      (mutex-unlock! *heartbeat-mutex*)
      
      (if (common:low-noise-print 120 (conc "server running on " iface ":" port))
	  (begin
	    (debug:print 0 *default-log-port* "SERVER STARTED: " iface ":" port " AT " (current-seconds))
	    (flush-output *default-log-port*)))
      (if (common:low-noise-print 60 "dbstats")
	  (begin
	    (debug:print 0 *default-log-port* "Server stats:")
	    (db:print-current-query-stats)))
      (let* ((hrs-since-start  (/ (- (current-seconds) server-start-time) 3600))
	     (adjusted-timeout (if (> hrs-since-start 1)
				   (- server-timeout (inexact->exact (round (* hrs-since-start 60))))  ;; subtract 60 seconds per hour
				   server-timeout)))
	(if (common:low-noise-print 120 "server timeout")
	    (debug:print-info 0 *default-log-port* "Adjusted server timeout: " adjusted-timeout))
	(cond
         ((and *server-run*
	       (> (+ last-access server-timeout)
		  (current-seconds))
	       (< (- (current-seconds) server-start-time) 3600)) ;; do not update log or touch log if we've been running for more than one hour.
          (if (common:low-noise-print 120 "server continuing")
              (debug:print-info 0 *default-log-port* "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
	      (let ((curr-time (current-seconds)))
		(handle-exceptions
		    exn
		    (debug:print 0 *default-log-port* "ERROR: Failed to change timestamp on log file " server-log-file ". Are you out of space on that disk?")
		  (change-file-times server-log-file curr-time curr-time))))
          (loop 0 server-state bad-sync-count (current-milliseconds)))
         (else
          (debug:print-info 0 *default-log-port* "Server timed out. seconds since last db access: " (- (current-seconds) last-access))
          (http-transport:server-shutdown port)))))))

(define (http-transport:server-shutdown port)
  (begin
    ;;(BB> "http-transport:server-shutdown called")
    (debug:print-info 0 *default-log-port* "Starting to shutdown the server. pid="(current-process-id))
    ;;
    ;; start_shutdown
    ;;
    (set! *time-to-exit* #t) ;; tell on-exit to be fast as we've already cleaned up
    (portlogger:open-run-close portlogger:set-port port "released")
    (thread-sleep! 1)

    ;; (debug:print-info 0 *default-log-port* "Max cached queries was    " *max-cache-size*)
    ;; (debug:print-info 0 *default-log-port* "Number of cached writes   " *number-of-writes*)
    ;; (debug:print-info 0 *default-log-port* "Average cached write time "
    ;; 		      (if (eq? *number-of-writes* 0)
    ;; 			  "n/a (no writes)"
    ;; 			  (/ *writes-total-delay*
    ;; 			     *number-of-writes*))
    ;; 		      " ms")
    ;; (debug:print-info 0 *default-log-port* "Number non-cached queries "  *number-non-write-queries*)
    ;; (debug:print-info 0 *default-log-port* "Average non-cached time   "
    ;; 		      (if (eq? *number-non-write-queries* 0)
    ;; 			  "n/a (no queries)"
    ;; 			  (/ *total-non-write-delay* 
    ;; 			     *number-non-write-queries*))
    ;; 		      " ms")
    
    (db:print-current-query-stats)
    
    (debug:print-info 0 *default-log-port* "Server shutdown complete. Exiting")
    (exit)))

;; all routes though here end in exit ...
;;
;; start_server? 
;;
(define (http-transport:launch)
  ;; (if (args:get-arg "-daemonize")
  ;;     (begin
  ;; 	(daemon:ize)
  ;; 	(if *alt-log-file* ;; we should re-connect to this port, I think daemon:ize disrupts it
  ;; 	    (begin
  ;; 	      (current-error-port *alt-log-file*)
  ;; 	      (current-output-port *alt-log-file*)))))
  (let* ((th2 (make-thread (lambda ()
			     (debug:print-info 0 *default-log-port* "Server run thread started")
			     (http-transport:run 
			      (if (args:get-arg "-server")
				  (args:get-arg "-server")
				  "-")
			      )) "Server run"))
	 (th3 (make-thread (lambda ()
			     (debug:print-info 0 *default-log-port* "Server monitor thread started")
			     (http-transport:keep-running)
			   "Keep running"))))
    (thread-start! th2)
    (thread-sleep! 0.25) ;; give the server time to settle before starting the keep-running monitor.
    (thread-start! th3)
    (set! *didsomething* #t)
    (thread-join! th2)
    (exit)))

(define (http-transport:server-signal-handler signum)
  (signal-mask! signum)
  (handle-exceptions
   exn
   (debug:print 0 *default-log-port* " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     (thread-sleep! 1))
			   "eat response"))
	 (th2 (make-thread (lambda ()
			     (debug:print-error 0 *default-log-port* "Received ^C, attempting clean exit. Please be patient and wait a few seconds before hitting ^C again.")
			     (thread-sleep! 3) ;; give the flush three seconds to do it's stuff
			     (debug:print 0 *default-log-port* "       Done.")
			     (exit 4))
			   "exit on ^C timer")))
     (thread-start! th2)
     (thread-start! th1)
     (thread-join! th2))))

;;======================================================================
;; web pages
;;======================================================================

(define (http-transport:main-page)
  (let ((linkpath (root-path)))
    (conc "<head><h1>" (pathname-strip-directory *toppath*) "</h1></head>"
	  "<body>"
	  "Run area: " *toppath*
	  "<h2>Server Stats</h2>"
	  (http-transport:stats-table) 
	  "<hr>"
	  (http-transport:runs linkpath)
	  "<hr>"
	  (http-transport:run-stats)
	  "</body>"
	  )))

(define (http-transport:stats-table)
  (mutex-lock! *heartbeat-mutex*)
  (let ((res 
	 (conc "<table>"
	       ;; "<tr><td>Max cached queries</td>        <td>" *max-cache-size* "</td></tr>"
	       "<tr><td>Number of cached writes</td>   <td>" *number-of-writes* "</td></tr>"
	       "<tr><td>Average cached write time</td> <td>" (if (eq? *number-of-writes* 0)
								 "n/a (no writes)"
								 (/ *writes-total-delay*
								    *number-of-writes*))
	       " ms</td></tr>"
	       "<tr><td>Number non-cached queries</td> <td>"  *number-non-write-queries* "</td></tr>"
	       ;; "<tr><td>Average non-cached time</td>   <td>" (if (eq? *number-non-write-queries* 0)
	       ;; 							 "n/a (no queries)"
	       ;; 							 (/ *total-non-write-delay* 
	       ;; 							    *number-non-write-queries*))
	       " ms</td></tr>"
	       "<tr><td>Last access</td><td>"              (seconds->time-string *db-last-access*) "</td></tr>"
	       "</table>")))
    (mutex-unlock! *heartbeat-mutex*)
    res))

(define (http-transport:runs linkpath)
  (conc "<h3>Runs</h3>"
	(string-intersperse
	 (let ((files (map pathname-strip-directory (glob (conc linkpath "/*")))))
	   (map (lambda (p)
		  (conc "<a href=\"" p "\">" p "</a><br>"))
		files))
	 " ")))

(define (http-transport:run-stats)
  (let ((stats (open-run-close db:get-running-stats #f)))
    (conc "<table>"
	  (string-intersperse
	   (map (lambda (stat)
		  (conc "<tr><td>" (car stat) "</td><td>" (cadr stat) "</td></tr>"))
		stats)
	   " ")
	  "</table>")))
