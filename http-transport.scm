
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use  srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest) ;; sqlite3
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

(include "common_records.scm")
(include "db_records.scm")

(define (http-transport:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

(define *db:process-queue-mutex* (make-mutex))

(define (http-transport:run hostn run-id server-id)
  (debug:print 2 *default-log-port* "Attempting to start the server ...")
  (let* ((db              #f) ;;        (open-db)) ;; we don't want the server to be opening and closing the db unnecesarily
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname))) 
	 (start-port      (portlogger:open-run-close portlogger:find-port))
	 (link-tree-path  (configf:lookup *configdat* "setup" "linktree")))
    ;; (set! db *inmemdb*)
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
				   (send-response body:    (api:process-request *inmemdb* $) ;; the $ is the request vars proc
						  headers: '((content-type text/plain)))
				   (mutex-lock! *heartbeat-mutex*)
				   (set! *last-db-access* (current-seconds))
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
    (http-transport:try-start-server run-id ipaddrstr start-port server-id)))

;; This is recursively run by http-transport:run until sucessful
;;
(define (http-transport:try-start-server run-id ipaddrstr portnum server-id)
  (let ((config-hostname (configf:lookup *configdat* "server" "hostname"))
	(tdbdat          (tasks:open-db)))
    (debug:print-info 0 *default-log-port* "http-transport:try-start-server run-id=" run-id " ipaddrsstr=" ipaddrstr " portnum=" portnum " server-id=" server-id " config-hostname=" config-hostname)
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
	     (http-transport:try-start-server run-id
					      ipaddrstr
					      (portlogger:open-run-close portlogger:find-port)
					      server-id))
	   (begin
	     (tasks:server-force-clean-run-record (db:delay-if-busy tdbdat) run-id ipaddrstr portnum " http-transport:try-start-server")
	     (print "ERROR: Tried and tried but could not start the server"))))
     ;; any error in following steps will result in a retry
     (set! *server-info* (list ipaddrstr portnum))
     (tasks:server-set-interface-port 
		     (db:delay-if-busy tdbdat)
		     server-id 
		     ipaddrstr portnum)
     (debug:print 0 *default-log-port* "INFO: Trying to start server on " ipaddrstr ":" portnum)
     ;; This starts the spiffy server
     ;; NEED WAY TO SET IP TO #f TO BIND ALL
     ;; (start-server bind-address: ipaddrstr port: portnum)
     (if config-hostname ;; this is a hint to bind directly
	 (start-server port: portnum bind-address: (if (equal? config-hostname "-")
						       ipaddrstr
						       config-hostname))
	 (start-server port: portnum))
     ;;  (portlogger:open-run-close portlogger:set-port portnum "released")
     (tasks:server-force-clean-run-record (db:delay-if-busy tdbdat) run-id ipaddrstr portnum " http-transport:try-start-server")
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
(define (http-transport:client-api-send-receive run-id serverdat cmd params #!key (numretries 3))
  (let* ((fullurl    (if (vector? serverdat)
			 (http-transport:server-dat-get-api-req serverdat)
			 (begin
			   (debug:print 0 *default-log-port* "FATAL ERROR: http-transport:client-api-send-receive called with no server info")
			   (exit 1))))
	 (res        #f)
	 (success    #t)
	 (sparams    (db:obj->string params transport: 'http)))
;;    (condition-case
;;     handle-exceptions
;;     exn
;;     (if (> numretries 0)
;;	 (begin
;;	   (mutex-unlock! *http-mutex*)
;;	   (thread-sleep! 1)
;;	   (handle-exceptions
;;	    exn
;;	    (debug:print 0 *default-log-port* "WARNING: closing connections failed. Server at " fullurl " almost certainly dead")
;;	    (close-all-connections!))
;;	   (debug:print 0 *default-log-port* "WARNING: Failed to communicate with server, trying again, numretries left: " numretries)
;;	   (http-transport:client-api-send-receive run-id serverdat cmd sparams numretries: (- numretries 1)))
;;	 (begin
;;	   (mutex-unlock! *http-mutex*)
;;	   (tasks:kill-server-run-id run-id)
;;	   #f))
;;     (begin
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
			      (set! res (vector
					 success
					 (db:string->obj 
					  (handle-exceptions
					   exn
					   (begin
					     (set! success #f)
					     (debug:print 0 *default-log-port* "WARNING: failure in with-input-from-request to " fullurl ".")
					     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
					     (hash-table-delete! *runremote* run-id)
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
					  transport: 'http)))
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
	     (if (vector-ref res 0)
		 res
		 (begin ;; note: this code also called in nmsg-transport - consider consolidating it
		   (debug:print-error 0 *default-log-port* "error occured at server, info=" (vector-ref res 2))
		   (debug:print 0 *default-log-port* " client call chain:")
		   (print-call-chain (current-error-port))
		   (debug:print 0 *default-log-port* " server call chain:")
		   (pp (vector-ref res 1) (current-error-port))
		   (signal (vector-ref result 0))))
	     (signal (make-composite-condition
		      (make-property-condition 
		       'timeout
		       'message "nmsg-transport:client-api-send-receive-raw timed out talking to server")))))))

;; careful closing of connections stored in *runremote*
;;
(define (http-transport:close-connections run-id)
  (let* ((server-dat (hash-table-ref/default *runremote* run-id #f)))
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
(define (http-transport:keep-running server-id run-id)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (debug:print-info 0 *default-log-port* "Starting the sync-back, keep alive thread in server for run-id=" run-id)
  (let* ((tdbdat      (tasks:open-db))
	 (server-start-time (current-seconds))
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
			      sdat
                              (begin
				(debug:print-info 0 *default-log-port* "Still waiting, last-sdat=" last-sdat)
                                (sleep 4)
				(if (> (- (current-seconds) start-time) 120) ;; been waiting for two minutes
				    (begin
				      (debug:print-error 0 *default-log-port* "transport appears to have died, exiting server " server-id " for run " run-id)
				      (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id "failed to start, never received server alive signature")
				      (exit))
				    (loop start-time
					  (equal? sdat last-sdat)
					  sdat)))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0)
	 (server-timeout (server:get-timeout)))
    (let loop ((count         0)
	       (server-state 'available)
	       (bad-sync-count 0))

      ;; Use this opportunity to sync the inmemdb to db
      (if *inmemdb* 
	  (let ((start-time (current-milliseconds))
		(sync-time  #f)
		(rem-time   #f))
	    ;; inmemdb is a dbstruct
	    (condition-case
	     (db:sync-touched *inmemdb* *run-id* force-sync: #t)
	     ((sync-failed)(cond
			    ((> bad-sync-count 10) ;; time to give up
			     (http-transport:server-shutdown server-id port))
			    (else ;; (> bad-sync-count 0)  ;; we've had a fail or two, delay and loop
			     (thread-sleep! 5)
			     (loop count server-state (+ bad-sync-count 1)))))
	     ((exn)
	      (debug:print-error 0 *default-log-port* "error from sync code other than 'sync-failed. Attempting to gracefully shutdown the server")
	      (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id " http-transport:keep-running crashed")
	      (exit)))
	    (set! sync-time  (- (current-milliseconds) start-time))
	    (set! rem-time (quotient (- 4000 sync-time) 1000))
	    (debug:print 4 *default-log-port* "SYNC: time= " sync-time ", rem-time=" rem-time)
	    
	    (if (and (<= rem-time 4)
		     (> rem-time 0))
		(thread-sleep! rem-time)
		(thread-sleep! 4))) ;; fallback for if the math is changed ...

	  ;;
	  ;; no *inmemdb* yet, set running after our first pass through and start the db
	  ;;
	  (if (eq? server-state 'available)
	      (let ((new-server-id (tasks:server-am-i-the-server? (db:delay-if-busy tdbdat) run-id))) ;; try to ensure no double registering of servers
		(if (equal? new-server-id server-id)
		    (begin
		      (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "dbprep")
		      (thread-sleep! 0.5) ;; give some margin for queries to complete before switching from file based access to server based access
		      (set! *inmemdb*  (db:setup)) ;;  run-id))
		      ;; force initialization
		      ;; (db:get-db *inmemdb* #t)
		      ;; (db:get-db *inmemdb* run-id)
		      (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "running"))
		    (begin ;; gotta exit nicely
		      (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "collision")
		      (http-transport:server-shutdown server-id port))))))
      
      (if (< count 1) ;; 3x3 = 9 secs aprox
	  (loop (+ count 1) 'running bad-sync-count))
      
      ;; Check that iface and port have not changed (can happen if server port collides)
      (mutex-lock! *heartbeat-mutex*)
      (set! sdat *server-info*)
      (mutex-unlock! *heartbeat-mutex*)
      
      (if (or (not (equal? sdat (list iface port)))
	      (not server-id))
	  (begin 
	    (debug:print-info 0 *default-log-port* "interface changed, refreshing iface and port info")
	    (set! iface (car sdat))
	    (set! port  (cadr sdat))))
      
      ;; Transfer *last-db-access* to last-access to use in checking that we are still alive
      (mutex-lock! *heartbeat-mutex*)
      (set! last-access *last-db-access*)
      (mutex-unlock! *heartbeat-mutex*)

      ;; (debug:print 11 *default-log-port* "last-access=" last-access ", server-timeout=" server-timeout)
      ;;
      ;; no_traffic, no running tests, if server 0, no running servers
      ;;
      ;; (let ((wait-on-running (configf:lookup *configdat* "server" b"wait-on-running"))) ;; wait on running tasks (if not true then exit on time out)
      ;;
      (let* ((hrs-since-start  (/ (- (current-seconds) server-start-time) 3600))
	     (adjusted-timeout (if (> hrs-since-start 1)
				   (- server-timeout (inexact->exact (round (* hrs-since-start 60))))  ;; subtract 60 seconds per hour
				   server-timeout)))
	(if (common:low-noise-print 120 "server timeout")
	    (debug:print-info 0 *default-log-port* "Adjusted server timeout: " adjusted-timeout))
	(if (and *server-run*
		 (> (+ last-access server-timeout)
		    (current-seconds)))
	    (begin
	      (if (common:low-noise-print 120 "server continuing")
		  (debug:print-info 0 *default-log-port* "Server continuing, seconds since last db access: " (- (current-seconds) last-access)))
	      ;;
	      ;; Consider implementing some smarts here to re-insert the record or kill self is
	      ;; the db indicates so
	      ;;
	      ;; (if (tasks:server-am-i-the-server? tdb run-id)
	      ;;     (tasks:server-set-state! tdb server-id "running"))
	      ;;
	      (loop 0 server-state bad-sync-count))
	    (http-transport:server-shutdown server-id port))))))
  
(define (http-transport:server-shutdown server-id port)
  (let ((tdbdat (tasks:open-db)))
    (debug:print-info 0 *default-log-port* "Starting to shutdown the server.")
    ;; need to delete only *my* server entry (future use)
    (set! *time-to-exit* #t)
    (if *inmemdb* (db:sync-touched *inmemdb* *run-id* force-sync: #t))
    ;;
    ;; start_shutdown
    ;;
    (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "shutting-down")
    (portlogger:open-run-close portlogger:set-port port "released")
    (thread-sleep! 5)
    (debug:print-info 0 *default-log-port* "Max cached queries was    " *max-cache-size*)
    (debug:print-info 0 *default-log-port* "Number of cached writes   " *number-of-writes*)
    (debug:print-info 0 *default-log-port* "Average cached write time "
		      (if (eq? *number-of-writes* 0)
			  "n/a (no writes)"
			  (/ *writes-total-delay*
			     *number-of-writes*))
		      " ms")
    (debug:print-info 0 *default-log-port* "Number non-cached queries "  *number-non-write-queries*)
    (debug:print-info 0 *default-log-port* "Average non-cached time   "
		      (if (eq? *number-non-write-queries* 0)
			  "n/a (no queries)"
			  (/ *total-non-write-delay* 
			     *number-non-write-queries*))
		      " ms")
    (debug:print-info 0 *default-log-port* "Server shutdown complete. Exiting")
    (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id " http-transport:keep-running complete")
    (exit)))

;; all routes though here end in exit ...
;;
;; start_server? 
;;
(define (http-transport:launch run-id)
  (let* ((tdbdat (tasks:open-db)))
    (set! *run-id*   run-id)
    (if (args:get-arg "-daemonize")
	(begin
	  (daemon:ize)
	  (if *alt-log-file* ;; we should re-connect to this port, I think daemon:ize disrupts it
	      (begin
		(current-error-port *alt-log-file*)
		(current-output-port *alt-log-file*)))))
    (if (server:check-if-running run-id)
	(begin
	  (debug:print 0 *default-log-port* "INFO: Server for run-id " run-id " already running")
	  (exit 0)))
    (let loop ((server-id (tasks:server-lock-slot (db:delay-if-busy tdbdat) run-id))
	       (remtries  4))
      (if (not server-id)
	  (if (> remtries 0)
	      (begin
		(thread-sleep! 2)
		(loop (tasks:server-lock-slot (db:delay-if-busy tdbdat) run-id)
		      (- remtries 1)))
	      (begin
		;; since we didn't get the server lock we are going to clean up and bail out
		(debug:print-info 2 *default-log-port* "INFO: server pid=" (current-process-id) ", hostname=" (get-host-name) " not starting due to other candidates ahead in start queue")
		(tasks:server-delete-records-for-this-pid (db:delay-if-busy tdbdat) " http-transport:launch")
		))
	  (let* ((th2 (make-thread (lambda ()
				     (debug:print-info 0 *default-log-port* "Server run thread started")
				     (http-transport:run 
				      (if (args:get-arg "-server")
					  (args:get-arg "-server")
					  "-")
				      run-id
				      server-id)) "Server run"))
		 (th3 (make-thread (lambda ()
				     (debug:print-info 0 *default-log-port* "Server monitor thread started")
				     (http-transport:keep-running server-id run-id))
				   "Keep running")))
	    (thread-start! th2)
	    (thread-sleep! 0.25) ;; give the server time to settle before starting the keep-running monitor.
	    (thread-start! th3)
	    (set! *didsomething* #t)
	    (thread-join! th2)
	    (exit))))))

(define (http:ping run-id host-port)
  (let* ((server-dat (http-transport:client-connect (car host-port)(cadr host-port)))
	 (login-res  (rmt:login-no-auto-client-setup server-dat run-id)))
    (if (and (list? login-res)
	     (car login-res))
	(begin
	  (print "LOGIN_OK")
	  (exit 0))
	(begin
	  (print "LOGIN_FAILED")
	  (exit 1)))))

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
	       "<tr><td>Max cached queries</td>        <td>" *max-cache-size* "</td></tr>"
	       "<tr><td>Number of cached writes</td>   <td>" *number-of-writes* "</td></tr>"
	       "<tr><td>Average cached write time</td> <td>" (if (eq? *number-of-writes* 0)
								 "n/a (no writes)"
								 (/ *writes-total-delay*
								    *number-of-writes*))
	       " ms</td></tr>"
	       "<tr><td>Number non-cached queries</td> <td>"  *number-non-write-queries* "</td></tr>"
	       "<tr><td>Average non-cached time</td>   <td>" (if (eq? *number-non-write-queries* 0)
								 "n/a (no queries)"
								 (/ *total-non-write-delay* 
								    *number-non-write-queries*))
	       " ms</td></tr>"
	       "<tr><td>Last access</td><td>"              (seconds->time-string *last-db-access*) "</td></tr>"
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
