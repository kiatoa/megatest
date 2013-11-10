
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

(define (server:get-best-guess-address hostname)
  (let ((res #f))
    (for-each 
     (lambda (adr)
       (if (not (eq? (u8vector-ref adr 0) 127))
	   (set! res adr)))
     (vector->list (hostinfo-addresses (hostname->hostinfo hostname))))
    (string-intersperse 
     (map number->string
	  (u8vector->list
	   (if res res (hostname->ip hostname)))) ".")))

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
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
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
    (handle-directory spiffy-directory-listing)
    ;; http-transport:handle-directory) ;; simple-directory-handler)
    ;; Setup the web server and a /ctrl interface
    ;;
    (vhost-map `(((* any) . ,(lambda (continue)
			       ;; open the db on the first call 
			       (if (not db)(set! db (open-db)))
			       (let* (($   (request-vars source: 'both))
				      (dat ($ 'dat))
				      (res #f))
				 (cond
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
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ ""))
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
  (handle-exceptions
   exn
   (begin
     (print-error-message exn)
     (if (< portnum 9000)
	 (begin 
	   (debug:print 0 "WARNING: failed to start on portnum: " portnum ", trying next port")
	   (thread-sleep! 0.1)
	   ;; (open-run-close tasks:remove-server-records tasks:open-db)
	   (open-run-close tasks:server-delete tasks:open-db ipaddrstr portnum)
	   (http-transport:try-start-server ipaddrstr (+ portnum 1)))
	 (print "ERROR: Tried and tried but could not start the server")))
   ;; any error in following steps will result in a retry
   (set! *runremote* (list ipaddrstr portnum))
   ;; (open-run-close tasks:remove-server-records tasks:open-db)
   (open-run-close tasks:server-register 
		   tasks:open-db 
		   (current-process-id)
		   ipaddrstr portnum 0 'startup 'http)
   (debug:print 1 "INFO: Trying to start server on " ipaddrstr ":" portnum)
   ;; This starts the spiffy server
   ;; NEED WAY TO SET IP TO #f TO BIND ALL
   (start-server bind-address: ipaddrstr port: portnum)
   (open-run-close tasks:server-delete tasks:open-db ipaddrstr portnum)
   (debug:print 1 "INFO: server has been stopped")))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;;======================================================================
;; C L I E N T S
;;======================================================================

(define *http-mutex* (make-mutex))

;; (system "megatest -list-servers | grep alive || megatest -server - -daemonize && sleep 4")

;; <html>
;; <head></head>
;; <body>1 Hello, world! Goodbye Dolly</body></html>
;; Send msg to serverdat and receive result
(define (http-transport:client-send-receive serverdat msg #!key (numretries 30))
  (let* (;; (url        (http-transport:make-server-url serverdat))
	 (fullurl    (if (list? serverdat)
			 (caddr serverdat)
			 (begin
			   (debug:print 0 "FATAL ERROR: http-transport:client-send-receive called with no server info")
			   (exit 1)))) ;; (conc url "/ctrl")) ;; (conc url "/?dat=" msg)))
	 (res        #f))
    (handle-exceptions
     exn
     (begin
       (print "ERROR IN http-transport:client-send-receive " ((condition-property-accessor 'exn 'message) exn))
       (thread-sleep! 2)
       (if (> numretries 0)
	   (http-transport:client-send-receive serverdat msg numretries: (- numretries 1))))
     (begin
       (debug:print-info 11 "fullurl=" fullurl "\n")
       ;; set up the http-client here
       (max-retry-attempts 5)
       ;; consider all requests indempotent
       (retry-request? (lambda (request)
			 #t))   ;;  		 (thread-sleep! (/ (if (> numretries 100) 100 numretries) 10))
       ;; (set! numretries (- numretries 1))
       ;;  		 #t))
       ;; send the data and get the response
       ;; extract the needed info from the http data and 
       ;; process and return it.
       (let* ((send-recieve (lambda ()
			      (mutex-lock! *http-mutex*)
			      (set! res (with-input-from-request 
					 fullurl 
					 (list (cons 'dat msg)) 
					 read-string))
			      (close-all-connections!) 
			      (mutex-unlock! *http-mutex*)))
	      (time-out     (lambda ()
			      (thread-sleep! 45)
			      (if (not res)
				  (begin
				    (debug:print 0 "WARNING: communication with the server timed out.")
				    (mutex-unlock! *http-mutex*)
				    (http-transport:client-send-receive serverdat msg numretries: (- numretries 1))
				    (if (< numretries 3) ;; on last try just exit
					(begin
					  (debug:print 0 "ERROR: communication with the server timed out. Giving up.")
					  (exit 1)))))))
	      (th1 (make-thread send-recieve "with-input-from-request"))
	      (th2 (make-thread time-out     "time out")))
	 (thread-start! th1)
	 (thread-start! th2)
	 (thread-join! th1)
	 (thread-terminate! th2)
	 (debug:print-info 11 "got res=" res)
	 (let ((match (string-search (regexp "<body>(.*)<.body>") res)))
	   (debug:print-info 11 "match=" match)
	   (let ((final (cadr match)))
	     (debug:print-info 11 "final=" final)
	     final)))))))

(define (http-transport:client-connect iface port)
  (let* ((login-res   #f)
	 (uri-dat     (make-request method: 'POST uri: (uri-reference (conc "http://" iface ":" port "/ctrl"))))
	 (serverdat   (list iface port uri-dat)))
    (set! login-res (client:login serverdat))
    (if (and (not (null? login-res))
	     (car login-res))
	(begin
	  (debug:print-info 2 "Logged in and connected to " iface ":" port)
	  (set! *runremote* serverdat)
	  serverdat)
	(begin
	  (debug:print-info 0 "ERROR: Failed to login or connect to " iface ":" port)
	  (exit 1)))))
;; 	  (set! *runremote* #f)
;; 	  (set! *transport-type* 'fs)
;; 	  #f))))


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
                          (if sdat
			      sdat
                              (begin
                                (sleep 4)
                                (loop))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0)
	 (tdb         (tasks:open-db))
	 (spid        ;;(open-run-close tasks:server-get-server-id tasks:open-db #f iface port #f))
	   (tasks:server-get-server-id tdb #f iface port #f))
	 (server-timeout (let ((tmo (config-lookup  *configdat* "server" "timeout")))
			   (if (and (string? tmo)
				    (string->number tmo))
			       (* 60 60 (string->number tmo))
			       ;; default to three days
			       (* 3 24 60 60)))))
    (debug:print-info 2 "server-timeout: " server-timeout ", server pid: " spid " on " iface ":" port)
    (let loop ((count 0))
      (thread-sleep! 4) ;; no need to do this very often
      ;; NB// sync currently does NOT return queue-length
      (let () ;; (queue-len (cdb:client-call server-info 'sync #t 1)))
      ;; (print "Server running, count is " count)
        (if (< count 1) ;; 3x3 = 9 secs aprox
            (loop (+ count 1)))
        
	;; Check that iface and port have not changed (can happen if server port collides)
	(mutex-lock! *heartbeat-mutex*)
	(set! sdat *runremote*)
	(mutex-unlock! *heartbeat-mutex*)

	(if (or (not (equal? sdat (list iface port)))
		(not spid))
	    (begin 
	      (debug:print-info 0 "interface changed, refreshing iface and port info")
	      (set! iface (car sdat))
	      (set! port  (cadr sdat))
	      (set! spid  (tasks:server-get-server-id tdb #f iface port #f))))

        ;; NOTE: Get rid of this mechanism! It really is not needed...
        ;; (open-run-close tasks:server-update-heartbeat tasks:open-db spid)
        (tasks:server-update-heartbeat tdb spid)
      
        ;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
        (mutex-lock! *heartbeat-mutex*)
        (set! last-access *last-db-access*)
        (mutex-unlock! *heartbeat-mutex*)
	;; (debug:print 11 "last-access=" last-access ", server-timeout=" server-timeout)
        (if (and *server-run*
		 (> (+ last-access server-timeout)
		    (current-seconds)))
            (begin
              (debug:print-info 0 "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
              (loop 0))
            (begin
              (debug:print-info 0 "Starting to shutdown the server.")
              ;; need to delete only *my* server entry (future use)
              (set! *time-to-exit* #t)
              (open-run-close tasks:server-deregister-self tasks:open-db (get-host-name))
              (thread-sleep! 1)
              (debug:print-info 0 "Max cached queries was    " *max-cache-size*)
	      (debug:print-info 0 "Number of cached writes   " *number-of-writes*)
	      (debug:print-info 0 "Average cached write time "
				(if (eq? *number-of-writes* 0)
				    "n/a (no writes)"
				    (/ *writes-total-delay*
				       *number-of-writes*))
				" ms")
	      (debug:print-info 0 "Number non-cached queries "  *number-non-write-queries*)
	      (debug:print-info 0 "Average non-cached time   "
				(if (eq? *number-non-write-queries* 0)
				    "n/a (no queries)"
				    (/ *total-non-write-delay* 
				       *number-non-write-queries*))
				" ms")
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
  (if (args:get-arg "-daemonize")
      (daemon:ize))
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
	      ;; This is were we set up the database connections
	      (set! *db* (open-db))
	      (set! *inmemdb* (open-in-mem-db))
	      (db:sync-to *db* *inmemdb*)
	      (thread-start! th2)
	      (thread-start! th3)
	      (thread-start! th1)
	      (set! *didsomething* #t)
	      (thread-join! th2))
	    (debug:print 0 "ERROR: Failed to setup for megatest")))
    (exit)))

;; (use trace)
;; (trace http-transport:keep-running 
;;        tasks:server-update-heartbeat
;;        tasks:server-get-server-id)
;;        tasks:get-best-server
;;        http-transport:run
;;        http-transport:launch
;;        http-transport:try-start-server
;;        http-transport:client-send-receive
;;        http-transport:make-server-url
;;        tasks:server-register
;;        tasks:server-delete
;;        start-server
;;        hostname->ip
;;        with-input-from-request
;;        tasks:server-deregister-self)

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