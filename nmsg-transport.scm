
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

(define (nmsg-transport:make-server-url hostport #!key (bindall #f))
  (if (not hostport)
      #f
      (conc "tcp://" (if bindall "*" (car hostport)) ":" (cadr hostport))))

(define *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

(define (nmsg-transport:run dbstruct hostn run-id server-id)
  (debug:print 2 "Attempting to start the server ...")
  (let* ((start-port      (portlogger:open-run-close portlogger:find-port))
	 (server-thread   (make-thread (lambda ()
					 (nmsg-transport:try-start-server dbstruct run-id start-port server-id))
				       "server thread"))
	 (tdbdat          (tasks:open-db)))
    (thread-start! server-thread)
    (if (nmsg-transport:ping hostn start-port timeout: 2 expected-key: (current-process-id))
	(let ((interface (if (equal? hostn "-")(get-host-name) hostn)))
	  (tasks:server-set-interface-port (db:delay-if-busy tdbdat) server-id interface start-port)
	  (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "dbprep")
	  (set! *server-info* (list hostn start-port)) ;; probably not needed anymore? currently used by keep-running
	  (thread-sleep! 3) ;; give some margin for queries to complete before switching from file based access to server based access
	  (set! *inmemdb*  dbstruct)
	  (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "running")
	  (thread-start! (make-thread
			  (lambda ()(nmsg-transport:keep-running server-id))
			  "keep running"))
	  (thread-join! server-thread))
	(begin
	  (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id "failed to start, never received server alive signature")
	  (portlogger:open-run-close portlogger:set-failed start-port)
	  (nmsg-transport:run dbstruct hostn run-id server-id)))))

(define (nmsg-transport:try-start-server dbstruct run-id portnum server-id)
  (let ((repsoc (nn-socket 'rep)))
    (nn-bind repsoc (conc "tcp://*:" portnum))
    (let loop ((msg-in (nn-recv repsoc)))
      (cond
       ((equal? msg-in "quit")
	(nn-send repsoc "Ok, quitting"))
       ((and (>= (string-length msg-in) 4)
	     (equal? (substring msg-in 0 4) "ping"))
	(nn-send repsoc (conc (current-process-id)))
	(loop (nn-recv repsoc)))
       (else
	(let* ((dat    (db:string->obj msg-in transport: 'nmsg))
	       (cmd    (vector-ref dat 0))
	       (params (vector-ref dat 1))
	       (result (api:execute-requests dbstruct cmd params))
	       (newdat (db:obj->string result transport: 'nmsg)))
	  (nn-send repsoc newdat)
	  (loop (nn-recv repsoc))))))))


;; all routes though here end in exit ...
;;
(define (nmsg-transport:launch run-id)
  (let* ((tdbdat   (tasks:open-db))
	 (dbstruct (db:setup run-id))
	 (hostn    (or (args:get-arg "-server") "-")))
    (set! *run-id*   run-id)
    ;; with nbfake daemonize isn't really needed
    ;;
    ;; (if (args:get-arg "-daemonize")
    ;;     (begin
    ;;       (daemon:ize)
    ;;       (if *alt-log-file* ;; we should re-connect to this port, I think daemon:ize disrupts it
    ;;           (begin
    ;;     	(current-error-port *alt-log-file*)
    ;;     	(current-output-port *alt-log-file*)))))
    (if (server:check-if-running run-id)
	(begin
	  (debug:print-info 0 "Server for run-id " run-id " already running")
	  (exit 0)))
    (let loop ((server-id (tasks:server-lock-slot (db:delay-if-busy tdbdat) run-id))
	       (remtries  4))
      (if (not server-id)
	  (if (> remtries 0)
	      (begin
		(thread-sleep! 2)
		(if (not (server:check-if-running run-id))
		    (loop (tasks:server-lock-slot (db:delay-if-busy tdbdat) run-id)
			  (- remtries 1))
		    (begin
		      (debug:print-info 0 "Another server took the slot, exiting")
		      (exit 0))))
	      (begin
		;; since we didn't get the server lock we are going to clean up and bail out
		(debug:print-info 2 "INFO: server pid=" (current-process-id) ", hostname=" (get-host-name) " not starting due to other candidates ahead in start queue")
		(tasks:server-delete-records-for-this-pid (db:delay-if-busy tdbdat) " http-transport:launch")
		))
	  ;; locked in a server id, try to start up
	  (nmsg-transport:run dbstruct hostn run-id server-id))
      (set! *didsomething* #t)
      (exit))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

(define (nmsg-transport:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

;;======================================================================
;; C L I E N T S
;;======================================================================

;; ping the server at host:port
;;   return the open socket if successful (return-socket == #t)
;;   expect the key expected-key returned in payload
;;   send our-key or #f as payload
;;
(define (nmsg-transport:ping hostn port #!key (timeout 3)(return-socket #t)(expected-key #f)(our-key #f)(socket #f))
  ;; send a random number along with pid and check that we get it back
  (let* ((req     (or socket (nn-socket 'req)))
	 (host    (if (or (not hostn)
			  (equal? hostn "-")) ;; use localhost
		      (get-host-name)
		      hostn))
	 (success #f)
	 (keepwaiting #t)
	 (dat     (db:obj->string (vector "ping" our-key) transport: 'nmsg))
	 (ping    (make-thread
		   (lambda ()
		     (nn-send req dat)
		     (let* ((result  (nn-recv req))
			    (key     (vector-ref (db:string->obj result transport: 'nmsg) 1)))
		       (if (or (not expected-key) ;; just getting a reply is good enough then
			       (equal? key expected-key)) 
			   (begin
			     ;; (print "ping, success: received \"" result "\"")
			     (set! success #t))
			   (begin
			     ;; (print "ping, failed: received key \"" result "\"")
			     (set! keepwaiting #f)
			     (set! success #f)))))
		   "ping"))
	 (timeout (make-thread (lambda ()
				 (let loop ((count 0))
				   (thread-sleep! 1)
				   (print "still waiting after count seconds...")
				   (if (and keepwaiting (< count timeout)) ;; yes, this is very aproximate
				       (loop (+ count 1))))
				 (if keepwaiting
				     (begin
				       (print "timeout waiting for ping")
				       (thread-terminate! ping))))
			       "timeout")))
    (if (not socket)(nn-connect req (conc "tcp://" host ":" port)))
    (handle-exceptions
     exn
     (begin
       ;; (print-call-chain)
       ;; (print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       ;; (print "exn=" (condition->list exn))
       (debug:print-info 1 "ping failed to connect to " host ":" port))
     (thread-start! timeout)
     (thread-start! ping)
     (thread-join! ping)
     (if success (thread-terminate! timeout)))
    (if return-socket
	(if success req #f)
	(begin
	  (nn-close req) ;; should it be closed if we were handed a socket?
	  success))))

;; run nmsg-transport:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (nmsg-transport:keep-running server-id)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (let* ((server-info (let loop ()
                        (let ((sdat #f))
                          (mutex-lock! *heartbeat-mutex*)
                          (set! sdat *server-info*)
                          (mutex-unlock! *heartbeat-mutex*)
                          (if sdat 
			      (begin
				(debug:print-info 0 "keep-running got sdat=" sdat)
				sdat)
                              (begin
                                (thread-sleep! 0.5)
                                (loop))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0)
	 (tdbdat      (tasks:open-db))
	 (server-timeout (let ((tmo (configf:lookup  *configdat* "server" "timeout")))
			   (if (and (string? tmo)
				    (string->number tmo))
			       (* 60 60 (string->number tmo))
			       ;; (* 3 24 60 60) ;; default to three days
			       (* 60 1)         ;; default to one minute
			       ;; (* 60 60 25)      ;; default to 25 hours
			       ))))
    (print "Keep-running got server pid " server-id ", using iface " iface " and port " port)
    (let loop ((count 0))
      (thread-sleep! 4) ;; no need to do this very often
      ;; NB// sync currently does NOT return queue-length
      (let () ;; (queue-len (cdb:client-call server-info 'sync #t 1)))
      ;; (print "Server running, count is " count)
        (if (< count 1) ;; 3x3 = 9 secs aprox
            (loop (+ count 1)))
        
        ;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
        (mutex-lock! *heartbeat-mutex*)
        (set! last-access *last-db-access*)
        (mutex-unlock! *heartbeat-mutex*)
        (if (and *server-run*
	       (> (+ last-access server-timeout)
		  (current-seconds)))
            (begin
              (debug:print-info 0 "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
              (loop 0))
            (begin
              (debug:print-info 0 "Starting to shutdown the server.")
              (set! *time-to-exit* #t)
              (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id " http-transport:keep-running")
              (debug:print-info 0 "Server shutdown complete. Exiting")
              (exit)
	      ))))))

;;======================================================================
;; C L I E N T S
;;======================================================================

(define (nmsg-transport:client-connect iface portnum)
  (let* ((reqsoc      (nmsg-transport:ping iface portnum return-socket: #t)))
    (vector iface portnum #f #f #f (current-seconds) reqsoc)))

(define (nmsg-transport:client-api-send-receive run-id connection-info cmd param)
  (mutex-lock! *http-mutex*)
  (let ((packet  (vector cmd param))
	(reqsoc  (http-transport:server-dat-get-socket connection-info)))
    (nn-send reqsoc (db:obj->string packet transport: 'nmsg))
    (let ((res (db:string->obj (nn-recv reqsoc) transport: 'nmsg)))
      (mutex-unlock! *http-mutex*)
      res)))

;;======================================================================
;; J U N K 
;;======================================================================

;; DO NOT USE
;;
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

