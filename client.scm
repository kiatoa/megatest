
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; C L I E N T S
;;======================================================================

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
;; (use zmq)

(import (prefix sqlite3 sqlite3:))

(use spiffy uri-common intarweb http-client spiffy-request-vars uri-common intarweb directory-utils)

(declare (unit client))

(declare (uses common))
(declare (uses db))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

;; client:get-signature
(define (client:get-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (conc (get-host-name) " " (current-process-id))))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; Not currently used! But, I think it *should* be used!!!
(define (client:logout serverdat)
  (let ((ok (and (socket? serverdat)
		 (cdb:logout serverdat *toppath* (client:get-signature)))))
    ok))

(define (client:connect iface port)
  (case (server:get-transport)
    ((rpc)  (rpc:client-connect  iface port))
    ((http) (http:client-connect iface port))
    ((zmq)  (zmq:client-connect  iface port))
    (else   (rpc:client-connect  iface port))))

(define (client:login-no-auto-setup server-info run-id)
  (case (server:get-transport)
    ((rpc)  (rpc:login-no-auto-client-setup server-info run-id))
    ((http) (rmt:login-no-auto-client-setup server-info run-id))
    (else   (rpc:login-no-auto-client-setup server-info run-id))))

(define (client:setup  run-id #!key (remaining-tries 10) (failed-connects 0))
  (case (server:get-transport)
    ((rpc) (rpc-transport:client-setup run-id)) ;;(client:setup-rpc run-id))
    ((http)(client:setup-http run-id))
    (else  (rpc-transport:client-setup run-id)))) ;; (client:setup-rpc run-id))))

;; (define (client:setup-rpc run-id)
;;   (debug:print 0 "INFO: client:setup remaining-tries=" remaining-tries)
;;   (if (<= remaining-tries 0)
;;       (begin
;; 	(debug:print 0 "ERROR: failed to start or connect to server for run-id " run-id)
;; 	(exit 1))
;;       (let ((host-info (hash-table-ref/default *runremote* run-id #f)))
;; 	(debug:print-info 0 "client:setup host-info=" host-info ", remaining-tries=" remaining-tries)
;; 	(if host-info
;; 	    (let* ((iface     (car  host-info))
;; 		   (port      (cadr host-info))
;; 		   (start-res (client:connect iface port))
;; 		   ;; (ping-res  (server:ping-server run-id iface port))
;; 		   (ping-res  (client:login-no-auto-setup start-res run-id)))
;; 	      (if ping-res   ;; sucessful login?
;; 		  (begin
;; 		    (hash-table-set! *runremote* run-id start-res)
;; 		    start-res)  ;; return the server info
;; 		  (if (member remaining-tries '(3 4 6))
;; 		      (begin    ;; login failed
;; 			(debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" host-info)
;; 			(hash-table-delete! *runremote* run-id)
;; 			(open-run-close tasks:server-force-clean-run-record
;; 			 		tasks:open-db
;; 			 		run-id 
;; 			 		(car  host-info)
;; 			 		(cadr host-info)
;; 					" client:setup (host-info=#t)")
;; 			(thread-sleep! 5)
;; 			(client:setup run-id remaining-tries: 10)) ;; (- remaining-tries 1)))
;; 		      (begin
;; 			(debug:print 25 "INFO: client:setup failed to connect, start-res=" start-res ", run-id=" run-id ", host-info=" host-info)
;; 			(thread-sleep! 5)
;; 			(client:setup run-id remaining-tries: (- remaining-tries 1))))))
;; 	    ;; YUK: rename server-dat here
;; 	    (let* ((server-dat (open-run-close tasks:get-server tasks:open-db run-id)))
;; 	      (debug:print-info 0 "client:setup server-dat=" server-dat ", remaining-tries=" remaining-tries)
;; 	      (if server-dat
;; 		  (let* ((iface     (tasks:hostinfo-get-interface server-dat))
;; 			 (port      (tasks:hostinfo-get-port      server-dat))
;; 			 (start-res (http-transport:client-connect iface port))
;; 			 ;; (ping-res  (server:ping-server run-id iface port))
;; 			 (ping-res  (rmt:login-no-auto-client-setup start-res run-id)))
;; 		    (if start-res
;; 			(begin
;; 			  (hash-table-set! *runremote* run-id start-res)
;; 			  start-res)
;; 			(if (member remaining-tries '(2 5))
;; 			    (begin    ;; login failed
;; 			      (debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
;; 			      (hash-table-delete! *runremote* run-id)
;; 			      (open-run-close tasks:server-force-clean-run-record
;; 					      tasks:open-db
;; 					      run-id 
;; 					      (tasks:hostinfo-get-interface server-dat)
;; 					      (tasks:hostinfo-get-port      server-dat)
;; 					      " client:setup (server-dat = #t)")
;; 			      (thread-sleep! 2)
;; 			      (server:try-running run-id)
;; 			      (thread-sleep! 10) ;; give server a little time to start up
;; 			      (client:setup run-id remaining-tries: 10)) ;; (- remaining-tries 1)))
;; 			    (begin
;; 			      (debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
;; 			      (thread-sleep! 5)
;; 			      (client:setup run-id remaining-tries: (- remaining-tries 1))))))
;; 		  (begin    ;; no server registered
;; 		    (if (eq? remaining-tries 2)
;; 			(begin
;; 			  ;; (open-run-close tasks:server-clean-out-old-records-for-run-id tasks:open-db run-id " client:setup (server-dat=#f)")
;; 			  (client:setup run-id remaining-tries: 10))
;; 			(begin
;; 			  (thread-sleep! 2) 
;; 			  (debug:print 25 "INFO: client:setup start-res (not defined here), run-id=" run-id ", server-dat=" server-dat)
;; 			  (if (< (open-run-close tasks:num-in-available-state tasks:open-db run-id) 3)
;; 			      (begin
;; 				;; (open-run-close tasks:server-clean-out-old-records-for-run-id tasks:open-db run-id " client:setup (server-dat=#f)")
;; 				(server:try-running run-id)))
;; 			  (thread-sleep! 10) ;; give server a little time to start up
;; 			  (client:setup run-id remaining-tries: (- remaining-tries 1)))))))))))

;; Do all the connection work, look up the transport type and set up the
;; connection if required.
;;
;; There are two scenarios. 
;;   1. We are a test manager and we received *transport-type* and *runremote* via cmdline
;;   2. We are a run tests, list runs or other interactive process and we must figure out
;;      *transport-type* and *runremote* from the monitor.db
;;
;; client:setup
;;
;; lookup_server, need to remove *runremote* stuff
;;
(define (client:setup-http run-id #!key (remaining-tries 10) (failed-connects 0))
  (debug:print-info 2 "client:setup remaining-tries=" remaining-tries)
  (let* ((tdbdat (tasks:open-db)))
    (if (<= remaining-tries 0)
	(begin
	  (debug:print 0 "ERROR: failed to start or connect to server for run-id " run-id)
	  (exit 1))
	(let* ((server-dat (tasks:get-server (db:delay-if-busy tdbdat) run-id)))
	  (debug:print-info 4 "client:setup server-dat=" server-dat ", remaining-tries=" remaining-tries)
	  (if server-dat
	      (let* ((iface     (tasks:hostinfo-get-interface server-dat))
		     (hostname  (tasks:hostinfo-get-hostname  server-dat))
		     (port      (tasks:hostinfo-get-port      server-dat))
		     (start-res (case *transport-type*
				  ((http)(http-transport:client-connect iface port))
				  ((nmsg)(nmsg-transport:client-connect hostname port))))
		     (ping-res  (case *transport-type* 
				  ((http)(rmt:login-no-auto-client-setup start-res run-id))
				  ((nmsg)(let ((logininfo (rmt:login-no-auto-client-setup start-res run-id)))
 					   (if logininfo
 					       (car (vector-ref logininfo 1))
 					       #f))))))
		(if (and start-res
			 ping-res)
		    (begin
		      (hash-table-set! *runremote* run-id start-res)
		      (debug:print-info 2 "connected to " (http-transport:server-dat-make-url start-res))
		      start-res)
		    (begin    ;; login failed but have a server record, clean out the record and try again
		      (debug:print-info 0 "client:setup, login failed, will attempt to start server ... start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
		      (case *transport-type* 
			((http)(http-transport:close-connections run-id)))
		      (hash-table-delete! *runremote* run-id)
		      (tasks:kill-server-run-id run-id)
		      (tasks:server-force-clean-run-record (db:delay-if-busy tdbdat)
							   run-id 
							   (tasks:hostinfo-get-interface server-dat)
							   (tasks:hostinfo-get-port      server-dat)
							   " client:setup (server-dat = #t)")
		      (if (> remaining-tries 8)
			  (thread-sleep! (+ 1 (random 5))) ;; spread out the starts a little
			  (thread-sleep! (+ 15 (random 20)))) ;; it isn't going well. give it plenty of time
		      (server:try-running run-id)
		      (thread-sleep! 5)   ;; give server a little time to start up
		      (client:setup run-id remaining-tries: (- remaining-tries 1))
		      )))
	      (begin    ;; no server registered
		(let ((num-available (tasks:num-in-available-state (db:dbdat-get-db tdbdat) run-id)))
		  (debug:print-info 0 "client:setup, no server registered, remaining-tries=" remaining-tries " num-available=" num-available)
		  (if (< num-available 2)
		      (server:try-running run-id))
		  (thread-sleep! (+ 5 (random (- 20 remaining-tries))))  ;; give server a little time to start up, randomize a little to avoid start storms.
		  (client:setup run-id remaining-tries: (- remaining-tries 1)))))))))

;; 	(let ((host-info (hash-table-ref/default *runremote* run-id #f)))
;; 	  (if host-info ;; this is a bit circular. the host-info *is* the start-res FIXME
;; 	      (let* ((iface     (http-transport:server-dat-get-iface host-info))
;; 		     (port      (http-transport:server-dat-get-port  host-info))
;; 		     (start-res (case *transport-type* 
;; 				  ((http)(http-transport:client-connect iface port))
;; 				  ((nmsg)(nmsg-transport:client-connect iface port)) ;; (http-transport:server-dat-get-socket host-info))
;; 				  (else #f)))
;; 		     (ping-res  (case *transport-type*
;; 				  ((http)(rmt:login-no-auto-client-setup start-res run-id))
;; 				  ((nmsg)(let ((logininfo (rmt:login-no-auto-client-setup start-res run-id)))
;; 					   (if logininfo
;; 					       (vector-ref (vector-ref logininfo 1) 1)
;; 					       #f)))
;; 				  (else #f))))
;; 		(if ping-res   ;; sucessful login?
;; 		    (begin
;; 		      (debug:print-info 2 "client:setup, ping is good using host-info=" host-info ", remaining-tries=" remaining-tries)
;; 		      start-res)  ;; return the server info
;; 		    ;; have host info but no ping. shutdown the current connection and try again
;; 		    (begin    ;; login failed
;; 		      (debug:print-info 1 "client:setup, ping is bad for start-res=" start-res " and *runremote*=" host-info)
;; 		      (case *transport-type*
;; 			((http)(http-transport:close-connections run-id)))
;; 		      (hash-table-delete! *runremote* run-id)
;; 		      (if (< remaining-tries 8)
;; 			  (thread-sleep! 5)
;; 			  (thread-sleep! 1))
;; 		      (client:setup run-id remaining-tries: (- remaining-tries 1)))))
;; 	      ;; YUK: rename server-dat here
;; 

;; keep this as a function to ease future 
(define (client:start run-id server-info)
  (http-transport:client-connect (tasks:hostinfo-get-interface server-info)
				 (tasks:hostinfo-get-port server-info)))

;; client:signal-handler
(define (client:signal-handler signum)
  (signal-mask! signum)
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

;; client:launch
;; Need to set the signal handler somewhere other than here as this
;; routine will go away.
;;
(define (client:launch run-id)
  (set-signal-handler! signal/int client:signal-handler)
  (if (client:setup run-id)
      (debug:print-info 2 "connected as client")
      (begin
	(debug:print 0 "ERROR: Failed to connect as client")
	(exit))))

