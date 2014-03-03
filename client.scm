
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
(define (client:setup run-id #!key (remaining-tries 10) (failed-connects 0))
  (if (<= remaining-tries 0)
      (begin
	(debug:print 0 "ERROR: failed to start or connect to server for run-id " run-id)
	(exit 1))
      (let ((host-info (hash-table-ref/default *runremote* run-id #f)))
	(debug:print-info 0 "client:setup host-info=" host-info ", remaining-tries=" remaining-tries)
	(thread-sleep! 1) ;; try to avoid race conditons
	(if host-info
	    (let* ((iface     (car  host-info))
		   (port      (cadr host-info))
		   (start-res (http-transport:client-connect iface port))
		   ;; (ping-res  (server:ping-server run-id iface port))
		   (ping-res  (rmt:login-no-auto-client-setup start-res run-id)))
	      (if ping-res   ;; sucessful login?
		    start-res)  ;; return the server info
		  (if (member remaining-tries '(3 4 6))
		      (begin    ;; login failed
			(debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" host-info)
			(hash-table-delete! *runremote* run-id)
			(open-run-close tasks:server-force-clean-run-record
			 		tasks:open-db
			 		run-id 
			 		(car  host-info)
			 		(cadr host-info)
					" client:setup (host-info=#t)")
			(thread-sleep! 5)
			(client:setup run-id remaining-tries: 10)) ;; (- remaining-tries 1)))
		      (begin
			(debug:print 25 "INFO: client:setup failed to connect, start-res=" start-res ", run-id=" run-id ", host-info=" host-info)
			(thread-sleep! 5)
			(client:setup run-id remaining-tries: (- remaining-tries 1))))))
	    ;; YUK: rename server-dat here
		    (if new-dat
			new-dat
	      (debug:print-info 0 "client:setup server-dat=" server-dat ", remaining-tries=" remaining-tries)
		  (let* ((iface     (tasks:hostinfo-get-interface server-dat))
			 (port      (tasks:hostinfo-get-port      server-dat))
			 (start-res (http-transport:client-connect iface port))
			 ;; (ping-res  (server:ping-server run-id iface port))
			 (ping-res  (rmt:login-no-auto-client-setup start-res run-id)))
			(if (member remaining-tries '(2 5))
			    (begin    ;; login failed
			      (debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
			  ;;(debug:print 0 "INFO: login failed in client:setup with no existing server-dat: " server-dat ", new-dat: " new-dat ", and server-info: " server-info ", cleaning out records and then trying again")
			      (hash-table-delete! *runremote* run-id)
			      (open-run-close tasks:server-force-clean-run-record
					      tasks:open-db
					      run-id 
					      (tasks:hostinfo-get-interface server-dat)
					      (tasks:hostinfo-get-port      server-dat)
					      " client:setup (server-dat = #t)")
			      (server:try-running run-id)
			      (thread-sleep! 10) ;; give server a little time to start up
			      (client:setup run-id remaining-tries: 10)) ;; (- remaining-tries 1)))
			    (begin
			      (debug:print 25 "INFO: client:setup start-res=" start-res ", run-id=" run-id ", server-dat=" server-dat)
			      (thread-sleep! 5)
			      (client:setup run-id remaining-tries: (- remaining-tries 1))))))
		  (begin    ;; no server registered
		    (if (eq? remaining-tries 2)
			(begin
			  ;; (open-run-close tasks:server-clean-out-old-records-for-run-id tasks:open-db run-id " client:setup (server-dat=#f)")
			  (client:setup run-id remaining-tries: 10))
			(begin
			  (debug:print 25 "INFO: client:setup start-res (not defined here), run-id=" run-id ", server-dat=" server-dat)
			  (if (< (open-run-close tasks:num-in-available-state tasks:open-db run-id) 3)
			      (begin
				;; (open-run-close tasks:server-clean-out-old-records-for-run-id tasks:open-db run-id " client:setup (server-dat=#f)")
				(server:try-running run-id)))
			  (thread-sleep! 10) ;; give server a little time to start up
			  (client:setup run-id remaining-tries: (- remaining-tries 1)))))))))))

;; keep this as a function to ease future 
(define (client:start run-id server-info)
  (http-transport:client-connect (tasks:hostinfo-get-interface server-info)
				 (tasks:hostinfo-get-port server-info)))

;; client:signal-handler
(define (client:signal-handler signum)
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

