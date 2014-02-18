
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
(define (client:setup run-id #!key (remaining-tries 10))
  (debug:print 0 "INFO: client:setup remaining-tries=" remaining-tries)
  (if (<= remaining-tries 0)
      (begin
	(debug:print 0 "ERROR: failed to start or connect to server for run-id " run-id)
	(exit 1))
      (let ((server-dat (and run-id (hash-table-ref/default *runremote* run-id #f))))
	(if server-dat
	    (let ((start-res (http-transport:client-connect run-id ;; NB// confusion over server-dat and connection result!
							    (tasks:hostinfo-get-interface server-dat)
							    (tasks:hostinfo-get-port      server-dat))))
	      (if start-res ;; sucessful login?
		  (begin
		    (hash-table-set! *runremote* run-id start-res)
		    start-res)
		  (begin    ;; login failed
		    (hash-table-delete! *runremote* run-id)
		    (open-run-close tasks:server-force-clean-run-record
				    tasks:open-db
				    run-id 
				    (tasks:hostinfo-get-interface server-dat)
				    (tasks:hostinfo-get-port      server-dat))
		    (thread-sleep! 5)
		    (client:setup run-id remaining-tries: (- remaining-tries 1)))))
	    (let* ((server-dat (open-run-close tasks:get-server tasks:open-db run-id)))
	      (if server-dat
		  (let ((start-res (http-transport:client-connect run-id
								  (tasks:hostinfo-get-interface server-dat)
								  (tasks:hostinfo-get-port      server-dat))))
		    (if start-res
			(begin
			  (hash-table-set! *runremote* run-id start-res)
			  start-res)
			(begin    ;; login failed
			  (hash-table-delete! *runremote* run-id)
			  (open-run-close tasks:server-force-clean-run-record
					  tasks:open-db
					  run-id 
					  (tasks:hostinfo-get-interface server-dat)
					  (tasks:hostinfo-get-port      server-dat))
			  (thread-sleep! 2)
			  (server:try-running run-id)
			  (thread-sleep! 5) ;; give server a little time to start up
			  (client:setup run-id remaining-tries: (- remaining-tries 1)))))
		  (begin    ;; no server registered
		    (thread-sleep! 2)
		    (server:try-running run-id)
		    (thread-sleep! 5) ;; give server a little time to start up
		    (client:setup run-id remaining-tries: (- remaining-tries 1)))))))))

;; keep this as a function to ease future 
(define (client:start run-id server-info)
  (http-transport:client-connect run-id 
				 (tasks:hostinfo-get-interface server-info)
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

