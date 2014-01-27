
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
(define (client:setup run-id #!key (numtries 3))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: failed to find megatest.config, exiting")
	    (exit))))
  ;; (push-directory *toppath*) ;; This is probably NOT needed 
  ;; clients get the sdb:qry proc created here
  ;; (if (not sdb:qry)
  ;;     (begin
  ;;       (set! sdb:qry (make-sdb:qry (conc *toppath* "/db/strings.db"))) ;; we open the normalization helpers here
  ;;       (sdb:qry 'setup #f)))
  (let ((hostinfo (and run-id (hash-table-ref/default *runremote* run-id #f))))
    (debug:print-info 11 "for run-id=" run-id ", *transport-type* is " *transport-type*)
    (if hostinfo
	hostinfo ;; have hostinfo - just return it
	(let* ((hostinfo  (open-run-close tasks:get-server tasks:open-db run-id))
	       (transport (if hostinfo 
			      (string->symbol (tasks:hostinfo-get-transport hostinfo))
			      'http)))
	  (if (not hostinfo)
	      (begin
		(debug:print 0 "ERROR: Expected to be able to connect to a server by now. No server available for run-id = " run-id)
		(exit 1)))
	  (hash-table-set! *runremote* run-id hostinfo)
	  (debug:print-info 11 "CLIENT SETUP, hostinfo=" hostinfo)
	  (debug:print-info 11 "Using transport type of " transport (if hostinfo (conc " to connect to " hostinfo) ""))
	  (case *transport-type* 
	    ;; ((fs)(if (not *megatest-db*)(set! *megatest-db* (open-db))))
	    ((http)
	     ;; this saves the hostinfo in the *runremote* hash and returns it
	     (http-transport:client-connect run-id 
					    (tasks:hostinfo-get-interface hostinfo)
					    (tasks:hostinfo-get-port hostinfo)))
	    ((zmq)
	     (zmq-transport:client-connect (tasks:hostinfo-get-interface hostinfo)
					   (tasks:hostinfo-get-port      hostinfo)
					   (tasks:hostinfo-get-pubport   hostinfo)))
	    (else  ;; default to fs
	     (debug:print 0 "ERROR: unrecognised transport type " *transport-type* " exiting now.")
	     (exit)))))))
;;	  (pop-directory)))

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

