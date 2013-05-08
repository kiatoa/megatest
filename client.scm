
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

(use spiffy uri-common intarweb http-client spiffy-request-vars uri-common intarweb)

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

;; client:login serverdat
(define (client:login serverdat)
  (cdb:login serverdat *toppath* (client:get-signature)))

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
;;   2. We are a run tests, list runs or other interactive process and we mush figure out
;;      *transport-type* and *runremote* from the monitor.db
;;
;; client:setup
(define (client:setup #!key (numtries 50))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: failed to find megatest.config, exiting")
	    (exit))))
  (debug:print-info 11 "*transport-type* is " *transport-type* ", *runremote* is " *runremote*)
  (let* ((hostinfo  (if (not *transport-type*) ;; If we dont' already have transport type set then figure it out
			(open-run-close tasks:get-best-server tasks:open-db)
			#f)))
    ;; if have hostinfo then extract the transport type 
    ;; else fall back to fs
    (debug:print-info 11 "CLIENT SETUP, hostinfo=" hostinfo)
    (set! *transport-type* (if hostinfo 
    			       (string->symbol (tasks:hostinfo-get-transport hostinfo))
			       'fs))
    ;; ;; DEBUG STUFF
    ;; (if (eq? *transport-type* 'fs)(begin (print "ERROR!!!!!!! refusing to run with transport " *transport-type*)(exit 99)))
    
    (debug:print-info 11 "Using transport type of " *transport-type* (if hostinfo (conc " to connect to " hostinfo) ""))
    (case *transport-type* 
      ((fs)(if (not *megatest-db*)(set! *megatest-db* (open-db))))
      ((http)
       (http-transport:client-connect (tasks:hostinfo-get-interface hostinfo)
				      (tasks:hostinfo-get-port hostinfo)))
      ((zmq)
       (zmq-transport:client-connect (tasks:hostinfo-get-interface hostinfo)
				     (tasks:hostinfo-get-port      hostinfo)
				     (tasks:hostinfo-get-pubport   hostinfo)))
      (else  ;; default to fs
       (debug:print 0 "ERROR: unrecognised transport type " *transport-type* " attempting to continue with fs")
       (set! *transport-type* 'fs)
       (set! *megatest-db*    (open-db))))))

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
(define (client:launch)
  (set-signal-handler! signal/int client:signal-handler)
   (if (client:setup)
       (debug:print-info 2 "connected as client")
       (begin
	 (debug:print 0 "ERROR: Failed to connect as client")
	 (exit))))

