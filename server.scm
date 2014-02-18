
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest directory-utils)
;; (use zmq)

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses synchash))
(declare (uses http-transport))
;; (declare (uses zmq-transport))
(declare (uses daemon))

(include "common_records.scm")
(include "db_records.scm")

(define (server:make-server-url hostport)
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

;; all routes though here end in exit ...
;;
;; start_server
;;
(define (server:launch run-id)
  ;; (if (server:check-if-running run-id)
  ;; a server is already running
  ;; (exit)
  (http-transport:launch run-id))

;; (define (server:launch-no-exit run-id)
;;   (if (server:check-if-running run-id)
;;       #t ;; if running
;;       (http-transport:launch run-id)))

;;======================================================================
;; Q U E U E   M A N A G E M E N T
;;======================================================================

;; We don't want to flush the queue if it was just flushed
(define *server:last-write-flush* (current-milliseconds))

;; Flush the queue every third of a second. Can we assume that setup-for-run 
;; has already been done?
;; (define (server:write-queue-handler)
;;   (if (setup-for-run)
;;       (let ((db (open-db)))
;; 	(let loop ()
;; 	  (let ((last-write-flush-time #f))
;; 	    (mutex-lock! *incoming-mutex*)
;; 	    (set! last-write-flush-time *server:last-write-flush*)
;; 	    (mutex-unlock! *incoming-mutex*)
;; 	    (if (> (- (current-milliseconds) last-write-flush-time) 10)
;; 		(begin
;; 		  (mutex-lock! *db:process-queue-mutex*)
;; 		  (db:process-cached-writes db)
;; 		  (mutex-unlock! *db:process-queue-mutex*)
;; 		  (thread-sleep! 0.005))))
;; 	  (loop)))
;;       (begin
;; 	(debug:print 0 "ERROR: failed to setup for Megatest in server:write-queue-handler")
;; 	(exit 1))))
    
;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;; Generate a unique signature for this server
(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))


;; When using zmq this would send the message back (two step process)
;; with spiffy or rpc this simply returns the return data to be returned
;; 
(define (server:reply return-addr query-sig success/fail result)
  (db:obj->string (vector success/fail query-sig result)))

(define (server:try-running run-id)
  (let ((cmdln (conc (if (getenv "MT_MEGATEST") (getenv "MT_MEGATEST") "megatest")
		     " -server - -run-id " run-id " &> " *toppath* "/db/" run-id ".log &")))
    (debug:print 0 "INFO: Starting server (" cmdln ") as none running ...")
    (push-directory *toppath*)
    (system cmdln)
    (pop-directory)))

(define (server:check-if-running run-id)
  (let loop ((server (open-run-close tasks:get-server tasks:open-db run-id))
	     (trycount 0))
    (thread-sleep! 2)
    (if server
	;; note: client:start will set *runremote*. this needs to be changed
	;;       also, client:start will login to the server, also need to change that.
	;;
	;; client:start returns #t if login was successful.
	;;
	(let ((res (client:start run-id server)))
	  ;; if the server didn't respond we must remove the record
	  (if res
	      res
	      (begin
		(open-run-close tasks:server-force-clean-running-records-for-run-id tasks:open-db run-id)
		res)))
	#f)))
