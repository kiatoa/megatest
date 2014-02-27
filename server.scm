
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
  (http-transport:launch run-id))

;;======================================================================
;; Q U E U E   M A N A G E M E N T
;;======================================================================

;; We don't want to flush the queue if it was just flushed
(define *server:last-write-flush* (current-milliseconds))

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

;; Given a run id start a server process    ### NOTE ### > file 2>&1 
;; if the run-id is zero and the target-host is set 
;; try running on that host
;;
(define  (server:run run-id)
  (let* ((target-host (configf:lookup *configdat* "server" "homehost" ))
	 (cmdln (conc (common:get-megatest-exe)
		      " -server - -run-id " run-id " >> " *toppath* "/db/" run-id ".log 2>&1 &")))
    (debug:print 0 "INFO: Starting server (" cmdln ") as none running ...")
    (push-directory *toppath*)
    (if target-host
	(begin
	  (set-environment-variable "TARGETHOST" target-host)
	  (system (conc "nbfake " cmdln)))
	(system cmdln))
    (pop-directory)))

;; kind start up of servers, wait 40 seconds before allowing another server for a given
;; run-id to be launched
(define (server:kind-run run-id)
  (let ((last-run-time (hash-table-ref/default *server-kind-run* run-id #f)))
    (if (or (not last-run-time)
	    (> (- (current-seconds) last-run-time) 40))
	(begin
	  (server:run run-id)
	  (hash-table-set! *server-kind-run* run-id (current-seconds))))))

;; The generic run a server command. Dispatches the call to server 0 if run-id != 0
;; 
(define (server:try-running run-id)
  (if (eq? run-id 0)
      (server:run run-id)
      (rmt:start-server run-id)))

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
		(open-run-close tasks:server-force-clean-running-records-for-run-id tasks:open-db run-id 
				" server:check-if-running")
		res)))
	#f)))

(define (server:ping-server run-id iface port)
  (with-input-from-pipe 
   (conc (common:get-megatest-exe) " -run-id " run-id " -ping " (conc iface ":" port))
   (lambda ()
     (let loop ((inl (read-line))
		(res "NOREPLY"))
       (if (eof-object? inl)
	   (case (string->symbol res)
	     ((NOREPLY)  #f)
	     ((LOGIN_OK) #t)
	     (else       #f))
	   (loop (read-line) inl))))))