
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
(declare (uses rpc-transport))
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
  (let ((transport (server:get-transport)))
    (case transport
      ((http) (http-transport:launch run-id))
      ((rpc)  (rpc-transport:launch run-id))
      (else   (debug:print 0 "ERROR: No known transport set, transport=" transport ", using rpc")
	      (rpc-transport:launch run-id)))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;; Get the transport
(define (server:get-transport)
  (if *transport-type*
      *transport-type*
      (let ((ttype (string->symbol
		    (or (args:get-arg "-transport")
			(configf:lookup *configdat* "server" "transport")
			"rpc"))))
	(set! *transport-type* ttype)
	ttype)))
	    
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
  (debug:print-info 11 "server:reply return-addr=" return-addr ", result=" result)
  ;; (send-message pubsock target send-more: #t)
  ;; (send-message pubsock 
  (case (server:get-transport)
    ((rpc)  (db:obj->string (vector success/fail query-sig result)))
    ((http) (db:obj->string (vector success/fail query-sig result)))
    ((zmq)
     (let ((pub-socket (vector-ref *runremote* 1)))
       (send-message pub-socket return-addr send-more: #t)
       (send-message pub-socket (db:obj->string (vector success/fail query-sig result)))))
    ((fs)   result)
    (else 
     (debug:print 0 "ERROR: unrecognised transport type: " *transport-type*)
     result)))

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

(define (server:get-client-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

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
    (if server
	;; note: client:start will set *runremote*. this needs to be changed
	;;       also, client:start will login to the server, also need to change that.
	;;
	;; client:start returns #t if login was successful.
	;;
	(let ((res (server:ping-server run-id (vector-ref server 1)(vector-ref server 0))))
	  ;; if the server didn't respond we must remove the record
	  (if res
	      #t
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