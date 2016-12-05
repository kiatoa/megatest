
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
;;(declare (uses nmsg-transport))
(declare (uses launch))
(declare (uses daemon))

(include "common_records.scm")
(include "db_records.scm")

(define (server:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;

;; all routes though here end in exit ...
;;
;; start_server
;;
(define (server:launch run-id transport-type)
  (BB> "server:launch fired for run-id="run-id" transport-type="transport-type)
  (case transport-type
    ((http)(http-transport:launch run-id))
    ;;((nmsg)(nmsg-transport:launch run-id))
    ((rpc)  (rpc-transport:launch run-id))
    (else (debug:print-error 0 *default-log-port* "unknown server type " transport-type))))
;;       (else   (debug:print-error 0 *default-log-port* "No known transport set, transport=" transport ", using rpc")
;; 	      (rpc-transport:launch run-id)))))

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
  (debug:print-info 11 *default-log-port* "server:reply return-addr=" return-addr ", result=" result)
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
     (debug:print-error 0 *default-log-port* "unrecognised transport type: " *transport-type*)
     result)))

;; Given a run id start a server process    ### NOTE ### > file 2>&1 
;; if the run-id is zero and the target-host is set 
;; try running on that host
;;   incidental: rotate logs in logs/ dir.
;;
(define  (server:run areapath) ;; areapath is ignored for now.
  (let* ((curr-host   (get-host-name))
	 (curr-ip     (server:get-best-guess-address curr-host))
	 (curr-pid    (current-process-id))
	 (homehost    (common:get-homehost)) ;; configf:lookup *configdat* "server" "homehost" ))
	 (target-host (car homehost))
	 (testsuite   (common:get-testsuite-name))
	 (logfile     (conc *toppath* "/logs/server.log"))
	 (cmdln (conc (common:get-megatest-exe)
		      " -server " (or target-host "-") " -run-id " 0 (if (equal? (configf:lookup *configdat* "server" "daemonize") "yes")
									      (conc " -daemonize -log " logfile)
									      "")
		      " -m testsuite:" testsuite)) ;; (conc " >> " logfile " 2>&1 &")))))
	 (log-rotate  (make-thread common:rotate-logs  "server run, rotate logs thread")))
    ;; we want the remote server to start in *toppath* so push there
    (push-directory *toppath*)
    (debug:print 0 *default-log-port* "INFO: Trying to start server (" cmdln ") ...")
    (thread-start! log-rotate)

    ;; host.domain.tld match host?
    (if (and target-host 
	     ;; look at target host, is it host.domain.tld or ip address and does it 
	     ;; match current ip or hostname
	     (not (string-match (conc "("curr-host "|" curr-host"\\..*)") target-host))
	     (not (equal? curr-ip target-host)))
	(begin
	  (debug:print-info 0 *default-log-port* "Starting server on " target-host ", logfile is " logfile)
	  (setenv "TARGETHOST" target-host)))
    
    (setenv "TARGETHOST_LOGF" logfile)
    (common:wait-for-normalized-load 4 " delaying server start due to load" remote-host: (get-environment-variable "TARGETHOST")) ;; do not try starting servers on an already overloaded machine, just wait forever
    (system (conc "nbfake " cmdln))
    (unsetenv "TARGETHOST_LOGF")
    (if (get-environment-variable "TARGETHOST")(unsetenv "TARGETHOST"))
    (thread-join! log-rotate)
    (pop-directory)))

(define (server:get-client-signature) ;; BB> why is this proc named "get-"?  it returns nothing -- set! has not return value.
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; kind start up of servers, wait 40 seconds before allowing another server for a given
;; run-id to be launched
(define (server:kind-run areapath)
  (let ((last-run-time (hash-table-ref/default *server-kind-run* areapath #f)))
    (if (or (not last-run-time)
	    (> (- (current-seconds) last-run-time) 30))
	(begin
	  (server:run areapath)
	  (hash-table-set! *server-kind-run* areapath (current-seconds))))))

;; The generic run a server command. Dispatches the call to server 0 if run-id != 0
;; 
;;  (define (server:try-running run-id)
;;    (if (eq? run-id 0)
;;        (server:run run-id)
;;        (rmt:start-server run-id)))
(define server:try-running server:run) ;; there is no more per-run servers ;; REMOVE ME. BUG.

(define (server:start-attempted? areapath)
  (let ((flagfile (conc areapath "/.starting-server")))
    (handle-exceptions
     exn
     #f  ;; if things go wrong pretend we can't see the file
     (and (file-exists? flagfile)
	  (< (- (current-seconds)
		(file-modification-time flagfile))
	     15))))) ;; exists and less than 15 seconds old
    
(define (server:read-dotserver areapath)
  (let ((dotfile (conc areapath "/.server")))
    (handle-exceptions
     exn
     #f  ;; if things go wrong pretend we can't see the file
     (if (and (file-exists? dotfile)
	      (file-read-access? dotfile))
	 (with-input-from-file
	     dotfile
	   (lambda ()
	     (read-line)))
	 #f))))

;; write a .server file in *toppath* with hostport
;; return #t on success, #f otherwise
;;
(define (server:write-dotserver areapath hostport)
  (let ((lock-file   (conc areapath "/.server.lock"))
	(server-file (conc areapath "/.server")))
    (if (common:simple-file-lock lock-file)
	(let ((res (handle-exceptions
		    exn
		    #f ;; failed for some reason, for the moment simply return #f
		    (with-output-to-file server-file
		      (lambda ()
			(print hostport)))
		    #t)))
	  (debug:print-info 0 *default-log-port* "server file " server-file " for " hostport " created")
	  (common:simple-file-release-lock lock-file)
	  res)
	#f)))

(define (server:remove-dotserver-file areapath hostport)
  (let ((dotserver   (server:read-dotserver areapath))
	(server-file (conc areapath "/.server"))
	(lock-file   (conc areapath "/.server.lock")))
    (if (and dotserver (string-match (conc ".*:" hostport "$") dotserver)) ;; port matches, good enough info to decide to remove the file
	(if (common:simple-file-lock lock-file)
	    (begin
	      (handle-exceptions
	       exn
	       #f
	       (delete-file* server-file))
	      (debug:print-info 0 *default-log-port* "server file " server-file " for " hostport " removed")
	      (common:simple-file-release-lock lock-file))))))

;; no longer care if multiple servers are started by accident. older servers will drop off in time.
;;
(define (server:check-if-running areapath)
  (let* ((dotserver (server:read-dotserver areapath))) ;; tdbdat (tasks:open-db)))
    (if dotserver
	(let* ((res (case *transport-type*
		      ((http rpc)(server:ping-server dotserver))
		      ;; ((nmsg)(nmsg-transport:ping (tasks:hostinfo-get-interface server)
		      )))
	  (if res
	      dotserver
	      #f))
	#f)))

;; called in megatest.scm, host-port is string hostname:port
;;
;; NOTE: This is NOT called directly from clients as not all transports support a client running
;;       in the same process as the server.
;;
(define (server:ping host-port-in #!key (do-exit #f))
  (let ((host:port (if (not host-port-in) ;; use read-dotserver to find
		       (server:read-dotserver *toppath*)
		       (if (number? host-port-in) ;; we were handed a server-id
			   (let ((srec (tasks:get-server-by-id (db:delay-if-busy (tasks:open-db)) host-port-in)))
			     ;; (print "srec: " srec " host-port-in: " host-port-in)
			     (if srec
				 (conc (vector-ref srec 3) ":" (vector-ref srec 4))
				 (conc "no such server-id " host-port-in)))
			   host-port-in))))
    (let* ((host-port (if host:port
			  (let ((slst (string-split   host:port ":")))
			    (if (eq? (length slst) 2)
				(list (car slst)(string->number (cadr slst)))
				#f))
			  #f))
	   (toppath       (launch:setup)))
      ;; (print "host-port=" host-port)
      (if (not host-port)
	  (begin
	    (if host-port-in
		(debug:print 0 *default-log-port*  "ERROR: bad host:port"))
	    (if do-exit (exit 1))
	    #f)
	  (let* ((iface      (car host-port))
		 (port       (cadr host-port))
		 (server-dat
                  (case (remote-transport *runremote*)
                    ((http) (http-transport:client-connect iface port))
                    ((rpc) (rpc-transport:client-connect iface port))
                    (else
                     (debug:print 0 *default-log-port* "ERROR: transport " (remote-transport *runremote*) " not supported (4)")
                     (exit))))
		 (login-res  (rmt:login-no-auto-client-setup server-dat)))
	    (if (and (list? login-res)
		     (car login-res))
		(begin
		  (print "LOGIN_OK")
		  (if do-exit (exit 0)))
		(begin
		  (print "LOGIN_FAILED")
		  (if do-exit (exit 1)))))))))

;; run ping in separate process, safest way in some cases
;;
(define (server:ping-server ifaceport)
  (with-input-from-pipe 
   (conc (common:get-megatest-exe) " -ping " ifaceport)
   (lambda ()
     (let loop ((inl (read-line))
		(res "NOREPLY"))
       (if (eof-object? inl)
	   (case (string->symbol res)
	     ((NOREPLY)  #f)
	     ((LOGIN_OK) #t)
	     (else       #f))
	   (loop (read-line) inl))))))

(define (server:login toppath)
  (lambda (toppath)
    (set! *db-last-access* (current-seconds)) ;; might not be needed.
    (if (equal? *toppath* toppath)
	#t
	#f)))

(define (server:get-timeout)
  (let ((tmo (configf:lookup  *configdat* "server" "timeout")))
    (if (and (string? tmo)
	     (string->number tmo))
	(* 60 60 (string->number tmo))
	;; (* 3 24 60 60) ;; default to three days
	(* 60 1)         ;; default to one minute
	;; (* 60 60 25)      ;; default to 25 hours
	)))

