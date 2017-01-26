
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest directory-utils posix-extras)
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
  (case transport-type
    ((http)(http-transport:launch))
    ;;((nmsg)(nmsg-transport:launch run-id))
    ((rpc)  (rpc-transport:launch run-id))
    (else (debug:print-error 0 *default-log-port* "unknown server type " transport-type))))

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
(define  (server:run areapath) ;; areapath is *toppath* for a given testsuite area
  (let* ((curr-host   (get-host-name))
         ;; (attempt-in-progress (server:start-attempted? areapath))
         ;; (dot-server-url (server:check-if-running areapath))
	 (curr-ip     (server:get-best-guess-address curr-host))
	 (curr-pid    (current-process-id))
	 (homehost    (common:get-homehost)) ;; configf:lookup *configdat* "server" "homehost" ))
	 (target-host (car homehost))
	 (testsuite   (common:get-testsuite-name))
	 (logfile     (conc areapath "/logs/server-" curr-pid "-" target-host ".log"))
	 (cmdln (conc (common:get-megatest-exe)
		      " -server " (or target-host "-") (if (equal? (configf:lookup *configdat* "server" "daemonize") "yes")
							   " -daemonize "
							   "")
		      " -log " logfile
		      " -m testsuite:" testsuite)) ;; (conc " >> " logfile " 2>&1 &")))))
	 (log-rotate  (make-thread common:rotate-logs  "server run, rotate logs thread")))
    ;; we want the remote server to start in *toppath* so push there
    (push-directory areapath)
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
      
    (setenv "TARGETHOST_LOGF" "server.log") ;; logfile)
    (common:wait-for-normalized-load 4 " delaying server start due to load" remote-host: (get-environment-variable "TARGETHOST")) ;; do not try starting servers on an already overloaded machine, just wait forever
    (system (conc "nbfake " cmdln))
    (unsetenv "TARGETHOST_LOGF")
    (if (get-environment-variable "TARGETHOST")(unsetenv "TARGETHOST"))
    (thread-join! log-rotate)
    (pop-directory)))

;; given a path to a server log return: host port startseconds
;;
(define (server:logf-get-start-info logf)
  (let ((rx (regexp "^SERVER STARTED: (\\S+):(\\d+) AT ([\\d\\.]+)"))) ;; SERVER STARTED: host:port AT timesecs
    (with-input-from-file
	logf
      (lambda ()
	(let loop ((inl  (read-line))
		   (lnum 0))
	  (if (not (eof-object? inl))
	      (let ((mlst (string-match rx inl)))
		(if (not mlst)
		    (if (< lnum 500) ;; give up if more than 500 lines of server log read
			(loop (read-line)(+ lnum 1))
			(list #f #f #f))
		    (let ((dat  (cdr mlst)))
		      (list (car dat) ;; host
			    (string->number (cadr dat)) ;; port
			    (string->number (caddr dat))))))
	      (list #f #f #f)))))))

;; get a list of servers with all relevant data
;; ( mod-time host port start-time )
;;
(define (server:get-list areapath)
  (if (directory-exists? areapath)
      (let ((server-logs (glob (conc areapath "/logs/server-*.log"))))
	(if (null? server-logs)
	    '()
	    (let loop ((hed  (car server-logs))
		       (tal  (cdr server-logs))
		       (res '()))
	      (let* ((mod-time (file-modification-time hed))
		     (serv-dat (server:logf-get-start-info hed))
		     (serv-rec (cons mod-time serv-dat))
		     (new-res  (cons serv-rec res)))
		(if (null? tal)
		    new-res
		    (loop (car tal)(cdr tal) new-res))))))))

;; given a list of servers get a list of valid servers, i.e. at least
;; 10 seconds old, has started and is less than 1 hour old and is
;; active (i.e. mod-time < 10 seconds
;;
;; mod-time host port start-time
;;
;; sort by start-time descending. I.e. get the oldest first. Young servers will thus drop off
;; and servers should stick around for about two hours or so.
;;
(define (server:get-best srvlst)
  (let ((now (current-seconds)))
    (sort
     (filter (lambda (rec)
	       (let ((start-time (list-ref rec 3))
		     (mod-time   (list-ref rec 0)))
		 (print "start-time: " start-time " mod-time: " mod-time)
		 (and start-time mod-time
		      (> (- now start-time) 1)    ;; been running at least 1 seconds
		      (< (- now mod-time)   10)   ;; still alive - file touched in last 10 seconds
		      (< (- now start-time) 3600) ;; under one hour running time
		      )))
	     srvlst)
     (lambda (a b)
       (< (list-ref a 3)
	  (list-ref b 3))))))
		

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

(define server:try-running server:run) ;; there is no more per-run servers ;; REMOVE ME. BUG.

;; (define (server:attempting-start areapath)
;;   (with-output-to-file
;;       (conc areapath "/.starting-server")
;;     (lambda ()
;;       (print (current-process-id) " on " (get-host-name)))))
;;   
;; (define (server:complete-attempt areapath)
;;   (delete-file* (conc areapath "/.starting-server")))
;;   
;; (define (server:start-attempted? areapath)
;;   (let ((flagfile (conc areapath "/.starting-server")))
;;     (handle-exceptions
;;      exn
;;      #f  ;; if things go wrong pretend we can't see the file
;;      (cond
;;       ((and (file-exists? flagfile)
;;             (< (- (current-seconds)
;;                   (file-modification-time flagfile))
;;                15)) ;; exists and less than 15 seconds old
;;        (with-input-from-file flagfile (lambda () (read-line))))
;;       ((file-exists? flagfile) ;; it is stale.
;;        (server:complete-attempt areapath)
;;        #f)
;;       (else #f)))))
;; 
;; (define (server:read-dotserver areapath)
;;   (let ((dotfile (conc areapath "/.server")))
;;     (handle-exceptions
;;      exn
;;      #f  ;; if things go wrong pretend we can't see the file
;;      (cond
;;       ((not (file-exists? dotfile))
;;        #f)
;;       ((not (file-read-access? dotfile))
;;        #f)
;;       ((> (server:dotserver-age-seconds areapath) (+ 5 (server:get-timeout)))
;;        (server:remove-dotserver-file areapath ".*")
;;        #f)
;;       (else
;;        (let* ((line
;;                (with-input-from-file
;;                    dotfile
;;                  (lambda ()
;;                    (read-line))))
;;               (tokens (if (string? line) (string-split line ":") #f)))
;;          (cond
;;           ((eq? 4 (length tokens))
;;            tokens)
;;           (else #f))))))))
;;        
;; (define (server:read-dotserver->url areapath)
;;   (let ((dotserver-tokens (server:read-dotserver areapath)))
;;     (if dotserver-tokens
;;         (conc (list-ref dotserver-tokens 0) ":" (list-ref dotserver-tokens 1))
;; #f)))
;; 
;; ;; write a .server file in *toppath* with hostport
;; ;; return #t on success, #f otherwise
;; ;;
;; (define (server:write-dotserver areapath host port pid transport)
;;   (let ((lock-file   (conc areapath "/.server.lock"))
;; 	(server-file (conc areapath "/.server")))
;;     (if (common:simple-file-lock lock-file)
;; 	(let ((res (handle-exceptions
;; 		    exn
;; 		    #f ;; failed for some reason, for the moment simply return #f
;; 		    (with-output-to-file server-file
;; 		      (lambda ()
;; 			(print (conc host ":" port ":" pid ":" transport))))
;; 		    #t)))
;; 	  (debug:print-info 0 *default-log-port* "server file " server-file " for " host ":" port " created pid="pid)
;; 	  (common:simple-file-release-lock lock-file)
;; 	  res)
;; 	#f)))
;; 
;; 
;; ;; this will check that the .server file present matches the server calling this procedure.
;; ;; if parameters match (this-pid and transport) the file will be touched and #t returned
;; ;; otherwise #f will be returned.
;; (define (server:confirm-dotserver areapath this-iface this-port this-pid this-transport)
;;   (let* ((tokens (server:read-dotserver areapath)))
;;     (cond
;;      ((not tokens)
;;       (debug:print-info 0 *default-log-port* "INFO: .server file does not exist.")
;;       #f)
;;      ((not (eq? 4 (length tokens)))
;;       (debug:print-info 0 *default-log-port* "INFO: .server file is corrupt.  There are not 4 tokens as expeted; there are "(length tokens)".")
;;       #f)
;;      ((not (equal? this-iface (list-ref tokens 0)))
;;       (debug:print-info 0 *default-log-port* "INFO: .server file mismatch.  for iface, server has value >"(list-ref tokens 0)"< but this server's value is >"this-iface"<")
;;       #f)
;;      ((not (equal? (->string this-port)  (list-ref tokens 1)))
;;       (debug:print-info 0 *default-log-port* "INFO: .server file mismatch.  for port, .server has value >"(list-ref tokens 1)"< but this server's value is >"(->string this-port)"<")
;;       #f)
;;      ((not (equal? (->string this-pid)   (list-ref tokens 2)))
;;       (debug:print-info 0 *default-log-port* "INFO: .server file mismatch.  for pid, .server has value >"(list-ref tokens 2)"< but this server's value is >"(->string this-pid)"<")
;;       #f)
;;      ((not (equal? (->string this-transport) (->string (list-ref tokens 3))))
;;       (debug:print-info 0 *default-log-port* "INFO: .server file mismatch.  for transport, .server has value >"(list-ref tokens 3)"< but this server's value is >"this-transport"<")
;;       #f)
;;      (else (server:touch-dotserver areapath)
;;       #t))))
;; 
;; (define (server:touch-dotserver areapath)
;;   (let ((server-file (conc areapath "/.server")))
;;     (change-file-times server-file (current-seconds) (current-seconds))))

(define (server:dotserver-age-seconds areapath)
  (let ((server-file (conc areapath "/.server")))
    (begin
      (handle-exceptions
       exn
       #f
       (- (current-seconds)
          (file-modification-time server-file))))))
    
;; (define (server:remove-dotserver-file areapath hostport)
;;   (let ((dotserver-url   (server:read-dotserver->url areapath))
;; 	(server-file (conc areapath "/.server"))
;; 	(lock-file   (conc areapath "/.server.lock")))
;;     (if (and dotserver-url (string-match (conc ".*:" hostport "$") dotserver-url)) ;; port matches, good enough info to decide to remove the file
;; 	(if (common:simple-file-lock lock-file)
;; 	    (begin
;; 	      (handle-exceptions
;; 	       exn
;; 	       #f
;; 	       (delete-file* server-file))
;; 	      (debug:print-info 0 *default-log-port* "server file " server-file " for " hostport " removed")
;; 	      (common:simple-file-release-lock lock-file))
;;             (debug:print-info 0 *default-log-port* "server file " server-file " for " hostport " NOT removed - could not get lock."))
;;         (debug:print-info 0 *default-log-port* "server file " server-file " for " hostport " NOT removed - dotserver-url("dotserver-url") did not match hostport pattern ("hostport")"))))

;; no longer care if multiple servers are started by accident. older servers will drop off in time.
;;
(define (server:check-if-running areapath)
  (let* ((dotserver-url (server:read-dotserver->url areapath))) ;; tdbdat (tasks:open-db)))
    (if dotserver-url
	(let* ((res (case *transport-type*
		      ((http)(server:ping-server dotserver-url))
		      ;; ((nmsg)(nmsg-transport:ping (tasks:hostinfo-get-interface server)
		      )))
	  (if res
	      dotserver-url
	      (begin
                (server:remove-dotserver-file areapath ".*") ;; remove stale dotserver
                #f)))
	#f)))

;; called in megatest.scm, host-port is string hostname:port
;;
;; NOTE: This is NOT called directly from clients as not all transports support a client running
;;       in the same process as the server.
;;
(define (server:ping host-port-in #!key (do-exit #f))
  (let ((host:port (if (not host-port-in) ;; use read-dotserver to find
		       (server:read-dotserver->url *toppath*)
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
		 (server-dat (http-transport:client-connect iface port))
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

