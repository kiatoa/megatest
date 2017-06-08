
;; Copyright 2006-2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest directory-utils posix-extras matchable
     )

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
;; (declare (uses synchash))
(declare (uses http-transport))
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
;; P K T S   S T U F F 
;;======================================================================

;; ???

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
	 (logfile     (conc areapath "/logs/server.log")) ;; -" curr-pid "-" target-host ".log"))
	 (cmdln (conc (common:get-megatest-exe)
		      " -server " (or target-host "-") (if (equal? (configf:lookup *configdat* "server" "daemonize") "yes")
							   " -daemonize "
							   "")
		      ;; " -log " logfile
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
      
    (setenv "TARGETHOST_LOGF" logfile)
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
    (handle-exceptions
	exn
	(list #f #f #f) ;; no idea what went wrong, call it a bad server
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
		(list #f #f #f))))))))

;; get a list of servers with all relevant data
;; ( mod-time host port start-time pid )
;;
(define (server:get-list areapath #!key (limit #f))
  (let ((fname-rx    (regexp "^(|.*/)server-(\\d+)-(\\S+).log$"))
	(day-seconds (* 24 60 60)))
    ;; if the directory exists continue to get the list
    ;; otherwise attempt to create the logs dir and then
    ;; continue
    (if (if (directory-exists? (conc areapath "/logs"))
	    '()
	    (if (file-write-access? areapath)
		(begin
		  (condition-case
		      (create-directory (conc areapath "/logs") #t)
		    (exn (i/o file)(debug:print 0 *default-log-port* "ERROR: Cannot create directory at " (conc areapath "/logs")))
		    (exn ()(debug:print 0 *default-log-port* "ERROR: Unknown error attemtping to get server list.")))
		  (directory-exists? (conc areapath "/logs")))
		'()))
	(let* ((server-logs   (glob (conc areapath "/logs/server-*.log")))
	       (num-serv-logs (length server-logs)))
	  (if (null? server-logs)
	      '()
	      (let loop ((hed  (car server-logs))
			 (tal  (cdr server-logs))
			 (res '()))
		(let* ((mod-time  (handle-exceptions
				      exn
				      (current-seconds) ;; 0
				    (file-modification-time hed))) ;; default to *very* old so log gets ignored if deleted
		       (down-time (- (current-seconds) mod-time))
		       (serv-dat  (if (or (< num-serv-logs 10)
				  	  (< down-time 900)) ;; day-seconds))
				      (server:logf-get-start-info hed)
				      '())) ;; don't waste time processing server files not touched in the 15 minutes if there are more than ten servers to look at
		       (serv-rec (cons mod-time serv-dat))
		       (fmatch   (string-match fname-rx hed))
		       (pid      (if fmatch (string->number (list-ref fmatch 2)) #f))
		       (new-res  (if (null? serv-dat)
				     res
				     (cons (append serv-rec (list pid)) res))))
		(if (null? tal)
		    (if (and limit
			     (> (length new-res) limit))
			new-res ;; (take new-res limit)  <= need intelligent sorting before this will work
			new-res)
		    (loop (car tal)(cdr tal) new-res)))))))))

(define (server:get-num-alive srvlst)
  (let ((num-alive 0))
    (for-each
     (lambda (server)
       (match-let (((mod-time host port start-time pid)
		    server))
	 (let* ((uptime  (- (current-seconds) mod-time))
		(runtime (if start-time
			     (- mod-time start-time)
			     0)))
	   (if (< uptime 5)(set! num-alive (+ num-alive 1))))))
     srvlst)
    num-alive))

;; given a list of servers get a list of valid servers, i.e. at least
;; 10 seconds old, has started and is less than 1 hour old and is
;; active (i.e. mod-time < 10 seconds
;;
;; mod-time host port start-time pid
;;
;; sort by start-time descending. I.e. get the oldest first. Young servers will thus drop off
;; and servers should stick around for about two hours or so.
;;
(define (server:get-best srvlst)
  (let* ((nums (server:get-num-servers))
	 (now  (current-seconds))
	 (slst (sort
		(filter (lambda (rec)
			  (if (and (list? rec)
				   (> (length rec) 2))
			      (let ((start-time (list-ref rec 3))
				    (mod-time   (list-ref rec 0)))
				;; (print "start-time: " start-time " mod-time: " mod-time)
				(and start-time mod-time
				     (> (- now start-time) 0)    ;; been running at least 0 seconds
				     (< (- now mod-time)   16)   ;; still alive - file touched in last 16 seconds
				     (< (- now start-time) 
					(+ (- (string->number (or (configf:lookup *configdat* "server" "runtime") "3600"))
					      180)
					   (random 360))) ;; under one hour running time +/- 180
				     ))
			      #f))
			srvlst)
		(lambda (a b)
		  (< (list-ref a 3)
		     (list-ref b 3))))))
    (if (> (length slst) nums)
	(take slst nums)
	slst)))

(define (server:get-first-best areapath)
  (let ((srvrs (server:get-best (server:get-list areapath))))
    (if (and srvrs
	     (not (null? srvrs)))
	(car srvrs)
	#f)))

(define (server:get-rand-best areapath)
  (let ((srvrs (server:get-best (server:get-list areapath))))
    (if (and (list? srvrs)
	     (not (null? srvrs)))
	(let* ((len (length srvrs))
	       (idx (random len)))
	  (list-ref srvrs idx))
	#f)))


(define (server:record->url servr)
  (match-let (((mod-time host port start-time pid)
	       servr))
    (if (and host port)
	(conc host ":" port)
	#f)))

(define (server:get-client-signature) ;; BB> why is this proc named "get-"?  it returns nothing -- set! has not return value.
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
        (set! *my-client-signature* sig)
        *my-client-signature*)))

;; kind start up of servers, wait 40 seconds before allowing another server for a given
;; run-id to be launched
(define (server:kind-run areapath)
  (if (not (server:check-if-running areapath)) ;; why try if there is already a server running?
      (let* ((last-run-dat (hash-table-ref/default *server-kind-run* areapath '(0 0))) ;; callnum, whenrun
	     (call-num     (car last-run-dat))
	     (when-run     (cadr last-run-dat))
	     (run-delay    (+ (case call-num
				((0)    0)
				((1)   20)
				((2)  300)
				(else 600))
			      (random 5)))   ;; add a small random number just in case a lot of jobs hit the work hosts simultaneously
	     (lock-file    (conc areapath "/logs/server-start.lock")))
	(if	(> (- (current-seconds) when-run) run-delay)
		(begin
		  (common:simple-file-lock-and-wait lock-file expire-time: 15)
		  (server:run areapath)
		  (thread-sleep! 5) ;; don't release the lock for at least a few seconds
		  (common:simple-file-release-lock lock-file)))
	(hash-table-set! *server-kind-run* areapath (list (+ call-num 1)(current-seconds))))))

(define (server:start-and-wait areapath #!key (timeout 60))
  (let ((give-up-time (+ (current-seconds) timeout)))
    (let loop ((server-url (server:check-if-running areapath))
	       (try-num    0))
      (if (or server-url
	      (> (current-seconds) give-up-time)) ;; server-url will be #f if no server available.
	  server-url
	  (let ((num-ok (length (server:get-best (server:get-list areapath)))))
	    (if (and (> try-num 0)  ;; first time through simply wait a little while then try again
		     (< num-ok 1))  ;; if there are no decent candidates for servers then try starting a new one
		(server:kind-run areapath))
	    (thread-sleep! 5)
	    (loop (server:check-if-running areapath)
		  (+ try-num 1)))))))

(define server:try-running server:run) ;; there is no more per-run servers ;; REMOVE ME. BUG.

(define (server:get-num-servers #!key (numservers 2))
  (let ((ns (string->number
	     (or (configf:lookup *configdat* "server" "numservers") "notanumber"))))
    (or ns numservers)))

;; no longer care if multiple servers are started by accident. older servers will drop off in time.
;;
(define (server:check-if-running areapath) ;;  #!key (numservers "2"))
  (let* ((ns            (server:get-num-servers))
	 (servers       (server:get-best (server:get-list areapath))))
    ;; (print "servers: " servers " ns: " ns)
    (if (or (and servers
		 (null? servers))
	    (not servers)
	    (and (list? servers)
		 (< (length servers) (random ns)))) ;; somewhere between 0 and numservers
        #f
        (let loop ((hed (car servers))
                   (tal (cdr servers)))
          (let ((res (server:check-server hed)))
            (if res
                res
                (if (null? tal)
                    #f
                    (loop (car tal)(cdr tal)))))))))

;; ping the given server
;;
(define (server:check-server server-record)
  (let* ((server-url (server:record->url server-record))
         (res        (case *transport-type*
                       ((http)(server:ping server-url))
                       ;; ((nmsg)(nmsg-transport:ping (tasks:hostinfo-get-interface server)
                       )))
    (if res
        server-url
	#f)))

(define (server:kill servr)
  (match-let (((mod-time hostname port start-time pid)
	       servr))
    (tasks:kill-server hostname pid)))

;; called in megatest.scm, host-port is string hostname:port
;;
;; NOTE: This is NOT called directly from clients as not all transports support a client running
;;       in the same process as the server.
;;
(define (server:ping host-port-in #!key (do-exit #f))
  (let ((host:port (if (not host-port-in) ;; use read-dotserver to find
		       #f ;; (server:check-if-running *toppath*)
		;; (if (number? host-port-in) ;; we were handed a server-id
		;; 	   (let ((srec (tasks:get-server-by-id (db:delay-if-busy (tasks:open-db)) host-port-in)))
		;; 	     ;; (print "srec: " srec " host-port-in: " host-port-in)
		;; 	     (if srec
		;; 		 (conc (vector-ref srec 3) ":" (vector-ref srec 4))
		;; 		 (conc "no such server-id " host-port-in)))
		       host-port-in))) ;; )
    (let* ((host-port (if host:port
			  (let ((slst (string-split   host:port ":")))
			    (if (eq? (length slst) 2)
				(list (car slst)(string->number (cadr slst)))
				#f))
			  #f)))
;;	   (toppath       (launch:setup)))
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
		  ;; (print "LOGIN_OK")
		  (if do-exit (exit 0))
		  #t)
		(begin
		  ;; (print "LOGIN_FAILED")
		  (if do-exit (exit 1))
		  #f)))))))

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

;; NOT USED (well, ok, reference in rpc-transport but otherwise not used).
;;
(define (server:login toppath)
  (lambda (toppath)
    (set! *db-last-access* (current-seconds)) ;; might not be needed.
    (if (equal? *toppath* toppath)
	#t
	#f)))

;; timeout is in hours
(define (server:get-timeout)
  (let ((tmo (configf:lookup  *configdat* "server" "timeout")))
    (if (and (string? tmo)
	     (string->number tmo))
	(* 60 60 (string->number tmo))
	;; (* 3 24 60 60) ;; default to three days
	;;(* 60 60 1)     ;; default to one hour
	(* 60 5)          ;; default to five minutes
	)))

(define (server:get-best-guess-address hostname)
  (let ((res #f))
    (for-each 
     (lambda (adr)
       (if (not (eq? (u8vector-ref adr 0) 127))
	   (set! res adr)))
     ;; NOTE: This can fail when there is no mention of the host in /etc/hosts. FIXME
     (vector->list (hostinfo-addresses (hostname->hostinfo hostname))))
    (string-intersperse 
     (map number->string
	  (u8vector->list
	   (if res res (hostname->ip hostname)))) ".")))

;; moving this here as it needs access to db and cannot be in common.
;;
(define (server:writable-watchdog dbstruct)
  (thread-sleep! 0.05) ;; delay for startup
  (let ((legacy-sync  (common:run-sync?))
	(debug-mode   (debug:debug-mode 1))
	(last-time    (current-seconds))
	(no-sync-db   (db:open-no-sync-db))
        (this-wd-num  (begin (mutex-lock! *wdnum*mutex) (let ((x *wdnum*)) (set! *wdnum* (add1 *wdnum*)) (mutex-unlock! *wdnum*mutex) x))))
    (set! *no-sync-db* no-sync-db) ;; make the no sync db available to api calls
    (debug:print-info 2 *default-log-port* "Periodic sync thread started.")
    (debug:print-info 3 *default-log-port* "watchdog starting. legacy-sync is " legacy-sync" pid="(current-process-id)" this-wd-num="this-wd-num)
    (if (and legacy-sync (not *time-to-exit*))
	(let* (;;(dbstruct (db:setup))
	       (mtdb     (dbr:dbstruct-mtdb dbstruct))
	       (mtpath   (db:dbdat-get-path mtdb)))
	  (debug:print-info 0 *default-log-port* "Server running, periodic sync started.")
	  (let loop ()
	    ;; sync for filesystem local db writes
	    ;;
	    (mutex-lock! *db-multi-sync-mutex*)
	    (let* ((need-sync        (>= *db-last-access* *db-last-sync*)) ;; no sync since last write
		   (sync-in-progress *db-sync-in-progress*)
		   (should-sync      (and (not *time-to-exit*)
                                          (> (- (current-seconds) *db-last-sync*) 5))) ;; sync every five seconds minimum
		   (start-time       (current-seconds))
		   (mt-mod-time      (file-modification-time mtpath))
		   (recently-synced  (< (- start-time mt-mod-time) 4))
		   (will-sync        (and (or need-sync should-sync)
					  (not sync-in-progress)
					  (not recently-synced))))
              (debug:print-info 13 *default-log-port* "WD writable-watchdog top of loop.  need-sync="need-sync" sync-in-progress="sync-in-progress" should-sync="should-sync" start-time="start-time" mt-mod-time="mt-mod-time" recently-synced="recently-synced" will-sync="will-sync)
	      ;; (if recently-synced (debug:print-info 0 *default-log-port* "Skipping sync due to recently-synced flag=" recently-synced))
	      ;; (debug:print-info 0 *default-log-port* "need-sync: " need-sync " sync-in-progress: " sync-in-progress " should-sync: " should-sync " will-sync: " will-sync)
	      (if will-sync (set! *db-sync-in-progress* #t))
	      (mutex-unlock! *db-multi-sync-mutex*)
	      (if will-sync
		  (let ((res (db:sync-to-megatest.db dbstruct no-sync-db: no-sync-db))) ;; did we sync any data? If so need to set the db touched flag to keep the server alive
		    (if (> res 0) ;; some records were transferred, keep the db alive
			(begin
			  (mutex-lock! *heartbeat-mutex*)
			  (set! *db-last-access* (current-seconds))
			  (mutex-unlock! *heartbeat-mutex*)
			  (debug:print-info 0 *default-log-port* "sync called, " res " records transferred."))
			(debug:print-info 2 *default-log-port* "sync called but zero records transferred"))))
	      (if will-sync
		  (begin
		    (mutex-lock! *db-multi-sync-mutex*)
		    (set! *db-sync-in-progress* #f)
		    (set! *db-last-sync* start-time)
		    (mutex-unlock! *db-multi-sync-mutex*)))
	      (if (and debug-mode
		       (> (- start-time last-time) 60))
		  (begin
		    (set! last-time start-time)
		    (debug:print-info 4 *default-log-port* "timestamp -> " (seconds->time-string (current-seconds)) ", time since start -> " (seconds->hr-min-sec (- (current-seconds) *time-zero*))))))
	    
	    ;; keep going unless time to exit
	    ;;
	    (if (not *time-to-exit*)
		(let delay-loop ((count 0))
                  ;;(debug:print-info 13 *default-log-port* "delay-loop top; count="count" pid="(current-process-id)" this-wd-num="this-wd-num" *time-to-exit*="*time-to-exit*)
                                                            
		  (if (and (not *time-to-exit*)
			   (< count 6)) ;; was 11, changing to 4. 
		      (begin
			(thread-sleep! 1)
			(delay-loop (+ count 1))))
		  (if (not *time-to-exit*) (loop))))
	    ;; time to exit, close the no-sync db here
	    (db:no-sync-close-db no-sync-db)
	    (if (common:low-noise-print 30)
		(debug:print-info 0 *default-log-port* "Exiting watchdog timer, *time-to-exit* = " *time-to-exit*" pid="(current-process-id)" this-wd-num="this-wd-num)))))))

