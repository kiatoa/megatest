
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp rpc s11n)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo zmq)
(import (prefix sqlite3 sqlite3:))

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

(define (server:make-server-url hostport)
  (if (not hostport)
      #f
      (conc "tcp://" (car hostport) ":" (cadr hostport))))
(define *time-to-exit* #f)

(define (server:run hostn)
  (debug:print 0 "Attempting to start the server ...")
  (if (not *toppath*)(setup-for-run))
  (let* ((hostport      (open-run-close tasks:get-best-server tasks:open-db)) ;; do whe already have a server running?
	 (host:port (server:make-server-url hostport)))
    (if host:port 
	(begin
	  (debug:print 0 "NOTE: server already running.")
	  (if (server:client-setup)
	      (begin 
		(debug:print-info 0 "Server is alive, not starting another"))
	      (begin
		(debug:print-info 0 "Server is dead, deregistering it, please try again")
		(open-run-close tasks:server-deregister tasks:open-db (car hostport) port: (cadr port))
		;; (server:run hostn)
		(debug:print 0 "WOULD NORMALLY START ANOTHER SERVER HERE")
		)
	      )
	  )
	(let* ((zmq-socket     #f)
	       (hostname       (if (string=? "-" hostn)
				   (get-host-name) 
				   hostn))
	       (ipaddrstr      (let ((ipstr (if (string=? "-" hostn)
						(string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
						#f)))
				 (if ipstr ipstr hostname))))
	  (set! zmq-socket (server:find-free-port-and-open ipaddrstr zmq-socket 5555 0))
	  (set! *cache-on* #t)
	  
	  ;; what to do when we quit
	  ;;
	  (on-exit (lambda ()
		     (open-run-close tasks:server-deregister-self tasks:open-db)
		     (let loop () 
		       (let ((queue-len 0))
			 (thread-sleep! (random 5))
			 (mutex-lock! *incoming-mutex*)
			 (set! queue-len (length *incoming-data*))
			 (mutex-unlock! *incoming-mutex*)
			 (if (> queue-len 0)
			     (begin
			       (debug:print-info 0 "Queue not flushed, waiting ...")
			       (loop)))))))

	  ;; The heavy lifting
	  ;;
	  (let loop ()
	    (let* ((rawmsg (receive-message zmq-socket))
		   (params (db:string->obj rawmsg)) ;; (with-input-from-string rawmsg (lambda ()(deserialize))))
		   (res    #f))
	      (debug:print-info 12 "server=> received params=" params)
	      (set! res (cdb:cached-access params))
	      (debug:print-info 12 "server=> processed res=" res)
	      (send-message zmq-socket (db:obj->string res))
	      (if *time-to-exit* (exit))
	      (loop)))))))

;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 1) ;; no need to do this very often
    (db:write-cached-data)
    (if (< count 100)
	(loop 0)
	(let ((numrunning (open-run-close db:get-count-tests-running #f)))
	  (if (or (> numrunning 0)
		  (> (+ *last-db-access* 60)(current-seconds)))
	      (begin
		(debug:print-info 0 "Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
		(loop (+ count 1)))
	      (begin
		(debug:print-info 0 "Starting to shutdown the server side")
		;; need to delete only *my* server entry (future use)
		(open-run-close db:del-var #f "SERVER")
		(thread-sleep! 10)
		(debug:print-info 0 "Max cached queries was " *max-cache-size*)
		(debug:print-info 0 "Server shutdown complete. Exiting")
		;; (exit)))
		))))))

(define (server:find-free-port-and-open host s port trynum)
  (let ((s (if s s (make-socket 'rep)))
	(p (if (number? port) port 5555)))
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "Failed to bind to port " p ", trying next port")
       (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
       (if (< trynum 100)
	   (server:find-free-port-and-open host s (+ p 1) (+ trynum 1))
	   (debug:print-info 0 "Tried ports from " (- p trynum) " to " p 
			     " but all were in use. Please try a different port range by starting the server with parameter \" -port N\" where N is the starting port number to use")))
     (let ((zmq-url (conc "tcp://" host ":" p)))
       (print "Trying to start server on " zmq-url)
       (bind-socket s zmq-url)
       (set! *runremote* #f)
       (debug:print 0 "Server started on " zmq-url)
       (open-run-close tasks:server-register tasks:open-db (current-process-id) host p 0 'live)
       s))))

(define (server:client-setup)
  (if (not *toppath*)(setup-for-run))
  (let* ((hostinfo   (open-run-close tasks:get-best-server tasks:open-db))
	 (zmq-socket (make-socket 'req)))
    (if hostinfo
	(begin
	  (debug:print-info 2 "Setting up to connect to " hostinfo)
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 0 "ERROR: Failed to open a connection to the server at: " hostinfo)
	     (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 0 "   perhaps jobs killed with -9? Removing server records")
	     (open-run-close tasks:server-deregister tasks:open-db (car hostinfo) port: (cadr hostinfo))
	     ;; (exit) ;; why forced exit?
	     #f)
	   (let ((connect-ok #f)
		 (conurl     (server:make-server-url hostinfo)))
	     (connect-socket zmq-socket conurl)
	     (set! connect-ok (cdb:client-call zmq-socket 'login #t *toppath*))
	     (if connect-ok
		 (begin
		   (debug:print-info 2 "Logged in and connected to " conurl)
		   (set! *runremote* zmq-socket)
		   #t)
		 (begin
		   (debug:print-info 2 "Failed to login or connect to " conurl)
		   (set! *runremote* #f)
		   #f)))))
	(begin
	  (debug:print-info 0 "NO SERVER RUNNING! PLEASE START ONE! E.g. \"megatest -server - &\"")
	;;   (debug:print-info 2 "No server available, attempting to start one...")
	;;   (system (conc (car (argv)) " -server - " (if (args:get-arg "-debug")
	;; 					  (conc "-debug " (args:get-arg "-debug"))
	;; 					  "")
	;; 		" &"))
	  ;; (sleep 5)
	  ;; (server:client-setup)
	  ))))

(define (server:launch)
  (let* ((toppath (setup-for-run)))
    (debug:print-info 0 "Starting the standalone server")
    (if *toppath* 
	(let* ((th2 (make-thread (lambda ()
				   (server:run (args:get-arg "-server")))))
	       (th3 (make-thread (lambda ()
				   (server:keep-running)))))
	  (thread-start! th3)
	  (thread-start! th2)
	  (thread-join! th2)
	  (set! *didsomething* #t))
	(debug:print 0 "ERROR: Failed to setup for megatest"))))

(define (server:client-launch)
  (if (server:client-setup)
      (debug:print-info 0 "connected as client")
      (begin
	(debug:print 0 "ERROR: Failed to connect as client")
	(exit))))
