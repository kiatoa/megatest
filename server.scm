
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(use spiffy awful http-client)

(tcp-buffer-size 2048)

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
      (conc "http://" (car hostport) ":" (cadr hostport))))

(define  *server-loop-heart-beat* (current-seconds))
(define *heartbeat-mutex* (make-mutex))

;;======================================================================
;; S E R V E R
;;======================================================================

;; Call this to start the actual server
;;
(define (server:run hostn)
  (debug:print 2 "Attempting to start the server ...")
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, cannot start server, exiting")
	    (exit))))
  (let* (;; (iface           (if (string=? "-" hostn)
	 ;;        	      #f ;; (get-host-name) 
	 ;;        	      hostn))
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname)))
	 (start-port    (if (args:get-arg "-port")
			    (string->number (args:get-arg "-port"))
			    (+ 5000 (random 1001)))))
    (set! *cache-on* #t)
    (server:try-start-server ipaddrstr start-port)))


(define (server:main-loop)
  (print "INFO: Exectuing main server loop")
  (access-log "megatest-http.log")
  (server-bind-address #f)
  (define-page (main-page-path)
    (lambda ()
      (with-request-variables (dat)
        (print "Got dat=" dat)
	(let* ((packet (db:string->obj dat))
	       (qtype  (cdb:packet-get-qtype packet)))
	  (debug:print-info 12 "server=> received packet=" packet)
	  (if (not (member qtype '(sync ping)))
	      (begin
		(mutex-lock! *heartbeat-mutex*)
		(set! *last-db-access* (current-seconds))
		(mutex-unlock! *heartbeat-mutex*)))
	  (open-run-close db:process-queue-item packet))))))


;; This is recursively run by server:run until sucessful
;;
(define (server:try-start-server ipaddrstr portnum)
  (handle-exceptions
   exn
   (begin
     (print-error-message exn)
     (if (< portnum 9000)
	 (begin 
	   (print "WARNING: failed to start on portnum: " portnum ", trying next port")
	   (thread-sleep! 0.1)
	   (open-run-close tasks:remove-server-records tasks:open-db)
	   (server:try-start-server ipaddrstr (+ portnum 1)))
	 (print "ERROR: Tried and tried but could not start the server")))
   (set! *runremote* (list ipaddrstr portnum))
   (open-run-close tasks:remove-server-records tasks:open-db)
   (open-run-close tasks:server-register 
		   tasks:open-db 
		   (current-process-id)
		   ipaddrstr portnum 0 'live)
   (print "INFO: Trying to start server on " ipaddrstr ":" portnum)
   (awful-start server:main-loop port: portnum) ;; ip-address: ipaddrstr 
   (print "INFO: server has been stopped")))

(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

(define (server:reply pubsock target query-sig success/fail result)
  (debug:print-info 11 "server:reply target=" target ", result=" result)
  ;; (send-message pubsock target send-more: #t)
  ;; (send-message pubsock 
  (db:obj->string (vector success/fail query-sig result)))

;;======================================================================
;; C L I E N  T S
;;======================================================================

(define (server:get-client-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

;; <html>
;; <head></head>
;; <body>1 Hello, world! Goodbye Dolly</body></html>
;; Send msg to serverdat and receive result
(define (server:client-send-receive serverdat msg)
  (let* ((url     (server:make-server-url serverdat))
	 (fullurl (conc url "/?dat=" msg)))
    (print "url=" url ", fullurl=" fullurl)
    (let* ((res   (with-input-from-request fullurl #f read-string)))
      (print "got res=" res)
      (let ((match (string-search (regexp "<body>(.*)<.body>") (caddr (string-split res "\n")))))
	(cadr match)))))

(define (server:client-login serverdat)
  (cdb:login serverdat *toppath* (server:get-client-signature)))

;; Not currently used! But, I think it *should* be used!!!
(define (server:client-logout serverdat)
  (let ((ok (and (socket? serverdat)
		 (cdb:logout serverdat *toppath* (server:get-client-signature)))))
    ;; (close-socket serverdat)
    ok))

(define (server:client-connect iface port)
  (let* ((login-res   #f)
	 (serverdat   (list iface port)))
    (set! login-res (server:client-login serverdat))
    (if (and (not (null? login-res))
	     (car login-res))
	(begin
	  (debug:print-info 2 "Logged in and connected to " iface ":" port)
	  (set! *runremote* serverdat)
	  serverdat)
	(begin
	  (debug:print-info 2 "Failed to login or connect to " iface ":" port)
	  (set! *runremote* #f)
	  #f))))

;; Do all the connection work, start a server if not already running
(define (server:client-setup #!key (numtries 50))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: failed to find megatest.config, exiting")
	    (exit))))
  (let ((hostinfo   (open-run-close tasks:get-best-server tasks:open-db)))
    (if hostinfo
	(let ((host     (list-ref hostinfo 0))
	      (iface    (list-ref hostinfo 1)))
	  (debug:print-info 2 "Setting up to connect to " hostinfo)
	  (server:client-connect iface port)) ;; )
	(if (> numtries 0)
	    (let ((exe (car (argv)))
		  (pid #f))
	      (debug:print-info 0 "No server available, attempting to start one...")
	      ;; (set! pid (process-run exe (list "-server" "-" "-debug" (if (list? *verbosity*)
	      ;;   							  (string-intersperse *verbosity* ",")
	      ;;   							  (conc *verbosity*)))))
	      (set! pid (process-fork (lambda ()
					;; (current-input-port  (open-input-file  "/dev/null"))
					;; (current-output-port (open-output-file "/dev/null"))
					;; (current-error-port  (open-output-file "/dev/null"))
					(server:launch)))) ;; should never get here ....
	      (let loop ((count 0))
		(let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
		  (if (not hostinfo)
		      (begin
			(debug:print-info 0 "Waiting for server pid=" pid " to start")
			(sleep 2) ;; give server time to start
			(if (< count 5)
			    (loop (+ count 1)))))))
	      ;; we are starting a server, do not try again! That can lead to 
	      ;; recursively starting many processes!!!
	      (server:client-setup numtries: 0))
	    (debug:print-info 1 "Too many attempts, giving up")))))

;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  ;; This thread waits for the server to come alive
  (let* ((server-info (let loop ()
                        (let ((sdat #f))
                          (mutex-lock! *heartbeat-mutex*)
                          (set! sdat *runremote*)
                          (mutex-unlock! *heartbeat-mutex*)
                          (if sdat sdat
                              (begin
                                (sleep 4)
                                (loop))))))
         (iface       (car server-info))
         (port        (cadr server-info))
         (last-access 0))
    ;; (print "Keep-running got server-info " server-info)
    (let loop ((count 0))
      (thread-sleep! 4) ;; no need to do this very often
      ;; NB// sync currently does NOT return queue-length
      (let ((queue-len (cdb:client-call server-info 'sync #t 1)))
      ;; (print "Server running, count is " count)
        (if (< count 1) ;; 3x3 = 9 secs aprox
            (loop (+ count 1)))
        
        ;; NOTE: Get rid of this mechanism! It really is not needed...
        (open-run-close tasks:server-update-heartbeat tasks:open-db (car server-info))
      
        ;; (if ;; (or (> numrunning 0) ;; stay alive for two days after last access
        (mutex-lock! *heartbeat-mutex*)
        (set! last-access *last-db-access*)
        (mutex-unlock! *heartbeat-mutex*)
        (if (> (+ last-access
                  ;; (* 50 60 60)    ;; 48 hrs
                  ;; 60              ;; one minute
                  ;; (* 60 60)       ;; one hour
                  (* 45 60)          ;; 45 minutes, until the db deletion bug is fixed.
                  )
               (current-seconds))
            (begin
              (debug:print-info 2 "Server continuing, seconds since last db access: " (- (current-seconds) last-access))
              (loop 0))
            (begin
              (debug:print-info 0 "Starting to shutdown the server.")
              ;; need to delete only *my* server entry (future use)
              (set! *time-to-exit* #t)
              (open-run-close tasks:server-deregister-self tasks:open-db (get-host-name))
              (thread-sleep! 1)
              (debug:print-info 0 "Max cached queries was " *max-cache-size*)
              (debug:print-info 0 "Server shutdown complete. Exiting")
              (exit)))))))



;; all routes though here end in exit ...
(define (server:launch)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting the standalone server")
  (let ((hostinfo (open-run-close tasks:get-best-server tasks:open-db)))
    (if hostinfo
	(debug:print-info 2 "NOT starting new server, one is already running on " (car hostinfo) ":" (cadr hostinfo))
	(if *toppath* 
	    (let* ((th2 (make-thread (lambda ()
				       (server:run 
					(if (args:get-arg "-server")
					    (args:get-arg "-server")
					    "-"))) "Server run"))
		   ;; (th3 (make-thread (lambda ()(server:keep-running)) "Keep running"))
		   )
	      (set! *client-non-blocking-mode* #t)
	      ;; (thread-start! th1)
	      (thread-start! th2)
	      ;; (thread-start! th3)
	      (set! *didsomething* #t)
	      ;; (thread-join! th3)
	      (thread-join! th2)
	      )
	    (debug:print 0 "ERROR: Failed to setup for megatest")))
    (exit)))

(define (server:client-signal-handler signum)
  (handle-exceptions
   exn
   (debug:print " ... exiting ...")
   (let ((th1 (make-thread (lambda ()
			     (if (not *received-response*)
				 (receive-message* *runremote*))) ;; flush out last call if applicable
			   "eat response"))
	 (th2 (make-thread (lambda ()
			     (debug:print 0 "ERROR: Received ^C, attempting clean exit. Please be patient and wait a few seconds before hitting ^C again.")
			     (thread-sleep! 3) ;; give the flush three seconds to do it's stuff
			     (debug:print 0 "       Done.")
			     (exit 4))
			   "exit on ^C timer")))
     (thread-start! th2)
     (thread-start! th1)
     (thread-join! th2))))

(define (server:client-launch)
  (set-signal-handler! signal/int server:client-signal-handler)
   (if (server:client-setup)
       (debug:print-info 2 "connected as client")
       (begin
	 (debug:print 0 "ERROR: Failed to connect as client")
	 (exit))))

