
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest zmq)
(import (prefix sqlite3 sqlite3:))

(use spiffy uri-common intarweb http-client spiffy-request-vars)

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.
(declare (uses http-transport))
(declare (uses zmq-transport))

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

(define *db:process-queue-mutex* (make-mutex))

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
	 (db              #f) ;;        (open-db)) ;; we don't want the server to be opening and closing the db unnecesarily
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname)))
	 (start-port    (if (args:get-arg "-port")
			    (string->number (args:get-arg "-port"))
			    (+ 5000 (random 1001))))
	 (link-tree-path (config-lookup *configdat* "setup" "linktree")))
    (set! *cache-on* #t)
    (root-path     (if link-tree-path 
		       link-tree-path
		       (current-directory))) ;; WARNING: SECURITY HOLE. FIX ASAP!

    ;; Setup the web server and a /ctrl interface
    ;;
    (vhost-map `(((* any) . ,(lambda (continue)
			       ;; open the db on the first call 
			       (if (not db)(set! db (open-db)))
			       (let* (($   (request-vars source: 'both))
				      (dat ($ 'dat))
				      (res #f))
				 (cond
				  ((equal? (uri-path (request-uri (current-request))) 
					   '(/ "hey"))
				   (send-response body: "hey there!\n"
						  headers: '((content-type text/plain))))
				  ;; This is the /ctrl path where data is handed to the server and
				  ;; responses 
				  ((equal? (uri-path (request-uri (current-request)))
					   '(/ "ctrl"))
				   (let* ((packet (db:string->obj dat))
					  (qtype  (cdb:packet-get-qtype packet)))
				     (debug:print-info 12 "server=> received packet=" packet)
				     (if (not (member qtype '(sync ping)))
					 (begin
					   (mutex-lock! *heartbeat-mutex*)
					   (set! *last-db-access* (current-seconds))
					   (mutex-unlock! *heartbeat-mutex*)))
				     ;; (mutex-lock! *db:process-queue-mutex*) ;; trying a mutex
				     ;; (set! res (open-run-close db:process-queue-item open-db packet))
				     (set! res (db:process-queue-item db packet))
				     ;; (mutex-unlock! *db:process-queue-mutex*)
				     (debug:print-info 11 "Return value from db:process-queue-item is " res)
				     (send-response body: (conc "<head>ctrl data</head>\n<body>"
								res
								"</body>")
						    headers: '((content-type text/plain)))))
				  (else (continue))))))))
    (server:try-start-server ipaddrstr start-port)
    ;; lite3:finalize! db)))
    ))


(define (server:mk-signature)
  (message-digest-string (md5-primitive) 
			 (with-output-to-string
			   (lambda ()
			     (write (list (current-directory)
					  (argv)))))))

;;======================================================================
;; S E R V E R   U T I L I T I E S 
;;======================================================================

;; When using zmq this would send the message back (two step process)
;; with spiffy or rpc this simply returns the return data to be returned
;; 
(define (server:reply return-addr query-sig success/fail result)
  (debug:print-info 11 "server:reply return-addr=" return-addr ", result=" result)
  ;; (send-message pubsock target send-more: #t)
  ;; (send-message pubsock 
  (case *transport-type*
    ((fs) result)
    ((http)(db:obj->string (vector success/fail query-sig result)))
    ((zmq)
     (let ((pub-socket (vector-ref *runremote* 1)))
       (send-message pub-socket return-addr send-more: #t)
       (send-message pub-socket (db:obj->string (vector success/fail query-sig result)))))
    (else 
     (debug:print 0 "ERROR: unrecognised transport type: " *transport-type*)
     result)))

;;======================================================================
;; C L I E N T S
;;======================================================================

(define (server:get-client-signature)
  (if *my-client-signature* *my-client-signature*
      (let ((sig (server:mk-signature)))
	(set! *my-client-signature* sig)
	*my-client-signature*)))

(define (server:client-login serverdat)
  (cdb:login serverdat *toppath* (server:get-client-signature)))

;; Not currently used! But, I think it *should* be used!!!
(define (server:client-logout serverdat)
  (let ((ok (and (socket? serverdat)
		 (cdb:logout serverdat *toppath* (server:get-client-signature)))))
    ok))

;; Do all the connection work, look up the transport type and set up the
;; connection if required.
;;
;; There are two scenarios. 
;;   1. We are a test manager and we received *transport-type* and *runremote* via cmdline
;;   2. We are a run tests, list runs or other interactive process and we mush figure out
;;      *transport-type* and *runremote* from the monitor.db
;;
(define (server:client-setup #!key (numtries 50))
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
    ;; DEBUG STUFF
    (if (eq? *transport-type* 'fs)(begin (print "ERROR!!!!!!! refusing to run with transport " *transport-type*)(exit 99)))
    
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


;; all routes though here end in exit ...
(define (server:launch transport)
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: cannot find megatest.config, exiting")
	    (exit))))
  (debug:print-info 2 "Starting server using " transport " transport")
  (set! *transport-type* transport)
  (case transport
    ((fs)   (exit)) ;; there is no "fs" transport
    ((http) (http-transport:launch))
    ((zmq)  (zmq-transport:launch))
    (else
     (debug:print "WARNING: unrecognised transport " transport)
     (exit))))

(define (server:client-signal-handler signum)
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

(define (server:client-launch)
  (set-signal-handler! signal/int server:client-signal-handler)
   (if (server:client-setup)
       (debug:print-info 2 "connected as client")
       (begin
	 (debug:print 0 "ERROR: Failed to connect as client")
	 (exit))))

