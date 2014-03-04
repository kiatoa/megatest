
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n rpc)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(declare (unit rpc-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

;; procstr is the name of the procedure to be called as a string
(define (rpc-transport:autoremote procstr params)
  (handle-exceptions
   exn
   (begin
     (debug:print 1 "Remote failed for " proc " " params)
     (apply (eval (string->symbol procstr)) params))
   ;; (if *runremote*
   ;;    (apply (eval (string->symbol (conc "remote:" procstr))) params)
   (apply (eval (string->symbol procstr)) params)))

;; all routes though here end in exit ...
;;
;; start_server? 
;;
(define (rpc-transport:launch run-id)
  (set! *run-id*   run-id)
  (if (args:get-arg "-daemonize")
      (daemon:ize))
  (if (server:check-if-running run-id)
      (begin
	(debug:print 0 "INFO: Server for run-id " run-id " already running")
	(exit 0)))
  (let loop ((server-id (open-run-close tasks:server-lock-slot tasks:open-db run-id))
	     (remtries  4))
    (if (not server-id)
	(if (> remtries 0)
	    (begin
	      (thread-sleep! 2)
	      (loop (open-run-close tasks:server-lock-slot tasks:open-db run-id)
		    (- remtries 1)))
	    (begin
	      ;; since we didn't get the server lock we are going to clean up and bail out
	      (debug:print-info 2 "INFO: server pid=" (current-process-id) ", hostname=" (get-host-name) " not starting due to other candidates ahead in start queue")
	      (open-run-close tasks:server-delete-records-for-this-pid tasks:open-db " rpc-transport:launch")
	      ))
	(let* ((th2 (make-thread (lambda ()
				   (rpc-transport:run 
				    (if (args:get-arg "-server")
					(args:get-arg "-server")
					"-")
				    run-id
				    server-id)) "Server run"))
	       (th3 (make-thread (lambda ()
				   (rpc-transport:keep-running run-id server-id))
				 "Keep running")))
	  ;; Database connection
	  (set! *inmemdb*  (db:setup run-id))
	  (thread-start! th2)
	  (thread-start! th3)
	  (set! *didsomething* #t)
	  (thread-join! th3)
	  (exit)))))

(define (rpc-transport:run hostn run-id server-id)
  (debug:print 2 "Attempting to start the rpc server ...")
  (let* ((db              #f)
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname))) 
	 (start-port      (open-run-close tasks:server-get-next-port tasks:open-db))
	 (link-tree-path  (configf:lookup *configdat* "setup" "linktree"))
	 (rpc:listener   (rpc-transport:find-free-port-and-open (rpc:default-server-port)))
	 (th1            (make-thread
			  (cute (rpc:make-server rpc:listener) "rpc:server")
			  'rpc:server))
	 (hostname       (if (string=? "-" hostn)
			     (get-host-name) 
			     hostn))
	 (ipaddrstr      (if (string=? "-" hostn)
			     (server:get-best-guess-address hostname) ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
			     #f))
	 (portnum        (rpc:default-server-port))
	 (host:port      (conc (if ipaddrstr ipaddrstr hostname) ":" portnum))
	 (tdb            (tasks:open-db)))
    (set! db *inmemdb*)
    (open-run-close tasks:server-set-interface-port 
		    tasks:open-db 
		    server-id 
		    ipaddrstr portnum)
    (debug:print 0 "Server started on " host:port)
    
    ;; can use this to run most anything at the remote
    (rpc:publish-procedure! 
     'remote:run 
     (lambda (procstr . params)
       (rpc-transport:autoremote procstr params)))
    
    ;;    (rpc:publish-procedure!
    ;;     'server:login
    ;;     (lambda (toppath)
    ;;       (set! *last-db-access* (current-seconds))
    ;;       (if (equal? *toppath* toppath)
    ;;	   (begin
    ;;	     (debug:print-info 2 "login successful")
    ;;	     #t)
    ;;	   #f)))
    ;;
    ;;	  ;;======================================================================
    ;;	  ;; db specials here
    ;;	  ;;======================================================================
    ;;	  ;; remote call to open-run-close
    ;;	  (rpc:publish-procedure!
    ;;	   'rdb:open-run-close 
    ;;	   (lambda (procname . remargs)
    ;;	     (debug:print-info 12 "Remote call of rdb:open-run-close " procname " " remargs)
    ;;					   (set! *last-db-access* (current-seconds))
    ;;	     (apply open-run-close (eval procname) remargs)))
    ;;
    ;;	  (rpc:publish-procedure!
    ;;	   'cdb:test-set-status-state
    ;;	   (lambda (test-id status state msg)
    ;;	     (debug:print-info 12 "Remote call of cdb:test-set-status-state test-id=" test-id ", status=" status ", state=" state ", msg=" msg)
    ;;	     (cdb:test-set-status-state test-id status state msg)))
    ;;
    ;;	  (rpc:publish-procedure!
    ;;	   'cdb:test-rollup-test_data-pass-fail
    ;;	   (lambda (test-id)
    ;;	     (debug:print-info 12 "Remote call of cdb:test-rollup-test_data-pass-fail " test-id)
    ;;	     (cdb:test-rollup-test_data-pass-fail test-id)))
    ;;
    ;;	  (rpc:publish-procedure!
    ;;	   'cdb:pass-fail-counts
    ;;	   (lambda (test-id fail-count pass-count)
    ;;	     (debug:print-info 12 "Remote call of cdb:pass-fail-counts " test-id " passes: " pass-count " fails: " fail-count)
    ;;	     (cdb:pass-fail-counts test-id fail-count pass-count)))
    ;;
    ;;	  (rpc:publish-procedure!
    ;;	   'cdb:tests-register-test
    ;;	   (lambda (db run-id test-name item-path)
    ;;	     (debug:print-info 12 "Remote call of cdb:tests-register-test " run-id " testname: " test-name " item-path: " item-path)
    ;;	     (cdb:tests-register-test db run-id test-name item-path)))
    ;;
    ;;	  (rpc:publish-procedure!
    ;;	   'cdb:flush-queue
    ;;			   (lambda ()
    ;;	     (debug:print-info 12 "Remote call of cdb:flush-queue")
    ;;	     (cdb:flush-queue)))
    ;;

    ;;======================================================================
    ;;	  ;; end of publish-procedure section
    ;;======================================================================
    ;;
    (on-exit (lambda ()
	       (open-run-close tasks:server-set-state! tasks:open-db server-id "stopped")))

    (thread-start! th1)

    (set! *rpc:listener* rpc:listener)
    (tasks:server-set-state! tdb server-id "running")
    ; (sqlite3:finalize! tdb)
    th1
    )) ;; rpc:server)))

(define (rpc-transport:keep-running run-id server-id)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 5) ;; no need to do this very often
    (let ((numrunning -1)) ;; (db:get-count-tests-running db)))
      (if (or (> numrunning 0)
	      (> (+ *last-db-access* 60)(current-seconds)))
	  (begin
	    (debug:print-info 0 "Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
	    (loop (+ 1 count)))
	  (begin
	    (debug:print-info 0 "Starting to shutdown the server side")
	    (open-run-close tasks:server-delete-record tasks:open-db server-id " rpc-transport:try-start-server stop")
	    (thread-sleep! 10)
	    (debug:print-info 0 "Max cached queries was " *max-cache-size*)
	    (debug:print-info 0 "Server shutdown complete. Exiting")
	    )))))

(define (rpc-transport:find-free-port-and-open port)
  (handle-exceptions
   exn
	  (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (rpc-transport:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-read-timeout 240000)
   (tcp-listen (rpc:default-server-port) 10000)))

(define (rpc:ping run-id host-port)
  #f)

(define (rpc-transport:client-setup)
  (if *runremote*
      (begin
	(debug:print 0 "ERROR: Attempt to connect to server but already connected")
	#f)
      (let* ((hostinfo (open-run-close db:get-var #f "SERVER"))
	     (hostdat  (if hostinfo (string-split hostinfo ":") #f))
	     (host     (if hostinfo (car hostdat) #f))
	     (port     (if (and hostinfo (> (length hostdat) 1))(cadr hostdat) #f)))
	(if (and port
		 (string->number port))
	    (let ((portn (string->number port)))
	      (debug:print-info 2 "Setting up to connect to host " host ":" port)
  (handle-exceptions
   exn
       (begin
		 (debug:print 0 "ERROR: Failed to open a connection to the server at host: " host " port: " port)
		 (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
		 ;; (open-run-close 
		 ;;  (lambda (db . param) 
		 ;;    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER'"))
		 ;;  #f)
		 (set! *runremote* #f))
	       (if (and (not (args:get-arg "-server")) ;; no point in the server using the server using the server
			((rpc:procedure 'server:login host portn) *toppath*))
		   (begin
		     (debug:print-info 2 "Logged in and connected to " host ":" port)
		     (set! *runremote* (vector host portn)))
		   (begin
		     (debug:print-info 2 "Failed to login or connect to " host ":" port)
		     (set! *runremote* #f)))))
	    (debug:print-info 2 "no server available")))))

