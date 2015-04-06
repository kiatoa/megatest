
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
   ;; (if (common:get-remote remote)
   ;;    (apply (eval (string->symbol (conc "remote:" procstr))) params)
   (apply (eval (string->symbol procstr)) params)))

;; all routes though here end in exit ...
;;
;; start_server? 
;;
(define (rpc-transport:launch run-id area-dat)
  (set! *run-id*   run-id)
  (if (args:get-arg "-daemonize")
      (daemon:ize))
  (if (server:check-if-running run-id area-dat)
      (begin
	(debug:print 0 "INFO: Server for run-id " run-id " already running")
	(exit 0)))
  (let loop ((server-id (open-run-close tasks:server-lock-slot (lambda ()(tasks:open-db area-dat))run-id))
	     (remtries  4))
    (if (not server-id)
	(if (> remtries 0)
	    (begin
	      (thread-sleep! 2)
	      (loop (open-run-close tasks:server-lock-slot (lambda ()(tasks:open-db area-dat)) run-id)
		    (- remtries 1)))
	    (begin
	      ;; since we didn't get the server lock we are going to clean up and bail out
	      (debug:print-info 2 "INFO: server pid=" (current-process-id) ", hostname=" (get-host-name) " not starting due to other candidates ahead in start queue")
	      (open-run-close tasks:server-delete-records-for-this-pid (lambda ()(tasks:open-db area-dat)) " rpc-transport:launch")))
	(begin
	  (rpc-transport:run (if (args:get-arg "-server")(args:get-arg "-server") "-") run-id server-id)
	  (exit)))))

(define (rpc-transport:run hostn run-id server-id area-dat)
  (debug:print 2 "Attempting to start the rpc server ...")
   ;; (trace rpc:publish-procedure!)

  (rpc:publish-procedure! 'server:login server:login)
  (rpc:publish-procedure! 'testing (lambda () "Just testing"))

  (let* ((configdat       (megatest:area-configdat area-dat))
	 (db              #f)
	 (hostname        (get-host-name))
	 (ipaddrstr       (let ((ipstr (if (string=? "-" hostn)
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
					   #f)))
			    (if ipstr ipstr hostn))) ;; hostname))) 
	 (start-port      (open-run-close tasks:server-get-next-port (lambda ()(tasks:open-db area-dat))))
	 (link-tree-path  (configf:lookup configdat "setup" "linktree"))
	 (rpc:listener    (rpc-transport:find-free-port-and-open (rpc:default-server-port)))
	 (th1             (make-thread
			   (lambda ()
			     ((rpc:make-server rpc:listener) #t))
			   "rpc:server"))
			   ;; (cute (rpc:make-server rpc:listener) "rpc:server")
			   ;; 'rpc:server))
	 (hostname        (if (string=? "-" hostn)
			      (get-host-name) 
			      hostn))
	 (ipaddrstr       (if (string=? "-" hostn)
			      (server:get-best-guess-address hostname) ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
			      #f))
	 (portnum         (rpc:default-server-port))
	 (host:port       (conc (if ipaddrstr ipaddrstr hostname) ":" portnum))
	 (tdb             (tasks:open-db area-dat)))
    (thread-start! th1)
    (set! db *inmemdb*)
    (open-run-close tasks:server-set-interface-port 
		    (lambda ()(tasks:open-db area-dat))
		    server-id 
		    ipaddrstr portnum)
    (debug:print 0 "Server started on " host:port)
    
    ;; (trace rpc:publish-procedure!)
    ;; (rpc:publish-procedure! 'server:login server:login)
    ;; (rpc:publish-procedure! 'testing (lambda () "Just testing"))

    ;;======================================================================
    ;;	  ;; end of publish-procedure section
    ;;======================================================================
    ;;
    (on-exit (lambda ()
	       (open-run-close tasks:server-set-state! (lambda ()(tasks:open-db area-dat)) server-id "stopped")))

    (set! *rpc:listener* rpc:listener)
    (tasks:server-set-state! tdb server-id "running")
    (set! *inmemdb*  (db:setup run-id))
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
	      (open-run-close tasks:server-delete-record (lambda ()(tasks:open-db area-dat)) server-id " rpc-transport:try-start-server stop")
	      (thread-sleep! 10)
	      (debug:print-info 0 "Max cached queries was " *max-cache-size*)
	      (debug:print-info 0 "Server shutdown complete. Exiting")
	      ))))))

(define (rpc-transport:find-free-port-and-open port)
  (handle-exceptions
   exn
	  (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (rpc-transport:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-read-timeout 240000)
   (tcp-listen (rpc:default-server-port) 10000)))

(define (rpc-transport:ping run-id host port area-dat)
  (handle-exceptions
   exn
   (begin
     (print "SERVER_NOT_FOUND")
     (exit 1))
   (let ((login-res ((rpc:procedure 'server:login host port) (megatest:area-path area-dat))))
     (if (and (list? login-res)
	      (car login-res))
	 (begin
	   (print "LOGIN_OK")
	   (exit 0))
	 (begin
	   (print "LOGIN_FAILED")
	   (exit 1))))))

(define (rpc-transport:client-setup run-id area-dat #!key (remtries 10))
  (if (common:get-remote remote run-id)
      (begin
	(debug:print 0 "ERROR: Attempt to connect to server but already connected")
	#f)
      (let* ((toppath (megatest:area-path area-dat))
	     (host-info (common:get-remote remote run-id))) ;; (open-run-close db:get-var #f "SERVER"))
	(if host-info
	    (let ((iface    (car host-info))
		  (port     (cadr host-info))
		  (ping-res ((rpc:procedure 'server:login host port) toppath)))
	      (if ping-res
		  (let ((server-dat (list iface port #f #f #f)))
		    (common:set-remote! remote run-id server-dat)
		    server-dat)
		  (begin
		    (server:try-running run-id)
		    (thread-sleep! 2)
		    (rpc-transport:client-setup run-id area-dat remtries: (- remtries 1)))))
 	    (let* ((server-db-info (open-run-close tasks:get-server (lambda ()(tasks:open-db area-dat)) run-id)))
 	      (debug:print-info 0 "client:setup server-dat=" server-dat ", remaining-tries=" remaining-tries)
	      (if server-db-info
 		  (let* ((iface     (tasks:hostinfo-get-interface server-db-info))
 			 (port      (tasks:hostinfo-get-port      server-db-info))
			 (server-dat (list iface port #f #f #f))
 			 (ping-res  ((rpc:procedure 'server:login host port) toppath)))
 		    (if start-res
 			(begin
 			  (common:set-remote! remote run-id server-dat)
			  server-dat)
			(begin
			  (server:try-running run-id)
			  (thread-sleep! 2)
			  (rpc-transport:client-setup run-id area-dat remtries: (- remtries 1)))))
		  (begin
		    (server:try-running run-id)
		    (thread-sleep! 2)
		    (rpc-transport:client-setup run-id area-dat remtries: (- remtries 1)))))))))
;; 
;; 	     (port     (if (and hostinfo (> (length hostdat) 1))(cadr hostdat) #f)))
;; 	(if (and port
;; 		 (string->number port))
;; 	    (let ((portn (string->number port)))
;; 	      (debug:print-info 2 "Setting up to connect to host " host ":" port)
;; 	      (handle-exceptions
;; 	       exn
;; 	       (begin
;; 		 (debug:print 0 "ERROR: Failed to open a connection to the server at host: " host " port: " port)
;; 		 (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
;; 		 ;; (open-run-close 
;; 		 ;;  (lambda (db . param) 
;; 		 ;;    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER'"))
;; 		 ;;  #f)
;; 		 (set! (common:get-remote remote) #f))
;; 	       (if (and (not (args:get-arg "-server")) ;; no point in the server using the server using the server
;; 			((rpc:procedure 'server:login host portn) toppath))
;; 		   (begin
;; 		     (debug:print-info 2 "Logged in and connected to " host ":" port)
;; 		     (set! (common:get-remote remote) (vector host portn)))
;; 		   (begin
;; 		     (debug:print-info 2 "Failed to login or connect to " host ":" port)
;; 		     (set! (common:get-remote remote) #f)))))
;; 	    (debug:print-info 2 "no server available")))))

