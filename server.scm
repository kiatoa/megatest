
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp rpc)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo)
(import (prefix sqlite3 sqlite3:))

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))

(include "common_records.scm")
(include "db_records.scm")

;; procstr is the name of the procedure to be called as a string
(define (server:autoremote procstr params)
  (handle-exceptions
   exn
   (begin
     (debug:print 1 "Remote failed for " proc " " params)
     (apply (eval (string->symbol procstr)) params))
   ;; (if *runremote*
   ;;    (apply (eval (string->symbol (conc "remote:" procstr))) params)
   (apply (eval (string->symbol procstr)) params)))

(define (server:start db hostn)
  (debug:print 0 "Attempting to start the server ...")
  (let ((host:port      (db:get-var db "SERVER"))) ;; do whe already have a server running?
    (if host:port 
	(set! *runremote* (let* ((lst  (string-split host:port ":"))
				 (port (if (> (length lst) 1)
					   (string->number (cadr lst))
					   #f)))
			    (if port (vector (car lst) port) #f)))
	(let* ((rpc:listener   (server:find-free-port-and-open (rpc:default-server-port)))
	       (th1            (make-thread
				(cute (rpc:make-server rpc:listener) "rpc:server")
				'rpc:server))
	       (th2            (make-thread (lambda ()(db:updater))))
	       (hostname       (if (string=? "-" hostn)
				   (get-host-name) 
				   hostn))
	       (ipaddrstr      (if (string=? "-" hostn)
				   (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
				   #f))
	       (host:port      (conc (if ipaddrstr ipaddrstr hostname) ":" (rpc:default-server-port))))
	  (db:set-var db "SERVER" host:port)
	  (set! *cache-on* #t)
	  
	  ;; can use this to run most anything at the remote
	  (rpc:publish-procedure! 
	   'remote:run 
	   (lambda (procstr . params)
	     (server:autoremote procstr params)))
	  
	  (rpc:publish-procedure!
	   'server:login
	   (lambda (toppath)
	     (set! *last-db-access* (current-seconds))
	     (if (equal? *toppath* toppath)
		 (begin
		   (debug:print 2 "INFO: login successful")
		   #t)
		 #f)))

	  ;;======================================================================
	  ;; db specials here
	  ;;======================================================================
	  ;; remote call to open-run-close
	  (rpc:publish-procedure!
	   'rdb:open-run-close 
	   (lambda (procname . remargs)
	     (debug:print 4 "INFO: Remote call of rdb:open-run-close " procname " " remargs)
	     (set! *last-db-access* (current-seconds))
	     (apply open-run-close (eval procname) remargs)))
	  
	  (rpc:publish-procedure!
	   'cdb:test-set-status-state
	   (lambda (test-id status state msg)
	     (debug:print 4 "INFO: Remote call of cdb:test-set-status-state test-id=" test-id ", status=" status ", state=" state ", msg=" msg)
	     (cdb:test-set-status-state test-id status state msg)))

	  (rpc:publish-procedure!
	   'cdb:test-rollup-iterated-pass-fail
	   (lambda (test-id)
	     (debug:print 4 "INFO: Remote call of cdb:test-rollup-iterated-pass-fail " test-id)
	     (cdb:test-rollup-iterated-pass-fail test-id)))

	  (rpc:publish-procedure!
	   'cdb:pass-fail-counts
	   (lambda (test-id fail-count pass-count)
	     (debug:print 4 "INFO: Remote call of cdb:pass-fail-counts " test-id " passes: " pass-count " fails: " fail-count)
	     (cdb:pass-fail-counts test-id fail-count pass-count)))

	  (rpc:publish-procedure!
	   'cdb:tests-register-test
	   (lambda (db run-id test-name item-path)
	     (debug:print 4 "INFO: Remote call of cdb:tests-register-test " run-id " testname: " test-name " item-path: " item-path)
	     (cdb:tests-register-test db run-id test-name item-path)))

	  (rpc:publish-procedure!
	   'cdb:flush-queue
	   (lambda ()
	     (debug:print 4 "INFO: Remote call of cdb:flush-queue")
	     (cdb:flush-queue)))

	  ;;======================================================================
	  ;; end of publish-procedure section
	  ;;======================================================================

	  (set! *rpc:listener* rpc:listener)
	  (on-exit (lambda ()
		     (open-run-close
		      (lambda (db . params)
			(sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER' and val=?;" host:port))
		      #f ;; for db
		      #f) ;; for a param
		     (let loop ((n 0))
		       (let ((queue-len 0))
			 (thread-sleep! (random 5))
			 (mutex-lock! *incoming-mutex*)
			 (set! queue-len (length *incoming-data*))
			 (mutex-unlock! *incoming-mutex*)
			 (if (> queue-len 0)
			     (begin
			       (debug:print 0 "INFO: Queue not flushed, waiting ...")
			       (loop (+ n 1)))))
		      )))
	  (thread-start! th1)
	  (debug:print 0 "Server started...")
	  (thread-start! th2)
	  ;; (thread-join!  th2)
	  ;; return th2 for the calling process to do a join with 
	  th2
	  )))) ;; rpc:server)))

(define (server:keep-running db host:port)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 20) ;; no need to do this very often
    (let ((numrunning (db:get-count-tests-running db)))
      (if (or (not (> numrunning 0))
	      (> *last-db-access* (+ (current-seconds) 60)))
	  (begin
	    (debug:print 0 "INFO: Starting to shutdown the server side")
	    ;; need to delete only *my* server entry (future use)
	    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER' AND val like ?;"  host:port)
	    (thread-sleep! 10)
	    (debug:print 0 "INFO: Server shutdown complete. Exiting")
	    (exit))
	  (debug:print 0 "INFO: Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
	  ))
    (loop (+ 1 count))))

(define (server:find-free-port-and-open port)
  (handle-exceptions
   exn
   (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (server:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-listen (rpc:default-server-port))))

(define (server:client-setup)
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
	      (debug:print 2 "INFO: Setting up to connect to host " host ":" port)
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
		     (debug:print 2 "INFO: Connected to " host ":" port)
		     (set! *runremote* (vector host portn)))
		   (begin
		     (debug:print 2 "INFO: Failed to connect to " host ":" port)
		     (set! *runremote* #f)))))
	    (debug:print 2 "INFO: no server available")))))

