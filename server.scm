
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
	(set! *runremote* #t)
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
	  
	  ;;======================================================================
	  ;; db specials here
	  ;;======================================================================
	  ;; remote call to open-run-close
	  (rpc:publish-procedure!
	   'rdb:open-run-close 
	   (lambda (procname . remargs)
	     (debug:print 4 "INFO: rdb:open-run-close " procname " " remargs)
	     (set! *last-db-access* (current-seconds))
	     (apply open-run-close (eval procname) remargs)))
	  
	  (rpc:publish-procedure!
	   'cdb:test-set-status-state
	   (lambda (test-id status state)
	     (debug:print 4 "INFO: cdb:test-set-status-state " procname " " remargs)
	     (apply cdb:test-set-status-state remargs)))

	  ;;======================================================================
	  ;; end of publish-procedure section
	  ;;======================================================================

	  (set! *rpc:listener* rpc:listener)
	  (on-exit (lambda ()
		     (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER' and val=?;" host:port)
		     (sqlite3:finalize! db)))
	  (thread-start! th1)
	  (thread-start! th2)
	  ;; (thread-join!  th2)
	  ;; return th2 for the calling process to do a join with 
	  th2
	  )))) ;; rpc:server)))

(define (server:keep-running db)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 20) ;; no need to do this very often
    (let ((numrunning (db:get-count-tests-running db)))
      (if (or (not (> numrunning 0))
	      (> *last-db-access* (+ (current-seconds) 20)))
	  (begin
	    (debug:print 0 "INFO: Starting to shutdown the server side")
	    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER'"); ;;  AND val like ?;"
			  ;; host:port) ;; need to delete only *my* server entry (future use)
	    (thread-sleep! 10)
	    (debug:print 0 "INFO: Server shutdown complete. Exiting")
	    (exit))))
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
		 (print "Exception: " ((condition-property-accessor 'exn 'message) exn))
		 (open-run-close 
		  (lambda (db . param) 
		    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER'"))
		  #f)
		 (set! *runremote* #f))
	       (if (and (not (args:get-arg "-server")) ;; no point in the server using the server using the server
			((rpc:procedure 'serve:login host portn) *toppath*))
		   (begin
		     (debug:print 2 "INFO: Connected to " host ":" port)
		     (set! *runremote* (vector host portn)))
		   (begin
		     (debug:print 2 "INFO: Failed to connect to " host ":" port)
		     (set! *runremote* #f)))))
	    (debug:print 2 "INFO: no server available")))))

