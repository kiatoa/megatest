
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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo)
(import (prefix sqlite3 sqlite3:))

(declare (unit server))

(declare (uses common))
(declare (uses db))
(declare (uses tests))

(include "common_records.scm")
(include "db_records.scm")

 (define a (with-output-to-string (lambda ()(serialize '(1 2 3 "Hello and goodbye" #t)))))
 (define b (with-input-from-string a (lambda ()(deserialize))))


(define (server:run hostn)
  (debug:print 0 "Attempting to start the server ...")
  (let ((host:port      (open-run-close db:get-var db "SERVER"))) ;; do whe already have a server running?
    (if host:port 
	(set! *runremote* host:port)
	(let* ((zmq-socket     #f)
	       (hostname       (if (string=? "-" hostn)
				   (get-host-name) 
				   hostn))
	       (ipaddrstr      (let ((ipstr (if (string=? "-" hostn)
						(string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
						#f)))
				 (if ipstr ipstr hostname))))
	  (set! zmq-socket (server:find-free-port-and-open ipaddrstr))
	  (set! *cache-on* #t)
	  
	  ;; what to do when we quit
	  ;;
	  (on-exit (lambda ()
		     (open-run-close
		      (lambda (db . params)
			(sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER';"))
		      #f ;; for db
		      #f) ;; for a param
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
		   (params (with-input-from-string rawmsg (lambda ()(deserialize))))
		   (res    #f))
	      (debug:print-info 12 "server=> received msg=" msg)
	      (set! res (cdb:cached-access params))
	      (debug:print-info 12 "server=> processed msg=" msg)
	      (send-message zmq-socket res)
	      (loop)))))))

;; run server:keep-running in a parallel thread to monitor that the db is being 
;; used and to shutdown after sometime if it is not.
;;
(define (server:keep-running db host:port)
  ;; if none running or if > 20 seconds since 
  ;; server last used then start shutdown
  (let loop ((count 0))
    (thread-sleep! 20) ;; no need to do this very often
    (let ((numrunning (db:get-count-tests-running db)))
      (if (or (> numrunning 0)
	      (> (+ *last-db-access* 60)(current-seconds)))
	  (begin
	    (debug:print-info 0 "Server continuing, tests running: " numrunning ", seconds since last db access: " (- (current-seconds) *last-db-access*))
	    (loop (+ 1 count)))
	  (begin
	    (debug:print-info 0 "Starting to shutdown the server side")
	    ;; need to delete only *my* server entry (future use)
	    (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER';")
	    (thread-sleep! 10)
	    (debug:print-info 0 "Max cached queries was " *max-cache-size*)
	    (debug:print-info 0 "Server shutdown complete. Exiting")
	    ;; (exit)))
	    )))))

(define (server:find-free-port-and-open host s port)
  (let ((s (if s s (make-socket 'rep)))
	(p (if (number? port) port 5555)))
    (handle-exceptions
     exn
     (begin
       (print "Failed to bind to port " p ", trying next port")
       (server:find-free-port-and-open host s (+ p 1)))
     (let ((zmq-url (conc "tcp://" host ":" p)))
       (bind-socket s zmq-url)
       (set! *runremote* zmq-url)
       (debug:print 0 "Server started on " zmq-url)
       (db:set-var db "SERVER" zmq-url)
       s))))

(define (server:client-setup)
  (if *runremote*
      (begin
	(debug:print 0 "ERROR: Attempt to connect to server but already connected")
	#f)
      (let* ((hostinfo   (open-run-close db:get-var #f "SERVER"))
	     (zmq-socket (make-socket 'req)))
	(if hostinfo
	    (begin
	      (debug:print-info 2 "Setting up to connect to " hostinfo)
	      (handle-exceptions
	       exn
	       (begin
		 (debug:print 0 "ERROR: Failed to open a connection to the server at host: " host " port: " port)
		 (debug:print 0 "   EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
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

