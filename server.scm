
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp rpc)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit server))

(declare (uses common))
(declare (uses db))

(include "common_records.scm")
(include "db_records.scm")

;; procstr is the name of the procedure to be called as a string
(define (server:autoremote procstr params)
  (handle-exceptions
   exn
   (begin
     (debug:print 1 "Remote failed for " proc " " params)
     (apply (eval (string->symbol proc)) params))
   (if *runremote*
       (apply (eval (string->symbol (conc "remote:" procstr))) params)
       (eval (string->symbol procstr) params))))

(define (server:start db)
  (debug:print 0 "Attempting to start the server ...")
  (let* ((rpc:listener (server:find-free-port-and-open (rpc:default-server-port)))
	 (th1          (make-thread
			(cute (rpc:make-server rpc:listener) "rpc:server")
			'rpc:server))
	 (host:port    (conc (get-host-name) ":" (rpc:default-server-port))))
    (db:set-var db "SERVER" host:port)
    (rpc:publish-procedure! 
     'remote:run 
     (lambda (procstr . params)
       (server:autoremote procstr params)))

    ;;======================================================================
    ;; db specials here
    ;;======================================================================
    ;; ** set-tests-state-status
    (rpc:publish-procedure!
     'rdb:set-tests-state-status 
     (lambda (run-id testnames currstate currstatus newstate newstatus)
       ;; (debug:print 2 "rdb:set-tests-state-status newstate: " newstate " newstatus: " newstatus)
       (db:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)))

    (rpc:publish-procedure!
     'rdb:teststep-set-status!
     (lambda (run-id test-name teststep-name state-in status-in item-path comment logfile)
       ;; (debug:print 2 "rdb:teststep-state-set-status! test-name: " test-name " teststep-name: " teststep-name)
       (db:teststep-set-status! db run-id test-name teststep-name state-in status-in item-path comment logfile)))

    (set! *rpc:listener* rpc:listener)
    (on-exit (lambda ()
	       (sqlite3:execute db "DELETE FROM metadat WHERE var='SERVER' and val=?;" host:port)
	       (sqlite3:finalize! db)))
    (thread-start! th1)
    (thread-join! th1))) ;; rpc:server)))

(define (server:find-free-port-and-open port)
  (handle-exceptions
   exn
   (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (server:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-listen (rpc:default-server-port))))

(define (server:client-setup db)
  (let* ((hostinfo (db:get-var db "SERVER"))
	 (hostdat  (if hostinfo (string-split hostinfo ":")))
	 (host     (if hostinfo (car hostdat)))
	 (port     (if (and hostinfo (> (length hostdat) 1))(cadr hostdat) #f)))
    (if (and port
	     (string->number port))
	(debug:print 2 "INFO: Setting up to connect to host " host ":" port))
    (set! *runremote* (if port (vector host (string->number port)) #f))))

