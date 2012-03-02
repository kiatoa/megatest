
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
     (apply (eval (string->symbol proc)) params))
   (if *runremote*
       (apply (eval (string->symbol (conc "remote:" procstr))) params)
       (eval (string->symbol procstr) params))))

(define (server:start db hostn)
  (debug:print 0 "Attempting to start the server ...")
  (let* ((rpc:listener   (server:find-free-port-and-open (rpc:default-server-port)))
	 (th1            (make-thread
			  (cute (rpc:make-server rpc:listener) "rpc:server")
			  'rpc:server))
	 (th2            (make-thread (lambda ()(db:updater db))))
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
    ;; ** set-tests-state-status
    (rpc:publish-procedure!
     'rdb:set-tests-state-status 
     (lambda (run-id testnames currstate currstatus newstate newstatus)
       (db:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)))

    (rpc:publish-procedure!
     'rdb:teststep-set-status!
     (lambda (run-id test-name teststep-name state-in status-in item-path comment logfile)
       (db:teststep-set-status! db run-id test-name teststep-name state-in status-in item-path comment logfile)))

    (rpc:publish-procedure!
     'rdb:test-update-meta-info
     (lambda (run-id testname item-path minutes cpuload diskfree tmpfree)
       (db:test-update-meta-info db run-id testname item-path minutes cpuload diskfree tmpfree)))
     
    (rpc:publish-procedure!
     'rdb:test-set-state-status-by-run-id-testname
     (lambda (run-id test-name item-path status state)
       (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)))

    (rpc:publish-procedure!
     'rdb:csv->test-data 
     (lambda (test-id csvdata)
       (db:csv->data db test-id csvdata)))

    (rpc:publish-procedure!
     'rdb:roll-up-pass-fail-counts
     (lambda (run-id test-name item-path status)
       (db:roll-up-pass-fail-counts db run-id test-name item-path status)))

    (rpc:publish-procedure!
     'rdb:test-set-comment 
     (lambda (run-id test-name item-path comment)
       (db:test-set-comment db run-id test-name item-path comment)))
    
    (rpc:publish-procedure!
     'rdb:test-set-log!
     (lambda (run-id test-name item-path logf)
       (db:test-set-log! db run-id test-name item-path logf)))
    
    (rpc:publish-procedure!
     'rpc:get-test-data-by-id
     (lambda (test-id)
       (db:get-test-data-by-id db test-id)))

    (rpc:publish-procedure!
     'serve:get-toppath
     (lambda ()
       *toppath*))

    (rpc:publish-procedure!
     'serve:login
     (lambda (toppath)
       (if (equal? *toppath* toppath)
	   (begin
	     (debug:print 2 "INFO: login successful")
	     #t)
	   #f)))	     
    
    (rpc:publish-procedure!
     'rdb:get-runs
     (lambda (runnamepatt numruns startrunoffset keypatts)
       (db:get-runs db runnamepatt numruns startrunoffset keypatts)))

    (rpc:publish-procedure!
     'rdb:get-tests-for-run 
     (lambda (run-id testpatt itempatt states statuses)
       (db:get-tests-for-run db run-id testpatt itempatt states statuses)))

    (rpc:publish-procedure!
     'rdb:get-keys
     (lambda ()
       (db:get-keys db)))

    (rpc:publish-procedure!
     'rdb:get-num-runs
     (lambda (runpatt)
       (db:get-num-runs db runpatt)))

    (rpc:publish-procedure!
     'rdb:test-set-state-status-by-id
     (lambda (test-id newstate newstatus newcomment)
       (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)))

    (rpc:publish-procedure!
     'rdb:get-key-val-pairs
     (lambda (run-id)
       (db:get-key-val-pairs db run-id)))

    (rpc:publish-procedure!
     'rdb:get-key-vals
     (lambda (run-id)
       (db:get-key-vals db run-id)))

    (rpc:publish-procedure!
     'rdb:testmeta-get-record
     (lambda (run-id)
       (db:testmeta-get-record db run-id)))

    (rpc:publish-procedure!
     'rdb:get-test-data-by-id
     (lambda (test-id)
       (db:get-test-data-by-id db test-id)))

    (rpc:publish-procedure!
     'rdb:get-run-info
     (lambda (run-id)
       (db:get-run-info db run-id)))

    (rpc:publish-procedure!
     'rdb:get-steps-for-test
     (lambda (test-id)
       (db:get-steps-for-test db test-id)))

    (rpc:publish-procedure!
     'rdb:get-steps-table
     (lambda (test-id)
       (db:get-steps-table db test-id)))

    (rpc:publish-procedure!
     'rdb:read-test-data
     (lambda (test-id categorypatt)
       (db:read-test-data db test-id categorypatt)))

    (rpc:publish-procedure!
     'rdb:get-test-info
     (lambda (run-id testname item-path)
       (db:get-test-info db  run-id testname item-path)))

    (rpc:publish-procedure!
     'rdb:delete-test-records
     (lambda (test-id)
       (db:delete-test-records db test-id)))

    (rpc:publish-procedure!
     'rtests:register-test
     (lambda (run-id test-name item-path)
       (tests:register-test db run-id test-name item-path)))

    (rpc:publish-procedure!
     'rdb:test-data-rollup
     (lambda (test-id status)
       (db:test-data-rollup db test-id status)))
    
    (rpc:publish-procedure!
     'rtests:test-set-status!
     (lambda (run-id test-name state status itemdat-or-path comment dat)
       (test-set-status! db run-id test-name state status itemdat-or-path comment dat)))

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
    )) ;; rpc:server)))

(define (server:find-free-port-and-open port)
  (handle-exceptions
   exn
   (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (server:find-free-port-and-open (+ port 1)))
   (rpc:default-server-port port)
   (tcp-listen (rpc:default-server-port))))

(define (server:client-setup db)
  (if *runremote*
      (debug:print 0 "ERROR: Attempt to connect to server but already connected")
      (let* ((hostinfo (db:get-var db "SERVER"))
	     (hostdat  (if hostinfo (string-split hostinfo ":")))
	     (host     (if hostinfo (car hostdat)))
	     (port     (if (and hostinfo (> (length hostdat) 1))(cadr hostdat) #f)))
	(if (and port
		 (string->number port))
	    (let ((portn (string->number port)))
	      (debug:print 2 "INFO: Setting up to connect to host " host ":" port)
	      (handle-exceptions
	       exn
	       (begin
		 (print "Exception: " exn)
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

