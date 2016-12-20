;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(use sqlite3 srfi-18)
(import (prefix sqlite3 sqlite3:))
(include "/nfs/site/disks/icf_fdk_cw_gwa002/srehman/fossil/dbi/dbi.scm")
(import (prefix dbi dbi:))

(declare (unit lock-queue))
(declare (uses common))
(declare (uses tasks))

;;======================================================================
;; attempt to prevent overlapping updates of rollup files by queueing
;; update requests in an sqlite db
;;======================================================================

;;======================================================================
;; db record, <vector db path-to-db>
;;======================================================================

(define (make-lock-queue:db-dat)(make-vector 3))
(define-inline (lock-queue:db-dat-get-db        vec)    (vector-ref  vec 0))
(define-inline (lock-queue:db-dat-get-path      vec)    (vector-ref  vec 1))
(define-inline (lock-queue:db-dat-set-db!       vec val)(vector-set! vec 0 val))
(define-inline (lock-queue:db-dat-set-path!     vec val)(vector-set! vec 1 val))

(define (lock-queue:delete-lock-db dbdat)
  (let ((fname (lock-queue:db-dat-get-path dbdat)))
    (system (conc "rm -f " fname "*"))))

(define (lock-queue:open-db fname #!key (count 10))
  (let* ((actualfname (conc fname ".lockdb"))
	 (dbexists (file-exists? actualfname))
	 (db       (dbi:open 'sqlite3 (cons (cons ('dbname actualfname) '())))))
	 ;;(handler  (make-busy-timeout 136000)))
    (if dbexists
	(vector db actualfname)
	(begin
	  (handle-exceptions
	   exn
	   (begin
	     (thread-sleep! 10)
	     (if (> count 0)
		 (lock-queue:open-db fname count: (- count 1))
		 (vector db actualfname)))
	   (dbi:with-transaction
	    db
	    (lambda ()
	      (dbi:exec 
	       db
	       "CREATE TABLE IF NOT EXISTS queue (
     	         id         INTEGER PRIMARY KEY,
                 test_id    INTEGER,
                 start_time INTEGER,
                 state      TEXT,
                 CONSTRAINT queue_constraint UNIQUE (test_id));")
	      (dbi:exec
	       db
	       "CREATE TABLE IF NOT EXISTS runlocks (
                 id         INTEGER PRIMARY KEY,
                 test_id    INTEGER,
                 run_lock   TEXT,
                 CONSTRAINT runlock_constraint UNIQUE (run_lock));"))))))
    ;;(sqlite3:set-busy-handler! db handler)
    (vector db actualfname)))

(define (lock-queue:set-state dbdat test-id newstate #!key (remtries 10))
  (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200)
  (handle-exceptions
   exn
   (if (> remtries 0)
       (begin
	 (debug:print 0 *default-log-port* "WARNING: exception on lock-queue:set-state. Trying again in 30 seconds.")
	 (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	 (thread-sleep! 30)
	 (lock-queue:set-state dbdat test-id newstate remtries: (- remtries 1)))
       (begin
	 (debug:print-error 0 *default-log-port* " Failed to set lock state for test with id " test-id ", error: " ((condition-property-accessor 'exn 'message) exn) ", giving up.")
	 #f))
   (dbi:exec (lock-queue:db-dat-get-db dbdat) "UPDATE queue SET state=? WHERE test_id=?;"
		    newstate
		    test-id)))

(define (lock-queue:any-younger? dbdat mystart test-id #!key (remtries 10))
  ;; no need to wait on journal on read only queries
  ;; (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200)
  (handle-exceptions
   exn
   (if (> remtries 0)
       (begin
	 (debug:print 0 *default-log-port* "WARNING: exception on lock-queue:any-younger. Removing lockdb and trying again in 5 seconds.")
	 (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	 (thread-sleep! 5)
         (lock-queue:delete-lock-db dbdat)
	 (lock-queue:any-younger? dbdat mystart test-id remtries: (- remtries 1)))
       (begin
	 (debug:print-error 0 *default-log-port* " Failed to find younger locks for test with id " test-id ", error: " ((condition-property-accessor 'exn 'message) exn) ", giving up.")
	 #f))
   (let ((res #f))
     (dbi:for-each-row
     	(lambda (output)
      (lambda (tid)
	;; Actually this should not be needed as mystart cannot be simultaneously less than and test-id same as 
	(if (not (equal? tid test-id)) 
	    (set! res tid))))
      (lock-queue:db-dat-get-db dbdat)
      "SELECT test_id FROM queue WHERE start_time > ?;" mystart)
     res)))

(define (lock-queue:get-lock dbdat test-id #!key (count 10)(waiting-msg #f))
  (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200 remove: #t waiting-msg: "lock-queue:get-lock, waiting on journal")
  (let* ((res       #f)
	 (db        (lock-queue:db-dat-get-db dbdat))
	 (lckqry    (dbi:prepare db "SELECT test_id,run_lock FROM runlocks WHERE run_lock='locked';"))
	 (mklckqry  (dbi:prepare db "INSERT INTO runlocks (test_id,run_lock) VALUES (?,'locked');")))
    (let ((result 
	   (handle-exceptions
	    exn
	    (begin
	      (debug:print 0 *default-log-port* "WARNING: failed to get queue lock. Removing lock db and returning fail") ;; Will try again in a few seconds")
	      (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	      (thread-sleep! 10)
	      ;; (if (> count 0)	
	      ;;  #f ;; (lock-queue:get-lock dbdat test-id count: (- count 1)) - give up on retries 
	      ;; (begin ;; never recovered, remote the lock file and return #f, no lock obtained
	      (lock-queue:delete-lock-db dbdat)
	      #f)
	    (dbi:with-transaction
	     db
	     (lambda ()
	       (dbi:for-each-row (lambda (output) (lambda (tid lockstate)
				       (set! res (list tid lockstate))))
				     lckqry)
	       (if res
		   (if (equal? (car res) test-id)
		       #t ;; already have the lock
		       #f)
		   (begin
		     (dbi:exec mklckqry test-id)
		     ;; if no error handled then return #t for got the lock
		     #t)))))))
      (dbi:close lckqry)
      (dbi:close mklckqry)
      result)))

(define (lock-queue:release-lock fname test-id #!key (count 10))
  (let* ((dbdat (lock-queue:open-db fname)))
    (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200 "lock-queue:release-lock; waiting on journal")
    (handle-exceptions
     exn
     (begin
       (debug:print 0 *default-log-port* "WARNING: Failed to release queue lock. Will try again in few seconds")
       (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
       (thread-sleep! (/ count 10))
       (if (> count 0)
	   (begin
	     (dbi:close (lock-queue:db-dat-get-db dbdat))
	     (lock-queue:release-lock fname test-id count: (- count 1)))
	   (let ((journal (conc fname "-journal")))
	     ;; If we've tried ten times and failed there is a serious problem
	     ;; try to remove the lock db and allow it to be recreated
	     (handle-exceptions
	      exn
	      #f
	      (if (file-exists? journal)(delete-file journal))
	      (if (file-exists? fname)  (delete-file fname))
	      #f))))
     (dbi:exec (lock-queue:db-dat-get-db dbdat) "DELETE FROM runlocks WHERE test_id=?;" test-id)
     (dbi:close (lock-queue:db-dat-get-db dbdat)))))

(define (lock-queue:steal-lock dbdat test-id #!key (count 10))
  (debug:print-info 0 *default-log-port* "Attempting to steal lock at " (lock-queue:db-dat-get-path dbdat))
  (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200 "lock-queue:steal-lock; waiting on journal")
  (handle-exceptions
   exn
   (begin
     (debug:print 0 *default-log-port* "WARNING: Failed to steal queue lock. Will try again in few seconds")
     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
     (thread-sleep! 10)
     (if (> count 0)
	 (lock-queue:steal-lock dbdat test-id count: (- count 1))
	 #f))
   (dbi:exec (lock-queue:db-dat-get-db dbdat) "DELETE FROM runlocks WHERE run_lock='locked';"))
  (lock-queue:get-lock dbdat test-it))

;; returns #f if ok to skip the task
;; returns #t if ok to proceed with task
;; otherwise waits
;;
(define (lock-queue:wait-turn fname test-id #!key (count 10)(waiting-msg #f))
  (let* ((dbdat   (lock-queue:open-db fname))
	 (mystart (current-seconds))
	 (db      (lock-queue:db-dat-get-db dbdat)))
    ;; (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200 waiting-msg: "lock-queue:wait-turn; waiting on journal file")
    (handle-exceptions
     exn
     (begin
       (debug:print 0 *default-log-port* "WARNING: Failed to find out if it is ok to skip the wait queue. Will try again in few seconds")
       (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
       (print-call-chain (current-error-port))
       (thread-sleep! 10)
       (if (> count 0)
	   (begin
	     (dbi:close db)
	     (lock-queue:wait-turn fname test-id count: (- count 1)))
	   (begin
	     (debug:print 0 *default-log-port* "Giving up calls to lock-queue:wait-turn for test-id " test-id " at path " fname ", printing call chain")
	     (print-call-chain (current-error-port))
	     #f)))
     ;; wait 10 seconds and then check to see if someone is already updating the html
     (thread-sleep! 10)
     (if (not (lock-queue:any-younger? dbdat mystart test-id)) ;; no processing in flight, must try to start processing
	 (begin
	   (tasks:wait-on-journal (lock-queue:db-dat-get-path dbdat) 1200 waiting-msg: "lock-queue:wait-turn; waiting on journal file")
	   (dbi:exec
	    db
	    "INSERT OR REPLACE INTO queue (test_id,start_time,state) VALUES (?,?,'waiting');"
	    test-id mystart)
	   ;; (thread-sleep! 1) ;; give other tests a chance to register
	   (let ((result 
		  (let loop ((younger-waiting (lock-queue:any-younger? dbdat mystart test-id)))
		    (if younger-waiting
			(begin
			  ;; no need for us to wait. mark in the lock queue db as skipping
			  ;; no point in marking anything in the queue - simply never register this
			  ;; test as it is *covered* by a previously started update to the html file
			  ;; (lock-queue:set-state dbdat test-id "skipping")
			  #f) ;; let the calling process know that nothing needs to be done
			(if (lock-queue:get-lock dbdat test-id)
			    #t
			    (if (> (- (current-seconds) mystart) 36000) ;; waited too long, steal the lock
				(lock-queue:steal-lock dbdat test-id)
				(begin
				  (thread-sleep! 1)
				  (loop (lock-queue:any-younger? dbdat mystart test-id)))))))))
	     (dbi:close db)
	     result))))))
	  
            
;; (use trace)
;; (trace lock-queue:get-lock lock-queue:release-lock lock-queue:wait-turn lock-queue:any-younger? lock-queue:set-state)
