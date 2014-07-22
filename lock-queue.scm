;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; launch a task - this runs on the originating host, tests themselves
;;
;;======================================================================

(use sqlite3 srfi-18)
(import (prefix sqlite3 sqlite3:))

(declare (unit lock-queue))
(declare (uses common))

;;======================================================================
;; attempt to prevent overlapping updates of rollup files by queueing
;; update requests in an sqlite db
;;======================================================================

(define (lock-queue:open-db fname #!key (count 10))
  (let* ((actualfname (conc fname ".lockdb"))
	 (dbexists (file-exists? actualfname))
	 (db       (sqlite3:open-database actualfname))
	 (handler  (make-busy-timeout 136000)))
    (if dbexists
	db
	(begin
	  (handle-exceptions
	   exn
	   (begin
	     (thread-sleep! 10)
	     (if (> count 0)
		 (lock-queue:open-db fname count: (- count 1))
		 db))
	   (sqlite3:execute 
	    db
	    "CREATE TABLE IF NOT EXISTS queue (
  	      id         INTEGER PRIMARY KEY,
              test_id    INTEGER,
              start_time INTEGER,
              state      TEXT,
              CONSTRAINT queue_constraint UNIQUE (test_id));")
	   (sqlite3:execute
	    db
	    "CREATE TABLE IF NOT EXISTS runlocks (
              id         INTEGER PRIMARY KEY,
              test_id    INTEGER,
              run_lock   TEXT,
              CONSTRAINT runlock_constraint UNIQUE (run_lock));"))))
    (sqlite3:set-busy-handler! db handler)
    db))

(define (lock-queue:set-state db test-id newstate #!key (count 10))
  (handle-exceptions
   exn
   (begin
     (thread-sleep! 10)
     (if (> count 0)
	 (lock-queue:set-state db test-id newstate (- count 1))
	 #f))
   (sqlite3:execute db "UPDATE queue SET state=? WHERE test_id=?;"
		    newstate
		    test-id)))

(define (lock-queue:any-younger? db mystart test-id #!key (count 10))
  (let ((res #f))
    (handle-exceptions
     exn
     (begin
       (thread-sleep! 10)
       (if (> count 0)
	   (lock-queue:any-younger? db mystart test-id count: (- count 1))
	   #f))
     (sqlite3:for-each-row
      (lambda (tid)
	;; Actually this should not be needed as mystart cannot be simultaneously less than and test-id same as 
	(if (not (equal? tid test-id)) 
	    (set! res tid)))
      db
      "SELECT test_id FROM queue WHERE start_time > ?;" mystart)
     res)))

(define (lock-queue:get-lock db test-id #!key (count 10))
  (let ((res       #f)
	(lckqry    (sqlite3:prepare db "SELECT test_id,run_lock FROM runlocks WHERE run_lock='locked';"))
	(mklckqry  (sqlite3:prepare db "INSERT INTO runlocks (test_id,run_lock) VALUES (?,'locked');")))
    (let ((result 
	   (handle-exceptions
	    exn
	    (begin
	      (thread-sleep! 10)
	      (if (> count 0)
		  (lock-queue:get-lock db test-id count: (- count 1)))
	      #f)
	    (sqlite3:with-transaction
	     db
	     (lambda ()
	       (sqlite3:for-each-row (lambda (tid lockstate)
				       (set! res (list tid lockstate)))
				     lckqry)
	       (if res
		   (if (equal? (car res) test-id)
		       #t ;; already have the lock
		       #f)
		   (begin
		     (sqlite3:execute mklckqry test-id)
		     ;; if no error handled then return #t for got the lock
		     #t)))))))
      (sqlite3:finalize! lckqry)
      (sqlite3:finalize! mklckqry)
      result)))

(define (lock-queue:release-lock fname test-id #!key (count 10))
  (let ((db (lock-queue:open-db fname)))
    (handle-exceptions
     exn
     (begin
       (thread-sleep! 10)
       (if (> count 0)
	   (lock-queue:release-lock fname test-id count: (- count 1))
	   #f))
     (sqlite3:execute db "DELETE FROM runlocks WHERE test_id=?;" test-id)
     (sqlite3:finalize! db))))

(define (lock-queue:steal-lock db test-id #!key (count 10))
  (handle-exceptions
   exn
   (begin
     (thread-sleep! 10)
     (if (> count 0)
	 (lock-queue:steal-lock db test-id count: (- count 1))
	 #f))
   (sqlite3:execute db "DELETE FROM runlocks WHERE run_lock='locked';"))
  (lock-queue:get-lock db test-it))

;; returns #f if ok to skip the task
;; returns #t if ok to proceed with task
;; otherwise waits
;;
(define (lock-queue:wait-turn fname test-id #!key (count 10))
  (let ((db      (lock-queue:open-db fname))
	(mystart (current-seconds)))
    (handle-exceptions
     exn
     (begin
       (thread-sleep! 10)
       (if (> count 0)
	   (lock-queue:wait-turn fname test-id count: (- count 1))
	   #f))
     (sqlite3:execute
      db
      "INSERT OR REPLACE INTO queue (test_id,start_time,state) VALUES (?,?,'waiting');"
      test-id mystart)
     (thread-sleep! 1) ;; give other tests a chance to register
     (let ((result 
	    (let loop ((younger-waiting (lock-queue:any-younger? db mystart test-id)))
	      (if younger-waiting
		  (begin
		    ;; no need for us to wait. mark in the lock queue db as skipping
		    (lock-queue:set-state db test-id "skipping")
		    #f) ;; let the calling process know that nothing needs to be done
		  (if (lock-queue:get-lock db test-id)
		      #t
		      (if (> (- (current-seconds) mystart) 36000) ;; waited too long, steal the lock
			  (lock-queue:steal-lock db test-id)
			  (begin
			    (thread-sleep! 1)
			    (loop (lock-queue:any-younger? db mystart test-id)))))))))
       (sqlite3:finalize! db)
       result))))
	  
            
;; (use trace)
;; (trace lock-queue:get-lock lock-queue:release-lock lock-queue:wait-turn lock-queue:any-younger? lock-queue:set-state)