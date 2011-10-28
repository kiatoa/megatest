;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking format)
(import (prefix sqlite3 sqlite3:))

(declare (unit tasks))
(declare (uses db))
(declare (uses common))

(include "task_records.scm")

;;======================================================================
;; Tasks db
;;======================================================================

(define (tasks:open-db)
  (let* ((dbpath  (conc *toppath* "/monitor.db"))
	 (exists  (file-exists? dbpath))
	 (tdb     (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler (make-busy-timeout 36000)))
    (sqlite3:set-busy-handler! tdb handler)
    (if (not exists)
	(begin
	  (sqlite3:execute tdb "CREATE TABLE IF NOT EXISTS tasks_queue (id INTEGER PRIMARY KEY,
                                action TEXT DEFAULT '',
                                owner TEXT,
                                state TEXT DEFAULT 'new',
                                target TEXT DEFAULT '',
                                name TEXT DEFAULT '',
                                test TEXT DEFAULT '',
                                item TEXT DEFAULT '',
                                keylock TEXT,
                                params TEXT,
                                creation_time TIMESTAMP,
                                execution_time TIMESTAMP);")
	  (sqlite3:execute tdb "CREATE TABLE IF NOT EXISTS monitors (id INTEGER PRIMARY KEY,
                                pid INTEGER,
                                start_time TIMESTAMP,
                                last_update TIMESTAMP,
                                hostname TEXT,
                                username TEXT,
                               CONSTRAINT monitors_constraint UNIQUE (pid,hostname));")))
    tdb))
    

;;======================================================================
;; Tasks and Task monitors
;;======================================================================


;;======================================================================
;; Tasks
;;======================================================================



;;======================================================================
;; Task Monitors
;;======================================================================

(define (tasks:register-monitor db tdb)
  (let* ((pid (current-process-id))
	 (hostname (get-host-name))
	 (userinfo (user-information (current-user-id)))
	 (username (car userinfo)))
    (print "Register monitor, pid: " pid ", hostname: " hostname ", username: " username)
    (sqlite3:execute tdb "INSERT INTO monitors (pid,start_time,last_update,hostname,username) VALUES (?,strftime('%s','now'),strftime('%s','now'),?,?);"
		     pid hostname username)))

(define (tasks:get-num-alive-monitors tdb)
  (let ((res 0))
    (sqlite3:for-each-row 
     (lambda (count)
       (set! res count))
     tdb
     "SELECT count(id) FROM monitors WHERE last_update < (strftime('%s','now') - 300) AND username=?;"
     (car (user-information (current-user-id))))
    res))

;; register a task
(define (tasks:add tdb action owner target runname test item params)
  (sqlite3:execute tdb "INSERT INTO tasks_queue (action,owner,state,target,name,test,item,params,creation_time,execution_time)
                       VALUES (?,?,'new',?,?,?,?,strftime('%s','now'),0);" 
		   action
		   owner
		   target
		   runname
		   test
		   item
		   (if params params "")))

(define (keys:key-vals-hash->target keys key-params)
  (let ((tmp (hash-table-ref/default key-params (vector-ref (car keys) 0) "")))
    (if (> (length keys) 1)
	(for-each (lambda (key)
		    (set! tmp (conc tmp "/" (hash-table-ref/default key-params (vector-ref key 0) ""))))
		  (cdr keys)))
    tmp))
								
;; for use from the gui
(define (tasks:add-from-params tdb action keys key-params var-params)
  (let ((target    (keys:key-vals-hash->target keys key-params))
	(owner     (car (user-information (current-user-id))))
	(runname   (hash-table-ref/default var-params "runname" #f))
	(testpatts (hash-table-ref/default var-params "testpatts" "%"))
	(itempatts (hash-table-ref/default var-params "itempatts" "%")))
    (tasks:add tdb action owner target runname testpatts itempatts)))

;; return one task from those who are 'new' OR 'waiting' AND more than 10sec old
;;
(define (tasks:snag-a-task tdb)
  (let ((res    #f)
	(keytxt (conc (current-process-id) "-" (get-host-name) "-" (car (user-information (current-user-id))))))

    ;; first randomly set a new to pid-hostname-hostname
    (sqlite3:execute
     tdb 
     "UPDATE tasks_queue SET keylock=? WHERE id IN
        (SELECT id FROM tasks_queue 
           WHERE state='new' OR 
                 (state='waiting' AND (strftime('%s','now')-execution_time) > 10) OR
                 state='reset'
           ORDER BY RANDOM() LIMIT 1);" keytxt)

    (sqlite3:for-each-row
     (lambda (id . rem)
       (set! res (apply vector id rem)))
     tdb
     "SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time FROM tasks_queue WHERE keylock=? ORDER BY execution_time ASC LIMIT 1;" keytxt)
    (if res ;; yep, have work to be done
	(begin
	  (sqlite3:execute tdb "UPDATE tasks_queue SET state='inprogress',execution_time=strftime('%s','now') WHERE id=?;"
			   (tasks:task-get-id res))
	  res)
	#f)))

(define (tasks:reset-stuck-tasks tdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id delta)
       (set! res (cons id res)))
     tdb
     "SELECT id,strftime('%s','now')-execution_time AS delta FROM tasks_queue WHERE state='inprogress' AND delta>700 ORDER BY delta DESC LIMIT 2;")
    (sqlite3:execute 
     tdb 
     (conc "UPDATE tasks_queue SET state='reset' WHERE id IN ('" (string-intersperse (map conc res) "','") "');"))))

;; return all tasks in the tasks_queue table
;;
(define (tasks:get-tasks tdb types states)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id . rem)
       (set! res (cons (apply vector id rem) res)))
     tdb
     (conc "SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time 
               FROM tasks_queue "
               ;; WHERE  
               ;;   state IN " statesstr " AND 
	       ;;   action IN " actionsstr 
	   " ORDER BY creation_time DESC;"))
    res))

;; remove tasks given by a string of numbers comma separated
(define (tasks:remove-queue-entries tdb task-ids)
  (sqlite3:execute tdb (conc "DELETE FROM tasks_queue WHERE id IN (" task-ids ");")))

;; 
(define (tasks:start-monitor db tdb)
  (if (> (tasks:get-num-alive-monitors tdb) 2) ;; have two running, no need for more
      (debug:print 1 "INFO: Not starting monitor, already have more than two running")
      (let* ((megatestdb     (conc *toppath* "/megatest.db"))
	     (monitordbf     (conc *toppath* "/monitor.db"))
	     (last-db-update 0)) ;; (file-modification-time megatestdb)))
	(task:register-monitor tdb)
	(let loop ((count      0)
		   (next-touch 0)) ;; next-touch is the time where we need to update last_update
	  ;; if the db has been modified we'd best look at the task queue
	  (let ((modtime (file-modification-time megatestdbpath )))
	    (if (> modtime last-db-update)
		(tasks:process-queue db tdb last-db-update megatestdb next-touch))
	    ;; WARNING: Possible race conditon here!!
	    ;; should this update be immediately after the task-get-action call above?
	    (if (> (current-seconds) next-touch)
		(begin
		  (tasks:monitors-update tdb)
		  (loop (+ count 1)(+ (current-seconds) 240)))
		(loop (+ count 1) next-touch)))))))
      
(define (tasks:process-queue db tdb)
  (let* ((task   (tasks:snag-a-task tdb))
	 (action (if task (tasks:task-get-action task) #f)))
    (if action (print "tasks:process-queue task: " task))
    (if action
	(case (string->symbol action)
	  ((run)       (tasks:start-run   db tdb task))
	  ((remove)    (tasks:remove-runs db tdb task))
	  ((lock)      (tasks:lock-runs   db tdb task))
	  ;; ((monitor)   (tasks:start-monitor db task))
	  ((rollup)    (tasks:rollup-runs db tdb task))
	  ((updatemeta)(tasks:update-meta db tdb task))
	  ((kill)      (tasks:kill-monitors db tdb task))))))

(define (tasks:get-monitors tdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (a . rem)
       (set! res (cons (apply vector a rem) res)))
     tdb
     "SELECT id,pid,strftime('%m/%d/%Y %H:%M',datetime(start_time,'unixepoch'),'localtime'),strftime('%m/%d/%Y %H:%M:%S',datetime(last_update,'unixepoch'),'localtime'),hostname,username FROM monitors ORDER BY last_update ASC;")
    (reverse res)
    ))

(define (tasks:tasks->text tasks)
  (let ((fmtstr "~10a~10a~10a~12a~20a~12a~12a~12a~10a"))
    (conc (format #f fmtstr "id" "action" "owner" "state" "target" "runname" "testpatts" "itempatts" "params") "\n"
	  (string-intersperse 
	   (map (lambda (task)
		  (format #f fmtstr
			  (tasks:task-get-id     task)
			  (tasks:task-get-action task)
			  (tasks:task-get-owner  task)
			  (tasks:task-get-state  task)
			  (tasks:task-get-target task)
			  (tasks:task-get-name   task)
			  (tasks:task-get-test   task)
			  (tasks:task-get-item   task)
			  (tasks:task-get-params task)))
		tasks) "\n"))))
   
(define (tasks:monitors->text-table monitors)
  (let ((fmtstr "~4a~8a~20a~20a~10a~10a"))
    (conc (format #f fmtstr "id" "pid" "start time" "last update" "hostname" "user") "\n"
	  (string-intersperse 
	   (map (lambda (monitor)
		  (format #f fmtstr
			  (tasks:monitor-get-id          monitor)
			  (tasks:monitor-get-pid         monitor)
			  (tasks:monitor-get-start_time  monitor)
			  (tasks:monitor-get-last_update monitor)
			  (tasks:monitor-get-hostname    monitor)
			  (tasks:monitor-get-username    monitor)))
		monitors)
	   "\n"))))
   
;; update the last_update field with the current time and
;; if any monitors appear dead, remove them
(define (tasks:monitors-update tdb)
  (sqlite3:execute tdb "UPDATE monitors SET last_update=strftime('%s','now') WHERE pid=? AND hostname=?;"
			  (current-process-id)
			  (get-host-name))
  (let ((deadlist '()))
    (sqlite3:for-each-row
     (lambda (id pid host last-update delta)
       (print "Going to delete stale record for monitor with pid " pid " on host " host " last updated " delta " seconds ago")
       (set! deadlist (cons id deadlist)))
     tdb 
     "SELECT id,pid,hostname,last_update,strftime('%s','now')-last_update AS delta FROM monitors WHERE delta > 700;")
    (sqlite3:execute tdb (conc "DELETE FROM monitors WHERE id IN ('" (string-intersperse (map conc deadlist) "','") "');")))
  )

(define (tasks:remove-monitor-record tdb)
  (sqlite3:execute tdb "DELETE FROM monitors WHERE pid=? AND hostname=?;"
		   (current-process-id)
		   (get-host-name)))

(define (tasks:set-state tdb task-id state)
  (sqlite3:execute tdb "UPDATE tasks_queue SET state=? WHERE id=?;" 
		   state 
		   task-id))

;;======================================================================
;; The routines to process tasks
;;======================================================================

;; NOTE: It might be good to add one more layer of checking to ensure
;;       that no task gets run in parallel.

(define (tasks:start-run db tdb task)
  (let ((flags (make-hash-table)))
    (hash-table-set! flags "-rerun" "NOT_STARTED")
    (if (not (string=? (tasks:task-get-params task) ""))
	(hash-table-set! flags "-
    (print "Starting run " task)
    ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
    (runs:run-tests db
		    (tasks:task-get-target task)
		    (tasks:task-get-name   task)
		    (tasks:task-get-test   task)
		    (tasks:task-get-item   task)
		    (tasks:task-get-owner  task)
		    flags)
    (tasks:set-state tdb (tasks:task-get-id task) "waiting")))

(define (tasks:rollup-runs db tdb task)
  (let* ((flags (make-hash-table))
	 (keys  (db:get-keys db))
	 (keyvallst (keys:target->keyval keys (tasks:task-get-target task))))
    ;; (hash-table-set! flags "-rerun" "NOT_STARTED")
    (print "Starting rollup " task)
    ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
    (runs:rollup-run db
		     keys 
		     keyvallst
		     (tasks:task-get-name  task)
		     (tasks:task-get-owner  task))
    (tasks:set-state tdb (tasks:task-get-id task) "waiting")))
