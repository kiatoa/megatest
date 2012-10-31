;; Copyright 2006-2012, Matthew Welland.
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
	 (mdb     (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler (make-busy-timeout 36000)))
    (sqlite3:set-busy-handler! mdb handler)
    (if (not exists)
	(begin
	  (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS tasks_queue (id INTEGER PRIMARY KEY,
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
	  (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS monitors (id INTEGER PRIMARY KEY,
                                pid INTEGER,
                                start_time TIMESTAMP,
                                last_update TIMESTAMP,
                                hostname TEXT,
                                username TEXT,
                               CONSTRAINT monitors_constraint UNIQUE (pid,hostname));")
	  (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS servers (id INTEGER PRIMARY KEY,
                                  pid INTEGER,
                                  hostname TEXT,
                                  port INTEGER,
                                  start_time TIMESTAMP,
                                  priority INTEGER,
                                  state TEXT,
                               CONSTRAINT servers_constraint UNIQUE (pid,hostname));")
	  (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS clients (id INTEGER PRIMARY KEY,
                                  server_id INTEGER,
                                  pid INTEGER,
                                  hostname TEXT,
                                  cmdline TEXT,
                                  login_time TIMESTAMP,
                                  logout_time TIMESTAMP DEFAULT -1,
                                CONSTRAINT clients_constraint UNIQUE (pid,hostname));")
                                  
	  ))
    mdb))
    
;;======================================================================
;; Server and client management
;;======================================================================

;; state: 'live, 'shutting-down, 'dead
(define (tasks:server-register mdb pid hostname port priority state)
  (sqlite3:execute 
   mdb 
   "INSERT OR REPLACE INTO servers (pid,hostname,port,start_time,priority,state) VALUES(?,?,?,strftime('%s','now'),?,?);"
   pid hostname port priority (conc state)))

(define (tasks:server-deregister mdb hostname #!key (port #f)(pid #f))
  (if pid
      (sqlite3:execute mdb "DELETE FROM servers WHERE  hostname=? AND pid=?;" hostname pid)
      (if port
	  (sqlite3:execute mdb "DELETE FROM servers WHERE  hostname=? AND port=?;" hostname port)
	  (debug:print 0 "ERROR: tasks:server-deregister called with neither pid nor port specified"))))

(define (tasks:server-deregister-self mdb)
  (tasks:server-deregister mdb (get-host-name) pid: (current-process-id)))

(define (tasks:server-get-server-id mdb)
  ;; dunno yet
  0)

(define (tasks:client-register mdb pid hostname cmdline)
  (sqlite3:execute
   mdb
   "INSERT OR REPLACE INTO clients (server_id,pid,hostname,cmdline,login_time) VALUES(?,?,?,?,strftime('%s','now'));")
  (tasks:server-get-server-id mdb)
  pid hostname cmdline)

(define (tasks:client-logout mdb pid hostname cmdline)
  (sqlite3:execute
   mdb
   "UPDATE clients SET logout_time=strftime('%s','now') WHERE pid=? AND hostname=? AND cmdline=?;"
   pid hostname cmdline))

(define (tasks:get-logged-in-clients mdb server-id)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id server-id pid hostname cmdline login-time logout-time)
       (set! res (cons (vector id server-id pid hostname cmdline login-time lougout-time) res)))
     mdb
     "SELECT id,server_id,pid,hostname,cmdline,login_time,logout_time FROM clients WHERE server_id=?;"
     server-id)))

(define (tasks:have-clients? mdb server-id)
  (null? (tasks:get-logged-in-clients mdb server-id)))

(define (tasks:get-best-server mdb)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id hostname port)
       (set! res (list hostname port)))
     mdb
     "SELECT id,hostname,port FROM servers WHERE state='live' ORDER BY start_time DESC LIMIT 1;")
    res))

(define (tasks:get-all-servers mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id pid hostname port start-time priority state)
       (set! res (cons (vector id pid hostname port start-time priority state) res)))
     mdb
     "SELECT id,pid,hostname,port,start_time,priority,state FROM servers ORDER BY start_time ASC;")
    res))
       

;;======================================================================
;; Tasks and Task monitors
;;======================================================================


;;======================================================================
;; Tasks
;;======================================================================



;;======================================================================
;; Task Monitors
;;======================================================================

(define (tasks:register-monitor db mdb)
  (let* ((pid (current-process-id))
	 (hostname (get-host-name))
	 (userinfo (user-information (current-user-id)))
	 (username (car userinfo)))
    (print "Register monitor, pid: " pid ", hostname: " hostname ", username: " username)
    (sqlite3:execute mdb "INSERT INTO monitors (pid,start_time,last_update,hostname,username) VALUES (?,strftime('%s','now'),strftime('%s','now'),?,?);"
		     pid hostname username)))

(define (tasks:get-num-alive-monitors mdb)
  (let ((res 0))
    (sqlite3:for-each-row 
     (lambda (count)
       (set! res count))
     mdb
     "SELECT count(id) FROM monitors WHERE last_update < (strftime('%s','now') - 300) AND username=?;"
     (car (user-information (current-user-id))))
    res))

;; register a task
(define (tasks:add mdb action owner target runname test item params)
  (sqlite3:execute mdb "INSERT INTO tasks_queue (action,owner,state,target,name,test,item,params,creation_time,execution_time)
                       VALUES (?,?,'new',?,?,?,?,?,strftime('%s','now'),0);" 
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
(define (tasks:add-from-params mdb action keys key-params var-params)
  (let ((target    (keys:key-vals-hash->target keys key-params))
	(owner     (car (user-information (current-user-id))))
	(runname   (hash-table-ref/default var-params "runname" #f))
	(testpatts (hash-table-ref/default var-params "testpatts" "%"))
	(itempatts (hash-table-ref/default var-params "itempatts" "%"))
	(params    (hash-table-ref/default var-params "params"    "")))
    (tasks:add mdb action owner target runname testpatts itempatts params)))

;; return one task from those who are 'new' OR 'waiting' AND more than 10sec old
;;
(define (tasks:snag-a-task mdb)
  (let ((res    #f)
	(keytxt (conc (current-process-id) "-" (get-host-name) "-" (car (user-information (current-user-id))))))

    ;; first randomly set a new to pid-hostname-hostname
    (sqlite3:execute
     mdb 
     "UPDATE tasks_queue SET keylock=? WHERE id IN
        (SELECT id FROM tasks_queue 
           WHERE state='new' OR 
                 (state='waiting' AND (strftime('%s','now')-execution_time) > 10) OR
                 state='reset'
           ORDER BY RANDOM() LIMIT 1);" keytxt)

    (sqlite3:for-each-row
     (lambda (id . rem)
       (set! res (apply vector id rem)))
     mdb
     "SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time FROM tasks_queue WHERE keylock=? ORDER BY execution_time ASC LIMIT 1;" keytxt)
    (if res ;; yep, have work to be done
	(begin
	  (sqlite3:execute mdb "UPDATE tasks_queue SET state='inprogress',execution_time=strftime('%s','now') WHERE id=?;"
			   (tasks:task-get-id res))
	  res)
	#f)))

(define (tasks:reset-stuck-tasks mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id delta)
       (set! res (cons id res)))
     mdb
     "SELECT id,strftime('%s','now')-execution_time AS delta FROM tasks_queue WHERE state='inprogress' AND delta>700 ORDER BY delta DESC LIMIT 2;")
    (sqlite3:execute 
     mdb 
     (conc "UPDATE tasks_queue SET state='reset' WHERE id IN ('" (string-intersperse (map conc res) "','") "');"))))

;; return all tasks in the tasks_queue table
;;
(define (tasks:get-tasks mdb types states)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id . rem)
       (set! res (cons (apply vector id rem) res)))
     mdb
     (conc "SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time 
               FROM tasks_queue "
               ;; WHERE  
               ;;   state IN " statesstr " AND 
	       ;;   action IN " actionsstr 
	   " ORDER BY creation_time DESC;"))
    res))

;; remove tasks given by a string of numbers comma separated
(define (tasks:remove-queue-entries mdb task-ids)
  (sqlite3:execute mdb (conc "DELETE FROM tasks_queue WHERE id IN (" task-ids ");")))

;; 
(define (tasks:start-monitor db mdb)
  (if (> (tasks:get-num-alive-monitors mdb) 2) ;; have two running, no need for more
      (debug:print-info 1 "Not starting monitor, already have more than two running")
      (let* ((megatestdb     (conc *toppath* "/megatest.db"))
	     (monitordbf     (conc *toppath* "/monitor.db"))
	     (last-db-update 0)) ;; (file-modification-time megatestdb)))
	(task:register-monitor mdb)
	(let loop ((count      0)
		   (next-touch 0)) ;; next-touch is the time where we need to update last_update
	  ;; if the db has been modified we'd best look at the task queue
	  (let ((modtime (file-modification-time megatestdbpath )))
	    (if (> modtime last-db-update)
		(tasks:process-queue db mdb last-db-update megatestdb next-touch))
	    ;; WARNING: Possible race conditon here!!
	    ;; should this update be immediately after the task-get-action call above?
	    (if (> (current-seconds) next-touch)
		(begin
		  (tasks:monitors-update mdb)
		  (loop (+ count 1)(+ (current-seconds) 240)))
		(loop (+ count 1) next-touch)))))))
      
(define (tasks:process-queue db mdb)
  (let* ((task   (tasks:snag-a-task mdb))
	 (action (if task (tasks:task-get-action task) #f)))
    (if action (print "tasks:process-queue task: " task))
    (if action
	(case (string->symbol action)
	  ((run)       (tasks:start-run   db mdb task))
	  ((remove)    (tasks:remove-runs db mdb task))
	  ((lock)      (tasks:lock-runs   db mdb task))
	  ;; ((monitor)   (tasks:start-monitor db task))
	  ((rollup)    (tasks:rollup-runs db mdb task))
	  ((updatemeta)(tasks:update-meta db mdb task))
	  ((kill)      (tasks:kill-monitors db mdb task))))))

(define (tasks:get-monitors mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (a . rem)
       (set! res (cons (apply vector a rem) res)))
     mdb
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
(define (tasks:monitors-update mdb)
  (sqlite3:execute mdb "UPDATE monitors SET last_update=strftime('%s','now') WHERE pid=? AND hostname=?;"
			  (current-process-id)
			  (get-host-name))
  (let ((deadlist '()))
    (sqlite3:for-each-row
     (lambda (id pid host last-update delta)
       (print "Going to delete stale record for monitor with pid " pid " on host " host " last updated " delta " seconds ago")
       (set! deadlist (cons id deadlist)))
     mdb 
     "SELECT id,pid,hostname,last_update,strftime('%s','now')-last_update AS delta FROM monitors WHERE delta > 700;")
    (sqlite3:execute mdb (conc "DELETE FROM monitors WHERE id IN ('" (string-intersperse (map conc deadlist) "','") "');")))
  )

(define (tasks:remove-monitor-record mdb)
  (sqlite3:execute mdb "DELETE FROM monitors WHERE pid=? AND hostname=?;"
		   (current-process-id)
		   (get-host-name)))

(define (tasks:set-state mdb task-id state)
  (sqlite3:execute mdb "UPDATE tasks_queue SET state=? WHERE id=?;" 
		   state 
		   task-id))

;;======================================================================
;; The routines to process tasks
;;======================================================================

;; NOTE: It might be good to add one more layer of checking to ensure
;;       that no task gets run in parallel.

(define (tasks:start-run db mdb task)
  (let ((flags (make-hash-table)))
    (hash-table-set! flags "-rerun" "NOT_STARTED")
    (if (not (string=? (tasks:task-get-params task) ""))
	(hash-table-set! flags "-setvars" (tasks:task-get-params task)))
    (print "Starting run " task)
    ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
    (runs:run-tests db
		    (tasks:task-get-target task)
		    (tasks:task-get-name   task)
		    (tasks:task-get-test   task)
		    (tasks:task-get-item   task)
		    (tasks:task-get-owner  task)
		    flags)
    (tasks:set-state mdb (tasks:task-get-id task) "waiting")))

(define (tasks:rollup-runs db mdb task)
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
    (tasks:set-state mdb (tasks:task-get-id task) "waiting")))
