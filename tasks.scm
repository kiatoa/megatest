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
  (let* ((dbpath       (conc *toppath* "/db/monitor.db"))
	 (exists       (file-exists? dbpath))
	 (write-access (file-write-access? dbpath))
	 (mdb          (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler      (make-busy-timeout 36000)))
    (if (and exists
	     (not write-access))
	(set! *db-write-access* write-access)) ;; only unset so other db's also can use this control
    (sqlite3:set-busy-handler! mdb handler)
    (sqlite3:execute mdb (conc "PRAGMA synchronous = 0;"))
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
                                  interface TEXT,
                                  hostname TEXT,
                                  port INTEGER,
                                  pubport INTEGER,
                                  start_time TIMESTAMP,
                                  priority INTEGER,
                                  state TEXT,
                                  mt_version TEXT,
                                  heartbeat TIMESTAMP,
                                  transport TEXT,
                                  run_id INTEGER,
                               CONSTRAINT servers_constraint UNIQUE (pid,hostname,port));")
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

;; make-vector-record tasks hostinfo id interface port pubport transport pid hostname
(define (tasks:hostinfo-get-id          vec)    (vector-ref  vec 0))
(define (tasks:hostinfo-get-interface   vec)    (vector-ref  vec 1))
(define (tasks:hostinfo-get-port        vec)    (vector-ref  vec 2))
(define (tasks:hostinfo-get-pubport     vec)    (vector-ref  vec 3))
(define (tasks:hostinfo-get-transport   vec)    (vector-ref  vec 4))
(define (tasks:hostinfo-get-pid         vec)    (vector-ref  vec 5))
(define (tasks:hostinfo-get-hostname    vec)    (vector-ref  vec 6))

;; state: 'live, 'shutting-down, 'dead
(define (tasks:server-register mdb pid interface port priority state transport #!key (pubport -1))
  (debug:print-info 11 "tasks:server-register " pid " " interface " " port " " priority " " state)
  (sqlite3:execute 
   mdb 
   "INSERT OR REPLACE INTO servers (pid,hostname,port,pubport,start_time,priority,state,mt_version,heartbeat,interface,transport)
                             VALUES(?,  ?,       ?,   ?,  strftime('%s','now'), ?, ?, ?, strftime('%s','now'),?,?);"
   pid (get-host-name) port pubport priority (conc state) 
   (common:version-signature)
   interface 
   (conc transport))
  (vector 
   (tasks:server-get-server-id mdb (get-host-name) interface port pid)
   interface
   port
   pubport
   transport
   ))

;; NB// two servers with same pid on different hosts will be removed from the list if pid: is used!
(define (tasks:server-deregister mdb hostname #!key (port #f)(pid #f)(action 'delete))
  (debug:print-info 11 "server-deregister " hostname ", port " port ", pid " pid)
  (if *db-write-access*
      (if pid
	  (case action
	    ((delete)(sqlite3:execute mdb "DELETE FROM servers WHERE pid=?;" pid))
	    (else    (sqlite3:execute mdb "UPDATE servers SET state='dead' WHERE pid=?;" pid)))
	  (if port
	      (case action
	    ((delete)(sqlite3:execute mdb "DELETE FROM servers WHERE (interface=? or hostname=?) AND port=?;" hostname hostname port))
	    (else    (sqlite3:execute mdb "UPDATE servers SET state='dead' WHERE (interface=? or hostname=?) AND port=?;" hostname hostname port)))
	      (debug:print 0 "ERROR: tasks:server-deregister called with neither pid nor port specified")))))

(define (tasks:server-deregister-self mdb hostname)
  (tasks:server-deregister mdb hostname pid: (current-process-id)))

;; need a simple call for robustly removing records given host and port
(define (tasks:server-delete mdb hostname port)
  (tasks:server-deregister mdb hostname port: port action: 'delete))

(define (tasks:server-get-server-id mdb hostname iface port pid)
  (debug:print-info 12 "tasks:server-get-server-id " mdb " " hostname " " iface " " port " " pid)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     mdb
     (cond
      ((and hostname  pid)  "SELECT id FROM servers WHERE hostname=?  AND pid=?;")
      ((and iface     port) "SELECT id FROM servers WHERE interface=? AND port=?;")
      ((and hostname  port) "SELECT id FROM servers WHERE hostname=?  AND port=?;")
      (else
       (begin
	 (debug:print 0 "ERROR: tasks:server-get-server-id needs (hostname and pid) OR (iface and port) OR (hostname and port)")
	 "SELECT id FROM servers WHERE pid=-999;")))
     (if hostname hostname iface)(if pid pid port))
    res))

(define (tasks:server-update-heartbeat mdb server-id)
  (debug:print-info 1 "Heart beat update of server id=" server-id)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 "WARNING: probable timeout on monitor.db access")
     (thread-sleep! 1)
     (tasks:server-update-heartbeat mdb server-id))
   (sqlite3:execute mdb "UPDATE servers SET heartbeat=strftime('%s','now') WHERE id=?;" server-id)))

;; alive servers keep the heartbeat field upto date with seconds every 6 or so seconds
(define (tasks:server-alive? mdb server-id #!key (iface #f)(hostname #f)(port #f)(pid #f))
  (let* ((server-id  (if server-id 
			 server-id
			 (tasks:server-get-server-id mdb hostname iface port pid)))
	 (heartbeat-delta 99e9))
    (sqlite3:for-each-row
     (lambda (delta)
       (set! heartbeat-delta delta))
     mdb "SELECT strftime('%s','now')-heartbeat FROM servers WHERE id=?;" server-id)
    (< heartbeat-delta 10)))

(define (tasks:client-register mdb pid hostname cmdline)
  (sqlite3:execute
   mdb
   "INSERT OR REPLACE INTO clients (server_id,pid,hostname,cmdline,login_time) VALUES(?,?,?,?,strftime('%s','now'));")
  (tasks:server-get-server-id mdb hostname #f #f pid)
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

;; ping each server in the db and return first found that responds. 
;; remove any others. will not necessarily remove all!
(define (tasks:get-best-server mdb)
  (let ((res '())
	(best #f)
	(transport (if (and *transport-type*
			    (not (eq? *transport-type* 'fs)))
		       (conc *transport-type*)
		       "%")))
    (sqlite3:for-each-row
     (lambda (id interface port pubport transport pid hostname)
       (set! res (cons (vector id interface port pubport transport pid hostname) res))
       ;;(debug:print-info 2 "Found existing server " hostname ":" port " registered in db"))
       )
     mdb
     
     "SELECT id,interface,port,pubport,transport,pid,hostname FROM servers
          WHERE strftime('%s','now')-heartbeat < 10 
          AND mt_version=? AND transport LIKE ? 
          ORDER BY start_time DESC LIMIT 1;" (common:version-signature) transport)
    ;; for now we are keeping only one server registered in the db, return #f or first server found
    (if (null? res) #f (car res))))

;; BUG: This logic is probably needed unless methodology changes completely...
;;
;;     (if (null? res) #f
;; 	(let loop ((hed (car res))
;; 		   (tal (cdr res)))
;; 	  ;; (print "hed=" hed ", tal=" tal)
;; 	  (let* ((host     (list-ref hed 0))
;; 		 (iface    (list-ref hed 1))
;; 		 (port     (list-ref hed 2))
;; 		 (pid      (list-ref hed 4))
;; 		 (alive    (open-run-close tasks:server-alive? tasks:open-db #f hostname: host port: port)))
;; 	    (if alive
;; 		(begin
;; 		  (debug:print-info 2 "Found an existing, alive, server " host ", " port ".")
;; 		  (list host iface port))
;; 		(begin
;; 		  (debug:print-info 1 "Marking " host ":" port " as dead in server registry.")
;; 		  (if port
;; 		      (open-run-close tasks:server-deregister tasks:open-db host port: port)
;; 		      (open-run-close tasks:server-deregister tasks:open-db host pid:  pid))
;; 		  (if (null? tal)
;; 		      #f
;; 		      (loop (car tal)(cdr tal))))))))))

(define (tasks:remove-server-records mdb)
  (sqlite3:execute mdb "DELETE FROM servers;"))

(define (tasks:mark-server hostname port pid state transport)
  (if port
      (open-run-close tasks:server-deregister tasks:open-db hostname port: port)
      (open-run-close tasks:server-deregister tasks:open-db hostname pid:  pid)))


(define (tasks:kill-server status hostname port pid transport)
  (debug:print-info 1 "Removing defunct server record for " hostname ":" port)
  (if port
      (open-run-close tasks:server-deregister tasks:open-db hostname port: port)
      (open-run-close tasks:server-deregister tasks:open-db hostname pid:  pid))
  (if status ;; #t means alive
      (begin
	(if (equal? hostname (get-host-name))
	    (handle-exceptions
	     exn
	     (debug:print-info 0 "server may or may not be dead, check for megatest -server running as pid " pid "\n"
			       "  EXCEPTION: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 1 "Sending signal/term to " pid " on " hostname)
	     (process-signal pid signal/term)
	     (thread-sleep! 5) ;; give it five seconds to die peacefully then do a brutal kill
	     ;;(process-signal pid signal/kill)
	     ) ;; local machine, send sig term
	    (begin
	      ;;(debug:print-info 1 "Stopping remote servers not yet supported."))))
	      (debug:print-info 1 "Telling alive server on " hostname ":" port " to commit servercide")
	      (let ((serverdat (list hostname port)))
	      	(case (if (string? transport) (string->symbol transport) transport)
	      	  ((http)(http-transport:client-connect hostname port))
	      	  (else  (debug:print "ERROR: remote stopping servers of type " transport " not supported yet")))
	      	(cdb:kill-server serverdat pid)))))    ;; remote machine, try telling server to commit suicide
      (begin
	(if status 
	    (if (equal? hostname (get-host-name))
		(begin
		  (debug:print-info 1 "Sending signal/term to " pid " on " hostname)
		  (process-signal pid signal/term)  ;; local machine, send sig term
		  (thread-sleep! 5)                 ;; give it five seconds to die peacefully then do a brutal kill
		  (process-signal pid signal/kill)) 
		(debug:print 0 "WARNING: Can't kill frozen server on remote host " hostname))))))



(define (tasks:get-all-servers mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id pid hostname interface port pubport start-time priority state mt-version last-update transport)
       (set! res (cons (vector id pid hostname interface port pubport start-time priority state mt-version last-update transport) res)))
     mdb
     "SELECT id,pid,hostname,interface,port,pubport,start_time,priority,state,mt_version,strftime('%s','now')-heartbeat AS last_update,transport FROM servers ORDER BY start_time DESC;")
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
	(params    (hash-table-ref/default var-params "params"    "")))
    (tasks:add mdb action owner target runname testpatts params)))

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
	     (monitordbf     (conc *toppath* "/db/monitor.db"))
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
  (let ((fmtstr "~10a~10a~10a~12a~20a~12a~12a~10a"))
    (conc (format #f fmtstr "id" "action" "owner" "state" "target" "runname" "testpatts" "params") "\n"
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
			  ;; (tasks:task-get-item   task)
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
	 (keyvals (keys:target-keyval keys (tasks:task-get-target task))))
    ;; (hash-table-set! flags "-rerun" "NOT_STARTED")
    (print "Starting rollup " task)
    ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
    (runs:rollup-run db
		     keys 
		     keyvals
		     (tasks:task-get-name  task)
		     (tasks:task-get-owner  task))
    (tasks:set-state mdb (tasks:task-get-id task) "waiting")))
