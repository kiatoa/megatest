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
(declare (uses rmt))
(declare (uses common))
(declare (uses pgdb))

;; (import pgdb) ;; pgdb is a module

(include "task_records.scm")
(include "db_records.scm")

;;======================================================================
;; Tasks db
;;======================================================================

;; wait up to aprox n seconds for a journal to go away
;;
(define (tasks:wait-on-journal path n #!key (remove #f)(waiting-msg #f))
  (if (not (string? path))
      (debug:print-error 0 *default-log-port* "Called tasks:wait-on-journal with path=" path " (not a string)")
      (let ((fullpath (conc path "-journal")))
	(handle-exceptions
	 exn
	 (begin
	   (print-call-chain (current-error-port))
	   (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	   (debug:print 5 *default-log-port* " exn=" (condition->list exn))
	   (debug:print 0 *default-log-port* "tasks:wait-on-journal failed. Continuing on, you can ignore this call-chain")
	   #t) ;; if stuff goes wrong just allow it to move on
	 (let loop ((journal-exists (common:file-exists? fullpath))
		    (count          n)) ;; wait ten times ...
	   (if journal-exists
	       (begin
		 (if (and waiting-msg
			  (eq? (modulo n 30) 0))
		     (debug:print 0 *default-log-port* waiting-msg))
		 (if (> count 0)
		     (begin
		       (thread-sleep! 1)
		       (loop (common:file-exists? fullpath)
			     (- count 1)))
		     (begin
		       (debug:print 0 *default-log-port* "ERROR: removing the journal file " fullpath ", this is not good. Look for disk full, write access and other issues.")
		       (if remove (system (conc "rm -rf " fullpath)))
		       #f)))
	       #t))))))

(define (tasks:get-task-db-path)
  (let ((dbdir  (or (configf:lookup *configdat* "setup" "monitordir")
		    (configf:lookup *configdat* "setup" "dbdir")
		    (conc (common:get-linktree) "/.db"))))
    (handle-exceptions
     exn
     (begin
       (debug:print-error 0 *default-log-port* "Couldn't create path to " dbdir)
       (exit 1))
     (if (not (directory? dbdir))(create-directory dbdir #t)))
    dbdir))

;; If file exists AND
;;    file readable
;;         ==> open it
;; If file exists AND
;;    file NOT readable
;;         ==> open in-mem version
;; If file NOT exists
;;    ==> open in-mem version
;;
(define (tasks:open-db #!key (numretries 4))
  (if *task-db*
      *task-db*
      (handle-exceptions
       exn
       (if (> numretries 0)
	   (begin
	     (print-call-chain (current-error-port))
	     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 5 *default-log-port* " exn=" (condition->list exn))
	     (thread-sleep! 1)
	     (tasks:open-db numretries (- numretries 1)))
	   (begin
	     (print-call-chain (current-error-port))
	     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 5 *default-log-port* " exn=" (condition->list exn))))
       (let* ((dbpath        (db:dbfile-path )) ;; (tasks:get-task-db-path))
	      (dbfile       (conc dbpath "/monitor.db"))
	      (avail        (tasks:wait-on-journal dbpath 10)) ;; wait up to about 10 seconds for the journal to go away
	      (exists       (common:file-exists? dbpath))
	      (write-access (file-write-access? dbpath))
	      (mdb          (cond ;; what the hek is *toppath* doing here?
			     ((and (string? *toppath*)(file-write-access? *toppath*))
			      (sqlite3:open-database dbfile))
			     ((file-read-access? dbpath)    (sqlite3:open-database dbfile))
			     (else (sqlite3:open-database ":memory:")))) ;; (never-give-up-open-db dbpath))
	      (handler      (make-busy-timeout 36000)))
	 (if (and exists
		  (not write-access))
	     (set! *db-write-access* write-access)) ;; only unset so other db's also can use this control
	 (sqlite3:set-busy-handler! mdb handler)
	 (db:set-sync mdb) ;; (sqlite3:execute mdb (conc "PRAGMA synchronous = 0;"))
	 ;;  (if (or (and (not exists)
	 ;; 	      (file-write-access? *toppath*))
	 ;; 	 (not (file-read-access? dbpath)))
	 ;;      (begin
	 ;; 
	 ;; TASKS QUEUE MOVED TO main.db
	 ;;
	 ;; (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS tasks_queue (id INTEGER PRIMARY KEY,
         ;;                        action TEXT DEFAULT '',
         ;;                        owner TEXT,
         ;;                        state TEXT DEFAULT 'new',
         ;;                        target TEXT DEFAULT '',
         ;;                        name TEXT DEFAULT '',
         ;;                        testpatt TEXT DEFAULT '',
         ;;                        keylock TEXT,
         ;;                        params TEXT,
         ;;                        creation_time TIMESTAMP,
         ;;                        execution_time TIMESTAMP);")
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
                                  run_id INTEGER);")
	 ;;                               CONSTRAINT servers_constraint UNIQUE (pid,hostname,port));")
	 (sqlite3:execute mdb "CREATE TABLE IF NOT EXISTS clients (id INTEGER PRIMARY KEY,
                                  server_id INTEGER,
                                  pid INTEGER,
                                  hostname TEXT,
                                  cmdline TEXT,
                                  login_time TIMESTAMP,
                                  logout_time TIMESTAMP DEFAULT -1,
                                CONSTRAINT clients_constraint UNIQUE (pid,hostname));")
	       
	       ;))
	 (set! *task-db* (cons mdb dbpath))
	 *task-db*))))

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

(define (tasks:need-server run-id)
  (equal? (configf:lookup *configdat* "server" "required") "yes"))

;; no elegance here ...
;;
(define (tasks:kill-server hostname pid #!key (kill-switch ""))
  (debug:print-info 0 *default-log-port* "Attempting to kill server process " pid " on host " hostname)
  (setenv "TARGETHOST" hostname)
  (let* ((logdir (if (directory-exists? "logs")
                    "logs/"
                    ""))
         (logfile (if logdir (conc "logs/server-"pid"-"hostname".log") #f))
         (gzfile  (if logfile (conc logfile ".gz"))))
    (setenv "TARGETHOST_LOGF" (conc logdir "server-kills.log"))

    (system (conc "nbfake kill "kill-switch" "pid))

    (when logfile
      (thread-sleep! 0.5)
      (if (common:file-exists? gzfile) (delete-file gzfile))
      (system (conc "gzip " logfile))
      
      (unsetenv "TARGETHOST_LOGF")
      (unsetenv "TARGETHOST"))))
    
 
;;======================================================================
;; M O N I T O R S
;;======================================================================

(define (tasks:remove-monitor-record mdb)
  (sqlite3:execute mdb "DELETE FROM monitors WHERE pid=? AND hostname=?;"
		   (current-process-id)
		   (get-host-name)))

(define (tasks:get-monitors mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (a . rem)
       (set! res (cons (apply vector a rem) res)))
     mdb
     "SELECT id,pid,strftime('%m/%d/%Y %H:%M',datetime(start_time,'unixepoch'),'localtime'),strftime('%m/%d/%Y %H:%M:%S',datetime(last_update,'unixepoch'),'localtime'),hostname,username FROM monitors ORDER BY last_update ASC;")
    (reverse res)
    ))

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
(define (tasks:register-monitor db port)
  (let* ((pid (current-process-id))
	 (hostname (get-host-name))
	 (userinfo (user-information (current-user-id)))
	 (username (car userinfo)))
    (print "Register monitor, pid: " pid ", hostname: " hostname ", port: " port ", username: " username)
    (sqlite3:execute db "INSERT INTO monitors (pid,start_time,last_update,hostname,username) VALUES (?,strftime('%s','now'),strftime('%s','now'),?,?);"
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

;; 
(define (tasks:start-monitor db mdb)
  (if (> (tasks:get-num-alive-monitors mdb) 2) ;; have two running, no need for more
      (debug:print-info 1 *default-log-port* "Not starting monitor, already have more than two running")
      (let* ((megatestdb     (conc *toppath* "/megatest.db"))
	     (monitordbf     (conc (db:dbfile-path #f) "/monitor.db"))
	     (last-db-update 0)) ;; (file-modification-time megatestdb)))
	(task:register-monitor mdb)
	(let loop ((count      0)
		   (next-touch 0)) ;; next-touch is the time where we need to update last_update
	  ;; if the db has been modified we'd best look at the task queue
	  (let ((modtime (file-modification-time megatestdbpath )))
	    (if (> modtime last-db-update)
		(tasks:process-queue db)) ;; BROKEN. mdb last-db-update megatestdb next-touch))
	    ;; WARNING: Possible race conditon here!!
	    ;; should this update be immediately after the task-get-action call above?
	    (if (> (current-seconds) next-touch)
		(begin
		  (tasks:monitors-update mdb)
		  (loop (+ count 1)(+ (current-seconds) 240)))
		(loop (+ count 1) next-touch)))))))
      
;;======================================================================
;; T A S K S   Q U E U E
;;
;;   NOTE:: These operate on task_queue which is in main.db
;;
;;======================================================================

;; NOTE: It might be good to add one more layer of checking to ensure
;;       that no task gets run in parallel.

;; id INTEGER PRIMARY KEY,
;; action TEXT DEFAULT '',
;; owner TEXT,
;; state TEXT DEFAULT 'new',
;; target TEXT DEFAULT '',
;; name TEXT DEFAULT '',
;; testpatt TEXT DEFAULT '',
;; keylock TEXT,
;; params TEXT,
;; creation_time TIMESTAMP DEFAULT (strftime('%s','now')),
;; execution_time TIMESTAMP);


;; register a task
(define (tasks:add dbstruct action owner target runname testpatt params)
  (db:with-db 
   dbstruct #f #t
   (lambda (db)
     (sqlite3:execute db "INSERT INTO tasks_queue (action,owner,state,target,name,testpatt,params,creation_time,execution_time)
                             VALUES (?,?,'new',?,?,?,?,strftime('%s','now'),0);" 
		      action
		      owner
		      target
		      runname
		      testpatt
		      (if params params "")))))

(define (keys:key-vals-hash->target keys key-params)
  (let ((tmp (hash-table-ref/default key-params (vector-ref (car keys) 0) "")))
    (if (> (length keys) 1)
	(for-each (lambda (key)
		    (set! tmp (conc tmp "/" (hash-table-ref/default key-params (vector-ref key 0) ""))))
		  (cdr keys)))
    tmp))
								
;; for use from the gui, not ported
;;
;; (define (tasks:add-from-params mdb action keys key-params var-params)
;;   (let ((target    (keys:key-vals-hash->target keys key-params))
;; 	(owner     (car (user-information (current-user-id))))
;; 	(runname   (hash-table-ref/default var-params "runname" #f))
;; 	(testpatts (hash-table-ref/default var-params "testpatts" "%"))
;; 	(params    (hash-table-ref/default var-params "params"    "")))
;;     (tasks:add mdb action owner target runname testpatts params)))

;; return one task from those who are 'new' OR 'waiting' AND more than 10sec old
;;
(define (tasks:snag-a-task dbstruct)
  (let ((res    #f)
	(keytxt (conc (current-process-id) "-" (get-host-name) "-" (car (user-information (current-user-id))))))
    (db:with-db
     dbstruct #f #t
     (lambda (db)
       ;; first randomly set a new to pid-hostname-hostname
       (sqlite3:execute
	db 
	"UPDATE tasks_queue SET keylock=? WHERE id IN
           (SELECT id FROM tasks_queue 
              WHERE state='new' OR 
                    (state='waiting' AND (strftime('%s','now')-execution_time) > 10) OR
                    state='reset'
              ORDER BY RANDOM() LIMIT 1);" keytxt)

       (sqlite3:for-each-row
	(lambda (id . rem)
	  (set! res (apply vector id rem)))
	db
	"SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time FROM tasks_queue WHERE keylock=? ORDER BY execution_time ASC LIMIT 1;" keytxt)
       (if res ;; yep, have work to be done
	   (begin
	     (sqlite3:execute db "UPDATE tasks_queue SET state='inprogress',execution_time=strftime('%s','now') WHERE id=?;"
			      (tasks:task-get-id res))
	     res)
	   #f)))))

(define (tasks:reset-stuck-tasks dbstruct)
  (let ((res '()))
    (db:with-db
     dbstruct #f #t
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (id delta)
	  (set! res (cons id res)))
	db
	"SELECT id,strftime('%s','now')-execution_time AS delta FROM tasks_queue WHERE state='inprogress' AND delta>700 ORDER BY delta DESC LIMIT 2;")
       (sqlite3:execute 
	db 
	(conc "UPDATE tasks_queue SET state='reset' WHERE id IN ('" (string-intersperse (map conc res) "','") "');")
	)))))

;; return all tasks in the tasks_queue table
;;
(define (tasks:get-tasks dbstruct types states)
  (let ((res '()))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (id . rem)
	  (set! res (cons (apply vector id rem) res)))
	db
	(conc "SELECT id,action,owner,state,target,name,test,item,params,creation_time,execution_time 
                  FROM tasks_queue "
	      ;; WHERE  
	      ;;   state IN " statesstr " AND 
	      ;;   action IN " actionsstr 
	      " ORDER BY creation_time DESC;"))
       res))))

(define (tasks:get-last dbstruct target runname)
  (let ((res #f))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (id . rem)
	  (set! res (apply vector id rem)))
	db
	(conc "SELECT id,action,owner,state,target,name,testpatt,keylock,params,creation_time,execution_time 
                  FROM tasks_queue 
 	       WHERE  
	        target = ? AND name =?
	       ORDER BY creation_time DESC LIMIT 1;")
	target runname)
       res))))

;; remove tasks given by a string of numbers comma separated
(define (tasks:remove-queue-entries dbstruct task-ids)
  (db:with-db
   dbstruct #f #t
   (lambda (db)
     (sqlite3:execute db (conc "DELETE FROM tasks_queue WHERE id IN (" task-ids ");")))))

(define (tasks:process-queue dbstruct)
  (let* ((task   (tasks:snag-a-task dbstruct))
	 (action (if task (tasks:task-get-action task) #f)))
    (if action (print "tasks:process-queue task: " task))
    (if action
	(case (string->symbol action)
	  ((run)       (tasks:start-run     dbstruct task))
	  ((remove)    (tasks:remove-runs   dbstruct task))
	  ((lock)      (tasks:lock-runs     dbstruct task))
	  ;; ((monitor)   (tasks:start-monitor db task))
	  ((rollup)    (tasks:rollup-runs   dbstruct task))
	  ((updatemeta)(tasks:update-meta   dbstruct task))
	  ((kill)      (tasks:kill-monitors dbstruct task))))))

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
   
(define (tasks:set-state dbstruct task-id state)
  (db:with-db 
   dbstruct #f #t
   (lambda (db)
     (sqlite3:execute db "UPDATE tasks_queue SET state=? WHERE id=?;" 
		      state 
		      task-id))))

;;======================================================================
;; Access using task key (stored in params; (hash-table->alist flags) hostname pid
;;======================================================================

(define (tasks:param-key->id dbstruct task-params)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (handle-exceptions
      exn
      #f
      (sqlite3:first-result db "SELECT id FROM tasks_queue WHERE params LIKE ?;"
			    task-params)))))

(define (tasks:set-state-given-param-key dbstruct param-key new-state)
  (db:with-db
   dbstruct #f #t
   (lambda (db)
     (sqlite3:execute db "UPDATE tasks_queue SET state=? WHERE params LIKE ?;" new-state param-key))))

(define (tasks:get-records-given-param-key dbstruct param-key state-patt action-patt test-patt)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (handle-exceptions
      exn
      '()
      (sqlite3:first-row db "SELECT id,action,owner,state,target,name,testpatt,keylock,params WHERE
                               params LIKE ? AND state LIKE ? AND action LIKE ? AND testpatt LIKE ?;"
			 param-key state-patt action-patt test-patt)))))

(define (tasks:find-task-queue-records dbstruct target run-name test-patt state-patt action-patt)
  ;; (handle-exceptions
  ;;  exn
  ;;  '()
  ;;  (sqlite3:first-row
  (let ((db (db:delay-if-busy (db:get-db dbstruct)))
	(res '()))
    (sqlite3:for-each-row 
     (lambda (a . b)
       (set! res (cons (cons a b) res)))
     db "SELECT id,action,owner,state,target,name,testpatt,keylock,params FROM tasks_queue 
           WHERE
              target = ? AND name = ? AND state LIKE ? AND action LIKE ? AND testpatt LIKE ?;"
     target run-name state-patt action-patt test-patt)
    res)) ;; )

;; kill any runner processes (i.e. processes handling -runtests) that match target/runname
;; 
;; do a remote call to get the task queue info but do the killing as self here.
;;
(define (tasks:kill-runner target run-name testpatt)
  (let ((records    (rmt:tasks-find-task-queue-records target run-name testpatt "running" "run-tests"))
	(hostpid-rx (regexp "\\s+(\\w+)\\s+(\\d+)$"))) ;; host pid is at end of param string
    (if (null? records)
	(debug:print 0 *default-log-port* "No run launching processes found for " target " / " run-name " with testpatt " (or testpatt "* no testpatt specified! *"))
	(debug:print 0 *default-log-port* "Found " (length records) " run(s) to kill."))
    (for-each 
     (lambda (record)
       (let* ((param-key (list-ref record 8))
	      (match-dat (string-search hostpid-rx param-key)))
	 (if match-dat
	     (let ((hostname  (cadr match-dat))
		   (pid       (string->number (caddr match-dat))))
	       (debug:print 0 *default-log-port* "Sending SIGINT to process " pid " on host " hostname)
	       (if (equal? (get-host-name) hostname)
		   (if (process:alive? pid)
		       (begin
			 (handle-exceptions
			  exn
			  (begin
			    (debug:print 0 *default-log-port* "Kill of process " pid " on host " hostname " failed.")
			    (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
			    #t)
			  (process-signal pid signal/int)
			  (thread-sleep! 5)
			  (if (process:alive? pid)
			      (process-signal pid signal/kill)))))
		   ;;  (call-with-environment-variables
		   (let ((old-targethost (getenv "TARGETHOST")))
		     (setenv "TARGETHOST" hostname)
		     (setenv "TARGETHOST_LOGF" "server-kills.log")
		     (system (conc "nbfake kill " pid))
		     (if old-targethost (setenv "TARGETHOST" old-targethost))
		     (unsetenv "TARGETHOST")
		     (unsetenv "TARGETHOST_LOGF"))))
	     (debug:print-error 0 *default-log-port* "no record or improper record for " target "/" run-name " in tasks_queue in main.db"))))
     records)))

;; (define (tasks:start-run dbstruct mdb task)
;;   (let ((flags (make-hash-table)))
;;     (hash-table-set! flags "-rerun" "NOT_STARTED")
;;     (if (not (string=? (tasks:task-get-params task) ""))
;; 	(hash-table-set! flags "-setvars" (tasks:task-get-params task)))
;;     (print "Starting run " task)
;;     ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
;;     (runs:run-tests db
;; 		    (tasks:task-get-target task)
;; 		    (tasks:task-get-name   task)
;; 		    (tasks:task-get-test   task)
;; 		    (tasks:task-get-item   task)
;; 		    (tasks:task-get-owner  task)
;; 		    flags)
;;     (tasks:set-state mdb (tasks:task-get-id task) "waiting")))
;; 
;; (define (tasks:rollup-runs db mdb task)
;;   (let* ((flags (make-hash-table)) 
;; 	 (keys  (db:get-keys db))
;; 	 (keyvals (keys:target-keyval keys (tasks:task-get-target task))))
;;     ;; (hash-table-set! flags "-rerun" "NOT_STARTED")
;;     (print "Starting rollup " task)
;;     ;; sillyness, just call the damn routine with the task vector and be done with it. FIXME SOMEDAY
;;     (runs:rollup-run db
;; 		     keys 
;; 		     keyvals
;; 		     (tasks:task-get-name  task)
;; 		     (tasks:task-get-owner  task))
;;     (tasks:set-state mdb (tasks:task-get-id task) "waiting")))

;;======================================================================
;;  S Y N C   T O   P O S T G R E S Q L
;;======================================================================

;; In the spirit of "dump your junk in the tasks module" I'll put the
;; sync to postgres here for now.

;; attempt to automatically set up an area. call only if get area by path
;; returns naught of interest
;;
(define (tasks:set-area dbh configdat #!key (toppath #f)) ;; could I safely put *toppath* in for the default for toppath? when would it be evaluated?
  (let loop ((area-name (or (configf:lookup configdat "setup" "area-name")
			    (common:get-area-name)))
	     (modifier  'none))
    (let ((success (handle-exceptions
		       exn
		       (begin
			 (debug:print 0 *default-log-port* "ERROR: cannot create area entry, " ((condition-property-accessor 'exn 'message) exn))
			 #f) ;; FIXME: I don't care for now but I should look at *why* there was an exception
		     (pgdb:add-area dbh area-name (or toppath *toppath*)))))
      (or success
	  (case modifier
	    ((none)(loop (conc (current-user-name) "_" area-name) 'user))
	    ((user)(loop (conc (substring (common:get-area-path-signature) 0 4)
			       area-name) 'areasig))
	    (else #f)))))) ;; give up

(define (task:print-runtime run-times saperator)
(for-each
    (lambda (run-time-info)
     (let* ((run-name  (vector-ref run-time-info 0))
            (run-time  (vector-ref run-time-info 1))
            (target  (vector-ref run-time-info 2)))
        (print target saperator run-name saperator run-time )))
   run-times))

(define (task:print-runtime-as-json run-times)
 (let loop ((run-time-info (car run-times))
            (rema (cdr run-times)) 
            (str ""))
     (let* ((run-name  (vector-ref run-time-info 0))
            (run-time  (vector-ref run-time-info 1))
            (target  (vector-ref run-time-info 2)))
        ;(print (not (equal? str "")))
        (if (not (equal? str "")) 
            (set! str (conc str ",")))
        (if (null? rema)
		(print "[" str "{target:" target ",run-name:" run-name ", run-time:" run-time "}]")
            (loop (car rema) (cdr rema) (conc str "{target:" target ", run-name:" run-name ", run-time:" run-time "}"))))))

(define (task:get-run-times)
   (let* ( 
           (run-patt (if (args:get-arg "-run-patt")
                        (args:get-arg "-run-patt")
                        "%"))
           (target-patt (if (args:get-arg "-target-patt")
                        (args:get-arg "-target-patt")
                        "%"))
 
           (run-times  (rmt:get-run-times  run-patt target-patt )))
   (if (eq? (length run-times) 0)
     (begin
       (print "Data not found!!")
       (exit)))
   (if (equal? (args:get-arg "-dumpmode") "json")
       (task:print-runtime-as-json run-times)
         (if (equal? (args:get-arg "-dumpmode") "csv")
	     (task:print-runtime run-times ",")
	     (task:print-runtime run-times "  ")))))


(define (task:print-testtime test-times saperator)
(for-each
    (lambda (test-time-info)
     (let* ((test-name  (vector-ref test-time-info 0))
            (test-time  (vector-ref test-time-info 2))
            (test-item  (if (eq? (string-length (vector-ref test-time-info 1)) 0)
                               "N/A"
				(vector-ref test-time-info 1))))
        (print  test-name saperator test-item saperator test-time )))
   test-times))

(define (task:print-testtime-as-json test-times)
 (let loop ((test-time-info (car test-times))
            (rema (cdr test-times)) 
            (str ""))
     (let* ((test-name  (vector-ref test-time-info 0))
            (test-time  (vector-ref test-time-info 2))
            (item  (vector-ref test-time-info 1)))
        ;(print (not (equal? str "")))
        (if (not (equal? str "")) 
            (set! str (conc str ",")))
        (if (null? rema)
		(print "[" str "{test-name:" test-name ", item-path:" item ", test-time:" test-time "}]")
            (loop (car rema) (cdr rema) (conc str "{test-name:" test-name ", item-path:" item ", test-time:" test-time "}"))))))


 (define (task:get-test-times)
   (let* ((runname (if (args:get-arg "-runname")
                        (args:get-arg "-runname")
                        #f))
           (target (if (args:get-arg "-target")
                        (args:get-arg "-target")
                        #f))
 
           (test-times  (rmt:get-test-times  runname target )))
   (if (not runname)
      (begin
      (print "Error: Missing argument -runname")
      (exit))) 
    (if (string-contains runname "%")
      (begin
      (print "Error: Invalid runname, '%' not allowed  (" runname ") ")
      (exit)))
    (if (not target)
      (begin
      (print "Error: Missing argument -target")
      (exit)))
     (if  (string-contains target "%")
      (begin
      (print "Error: Invalid target, '%' not allowed  (" target ") ")
      (exit)))
 
   (if (eq? (length test-times) 0)
     (begin
       (print "Data not found!!")
       (exit)))
   (if (equal? (args:get-arg "-dumpmode") "json")
       (task:print-testtime-as-json test-times)
         (if (equal? (args:get-arg "-dumpmode") "csv")
	     (task:print-testtime test-times ",")
	     (task:print-testtime test-times "  ")))))



;; gets mtpg-run-id and syncs the record if different
;;
(define (tasks:run-id->mtpg-run-id dbh cached-info run-id area-info)
  (let* ((runs-ht (hash-table-ref cached-info 'runs))
	 (runinf  (hash-table-ref/default runs-ht run-id #f))
         (area-id (vector-ref area-info 0)))
       (if runinf
	runinf ;; already cached
	(let* ((run-dat    (rmt:get-run-info run-id))               ;; NOTE: get-run-info returns a vector < row header >
	       (run-name   (rmt:get-run-name-from-id run-id))
	       (row        (db:get-rows run-dat))                   ;; yes, this returns a single row
	       (header     (db:get-header run-dat))
	       (state      (db:get-value-by-header row header "state"))
	       (status     (db:get-value-by-header row header "status"))
	       (owner      (db:get-value-by-header row header "owner"))
	       (event-time (db:get-value-by-header row header "event_time"))
	       (comment    (db:get-value-by-header row header "comment"))
	       (fail-count (db:get-value-by-header row header "fail_count"))
	       (pass-count (db:get-value-by-header row header "pass_count"))
               (db-contour (db:get-value-by-header row header "contour"))
	       (contour    (if (args:get-arg "-prepend-contour") 
                                 (if (and db-contour (not (equal? db-contour ""))) 
                                           (begin 
                                            (print "db-contour") 
 						db-contour)
					    (args:get-arg "-contour"))))
	       (keytarg    (if (or (args:get-arg "-prepend-contour") (args:get-arg "-prefix-target"))
	       			(conc "MT_CONTOUR/MT_AREA/" (string-intersperse (rmt:get-keys) "/")) (string-intersperse (rmt:get-keys) "/"))) ;; e.g. version/iteration/platform
	       (target     (if (or (args:get-arg "-prepend-contour") (args:get-arg "-prefix-target")) 
	       			(conc (or (args:get-arg "-prefix-target") (conc contour "/" (common:get-area-name) "/")) (rmt:get-target run-id)) (rmt:get-target run-id)))                 ;; e.g. v1.63/a3e1/ubuntu
	       (spec-id    (pgdb:get-ttype dbh keytarg))
	       (new-run-id (pgdb:get-run-id dbh spec-id target run-name area-id))
	       ;; (area-id    (db:get-value-by-header row header "area_id)"))
	       )
              (if new-run-id
	      (begin ;; let ((run-record (pgdb:get-run-info dbh new-run-id))
		(hash-table-set! runs-ht run-id new-run-id)
		;; ensure key fields are up to date
		(pgdb:refresh-run-info
		 dbh
		 new-run-id
		 state status owner event-time comment fail-count pass-count area-id)
		new-run-id)
	      (if (equal? state "deleted")
                 (begin 
                 (print "Warning: Run with id " run-id " was created after previous sync and deleted before the sync") #f)
               (if (handle-exceptions
		      exn
		      (begin (print-call-chain)
                              (print ((condition-property-accessor 'exn 'message) exn))     
			#f)
                     
                     (pgdb:insert-run
		     dbh
		     spec-id target run-name state status owner event-time comment fail-count pass-count  area-id))
		       (tasks:run-id->mtpg-run-id dbh cached-info run-id area-info)
		  #f)))))))


(define (tasks:sync-test-steps dbh cached-info test-step-ids)
  (print "Sync Steps " test-step-ids )
  (let ((test-ht (hash-table-ref cached-info 'tests))
        (step-ht (hash-table-ref cached-info 'steps)))
    (for-each
     (lambda (test-step-id)
        (let* ((test-step-info  (rmt:get-steps-info-by-id test-step-id))
               (step-id (tdb:step-get-id test-step-info))
               (test-id  (tdb:step-get-test_id    test-step-info))   
	       (stepname (tdb:step-get-stepname  test-step-info))
	       (state (tdb:step-get-state test-step-info))	
	       (status (tdb:step-get-status test-step-info))	
	       (event_time (tdb:step-get-event_time  test-step-info))	
	       (comment  (tdb:step-get-comment test-step-info))	
	       (logfile (tdb:step-get-logfile test-step-info))	
	       (pgdb-test-id  (hash-table-ref/default test-ht test-id #f))
               (pgdb-step-id (if pgdb-test-id 
                                 (pgdb:get-test-step-id dbh pgdb-test-id stepname state)
                                  #f)))
    (if step-id
      (begin  
        (if pgdb-test-id
           (begin 
                (if  pgdb-step-id
                   (begin
                    (print "Updating existing test-step with test-id: " test-id " and step-id " step-id " pgdb test id: " pgdb-test-id )
                    (pgdb:update-test-step dbh pgdb-step-id pgdb-test-id stepname state status event_time comment logfile))
                    (begin
 		      (print "Inserting test-step with test-id: " test-id " and step-id " step-id  " pgdb test id: " pgdb-test-id)
                      (pgdb:insert-test-step dbh pgdb-test-id stepname state status event_time comment logfile )
                      (set! pgdb-step-id  (pgdb:get-test-step-id dbh pgdb-test-id stepname state))))
                (hash-table-set! step-ht step-id pgdb-step-id ))
           (print "Error: Test not cashed")))
      (print "Error: Could not get test step info for step id " test-step-id ))))	;; this is a wierd senario need to debug      	
   test-step-ids)))

(define (tasks:sync-test-gen-data dbh cached-info test-data-ids)
  (let ((test-ht (hash-table-ref cached-info 'tests))
        (data-ht (hash-table-ref cached-info 'data)))
    (for-each
     (lambda (test-data-id)
        (let* ((test-data-info  (rmt:get-data-info-by-id test-data-id))
               (data-id (db:test-data-get-id  test-data-info))
               (test-id  (db:test-data-get-test_id   test-data-info))   
	       (category  (db:test-data-get-category  test-data-info))
	       (variable  (db:test-data-get-variable test-data-info))	
	       (value (db:test-data-get-value  test-data-info))	
               (expected (db:test-data-get-expected  test-data-info))
               (tol (db:test-data-get-tol  test-data-info))
               (units (db:test-data-get-units  test-data-info))     
	       (comment  (db:test-data-get-comment test-data-info))	
               (status (db:test-data-get-status test-data-info))	
	       (type (db:test-data-get-type test-data-info))	
	       (pgdb-test-id  (hash-table-ref/default test-ht test-id #f))
               (pgdb-data-id (if pgdb-test-id 
                                 (pgdb:get-test-data-id dbh pgdb-test-id category variable)
                                  #f)))
    (if data-id
      (begin
        (if pgdb-test-id
           (begin 
                (if  pgdb-data-id
                   (begin
                    (print "Updating existing test-data with test-id: " test-id " and data-id " data-id " pgdb test id: " pgdb-test-id)
                    (pgdb:update-test-data dbh pgdb-data-id pgdb-test-id  category variable value expected tol units comment status type))
                    (begin
 		      (print "Inserting test-data with test-id: " test-id " and data-id " data-id " pgdb test id: " pgdb-test-id)
                      (pgdb:insert-test-data dbh pgdb-test-id category variable value expected tol units comment status type )
                      (set! pgdb-data-id  (pgdb:get-test-data-id dbh pgdb-test-id  category variable))))
                (hash-table-set! data-ht data-id pgdb-data-id ))
             (begin
                 (print "Error: Test not in pgdb"))))

      (print "Error: Could not get test data info for data id " test-data-id ))))	;; this is a wierd senario need to debug      	
   test-data-ids)))



(define (tasks:sync-tests-data dbh cached-info test-ids area-info)
  (let ((test-ht (hash-table-ref cached-info 'tests)))
    (for-each
     (lambda (test-id)
       (print test-id)
       (let* ((test-info    (rmt:get-test-info-by-id #f test-id))
	      (run-id       (db:test-get-run_id    test-info)) ;; look these up in db_records.scm
	      (test-id      (db:test-get-id        test-info))
	      (test-name    (db:test-get-testname  test-info))
	      (item-path    (db:test-get-item-path test-info))
	      (state        (db:test-get-state     test-info))
	      (status       (db:test-get-status    test-info))
	      (host         (db:test-get-host      test-info))
	      (cpuload      (db:test-get-cpuload   test-info))
	      (diskfree     (db:test-get-diskfree  test-info))
	      (uname        (db:test-get-uname     test-info))
	      (run-dir      (db:test-get-rundir    test-info))
	      (log-file     (db:test-get-final_logf test-info))
	      (run-duration (db:test-get-run_duration test-info))
	      (comment      (db:test-get-comment   test-info))
	      (event-time   (db:test-get-event_time test-info))
	      (archived     (db:test-get-archived  test-info))
	      (pgdb-run-id  (tasks:run-id->mtpg-run-id dbh cached-info run-id area-info))
                
	      (pgdb-test-id (if pgdb-run-id 
				(begin
                                  ;(print pgdb-run-id)    
                                 (pgdb:get-test-id dbh pgdb-run-id test-name item-path))
                                 #f)))
	 ;; "id"           "run_id"        "testname"  "state"      "status"      "event_time"
	 ;; "host"         "cpuload"       "diskfree"  "uname"      "rundir"      "item_path"
	 ;; "run_duration" "final_logf"    "comment"   "shortdir"   "attemptnum"  "archived"
         (if pgdb-run-id
           (begin
	   (if pgdb-test-id ;; have a record
	     (begin ;; let ((key-name (conc run-id "/" test-name "/" item-path)))
	       (print "Updating existing test with run-id: " run-id " and test-id: " test-id " pgdb run id: " pgdb-run-id)
	       (pgdb:update-test dbh pgdb-test-id pgdb-run-id test-name item-path state status host cpuload diskfree uname run-dir log-file run-duration comment event-time archived))
	     (begin 
                 (print "Inserting test with run-id: " run-id " and test-id: " test-id  " pgdb run id: " pgdb-run-id)
                (pgdb:insert-test dbh pgdb-run-id test-name item-path state status host cpuload diskfree uname run-dir log-file run-duration comment event-time archived)
                (set! pgdb-test-id (pgdb:get-test-id dbh pgdb-run-id test-name item-path))))
               (hash-table-set! test-ht test-id pgdb-test-id))
              (print "WARNING: Skipping run with run-id:" run-id ". This run was created after privious sync and removed before this sync."))))
     test-ids)))

(define (task:add-area-tag dbh area-info tag) 
  (let* ((tag-info (pgdb:get-tag-info-by-name dbh tag)))
   (if (not tag-info)
     (begin   
     (if (handle-exceptions
	   exn
	   (begin 
               (print ((condition-property-accessor 'exn 'message) exn))     
	   #f)
	   (pgdb:insert-tag  dbh   tag))
                       (set! tag-info (pgdb:get-tag-info-by-name dbh tag))
		  #f)))
     ;;add to area_tags
     (handle-exceptions
	   exn
	   (begin 
               (print ((condition-property-accessor 'exn 'message) exn))     
	   #f)
           (if (not (pgdb:is-area-taged-with-a-tag dbh (vector-ref tag-info 0)  (vector-ref area-info 0)))  
	   (pgdb:insert-area-tag  dbh   (vector-ref tag-info 0)  (vector-ref area-info 0))))))

;; get runs changed since last sync
;; (define (tasks:sync-test-data dbh cached-info area-info)
;;   (let* ((

(define (tasks:sync-to-postgres configdat dest)
  (let* ((dbh         (pgdb:open configdat dbname: dest))
	 (area-info   (pgdb:get-area-by-path dbh *toppath*))
	 (cached-info (make-hash-table))
	 (start       (current-seconds)))
    (for-each (lambda (dtype)
		(hash-table-set! cached-info dtype (make-hash-table)))
	      '(runs targets tests steps data))
    (hash-table-set! cached-info 'start start) ;; when done we'll set sync times to this
    (if area-info
	(let* ((last-sync-time (vector-ref area-info 3))
	       (changed        (rmt:get-changed-record-ids last-sync-time))
	       (run-ids        (alist-ref 'runs       changed))
	       (test-ids       (alist-ref 'tests      changed))
	       (test-step-ids  (alist-ref 'test_steps changed))
	       (test-data-ids  (alist-ref 'test_data  changed))
	       (run-stat-ids   (alist-ref 'run_stats  changed))
               (area-tag    (if (args:get-arg "-area-tag") 
                                 (args:get-arg "-area-tag")
                                  "")))
	 ; (print "area-info: " area-info)
           (if (and (equal? area-tag "") (not (pgdb:is-area-taged dbh (vector-ref area-info 0))))
            (set! area-tag *default-area-tag*)) 
           (if (not (equal? area-tag "")) 
             (task:add-area-tag dbh area-info area-tag)) 
	  (if (not (null? test-ids))
	      (begin
		(print "Syncing " (length test-step-ids) " changed tests")
                ;;Assumption here is that if test-step or test data is changed then the test last update time is changed 
                ;; not syncing run stats at this time as they can be derived from tests table.
		(tasks:sync-tests-data dbh cached-info test-ids area-info)
                ;(exit)   
                (tasks:sync-test-steps dbh cached-info test-step-ids)
                (tasks:sync-test-gen-data dbh cached-info test-data-ids)))
	  (pgdb:write-sync-time dbh area-info start))
	(if (tasks:set-area dbh configdat)
	    (tasks:sync-to-postgres configdat dest)
	    (begin
	      (debug:print 0 *default-log-port* "ERROR: unable to create an area record")
	      #f)))))

