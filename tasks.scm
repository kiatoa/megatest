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

(include "task_records.scm")

;;======================================================================
;; Tasks db
;;======================================================================

;; wait up to aprox n seconds for a journal to go away
;;
(define (tasks:wait-on-journal path n #!key (remove #f)(waiting-msg #f))
  (if (not (string? path))
      (debug:print 0 "ERROR: Called tasks:wait-on-journal with path=" path " (not a string)")
      (let ((fullpath (conc path "-journal")))
	(handle-exceptions
	 exn
	 (begin
	   (print-call-chain (current-error-port))
	   (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	   (debug:print 0 " exn=" (condition->list exn))
	   (debug:print 0 "tasks:wait-on-journal failed. Continuing on, you can ignore this call-chain")
	   #t) ;; if stuff goes wrong just allow it to move on
	 (let loop ((journal-exists (file-exists? fullpath))
		    (count          n)) ;; wait ten times ...
	   (if journal-exists
	       (begin
		 (if (and waiting-msg
			  (eq? (modulo n 30) 0))
		     (debug:print 0 waiting-msg))
		 (if (> count 0)
		     (begin
		       (thread-sleep! 1)
		       (loop (file-exists? fullpath)
			     (- count 1)))
		     (begin
		       (if remove (system (conc "rm -rf " fullpath)))
		       #f)))
	       #t))))))

(define (tasks:get-task-db-path)
  (let* ((linktree     (configf:lookup *configdat* "setup" "linktree"))
	 (dbpath       (conc linktree "/.db")))
    dbpath))



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
	     (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 0 " exn=" (condition->list exn))
	     (thread-sleep! 1)
	     (tasks:open-db numretries (- numretries 1)))
	   (begin
	     (print-call-chain (current-error-port))
	     (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	     (debug:print 0 " exn=" (condition->list exn))))
       (let* ((dbpath       (tasks:get-task-db-path))
	      (dbfile       (conc dbpath "/monitor.db"))
	      (avail        (tasks:wait-on-journal dbpath 10)) ;; wait up to about 10 seconds for the journal to go away
	      (exists       (file-exists? dbpath))
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

(define (tasks:server-lock-slot mdb run-id)
  (tasks:server-clean-out-old-records-for-run-id mdb run-id " tasks:server-lock-slot")
  (if (< (tasks:num-in-available-state mdb run-id) 4)
      (begin 
	(tasks:server-set-available mdb run-id)
	;; (thread-sleep! 2) ;; Try removing this. It may not be needed.
	(tasks:server-am-i-the-server? mdb run-id))
      #f))
	
;; register that this server may come online (first to register goes though with the process)
(define (tasks:server-set-available mdb run-id)
  (sqlite3:execute 
   mdb 
   "INSERT INTO servers (pid,hostname,port,pubport,start_time,      priority,state,mt_version,heartbeat,   interface,transport,run_id)
                   VALUES(?, ?,       ?,   ?, strftime('%s','now'), ?,       ?,    ?,-1,?,        ?,        ?);"
   (current-process-id)       ;; pid
   (get-host-name)            ;; hostname
   -1                         ;; port
   -1                         ;; pubport
   (random 1000)              ;; priority (used a tiebreaker on get-available)
   "available"                ;; state
   (common:version-signature) ;; mt_version
   -1                         ;; interface
   "http"                     ;; transport
   run-id
   ))

(define (tasks:num-in-available-state mdb run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (num-in-queue)
       (set! res num-in-queue))
     mdb
     "SELECT count(id) FROM servers WHERE run_id=? AND state = 'available' AND (strftime('%s','now') - start_time) < 30 ;"
     run-id)
    res))

(define (tasks:num-servers-non-zero-running mdb)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (num-running)
       (set! res num-running))
     mdb
     "SELECT count(id) FROM servers WHERE run_id != 0 AND state = 'running';")
    res))

(define (tasks:server-clean-out-old-records-for-run-id mdb run-id tag)
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE state in ('available','dbprep','shutting-down') AND (strftime('%s','now') - start_time) > 50 AND run_id=?;"
		   (conc "defunct" tag) run-id))

(define (tasks:server-force-clean-running-records-for-run-id mdb run-id tag)
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE state = 'running' AND run_id=?;"
		   (conc "defunct" tag) run-id))

(define (tasks:server-force-clean-run-record mdb run-id iface port tag)
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE state = 'running' AND run_id=? AND interface=? AND port=?;"
		   (conc "defunct" tag) run-id iface port))

(define (tasks:server-delete-records-for-this-pid mdb tag)
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE hostname=? AND pid=?;"
		   (conc "defunct" tag) (get-host-name) (current-process-id)))

(define (tasks:server-delete-record mdb server-id tag) 
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE id=?;"
		   (conc "defunct" tag) server-id)
  ;; use this opportuntity to clean out records over one month old or over 10 minutes old with port = -1 (i.e. a never used placeholder)
  (sqlite3:execute mdb "DELETE FROM servers WHERE state not in ('running','shutting-down','dbprep') AND (strftime('%s','now') - start_time) > 2628000;")
  (sqlite3:execute mdb "DELETE FROM servers WHERE state like 'defunct%' AND port=-1 AND (strftime('%s','now') - start_time) > 600;")
  )

(define (tasks:server-set-state! mdb server-id state)
  (sqlite3:execute mdb "UPDATE servers SET state=?,heartbeat=strftime('%s','now') WHERE id=?;" state server-id))

(define (tasks:server-set-interface-port mdb server-id interface port)
  (sqlite3:execute mdb "UPDATE servers SET interface=?,port=?,heartbeat=strftime('%s','now') WHERE id=?;" interface port server-id))

;; Get random port not used in long time
;;
(define (tasks:server-get-next-port mdb)
  (let* ((lownum        30000)
	(highnum        64000)
	(used-ports     '())
	(get-rand-port  (lambda ()
			  (+ lownum (random (- highnum lownum)))))
	(port-param     (if (and (args:get-arg "-port")
				 (string->number (args:get-arg "-port")))
			    (string->number (args:get-arg "-port"))
			    #f))
	;; (config-port    (if (and (config-lookup  *configdat* "server" "port")
	;; 			 (string->number (config-lookup  *configdat* "server" "port")))
	;; 		    (string->number (config-lookup  *configdat* "server" "port"))
	;; 		    #f))
	)
    (sqlite3:for-each-row
     (lambda (port)
       (set! used-ports (cons port used-ports)))
     mdb
     "SELECT port FROM servers;")
    (cond
     ((and port-param res)   (if (> res port-param) res port-param))
     (port-param             port-param)
     ;; ((and config-port res)  (if (> res config-port) res config-port))
     ;; (config-port            config-port)
     (else
      (let loop ((port     (get-rand-port))
		 (remtries 100))
	(if (member port used-ports)
	    (if (> remtries 0)
		(loop (get-rand-port)(- remtries 1))
		(get-rand-port))
	    port))))))

(define (tasks:server-am-i-the-server? mdb run-id)
  (let* ((all    (tasks:server-get-servers-vying-for-run-id mdb run-id))
	 (first  (if (null? all)
		     (begin (debug:print 0 "ERROR: no servers listed, should be at least one by now.") 
			    (sqlite3:finalize! mdb)
			    (exit 1))
		     (car (db:get-rows all))))
	 (header   (db:get-header all))
	 (id       (db:get-value-by-header first header "id"))
	 (hostname (db:get-value-by-header first header "hostname"))
	 (pid      (db:get-value-by-header first header "pid"))
	 (priority (db:get-value-by-header first header "priority")))
    (debug:print 0 "INFO: am-i-the-server got record " first)
    ;; for now a basic check. add tiebreaking by priority later
    (if (and (equal? hostname (get-host-name))
	     (equal? pid      (current-process-id)))
	id
	#f)))
	     
;; Use: (db:get-value-by-header (car (db:get-rows dat)) (db:get-header dat) "fieldname")
;;  to extract info from the structure returned
;;
(define (tasks:server-get-servers-vying-for-run-id mdb run-id)
   (let* ((header (list "id" "hostname" "pid" "interface" "port" "pubport" "state" "run_id" "priority" "start_time"))
	  (selstr (string-intersperse header ","))
	  (res    '()))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (cons (apply vector a b) res)))
     mdb
     (conc "SELECT " selstr " FROM servers WHERE run_id=? AND state in ('available','running','dbprep') ORDER BY start_time DESC;")
     run-id)
    (vector header res)))

(define (tasks:get-server mdb run-id #!key (retries 10))
  (let ((res  #f)
	(best #f))
    (handle-exceptions
     exn
     (begin
       (print-call-chain (current-error-port))
       (debug:print 0 "WARNING: tasks:get-server db access error.")
       (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (debug:print 0 " for run " run-id)
       (print-call-chain (current-error-port))
       (if (> retries 0)
	   (begin
	     (debug:print 0 " trying call to tasks:get-server again in 10 seconds")
	     (thread-sleep! 10)
	     (tasks:get-server mdb run-id retries: (- retries 0)))
	   (debug:print 0 "10 tries of tasks:get-server all crashed and burned. Giving up and returning \"no server found\"")))
     (sqlite3:for-each-row
      (lambda (id interface port pubport transport pid hostname)
	(set! res (vector id interface port pubport transport pid hostname)))
      mdb
      ;; removed:
      ;; strftime('%s','now')-heartbeat < 10 AND mt_version = ?
      "SELECT id,interface,port,pubport,transport,pid,hostname FROM servers
          WHERE run_id=? AND state='running'
          ORDER BY start_time DESC LIMIT 1;" run-id) ;; (common:version-signature) run-id)
     res)))

(define (tasks:server-running-or-starting? mdb run-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     mdb ;; NEEDS dbprep ADDED
     "SELECT id FROM servers WHERE run_id=? AND (state = 'running' OR (state = 'dbprep' AND  (strftime('%s','now') - start_time) < 60));" run-id)
    res))

(define (tasks:server-running? mdb run-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     mdb ;; NEEDS dbprep ADDED
     "SELECT id FROM servers WHERE run_id=? AND state = 'running';" run-id)
    res))

(define (tasks:need-server run-id)
  (let ((forced (configf:lookup *configdat* "server" "required"))
	(maxqry (cdr (rmt:get-max-query-average run-id)))
	(threshold   (string->number (or (configf:lookup *configdat* "server" "server-query-threshold") "10"))))
    (cond
     (forced 
      (if (common:low-noise-print 60 run-id "server required is set")
	  (debug:print-info 0 "Server required is set, starting server."))
      #t)
     ((> maxqry threshold)
      (if (common:low-noise-print 60 run-id "Max query time execeeded")
	  (debug:print-info 0 "Max avg query time of " maxqry "ms exceeds limit of " threshold "ms, starting server."))
      #t)
     (else
      #f))))

;; try to start a server and wait for it to be available
;;
(define (tasks:start-and-wait-for-server tdbdat run-id delay-max-tries)
  ;; ensure a server is running for this run
  (let loop ((server-dat (tasks:get-server (db:delay-if-busy tdbdat) run-id))
	     (delay-time 0))
      (if (and (not server-dat)
	       (< delay-time delay-max-tries))
	  (begin
	    (if (common:low-noise-print 60 "tasks:start-and-wait-for-server" run-id)
		(debug:print 0 "Try starting server for run-id " run-id))
	    (server:kind-run run-id)
	    (thread-sleep! (min delay-time 5))
	    (loop (tasks:get-server (db:delay-if-busy tdbdat) run-id)(+ delay-time 1))))))

(define (tasks:get-all-servers mdb)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id pid hostname interface port pubport start-time priority state mt-version last-update transport run-id)
       ;;                       0  1     2         3      4     5          6        7     8          9          10        11     12
       (set! res (cons (vector id pid hostname interface port pubport start-time priority state mt-version last-update transport run-id) res)))
     mdb
     "SELECT id,pid,hostname,interface,port,pubport,start_time,priority,state,mt_version,strftime('%s','now')-heartbeat AS last_update,transport,run_id FROM servers WHERE state NOT LIKE 'defunct%' ORDER BY start_time DESC;")
    res))

;; no elegance here ...
;;
(define (tasks:kill-server hostname pid)
  (debug:print-info 0 "Attempting to kill server process " pid " on host " hostname)
  (setenv "TARGETHOST" hostname)
  (setenv "TARGETHOST_LOGF" "server-kills.log")
  (system (conc "nbfake kill " pid))
  (unsetenv "TARGETHOST_LOGF")
  (unsetenv "TARGETHOST"))
 
;; look up a server by run-id and send it a kill, also delete the record for that server
;;
(define (tasks:kill-server-run-id run-id #!key (tag "default"))
  (let* ((tdbdat  (tasks:open-db))
	 (sdat    (tasks:get-server (db:delay-if-busy tdbdat) run-id)))
    (if sdat
	(let ((hostname (vector-ref sdat 6))
	      (pid      (vector-ref sdat 5))
	      (server-id (vector-ref sdat 0)))
	  (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "killed")
	  (debug:print-info 0 "Killing server " server-id " for run-id " run-id " on host " hostname " with pid " pid)
	  (tasks:kill-server hostname pid)
	  (tasks:server-delete-record (db:delay-if-busy tdbdat) server-id tag) )
	(debug:print-info 0 "No server found for run-id " run-id ", nothing to kill"))
    ;; (sqlite3:finalize! tdb)
    ))
    
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

;; 
(define (tasks:start-monitor db mdb)
  (if (> (tasks:get-num-alive-monitors mdb) 2) ;; have two running, no need for more
      (debug:print-info 1 "Not starting monitor, already have more than two running")
      (let* ((megatestdb     (conc *toppath* "/megatest.db"))
	     (monitordbf     (conc (configf:lookup *configdat* "setup" "linktree") "/.db/monitor.db"))
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
      
;;======================================================================
;; T A S K S   Q U E U E
;;
;;   NOTE:: These operate on task_queue which is in main.db
;;
;;======================================================================

;; NOTE: It might be good to add one more layer of checking to ensure
;;       that no task gets run in parallel.



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
  (let ((db (db:delay-if-busy (db:get-db dbstruct #f)))
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
(define (tasks:kill-runner target run-name)
  (let ((records    (rmt:tasks-find-task-queue-records target run-name "%" "running" "run-tests"))
	(hostpid-rx (regexp "\\s+(\\w+)\\s+(\\d+)$"))) ;; host pid is at end of param string
    (if (null? records)
	(debug:print 0 "No run launching processes found for " target " / " run-name)
	(debug:print 0 "Found " (length records) " run(s) to kill."))
    (for-each 
     (lambda (record)
       (let* ((param-key (list-ref record 8))
	      (match-dat (string-search hostpid-rx param-key)))
	 (if match-dat
	     (let ((hostname  (cadr match-dat))
		   (pid       (string->number (caddr match-dat))))
	       (debug:print 0 "Sending SIGINT to process " pid " on host " hostname)
	       (if (equal? (get-host-name) hostname)
		   (if (process:alive? pid)
		       (begin
			 (handle-exceptions
			  exn
			  (begin
			    (debug:print 0 "Kill of process " pid " on host " hostname " failed.")
			    (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
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
	     (debug:print 0 "ERROR: no record or improper record for " target "/" run-name " in tasks_queue in main.db"))))
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

