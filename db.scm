;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Database access
;;======================================================================

(require-extension (srfi 18) extras tcp) ;;  rpc)
;; (import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

;; Note, try to remove this dependency 
;; (use zmq)

(declare (unit db))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))
(declare (uses fs-transport))
(declare (uses client))
(declare (uses mt))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

;; timestamp type (val1 val2 ...)
;; type: meta-info, step
(define *incoming-writes*      '())
(define *completed-writes*   (make-hash-table))
(define *incoming-last-time* (current-seconds))
(define *incoming-mutex*     (make-mutex))
(define *completed-mutex*    (make-mutex))
(define *cache-on* #f)

(define (db:set-sync db)
  (let* ((syncval  (config-lookup *configdat* "setup"     "synchronous"))
	 (val      (cond   ;; 0 | OFF | 1 | NORMAL | 2 | FULL;
		    ((not syncval) #f)
		    ((string->number syncval)
		     (let ((val (string->number syncval)))
		       (if (member val '(0 1 2)) val #f)))
		    ((string-match (regexp "yes" #t) syncval) 1)
		    ((string-match (regexp "no"  #t) syncval) 0)
		    ((string-match (regexp "(off|normal|full)" #t) syncval) syncval)
		    (else 
		     (debug:print 0 "ERROR: synchronous must be 0,1,2,OFF,NORMAL or FULL, you provided: " syncval)
		     #f))))
    (if val
	(begin
	  (debug:print-info 9 "db:set-sync, setting pragma synchronous to " val)
	  (sqlite3:execute db (conc "PRAGMA synchronous = '" val "';"))))))

(define (open-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: Attempted to open db when not in megatest area. Exiting.")
	    (exit))))
  (let* ((dbpath       (conc *toppath* "/megatest.db")) ;; fname)
	 (dbexists     (file-exists? dbpath))
	 (write-access (file-write-access? dbpath))
	 (db           (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler      (make-busy-timeout (if (args:get-arg "-override-timeout")
					      (string->number (args:get-arg "-override-timeout"))
					      6000)))) ;; NB// this is in milliseconds. 136000))) ;; 136000 = 2.2 minutes
    (if (and dbexists
	     (not write-access))
	(set! *db-write-access* write-access)) ;; only unset so other db's also can use this control
    (debug:print-info 11 "open-db, dbpath=" dbpath " argv=" (argv))
    (if write-access (sqlite3:set-busy-handler! db handler))
    (if (not dbexists)
	(db:initialize db))
    ;; Moving db:set-sync to a call in run.scm - it is a persistent value and only needs to be set once
    ;; (db:set-sync db)
    db))

;; keeping it around for debugging purposes only
(define (open-run-close-no-exception-handling  proc idb . params)
  (debug:print-info 11 "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (if (or *db-write-access*
	  (not (member proc *db:all-write-procs*)))
      (let* ((db   (cond
		    ((sqlite3:database? idb) idb)
		    ((not idb)               (open-db))
		    ((procedure? idb)       (idb))
		    (else   	            (open-db))))
	     (res #f))
	(set! res (apply proc db params))
	(if (not idb)(sqlite3:finalize! db))
	(debug:print-info 11 "open-run-close-no-exception-handling END" )
	res)
      #f))

(define (open-run-close-exception-handling proc idb . params)
  (handle-exceptions
   exn
   (let ((sleep-time (random 30))
	 (err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
     (case err-status
       ((busy)
	(thread-sleep! sleep-time))
       (else
	(debug:print 0 "EXCEPTION: database probably overloaded or unreadable.")
	(debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	(print "exn=" (condition->list exn))
	(debug:print 0 " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
	(print-call-chain)
	(thread-sleep! sleep-time)
	(debug:print-info 0 "trying db call one more time....this may never recover, if necessary kill process " (current-process-id) " on host " (get-host-name) " to clean up")))
     (apply open-run-close-exception-handling proc idb params))
   (apply open-run-close-no-exception-handling proc idb params)))

;; (define open-run-close open-run-close-exception-handling)
(define open-run-close open-run-close-exception-handling)

(define *global-delta* 0)
(define *last-global-delta-printed* 0)

(define (open-run-close-measure  proc idb . params)
  (debug:print-info 11 "open-run-close-measure START, idb=" idb ", params=" params)
  (let* ((start-ms (current-milliseconds))
	 (db       (if idb idb (open-db)))
         (throttle (string->number (config-lookup *configdat* "setup" "throttle"))))
    ;; (db:set-sync db)
    (set! res      (apply proc db params))
    (if (not idb)(sqlite3:finalize! db))
    ;; scale by 10, average with current value.
    (set! *global-delta* (/ (+ *global-delta* (* (- (current-milliseconds) start-ms)
						 (if throttle throttle 0.01)))
			    2))
    (if (> (abs (- *last-global-delta-printed* *global-delta*)) 0.08) ;; don't print all the time, only if it changes a bit
	(begin
	  (debug:print-info 1 "launch throttle factor=" *global-delta*)
	  (set! *last-global-delta-printed* *global-delta*)))
    (debug:print-info 11 "open-run-close-measure END" )
    res))

(define (db:initialize db)
  (debug:print-info 11 "db:initialize START")
  (let* ((configdat (car *configinfo*))  ;; tut tut, global warning...
	 (keys     (keys:config-get-fields configdat))
	 (havekeys (> (length keys) 0))
	 (keystr   (keys->keystr keys))
	 (fieldstr (keys->key/field keys)))
    (for-each (lambda (key)
		(let ((keyn key))
		  (if (member (string-downcase keyn)
			      (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count"
				    "pass_count"))
		      (begin
			(print "ERROR: your key cannot be named " keyn " as this conflicts with the same named field in the runs table")
			(system (conc "rm -f " dbpath))
			(exit 1)))))
	      keys)
    ;; (sqlite3:execute db "PRAGMA synchronous = OFF;")
    (db:set-sync db)
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS keys (id INTEGER PRIMARY KEY, fieldname TEXT, fieldtype TEXT, CONSTRAINT keyconstraint UNIQUE (fieldname));")
    (for-each (lambda (key)
		(sqlite3:execute db "INSERT INTO keys (fieldname,fieldtype) VALUES (?,?);" key "TEXT"))
	      keys)
    (sqlite3:execute db (conc 
			 "CREATE TABLE IF NOT EXISTS runs (id INTEGER PRIMARY KEY, " 
			 fieldstr (if havekeys "," "")
			 "runname TEXT,"
			 "state TEXT DEFAULT '',"
			 "status TEXT DEFAULT '',"
			 "owner TEXT DEFAULT '',"
			 "event_time TIMESTAMP,"
			 "comment TEXT DEFAULT '',"
			 "fail_count INTEGER DEFAULT 0,"
			 "pass_count INTEGER DEFAULT 0,"
			 "CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
    (sqlite3:execute db (conc "CREATE INDEX runs_index ON runs (runname" (if havekeys "," "") keystr ");"))
    (sqlite3:execute db 
		     "CREATE TABLE IF NOT EXISTS tests 
                    (id INTEGER PRIMARY KEY,
                     run_id     INTEGER,
                     testname   TEXT,
                     host       TEXT DEFAULT 'n/a',
                     cpuload    REAL DEFAULT -1,
                     diskfree   INTEGER DEFAULT -1,
                     uname      TEXT DEFAULT 'n/a', 
                     rundir     TEXT DEFAULT 'n/a',
                     shortdir   TEXT DEFAULT '',
                     item_path  TEXT DEFAULT '',
                     state      TEXT DEFAULT 'NOT_STARTED',
                     status     TEXT DEFAULT 'FAIL',
                     attemptnum INTEGER DEFAULT 0,
                     final_logf TEXT DEFAULT 'logs/final.log',
                     logdat     BLOB, 
                     run_duration INTEGER DEFAULT 0,
                     comment    TEXT DEFAULT '',
                     event_time TIMESTAMP,
                     fail_count INTEGER DEFAULT 0,
                     pass_count INTEGER DEFAULT 0,
                     archived   INTEGER DEFAULT 0, -- 0=no, 1=in progress, 2=yes
                     CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path)
          );")
    (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname, item_path);")
    (sqlite3:execute db "CREATE VIEW runs_tests AS SELECT * FROM runs INNER JOIN tests ON runs.id=tests.run_id;")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_steps 
                              (id INTEGER PRIMARY KEY,
                               test_id INTEGER, 
                               stepname TEXT, 
                               state TEXT DEFAULT 'NOT_STARTED', 
                               status TEXT DEFAULT 'n/a',
                               event_time TIMESTAMP,
                               comment TEXT DEFAULT '',
                               logfile TEXT DEFAULT '',
                               CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS extradat (id INTEGER PRIMARY KEY, run_id INTEGER, key TEXT, val TEXT);")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_meta (id INTEGER PRIMARY KEY,
                                     testname    TEXT DEFAULT '',
                                     author      TEXT DEFAULT '',
                                     owner       TEXT DEFAULT '',
                                     description TEXT DEFAULT '',
                                     reviewed    TIMESTAMP,
                                     iterated    TEXT DEFAULT '',
                                     avg_runtime REAL,
                                     avg_disk    REAL,
                                     tags        TEXT DEFAULT '',
                                     jobgroup    TEXT DEFAULT 'default',
                                CONSTRAINT test_meta_constraint UNIQUE (testname));")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data (id INTEGER PRIMARY KEY,
                                test_id INTEGER,
                                category TEXT DEFAULT '',
                                variable TEXT,
	                        value REAL,
	                        expected REAL,
	                        tol REAL,
                                units TEXT,
                                comment TEXT DEFAULT '',
                                status TEXT DEFAULT 'n/a',
                                type TEXT DEFAULT '',
                              CONSTRAINT test_data_constraint UNIQUE (test_id,category,variable));")
    ;; Must do this *after* running patch db !! No more. 
    (db:set-var db "MEGATEST_VERSION" megatest-version)
    (debug:print-info 11 "db:initialize END")
    ))

;;======================================================================
;; T E S T   S P E C I F I C   D B 
;;======================================================================

;; Create the sqlite db for the individual test(s)
(define (open-test-db work-area) 
  (debug:print-info 11 "open-test-db " work-area)
  (if (and work-area 
	   (directory? work-area)
	   (file-read-access? work-area))
      (let* ((dbpath    (conc work-area "/testdat.db"))
	     (tdb-writeable (file-write-access? dbpath))
	     (dbexists  (file-exists? dbpath))
	     (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					       (string->number (args:get-arg "-override-timeout"))
					       136000))))
	(handle-exceptions
	 exn
	 (begin
	   (debug:print 2 "ERROR: problem accessing test db " work-area ", you probably should clean and re-run this test"
			((condition-property-accessor 'exn 'message) exn))
	   (set! db (sqlite3:open-database ":memory:"))) ;; open an in-memory db to allow readonly access 
	 (set! db (sqlite3:open-database dbpath)))
	(if *db-write-access* (sqlite3:set-busy-handler! db handler))
	(if (not dbexists)
	    (begin
	      ;; Why use FULL here? This data is not that critical
	      ;; (sqlite3:execute db "PRAGMA synchronous = FULL;")
	      (debug:print-info 11 "Initialized test database " dbpath)
	      (db:testdb-initialize db)))
	;; (sqlite3:execute db "PRAGMA synchronous = 0;")
	(debug:print-info 11 "open-test-db END (sucessful)" work-area)
	;; now let's test that everything is correct
	(handle-exceptions
	 exn
	 (begin
	   (debug:print 0 "ERROR: problem accessing test db " work-area ", you probably should clean and re-run this test"
			((condition-property-accessor 'exn 'message) exn))
	   #f)
	 ;; Is there a cheaper single line operation that will check for existance of a table
	 ;; and raise an exception ?
	 (sqlite3:execute db "SELECT id FROM test_data LIMIT 1;"))
	db)
      (begin
	(debug:print-info 11 "open-test-db END (unsucessful)" work-area)
	#f)))

;; find and open the testdat.db file for an existing test
(define (db:open-test-db-by-test-id db test-id #!key (work-area #f))
  (let* ((test-path (if work-area
			work-area
			(cdb:remote-run db:test-get-rundir-from-test-id db test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

(define (db:testdb-initialize db)
  (debug:print 11 "db:testdb-initialize START")
  (for-each
   (lambda (sqlcmd)
     (sqlite3:execute db sqlcmd))
   (list "CREATE TABLE IF NOT EXISTS test_rundat (
              id INTEGER PRIMARY KEY,
              update_time TIMESTAMP,
              cpuload INTEGER DEFAULT -1,
              diskfree INTEGER DEFAULT -1,
              diskusage INTGER DEFAULT -1,
              run_duration INTEGER DEFAULT 0);"
	 "CREATE TABLE IF NOT EXISTS test_data (
              id INTEGER PRIMARY KEY,
              test_id INTEGER,
              category TEXT DEFAULT '',
              variable TEXT,
	      value REAL,
	      expected REAL,
	      tol REAL,
              units TEXT,
              comment TEXT DEFAULT '',
              status TEXT DEFAULT 'n/a',
              type TEXT DEFAULT '',
              CONSTRAINT test_data_constraint UNIQUE (test_id,category,variable));"
	 "CREATE TABLE IF NOT EXISTS test_steps (
              id INTEGER PRIMARY KEY,
              test_id INTEGER, 
              stepname TEXT, 
              state TEXT DEFAULT 'NOT_STARTED', 
              status TEXT DEFAULT 'n/a',
              event_time TIMESTAMP,
              comment TEXT DEFAULT '',
              logfile TEXT DEFAULT '',
              CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));"
	 ;; test_meta can be used for handing commands to the test
	 ;; e.g. KILLREQ
	 ;;      the ackstate is set to 1 once the command has been completed
	 "CREATE TABLE IF NOT EXISTS test_meta (
              id INTEGER PRIMARY KEY,
              var TEXT,
              val TEXT,
              ackstate INTEGER DEFAULT 0,
              CONSTRAINT metadat_constraint UNIQUE (var));"))
  (debug:print 11 "db:testdb-initialize END"))

;;======================================================================
;; L O G G I N G    D B 
;;======================================================================

(define (open-logging-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((dbpath    (conc (if *toppath* (conc *toppath* "/") "") "logging.db")) ;; fname)
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					   (string->number (args:get-arg "-override-timeout"))
					   136000)))) ;; 136000)))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(begin
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS log (id INTEGER PRIMARY KEY,event_time TIMESTAMP DEFAULT (strftime('%s','now')),logline TEXT,pwd TEXT,cmdline TEXT,pid INTEGER);")
	  (sqlite3:execute db (conc "PRAGMA synchronous = 0;"))))
    db))

(define (db:log-local-event . loglst)
  (let ((logline (apply conc loglst)))
    ;; (pwd     (current-directory))
    ;; (cmdline (string-intersperse (argv) " "))
    ;; (pid     (current-process-id)))
    (db:log-event logline)))

(define (db:log-event logline)
  (let ((db (open-logging-db)))
    (sqlite3:execute db "INSERT INTO log (logline,pwd,cmdline,pid) VALUES (?,?,?,?);"
		     logline
		     (current-directory)
		     (string-intersperse (argv) " ")
		     (current-process-id))
    (sqlite3:finalize! db)
    logline))

;;======================================================================
;; TODO:
;;   put deltas into an assoc list with version numbers
;;   apply all from last to current
;;======================================================================
(define (patch-db db)
  (handle-exceptions
   exn
   (begin
     (print "Exception: " exn)
     (print "ERROR: Possible out of date schema, attempting to add table metadata...")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER, var TEXT, val TEXT,
                                 CONSTRAINT metadat_constraint UNIQUE (var));")
     (if (not (db:get-var db "MEGATEST_VERSION"))
	 (db:set-var db "MEGATEST_VERSION" 1.17)))
   (let ((mver (db:get-var db "MEGATEST_VERSION"))
	 (test-meta-def "CREATE TABLE IF NOT EXISTS test_meta (id INTEGER PRIMARY KEY,
                                     testname    TEXT DEFAULT '',
                                     author      TEXT DEFAULT '',
                                     owner       TEXT DEFAULT '',
                                     description TEXT DEFAULT '',
                                     reviewed    TIMESTAMP,
                                     iterated    TEXT DEFAULT '',
                                     avg_runtime REAL,
                                     avg_disk    REAL,
                                     tags        TEXT DEFAULT '',
                                CONSTRAINT test_meta_constraint UNIQUE (testname));"))
     (print "Current schema version: " mver " current megatest version: " megatest-version)
     (cond
      ((not mver)
       (print "Adding megatest-version to metadata") ;; Need to recreate the table
       (sqlite3:execute db "DROP TABLE IF EXISTS metadat;")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
       (db:set-var db "MEGATEST_VERSION" 1.17)
       (patch-db))
      ((< mver 1.21)
       (sqlite3:execute db "DROP TABLE IF EXISTS metadat;")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
       (db:set-var db "MEGATEST_VERSION" 1.21) ;; set before, just in case the changes are already applied
       (sqlite3:execute db test-meta-def)
					;(for-each 
					; (lambda (stmt)
					;   (sqlite3:execute db stmt))
					; (list 
					;  "ALTER TABLE tests ADD COLUMN first_err TEXT;"
					;  "ALTER TABLE tests ADD COLUMN first_warn TEXT;"
					;  ))
       (patch-db))
      ((< mver 1.24)
       (db:set-var db "MEGATEST_VERSION" 1.24)
       (sqlite3:execute db "DROP TABLE IF EXISTS test_data;")
       (sqlite3:execute db "DROP TABLE IF EXISTS test_meta;")
       (sqlite3:execute db test-meta-def)
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data (id INTEGER PRIMARY KEY,
                                test_id INTEGER,
                                category TEXT DEFAULT '',
                                variable TEXT,
	                        value REAL,
	                        expected REAL,
	                        tol REAL,
                                units TEXT,
                                comment TEXT DEFAULT '',
                                status TEXT DEFAULT 'n/a',
                              CONSTRAINT test_data UNIQUE (test_id,category,variable));")
       (print "WARNING: Table test_data and test_meta were recreated. Please do megatest -update-meta")
       (patch-db))
      ((< mver 1.27)
       (db:set-var db "MEGATEST_VERSION" 1.27)
       (sqlite3:execute db "ALTER TABLE test_data ADD COLUMN type TEXT DEFAULT '';")
       (patch-db))
      ((< mver 1.29)
       (db:set-var db "MEGATEST_VERSION" 1.29)
       (sqlite3:execute db "ALTER TABLE test_steps ADD COLUMN logfile TEXT DEFAULT '';")
       (sqlite3:execute db "ALTER TABLE tests ADD COLUMN shortdir TEXT DEFAULT '';"))
      ((< mver 1.36)
       (db:set-var db "MEGATEST_VERSION" 1.36)
       (sqlite3:execute db "ALTER TABLE test_meta ADD COLUMN jobgroup TEXT DEFAULT 'default';"))
      ((< mver 1.37)
       (db:set-var db "MEGATEST_VERSION" 1.37)
       (sqlite3:execute db "ALTER TABLE tests ADD COLUMN archived INTEGER DEFAULT 0;")) 
      ((< mver megatest-version)
       (db:set-var db "MEGATEST_VERSION" megatest-version))))))

;;======================================================================
;; M A I N T E N A N C E
;;======================================================================

;;  select end_time-now from
;;      (select testname,item_path,event_time+run_duration as
;;                          end_time,strftime('%s','now') as now from tests where state in
;;      ('RUNNING','REMOTEHOSTSTART','LAUNCED'));

(define (db:find-and-mark-incomplete db #!key (ovr-deadtime #f))
  (let* ((incompleted '())
	 (oldlaunched '())
	 (toplevels   '())
	 (deadtime-str (configf:lookup *configdat* "setup" "deadtime"))
	 (deadtime     (if (and deadtime-str
				(string->number deadtime-str))
			   (string->number deadtime-str)
			   7200)) ;; two hours
	 (run-ids      (db:get-run-ids db))) ;; iterate over runs to divy up the calls
    (if (number? ovr-deadtime)(set! deadtime ovr-deadtime))
    (for-each
     (lambda (run-id)

       ;; in RUNNING or REMOTEHOSTSTART for more than 10 minutes
       ;;
       ;; THIS CANNOT WORK. The run_duration is not updated in the central db due to performance concerns.
       ;;                   The testdat.db file must be consulted.
       ;;
       ;; HOWEVER: this code in run:test seems to work fine
       ;;              (> (- (current-seconds)(+ (db:test-get-event_time testdat)
       ;;                     (db:test-get-run_duration testdat)))
       ;;                    600) 
       (db:delay-if-busy)
       (sqlite3:for-each-row 
	(lambda (test-id run-dir uname testname item-path)
	  (if (and (equal? uname "n/a")
		   (equal? item-path "")) ;; this is a toplevel test
	      ;; what to do with toplevel? call rollup?
	      (begin
		(set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
		(debug:print-info 0 "Found old toplevel test in RUNNING state, test-id=" test-id))
	      (set! incompleted (cons (list test-id run-dir uname testname item-path run-id) incompleted))))
	db
	"SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > 600 AND state IN ('RUNNING','REMOTEHOSTSTART');"
	run-id)

       ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
       ;;
       (db:delay-if-busy)
       (sqlite3:for-each-row
	(lambda (test-id run-dir uname testname item-path)
	  (if (and (equal? uname "n/a")
		   (equal? item-path "")) ;; this is a toplevel test
	      ;; what to do with toplevel? call rollup?
	      (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
	      (set! oldlaunched (cons (list test-id run-dir uname testname item-path run-id) oldlaunched))))
	db
	"SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > 86400 AND state IN ('LAUNCHED');"
	run-id))
     run-ids)
    
    ;; These are defunct tests, do not do all the overhead of set-state-status. Force them to INCOMPLETE.
    ;;
    (db:delay-if-busy)
    (let* ((min-incompleted (filter (lambda (x)
				     (let* ((testpath (cadr x))
					    (tdatpath (conc testpath "/testdat.db"))
					    (dbexists (file-exists? tdatpath)))
				       (or (not dbexists) ;; if no file then something wrong - mark as incomplete
					   (> (- (current-seconds)(file-modification-time tdatpath)) 600)))) ;; no change in 10 minutes to testdat.db - she's dead Jim
				   incompleted))
	  (min-incompleted-ids (map car min-incompleted))
	  (all-ids             (append min-incompleted-ids (map car oldlaunched))))
      (if (> (length all-ids) 0)
	  (begin
	    (debug:print 0 "WARNING: Marking test(s); " (string-intersperse (map conc all-ids) ", ") " as INCOMPLETE")
	    (sqlite3:execute 
	     db
	     (conc "UPDATE tests SET state='INCOMPLETE' WHERE id IN (" 
		   (string-intersperse (map conc all-ids) ",")
		   ");")))))

    ;; Now do rollups for the toplevel tests
    ;;
    (for-each
     (lambda (toptest)
       (let ((test-name (list-ref toptest 3))
	     (run-id    (list-ref toptest 5)))
	 (cdb:top-test-set-per-pf-counts *runremote* run-id test-name)))
     toplevels)))
		     
;; Clean out old junk and vacuum the database
;;
;; Ultimately do something like this:
;;
;; 1. Look at test records either deleted or part of deleted run:
;;    a. If test dir exists, set the the test to state='UNKNOWN', Set the run to 'unknown'
;;    b. If test dir gone, delete the test record
;; 2. Look at run records
;;    a. If have tests that are not deleted, set state='unknown'
;;    b. ....
;;
(define (db:clean-up db)
  (let ((count-stmt (sqlite3:prepare db "SELECT (SELECT count(id) FROM tests)+(SELECT count(id) FROM runs);"))
	(statements
	 (map (lambda (stmt)
		(sqlite3:prepare db stmt))
	      (list
	       ;; delete all tests that belong to runs that are 'deleted'
	       "DELETE FROM tests WHERE run_id in (SELECT id FROM runs WHERE state='deleted');"
	       ;; delete all tests that are 'DELETED'
	       "DELETE FROM tests WHERE state='DELETED';"
	       ;; delete all tests that have no run
	       "DELETE FROM tests WHERE run_id NOT IN (SELECT DISTINCT id FROM runs);"
	       ;; delete all runs that are state='deleted'
	       "DELETE FROM runs WHERE state='deleted';"
	       ;; delete empty runs
	       "DELETE FROM runs WHERE id NOT IN (SELECT DISTINCT r.id FROM runs AS r INNER JOIN tests AS t ON t.run_id=r.id);"
	       ))))
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    (db:find-and-mark-incomplete db)
    (sqlite3:execute db "VACUUM;")))

;;======================================================================
;; meta get and set vars
;;======================================================================

;; returns number if string->number is successful, string otherwise
;; also updates *global-delta*
(define (db:get-var db var)
  (debug:print-info 11 "db:get-var START " var)
  (let* ((start-ms (current-milliseconds))
         (throttle (let ((t  (config-lookup *configdat* "setup" "throttle")))
		     (if t (string->number t) t)))
	 (res      #f))
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     db "SELECT val FROM metadat WHERE var=?;" var)
    ;; convert to number if can
    (if (string? res)
	(let ((valnum (string->number res)))
	  (if valnum (set! res valnum))))
    ;; scale by 10, average with current value.
    (set! *global-delta* (/ (+ *global-delta* (* (- (current-milliseconds) start-ms)
						 (if throttle throttle 0.01)))
			    2))
    (if (> (abs (- *last-global-delta-printed* *global-delta*)) 0.08) ;; don't print all the time, only if it changes a bit
	(begin
	  (debug:print-info 4 "launch throttle factor=" *global-delta*)
	  (set! *last-global-delta-printed* *global-delta*)))
    (debug:print-info 11 "db:get-var END " var " val=" res)
    res))

(define (db:set-var db var val)
  (debug:print-info 11 "db:set-var START " var " " val)
  (db:delay-if-busy)
  (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val)
  (debug:print-info 11 "db:set-var END " var " " val))

(define (db:del-var db var)
  (debug:print-info 11 "db:del-var START " var)
  (db:delay-if-busy)
  (sqlite3:execute db "DELETE FROM metadat WHERE var=?;" var)
  (debug:print-info 11 "db:del-var END " var))

;; use a global for some primitive caching, it is just silly to
;; re-read the db over and over again for the keys since they never
;; change

;; why get the keys from the db? why not get from the *configdat*
;; using keys:config-get-fields?

(define (db:get-keys db)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(sqlite3:for-each-row 
	 (lambda (key)
	   (set! res (cons key res)))
	 db
	 "SELECT fieldname FROM keys ORDER BY id DESC;")
	(set! *db-keys* res)
	res)))

;; 
(define (db:get-value-by-header row header field)
  (debug:print-info 4 "db:get-value-by-header row: " row " header: " header " field: " field)
  (if (null? header) #f
      (let loop ((hed (car header))
		 (tal (cdr header))
		 (n   0))
	(if (equal? hed field)
	    (vector-ref row n)
	    (if (null? tal) #f (loop (car tal)(cdr tal)(+ n 1)))))))

;;======================================================================
;;  R U N S
;;======================================================================

(define (db:get-run-name-from-id db run-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (runname)
       (set! res runname))
     db
     "SELECT runname FROM runs WHERE id=?;"
     run-id)
    res))

(define (db:get-run-key-val db run-id key)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     db 
     (conc "SELECT " key " FROM runs WHERE id=?;")
     run-id)
    res))

;; keys list to key1,key2,key3 ...
(define (runs:get-std-run-fields keys remfields)
  (let* ((header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (list keystr header)))

;; make a query (fieldname like 'patt1' OR fieldname 
(define (db:patt->like fieldname pattstr #!key (comparator " OR "))
  (let ((patts (if (string? pattstr)
		   (string-split pattstr ",")
		   '("%"))))
    (string-intersperse (map (lambda (patt)
			       (let ((wildtype (if (substring-index "%" patt) "LIKE" "GLOB")))
				 (conc fieldname " " wildtype " '" patt "'")))
			     (if (null? patts)
				 '("")
				 patts))
			comparator)))


;; register a test run with the db
(define (db:register-run db keyvals runname state status user)
  (debug:print 3 "runs:register-run runname: " runname " state: " state " status: " status " user: " user)
  (let* ((keys      (map car keyvals))
	 (keystr    (keys->keystr keys))	 
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (allvals   (append (list runname state status user) (map cadr keyvals)))
	 (qryvals   (append (list runname) (map cadr keyvals)))
	 (key=?str  (string-intersperse (map (lambda (k)(conc k "=?")) keys) " AND ")))
    (debug:print 3 "keys: " keys " allvals: " allvals " keyvals: " keyvals " key=?str is " key=?str)
    (debug:print 2 "NOTE: using target " (string-intersperse (map cadr keyvals) "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvals))) ;; there must be a better way to "apply and"
	(let ((res #f))
	  (db:delay-if-busy)
	  (apply sqlite3:execute db (conc "INSERT OR IGNORE INTO runs (runname,state,status,owner,event_time" comma keystr ") VALUES (?,?,?,?,strftime('%s','now')" comma valslots ");")
		 allvals)
	  (apply sqlite3:for-each-row 
		 (lambda (id)
		   (set! res id))
		 db
		 (let ((qry (conc "SELECT id FROM runs WHERE (runname=? " andstr key=?str ");")))
					;(debug:print 4 "qry: " qry) 
		   qry)
		 qryvals)
	  (db:delay-if-busy)
	  (sqlite3:execute db "UPDATE runs SET state=?,status=?,event_time=strftime('%s','now') WHERE id=? AND state='deleted';" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
	  #f))))


;; replace header and keystr with a call to runs:get-std-run-fields
;;
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;; runpatts: patt1,patt2 ...
;;
(define (db:get-runs db runpatt count offset keypatts)
  (let* ((res       '())
	 (keys       (db:get-keys db))
	 (runpattstr (db:patt->like "runname" runpatt))
	 (remfields  (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header     (append keys remfields))
	 (keystr     (conc (keys->keystr keys) ","
		           (string-intersperse remfields ",")))
	 (qrystr     (conc "SELECT " keystr " FROM runs WHERE (" runpattstr ") " ;; runname LIKE ? "
		           ;; Generate: " AND x LIKE 'keypatt' ..."
		           (if (null? keypatts) ""
		               (conc " AND "
				     (string-join 
				      (map (lambda (keypatt)
					     (let ((key  (car keypatt))
						   (patt (cadr keypatt)))
					       (db:patt->like key patt)))
					   keypatts)
				      " AND ")))
		           " AND state != 'deleted' ORDER BY event_time DESC "
		           (if (number? count)
		               (conc " LIMIT " count)
		               "")
		           (if (number? offset)
		               (conc " OFFSET " offset)
		               ""))))
    (debug:print-info 11 "db:get-runs START qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (cons (apply vector a x) res)))
     db
     qrystr
     )
    (debug:print-info 11 "db:get-runs END qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (vector header res)))

;; Get all targets from the db
;;
(define (db:get-targets db)
  (let* ((res       '())
	 (keys       (db:get-keys db))
	 (header     keys) ;; (map key:get-fieldname keys))
	 (keystr     (keys->keystr keys))
	 (qrystr     (conc "SELECT " keystr " FROM runs;"))
	 (seen       (make-hash-table)))
    (sqlite3:for-each-row
     (lambda (a . x)
       (let ((targ (cons a x)))
	 (if (not (hash-table-ref/default seen targ #f))
	     (begin
	       (hash-table-set! seen targ #t)
	       (set! res (cons (apply vector targ) res))))))
     db
     qrystr)
    (debug:print-info 11 "db:get-targets END qrystr: " qrystr )
    (vector header res)))

;; just get count of runs
(define (db:get-num-runs db runpatt)
  (let ((numruns 0))
    (debug:print-info 11 "db:get-num-runs START " runpatt)
    (sqlite3:for-each-row 
     (lambda (count)
       (set! numruns count))
     db
     "SELECT COUNT(id) FROM runs WHERE runname LIKE ? AND state != 'deleted';" runpatt)
    (debug:print-info 11 "db:get-num-runs END " runpatt)
    numruns))

;; get some basic run stats
;;
;; ( (runname (( state  count ) ... ))
;;   (   ...  
(define (db:get-run-stats db)
  (let ((totals       (make-hash-table))
	(res          '()))
    (sqlite3:for-each-row
     (lambda (runname state count)
       (let* ((stateparts (string-split state "|"))
	      (newstate   (conc (car stateparts) "\n" (cadr stateparts))))
	 (hash-table-set! totals newstate (+ (hash-table-ref/default totals newstate 0) count))
	 (set! res (cons (list runname newstate count) res))))
     db
     "SELECT runname,t.state||'|'||t.status AS s,count(t.id) FROM runs AS r INNER JOIN tests AS t ON r.id=t.run_id GROUP BY s,runname ORDER BY r.event_time,s DESC;" )
    ;; (set! res (reverse res))
    (for-each (lambda (state)
		(set! res (cons (list "Totals" state (hash-table-ref totals state)) res)))
	      (sort (hash-table-keys totals) string>=))
    res))

;; db:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
;;  to extract info from the structure returned
;;
(define (db:get-runs-by-patt db keys runnamepatt targpatt offset limit) ;; test-name)
  (let* ((tmp      (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time")))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (res     '())
	 (key-patt "")
	 (runwildtype (if (substring-index "%" runnamepatt) "like" "glob"))
	 (qry-str  #f)
	 (keyvals  (if targpatt (keys:target->keyval keys targpatt) '())))
    (for-each (lambda (keyval)
		(let* ((key    (car keyval))
		       (patt   (cadr keyval))
		       (fulkey (conc ":" key))
		       (wildtype (if (substring-index "%" patt) "like" "glob")))
		  (if patt
		      (set! key-patt (conc key-patt " AND " key " " wildtype " '" patt "'"))
		      (begin
			(debug:print 0 "ERROR: searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keyvals)
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt " ORDER BY event_time"
			(if limit  (conc " LIMIT " limit)   "")
			(if offset (conc " OFFSET " offset) "")
			";"))
    (debug:print-info 4 "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
    (sqlite3:for-each-row 
     (lambda (a . r)
       (set! res (cons (list->vector (cons a r)) res)))
     db 
     qry-str
     runnamepatt)
    (vector header res)))

;; use (get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
(define (db:get-run-info db run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
  (let* ((res      #f)
	 (keys      (db:get-keys db))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (apply vector a x)))
     db
     (conc "SELECT " keystr " FROM runs WHERE id=? AND state != 'deleted';")
     run-id)
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (let ((finalres (vector header res)))
      ;; (hash-table-set! *run-info-cache* run-id finalres)
      finalres)))

(define (db:set-comment-for-run db run-id comment)
  (debug:print-info 11 "db:set-comment-for-run START run-id: " run-id " comment: " comment)
  (db:delay-if-busy)
  (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment run-id)
  (debug:print-info 11 "db:set-comment-for-run END run-id: " run-id " comment: " comment))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run db run-id)
  (common:clear-caches) ;; don't trust caches after doing any deletion
  ;; First set any related tests to DELETED
  (db:delay-if-busy)
  (let ((stmt1 (sqlite3:prepare db "UPDATE tests SET state='DELETED',comment='' WHERE run_id=?;"))
	(stmt2 (sqlite3:prepare db "UPDATE runs SET state='deleted',comment='' WHERE id=?;")))
    (sqlite3:with-transaction
     db (lambda ()
	  (sqlite3:execute stmt1 run-id)
	  (sqlite3:execute stmt2 run-id)))
    (sqlite3:finalize! stmt1)
    (sqlite3:finalize! stmt2)))
;;  (sqlite3:execute db "DELETE FROM runs WHERE id=?;" run-id))

(define (db:update-run-event_time db run-id)
  (debug:print-info 11 "db:update-run-event_time START run-id: " run-id)
  (db:delay-if-busy)
  (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id)
  (debug:print-info 11 "db:update-run-event_time END run-id: " run-id)) 

(define (db:lock/unlock-run db run-id lock unlock user)
  (let ((newlockval (if lock "locked"
			(if unlock
			    "unlocked"
			    "locked")))) ;; semi-failsafe
    (db:delay-if-busy)
    (sqlite3:execute db "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
    (sqlite3:execute db "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
		     user (conc newlockval " " run-id))
    (debug:print-info 1 "" newlockval " run number " run-id)))

(define (db:set-run-status db run-id status #!key (msg #f))
  (db:delay-if-busy)
  (if msg
      (sqlite3:execute db "UPDATE runs SET status=?,comment=? WHERE id=?;" status msg run-id)
      (sqlite3:execute db "UPDATE runs SET status=? WHERE id=?;" status run-id)))

(define (db:get-run-status db run-id)
  (let ((res "n/a"))
    (sqlite3:for-each-row 
     (lambda (status)
       (set! res status))
     db 
     "SELECT status FROM runs WHERE id=?;" 
     run-id)
    res))

(define (db:get-run-ids db)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res (cons id res)))
     db 
     "SELECT id FROM runs;")))

;;======================================================================
;; K E Y S
;;======================================================================

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (db:get-key-val-pairs db run-id)
  (let* ((keys (db:get-keys db))
	 (res  '()))
    (debug:print-info 11 "db:get-key-val-pairs START keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list key key-val) res)))
	  db qry run-id)))
     keys)
    (debug:print-info 11 "db:get-key-val-pairs END keys: " keys " run-id: " run-id)
    (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals db run-id)
  (let ((mykeyvals (hash-table-ref/default *keyvals* run-id #f)))
    (if mykeyvals 
	mykeyvals
	(let* ((keys (db:get-keys db))
	       (res  '()))
	  (debug:print-info 11 "db:get-key-vals START keys: " keys " run-id: " run-id)
	  (for-each 
	   (lambda (key)
	     (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	       ;; (debug:print 0 "qry: " qry)
	       (sqlite3:for-each-row 
		(lambda (key-val)
		  (set! res (cons key-val res)))
		db qry run-id)))
	   keys)
	  (debug:print-info 11 "db:get-key-vals END keys: " keys " run-id: " run-id)
	  (let ((final-res (reverse res)))
	    (hash-table-set! *keyvals* run-id final-res)
	    final-res)))))

;; The target is keyval1/keyval2..., cached in *target* as it is used often
(define (db:get-target db run-id)
  (let ((mytarg (hash-table-ref/default *target* run-id #f)))
    (if mytarg
	mytarg
	(let* ((keyvals (db:get-key-vals db run-id))
	       (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/")))
	  (hash-table-set! *target* run-id thekey)
	  thekey))))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
(define (db:get-tests-for-run db run-id testpatt states statuses offset limit not-in sort-by sort-order
			      #!key
			      (qryvals #f))
  (let* ((qryvalstr       (case qryvals
			    ((shortlist) "id,run_id,testname,item_path,state,status")
			    ((#f)        "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")
			    (else        qryvals)))
	 (res            '())
	 ;; if states or statuses are null then assume match all when not-in is false
	 (states-qry      (if (null? states) 
			      #f
			      (conc " state "  
				    (if not-in
					" NOT IN ('"
					" IN ('") 
				    (string-intersperse states   "','")
				    "')")))
	 (statuses-qry    (if (null? statuses)
			      #f
			      (conc " status "
				    (if not-in 
					" NOT IN ('"
					" IN ('") 
				    (string-intersperse statuses "','")
				    "')")))
	 (states-statuses-qry 
	  (cond 
	   ((and states-qry statuses-qry)
	    (conc " AND ( " states-qry " AND " statuses-qry " ) "))
	   (states-qry  
	    (conc " AND " states-qry))
	   (statuses-qry 
	    (conc " AND " statuses-qry))
	   (else "")))
	 (tests-match-qry (tests:match->sqlqry testpatt))
	 (qry             (conc "SELECT " qryvalstr
				" FROM tests WHERE run_id=? AND state != 'DELETED' "
				states-statuses-qry
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
				(case sort-by
				  ((rundir)      " ORDER BY length(rundir) ")
				  ((testname)    (conc " ORDER BY testname " (if sort-order (conc sort-order ",") "") " item_path "))
				  ((statestatus) (conc " ORDER BY state " (if  sort-order (conc sort-order ",") "") " status "))
				  ((event_time)  " ORDER BY event_time ")
				  (else          (if (string? sort-by)
						     (conc " ORDER BY " sort-by)
						     "")))
				(if sort-order sort-order "")
				(if limit  (conc " LIMIT " limit)   "")
				(if offset (conc " OFFSET " offset) "")
				";"
				)))
    (debug:print-info 8 "db:get-tests-for-run qry=" qry)
    (sqlite3:for-each-row 
     (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
       (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
     db 
     qry
     run-id
     )
    (case qryvals
      ((shortlist)(map db:test-short-record->norm res))
      ((#f)       res)
      (else       res))))

(define (db:test-short-record->norm inrec)
  ;;  "id,run_id,testname,item_path,state,status"
  ;;  "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
  (vector (vector-ref inrec 0) ;; id
	  (vector-ref inrec 1) ;; run_id
	  (vector-ref inrec 2) ;; testname
	  (vector-ref inrec 4) ;; state
	  (vector-ref inrec 5) ;; status
	  -1 "" -1 -1 "" "-" 
	  (vector-ref inrec 3) ;; item-path
	  -1 "-" "-"))


(define (db:get-tests-for-run-state-status db run-id testpatt)
  (let ((res            '())
	(tests-match-qry (tests:match->sqlqry testpatt)))
    (sqlite3:for-each-row
     (lambda (id testname item-path state status)
       ;; id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
       (set! res (cons (vector id run-id testname state status -1 "" -1 -1 "" "-" item-path -1 "-" "-") res)))
     db 
     (conc "SELECT id,testname,item_path,state,status FROM tests WHERE run_id=? " 
	   (if tests-match-qry (conc " AND (" tests-match-qry ") ") ""))
     run-id)
    res))

(define (db:get-testinfo-state-status db test-id)
  (let ((res            #f))
    (sqlite3:for-each-row
     (lambda (run-id testname item-path state status)
       ;; id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
       (set! res (vector test-id run-id testname state status -1 "" -1 -1 "" "-" item-path -1 "-" "-")))
     db 
     "SELECT run_id,testname,item_path,state,status FROM tests WHERE id=?;" 
     test-id)
    res))

;; get a useful subset of the tests data (used in dashboard
;; use db:mintests-get-{id ,run_id,testname ...}
(define (db:get-tests-for-runs-mindata db run-ids testpatt states status not-in)
  (db:get-tests-for-runs db run-ids testpatt states status not-in: not-in qryvals: "id,run_id,testname,state,status,event_time,item_path"))


;; NB // This is get tests for "runs" (note the plural!!)
;;
;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
;; run-ids is a list of run-ids or a single number or #f for all runs
(define (db:get-tests-for-runs db run-ids testpatt states statuses 
			       #!key (not-in #t)
			       (sort-by #f)
			       (qryvals "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")) ;; 'rundir 'event_time
  (let* ((res '())
	 ;; if states or statuses are null then assume match all when not-in is false
	 (states-qry      (if (null? states) 
			      #f
			      (conc " state "  
				    (if not-in "NOT" "") 
				    " IN ('" 
				    (string-intersperse states   "','")
				    "')")))
	 (statuses-qry    (if (null? statuses)
			      #f
			      (conc " status "
				    (if not-in "NOT" "") 
				    " IN ('" 
				    (string-intersperse statuses "','")
				    "')")))
	 (tests-match-qry (tests:match->sqlqry testpatt))
	 (qry             (conc "SELECT " qryvals 
				" FROM tests WHERE state != 'DELETED' "
				(if run-ids
				    (if (list? run-ids)
					(conc "AND run_id IN (" (string-intersperse (map conc run-ids) ",") ") ")
					(conc "AND run_id=" run-ids " "))
				    " ") ;; #f => run-ids don't filter on run-ids
				(if states-qry   (conc " AND " states-qry)   "")
				(if statuses-qry (conc " AND " statuses-qry) "")
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
				(case sort-by
				  ((rundir)     " ORDER BY length(rundir) DESC;")
				  ((event_time) " ORDER BY event_time ASC;")
				  (else         ";"))
				)))
    (debug:print-info 8 "db:get-tests-for-runs qry=" qry)
    (sqlite3:for-each-row 
     (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
       (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
     db 
     qry
     )
    res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records db test-id #!key (work-area #f))
  ;; Breaking it into two queries for better file access interleaving
  (let* ((tdb (db:open-test-db-by-test-id db test-id work-area: work-area)))
    ;; test db's can go away - must check every time
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:execute tdb "DELETE FROM test_steps;")
	  (sqlite3:execute tdb "DELETE FROM test_data;")
	  (sqlite3:finalize! tdb)))))

;; 
(define (db:delete-test-records db tdb test-id #!key (force #f))
  (common:clear-caches)
  (db:delay-if-busy)
  (if tdb 
      (begin
	(sqlite3:execute tdb "DELETE FROM test_steps;")
	(sqlite3:execute tdb "DELETE FROM test_data;")))
  ;; (sqlite3:execute db "DELETE FROM tests WHERE id=?;" test-id))
  (if db 
      (begin
	(sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" test-id)
	(sqlite3:execute db "DELETE FROM test_data  WHERE test_id=?;" test-id)
	(if force
	    (sqlite3:execute db "DELETE FROM tests WHERE id=?;" test-id)
	    (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id)))))

(define (db:delete-tests-for-run db run-id)
  (common:clear-caches)
  (db:delay-if-busy)
  (sqlite3:execute db "DELETE FROM tests WHERE run_id=?;" run-id))

(define (db:delete-old-deleted-test-records db)
  (common:clear-caches)
  (let ((targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
    (db:delay-if-busy)
    (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED' AND event_time<?;" targtime)))

;; set tests with state currstate and status currstatus to newstate and newstatus
;; use currstate = #f and or currstatus = #f to apply to any state or status respectively
;; WARNING: SQL injection risk. NB// See new but not yet used "faster" version below
;;
(define (db:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)
  (for-each (lambda (testname)
	      (let ((qry (conc "UPDATE tests SET state=?,status=? WHERE "
			       (if currstate  (conc "state='" currstate "' AND ") "")
			       (if currstatus (conc "status='" currstatus "' AND ") "")
			       " run_id=? AND testname=? AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
		;;(debug:print 0 "QRY: " qry)
		(db:delay-if-busy)
		(sqlite3:execute db qry run-id newstate newstatus testname testname)))
	    testnames))

(define (cdb:delete-tests-in-state serverdat run-id state)
  (common:clear-caches)
  (cdb:client-call serverdat 'delete-tests-in-state #t *default-numtries* run-id state))

(define (cdb:tests-update-cpuload-diskfree serverdat test-id cpuload diskfree)
  (cdb:client-call serverdat 'update-cpuload-diskfree #t *default-numtries* cpuload diskfree test-id))

(define (cdb:tests-update-run-duration serverdat test-id minutes)
  (cdb:client-call serverdat 'update-run-duration #t *default-numtries* minutes test-id))

(define (cdb:tests-update-uname-host serverdat test-id uname hostname)
  (cdb:client-call serverdat 'update-uname-host #t *default-numtries* uname hostname test-id))

;; speed up for common cases with a little logic
;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;;
(define (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)
  (db:delay-if-busy)
  (cond
   ((and newstate newstatus newcomment)
    (sqlite3:execute db "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;" newstate newstatus newcomment test-id))
   ((and newstate newstatus)
    (sqlite3:execute db "UPDATE tests SET state=?,status=? WHERE id=?;" newstate newstatus test-id))
   (else
    (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
    (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
    (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment test-id))))
  (mt:process-triggers test-id newstate newstatus))

;; Never used, but should be?
(define (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)
  (db:delay-if-busy)
  (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
 		   state status run-id test-name item-path))

(define (db:get-count-tests-running db)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id NOT IN (SELECT id FROM runs WHERE state='deleted') AND NOT (uname = 'n/a' AND item_path = '');")
    res))

(define (db:get-count-tests-running-for-run-id db run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))  ;; select * from tests where run_id=1 and uname = 'n/a' and item_path='';
     db
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=? AND NOT (uname = 'n/a' AND item_path = '');" run-id)
    res))

(define (db:get-running-stats db)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (state count)
       (set! res (cons (list state count) res)))
     db
     "SELECT state,count(id) FROM tests GROUP BY state ORDER BY id DESC;")
    res))

(define (db:get-count-tests-running-in-jobgroup db jobgroup)
  (if (not jobgroup)
      0 ;; 
      (let ((res 0))
	(sqlite3:for-each-row
	 (lambda (count)
	   (set! res count))
	 db
	 "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART')
             AND testname in (SELECT testname FROM test_meta WHERE jobgroup=?)
             AND NOT (uname = 'n/a' AND item_path = '');"
	 jobgroup)
	res)))

;; done with run when:
;;   0 tests in LAUNCHED, NOT_STARTED, REMOTEHOSTSTART, RUNNING
(define (db:estimated-tests-remaining db run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db ;; NB// KILLREQ means the jobs is still probably running
     "SELECT count(id) FROM tests WHERE state in ('LAUNCHED','NOT_STARTED','REMOTEHOSTSTART','RUNNING','KILLREQ') AND run_id=?;" run-id)
    res))

;; map run-id, testname item-path to test-id
(define (db:get-test-id-cached db run-id testname item-path)
  (let* ((test-key (conc run-id "-" testname "-" item-path))
	 (res      (hash-table-ref/default *test-ids* test-key #f)))
    (if res 
	res
	(begin
	  (sqlite3:for-each-row
	   (lambda (id) ;;  run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )
	     (set! res id)) ;; (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )))
	   db 
	   "SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
	   run-id testname item-path)
	  (hash-table-set! *test-ids* test-key res)
	  res))))

;; map run-id, testname item-path to test-id
(define (db:get-test-id-not-cached db run-id testname item-path)
  (let* ((res #f))
    (sqlite3:for-each-row
     (lambda (id) ;;  run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )
       (set! res id)) ;; (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )))
     db 
     "SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
     run-id testname item-path)
    res))

(define db:get-test-id db:get-test-id-not-cached)

;; given a test-info record, patch in the latest data from the testdat.db file
;; found in the test run directory
;;
;; NOT USED
;;
(define (db:patch-tdb-data-into-test-info db test-id res #!key (work-area #f))
  (let ((tdb (db:open-test-db-by-test-id db test-id work-area: work-area)))
    ;; get state and status from megatest.db in real time
    ;; other fields that perhaps should be updated:
    ;;   fail_count
    ;;   pass_count
    ;;   final_logf
    (sqlite3:for-each-row
     (lambda (state status final_logf)
       (db:test-set-state!        res state)
       (db:test-set-status!       res status)
       (db:test-set-final_logf!   res final_logf))
     db
     "SELECT state,status,final_logf FROM tests WHERE id=?;"
     test-id)
    (if tdb
	(begin
	  (sqlite3:for-each-row
	   (lambda (update_time cpuload disk_free run_duration)
	     (db:test-set-cpuload!      res cpuload)
	     (db:test-set-diskfree!     res disk_free)
	     (db:test-set-run_duration! res run_duration))
	   tdb
	   "SELECT update_time,cpuload,diskfree,run_duration FROM test_rundat ORDER BY id DESC LIMIT 1;")
	  (sqlite3:finalize! tdb))
	;; if the test db is not found what to do?
	;; 1. set state to DELETED
	;; 2. set status to n/a
	(begin
	  (db:test-set-state!  res "NOT_STARTED")
	  (db:test-set-status! res "n/a")))))

(define *last-test-cache-delete* (current-seconds))

(define (db:clean-all-caches)
  (set! *test-info* (make-hash-table))
  (set! *test-id-cache* (make-hash-table)))

;; Use db:test-get* to access
;;
;; Get test data using test_id
(define (db:get-test-info-by-id db test-id)
  (if (not test-id)
      (begin
	(debug:print-info 4 "db:get-test-info-by-id called with test-id=" test-id)
	#f)
      (let ((res #f))
	(sqlite3:for-each-row
	 (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
	   ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
	   (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)))
	 db 
	 "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE id=?;"
	 test-id)
	res)))

;; Use db:test-get* to access
;;
;; Get test data using test_ids
(define (db:get-test-info-by-ids db test-ids)
  (if (null? test-ids)
      (begin
	(debug:print-info 4 "db:get-test-info-by-ids called with test-ids=" test-ids)
	'())
      (let ((res '()))
	(sqlite3:for-each-row
	 (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
	   ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
	   (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
			   res)))
	 db 
	 (conc "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE id in ("
	       (string-intersperse (map conc test-ids) ",") ");"))
	res)))

(define (db:get-test-info db run-id testname item-path)
  (db:get-test-info-by-id db (db:get-test-id db run-id testname item-path)))

(define (db:test-set-comment db test-id comment)
  (db:delay-if-busy)
  (sqlite3:execute 
   db
   "UPDATE tests SET comment=? WHERE id=?;"
   comment test-id))

(define (cdb:test-set-rundir! serverdat run-id test-name item-path rundir)
  (cdb:client-call serverdat 'test-set-rundir #t *default-numtries* rundir run-id test-name item-path))

(define (cdb:test-set-rundir-by-test-id serverdat test-id rundir)
  (cdb:client-call serverdat 'test-set-rundir-by-test-id #t *default-numtries* rundir test-id))

(define (db:test-get-rundir-from-test-id db test-id)
  (let ((res #f)) ;; (hash-table-ref/default *test-paths* test-id #f)))
    ;; (if res
    ;;     res
    ;;     (begin
    (sqlite3:for-each-row
     (lambda (tpath)
       (set! res tpath))
     db 
     "SELECT rundir FROM tests WHERE id=?;"
     test-id)
    ;; (hash-table-set! *test-paths* test-id res)
    res)) ;; ))

(define (cdb:test-set-log! serverdat test-id logf)
  (if (string? logf)(cdb:client-call serverdat 'test-set-log #f *default-numtries* logf test-id)))

;;======================================================================
;; Misc. test related queries
;;======================================================================

;; MUST BE CALLED local!
(define (db:test-get-paths-matching db keynames target fnamepatt #!key (res '()))
  ;; BUG: Move the values derived from args to parameters and push to megatest.scm
  (let* ((testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (or (args:get-arg "-state")   (args:get-arg ":state")    "%"))
	 (statuspatt (or (args:get-arg "-status")  (args:get-arg ":status")   "%"))
	 (runname    (or (args:get-arg "-runname") (args:get-arg ":runname")  "%"))
	 (paths-from-db (cdb:remote-run db:test-get-paths-matching-keynames-target-new db keynames target res
					testpatt:   testpatt
					statepatt:  statepatt
					statuspatt: statuspatt
					runname:    runname)))
    (if fnamepatt
	(apply append 
	       (map (lambda (p)
		      (if (directory-exists? p)
			  (glob (conc p "/" fnamepatt))
			  '()))
		    paths-from-db))
	paths-from-db)))

(define (db:test-get-paths-matching-keynames-target db keynames target res 
						    #!key
						    (testpatt   "%")
						    (statepatt  "%")
						    (statuspatt "%")
						    (runname    "%"))
  (let* ((keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc "r." key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 (testqry (tests:match->sqlqry testpatt))
	 (qrystr (conc "SELECT t.rundir FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id WHERE "
		       keystr " AND r.runname LIKE '" runname "' AND " testqry
		       " AND t.state LIKE '" statepatt "' AND t.status LIKE '" statuspatt 
		       "' ORDER BY t.event_time ASC;")))
    (sqlite3:for-each-row 
     (lambda (p)
       (set! res (cons p res)))
     db 
     qrystr)
    res))

(define (db:test-get-paths-matching-keynames-target-new db keynames target res 
							#!key
							(testpatt   "%")
							(statepatt  "%")
							(statuspatt "%")
							(runname    "%"))
  (let* ((row-ids '())
	 (keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 (testqry (tests:match->sqlqry testpatt))
	 (runsqry (sqlite3:prepare db (conc "SELECT id FROM runs WHERE " keystr " AND runname LIKE '" runname "';")))
	 (tstsqry (sqlite3:prepare db (conc "SELECT rundir FROM tests WHERE run_id=? AND " testqry " AND state LIKE '" statepatt "' AND status LIKE '" statuspatt "' ORDER BY event_time ASC;"))))
    (sqlite3:for-each-row
     (lambda (rid)
       (set! row-ids (cons rid row-ids)))
     runsqry)
    (for-each (lambda (rid)
		(sqlite3:for-each-row 
		 (lambda (p)
		   (set! res (cons p res)))
		 tstsqry rid))
	      row-ids)
    (sqlite3:finalize! tstsqry)
    (sqlite3:finalize! runsqry)
    res))

;; look through tests from matching runs for a file
(define (db:test-get-first-path-matching db keynames target fname)
  ;; [refpaths] is the section where references to other megatest databases are stored
  (let ((mt-paths (configf:get-section "refpaths"))
	(res       (db:test-get-paths-matching db keynames target fname)))
    (let loop ((pathdat (if (null? paths) #f (car mt-paths)))
	       (tal     (if (null? paths) '()(cdr mt-paths))))
      (if (not (null? res))
	  (car res) ;; return first found
	  (if path
	      (let* ((db     (open-db path: (cadr pathdat)))
		     (newres (db:test-get-paths-matching db keynames target fname)))
		(debug:print-info 4 "Trying " (car pathdat) " at " (cadr pathdat))
		(sqlite3:finalize! db)
		(if (not (null? newres))
		    (car newres)
		    (if (null? tal)
			#f
			(loop (car tal)(cdr tal))))))))))

(define (db:test-toplevel-num-items db run-id testname)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (num-items)
       (set! res num-items))
     db
     "SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND state NOT IN ('DELETED');"
     run-id
     testname)
    res))

;;======================================================================
;; QUEUE UP META, TEST STATUS AND STEPS REMOTE ACCESS
;;======================================================================

;; NOTE: Can remove the regex and base64 encoding for zmq
(define (db:obj->string obj)
  (case *transport-type*
    ((fs) obj)
    ((http)
     (string-substitute
      (regexp "=") "_"
      (base64:base64-encode (with-output-to-string (lambda ()(serialize obj))))
      #t))
    ((zmq)(with-output-to-string (lambda ()(serialize obj))))
    (else obj)))

(define (db:string->obj msg)
  (case *transport-type*
    ((fs) msg)
    ((http)
     (if (string? msg)
	 (with-input-from-string 
	     (base64:base64-decode
	      (string-substitute 
	       (regexp "_") "=" msg #t))
	   (lambda ()(deserialize)))
	 (vector #f #f #f))) ;; crude reply for when things go awry
    ((zmq)(with-input-from-string msg (lambda ()(deserialize))))
    (else msg)))

(define (cdb:use-non-blocking-mode proc)
  (set! *client-non-blocking-mode* #t)
  (let ((res (proc)))
    (set! *client-non-blocking-mode* #f)
    res))

;; params = 'target cached remparams
;;
;; make-vector-record cdb packet client-sig qtype immediate query-sig params qtime
;;
;; cdb:client-call is the unified interface to all the transports. It dispatches the
;;                 query to a server routine (e.g. server:client-send-recieve) that 
;;                 transports the data to the server where it is passed to db:process-queue-item
;;                 which either returns the data to the calling server routine or 
;;                 directly calls the returning procedure (e.g. zmq).
;;
(define (cdb:client-call serverdat qtype immediate numretries . params)
  (debug:print-info 11 "cdb:client-call serverdat=" serverdat ", qtype=" qtype ", immediate=" immediate ", numretries=" numretries ", params=" params)
  (case *transport-type* 
    ((fs)
     (let ((packet (vector "na" qtype immediate "na" params 0)))
       (fs:process-queue-item packet)))
    ((http)
     (let* ((client-sig  (client:get-signature))
	    (query-sig   (message-digest-string (md5-primitive) (conc qtype immediate params)))
	    (zdat        (db:obj->string (vector client-sig qtype immediate query-sig params (current-seconds))))) ;; (with-output-to-string (lambda ()(serialize params))))
       (debug:print-info 11 "zdat=" zdat)
       (let* ((res  #f)
	      (rawdat      (http-transport:client-send-receive serverdat zdat))
	      (tmp         #f))
	 (debug:print-info 11 "Sent " zdat ", received " rawdat)
	 (if rawdat
	     (begin
	       (set! tmp (db:string->obj rawdat))
	       (vector-ref tmp 2))
	     (begin
	       (debug:print 0 "ERROR: Communication with the server failed. Exiting if possible")
	       (exit 1))))))
    ((zmq)
     (handle-exceptions
      exn
      (begin
	(debug:print-info 0 "cdb:client-call timeout or error. Trying again in 5 seconds")
	(thread-sleep! 5) 
	(if (> numretries 0)(apply cdb:client-call serverdat qtype immediate (- numretries 1) params)))
      (let* ((push-socket (vector-ref serverdat 0))
	     (sub-socket  (vector-ref serverdat 1))
	     (client-sig  (client:get-signature))
	     (query-sig   (message-digest-string (md5-primitive) (conc qtype immediate params)))
	     (zdat        (db:obj->string (vector client-sig qtype immediate query-sig params (current-seconds)))) ;; (with-output-to-string (lambda ()(serialize params))))
	     (res  #f)
	     (send-receive (lambda ()
			     (debug:print-info 11 "sending message")
			     (send-message push-socket zdat)
			     (debug:print-info 11 "message sent")
			     (let loop ()
			       ;; get the sender info
			       ;; this should match (client:get-signature)
			       ;; we will need to process "all" messages here some day
			       (receive-message* sub-socket)
			       ;; now get the actual message
			       (let ((myres (db:string->obj (receive-message* sub-socket))))
				 (if (equal? query-sig (vector-ref myres 1))
				     (set! res (vector-ref myres 2))
				     (loop)))))))
	;; (timeout (lambda ()
	;;     	(let loop ((n numretries))
	;;     	  (thread-sleep! 15)
	;;     	  (if (not res)
	;;     	      (if (> numretries 0)
	;;     		  (begin
	;;     		    (debug:print 2 "WARNING: no reply to query " params ", trying resend")
	;;     		    (debug:print-info 11 "re-sending message")
	;;     		    (send-message push-socket zdat)
	;;     		    (debug:print-info 11 "message re-sent")
	;;     		    (loop (- n 1)))
	;;     		  ;; (apply cdb:client-call *runremote* qtype immediate (- numretries 1) params))
	;;     		  (begin
	;;     		    (debug:print 0 "ERROR: cdb:client-call timed out " params ", exiting.")
	;;     		    (exit 5))))))))
	(debug:print-info 11 "Starting threads")
	(let ((th1 (make-thread send-receive "send receive"))
	      ;; (th2 (make-thread timeout      "timeout"))
	      )
	  (thread-start! th1)
	  ;; (thread-start! th2)
	  (thread-join!  th1)
	  (debug:print-info 11 "cdb:client-call returning res=" res)
	  res))))))

(define (cdb:set-verbosity serverdat val)
  (cdb:client-call serverdat 'set-verbosity #f *default-numtries* val))

(define (cdb:login serverdat keyval signature)
  (cdb:client-call serverdat 'login #t *default-numtries* keyval megatest-version signature))

(define (cdb:logout serverdat keyval signature)
  (cdb:client-call serverdat 'logout #t *default-numtries* keyval signature))

(define (cdb:num-clients serverdat)
  (cdb:client-call serverdat 'numclients #t *default-numtries*))

;; I think this would be more efficient if executed on client side FIXME???
(define (cdb:test-set-status-state serverdat test-id status state msg)
  (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
      (cdb:client-call serverdat 'set-test-start-time #t *default-numtries* test-id))
  (if msg
      (cdb:client-call serverdat 'state-status-msg #t *default-numtries* state status msg test-id)
      (cdb:client-call serverdat 'state-status #t *default-numtries* state status test-id))) ;; run-id test-name item-path minutes cpuload diskfree tmpfree) 

(define (cdb:test-rollup-test_data-pass-fail serverdat test-id)
  (cdb:client-call serverdat 'test_data-pf-rollup #t *default-numtries* test-id test-id test-id test-id))

(define (cdb:pass-fail-counts serverdat test-id fail-count pass-count)
  (cdb:client-call serverdat 'pass-fail-counts #t *default-numtries* fail-count pass-count test-id))

(define (cdb:tests-register-test serverdat run-id test-name item-path)
  (cdb:client-call serverdat 'register-test #t *default-numtries* run-id test-name item-path))

;; more transactioned calls, these for roll-up-pass-fail stuff
(define (cdb:update-pass-fail-counts serverdat run-id test-name)
  (cdb:client-call serverdat 'update-fail-pass-counts #t *default-numtries* run-id test-name run-id test-name run-id test-name))

(define (cdb:top-test-set-running serverdat run-id test-name)
  (cdb:client-call serverdat 'top-test-set-running #t *default-numtries* run-id test-name))

(define (cdb:top-test-set-per-pf-counts serverdat run-id test-name)
  (cdb:client-call serverdat 'top-test-set-per-pf-counts #t *default-numtries* run-id test-name run-id test-name run-id test-name run-id test-name))

;;=

(define (cdb:flush-queue serverdat)
  (cdb:client-call serverdat 'flush #f *default-numtries*))

(define (cdb:kill-server serverdat pid)
  (cdb:client-call serverdat 'killserver #t *default-numtries* pid))

(define (cdb:roll-up-pass-fail-counts serverdat run-id test-name item-path status)
  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:roll-up-pass-fail-counts #f run-id test-name item-path status))

(define (cdb:get-test-info serverdat run-id test-name item-path)
  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info #f run-id test-name item-path))

(define (cdb:get-test-info-by-id serverdat test-id)
  (let ((test-dat (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info-by-id #f test-id)))
    (hash-table-set! *test-info* test-id (vector (current-seconds) test-dat)) ;; cached for use where up-to-date info is not needed
    test-dat))

;; db should be db open proc or #f
(define (cdb:remote-run proc db . params)
  (if (or *db-write-access*
	  (not (member proc *db:all-write-procs*)))
      (handle-exceptions
       exn
       (let ((sleep-time (random 20))
	     (err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
	 (case err-status
	   ((busy)(thread-sleep! 4))
	   (else
	    (debug:print 0 "WARNING: possible problem with call to cdb:remote-run, database may be read-only and locked, waiting and trying again ...")
	    (thread-sleep! sleep-time)))
	 (apply cdb:remote-run proc db params))
       (apply cdb:client-call *runremote* 'immediate #f *default-numtries* open-run-close proc #f params))
      (begin
	(debug:print 0 "ERROR: Attempt to access read-only database")
	#f)))

(define (db:test-get-logfile-info db run-id test-name)
  (let ((res #f))
    (sqlite3:for-each-row 
     (lambda (path final_logf)
       (set! logf final_logf)
       (set! res (list path final_logf))
       (if (directory? path)
	   (debug:print 2 "Found path: " path)
	   (debug:print 2 "No such path: " path)))
     db
     "SELECT rundir,final_logf FROM tests WHERE run_id=? AND testname=? AND item_path='';"
     run-id test-name)
    res))

;;======================================================================
;; A G R E G A T E D   T R A N S A C T I O N   D B   W R I T E S 
;;======================================================================

(define db:queries 
  (list '(register-test          "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');")
	;; Test state and status
	'(set-test-state         "UPDATE tests SET state=?   WHERE id=?;")
	'(set-test-status        "UPDATE tests SET state=?   WHERE id=?;")
	'(state-status           "UPDATE tests SET state=?,status=? WHERE id=?;")
	'(state-status-msg       "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;")
	;; Test comment
	'(set-test-comment       "UPDATE tests SET comment=? WHERE id=?;")
	'(set-test-start-time    "UPDATE tests SET event_time=strftime('%s','now') WHERE id=?;")
	'(pass-fail-counts       "UPDATE tests SET fail_count=?,pass_count=? WHERE id=?;")
	;; test_data-pf-rollup is used to set a tests PASS/FAIL based on the pass/fail info from the steps
	'(test_data-pf-rollup    "UPDATE tests
                                    SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                                      THEN 'FAIL'
                                    WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
                                      (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
                                    THEN 'PASS'
                                    ELSE status
                                    END WHERE id=?;")
	'(test-set-log            "UPDATE tests SET final_logf=? WHERE id=?;")
	'(test-set-rundir-by-test-id "UPDATE tests SET rundir=? WHERE id=?")
	'(test-set-rundir         "UPDATE tests SET rundir=? WHERE run_id=? AND testname=? AND item_path=?;")
	'(delete-tests-in-state   "DELETE FROM tests WHERE state=? AND run_id=?;")
	'(tests:test-set-toplog   "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';")
	'(update-cpuload-diskfree "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;")
	'(update-run-duration     "UPDATE tests SET run_duration=? WHERE id=?;")
	'(update-uname-host       "UPDATE tests SET uname=?,host=? WHERE id=?;")
	'(update-test-state       "UPDATE tests SET state=? WHERE state=? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	'(update-test-status      "UPDATE tests SET status=? WHERE status like ? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	;; stuff for roll-up-pass-fail-counts
	'(update-fail-pass-counts "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status IN ('FAIL','CHECK')),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status IN ('PASS','WARN','WAIVED'))
             WHERE run_id=? AND testname=? AND item_path='';")
	'(top-test-set-running  "UPDATE tests SET state='RUNNING' WHERE run_id=? AND testname=? AND item_path='';")
	'(top-test-set-per-pf-counts "UPDATE tests
                       SET state=CASE 
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE run_id=? AND testname=?
                                                     AND item_path != '' 
                                                     AND status NOT IN ('TEN_STRIKES','BLOCKED')
                                                     AND state in ('RUNNING','NOT_STARTED','LAUNCHED','REMOTEHOSTSTART')) > 0 THEN 'RUNNING'
                                   ELSE 'COMPLETED' END,
                            status=CASE 
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE run_id=? AND testname=?
                                              AND item_path != ''
                                              AND state IN ('NOT_STARTED','BLOCKED')) > 0 THEN 'FAIL'
                                  WHEN fail_count > 0 THEN 'FAIL' 
                                  WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' 
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE run_id=? AND testname=?
                                              AND item_path != ''
                                              AND status = 'SKIP') > 0 THEN 'SKIP'
                                  ELSE 'UNKNOWN' END
                       WHERE run_id=? AND testname=? AND item_path='';")
	))

;; do not run these as part of the transaction
(define db:special-queries   '(rollup-tests-pass-fail
			       ;; db:roll-up-pass-fail-counts  ;; WHY NOT!?
			       login
			       immediate
			       flush
			       sync
			       set-verbosity
			       killserver
			       ))

;; not used, intended to indicate to run in calling process
(define db:run-local-queries '()) ;; rollup-tests-pass-fail))

(define (db:process-cached-writes db)
  (let ((queries    (make-hash-table))
	(data       #f))
    (mutex-lock! *incoming-mutex*)
    ;; data is a list of query packets <vector qry-sig query params
    (set! data (reverse *incoming-writes*)) ;;  (sort ... (lambda (a b)(< (vector-ref a 1)(vector-ref b 1)))))
    (set! *server:last-write-flush* (current-milliseconds))
    (set! *incoming-writes* '())
    (mutex-unlock! *incoming-mutex*)
    (if (> (length data) 0)
	;; Process if we have data
	(begin
	  (debug:print-info 7 "Writing cached data " data)
	  
	  ;; Prepare the needed sql statements
	  ;;
	  (for-each (lambda (request-item)
		      (let ((stmt-key (vector-ref request-item 0))
			    (query    (vector-ref request-item 1)))
			(hash-table-set! queries stmt-key (sqlite3:prepare db query))))
		    data)
	  
	  ;; No outer loop needed. Single loop for write items only. Reads trigger flush of queue
	  ;; and then are executed.
	  (sqlite3:with-transaction 
	   db
	   (lambda ()
	     (for-each
	      (lambda (hed)
		(let* ((params   (vector-ref hed 2))
		       (stmt-key (vector-ref hed 0))
		       (stmt     (hash-table-ref/default queries stmt-key #f)))
		  (if stmt
		      (begin
			(db:delay-if-busy)
			(apply sqlite3:execute stmt params))
		      (debug:print 0 "ERROR: Problem Executing " stmt-key " for " params))))
	      data)))
	  
	  ;; let all the waiting calls know all is done
	  (mutex-lock! *completed-mutex*)
	  (for-each (lambda (item)
		      (let ((qry-sig (cdb:packet-get-client-sig item)))
			(debug:print-info 7 "Registering query " qry-sig " as done")
			(hash-table-set! *completed-writes* qry-sig #t)))
		    data)
	  (mutex-unlock! *completed-mutex*)
	  
	  ;; Finalize the statements. Should this be done inside the mutex above?
	  ;; I think sqlite3 mutexes will keep the data safe
	  (for-each (lambda (stmt-key)
		      (sqlite3:finalize! (hash-table-ref queries stmt-key)))
		    (hash-table-keys queries))
	  
	  ;; Do a little record keeping
	  (let ((cache-size (length data)))
	    (if (> cache-size *max-cache-size*)
		(set! *max-cache-size* cache-size)))
	  #t)
	#f)))

(define *db:process-queue-mutex* (make-mutex))

(define *number-of-writes*         0)
(define *writes-total-delay*       0)
(define *total-non-write-delay*    0)
(define *number-non-write-queries* 0)

;; The queue is a list of vectors where the zeroth slot indicates the type of query to
;; apply and the second slot is the time of the query and the third entry is a list of 
;; values to be applied
;;
(define (db:queue-write-and-wait db qry-sig query params)
  (let ((queue-len  0)
	(res        #f)
	(got-it     #f)
	(qry-pkt    (vector qry-sig query params))
	(start-time (current-milliseconds))
	(timeout    (+ 10 (current-seconds)))) ;; set the time out to 10 secs in future

    ;; Put the item in the queue *incoming-writes* 
    (mutex-lock! *incoming-mutex*)
    (set! *incoming-writes* (cons qry-pkt *incoming-writes*))
    (set! queue-len (length *incoming-writes*))
    (mutex-unlock! *incoming-mutex*)

    (debug:print-info 7 "Current write queue length is " queue-len)

    ;; poll for the write to complete, timeout after 10 seconds
    ;; periodic flushing of the queue is taken care of by 
    ;; db:flush-queue
    (let loop ()
      (thread-sleep! 0.001)
      (mutex-lock! *completed-mutex*)
      (if (hash-table-ref/default *completed-writes* qry-sig #f)
	  (begin
	    (hash-table-delete! *completed-writes* qry-sig)
	    (set! got-it #t)))
      (mutex-unlock! *completed-mutex*)
      (if (and (not got-it)
	       (< (current-seconds) timeout))
	  (begin
	    (thread-sleep! 0.01)
	    (loop))))
    (set! *number-of-writes*   (+ *number-of-writes*   1))
    (set! *writes-total-delay* (+ *writes-total-delay* (- (current-milliseconds) start-time)))
    got-it))

(define (db:delay-if-busy #!key (count 6))
  (let ((dbfj (conc *toppath* "/megatest.db-journal")))
    (if (file-exists? dbfj)
	(case count
	  ((6)
	   (thread-sleep! 0.2)
	   (db:delay-if-busy count: 5))
	  ((5)
	   (thread-sleep! 0.4)
	   (db:delay-if-busy count: 4))
	  ((4)
	   (thread-sleep! 0.8)
	   (db:delay-if-busy count: 3))
	  ((3)
	   (thread-sleep! 1.6)
	   (db:delay-if-busy count: 2))
	  ((2)
	   (thread-sleep! 3.2)
	   (db:delay-if-busy count: 1))
	  ((1)
	   (thread-sleep! 6.4)
	   (db:delay-if-busy count: 0))
	  (else
	   (debug:print-info 0 "delaying db access due to high database load.")
	   (thread-sleep! 12.8))))))

(define (db:process-queue-item db item)
  (let* ((stmt-key       (cdb:packet-get-qtype item))
	 (qry-sig        (cdb:packet-get-query-sig item))
	 (return-address (cdb:packet-get-client-sig item))
	 (params         (cdb:packet-get-params item))
	 (query          (let ((q (alist-ref stmt-key db:queries)))
			   (if q (car q) #f))))
    (debug:print-info 11 "Special queries/requests stmt-key=" stmt-key ", return-address=" return-address ", query=" query ", params=" params)
    (if query
	;; hand queries off to the write queue
	(let ((response (case *transport-type*
			  ((http)
			   (debug:print-info 7 "Queuing item " item " for wrapped write")
			   (db:queue-write-and-wait db qry-sig query params))
			  (else 
			   (let* ((remtries 10)
				  (proc     #f))
			     (set! proc (lambda (remtries)
					  (if (> remtries 0)
					      (handle-exceptions
					       exn
					       (let ((sleep-time (random 30))
						     (err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
						 (case err-status
						   ((busy)
						    (thread-sleep! sleep-time)
						    (proc 10)) ;; we never give up on busy
						   (else
						    (debug:print 0 "EXCEPTION: database probably overloaded or unreadable.")
						    (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
						    (debug:print 0 " status:  " ((condition-property-accessor 'sqlite3 'status)  exn))
						    (print-call-chain)
						    (debug:print 0 "Sleeping for " sleep-time)
						    (thread-sleep! sleep-time)
						    (debug:print-info 0 "trying db call one more time....this may never recover, if necessary kill process " (current-process-id) " on host " (get-host-name) " to clean up")
						    (proc (- remtries 1)))))
					       (begin
						 (db:delay-if-busy)
						 (apply sqlite3:execute db query params)))
					      (debug:print 0 "ERROR: too many attempts to access db were made and no sucess. query: "
							   query ", params: " params))))
			     (proc remtries))
			   #t))))
	  (debug:print-info 7 "Received " response " from wrapped write")
	  (server:reply return-address qry-sig response response))
	;; otherwise if appropriate flush the queue (this is a read or complex query)
	(begin
	  (cond
	   ((member stmt-key db:special-queries)
	    (let ((starttime (current-milliseconds)))
	      (debug:print-info 9 "Handling special statement " stmt-key)
	      (case stmt-key
		((immediate)
		 ;; This is a read or mixed read-write query, must clear the cache
		 (case *transport-type*
		   ((http)
		    (mutex-lock! *db:process-queue-mutex*)
		    (db:process-cached-writes db)
		    (mutex-unlock! *db:process-queue-mutex*)))
		 (let* ((proc      (car params))
			(remparams (cdr params))
			;; we are being handed a procedure so call it
			;; (debug:print-info 11 "Running (apply " proc " " remparams ")")
			(result (server:reply return-address qry-sig #t (apply proc remparams))))
		   (set! *total-non-write-delay* (+ *total-non-write-delay* (- (current-milliseconds) starttime))) 
		   (set! *number-non-write-queries* (+ *number-non-write-queries* 1))
		   result))
		((login)
		 (if (< (length params) 3) ;; should get toppath, version and signature
		     (server:reply return-address qry-sig '(#f "login failed due to missing params")) ;; missing params
		     (let ((calling-path (car   params))
			   (calling-vers (cadr  params))
			   (client-key   (caddr params)))
		       (if (and (equal? calling-path *toppath*)
				(equal? megatest-version calling-vers))
			   (begin
			     (hash-table-set! *logged-in-clients* client-key (current-seconds))
			     (server:reply return-address qry-sig #t '(#t "successful login")))      ;; path matches - pass! Should vet the caller at this time ...
			   (server:reply return-address qry-sig #f (list #f (conc "Login failed due to mismatch paths: " calling-path ", " *toppath*)))))))
		((flush sync)
		 (server:reply return-address qry-sig #t 1)) ;; (length data)))
		((set-verbosity)
		 (set! *verbosity* (car params))
		 (server:reply return-address qry-sig #t (list #t *verbosity*)))
		((killserver)
		 (let ((hostname (car  *runremote*))
		       (port     (cadr *runremote*))
		       (pid      (car params)))
		   (debug:print 0 "WARNING: Server on " hostname ":" port " going down by user request!")
		   (debug:print-info 1 "current pid=" (current-process-id))
		   (open-run-close tasks:server-deregister tasks:open-db 
				   hostname
				   port: port)
		   (set! *server-run* #f)
		   (thread-sleep! 3)
		   (process-signal pid signal/kill)
		   (server:reply return-address qry-sig #t '(#t "exit process started"))))
		(else ;; not a command, i.e. is a query
		 (debug:print 0 "ERROR: Unrecognised query/command " stmt-key)
		 (server:reply return-address qry-sig #f 'failed)))))
	   (else
	    (debug:print-info 11 "Executing " stmt-key " for " params)
	    (db:delay-if-busy)
	    (apply sqlite3:execute (hash-table-ref queries stmt-key) params)
	    (server:reply return-address qry-sig #t #t)))))))

(define (db:test-get-records-for-index-file db run-id test-name)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id itempath state status run_duration logf comment)
       (set! res (cons (vector id itempath state status run_duration logf comment) res)))
     db
     "SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname=? AND item_path != '';"
     run-id test-name)
    res))

;;======================================================================
;; Tests meta data
;;======================================================================

;; read the record given a testname
(define (db:testmeta-get-record db testname)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)
       (set! res (vector id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)))
     db "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta WHERE testname=?;"
     testname)
    res))

;; create a new record for a given testname
(define (db:testmeta-add-record db testname)
  (db:delay-if-busy)
  (sqlite3:execute db "INSERT OR IGNORE INTO test_meta (testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags) VALUES (?,'','','','','','','','');" testname))

;; update one of the testmeta fields
(define (db:testmeta-update-field db testname field value)
  (db:delay-if-busy)
  (sqlite3:execute db (conc "UPDATE test_meta SET " field "=? WHERE testname=?;") value testname))

;;======================================================================
;; T E S T   D A T A 
;;======================================================================

(define (db:csv->test-data db test-id csvdata #!key (work-area #f))
  (debug:print 4 "test-id " test-id ", csvdata: " csvdata)
  (let ((tdb     (db:open-test-db-by-test-id db test-id work-area: work-area)))
    (if (sqlite3:database? tdb)
	(let ((csvlist (csv->list (make-csv-reader
				   (open-input-string csvdata)
				   '((strip-leading-whitespace? #t)
				     (strip-trailing-whitespace? #t)) )))) ;; (csv->list csvdata)))
	  (for-each 
	   (lambda (csvrow)
	     (let* ((padded-row  (take (append csvrow (list #f #f #f #f #f #f #f #f #f)) 9))
		    (category    (list-ref padded-row 0))
		    (variable    (list-ref padded-row 1))
		    (value       (any->number-if-possible (list-ref padded-row 2)))
		    (expected    (any->number-if-possible (list-ref padded-row 3)))
		    (tol         (any->number-if-possible (list-ref padded-row 4))) ;; >, <, >=, <=, or a number
		    (units       (list-ref padded-row 5))
		    (comment     (list-ref padded-row 6))
		    (status      (let ((s (list-ref padded-row 7)))
				   (if (and (string? s)(or (string-match (regexp "^\\s*$") s)
							   (string-match (regexp "^n/a$") s)))
				       #f
				       s))) ;; if specified on the input then use, else calculate
		    (type        (list-ref padded-row 8)))
	       ;; look up expected,tol,units from previous best fit test if they are all either #f or ''
	       (debug:print 4 "BEFORE: category: " category " variable: " variable " value: " value 
			    ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment " type: " type)

	       (if (and (or (not expected)(equal? expected ""))
			(or (not tol)     (equal? expected ""))
			(or (not units)   (equal? expected "")))
		   (let-values (((new-expected new-tol new-units)(db:get-prev-tol-for-test db test-id category variable)))
			       (set! expected new-expected)
			       (set! tol      new-tol)
			       (set! units    new-units)))

	       (debug:print 4 "AFTER:  category: " category " variable: " variable " value: " value 
			    ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	       ;; calculate status if NOT specified
	       (if (and (not status)(number? expected)(number? value)) ;; need expected and value to be numbers
		   (if (number? tol) ;; if tol is a number then we do the standard comparison
		       (let* ((max-val (+ expected tol))
			      (min-val (- expected tol))
			      (result  (and (>=  value min-val)(<= value max-val))))
			 (debug:print 4 "max-val: " max-val " min-val: " min-val " result: " result)
			 (set! status (if result "pass" "fail")))
		       (set! status ;; NB// need to assess each one (i.e. not return operator since need to act if not valid op.
			     (case (string->symbol tol) ;; tol should be >, <, >=, <=
			       ((>)  (if (>  value expected) "pass" "fail"))
			       ((<)  (if (<  value expected) "pass" "fail"))
			       ((>=) (if (>= value expected) "pass" "fail"))
			       ((<=) (if (<= value expected) "pass" "fail"))
			       (else (conc "ERROR: bad tol comparator " tol))))))
	       (debug:print 4 "AFTER2: category: " category " variable: " variable " value: " value 
			    ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	       (sqlite3:execute tdb "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status,type) VALUES (?,?,?,?,?,?,?,?,?,?);"
				test-id category variable value expected tol units (if comment comment "") status type)))
	   csvlist)
	  (sqlite3:finalize! tdb)))))

;; get a list of test_data records matching categorypatt
(define (db:read-test-data db test-id categorypatt #!key (work-area #f))
  (let ((tdb  (db:open-test-db-by-test-id db test-id work-area: work-area)))
    (if (sqlite3:database? tdb)
	(let ((res '()))
	  (sqlite3:for-each-row 
	   (lambda (id test_id category variable value expected tol units comment status type)
	     (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
	   tdb
	   "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
	  (sqlite3:finalize! tdb)
	  (reverse res))
	'())))

;; NOTE: Run this local with #f for db !!!
(define (db:load-test-data db test-id #!key (work-area #f))
  (let loop ((lin (read-line)))
    (if (not (eof-object? lin))
	(begin
	  (debug:print 4 lin)
	  (db:csv->test-data db test-id lin work-area: work-area)
	  (loop (read-line)))))
  ;; roll up the current results.
  ;; FIXME: Add the status to 
  (db:test-data-rollup db test-id #f work-area: work-area))

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup db test-id status #!key (work-area #f))
  (let ((tdb (db:open-test-db-by-test-id db test-id work-area: work-area))
	(fail-count 0)
	(pass-count 0))
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:for-each-row
	   (lambda (fcount pcount)
	     (set! fail-count fcount)
	     (set! pass-count pcount))
	   tdb 
	   "SELECT (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail') AS fail_count,
                   (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass') AS pass_count;"
	   test-id test-id)
	  (sqlite3:finalize! tdb)

	  ;; Now rollup the counts to the central megatest.db
	  (cdb:pass-fail-counts *runremote* test-id fail-count pass-count)
	  ;; (sqlite3:execute db "UPDATE tests SET fail_count=?,pass_count=? WHERE id=?;" 
	  ;;                     fail-count pass-count test-id)

	  ;; The flush is not needed with the transaction based write agregation enabled. Remove these commented lines
	  ;; next time you read this!
	  ;;
	  ;; (cdb:flush-queue *runremote*)
	  ;; (thread-sleep! 1) ;; play nice with the queue by ensuring the rollup is at least 10ms later than the set
	  
	  ;; if the test is not FAIL then set status based on the fail and pass counts.
	  (cdb:test-rollup-test_data-pass-fail *runremote* test-id)
	  ;; (sqlite3:execute
	  ;;  db   ;;; NOTE: Should this be WARN,FAIL? A WARN is not a FAIL????? BUG FIXME
	  ;;  "UPDATE tests
	  ;;             SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
	  ;;                THEN 'FAIL'
	  ;;             WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
	  ;;                  (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
	  ;;             THEN 'PASS'
	  ;;             ELSE status
	  ;;         END WHERE id=?;"
	  ;;  test-id test-id test-id test-id)
	  ))))

(define (db:get-prev-tol-for-test db test-id category variable)
  ;; Finish me?
  (values #f #f #f))

;;======================================================================
;; S T E P S 
;;======================================================================

(define (db:step-get-time-as-string vec)
  (seconds->time-string (db:step-get-event_time vec)))

;; db-get-test-steps-for-run
(define (db:get-steps-for-test db test-id #!key (work-area #f))
  (let* ((tdb (db:open-test-db-by-test-id db test-id work-area: work-area))
	 (res '()))
    (if (sqlite3:database? tdb)
	(handle-exceptions
	 exn
	 (begin
	   (debug:print 0 "ERROR: error on access to testdat for test with id " test-id)
	   '())
	 (begin
	   (sqlite3:for-each-row 
	    (lambda (id test-id stepname state status event-time logfile)
	      (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
	    tdb
	    "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
	    test-id)
	   (sqlite3:finalize! tdb)
	   (reverse res)))
	'())))

;; get a pretty table to summarize steps
;;
(define (db:get-steps-table db test-id #!key (work-area #f))
  (let ((steps   (db:get-steps-for-test db test-id work-area: work-area)))
    ;; organise the steps for better readability
    (let ((res (make-hash-table)))
      (for-each 
       (lambda (step)
	 (debug:print 6 "step=" step)
	 (let ((record (hash-table-ref/default 
			res 
			(db:step-get-stepname step) 
			;;        stepname                start end status Duration  Logfile 
			(vector (db:step-get-stepname step) ""   "" ""     ""        ""))))
	   (debug:print 6 "record(before) = " record 
			"\nid:       " (db:step-get-id step)
			"\nstepname: " (db:step-get-stepname step)
			"\nstate:    " (db:step-get-state step)
			"\nstatus:   " (db:step-get-status step)
			"\ntime:     " (db:step-get-event_time step))
	   (case (string->symbol (db:step-get-state step))
	     ((start)(vector-set! record 1 (db:step-get-event_time step))
	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
					(db:step-get-status step)))
	      (if (> (string-length (db:step-get-logfile step))
		     0)
		  (vector-set! record 5 (db:step-get-logfile step))))
	     ((end)  
	      (vector-set! record 2 (any->number (db:step-get-event_time step)))
	      (vector-set! record 3 (db:step-get-status step))
	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
					  (endt   (any->number (vector-ref record 2))))
				      (debug:print 4 "record[1]=" (vector-ref record 1) 
						   ", startt=" startt ", endt=" endt
						   ", get-status: " (db:step-get-status step))
				      (if (and (number? startt)(number? endt))
					  (seconds->hr-min-sec (- endt startt)) "-1")))
	      (if (> (string-length (db:step-get-logfile step))
		     0)
		  (vector-set! record 5 (db:step-get-logfile step))))
	     (else
	      (vector-set! record 2 (db:step-get-state step))
	      (vector-set! record 3 (db:step-get-status step))
	      (vector-set! record 4 (db:step-get-event_time step))))
	   (hash-table-set! res (db:step-get-stepname step) record)
	   (debug:print 6 "record(after)  = " record 
			"\nid:       " (db:step-get-id step)
			"\nstepname: " (db:step-get-stepname step)
			"\nstate:    " (db:step-get-state step)
			"\nstatus:   " (db:step-get-status step)
			"\ntime:     " (db:step-get-event_time step))))
       ;; (else   (vector-set! record 1 (db:step-get-event_time step)))
       (sort steps (lambda (a b)
		     (cond
		      ((<   (db:step-get-event_time a)(db:step-get-event_time b)) #t)
		      ((eq? (db:step-get-event_time a)(db:step-get-event_time b)) 
		       (<   (db:step-get-id a)        (db:step-get-id b)))
		      (else #f)))))
      res)))

;; get a pretty table to summarize steps
;;
(define (db:get-steps-table-list db test-id #!key (work-area #f))
  (let ((steps   (db:get-steps-for-test db test-id work-area: work-area)))
    ;; organise the steps for better readability
    (let ((res (make-hash-table)))
      (for-each 
       (lambda (step)
	 (debug:print 6 "step=" step)
	 (let ((record (hash-table-ref/default 
			res 
			(db:step-get-stepname step) 
			;;        stepname                start end status    
			(vector (db:step-get-stepname step) ""   "" ""     "" ""))))
	   (debug:print 6 "record(before) = " record 
			"\nid:       " (db:step-get-id step)
			"\nstepname: " (db:step-get-stepname step)
			"\nstate:    " (db:step-get-state step)
			"\nstatus:   " (db:step-get-status step)
			"\ntime:     " (db:step-get-event_time step))
	   (case (string->symbol (db:step-get-state step))
	     ((start)(vector-set! record 1 (db:step-get-event_time step))
	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
					(db:step-get-status step)))
	      (if (> (string-length (db:step-get-logfile step))
		     0)
		  (vector-set! record 5 (db:step-get-logfile step))))
	     ((end)  
	      (vector-set! record 2 (any->number (db:step-get-event_time step)))
	      (vector-set! record 3 (db:step-get-status step))
	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
					  (endt   (any->number (vector-ref record 2))))
				      (debug:print 4 "record[1]=" (vector-ref record 1) 
						   ", startt=" startt ", endt=" endt
						   ", get-status: " (db:step-get-status step))
				      (if (and (number? startt)(number? endt))
					  (seconds->hr-min-sec (- endt startt)) "-1")))
	      (if (> (string-length (db:step-get-logfile step))
		     0)
		  (vector-set! record 5 (db:step-get-logfile step))))
	     (else
	      (vector-set! record 2 (db:step-get-state step))
	      (vector-set! record 3 (db:step-get-status step))
	      (vector-set! record 4 (db:step-get-event_time step))))
	   (hash-table-set! res (db:step-get-stepname step) record)
	   (debug:print 6 "record(after)  = " record 
			"\nid:       " (db:step-get-id step)
			"\nstepname: " (db:step-get-stepname step)
			"\nstate:    " (db:step-get-state step)
			"\nstatus:   " (db:step-get-status step)
			"\ntime:     " (db:step-get-event_time step))))
       ;; (else   (vector-set! record 1 (db:step-get-event_time step)))
       (sort steps (lambda (a b)
		     (cond
		      ((<   (db:step-get-event_time a)(db:step-get-event_time b)) #t)
		      ((eq? (db:step-get-event_time a)(db:step-get-event_time b)) 
		       (<   (db:step-get-id a)        (db:step-get-id b)))
		      (else #f)))))
      res)))

(define (db:get-compressed-steps test-id #!key (work-area #f)(tdb #f))
  (if (or (not work-area)
	  (file-exists? (conc work-area "/testdat.db")))
      (let* ((comprsteps (open-run-close db:get-steps-table tdb test-id work-area: work-area)))
	(map (lambda (x)
	       ;; take advantage of the \n on time->string
	       (vector
		(vector-ref x 0)
		(let ((s (vector-ref x 1)))
		  (if (number? s)(seconds->time-string s) s))
		(let ((s (vector-ref x 2)))
		  (if (number? s)(seconds->time-string s) s))
		(vector-ref x 3)    ;; status
		(vector-ref x 4)
		(vector-ref x 5)))  ;; time delta
	     (sort (hash-table-values comprsteps)
		   (lambda (a b)
		     (let ((time-a (vector-ref a 1))
			   (time-b (vector-ref b 1)))
		       (if (and (number? time-a)(number? time-b))
			   (if (< time-a time-b)
			       #t
			       (if (eq? time-a time-b)
				   (string<? (conc (vector-ref a 2))
					     (conc (vector-ref b 2)))
				   #f))
			   (string<? (conc time-a)(conc time-b))))))))
      '()))

;;======================================================================
;; M I S C   M A N A G E M E N T   I T E M S 
;;======================================================================

;; A routine to map itempaths using a itemmap
(define (db:compare-itempaths patha pathb itemmap)
  (debug:print-info 3 "ITEMMAP is " itemmap)
  (if itemmap
      (let* ((mapparts    (string-split itemmap))
	     (pattern     (car mapparts))
	     (replacement (if (> (length mapparts) 1) (cadr mapparts) "")))
	(if replacement
	    (equal? (string-substitute pattern replacement patha)
		    (string-substitute pattern replacement pathb))
	    (equal? (string-substitute pattern "" patha)
		    (string-substitute pattern "" pathb))))
      (equal? patha pathb)))

;; the new prereqs calculation, looks also at itempath if specified
;; all prereqs must be met:
;;    if prereq test with itempath='' is COMPLETED and PASS, WARN, CHECK, or WAIVED then prereq is met
;;    if prereq test with itempath=ref-item-path and COMPLETED with PASS, WARN, CHECK, or WAIVED then prereq is met
;;
;; Note: mode 'normal means that tests must be COMPLETED and ok (i.e. PASS, WARN, CHECK, SKIP or WAIVED)
;;       mode 'toplevel means that tests must be COMPLETED only
;;       mode 'itemmatch or 'itemwait means that tests items must be COMPLETED and (PASS|WARN|WAIVED|CHECK) [[ NB// NOT IMPLEMENTED YET ]]
;; 
(define (db:get-prereqs-not-met run-id waitons ref-item-path #!key (mode '(normal))(itemmap #f))
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   (let ((tests             (cdb:remote-run db:get-tests-for-run-state-status #f run-id waitontest-name)) ;; (mt:get-tests-for-run run-id waitontest-name '() '()))
		 (ever-seen         #f)
		 (parent-waiton-met #f)
		 (item-waiton-met   #f))
	     (for-each 
	      (lambda (test)
		;; (if (equal? waitontest-name (db:test-get-testname test)) ;; by defintion this had better be true ...
		(let* ((state             (db:test-get-state test))
		       (status            (db:test-get-status test))
		       (item-path         (db:test-get-item-path test))
		       (is-completed      (equal? state "COMPLETED"))
		       (is-running        (equal? state "RUNNING"))
		       (is-killed         (equal? state "KILLED"))
		       (is-ok             (member status '("PASS" "WARN" "CHECK" "WAIVED" "SKIP")))
		       (same-itempath     (db:compare-itempaths ref-item-path item-path itemmap))) ;; (equal? ref-item-path item-path)))
		  (set! ever-seen #t)
		  (cond
		   ;; case 1, non-item (parent test) is 
		   ((and (equal? item-path "") ;; this is the parent test of the waiton being examined
			 is-completed
			 (or is-ok (not (null? (lset-intersection eq? mode '(toplevel)))))) ;;  itemmatch itemwait))))))
		    (set! parent-waiton-met #t))
		   ;; Special case for toplevel and KILLED
		   ((and (equal? item-path "") ;; this is the parent test
			 is-killed
			 (member 'toplevel mode))
		    (set! parent-waiton-met #t))
		   ;; For itemwait mode IFF the previous matching item is good the set parent-waiton-met
		   ((and (not (null? (lset-intersection eq? mode '(itemmatch itemwait))))
			 ;; (not (equal? item-path "")) ;; this applies to both top level (to allow launching of next batch) and items
			 same-itempath)
		    (if (and is-completed is-ok)
			(set! item-waiton-met #t))
		    (if (and (equal? item-path "")
			     (or is-completed is-running));; this is the parent, set it to run if completed or running
			(set! parent-waiton-met #t)))
		   ;; normal checking of parent items, any parent or parent item not ok blocks running
		   ((and is-completed
			 (or is-ok 
			     (member 'toplevel mode))              ;; toplevel does not block on FAIL
			 (and is-ok (member 'itemmatch mode))) ;; itemmatch blocks on not ok
		    (set! item-waiton-met #t)))))
		tests)
	     ;; both requirements, parent and item-waiton must be met to NOT add item to
	     ;; prereq's not met list
	     (if (not (or parent-waiton-met item-waiton-met))
		 (set! result (append (if (null? tests) (list waitontest-name) tests) result)))
	     ;; if the test is not found then clearly the waiton is not met...
	     ;; (if (not ever-seen)(set! result (cons waitontest-name result)))))
	     (if (not ever-seen)
		 (set! result (append (if (null? tests)(list waitontest-name) tests) result)))))
	 waitons)
	(delete-duplicates result))))

(define (db:teststep-set-status! db test-id teststep-name state-in status-in comment logfile #!key (work-area #f))
  ;;                 db:open-test-db-by-test-id does cdb:remote-run
  (let* ((tdb       (db:open-test-db-by-test-id db test-id work-area: work-area))
	 (state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:execute 
	   tdb
	   "INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment,logfile) VALUES(?,?,?,?,?,?,?);"
	   test-id teststep-name state-in status-in (current-seconds) (if comment comment "") (if logfile logfile ""))
	  (sqlite3:finalize! tdb)
	  #t)
	#f)))

;;======================================================================
;; Extract ods file from the db
;;======================================================================

;; runspatt is a comma delimited list of run patterns
;; keypatt-alist must contain *all* keys with an associated pattern: '( ("KEY1" "%") .. )
(define (db:extract-ods-file db outputfile keypatt-alist runspatt pathmod)
  (let* ((keysstr  (string-intersperse (map car keypatt-alist) ","))
	 (keyqry   (string-intersperse (map (lambda (p)(conc (car p) " LIKE ? ")) keypatt-alist) " AND "))
	 (numkeys  (length keypatt-alist))
	 (test-ids '())
	 (windows  (and pathmod (substring-index "\\" pathmod)))
	 (tempdir  (conc "/tmp/" (current-user-name) "/" runspatt "_" (random 10000) "_" (current-process-id)))
	 (runsheader (append (list "Run Id" "Runname") ; 0 1
			     (map car keypatt-alist)   ; + N = length keypatt-alist
			     (list "Testname"          ; 2
				   "Item Path"         ; 3 
				   "Description"       ; 4 
				   "State"             ; 5 
				   "Status"            ; 6  
				   "Final Log"         ; 7 
				   "Run Duration"      ; 8 
				   "When Run"          ; 9 
				   "Tags"              ; 10
				   "Run Owner"         ; 11
				   "Comment"           ; 12
				   "Author"            ; 13
				   "Test Owner"        ; 14
				   "Reviewed"          ; 15
				   "Diskfree"          ; 16
				   "Uname"             ; 17
				   "Rundir"            ; 18
				   "Host"              ; 19
				   "Cpu Load"          ; 20
				   )))
	 (results (list runsheader))			 
	 (testdata-header (list "Run Id" "Testname" "Item Path" "Category" "Variable" "Value" "Expected" "Tol" "Units" "Status" "Comment"))
	 (mainqry (conc "SELECT
              t.testname,r.id,runname," keysstr ",t.testname,
              t.item_path,tm.description,t.state,t.status,
              final_logf,run_duration, 
              strftime('%m/%d/%Y %H:%M:%S',datetime(t.event_time,'unixepoch'),'localtime'),
              tm.tags,r.owner,t.comment,
              author,
              tm.owner,reviewed,
              diskfree,uname,rundir,
              host,cpuload
            FROM tests AS t JOIN runs AS r ON t.run_id=r.id JOIN test_meta AS tm ON tm.testname=t.testname
            WHERE runname LIKE ? AND " keyqry ";")))
    (debug:print 2 "Using " tempdir " for constructing the ods file. keyqry: " keyqry " keystr: " keysstr " with keys: " (map cadr keypatt-alist)
		 "\n      mainqry: " mainqry)
    ;; "Expected Value"
    ;; "Value Found"
    ;; "Tolerance"
    (apply sqlite3:for-each-row
	   (lambda (test-id . b)
	     (set! test-ids (cons test-id test-ids))   ;; test-id is now testname
	     (set! results (append results ;; note, drop the test-id
				   (list
				    (if pathmod
					(let* ((vb        (apply vector b))
					       (keyvals   (let loop ((i    0)
								     (res '()))
							    (if (>= i numkeys)
								res
								(loop (+ i 1)
								      (append res (list (vector-ref vb (+ i 2))))))))
					       (runname   (vector-ref vb 1))
					       (testname  (vector-ref vb (+  2 numkeys)))
					       (item-path (vector-ref vb (+  3 numkeys)))
					       (final-log (vector-ref vb (+  7 numkeys)))
					       (run-dir   (vector-ref vb (+ 18 numkeys)))
					       (log-fpath (conc run-dir "/"  final-log))) ;; (string-intersperse keyvals "/") "/" testname "/" item-path "/"
					  (debug:print 4 "log: " log-fpath " exists: " (file-exists? log-fpath))
					  (vector-set! vb (+ 7 numkeys) (if (file-exists? log-fpath)
									    (let ((newpath (conc pathmod "/"
												 (string-intersperse keyvals "/")
												 "/" runname "/" testname "/"
												 (if (string=? item-path "") "" (conc "/" item-path))
												 final-log)))
									      ;; for now throw away newpath and use the log-fpath conc'd with pathmod
									      (set! newpath (conc pathmod log-fpath))
									      (if windows (string-translate newpath "/" "\\") newpath))
									    (if (debug:debug-mode 1)
										(conc final-log " not-found")
										"")))
					  (vector->list vb))
					b)))))
	   db
	   mainqry
	   runspatt (map cadr keypatt-alist))
    (debug:print 2 "Found " (length test-ids) " records")
    (set! results (list (cons "Runs" results)))
    ;; now, for each test, collect the test_data info and add a new sheet
    (for-each
     (lambda (test-id)
       (let ((test-data (list testdata-header))
	     (curr-test-name #f))
	 (sqlite3:for-each-row
	  (lambda (run-id testname item-path category variable value expected tol units status comment)
	    (set! curr-test-name testname)
	    (set! test-data (append test-data (list (list run-id testname item-path category variable value expected tol units status comment)))))
	  db 
	  ;; "SELECT run_id,testname,item_path,category,variable,td.value AS value,expected,tol,units,td.status AS status,td.comment AS comment FROM test_data AS td INNER JOIN tests ON tests.id=td.test_id WHERE test_id=?;"
	  "SELECT run_id,testname,item_path,category,variable,td.value AS value,td.expected,td.tol,td.units,td.status AS status,td.comment AS comment FROM test_data AS td INNER JOIN tests ON tests.id=td.test_id WHERE testname=?;"
	  test-id)
	 (if curr-test-name
	     (set! results (append results (list (cons curr-test-name test-data)))))
	 ))
     (sort (delete-duplicates test-ids) string<=))
    (system (conc "mkdir -p " tempdir))
    ;; (pp results)
    (ods:list->ods 
     tempdir
     (if (string-match (regexp "^[/~]+.*") outputfile) ;; full path?
	 outputfile
	 (begin
	   (debug:print 0 "WARNING: path given, " outputfile " is relative, prefixing with current directory")
	   (conc (current-directory) "/" outputfile)))
     results)
    ;; brutal clean up
    (system "rm -rf tempdir")))

;; (db:extract-ods-file db "outputfile.ods" '(("sysname" "%")("fsname" "%")("datapath" "%")) "%")

;; This is a list of all procs that write to the db
;;
(define *db:all-write-procs*
  (list 
   db:set-var 
   db:del-var
   db:register-run
   db:set-comment-for-run
   db:delete-run
   db:update-run-event_time
   db:lock/unlock-run 
   db:delete-test-step-records
   db:delete-test-records
   db:delete-tests-for-run
   db:delete-old-deleted-test-records
   db:set-tests-state-status
   db:test-set-state-status-by-id
   db:test-set-state-status-by-run-id-testname
   db:test-set-comment
   db:testmeta-add-record
   db:csv->test-data
   db:test-data-rollup
   db:teststep-set-status! ))

