;;======================================================================
;; Copyright 2006-2011, Matthew Welland.
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

(require-extension (srfi 18) extras tcp rpc)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml)
(import (prefix sqlite3 sqlite3:))

(declare (unit db))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

;; timestamp type (val1 val2 ...)
;; type: meta-info, step
(define *incoming-data*      '())
(define *incoming-last-time* (current-seconds))
(define *incoming-mutex*     (make-mutex))
(define *cache-on* #f)

(define (open-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((dbpath    (conc *toppath* "/megatest.db")) ;; fname)
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler   (make-busy-timeout 3600))) ;; 136000)))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(db:initialize db))
    db))

(define (db:initialize db)
  (let* ((configdat (car *configinfo*))  ;; tut tut, global warning...
	 (keys     (config-get-fields configdat))
	 (havekeys (> (length keys) 0))
	 (keystr   (keys->keystr keys))
	 (fieldstr (keys->key/field keys)))
    (for-each (lambda (key)
		(let ((keyn (vector-ref key 0)))
		  (if (member (string-downcase keyn)
			      (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count"
				    "pass_count"))
		      (begin
			(print "ERROR: your key cannot be named " keyn " as this conflicts with the same named field in the runs table")
			(system (conc "rm -f " dbpath))
			(exit 1)))))
	      keys)
    ;; (sqlite3:execute db "PRAGMA synchronous = OFF;")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS keys (id INTEGER PRIMARY KEY, fieldname TEXT, fieldtype TEXT, CONSTRAINT keyconstraint UNIQUE (fieldname));")
    (for-each (lambda (key)
		(sqlite3:execute db "INSERT INTO keys (fieldname,fieldtype) VALUES (?,?);" (key:get-fieldname key)(key:get-fieldtype key)))
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
    (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname);")
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
    ))

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
;; meta get and set vars
;;======================================================================

;; returns number if string->number is successful, string otherwise
(define (db:get-var db var)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     db "SELECT val FROM metadat WHERE var=?;" var)
    (if (string? res)
	(let ((valnum (string->number res)))
	  (if valnum valnum res))
	res)))

(define (db:set-var db var val)
  (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val))

;; use a global for some primitive caching, it is just silly to re-read the db 
;; over and over again for the keys since they never change

(define *db-keys* #f)

(define (db:get-keys db)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(sqlite3:for-each-row 
	 (lambda (key keytype)
	   (set! res (cons (vector key keytype) res)))
	 db
	 "SELECT fieldname,fieldtype FROM keys ORDER BY id DESC;")
	(set! *db-keys* res)
	res)))

(define (db:get-value-by-header row header field)
  ;; (debug:print 2 "db:get-value-by-header row: " row " header: " header " field: " field)
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

(define (runs:get-std-run-fields keys remfields)
  (let* ((header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (list keystr header)))

;; WAS db-get-runs FIXME IN REMAINING CODE
;;
;; MERGE THIS WITH db:get-runs, accidently wrote it twice
;;
;; replace header and keystr with a call to runs:get-std-run-fields
;;
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;;
(define (db:get-runs db runpatt count offset keypatts)
  (let* ((res      '())
	 (keys      (db:get-keys db))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ",")))
	 (qrystr    (conc "SELECT " keystr " FROM runs WHERE runname LIKE ? "
			  ;; Generate: " AND x LIKE 'keypatt' ..."
			  (if (null? keypatts) ""
			      (conc " AND "
				    (string-join 
				     (map (lambda (keypatt)
					    (let ((key  (car keypatt))
						  (patt (cadr keypatt)))
					      (conc key " LIKE '" patt "'")))
					  keypatts)
				     " AND ")))
			  " ORDER BY event_time DESC "
			  (if (number? count)
			      (conc " LIMIT " count)
			      "")
			  (if (number? offset)
			      (conc " OFFSET " offset)
			      ""))))
    (debug:print 8 "INFO: db:get-runs qrystr: " qrystr "\nkeypatts: " keypatts "\n  offset: " offset " limit: " count)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (cons (apply vector a x) res)))
     db
     qrystr
     runpatt)
    (vector header res)))

;; just get count of runs
(define (db:get-num-runs db runpatt)
  (let ((numruns 0))
    (sqlite3:for-each-row 
     (lambda (count)
       (set! numruns count))
     db
     "SELECT COUNT(id) FROM runs WHERE runname LIKE ?;" runpatt)
    numruns))

;; use (get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
(define (db:get-run-info db run-id)
  (let* ((res      #f)
	 (keys      (db:get-keys db))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    ;; (debug:print 0 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (apply vector a x)))
     db
     (conc "SELECT " keystr " FROM runs WHERE id=?;")
     run-id)
    (vector header res)))

(define (db:set-comment-for-run db run-id comment)
  (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment run-id))

;; does not (obviously!) removed dependent data. 
(define (db:delete-run db run-id)
  (sqlite3:execute db "DELETE FROM runs WHERE id=?;" run-id))

(define (db:update-run-event_time db run-id)
  (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id)) 

;;======================================================================
;; K E Y S
;;======================================================================

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (db:get-key-val-pairs db run-id)
  (let* ((keys (get-keys db))
	 (res  '()))
    (debug:print 6 "keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list (key:get-fieldname key) key-val) res)))
	  db qry run-id)))
     keys)
    (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals db run-id)
  (let* ((keys (get-keys db))
	 (res  '()))
    (debug:print 6 "keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons key-val res)))
	  db qry run-id)))
     keys)
    (reverse res)))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
(define (db:get-tests-for-run db run-id testpatt itempatt states statuses)
  (let* ((res '())
	 (states-str    (conc "('" (string-intersperse states   "','") "')"))
	 (statuses-str  (conc "('" (string-intersperse statuses "','") "')"))
	 (qry      (conc "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment "
			 " FROM tests WHERE run_id=? AND testname like ? AND item_path LIKE ? " 
			 " AND NOT (state in " states-str " AND status IN " statuses-str ") "
			 ;; " ORDER BY id DESC;"
			 " ORDER BY event_time ASC;" ;; POTENTIAL ISSUE! CHECK ME! Does anyting depend on this being sorted by id?
			 )))
    (debug:print 8 "INFO: db:get-tests-for-run qry=" qry)
    (sqlite3:for-each-row 
     (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
       (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
     db 
     qry
     run-id
     (if testpatt testpatt "%")
     (if itempatt itempatt "%"))
    res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records db run-id test-name itemdat)
  ;; Breaking it into two queries for better file access interleaving
  (let ((ids '()))
    (sqlite3:for-each-row (lambda (id)
			    (set! ids (cons id ids)))
			  db
			  "SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
			  run-id test-name (item-list->path itemdat))
    (for-each (lambda (id)
		(sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" id)
		(thread-sleep! 0.1) ;; give others access to the db
                (sqlite3:execute db "DELETE FROM test_data WHERE test_id=?;" id)
                (thread-sleep! 0.1)) ;; give others access to the db
	      ids)))
;;"DELETE FROM test_steps WHERE test_id in (SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?);" 
		   
;; 
(define (db:delete-test-records db test-id)
  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" test-id)
  (sqlite3:execute db "DELETE FROM test_data  WHERE test_id=?;" test-id)
  (sqlite3:execute db "DELETE FROM tests WHERE id=?;" test-id))

;; set tests with state currstate and status currstatus to newstate and newstatus
;; use currstate = #f and or currstatus = #f to apply to any state or status respectively
;; WARNING: SQL injection risk
(define (db:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)
  (for-each (lambda (testname)
	      (let ((qry (conc "UPDATE tests SET state=?,status=? WHERE "
			       (if currstate  (conc "state='" currstate "' AND ") "")
			       (if currstatus (conc "status='" currstatus "' AND ") "")
			       " run_id=? AND testname=? AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
		;;(debug:print 0 "QRY: " qry)
		(sqlite3:execute db qry run-id newstate newstatus testname testname)))
	    testnames))

(define (db:delete-tests-in-state db run-id state)
  (sqlite3:execute db "DELETE FROM tests WHERE state=? AND run_id=?;" state run-id))

(define (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)
  (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
  (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
  (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment test-id)))

(define (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)
  (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
		   state status run-id test-name item-path))

(define (db:get-count-tests-running db)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART';")
    res))

(define (db:get-count-tests-running-in-jobgroup db jobgroup)
  (if (not jobgroup)
      0 ;; 
      (let ((res 0))
	(sqlite3:for-each-row
	 (lambda (count)
	   (set! res count))
	 db
	 "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART'
             AND testname in (SELECT testname FROM test_meta WHERE jobgroup=?;"
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

;; NB// Sync this with runs:get-test-info
(define (db:get-test-info db run-id testname item-path)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
     run-id testname item-path)
    res))

;; Get test data using test_id
(define (db:get-test-data-by-id db test-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE id=?;"
     test-id)
    res))


(define (db:test-set-comment db run-id test-name item-path comment)
  (sqlite3:execute 
   db 
   "UPDATE tests SET comment=? WHERE run_id=? AND testname=? AND item_path=?;"
   comment run-id test-name item-path))

;;
(define (db:test-set-rundir! db run-id test-name item-path rundir)
  (sqlite3:execute 
   db 
   "UPDATE tests SET rundir=? WHERE run_id=? AND testname=? AND item_path=?;"
   rundir run-id test-name item-path))

(define (db:test-set-log! db run-id test-name item-path logf)
  (if (string? logf)
      (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path=?;" 
		   logf run-id test-name item-path)
      (debug:print 0 "ERROR: db:test-set-log! called with non-string log file name " logf)))

;;======================================================================
;; Misc. test related queries
;;======================================================================

(define (db:test-get-paths-matching db keynames target)
  (let* ((res '())
	 (itempatt   (if (args:get-arg "-itempatt")(args:get-arg "-itempatt") "%"))
	 (testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc "r." key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 (qrystr (conc "SELECT t.rundir FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id WHERE "
		       keystr " AND r.runname LIKE '" runname "' AND item_path LIKE '" itempatt "' AND testname LIKE '"
		       testpatt "' AND t.state LIKE '" statepatt "' AND t.status LIKE '" statuspatt 
		       "'ORDER BY t.event_time ASC;")))
    (debug:print 3 "qrystr: " qrystr)
    (sqlite3:for-each-row 
     (lambda (p)
       (set! res (cons p res)))
     db 
     qrystr)
    res))

(define (db:test-get-test-records-matching db keynames target)
  (let* ((res '())
	 (itempatt   (if (args:get-arg "-itempatt")(args:get-arg "-itempatt") "%"))
	 (testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc "r." key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 (qrystr (conc "SELECT 
                            t.id
                            t.run_id     
                            t.testname   
                            t.host       
                            t.cpuload    
                            t.diskfree   
                            t.uname      
                            t.rundir     
                            t.shortdir   
                            t.item_path  
                            t.state      
                            t.status     
                            t.attemptnum 
                            t.final_logf 
                            t.logdat     
                            t.run_duratio
                            t.comment    
                            t.event_time 
                            t.fail_count 
                            t.pass_count 
                            t.archived   



 FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id WHERE "
		       keystr " AND r.runname LIKE '" runname "' AND item_path LIKE '" itempatt "' AND testname LIKE '"
		       testpatt "' AND t.state LIKE '" statepatt "' AND t.status LIKE '" statuspatt 
		       "'ORDER BY t.event_time ASC;")))
    (debug:print 3 "qrystr: " qrystr)
    (sqlite3:for-each-row 
     (lambda (p)
       (set! res (cons p res)))
     db 
     qrystr)
    res))

;;======================================================================
;; QUEUE UP META, TEST STATUS AND STEPS
;;======================================================================

(define (db:updater db)
  (let loop ((start-time (current-time)))
    (thread-sleep! 0.5) ;; move save time around to minimize regular collisions?
    (db:write-cached-data db)
    (loop start-time)))
    
(define (db:test-update-meta-info db run-id test-name item-path minutes cpuload diskfree tmpfree)
  (if (not item-path)
      (begin (debug:print 0 "WARNING: ITEMPATH not set.")   
	     (set! item-path "")))
  (mutex-lock! *incoming-mutex*)
  (set! *incoming-data* (cons (vector 'meta-info
				      (current-seconds)
				      (list cpuload
					    diskfree
					    minutes
					    run-id
					    test-name
					    item-path)) ;; run-id test-name item-path minutes cpuload diskfree tmpfree) 
			      *incoming-data*))
  (mutex-unlock! *incoming-mutex*)
  (if (not *cache-on*)(db:write-cached-data db)))

(define (db:write-cached-data db)
  (let ((meta-stmt (sqlite3:prepare db "UPDATE tests SET cpuload=?,diskfree=?,run_duration=?,state='RUNNING' WHERE run_id=? AND testname=? AND item_path=? AND state NOT IN ('COMPLETED','KILLREQ','KILLED');"))
	(step-stmt (sqlite3:prepare db "INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment,logfile) VALUES(?,?,?,?,?,?,?);")) ;; strftime('%s','now')#f)
	(data (sort *incoming-data* (lambda (a b)(< (vector-ref a 1)(vector-ref b 1))))))
    (if (> (length data) 0)
	(debug:print 4 "Writing cached data " data))
    (mutex-lock! *incoming-mutex*)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (for-each (lambda (entry)
		   (case (vector-ref entry 0)
		     ((meta-info)
		      (apply sqlite3:execute meta-stmt (vector-ref entry 2)))
		     ((step-status)
		      (apply sqlite3:execute step-stmt (vector-ref entry 2)))
		     (else
		      (debug:print 0 "ERROR: Queued entry not recognised " entry))))
		 data)))
    (set! *incoming-data* '())
    (mutex-unlock! *incoming-mutex*)
    (sqlite3:finalize! meta-stmt)
    (sqlite3:finalize! step-stmt)))

(define (db:roll-up-pass-fail-counts db run-id test-name item-path status)
  (if (and (not (equal? item-path ""))
	   (or (equal? status "PASS")
	       (equal? status "WARN")
	       (equal? status "FAIL")
	       (equal? status "WAIVED")
	       (equal? status "RUNNING")))
      (begin
	(sqlite3:execute 
	 db
	 "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status='FAIL'),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND (status='PASS' OR status='WARN' OR status='WAIVED'))
             WHERE run_id=? AND testname=? AND item_path='';"
	 run-id test-name run-id test-name run-id test-name)
	(if (equal? status "RUNNING") ;; running takes priority over all other states, force the test state to RUNNING
	    (sqlite3:execute db "UPDATE tests SET state=? WHERE run_id=? AND testname=? AND item_path='';" "RUNNING" run-id test-name)
	    (sqlite3:execute
	     db
	     "UPDATE tests
                       SET state=CASE WHEN (SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND state in ('RUNNING','NOT_STARTED')) > 0 THEN 
                          'RUNNING'
                       ELSE 'COMPLETED' END,
                          status=CASE WHEN fail_count > 0 THEN 'FAIL' WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' ELSE 'UNKNOWN' END
                       WHERE run_id=? AND testname=? AND item_path='';"
	     run-id test-name run-id test-name)))))


;;======================================================================
;; Tests meta data
;;======================================================================

;; read the record given a testname
(define (db:testmeta-get-record db testname)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id testname author owner description reviewed iterated avg_runtime avg_disk tags)
       (set! res (vector id testname author owner description reviewed iterated avg_runtime avg_disk tags)))
     db "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags FROM test_meta WHERE testname=?;"
     testname)
    res))

;; create a new record for a given testname
(define (db:testmeta-add-record db testname)
  (sqlite3:execute db "INSERT OR IGNORE INTO test_meta (testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags) VALUES (?,'','','','','','','','');" testname))

;; update one of the testmeta fields
(define (db:testmeta-update-field db testname field value)
  (sqlite3:execute db (conc "UPDATE test_meta SET " field "=? WHERE testname=?;") value testname))

;;======================================================================
;; T E S T   D A T A 
;;======================================================================

(define (db:csv->test-data db test-id csvdata)
  (debug:print 4 "test-id " test-id ", csvdata: " csvdata)
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
	 (sqlite3:execute db "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status,type) VALUES (?,?,?,?,?,?,?,?,?,?);"
			  test-id category variable value expected tol units (if comment comment "") status type)))
     csvlist)))

;; get a list of test_data records matching categorypatt
(define (db:read-test-data db test-id categorypatt)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status type)
       (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
     db
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (reverse res)))

(define (db:load-test-data db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat (rdb:get-test-info db run-id test-name item-path))
	 (test-id (if testdat (db:test-get-id testdat) #f)))
    ;; (debug:print 1 "Enter records to insert in the test_data table, seven fields, comma separated per line")
    (debug:print 4 "itemdat: " itemdat ", test-name: " test-name ", test-id: " test-id)
    (if test-id
	(let loop ((lin (read-line)))
	  (if (not (eof-object? lin))
	      (begin
		(debug:print 4 lin)
		(rdb:csv->test-data db test-id lin)
		(loop (read-line))))))
    ;; roll up the current results.
    ;; FIXME: Add the status to 
    (rdb:test-data-rollup db test-id #f)))

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup db test-id status)
  (sqlite3:execute 
   db 
   "UPDATE tests 
      SET fail_count=(SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail'),
          pass_count=(SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass')
      WHERE id=?;"
   test-id test-id test-id)
  ;; if the test is not FAIL then set status based on the fail and pass counts.
  (thread-sleep! 1)
  (sqlite3:execute
   db   ;;; NOTE: Should this be WARN,FAIL? A WARN is not a FAIL????? BUG FIXME
   "UPDATE tests
      SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                         THEN 'FAIL'
                      WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
                           (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
                         THEN 'PASS'
                      ELSE status
                  END WHERE id=?;"
   test-id test-id test-id test-id))

(define (db:get-prev-tol-for-test db test-id category variable)
  ;; Finish me?
  (values #f #f #f))

;;======================================================================
;; S T E P S 
;;======================================================================

(define (db:step-get-time-as-string vec)
  (seconds->time-string (db:step-get-event_time vec)))

;; db-get-test-steps-for-run
(define (db:get-steps-for-test db test-id)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     db
     "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (reverse res)))

;; get a pretty table to summarize steps
;;
(define (db:get-steps-table db test-id)
  (let ((steps   (db:get-steps-for-test db test-id)))
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
       (sort steps (lambda (a b)(< (db:step-get-event_time a)(db:step-get-event_time b)))))
      res)))

;; the new prereqs calculation, looks also at itempath if specified
;; all prereqs must be met:
;;    if prereq test with itempath='' is COMPLETED and PASS, WARN, CHECK, or WAIVED then prereq is met
;;    if prereq test with itempath=ref-item-path and COMPLETED with PASS, WARN, CHECK, or WAIVED then prereq is met
;;
;; Note: do not convert to remote as it calls remote under the hood
;;
(define (db:get-prereqs-not-met db run-id waitons ref-item-path)
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   (let ((tests             (rdb:get-tests-for-run db run-id waitontest-name #f '() '()))
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
		       (is-ok             (member status '("PASS" "WARN" "CHECK" "WAIVED")))
		       (same-itempath     (equal? ref-item-path item-path)))
		  (set! ever-seen #t)
		  (cond
		   ;; case 1, non-item (parent test) is 
		   ((and (equal? item-path "") ;; this is the parent test
			 is-completed
			 is-ok)
		    (set! parent-waiton-met #t))
		   ((and same-itempath
			 is-completed
			 is-ok)
		    (set! item-waiton-met #t)))))
	      tests)
	     (if (not (or parent-waiton-met item-waiton-met))
		 (set! result (cons waitontest-name result)))
	     ;; if the test is not found then clearly the waiton is not met...
	     (if (not ever-seen)(set! result (cons waitontest-name result)))))
	waitons)
      (delete-duplicates result))))

(define (db:teststep-set-status! db run-id test-name teststep-name state-in status-in item-path comment logfile)
  (debug:print 4 "run-id: " run-id " test-name: " test-name)
  (let* ((state     (check-valid-items "state" state-in))
	 (status    (check-valid-items "status" status-in))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    (debug:print 5 "testdat: " testdat)
    (if (and testdat ;; if the section exists then force specification BUG, I don't like how this works.
	     (or (not state)(not status)))
	(debug:print 0 "WARNING: Invalid " (if status "status" "state")
	       " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (if testdat
	(let ((test-id (test:get-id testdat)))
	  (mutex-lock! *incoming-mutex*)
	  (set! *incoming-data* (cons (vector 'step-status
					      (current-seconds)
					      ;; FIXME - this should not update the logfile unless it is specified.
					      (list test-id teststep-name state-in status-in (current-seconds) (if comment comment "") (if logfile logfile "")))
				      *incoming-data*))
	  (mutex-unlock! *incoming-mutex*)
	  (if (not *cache-on*)(db:write-cached-data db))
	  #t)
	(debug:print 0 "ERROR: Can't update " test-name " for run " run-id " -> no such test in db"))))

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
									    (if (> *verbosity* 1)
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


;;======================================================================
;; REMOTE DB ACCESS VIA RPC
;;======================================================================

(define (rdb:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:set-tests-state-status host port)
	 run-id testnames currstate currstatus newstate newstatus))
      (db:set-tests-state-status db run-id testnames currstate currstatus newstate newstatus)))

(define (rdb:teststep-set-status! db run-id test-name teststep-name state-in status-in itemdat comment logfile)
  (let ((item-path (item-list->path itemdat)))
    (if *runremote*
	(let ((host (vector-ref *runremote* 0))
	      (port (vector-ref *runremote* 1)))
	  ((rpc:procedure 'rdb:teststep-set-status! host port)
	   run-id test-name teststep-name state-in status-in item-path comment logfile))
	(db:teststep-set-status! db run-id test-name teststep-name state-in status-in item-path comment logfile))))

(define (rdb:test-update-meta-info db run-id test-name itemdat minutes cpuload diskfree tmpfree)
  (let ((item-path (item-list->path itemdat)))
    (if *runremote*
	(let ((host (vector-ref *runremote* 0))
	      (port (vector-ref *runremote* 1)))
	  ((rpc:procedure 'rdb:test-update-meta-info host port)
	   run-id test-name item-path minutes cpuload diskfree tmpfree))
	(db:test-update-meta-info db run-id test-name item-path minutes cpuload diskfree tmpfree))))

(define (rdb:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:test-set-state-status-by-run-id-testname host port)
	  run-id test-name item-path status state))
      (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)))

(define (rdb:csv->test-data db test-id csvdata)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:csv->test-data host port)
	 test-id csvdata))
      (db:csv->test-data db test-id csvdata)))

(define (rdb:roll-up-pass-fail-counts db run-id test-name item-path status)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:roll-up-pass-fail-counts host port)
	 run-id test-name item-path status))
      (db:roll-up-pass-fail-counts db run-id test-name item-path status)))

(define (rdb:test-set-comment db run-id test-name item-path comment)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:test-set-comment host port)
	 run-id test-name item-path comment))
      (db:test-set-comment db run-id test-name item-path comment)))

(define (rdb:test-set-log! db run-id test-name item-path logf)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:test-set-log! host port) run-id test-name item-path logf))
      (db:test-set-log! db run-id test-name item-path logf)))

(define (rdb:get-runs db runnamepatt numruns startrunoffset keypatts)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-runs host port)
	 runnamepatt numruns startrunoffset keypatts))
      (db:get-runs db runnamepatt numruns startrunoffset keypatts)))

(define (rdb:get-tests-for-run db run-id testpatt itempatt states statuses)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-tests-for-run host port)
	  run-id testpatt itempatt states statuses))
      (db:get-tests-for-run db run-id testpatt itempatt states statuses)))

(define (rdb:get-test-data-by-id db test-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rpc:get-test-data-by-id host port)
	 test-id))
      (db:get-test-data-by-id db test-id)))
      
(define (rdb:get-keys db)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	(if *db-keys* *db-keys* 
	    (let ((keys ((rpc:procedure 'rdb:get-keys host port))))
	      (set! *db-keys* keys)
	      keys)))
      (db:get-keys db)))
	 
(define (rdb:get-num-runs db runpatt)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-num-runs host port) runpatt))
      (db:get-num-runs db runpatt)))

(define (rdb:test-set-state-status-by-id db test-id newstate newstatus newcomment)
    (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:test-set-state-status-by-id host port)
	 test-id newstate newstatus newcomment))
      (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)))

(define (rdb:get-key-val-pairs db run-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-key-val-pairs host port) run-id))
      (db:get-key-val-pairs db run-id)))
	 
(define (rdb:get-key-vals db run-id)
    (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-key-vals host port) run-id))
      (db:get-key-vals db run-id)))

(define (rdb:testmeta-get-record db testname)
   (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:testmeta-get-record host port) testname))
      (db:testmeta-get-record db testname)))

(define (rdb:get-test-data-by-id db test-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-test-data-by-id host port) test-id))
      (db:get-test-data-by-id db test-id)))

(define (rdb:get-run-info db run-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-run-info host port) run-id))
      (db:get-run-info db run-id)))

(define (rdb:get-steps-for-test db test-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-steps-for-test host port) test-id))
      (db:get-steps-for-test db test-id)))

(define (rdb:get-steps-table db test-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-steps-table host port) test-id))
      (db:get-steps-table db test-id)))

(define (rdb:read-test-data db test-id categorypatt)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:read-test-data host port) test-id categorypatt))
      (db:read-test-data db test-id categorypatt)))

(define (rdb:get-test-info db run-id testname item-path)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:get-test-info host port) run-id testname item-path))
      (db:get-test-info db run-id testname item-path)))

(define (rdb:delete-test-records db test-id)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:delete-test-records host port) test-id))
      (db:delete-test-records db test-id)))

(define (rdb:test-data-rollup db test-id status)
    (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rdb:test-data-rollup host port) test-id status))
      (db:test-data-rollup db test-id status)))
