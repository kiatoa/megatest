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
(declare (uses filedb))

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
  (let* ((dbpath    (conc *toppath* "/megatest.db")) ;; fname)
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (write-access (file-write-access? dbpath))
	 (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					   (string->number (args:get-arg "-override-timeout"))
					   136000)))) ;; 136000))) ;; 136000 = 2.2 minutes
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

(define (open-in-mem-db)
  (let* ((path   (configf:lookup *configdat* "setup" "tmpdb"))
	 (fname  (if path (conc path "/temp-megatest.db") #f))
	 (exists (and path (file-exists? fname)))
	 (db     (if path
		     (begin
		       (create-directory path #t)
		       (sqlite3:open-database fname))
		     (sqlite3:open-database ":memory:")))
	 (handler   (make-busy-timeout 3600)))
    (if (or (not path)
	    (not exists))
	(db:initialize db))
    (sqlite3:set-busy-handler! db handler)
    db))

;; (define (db:sync-table tblname fields fromdb todb)

(define (db:tbls db)
  (let ((keys  (db:get-keys db)))
    (list
     (list "metadat" '("var" #f) '("val" #f))
     (append (list "runs" 
		   '("id"  #f))
	     (map (lambda (k)(list k #f))
		  (append keys
			  (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count" "pass_count")))))))

;; tbls is ( ("tablename" ( "field1" [#f|proc1] ) ( "field2" [#f|proc2] ) .... ) )
(define (db:sync-tables tbls fromdb todb)
  (let ((stmts      (make-hash-table)) ;; table-field => stmt
	(all-stmts  '()))              ;; ( ( stmt1 value1 ) ( stml2 value2 ))
    (for-each ;; table
     (lambda (tabledat)
       (let* ((tablename  (car tabledat))
	      (fields     (cdr tabledat))
	      (num-fields (length fields))
	      (field->num (make-hash-table))
	      (num->field (apply vector (map car fields)))
	      (full-sel   (conc "SELECT " (string-intersperse (map car fields) ",") 
				" FROM " tablename ";"))
	      (full-ins   (conc "INSERT OR REPLACE INTO " tablename " ( " (string-intersperse (map car fields) ",") " ) "
				" VALUES ( " (string-intersperse (make-list num-fields "?") ",") " );"))
	      (fromdat    '())
	      (todat      (make-hash-table))
	      (count      0))

	 ;; set up the field->num table
	 (for-each
	  (lambda (field)
	    (hash-table-set! field->num field count)
	    (set! count (+ count 1)))
	  fields)

	 ;; read the source table
	 (sqlite3:for-each-row
	  (lambda (a . b)
	    (set! fromdat (cons (apply vector a b) fromdat)))
	  fromdb
	  full-sel)

	 ;; read the target table
	 (sqlite3:for-each-row
	  (lambda (a . b)
	    (hash-table-set! todat a (apply vector a b)))
	  todb
	  full-sel)

	 ;; first pass implementation, just insert all changed rows
	 (let ((stmth (sqlite3:prepare todb full-ins)))
	   (sqlite3:with-transaction
	    todb
	    (lambda ()
	      (for-each ;; 
	       (lambda (fromrow)
		 (let* ((a    (vector-ref fromrow 0))
			(curr (hash-table-ref todat a))
			(same #t))
		   (let loop ((i 0))
		     (if (not (equal? (vector-ref fromrow i)(vector-ref curr i)))
			 (set! same #f))
		     (if (and same
			      (< i (- num-fields 1)))
			 (loop (+ i 1))))
		   (if (not same)(apply sqlite3:execute full-ins (vector->list fromrow)))))
	       fromdat)))
	   (sqlite3:finalize! stmth))))
     tbls)))
		   

(define (db:sync-to fromdb todb)
  ;; strategy
  ;;  1. Get all run-ids
  ;;  2. For each run-id 
  ;;     a. Sync that run in a transaction
  (let ((trecchgd    0)
	(rrecchgd    0)
	(tmrecchgd   0))

    ;; First sync test_meta data
    (let ((tmgetstmt (sqlite3:prepare todb "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta WHERE id=?;"))
	  (tmputstmt (sqlite3:prepare todb "INSERT OR REPLACE INTO test_meta (id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup) 
                                                                      VALUES (?, ?,       ?,     ?,    ?,          ?,       ?,       ?,          ?,       ?,   ?);"))
	  (tmdats    (db:testmeta-get-all fromdb)))
      ;; (debug:print 7 "Updating as many as " (length tdats) " records for run " run-id)
      (for-each
       (lambda (tmdat) ;; iterate over tests
	 (let ((testm-id (vector-ref tmdat 0)))
	   (sqlite3:with-transaction
	    todb
	    (lambda ()
	      (let ((curr-tmdat #f))
		(sqlite3:for-each-row
		 (lambda (a . b)
		   (set! curr-tmdat (apply vector a b)))
		 tmgetstmt testm-id)
		(if (not (equal? curr-tmdat tmdat)) ;; something changed
		    (begin
		      (debug:print 0 "  test-id: " testm-id
				   "\ncurr-tdat: " curr-tmdat
				   "\n     tdat: " tmdat)
		      (apply sqlite3:execute tmputstmt (vector->list tmdat))
		      (set! tmrecchgd (+ tmrecchgd 1)))))))))
       tmdats)
      (sqlite3:finalize! tmgetstmt)
      (sqlite3:finalize! tmputstmt))

    ;; First sync tests data
    (let ((run-ids     (db:get-all-run-ids fromdb))
	  (tgetstmt    (sqlite3:prepare todb "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE id=?;"))
	  (tputstmt    (sqlite3:prepare todb "INSERT OR REPLACE INTO tests  (id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment)
                                                                    VALUES (?, ?,     ?,       ?,    ?,     ?,         ?,   ?,      ?,       ?,    ?,     ?,        ?,           ?,         ?     );")))
      (for-each
       (lambda (run-id)
	 (let ((tdats     (db:get-all-tests-info-by-run-id fromdb run-id)))
	   ;; (debug:print 7 "Updating as many as " (length tdats) " records for run " run-id)
	   (for-each
	    (lambda (tdat) ;; iterate over tests
	      (let ((test-id (vector-ref tdat 0)))
		(sqlite3:with-transaction
		 todb
		 (lambda ()
		   (let ((curr-tdat #f))
		     (sqlite3:for-each-row
		      (lambda (a . b)
			(set! curr-tdat (apply vector a b)))
		      tgetstmt
		      test-id)
		     (if (not (equal? curr-tdat tdat)) ;; something changed
			 (begin
			   (debug:print 0 "  test-id: " test-id
					"\ncurr-tdat: " curr-tdat
					"\n     tdat: " tdat)
			   (apply sqlite3:execute tputstmt (vector->list tdat))
			   (set! trecchgd (+ trecchgd 1)))))))))
	    tdats)))
       run-ids)
      (sqlite3:finalize! tgetstmt)
      (sqlite3:finalize! tputstmt))

    ;; Next sync runs table
    (let* ((rdats       '())
	   (keys        (db:get-keys fromdb))
	   (rstdfields  (conc "id," (string-intersperse keys ",") ",runname,state,status,owner,event_time,comment,fail_count,pass_count"))
	   (rnumfields  (length (string-split rstdfields ",")))
	   (runslots    (string-intersperse (make-list rnumfields "?") ","))
	   (rgetstmt    (sqlite3:prepare todb (conc "SELECT " rstdfields " FROM runs WHERE id=?;")))
	   (rputstmt    (sqlite3:prepare todb (conc "INSERT OR REPLACE INTO runs (" rstdfields ") VALUES ( " runslots " );"))))
      ;; first collect all the source run data
      (sqlite3:for-each-row
       (lambda (a . b)
	 (set! rdats (cons (apply vector a b) rdats)))
       fromdb
       (conc "SELECT " rstdfields " FROM runs;"))
      (sqlite3:with-transaction
       todb
       (lambda ()
	 (for-each 
	  (lambda (rdat)
	    (let ((run-id    (vector-ref rdat 0))
		  (curr-rdat #f))
	      ;; first get the current value of the equivalent row from the target
	      ;; read, then insert/overwrite if different
	      (sqlite3:for-each-row 
	       (lambda (a . b)
		 (set! curr-rdat (apply vector a b)))
	       rgetstmt
	       run-id)
	      (if (not (equal? curr-rdat rdat))
		  (begin
		    (debug:print 0 "   run-id: " run-id
				 "\ncurr-rdat: " curr-rdat
				 "\n     rdat: " rdat)
		    (set! rrecchgd (+ rrecchgd 1))
		    (apply sqlite3:execute rputstmt (vector->list rdat))))))
	  rdats)))
      (sqlite3:finalize! rgetstmt)
      (sqlite3:finalize! rputstmt))

    (if (> rrecchgd 0)  (debug:print 0 "synced " rrecchgd " changed records in runs  table"))
    (if (> trecchgd 0)  (debug:print 0 "synced " trecchgd " changed records in tests table"))
    (if (> tmrecchgd 0) (debug:print 0 "sync'd " tmrecchgd " changed records in test_meta table"))
    (+ rrecchgd trecchgd tmrecchgd)))

(define (db:sync-back)
  (db:sync-to *inmemdb* *db*))

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
   (begin
     (debug:print 0 "EXCEPTION: database probably overloaded or unreadable.")
     (debug:print 0 "  " ((condition-property-accessor 'exn 'message) exn))
     (print-call-chain)
     (thread-sleep! (random 120))
     (debug:print-info 0 "trying db call one more time....")
     (apply open-run-close-no-exception-handling proc idb params))
   (apply open-run-close-no-exception-handling proc idb params)))

;; (define open-run-close open-run-close-exception-handling)
(define open-run-close open-run-close-no-exception-handling)

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
			 "runname    TEXT DEFAULT 'norun',"
			 "state      TEXT DEFAULT '',"
			 "status     TEXT DEFAULT '',"
			 "owner      TEXT DEFAULT '',"
			 "event_time TIMESTAMP DEFAULT (strftime('%s','now')),"
			 "comment    TEXT DEFAULT '',"
			 "fail_count INTEGER DEFAULT 0,"
			 "pass_count INTEGER DEFAULT 0,"
			 "CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
    (sqlite3:execute db (conc "CREATE INDEX runs_index ON runs (runname" (if havekeys "," "") keystr ");"))
    (sqlite3:execute db 
		     "CREATE TABLE IF NOT EXISTS tests 
                    (id INTEGER PRIMARY KEY,
                     run_id       INTEGER   DEFAULT -1,
                     testname     TEXT      DEFAULT 'noname',
                     host         TEXT      DEFAULT 'n/a',
                     cpuload      REAL      DEFAULT -1,
                     diskfree     INTEGER   DEFAULT -1,
                     uname        TEXT      DEFAULT 'n/a', 
                     rundir       TEXT      DEFAULT 'n/a',
                     shortdir     TEXT      DEFAULT '',
                     item_path    TEXT      DEFAULT '',
                     state        TEXT      DEFAULT 'NOT_STARTED',
                     status       TEXT      DEFAULT 'FAIL',
                     attemptnum   INTEGER   DEFAULT 0,
                     final_logf   TEXT      DEFAULT 'logs/final.log',
                     logdat       TEXT      DEFAULT '', 
                     run_duration INTEGER   DEFAULT 0,
                     comment      TEXT      DEFAULT '',
                     event_time   TIMESTAMP DEFAULT (strftime('%s','now')),
                     fail_count   INTEGER   DEFAULT 0,
                     pass_count   INTEGER   DEFAULT 0,
                     archived     INTEGER   DEFAULT 0, -- 0=no, 1=in progress, 2=yes
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
                                     reviewed    TIMESTAMP DEFAULT (strftime('%s','now')),
                                     iterated    TEXT DEFAULT '',
                                     avg_runtime REAL DEFAULT -1,
                                     avg_disk    REAL DEFAULT -1,
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
       (sqlite3:for-each-row 
	(lambda (test-id)
	  (set! incompleted (cons test-id incompleted)))
	db
	"SELECT id FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time - run_duration) > ? AND state IN ('RUNNING','REMOTEHOSTSTART');"
	run-id deadtime)

       ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
       ;;
       (sqlite3:for-each-row
	(lambda (test-id)
	  (set! incompleted (cons test-id incompleted)))
	db
	"SELECT id FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time - run_duration) > ? AND state IN ('LAUNCHED');"
	run-id (* 60 60 24)))
     run-ids)
       
    ;; These are defunct tests, do not do all the overhead of set-state-status. Force them to INCOMPLETE.
    ;;
    (if (> (length incompleted) 0)
	(begin
	  (debug:print 0 "WARNING: Marking test(s); " (string-intersperse (map conc incompleted) ", ") " as INCOMPLETE")
	  (sqlite3:execute 
	   db
	   (conc "UPDATE tests SET state='INCOMPLETE' WHERE id IN (" 
		 (string-intersperse (map conc incompleted) ",")
		 ");"))))))
		     
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
  (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val)
  (debug:print-info 11 "db:set-var END " var " " val))

(define (db:del-var db var)
  (debug:print-info 11 "db:del-var START " var)
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
	  (sqlite3:execute db "UPDATE runs SET state=?,status=?,event_time=strftime('%s','now') WHERE id=? AND state='deleted';" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
	  #f))))

(define (db:get-all-run-ids db)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (run-id)
       (set! res (cons run-id res)))
     db 
     "SELECT DISTINCT run_id FROM tests;")
    res))

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
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt " ORDER BY event_time "
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
  (let* ((res       (vector #f #f #f #f))
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
  (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment run-id))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run db run-id)
  ;; First set any related tests to DELETED
  (let ((stmt1 (sqlite3:prepare db "UPDATE tests SET state='DELETED',comment='' WHERE run_id=?;"))
	(stmt2 (sqlite3:prepare db "UPDATE runs SET state='deleted',comment='' WHERE id=?;")))
    (sqlite3:with-transaction
     db (lambda ()
	  (sqlite3:execute stmt1 run-id)
	  (sqlite3:execute stmt2 run-id)))
    (sqlite3:finalize! stmt1)
    (sqlite3:finalize! stmt2)))

(define (db:update-run-event_time db run-id)
  (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id))

(define (db:lock/unlock-run db run-id lock unlock user)
  (let ((newlockval (if lock "locked"
			(if unlock
			    "unlocked"
			    "locked")))) ;; semi-failsafe
    (sqlite3:execute db "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
    (sqlite3:execute db "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
		     user (conc newlockval " " run-id))
    (debug:print-info 1 "" newlockval " run number " run-id)))

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
(define (db:get-tests-for-run db run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals)
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
						     (conc " ORDER BY " sort-by " ")
						     " ")))
				(if sort-order sort-order " ")
				(if limit  (conc " LIMIT " limit)   " ")
				(if offset (conc " OFFSET " offset) " ")
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
  (let* ((res            '())
	 (tests-match-qry (tests:match->sqlqry testpatt))
	 (qry             (conc "SELECT id,testname,item_path,state,status FROM tests WHERE run_id=? " 
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") ""))))
    (debug:print-info 8 "db:get-tests-for-run qry=" qry)
    (sqlite3:for-each-row
     (lambda (id testname item-path state status)
       ;; id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
       (set! res (cons (vector id run-id testname state status -1 "" -1 -1 "" "-" item-path -1 "-" "-") res)))
     db 
     qry
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

(define (db:delete-test-records db test-id)
  (tdb:delete-test-step-records db test-id)
  (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id))

(define (db:delete-tests-for-run db run-id)
  (sqlite3:execute db "DELETE FROM tests WHERE run_id=?;" run-id))

(define (db:delete-old-deleted-test-records db)
  (let ((targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
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
		(sqlite3:execute db qry run-id newstate newstatus testname testname)))
	    testnames))

;; speed up for common cases with a little logic
;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;;
(define (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)
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
  (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
 		   state status run-id test-name item-path))

(define (db:get-count-tests-running db)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART');")
    res))

(define (db:get-count-tests-running-for-run-id db run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND id=?;" run-id)
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
	 "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART'
             AND testname in (SELECT testname FROM test_meta WHERE jobgroup=?);"
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

(define (db:get-all-tests-info-by-run-id db run-id)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
       ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
		       res)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE run_id=?;"
     run-id)
    res))

;; Get test data using test_id
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

;;======================================================================
;; Misc. test related queries
;;======================================================================

;; MUST BE CALLED local!
(define (db:test-get-paths-matching db keynames target fnamepatt #!key (res '()))
  ;; BUG: Move the values derived from args to parameters and push to megatest.scm
  (let* ((testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (paths-from-db (cdb:remote-run db:test-get-paths-matching-keynames-target-new db keynames target res
					testpatt
					statepatt
					statuspatt
					runname)))
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
							testpatt  
							statepatt 
							statuspatt
							runname)
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

;;======================================================================
;; QUEUE UP META, TEST STATUS AND STEPS REMOTE ACCESS
;;======================================================================

;; NOTE: Can remove the regex and base64 encoding for zmq
(define (db:obj->string obj)
  (case *transport-type*
    ;; ((fs) obj)
    ((http fs)
     (string-substitute
      (regexp "=") "_"
      (base64:base64-encode (with-output-to-string (lambda ()(serialize obj))))
      #t))
    ((zmq)(with-output-to-string (lambda ()(serialize obj))))
    (else obj)))

(define (db:string->obj msg)
  (case *transport-type*
    ;; ((fs) msg)
    ((http fs)
     (if (string? msg)
	 (with-input-from-string 
	     (base64:base64-decode
	      (string-substitute 
	       (regexp "_") "=" msg #t))
	   (lambda ()(deserialize)))
	 (vector #f #f #f))) ;; crude reply for when things go awry
    ((zmq)(with-input-from-string msg (lambda ()(deserialize))))
    (else msg)))

(define (db:test-set-status-state db test-id status state msg)
  (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
      (db:general-call db 'set-test-start-time (list test-id)))
  (if msg
      (db:general-call db 'state-status-msg (list state status msg test-id))
      (db:general-call db 'state-status     (list state status test-id))))

(define (db:roll-up-pass-fail-counts db run-id test-name item-path status)
  (if (and (not (equal? item-path ""))
	   (member status '("PASS" "WARN" "FAIL" "WAIVED" "RUNNING" "CHECK" "SKIP")))
      (begin
	(db:general-call db 'update-pass-fail-counts (list run-id test-name run-id test-name run-id test-name))
	(if (equal? status "RUNNING")
	    (db:general-call db 'top-test-set-running (list run-id test-name))
	    (db:general-call db 'top-test-set-per-pf-counts (list run-id test-name run-id test-name run-id test-name)))
	#f)
      #f))

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
	'(update-pass-fail-counts "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status IN ('FAIL','CHECK')),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status IN ('PASS','WARN','WAIVED'))
             WHERE run_id=? AND testname=? AND item_path='';")
	'(top-test-set-running  "UPDATE tests SET state='RUNNING' WHERE run_id=? AND testname=? AND item_path='';")
	'(top-test-set-per-pf-counts "UPDATE tests
                       SET state=CASE 
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE run_id=? AND testname=?
                                                     AND item_path != '' 
                                                     AND state in ('RUNNING','NOT_STARTED','LAUNCHED','REMOTEHOSTSTART')) > 0 THEN 'RUNNING'
                                   ELSE 'COMPLETED' END,
                            status=CASE 
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

(define (db:login db calling-path calling-version client-signature)
  (if (and (equal? calling-path *toppath*)
	   (equal? megatest-version calling-version))
      (begin
	(hash-table-set! *logged-in-clients* client-signature (current-seconds))
	'(#t "successful login"))      ;; path matches - pass! Should vet the caller at this time ...
      (list #f (conc "Login failed due to mismatch paths: " calling-path ", " *toppath*))))

(define (db:process-write db request-item)
  (let ((stmt-key (vector-ref request-item 0))
	(query    (vector-ref request-item 1))
	(params   (vector-ref request-item 2))
	(queryh   (sqlite3:prepare db query)))
    (apply sqlite3:execute stmt params)
    #f))

(define *db:process-queue-mutex* (make-mutex))

(define *number-of-writes*         0)
(define *writes-total-delay*       0)
(define *total-non-write-delay*    0)
(define *number-non-write-queries* 0)

(define (db:general-call db stmtname params)
  (let ((query (let ((q (alist-ref (if (string? stmtname)
				       (string->symbol stmtname)
				       stmtname)
				   db:queries)))
		 (if q (car q) #f))))
    (apply sqlite3:execute db query params)
    #t))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
;; 
;; Run this server-side
;;
(define (db:get-previous-test-run-record db run-id test-name item-path)
  (let* ((keys    (db:get-keys db))
	 (selstr  (string-intersperse  keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND "))
	 (keyvals #f))
    ;; first look up the key values from the run selected by run-id
    (sqlite3:for-each-row 
     (lambda (a . b)
       (set! keyvals (cons a b)))
     db
     (conc "SELECT " selstr " FROM runs WHERE id=? ORDER BY event_time DESC;") run-id)
    (if (not keyvals)
	#f
	(let ((prev-run-ids '()))
	  (apply sqlite3:for-each-row
		 (lambda (id)
		   (set! prev-run-ids (cons id prev-run-ids)))
		 db
		 (conc "SELECT id FROM runs WHERE " qrystr " AND id != ?;") (append keyvals (list run-id)))
	  ;; for each run starting with the most recent look to see if there is a matching test
	  ;; if found then return that matching test record
	  (debug:print 4 "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) #f
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (db:get-tests-for-run db hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f)))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))

;; get the previous records for when these tests were run where all keys match but runname
;; NB// Merge this with test:get-previous-test-run-records? This one looks for all matching tests
;; can use wildcards. Also can likely be factored in with get test paths?
;;
;; Run this remotely!!
;;
(define (db:get-matching-previous-test-run-records db run-id test-name item-path)
  (let* ((keys    (db:get-keys db))
	 (selstr  (string-intersperse (map (lambda (x)(vector-ref x 0)) keys) ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc (vector-ref x 0) "=?")) keys) " AND "))
	 (keyvals #f)
	 (tests-hash (make-hash-table)))
    ;; first look up the key values from the run selected by run-id
    (sqlite3:for-each-row 
     (lambda (a . b)
       (set! keyvals (cons a b)))
     db
     (conc "SELECT " selstr " FROM runs WHERE id=? ORDER BY event_time DESC;") run-id)
    (if (not keyvals)
	'()
	(let ((prev-run-ids '()))
	  (apply sqlite3:for-each-row
		 (lambda (id)
		   (set! prev-run-ids (cons id prev-run-ids)))
		 db
		 (conc "SELECT id FROM runs WHERE " qrystr " AND id != ?;") (append keyvals (list run-id)))
	  ;; collect all matching tests for the runs then
	  ;; extract the most recent test and return that.
	  (debug:print 4 "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals 
		       ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) '()  ;; no previous runs? return null
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (db:get-tests-for-run db hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f)))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name 
			       ", item-path " item-path " results: " (intersperse results "\n"))
		  ;; Keep only the youngest of any test/item combination
		  (for-each 
		   (lambda (testdat)
		     (let* ((full-testname (conc (db:test-get-testname testdat) "/" (db:test-get-item-path testdat)))
			    (stored-test   (hash-table-ref/default tests-hash full-testname #f)))
		       (if (or (not stored-test)
			       (and stored-test
				    (> (db:test-get-event_time testdat)(db:test-get-event_time stored-test))))
			   ;; this test is younger, store it in the hash
			   (hash-table-set! tests-hash full-testname testdat))))
		   results)
		  (if (null? tal)
		      (map cdr (hash-table->alist tests-hash)) ;; return a list of the most recent tests
		      (loop (car tal)(cdr tal))))))))))

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

(define (db:testmeta-get-all db)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (cons (apply vector a b) res)))
     db
     "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta;")
    res))

;;======================================================================
;; M I S C   M A N A G E M E N T   I T E M S 
;;======================================================================

;; the new prereqs calculation, looks also at itempath if specified
;; all prereqs must be met:
;;    if prereq test with itempath='' is COMPLETED and PASS, WARN, CHECK, or WAIVED then prereq is met
;;    if prereq test with itempath=ref-item-path and COMPLETED with PASS, WARN, CHECK, or WAIVED then prereq is met
;;
;; Note: mode 'normal means that tests must be COMPLETED and ok (i.e. PASS, WARN, CHECK, SKIP or WAIVED)
;;       mode 'toplevel means that tests must be COMPLETED only
;;       mode 'itemmatch or 'itemwait means that tests items must be COMPLETED and (PASS|WARN|WAIVED|CHECK) [[ NB// NOT IMPLEMENTED YET ]]
;; 
(define (db:get-prereqs-not-met db run-id waitons ref-item-path mode)
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   ;; next should be using mt:get-tests-for-run?
	   (let ((tests             (db:get-tests-for-run-state-status db run-id waitontest-name))
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
		       (same-itempath     (equal? ref-item-path item-path)))
		  (set! ever-seen #t)
		  (cond
		   ;; case 1, non-item (parent test) is 
		   ((and (equal? item-path "") ;; this is the parent test
			 is-completed
			 (or is-ok (member mode '(toplevel itemmatch itemwait))))
		    (set! parent-waiton-met #t))
		   ;; Special case for toplevel and KILLED
		   ((and (equal? item-path "") ;; this is the parent test
			 is-killed
			 (eq? mode 'toplevel))
		    (set! parent-waiton-met #t))
		   ;; For itemwait mode IFF the previous matching item is good the set parent-waiton-met
		   ((and (member mode '(itemmatch itemwait))
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
			     (eq? mode 'toplevel))              ;; toplevel does not block on FAIL
			 (and is-ok (eq? mode 'itemmatch))) ;; itemmatch blocks on not ok
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
;; (define *db:all-write-procs*
;;   (list 
;;    db:set-var 
;;    db:del-var
;;    db:register-run
;;    db:set-comment-for-run
;;    db:delete-run
;;    db:update-run-event_time
;;    db:lock/unlock-run 
;;    db:delete-test-step-records
;;    db:delete-test-records
;;    db:delete-tests-for-run
;;    db:delete-old-deleted-test-records
;;    db:set-tests-state-status
;;    db:test-set-state-status-by-id
;;    db:test-set-state-status-by-run-id-testname
;;    db:testmeta-add-record
;;    db:csv->test-data
;;    ))

