;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
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
	  (debug:print-info 11 "db:set-sync, setting pragma synchronous to " val)
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
	 (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					   (string->number (args:get-arg "-override-timeout"))
					   136000)))) ;; 136000))) ;; 136000 = 2.2 minutes
    (debug:print-info 11 "open-db, dbpath=" dbpath " argv=" (argv))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(db:initialize db))
    (db:set-sync db)
    db))

;; keeping it around for debugging purposes only
(define (open-run-close-no-exception-handling  proc idb . params)
  (debug:print-info 11 "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (let* ((db   (if idb 
		   (if (procedure? idb)
		       (idb)
		       idb)
		   (open-db)))
	 (res #f))
    (set! res (apply proc db params))
    (if (not idb)(sqlite3:finalize! db))
    (debug:print-info 11 "open-run-close-no-exception-handling END" )
    res))

(define (open-run-close-exception-handling proc idb . params)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 "EXCEPTION: database probably overloaded?")
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
    (db:set-sync db)
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
    (sqlite3:execute db "PRAGMA synchronous = OFF;")
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
(define (open-test-db testpath) 
  (debug:print-info 11 "open-test-db " testpath)
  (if (and testpath 
	   (directory? testpath)
	   (file-read-access? testpath))
      (let* ((dbpath    (conc testpath "/testdat.db"))
	     (dbexists  (file-exists? dbpath))
	     (db        (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	     (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					       (string->number (args:get-arg "-override-timeout"))
					       136000))))
	(sqlite3:set-busy-handler! db handler)
	(if (not dbexists)
	    (begin
	      (sqlite3:execute db "PRAGMA synchronous = FULL;")
	      (debug:print-info 11 "Initialized test database " dbpath)
	      (db:testdb-initialize db)))
	;; (sqlite3:execute db "PRAGMA synchronous = 0;")
	(debug:print-info 11 "open-test-db END (sucessful)" testpath)
	db)
      (begin
	(debug:print-info 11 "open-test-db END (unsucessful)" testpath)
	#f)))

;; find and open the testdat.db file for an existing test
(define (db:open-test-db-by-test-id db test-id)
  (let* ((test-path (db:test-get-rundir-from-test-id db test-id)))
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
  (let ((logline (apply conc loglst))
	(pwd     (current-directory))
	(cmdline (string-intersperse (argv) " "))
	(pid     (current-process-id)))
    (db:log-event logline pwd cmdline pid)))

(define (db:log-event logline pwd cmdline pid)
  (let ((db (open-logging-db)))
    (sqlite3:execute db "INSERT INTO log (logline,pwd,cmdline,pid) VALUES (?,?,?,?);" logline (current-directory)(string-intersperse (argv) " ")(current-process-id))
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

;; use a global for some primitive caching, it is just silly to re-read the db 
;; over and over again for the keys since they never change

(define (db:get-keys db)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(debug:print-info 11 "db:get-keys START (cache miss)")
	(sqlite3:for-each-row 
	 (lambda (key keytype)
	   (set! res (cons (vector key keytype) res)))
	 db
	 "SELECT fieldname,fieldtype FROM keys ORDER BY id DESC;")
	(set! *db-keys* res)
	(debug:print-info 11 "db:get-keys END (cache miss)")
	res)))

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

(define (runs:get-std-run-fields keys remfields)
  (let* ((header    (append (map key:get-fieldname keys)
			    remfields))
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
	 (header     (append (map key:get-fieldname keys)
		             remfields))
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
		           " ORDER BY event_time DESC "
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

;; just get count of runs
(define (db:get-num-runs db runpatt)
  (let ((numruns 0))
    (debug:print-info 11 "db:get-num-runs START " runpatt)
    (sqlite3:for-each-row 
     (lambda (count)
       (set! numruns count))
     db
     "SELECT COUNT(id) FROM runs WHERE runname LIKE ?;" runpatt)
    (debug:print-info 11 "db:get-num-runs END " runpatt)
    numruns))

;; use (get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
(define (db:get-run-info db run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
      (let* ((res      #f)
	     (keys      (db:get-keys db))
	     (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	     (header    (append (map key:get-fieldname keys)
				remfields))
	     (keystr    (conc (keys->keystr keys) ","
			      (string-intersperse remfields ","))))
	(debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
	(sqlite3:for-each-row
	 (lambda (a . x)
	   (set! res (apply vector a x)))
	 db
	 (conc "SELECT " keystr " FROM runs WHERE id=?;")
	 run-id)
	(debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
	(let ((finalres (vector header res)))
	  ;; (hash-table-set! *run-info-cache* run-id finalres)
	  finalres)))

(define (db:set-comment-for-run db run-id comment)
  (debug:print-info 11 "db:set-comment-for-run START run-id: " run-id " comment: " comment)
  (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment run-id)
  (debug:print-info 11 "db:set-comment-for-run END run-id: " run-id " comment: " comment))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run db run-id)
  (common:clear-caches) ;; don't trust caches after doing any deletion
  (sqlite3:execute db "DELETE FROM runs WHERE id=?;" run-id))

(define (db:update-run-event_time db run-id)
  (debug:print-info 11 "db:update-run-event_time START run-id: " run-id)
  (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id)
  (debug:print-info 11 "db:update-run-event_time END run-id: " run-id)) 

(define (db:lock/unlock-run db run-id lock unlock user)
  (let ((newlockval (if lock "locked"
			(if unlock
			    "unlocked"
			    "locked")))) ;; semi-failsafe
    (sqlite3:execute db "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
    (sqlite3:execute db "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
		     user (conc newlockval " " run-id))
    (debug:print-info 1 "" newlockval " run number " run-id)))

;;======================================================================
;; K E Y S
;;======================================================================

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (db:get-key-val-pairs db run-id)
  (let* ((keys (get-keys db))
	 (res  '()))
    (debug:print-info 11 "db:get-key-val-pairs START keys: " keys " run-id: " run-id)
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list (key:get-fieldname key) key-val) res)))
	  db qry run-id)))
     keys)
    (debug:print-info 11 "db:get-key-val-pairs END keys: " keys " run-id: " run-id)
    (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals db run-id)
  (let ((mykeyvals (hash-table-ref/default *keyvals* run-id #f)))
    (if mykeyvals 
	mykeyvals
	(let* ((keys (get-keys db))
	       (res  '()))
	  (debug:print-info 11 "db:get-key-vals START keys: " keys " run-id: " run-id)
	  (for-each 
	   (lambda (key)
	     (let ((qry (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")))
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

(define (db:tests-register-test db run-id test-name item-path)
  (debug:print-info 11 "db:tests-register-test START db=" db ", run-id=" run-id ", test-name=" test-name ", item-path=\"" item-path "\"")
  (let ((item-paths (if (equal? item-path "")
			(list item-path)
			(list item-path ""))))
    (for-each 
     (lambda (pth)
       (sqlite3:execute db "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');" 
			run-id 
			test-name
			pth))
     item-paths)
  (debug:print-info 11 "db:tests-register-test END db=" db ", run-id=" run-id ", test-name=" test-name ", item-path=\"" item-path "\"")
    #f))


;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
(define (db:get-tests-for-run db run-id testpatt states statuses 
			      #!key (not-in #t)
			      (sort-by #f) ;; 'rundir 'event_time
			      )
  (debug:print-info 11 "db:get-tests-for-run START run-id=" run-id ", testpatt=" testpatt ", states=" states ", statuses=" statuses ", not-in=" not-in ", sort-by=" sort-by)
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
	 (qry             (conc "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment "
				" FROM tests WHERE run_id=? "
				(if states-qry   (conc " AND " states-qry)   "")
				(if statuses-qry (conc " AND " statuses-qry) "")
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
				(case sort-by
				  ((rundir)     " ORDER BY length(rundir) DESC;")
				  ((event_time) " ORDER BY event_time ASC;")
				  (else         ";"))
			 )))
    (debug:print-info 8 "db:get-tests-for-run qry=" qry)
    (sqlite3:for-each-row 
     (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
       (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
     db 
     qry
     run-id
     ;; (if testpatt testpatt "%")
     ;; (if itempatt itempatt "%"))
     )
    (debug:print-info 11 "db:get-tests-for-run START run-id=" run-id ", testpatt=" testpatt ", states=" states ", statuses=" statuses ", not-in=" not-in ", sort-by=" sort-by)
    res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records db test-id)
  ;; Breaking it into two queries for better file access interleaving
  (let* ((tdb (db:open-test-db-by-test-id db test-id)))
    ;; test db's can go away - must check every time
    (if tdb
	(begin
	  (sqlite3:execute tdb "DELETE FROM test_steps;")
	  (sqlite3:execute tdb "DELETE FROM test_data;")
	  (sqlite3:finalize! tdb)))))

;; 
(define (db:delete-test-records db tdb test-id #!key (force #f))
  (common:clear-caches)
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
	    (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a' WHERE id=?;" test-id)))))

(define (db:delete-tests-for-run db run-id)
  (common:clear-caches)
  (sqlite3:execute db "DELETE FROM tests WHERE run_id=?;" run-id))

(define (db:delete-old-deleted-test-records db)
  (let ((targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
    (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED' AND event_time<?;" targtime)))

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

(define (cdb:delete-tests-in-state serverdat run-id state)
  (cdb:client-call serverdat 'delete-tests-in-state #t *default-numtries* run-id state))

;; speed up for common cases with a little logic
(define (db:test-set-state-status-by-id db test-id newstate newstatus newcomment)
  (cond
   ((and newstate newstatus newcomment)
    (sqlite3:exectute db "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;" newstate newstatus test-id))
   ((and newstate newstatus)
    (sqlite3:execute db "UPDATE tests SET state=?,status=? WHERE id=?;" newstate newstatus test-id))
   (else
    (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
    (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
    (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment test-id)))))

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
(define (db:patch-tdb-data-into-test-info db test-id res)
  (let ((tdb (db:open-test-db-by-test-id db test-id)))
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

;; Get test data using test_id
(define (db:get-test-info-cached-by-id db test-id)
  ;; is all this crap really worth it? I somehow doubt it.
  (let* ((last-delete-str (db:get-var db "DELETED_TESTS"))
	 (last-delete     (if (string? last-delete-str)(string->number last-delete-str) #f)))
    (if (and last-delete (> last-delete *last-test-cache-delete*))
	(begin
	  (set! *test-info* (make-hash-table))
	  (set! *test-id-cache* (make-hash-table))
	  (set! *last-test-cache-delete* last-delete)
	  (debug:print-info 4 "Clearing test data cache"))))
  (if (not test-id)
      (begin
	(debug:print-info 4 "db:get-test-info-by-id called with test-id=" test-id)
	#f)
      (let* ((res (hash-table-ref/default *test-info* test-id #f)))
	(if (and res
		 (member (db:test-get-state res) '("RUNNING" "COMPLETED")))
	    (db:patch-tdb-data-into-test-info db test-id res)
	    ;; if no cached value then full read and write to cache
	    (begin
	      (sqlite3:for-each-row
	       (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
		 ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
		 (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)))
	       db 
	       "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE id=?;"
	       test-id)
	      (if res (db:patch-tdb-data-into-test-info db test-id res))
	      res)))))

;; Get test data using test_id
(define (db:get-test-info-not-cached-by-id db test-id)
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

(define db:get-test-info-by-id db:get-test-info-not-cached-by-id)

(define (db:get-test-info db run-id testname item-path)
  (db:get-test-info-by-id db (db:get-test-id db run-id testname item-path)))

(define (db:test-set-comment db test-id comment)
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

(define (db:test-get-paths-matching db keynames target fnamepatt #!key (res '()))
  (let* ((testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (keystr (string-intersperse 
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
    (debug:print 3 "qrystr: " qrystr)
    (sqlite3:for-each-row 
     (lambda (p)
       (set! res (cons p res)))
     db 
     qrystr)
    (if fnamepatt
	(apply append 
	       (map (lambda (p)
		      (glob (conc p "/" fnamepatt)))
		    res))
	res)))

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
;; QUEUE UP META, TEST STATUS AND STEPS REMOTE ACCESS
;;======================================================================

;; db:updater is run in a thread to write out the cached data periodically
;; (define (db:updater)
;;   (debug:print-info 4 "Starting cache processing")
;;   (let loop ()
;;     (thread-sleep! 10) ;; move save time around to minimize regular collisions?
;;     (db:write-cached-data)
;;     (loop)))

(define (db:obj->string obj)
  (string-substitute
   (regexp "=") "_"
   (base64:base64-encode (with-output-to-string (lambda ()(serialize obj))))
   #t))

(define (db:string->obj msg)
  (with-input-from-string 
      (base64:base64-decode
       (string-substitute 
	(regexp "_") "=" msg #t))
    (lambda ()(deserialize))))

(define (cdb:use-non-blocking-mode proc)
  (set! *client-non-blocking-mode* #t)
  (let ((res (proc)))
    (set! *client-non-blocking-mode* #f)
    res))
  
;; params = 'target cached remparams
;;
;; make-vector-record cdb packet client-sig qtype immediate query-sig params qtime
;;
(define (cdb:client-call serverdat qtype immediate numretries . params)
  (debug:print-info 11 "cdb:client-call serverdat=" serverdat ", qtype=" qtype ", immediate=" immediate ", numretries=" numretries ", params=" params)
  ;; (handle-exceptions
  ;;  exn
  ;;  (begin
  ;;    (thread-sleep! 5) 
  ;;    (if (> numretries 0)(apply cdb:client-call serverdat qtype immediate (- numretries 1) params)))
   (let* ((client-sig  (server:get-client-signature))
	  (query-sig   (message-digest-string (md5-primitive) (conc qtype immediate params)))
	  (zdat        (db:obj->string (vector client-sig qtype immediate query-sig params (current-seconds)))) ;; (with-output-to-string (lambda ()(serialize params))))
	  )
     (debug:print-info 11 "zdat=" zdat)
     (let* (
	  (res  #f)
	  (rawdat      (server:client-send-receive serverdat zdat))
	  (tmp         #f))
     (debug:print-info 11 "Sent " zdat ", received " rawdat)
     (set! tmp (db:string->obj rawdat))
     ;; (if (equal? query-sig (vector-ref myres 1))
     ;; (set! res
     (vector-ref tmp 2)
     ;; (loop (server:client-send-receive serverdat zdat)))))))
	  ;; (timeout (lambda ()
	  ;;            (let loop ((n numretries))
	  ;;              (thread-sleep! 15)
	  ;;              (if (not res)
	  ;;       	   (if (> numretries 0)
	  ;;       	       (begin
	  ;;       		 (debug:print 2 "WARNING: no reply to query " params ", trying resend")
	  ;;       		 (debug:print-info 11 "re-sending message")
	  ;;       		 (apply cdb:client-call serverdat qtype immediate numretries params)
	  ;;       		 (debug:print-info 11 "message re-sent")
	  ;;       		 (loop (- n 1)))
	  ;;       	       ;; (apply cdb:client-call serverdats qtype immediate (- numretries 1) params))
	  ;;       	       (begin
	  ;;       		 (debug:print 0 "ERROR: cdb:client-call timed out " params ", exiting.")
	  ;;       		 (exit 5))))))))
     ;; (send-receive)
     )))
     ;; (debug:print-info 11 "Starting threads")
     ;; (let ((th1 (make-thread send-receive "send receive"))
     ;;       (th2 (make-thread timeout      "timeout")))
     ;;   (thread-start! th1)
     ;;   (thread-start! th2)
     ;;   (thread-join!  th1)
     ;;   (debug:print-info 11 "cdb:client-call returning res=" res)
     ;;   res))))
  
(define (cdb:set-verbosity serverdat val)
  (cdb:client-call serverdat 'set-verbosity #f *default-numtries* val))

(define (cdb:login serverdat keyval signature)
  (cdb:client-call serverdat 'login #t *default-numtries* keyval megatest-version signature))

(define (cdb:logout serverdat keyval signature)
  (cdb:client-call serverdat 'logout #t *default-numtries* keyval signature))

(define (cdb:num-clients serverdat)
  (cdb:client-call serverdat 'numclients #t *default-numtries*))

(define (cdb:test-set-status-state serverdat test-id status state msg)
  (if msg
      (cdb:client-call serverdat 'state-status-msg #t *default-numtries* state status msg test-id)
      (cdb:client-call serverdat 'state-status #t *default-numtries* state status test-id))) ;; run-id test-name item-path minutes cpuload diskfree tmpfree) 

(define (cdb:test-rollup-test_data-pass-fail serverdat test-id)
  (cdb:client-call serverdat 'test_data-pf-rollup #t *default-numtries* test-id test-id test-id test-id))

(define (cdb:pass-fail-counts serverdat test-id fail-count pass-count)
  (cdb:client-call serverdat 'pass-fail-counts #t *default-numtries* fail-count pass-count test-id))

(define (cdb:tests-register-test serverdat run-id test-name item-path)
  (let ((item-paths (if (equal? item-path "")
			(list item-path)
			(list item-path ""))))
    (cdb:client-call serverdat 'register-test #t *default-numtries* run-id test-name item-path)))

(define (cdb:flush-queue serverdat)
  (cdb:client-call serverdat 'flush #f *default-numtries*))

(define (cdb:kill-server serverdat)
  (cdb:client-call serverdat 'killserver #f *default-numtries*))

(define (cdb:roll-up-pass-fail-counts serverdat run-id test-name item-path status)
  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:roll-up-pass-fail-counts #f run-id test-name item-path status))

(define (cdb:get-test-info serverdat run-id test-name item-path)
  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info #f run-id test-name item-path))

(define (cdb:get-test-info-by-id serverdat test-id)
  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info-by-id #f test-id))

;; db should be db open proc or #f
(define (cdb:remote-run proc db . params)
  (apply cdb:client-call *runremote* 'immediate #f *default-numtries* open-run-close proc #f params))

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

(define db:queries 
  (list '(register-test          "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');")
	'(state-status           "UPDATE tests SET state=?,status=? WHERE id=?;")
	'(state-status-msg       "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;")
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
    ))

;; do not run these as part of the transaction
(define db:special-queries   '(rollup-tests-pass-fail
			       db:roll-up-pass-fail-counts
                               login
                               immediate
			       flush
			       sync
			       set-verbosity
			       killserver))

;; not used, intended to indicate to run in calling process
(define db:run-local-queries '()) ;; rollup-tests-pass-fail))

;; The queue is a list of vectors where the zeroth slot indicates the type of query to
;; apply and the second slot is the time of the query and the third entry is a list of 
;; values to be applied
;;
(define (db:process-queue db pubsock indata)
  (let* ((data       (sort indata (lambda (a b)
				    (< (cdb:packet-get-qtime a)(cdb:packet-get-qtime b))))))
    (for-each
     (lambda (item)
       (db:process-queue-item db pubsock item))
     data)))

(define (db:process-queue-item db item)
  (let* ((stmt-key       (cdb:packet-get-qtype item))
	 (qry-sig        (cdb:packet-get-query-sig item))
	 (return-address (cdb:packet-get-client-sig item))
	 (params         (cdb:packet-get-params item))
	 (query          (let ((q (alist-ref stmt-key db:queries)))
			   (if q (car q) #f))))
    (debug:print-info 11 "Special queries/requests stmt-key=" stmt-key ", return-address=" return-address ", query=" query ", params=" params)
    (cond
     (query
      (apply sqlite3:execute db query params)
      (server:reply return-address qry-sig #t #t))
     ((member stmt-key db:special-queries)
      (debug:print-info 11 "Handling special statement " stmt-key)
      (case stmt-key
	((immediate)
	 (let ((proc      (car params))
	       (remparams (cdr params)))
	   ;; we are being handed a procedure so call it
	   (debug:print-info 11 "Running (apply " proc " " remparams ")")
	   (server:reply return-address qry-sig #t (apply proc remparams))))
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
	 (server:reply return-address qry-sig #t '(#t *verbosity*)))
	((killserver)
	 (debug:print 0 "WARNING: Server going down in 15 seconds by user request!")
	 (open-run-close tasks:server-deregister tasks:open-db 
			 (car *runremote*)
			 pullport: (cadr *runremote*))
	 (thread-start! (make-thread (lambda ()(thread-sleep! 15)(exit))))
	 (server:reply return-address qry-sig #t '(#t "exit process started")))
	(else ;; not a command, i.e. is a query
	 (debug:print 0 "ERROR: Unrecognised query/command " stmt-key)
	 (server:reply pubsock return-address qry-sig #f 'failed))))
     (else
      (debug:print-info 11 "Executing " stmt-key " for " params)
      (apply sqlite3:execute (hash-table-ref queries stmt-key) params)
      (server:reply return-address qry-sig #t #t)))))

(define (db:test-get-records-for-index-file db run-id test-name)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id itempath state status run_duration logf comment)
       (set! res (cons (vector id itempath state status run_duration logf comment) res)))
     db
     "SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname=? AND item_path != '';"
     run-id test-name)
    res))

;; Rollup the pass/fail counts from itemized tests into fail_count and pass_count
(define (db:roll-up-pass-fail-counts db run-id test-name item-path status)
  ;; (cdb:flush-queue *runremote*)
  (if (and (not (equal? item-path ""))
	   (member status '("PASS" "WARN" "FAIL" "WAIVED" "RUNNING" "CHECK")))
      (begin
	(sqlite3:execute 
	 db
	 "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status='FAIL'),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND (status='PASS' OR status='WARN' OR status='WAIVED'))
             WHERE run_id=? AND testname=? AND item_path='';"
	 run-id test-name run-id test-name run-id test-name)
        ;; (thread-sleep! 0.1) ;; give other processes a chance here, no, better to be done ASAP?
	(if (equal? status "RUNNING") ;; running takes priority over all other states, force the test state to RUNNING
	    (sqlite3:execute db "UPDATE tests SET state=? WHERE run_id=? AND testname=? AND item_path='';" "RUNNING" run-id test-name)
	    (sqlite3:execute
	     db
	     "UPDATE tests
                       SET state=CASE 
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE run_id=? AND testname=?
                                                     AND item_path != '' 
                                                     AND state in ('RUNNING','NOT_STARTED')) > 0 THEN 'RUNNING'
                                   ELSE 'COMPLETED' END,
                                      status=CASE 
                                            WHEN fail_count > 0 THEN 'FAIL' 
                                            WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' 
                                            ELSE 'UNKNOWN' END
                       WHERE run_id=? AND testname=? AND item_path='';"
	     run-id test-name run-id test-name))
	#f)
      #f))

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
  (let ((tdb     (db:open-test-db-by-test-id db test-id)))
    (if tdb
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
				test-id category variable value expected tol units (if comment comment "") status type)
	       (sqlite3:finalize! tdb)))
	   csvlist)))))

;; get a list of test_data records matching categorypatt
(define (db:read-test-data db test-id categorypatt)
  (let ((tdb  (db:open-test-db-by-test-id db test-id)))
    (if tdb
	(let ((res '()))
	  (sqlite3:for-each-row 
	   (lambda (id test_id category variable value expected tol units comment status type)
	     (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
	   tdb
	   "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
	  (sqlite3:finalize! tdb)
	  (reverse res))
	'())))

(define (db:load-test-data db test-id)
  (let loop ((lin (read-line)))
    (if (not (eof-object? lin))
	(begin
	  (debug:print 4 lin)
	  (db:csv->test-data db test-id lin)
	  (loop (read-line)))))
  ;; roll up the current results.
  ;; FIXME: Add the status to 
  (db:test-data-rollup db test-id #f))

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup db test-id status)
  (let ((tdb (open-run-close db:open-test-db-by-test-id db test-id))
	(fail-count 0)
	(pass-count 0))
    (if tdb
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
	  (cdb:flush-queue *runremote*)
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
(define (db:get-steps-for-test db test-id)
  (let* ((tdb (db:open-test-db-by-test-id db test-id))
	 (res '()))
    (if tdb
	(begin
	  (sqlite3:for-each-row 
	   (lambda (id test-id stepname state status event-time logfile)
	     (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
	   tdb
	   "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
	   test-id)
	  (sqlite3:finalize! tdb)
	  (reverse res))
	'())))

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
       (sort steps (lambda (a b)
		     (cond
		      ((<   (db:step-get-event_time a)(db:step-get-event_time b)) #t)
		      ((eq? (db:step-get-event_time a)(db:step-get-event_time b)) 
		       (<   (db:step-get-id a)        (db:step-get-id b)))
		      (else #f)))))
      res)))

;;======================================================================
;; M I S C   M A N A G E M E N T   I T E M S 
;;======================================================================

;; the new prereqs calculation, looks also at itempath if specified
;; all prereqs must be met:
;;    if prereq test with itempath='' is COMPLETED and PASS, WARN, CHECK, or WAIVED then prereq is met
;;    if prereq test with itempath=ref-item-path and COMPLETED with PASS, WARN, CHECK, or WAIVED then prereq is met
;;
;; Note: do not convert to remote as it calls remote under the hood
;; Note: mode 'normal means that tests must be COMPLETED and ok (i.e. PASS, WARN, CHECK or WAIVED)
;;       mode 'toplevel means that tests must be COMPLETED only
;;       mode 'itemmatch means that tests items must be COMPLETED and (PASS|WARN|WAIVED|CHECK) [[ NB// NOT IMPLEMENTED YET ]]
;; 
(define (db:get-prereqs-not-met db run-id waitons ref-item-path #!key (mode 'normal))
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   (let ((tests             (db:get-tests-for-run db run-id waitontest-name '() '()))
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
			 (or is-ok (eq? mode 'toplevel)))
		    (set! parent-waiton-met #t))
		   ((and same-itempath
			 is-completed
			 (or is-ok (eq? mode 'toplevel)))
		    (set! item-waiton-met #t)))))
	      tests)
	     (if (not (or parent-waiton-met item-waiton-met))
		 (set! result (append (if (null? tests) (list waitontest-name) tests) result)))
	     ;; if the test is not found then clearly the waiton is not met...
	     ;; (if (not ever-seen)(set! result (cons waitontest-name result)))))
	     (if (not ever-seen)
		 (set! result (append (if (null? tests)(list waitontest-name) tests) result)))))
	 waitons)
	(delete-duplicates result))))

(define (db:teststep-set-status! db test-id teststep-name state-in status-in comment logfile)
  (debug:print 4 "test-id: " test-id " teststep-name: " teststep-name)
  (let* ((tdb       (db:open-test-db-by-test-id db test-id))
	 (state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (if tdb
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
