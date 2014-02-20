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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64 format)
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
;; (declare (uses sdb))
;; (declare (uses filedb))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

(define *rundb-mutex* (make-mutex)) ;; prevent problems opening/closing rundb's
(define *number-of-writes* 0)
(define *number-non-write-queries* 0)

;; Get/open a database
;;    if run-id => get run specific db
;;    if #f     => get main db
;;    if db already open - return inmem
;;    if db not open, open inmem, rundb and sync then return inmem
;;    inuse gets set automatically for rundb's
;;
(define (db:get-db dbstruct run-id)
  (if (sqlite3:database? dbstruct) ;; pass sqlite3 databases on through
      dbstruct
      (begin
	(mutex-lock! *rundb-mutex*)
	(let ((db (if run-id
		      (db:open-rundb dbstruct run-id)
		      (db:open-main dbstruct))))
	  ;; db prunning would go here
	  (mutex-unlock! *rundb-mutex*)
	  db))))

;; mod-read:
;;     'mod   modified data
;;     'read  read data
;;
(define (db:done-with dbstruct run-id mod-read)
  (if (not (sqlite3:database? dbstruct))
      (begin
	(mutex-lock! *rundb-mutex*)
	(if (eq? mod-read 'mod)
	    (dbr:dbstruct-set-mtime! dbstruct (current-milliseconds))
	    (dbr:dbstruct-set-rtime! dbstruct (current-milliseconds)))
	(dbr:dbstruct-set-inuse! dbstruct #f)
	(mutex-unlock! *rundb-mutex*))))

;; (db:with-db dbstruct run-id sqlite3:exec "select blah from blaz;")
;; r/w is a flag to indicate if the db is modified by this query #t = yes, #f = no
;;
(define (db:with-db dbstruct run-id r/w proc . params)
  (let* ((db    (db:get-db dbstruct run-id))
	 )
    ;; (proc2 (lambda ()
    (let ((res (apply proc db params)))
      (db:done-with dbstruct run-id r/w)
      res)))
;;     (handle-exceptions
;;      exn
;;      (begin
;;        (thread-sleep! 10)
;;        (proc2))
;;      (proc2))))

;;======================================================================
;; K E E P   F I L E D B   I N   dbstruct
;;======================================================================

;; (define (db:get-filedb dbstruct run-id)
;;   (let ((db (vector-ref dbstruct 2)))
;;     (if db
;; 	db
;; 	(let ((fdb (filedb:open-db (conc *toplevel* "/db/files.db"))))
;; 	  (vector-set! dbstruct 2 fdb)
;; 	  fdb))))
;; 
;; ;; Can also be used to save arbitrary strings
;; ;;
;; (define (db:save-path dbstruct path)
;;   (let ((fdb (db:get-filedb dbstruct)))
;;     (filedb:register-path fdb path)))
;; 
;; ;; Use to get a path. To get an arbitrary string see next define
;; ;;
;; (define (db:get-path dbstruct id)
;;   (let ((fdb (db:get-filedb dbstruct)))
;;     (filedb:get-path db id)))

;; This routine creates the db. It is only called if the db is not already opened
;; 
(define (db:open-rundb dbstruct run-id) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((local  (dbr:dbstruct-get-local dbstruct))
	 (rdb    (if local
		     (dbr:dbstruct-get-localdb dbstruct run-id)
		     (dbr:dbstruct-get-inmem dbstruct)))) ;; (dbr:dbstruct-get-runrec dbstruct run-id 'inmem)))
    (if rdb
	rdb
	(let* ((toppath      (dbr:dbstruct-get-path  dbstruct))
	       (dbpath       (conc toppath "/db/" run-id ".db"))
	       (dbexists     (file-exists? dbpath))
	       (inmem        (if local #f (db:open-inmem-db)))
	       (refdb        (if local #f (db:open-inmem-db)))
	       (db           (sqlite3:open-database dbpath))
	       (write-access (file-write-access? dbpath))
	       (handler      (make-busy-timeout 136000)))
	  (if (and dbexists (not write-access))
	      (set! *db-write-access* #f)) ;; only unset so other db's also can use this control
	  (if write-access
	      (begin
		(if (not dbexists)
		    (begin
		      (db:initialize-run-id-db db)
		      ;; (sdb:initialize db) 
		      )) ;; add strings db to rundb, not in use yet
		(sqlite3:set-busy-handler! db handler)
		(sqlite3:execute db "PRAGMA synchronous = 1;"))) ;; was 0 but 0 is a gamble
	  (dbr:dbstruct-set-rundb! dbstruct db)
	  (dbr:dbstruct-set-inuse! dbstruct #t)
	  (if local
	      (begin
		(dbr:dbstruct-set-localdb! dbstruct run-id db) ;; (dbr:dbstruct-set-inmem! dbstruct db) ;; direct access ...
		db)
	      (begin
		(dbr:dbstruct-set-inmem! dbstruct inmem)
		(db:sync-tables db:sync-tests-only db inmem)
		(dbr:dbstruct-set-refdb! dbstruct refdb)
		(db:sync-tables db:sync-tests-only db refdb)
		inmem))))))

;; This routine creates the db. It is only called if the db is not already opened
;;
(define (db:open-main dbstruct) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let ((mdb (dbr:dbstruct-get-main dbstruct)))
    (if mdb
	mdb
	(let* ((toppath      (dbr:dbstruct-get-path dbstruct))
	       (dbpath       (let ((dbdir (conc *toppath* "/db"))) ;; use this opportunity to create our db dir
			       (if (not (directory-exists? dbdir))
				   (create-direcory dbdir))
			       (conc *toppath* "/db/main.db")))
	       (dbexists     (file-exists? dbpath))
	       (db           (sqlite3:open-database dbpath))
	       (write-access (file-write-access? dbpath))
	       (handler      (make-busy-timeout 136000)))
	  (if (and dbexists (not write-access))
	      (set! *db-write-access* #f))
	  (if write-access 
	      (begin
		(sqlite3:set-busy-handler! db handler)
		(sqlite3:execute db "PRAGMA synchronous = 0;")))
	  (if (not dbexists)
	      (db:initialize-main-db db))
	  (dbr:dbstruct-set-main! dbstruct db)
	  db))))

;; Make the dbstruct, setup up auxillary db's and call for main db at least once
;;
(define (db:setup run-id #!key (local #f))
  (let ((dbstruct (make-dbr:dbstruct path: *toppath* local: local)))
    (db:get-db dbstruct #f) ;; force one call to main
    dbstruct))

;; Open the classic megatest.db file in toppath
;;
(define (db:open-megatest-db)
  (let* ((dbpath       (conc *toppath* "/megatest.db"))
	 (dbexists     (file-exists? dbpath))
	 (db           (sqlite3:open-database dbpath))
	 (write-access (file-write-access? dbpath))
	 (handler      (make-busy-timeout 136000)))
    (if (and dbexists (not write-access))
	(set! *db-write-access* #f))
    (if write-access 
	(begin
	  (sqlite3:set-busy-handler! db handler)
	  (sqlite3:execute db "PRAGMA synchronous = 0;")))
    (if (not dbexists)
	(begin
	  (db:initialize-main-db db)
	  (db:initialize-run-id-db db)))
    db))

;; sync all touched runs to disk
;;
(define (db:sync-touched dbstruct #!key (force-sync #f))
  (let ((tot-synced 0))
    (for-each
     (lambda (runvec)
       (let ((mtime (vector-ref runvec (dbr:dbstruct-field-name->num 'mtime)))
	     (stime (vector-ref runvec (dbr:dbstruct-field-name->num 'stime)))
	     (rundb (vector-ref runvec (dbr:dbstruct-field-name->num 'rundb)))
	     (inmem (vector-ref runvec (dbr:dbstruct-field-name->num 'inmem)))
	     (refdb (vector-ref runvec (dbr:dbstruct-field-name->num 'refdb))))
	 (if (or (> mtime stime) force-sync)
	     (let ((num-synced (db:sync-tables db:sync-tests-only inmem refdb rundb)))
	       (set! tot-synced (+ tot-synced num-synced))
	       (vector-set! runvec (dbr:dbstruct-field-name->num 'stime) (current-milliseconds))))))
     (hash-table-values (vector-ref dbstruct 1)))
    tot-synced))

;; sync run to disk if touched
;;
(define (db:sync-touched dbstruct #!key (force-sync #f))
  (let ((mtime (dbr:dbstruct-get-mtime dbstruct))
	(stime (dbr:dbstruct-get-stime dbstruct))
	(rundb (dbr:dbstruct-get-rundb dbstruct))
	(inmem (dbr:dbstruct-get-inmem dbstruct))
	(refdb (dbr:dbstruct-get-refdb dbstruct)))
    (if (or (not (number? mtime))
	    (not (number? stime))
	    (> mtime stime)
	    force-sync)
	(let ((num-synced (db:sync-tables db:sync-tests-only inmem refdb rundb)))
	  (dbr:dbstruct-set-stime! dbstruct (current-milliseconds))
	  num-synced)
	0)))

;; close all opened run-id dbs
(define (db:close-all dbstruct)
  ;; finalize main.db
  (db:sync-touched dbstruct force-sync: #t)
  (sqlite3:finalize! (db:get-db dbstruct #f))
  (let* ((local (dbr:dbstruct-get-local dbstruct))
	 (rundb (dbr:dbstruct-get-rundb dbstruct)))
    (if local
	(for-each
	 (lambda (db)
	   (if (sqlite3:database? db)
	       (sqlite3:finalize! db)))
	 (hash-table-values (dbr:dbstruct-get-locdbs dbstruct)))
	(if (sqlite3:database? rundb)
	    (sqlite3:finalize! rundb)
	    (debug:print 0 "WARNING: attempting to close databases but got " rundb " instead of a database")))))

(define (db:open-inmem-db)
  (let* ((db      (sqlite3:open-database ":memory:"))
	 (handler   (make-busy-timeout 3600)))
    (db:initialize-run-id-db db)
    ;; (sdb:initialize db) ;; for future use
    (sqlite3:set-busy-handler! db handler)
    db))

;; just tests, test_steps and test_data tables
(define db:sync-tests-only
  (list
   ;; (list "strs"
   ;;       '("id"             #f)
   ;;       '("str"            #f))
   (list "tests" 
	 '("id"             #f)
	 '("run_id"         #f)
	 '("testname"       #f)
	 '("host"           #f)
	 '("cpuload"        #f)
	 '("diskfree"       #f)
	 '("uname"          #f)
	 '("rundir"         #f)
	 '("shortdir"       #f)
	 '("item_path"      #f)
	 '("state"          #f)
	 '("status"         #f)
	 '("attemptnum"     #f)
	 '("final_logf"     #f)
	 '("logdat"         #f)
	 '("run_duration"   #f)
	 '("comment"        #f)
	 '("event_time"     #f)
	 '("fail_count"     #f)
	 '("pass_count"     #f)
	 '("archived"       #f))
  (list "test_steps"
	 '("id"             #f)
	 '("test_id"        #f)
	 '("stepname"       #f)
	 '("state"          #f)
	 '("status"         #f)
	 '("event_time"     #f)
	 '("comment"        #f)
	 '("logfile"        #f))
   (list "test_data"
	 '("id"             #f)
	 '("test_id"        #f)
	 '("category"       #f)
	 '("variable"       #f)
	 '("value"          #f)
	 '("expected"       #f)
	 '("tol"            #f)
	 '("units"          #f)
	 '("comment"        #f)
	 '("status"         #f)
	 '("type"           #f))))

;; needs db to get keys, this is for syncing all tables
;;
(define (db:sync-main-list db)
  (let ((keys  (db:get-keys db)))
    (list
     (list "keys"
	   '("id"        #f)
	   '("fieldname" #f)
	   '("fieldtype" #f))
     (list "metadat" '("var" #f) '("val" #f))
     (append (list "runs" 
		   '("id"  #f))
	     (map (lambda (k)(list k #f))
		  (append keys
			  (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count" "pass_count"))))
     (list "test_meta"
	   '("id"             #f)
	   '("testname"       #f)
	   '("owner"          #f)
	   '("description"    #f)
	   '("reviewed"       #f)
	   '("iterated"       #f)
	   '("avg_runtime"    #f)
	   '("avg_disk"       #f)
	   '("tags"           #f)
	   '("jobgroup"       #f)))))
    
;; tbls is ( ("tablename" ( "field1" [#f|proc1] ) ( "field2" [#f|proc2] ) .... ) )
(define (db:sync-tables tbls fromdb todb . slave-dbs)
  (cond
   ((not fromdb) (debug:print 0 "ERROR: db:sync-tables called with fromdb missing") -1)
   ((not todb)   (debug:print 0 "ERROR: db:sync-tables called with todb missing") -2)
   ((not (sqlite3:database? fromdb))
    (debug:print 0 "ERROR: db:sync-tables called with fromdb not a database " fromdb) -3)
   ((not (sqlite3:database? todb))
    (debug:print 0 "ERROR: db:sync-tables called with todb not a database " todb) -4)
   (else
    (let ((stmts       (make-hash-table)) ;; table-field => stmt
	  (all-stmts   '())              ;; ( ( stmt1 value1 ) ( stml2 value2 ))
	  (numrecs     (make-hash-table))
	  (start-time  (current-milliseconds))
	  (tot-count   0))
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

	   (debug:print 0 "INFO: found " (length fromdat) " records to sync")

	   ;; read the target table
	   (sqlite3:for-each-row
	    (lambda (a . b)
	      (hash-table-set! todat a (apply vector a b)))
	    todb
	    full-sel)

	   ;; first pass implementation, just insert all changed rows
	   (for-each 
	    (lambda (targdb)
	      (let ((stmth (sqlite3:prepare targdb full-ins)))
		(sqlite3:with-transaction
		 targdb
		 (lambda ()
		   (for-each ;; 
		    (lambda (fromrow)
		      (let* ((a    (vector-ref fromrow 0))
			     (curr (hash-table-ref/default todat a #f))
			     (same #t))
			(let loop ((i 0))
			  (if (or (not curr)
				  (not (equal? (vector-ref fromrow i)(vector-ref curr i))))
			      (set! same #f))
			  (if (and same
				   (< i (- num-fields 1)))
			      (loop (+ i 1))))
			(if (not same)
			    (begin
			      (apply sqlite3:execute stmth (vector->list fromrow))
			      (hash-table-set! numrecs tablename (+ 1 (hash-table-ref/default numrecs tablename 0)))))))
		    fromdat)))
		(sqlite3:finalize! stmth)))
	    (append (list todb) slave-dbs))))
       tbls)
      (let ((runtime (- (current-milliseconds) start-time)))
	(debug:print 0 "INFO: db sync, total run time " runtime " ms")
	(for-each 
	 (lambda (dat)
	   (let ((tblname (car dat))
		 (count   (cdr dat)))
	     (set! tot-count (+ tot-count count))
	     (if (> count 0)
		 (debug:print 0 (format #f "    ~10a ~5a" tblname count)))))
	 (sort (hash-table->alist numrecs)(lambda (a b)(> (cdr a)(cdr b))))))
      tot-count))))

;; keeping it around for debugging purposes only
(define (open-run-close-no-exception-handling  proc idb . params)
  (debug:print-info 11 "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (if (or *db-write-access*
	  (not (member proc *db:all-write-procs*)))
      (let* ((db (cond
		  ((sqlite3:database? idb)     idb)
		  ((not idb)                   (debug:print 0 "ERROR: cannot open-run-close with #f anymore"))
		  ((procedure? idb)            (idb))
		  (else   	               (debug:print 0 "ERROR: cannot open-run-close with #f anymore"))))
	     (res #f))
	(set! res (apply proc db params))
	(if (not idb)(sqlite3:finalize! dbstruct))
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

;; (define open-run-close 
(define open-run-close ;; (if (debug:debug-mode 2)
			   open-run-close-no-exception-handling)
			 ;;  open-run-close-exception-handling))

(define (db:initialize-main-db db)
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
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS keys (id INTEGER PRIMARY KEY, fieldname TEXT, fieldtype TEXT, CONSTRAINT keyconstraint UNIQUE (fieldname));")
    (for-each (lambda (key)
		(sqlite3:execute db "INSERT INTO keys (fieldname,fieldtype) VALUES (?,?);" key "TEXT"))
	      keys)
    (sqlite3:execute db (conc 
			 "CREATE TABLE IF NOT EXISTS runs (id INTEGER PRIMARY KEY, \n			 " 
			 fieldstr (if havekeys "," "") "
			 runname    TEXT DEFAULT 'norun',
			 state      TEXT DEFAULT '',
			 status     TEXT DEFAULT '',
			 owner      TEXT DEFAULT '',
			 event_time TIMESTAMP DEFAULT (strftime('%s','now')),
			 comment    TEXT DEFAULT '',
			 fail_count INTEGER DEFAULT 0,
			 pass_count INTEGER DEFAULT 0,
			 CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_meta (
                                     id          INTEGER PRIMARY KEY,
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
    (sqlite3:execute db (conc "CREATE INDEX runs_index ON runs (runname" (if havekeys "," "") keystr ");"))
    ;; (sqlite3:execute db "CREATE VIEW runs_tests AS SELECT * FROM runs INNER JOIN tests ON runs.id=tests.run_id;")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS extradat (id INTEGER PRIMARY KEY, run_id INTEGER, key TEXT, val TEXT);")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
    (sqlite3:execute db "CREATE TABLE IF NOT EXISTS access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")
    ;; Must do this *after* running patch db !! No more. 
    ;; cannot use db:set-var since it will deadlock, hardwire the code here
    (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" "MEGATEST_VERSION" megatest-version)
    (debug:print-info 11 "db:initialize END")))

;;======================================================================
;; R U N   S P E C I F I C   D B 
;;======================================================================

(define (db:initialize-run-id-db db)
  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS tests 
                    (id INTEGER PRIMARY KEY,
                     run_id       INTEGER   DEFAULT -1,
                     testname     TEXT      DEFAULT 'noname',
                     host         TEXT      DEFAULT 'n/a',
                     cpuload      REAL      DEFAULT -1,
                     diskfree     INTEGER   DEFAULT -1,
                     uname        TEXT      DEFAULT 'n/a', 
                     rundir       TEXT      DEFAULT '/tmp/badname',
                     shortdir     TEXT      DEFAULT '/tmp/badname',
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
                        CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path));")
    (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname, item_path);")
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
;;   (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data 
;;                               (id          INTEGER PRIMARY KEY,
;;                                      reviewed    TIMESTAMP DEFAULT (strftime('%s','now')),
;;                                      iterated    TEXT DEFAULT '',
;;                                      avg_runtime REAL DEFAULT -1,
;;                                      avg_disk    REAL DEFAULT -1,
;;                                      tags        TEXT DEFAULT '',
;;                                      jobgroup    TEXT DEFAULT 'default',
;;                                 CONSTRAINT test_meta_constraint UNIQUE (testname));")
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
  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_rundat (
                              id           INTEGER PRIMARY KEY,
                              test_id      INTEGER,
                              update_time  TIMESTAMP,
                              cpuload      INTEGER DEFAULT -1,
                              diskfree     INTEGER DEFAULT -1,
                              diskusage    INTGER DEFAULT -1,
                              run_duration INTEGER DEFAULT 0);")
  db)

;;======================================================================
;; L O G G I N G    D B 
;;======================================================================

(define (open-logging-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((dbpath    (conc (if *toppath* (conc *toppath* "/") "") "logging.db")) ;; fname)
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath))
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
;; D B   U T I L S
;;======================================================================

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
	 (run-ids      (db:get-all-run-ids db))) ;; iterate over runs to divy up the calls
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
(define (db:clean-up dbstruct)

  (debug:print 0 "ERROR: db clean up not ported yet")

  (let* ((db         (db:get-db dbstruct #f))
	 (count-stmt (sqlite3:prepare db "SELECT (SELECT count(id) FROM tests)+(SELECT count(id) FROM runs);"))
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
;; M E T A   G E T   A N D   S E T   V A R S
;;======================================================================

;; returns number if string->number is successful, string otherwise
;; also updates *global-delta*
;;
;; Operates on megatestdb
;;
(define (db:get-var dbstruct var)
  (let* ((start-ms (current-milliseconds))
         (throttle (let ((t  (config-lookup *configdat* "setup" "throttle")))
		     (if t (string->number t) t)))
	 (res      #f))
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     (db:get-db dbstruct #f)
     "SELECT val FROM metadat WHERE var=?;" var)
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
    res))

(define (db:set-var dbstruct var val)
  (sqlite3:execute (db:get-db dbstruct #f) "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val))

(define (db:del-var dbstruct var)
  (sqlite3:execute (db:get-db dbstruct #f) "DELETE FROM metadat WHERE var=?;" var))

;; use a global for some primitive caching, it is just silly to
;; re-read the db over and over again for the keys since they never
;; change

;; why get the keys from the db? why not get from the *configdat*
;; using keys:config-get-fields?

(define (db:get-keys dbstruct)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(db:with-db dbstruct #f #f
		    (lambda (db)
		      (sqlite3:for-each-row 
		       (lambda (key)
			 (set! res (cons key res)))
		       (db:get-db dbstruct #f)
		       "SELECT fieldname FROM keys ORDER BY id DESC;")))
	(set! *db-keys* res)
	res)))

;; look up values in a header/data structure
(define (db:get-value-by-header row header field)
  (if (null? header) #f
      (let loop ((hed (car header))
		 (tal (cdr header))
		 (n   0))
	(if (equal? hed field)
	    (vector-ref row n)
	    (if (null? tal) #f (loop (car tal)(cdr tal)(+ n 1)))))))

;; Accessors for the header/data structure
;; get rows and header from 
(define (db:get-header vec)(vector-ref vec 0))
(define (db:get-rows   vec)(vector-ref vec 1))

;;======================================================================
;;  R U N S
;;======================================================================

(define (db:get-run-name-from-id dbstruct run-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (runname)
       (set! res runname))
     (db:get-db dbstruct #f)
     "SELECT runname FROM runs WHERE id=?;"
     run-id)
    res))

(define (db:get-run-key-val dbstruct run-id key)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     (db:get-db dbstruct #f) 
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


;; register a test run with the db, this accesses the main.db and does NOT
;; use server api
;;
(define (db:register-run dbstruct keyvals runname state status user)
  (let* ((db        (db:get-db dbstruct #f))
	 (keys      (map car keyvals))
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

;; replace header and keystr with a call to runs:get-std-run-fields
;;
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;; runpatts: patt1,patt2 ...
;;
(define (db:get-runs dbstruct runpatt count offset keypatts)
  (let* ((res       '())
	 (keys       (db:get-keys dbstruct))
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
    (db:with-db dbstruct #f #f
		(lambda (db)		
		  (sqlite3:for-each-row
		   (lambda (a . x)
		     (set! res (cons (apply vector a x) res)))
		   db
		   qrystr
		   )))
    (debug:print-info 11 "db:get-runs END qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (vector header res)))

;; db:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db:get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
;;  to extract info from the structure returned
;;
;; NOTE: THIS IS COMPLETELY UNFINISHED. IT GOES WITH rmt:get-get-paths-matching-keynames
;;
(define (db:get-run-ids-matching dbstruct keynames target res)
;; (define (db:get-runs-by-patt dbstruct keys runnamepatt targpatt offset limit) ;; test-name)
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
    (db:with-db dbstruct #f #f ;; reads db, does not write to it.
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (a . r)
		     (set! res (cons (list->vector (cons a r)) res)))
		   (db:get-db dbstruct #f)
		   qry-str
		   runnamepatt)))
    (vector header res)))

;; Get all targets from the db
;;
(define (db:get-targets dbstruct)
  (let* ((res       '())
	 (keys       (db:get-keys dbstruct))
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
     (db:get-db dbstruct #f)
     qrystr)
    (debug:print-info 11 "db:get-targets END qrystr: " qrystr )
    (vector header res)))

;; just get count of runs
(define (db:get-num-runs dbstruct runpatt)
  (let ((numruns 0))
    (debug:print-info 11 "db:get-num-runs START " runpatt)
    (sqlite3:for-each-row 
     (lambda (count)
       (set! numruns count))
     (db:get-db dbstruct #f)
     "SELECT COUNT(id) FROM runs WHERE runname LIKE ? AND state != 'deleted';" runpatt)
    (debug:print-info 11 "db:get-num-runs END " runpatt)
    numruns))

(define (db:get-all-run-ids dbstruct)
  (let ((run-ids '()))
    (sqlite3:for-each-row
     (lambda (run-id)
       (set! run-ids (cons run-id run-ids)))
     (db:get-db dbstruct #f)
     "SELECT id FROM runs WHERE state != 'deleted';")
    (reverse run-ids)))

;; get some basic run stats
;;
;; ( (runname (( state  count ) ... ))
;;   (   ...  
(define (db:get-run-stats dbstruct)
  (let ((totals       (make-hash-table))
	(res          '())
	(runs-info    '()))
    ;; First get all the runname/run-ids
    (sqlite3:for-each-row
     (lambda (run-id runname)
       (set! runs-info (cons (list run-id runname) runs-info)))
     (db:get-db dbstruct #f)
     "SELECT id,runname FROM runs;")
    ;; for each run get stats data
    (for-each
     (lambda (run-info)
       (let ((run-id   (car  run-info))
	     (run-name (cadr run-info)))
	 (sqlite3:for-each-row
	  (lambda (state count)
	    (if (string? state)
		(let* ((stateparts (string-split state "|"))
		       (newstate   (conc (car stateparts) "\n" (cadr stateparts))))
		  (hash-table-set! totals newstate (+ (hash-table-ref/default totals newstate 0) count))
		  (set! res (cons (list run-name newstate count) res)))))
	  (db:get-db dbstruct run-id)
	  "SELECT state||'|'||status AS s,count(id) FROM tests AS t ORDER BY s DESC;" )
    ;; (set! res (reverse res))
    (for-each (lambda (state)
		(set! res (cons (list "Totals" state (hash-table-ref totals state)) res)))
		   (sort (hash-table-keys totals) string>=))))
     runs-info)
    res))

;; db:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db:get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
;;  to extract info from the structure returned
;;
(define (db:get-runs-by-patt dbstruct keys runnamepatt targpatt offset limit) ;; test-name)
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
    (db:with-db dbstruct #f #f ;; reads db, does not write to it.
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (a . r)
		     (set! res (cons (list->vector (cons a r)) res)))
		   (db:get-db dbstruct #f)
		   qry-str
		   runnamepatt)))
    (vector header res)))

;; use (get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
(define (db:get-run-info dbstruct run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
  (let* ((res       (vector #f #f #f #f))
	 (keys      (db:get-keys dbstruct))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (apply vector a x)))
     (db:get-db dbstruct #f)
     (conc "SELECT " keystr " FROM runs WHERE id=? AND state != 'deleted';")
     run-id)
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (let ((finalres (vector header res)))
      ;; (hash-table-set! *run-info-cache* run-id finalres)
      finalres)))

(define (db:set-comment-for-run dbstruct run-id comment)
  (sqlite3:execute (db:get-db dbstruct #f) "UPDATE runs SET comment=? WHERE id=?;" comment ;; (sdb:qry 'getid comment)
		   run-id))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run dbstruct run-id)
  ;; First set any related tests to DELETED
  (let ((db (db:get-db dbstruct run-id)))
    (sqlite3:execute db "UPDATE tests SET state='DELETED',comment='';")
    (sqlite3:execute db "DELETE FROM test_steps;")
    (sqlite3:execute db "DELETE FROM test_data;")
    (sqlite3:execute (db:get-db dbstruct #f) "UPDATE runs SET state='deleted',comment='' WHERE id=?;" run-id)))

(define (db:update-run-event_time dbstruct run-id)
  (sqlite3:execute (db:get-db dbstruct #f) "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id))

(define (db:lock/unlock-run dbstruct run-id lock unlock user)
  (let ((newlockval (if lock "locked"
			(if unlock
			    "unlocked"
			    "locked")))) ;; semi-failsafe
    (sqlite3:execute (db:get-db dbstruct #f) "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
    (sqlite3:execute (db:get-db dbstruct #f) "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
		     user (conc newlockval " " run-id))
    (debug:print-info 1 "" newlockval " run number " run-id)))

;;======================================================================
;; K E Y S
;;======================================================================

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (db:get-key-val-pairs dbstruct run-id)
  (let* ((keys (db:get-keys dbstruct))
	 (res  '()))
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	 ;; (debug:print 0 "qry: " qry)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list key key-val) res)))
	  (db:get-db dbstruct #f) qry run-id)))
     keys)
    (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals dbstruct run-id)
  (let* ((keys (db:get-keys dbstruct))
	 (res  '()))
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons key-val res)))
	  (db:get-db dbstruct #f) qry run-id)))
     keys)
    (let ((final-res (reverse res)))
      (hash-table-set! *keyvals* run-id final-res)
      final-res)))

;; The target is keyval1/keyval2..., cached in *target* as it is used often
(define (db:get-target dbstruct run-id)
  (let* ((keyvals (db:get-key-vals dbstruct run-id))
	 (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/")))
    thekey))

;; Get run-ids for runs with same target but different runnames and NOT run-id
;;
(define (db:get-prev-run-ids dbstruct run-id)
  (let* ((keyvals (rmt:get-key-val-pairs run-id))
	 (kvalues (map cadr keyvals))
	 (keys    (rmt:get-keys))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND ")))
    (let ((prev-run-ids '()))
      (db:with-db dbstruct #f #f ;; #f means work with the zeroth db - i.e. the runs db
       (lambda (db)
	 (apply sqlite3:for-each-row
		(lambda (id)
		  (set! prev-run-ids (cons id prev-run-ids)))
		db
		(conc "SELECT id FROM runs WHERE " qrystr " AND id != ?;") (append kvalues (list run-id)))))
      prev-run-ids)))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
(define (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals)
  (if (not (number? run-id))
      (begin
	(debug:print 0 "ERROR: call to db:get-tests-for-run with bad run-id=" run-id)
	(print-call-chain)
	'())
      (let* ((qryvalstr       (case qryvals
				((shortlist) "id,run_id,testname,item_path,state,status")
				((#f)        db:test-record-qry-selector) ;; "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")
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
	(debug:print-info 8 "db:get-tests-for-run run-id=" run-id ", qry=" qry)
	(db:with-db dbstruct run-id #f
		    (lambda (db)
		      (sqlite3:for-each-row 
		       (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
			 (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
		       db
		       qry
		       run-id
		       )))
	(case qryvals
	  ((shortlist)(map db:test-short-record->norm res))
	  ((#f)       res)
	  (else       res)))))

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


(define (db:get-tests-for-run-state-status dbstruct run-id testpatt)
  (let* ((res            '())
	 (tests-match-qry (tests:match->sqlqry testpatt))
	 (qry             (conc "SELECT id,testname,item_path,state,status FROM tests WHERE run_id=? " 
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") ""))))
    (debug:print-info 8 "db:get-tests-for-run qry=" qry)
    (db:with-db dbstruct run-id #f
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (id testname item-path state status)
		     ;;                      id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
		     (set! res (cons (vector id run-id testname state status -1         ""     -1      -1       ""    "-"  item-path -1           "-"         "-") res)))
		   db 
		   qry
		   run-id)))
    res))

(define (db:get-testinfo-state-status dbstruct run-id test-id)
  (let ((res            #f))
    (db:with-db dbstruct run-id #f
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (run-id testname item-path state status)
		     ;; id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
		     (set! res (vector test-id run-id testname state status -1 "" -1 -1 "" "-" item-path -1 "-" "-")))
		   db 
		   "SELECT run_id,testname,item_path,state,status FROM tests WHERE id=?;" 
		   test-id)))
    res))

;; get a useful subset of the tests data (used in dashboard
;; use db:mintests-get-{id ,run_id,testname ...}
(define (db:get-tests-for-runs-mindata dbstruct run-ids testpatt states statuses not-in)
  (db:get-tests-for-runs dbstruct run-ids testpatt states statuses not-in: not-in qryvals: "id,run_id,testname,state,status,event_time,item_path"))

(define (db:get-tests-for-runs dbstruct run-ids testpatt states statuses #!key (not-in #f)(qryvals #f))
  (let ((res '()))
    (for-each 
     (lambda (run-id)
       (set! res (append 
		  res 
		  (db:get-tests-for-run dbstruct run-id testpatt states statuses #f #f not-in #f #f qryvals))))
     (if run-ids
	 run-ids
	 (db:get-all-run-ids dbstruct)))
    res))

;; Convert calling routines to get list of run-ids and loop, do not use the get-tests-for-runs
;;

(define (db:delete-test-records dbstruct run-id test-id)
  (let ((db (db:get-db dbstruct run-id)))
    (db:general-call db 'delete-test-step-records (list test-id))
    (db:general-call db 'delete-test-data-records (list test-id))
    (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id)))

(define (db:delete-tests-for-run dbdbstruct run-id)
  (let ((db (db:get-db dbstruct run-id)))
     (sqlite3:execute db "DELETE FROM tests WHERE run_id=?;" run-id)))

(define (db:delete-old-deleted-test-records dbstruct)
  (let ((run-ids  (db:get-all-run-ids dbstruct))
	(targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
    (for-each
     (lambda (run-id)
       (sqlite3:execute (db:get-db dbstruct run-id) "DELETE FROM tests WHERE state='DELETED' AND event_time<?;" targtime))
     run-ids)))

;; set tests with state currstate and status currstatus to newstate and newstatus
;; use currstate = #f and or currstatus = #f to apply to any state or status respectively
;; WARNING: SQL injection risk. NB// See new but not yet used "faster" version below
;;
(define (db:set-tests-state-status dbstruct run-id testnames currstate currstatus newstate newstatus)
  (for-each (lambda (testname)
	      (let ((qry (conc "UPDATE tests SET state=?,status=? WHERE "
			       (if currstate  (conc "state='" currstate "' AND ") "")
			       (if currstatus (conc "status='" currstatus "' AND ") "")
			       " run_id=? AND testname=? AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
		;;(debug:print 0 "QRY: " qry)
		(sqlite3:execute (db:get-db dbstruct run-id) qry run-id newstate newstatus testname testname)))
	    testnames))

;; speed up for common cases with a little logic
;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;;
(define (db:test-set-state-status-by-id dbstruct run-id test-id newstate newstatus newcomment)
  (let ((db (db:get-db dbstruct run-id)))
    (cond
     ((and newstate newstatus newcomment)
    (sqlite3:execute db "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;" newstate newstatus newcomment ;; (sdb:qry 'getid newcomment)
		     test-id))
     ((and newstate newstatus)
      (sqlite3:execute db "UPDATE tests SET state=?,status=? WHERE id=?;" newstate newstatus test-id))
     (else
      (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
      (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
    (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment ;; (sdb:qry 'getid newcomment)
				    test-id))))
    (mt:process-triggers run-id test-id newstate newstatus)))

;; Never used, but should be?
(define (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)
  (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
 		   state status run-id test-name item-path))

;; NEW BEHAVIOR: Count tests running in only one run!
;;
(define (db:get-count-tests-running dbstruct run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     (db:get-db dbstruct run-id)
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=?;" 
     run-id) ;; NOT IN (SELECT id FROM runs WHERE state='deleted');")
    res))

;; NEW BEHAVIOR: Look only at single run with run-id
;; 
;; (define (db:get-running-stats dbstruct run-id)
(define (db:get-count-tests-running-for-run-id dbstruct run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     (db:get-db dbstruct run-id)
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=?;" run-id)
    res))

(define (db:get-count-tests-running-in-jobgroup dbstruct run-id jobgroup)
  (if (not jobgroup)
      0 ;; 
      (let ((res        0)
	    (testnames '()))
	;; get the testnames
	(sqlite3:for-each-row
	 (lambda (testname)
	   (set! testnames (cons testname testnames)))
	 (db:get-db dbstruct #f)
	 "SELECT testname FROM test_meta WHERE jobgroup=?"
	 jobgroup)
	;; get the jobcount NB// EXTEND THIS TO OPPERATE OVER ALL RUNS?
	(if (not (null? testnames))
	    (sqlite3:for-each-row
	     (lambda (count)
	       (set! res count))
	     (db:get-db dbstruct run-id)
	     (conc "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND testname in ('"
		   (string-intersperse testnames "','")
		   "');")))
	res)))

;; done with run when:
;;   0 tests in LAUNCHED, NOT_STARTED, REMOTEHOSTSTART, RUNNING
(define (db:estimated-tests-remaining dbstruct run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     (db:get-db dbstruct run-id) ;; NB// KILLREQ means the jobs is still probably running
     "SELECT count(id) FROM tests WHERE state in ('LAUNCHED','NOT_STARTED','REMOTEHOSTSTART','RUNNING','KILLREQ');")
    res))

;; map run-id, testname item-path to test-id
(define (db:get-test-id dbstruct run-id testname item-path)
  (let* ((db (db:get-db dbstruct run-id))
	 (res #f))
    (sqlite3:for-each-row
     (lambda (id) ;;  run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )
       (set! res id)) ;; (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )))
     (db:get-db dbstruct run-id)
     "SELECT id FROM tests WHERE testname=? AND item_path=?;"
     testname item-path)
    res))

(define db:test-record-fields '("id"           "run_id"        "testname"  "state"      "status"      "event_time"
				"host"         "cpuload"       "diskfree"  "uname"      "rundir"   "item_path"
                                "run_duration" "final_logf" "comment"   "shortdir"))

(define db:test-record-qry-selector (string-intersperse db:test-record-fields ","))

;; NOTE: Use db:test-get* to access records
;; NOTE: This needs rundir decoding? Decide, decode here or where used? For the moment decode where used.
(define (db:get-all-tests-info-by-run-id dbstruct run-id)
  (let ((db (db:get-db dbstruct run-id))
	(res '()))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir)
       ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir)
		       res)))
     (db:get-db dbstruct run-id)
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE run_id=?;")
     run-id)
    res))

(define (db:replace-test-records dbstruct run-id testrecs)
  (db:with-db dbstruct run-id #t 
	      (lambda (db)
		(let* ((qmarks (string-intersperse (make-list (length db:test-record-fields) "?") ","))
		       (qrystr (conc "INSERT OR REPLACE INTO tests (" db:test-record-qry-selector ") VALUES (" qmarks ");"))
		       (qry    (sqlite3:prepare db qrystr)))
		  ;; (debug:print 8 "INFO: replace-test-records, qrystr=" qrystr)
		  (for-each 
		   (lambda (rec)
		     (debug:print 0 "INFO: Inserting values: " (string-intersperse (map conc (vector->list rec)) ", "))
		     (apply sqlite3:execute qry (vector->list rec)))
		   testrecs)
		  (sqlite3:finalize! qry)))))
	

;; Get test data using test_id
(define (db:get-test-info-by-id dbstruct run-id test-id)
  (let ((db (db:get-db dbstruct run-id))
	(res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id)
	   ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id)))
     (db:get-db dbstruct run-id)
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id=?;")
	 test-id)
    res))

;; Use db:test-get* to access
;; Get test data using test_ids. NB// Only works within a single run!!
;;
(define (db:get-test-info-by-ids dbstruct run-id test-ids)
  (let ((db (db:get-db dbstruct run-id))
	(res '()))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id)
	   ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id)
			   res)))
     (db:get-db dbstruct run-id) 
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id in ("
	       (string-intersperse (map conc test-ids) ",") ");"))
    res))

(define (db:get-test-info dbstruct run-id testname item-path)
  (let ((db (db:get-db dbstruct run-id))
	(res #f))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (apply vector a b)))
     (db:get-db dbstruct run-id)
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE testname=? AND item_path=?;")
     test-name item-path)
    res))

(define (db:test-get-rundir-from-test-id dbstruct run-id test-id)
  (let ((db (db:get-db dbstruct run-id))
	(res #f))
    (sqlite3:for-each-row
     (lambda (tpath)
       (set! res tpath))
     (db:get-db dbstruct run-id)
     "SELECT rundir FROM tests WHERE id=?;"
     test-id)
    res))

;;======================================================================
;; S T E P S
;;======================================================================

(define (db:teststep-set-status! dbstruct run-id test-id teststep-name state-in status-in comment logfile)
  (let ((db (db:get-db dbstruct run-id)))
    (sqlite3:execute 
     db
     "INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment,logfile) VALUES(?,?,?,?,?,?,?);"
     test-id teststep-name state-in status-in (current-seconds)
     ;; (sdb:qry 'getid 
     (if comment comment "") ;; )
     ;; (sdb:qry 'getid  
     (if logfile logfile "")))) ;; )
   
;; db-get-test-steps-for-run
(define (db:get-steps-for-test dbstruct run-id test-id)
  (let* ((db (db:get-db dbstruct run-id))
	 (res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     db
     "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE status != 'DELETED' AND test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (reverse res)))

(define (db:get-steps-data dbstruct run-id test-id)
  (let ((db  (db:get-db dbstruct run-id))
	(res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     db
     "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE status != 'DELETED' AND test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (reverse res)))

;;======================================================================
;; T E S T  D A T A 
;;======================================================================

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup dbstruct run-id test-id status)
  (let ((db        (db:get-db dbstruct run-id))
	(fail-count 0)
	(pass-count 0))
    (sqlite3:for-each-row
     (lambda (fcount pcount)
       (set! fail-count fcount)
       (set! pass-count pcount))
     db 
     "SELECT (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail') AS fail_count,
             (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass') AS pass_count;"
     test-id test-id)
    ;; Now rollup the counts to the central megatest.db
    (db:general-call db 'pass-fail-counts (list pass-count fail-count test-id))
    ;; if the test is not FAIL then set status based on the fail and pass counts.
    (db:general-call db 'test_data-pf-rollup (list test-id test-id test-id test-id))))

(define (db:csv->test-data dbstruct run-id test-id csvdata)
  (debug:print 4 "test-id " test-id ", csvdata: " csvdata)
  (let ((db (db:get-db dbstruct run-id))
	(csvlist (csv->list (make-csv-reader
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
	     (let-values (((new-expected new-tol new-units)(tdb:get-prev-tol-for-test tdb test-id category variable)))
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

;;======================================================================
;; Misc. test related queries
;;======================================================================

(define (db:get-run-ids-matching-target dbstruct keynames target res runname testpatt statepatt statuspatt)
  (let* ((row-ids '())
	 (keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 ;; (testqry (tests:match->sqlqry testpatt))
	 (runsqry (sqlite3:prepare (db:get-db dbstruct #f)(conc "SELECT id FROM runs WHERE " keystr " AND runname LIKE '" runname "';"))))
    ;; (debug:print 8 "db:test-get-paths-matching-keynames-target-new\n  runsqry=" runsqry "\n  tstsqry=" testqry)
    (sqlite3:for-each-row
     (lambda (rid)
       (set! row-ids (cons rid row-ids)))
     runsqry)
    (sqlite3:finalize! runsqry)
    row-ids))

(define (db:test-get-paths-matching-keynames-target-new dbstruct run-id keynames target res testpatt statepatt statuspatt runname)
  (let* ((testqry (tests:match->sqlqry testpatt))
	 (tstsqry (conc "SELECT rundir FROM tests WHERE " testqry " AND state LIKE '" statepatt "' AND status LIKE '" statuspatt "' ORDER BY event_time ASC;")))
    (sqlite3:for-each-row 
     (lambda (p)
       (set! res (cons p res)))
     (db:get-db dbstruct run-id)
     tstsqry)
    res))

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

(define (db:test-set-status-state dbstruct run-id test-id status state msg)
  (let ((db  (db:get-db dbstruct run-id)))
  (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
      (db:general-call db 'set-test-start-time (list test-id)))
  (if msg
      (db:general-call db 'state-status-msg (list state status msg test-id))
	(db:general-call db 'state-status     (list state status test-id)))))

(define (db:roll-up-pass-fail-counts dbstruct run-id test-name item-path status)
  (if (and (not (equal? item-path ""))
	   (member status '("PASS" "WARN" "FAIL" "WAIVED" "RUNNING" "CHECK" "SKIP")))
      (let ((db (db:get-db dbstruct run-id)))
	(db:general-call db 'update-pass-fail-counts (list test-name test-name test-name))
	(if (equal? status "RUNNING")
	    (db:general-call db 'top-test-set-running (list test-name))
	    (db:general-call db 'top-test-set-per-pf-counts (list test-name test-name test-name)))
	#f)
      #f))

(define (db:tests-register-test dbstruct run-id test-name item-path)
  (sqlite3:execute (db:get-db dbstruct run-id) 'register-test run-id test-name item-path))

(define (db:test-get-logfile-info dbstruct run-id test-name)
  (let ((res #f))
    (sqlite3:for-each-row 
     (lambda (path final_logf)
       ;; (let ((path       (sdb:qry 'getstr path-id))
       ;;       (final_logf (sdb:qry 'getstr final_logf-id)))
       (set! logf final_logf)
       (set! res (list path final_logf))
       (if (directory? path)
	   (debug:print 2 "Found path: " path)
	   (debug:print 2 "No such path: " path))) ;; )
    (db:get-db dbstruct run-id)
     "SELECT rundir,final_logf FROM tests WHERE testname=? AND item_path='';"
     test-name)
    res))

;;======================================================================
;; A G R E G A T E D   T R A N S A C T I O N   D B   W R I T E S 
;;======================================================================

(define db:queries 
  (list '(update-run-duration     "UPDATE tests SET run_duration=? WHERE id=?;")

	;; TESTS
	'(register-test          "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');")
	;; Test state and status
	'(set-test-state         "UPDATE tests SET state=?   WHERE id=?;")
	'(set-test-status        "UPDATE tests SET state=?   WHERE id=?;")
	'(state-status           "UPDATE tests SET state=?,status=? WHERE id=?;") ;; DONE
	'(state-status-msg       "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;") ;; DONE
	;; Test comment
	'(set-test-comment       "UPDATE tests SET comment=? WHERE id=?;")
	'(set-test-start-time    "UPDATE tests SET event_time=strftime('%s','now') WHERE id=?;") ;; DONE
	'(pass-fail-counts       "UPDATE tests SET pass_count=?,fail_count=? WHERE id=?;")
	;; test_data-pf-rollup is used to set a tests PASS/FAIL based on the pass/fail info from the steps
	'(test_data-pf-rollup    "UPDATE tests
                                    SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                                      THEN 'FAIL'
                                    WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
                                      (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
                                    THEN 'PASS'
                                    ELSE status
                                    END WHERE id=?;") ;; DONE
	'(test-set-log            "UPDATE tests SET final_logf=? WHERE id=?;")      ;; DONE
	;; '(test-set-rundir-by-test-id "UPDATE tests SET rundir=? WHERE id=?")        ;; DONE
	;; '(test-set-rundir         "UPDATE tests SET rundir=? AND testname=? AND item_path=?;") ;; DONE
	'(test-set-rundir-shortdir "UPDATE tests SET rundir=?,shortdir=? WHERE testname=? AND item_path=?;")
	'(delete-tests-in-state   "DELETE FROM tests WHERE state=?;")                  ;; DONE
	'(tests:test-set-toplog   "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';")
	'(update-cpuload-diskfree "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;") ;; DONE
	'(update-uname-host       "UPDATE tests SET uname=?,host=? WHERE id=?;")       ;; DONE
	'(update-test-state       "UPDATE tests SET state=? WHERE state=? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	'(update-test-status      "UPDATE tests SET status=? WHERE status like ? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	;; stuff for roll-up-pass-fail-counts
	'(update-pass-fail-counts "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('FAIL','CHECK')),
                 pass_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('PASS','WARN','WAIVED'))
             WHERE testname=? AND item_path='';") ;; DONE
	'(top-test-set-running  "UPDATE tests SET state='RUNNING' WHERE testname=? AND item_path='';") ;; DONE
	'(top-test-set-per-pf-counts "UPDATE tests
                       SET state=CASE 
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE testname=?
                                                     AND item_path != '' 
                                                     AND state in ('RUNNING','NOT_STARTED','LAUNCHED','REMOTEHOSTSTART')) > 0 THEN 'RUNNING'
                                   ELSE 'COMPLETED' END,
                            status=CASE 
                                  WHEN fail_count > 0 THEN 'FAIL' 
                                  WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' 
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND status = 'SKIP') > 0 THEN 'SKIP'
                                  ELSE 'UNKNOWN' END
                       WHERE testname=? AND item_path='';") ;; DONE

	;; STEPS
	'(delete-test-step-records "UPDATE test_steps SET status='DELETED' WHERE id=?;")
	'(delete-test-data-records "UPDATE test_data  SET status='DELETED' WHERE id=?;") ;; using status since no state field
	))

(define (db:lookup-query qry-name)
  (let ((q (alist-ref qry-name db:queries)))
    (if q (car q) #f)))

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

(define (db:login dbstruct calling-path calling-version run-id client-signature)
  (cond 
   ((not (equal? calling-path *toppath*))
    (list #f "Login failed due to mismatch paths: " calling-path ", " *toppath*))
   ((not (equal? *run-id* run-id))
    (list #f "Login failed due to mismatch run-id: " run-id ", " *run-id*))
   ((not (equal? megatest-version calling-version))
    (list #f "Login failed due to mismatch megatest version: " calling-version ", " megatest-version))
   (else
    (hash-table-set! *logged-in-clients* client-signature (current-seconds))
    '(#t "successful login"))))

(define (db:general-call db stmtname params)
  (let ((query (let ((q (alist-ref (if (string? stmtname)
				       (string->symbol stmtname)
				       stmtname)
				   db:queries)))
 		 (if q (car q) #f))))
    (apply sqlite3:execute db query params)
    #t))

;; get the previous records for when these tests were run where all keys match but runname
;; NB// Merge this with test:get-previous-test-run-records? This one looks for all matching tests
;; can use wildcards. Also can likely be factored in with get test paths?
;;
;; Run this remotely!!
;;
(define (db:get-matching-previous-test-run-records dbstruct run-id test-name item-path)
  (let* ((db      (db:get-db dbstruct #f))
	 (keys    (db:get-keys db))
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
		(let ((results (db:get-tests-for-run dbstruct run-id hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f)))
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

(define (db:test-get-records-for-index-file dbstruct run-id test-name)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id itempath state status run_duration logf-id comment-id)
       (let ((logf    (db:get-string dbstruct logf-id))
	     (comment (db:get-string dbstruct comment-id)))
       (set! res (cons (vector id itempath state status run_duration logf comment) res)))
     (db:get-db dbstruct run-id)
     "SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE testname=? AND item_path != '';"
     test-name)
    res)))

;;======================================================================
;; Tests meta data
;;======================================================================

;; read the record given a testname
(define (db:testmeta-get-record dbstruct testname)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)
       (set! res (vector id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)))
     (db:get-db dbstruct #f)
     "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta WHERE testname=?;"
     testname)
    res))

;; create a new record for a given testname
(define (db:testmeta-add-record dbstruct testname)
  (sqlite3:execute (db:get-db dbstruct #f) "INSERT OR IGNORE INTO test_meta (testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags) VALUES (?,'','','','','','','','');" testname))

;; update one of the testmeta fields
(define (db:testmeta-update-field dbstruct testname field value)
  (sqlite3:execute (db:get-db dbstruct #f) (conc "UPDATE test_meta SET " field "=? WHERE testname=?;") value testname))

(define (db:testmeta-get-all dbstruct)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (cons (apply vector a b) res)))
     (db:get-db dbstruct run-id)
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
(define (db:get-prereqs-not-met dbstruct run-id waitons ref-item-path mode)
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
	   (let ((tests             (db:get-tests-for-run-state-status dbstruct run-id waitontest-name))
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

;; NOT REWRITTEN YET!!!!!

;; runspatt is a comma delimited list of run patterns
;; keypatt-alist must contain *all* keys with an associated pattern: '( ("KEY1" "%") .. )
(define (db:extract-ods-file dbstruct outputfile keypatt-alist runspatt pathmod)
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

