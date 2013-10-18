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

(define (db:get-db dbstruct run-id)
  (let ((db (if run-id
		(hash-table-ref/default (vector-ref dbstruct 1) run-id #f)
		(vector-ref dbstruct 0))))
    (if db
	db
	(let ((db (open-db run-id)))
	  (if run-id
	      (hash-table-set! (vector-ref dbstruct 1) run-id db)
	      (vector-set! dbstruct 0 db))
	  db))))

;;======================================================================
;; K E E P   F I L E D B   I N   dbstruct
;;======================================================================

(define (db:get-filedb dbstruct)
  (let ((db (vector-ref dbstruct 2)))
    (if db
	db
	(let ((fdb (filedb:open-db (conc *toplevel* "/db/files.db"))))
	  (vector-set! dbstruct 2 fdb)
	  fdb))))

;; Can also be used to save arbitrary strings
;;
(define (db:save-path dbstruct path)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:register-path fdb path)))

;; Use to get a path. To get an arbitrary string see next define
;;
(define (db:get-path dbstruct id)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:get-path db id)))

;;======================================================================
;; U S E   F I L E   D B   T O   S T O R E   S T R I N G S 
;;
;; N O T E ! !   T H I S   C L O B B E R S   M U L T I P L E  ////  T O  /
;;
;; Replace with something proper!
;;
;;======================================================================

;; Use to save a stored string, pad with _ to deal with trimming the prepending of /
;; 
(define (db:save-string dbstruct str)
  (let ((fdb (db:get-filedb dbstruct)))
    (filedb:register-path fdb (conc "_" str))))

;; Use to get a stored string
;;
(define (db:get-string dbstruct id)
  (let ((fdb (db:get-filedb dbstruct)))
    (string-drop (filedb:get-path fdb id) 2)))

;; This routine creates the db. It is only called if the db is not already opened
;;
(define (open-db dbstruct run-id) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (if (not *toppath*)
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "ERROR: Attempted to open db when not in megatest area. Exiting.")
	    (exit))))
  (let* ((dbpath       (if run-id 
			   (conc *toppath* "/db/" run-id ".db")
			   (let ((dbdir (conc *toppath* "/db"))) ;; use this opportunity to create our db dir
			     (if (not (directory-exists? dbdir))
				 (create-direcory dbdir))
			     (conc *toppath* "/megatest.db"))))
	 (dbexists     (file-exists? dbpath))
	 (write-access (file-write-access? dbpath))
	 (db           (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler      (make-busy-timeout (if (args:get-arg "-override-timeout")
					      (string->number (args:get-arg "-override-timeout"))
					      136000)))) ;; 136000))) ;; 136000 = 2.2 minutes
    (if (and dbexists
	     (not write-access))
	(set! *db-write-access* write-access)) ;; only unset so other db's also can use this control
    (debug:print-info 11 "open-db, dbpath=" dbpath " argv=" (argv))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(if (not run-id) ;; do the megatest.db
	    (db:initialize-megatest-db db)
	    (db:initialize-run-id-db   db run-id)))
    (sqlite3:execute db "PRAGMA synchronous = 0;")
    db))

;; close all opened run-id dbs
(define (db:close-all-db)
  (for-each
   (lambda (db)
     (finalize! db))
   (hash-table-values (vector-ref *open-dbs* 1)))
  (finalize! (vector-ref *open-dbs* 0)))

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

;; (define open-run-close 
(define open-run-close (if (debug:debug-mode 2)
			   open-run-close-no-exception-handling
			   open-run-close-exception-handling))

(define (db:initialize-megatest-db db)
  (let* ((configdat (car *configinfo*))  ;; tut tut, global warning...
	 (keys     (keys:configq-get-fields configdat))
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
    (db:set-var db "MEGATEST_VERSION" megatest-version)
    (debug:print-info 11 "db:initialize END")))

;;======================================================================
;; R U N   S P E C I F I C   D B 
;;======================================================================

(define (db:initialized-run-id-db db run-id)
  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS tests 
                              (id           INTEGER   PRIMARY KEY,
                               run_id       INTEGER,
                               testname     TEXT,
                               host         TEXT DEFAULT 'n/a',
                               cpuload      REAL DEFAULT -1,
                               diskfree     INTEGER DEFAULT -1,
                               uname        TEXT DEFAULT 'n/a', 
                               rundir_id    INTEGER,
                               realdir_id   INTEGER,
                               item_path    TEXT DEFAULT '',
                               state        TEXT DEFAULT 'NOT_STARTED',
                               status       TEXT DEFAULT 'FAIL',
                               attemptnum   INTEGER DEFAULT 0,
                               final_logf   TEXT DEFAULT 'logs/final.log',
                               logdat       BLOB, 
                               run_duration INTEGER DEFAULT 0,
                               comment      TEXT DEFAULT '',
                               event_time   TIMESTAMP,
                               fail_count   INTEGER DEFAULT 0,
                               pass_count   INTEGER DEFAULT 0,
                               archived     INTEGER DEFAULT 0, -- 0=no, 1=in progress, 2=yes
                        CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path));")
  (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname, item_path);")
  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_steps 
                              (id          INTEGER PRIMARY KEY,
                               test_id     INTEGER, 
                               stepname    TEXT, 
                               state       TEXT DEFAULT 'NOT_STARTED', 
                               status      TEXT DEFAULT 'n/a',
                               event_time  TIMESTAMP,
                               comment     TEXT DEFAULT '',
                               logfile     TEXT DEFAULT '',
                        CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data 
                              (id          INTEGER PRIMARY KEY,
                               test_id     INTEGER,
                               category    TEXT DEFAULT '',
                               variable    TEXT,
	                       value       REAL,
	                       expected    REAL,
	                       tol         REAL,
                               units       TEXT,
                               comment     TEXT DEFAULT '',
                               status      TEXT DEFAULT 'n/a',
                               type        TEXT DEFAULT '',
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
	(sqlite3:for-each-row 
	 (lambda (key)
	   (set! res (cons key res)))
	 (db:get-db dbstruct #f)
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


;; register a test run with the db
(define (db:register-run dbstruct keyvals runname state status user)
  (debug:print 3 "runs:register-run runname: " runname " state: " state " status: " status " user: " user)
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
  (let* ((db         (db:get-db dbstruct #f))
	 (res       '())
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
       (set! runs-info (cons (list runname run-id) runs-info)))
     (db:get-db dbstruct #f)
     "SELECT id,runname FROM runs;")
    ;; for each run get stats data
    (for-each
     (lambda (run-info)
       (let ((run-name (cadr run-info))
	     (run-id   (car  run-info)))
	 (sqlite3:for-each-row
	  (lambda (state count)
	    (let* ((stateparts (string-split state "|"))
		   (newstate   (conc (car stateparts) "\n" (cadr stateparts))))
	      (hash-table-set! totals newstate (+ (hash-table-ref/default totals newstate 0) count))
	      (set! res (cons (list runname newstate count) res))))
	  (db:get-db dbstruct run-id)
	  "SELECT state||'|'||status AS s,count(id) FROM tests AS t ON ORDER BY s DESC;" )
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
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
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
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt " ORDER BY event_time"
			(if limit  (conc " LIMIT " limit)   "")
			(if offset (conc " OFFSET " offset) "")
			";"))
    (debug:print-info 4 "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
    (sqlite3:for-each-row 
     (lambda (a . r)
       (set! res (cons (list->vector (cons a r)) res)))
     (db:get-db dbstruct #f)
     qry-str
     runnamepatt)
    (vector header res)))

;; use (get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
(define (db:get-run-info dbstruct run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
  (let* ((res      #f)
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
  (sqlite3:execute (db:get-db dbstruct #f) "UPDATE runs SET comment=? WHERE id=?;" comment run-id))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run dbstruct run-id)
  ;; (common:clear-caches) ;; don't trust caches after doing any deletion
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

(define (db:get-all-run-ids dbstruct)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (run-id)
       (set! res (cons run-id res)))
     (db:get-db dbstruct #f)
     "SELECT id FROM runs;")
    (reverse res)))

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
  (let ((mykeyvals (hash-table-ref/default *keyvals* run-id #f)))
    (if mykeyvals 
	mykeyvals
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
	    final-res)))))

;; The target is keyval1/keyval2..., cached in *target* as it is used often
(define (db:get-target dbstruct run-id)
  (let ((mytarg (hash-table-ref/default *target* run-id #f)))
    (if mytarg
	mytarg
	(let* ((keyvals (db:get-key-vals dbstruct run-id))
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
(define (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order
			      #!key
			      (qryvals #f)
			      )
  (let* ((qryvals         (if qryvals qryvals "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment"))
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
	 (qry             (conc "SELECT " qryvals
				" FROM tests WHERE AND state != 'DELETED' "
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
     (db:get-db dbstruct run-id)
     qry)
    res))


;; Convert calling routines to get list of run-ids and loop, do not use the get-tests-for-runs
;;

;; ;; ;; get a useful subset of the tests data (used in dashboard
;; ;; ;; use db:mintests-get-{id ,run_id,testname ...}
;; ;; (define (db:get-tests-for-runs-mindata dbstruct run-ids testpatt states status not-in)
;; ;;   (db:get-tests-for-runs dbstruct run-ids testpatt states status not-in: not-in qryvals: "id,run_id,testname,state,status,event_time,item_path"))
;; ;; 
;; ;; ;; NB // This is get tests for "runs" (note the plural!!)
;; ;; ;;
;; ;; ;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; ;; ;; i.e. these lists define what to NOT show.
;; ;; ;; states and statuses are required to be lists, empty is ok
;; ;; ;; not-in #t = above behaviour, #f = must match
;; ;; ;; run-ids is a list of run-ids or a single number or #f for all runs
;; ;; (define (db:get-tests-for-runs dbstruct run-ids testpatt states statuses 
;; ;; 			       #!key (not-in #t)
;; ;; 			       (sort-by #f)
;; ;; 			       (qryvals "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")) ;; 'rundir 'event_time
;; ;;   (let* ((res '())
;; ;; 	 ;; if states or statuses are null then assume match all when not-in is false
;; ;; 	 (states-qry      (if (null? states) 
;; ;; 			      #f
;; ;; 			      (conc " state "  
;; ;; 				    (if not-in "NOT" "") 
;; ;; 				    " IN ('" 
;; ;; 				    (string-intersperse states   "','")
;; ;; 				    "')")))
;; ;; 	 (statuses-qry    (if (null? statuses)
;; ;; 			      #f
;; ;; 			      (conc " status "
;; ;; 				    (if not-in "NOT" "") 
;; ;; 				    " IN ('" 
;; ;; 				    (string-intersperse statuses "','")
;; ;; 				    "')")))
;; ;; 	 (tests-match-qry (tests:match->sqlqry testpatt))
;; ;; 	 (qry             (conc "SELECT " qryvals 
;; ;; 				" FROM tests WHERE state != 'DELETED' "
;; ;; 				(if run-ids
;; ;; 				    (if (list? run-ids)
;; ;; 					(conc "AND run_id IN (" (string-intersperse (map conc run-ids) ",") ") ")
;; ;; 					(conc "AND run_id=" run-ids " "))
;; ;; 				    " ") ;; #f => run-ids don't filter on run-ids
;; ;; 				(if states-qry   (conc " AND " states-qry)   "")
;; ;; 				(if statuses-qry (conc " AND " statuses-qry) "")
;; ;; 				(if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
;; ;; 				(case sort-by
;; ;; 				  ((rundir)     " ORDER BY length(rundir) DESC;")
;; ;; 				  ((event_time) " ORDER BY event_time ASC;")
;; ;; 				  (else         ";"))
;; ;; 				)))
;; ;;     (debug:print-info 8 "db:get-tests-for-runs qry=" qry)
;; ;;     (sqlite3:for-each-row 
;; ;;      (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
;; ;;        (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
;; ;;      db 
;; ;;      qry
;; ;;      )
;; ;;     res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records dbstruct run-id test-id)
  (let ((db (db:get-db dbstruct run-id)))
    (sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" test-id)
    (sqlite3:execute db "DELETE FROM test_data WHERE test_id=?;" test-id)))

;; 
(define (db:delete-test-records dbstruct run-id test-id)
  (let ((db (db:get-db dbstruct run-id)))
    (sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" test-id)
    (sqlite3:execute db "DELETE FROM test_data WHERE test_id=?;" test-id)
    ;; (sqlite3:execute db "DELETE FROM tests WHERE id=?;" test-id)
    (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id)))

(define (db:delete-tests-for-run dbstruct run-id)
  (let ((db (db:get-db dbstruct run-id)))
    (sqlite3:execute db "DELETE FROM tests;")))

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
			       " testname=? AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
		;;(debug:print 0 "QRY: " qry)
		(sqlite3:execute (db:get-db dbstruct run-id) qry run-id newstate newstatus testname testname)))
	    testnames))


;; (define (cdb:set-tests-state-status-faster serverdat run-id testnames currstate currstatus newstate newstatus)
;;   ;; Convert #f to wildcard %
;;   (if (null? testnames)
;;       #t
;;       (let ((currstate  (if currstate currstate "%"))
;; 	    (currstatus (if currstatus currstatus "%")))
;; 	(let loop ((hed (car testnames))
;; 		   (tal (cdr testnames))
;; 		   (thr '()))
;; 	  (let ((th1 (if newstate  (create-thread (cbd:client-call serverdat 'update-test-state  #t *default-numtries* newstate  currstate  run-id testname testname)) #f))
;; 		(th2 (if newstatus (create-thread (cbd:client-call serverdat 'update-test-status #t *default-numtries* newstatus currstatus run-id testname testname)) #f)))
;; 	    (thread-start! th1)
;; 	    (thread-start! th2)
;; 	    (if (null? tal)
;; 		(loop (car tal)(cdr tal)(cons th1 (cons th2 thr)))
;; 		(for-each
;; 		 (lambda (th)
;; 		   (if th (thread-join! th)))
;; 		 thr)))))))

(define (db:delete-tests-in-state dbstruct run-id state)
  (sqlite3:execute (db:get-db dbstruct run-id)(db:lookup-query 'delete-tests-in-state) state))

(define (db:tests-update-cpuload-diskfree dbstruct run-id test-id cpuload diskfree)
  (sqlite3:execute (db:get-db dbstruct run-id)(db:lookup-query 'update-cpuload-diskfree) cpuload diskfree test-id))

(define (db:tests-update-run-duration dbstruct run-id test-id minutes)
  (sqlite3:execute (db:get-db dbstruct run-id)(db:lookup-query 'update-run-duration) minutes test-id))

(define (db:tests-update-uname-host dbstruct run-id test-id uname hostname)
  (sqlite3:execute (db:get-db dbstruct run-id)(db:lookup-query 'update-uname-host) uname hostname test-id))

;; speed up for common cases with a little logic
;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;;
(define (db:test-set-state-status-by-id dbstruct run-id test-id newstate newstatus newcomment)
  (let ((db (db:get-db dbstruct run-id)))
    (cond
     ((and newstate newstatus newcomment)
      (sqlite3:execute db "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;" newstate newstatus newcomment test-id))
     ((and newstate newstatus)
      (sqlite3:execute db "UPDATE tests SET state=?,status=? WHERE id=?;" newstate newstatus test-id))
     (else
      (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
      (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
      (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment test-id))))
    (mt:process-triggers run-id test-id newstate newstatus)))

;; Never used
;; (define (db:test-set-state-status-by-run-id-testname db run-id test-name item-path status state)
;;   (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
;; 		   state status run-id test-name item-path))

;; NEW BEHAVIOR: Count tests running in only one run!
;;
(define (db:get-count-tests-running dbstruct run-id)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     (db:get-db dbstruct run-id)
     "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART');")
    res))

;; NEW BEHAVIOR: Look only at single run with run-id
;; 
(define (db:get-running-stats dbstruct run-id)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (state count)
       (set! res (cons (list state count) res)))
     (db:get-db dbstruct run-id)
     "SELECT state,count(id) FROM tests GROUP BY state ORDER BY id DESC;")
    res))

(define (db:get-count-tests-running-in-jobgroup dbstruct run-id jobgroup)
  (if (not jobgroup)
      0 ;; 
      (let ((res 0))
	(sqlite3:for-each-row
	 (lambda (count)
	   (set! res count))
	 (db:get-db dbstruct run-id)
	 "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART'
             AND testname in (SELECT testname FROM test_meta WHERE jobgroup=?);"
	 jobgroup)
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
(define (db:get-test-id-not dbstruct run-id testname item-path)
  (let* ((res #f))
    (sqlite3:for-each-row
     (lambda (id) ;;  run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )
       (set! res id)) ;; (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment )))
     (db:get-db dbstruct run-id)
     "SELECT id FROM tests WHERE testname=? AND item_path=?;"
     testname item-path)
    res))

(define db:test-record-qry-selector "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir_id,item_path,run_duration,final_logf,comment,realdir_id")

;;
;; NOTE: Use db:test-get* to access records
;; 
;; NOTE: This needs rundir_id decoding? Decide, decode here or where used? For the moment decode where used.
;;
;; Get test data using test_id
(define (db:get-test-info-by-id dbstruct run-id test-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final_logf comment realdir-id)
       ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final_logf comment realdir-id)))
     (db:get-db dbstruct run-id)
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id=?;")
     test-id)
    res))

;; Use db:test-get* to access
;;
;; Get test data using test_ids. NB// Only works within a single run!!
;;
(define (db:get-test-info-by-ids dbstruct run-id test-ids)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final_logf comment realdir-id)
       ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final_logf comment realdir-id)
		       res)))
     (db:get-db dbstruct run-id) 
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id in ("
	   (string-intersperse (map conc test-ids) ",") ");"))
    res))

(define (db:get-test-info dbstruct run-id testname item-path)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (a . b)
       (set! res (apply vector a b)))
     (db:get-db dbstruct run-id)
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE testname=? AND item_path=?;")
     test-name item-path)
    res))

(define (db:test-set-comment dbstruct run-id test-id comment)
  (sqlite3:execute 
   (db:get-db dbstruct run-id)
   "UPDATE tests SET comment=? WHERE id=?;"
   comment test-id))

(define (db:test-set-rundir! dbstruct run-id test-name item-path rundir-id)
  (sqlite3:execute (db:get-db dbstruct run-id)(db:lookup-query 'test-set-rundir) test-name item-path))

(define (db:test-set-rundir-by-test-id dbstruct run-id test-id rundir-id)
  (sqlite3:execute (db:get-db dbstruct run-id) 'test-set-rundir-by-test-id rundir-id test-id))

(define (db:test-get-rundir-from-test-id dbstruct run-id test-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (tpath)
       (set! res tpath))
     (db:get-db dbstruct run-id)
     "SELECT rundir FROM tests WHERE id=?;"
     test-id)
    res))

(define (db:test-set-log! dbstruct run-id test-id logf-id)
  (if (string? logf)(sqlite3:execute (db:get-db dbstruct run-id) 'test-set-log logf-id test-id)))

;;======================================================================
;; Misc. test related queries
;;======================================================================

;; MUST BE CALLED local!
;;
(define (db:test-get-paths-matching dbstruct keynames target fnamepatt #!key (res '()))
  ;; BUG: Move the values derived from args to parameters and push to megatest.scm
  (let* ((testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (paths-from-db (db:test-get-paths-matching-keynames-target-new dbstruct keynames target res
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

(define (db:test-get-paths-matching-keynames-target-new dbstruct keynames target res 
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
	 (runsqry (sqlite3:prepare (db:get-db dbstruct #f)(conc "SELECT id FROM runs WHERE " keystr " AND runname LIKE '" runname "';")))
	 (tstsqry (conc "SELECT rundir_id FROM tests WHERE " testqry " AND state LIKE '" statepatt "' AND status LIKE '" statuspatt "' ORDER BY event_time ASC;")))
    (sqlite3:for-each-row
     (lambda (rid)
       (set! row-ids (cons rid row-ids)))
     runsqry)
    (for-each (lambda (rid)
		(sqlite3:for-each-row 
		 (lambda (p)
		   (set! res (cons p res)))
		 (db:get-db dbstruct rid)
		 tstsqry))
	      row-ids)
    ;; (sqlite3:finalize! tstsqry)
    (sqlite3:finalize! runsqry)
    res))

;; NEVER FINISHED? ;; look through tests from matching runs for a file
;; NEVER FINISHED? (define (db:test-get-first-path-matching dbstruct keynames target fname)
;; NEVER FINISHED?   ;; [refpaths] is the section where references to other megatest databases are stored
;; NEVER FINISHED? ;;
;; NEVER FINISHED? ;; NEED TO REVISIT THIS!!! BUGGISHNESS
;; NEVER FINISHED? ;;
;; NEVER FINISHED?   (let ((mt-paths (configf:get-section "refpaths"))
;; NEVER FINISHED? 	(res       (db:test-get-paths-matching dbstruct keynames target fname)))
;; NEVER FINISHED?     (let loop ((pathdat (if (null? paths) #f (car mt-paths)))
;; NEVER FINISHED? 	       (tal     (if (null? paths) '()(cdr mt-paths))))
;; NEVER FINISHED?       (if (not (null? res))
;; NEVER FINISHED? 	  (car res) ;; return first found
;; NEVER FINISHED? 	  (if path
;; NEVER FINISHED? 	      (let* ((db     (open-db path: (cadr pathdat)))
;; NEVER FINISHED? 		     (newres (db:test-get-paths-matching db keynames target fname)))
;; NEVER FINISHED? 		(debug:print-info 4 "Trying " (car pathdat) " at " (cadr pathdat))
;; NEVER FINISHED? 		(sqlite3:finalize! db)
;; NEVER FINISHED? 		(if (not (null? newres))
;; NEVER FINISHED? 		    (car newres)
;; NEVER FINISHED? 		    (if (null? tal)
;; NEVER FINISHED? 			#f
;; NEVER FINISHED? 			(loop (car tal)(cdr tal))))))))))

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
;; (define (cdb:client-call serverdat qtype immediate numretries . params)
;;   (debug:print-info 11 "cdb:client-call serverdat=" serverdat ", qtype=" qtype ", immediate=" immediate ", numretries=" numretries ", params=" params)
;;   (case *transport-type* 
;;     ((fs)
;;      (let ((packet (vector "na" qtype immediate "na" params 0)))
;;        (fs:process-queue-item packet)))
;;     ((http)
;;      (let* ((client-sig  (client:get-signature))
;; 	    (query-sig   (message-digest-string (md5-primitive) (conc qtype immediate params)))
;; 	    (zdat        (db:obj->string (vector client-sig qtype immediate query-sig params (current-seconds))))) ;; (with-output-to-string (lambda ()(serialize params))))
;;        (debug:print-info 11 "zdat=" zdat)
;;        (let* ((res  #f)
;; 	      (rawdat      (http-transport:client-send-receive serverdat zdat))
;; 	      (tmp         #f))
;; 	 (debug:print-info 11 "Sent " zdat ", received " rawdat)
;; 	 (if rawdat
;; 	     (begin
;; 	       (set! tmp (db:string->obj rawdat))
;; 	       (vector-ref tmp 2))
;; 	     (begin
;; 	       (debug:print 0 "ERROR: Communication with the server failed. Exiting if possible")
;; 	       (exit 1))))))
;;     ((zmq)
;;      (handle-exceptions
;;       exn
;;       (begin
;; 	(debug:print-info 0 "cdb:client-call timeout or error. Trying again in 5 seconds")
;; 	(thread-sleep! 5) 
;; 	(if (> numretries 0)(apply cdb:client-call serverdat qtype immediate (- numretries 1) params)))
;;       (let* ((push-socket (vector-ref serverdat 0))
;; 	     (sub-socket  (vector-ref serverdat 1))
;; 	     (client-sig  (client:get-signature))
;; 	     (query-sig   (message-digest-string (md5-primitive) (conc qtype immediate params)))
;; 	     (zdat        (db:obj->string (vector client-sig qtype immediate query-sig params (current-seconds)))) ;; (with-output-to-string (lambda ()(serialize params))))
;; 	     (res  #f)
;; 	     (send-receive (lambda ()
;; 			     (debug:print-info 11 "sending message")
;; 			     (send-message push-socket zdat)
;; 			     (debug:print-info 11 "message sent")
;; 			     (let loop ()
;; 			       ;; get the sender info
;; 			       ;; this should match (client:get-signature)
;; 			       ;; we will need to process "all" messages here some day
;; 			       (receive-message* sub-socket)
;; 			       ;; now get the actual message
;; 			       (let ((myres (db:string->obj (receive-message* sub-socket))))
;; 				 (if (equal? query-sig (vector-ref myres 1))
;; 				     (set! res (vector-ref myres 2))
;; 				     (loop)))))))
;; 	;; (timeout (lambda ()
;; 	;;     	(let loop ((n numretries))
;; 	;;     	  (thread-sleep! 15)
;; 	;;     	  (if (not res)
;; 	;;     	      (if (> numretries 0)
;; 	;;     		  (begin
;; 	;;     		    (debug:print 2 "WARNING: no reply to query " params ", trying resend")
;; 	;;     		    (debug:print-info 11 "re-sending message")
;; 	;;     		    (send-message push-socket zdat)
;; 	;;     		    (debug:print-info 11 "message re-sent")
;; 	;;     		    (loop (- n 1)))
;; 	;;     		  ;; (apply cdb:client-call *runremote* qtype immediate (- numretries 1) params))
;; 	;;     		  (begin
;; 	;;     		    (debug:print 0 "ERROR: cdb:client-call timed out " params ", exiting.")
;; 	;;     		    (exit 5))))))))
;; 	(debug:print-info 11 "Starting threads")
;; 	(let ((th1 (make-thread send-receive "send receive"))
;; 	      ;; (th2 (make-thread timeout      "timeout"))
;; 	      )
;; 	  (thread-start! th1)
;; 	  ;; (thread-start! th2)
;; 	  (thread-join!  th1)
;; 	  (debug:print-info 11 "cdb:client-call returning res=" res)
;; 	  res))))))

;; NOT NEEDED FOR NOW (define (cdb:set-verbosity serverdat val)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'set-verbosity #f *default-numtries* val))
;; NOT NEEDED FOR NOW 
;; NOT NEEDED FOR NOW (define (cdb:login serverdat keyval signature)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'login #t *default-numtries* keyval megatest-version signature))
;; NOT NEEDED FOR NOW 
;; NOT NEEDED FOR NOW (define (cdb:logout serverdat keyval signature)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'logout #t *default-numtries* keyval signature))
;; NOT NEEDED FOR NOW 
;; NOT NEEDED FOR NOW (define (cdb:num-clients serverdat)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'numclients #t *default-numtries*))
;; NOT NEEDED FOR NOW 

;; I think this would be more efficient if executed on client side FIXME???
(define (db:test-set-status-state dbstruct run-id test-id status state msg-id)
  (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
      (sqlite3:execute (db:get-db dbstruct run-id) 'set-test-start-time test-id))
  (if msg
      (sqlite3:execute (db:get-db dbstruct run-id) 'state-status-msg state status msg-id test-id)
      (sqlite3:execute (db:get-db dbstruct run-id) 'state-status state status test-id)))

(define (db:test-rollup-test_data-pass-fail dbstruct run-id test-id)
  (sqlite3:execute (db:get-db dbstruct run-id) 'test_data-pf-rollup test-id test-id test-id test-id))

(define (db:pass-fail-counts dbstruct run-id test-id fail-count pass-count)
  (sqlite3:execute (db:get-db dbstruct run-id) 'pass-fail-counts fail-count pass-count test-id))

(define (db:tests-register-test dbstruct run-id test-name item-path)
  (sqlite3:execute (db:get-db dbstruct run-id) 'register-test run-id test-name item-path))

;; more transactioned calls, these for roll-up-pass-fail stuff
(define (db:update-pass-fail-counts dbstruct run-id test-name)
  (sqlite3:execute (db:get-db dbstruct run-id) 'update-fail-pass-counts test-name test-name test-name))

(define (db:top-test-set-running dbstruct run-id test-name)
  (sqlite3:execute (db:get-db dbstruct run-id) 'top-test-set-running test-name))

(define (db:top-test-set-per-pf-counts dbstruct run-id test-name)
  (sqlite3:execute (db:get-db dbstruct run-id) 'top-test-set-per-pf-counts test-name test-name test-name))

;;=

;; NOT NEEDED FOR NOW (define (cdb:flush-queue serverdat)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'flush #f *default-numtries*))
;; NOT NEEDED FOR NOW 
;; NOT NEEDED FOR NOW (define (cdb:kill-server serverdat pid)
;; NOT NEEDED FOR NOW   (cdb:client-call serverdat 'killserver #t *default-numtries* pid))

;; (define (cdb:roll-up-pass-fail-counts serverdat run-id test-name item-path status)
;;  (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:roll-up-pass-fail-counts #f run-id test-name item-path status))
;; 
;; (define (db:get-test-info serverdat run-id test-name item-path)
;;   (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info #f run-id test-name item-path))
;; 
;; (define (cdb:get-test-info-by-id serverdat test-id)
;;   (let ((test-dat (cdb:client-call serverdat 'immediate #f *default-numtries* open-run-close db:get-test-info-by-id #f test-id)))
;;     (hash-table-set! *test-info* test-id (vector (current-seconds) test-dat)) ;; cached for use where up-to-date info is not needed
;;     test-dat))
;; 
;; ;; db should be db open proc or #f
;; (define (cdb:remote-run proc db . params)
;;   (apply cdb:client-call *runremote* 'immediate #f *default-numtries* open-run-close proc #f params))
;; 

(define (db:test-get-logfile-info dbstruct run-id test-name)
  (let ((res #f))
    (sqlite3:for-each-row 
     (lambda (path-id final_logf-id)
       (let ((path       (db:get-path   dbstruct path-id))
	     (final_logf (db:get-string dbstruct final_logf-id)))
	 (set! logf final_logf)
	 (set! res (list path final_logf))
	 (if (directory? path)
	     (debug:print 2 "Found path: " path)
	     (debug:print 2 "No such path: " path))))
     (db:get-db dbstruct run-id)
     "SELECT rundir_id,final_logf_id FROM tests WHERE testname=? AND item_path='';"
     test-name)
    res))

;;======================================================================
;; A G R E G A T E D   T R A N S A C T I O N   D B   W R I T E S 
;;======================================================================

(define db:queries 
  (list '(register-test          "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');") ;; DONE
	;; Test state and status
	'(set-test-state         "UPDATE tests SET state=?   WHERE id=?;")
	'(set-test-status        "UPDATE tests SET state=?   WHERE id=?;")
	'(state-status           "UPDATE tests SET state=?,status=? WHERE id=?;") ;; DONE
	'(state-status-msg       "UPDATE tests SET state=?,status=?,comment_id=? WHERE id=?;") ;; DONE
	;; Test comment
	'(set-test-comment       "UPDATE tests SET comment_id=? WHERE id=?;")
	'(set-test-start-time    "UPDATE tests SET event_time=strftime('%s','now') WHERE id=?;") ;; DONE
	'(pass-fail-counts       "UPDATE tests SET fail_count=?,pass_count=? WHERE id=?;") ;; DONE
	;; test_data-pf-rollup is used to set a tests PASS/FAIL based on the pass/fail info from the steps
	'(test_data-pf-rollup    "UPDATE tests
                                    SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                                      THEN 'FAIL'
                                    WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
                                      (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
                                    THEN 'PASS'
                                    ELSE status
                                    END WHERE id=?;") ;; DONE
	'(test-set-log            "UPDATE tests SET final_logf_id=? WHERE id=?;")      ;; DONE
	'(test-set-rundir-by-test-id "UPDATE tests SET rundir_id=? WHERE id=?")        ;; DONE
	'(test-set-rundir         "UPDATE tests SET rundir_id=? AND testname=? AND item_path=?;") ;; DONE
	'(delete-tests-in-state   "DELETE FROM tests WHERE state=?;")                  ;; DONE
	'(tests:test-set-toplog   "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';")
	'(update-cpuload-diskfree "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;") ;; DONE
	'(update-run-duration     "UPDATE tests SET run_duration=? WHERE id=?;")       ;; DONE
	'(update-uname-host       "UPDATE tests SET uname=?,host=? WHERE id=?;")       ;; DONE
	'(update-test-state       "UPDATE tests SET state=? WHERE state=? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	'(update-test-status      "UPDATE tests SET status=? WHERE status like ? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	;; stuff for roll-up-pass-fail-counts
	'(update-fail-pass-counts "UPDATE tests 
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

;; not used, intended to indicate to run in calling process
(define db:run-local-queries '()) ;; rollup-tests-pass-fail))

;; DISABLING FOR NOW (define (db:process-cached-writes db)
;; DISABLING FOR NOW   (let ((queries    (make-hash-table))
;; DISABLING FOR NOW 	(data       #f))
;; DISABLING FOR NOW     (mutex-lock! *incoming-mutex*)
;; DISABLING FOR NOW     ;; data is a list of query packets <vector qry-sig query params
;; DISABLING FOR NOW     (set! data (reverse *incoming-writes*)) ;;  (sort ... (lambda (a b)(< (vector-ref a 1)(vector-ref b 1)))))
;; DISABLING FOR NOW     (set! *server:last-write-flush* (current-milliseconds))
;; DISABLING FOR NOW     (set! *incoming-writes* '())
;; DISABLING FOR NOW     (mutex-unlock! *incoming-mutex*)
;; DISABLING FOR NOW     (if (> (length data) 0)
;; DISABLING FOR NOW 	;; Process if we have data
;; DISABLING FOR NOW 	(begin
;; DISABLING FOR NOW 	  (debug:print-info 7 "Writing cached data " data)
;; DISABLING FOR NOW 	  
;; DISABLING FOR NOW 	  ;; Prepare the needed sql statements
;; DISABLING FOR NOW 	  ;;
;; DISABLING FOR NOW 	  (for-each (lambda (request-item)
;; DISABLING FOR NOW 		      (let ((stmt-key (vector-ref request-item 0))
;; DISABLING FOR NOW 			    (query    (vector-ref request-item 1)))
;; DISABLING FOR NOW 			(hash-table-set! queries stmt-key (sqlite3:prepare db query))))
;; DISABLING FOR NOW 		    data)
;; DISABLING FOR NOW 	  
;; DISABLING FOR NOW 	  ;; No outer loop needed. Single loop for write items only. Reads trigger flush of queue
;; DISABLING FOR NOW 	  ;; and then are executed.
;; DISABLING FOR NOW 	  (sqlite3:with-transaction 
;; DISABLING FOR NOW 	   db
;; DISABLING FOR NOW 	   (lambda ()
;; DISABLING FOR NOW 	     (for-each
;; DISABLING FOR NOW 	      (lambda (hed)
;; DISABLING FOR NOW 		(let* ((params   (vector-ref hed 2))
;; DISABLING FOR NOW 		       (stmt-key (vector-ref hed 0))
;; DISABLING FOR NOW 		       (stmt     (hash-table-ref/default queries stmt-key #f)))
;; DISABLING FOR NOW 		  (if stmt
;; DISABLING FOR NOW 		      (apply sqlite3:execute stmt params)
;; DISABLING FOR NOW 		      (debug:print 0 "ERROR: Problem Executing " stmt-key " for " params))))
;; DISABLING FOR NOW 	      data)))
;; DISABLING FOR NOW 	  
;; DISABLING FOR NOW 	  ;; let all the waiting calls know all is done
;; DISABLING FOR NOW 	  (mutex-lock! *completed-mutex*)
;; DISABLING FOR NOW 	  (for-each (lambda (item)
;; DISABLING FOR NOW 		      (let ((qry-sig (cdb:packet-get-client-sig item)))
;; DISABLING FOR NOW 			(debug:print-info 7 "Registering query " qry-sig " as done")
;; DISABLING FOR NOW 			(hash-table-set! *completed-writes* qry-sig #t)))
;; DISABLING FOR NOW 		    data)
;; DISABLING FOR NOW 	  (mutex-unlock! *completed-mutex*)
;; DISABLING FOR NOW 	  
;; DISABLING FOR NOW 	  ;; Finalize the statements. Should this be done inside the mutex above?
;; DISABLING FOR NOW 	  ;; I think sqlite3 mutexes will keep the data safe
;; DISABLING FOR NOW 	  (for-each (lambda (stmt-key)
;; DISABLING FOR NOW 		      (sqlite3:finalize! (hash-table-ref queries stmt-key)))
;; DISABLING FOR NOW 		    (hash-table-keys queries))
;; DISABLING FOR NOW 	  
;; DISABLING FOR NOW 	  ;; Do a little record keeping
;; DISABLING FOR NOW 	  (let ((cache-size (length data)))
;; DISABLING FOR NOW 	    (if (> cache-size *max-cache-size*)
;; DISABLING FOR NOW 		(set! *max-cache-size* cache-size)))
;; DISABLING FOR NOW 	  #t)
;; DISABLING FOR NOW 	#f)))
;; DISABLING FOR NOW 
;; DISABLING FOR NOW (define *db:process-queue-mutex* (make-mutex))
;; DISABLING FOR NOW 
;; DISABLING FOR NOW (define *number-of-writes*         0)
;; DISABLING FOR NOW (define *writes-total-delay*       0)
;; DISABLING FOR NOW (define *total-non-write-delay*    0)
;; DISABLING FOR NOW (define *number-non-write-queries* 0)
;; DISABLING FOR NOW 
;; DISABLING FOR NOW ;; The queue is a list of vectors where the zeroth slot indicates the type of query to
;; DISABLING FOR NOW ;; apply and the second slot is the time of the query and the third entry is a list of 
;; DISABLING FOR NOW ;; values to be applied
;; DISABLING FOR NOW ;;
;; DISABLING FOR NOW (define (db:queue-write-and-wait db qry-sig query params)
;; DISABLING FOR NOW   (let ((queue-len  0)
;; DISABLING FOR NOW 	(res        #f)
;; DISABLING FOR NOW 	(got-it     #f)
;; DISABLING FOR NOW 	(qry-pkt    (vector qry-sig query params))
;; DISABLING FOR NOW 	(start-time (current-milliseconds))
;; DISABLING FOR NOW 	(timeout    (+ 10 (current-seconds)))) ;; set the time out to 10 secs in future
;; DISABLING FOR NOW 
;; DISABLING FOR NOW     ;; Put the item in the queue *incoming-writes* 
;; DISABLING FOR NOW     (mutex-lock! *incoming-mutex*)
;; DISABLING FOR NOW     (set! *incoming-writes* (cons qry-pkt *incoming-writes*))
;; DISABLING FOR NOW     (set! queue-len (length *incoming-writes*))
;; DISABLING FOR NOW     (mutex-unlock! *incoming-mutex*)
;; DISABLING FOR NOW 
;; DISABLING FOR NOW     (debug:print-info 7 "Current write queue length is " queue-len)
;; DISABLING FOR NOW 
;; DISABLING FOR NOW     ;; poll for the write to complete, timeout after 10 seconds
;; DISABLING FOR NOW     ;; periodic flushing of the queue is taken care of by 
;; DISABLING FOR NOW     ;; db:flush-queue
;; DISABLING FOR NOW     (let loop ()
;; DISABLING FOR NOW       (thread-sleep! 0.001)
;; DISABLING FOR NOW       (mutex-lock! *completed-mutex*)
;; DISABLING FOR NOW       (if (hash-table-ref/default *completed-writes* qry-sig #f)
;; DISABLING FOR NOW 	  (begin
;; DISABLING FOR NOW 	    (hash-table-delete! *completed-writes* qry-sig)
;; DISABLING FOR NOW 	    (set! got-it #t)))
;; DISABLING FOR NOW       (mutex-unlock! *completed-mutex*)
;; DISABLING FOR NOW       (if (and (not got-it)
;; DISABLING FOR NOW 	       (< (current-seconds) timeout))
;; DISABLING FOR NOW 	  (begin
;; DISABLING FOR NOW 	    (thread-sleep! 0.01)
;; DISABLING FOR NOW 	    (loop))))
;; DISABLING FOR NOW     (set! *number-of-writes*   (+ *number-of-writes*   1))
;; DISABLING FOR NOW     (set! *writes-total-delay* (+ *writes-total-delay* (- (current-milliseconds) start-time)))
;; DISABLING FOR NOW     got-it))
;; DISABLING FOR NOW 
;; DISABLING FOR NOW (define (db:process-queue-item db item)
;; DISABLING FOR NOW   (let* ((stmt-key       (cdb:packet-get-qtype item))
;; DISABLING FOR NOW 	 (qry-sig        (cdb:packet-get-query-sig item))
;; DISABLING FOR NOW 	 (return-address (cdb:packet-get-client-sig item))
;; DISABLING FOR NOW 	 (params         (cdb:packet-get-params item))
;; DISABLING FOR NOW 	 (query          (let ((q (alist-ref stmt-key db:queries)))
;; DISABLING FOR NOW 			   (if q (car q) #f))))
;; DISABLING FOR NOW     (debug:print-info 11 "Special queries/requests stmt-key=" stmt-key ", return-address=" return-address ", query=" query ", params=" params)
;; DISABLING FOR NOW     (if query
;; DISABLING FOR NOW 	;; hand queries off to the write queue
;; DISABLING FOR NOW 	(let ((response (case *transport-type*
;; DISABLING FOR NOW 			  ((http)
;; DISABLING FOR NOW 			   (debug:print-info 7 "Queuing item " item " for wrapped write")
;; DISABLING FOR NOW 			   (db:queue-write-and-wait db qry-sig query params))
;; DISABLING FOR NOW 			  (else  
;; DISABLING FOR NOW 			   (apply sqlite3:execute db query params)
;; DISABLING FOR NOW 			   #t))))
;; DISABLING FOR NOW 	  (debug:print-info 7 "Received " response " from wrapped write")
;; DISABLING FOR NOW 	  (server:reply return-address qry-sig response response))
;; DISABLING FOR NOW 	;; otherwise if appropriate flush the queue (this is a read or complex query)
;; DISABLING FOR NOW 	(begin
;; DISABLING FOR NOW 	  (cond
;; DISABLING FOR NOW 	   ((member stmt-key db:special-queries)
;; DISABLING FOR NOW 	    (let ((starttime (current-milliseconds)))
;; DISABLING FOR NOW 	      (debug:print-info 9 "Handling special statement " stmt-key)
;; DISABLING FOR NOW 	      (case stmt-key
;; DISABLING FOR NOW 		((immediate)
;; DISABLING FOR NOW 		 ;; This is a read or mixed read-write query, must clear the cache
;; DISABLING FOR NOW 		 (case *transport-type*
;; DISABLING FOR NOW 		   ((http)
;; DISABLING FOR NOW 		    (mutex-lock! *db:process-queue-mutex*)
;; DISABLING FOR NOW 		    (db:process-cached-writes db)
;; DISABLING FOR NOW 		    (mutex-unlock! *db:process-queue-mutex*)))
;; DISABLING FOR NOW 		 (let* ((proc      (car params))
;; DISABLING FOR NOW 			(remparams (cdr params))
;; DISABLING FOR NOW 			;; we are being handed a procedure so call it
;; DISABLING FOR NOW 			;; (debug:print-info 11 "Running (apply " proc " " remparams ")")
;; DISABLING FOR NOW 			(result (server:reply return-address qry-sig #t (apply proc remparams))))
;; DISABLING FOR NOW 		   (set! *total-non-write-delay* (+ *total-non-write-delay* (- (current-milliseconds) starttime))) 
;; DISABLING FOR NOW 		   (set! *number-non-write-queries* (+ *number-non-write-queries* 1))
;; DISABLING FOR NOW 		   result))
;; DISABLING FOR NOW 		((login)
;; DISABLING FOR NOW 		 (if (< (length params) 3) ;; should get toppath, version and signature
;; DISABLING FOR NOW 		     (server:reply return-address qry-sig '(#f "login failed due to missing params")) ;; missing params
;; DISABLING FOR NOW 		     (let ((calling-path (car   params))
;; DISABLING FOR NOW 			   (calling-vers (cadr  params))
;; DISABLING FOR NOW 			   (client-key   (caddr params)))
;; DISABLING FOR NOW 		       (if (and (equal? calling-path *toppath*)
;; DISABLING FOR NOW 				(equal? megatest-version calling-vers))
;; DISABLING FOR NOW 			   (begin
;; DISABLING FOR NOW 			     (hash-table-set! *logged-in-clients* client-key (current-seconds))
;; DISABLING FOR NOW 			     (server:reply return-address qry-sig #t '(#t "successful login")))      ;; path matches - pass! Should vet the caller at this time ...
;; DISABLING FOR NOW 			   (server:reply return-address qry-sig #f (list #f (conc "Login failed due to mismatch paths: " calling-path ", " *toppath*)))))))
;; DISABLING FOR NOW 		((flush sync)
;; DISABLING FOR NOW 		 (server:reply return-address qry-sig #t 1)) ;; (length data)))
;; DISABLING FOR NOW 		((set-verbosity)
;; DISABLING FOR NOW 		 (set! *verbosity* (car params))
;; DISABLING FOR NOW 		 (server:reply return-address qry-sig #t (list #t *verbosity*)))
;; DISABLING FOR NOW 		((killserver)
;; DISABLING FOR NOW 		 (let ((hostname (car  *runremote*))
;; DISABLING FOR NOW 		       (port     (cadr *runremote*))
;; DISABLING FOR NOW 		       (pid      (car params)))
;; DISABLING FOR NOW 		   (debug:print 0 "WARNING: Server on " hostname ":" port " going down by user request!")
;; DISABLING FOR NOW 		   (debug:print-info 1 "current pid=" (current-process-id))
;; DISABLING FOR NOW 		   (open-run-close tasks:server-deregister tasks:open-db 
;; DISABLING FOR NOW 				   hostname
;; DISABLING FOR NOW 				   port: port)
;; DISABLING FOR NOW 		   (set! *server-run* #f)
;; DISABLING FOR NOW 		   (thread-sleep! 3)
;; DISABLING FOR NOW 		   (process-signal pid signal/kill)
;; DISABLING FOR NOW 		   (server:reply return-address qry-sig #t '(#t "exit process started"))))
;; DISABLING FOR NOW 		(else ;; not a command, i.e. is a query
;; DISABLING FOR NOW 		 (debug:print 0 "ERROR: Unrecognised query/command " stmt-key)
;; DISABLING FOR NOW 		 (server:reply return-address qry-sig #f 'failed)))))
;; DISABLING FOR NOW 	   (else
;; DISABLING FOR NOW 	    (debug:print-info 11 "Executing " stmt-key " for " params)
;; DISABLING FOR NOW 	    (apply sqlite3:execute (hash-table-ref queries stmt-key) params)
;; DISABLING FOR NOW 	    (server:reply return-address qry-sig #t #t)))))))
;; DISABLING FOR NOW 

(define (db:test-get-records-for-index-file dbstruct run-id test-name)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id itempath state status run_duration logf-id comment-id)
       (let ((logf    (db:get-string dbstruct logf-id))
	     (comment (db:get-string dbstruct comment-id)))
	 (set! res (cons (vector id itempath state status run_duration logf comment) res)))
     (db:get-db dbstruct run-id)
     "SELECT id,item_path,state,status,run_duration,final_logf_id,comment_id FROM tests WHERE testname=? AND item_path != '';"
     test-name)
    res)))

;;======================================================================
;; Tests meta data
;;======================================================================

;; read the record given a testname
(define (db:testmeta-get-record dbstruct testname)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id testname author owner description reviewed iterated avg_runtime avg_disk tags)
       (set! res (vector id testname author owner description reviewed iterated avg_runtime avg_disk tags)))
     (db:get-db dbstruct #f)
     "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags FROM test_meta WHERE testname=?;"
     testname)
    res))

;; create a new record for a given testname
(define (db:testmeta-add-record dbstruct testname)
  (sqlite3:execute (db:get-db dbstruct #f) "INSERT OR IGNORE INTO test_meta (testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags) VALUES (?,'','','','','','','','');" testname))

;; update one of the testmeta fields
(define (db:testmeta-update-field dbstruct testname field value)
  (sqlite3:execute (db:get-db dbstruct #f) (conc "UPDATE test_meta SET " field "=? WHERE testname=?;") value testname))

;;======================================================================
;; T E S T   D A T A 
;;======================================================================

(define (db:csv->test-data dbstruct run-id test-id csvdata)
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
	     (let-values (((new-expected new-tol new-units)(db:get-prev-tol-for-test dbstruct run-id test-id category variable)))
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
	 (sqlite3:execute (db:get-db dbstruct run-id) "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status,type) VALUES (?,?,?,?,?,?,?,?,?,?);"
			  test-id category variable value expected tol units (if comment comment "") status type)))
     csvlist)))

;; get a list of test_data records matching categorypatt
(define (db:read-test-data dbstruct run-id test-id categorypatt)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status type)
       (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
     (db:get-db dbstruct run-id)
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (reverse res)))

;; NOTE: Run this local with #f for db !!!
(define (db:load-test-data dbstruct run-id test-id)
  (let loop ((lin (read-line)))
    (if (not (eof-object? lin))
	(begin
	  (debug:print 4 lin)
	  (db:csv->test-data dbstruct run-id test-id lin)
	  (loop (read-line)))))
  ;; roll up the current results.
  ;; FIXME: Add the status to 
  (db:test-data-rollup dbstruct run-id test-id #f))

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup dbstruct run-id test-id status)
  (let ((fail-count 0)
	(pass-count 0))
    (sqlite3:for-each-row
     (lambda (fcount pcount)
       (set! fail-count fcount)
       (set! pass-count pcount))
     (db:get-db dbstruct run-id)
     "SELECT (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail') AS fail_count,
                   (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass') AS pass_count;"
     test-id test-id)
    
    ;; Now rollup the counts to the central megatest.db
    (cdb:pass-fail-counts *runremote* test-id fail-count pass-count)
    ;; if the test is not FAIL then set status based on the fail and pass counts.
    (cdb:test-rollup-test_data-pass-fail *runremote* test-id)))

(define (db:get-prev-tol-for-test dbstruct run-id test-id category variable)
  ;; Finish me?
  (values #f #f #f))

;;======================================================================
;; S T E P S 
;;======================================================================

(define (db:step-get-time-as-string vec)
  (seconds->time-string (db:step-get-event_time vec)))

;; db-get-test-steps-for-run
(define (db:get-steps-for-test dbstruct run-id test-id)
  (let ((res '()))

    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     (db:get-db dbstruct run-id)
     "SELECT id,test_id,stepname,state,status,event_time,logfile_id FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (reverse res)))

;; get a pretty table to summarize steps
;;
(define (db:get-steps-table dbstruct run-id test-id)
  (let ((steps   (db:get-steps-for-test dbstruct run-id test-id)))
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

;; ;; get a pretty table to summarize steps
;; ;;
;; (define (db:get-steps-table-list dbstruct run-id test-id #!key (work-area #f))
;;   (let ((steps   (db:get-steps-for-test dbstruct run-id test-id)))
;;     ;; organise the steps for better readability
;;     (let ((res (make-hash-table)))
;;       (for-each 
;;        (lambda (step)
;; 	 (debug:print 6 "step=" step)
;; 	 (let ((record (hash-table-ref/default 
;; 			res 
;; 			(db:step-get-stepname step) 
;; 			;;        stepname                start end status    
;; 			(vector (db:step-get-stepname step) ""   "" ""     "" ""))))
;; 	   (debug:print 6 "record(before) = " record 
;; 			"\nid:       " (db:step-get-id step)
;; 			"\nstepname: " (db:step-get-stepname step)
;; 			"\nstate:    " (db:step-get-state step)
;; 			"\nstatus:   " (db:step-get-status step)
;; 			"\ntime:     " (db:step-get-event_time step))
;; 	   (case (string->symbol (db:step-get-state step))
;; 	     ((start)(vector-set! record 1 (db:step-get-event_time step))
;; 	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
;; 					(db:step-get-status step)))
;; 	      (if (> (string-length (db:step-get-logfile step))
;; 		     0)
;; 		  (vector-set! record 5 (db:step-get-logfile step))))
;; 	     ((end)  
;; 	      (vector-set! record 2 (any->number (db:step-get-event_time step)))
;; 	      (vector-set! record 3 (db:step-get-status step))
;; 	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
;; 					  (endt   (any->number (vector-ref record 2))))
;; 				      (debug:print 4 "record[1]=" (vector-ref record 1) 
;; 						   ", startt=" startt ", endt=" endt
;; 						   ", get-status: " (db:step-get-status step))
;; 				      (if (and (number? startt)(number? endt))
;; 					  (seconds->hr-min-sec (- endt startt)) "-1")))
;; 	      (if (> (string-length (db:step-get-logfile step))
;; 		     0)
;; 		  (vector-set! record 5 (db:step-get-logfile step))))
;; 	     (else
;; 	      (vector-set! record 2 (db:step-get-state step))
;; 	      (vector-set! record 3 (db:step-get-status step))
;; 	      (vector-set! record 4 (db:step-get-event_time step))))
;; 	   (hash-table-set! res (db:step-get-stepname step) record)
;; 	   (debug:print 6 "record(after)  = " record 
;; 			"\nid:       " (db:step-get-id step)
;; 			"\nstepname: " (db:step-get-stepname step)
;; 			"\nstate:    " (db:step-get-state step)
;; 			"\nstatus:   " (db:step-get-status step)
;; 			"\ntime:     " (db:step-get-event_time step))))
;;        ;; (else   (vector-set! record 1 (db:step-get-event_time step)))
;;        (sort steps (lambda (a b)
;; 		     (cond
;; 		      ((<   (db:step-get-event_time a)(db:step-get-event_time b)) #t)
;; 		      ((eq? (db:step-get-event_time a)(db:step-get-event_time b)) 
;; 		       (<   (db:step-get-id a)        (db:step-get-id b)))
;; 		      (else #f)))))
;;       res)))

(define (db:get-compressed-steps dbstruct run-id test-id)
  (let ((comprsteps (open-run-close db:get-steps-table (db:get-db dbstruct run-id) test-id)))
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
		       (string<? (conc time-a)(conc time-b)))))))))

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
(define (db:get-prereqs-not-met dbstruct run-id waitons ref-item-path #!key (mode 'normal))
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   (let ((tests             (mt:get-tests-for-run dbstruct run-id waitontest-name '() '()))
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

(define (db:teststep-set-status! dbstruct run-id test-id teststep-name state-in status-in comment logfile #!key (work-area #f))
  (let ((state     (items:check-valid-items "state" state-in))
	(status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (sqlite3:execute 
     (db:get-db dbstruct run-id)
     "INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment_id,logfile_id) VALUES(?,?,?,?,?,?,?);"
     test-id teststep-name state-in status-in (current-seconds) 
     (db:save-string (if comment comment ""))
     (db:save-string (if logfile logfile "")))
    #t))

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
