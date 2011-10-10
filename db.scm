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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml)
(import (prefix sqlite3 sqlite3:))

(declare (unit db))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

(define (open-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((dbpath    (conc *toppath* "/megatest.db")) ;; fname)
	 (configdat (car *configinfo*))
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler   (make-busy-timeout 36000)))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(let* ((keys     (config-get-fields configdat))
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
                     first_err  TEXT,
                     first_warn TEXT,
                     CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path)
          );")
	  (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname);")
	  (sqlite3:execute db "CREATE VIEW runs_tests AS SELECT * FROM runs INNER JOIN tests ON runs.id=tests.run_id;")
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_steps 
                              (id INTEGER PRIMARY KEY,
                               test_id INTEGER, 
                               stepname TEXT, 
                               state TEXT DEFAULT 'NOT_STARTED', 
                               status TEXT DEFAULT 'n/a',event_time TIMESTAMP,
                               comment TEXT DEFAULT '',
                               CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS extradat (id INTEGER PRIMARY KEY, run_id INTEGER, key TEXT, val TEXT);")
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")
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
                              CONSTRAINT test_data UNIQUE (test_id,category,variable));")
	  ;; Must do this *after* running patch db !! No more. 
	  (db:set-var db "MEGATEST_VERSION" megatest-version)
	  ))
    db))

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
       (for-each 
	(lambda (stmt)
	  (sqlite3:execute db stmt))
	(list 
	 "ALTER TABLE tests ADD COLUMN first_err TEXT;"
	 "ALTER TABLE tests ADD COLUMN first_warn TEXT;"
	 ))
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
       (patch-db))
      ((< mver 1.27)
       (db:set-var db "MEGATEST_VERSION" 1.27)
       (sqlite3:execute db "ALTER TABLE test_data ADD COLUMN type TEXT DEFAULT '';")
       (patch-db))
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

(define (db-get-keys db)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(sqlite3:for-each-row 
	 (lambda (key keytype)
	   (set! res (cons (vector key keytype) res)))
	 db
	 "SELECT fieldname,fieldtype FROM keys ORDER BY id DESC;")
	(set! *db-keys* res)
	res)))

(define db:get-keys db-get-keys)

(define (db:get-value-by-header row header field)
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
	 (keys      (db-get-keys db))
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
    (debug:print 4 "db:get-runs qrystr: " qrystr "\nkeypatts: " keypatts)
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
	 (keys      (db-get-keys db))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (apply vector a x)))
     db
     (conc "SELECT " keystr " FROM runs WHERE id=?;")
     run-id)
    (vector header res)))

(define (db:set-comment-for-run db run-id comment)
  (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment run-id))

(define (db:delete-run db run-id)
  (sqlite3:execute db "DELETE FROM runs WHERE id=?;" run-id))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
(define (db-get-tests-for-run db run-id testpatt itempatt states statuses)
  (let ((res '())
	(states-str   (if (and states (not (null? states)))
			  (conc " AND state NOT IN ('" (string-intersperse states   "','") "')") ""))
	(statuses-str (if (and statuses (not (null? statuses)))
			  (conc " AND status NOT IN ('" (string-intersperse statuses "','") "')") "")))
    (sqlite3:for-each-row 
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment first-err first-warn)
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment first-err first-warn) res)))
     db 
     (conc "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment,first_err,first_warn "
	   " FROM tests WHERE run_id=? AND testname like ? AND item_path LIKE ? "
	   states-str statuses-str
	   " ORDER BY id DESC;")
     run-id
     (if testpatt testpatt "%")
     (if itempatt itempatt "%"))
    res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records db run-id test-name itemdat)
  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id in (SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?);" 
		   run-id test-name (item-list->path itemdat)))
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

(define (db:get-count-tests-running db)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART';")
    res))

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
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment first-err first-warn)
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment first-err first-warn)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment,first_err,first_warn FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
     run-id testname item-path)
    res))

;; Get test data using test_id
(define (db:get-test-data-by-id db test-id)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment first-err first-warn)
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment first-err first-warn)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment,first_err,first_warn FROM tests WHERE id=?;"
     test-id)
    res))


(define (db:test-set-comment db run-id testname item-path comment)
  (sqlite3:execute 
   db 
   "UPDATE tests SET comment=? WHERE run_id=? AND testname=? AND item_path=?;"
     comment run-id testname item-path))

;;
(define (db:test-set-rundir! db run-id testname item-path rundir)
  (sqlite3:execute 
   db 
   "UPDATE tests SET rundir=? WHERE run_id=? AND testname=? AND item_path=?;"
     rundir run-id testname item-path))

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
       (let* ((padded-row  (take (append csvrow (list #f #f #f #f #f #f #f #f)) 8))
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
				 s)))) ;; if specified on the input then use, else calculate
	 ;; look up expected,tol,units from previous best fit test if they are all either #f or ''
	 (debug:print 4 "BEFORE: category: " category " variable: " variable " value: " value 
		      ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)

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
	 (sqlite3:execute db "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status) VALUES (?,?,?,?,?,?,?,?,?);"
	      test-id category variable value expected tol units (if comment comment "") status)))
     csvlist)))

;; get a list of test_data records matching categorypatt
(define (db:read-test-data db test-id categorypatt)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status)
       (set! res (cons (vector id test_id category variable value expected tol units comment status) res)))
     db
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (reverse res)))

(define (db:load-test-data db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat (db:get-test-info db run-id test-name item-path))
	 (test-id (if testdat (db:test-get-id testdat) #f)))
    ;; (debug:print 1 "Enter records to insert in the test_data table, seven fields, comma separated per line")
    (debug:print 4 "itemdat: " itemdat ", test-name: " test-name ", test-id: " test-id)
    (if test-id
	(let loop ((lin (read-line)))
	  (if (not (eof-object? lin))
	      (begin
		(debug:print 4 lin)
		(db:csv->test-data db test-id lin)
		(loop (read-line))))))
    ;; roll up the current results.
    (db:test-data-rollup db test-id)))
  
;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup db test-id)
  (sqlite3:execute 
   db 
   "UPDATE tests 
      SET fail_count=(SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail'),
          pass_count=(SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass')
      WHERE id=?;"
   test-id test-id test-id)
  ;; if the test is not FAIL then set status based on the fail and pass counts.
  (sleep 1)
  (sqlite3:execute
   db
   "UPDATE tests
      SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                         THEN 'FAIL'
                      WHEN (SELECT pass_count FROM tests WHERE id=?) > 0
                         THEN 'PASS'
                      ELSE status
                  END WHERE id=?;"
   test-id test-id test-id))

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
     (lambda (id test-id stepname state status event-time)
       (set! res (cons (vector id test-id stepname state status event-time) res)))
     db
     "SELECT id,test_id,stepname,state,status,event_time FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
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
			(vector (db:step-get-stepname step) ""   "" ""     ""))))
	   (debug:print 6 "record(before) = " record 
			"\nid:       " (db:step-get-id step)
			"\nstepname: " (db:step-get-stepname step)
			"\nstate:    " (db:step-get-state step)
			"\nstatus:   " (db:step-get-status step)
			"\ntime:     " (db:step-get-event_time step))
	   (case (string->symbol (db:step-get-state step))
	     ((start)(vector-set! record 1 (db:step-get-event_time step))
	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
					(db:step-get-status step))))
	     ((end)  
	      (vector-set! record 2 (any->number (db:step-get-event_time step)))
	      (vector-set! record 3 (db:step-get-status step))
	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
					  (endt   (any->number (vector-ref record 2))))
				      (debug:print 4 "record[1]=" (vector-ref record 1) 
						   ", startt=" startt ", endt=" endt
						   ", get-status: " (db:step-get-status step))
				      (if (and (number? startt)(number? endt))
					  (seconds->hr-min-sec (- endt startt)) "-1"))))
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

;; USE: (lset-difference string=? '("a" "b" "c") '("d" "c" "e" "a"))
;;
;; Return a list of prereqs that were NOT met
;;  Tests (and all items) in waiton list must be "COMPLETED" and "PASS"
(define (db-get-prereqs-not-met db run-id waiton)
  (if (null? waiton)
      '()
      (let* ((unmet-pre-reqs '())
	     (tests           (db-get-tests-for-run db run-id #f #f))
	     (result         '()))
	(for-each (lambda (waitontest-name)
		    (let ((ever-seen #f))
		      (for-each (lambda (test)
				  (if (equal? waitontest-name (db:test-get-testname test))
				      (begin
					(set! ever-seen #t)
					(if (not (and (equal? (db:test-get-state test) "COMPLETED")
						      (member (db:test-get-status test) '("PASS" "WARN" "CHECK"))))
					    (set! result (cons waitontest-name result))))))
				tests)
		      (if (not ever-seen)(set! result (cons waitontest-name result)))))
		  waiton)
	(delete-duplicates result))))

;;======================================================================
;; Extract ods file from the db
;;======================================================================

;; runspatt is a comma delimited list of run patterns
;; keypatt-alist must contain *all* keys with an associated pattern: '( ("KEY1" "%") .. )
(define (db:extract-ods-file db outputfile keypatt-alist runspatt)
  (let* ((keysstr  (string-intersperse (map car keypatt-alist) ","))
	 (keyqry   (string-intersperse (map (lambda (p)(conc (car p) " like ? ")) keypatt-alist) " AND "))
	 (test-ids '())
	 (tempdir  (conc "/tmp/" (current-user-name) "/" runspatt "_" (random 10000) "_" (current-process-id)))
	 (runsheader (append (list "Run Id" "Runname")
			     (map car keypatt-alist)
			     (list "Testname" 
				   "Item Path"
				   "Description"
				   "State"
				   "Status"
				   "Final Log"
				   "Run Duration"
				   "When Run"
				   "Tags"
				   "Run Owner"
				   "Comment"
				   "Author"
				   "Test Owner"
				   "Reviewed"
				   "Diskfree"
				   "Uname"
				   "Rundir"
				   "Host"
				   "Cpu Load"
                                   "Warn"
                                   "Error")))
	 (results (list runsheader))
	 (testdata-header (list "Run Id" "Testname" "Item Path" "Category" "Variable" "Value" "Expected" "Tol" "Units" "Status" "Comment")))
    (debug:print 2 "Using " tempdir " for constructing the ods file")
    ;; "Expected Value"
    ;; "Value Found"
    ;; "Tolerance"
    (apply sqlite3:for-each-row
     (lambda (test-id . b)
       (set! test-ids (cons test-id test-ids))   ;; test-id is now testname
       (set! results (append results (list b)))) ;; note, drop the test-id
     db
     (conc "SELECT
              t.testname,r.id,runname," keysstr ",t.testname,
              t.item_path,tm.description,t.state,t.status,
              final_logf,run_duration, 
              strftime('%m/%d/%Y %H:%M:%S',datetime(t.event_time,'unixepoch'),'localtime'),
              tm.tags,r.owner,t.comment,
              author,
              tm.owner,reviewed,
              diskfree,uname,rundir,
              host,cpuload,first_err,first_warn
            FROM tests AS t INNER JOIN runs AS r ON t.run_id=r.id INNER JOIN test_meta AS tm ON tm.testname=t.testname
            WHERE runname LIKE ? AND " keyqry ";")
     runspatt (map cadr keypatt-alist))
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
     (delete-duplicates test-ids))
    (system (conc "mkdir -p " tempdir))
    ;; (pp results)
    (ods:list->ods 
     tempdir
     (if (string-match (regexp "^[/~]+.*") outputfile) ;; full path?
	 outputfile
	 (begin
	   (debug:print 0 "WARNING: path given, " outputfile " is relative, prefixing with current directory")
	   (conc (current-directory) "/" outputfile)))
     results)))

;; (db:extract-ods-file db "outputfile.ods" '(("sysname" "%")("fsname" "%")("datapath" "%")) "%")
