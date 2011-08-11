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
	  ;; (sqlite3:execute db "PRAGMA synchronous = OFF;")
	  (sqlite3:execute db "CREATE TABLE keys (id INTEGER PRIMARY KEY, fieldname TEXT, fieldtype TEXT, CONSTRAINT keyconstraint UNIQUE (fieldname));")
	  (for-each (lambda (key)
		      (sqlite3:execute db "INSERT INTO keys (fieldname,fieldtype) VALUES (?,?);" (key:get-fieldname key)(key:get-fieldtype key)))
		    keys)
	  (sqlite3:execute db (conc 
			    "CREATE TABLE runs (id INTEGER PRIMARY KEY, " 
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
			"CREATE TABLE tests 
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
                     tags       TEXT DEFAULT '',
                     CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path)
          );")
	  (sqlite3:execute db "CREATE INDEX tests_index ON tests (run_id, testname);")
	  (sqlite3:execute db "CREATE VIEW runs_tests AS SELECT * FROM runs INNER JOIN tests ON runs.id=tests.run_id;")
	  (sqlite3:execute db "CREATE TABLE test_steps 
                              (id INTEGER PRIMARY KEY,
                               test_id INTEGER, 
                               stepname TEXT, 
                               state TEXT DEFAULT 'NOT_STARTED', 
                               status TEXT DEFAULT 'n/a',event_time TIMESTAMP,
                               comment TEXT DEFAULT '',
                               CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
	  (sqlite3:execute db "CREATE TABLE extradat (id INTEGER PRIMARY KEY, run_id INTEGER, key TEXT, val TEXT);")
	  (sqlite3:execute db "CREATE TABLE metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (id,var));")
	  (db:set-var db "MEGATEST_VERSION" megatest-version)
	  (sqlite3:execute db "CREATE TABLE access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")
	  (patch-db db)))
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
     (sqlite3:execute db "CREATE TABLE metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (id,var));")
     (db:set-var db "MEGATEST_VERSION" 1.17)
     )
   (let ((mver (db:get-var db "MEGATEST_VERSION")))
     (print "Current schema version: " mver " current megatest version: " megatest-version)
     (if (not mver)
	(begin
	  (print "Adding megatest-version to metadata")
	  (sqlite3:execute db (db:set-var db "MEGATEST_VERSION" megatest-version))))
     ;;      (if (< mver 1.18)
     ;; 	 (begin
     ;; 	   (print "Adding tags column to tests table")
     ;; 	   (sqlite3:execute db "ALTER TABLE tests ADD COLUMN tags TEXT DEFAULT '';")))
     (if (< mver 1.20)
	 (begin
	   (sqlite3:execute db "CREATE TABLE test_meta (id INTEGER PRIMARY KEY,
                                     testname    TEXT DEFAULT '',
                                     author      TEXT DEFAULT '',
                                     owner       TEXT DEFAULT '',
                                     description TEXT DEFAULT '',
                                     reviewed    TIMESTAMP,
                                     iterated    TEXT DEFAULT '',
                                     avg_runtime REAL,
                                     avg_disk    REAL,
                                     tags        TEXT DEFAULT '',
                                CONSTRAINT test_meta_contstraint UNIQUE (id,testname));")
	   (for-each 
	    (lambda (stmt)
	      (sqlite3:execute db stmt))
	    (list 
	     "ALTER TABLE tests ADD COLUMN expected_value REAL;" ;; DO NOT Add a default, we want it to be NULL
	     "ALTER TABLE tests ADD COLUMN value REAL;"
	     "ALTER TABLE tests ADD COLUMN tol REAL;"
	     "ALTER TABLE tests ADD COLUMN tol_perc REAL;"
	     "ALTER TABLE tests ADD COLUMN first_err TEXT;"
	     "ALTER TABLE tests ADD COLUMN first_warn TEXT;"
	     ))))
     (if (< mver megatest-version)
	 (db:set-var db "MEGATEST_VERSION" megatest-version)))))

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

(define (db-get-keys db)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (key keytype)
       (set! res (cons (vector key keytype) res)))
     db
     "SELECT fieldname,fieldtype FROM keys ORDER BY id DESC;")
    res))


(define-inline (db:get-header vec)(vector-ref vec 0))
(define-inline (db:get-rows   vec)(vector-ref vec 1))

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

;; replace header and keystr with a call to runs:get-std-run-fields
(define (db-get-runs db runpatt . count)
  (let* ((res      '())
	 (keys      (db-get-keys db))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (cons (apply vector a x) res)))
     db
     (conc "SELECT " keystr " FROM runs WHERE runname LIKE ? ORDER BY event_time DESC "
	   (if (and (not (null? count))
		    (number? (car count)))
	       (conc " LIMIT " (car count))
	       "")
	   (if (and (> (length count) 1)
		    (number? (cadr count)))
	       (conc " OFFSET " (cadr count))
	       ""))
     runpatt)
    (vector header res)))

;; replace header and keystr with a call to runs:get-std-run-fields
;; keypatt: '(("key1" "patt1")("key2" "patt2")...)
(define (db:get-runs db keys keypatts runpatt)
  (let* ((res      '())
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append (map key:get-fieldname keys)
			    remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (sqlite3:for-each-row
     (lambda (a . x) ;; turn all the fields returned into a vector and add to the list
       (set! res (cons (apply vector a x) res)))
     db
     (conc "SELECT " keystr " FROM runs WHERE runname LIKE ? "
	   (map (lambda (keypatt)
		  (conc "AND " (car keypatt) " LIKE " (cadr keypatt) " "))
		keypatts)
	   "ORDER BY event_time DESC;")
     runpatt)
    (vector header res)))

;; use this one for db-get-run-info
(define-inline (db:get-row    vec)(vector-ref vec 1))

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

(define (make-db:test)(make-vector 6))
(define-inline (db:test-get-id           vec) (vector-ref vec 0))
(define-inline (db:test-get-run_id       vec) (vector-ref vec 1))
(define-inline (db:test-get-testname     vec) (vector-ref vec 2))
(define-inline (db:test-get-state        vec) (vector-ref vec 3))
(define-inline (db:test-get-status       vec) (vector-ref vec 4))
(define-inline (db:test-get-event_time   vec) (vector-ref vec 5))
(define-inline (db:test-get-host         vec) (vector-ref vec 6))
(define-inline (db:test-get-cpuload      vec) (vector-ref vec 7))
(define-inline (db:test-get-diskfree     vec) (vector-ref vec 8))
(define-inline (db:test-get-uname        vec) (vector-ref vec 9))
(define-inline (db:test-get-rundir       vec) (vector-ref vec 10))
(define-inline (db:test-get-item-path    vec) (vector-ref vec 11))
(define-inline (db:test-get-run_duration vec) (vector-ref vec 12))
(define-inline (db:test-get-final_logf   vec) (vector-ref vec 13))
(define-inline (db:test-get-comment      vec) (vector-ref vec 14))
(define-inline (db:test-get-fullname     vec)
  (conc (db:test-get-testname vec) "/" (db:test-get-item-path vec)))

(define-inline (db:test-set-testname! vec val)(vector-set! vec 2 val))
(define-inline (db:test-set-state!    vec val)(vector-set! vec 3 val))
(define-inline (db:test-set-status!   vec val)(vector-set! vec 4 val))

(define (db-get-tests-for-run db run-id . params)
  (let ((res '())
	(testpatt (if (or (null? params)(not (car params))) "%" (car params)))
	(itempatt (if (> (length params) 1)(cadr params) "%")))
    (sqlite3:for-each-row 
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname like ? AND item_path LIKE ? ORDER BY id DESC;"
     run-id testpatt (if itempatt itempatt "%"))
    res))

;; this one is a bit broken BUG FIXME
(define (db:delete-test-step-records db run-id test-name itemdat)
  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id in (SELECT id FROM tests WHERE run_id=? AND testname=? AND item_path=?);" 
		   run-id test-name (item-list->path itemdat)))
;; 
(define (db:delete-test-records db test-id)
  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id=?;" test-id)
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
     db
     "SELECT count(id) FROM tests WHERE state in ('LAUNCHED','NOT_STARTED','REMOTEHOSTSTART','RUNNING') AND run_id=?;" run-id)
    res))

;; NB// Sync this with runs:get-test-info
(define (db:get-test-info db run-id testname item-path)
  (let ((res #f))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)))
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

;; make-vector-record db testmeta id testname author owner description reviewed iterated avg_runtime avg_disk
(define (make-db:testmeta)(make-vector 10 ""))
(define-inline (db:testmeta-get-id            vec)    (vector-ref  vec 0))
(define-inline (db:testmeta-get-testname      vec)    (vector-ref  vec 1))
(define-inline (db:testmeta-get-author        vec)    (vector-ref  vec 2))
(define-inline (db:testmeta-get-owner         vec)    (vector-ref  vec 3))
(define-inline (db:testmeta-get-description   vec)    (vector-ref  vec 4))
(define-inline (db:testmeta-get-reviewed      vec)    (vector-ref  vec 5))
(define-inline (db:testmeta-get-iterated      vec)    (vector-ref  vec 6))
(define-inline (db:testmeta-get-avg_runtime   vec)    (vector-ref  vec 7))
(define-inline (db:testmeta-get-avg_disk      vec)    (vector-ref  vec 8))
(define-inline (db:testmeta-get-tags          vec)    (vector-ref  vec 9))
(define-inline (db:testmeta-set-id!           vec val)(vector-set! vec 0 val))
(define-inline (db:testmeta-set-testname!     vec val)(vector-set! vec 1 val))
(define-inline (db:testmeta-set-author!       vec val)(vector-set! vec 2 val))
(define-inline (db:testmeta-set-owner!        vec val)(vector-set! vec 3 val))
(define-inline (db:testmeta-set-description!  vec val)(vector-set! vec 4 val))
(define-inline (db:testmeta-set-reviewed!     vec val)(vector-set! vec 5 val))
(define-inline (db:testmeta-set-iterated!     vec val)(vector-set! vec 6 val))
(define-inline (db:testmeta-set-avg_runtime!  vec val)(vector-set! vec 7 val))
(define-inline (db:testmeta-set-avg_disk!     vec val)(vector-set! vec 8 val))

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
;; Steps
;;======================================================================
;; Run steps
;; make-vector-record "Run steps" db step id test_id stepname step_complete step_pass event_time    
(define (make-db:step)(make-vector 6))
(define-inline (db:step-get-id              vec)    (vector-ref  vec 0))
(define-inline (db:step-get-test_id         vec)    (vector-ref  vec 1))
(define-inline (db:step-get-stepname        vec)    (vector-ref  vec 2))
(define-inline (db:step-get-state           vec)    (vector-ref  vec 3))
(define-inline (db:step-get-status          vec)    (vector-ref  vec 4))
(define-inline (db:step-get-event_time      vec)    (vector-ref  vec 5))
(define-inline (db:step-set-id!             vec val)(vector-set! vec 0 val))
(define-inline (db:step-set-test_id!        vec val)(vector-set! vec 1 val))
(define-inline (db:step-set-stepname!       vec val)(vector-set! vec 2 val))
(define-inline (db:step-set-state!          vec val)(vector-set! vec 3 val))
(define-inline (db:step-set-status!         vec val)(vector-set! vec 4 val))
(define-inline (db:step-set-event_time!     vec val)(vector-set! vec 5 val))

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

;; check that *all* the prereqs are "COMPLETED"
(define (db-get-prereqs-met db run-id waiton)
  (let ((res          #f)
	(not-complete 0)
	(tests        (db-get-tests-for-run db run-id)))
    (for-each
     (lambda (test-name)
       (for-each 
	(lambda (test)
	  (if (equal? (db:test-get-testname test) test-name)
	      (begin
		(set! res #t)
		(if (not (equal? (db:test-get-state test) "COMPLETED"))
		    (set! not-complete (+ 1 not-complete))))))
	tests))
     waiton)
    (and (or (null? waiton) res)
	 (eq? not-complete 0))))

;; USE: (lset-difference string=? '("a" "b" "c") '("d" "c" "e" "a"))
;;
;; Return a list of prereqs that were NOT met
;;  Tests (and all items) in waiton list must be "COMPLETED" and "PASS"
(define (db-get-prereqs-not-met db run-id waiton)
  (if (null? waiton)
      '()
      (let* ((unmet-pre-reqs '())
	     (tests           (db-get-tests-for-run db run-id))
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

