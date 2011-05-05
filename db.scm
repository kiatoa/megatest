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
			    "CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
	  (sqlite3:execute db (conc "CREATE INDEX runs_index ON runs (runname" (if havekeys "," "") keystr ");"))
	  (sqlite3:execute db 
			"CREATE TABLE tests 
                    (id INTEGER PRIMARY KEY,
                     run_id     INTEGER,
                     testname   TEXT,
                     itempath   TEXT,
                     host       TEXT DEFAULT 'n/a',
                     cpuload    REAL DEFAULT -1,
                     diskfree   INTEGER DEFAULT -1,
                     uname      TEXT DEFAULT 'n/a', 
                     rundir     TEXT DEFAULT 'n/a',
                     item_path  TEXT DEFAULT '',
                     state      TEXT DEFAULT 'NOT_STARTED',
                     status     TEXT DEFAULT 'n/a',
                     attemptnum INTEGER DEFAULT 0,
                     final_logf TEXT DEFAULT 'logs/final.log',
                     logdat     BLOB, 
                     run_duration INTEGER DEFAULT 0,
                     comment    TEXT DEFAULT '',
                     event_time TIMESTAMP,
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
	  (sqlite3:execute db "CREATE TABLE access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")))
    db))

;; (if (args:get-arg "-db")
;;     (set! db (open-db (args:get-arg "-db"))))

;; TODO
;; 
;; 1. Implement basic registering of records
;; 2. Implement basic querying of records
;; eh?

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

(define (db-get-value-by-header row header field)
  (if (null? header) #f
      (let loop ((hed (car header))
		 (tal (cdr header))
		 (n   0))
	(if (equal? hed field)
	    (vector-ref row n)
	    (if (null? tal) #f (loop (car tal)(cdr tal)(+ n 1)))))))
	    
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

;; use this one for db-get-run-info
(define-inline (db:get-row    vec)(vector-ref vec 1))

;; use (get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
(define (db-get-run-info db run-id)
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

(define (db:delete-test-step-records db run-id test-name)
  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id in (SELECT id FROM tests WHERE run_id=? AND testname=?);" run-id test-name))

(define (db:get-count-tests-running db)
  (let ((res 0))
    (sqlite3:for-each-row
     (lambda (count)
       (set! res count))
     db
     "SELECT count(id) FROM tests WHERE state = 'RUNNING' OR state = 'LAUNCHED' OR state = 'REMOTEHOSTSTART';")
    res))

;; NB// Sync this with runs:get-test-info
(define (db:get-test-info db run-id testname item-path)
  (let ((res '()))
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)
       (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run_duration final_logf comment)))
     db 
     "SELECT id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
     run-id testname item-path)
    res))

;; Steps
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

(define (db-get-test-steps-for-run db test-id)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time)
       (set! res (cons (vector id test-id stepname state status event-time) res)))
     db
     "SELECT id,test_id,stepname,state,status,event_time FROM test_steps WHERE test_id=? ORDER BY event_time DESC;"
     test-id)
    res))

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
						      (equal? (db:test-get-status test) "PASS")))
					    (set! result (cons waitontest-name result))))))
				tests)
		      (if (not ever-seen)(set! result (cons waitontest-name result)))))
		  waiton)
	(delete-duplicates result))))
;;  
;;  	     ;; subtract from the waiton list the "COMPLETED" tests
;;  	     ;;(completed-tests (filter (lambda (x)
;;  	     ;;   			(equal? (db:test-get-state x) "COMPLETED"))
;;  	     ;;   		      tests))
;;  	     (completed-tests (let ((non-completed (make-hash-table)))
;;  				(for-each (lambda (x)
;;  					    ;; could add check for PASS here
;;  					    (if (not (and (equal? (db:test-get-state x) "COMPLETED")
;;  							  (equal? (db:test-get-status x) "PASS")))
;;  						(hash-table-set! non-completed (db:test-get-testname x) x)))
;;  					    ;; (print "Completed: " (db:test-get-testname x))))
;;  					  tests)
;;  				(filter (lambda (x)
;;  					  (not (hash-table-ref/default non-completed (db:test-get-testname x) #f)))
;;  					tests)))
;;  	     (pre-dep-names   (map db:test-get-testname completed-tests))
;;  	     (result          (lset-difference string=? waiton pre-dep-names)))
;;  	(print "pre-dep-names: " pre-dep-names " waiton: " waiton " result: " result)
