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

(declare (unit tdb))
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

;;======================================================================
;;
;; T E S T   D A T A B A S E S
;;
;;======================================================================

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
	      (sqlite3:execute db "PRAGMA synchronous = FULL;")
	      (debug:print-info 11 "Initialized test database " dbpath)
	      (tdb:testdb-initialize db)))
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
(define (tdb:open-test-db-by-test-id test-id #!key (work-area #f))
  (let* ((test-path (if work-area
			work-area
			(rmt:test-get-rundir-from-test-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

(define (tdb:testdb-initialize db)
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

(define (tdb:get-steps-data tdb test-id)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test-id stepname state status event-time logfile)
       (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
     tdb
     "SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
     test-id)
    (sqlite3:finalize! tdb)
    (reverse res)))

(define (tdb:read-test-data tdb test-id categorypatt)
  (let ((res '()))
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status type)
       (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
     tdb
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (sqlite3:finalize! tdb)
    (reverse res)))

;;======================================================================
;; T E S T   D A T A 
;;======================================================================

(define (tdb:csv->test-data test-id csvdata #!key (work-area #f))
  (debug:print 4 "test-id " test-id ", csvdata: " csvdata)
  (let ((tdb     (tdb:open-test-db-by-test-id test-id work-area: work-area)))
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
		   (let-values (((new-expected new-tol new-units)(tdb:get-prev-tol-for-test db test-id category variable)))
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
(define (tdb:read-test-data test-id categorypatt #!key (work-area #f))
  (let ((tdb  (tdb:open-test-db-by-test-id test-id work-area: work-area)))
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
(define (tdb:load-test-data test-id #!key (work-area #f))
  (let loop ((lin (read-line)))
    (if (not (eof-object? lin))
	(begin
	  (debug:print 4 lin)
	  (tdb:csv->test-data db test-id lin work-area: work-area)
	  (loop (read-line)))))
  ;; roll up the current results.
  ;; FIXME: Add the status to 
  (tdb:test-data-rollup db test-id #f work-area: work-area))

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (tdb:test-data-rollup test-id status #!key (work-area #f))
  (let ((tdb (tdb:open-test-db-by-test-id test-id work-area: work-area))
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
	  (rmt:general-call 'pass-fail-counts fail-count pass-count test-id)
	  ;; (sqlite3:execute db "UPDATE tests SET fail_count=?,pass_count=? WHERE id=?;" 
	  ;;                     fail-count pass-count test-id)

	  ;; The flush is not needed with the transaction based write agregation enabled. Remove these commented lines
	  ;; next time you read this!
	  ;;
	  ;; (cdb:flush-queue *runremote*)
	  ;; (thread-sleep! 1) ;; play nice with the queue by ensuring the rollup is at least 10ms later than the set
	  
	  ;; if the test is not FAIL then set status based on the fail and pass counts.
	  (rmt:general-call 'test-rollup-test_data-pass-fail test-id)
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

(define (tdb:get-prev-tol-for-test test-id category variable)
  ;; Finish me?
  (values #f #f #f))

;;======================================================================
;; S T E P S 
;;======================================================================

(define (tdb:step-get-time-as-string vec)
  (seconds->time-string (tdb:step-get-event_time vec)))

;; db-get-test-steps-for-run
(define (tdb:get-steps-for-test test-id #!key (work-area #f))
  (let* ((tdb (tdb:open-test-db-by-test-id test-id work-area: work-area))
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
(define (tdb:get-steps-table test-id #!key (work-area #f))
  (let ((steps   (tdb:get-steps-for-test test-id work-area: work-area)))
    ;; organise the steps for better readability
    (let ((res (make-hash-table)))
      (for-each 
       (lambda (step)
	 (debug:print 6 "step=" step)
	 (let ((record (hash-table-ref/default 
			res 
			(tdb:step-get-stepname step) 
			;;        stepname                start end status Duration  Logfile 
			(vector (tdb:step-get-stepname step) ""   "" ""     ""        ""))))
	   (debug:print 6 "record(before) = " record 
			"\nid:       " (tdb:step-get-id step)
			"\nstepname: " (tdb:step-get-stepname step)
			"\nstate:    " (tdb:step-get-state step)
			"\nstatus:   " (tdb:step-get-status step)
			"\ntime:     " (tdb:step-get-event_time step))
	   (case (string->symbol (tdb:step-get-state step))
	     ((start)(vector-set! record 1 (tdb:step-get-event_time step))
	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
					(tdb:step-get-status step)))
	      (if (> (string-length (tdb:step-get-logfile step))
		     0)
		  (vector-set! record 5 (tdb:step-get-logfile step))))
	     ((end)  
	      (vector-set! record 2 (any->number (tdb:step-get-event_time step)))
	      (vector-set! record 3 (tdb:step-get-status step))
	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
					  (endt   (any->number (vector-ref record 2))))
				      (debug:print 4 "record[1]=" (vector-ref record 1) 
						   ", startt=" startt ", endt=" endt
						   ", get-status: " (tdb:step-get-status step))
				      (if (and (number? startt)(number? endt))
					  (seconds->hr-min-sec (- endt startt)) "-1")))
	      (if (> (string-length (tdb:step-get-logfile step))
		     0)
		  (vector-set! record 5 (tdb:step-get-logfile step))))
	     (else
	      (vector-set! record 2 (tdb:step-get-state step))
	      (vector-set! record 3 (tdb:step-get-status step))
	      (vector-set! record 4 (tdb:step-get-event_time step))))
	   (hash-table-set! res (tdb:step-get-stepname step) record)
	   (debug:print 6 "record(after)  = " record 
			"\nid:       " (tdb:step-get-id step)
			"\nstepname: " (tdb:step-get-stepname step)
			"\nstate:    " (tdb:step-get-state step)
			"\nstatus:   " (tdb:step-get-status step)
			"\ntime:     " (tdb:step-get-event_time step))))
       ;; (else   (vector-set! record 1 (tdb:step-get-event_time step)))
       (sort steps (lambda (a b)
		     (cond
		      ((<   (tdb:step-get-event_time a)(tdb:step-get-event_time b)) #t)
		      ((eq? (tdb:step-get-event_time a)(tdb:step-get-event_time b)) 
		       (<   (tdb:step-get-id a)        (tdb:step-get-id b)))
		      (else #f)))))
      res)))

;; get a pretty table to summarize steps
;;
(define (tdb:get-steps-table-list test-id #!key (work-area #f))
  (let ((steps   (tdb:get-steps-for-test test-id work-area: work-area)))
    ;; organise the steps for better readability
    (let ((res (make-hash-table)))
      (for-each 
       (lambda (step)
	 (debug:print 6 "step=" step)
	 (let ((record (hash-table-ref/default 
			res 
			(tdb:step-get-stepname step) 
			;;        stepname                start end status    
			(vector (tdb:step-get-stepname step) ""   "" ""     "" ""))))
	   (debug:print 6 "record(before) = " record 
			"\nid:       " (tdb:step-get-id step)
			"\nstepname: " (tdb:step-get-stepname step)
			"\nstate:    " (tdb:step-get-state step)
			"\nstatus:   " (tdb:step-get-status step)
			"\ntime:     " (tdb:step-get-event_time step))
	   (case (string->symbol (tdb:step-get-state step))
	     ((start)(vector-set! record 1 (tdb:step-get-event_time step))
	      (vector-set! record 3 (if (equal? (vector-ref record 3) "")
					(tdb:step-get-status step)))
	      (if (> (string-length (tdb:step-get-logfile step))
		     0)
		  (vector-set! record 5 (tdb:step-get-logfile step))))
	     ((end)  
	      (vector-set! record 2 (any->number (tdb:step-get-event_time step)))
	      (vector-set! record 3 (tdb:step-get-status step))
	      (vector-set! record 4 (let ((startt (any->number (vector-ref record 1)))
					  (endt   (any->number (vector-ref record 2))))
				      (debug:print 4 "record[1]=" (vector-ref record 1) 
						   ", startt=" startt ", endt=" endt
						   ", get-status: " (tdb:step-get-status step))
				      (if (and (number? startt)(number? endt))
					  (seconds->hr-min-sec (- endt startt)) "-1")))
	      (if (> (string-length (tdb:step-get-logfile step))
		     0)
		  (vector-set! record 5 (tdb:step-get-logfile step))))
	     (else
	      (vector-set! record 2 (tdb:step-get-state step))
	      (vector-set! record 3 (tdb:step-get-status step))
	      (vector-set! record 4 (tdb:step-get-event_time step))))
	   (hash-table-set! res (tdb:step-get-stepname step) record)
	   (debug:print 6 "record(after)  = " record 
			"\nid:       " (tdb:step-get-id step)
			"\nstepname: " (tdb:step-get-stepname step)
			"\nstate:    " (tdb:step-get-state step)
			"\nstatus:   " (tdb:step-get-status step)
			"\ntime:     " (tdb:step-get-event_time step))))
       ;; (else   (vector-set! record 1 (tdb:step-get-event_time step)))
       (sort steps (lambda (a b)
		     (cond
		      ((<   (tdb:step-get-event_time a)(tdb:step-get-event_time b)) #t)
		      ((eq? (tdb:step-get-event_time a)(tdb:step-get-event_time b)) 
		       (<   (tdb:step-get-id a)        (tdb:step-get-id b)))
		      (else #f)))))
      res)))

(define (tdb:get-compressed-steps test-id #!key (work-area #f)(tdb #f))
  (if (or (not work-area)
	  (file-exists? (conc work-area "/testdat.db")))
      (let* ((comprsteps (tdb:get-steps-table test-id work-area: work-area)))
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

(define (tdb:teststep-set-status! test-id teststep-name state-in status-in comment logfile #!key (work-area #f))
  (let* ((tdb       (tdb:open-test-db-by-test-id test-id work-area: work-area))
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

;; this one is a bit broken BUG FIXME
(define (tdb:delete-test-step-records test-id #!key (work-area #f))
  ;; Breaking it into two queries for better file access interleaving
  (let* ((tdb (tdb:open-test-db-by-test-id test-id work-area: work-area)))
    ;; test db's can go away - must check every time
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:execute tdb "DELETE FROM test_steps;")
	  (sqlite3:execute tdb "DELETE FROM test_data;")
	  (sqlite3:finalize! tdb)))))

;; 
(define (tdb:update-testdat-meta-info test-id work-area cpuload diskfree minutes)
  (let ((tdb         (tdb:open-test-db-by-test-id test-id work-area: work-area)))
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:execute tdb "INSERT INTO test_rundat (update_time,cpuload,diskfree,run_duration) VALUES (strftime('%s','now'),?,?,?);"
			   cpuload diskfree minutes)
	  (sqlite3:finalize! tdb))
	(debug:print 2 "Can't update testdat.db for test " test-id " read-only or non-existant"))))
    
