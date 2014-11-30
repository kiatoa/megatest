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

(require-extension (srfi 18) extras tcp)
(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (unit tdb))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))
(declare (uses client))
(declare (uses mt))
(declare (uses db))

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
	   (print-call-chain (current-error-port))
	   (debug:print 2 "ERROR: problem accessing test db " work-area ", you probably should clean and re-run this test"
			((condition-property-accessor 'exn 'message) exn))
	   (set! db (sqlite3:open-database ":memory:")) ;; open an in-memory db to allow readonly access 
	   (set! dbexists #f)) ;; must force re-creation of tables, more tom-foolery
	 (set! db (sqlite3:open-database dbpath)))
	(if *db-write-access* (sqlite3:set-busy-handler! db handler))
	(if (not dbexists)
	    (begin
	      (db:set-sync db) ;; (sqlite3:execute db "PRAGMA synchronous = FULL;")
	      (debug:print-info 11 "Initialized test database " dbpath)
	      (tdb:testdb-initialize db)))
	;; (sqlite3:execute db "PRAGMA synchronous = 0;")
	(debug:print-info 11 "open-test-db END (sucessful)" work-area)
	;; now let's test that everything is correct
	(handle-exceptions
	 exn
	 (begin
	   (print-call-chain (current-error-port))
	   (debug:print 0 "ERROR: problem accessing test db " work-area ", you probably should clean and re-run this test or remove the file " 
			dbpath ".\n  "
			((condition-property-accessor 'exn 'message) exn))
	   #f)
	 ;; Is there a cheaper single line operation that will check for existance of a table
	 ;; and raise an exception ?
	 (sqlite3:execute db "SELECT id FROM test_data LIMIT 1;"))
	db)
      (let ((baddb (sqlite3:open-database ":memory:")))
	(debug:print-info 11 "open-test-db END (unsucessful)" work-area)
	;; provide an in-mem db (this is dangerous!)
	(tdb:testdb-initialize baddb)
	baddb)))

;; find and open the testdat.db file for an existing test
(define (tdb:open-test-db-by-test-id test-id #!key (work-area #f))
  (let* ((test-path (if work-area
			work-area
			(rmt:test-get-rundir-from-test-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

;; find and open the testdat.db file for an existing test
(define (tdb:open-test-db-by-test-id-local dbstruct run-id test-id #!key (work-area #f))
  (let* ((test-path (if work-area
			work-area
			(db:test-get-rundir-from-test-id dbstruct run-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

;; find and open the testdat.db file for an existing test
(define (tdb:open-run-close-db-by-test-id-local dbstruct run-id test-id work-area proc . params)
  (let* ((test-path (if work-area
			work-area
			(db:test-get-rundir-from-test-id dbstruct run-id test-id)))
	 (tdb        (open-test-db test-path)))
    (apply proc tdb params)))

(define (tdb:testdb-initialize db)
  (debug:print 11 "db:testdb-initialize START")
  (sqlite3:with-transaction
   db
   (lambda ()
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
              CONSTRAINT metadat_constraint UNIQUE (var));"))))
  (debug:print 11 "db:testdb-initialize END"))

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

;; ;; get a list of test_data records matching categorypatt
;; (define (tdb:read-test-data test-id categorypatt #!key (work-area #f))
;;   (let ((tdb  (tdb:open-test-db-by-test-id test-id work-area: work-area)))
;;     (if (sqlite3:database? tdb)
;; 	(let ((res '()))
;; 	  (sqlite3:for-each-row 
;; 	   (lambda (id test_id category variable value expected tol units comment status type)
;; 	     (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
;; 	   tdb
;; 	   "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
;; 	  (sqlite3:finalize! tdb)
;; 	  (reverse res))
;; 	'())))

;; NOTE: Run this local with #f for db !!!
(define (tdb:load-test-data run-id test-id)
  (let loop ((lin (read-line)))
    (if (not (eof-object? lin))
	(begin
	  (debug:print 4 lin)
	  (rmt:csv->test-data run-id test-id lin)
	  (loop (read-line)))))
  ;; roll up the current results.
  ;; FIXME: Add the status too 
  (rmt:test-data-rollup run-id test-id #f))

(define (tdb:get-prev-tol-for-test tdb test-id category variable)
  ;; Finish me?
  (values #f #f #f))

;;======================================================================
;; S T E P S 
;;======================================================================

(define (tdb:step-get-time-as-string vec)
  (seconds->time-string (tdb:step-get-event_time vec)))

;; get a pretty table to summarize steps
;;
(define (tdb:get-steps-table steps);; organise the steps for better readability
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
    res))

;; Move this to steps.scm
;;
;; get a pretty table to summarize steps
;;
(define (tdb:get-steps-table-list steps)
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
    res))

;;
;; Move to steps.scm
;;
(define (tdb:get-compressed-steps comprsteps) ;; from tdb:get-steps-table
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

;; 
(define (tdb:remote-update-testdat-meta-info run-id test-id work-area cpuload diskfree minutes)
  (let ((tdb         (rmt:open-test-db-by-test-id run-id test-id work-area: work-area)))
    (if (sqlite3:database? tdb)
	(begin
	  (sqlite3:execute tdb "INSERT INTO test_rundat (update_time,cpuload,diskfree,run_duration) VALUES (strftime('%s','now'),?,?,?);"
			   cpuload diskfree minutes)
	  (sqlite3:finalize! tdb))
	(debug:print 2 "Can't update testdat.db for test " test-id " read-only or non-existant"))))
    
