(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking tcp rpc)
(import (prefix sqlite3 sqlite3:))
(import (prefix rpc rpc:))

(declare (unit tests))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

(define (tests:register-test db run-id test-name item-path)
  (let ((item-paths (if (equal? item-path "")
			(list item-path)
			(list item-path ""))))
    (for-each 
     (lambda (pth)
       (sqlite3:execute db "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');" 
			run-id 
			test-name
			pth))
     item-paths )))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
(define (test:get-previous-test-run-record db run-id test-name item-path)
  (let* ((keys    (db:get-keys db))
	 (selstr  (string-intersperse (map (lambda (x)(vector-ref x 0)) keys) ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc (vector-ref x 0) "=?")) keys) " AND "))
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
		(let ((results (db:get-tests-for-run db hed test-name item-path '() '())))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))
    
;; get the previous records for when these tests were run where all keys match but runname
;; NB// Merge this with test:get-previous-test-run-records? This one looks for all matching tests
;; can use wildcards. 
(define (test:get-matching-previous-test-run-records db run-id test-name item-path)
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
		(let ((results (rdb:get-tests-for-run db hed test-name item-path '() '())))
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

;; 
(define (test-set-status! db test-id state status comment dat)
  (let* ((real-status status)
	 (otherdat    (if dat dat (make-hash-table)))
	 (testdat     (db:get-test-data-by-id db test-id))
	 (run-id      (db:test-get-run_id testdat))
	 (test-name   (db:test-get-testname   testdat))
	 (item-path   (db:test-get-item-path testdat))
	 ;; before proceeding we must find out if the previous test (where all keys matched except runname)
	 ;; was WAIVED if this test is FAIL
	 (waived   (if (equal? status "FAIL")
		       (let ((prev-test (test:get-previous-test-run-record db run-id test-name item-path)))
			 (if prev-test ;; true if we found a previous test in this run series
			     (let ((prev-status (db:test-get-status   prev-test))
				   (prev-state  (db:test-get-state    prev-test))
				   (prev-comment (db:test-get-comment prev-test)))
			       (debug:print 4 "prev-status " prev-status ", prev-state " prev-state ", prev-comment " prev-comment)
			       (if (and (equal? prev-state  "COMPLETED")
					(equal? prev-status "WAIVED"))
				   prev-comment ;; waived is either the comment or #f
				   #f))
			     #f))
		       #f)))
    (if waived (set! real-status "WAIVED"))
    (debug:print 4 "real-status " real-status ", waived " waived ", status " status)

    ;; update the primary record IF state AND status are defined
    (if (and state status)
	(rdb:test-set-state-status-by-run-id-testname db run-id test-name item-path real-status state))

    ;; if status is "AUTO" then call rollup (note, this one modifies data in test
    ;; run area, do not rpc it (yet)
    (if (and test-id state status (equal? status "AUTO")) 
	(db:test-data-rollup db test-id status))

    ;; add metadata (need to do this way to avoid SQL injection issues)

    ;; :first_err
    ;; (let ((val (hash-table-ref/default otherdat ":first_err" #f)))
    ;;   (if val
    ;;       (sqlite3:execute db "UPDATE tests SET first_err=? WHERE run_id=? AND testname=? AND item_path=?;" val run-id test-name item-path)))
    ;; 
    ;; ;; :first_warn
    ;; (let ((val (hash-table-ref/default otherdat ":first_warn" #f)))
    ;;   (if val
    ;;       (sqlite3:execute db "UPDATE tests SET first_warn=? WHERE run_id=? AND testname=? AND item_path=?;" val run-id test-name item-path)))

    (let ((category (hash-table-ref/default otherdat ":category" ""))
	  (variable (hash-table-ref/default otherdat ":variable" ""))
	  (value    (hash-table-ref/default otherdat ":value"    #f))
	  (expected (hash-table-ref/default otherdat ":expected" #f))
	  (tol      (hash-table-ref/default otherdat ":tol"      #f))
	  (units    (hash-table-ref/default otherdat ":units"    ""))
	  (type     (hash-table-ref/default otherdat ":type"     ""))
	  (dcomment (hash-table-ref/default otherdat ":comment"  "")))
      (debug:print 4 
		   "category: " category ", variable: " variable ", value: " value
		   ", expected: " expected ", tol: " tol ", units: " units)
      (if (and value expected tol) ;; all three required
	  (let ((dat (conc category ","
			   variable ","
			   value    ","
			   expected ","
			   tol      ","
			   units    ","
			   dcomment ",," ;; extra comma for status
			   type     )))
	    (rdb:csv->test-data db test-id
				dat))))
      
    ;; need to update the top test record if PASS or FAIL and this is a subtest
    (rdb:roll-up-pass-fail-counts db run-id test-name item-path status)

    (if (or (and (string? comment)
		 (string-match (regexp "\\S+") comment))
	    waived)
	(let ((cmt  (if waived waived comment)))
	  (rdb:test-set-comment db test-id cmt)))
    ))

(define (test-set-toplog! db run-id test-name logf) 
  (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';" 
		   logf run-id test-name))

(define (tests:summarize-items db run-id test-name force)
  ;; if not force then only update the record if one of these is true:
  ;;   1. logf is "log/final.log
  ;;   2. logf is same as outputfilename
  (let ((outputfilename (conc "megatest-rollup-" test-name ".html"))
	(orig-dir       (current-directory))
	(logf           #f))
    (sqlite3:for-each-row 
     (lambda (path final_logf)
       (set! logf final_logf)
       (if (directory? path)
	   (begin
	     (print "Found path: " path)
	     (change-directory path))
	     ;; (set! outputfilename (conc path "/" outputfilename)))
	   (print "No such path: " path)))
     db 
     "SELECT rundir,final_logf FROM tests WHERE run_id=? AND testname=? AND item_path='';"
     run-id test-name)
    (print "summarize-items with logf " logf)
    (if (or (equal? logf "logs/final.log")
	    (equal? logf outputfilename)
	    force)
	(begin
	  (if (obtain-dot-lock outputfilename 1 20 30) ;; retry every second for 20 seconds, call it dead after 30 seconds and steal the lock
	      (print "Obtained lock for " outputfilename)
	      (print "Failed to obtain lock for " outputfilename))
	  (let ((oup    (open-output-file outputfilename))
		(counts (make-hash-table))
		(statecounts (make-hash-table))
		(outtxt "")
		(tot    0))
	    (with-output-to-port
		oup
	      (lambda ()
		(set! outtxt (conc outtxt "<html><title>Summary: " test-name 
				   "</title><body><h2>Summary for " test-name "</h2>"))
		(sqlite3:for-each-row 
		 (lambda (id itempath state status run_duration logf comment)
		   (hash-table-set! counts status (+ 1 (hash-table-ref/default counts status 0)))
		   (hash-table-set! statecounts state (+ 1 (hash-table-ref/default statecounts state 0)))
		   (set! outtxt (conc outtxt "<tr>"
				      "<td><a href=\"" itempath "/" logf "\"> " itempath "</a></td>" 
				      "<td>" state    "</td>" 
				      "<td><font color=" (common:get-color-from-status status)
				      ">"   status   "</font></td>"
				      "<td>" (if (equal? comment "")
						 "&nbsp;"
						 comment) "</td>"
						 "</tr>")))
		 db
		 "SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE run_id=? AND testname=? AND item_path != '';"
		 run-id test-name)

		(print "<table><tr><td valign=\"top\">")
		;; Print out stats for status
		(set! tot 0)
		(print "<table cellspacing=\"0\" border=\"1\"><tr><td colspan=\"2\"><h2>State stats</h2></td></tr>")
		(for-each (lambda (state)
			    (set! tot (+ tot (hash-table-ref statecounts state)))
			    (print "<tr><td>" state "</td><td>" (hash-table-ref statecounts state) "</td></tr>"))
			  (hash-table-keys statecounts))
		(print "<tr><td>Total</td><td>" tot "</td></tr></table>")
		(print "</td><td valign=\"top\">")
		;; Print out stats for state
		(set! tot 0)
		(print "<table cellspacing=\"0\" border=\"1\"><tr><td colspan=\"2\"><h2>Status stats</h2></td></tr>")
		(for-each (lambda (status)
			    (set! tot (+ tot (hash-table-ref counts status)))
			    (print "<tr><td><font color=\"" (common:get-color-from-status status) "\">" status
				   "</font></td><td>" (hash-table-ref counts status) "</td></tr>"))
			  (hash-table-keys counts))
		(print "<tr><td>Total</td><td>" tot "</td></tr></table>")
		(print "</td></td></tr></table>")

		(print "<table cellspacing=\"0\" border=\"1\">" 
		       "<tr><td>Item</td><td>State</td><td>Status</td><td>Comment</td>"
		       outtxt "</table></body></html>")
		(release-dot-lock outputfilename)))
	    (close-output-port oup)
	    (change-directory orig-dir)
	    (test-set-toplog! db run-id test-name outputfilename)
	    )))))

(define (get-all-legal-tests)
  (let* ((tests  (glob (conc *toppath* "/tests/*")))
	 (res    '()))
    (debug:print 4 "INFO: Looking at tests " (string-intersperse tests ","))
    (for-each (lambda (testpath)
		(if (file-exists? (conc testpath "/testconfig"))
		    (set! res (cons (last (string-split testpath "/")) res))))
	      tests)
    res))

(define (tests:get-testconfig test-name system-allowed)
  (let* ((test-path    (conc *toppath* "/tests/" test-name))
	 (test-configf (conc test-path "/testconfig"))
	 (testexists   (and (file-exists? test-configf)(file-read-access? test-configf))))
    (if testexists
	(read-config test-configf #f system-allowed environ-patt: (if system-allowed
								      "pre-launch-env-vars"
								      #f))
	#f)))
  
;; sort tests by priority and waiton
;; Move test specific stuff to a test unit FIXME one of these days
(define (tests:sort-by-priority-and-waiton test-records)
  (let ((mungepriority (lambda (priority)
			 (if priority
			     (let ((tmp (any->number priority)))
			       (if tmp tmp (begin (debug:print 0 "ERROR: bad priority value " priority ", using 0") 0)))
			     0))))
    (sort 
     (hash-table-keys test-records) ;; avoid dealing with deleted tests, look at the hash table
     (lambda (a b)
       (let* ((a-record   (hash-table-ref test-records a))
	      (b-record   (hash-table-ref test-records b))
	      (a-waitons  (tests:testqueue-get-waitons a-record))
	      (b-waitons  (tests:testqueue-get-waitons b-record))
	      (a-config   (tests:testqueue-get-testconfig  a-record))
	      (b-config   (tests:testqueue-get-testconfig  b-record))
	      (a-raw-pri  (config-lookup a-config "requirements" "priority"))
	      (b-raw-pri  (config-lookup b-config "requirements" "priority"))
	      (a-priority (mungepriority a-raw-pri))
	      (b-priority (mungepriority b-raw-pri)))
	;;  (debug:print 5 "sort-by-priority-and-waiton, a: " a " b: " b
	;; 	      "\n     a-record:   " a-record 
	;; 	      "\n     b-record:   " b-record
	;; 	      "\n     a-waitons:  " a-waitons
	;; 	      "\n     b-waitons:  " b-waitons
	;; 	      "\n     a-config:   " (hash-table->alist a-config)
	;; 	      "\n     b-config:   " (hash-table->alist b-config)
	;; 	      "\n     a-raw-pri:  " a-raw-pri
	;; 	      "\n     b-raw-pri:  " b-raw-pri
	;; 	      "\n     a-priority: " a-priority
	;; 	      "\n     b-priority: " b-priority)
	 (tests:testqueue-set-priority! a-record a-priority)
	 (tests:testqueue-set-priority! b-record b-priority)
	 (if (and a-waitons (member (tests:testqueue-get-testname b-record) a-waitons))
	     #f ;; cannot have a which is waiting on b happening before b
	     (if (and b-waitons (member (tests:testqueue-get-testname a-record) b-waitons))
		 #t ;; this is the correct order, b is waiting on a and b is before a
		 (if (> a-priority b-priority)
		     #t ;; if a is a higher priority than b then we are good to go
		     #f))))))))

;; for each test:
;;   
(define (tests:filter-non-runnable db run-id testkeynames testrecordshash)
  (let ((runnables '()))
    (for-each
     (lambda (testkeyname)
       (let* ((test-record (hash-table-ref testrecordshash testkeyname))
	      (test-name   (tests:testqueue-get-testname  test-record))
	      (itemdat     (tests:testqueue-get-itemdat   test-record))
	      (item-path   (tests:testqueue-get-item_path test-record))
	      (waitons     (tests:testqueue-get-waitons   test-record))
	      (keep-test   #t)
	      (tdat        (db:get-test-info db run-id test-name item-path)))
	 (if tdat
	     (begin
	       ;; Look at the test state and status
	       (if (or (member (db:test-get-status tdat) 
			       '("PASS" "WARN" "WAIVED" "CHECK"))
		       (member (db:test-get-state tdat)
			       '("INCOMPLETE" "KILLED")))
		   (set! keep-test #f))

	       ;; examine waitons for any fails. If it is FAIL or INCOMPLETE then eliminate this test
	       ;; from the runnable list
	       (if keep-test
		   (for-each (lambda (waiton)
			       ;; for now we are waiting only on the parent test
			       (let ((wtdat (db:get-test-info db run-id waiton ""))) 
				 (if (or (member (db:test-get-status wtdat)
						 '("FAIL" "KILLED"))
					 (member (db:test-get-state wtdat)
						 '("INCOMPETE")))
				     (set! keep-test #f)))) ;; no point in running this one again
			     waitons))))
	 (if keep-test (set! runnables (cons testkeyname runnables)))))
     testkeynames)
    runnables))

;;======================================================================
;; test steps
;;======================================================================

;; teststep-set-status! used to be here

(define (test-get-kill-request db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    (equal? (test:get-state testdat) "KILLREQ")))

(define (test-set-meta-info db run-id testname itemdat)
  (let ((item-path (item-list->path itemdat))
	(cpuload  (get-cpu-load))
	(hostname (get-host-name))
	(diskfree (get-df (current-directory)))
	(uname    (get-uname "-srvpio")))
    (sqlite3:execute db "UPDATE tests SET host=?,cpuload=?,diskfree=?,uname=? WHERE run_id=? AND testname=? AND item_path=?;"
		  hostname
		  cpuload
		  diskfree
		  uname
		  run-id
		  testname
		  item-path)))

;;======================================================================
;; A R C H I V I N G
;;======================================================================

(define (test:archive db test-id)
  #f)

(define (test:archive-tests db keynames target)
  #f)

;;======================================================================
;; R P C
;;======================================================================

(define (rtests:register-test db run-id test-name item-path)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rtests:register-test host port) run-id test-name item-path))
      (tests:register-test db run-id test-name item-path)))

(define (rtests:test-set-status!  db test-id state status comment dat)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
	    (port (vector-ref *runremote* 1)))
	((rpc:procedure 'rtests:test-set-status! host port) test-id state status comment dat))
      (test-set-status! db test-id state status comment dat)))

(define (rtests:test-set-toplog! db run-id test-name logf)
  (if *runremote*
      (let ((host (vector-ref *runremote* 0))
            (port (vector-ref *runremote* 1)))
        ((rpc:procedure 'rtests:test-set-toplog! host port) run-id test-name logf))
      (test-set-toplog! db run-id test-name logf)))

