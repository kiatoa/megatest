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
;; Tests
;;======================================================================

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking tcp directory-utils)
(import (prefix sqlite3 sqlite3:))

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

;; tests:glob-like-match
(define (tests:glob-like-match patt str) 
  (let ((like (substring-index "%" patt)))
    (let* ((notpatt  (equal? (substring-index "~" patt) 0))
	   (newpatt  (if notpatt (substring patt 1) patt))
	   (finpatt  (if like
			(string-substitute (regexp "%") ".*" newpatt)
			(string-substitute (regexp "\\*") ".*" newpatt)))
	   (res      #f))
      ;; (print "tests:glob-like-match => notpatt: " notpatt ", newpatt: " newpatt ", finpatt: " finpatt)
      (set! res (string-match (regexp finpatt (if like #t #f)) str))
      (if notpatt (not res) res))))

;; if itempath is #f then look only at the testname part
;;
(define (tests:match patterns testname itempath)
  (if (string? patterns)
      (let ((patts (string-split patterns ",")))
	(if (null? patts) ;;; no pattern(s) means no match
	    #f
	    (let loop ((patt (car patts))
		       (tal  (cdr patts)))
	      ;; (print "loop: patt: " patt ", tal " tal)
	      (if (string=? patt "")
		  #f ;; nothing ever matches empty string - policy
		  (let* ((patt-parts (string-match (regexp "^([^\\/]*)(\\/(.*)|)$") patt))
			 (test-patt  (cadr patt-parts))
			 (item-patt  (cadddr patt-parts)))
		    ;; special case: test vs. test/
		    ;;   test  => "test" "%"
		    ;;   test/ => "test" ""
		    (if (and (not (substring-index "/" patt)) ;; no slash in the original
			     (or (not item-patt)
				 (equal? item-patt "")))      ;; should always be true that item-patt is ""
			(set! item-patt "%"))
		    ;; (print "tests:match => patt-parts: " patt-parts ", test-patt: " test-patt ", item-patt: " item-patt)
		    (if (and (tests:glob-like-match test-patt testname)
			     (or (not itempath)
				 (tests:glob-like-match (if item-patt item-patt "") itempath)))
			#t
			(if (null? tal)
			    #f
			    (loop (car tal)(cdr tal)))))))))))

;; if itempath is #f then look only at the testname part
;;
(define (tests:match->sqlqry patterns)
  (if (string? patterns)
      (let ((patts (string-split patterns ",")))
	(if (null? patts) ;;; no pattern(s) means no match, we will do no query
	    #f
	    (let loop ((patt (car patts))
		       (tal  (cdr patts))
		       (res  '()))
	      ;; (print "loop: patt: " patt ", tal " tal)
	      (let* ((patt-parts (string-match (regexp "^([^\\/]*)(\\/(.*)|)$") patt))
		     (test-patt  (cadr patt-parts))
		     (item-patt  (cadddr patt-parts))
		     (test-qry   (db:patt->like "testname" test-patt))
		     (item-qry   (db:patt->like "item_path" item-patt))
		     (qry        (conc "(" test-qry " AND " item-qry ")")))
		;; (print "tests:match => patt-parts: " patt-parts ", test-patt: " test-patt ", item-patt: " item-patt)
		(if (null? tal)
		    (string-intersperse (append (reverse res)(list qry)) " OR ")
		    (loop (car tal)(cdr tal)(cons qry res)))))))
      #f))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
(define (test:get-previous-test-run-record db run-id test-name item-path)
  (let* ((keys    (cdb:remote-run db:get-keys #f))
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
		(let ((results (cdb:remote-run db:get-tests-for-run #f hed (conc test-name "/" item-path)'() '())))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))
    
;; get the previous records for when these tests were run where all keys match but runname
;; NB// Merge this with test:get-previous-test-run-records? This one looks for all matching tests
;; can use wildcards. Also can likely be factored in with get test paths?
(define (test:get-matching-previous-test-run-records db run-id test-name item-path)
  (let* ((keys    (cdb:remote-run db:get-keys #f))
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
		(let ((results (cdb:remote-run db:get-tests-for-run #f hed (conc test-name "/" item-path) '() '())))
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

;; Check for waiver eligibility
;;
(define (tests:check-waiver-eligibility testdat prev-testdat)
  (let* ((testconfig  (tests:get-testconfig (db:test-get-testname testdat) #f))
	 (test-rundir (db:test-get-rundir testdat))
	 (prev-rundir (db:test-get-rundir prev-testdat))
	 (waivers     (configf:section-vars testconfig "waivers"))
	 (waiver-rx   (regexp "^(\\S+)\\s+(.*)$"))
	 (diff-rule   "diff %file1% %file2%")
	 (logpro-rule "diff %file1% %file2% | logpro %waivername%.logpro %waivername%.html"))
    (push-directory test-rundir)
    (let ((result (if (null? waivers)
		      #f
		      (let loop ((hed (car waivers))
				 (tal (cdr waivers)))
			(debug:print 0 "INFO: Applying waiver rule \"" hed "\"")
			(let* ((waiver      (configf:lookup testconfig "waivers" hed))
			       (wparts      (if waiver (string-match waiver-rx waiver) #f))
			       (waiver-rule (if wparts (cadr wparts)  #f))
			       (waiver-glob (if wparts (caddr wparts) #f))
			       (logpro-file (if waiver
						(let ((fname (conc hed ".logpro")))
						  (if (file-exists? fname)
						      fname 
						      (begin
							(debug:print 0 "INFO: No logpro file " fname " falling back to diff")
							#f)))
						#f))
			       ;; if rule by name of waiver-rule is found in testconfig - use it
			       ;; else if waivername.logpro exists use logpro-rule
			       ;; else default to diff-rule
			       (rule-string (let ((rule (configf:lookup testconfig "waiver_rules" waiver-rule)))
					      (if rule
						  rule
						  (if logpro-file
						      logpro-rule
						      (begin
							(debug:print 0 "INFO: No logpro file " logpro-file " found, using diff rule")
							diff-rule)))))
			       ;; (string-substitute "%file1%" "foofoo.txt" "This is %file1% and so is this %file1%." #t)
			       (processed-cmd (string-substitute 
					       "%file1%" (conc test-rundir "/" waiver-glob)
					       (string-substitute
						"%file2%" (conc prev-rundir "/" waiver-glob)
						(string-substitute
						 "%waivername%" hed rule-string #t) #t) #t))
			       (res            #f))
			  (debug:print 0 "INFO: waiver command is \"" processed-cmd "\"")
			  (if (eq? (system processed-cmd) 0)
			      (if (null? tal)
				  #t
				  (loop (car tal)(cdr tal)))
			      #f))))))
      (pop-directory)
      result)))


;; Do not rpc this one, do the underlying calls!!!
(define (tests:test-set-status! test-id state status comment dat)
  (debug:print-info 4 "tests:test-set-status! test-id=" test-id ", state=" state ", status=" status ", dat=" dat)
  (let* ((db          #f)
	 (real-status status)
	 (otherdat    (if dat dat (make-hash-table)))
	 (testdat     (cdb:get-test-info-by-id *runremote* test-id))
	 (run-id      (db:test-get-run_id testdat))
	 (test-name   (db:test-get-testname   testdat))
	 (item-path   (db:test-get-item-path testdat))
	 ;; before proceeding we must find out if the previous test (where all keys matched except runname)
	 ;; was WAIVED if this test is FAIL

	 ;; NOTES:
	 ;;  1. Is the call to test:get-previous-run-record remotified?
	 ;;  2. Add test for testconfig waiver propagation control here
	 ;;
	 (prev-test   (if (equal? status "FAIL")
			  (open-run-close test:get-previous-test-run-record db run-id test-name item-path)
			  #f))
	 (waived   (if prev-test
		       (if prev-test ;; true if we found a previous test in this run series
			   (let ((prev-status  (db:test-get-status  prev-test))
				 (prev-state   (db:test-get-state   prev-test))
				 (prev-comment (db:test-get-comment prev-test)))
			     (debug:print 4 "prev-status " prev-status ", prev-state " prev-state ", prev-comment " prev-comment)
			     (if (and (equal? prev-state  "COMPLETED")
				      (equal? prev-status "WAIVED"))
				 (if comment
				     comment
				     prev-comment) ;; waived is either the comment or #f
				 #f))
			   #f)
		       #f)))
    (if (and waived 
	     (tests:check-waiver-eligibility testdat prev-test))
	(set! real-status "WAIVED"))

    (debug:print 4 "real-status " real-status ", waived " waived ", status " status)

    ;; update the primary record IF state AND status are defined
    (if (and state status)
	(cdb:test-set-status-state *runremote* test-id real-status state (if waived waived comment)))
    
    ;; if status is "AUTO" then call rollup (note, this one modifies data in test
    ;; run area, it does remote calls under the hood.
    (if (and test-id state status (equal? status "AUTO")) 
	(db:test-data-rollup #f test-id status))

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
	    (cdb:remote-run db:csv->test-data #f test-id
				dat))))
      
    ;; need to update the top test record if PASS or FAIL and this is a subtest
    (if (not (equal? item-path ""))
	(cdb:roll-up-pass-fail-counts *runremote* run-id test-name item-path status))

    (if (or (and (string? comment)
		 (string-match (regexp "\\S+") comment))
	    waived)
	(let ((cmt  (if waived waived comment)))
	  (cdb:remote-run db:test-set-comment #f test-id cmt)))
    ))


(define (tests:test-set-toplog! db run-id test-name logf) 
  (cdb:client-call *runremote* 'tests:test-set-toplog #t 2 logf run-id test-name))

(define (tests:summarize-items db run-id test-name force)
  ;; if not force then only update the record if one of these is true:
  ;;   1. logf is "log/final.log
  ;;   2. logf is same as outputfilename
  (let* ((outputfilename (conc "megatest-rollup-" test-name ".html"))
	 (orig-dir       (current-directory))
	 (logf-info      (cdb:remote-run db:test-get-logfile-info #f run-id test-name))
	 (logf           (if logf-info (cadr logf-info) #f))
	 (path           (if logf-info (car  logf-info) #f)))
    ;; This query finds the path and changes the directory to it for the test
    (if (directory? path)
	(begin
	  (debug:print 4 "Found path: " path)
	  (change-directory path))
	;; (set! outputfilename (conc path "/" outputfilename)))
	(print "No such path: " path))
    (debug:print 4 "summarize-items with logf " logf ", outputfilename " outputfilename " and force " force)
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
		(tot    0)
		(testdat (cdb:remote-run db:test-get-records-for-index-file #f run-id test-name)))
	    (with-output-to-port
		oup
	      (lambda ()
		(set! outtxt (conc outtxt "<html><title>Summary: " test-name 
				   "</title><body><h2>Summary for " test-name "</h2>"))
		(for-each
		 (lambda (testrecord)
		   (let ((id             (vector-ref testrecord 0))
			 (itempath       (vector-ref testrecord 1))
			 (state          (vector-ref testrecord 2))
			 (status         (vector-ref testrecord 3))
			 (run_duration   (vector-ref testrecord 4))
			 (logf           (vector-ref testrecord 5))
			 (comment        (vector-ref testrecord 6)))
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
						   "</tr>"))))
		 testdat)
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
	    ;; NB// tests:test-set-toplog! is remote internal...
	    (tests:test-set-toplog! db run-id test-name outputfilename)
	    )))))

;;======================================================================
;; Gather data from test/task specifications
;;======================================================================

(define (tests:get-valid-tests testsdir test-patts) ;;  #!key (test-names '()))
  (let ((tests (glob (conc testsdir "/tests/*")))) ;; " (string-translate patt "%" "*")))))
    (set! tests (filter (lambda (test)(file-exists? (conc test "/testconfig"))) tests))
    (delete-duplicates
     (filter (lambda (testname)
	       (tests:match test-patts testname #f))
	     (map (lambda (testp)
		    (last (string-split testp "/")))
		  tests)))))

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
(define (tests:filter-non-runnable run-id testkeynames testrecordshash)
  (let ((runnables '()))
    (for-each
     (lambda (testkeyname)
       (let* ((test-record (hash-table-ref testrecordshash testkeyname))
	      (test-name   (tests:testqueue-get-testname  test-record))
	      (itemdat     (tests:testqueue-get-itemdat   test-record))
	      (item-path   (tests:testqueue-get-item_path test-record))
	      (waitons     (tests:testqueue-get-waitons   test-record))
	      (keep-test   #t)
	      (test-id     (cdb:remote-run db:get-test-id #f run-id test-name item-path))
	      (tdat        (cdb:get-test-info-by-id *runremote* test-id)))
	 (if tdat
	     (begin
	       ;; Look at the test state and status
	       (if (or (member (db:test-get-status tdat) 
			       '("PASS" "WARN" "WAIVED" "CHECK" "SKIP"))
		       (member (db:test-get-state tdat)
			       '("INCOMPLETE" "KILLED")))
		   (set! keep-test #f))

	       ;; examine waitons for any fails. If it is FAIL or INCOMPLETE then eliminate this test
	       ;; from the runnable list
	       (if keep-test
		   (for-each (lambda (waiton)
			       ;; for now we are waiting only on the parent test
			       (let* ((parent-test-id (cdb:remote-run db:get-test-id #f run-id waiton ""))
				      (wtdat (cdb:get-test-info-by-id *runremote* test-id)))
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
;; refactoring this block into tests:get-full-data from line 263 of runs.scm
;;======================================================================
;; hed is the test name
;; test-records is a hash of test-name => test record
(define (tests:get-full-data test-names test-records required-tests)
  (if (not (null? test-names))
      (let loop ((hed (car test-names))
		 (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	(debug:print-info 4 "hed=" hed " at top of loop")
	(let* ((config  (tests:get-testconfig hed 'return-procs))
	       (waitons (let ((instr (if config 
					 (config-lookup config "requirements" "waiton")
					 (begin ;; No config means this is a non-existant test
					   (debug:print 0 "ERROR: non-existent required test \"" hed "\", grep through your testconfigs to find and remove or create the test. Discarding and continuing.")
					     ""))))
			  (debug:print-info 8 "waitons string is " instr)
			  (string-split (cond
					 ((procedure? instr)
					  (let ((res (instr)))
					    (debug:print-info 8 "waiton procedure results in string " res " for test " hed)
					    res))
					 ((string? instr)     instr)
					 (else 
					  ;; NOTE: This is actually the case of *no* waitons! ;; (debug:print 0 "ERROR: something went wrong in processing waitons for test " hed)
					  ""))))))
	  (if (not config) ;; this is a non-existant test called in a waiton. 
	      (if (null? tal)
		  test-records
		  (loop (car tal)(cdr tal)))
	      (begin
		(debug:print-info 8 "waitons: " waitons)
		;; check for hed in waitons => this would be circular, remove it and issue an
		;; error
		(if (member hed waitons)
		    (begin
		      (debug:print 0 "ERROR: test " hed " has listed itself as a waiton, please correct this!")
		      (set! waitons (filter (lambda (x)(not (equal? x hed))) waitons))))
		
		;; (items   (items:get-items-from-config config)))
		(if (not (hash-table-ref/default test-records hed #f))
		    (hash-table-set! test-records
				     hed (vector hed     ;; 0
						 config  ;; 1
						 waitons ;; 2
						 (config-lookup config "requirements" "priority")     ;; priority 3
						 (let ((items      (hash-table-ref/default config "items" #f)) ;; items 4
						       (itemstable (hash-table-ref/default config "itemstable" #f))) 
						   ;; if either items or items table is a proc return it so test running
						   ;; process can know to call items:get-items-from-config
						   ;; if either is a list and none is a proc go ahead and call get-items
						   ;; otherwise return #f - this is not an iterated test
						   (cond
						    ((procedure? items)      
						     (debug:print-info 4 "items is a procedure, will calc later")
						     items)            ;; calc later
						    ((procedure? itemstable)
						     (debug:print-info 4 "itemstable is a procedure, will calc later")
						     itemstable)       ;; calc later
						    ((filter (lambda (x)
							       (let ((val (car x)))
								 (if (procedure? val) val #f)))
							     (append (if (list? items) items '())
								     (if (list? itemstable) itemstable '())))
						     'have-procedure)
						    ((or (list? items)(list? itemstable)) ;; calc now
						     (debug:print-info 4 "items and itemstable are lists, calc now\n"
								       "    items: " items " itemstable: " itemstable)
						     (items:get-items-from-config config))
						    (else #f)))                           ;; not iterated
						 #f      ;; itemsdat 5
						 #f      ;; spare - used for item-path
						 )))
		(for-each 
		 (lambda (waiton)
		   (if (and waiton (not (member waiton test-names)))
		       (begin
			 (set! required-tests (cons waiton required-tests))
			 (set! test-names (cons waiton test-names))))) ;; was an append, now a cons
		 waitons)
		(let ((remtests (delete-duplicates (append waitons tal))))
		  (if (not (null? remtests))
		      (loop (car remtests)(cdr remtests))
		      test-records))))))))

;;======================================================================
;; test steps
;;======================================================================

;; teststep-set-status! used to be here

(define (test-get-kill-request test-id) ;; run-id test-name itemdat)
  (let* (;; (item-path (item-list->path itemdat))
	 (testdat   (cdb:get-test-info-by-id *runremote* test-id))) ;; run-id test-name item-path)))
    (equal? (test:get-state testdat) "KILLREQ")))

(define (test:tdb-get-rundat-count tdb)
  (if tdb
      (let ((res 0))
	(sqlite3:for-each-row
	 (lambda (count)
	   (set! res count))
	 tdb
	 "SELECT count(id) FROM test_rundat;")
	res))
  0)

(define (db:update-central-meta-info db test-id cpuload diskfree minutes num-records uname hostname)
  (sqlite3:execute db "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;"
		   cpuload
		   diskfree
		   test-id)
  (if minutes (sqlite3:execute db "UPDATE tests SET run_duration=? WHERE id=?;" minutes test-id))
  (if (eq? num-records 0)
      (sqlite3:execute db "UPDATE tests SET uname=?,host=? WHERE id=?;"
		       uname hostname test-id)))

(define (test-set-meta-info db test-id run-id testname itemdat minutes)
  ;; DOES cdb:remote-run under the hood!
  (let* ((tdb         (db:open-test-db-by-test-id db test-id))
	 (num-records (test:tdb-get-rundat-count tdb))
	 (cpuload  (get-cpu-load))
	 (diskfree (get-df (current-directory))))
    (if (eq? (modulo num-records 10) 0) ;; every ten records update central
	(let ((uname    (get-uname "-srvpio"))
	      (hostname (get-host-name)))
	  (cdb:remote-run db:update-central-meta-info db test-id cpuload diskfree minutes num-records uname hostname)))
    (sqlite3:execute tdb "INSERT INTO test_rundat (update_time,cpuload,diskfree,run_duration) VALUES (strftime('%s','now'),?,?,?);"
		     cpuload diskfree minutes)))
	  
;;======================================================================
;; A R C H I V I N G
;;======================================================================

(define (test:archive db test-id)
  #f)

(define (test:archive-tests db keynames target)
  #f)

