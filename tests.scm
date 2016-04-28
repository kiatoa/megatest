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
;; Tests
;;======================================================================

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking tcp directory-utils)
(import (prefix sqlite3 sqlite3:))
(require-library stml)

(declare (unit tests))
(declare (uses lock-queue))
(declare (uses db))
(declare (uses tdb))
(declare (uses common))
;; (declare (uses dcommon)) ;; needed for the steps processing
(declare (uses items))
(declare (uses runconfig))
;; (declare (uses sdb))
(declare (uses server))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

;; Call this one to do all the work and get a standardized list of tests
;;   gets paths from configs and finds valid tests 
;;   returns hash of testname --> fullpath
;;
(define (tests:get-all)
  (let* ((test-search-path   (tests:get-tests-search-path *configdat*)))
    (tests:get-valid-tests (make-hash-table) test-search-path)))

(define (tests:get-tests-search-path cfgdat)
  (let ((paths (map cadr (configf:get-section cfgdat "tests-paths"))))
    (filter (lambda (d)
	      (if (directory-exists? d)
		  d
		  (begin
		    (if (common:low-noise-print 60 "tests:get-tests-search-path" d)
			(debug:print 0 "WARNING: problem with directory " d ", dropping it from tests path"))
		    #f)))
	    (append paths (list (conc *toppath* "/tests"))))))

(define (tests:get-valid-tests test-registry tests-paths)
  (if (null? tests-paths) 
      test-registry
      (let loop ((hed (car tests-paths))
		 (tal (cdr tests-paths)))
	(if (file-exists? hed)
	    (for-each (lambda (test-path)
			(let* ((tname   (last (string-split test-path "/")))
			       (tconfig (conc test-path "/testconfig")))
			  (if (and (not (hash-table-ref/default test-registry tname #f))
				   (file-exists? tconfig))
			      (hash-table-set! test-registry tname test-path))))
		      (glob (conc hed "/*"))))
	(if (null? tal)
	    test-registry
	    (loop (car tal)(cdr tal))))))

(define (tests:filter-test-names test-names test-patts)
  (delete-duplicates
   (filter (lambda (testname)
	     (tests:match test-patts testname #f))
	   test-names)))

;; itemmap is a list of testname patterns to maps
;;     test1 .*/bar/(\d+) foo/\1
;;     %     foo/([^/]+)  \1/bar
;;
;; # NOTE: the line with the single % could be the result of
;; #       itemmap entry in requirements (legacy). The itemmap
;; #       requirements entry is deprecated
;;
(define (tests:get-itemmaps tconfig)
  (let ((base-itemmap  (configf:lookup tconfig "requirements" "itemmap"))
	(itemmap-table (configf:get-section tconfig "itemmap")))
    (append (if base-itemmap
		(list (list "%" base-itemmap))
		'())
	    (if itemmap-table
		itemmap-table
		'()))))

;; given a list of itemmaps (testname . map), return the first match
;;
(define (tests:lookup-itemmap itemmaps testname)
  (let ((best-matches (filter (lambda (itemmap)
				(tests:match (car itemmap) testname #f))
			      itemmaps)))
    (if (null? best-matches)
	#f
	(let ((res (car best-matches)))
	  ;; (debug:print 0 "res=" res)
	  (cond
	   ((string? res) res) ;;; FIX THE ROOT CAUSE HERE ....
	   ((null? res)   #f)
	   ((string? (cdr res)) (cdr res))  ;; it is a pair
	   ((string? (cadr res))(cadr res)) ;; it is a list
	   (else cadr res))))))

;; returns waitons waitors tconfigdat
;;
(define (tests:get-waitons test-name all-tests-registry)
   (let* ((config  (tests:get-testconfig test-name all-tests-registry 'return-procs)))
     (let ((instr (if config 
		      (config-lookup config "requirements" "waiton")
		      (begin ;; No config means this is a non-existant test
			(debug:print 0 "ERROR: non-existent required test \"" test-name "\"")
			(exit 1))))
	   (instr2 (if config
		       (config-lookup config "requirements" "waitor")
		       "")))
       (debug:print-info 8 "waitons string is " instr ", waitors string is " instr2)
       (let ((newwaitons
	      (string-split (cond
			     ((procedure? instr)
			      (let ((res (instr)))
				(debug:print-info 8 "waiton procedure results in string " res " for test " test-name)
				res))
			     ((string? instr)     instr)
			     (else 
			      ;; NOTE: This is actually the case of *no* waitons! ;; (debug:print 0 "ERROR: something went wrong in processing waitons for test " test-name)
			      ""))))
	     (newwaitors
	      (string-split (cond
			     ((procedure? instr2)
			      (let ((res (instr2)))
				(debug:print-info 8 "waitor procedure results in string " res " for test " test-name)
				res))
			     ((string? instr2)     instr2)
			     (else 
			      ;; NOTE: This is actually the case of *no* waitons! ;; (debug:print 0 "ERROR: something went wrong in processing waitons for test " test-name)
			      "")))))
	 (values
	  ;; the waitons
	  (filter (lambda (x)
		    (if (hash-table-ref/default all-tests-registry x #f)
			#t
			(begin
			  (debug:print 0 "ERROR: test " test-name " has unrecognised waiton testname " x)
			  #f)))
		  newwaitons)
	  (filter (lambda (x)
		    (if (hash-table-ref/default all-tests-registry x #f)
			#t
			(begin
			  (debug:print 0 "ERROR: test " test-name " has unrecognised waiton testname " x)
			  #f)))
		  newwaitors)
	  config)))))
					     
;; given waiting-test that is waiting on waiton-test extend test-patt appropriately
;;
;;  genlib/testconfig               sim/testconfig
;;  genlib/sch                      sim/sch/cell1
;;
;;  [requirements]                  [requirements]
;;                                  mode itemwait
;;                                  # trim off the cell to determine what to run for genlib
;;                                  itemmap /.*
;;
;;                                  waiting-test is waiting on waiton-test so we need to create a pattern for waiton-test given waiting-test and itemmap
(define (tests:extend-test-patts test-patt waiting-test waiton-test itemmaps)
  (let* ((itemmap          (tests:lookup-itemmap itemmaps waiton-test))
	 (patts            (string-split test-patt ","))
	 (waiting-test-len (+ (string-length waiting-test) 1))
	 (patts-waiton     (map (lambda (x)  ;; for each incoming patt that matches the waiting test
				  (let* ((modpatt (if itemmap (db:convert-test-itempath x itemmap) x)) 
					 (newpatt (conc waiton-test "/" (substring modpatt waiting-test-len (string-length modpatt)))))
				    ;; (conc waiting-test "/," waiting-test "/" (substring modpatt waiton-test-len (string-length modpatt)))))
				    ;; (print "in map, x=" x ", newpatt=" newpatt)
				    newpatt))
				(filter (lambda (x)
					  (eq? (substring-index (conc waiting-test "/") x) 0)) ;; is this patt pertinent to the waiting test
					patts))))
    (string-intersperse (delete-duplicates (append patts (if (null? patts-waiton)
							     (list (conc waiton-test "/%")) ;; really shouldn't add the waiton forcefully like this
							     patts-waiton)))
			",")))
  
;; tests:glob-like-match 
(define (tests:glob-like-match patt str) 
  (let ((like (substring-index "%" patt)))
    (let* ((notpatt  (equal? (substring-index "~" patt) 0))
	   (newpatt  (if notpatt (substring patt 1) patt))
	   (finpatt  (if like
			(string-substitute (regexp "%") ".*" newpatt #f)
			(string-substitute (regexp "\\*") ".*" newpatt #f)))
	   (res      #f))
      ;; (print "tests:glob-like-match => notpatt: " notpatt ", newpatt: " newpatt ", finpatt: " finpatt)
      (set! res (string-match (regexp finpatt (if like #t #f)) str))
      (if notpatt (not res) res))))

;; if itempath is #f then look only at the testname part
;;
(define (tests:match patterns testname itempath #!key (required '()))
  (if (string? patterns)
      (let ((patts (append (string-split patterns ",") required)))
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

;; Check for waiver eligibility
;;
(define (tests:check-waiver-eligibility testdat prev-testdat)
  (let* ((test-registry (make-hash-table))
	 (testconfig  (tests:get-testconfig (db:test-get-testname testdat) test-registry #f))
	 (test-rundir ;; (sdb:qry 'passstr 
	  (db:test-get-rundir testdat)) ;; )
	 (prev-rundir ;; (sdb:qry 'passstr 
	  (db:test-get-rundir prev-testdat)) ;; )
	 (waivers     (if testconfig (configf:section-vars testconfig "waivers") '()))
	 (waiver-rx   (regexp "^(\\S+)\\s+(.*)$"))
	 (diff-rule   "diff %file1% %file2%")
	 (logpro-rule "diff %file1% %file2% | logpro %waivername%.logpro %waivername%.html"))
    (if (not (file-exists? test-rundir))
	(begin
	  (debug:print 0 "ERROR: test run directory is gone, cannot propagate waiver")
	  #f)
	(begin
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
	    result)))))

(define (tests:test-force-state-status! run-id test-id state status)
  (rmt:test-set-status-state run-id test-id status state #f)
  (mt:process-triggers run-id test-id state status))

;; Do not rpc this one, do the underlying calls!!!
(define (tests:test-set-status! run-id test-id state status comment dat #!key (work-area #f))
  (let* ((real-status status)
	 (otherdat    (if dat dat (make-hash-table)))
	 (testdat     (rmt:get-test-info-by-id run-id test-id))
	 (test-name   (db:test-get-testname  testdat))
	 (item-path   (db:test-get-item-path testdat))
	 ;; before proceeding we must find out if the previous test (where all keys matched except runname)
	 ;; was WAIVED if this test is FAIL

	 ;; NOTES:
	 ;;  1. Is the call to test:get-previous-run-record remotified?
	 ;;  2. Add test for testconfig waiver propagation control here
	 ;;
	 (prev-test   (if (equal? status "FAIL")
			  (rmt:get-previous-test-run-record run-id test-name item-path)
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
	(begin
	  (rmt:test-set-status-state run-id test-id real-status state (if waived waived comment))
	  (mt:process-triggers run-id test-id state real-status)))
    
    ;; if status is "AUTO" then call rollup (note, this one modifies data in test
    ;; run area, it does remote calls under the hood.
    (if (and test-id state status (equal? status "AUTO")) 
	(rmt:test-data-rollup run-id test-id status))

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
	    ;; This was run remote, don't think that makes sense. Perhaps not, but that is the easiest path for the moment.
	    (rmt:csv->test-data run-id test-id
				dat))))
      
    ;; need to update the top test record if PASS or FAIL and this is a subtest
    (if (not (equal? item-path ""))
	(rmt:roll-up-pass-fail-counts run-id test-name item-path state status))

    (if (or (and (string? comment)
		 (string-match (regexp "\\S+") comment))
	    waived)
	(let ((cmt  (if waived waived comment)))
	  (rmt:general-call 'set-test-comment run-id cmt test-id)))))

(define (tests:test-set-toplog! run-id test-name logf) 
  (rmt:general-call 'tests:test-set-toplog run-id logf run-id test-name))

(define (tests:summarize-items run-id test-id test-name force)
  ;; if not force then only update the record if one of these is true:
  ;;   1. logf is "log/final.log
  ;;   2. logf is same as outputfilename
  (let* ((outputfilename (conc "megatest-rollup-" test-name ".html"))
	 (orig-dir       (current-directory))
	 (logf-info      (rmt:test-get-logfile-info run-id test-name))
	 (logf           (if logf-info (cadr logf-info) #f))
	 (path           (if logf-info (car  logf-info) #f)))
    ;; This query finds the path and changes the directory to it for the test
    (if (and (string? path)
	     (directory? path)) ;; can get #f here under some wierd conditions. why, unknown ...
	(begin
	  (debug:print 4 "Found path: " path)
	  (change-directory path))
	;; (set! outputfilename (conc path "/" outputfilename)))
	(debug:print 0 "ERROR: summarize-items for run-id=" run-id ", test-name=" test-name ", no such path: " path))
    (debug:print 4 "summarize-items with logf " logf ", outputfilename " outputfilename " and force " force)
    (if (or (equal? logf "logs/final.log")
	    (equal? logf outputfilename)
	    force)
	(let ((my-start-time (current-seconds))
	      (lockf         (conc outputfilename ".lock")))
	  (let loop ((have-lock  (common:simple-file-lock lockf)))
	    (if have-lock
		(let ((script (configf:lookup *configdat* "testrollup" test-name)))
		  (print "Obtained lock for " outputfilename)
		  ;; (rmt:top-test-set-per-pf-counts run-id test-name)
		  (rmt:roll-up-pass-fail-counts run-id test-name "" #f #f)
		  (rmt:top-test-set-per-pf-counts run-id test-name)
		  (if script
		      (system (conc script " > " outputfilename " & "))
		      (tests:generate-html-summary-for-iterated-test run-id test-id test-name outputfilename))
		  (common:simple-file-release-lock lockf)
		  (change-directory orig-dir)
		  ;; NB// tests:test-set-toplog! is remote internal...
		  (tests:test-set-toplog! run-id test-name outputfilename))
		;; didn't get the lock, check to see if current update started later than this 
		;; update, if so we can exit without doing any work
		(if (> my-start-time (file-modification-time lockf))
		    ;; we started since current re-gen in flight, delay a little and try again
		    (begin
		      (debug:print-info 1 "Waiting to update " outputfilename ", another test currently updating it")
		      (thread-sleep! (+ 5 (random 5))) ;; delay between 5 and 10 seconds
		      (loop (common:simple-file-lock lockf))))))))))

(define (tests:generate-html-summary-for-iterated-test run-id test-id test-name outputfilename)
  (let ((counts              (make-hash-table))
	(statecounts         (make-hash-table))
	(outtxt              "")
	(tot                 0)
	(testdat             (rmt:test-get-records-for-index-file run-id test-name)))
    (with-output-to-file outputfilename
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
				;; "<td><a href=\"" itempath "/" logf "\"> " itempath "</a></td>" 
				"<td><a href=\"" itempath "/test-summary.html\"> " itempath "</a></td>" 
				"<td>" state    "</td>" 
				"<td><font color=" (common:get-color-from-status status)
				">"   status   "</font></td>"
				"<td>" (if (equal? comment "")
					   "&nbsp;"
					   comment) "</td>"
					   "</tr>"))))
	 (if (list? testdat)
	     testdat
	     (begin
	       (print "ERROR: failed to get records with rmt:test-get-records-for-index-file run-id=" run-id "test-name=" test-name)
	       '())))
	
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
	;; (release-dot-lock outputfilename)
	;;(rmt:update-run-stats 
	;; run-id
	;; (hash-table-map
	;;  state-status-counts
	;;  (lambda (key val)
	;;	(append key (list val)))))
	))))

;; CHECK - WAS THIS ADDED OR REMOVED? MANUAL MERGE WITH API STUFF!!!
;;
;; get a pretty table to summarize steps
;;
;; (define (dcommon:process-steps-table steps);; db test-id #!key (work-area #f))
(define (tests:process-steps-table steps);; db test-id #!key (work-area #f))
;;  (let ((steps   (db:get-steps-for-test db test-id work-area: work-area)))
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
      res))


;; temporarily passing in dbstruct to support direct access (i.e. bypassing servers)
;;
(define (tests:get-compressed-steps dbstruct run-id test-id)
  (let* ((steps-data  (if dbstruct 
			  (db:get-steps-for-test dbstruct run-id test-id)
			  (rmt:get-steps-for-test run-id test-id))) 
	 (comprsteps  (tests:process-steps-table steps-data))) ;; (open-run-close db:get-steps-table #f test-id work-area: work-area)))
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


;; summarize test
(define (tests:summarize-test run-id test-id)
  (let* ((test-dat  (rmt:get-test-info-by-id run-id test-id))
	 (steps-dat (rmt:get-steps-for-test run-id test-id))
	 (test-name (db:test-get-testname test-dat))
	 (item-path (db:test-get-item-path test-dat))
	 (full-name (db:test-make-full-name test-name item-path))
	 (oup       (open-output-file (conc (db:test-get-rundir test-dat) "/test-summary.html")))
	 (status    (db:test-get-status   test-dat))
	 (color     (common:get-color-from-status status))
	 (logf      (db:test-get-final_logf test-dat))
	 (steps-dat (tests:get-compressed-steps #f run-id test-id)))
    ;; (dcommon:get-compressed-steps #f 1 30045)
    ;; (#("wasting_time" "23:36:13" "23:36:21" "0" "8.0s" "wasting_time.log"))

    (s:output-new
     oup
     (s:html
      (s:title "Summary for " full-name)
      (s:body 
       (s:h2 "Summary for " full-name)
       (s:table 'cellspacing "0" 'border "1"
	(s:tr (s:td "run id")   (s:td (db:test-get-run_id   test-dat))
	      (s:td "test id")  (s:td (db:test-get-id       test-dat)))
	(s:tr (s:td "testname") (s:td test-name)
	      (s:td "itempath") (s:td item-path))
	(s:tr (s:td "state")    (s:td (db:test-get-state    test-dat))
	      (s:td "status")   (s:td (s:a 'href logf (s:font 'color color status))))
	(s:tr (s:td "TestDate") (s:td (seconds->work-week/day-time 
				       (db:test-get-event_time test-dat)))
	      (s:td "Duration") (s:td (seconds->hr-min-sec (db:test-get-run_duration test-dat)))))
       (s:h3 "Log files")
       (s:table
	'cellspacing "0" 'border "1"
	(s:tr (s:td "Final log")(s:td (s:a 'href logf logf))))
       (s:table
	'cellspacing "0" 'border "1"
	(s:tr (s:td "Step Name")(s:td "Start")(s:td "End")(s:td "Status")(s:td "Duration")(s:td "Log File"))
	(map (lambda (step-dat)
	       (s:tr (s:td (tdb:steps-table-get-stepname step-dat))
		     (s:td (tdb:steps-table-get-start    step-dat))
		     (s:td (tdb:steps-table-get-end      step-dat))
		     (s:td (tdb:steps-table-get-status   step-dat))
		     (s:td (tdb:steps-table-get-runtime  step-dat))
		     (s:td (let ((step-log (tdb:steps-table-get-log-file step-dat)))
			     (s:a 'href step-log step-log)))))
	     steps-dat))
	)))
    (close-output-port oup)))
	  
	  
;; MUST BE CALLED local!
;;
(define (tests:test-get-paths-matching keynames target fnamepatt #!key (res '()))
  ;; BUG: Move the values derived from args to parameters and push to megatest.scm
  (let* ((testpatt   (if (args:get-arg "-testpatt")(args:get-arg "-testpatt") "%"))
	 (statepatt  (if (args:get-arg ":state")   (args:get-arg ":state")    "%"))
	 (statuspatt (if (args:get-arg ":status")  (args:get-arg ":status")   "%"))
	 (runname    (if (args:get-arg ":runname") (args:get-arg ":runname")  "%"))
	 (paths-from-db (rmt:test-get-paths-matching-keynames-target-new keynames target res
					testpatt
					statepatt
					statuspatt
					runname)))
    (if fnamepatt
	(apply append 
	       (map (lambda (p)
		      (if (directory-exists? p)
			  (glob (conc p "/" fnamepatt))
			  '()))
		    paths-from-db))
	paths-from-db)))

			      
;;======================================================================
;; Gather data from test/task specifications
;;======================================================================

;; (define (tests:get-valid-tests testsdir test-patts) ;;  #!key (test-names '()))
;;   (let ((tests (glob (conc testsdir "/tests/*")))) ;; " (string-translate patt "%" "*")))))
;;     (set! tests (filter (lambda (test)(file-exists? (conc test "/testconfig"))) tests))
;;     (delete-duplicates
;;      (filter (lambda (testname)
;; 	       (tests:match test-patts testname #f))
;; 	     (map (lambda (testp)
;; 		    (last (string-split testp "/")))
;; 		  tests)))))

(define (tests:get-test-path-from-environment)
  (and (getenv "MT_LINKTREE")
       (getenv "MT_TARGET")
       (getenv "MT_RUNNAME")
       (getenv "MT_TEST_NAME")
       (getenv "MT_ITEMPATH")
       (conc (getenv "MT_LINKTREE")  "/"
	     (getenv "MT_TARGET")    "/"
	     (getenv "MT_RUNNAME")   "/"
	     (getenv "MT_TEST_NAME") "/"
	     (if (or (getenv "MT_ITEMPATH")
		     (not (string=? "" (getenv "MT_ITEMPATH"))))
		 (conc "/" (getenv "MT_ITEMPATH"))))))

;; if .testconfig exists in test directory read and return it
;; else if have cached copy in *testconfigs* return it IFF there is a section "have fulldata"
;; else read the testconfig file
;;   if have path to test directory save the config as .testconfig and return it
;;
(define (tests:get-testconfig test-name test-registry system-allowed #!key (force-create #f))
  (let* ((cache-path   (tests:get-test-path-from-environment))
	 (cache-file   (and cache-path (conc cache-path "/.testconfig")))
	 (cache-exists (and cache-file
			    (not force-create)  ;; if force-create then pretend there is no cache to read
			    (file-exists? cache-file)))
	 (cached-dat   (if (and (not force-create)
				cache-exists)
			   (handle-exceptions
			    exn
			    #f ;; any issues, just give up with the cached version and re-read
			    (configf:read-alist cache-file))
			   #f)))
    (if cached-dat
	cached-dat
	(let ((dat (hash-table-ref/default *testconfigs* test-name #f)))
	  (if (and  dat ;; have a locally cached version
		    (hash-table-ref/default dat "have fulldata" #f)) ;; marked as good data?
	      dat
	      ;; no cached data available
	      (let* ((treg         (or test-registry
				       (tests:get-all)))
		     (test-path    (or (hash-table-ref/default treg test-name #f)
				       (conc *toppath* "/tests/" test-name)))
		     (test-configf (conc test-path "/testconfig"))
		     (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
		     (tcfg         (if testexists
				       (read-config test-configf #f system-allowed
						    environ-patt: (if system-allowed
								      "pre-launch-env-vars"
								      #f))
				       #f)))
		(if cache-file (hash-table-set! tcfg "have fulldata" #t)) ;; mark this as fully read data
		(if tcfg (hash-table-set! *testconfigs* test-name tcfg))
		(if (and testexists
			 cache-file
			 (file-write-access? cache-path))
		    (let ((tpath (conc cache-path "/.testconfig")))
		      (debug:print-info 1 "Caching testconfig for " test-name " in " tpath)
		      (configf:write-alist tcfg tpath)))
		tcfg))))))
  
;; sort tests by priority and waiton
;; Move test specific stuff to a test unit FIXME one of these days
(define (tests:sort-by-priority-and-waiton test-records)
  (let* ((mungepriority (lambda (priority)
			  (if priority
			      (let ((tmp (any->number priority)))
				(if tmp tmp (begin (debug:print 0 "ERROR: bad priority value " priority ", using 0") 0)))
			      0)))
	 (all-tests      (hash-table-keys test-records))
	 (all-waited-on  (let loop ((hed (car all-tests))
				    (tal (cdr all-tests))
				    (res '()))
			   (let* ((trec    (hash-table-ref test-records hed))
				  (waitons (or (tests:testqueue-get-waitons trec) '())))
			     (if (null? tal)
				 (append res waitons)
				 (loop (car tal)(cdr tal)(append res waitons))))))
	 (sort-fn1 
	  (lambda (a b)
	    (let* ((a-record   (hash-table-ref test-records a))
		   (b-record   (hash-table-ref test-records b))
		   (a-waitons  (or (tests:testqueue-get-waitons a-record) '()))
		   (b-waitons  (or (tests:testqueue-get-waitons b-record) '()))
		   (a-config   (tests:testqueue-get-testconfig  a-record))
		   (b-config   (tests:testqueue-get-testconfig  b-record))
		   (a-raw-pri  (config-lookup a-config "requirements" "priority"))
		   (b-raw-pri  (config-lookup b-config "requirements" "priority"))
		   (a-priority (mungepriority a-raw-pri))
		   (b-priority (mungepriority b-raw-pri)))
	      (tests:testqueue-set-priority! a-record a-priority)
	      (tests:testqueue-set-priority! b-record b-priority)
	      ;; (debug:print 0 "a=" a ", b=" b ", a-waitons=" a-waitons ", b-waitons=" b-waitons)
	      (cond
	       ;; is 
	       ((member a b-waitons)          ;; is b waiting on a?
		;; (debug:print 0 "case1")
		#t)
	       ((member b a-waitons)          ;; is a waiting on b?
		;; (debug:print 0 "case2")
		#f)
	       ((and (not (null? a-waitons))  ;; both have waitons - do not disturb
		     (not (null? b-waitons)))
		;; (debug:print 0 "case2.1")
		#t)
	       ((and (null? a-waitons)        ;; no waitons for a but b has waitons
		     (not (null? b-waitons)))
		;; (debug:print 0 "case3")
		#f)
	       ((and (not (null? a-waitons))  ;; a has waitons but b does not
		     (null? b-waitons)) 
		;; (debug:print 0 "case4")
		#t)
	       ((not (eq? a-priority b-priority)) ;; use
		(> a-priority b-priority))
	       (else
		;; (debug:print 0 "case5")
		(string>? a b))))))
	 
	 (sort-fn2
	  (lambda (a b)
	    (> (mungepriority (tests:testqueue-get-priority (hash-table-ref test-records a)))
	       (mungepriority (tests:testqueue-get-priority (hash-table-ref test-records b)))))))
    ;; (let ((dot-res (tests:run-dot (tests:tests->dot test-records) "plain")))
    ;;   (debug:print "dot-res=" dot-res))
    ;; (let ((data (map cdr (filter
    ;;     		  (lambda (x)(equal? "node" (car x)))
    ;;     		  (map string-split (tests:easy-dot test-records "plain"))))))
    ;;   (map car (sort data (lambda (a b)
    ;;     		    (> (string->number (caddr a))(string->number (caddr b)))))))
    ;; ))
    (sort all-tests sort-fn1))) ;; avoid dealing with deleted tests, look at the hash table

(define (tests:easy-dot test-records outtype)
  (let-values (((fd temp-path) (file-mkstemp (conc "/tmp/" (current-user-name) ".XXXXXX"))))
    (let ((all-testnames (hash-table-keys test-records))
	  (temp-port     (open-output-file* fd)))
      ;; (format temp-port "This file is ~A.~%" temp-path)
      (format temp-port "digraph tests {\n")
      ;; (format temp-port "   splines=none\n")
      (for-each
       (lambda (testname)
	 (let* ((testrec (hash-table-ref test-records testname))
		(waitons (or (tests:testqueue-get-waitons testrec) '())))
	   (for-each
	    (lambda (waiton)
	      (format temp-port (conc "   " waiton " -> " testname " [splines=ortho]\n")))
	    waitons)))
       all-testnames)
      (format temp-port "}\n")
      (close-output-port temp-port)
      (with-input-from-pipe
       (conc "env -i PATH=$PATH dot -T" outtype " < " temp-path)
       (lambda ()
	 (let ((res (read-lines)))
	   ;; (delete-file temp-path)
	   res))))))

(define (tests:write-dot-file test-records fname)
  (if (file-write-access? (pathname-directory fname))
      (with-output-to-file fname
	(lambda ()
	  (map print (tests:tests->dot test-records))))))

(define (tests:tests->dot test-records)
  (let ((all-testnames (hash-table-keys test-records)))
    (if (null? all-testnames)
	'()
	(let loop ((hed (car all-testnames))
		   (tal (cdr all-testnames))
		   (res (list "digraph tests {")))
	  (let* ((testrec (hash-table-ref test-records hed))
		 (waitons (or (tests:testqueue-get-waitons testrec) '()))
		 (newres  (append res
				  (if (null? waitons)
				      (list (conc "   \"" hed "\";"))
				      (map (lambda (waiton)
					     (conc "   \"" waiton "\" -> \"" hed "\";"))
					   waitons)
				      ))))
	    (if (null? tal)
		(append newres (list "}"))
		(loop (car tal)(cdr tal) newres)
		))))))

;; (tests:run-dot (list "digraph tests {" "a -> b" "}") "plain")

(define (tests:run-dot indat outtype) ;; outtype is plain, fig, dot, etc. http://www.graphviz.org/content/output-formats
  (let-values (((inp oup pid)(process "env -i PATH=$PATH dot" (list "-T" outtype))))
    (with-output-to-port oup
      (lambda ()
	(map print indat)))
    (close-output-port oup)
    (let ((res (with-input-from-port inp
		 (lambda ()
		   (read-lines)))))
      (close-input-port inp)
      res)))

;; read data from tmp file or create if not exists
;; if exists regen in background
;;
(define (tests:lazy-dot testrecords  outtype)
  (let ((dfile (conc "/tmp/." (current-user-name) "-" (server:mk-signature) ".dot"))
	(fname (conc "/tmp/." (current-user-name) "-" (server:mk-signature) ".dotdat")))
    (tests:write-dot-file testrecords dfile)
    (if (file-exists? fname)
	(let ((res (with-input-from-file fname
		     (lambda ()
		       (read-lines)))))
	  (system (conc "env -i PATH=$PATH dot -T " outtype " < " dfile " > " fname "&"))
	  res)
	(begin
	  (system (conc "env -i PATH=$PATH dot -T " outtype " < " dfile " > " fname))
	  (with-input-from-file fname
	    (lambda ()
	      (read-lines)))))))
	  

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
	      (test-id     (rmt:get-test-id run-id test-name item-path))
	      (tdat        (rmt:get-testinfo-state-status run-id test-id))) ;; (cdb:get-test-info-by-id *runremote* test-id)))
	 (if tdat
	     (begin
	       ;; Look at the test state and status
	       (if (or (and (member (db:test-get-status tdat) 
				    '("PASS" "WARN" "WAIVED" "CHECK" "SKIP"))
			    (equal? (db:test-get-state tdat) "COMPLETED"))
		       (member (db:test-get-state tdat)
				    '("INCOMPLETE" "KILLED")))
		   (set! keep-test #f))

	       ;; examine waitons for any fails. If it is FAIL or INCOMPLETE then eliminate this test
	       ;; from the runnable list
	       (if keep-test
		   (for-each (lambda (waiton)
			       ;; for now we are waiting only on the parent test
			       (let* ((parent-test-id (rmt:get-test-id run-id waiton ""))
				      (wtdat          (rmt:get-testinfo-state-status run-id test-id))) ;; (cdb:get-test-info-by-id *runremote* test-id)))
				 (if (or (and (equal? (db:test-get-state wtdat) "COMPLETED")
					      (member (db:test-get-status wtdat) '("FAIL" "ABORT")))
					 (member (db:test-get-status wtdat)  '("KILLED"))
					 (member (db:test-get-state wtdat)   '("INCOMPETE")))
				 ;; (if (or (member (db:test-get-status wtdat)
				 ;;        	 '("FAIL" "KILLED"))
				 ;;         (member (db:test-get-state wtdat)
				 ;;        	 '("INCOMPETE")))
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
(define (tests:get-full-data test-names test-records required-tests all-tests-registry)
  (if (not (null? test-names))
      (let loop ((hed (car test-names))
		 (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	(debug:print-info 4 "hed=" hed " at top of loop")
	(let* ((config  (tests:get-testconfig hed all-tests-registry 'return-procs))
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

(define (test-get-kill-request run-id test-id) ;; run-id test-name itemdat)
  (let* ((testdat   (rmt:get-test-info-by-id run-id test-id)))
    (and testdat
	 (equal? (test:get-state testdat) "KILLREQ"))))

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

(define (tests:update-central-meta-info run-id test-id cpuload diskfree minutes uname hostname)
  (if (and cpuload diskfree)
      (rmt:general-call 'update-cpuload-diskfree run-id cpuload diskfree test-id))
  (if minutes 
      (rmt:general-call 'update-run-duration run-id minutes test-id))
  (if (and uname hostname)
      (rmt:general-call 'update-uname-host run-id uname hostname test-id)))
  
;; This one is for running with no db access (i.e. via rmt: internally)
(define (tests:set-full-meta-info db test-id run-id minutes work-area remtries)
;; (define (tests:set-full-meta-info test-id run-id minutes work-area)
;;  (let ((remtries 10))
  (let* ((cpuload  (get-cpu-load))
	 (diskfree (get-df (current-directory)))
	 (uname    (get-uname "-srvpio"))
	 (hostname (get-host-name)))
    (tests:update-central-meta-info run-id test-id cpuload diskfree minutes uname hostname)))
    
;; (define (tests:set-partial-meta-info test-id run-id minutes work-area)
(define (tests:set-partial-meta-info test-id run-id minutes work-area remtries)
  (let* ((cpuload  (get-cpu-load))
	 (diskfree (get-df (current-directory)))
	 (remtries 10))
    (handle-exceptions
     exn
     (if (> remtries 0)
	 (begin
	   (print-call-chain (current-error-port))
	   (debug:print-info 0 "WARNING: failed to set meta info. Will try " remtries " more times")
	   (set! remtries (- remtries 1))
	   (thread-sleep! 10)
	   (tests:set-full-meta-info db test-id run-id minutes work-area (- remtries 1)))
	 (let ((err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
	   (debug:print 0 "ERROR: tried for over a minute to update meta info and failed. Giving up")
	   (debug:print 0 "EXCEPTION: database probably overloaded or unreadable.")
	   (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	   (print "exn=" (condition->list exn))
	   (debug:print 0 " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
	   (print-call-chain (current-error-port))))
     (tests:update-testdat-meta-info db test-id work-area cpuload diskfree minutes)
  )))
	 
;;======================================================================
;; A R C H I V I N G
;;======================================================================

(define (test:archive db test-id)
  #f)

(define (test:archive-tests db keynames target)
  #f)

