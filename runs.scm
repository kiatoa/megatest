
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

;; register a test run with the db
(define (register-run db keys) ;; test-name)
  (let* ((keystr    (keys->keystr keys))
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (keyvallst (keys->vallist keys)) ;; extracts the values from remainder of (argv)
	 (runname   (get-with-default ":runname" #f))
	 (state     (get-with-default ":state" "no"))
	 (status    (get-with-default ":status" "n/a"))
	 (allvals   (append (list runname state status user) keyvallst))
	 (qryvals   (append (list runname) keyvallst))
	 (key=?str  (string-intersperse (map (lambda (k)(conc (key:get-fieldname k) "=?")) keys) " AND ")))
    (debug:print 3 "keys: " keys " allvals: " allvals " keyvallst: " keyvallst)
    (debug:print 2 "NOTE: using key " (string-intersperse keyvallst "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvallst))) ;; there must be a better way to "apply and"
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
	  (sqlite3:execute db "UPDATE runs SET state=?,status=? WHERE id=?;" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
	  #f))))

;; runs:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
;;  to extract info from the structure returned
;;
(define (runs:get-runs-by-patt db keys runnamepatt . params) ;; test-name)
  (let* ((keyvallst (keys->vallist keys))
	 (tmp      (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time")))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (res     '())
	 (key-patt ""))
    (for-each (lambda (keyval)
		(let* ((key    (vector-ref keyval 0))
		       (fulkey (conc ":" key))
		       (patt   (args:get-arg fulkey)))
		  (if patt
		      (set! key-patt (conc key-patt " AND " key " like '" patt "'"))
		      (begin
			(debug:print 0 "ERROR: searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keys)
    (sqlite3:for-each-row 
     (lambda (a . r)
       (set! res (cons (list->vector (cons a r)) res)))
     db 
     (conc "SELECT " keystr " FROM runs WHERE runname like ? " key-patt ";")
     runnamepatt)
    (vector header res)))

(define (register-test db run-id test-name item-path tags)
  (let ((item-paths (if (equal? item-path "")
			(list item-path)
			(list item-path ""))))
    (for-each 
     (lambda (pth)
       (sqlite3:execute db "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status,tags) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a',?);" 
			run-id 
			test-name
			pth 
			(conc "," (string-intersperse tags ",") ",")))
     item-paths )))

;;  (define db (open-db))
;;  (test-set-status! db 2 "runfirst" "COMPLETED" "PASS" "summer")

(define (test-set-status! db run-id test-name state status itemdat-or-path . comment)
  (let ((item-path (if (string? itemdat-or-path) itemdat-or-path (item-list->path itemdat-or-path))))
    (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
		     state status run-id test-name item-path)
    (if (and (not (equal? item-path "")) ;; need to update the top test record if PASS or FAIL and this is a subtest
	     (or (equal? status "PASS")
		 (equal? status "WARN")
		 (equal? status "FAIL")))
	(begin
	  (sqlite3:execute 
	   db
	   "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status='FAIL'),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND (status='PASS' OR status='WARN'))
             WHERE run_id=? AND testname=? AND item_path='';"
	   run-id test-name run-id test-name run-id test-name)
	  (sqlite3:execute
	   db
	   "UPDATE tests
             SET state=CASE WHEN (SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND state in ('RUNNING','NOT_STARTED')) > 0 THEN 
                          'RUNNING'
                       ELSE 'COMPLETED' END,
                status=CASE WHEN fail_count > 0 THEN 'FAIL' WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' ELSE 'UNKNOWN' END
             WHERE run_id=? AND testname=? AND item_path='';"
	   run-id test-name run-id test-name)))
    (if (and (not (null? comment))
	     (car comment))
	(sqlite3:execute db "UPDATE tests SET comment=? WHERE run_id=? AND testname=? AND item_path=?;"
			 (car comment) run-id test-name item-path))))

(define (test-set-log! db run-id test-name itemdat logf) 
  (let ((item-path (item-list->path itemdat)))
    (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path=?;" 
		     logf run-id test-name item-path)))

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

;; ;; TODO: Converge this with db:get-test-info
;; (define (runs:get-test-info db run-id test-name item-path)
;;   (let ((res #f)) ;; (vector #f #f #f #f #f #f)))
;;     (sqlite3:for-each-row 
;;      (lambda (id run-id test-name state status)
;;        (set! res (vector id run-id test-name state status item-path)))
;;      db "SELECT id,run_id,testname,state,status FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
;;      run-id test-name item-path)
;;     res))

(define-inline (test:get-id vec)       (vector-ref vec 0))
(define-inline (test:get-run_id vec)   (vector-ref vec 1))
(define-inline (test:get-test-name vec)(vector-ref vec 2))
(define-inline (test:get-state vec)    (vector-ref vec 3))
(define-inline (test:get-status vec)   (vector-ref vec 4))
(define-inline (test:get-item-path vec)(vector-ref vec 5))

(define (runs:test-get-full-path test)
  (let* ((testname (db:test-get-testname   test))
	 (itempath (db:test-get-item-path test)))
    (conc testname (if (equal? itempath "") "" (conc "(" itempath ")")))))

(define-inline (test:test-get-fullname test)
   (conc (db:test-get-testname test)
	 (if (equal? (db:test-get-item-path test) "")
	     ""
	     (conc "(" (db:test-get-item-path test) ")"))))

(define (check-valid-items class item)
  (let ((valid-values (let ((s (config-lookup *configdat* "validvalues" class)))
			(if s (string-split s) #f))))
    (if valid-values
	(if (member item valid-values)
	    item #f)
	item)))

(define (teststep-set-status! db run-id test-name teststep-name state-in status-in itemdat comment)
  (debug:print 4 "run-id: " run-id " test-name: " test-name)
  (let* ((state     (check-valid-items "state" state-in))
	 (status    (check-valid-items "status" status-in))
	 (item-path (item-list->path itemdat))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    (debug:print 5 "testdat: " testdat)
    (if (and testdat ;; if the section exists then force specification BUG, I don't like how this works.
	     (or (not state)(not status)))
	(debug:print 0 "WARNING: Invalid " (if status "status" "state")
	       " value \"" (if status status-in state-in) "\", update your validstates section in megatest.config"))
    (if testdat
	(let ((test-id (test:get-id testdat)))
	  (sqlite3:execute db 
			"INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment) VALUES(?,?,?,?,strftime('%s','now'),?);"
			test-id teststep-name state status (if comment comment "")))
	(debug:print 0 "ERROR: Can't update " test-name " for run " run-id " -> no such test in db"))))

(define (test-get-kill-request db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    (equal? (test:get-state testdat) "KILLREQ")))

(define (test-set-meta-info db run-id testname itemdat)
  (let ((item-path (item-list->path itemdat))
	(cpuload  (get-cpu-load))
	(hostname (get-host-name))
	(diskfree (get-df (current-directory)))
	(uname    (get-uname "-srvpio"))
	(runpath  (current-directory)))
    (sqlite3:execute db "UPDATE tests SET host=?,cpuload=?,diskfree=?,uname=?,rundir=? WHERE run_id=? AND testname=? AND item_path=?;"
		  hostname
		  cpuload
		  diskfree
		  uname
		  runpath
		  run-id
		  testname
		  item-path)))

(define (test-update-meta-info db run-id testname itemdat minutes)
  (let ((item-path (item-list->path itemdat))
	(cpuload  (get-cpu-load))
	(diskfree (get-df (current-directory))))
    (if (not cpuload)  (begin (debug:print 0 "WARNING: CPULOAD not found.")  (set! cpuload "n/a")))
    (if (not diskfree) (begin (debug:print 0 "WARNING: DISKFREE not found.") (set! diskfree "n/a")))
    (if (not item-path)(begin (debug:print 0 "WARNING: ITEMPATH not set.")   (set! item-path "")))
    ;; (let ((testinfo (db:get-test-info db run-id testname item-path)))
    ;;   (if (and (not (equal? (db:test-get-status testinfo) "COMPLETED"))
    ;;            (not (equal? (db:test-get-status testinfo) "KILLREQ"))
    (sqlite3:execute
     db
     "UPDATE tests SET cpuload=?,diskfree=?,run_duration=?,state='RUNNING' WHERE run_id=? AND testname=? AND item_path=? AND state NOT IN ('COMPLETED','KILLREQ','KILLED');"
     cpuload
     diskfree
     minutes
     run-id
     testname
     item-path)))

(define (set-megatest-env-vars db run-id)
  (let ((keys (db-get-keys db)))
    (for-each (lambda (key)
		(sqlite3:for-each-row
		 (lambda (val)
		   (debug:print 2 "setenv " (key:get-fieldname key) " " val)
		   (setenv (key:get-fieldname key) val))
		 db 
		 (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")
		 run-id))
	      keys)))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (debug:print 2 "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

(define (get-all-legal-tests)
  (let* ((tests  (glob (conc *toppath* "/tests/*")))
	 (res    '()))
    (debug:print 4 "INFO: Looking at tests " (string-intersperse tests ","))
    (for-each (lambda (testpath)
		(if (file-exists? (conc testpath "/testconfig"))
		    (set! res (cons (last (string-split testpath "/")) res))))
	      tests)
    res))

(define (runs:can-run-more-tests db)
  (let ((num-running (db:get-count-tests-running db))
	(max-concurrent-jobs (config-lookup *configdat* "setup" "max_concurrent_jobs")))
    (debug:print 2 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
    (if (not (eq? 0 *globalexitstatus*))
	#f
	(if (or (not max-concurrent-jobs)
		(and max-concurrent-jobs
		     (string->number max-concurrent-jobs)
		     (not (>= num-running (string->number max-concurrent-jobs)))))
	    #t
	    (begin 
	      (debug:print 0 "WARNING: Max running jobs exceeded, current number running: " num-running 
			   ", max_concurrent_jobs: " max-concurrent-jobs)
	      #f)))))
  
(define (run-tests db test-names)
  (let* ((keys        (db-get-keys db))
	 (keyvallst   (keys->vallist keys #t))
	 (run-id      (register-run db keys))  ;;  test-name)))
	 (deferred    '())) ;; delay running these since they have a waiton clause
    ;; on the first pass or call to run-tests set FAILS to NOT_STARTED if
    ;; -keepgoing is specified
    (if (and (eq? *passnum* 0)
	     (args:get-arg "-keepgoing"))
	(begin
	  ;; have to delete test records where NOT_STARTED since they can cause -keepgoing to 
	  ;; get stuck due to becoming inaccessible from a failed test. I.e. if test B depends 
	  ;; on test A but test B reached the point on being registered as NOT_STARTED and test
	  ;; A failed for some reason then on re-run using -keepgoing the run can never complete.
	  (db:delete-tests-in-state db run-id "NOT_STARTED")
	  (db:set-tests-state-status db run-id test-names #f "FAIL" "NOT_STARTED" "FAIL")))
    (set! *passnum* (+ *passnum* 1))
    (let loop ((numtimes 0))
      (for-each 
       (lambda (test-name)
	 (if (runs:can-run-more-tests db)
	     (run-one-test db run-id test-name keyvallst)
	     ;; add some delay 
	     ;(sleep 2)
	     ))
       test-names)
      ;; (run-waiting-tests db)
      (if (args:get-arg "-keepgoing")
	  (let ((estrem (db:estimated-tests-remaining db run-id)))
	    (if (and (> estrem 0)
		     (eq? *globalexitstatus* 0))
		(begin
		  (debug:print 1 "Keep going, estimated " estrem " tests remaining to run, will continue in 3 seconds ...")
		  (sleep 3)
		  (run-waiting-tests db)
		  (loop (+ numtimes 1)))))))))
	   
;; VERY INEFFICIENT! Move stuff that should be done once up to calling proc
(define (run-one-test db run-id test-name keyvallst)
  (debug:print 1 "Launching test " test-name)
  ;; All these vars might be referenced by the testconfig file reader
  (setenv "MT_TEST_NAME" test-name) ;; 
  (setenv "MT_RUNNAME"   (args:get-arg ":runname"))
  (set-megatest-env-vars db run-id) ;; these may be needed by the launching process
  (change-directory *toppath*)
  (let* ((test-path    (conc *toppath* "/tests/" test-name))
	 (test-configf (conc test-path "/testconfig"))
	 (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
	 (test-conf    (if testexists (read-config test-configf) (make-hash-table)))
	 (waiton       (let ((w (config-lookup test-conf "requirements" "waiton")))
			 (if (string? w)(string-split w)'())))
	 (tags         (let ((t (config-lookup test-conf "setup" "tags")))
			 (if (string? t)(string-split t ",") '()))))
    (if (not testexists)
	(begin
	  (debug:print 0 "ERROR: Can't find config file " test-configf)
	  (exit 2))
	;; put top vars into convenient variables and open the db
	(let* (;; db is always at *toppath*/db/megatest.db
	       (items       (hash-table-ref/default test-conf "items" '()))
	       (itemstable  (hash-table-ref/default test-conf "itemstable" '()))
	       (allitems    (if (or (not (null? items))(not (null? itemstable)))
				(append (item-assoc->item-list items)
					(item-table->item-list itemstable))
				'(()))) ;; a list with one null list is a test with no items
	       (runconfigf  (conc  *toppath* "/runconfigs.config")))
	  (debug:print 1 "items: ")
	  (if (>= *verbosity* 1)(pp allitems))
	  (if (>= *verbosity* 5)
	      (begin
		(print "items: ")(pp (item-assoc->item-list items))
		(print "itestable: ")(pp (item-table->item-list itemstable))))
	  (if (args:get-arg "-m")
	      (db:set-comment-for-run db run-id (args:get-arg "-m")))
	  ;; braindead work-around for poorly specified allitems list BUG!!! FIXME
	  (if (null? allitems)(set! allitems '(())))
	  (let loop ((itemdat (car allitems))
		     (tal     (cdr allitems)))
	    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
	    ;; Handle lists of items
	    (let* ((item-path     (item-list->path itemdat)) ;; (string-intersperse (map cadr itemdat) "/"))
		   (new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
		   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
		   (testdat   #f)
		   (num-running (db:get-count-tests-running db))
		   (max-concurrent-jobs (config-lookup *configdat* "setup" "max_concurrent_jobs"))
		   (parent-test (and (not (null? items))(equal? item-path "")))
		   (single-test (and (null? items) (equal? item-path "")))
		   (item-test   (not (equal? item-path ""))))
	      (debug:print 3 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	      (if (runs:can-run-more-tests db)
		  (begin
		    (let loop2 ((ts (db:get-test-info db run-id test-name item-path)) ;; #f)
				(ct 0))
		      (if (and (not ts)
			       (< ct 10))
			  (begin
			    (register-test db run-id test-name item-path tags)
			    (db:test-set-comment db run-id test-name item-path "")
			    (loop2 (db:get-test-info db run-id test-name item-path)
				   (+ ct 1)))
			  (if ts
			      (set! testdat ts)
			      (begin
				(debug:print 0 "WARNING: Couldn't register test " test-name " with item path " item-path ", skipping")
				(if (not (null? tal))
				    (loop (car tal)(cdr tal)))))))
		    (change-directory test-path)
		    ;; this block is here only to inform the user early on
		    (if (file-exists? runconfigf)
			(setup-env-defaults db runconfigf run-id *already-seen-runconfig-info*)
			(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))
		    (debug:print 4 "run-id: " run-id " test-name: " test-name " item-path: " item-path " testdat: " (test:get-status testdat) " test-state: " (test:get-state testdat))
		    (case (if (args:get-arg "-force")
			      'NOT_STARTED
			      (if testdat
				  (string->symbol (test:get-state testdat))
				  'failed-to-insert))
		      ((failed-to-insert)
		       (debug:print 0 "ERROR: Failed to insert the record into the db"))
		      ((NOT_STARTED COMPLETED)
		       (debug:print 6 "Got here, " (test:get-state testdat))
		       (let ((runflag #f))
			 (cond
			  ;; i.e. this is the parent test to a suite of items, never "run" it
			  (parent-test
			   (set! runflag #f))
			  ;; -force, run no matter what
			  ((args:get-arg "-force")(set! runflag #t))
			  ;; NOT_STARTED, run no matter what
			  ((equal? (test:get-state testdat) "NOT_STARTED")(set! runflag #t))
			  ;; not -rerun and PASS, WARN or CHECK, do no run
			  ((and (or (not (args:get-arg "-rerun"))
				    (args:get-arg "-keepgoing"))
				(member (test:get-status testdat) '("PASS" "WARN" "CHECK")))
			   (set! runflag #f))
			  ;; -rerun and status is one of the specifed, run it
			  ((and (args:get-arg "-rerun")
				(let ((rerunlst (string-split (args:get-arg "-rerun") ","))) ;; FAIL,
				  (member (test:get-status testdat) rerunlst)))
			   (set! runflag #t))
			  ;; -keepgoing, do not rerun FAIL
			  ((and (args:get-arg "-keepgoing")
				(member (test:get-status testdat) '("FAIL")))
			   (set! runflag #f))
			  ((and (not (args:get-arg "-rerun"))
				(member (test:get-status testdat) '("FAIL" "n/a")))
			   (set! runflag #t))
			  (else (set! runflag #f)))
			 (debug:print 6 "RUNNING => runflag: " runflag " STATE: " (test:get-state testdat) " STATUS: " (test:get-status testdat))
			 (if (not runflag)
			     (if (not parent-test)
				 (debug:print 1 "NOTE: Not starting test " new-test-name " as it is state \"COMPLETED\" and status \"" (test:get-status testdat) "\", use -force to override"))
			     (let* ((get-prereqs-cmd (lambda ()
						       (db-get-prereqs-not-met db run-id waiton))) ;; check before running ....
				    (launch-cmd      (lambda ()
						       (launch-test db run-id test-conf keyvallst test-name test-path itemdat)))
				    (testrundat      (list get-prereqs-cmd launch-cmd)))
			       (if (or (args:get-arg "-force")
				       (let ((preqs-not-yet-met ((car testrundat))))
					 (debug:print 2 "Preqrequesites for " test-name ": " preqs-not-yet-met)
					 (null? preqs-not-yet-met))) ;; are there any tests that must be run before this one...
				   (if (not ((cadr testrundat))) ;; this is the line that launches the test to the remote host
				       (begin
					 (print "ERROR: Failed to launch the test. Exiting as soon as possible")
					 (set! *globalexitstatus* 1) ;; 
					 (process-signal (current-process-id) signal/kill)
					 ;(exit 1)
					 ))
				   (if (not (args:get-arg "-keepgoing"))
				       (hash-table-set! *waiting-queue* new-test-name testrundat)))))))
		      ((KILLED) 
		       (debug:print 1 "NOTE: " new-test-name " is already running or was explictly killed, use -force to launch it."))
		      ((LAUNCHED REMOTEHOSTSTART RUNNING)  
		       (if (> (- (current-seconds)(+ (db:test-get-event_time testdat)
						     (db:test-get-run_duration testdat)))
			      100) ;; i.e. no update for more than 100 seconds
			   (begin
			     (debug:print 0 "WARNING: Test " test-name " appears to be dead. Forcing it to state INCOMPLETE and status STUCK/DEAD")
			     (test-set-status! db run-id test-name "INCOMPLETE" "STUCK/DEAD" itemdat "Test is stuck or dead"))
			   (debug:print 2 "NOTE: " test-name " is already running")))
		      (else       (debug:print 0 "ERROR: Failed to launch test " new-test-name ". Unrecognised state " (test:get-state testdat))))))
	      (if (not (null? tal))
		  (loop (car tal)(cdr tal)))))))))

(define (run-waiting-tests db)
  (let ((numtries           0)
	(last-try-time      (current-seconds))
	(times              (list 1))) ;; minutes to wait before trying again to kick off runs
    ;; BUG this hack of brute force retrying works quite well for many cases but 
    ;;     what is needed is to check the db for tests that have failed less than
    ;;     N times or never been started and kick them off again
    (let loop ((waiting-test-names (hash-table-keys *waiting-queue*)))
      (cond
       ((not (runs:can-run-more-tests db))
	(sleep 2)
	(loop waiting-test-names))
       ((null? waiting-test-names)
	(debug:print 1 "All tests launched"))
       (else
	(set! numtries (+ numtries 1))
	(for-each (lambda (testname)
		    (if (runs:can-run-more-tests db)
			(let* ((testdat (hash-table-ref *waiting-queue* testname))
			       (prereqs ((car testdat)))
			       (ldb     (if db db (open-db))))
			  (debug:print 2 "prereqs remaining: " prereqs)
			  (if (null? prereqs)
			      (begin
				(debug:print 2 "Prerequisites met, launching " testname)
				((cadr testdat))
				(hash-table-delete! *waiting-queue* testname)))
			  (if (not db)
			      (sqlite3:finalize! ldb)))))
		  waiting-test-names)
	;; (sleep 10) ;; no point in rushing things at this stage?
	(loop (hash-table-keys *waiting-queue*)))))))

(define (get-dir-up-n dir . params) 
  (let ((dparts  (string-split dir "/"))
	(count   (if (null? params) 1 (car params))))
    (conc "/" (string-intersperse 
	       (take dparts (- (length dparts) count))
	       "/"))))
;; Remove runs
;; fields are passing in through 
(define (runs:remove-runs db runnamepatt testpatt itempatt)
  (let* ((keys        (db-get-keys db))
	 (rundat      (runs:get-runs-by-patt db keys runnamepatt))
	 (header      (vector-ref rundat 0))
	 (runs        (vector-ref rundat 1)))
    (debug:print 1 "Header: " header)
    (for-each
     (lambda (run)
       (let ((runkey (string-intersperse (map (lambda (k)
						(db:get-value-by-header run header (vector-ref k 0))) keys) "/")))
	 (let* ((run-id (db:get-value-by-header run header "id") )
		(tests  (db-get-tests-for-run db (db:get-value-by-header run header "id") testpatt itempatt))
		(lasttpath "/does/not/exist/I/hope"))
	   (if (not (null? tests))
	       (begin
		 (debug:print 1 "Removing tests for run: " runkey " " (db:get-value-by-header run header "runname"))
		 (for-each
		  (lambda (test)
		    (let* ((item-path (db:test-get-item-path test))
			   (test-name (db:test-get-testname test))
			   (run-dir   (db:test-get-rundir test)))
		      (debug:print 1 "  " (db:test-get-testname test) " id: " (db:test-get-id test) " " item-path)
		      (db:delete-test-records db (db:test-get-id test))
		      (if (> (string-length run-dir) 5) ;; bad heuristic but should prevent /tmp /home etc.
			  (let ((fullpath run-dir)) ;; "/" (db:test-get-item-path test))))
			    (set! lasttpath fullpath)
			    (debug:print 1 "rm -rf " fullpath)
			    (system (conc "rm -rf " fullpath))
			    (let* ((dirs-count (+ 1 (length keys)(length (string-split item-path "/"))))
				   (dir-to-rem (get-dir-up-n fullpath dirs-count))
				   (remainingd (string-substitute (regexp (conc "^" dir-to-rem "/")) "" fullpath))
				   (cmd (conc "cd " dir-to-rem "; rmdir -p " remainingd )))
			      (if (file-exists? fullpath)
				  (begin
				    (debug:print 1 cmd)
				    (system cmd)))
			      ))
			    )))
		    tests)))
	   (let ((remtests (db-get-tests-for-run db (db:get-value-by-header run header "id"))))
	     (if (null? remtests) ;; no more tests remaining
		 (let* ((dparts  (string-split lasttpath "/"))
			(runpath (conc "/" (string-intersperse 
					    (take dparts (- (length dparts) 1))
					    "/"))))
		   (debug:print 1 "Removing run: " runkey " " (db:get-value-by-header run header "runname"))
		   (db:delete-run db run-id)
		   ;; need to figure out the path to the run dir and remove it if empty
		   ;;    (if (null? (glob (conc runpath "/*")))
		   ;;        (begin
		   ;; 	 (debug:print 1 "Removing run dir " runpath)
		   ;; 	 (system (conc "rmdir -p " runpath))))
		   ))))
	 ))
     runs)))

;;======================================================================
;; Routines for manipulating runs
;;======================================================================

(define (runs:rollup-run db keys n)
  (let* ((new-run-id   (register-run db keys))
	 (similar-runs (db:get-similar-runs db keys))
	 (tests-n-days (db:get-tests-n-days db similar-runs)))
    (for-each 
     (lambda (test-id)
       (db:rollup-test db run-id test-id))
     tests-n-days)))
