
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking)
(import (prefix sqlite3 sqlite3:))

(declare (unit runs))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
(declare (uses tests))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

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

;; ;; TODO: Converge this with db:get-test-info
;; (define (runs:get-test-info db run-id test-name item-path)
;;   (let ((res #f)) ;; (vector #f #f #f #f #f #f)))
;;     (sqlite3:for-each-row 
;;      (lambda (id run-id test-name state status)
;;        (set! res (vector id run-id test-name state status item-path)))
;;      db "SELECT id,run_id,testname,state,status FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
;;      run-id test-name item-path)
;;     res))

(define (runs:test-get-full-path test)
  (let* ((testname (db:test-get-testname   test))
	 (itempath (db:test-get-item-path test)))
    (conc testname (if (equal? itempath "") "" (conc "(" itempath ")")))))


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
	      keys)
    ;; Lets use this as an opportunity to put MT_RUNNAME in the environment
    (sqlite3:for-each-row
     (lambda (runname)
       (setenv "MT_RUNNAME" runname))
     db
     "SELECT runname FROM runs WHERE id=?;"
     run-id)
    ))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (debug:print 2 "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

(define (runs:can-run-more-tests db test-record)
  (let* ((tconfig                 (tests:testqueue-get-testconfig test-record))
	 (jobgroup                (config-lookup tconfig "requirements" "jobgroup"))
	 (num-running             (db:get-count-tests-running db))
	 (num-running-in-jobgroup (db:get-count-tests-running-in-jobgroup db jobgroup))
	 (max-concurrent-jobs     (config-lookup *configdat* "setup"     "max_concurrent_jobs"))
	 (job-group-limit         (config-lookup *configdat* "jobgroups" jobgroup)))
    (debug:print 2 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
    (if (not (eq? 0 *globalexitstatus*))
	#f
	(let ((can-not-run-more (cond
				 ;; if max-concurrent-jobs is set and the number running is greater 
				 ;; than it than cannot run more jobs
				 ((and max-concurrent-jobs
				       (string->number max-concurrent-jobs)
				       (>= num-running (string->number max-concurrent-jobs)))
				  (debug:print 0 "WARNING: Max running jobs exceeded, current number running: " num-running 
					       ", max_concurrent_jobs: " max-concurrent-jobs)
				  #t)
				 ;; if job-group-limit is set and number of jobs in the group is greater
				 ;; than the limit then cannot run more jobs of this kind
				 ((and job-group-limit
				       (>= num-running-in-jobgroup job-group-limit))
				  (debug:print 1 "WARNING: number of jobs " num-running-in-jobgroup 
					       " in " jobgroup " exceeded, will not run " (tests:testqueue-get-testname test-record))
				  #t)
				 (else #f))))
	  (not can-not-run-more)))))

;;======================================================================
;; New methodology. These routines will replace the above in time. For
;; now the code is duplicated. This stuff is initially used in the monitor
;; based code.
;;======================================================================

;; register a test run with the db
(define (runs:register-run db keys keyvallst runname state status user)
  (debug:print 3 "runs:register-run, keys: " keys " keyvallst: " keyvallst " runname: " runname " state: " state " status: " status " user: " user)
  (let* ((keystr    (keys->keystr keys))
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (keyvals   (map cadr keyvallst))
	 (allvals   (append (list runname state status user) keyvals))
	 (qryvals   (append (list runname) keyvals))
	 (key=?str  (string-intersperse (map (lambda (k)(conc (key:get-fieldname k) "=?")) keys) " AND ")))
    (debug:print 3 "keys: " keys " allvals: " allvals " keyvals: " keyvals)
    (debug:print 2 "NOTE: using target " (string-intersperse keyvals "/") " for this run")
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
	  (sqlite3:execute db "UPDATE runs SET state=?,status=? WHERE id=?;" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
	  #f))))

;; This is a duplicate of run-tests (which has been deprecated). Use this one instead of run tests.
;; keyvals
(define (runs:run-tests db target runname test-patts item-patts user flags)
  (let* ((keys        (db-get-keys db))
	 (keyvallst   (keys:target->keyval keys target))
	 (run-id      (runs:register-run db keys keyvallst runname "new" "n/a" user))  ;;  test-name)))
	 (deferred    '()) ;; delay running these since they have a waiton clause
	 ;; keepgoing is the defacto modality now, will add hit-n-run a bit later
	 ;; (keepgoing   (hash-table-ref/default flags "-keepgoing" #f))
	 (test-names  '())
	 (runconfigf   (conc  *toppath* "/runconfigs.config"))
	 (required-tests '())
	 (test-records (make-hash-table)))

    (set-megatest-env-vars db run-id) ;; these may be needed by the launching process

    (if (file-exists? runconfigf)
	(setup-env-defaults db runconfigf run-id *already-seen-runconfig-info* "pre-launch-env-vars")
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))
    
    ;; look up all tests matching the comma separated list of globs in
    ;; test-patts (using % as wildcard)
    (for-each 
     (lambda (patt)
       (let ((tests (glob (conc *toppath* "/tests/" (string-translate patt "%" "*")))))
	 (set! tests (filter (lambda (test)(file-exists? (conc test "/testconfig"))) tests))
	 (set! test-names (append test-names 
				  (map (lambda (testp)
					 (last (string-split testp "/")))
				       tests)))))
     (if test-patts (string-split test-patts ",")(list "%")))

     ;; now remove duplicates
    (set! test-names (delete-duplicates test-names))

    (debug:print 0 "INFO: test names " test-names)

    ;; on the first pass or call to run-tests set FAILS to NOT_STARTED if
    ;; -keepgoing is specified
    (if (eq? *passnum* 0)
	(begin
	  ;; have to delete test records where NOT_STARTED since they can cause -keepgoing to 
	  ;; get stuck due to becoming inaccessible from a failed test. I.e. if test B depends 
	  ;; on test A but test B reached the point on being registered as NOT_STARTED and test
	  ;; A failed for some reason then on re-run using -keepgoing the run can never complete.
	  (db:delete-tests-in-state db run-id "NOT_STARTED")
	  (db:set-tests-state-status db run-id test-names #f "FAIL" "NOT_STARTED" "FAIL")))

    ;; now add non-directly referenced dependencies (i.e. waiton)
    (if (not (null? test-names))
	(let loop ((hed (car test-names))
		   (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	  (let* ((config  (test:get-testconfig hed 'return-procs))
		 (waitons (string-split (let ((w (config-lookup config "requirements" "waiton")))
					  (if w w "")))))
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
						 (debug:print 4 "INFO: items is a procedure, will calc later")
						 items)            ;; calc later
						((procedure? itemstable)
						 (debug:print 4 "INFO: itemstable is a procedure, will calc later")
						 itemstable)       ;; calc later
						((filter (lambda (x)
							   (let ((val (car x)))
							     (if (procedure? val) val #f)))
							 (append (if (list? items) items '())
								 (if (list? itemstable) itemstable '())))
						 'have-procedure)
						((or (list? items)(list? itemstable)) ;; calc now
						 (debug:print 4 "INFO: items and itemstable are lists, calc now\n"
							      "    items: " items " itemstable: " itemstable)
						 (items:get-items-from-config config))
						(else #f)))                           ;; not iterated
					     #f      ;; itemsdat 5
					     ;; #f      ;; spare
					     )))
	    (for-each 
	     (lambda (waiton)
	       (if (and waiton (not (member waiton test-names)))
		   (begin
		     (set! required-tests (cons waiton required-tests))
		     (set! test-names (append test-names (list waiton))))))
	     waitons)
	    (let ((remtests (delete-duplicates (append waitons tal))))
	      (if (not (null? remtests))
		  (loop (car remtests)(cdr remtests)))))))

    (if (not (null? required-tests))
	(debug:print 1 "INFO: Adding " required-tests " to the run queue"))
    ;; NOTE: these are all parent tests, items are not expanded yet.
    (runs:run-tests-queue db run-id runname test-records keyvallst flags)))

(define (runs:run-tests-queue db run-id runname test-records keyvallst flags)
    ;; At this point the list of parent tests is expanded 
    ;; NB// Should expand items here and then insert into the run queue.
  (debug:print 5 "test-records: " test-records ", keyvallst: " keyvallst)
  (let ((sorted-test-names (tests:sort-by-priority-and-waiton test-records))
	(item-patts        (hash-table-ref/default flags "-itempatt" #f)))
    (let loop (; (numtimes 0) ;; shouldn't need this
	       (hed         (car sorted-test-names))
	       (tal         (cdr sorted-test-names)))
      (let* ((test-record (hash-table-ref test-records hed))
	     (tconfig     (tests:testqueue-get-testconfig test-record))
	     (waitons     (tests:testqueue-get-waitons    test-record))
	     (priority    (tests:testqueue-get-priority   test-record))
	     (itemdat     (tests:testqueue-get-itemdat    test-record))
	     (items       (tests:testqueue-get-items      test-record))
	     (item-path   (item-list->path itemdat)))
	(debug:print 6
		     "itemdat:     " itemdat
		     "\n  items:     " items
		     "\n  item-path: " item-path)
	(cond
	 ((not items) ;; when false the test is ok to be handed off to launch (but not before)
	  (let ((have-resources  (runs:can-run-more-tests db test-record)) ;; look at the test jobgroup and tot jobs running
		(prereqs-not-met (db:get-prereqs-not-met db run-id waitons item-path)))
	    ;; Don't know at this time if the test have been launched at some time in the past
	    ;; i.e. is this a re-launch?
	    (if (and have-resources
		     (null? prereqs-not-met))
		;; no loop - drop though and use the loop at the bottom 
		(run:test db run-id runname keyvallst test-record flags #f)
		;; else the run is stuck, temporarily or permanently
		(let ((newtal (append tal (list hed))))
		  ;; couldn't run, take a breather
		  (thread-sleep! 1)
		  (loop (car newtal)(cdr newtal))))))
	 
	 ;; case where an items came in as a list been processed
	 ((and (list? items)     ;; thus we know our items are already calculated
	       (not   itemdat)) ;; and not yet expanded into the list of things to be done
	  (if (>= *verbosity* 1)(pp items))
	  ;; (if (>= *verbosity* 5)
	  ;;     (begin
	  ;;       (print "items: ")     (pp (item-assoc->item-list items))
	  ;;       (print "itemstable: ")(pp (item-table->item-list itemstable))))
	  (for-each
	   (lambda (my-itemdat)
	     (let* ((new-test-record (let ((newrec (make-tests:testqueue)))
				       (vector-copy! test-record newrec)
				       newrec))
		    (my-item-path (item-list->path my-itemdat))
		    (item-matches (if item-patts       ;; here we are filtering for matches with -itempatt
				      (let ((res #f))	 ;; look through all the item-patts if defined, format is patt1,patt2,patt3 ... wildcard is %
					(for-each 
					 (lambda (patt)
					   (if (string-search (glob->regexp
							       (string-translate patt "%" "*"))
							      item-path)
					       (set! res #t)))
					 (string-split item-patts ","))
					res)
				      #t)))
	       (if item-matches ;; yes, we want to process this item
		   (let ((newtestname (conc hed "/" my-item-path))) 
		     (tests:testqueue-set-items!   new-test-record #f)
		     (tests:testqueue-set-itemdat! new-test-record my-itemdat)
		     (hash-table-set! test-records newtestname new-test-record)
		     (set! tal (cons newtestname tal)))))) ;; since these are itemized create new test names testname/itempath
	   items)
	  (loop (car tal)(cdr tal)))

	 ;; if items is a proc then need to run items:get-items-from-config, get the list and loop 
	 ;;    - but only do that if resources exist to kick off the job
	 ((or (procedure? items)(eq? items 'have-procedure))
	  (if (runs:can-run-more-tests db test-record)
	      (let ((items-list (items:get-items-from-config tconfig)))
		(if (list? items-list)
		    (begin
		      (tests:testqueue-set-items! test-record items-list)
		      (loop hed tal))
		    (begin
		      (debug:print 0 "ERROR: The proc from reading the setup did not yield a list - please report this")
		      (exit 1))))
	      (let ((newtal (append tal (list hed))))
		;; if can't run more tests, lets take a breather
		(thread-sleep! 1)
		(loop (car newtal)(cdr newtal)))))

	 ;; this case should not happen, added to help catch any bugs
	 ((and (list? items) itemdat)
	  (debug:print 0 "ERROR: Should not have a list of items in a test and the itemspath set - please report this")
	  (exit 1))))
	
      ;; we get here on "drop through" - loop for next test in queue
      (if (null? tal)
	  (debug:print 1 "INFO: All tests launched")
	  (loop (car tal)(cdr tal))))))

;; parent-test is there as a placeholder for when parent-tests can be run as a setup step
(define (run:test db run-id runname keyvallst test-record flags parent-test)
  ;; All these vars might be referenced by the testconfig file reader
  (let* ((test-name    (tests:testqueue-get-testname   test-record))
	 (test-waitons (tests:testqueue-get-waitons    test-record))
	 (test-conf    (tests:testqueue-get-testconfig test-record))
	 (itemdat      (tests:testqueue-get-itemdat    test-record))
	 (test-path    (conc *toppath* "/tests/" test-name)) ;; could use test:get-testconfig here ...
	 (force        (hash-table-ref/default flags "-force" #f))
	 (rerun        (hash-table-ref/default flags "-rerun" #f))
	 (keepgoing    (hash-table-ref/default flags "-keepgoing" #f))
	 (item-path     ""))
    (debug:print 5
		 "test-config: " (hash-table->alist test-conf)
		 "\n   itemdat: " itemdat
		 )
    ;; setting itemdat to a list if it is #f
    (if (not itemdat)(set! itemdat '()))
    (set! item-path (item-list->path itemdat))
    (debug:print 2 "Attempting to launch test " test-name "/" item-path)
    (setenv "MT_TEST_NAME" test-name) ;; 
    (setenv "MT_RUNNAME"   runname)
    (set-megatest-env-vars db run-id) ;; these may be needed by the launching process
    (change-directory *toppath*)

    ;; Here is where the test_meta table is best updated
    (runs:update-test_meta db test-name test-conf)
    
    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
    (let* ((new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
	   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
	   (testdat       (db:get-test-info db run-id test-name item-path)))
      (if (not testdat)
	  (begin
	    (register-test db run-id test-name item-path)
	    (set! testdat (db:get-test-info db run-id test-name item-path))))
      (change-directory test-path)
      (case (if force ;; (args:get-arg "-force")
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
	    ;; -force, run no matter what
	    (force (set! runflag #t))
	    ;; NOT_STARTED, run no matter what
	    ((equal? (test:get-state testdat) "NOT_STARTED")(set! runflag #t))
	    ;; not -rerun and PASS, WARN or CHECK, do no run
	    ((and (or (not rerun)
		      keepgoing)
		  ;; Require to force re-run for COMPLETED or *anything* + PASS,WARN or CHECK
		  (or (member (test:get-status testdat) '("PASS" "WARN" "CHECK"))
		      (member (test:get-state  testdat) '("COMPLETED")))) 
	     (set! runflag #f))
	    ;; -rerun and status is one of the specifed, run it
	    ((and rerun
		  (let ((rerunlst (string-split rerun ","))) ;; FAIL,
		    (member (test:get-status testdat) rerunlst)))
	     (set! runflag #t))
	    ;; -keepgoing, do not rerun FAIL
	    ((and keepgoing
		  (member (test:get-status testdat) '("FAIL")))
	     (set! runflag #f))
	    ((and (not rerun)
		  (member (test:get-status testdat) '("FAIL" "n/a")))
	     (set! runflag #t))
	    (else (set! runflag #f)))
	   (debug:print 6 "RUNNING => runflag: " runflag " STATE: " (test:get-state testdat) " STATUS: " (test:get-status testdat))
	   (if (not runflag)
	       (if (not parent-test)
		   (debug:print 1 "NOTE: Not starting test " new-test-name " as it is state \"" (test:get-state testdat) 
				"\" and status \"" (test:get-status testdat) "\", use -rerun \"" (test:get-state testdat) "\" or -force to override"))
	       ;; NOTE: No longer be checking prerequisites here! Will never get here unless prereqs are
	       ;;       already met.
	       (if (not (launch-test db run-id runname test-conf keyvallst test-name test-path itemdat flags))
		   (begin
		     (print "ERROR: Failed to launch the test. Exiting as soon as possible")
		     (set! *globalexitstatus* 1) ;; 
		     (process-signal (current-process-id) signal/kill))))))
	((KILLED) 
	 (debug:print 1 "NOTE: " new-test-name " is already running or was explictly killed, use -force to launch it."))
	((LAUNCHED REMOTEHOSTSTART RUNNING)  
	 (if (> (- (current-seconds)(+ (db:test-get-event_time testdat)
				       (db:test-get-run_duration testdat)))
		600) ;; i.e. no update for more than 600 seconds
	     (begin
	       (debug:print 0 "WARNING: Test " test-name " appears to be dead. Forcing it to state INCOMPLETE and status STUCK/DEAD")
	       (test-set-status! db run-id test-name "INCOMPLETE" "STUCK/DEAD" itemdat "Test is stuck or dead" #f))
	     (debug:print 2 "NOTE: " test-name " is already running")))
	(else       (debug:print 0 "ERROR: Failed to launch test " new-test-name ". Unrecognised state " (test:get-state testdat)))))))

;;======================================================================
;; END OF NEW STUFF
;;======================================================================

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
						(db:get-value-by-header run header (vector-ref k 0))) keys) "/"))
	     (dirs-to-remove (make-hash-table)))
	 (let* ((run-id (db:get-value-by-header run header "id") )
		(tests  (db-get-tests-for-run db (db:get-value-by-header run header "id") testpatt itempatt '() '()))
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
			    (hash-table-set! dirs-to-remove fullpath #t)
			    ;; The following was the safe delete code but it was not being exectuted.
			    ;; (let* ((dirs-count (+ 1 (length keys)(length (string-split item-path "/"))))
			    ;;        (dir-to-rem (get-dir-up-n fullpath dirs-count))
			    ;;        (remainingd (string-substitute (regexp (conc "^" dir-to-rem "/")) "" fullpath))
			    ;;        (cmd (conc "cd " dir-to-rem "; rmdir -p " remainingd )))
			    ;;   (if (file-exists? fullpath)
			    ;;       (begin
			    ;;         (debug:print 1 cmd)
			    ;;         (system cmd)))
			    ;;   ))
			    ))))
		    tests)))

	   ;; look though the dirs-to-remove for candidates for removal. Do this after deleting the records
	   ;; for each test in case we get killed. That should minimize the detritus left on disk
	   ;; process the dirs from longest string length to shortest
	   (for-each 
	    (lambda (dir-to-remove)
	      (if (file-exists? dir-to-remove)
		  (let ((dir-in-db '()))
		    (sqlite3:for-each-row
		     (lambda (dir)
		       (set! dir-in-db (cons dir dir-in-db)))
		     db "SELECT rundir FROM tests WHERE rundir LIKE ?;" 
		     (conc "%" dir-to-remove "%")) ;; yes, I'm going to bail if there is anything like this dir in the db
		    (if (null? dir-in-db)
			(begin
			  (debug:print 2 "Removing directory with zero db references: " dir-to-remove)
			  (system (conc "rm -rf " dir-to-remove))
			  (hash-table-delete! dirs-to-remove dir-to-remove))
			(debug:print 2 "Skipping removal of " dir-to-remove " for now as it still has references in the database")))))
	    (sort (hash-table-keys dirs-to-remove) (lambda (a b)(> (string-length a)(string-length b)))))

	   ;; remove the run if zero tests remain
	   (let ((remtests (db-get-tests-for-run db (db:get-value-by-header run header "id") #f #f '() '())))
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

;; Since many calls to a run require pretty much the same setup 
;; this wrapper is used to reduce the replication of code
(define (general-run-call switchname action-desc proc)
  (let ((runname (args:get-arg ":runname"))
	(target  (if (args:get-arg "-target")
		     (args:get-arg "-target")
		     (args:get-arg "-reqtarg"))))
    (cond
     ((not target)
      (debug:print 0 "ERROR: Missing required parameter for " switchname ", you must specify the target with -target")
      (exit 3))
     ((not runname)
      (debug:print 0 "ERROR: Missing required parameter for " switchname ", you must specify the run name with :runname runname")
      (exit 3))
     (else
      (let ((db   #f)
	    (keys #f))
	(if (not (setup-for-run))
	    (begin 
	      (debug:print 0 "Failed to setup, exiting")
	      (exit 1)))
	(set! db   (open-db))
	(set! keys (db-get-keys db))
	;; have enough to process -target or -reqtarg here
	(if (args:get-arg "-reqtarg")
	    (let* ((runconfigf (conc  *toppath* "/runconfigs.config")) ;; DO NOT EVALUATE ALL 
		   (runconfig  (read-config runconfigf #f #f environ-patt: #f))) 
	      (if (hash-table-ref/default runconfig (args:get-arg "-reqtarg") #f)
		  (keys:target-set-args keys (args:get-arg "-reqtarg") args:arg-hash)
		  (begin
		    (debug:print 0 "ERROR: [" (args:get-arg "-reqtarg") "] not found in " runconfigf)
		    (sqlite3:finalize! db)
		    (exit 1))))
	    (if (args:get-arg "-target")
		(keys:target-set-args keys (args:get-arg "-target" args:arg-hash) args:arg-hash)))
	(if (not (car *configinfo*))
	    (begin
	      (debug:print 0 "ERROR: Attempted to " action-desc " but run area config file not found")
	      (exit 1))
	    ;; Extract out stuff needed in most or many calls
	    ;; here then call proc
	    (let* ((keynames   (map key:get-fieldname keys))
		   (keyvallst  (keys->vallist keys #t)))
	      (proc db target runname keys keynames keyvallst)))
	(sqlite3:finalize! db)
	(set! *didsomething* #t))))))

;;======================================================================
;; Rollup runs
;;======================================================================

;; Update the test_meta table for this test
(define (runs:update-test_meta db test-name test-conf)
  (let ((currrecord (db:testmeta-get-record db test-name)))
    (if (not currrecord)
	(begin
	  (set! currrecord (make-vector 10 #f))
	  (db:testmeta-add-record db test-name)))
    (for-each 
     (lambda (key)
       (let* ((idx (cadr key))
	      (fld (car  key))
	      (val (config-lookup test-conf "test_meta" fld)))
	 ;; (debug:print 5 "idx: " idx " fld: " fld " val: " val)
	 (if (and val (not (equal? (vector-ref currrecord idx) val)))
	     (begin
	       (print "Updating " test-name " " fld " to " val)
	       (db:testmeta-update-field db test-name fld val)))))
     '(("author" 2)("owner" 3)("description" 4)("reviewed" 5)("tags" 9)))))

;; Update test_meta for all tests
(define (runs:update-all-test_meta db)
  (let ((test-names (get-all-legal-tests)))
    (for-each 
     (lambda (test-name)
       (let* ((test-path    (conc *toppath* "/tests/" test-name))
	      (test-configf (conc test-path "/testconfig"))
	      (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
	      ;; read configs with tricks turned off (i.e. no system)
	      (test-conf    (if testexists (read-config test-configf #f #f)(make-hash-table))))
	 (runs:update-test_meta db test-name test-conf)))
     test-names)))

;; This could probably be refactored into one complex query ...
(define (runs:rollup-run db keys keyvallst runname user) ;; was target, now keyvallst
  (debug:print 4 "runs:rollup-run, keys: " keys " keyvallst: " keyvallst " :runname " runname " user: " user)
  (let* (; (keyvalllst      (keys:target->keyval keys target))
	 (new-run-id      (runs:register-run db keys keyvallst runname "new" "n/a" user))
	 (prev-tests      (test:get-matching-previous-test-run-records db new-run-id "%" "%"))
	 (curr-tests      (db-get-tests-for-run db new-run-id "%" "%" '() '()))
	 (curr-tests-hash (make-hash-table)))
    (db:update-run-event_time db new-run-id)
    ;; index the already saved tests by testname and itemdat in curr-tests-hash
    (for-each
     (lambda (testdat)
       (let* ((testname  (db:test-get-testname testdat))
	      (item-path (db:test-get-item-path testdat))
	      (full-name (conc testname "/" item-path)))
	 (hash-table-set! curr-tests-hash full-name testdat)))
     curr-tests)
    ;; NOPE: Non-optimal approach. Try this instead.
    ;;   1. tests are received in a list, most recent first
    ;;   2. replace the rollup test with the new *always*
    (for-each 
     (lambda (testdat)
       (let* ((testname  (db:test-get-testname testdat))
	      (item-path (db:test-get-item-path testdat))
	      (full-name (conc testname "/" item-path))
	      (prev-test-dat (hash-table-ref/default curr-tests-hash full-name #f))
	      (test-steps      (db:get-steps-for-test db (db:test-get-id testdat)))
	      (new-test-record #f))
	 ;; replace these with insert ... select
	 (apply sqlite3:execute 
		db 
		(conc "INSERT OR REPLACE INTO tests (run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment) "
		      "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?);")
		new-run-id (cddr (vector->list testdat)))
	 (set! new-testdat (car (db-get-tests-for-run db new-run-id testname item-path '() '())))
	 (hash-table-set! curr-tests-hash full-name new-testdat) ;; this could be confusing, which record should go into the lookup table?
	 ;; Now duplicate the test steps
	 (debug:print 4 "Copying records in test_steps from test_id=" (db:test-get-id testdat) " to " (db:test-get-id new-testdat))
	 (sqlite3:execute 
	  db 
	  (conc "INSERT OR REPLACE INTO test_steps (test_id,stepname,state,status,event_time,comment) "
		"SELECT " (db:test-get-id new-testdat) ",stepname,state,status,event_time,comment FROM test_steps WHERE test_id=?;")
	  (db:test-get-id testdat))
	 ;; Now duplicate the test data
	 (debug:print 4 "Copying records in test_data from test_id=" (db:test-get-id testdat) " to " (db:test-get-id new-testdat))
	 (sqlite3:execute 
	  db 
	  (conc "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment) "
		"SELECT " (db:test-get-id new-testdat) ",category,variable,value,expected,tol,units,comment FROM test_data WHERE test_id=?;")
	  (db:test-get-id testdat))
	 ))
     prev-tests)))
	 
     
