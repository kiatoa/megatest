
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking (srfi 18) posix-extras directory-utils)
(import (prefix sqlite3 sqlite3:))

(declare (unit runs))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
(declare (uses tests))
(declare (uses server))

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

(define (runs:test-get-full-path test)
  (let* ((testname (db:test-get-testname   test))
	 (itempath (db:test-get-item-path test)))
    (conc testname (if (equal? itempath "") "" (conc "(" itempath ")")))))

;; Awful. Please FIXME
(define *env-vars-by-run-id* (make-hash-table))
(define *current-run-name*   #f)

(define (set-megatest-env-vars db run-id)
  (let ((keys (db:get-keys db))
	(vals (hash-table-ref/default *env-vars-by-run-id* run-id #f)))
    ;; get the info from the db and put it in the cache
    (if (not vals)
	(let ((ht (make-hash-table)))
	  (hash-table-set! *env-vars-by-run-id* run-id ht)
	  (set! vals ht)
	  (for-each
	   (lambda (key)
	     (sqlite3:for-each-row
	      (lambda (val)
		(hash-table-set! vals key val))
	      db 
	      (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")
	      run-id))
	   keys)))
    ;; from the cached data set the vars
    (hash-table-for-each
     vals
     (lambda (key val)
       (debug:print 2 "setenv " (key:get-fieldname key) " " val)
       (setenv (key:get-fieldname key) val)))
    (alist->env-vars (hash-table-ref/default *configdat* "env-override" '()))
    ;; Lets use this as an opportunity to put MT_RUNNAME in the environment
    (if (not *current-run-name*)
	(sqlite3:for-each-row
	 (lambda (runname)
	   (set! *current-run-name* runname))

	 db
	 "SELECT runname FROM runs WHERE id=?;"
	 run-id))
    (setenv "MT_RUNNAME" *current-run-name*)
    (setenv "MT_RUN_AREA_HOME" *toppath*)
    ))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (debug:print 2 "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

(define *last-num-running-tests* 0)
(define (runs:can-run-more-tests db test-record)
  (let* ((tconfig                 (tests:testqueue-get-testconfig test-record))
	 (jobgroup                (config-lookup tconfig "requirements" "jobgroup"))
	 (num-running             (db:get-count-tests-running db))
	 (num-running-in-jobgroup (db:get-count-tests-running-in-jobgroup db jobgroup))
	 (max-concurrent-jobs     (config-lookup *configdat* "setup"     "max_concurrent_jobs"))
	 (job-group-limit         (config-lookup *configdat* "jobgroups" jobgroup)))
    (if (not (eq? *last-num-running-tests* num-running))
	(begin
	  (debug:print 2 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	  (set! *last-num-running-tests* num-running)))
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
(define (runs:run-tests db target runname test-patts user flags)
  (let* ((keys        (db:get-keys db))
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

    ;; from here on out the db will be opened and closed on every call runs:run-tests-queue
    (sqlite3:finalize! db) 
    ;; now add non-directly referenced dependencies (i.e. waiton)
    (if (not (null? test-names))
	(let loop ((hed (car test-names))
		   (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	  (debug:print 4 "INFO: hed=" hed " at top of loop")
	  (let* ((config  (tests:get-testconfig hed 'return-procs))
		 (waitons (if config (string-split (let ((w (config-lookup config "requirements" "waiton")))
						     (if w w "")))
			      (begin
				(debug:print 0 "ERROR: non-existent required test \"" hed "\"")
                                (sqlite3:finalize! db)
				(exit 1)))))
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
		  (loop (car remtests)(cdr remtests)))))))

    (if (not (null? required-tests))
	(debug:print 1 "INFO: Adding " required-tests " to the run queue"))
    ;; NOTE: these are all parent tests, items are not expanded yet.
    (debug:print 4 "INFO: test-records=" (hash-table->alist test-records))
    (runs:run-tests-queue run-id runname test-records keyvallst flags)
    (debug:print 1 "INFO: running queue one more time to catch any changed test states")
    (runs:run-tests-queue run-id runname test-records keyvallst flags)
    (debug:print 4 "INFO: All done by here")))

;; test-records is a hash table testname:item_path => vector < testname testconfig waitons priority items-info ... >
(define (runs:run-tests-queue run-id runname test-records keyvallst flags)
    ;; At this point the list of parent tests is expanded 
    ;; NB// Should expand items here and then insert into the run queue.
  (debug:print 5 "test-records: " test-records ", keyvallst: " keyvallst " flags: " (hash-table->alist flags))
  (let ((sorted-test-names (tests:sort-by-priority-and-waiton test-records))
	(item-patts        (hash-table-ref/default flags "-itempatt" #f))
	(test-registery    (make-hash-table))
	(num-retries        0)
	(max-retries       (config-lookup *configdat* "setup" "maxretries")))
    (set! max-retries (if (and max-retries (string->number max-retries))(string->number max-retries) 100))
    (if (not (null? sorted-test-names))
	(let loop ((hed         (car sorted-test-names))
		   (tal         (cdr sorted-test-names))
		   (reruns      '()))
	  (if (not (null? reruns))(debug:print 4 "INFO: reruns=" reruns))
	  (let* ((test-record (hash-table-ref test-records hed))
		 (test-name   (tests:testqueue-get-testname test-record))
		 (tconfig     (tests:testqueue-get-testconfig test-record))
		 (testmode    (let ((m (config-lookup tconfig "requirements" "mode")))
				(if m (string->symbol m) 'normal)))
		 (waitons     (tests:testqueue-get-waitons    test-record))
		 (priority    (tests:testqueue-get-priority   test-record))
		 (itemdat     (tests:testqueue-get-itemdat    test-record)) ;; itemdat can be a string, list or #f
		 (items       (tests:testqueue-get-items      test-record))
		 (item-path   (item-list->path itemdat))
		 (newtal      (append tal (list hed)))
		 (calc-fails  (lambda (prereqs-not-met)
				(filter (lambda (test)
					  (and (vector? test) ;; not (string? test))
					       (equal? (db:test-get-state test) "COMPLETED")
					       (not (member (db:test-get-status test)
							    '("PASS" "WARN" "CHECK" "WAIVED")))))
					prereqs-not-met)))
		 (calc-not-completed (lambda (prereqs-not-met)
				       (filter
					(lambda (t)
					  (or (not (vector? t))
					      (not (equal? "COMPLETED" (db:test-get-state t)))))
					prereqs-not-met)))
		 (pretty-string (lambda (lst)
				  (map (lambda (t)
					 (if (not (vector? t))
					     (conc t)
					     (conc (db:test-get-testname t) ":" (db:test-get-state t) "/" (db:test-get-status t))))
				       lst))))
	    
	    (debug:print 6
			 "test-name: " test-name
			 "\n  hed:         " hed
			 "\n  itemdat:     " itemdat
			 "\n  items:       " items
			 "\n  item-path:   " item-path
			 "\n  waitons:     " waitons
			 "\n  num-retries: " num-retries)

	    ;; check for hed in waitons => this would be circular, remove it and issue an
	    ;; error
	    (if (member test-name waitons)
		(begin
		  (debug:print 0 "ERROR: test " test-name " has listed itself as a waiton, please correct this!")
		  (set! waiton (filter (lambda (x)(not (equal? x hed))) waitons))))

	    (cond
	     ((not items) ;; when false the test is ok to be handed off to launch (but not before)
	      (let* ((have-resources  (open-run-close runs:can-run-more-tests #f test-record)) ;; look at the test jobgroup and tot jobs running
		     (prereqs-not-met (open-run-close db:get-prereqs-not-met #f run-id waitons item-path mode: testmode))
		     (fails           (calc-fails prereqs-not-met))
		     (non-completed   (calc-not-completed prereqs-not-met)))
		(debug:print 8 "INFO: have-resources: " have-resources " prereqs-not-met: " 
			     (string-intersperse 
			      (map (lambda (t)
				     (if (vector? t)
					 (conc (db:test-get-state t) "/" (db:test-get-status t))
					 (conc " WARNING: t is not a vector=" t )))
				   prereqs-not-met) ", ") " fails: " fails)
		(debug:print 4 "INFO: hed=" hed)
		;; Don't know at this time if the test have been launched at some time in the past
		;; i.e. is this a re-launch?
		(cond
		 ((not (patt-list-match item-path item-patts))
		  ;; else the run is stuck, temporarily or permanently
		  ;; but should check if it is due to lack of resources vs. prerequisites
		  (debug:print 1 "INFO: Skipping " (tests:testqueue-get-testname test-record) " " item-path " as it doesn't match " item-patts)
		  (if (not (null? tal))
		      (loop (car tal)(cdr tal) reruns)))
		 ((not (hash-table-ref/default test-registery (conc test-name "/" item-path) #f))
		  (open-run-close db:tests-register-test #f run-id test-name item-path)
		  (hash-table-set! test-registery (conc test-name "/" item-path) #t)
		  (loop (car newtal)(cdr newtal) reruns))
		 ((not have-resources) ;; simply try again after waiting a second
		  (thread-sleep! (+ 1 *global-delta*))
		  (debug:print 1 "INFO: no resources to run new tests, waiting ...")
		  ;; could have done hed tal here but doing car/cdr of newtal to rotate tests
		  (loop (car newtal)(cdr newtal) reruns))
		 ((and have-resources
		       (or (null? prereqs-not-met)
			   (and (eq? testmode 'toplevel)
				(null? non-completed))))
		  ;; no loop here, just drop though and use the loop at the bottom 
		  (run:test run-id runname keyvallst test-record flags #f))
		 (else ;; must be we have unmet prerequisites
		    (debug:print 4 "FAILS: " fails)
		    ;; If one or more of the prereqs-not-met are FAIL then we can issue
		    ;; a message and drop hed from the items to be processed.
		    (if (null? fails)
			(begin
			  ;; couldn't run, take a breather
			  (debug:print 4 "INFO: Shouldn't really get here, race condition? Unable to launch more tests at this moment, killing time ...")
			  (thread-sleep! (+ 1 *global-delta*)) ;; long sleep here - no resources, may as well be patient
			  ;; we made new tal by sticking hed at the back of the list
			  (loop (car newtal)(cdr newtal) reruns))
			;; the waiton is FAIL so no point in trying to run hed ever again
			(if (not (null? tal))
			    (if (vector? hed)
				(begin (debug:print 1 "WARN: Dropping test " (db:test-get-testname hed) "/" (db:test-get-item-path hed)
						    " from the launch list as it has prerequistes that are FAIL")
				       (loop (car tal)(cdr tal) (cons hed reruns)))
				(begin
				  (debug:print 1 "WARN: Test not processed correctly. Could be a race condition in your test implementation? " hed) ;;  " as it has prerequistes that are FAIL. (NOTE: hed is not a vector)")
				  (loop hed tal reruns)))))))))
	     
	     ;; case where an items came in as a list been processed
	     ((and (list? items)     ;; thus we know our items are already calculated
		   (not   itemdat)) ;; and not yet expanded into the list of things to be done
	      (if (and (>= *verbosity* 1)
		       (> (length items) 0)
		       (> (length (car items)) 0))
		  (pp items))
	      ;; (if (>= *verbosity* 5)
	      ;;     (begin
	      ;;       (print "items: ")     (pp (item-assoc->item-list items))
	      ;;       (print "itemstable: ")(pp (item-table->item-list itemstable))))
	      (for-each
	       (lambda (my-itemdat)
		 (let* ((new-test-record (let ((newrec (make-tests:testqueue)))
					   (vector-copy! test-record newrec)
					   newrec))
			(my-item-path (item-list->path my-itemdat)))
		   (if (patt-list-match my-item-path item-patts)           ;; yes, we want to process this item, NOTE: Should not need this check here!
		       (let ((newtestname (conc hed "/" my-item-path)))    ;; test names are unique on testname/item-path
			 (tests:testqueue-set-items!     new-test-record #f)
			 (tests:testqueue-set-itemdat!   new-test-record my-itemdat)
			 (tests:testqueue-set-item_path! new-test-record my-item-path)
			 (hash-table-set! test-records newtestname new-test-record)
			 (set! tal (cons newtestname tal)))))) ;; since these are itemized create new test names testname/itempath
	       items)
	      (if (not (null? tal))
		  (loop (car tal)(cdr tal) reruns)))

	     ;; if items is a proc then need to run items:get-items-from-config, get the list and loop 
	     ;;    - but only do that if resources exist to kick off the job
	     ((or (procedure? items)(eq? items 'have-procedure))
	      (let ((can-run-more    (open-run-close runs:can-run-more-tests #f test-record)))
		(if can-run-more
		    (let* ((prereqs-not-met (open-run-close db:get-prereqs-not-met #f run-id waitons item-path mode: testmode))
			   (fails           (calc-fails prereqs-not-met))
			   (non-completed   (calc-not-completed prereqs-not-met)))
		      (debug:print 8 "INFO: can-run-more: " can-run-more
				   "\n testname:        " hed
				   "\n prereqs-not-met: " (pretty-string prereqs-not-met)
				   "\n non-completed:   " (pretty-string non-completed) 
				   "\n fails:           " (pretty-string fails)
				   "\n testmode:        " testmode
				   "\n num-retries:     " num-retries
				   "\n (eq? testmode 'toplevel): " (eq? testmode 'toplevel)
				   "\n (null? non-completed):    " (null? non-completed)
				   "\n reruns: " reruns)
		      (cond 
		       ((or (null? prereqs-not-met) ;; all prereqs met, fire off the test
			    ;; or, if it is a 'toplevel test and all prereqs not met are COMPLETED then launch
			    (and (eq? testmode 'toplevel)
				 (null? non-completed)))
			(let ((test-name (tests:testqueue-get-testname test-record)))
			  (setenv "MT_TEST_NAME" test-name) ;; 
			  (setenv "MT_RUNNAME"   runname)
			  (open-run-close set-megatest-env-vars #f run-id) ;; these may be needed by the launching process
			  (let ((items-list (items:get-items-from-config tconfig)))
			    (if (list? items-list)
				(begin
				  (tests:testqueue-set-items! test-record items-list)
				  (loop hed tal reruns))
				(begin
				  (debug:print 0 "ERROR: The proc from reading the setup did not yield a list - please report this")
				  (exit 1))))))
		       ((null? fails)
			(debug:print 4 "INFO: fails is null, moving on in the queue but keeping " hed " for now")
			(loop (car newtal)(cdr newtal) reruns)) ;; an issue with prereqs not yet met?
		       ((and (not (null? fails))(eq? testmode 'normal))
			(debug:print 1 "INFO: test "  hed " (mode=" testmode ") has failed prerequisite(s); "
				     (string-intersperse (map (lambda (t)(conc (db:test-get-testname t) ":" (db:test-get-state t)"/"(db:test-get-status t))) fails) ", ")
				     ", removing it from to-do list")
			(if (not (null? tal))
			    (loop (car tal)(cdr tal)(cons hed reruns))))
		       (else
			(debug:print 8 "ERROR: No handler for this condition.")
			;; 	     "\n  hed:            " hed 
			;; 	     "\n fails:           " (string-intersperse (map db:test-get-testname fails) ",")
			;; 	     "\n testmode:        " testmode
			;; 	     "\n prereqs-not-met: " (pretty-string prereqs-not-met)
			;; 	     "\n items:           " items)
			(loop (car newtal)(cdr newtal) reruns))))
		    ;; if can't run more just loop with next possible test
		    (begin
		      (debug:print 4 "INFO: processing the case with a lambda for items or 'have-procedure. Moving through the queue without dropping " hed)
		      (thread-sleep! (+ 1 *global-delta*))
		      (loop (car newtal)(cdr newtal) reruns)))))
	     
	     ;; this case should not happen, added to help catch any bugs
	     ((and (list? items) itemdat)
	      (debug:print 0 "ERROR: Should not have a list of items in a test and the itemspath set - please report this")
	      (exit 1))))
	  
	  ;; we get here on "drop through" - loop for next test in queue
	  (if (null? tal)
	      (begin
		;; FIXME!!!! THIS SHOULD NOT REQUIRE AN EXIT!!!!!!!
		(debug:print 1 "INFO: All tests launched")
		(thread-sleep! 0.5)
		;; FIXME! This harsh exit should not be necessary....
		(if (not *runremote*)(exit)) ;; 
		#f) ;; return a #f as a hint that we are done
	      ;; Here we need to check that all the tests remaining to be run are eligible to run
	      ;; and are not blocked by failed
	      (let* ((newlst (open-run-close tests:filter-non-runnable #f run-id tal test-records)) ;; i.e. not FAIL, WAIVED, INCOMPLETE, PASS, KILLED,
		     (junked (lset-difference equal? tal newlst)))
		(debug:print 4 "INFO: full drop through, if reruns is less than 100 we will force retry them: " reruns)
		(if (< num-retries max-retries)
		    (set! newlst (append reruns newlst)))
		(set! num-retries (+ num-retries 1))
		(thread-sleep! *global-delta*)
		(if (not (null? newlst))
		    ;; since reruns have been tacked on to newlst create new reruns from junked
		    (loop (car newlst)(cdr newlst)(delete-duplicates junked)))))))))

;; parent-test is there as a placeholder for when parent-tests can be run as a setup step
(define (run:test run-id runname keyvallst test-record flags parent-test)
  ;; All these vars might be referenced by the testconfig file reader
  (let* ((test-name    (tests:testqueue-get-testname   test-record))
	 (test-waitons (tests:testqueue-get-waitons    test-record))
	 (test-conf    (tests:testqueue-get-testconfig test-record))
	 (itemdat      (tests:testqueue-get-itemdat    test-record))
	 (test-path    (conc *toppath* "/tests/" test-name)) ;; could use tests:get-testconfig here ...
	 (force        (hash-table-ref/default flags "-force" #f))
	 (rerun        (hash-table-ref/default flags "-rerun" #f))
	 (keepgoing    (hash-table-ref/default flags "-keepgoing" #f))
	 (item-path     "")
	 (db           #f))
    (debug:print 4
		 "test-config: " (hash-table->alist test-conf)
		 "\n   itemdat: " itemdat
		 )
    ;; setting itemdat to a list if it is #f
    (if (not itemdat)(set! itemdat '()))
    (set! item-path (item-list->path itemdat))
    (debug:print 2 "Attempting to launch test " test-name (if (equal? item-path "/") "/" item-path))
    (setenv "MT_TEST_NAME" test-name) ;; 
    (setenv "MT_RUNNAME"   runname)
    (open-run-close set-megatest-env-vars db run-id) ;; these may be needed by the launching process
    (change-directory *toppath*)

    ;; Here is where the test_meta table is best updated
    ;; Yes, another use of a global for caching. Need a better way?
    (if (not (hash-table-ref/default *test-meta-updated* test-name #f))
        (begin
	   (hash-table-set! *test-meta-updated* test-name #t)
           (open-run-close runs:update-test_meta db test-name test-conf)))
    
    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
    (let* ((new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
	   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
	   (test-id       (open-run-close db:get-test-id db  run-id test-name item-path))
	   (testdat       (open-run-close db:get-test-info-by-id db test-id)))
      (if (not testdat)
	  (begin
	    ;; ensure that the path exists before registering the test
	    ;; NOPE: Cannot! Don't know yet which disk area will be assigned....
	    ;; (system (conc "mkdir -p " new-test-path))
	    ;;
	    ;; (open-run-close tests:register-test db run-id test-name item-path)
	    ;;
	    ;; NB// for the above line. I want the test to be registered long before this routine gets called!
	    ;;
	    (set! test-id (open-run-close db:get-test-id db run-id test-name item-path))
	    (if (not test-id)
		(begin
		  (debug:print 2 "WARN: Test not pre-created? test-name=" test-name ", item-path=" item-path ", run-id=" run-id)
		  (open-run-close db:tests-register-test db run-id test-name item-path)
		  (set! test-id (open-run-close db:get-test-id db run-id test-name item-path))))
	    (debug:print 4 "INFO: test-id=" test-id ", run-id=" run-id ", test-name=" test-name ", item-path=\"" item-path "\"")
	    (set! testdat (open-run-close db:get-test-info-by-id db test-id))))
      (set! test-id (db:test-get-id testdat))
      (change-directory test-path)
      (case (if force ;; (args:get-arg "-force")
		'NOT_STARTED
		(if testdat
		    (string->symbol (test:get-state testdat))
		    'failed-to-insert))
	((failed-to-insert)
	 (debug:print 0 "ERROR: Failed to insert the record into the db"))
	((NOT_STARTED COMPLETED)
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
	     (debug:print 2 "INFO: running test " test-name "/" item-path " suppressed as it is " (test:get-state testdat) " and " (test:get-status testdat))
	     (set! runflag #f))
	    ;; -rerun and status is one of the specifed, run it
	    ((and rerun
		  (let* ((rerunlst   (string-split rerun ","))
			 (must-rerun (member (test:get-status testdat) rerunlst)))
		    (debug:print 3 "INFO: -rerun list: " rerun ", test-status: " (test:get-status testdat)", must-rerun: " must-rerun)
		    must-rerun))
	     (debug:print 2 "INFO: Rerun forced for test " test-name "/" item-path)
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
				"\" and status \"" (test:get-status testdat) "\", use -rerun \"" (test:get-status testdat)
                                "\" or -force to override"))
	       ;; NOTE: No longer be checking prerequisites here! Will never get here unless prereqs are
	       ;;       already met.
	       (if (not (launch-test #f run-id runname test-conf keyvallst test-name test-path itemdat flags))
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
	       (open-run-close test-set-status! db test-id "INCOMPLETE" "STUCK/DEAD" "Test is stuck or dead" #f))
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
;; action:
;;    'remove-runs
;;    'set-state-status
;;
;; NB// should pass in keys?
;;
(define (runs:operate-on db action runnamepatt testpatt itempatt #!key (state #f)(status #f)(new-state-status #f))
  (let* ((keys         (db:get-keys db))
	 (rundat       (runs:get-runs-by-patt db keys runnamepatt))
	 (header       (vector-ref rundat 0))
	 (runs         (vector-ref rundat 1))
	 (states       (if state  (string-split state  ",") '()))
	 (statuses     (if status (string-split status ",") '()))
	 (state-status (if (string? new-state-status) (string-split new-state-status ",") '(#f #f))))
    (debug:print 2 "Header: " header " action: " action " new-state-status: " new-state-status)
    (for-each
     (lambda (run)
       (let ((runkey (string-intersperse (map (lambda (k)
						(db:get-value-by-header run header (vector-ref k 0))) keys) "/"))
	     (dirs-to-remove (make-hash-table)))
	 (let* ((run-id    (db:get-value-by-header run header "id"))
		(run-state (db:get-value-by-header run header "state"))
		(tests     (if (not (equal? run-state "locked"))
			       (db:get-tests-for-run db (db:get-value-by-header run header "id")
						      testpatt itempatt states statuses
						      not-in:  #f
						      sort-by: (case action
								 ((remove-runs) 'rundir)
								 (else          'event_time)))
			       '()))
		(lasttpath "/does/not/exist/I/hope"))

	   (if (not (null? tests))
	       (begin
		 (case action
		   ((remove-runs)
		    (debug:print 1 "Removing tests for run: " runkey " " (db:get-value-by-header run header "runname")))
		   ((set-state-status)
		    (debug:print 1 "Modifying state and staus for tests for run: " runkey " " (db:get-value-by-header run header "runname")))
		   (else
		    (print "INFO: action not recognised " action)))
		 (for-each
		  (lambda (test)
		    (let* ((item-path (db:test-get-item-path test))
			   (test-name (db:test-get-testname test))
			   (run-dir   (db:test-get-rundir test))
			   (test-id   (db:test-get-id test)))
		      ;;   (tdb       (db:open-test-db run-dir)))
		      (debug:print 1 "  " (db:test-get-testname test) " id: " (db:test-get-id test) " " item-path " action: " action)
		      (case action
			((remove-runs) ;; the tdb is for future possible. 
			 (db:delete-test-records db #f (db:test-get-id test))
			 (debug:print 1 "INFO: Attempting to remove dir " run-dir)
			 (if (and (> (string-length run-dir) 5)
				  (file-exists? run-dir)) ;; bad heuristic but should prevent /tmp /home etc.
			     (let* ((realpath (resolve-pathname run-dir)))
			       (debug:print 1 "INFO: Real path of is " realpath)
			       (if (file-exists? realpath)
				   (if (> (system (conc "rm -rf " realpath)) 0)
				       (debug:print 0 "ERROR: There was a problem removing " realpath " with rm -f"))
				   (debug:print 0 "WARNING: test run dir " realpath " appears to not exist"))
			       (if (file-exists? run-dir) ;; the link
				   (if (symbolic-link? run-dir)
				       (delete-file run-dir)
				       (if (directory? run-dir)
					   (if (> (directory-fold (lambda (f x)(+ 1 x)) 0 run-dir) 0)
					       (debug:print 0 "WARNING: refusing to remove " run-dir " as it is not empty")
					       (delete-directory run-dir)) ;; it should be empty by here BUG BUG, add error catch
					   (debug:print 0 "ERROR: refusing to remove " run-dir " as it is neither a symlink nor a directory")
					   ))))
			     (debug:print 0 "WARNING: directory already removed " run-dir)))
			((set-state-status)
			 (debug:print 2 "INFO: new state " (car state-status) ", new status " (cadr state-status))
			 (open-run-close db:test-set-state-status-by-id db (db:test-get-id test) (car state-status)(cadr state-status) #f)))))
		  tests)))
	   
	   ;; remove the run if zero tests remain
	   (if (eq? action 'remove-runs)
	       (let ((remtests (db:get-tests-for-run db (db:get-value-by-header run header "id") #f #f '("DELETED") '("n/a") not-in: #t)))
		 (if (null? remtests) ;; no more tests remaining
		     (let* ((dparts  (string-split lasttpath "/"))
			    (runpath (conc "/" (string-intersperse 
						(take dparts (- (length dparts) 1))
						"/"))))
		       (debug:print 1 "Removing run: " runkey " " (db:get-value-by-header run header "runname") " and related record")
		       (db:delete-run db run-id)
		       ;; This is a pretty good place to purge old DELETED tests
		       (db:delete-tests-for-run db run-id)
		       (db:delete-old-deleted-test-records db)
		       (db:set-var db "DELETED_TESTS" (current-seconds))
		       ;; need to figure out the path to the run dir and remove it if empty
		       ;;    (if (null? (glob (conc runpath "/*")))
		       ;;        (begin
		       ;; 	 (debug:print 1 "Removing run dir " runpath)
		       ;; 	 (system (conc "rmdir -p " runpath))))
		       )))))
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
		     (args:get-arg "-reqtarg")))
	(th1     #f))
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
	(if (args:get-arg "-server")
	    (server:start db (args:get-arg "-server"))
	    (if (not (or (args:get-arg "-runall")
			  (args:get-arg "-runtests")))
		(server:client-setup db)))
	(set! keys (db:get-keys db))
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
	(if th1 (thread-join! th1))
	(sqlite3:finalize! db)
	(set! *didsomething* #t))))))

;;======================================================================
;; Lock/unlock runs
;;======================================================================

(define (runs:handle-locking db target keys runname lock unlock user)
  (let* ((rundat   (runs:get-runs-by-patt db keys runname))
	 (header   (vector-ref rundat 0))
	 (runs     (vector-ref rundat 1)))
    (for-each (lambda (run)
		(let ((run-id (db:get-value-by-header run header "id")))
		  (if (or lock
			  (and unlock
			       (begin
				 (print "Do you really wish to unlock run " run-id "?\n   y/n: ")
				 (equal? "y" (read-line)))))
		      (db:lock/unlock-run db run-id lock unlock user)
		      (debug:print 0 "INFO: Skipping lock/unlock on " run-id))))
	      runs)))
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
	 (curr-tests      (db:get-tests-for-run db new-run-id "%" "%" '() '()))
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
	 (set! new-testdat (car (db:get-tests-for-run db new-run-id testname item-path '() '())))
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
	 
     
