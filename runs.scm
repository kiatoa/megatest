
;; Copyright 2006-2012, Matthew Welland.
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
(define (runs:get-runs-by-patt db keys runnamepatt targpatt) ;; test-name)
  (let* ((tmp      (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time")))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (res     '())
	 (key-patt "")
	 (runwildtype (if (substring-index "%" runnamepatt) "like" "glob"))
	 (qry-str  #f)
	 (keyvals  (keys:target->keyval keys targpatt)))
    (for-each (lambda (keyval)
		(let* ((key    (car keyval))
		       (patt   (cadr keyval))
		       (fulkey (conc ":" key))
		       (wildtype (if (substring-index "%" patt) "like" "glob")))
		  (if patt
		      (set! key-patt (conc key-patt " AND " key " " wildtype " '" patt "'"))
		      (begin
			(debug:print 0 "ERROR: searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keyvals)
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE runname " runwildtype " ? " key-patt ";"))
    (debug:print-info 4 "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
    (sqlite3:for-each-row 
     (lambda (a . r)
       (set! res (cons (list->vector (cons a r)) res)))
     db 
     qry-str
     runnamepatt)
    (vector header res)))

(define (runs:test-get-full-path test)
  (let* ((testname (db:test-get-testname   test))
	 (itempath (db:test-get-item-path test)))
    (conc testname (if (equal? itempath "") "" (conc "(" itempath ")")))))

;; This is the *new* methodology. One record to inform them and in the chaos, organise them.
;;
(define (runs:create-run-record)
  (let* ((mconfig      (if *configdat*
		           *configdat*
		           (if (setup-for-run)
		               *configdat*
		               (begin
		                 (debug:print 0 "ERROR: Called setup in a non-megatest area, exiting")
		                 (exit 1)))))
	  (runrec      (runs:runrec-make-record))
	  (target      (or (args:get-arg "-reqtarg")
		           (args:get-arg "-target")))
	  (runname     (or (args:get-arg ":runname")
		           (args:get-arg "-runname")))
	  (testpatt    (or (args:get-arg "-testpatt")
		           (args:get-arg "-runtests")))
	  (keys        (keys:config-get-fields mconfig))
	  (keyvals     (keys:target->keyval keys target))
	  (toppath     *toppath*)
	  (envdat      keyvals) ;; initial values start with keyvals
	  (runconfig   #f)
	  (serverdat   (if (args:get-arg "-server")
			   *runremote*
			   #f)) ;; to be used later
	  (transport   (or (args:get-arg "-transport") 'http))
	  (db          (if (and mconfig
				(or (args:get-arg "-server")
				    (eq? transport 'fs)))
			   (open-db)
			   #f))
	  (run-id      #f))
    ;; Set all the environment vars we know so far, start with keys
    (for-each (lambda (keyval)
		(setenv (car keyval)(cadr keyval)))
	      keyvals)
    ;; Set up various and sundry known vars here
    (setenv "MT_RUN_AREA_HOME" toppath)
    (setenv "MT_RUNNAME" runname)
    (setenv "MT_TARGET"  target)
    (set! envdat (append 
		  envdat
		  (list (list "MT_RUN_AREA_HOME" toppath)
			(list "MT_RUNNAME"       runname)
			(list "MT_TARGET"        target))))
    ;; Now can read the runconfigs file
    ;; 
    (set! runconfig (read-config (conc  *toppath* "/runconfigs.config") #f #t sections: (list "default" target)))
    (if (not (hash-table-ref/default runconfig (args:get-arg "-reqtarg") #f))
	(begin
	  (debug:print 0 "ERROR: [" (args:get-arg "-reqtarg") "] not found in " runconfigf)
	  (if db (sqlite3:finalize! db))
	  (exit 1)))
    ;; Now have runconfigs data loaded, set environment vars
    (for-each (lambda (section)
		(for-each (lambda (varval)
			    (set! envdat (append envdat (list varval)))
			    (setenv (car varval)(cadr varval)))
			  (configf:get-section runconfig section)))
	      (list "default" target))
    (vector target runname testpatt keys keyvals envdat mconfig runconfig serverdat transport db toppath run-id)))

	 
(define (set-megatest-env-vars run-id #!key (inkeys #f)(inrunname #f)(inkeyvals #f))
  (let* ((target      (or (args:get-arg "-reqtarg")
		           (args:get-arg "-target")))
	 (keys    (if inkeys    inkeys    (cdb:remote-run db:get-keys #f)))
	 (keyvals (if inkeyvals inkeyvals (keys:target->keyval keys target)))
	 (vals (hash-table-ref/default *env-vars-by-run-id* run-id #f)))
    ;; get the info from the db and put it in the cache
    (if (not vals)
	(let ((ht (make-hash-table)))
	  (hash-table-set! *env-vars-by-run-id* run-id ht)
	  (set! vals ht)
	  (for-each
	   (lambda (key)
	     (hash-table-set! vals (car key) (cadr key))) ;; (cdb:remote-run db:get-run-key-val #f run-id (car key))))
	   keyvals)))
    ;; from the cached data set the vars
    (hash-table-for-each
     vals
     (lambda (key val)
       (debug:print 2 "setenv " key " " val)
       (setenv key val)))
    (alist->env-vars (hash-table-ref/default *configdat* "env-override" '()))
    ;; Lets use this as an opportunity to put MT_RUNNAME in the environment
    (setenv "MT_RUNNAME" (if inrunname inrunname (cdb:remote-run db:get-run-name-from-id #f run-id)))
    (setenv "MT_RUN_AREA_HOME" *toppath*)))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (debug:print 2 "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

(define *last-num-running-tests* 0)

;; Every time can-run-more-tests is called increment the delay
;; if the cou
(define *runs:can-run-more-tests-count* 0)
(define (runs:shrink-can-run-more-tests-count)
  (set! *runs:can-run-more-tests-count* 0)) ;; (/ *runs:can-run-more-tests-count* 2)))

(define (runs:can-run-more-tests test-record max-concurrent-jobs)
  (thread-sleep! (cond
		  ((> *runs:can-run-more-tests-count* 20) 2);; obviously haven't had any work to do for a while
		  (else 0)))
  (let* ((tconfig                 (tests:testqueue-get-testconfig test-record))
	 (jobgroup                (config-lookup tconfig "requirements" "jobgroup"))
	 (num-running             (cdb:remote-run db:get-count-tests-running #f))
	 (num-running-in-jobgroup (cdb:remote-run db:get-count-tests-running-in-jobgroup #f jobgroup))
	 (job-group-limit         (config-lookup *configdat* "jobgroups" jobgroup)))
    (if (> (+ num-running num-running-in-jobgroup) 0)
	(set! *runs:can-run-more-tests-count* (+ *runs:can-run-more-tests-count* 1)))
    (if (not (eq? *last-num-running-tests* num-running))
	(begin
	  (debug:print 2 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	  (set! *last-num-running-tests* num-running)))
    (if (not (eq? 0 *globalexitstatus*))
	(list #f num-running num-running-in-jobgroup max-concurrent-jobs job-group-limit)
	(let ((can-not-run-more (cond
				 ;; if max-concurrent-jobs is set and the number running is greater 
				 ;; than it than cannot run more jobs
				 ((and max-concurrent-jobs (>= num-running max-concurrent-jobs))
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
	  (list (not can-not-run-more) num-running num-running-in-jobgroup max-concurrent-jobs job-group-limit)))))

;;======================================================================
;; New methodology. These routines will replace the above in time. For
;; now the code is duplicated. This stuff is initially used in the monitor
;; based code.
;;======================================================================


;; This is a duplicate of run-tests (which has been deprecated). Use this one instead of run tests.
;; keyvals.
;;
;;  test-names: Comma separated patterns same as test-patts but used in selection 
;;              of tests to run. The item portions are not respected.
;;              FIXME: error out if /patt specified
;;            
(define (runs:run-tests target runname test-patts user flags) ;; test-names
  (common:clear-caches) ;; clear all caches
  (let* ((db          #f)
	 (keys        (keys:config-get-fields *configdat*))
	 (keyvals     (keys:target->keyval keys target))
	 (run-id      (cdb:remote-run db:register-run #f keys keyvals runname "new" "n/a" user))  ;;  test-name)))
	 (deferred    '()) ;; delay running these since they have a waiton clause
	 ;; keepgoing is the defacto modality now, will add hit-n-run a bit later
	 ;; (keepgoing   (hash-table-ref/default flags "-keepgoing" #f))
	 (runconfigf   (conc  *toppath* "/runconfigs.config"))
	 (required-tests '())
	 (test-records (make-hash-table))
     (all-test-names (tests:get-valid-tests *toppath* "%"))) ;; we need a list of all valid tests to check waiton names)
	 (all-test-names (tests:get-valid-tests *toppath* "%"))) ;; we need a list of all valid tests to check waiton names

    (set-megatest-env-vars run-id inkeys: keys) ;; these may be needed by the launching process

    (if (file-exists? runconfigf)
	(setup-env-defaults runconfigf run-id *already-seen-runconfig-info* keys keyvals "pre-launch-env-vars")
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))
    
    ;; look up all tests matching the comma separated list of globs in
    ;; test-patts (using % as wildcard)

    (set! test-names (tests:get-valid-tests *toppath* test-patts))
    (set! test-names (delete-duplicates test-names))

    (debug:print-info 0 "test names " test-names)

    ;; on the first pass or call to run-tests set FAILS to NOT_STARTED if
    ;; -keepgoing is specified
    (if (eq? *passnum* 0)
	(begin
	  ;; have to delete test records where NOT_STARTED since they can cause -keepgoing to 
	  ;; get stuck due to becoming inaccessible from a failed test. I.e. if test B depends 
	  ;; on test A but test B reached the point on being registered as NOT_STARTED and test
	  ;; A failed for some reason then on re-run using -keepgoing the run can never complete.
	  (cdb:delete-tests-in-state *runremote* run-id "NOT_STARTED")
	  (cdb:remote-run db:set-tests-state-status #f run-id test-names #f "FAIL" "NOT_STARTED" "FAIL")))

    ;; from here on out the db will be opened and closed on every call runs:run-tests-queue
    ;; (sqlite3:finalize! db) 
    ;; now add non-directly referenced dependencies (i.e. waiton)
    (if (not (null? test-names))
	(let loop ((hed (car test-names))
		   (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	  (let* ((config  (tests:get-testconfig hed 'return-procs))
		 (waitons (let ((instr (if config 
					   (config-lookup config "requirements" "waiton")
					   (begin ;; No config means this is a non-existant test
					     (debug:print 0 "ERROR: non-existent required test \"" hed "\"")
					     (if db (sqlite3:finalize! db))
					     (exit 1)))))
			    (debug:print-info 8 "waitons string is " instr)
			    (let ((newwaitons
				   (string-split (cond
						  ((procedure? instr)
						   (let ((res (instr)))
						     (debug:print-info 8 "waiton procedure results in string " res " for test " hed)
						     res))
						  ((string? instr)     instr)
						  (else 
						   ;; NOTE: This is actually the case of *no* waitons! ;; (debug:print 0 "ERROR: something went wrong in processing waitons for test " hed)
						   "")))))
			      (filter (lambda (x)
					(if (member x all-test-names)
					    #t
					    (begin
					      (debug:print 0 "ERROR: test " hed " has unrecognised waiton testname " x)
					      #f)))
				      newwaitons)))))
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
		  (loop (car remtests)(cdr remtests)))))))

    (if (not (null? required-tests))
	(debug:print-info 1 "Adding " required-tests " to the run queue"))
    ;; NOTE: these are all parent tests, items are not expanded yet.
    (debug:print-info 4 "test-records=" (hash-table->alist test-records))
    (let ((reglen (any->number  (configf:lookup *configdat* "setup" "runqueue"))))
      (if reglen
	  (runs:run-tests-queue-new     run-id runname test-records keyvallst flags test-patts required-tests reglen)
	  (runs:run-tests-queue-classic run-id runname test-records keyvallst flags test-patts required-tests)))
    (debug:print-info 4 "All done by here")))

(define (runs:calc-fails prereqs-not-met)
  (filter (lambda (test)
	    (and (vector? test) ;; not (string? test))
		 (equal? (db:test-get-state test) "COMPLETED")
		 (not (member (db:test-get-status test)
			      '("PASS" "WARN" "CHECK" "WAIVED" "SKIP")))))
	  prereqs-not-met))

(define (runs:calc-not-completed prereqs-not-met)
  (filter
   (lambda (t)
     (or (not (vector? t))
	 (not (equal? "COMPLETED" (db:test-get-state t)))))
   prereqs-not-met))

(define (runs:pretty-string lst)
  (map (lambda (t)
	 (if (not (vector? t))
	     (conc t)
	     (conc (db:test-get-testname t) ":" (db:test-get-state t) "/" (db:test-get-status t))))
       lst))

(define (runs:make-full-test-name testname itempath)
  (if (equal? itempath "") testname (conc testname "/" itempath)))

(define (runs:queue-next-hed tal reg n regful)
  (if regful
      (if (null? reg) ;; doesn't make sense, this is probably NOT the problem of the car
	  (car tal)
	  (car reg))
      (car tal)))

(define (runs:queue-next-tal tal reg n regful)
  (if regful
      tal
      (let ((newtal (cdr tal)))
	(if (null? newtal)
	    reg
	    newtal
	    ))))

(define (runs:queue-next-reg tal reg n regful)
  (if regful
      (cdr reg)
      (if (eq? (length tal) 1)
	  '()
	  reg)))

(include "run-tests-queue-classic.scm")
(include "run-tests-queue-new.scm")

;; parent-test is there as a placeholder for when parent-tests can be run as a setup step
(define (run:test run-id run-info key-vals runname test-record flags parent-test)
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
    (set-megatest-env-vars run-id inrunname: runname) ;; these may be needed by the launching process
    (change-directory *toppath*)

    ;; Here is where the test_meta table is best updated
    ;; Yes, another use of a global for caching. Need a better way?
    (if (not (hash-table-ref/default *test-meta-updated* test-name #f))
        (begin
	   (hash-table-set! *test-meta-updated* test-name #t)
           (runs:update-test_meta test-name test-conf)))
    
    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
    (let* ((new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
	   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
	   (test-id       (cdb:remote-run db:get-test-id #f  run-id test-name item-path))
	   (testdat       (cdb:get-test-info-by-id *runremote* test-id)))
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
		  (cdb:tests-register-test *runremote* run-id test-name item-path)
		  (set! test-id (open-run-close db:get-test-id db run-id test-name item-path))))
	    (debug:print-info 4 "test-id=" test-id ", run-id=" run-id ", test-name=" test-name ", item-path=\"" item-path "\"")
	    (set! testdat (cdb:get-test-info-by-id *runremote* test-id))))
      (if (not testdat) ;; should NOT happen
	  (debug:print 0 "ERROR: failed to get test record for test-id " test-id))
      (set! test-id (db:test-get-id testdat))
      (change-directory test-path)
      (case (if force ;; (args:get-arg "-force")
		'NOT_STARTED
		(if testdat
		    (string->symbol (test:get-state testdat))
		    'failed-to-insert))
	((failed-to-insert)
	 (debug:print 0 "ERROR: Failed to insert the record into the db"))
	((NOT_STARTED COMPLETED DELETED)
	 (let ((runflag #f))
	   (cond
	    ;; -force, run no matter what
	    (force (set! runflag #t))
	    ;; NOT_STARTED, run no matter what
	    ((member (test:get-state testdat) '("DELETED" "NOT_STARTED"))(set! runflag #t))
	    ;; not -rerun and PASS, WARN or CHECK, do no run
	    ((and (or (not rerun)
		      keepgoing)
		  ;; Require to force re-run for COMPLETED or *anything* + PASS,WARN or CHECK
		  (or (member (test:get-status testdat) '("PASS" "WARN" "CHECK" "SKIP"))
		      (member (test:get-state  testdat) '("COMPLETED")))) 
	     (debug:print-info 2 "running test " test-name "/" item-path " suppressed as it is " (test:get-state testdat) " and " (test:get-status testdat))
	     (set! runflag #f))
	    ;; -rerun and status is one of the specifed, run it
	    ((and rerun
		  (let* ((rerunlst   (string-split rerun ","))
			 (must-rerun (member (test:get-status testdat) rerunlst)))
		    (debug:print-info 3 "-rerun list: " rerun ", test-status: " (test:get-status testdat)", must-rerun: " must-rerun)
		    must-rerun))
	     (debug:print-info 2 "Rerun forced for test " test-name "/" item-path)
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
	       ;; This would be a great place to do the process-fork
	       (if (not (launch-test test-id run-id run-info key-vals runname test-conf test-name test-path itemdat flags))
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
	       (tests:test-set-status! test-id "INCOMPLETE" "STUCK/DEAD" "Test is stuck or dead" #f))
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
(define (runs:operate-on action target runnamepatt testpatt #!key (state #f)(status #f)(new-state-status #f))
  (common:clear-caches) ;; clear all caches
  (let* ((db           #f)
	 (keys         (open-run-close db:get-keys db))
	 (rundat       (open-run-close runs:get-runs-by-patt db keys runnamepatt target))
	 (header       (vector-ref rundat 0))
	 (runs         (vector-ref rundat 1))
	 (states       (if state  (string-split state  ",") '()))
	 (statuses     (if status (string-split status ",") '()))
	 (state-status (if (string? new-state-status) (string-split new-state-status ",") '(#f #f))))
    (debug:print-info 4 "runs:operate-on => Header: " header " action: " action " new-state-status: " new-state-status)
    (if (> 2 (length state-status))
	(begin
	  (debug:print 0 "ERROR: the parameter to -set-state-status is a comma delimited string. E.g. COMPLETED,FAIL")
	  (exit)))
    (for-each
     (lambda (run)
       (let ((runkey (string-intersperse (map (lambda (k)
						(db:get-value-by-header run header k)) keys) "/"))
	     (dirs-to-remove (make-hash-table)))
	 (let* ((run-id    (db:get-value-by-header run header "id"))
		(run-state (db:get-value-by-header run header "state"))
		(tests     (if (not (equal? run-state "locked"))
			       (open-run-close db:get-tests-for-run db run-id
						      testpatt states statuses
						      not-in:  #f
						      sort-by: (case action
								 ((remove-runs) 'rundir)
								 (else          'event_time)))
			       '()))
		(lasttpath "/does/not/exist/I/hope"))
	   (debug:print-info 4 "runs:operate-on run=" run ", header=" header)
	   (if (not (null? tests))
	       (begin
		 (case action
		   ((remove-runs)
		    (debug:print 1 "Removing tests for run: " runkey " " (db:get-value-by-header run header "runname")))
		   ((set-state-status)
		    (debug:print 1 "Modifying state and staus for tests for run: " runkey " " (db:get-value-by-header run header "runname")))
		   ((print-run)
		    (debug:print 1 "Printing info for run " runkey ", run=" run ", tests=" tests ", header=" header)
		    action)
		   (else
		    (debug:print-info 0 "action not recognised " action)))
		 (for-each
		  (lambda (test)
		    (let* ((item-path (db:test-get-item-path test))
			   (test-name (db:test-get-testname test))
			   (run-dir   (db:test-get-rundir test))    ;; run dir is from the link tree
			   (real-dir  (if (file-exists? run-dir)
					  (resolve-pathname run-dir)
					  #f))
			   (test-id   (db:test-get-id test)))
		      ;;   (tdb       (db:open-test-db run-dir)))
		      (debug:print-info 4 "test=" test) ;;   " (db:test-get-testname test) " id: " (db:test-get-id test) " " item-path " action: " action)
		      (case action
			((remove-runs) ;; the tdb is for future possible. 
			 (open-run-close db:delete-test-records db #f (db:test-get-id test))
			 (debug:print-info 1 "Attempting to remove " (if real-dir (conc " dir " real-dir " and ") "") " link " run-dir)
			 (if (and real-dir 
				  (> (string-length real-dir) 5)
				  (file-exists? real-dir)) ;; bad heuristic but should prevent /tmp /home etc.
			     (begin ;; let* ((realpath (resolve-pathname run-dir)))
			       (debug:print-info 1 "Recursively removing " real-dir)
			       (if (file-exists? real-dir)
				   (if (> (system (conc "rm -rf " real-dir)) 0)
				       (debug:print 0 "ERROR: There was a problem removing " real-dir " with rm -f"))
				   (debug:print 0 "WARNING: test dir " real-dir " appears to not exist or is not readable")))
			     (if real-dir 
				 (debug:print 0 "WARNING: directory " real-dir " does not exist")
				 (debug:print 0 "WARNING: no real directory corrosponding to link " run-dir ", nothing done")))
			 (if (symbolic-link? run-dir)
			     (begin
			       (debug:print-info 1 "Removing symlink " run-dir)
			       (handle-exceptions
				exn
				(debug:print 0 "ERROR:  Failed to remove symlink " run-dir ((condition-property-accessor 'exn 'message) exn) ", attempting to continue")
				(delete-file run-dir)))
			     (if (directory? run-dir)
				 (if (> (directory-fold (lambda (f x)(+ 1 x)) 0 run-dir) 0)
				     (debug:print 0 "WARNING: refusing to remove " run-dir " as it is not empty")
				      (handle-exceptions
				       exn
				       (debug:print 0 "ERROR:  Failed to remove directory " run-dir ((condition-property-accessor 'exn 'message) exn) ", attempting to continue")
				       (delete-directory run-dir)))
				 (if run-dir
				     (debug:print 0 "WARNING: not removing " run-dir " as it either doesn't exist or is not a symlink")
				     (debug:print 0 "NOTE: the run dir for this test is undefined. Test may have already been deleted."))
				 )))
			((set-state-status)
			 (debug:print-info 2 "new state " (car state-status) ", new status " (cadr state-status))
			 (open-run-close db:test-set-state-status-by-id db (db:test-get-id test) (car state-status)(cadr state-status) #f)))))
		  (sort tests (lambda (a b)(let ((dira (db:test-get-rundir a))
						 (dirb (db:test-get-rundir b)))
					     (if (and (string? dira)(string? dirb))
						 (> (string-length dira)(string-length dirb))
						 #f)))))))
	   ;; remove the run if zero tests remain
	   (if (eq? action 'remove-runs)
	       (let ((remtests (open-run-close db:get-tests-for-run db (db:get-value-by-header run header "id") #f '("DELETED") '("n/a") not-in: #t)))
		 (if (null? remtests) ;; no more tests remaining
		     (let* ((dparts  (string-split lasttpath "/"))
			    (runpath (conc "/" (string-intersperse 
						(take dparts (- (length dparts) 1))
						"/"))))
		       (debug:print 1 "Removing run: " runkey " " (db:get-value-by-header run header "runname") " and related record")
		       (open-run-close db:delete-run db run-id)
		       ;; This is a pretty good place to purge old DELETED tests
		       (open-run-close db:delete-tests-for-run db run-id)
		       (open-run-close db:delete-old-deleted-test-records db)
		       (open-run-close db:set-var db "DELETED_TESTS" (current-seconds))
		       ;; need to figure out the path to the run dir and remove it if empty
		       ;;    (if (null? (glob (conc runpath "/*")))
		       ;;        (begin
		       ;; 	 (debug:print 1 "Removing run dir " runpath)
		       ;; 	 (system (conc "rmdir -p " runpath))))
		       )))))
	 ))
     runs))
  #t)

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
	;; (th1     #f))
    (cond
     ((not target)
      (debug:print 0 "ERROR: Missing required parameter for " switchname ", you must specify the target with -target")
      (exit 3))
     ((not runname)
      (debug:print 0 "ERROR: Missing required parameter for " switchname ", you must specify the run name with :runname runname")
      (exit 3))
     (else
      (let ((db   #f)
	    (keys #f)
	    (target (or (args:get-arg "-reqtarg")
			(args:get-arg "-target"))))
	(if (not (setup-for-run))
	    (begin 
	      (debug:print 0 "Failed to setup, exiting")
	      (exit 1)))
	;; (if (args:get-arg "-server")
	;;     (open-run-close server:start db (args:get-arg "-server")))
	(set! keys (keys:config-get-fields *configdat*))
	;; have enough to process -target or -reqtarg here
	(if (args:get-arg "-reqtarg")
	    (let* ((runconfigf (conc  *toppath* "/runconfigs.config")) ;; DO NOT EVALUATE ALL 
		   (runconfig  (read-config runconfigf #f #t environ-patt: #f)))
	      (if (hash-table-ref/default runconfig (args:get-arg "-reqtarg") #f)
		  (keys:target-set-args keys (args:get-arg "-reqtarg") args:arg-hash)
		    
		  (begin
		    (debug:print 0 "ERROR: [" (args:get-arg "-reqtarg") "] not found in " runconfigf)
		    (if db (sqlite3:finalize! db))
		    (exit 1))))
	    (if (args:get-arg "-target")
		(keys:target-set-args keys (args:get-arg "-target" args:arg-hash) args:arg-hash)))
	(if (not (car *configinfo*))
	    (begin
	      (debug:print 0 "ERROR: Attempted to " action-desc " but run area config file not found")
	      (exit 1))
	    ;; Extract out stuff needed in most or many calls
	    ;; here then call proc
	    (let* ((keyvals    (keys:target->keyval keys target)))
	      (proc target runname keys keyvals)))
	(if db (sqlite3:finalize! db))
	(set! *didsomething* #t))))))

;;======================================================================
;; Lock/unlock runs
;;======================================================================

(define (runs:handle-locking target keys runname lock unlock user)
  (let* ((db       #f)
	 (rundat   (open-run-close runs:get-runs-by-patt db keys runname target))
	 (header   (vector-ref rundat 0))
	 (runs     (vector-ref rundat 1)))
    (for-each (lambda (run)
		(let ((run-id (db:get-value-by-header run header "id")))
		  (if (or lock
			  (and unlock
			       (begin
				 (print "Do you really wish to unlock run " run-id "?\n   y/n: ")
				 (equal? "y" (read-line)))))
		      (open-run-close db:lock/unlock-run db run-id lock unlock user)
		      (debug:print-info 0 "Skipping lock/unlock on " run-id))))
	      runs)))
;;======================================================================
;; Rollup runs
;;======================================================================

;; Update the test_meta table for this test
(define (runs:update-test_meta test-name test-conf)
  (let ((currrecord (cdb:remote-run db:testmeta-get-record #f test-name)))
    (if (not currrecord)
	(begin
	  (set! currrecord (make-vector 10 #f))
	  (cdb:remote-run db:testmeta-add-record #f test-name)))
    (for-each 
     (lambda (key)
       (let* ((idx (cadr key))
	      (fld (car  key))
	      (val (config-lookup test-conf "test_meta" fld)))
	 ;; (debug:print 5 "idx: " idx " fld: " fld " val: " val)
	 (if (and val (not (equal? (vector-ref currrecord idx) val)))
	     (begin
	       (print "Updating " test-name " " fld " to " val)
	       (cdb:remote-run db:testmeta-update-field #f test-name fld val)))))
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
	 ;; use the open-run-close instead of passing in db
	 (runs:update-test_meta test-name test-conf)))
     test-names)))

;; This could probably be refactored into one complex query ...
(define (runs:rollup-run keys runname user keyvals)
  (debug:print 4 "runs:rollup-run, keys: " keys " :runname " runname " user: " user)
  (let* ((db              #f)
	 (new-run-id      (cdb:remote-run db:register-run #f keys keyvals runname "new" "n/a" user))
	 (prev-tests      (open-run-close test:get-matching-previous-test-run-records db new-run-id "%" "%"))
	 (curr-tests      (open-run-close db:get-tests-for-run db new-run-id "%/%" '() '()))
	 (curr-tests-hash (make-hash-table)))
    (open-run-close db:update-run-event_time db new-run-id)
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
	      (test-steps    (open-run-close db:get-steps-for-test db (db:test-get-id testdat)))
	      (new-test-record #f))
	 ;; replace these with insert ... select
	 (apply sqlite3:execute 
		db 
		(conc "INSERT OR REPLACE INTO tests (run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment) "
		      "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?);")
		new-run-id (cddr (vector->list testdat)))
	 (set! new-testdat (car (open-run-close db:get-tests-for-run db new-run-id (conc testname "/" item-path) '() '())))
	 (hash-table-set! curr-tests-hash full-name new-testdat) ;; this could be confusing, which record should go into the lookup table?
	 ;; Now duplicate the test steps
	 (debug:print 4 "Copying records in test_steps from test_id=" (db:test-get-id testdat) " to " (db:test-get-id new-testdat))
	 (open-run-close 
	  (lambda ()
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
	     (db:test-get-id testdat))))
	 ))
     prev-tests)))
	 
     
