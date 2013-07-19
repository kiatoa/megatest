
;; Copyright 2006-2013, Matthew Welland.
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
(declare (uses mt))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

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
			  (args:get-arg "-target")
			  (get-environment-variable "MT_TARGET")))
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
    (if (not (get-environment-variable "MT_TARGET"))(setenv "MT_TARGET" target))
    (alist->env-vars (hash-table-ref/default *configdat* "env-override" '()))
    ;; Lets use this as an opportunity to put MT_RUNNAME in the environment
    (setenv "MT_RUNNAME" (if inrunname inrunname (cdb:remote-run db:get-run-name-from-id #f run-id)))
    (setenv "MT_RUN_AREA_HOME" *toppath*)))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (debug:print 2 "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

;; Every time can-run-more-tests is called increment the delay
;;
;; NOTE: We run this server-side!! Do not use this global except in the runs:can-run-more-tests routine
;;
(define *last-num-running-tests* 0)
(define *runs:can-run-more-tests-count* 0)
(define (runs:shrink-can-run-more-tests-count) ;; the db is a dummy var so we can use cdb:remote-run
  (set! *runs:can-run-more-tests-count* 0)) ;; (/ *runs:can-run-more-tests-count* 2)))

(define (runs:can-run-more-tests jobgroup max-concurrent-jobs)
  (thread-sleep! (cond
		  ((> *runs:can-run-more-tests-count* 20) 2);; obviously haven't had any work to do for a while
		  (else 0)))
  (let* ((num-running             (cdb:remote-run db:get-count-tests-running #f))
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

;;  test-names: Comma separated patterns same as test-patts but used in selection 
;;              of tests to run. The item portions are not respected.
;;              FIXME: error out if /patt specified
;;            
(define (runs:run-tests target runname test-patts user flags) ;; test-names
  (common:clear-caches) ;; clear all caches
  (let* ((keys               (keys:config-get-fields *configdat*))
	 (keyvals            (keys:target->keyval keys target))
	 (run-id             (cdb:remote-run db:register-run #f keyvals runname "new" "n/a" user))  ;;  test-name)))
	 (deferred          '()) ;; delay running these since they have a waiton clause
	 (runconfigf         (conc  *toppath* "/runconfigs.config"))
	 (required-tests    '())
	 (test-records       (make-hash-table))
	 (all-tests-registry (tests:get-all)) ;; (tests:get-valid-tests (make-hash-table) test-search-path)) ;; all valid tests to check waiton names
	 (all-test-names     (hash-table-keys all-tests-registry))
	 (test-names         (tests:filter-test-names all-test-names test-patts)))
    (set-megatest-env-vars run-id inkeys: keys) ;; these may be needed by the launching process
    (if (file-exists? runconfigf)
	(setup-env-defaults runconfigf run-id *already-seen-runconfig-info* keyvals "pre-launch-env-vars")
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))
    
    ;; look up all tests matching the comma separated list of globs in
    ;; test-patts (using % as wildcard)

    ;; (set! test-names (delete-duplicates (tests:get-valid-tests *toppath* test-patts)))
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

    ;; now add non-directly referenced dependencies (i.e. waiton)
    ;;======================================================================
    ;; refactoring this block into tests:get-full-data
    ;;
    ;; What happended, this code is now duplicated in tests!?
    ;;
    ;;======================================================================
    (if (not (null? test-names))
	(let loop ((hed (car test-names))
		   (tal (cdr test-names)))         ;; 'return-procs tells the config reader to prep running system but return a proc
	  (change-directory *toppath*) ;; PLEASE OPTIMIZE ME!!! I think this should be a no-op but there are several places where change-directories could be happening.
	  (let* ((config  (tests:get-testconfig hed all-tests-registry 'return-procs))
		 (waitons (let ((instr (if config 
					   (config-lookup config "requirements" "waiton")
					   (begin ;; No config means this is a non-existant test
					     (debug:print 0 "ERROR: non-existent required test \"" hed "\"")
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
					(if (hash-table-ref/default all-tests-registry x #f)
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
    (let ((reglen (configf:lookup *configdat* "setup" "runqueue")))
      (if (> (length (hash-table-keys test-records)) 0)
	  (runs:run-tests-queue run-id runname test-records keyvals flags test-patts required-tests (any->number reglen) all-tests-registry)
	  (debug:print-info 0 "No tests to run")))
    (debug:print-info 4 "All done by here")))


;; loop logic. These are used in runs:run-tests-queue to make it a bit more readable.
;;
;; If reg not full and have items in tal then loop with (car tal)(cdr tal) reg reruns
;; If reg is full (i.e. length >= n
;;   loop with (car reg) tal (cdr reg) reruns
;; If tal is empty
;;   but have items in reg; loop with (car reg)(cdr reg) '() reruns
;;   If reg is empty => all done

(define (runs:queue-next-hed tal reg n regfull)
  (if regfull
      (car reg)
      (if (null? tal) ;; tal is used up, pop from reg
	  (car reg)
	  (car tal))))

;;   (cond
;;    ((and regfull (null? reg)(not (null? tal)))      (car tal))
;;    ((and regfull (not (null? reg)))                 (car reg))
;;    ((and (not regfull)(null? tal)(not (null? reg))) (car reg))
;;    ((and (not regfull)(not (null? tal)))            (car tal))
;;    (else
;;     (debug:print 0 "ERROR: runs:queue-next-hed, tal=" tal ", reg=" reg ", n=" n ", regfull=" regfull)
;;     #f)))

(define (runs:queue-next-tal tal reg n regfull)
  (if regfull
      tal
      (if (null? tal) ;; must transfer from reg
	  (cdr reg)
	  (cdr tal))))

(define (runs:queue-next-reg tal reg n regfull)
  (if regfull
      (cdr reg)
      (if (null? tal) ;; if tal is null and reg not full then '() as reg contents moved to tal
	  '()
	  reg)))

(define (runs:expand-items hed tal reg reruns regfull newtal jobgroup max-concurrent-jobs run-id waitons item-path testmode test-record can-run-more items runname tconfig reglen)
  (let* ((loop-list       (list hed tal reg reruns))
	 (prereqs-not-met (mt:get-prereqs-not-met run-id waitons item-path mode: testmode))
	 (fails           (runs:calc-fails prereqs-not-met))
	 (non-completed   (runs:calc-not-completed prereqs-not-met)))
    (debug:print-info 4 "START OF INNER COND #2 "
		      "\n can-run-more:    " can-run-more
		      "\n testname:        " hed
		      "\n prereqs-not-met: " (runs:pretty-string prereqs-not-met)
		      "\n non-completed:   " (runs:pretty-string non-completed) 
		      "\n fails:           " (runs:pretty-string fails)
		      "\n testmode:        " testmode
		      "\n (eq? testmode 'toplevel): " (eq? testmode 'toplevel)
		      "\n (null? non-completed):    " (null? non-completed)
		      "\n reruns:          " reruns
		      "\n items:           " items
		      "\n can-run-more:    " can-run-more)
    (cond
     ;; all prereqs met, fire off the test
     ;; or, if it is a 'toplevel test and all prereqs not met are COMPLETED then launch
     ((or (null? prereqs-not-met)
	  (and (eq? testmode 'toplevel)
	       (null? non-completed)))
      (debug:print-info 4 "runs:expand-items: (or (null? prereqs-not-met) (and (eq? testmode 'toplevel)(null? non-completed)))")
      (let ((test-name (tests:testqueue-get-testname test-record)))
	(setenv "MT_TEST_NAME" test-name) ;; 
	(setenv "MT_RUNNAME"   runname)
	(set-megatest-env-vars run-id inrunname: runname) ;; these may be needed by the launching process
	(let ((items-list (items:get-items-from-config tconfig)))
	  (if (list? items-list)
	      (begin
		(tests:testqueue-set-items! test-record items-list)
		(list hed tal reg reruns))
	      (begin
		(debug:print 0 "ERROR: The proc from reading the setup did not yield a list - please report this")
		(exit 1))))))

     ((null? fails)
      (debug:print-info 4 "fails is null, moving on in the queue but keeping " hed " for now")
      ;; num-retries code was here
      ;; we use this opportunity to move contents of reg to tal
      (list (car newtal)(append (cdr newtal) reg) '() reruns)) ;; an issue with prereqs not yet met?

     ((and (not (null? fails))(eq? testmode 'normal))
      (debug:print-info 1 "test "  hed " (mode=" testmode ") has failed prerequisite(s); "
			(string-intersperse (map (lambda (t)(conc (db:test-get-testname t) ":" (db:test-get-state t)"/"(db:test-get-status t))) fails) ", ")
			", removing it from to-do list")
      (if (or (not (null? reg))(not (null? tal)))
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		(runs:queue-next-reg tal reg reglen regfull)
		(cons hed reruns))
	  #f)) ;; #f flags do not loop

     (else
      (debug:print 4 "ERROR: No handler for this condition.")
      (list (car newtal)(cdr newtal) reg reruns)))))

(define (runs:process-expanded-tests hed tal reg reruns reglen regfull test-record runname test-name item-path jobgroup max-concurrent-jobs run-id waitons item-path testmode test-patts required-tests test-registry registry-mutex flags keyvals run-info newtal all-tests-registry)
  (let* ((run-limits-info         (runs:can-run-more-tests jobgroup max-concurrent-jobs)) ;; look at the test jobgroup and tot jobs running
	 (have-resources          (car run-limits-info))
	 (num-running             (list-ref run-limits-info 1))
	 (num-running-in-jobgroup (list-ref run-limits-info 2))
	 (max-concurrent-jobs     (list-ref run-limits-info 3))
	 (job-group-limit         (list-ref run-limits-info 4))
	 (prereqs-not-met         (mt:get-prereqs-not-met run-id waitons item-path mode: testmode))
	 (fails                   (runs:calc-fails prereqs-not-met))
	 (non-completed           (runs:calc-not-completed prereqs-not-met))
	 (loop-list               (list hed tal reg reruns)))
    (debug:print-info 4 "have-resources: " have-resources " prereqs-not-met: (" 
		      (string-intersperse 
		       (map (lambda (t)
			      (if (vector? t)
				  (conc (db:test-get-state t) "/" (db:test-get-status t))
				  (conc " WARNING: t is not a vector=" t )))
			    prereqs-not-met) ", ") ") fails: " fails)
    
    ;; Don't know at this time if the test have been launched at some time in the past
    ;; i.e. is this a re-launch?
    (debug:print-info 4 "run-limits-info = " run-limits-info)
    
    (cond
     
     ;; Check item path against item-patts, 
     ;;
     ((not (tests:match test-patts (tests:testqueue-get-testname test-record) item-path required: required-tests)) ;; This test/itempath is not to be run
      ;; else the run is stuck, temporarily or permanently
      ;; but should check if it is due to lack of resources vs. prerequisites
      (debug:print-info 1 "Skipping " (tests:testqueue-get-testname test-record) " " item-path " as it doesn't match " test-patts)
      (if (or (not (null? tal))(not (null? reg)))
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		(runs:queue-next-reg tal reg reglen regfull)
		reruns)
	  #f))
     
     ;; Register tests 
     ;;
     ((not (hash-table-ref/default test-registry (runs:make-full-test-name test-name item-path) #f))
      (debug:print-info 4 "Pre-registering test " test-name "/" item-path " to create placeholder" )
      (let ((th (make-thread (lambda ()
			       (mutex-lock! registry-mutex)
			       (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'start)
			       (mutex-unlock! registry-mutex)
			       ;; If haven't done it before register a top level test if this is an itemized test
			       (if (not (eq? (hash-table-ref/default test-registry (runs:make-full-test-name test-name "") #f) 'done))
				   (cdb:tests-register-test *runremote* run-id test-name ""))
			       (cdb:tests-register-test *runremote* run-id test-name item-path)
			       (mutex-lock! registry-mutex)
			       (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'done)
			       (mutex-unlock! registry-mutex))
			     (conc test-name "/" item-path))))
	(thread-start! th))
      (runs:shrink-can-run-more-tests-count)   ;; DELAY TWEAKER (still needed?)
      (if (and (null? tal)(null? reg))
	  (list hed tal (append reg (list hed)) reruns)
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		;; NB// Here we are building reg as we register tests
		;; if regfull we must pop the front item off reg
		(if regfull
		    (append (cdr reg) (list hed))
		    (append reg (list hed)))
		reruns)))
     
     ;; At this point hed test registration must be completed.
     ;;
     ((eq? (hash-table-ref/default test-registry (runs:make-full-test-name test-name item-path) #f)
	   'start)
      (debug:print-info 0 "Waiting on test registration(s): "
			(string-intersperse 
			 (filter (lambda (x)
				   (eq? (hash-table-ref/default test-registry x #f) 'start))
				 (hash-table-keys test-registry))
			 ", "))
      (thread-sleep! 0.1)
      (list hed tal reg reruns))
     
     ;; If no resources are available just kill time and loop again
     ;;
     ((not have-resources) ;; simply try again after waiting a second
      (debug:print-info 1 "no resources to run new tests, waiting ...")
      ;; Have gone back and forth on this but db starvation is an issue.
      ;; wait one second before looking again to run jobs.
      (thread-sleep! 1)
      ;; could have done hed tal here but doing car/cdr of newtal to rotate tests
      (list (car newtal)(cdr newtal) reg reruns))
     
     ;; This is the final stage, everything is in place so launch the test
     ;;
     ((and have-resources
	   (or (null? prereqs-not-met)
	       (and (eq? testmode 'toplevel)
		    (null? non-completed))))
      (run:test run-id run-info keyvals runname test-record flags #f test-registry all-tests-registry)
      (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'running)
      (runs:shrink-can-run-more-tests-count)  ;; DELAY TWEAKER (still needed?)
      ;; (thread-sleep! *global-delta*)
      (if (or (not (null? tal))(not (null? reg)))
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		(runs:queue-next-reg tal reg reglen regfull)
		reruns)
	  #f))
     
     ;; must be we have unmet prerequisites
     ;;
     (else
      (debug:print 4 "FAILS: " fails)
      ;; If one or more of the prereqs-not-met are FAIL then we can issue
      ;; a message and drop hed from the items to be processed.
      (if (null? fails)
	  (begin
	    ;; couldn't run, take a breather
	    (debug:print-info 4 "Shouldn't really get here, race condition? Unable to launch more tests at this moment, killing time ...")
	    ;; (thread-sleep! (+ 0.01 *global-delta*)) ;; long sleep here - no resources, may as well be patient
	    ;; we made new tal by sticking hed at the back of the list
	    (list (car newtal)(cdr newtal) reg reruns))
	  ;; the waiton is FAIL so no point in trying to run hed ever again
	  (if (or (not (null? reg))(not (null? tal)))
	      (if (vector? hed)
		  (begin 
		    (debug:print 1 "WARN: Dropping test " (db:test-get-testname hed) "/" (db:test-get-item-path hed)
				 " from the launch list as it has prerequistes that are FAIL")
		    (runs:shrink-can-run-more-tests-count) ;; DELAY TWEAKER (still needed?)
		    ;; (thread-sleep! *global-delta*)
		    (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'removed)
		    (list (runs:queue-next-hed tal reg reglen regfull)
			  (runs:queue-next-tal tal reg reglen regfull)
			  (runs:queue-next-reg tal reg reglen regfull)
			  (cons hed reruns)))
		  (begin
		    (debug:print 1 "WARN: Test not processed correctly. Could be a race condition in your test implementation? " hed) ;;  " as it has prerequistes that are FAIL. (NOTE: hed is not a vector)")
		    (runs:shrink-can-run-more-tests-count) ;; DELAY TWEAKER (still needed?)
		    (list hed tal reg reruns)))))))))

;; test-records is a hash table testname:item_path => vector < testname testconfig waitons priority items-info ... >
(define (runs:run-tests-queue run-id runname test-records keyvals flags test-patts required-tests reglen-in all-tests-registry)
  ;; At this point the list of parent tests is expanded 
  ;; NB// Should expand items here and then insert into the run queue.
  (debug:print 5 "test-records: " test-records ", flags: " (hash-table->alist flags))
  (let ((run-info              (cdb:remote-run db:get-run-info #f run-id))
	(tests-info            (mt:get-tests-for-run run-id #f '() '())) ;;  qryvals: "id,testname,item_path"))
	(sorted-test-names     (tests:sort-by-priority-and-waiton test-records))
	(test-registry         (make-hash-table))
	(registry-mutex        (make-mutex))
	(num-retries           0)
	(max-retries           (config-lookup *configdat* "setup" "maxretries"))
	(max-concurrent-jobs   (let ((mcj (config-lookup *configdat* "setup"     "max_concurrent_jobs")))
				 (if (and mcj (string->number mcj))
				     (string->number mcj)
				     1))) ;; length of the register queue ahead
	(reglen                (if (number? reglen-in) reglen-in 1)))

    ;; Initialize the test-registery hash with tests that already have a record
    ;; convert state to symbol and use that as the hash value
    (for-each (lambda (trec)
		(let ((id (db:test-get-id        trec))
		      (tn (db:test-get-testname  trec))
		      (ip (db:test-get-item-path trec))
		      (st (db:test-get-state     trec)))
		  (hash-table-set! test-registry (runs:make-full-test-name tn ip) (string->symbol st))))
	      tests-info)
    (set! max-retries (if (and max-retries (string->number max-retries))(string->number max-retries) 100))

    (let loop ((hed         (car sorted-test-names))
	       (tal         (cdr sorted-test-names))
	       (reg         '()) ;; registered, put these at the head of tal 
	       (reruns      '()))
      (if (not (null? reruns))(debug:print-info 4 "reruns=" reruns))
      ;; (print "Top of loop, hed=" hed ", tal=" tal " ,reruns=" reruns)
      (let* ((test-record (hash-table-ref test-records hed))
	     (test-name   (tests:testqueue-get-testname test-record))
	     (tconfig     (tests:testqueue-get-testconfig test-record))
	     (jobgroup    (config-lookup tconfig "requirements" "jobgroup"))
	     (testmode    (let ((m (config-lookup tconfig "requirements" "mode")))
			    (if m (string->symbol m) 'normal)))
	     (waitons     (tests:testqueue-get-waitons    test-record))
	     (priority    (tests:testqueue-get-priority   test-record))
	     (itemdat     (tests:testqueue-get-itemdat    test-record)) ;; itemdat can be a string, list or #f
	     (items       (tests:testqueue-get-items      test-record))
	     (item-path   (item-list->path itemdat))
	     (tfullname   (runs:make-full-test-name test-name item-path))
	     (newtal      (append tal (list hed)))
	     (regfull     (>= (length reg) reglen)))

	;; Fast skip of tests that are already "COMPLETED"
	;;
	(if (equal? (hash-table-ref/default test-registry tfullname #f) 'COMPLETED)
	    (begin
	      (debug:print-info 0 "Skipping COMPLETED test " tfullname)
	      (if (or (not (null? tal))(not (null? reg)))
		  (loop (runs:queue-next-hed tal reg reglen regfull)
			(runs:queue-next-tal tal reg reglen regfull)
			(runs:queue-next-reg tal reg reglen regfull)
			reruns))))
		  ;; (loop (car tal)(cdr tal) reg reruns))))

	(debug:print 4 "TOP OF LOOP => "
		     "test-name: " test-name
		     "\n  test-record  " test-record
		     "\n  hed:         " hed
		     "\n  itemdat:     " itemdat
		     "\n  items:       " items
		     "\n  item-path:   " item-path
		     "\n  waitons:     " waitons
		     "\n  num-retries: " num-retries
		     "\n  tal:         " tal
		     "\n  reruns:      " reruns
		     "\n  regfull:     " regfull
		     "\n  reglen:      " reglen
		     "\n  length reg:  " (length reg)
		     "\n  reg:         " reg)

	;; check for hed in waitons => this would be circular, remove it and issue an
	;; error
	(if (member test-name waitons)
	    (begin
	      (debug:print 0 "ERROR: test " test-name " has listed itself as a waiton, please correct this!")
	      (set! waiton (filter (lambda (x)(not (equal? x hed))) waitons))))

	(cond 

	 ;; items is #f then the test is ok to be handed off to launch (but not before)
	 ;; 
	 ((not items)
	  (debug:print-info 4 "OUTER COND: (not items)")
	  (if (and (not (tests:match test-patts (tests:testqueue-get-testname test-record) item-path required: required-tests))
		   (not (null? tal)))
	      (loop (car tal)(cdr tal) reg reruns))
	  (let ((loop-list (runs:process-expanded-tests hed tal reg reruns reglen regfull test-record runname test-name item-path jobgroup max-concurrent-jobs run-id waitons item-path testmode test-patts required-tests test-registry registry-mutex flags keyvals run-info newtal all-tests-registry)))
	    (if loop-list (apply loop loop-list))))

	 ;; items processed into a list but not came in as a list been processed
	 ;;
	 ((and (list? items)     ;; thus we know our items are already calculated
	       (not   itemdat))  ;; and not yet expanded into the list of things to be done
	  (debug:print-info 4 "OUTER COND: (and (list? items)(not itemdat))")
	  ;; Must determine if the items list is valid. Discard the test if it is not.
	  (if (and (list? items)
		   (> (length items) 0)
		   (and (list? (car items))
			(> (length (car items)) 0))
		   (debug:debug-mode 1))
	      (debug:print 2 (map (lambda (row)
				    (conc (string-intersperse
					   (map (lambda (varval)
						  (string-intersperse varval "="))
						row)
					   " ")
					  "\n"))
				  items)))
	  (for-each
	   (lambda (my-itemdat)
	     (let* ((new-test-record (let ((newrec (make-tests:testqueue)))
				       (vector-copy! test-record newrec)
				       newrec))
		    (my-item-path (item-list->path my-itemdat)))
	       (if (tests:match test-patts hed my-item-path required: required-tests) ;; (patt-list-match my-item-path item-patts)           ;; yes, we want to process this item, NOTE: Should not need this check here!
		   (let ((newtestname (runs:make-full-test-name hed my-item-path)))    ;; test names are unique on testname/item-path
		     (tests:testqueue-set-items!     new-test-record #f)
		     (tests:testqueue-set-itemdat!   new-test-record my-itemdat)
		     (tests:testqueue-set-item_path! new-test-record my-item-path)
		     (hash-table-set! test-records newtestname new-test-record)
		     (set! tal (append tal (list newtestname))))))) ;; since these are itemized create new test names testname/itempath
	   items)

	  ;; (debug:print-info 0 "Test " (tests:testqueue-get-testname test-record) " is itemized but has no items")

	  ;; At this point we have possibly added items to tal but all must be handed off to 
	  ;; INNER COND logic. I think loop without rotating the queue 
	  ;; (loop hed tal reg reruns))
	  ;; (let ((newtal (append tal (list hed))))  ;; We should discard hed as it has been expanded into it's items? Yes, but only if this *is* an itemized test
	  ;; (loop (car newtal)(cdr newtal) reg reruns)
	  (if (null? tal)
	      #f
	      (loop (car tal)(cdr tal) reg reruns)))
	    
	 ;; if items is a proc then need to run items:get-items-from-config, get the list and loop 
	 ;;    - but only do that if resources exist to kick off the job
	 ;; EXPAND ITEMS
	 ((or (procedure? items)(eq? items 'have-procedure))
	  (let ((can-run-more    (runs:can-run-more-tests jobgroup max-concurrent-jobs)))
	    (if (and (list? can-run-more)
		     (car can-run-more))
		(let ((loop-list (runs:expand-items hed tal reg reruns regfull newtal jobgroup max-concurrent-jobs run-id waitons item-path testmode test-record can-run-more items runname tconfig reglen)))
		  (if loop-list
		      (apply loop loop-list)))
		;; if can't run more just loop with next possible test
		(loop (car newtal)(cdr newtal) reg reruns))))
	    
	 ;; this case should not happen, added to help catch any bugs
	 ((and (list? items) itemdat)
	  (debug:print 0 "ERROR: Should not have a list of items in a test and the itemspath set - please report this")
	  (exit 1))
	 ((not (null? reruns))
	  (let* ((newlst (tests:filter-non-runnable run-id tal test-records)) ;; i.e. not FAIL, WAIVED, INCOMPLETE, PASS, KILLED,
		 (junked (lset-difference equal? tal newlst)))
	    (debug:print-info 4 "full drop through, if reruns is less than 100 we will force retry them, reruns=" reruns ", tal=" tal)
	    (if (< num-retries max-retries)
		(set! newlst (append reruns newlst)))
	    (set! num-retries (+ num-retries 1))
	    ;; (thread-sleep! (+ 1 *global-delta*))
	    (if (not (null? newlst))
		;; since reruns have been tacked on to newlst create new reruns from junked
		(loop (car newlst)(cdr newlst) reg (delete-duplicates junked)))))
	 ((not (null? tal))
	  (debug:print-info 4 "I'm pretty sure I shouldn't get here."))
	 ((not (null? reg)) ;; could we get here with leftovers?
	  (debug:print-info 0 "Have leftovers!")
	  (loop (car reg)(cdr reg) '() reruns))
	 (else
	  (debug:print-info 4 "Exiting loop with...\n  hed=" hed "\n  tal=" tal "\n  reruns=" reruns))
	 ))) ;; LET* ((test-record
    
    ;; we get here on "drop through". All done!
    (debug:print-info 1 "All tests launched")))

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

;; parent-test is there as a placeholder for when parent-tests can be run as a setup step
(define (run:test run-id run-info keyvals runname test-record flags parent-test test-registry all-tests-registry)
  ;; All these vars might be referenced by the testconfig file reader
  (let* ((test-name    (tests:testqueue-get-testname   test-record))
	 (test-waitons (tests:testqueue-get-waitons    test-record))
	 (test-conf    (tests:testqueue-get-testconfig test-record))
	 (itemdat      (tests:testqueue-get-itemdat    test-record))
	 (test-path    (hash-table-ref all-tests-registry test-name)) ;; (conc *toppath* "/tests/" test-name)) ;; could use tests:get-testconfig here ...
	 (force        (hash-table-ref/default flags "-force" #f))
	 (rerun        (hash-table-ref/default flags "-rerun" #f))
	 (keepgoing    (hash-table-ref/default flags "-keepgoing" #f))
	 (item-path     "")
	 (db           #f)
	 (full-test-name #f))

    ;; setting itemdat to a list if it is #f
    (if (not itemdat)(set! itemdat '()))
    (set! item-path (item-list->path itemdat))
    (set! full-test-name (runs:make-full-test-name test-name item-path))
    (debug:print-info 4
		      "\nTESTNAME: " full-test-name 
		      "\n   test-config: " (hash-table->alist test-conf)
		      "\n   itemdat: " itemdat
		      )
    (debug:print 2 "Attempting to launch test " full-test-name)
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
    
    ;; itemdat => ((ripeness "overripe") (temperature "cool") (season "summer"))
    (let* ((new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
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
	    (set! test-id (cdb:remote-run db:get-test-id #f run-id test-name item-path))
	    (if (not test-id)
		(begin
		  (debug:print 2 "WARN: Test not pre-created? test-name=" test-name ", item-path=" item-path ", run-id=" run-id)
		  (cdb:tests-register-test *runremote* run-id test-name item-path)
		  (set! test-id (cdb:remote-run db:get-test-id #f run-id test-name item-path))))
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
	     (hash-table-set! test-registry full-test-name 'COMPLETED)
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
	   (debug:print 4 "RUNNING => runflag: " runflag " STATE: " (test:get-state testdat) " STATUS: " (test:get-status testdat))
	   (if (not runflag)
	       (if (not parent-test)
		   (debug:print 1 "NOTE: Not starting test " full-test-name " as it is state \"" (test:get-state testdat) 
				"\" and status \"" (test:get-status testdat) "\", use -rerun \"" (test:get-status testdat)
                                "\" or -force to override"))
	       ;; NOTE: No longer be checking prerequisites here! Will never get here unless prereqs are
	       ;;       already met.
	       ;; This would be a great place to do the process-fork
	       ;; 
	       (let ((skip-test   #f)
		     (skip-check  (configf:get-section test-conf "skip")))
		 (cond 
		  ;; Have to check for skip conditions. This one skips if there are same-named tests
		  ;; currently running
		  ((and skip-check
			(configf:lookup test-conf "skip" "prevrunning"))
		   (let ((running-tests (cdb:remote-run db:get-tests-for-runs-mindata #f #f full-test-name '("RUNNING") '() #f)))
		     (if (not (null? running-tests)) ;; have to skip 
			 (set! skip-test "Skipping due to previous tests running"))))
		  ((and skip-check
			(configf:lookup test-conf "skip" "fileexists"))
		   (if (file-exists? (configf:lookup test-conf "skip" "fileexists"))
		       (set! skip-test (conc "Skipping due to existance of file " (configf:lookup test-conf "skip" "fileexists"))))))
		 (if skip-test
		     (begin
		       (cdb:remote-run db:test-set-state-status-by-id #f test-id "COMPLETED" "SKIP" skip-test)
		       (debug:print-info 1 "SKIPPING Test " full-test-name " due to " skip-test))
		     (if (not (launch-test test-id run-id run-info keyvals runname test-conf test-name test-path itemdat flags))
			 (begin
			   (print "ERROR: Failed to launch the test. Exiting as soon as possible")
			   (set! *globalexitstatus* 1) ;; 
			   (process-signal (current-process-id) signal/kill))))))))
	((KILLED) 
	 (debug:print 1 "NOTE: " full-test-name " is already running or was explictly killed, use -force to launch it."))
	((LAUNCHED REMOTEHOSTSTART RUNNING)  
	 (if (> (- (current-seconds)(+ (db:test-get-event_time testdat)
				       (db:test-get-run_duration testdat)))
		600) ;; i.e. no update for more than 600 seconds
	     (begin
	       (debug:print 0 "WARNING: Test " test-name " appears to be dead. Forcing it to state INCOMPLETE and status STUCK/DEAD")
	       (tests:test-set-status! test-id "INCOMPLETE" "STUCK/DEAD" "Test is stuck or dead" #f))
	     (debug:print 2 "NOTE: " test-name " is already running")))
	(else       (debug:print 0 "ERROR: Failed to launch test " full-test-name ". Unrecognised state " (test:get-state testdat)))))))

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
	 (keys         (cdb:remote-run db:get-keys db))
	 (rundat       (mt:get-runs-by-patt keys runnamepatt target))
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
	     (dirs-to-remove (make-hash-table))
	     (proc-get-tests (lambda (run-id)
			      (mt:get-tests-for-run run-id
						    testpatt states statuses
						    not-in:  #f
						    sort-by: (case action
							       ((remove-runs) 'rundir)
							       (else          'event_time))))))
	 (let* ((run-id    (db:get-value-by-header run header "id"))
		(run-state (db:get-value-by-header run header "state"))
		(tests     (if (not (equal? run-state "locked"))
			       (proc-get-tests run-id)
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
		   ((run-wait)
		    (debug:print 1 "Waiting for run " runkey ", run=" runnamepatt " to complete"))
		   (else
		    (debug:print-info 0 "action not recognised " action)))
		 (let ((sorted-tests     (sort tests (lambda (a b)(let ((dira (db:test-get-rundir a))
									(dirb (db:test-get-rundir b)))
								    (if (and (string? dira)(string? dirb))
									(> (string-length dira)(string-length dirb))
									#f)))))
		       (test-retry-time  (make-hash-table))
		       (allow-run-time   10)) ;; seconds to allow for killing tests before just brutally killing 'em
		   (let loop ((test (car sorted-tests))
			      (tal  (cdr sorted-tests)))
		     (let* ((test-id       (db:test-get-id test))
			    (new-test-dat  (cdb:remote-run db:get-test-info-by-id #f test-id))
			    (item-path     (db:test-get-item-path new-test-dat))
			    (test-name     (db:test-get-testname new-test-dat))
			    (run-dir       (db:test-get-rundir new-test-dat))    ;; run dir is from the link tree
			    (real-dir      (if (file-exists? run-dir)
					       (resolve-pathname run-dir)
					       #f))
			    (test-state    (db:test-get-state new-test-dat))
			    (test-fulln    (db:test-get-fullname new-test-dat)))
			   (case action
			     ((remove-runs)
			      (debug:print-info 0 "test: " test-name " itest-state: " test-state)
			      (if (member test-state (list "RUNNING" "LAUNCHED" "REMOTEHOSTSTART" "KILLREQ"))
				  (begin
				    (if (not (hash-table-ref/default test-retry-time test-fulln #f))
					(hash-table-set! test-retry-time test-fulln (current-seconds)))
				    (if (> (- (current-seconds)(hash-table-ref test-retry-time test-fulln)) allow-run-time)
				      ;; This test is not in a correct state for cleaning up. Let's try some graceful shutdown steps first
				      ;; Set the test to "KILLREQ" and wait five seconds then try again. Repeat up to five times then give
				      ;; up and blow it away.
				      (begin
					(debug:print 0 "WARNING: could not gracefully remove test " test-fulln ", tried to kill it to no avail. Forcing state to FAILEDKILL and continuing")
					(cdb:remote-run db:test-set-state-status-by-id db (db:test-get-id test) "FAILEDKILL" "n/a" #f)
					(thread-sleep! 1))
				      (begin
					(cdb:remote-run db:test-set-state-status-by-id db (db:test-get-id test) "KILLREQ" "n/a" #f)
					(thread-sleep! 1)))
				    ;; NOTE: This is suboptimal as the testdata will be used later and the state/status may have changed ...
				    (if (null? tal)
					(loop new-test-dat tal)
					(loop (car tal)(append tal (list new-test-dat)))))
				  (begin
				    (cdb:remote-run db:delete-test-records db #f (db:test-get-id test))
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
					    ))
				    (if (not (null? tal))
					(loop (car tal)(cdr tal))))))
			     ((set-state-status)
			      (debug:print-info 2 "new state " (car state-status) ", new status " (cadr state-status))
			      (cdb:remote-run db:test-set-state-status-by-id db (db:test-get-id test) (car state-status)(cadr state-status) #f)
			      (if (not (null? tal))
				  (loop (car tal)(cdr tal))))
			     ((run-wait)
			      (debug:print-info 2 "still waiting, " (length tests) " tests still running")
			      (thread-sleep! 10)
			      (let ((new-tests (proc-get-tests run-id)))
				(if (null? new-tests)
				    (debug:print-info 1 "Run completed according to zero tests matching provided criteria.")
				    (loop (car new-tests)(cdr new-tests))))))))
		   )))
	   ;; remove the run if zero tests remain
	   (if (eq? action 'remove-runs)
	       (let ((remtests (mt:get-tests-for-run (db:get-value-by-header run header "id") #f '("DELETED") '("n/a") not-in: #t)))
		 (if (null? remtests) ;; no more tests remaining
		     (let* ((dparts  (string-split lasttpath "/"))
			    (runpath (conc "/" (string-intersperse 
						(take dparts (- (length dparts) 1))
						"/"))))
		       (debug:print 1 "Removing run: " runkey " " (db:get-value-by-header run header "runname") " and related record")
		       (cdb:remote-run db:delete-run db run-id)
		       ;; This is a pretty good place to purge old DELETED tests
		       (cdb:remote-run db:delete-tests-for-run db run-id)
		       (cdb:remote-run db:delete-old-deleted-test-records db)
		       (cdb:remote-run db:set-var db "DELETED_TESTS" (current-seconds))
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
	;;     (cdb:remote-run server:start db (args:get-arg "-server")))
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
	 (rundat   (mt:get-runs-by-patt keys runname target))
	 (header   (vector-ref rundat 0))
	 (runs     (vector-ref rundat 1)))
    (for-each (lambda (run)
		(let ((run-id (db:get-value-by-header run header "id")))
		  (if (or lock
			  (and unlock
			       (begin
				 (print "Do you really wish to unlock run " run-id "?\n   y/n: ")
				 (equal? "y" (read-line)))))
		      (cdb:remote-run db:lock/unlock-run db run-id lock unlock user)
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
  (let ((test-names (tests:get-valid-tests)))
    (for-each 
     (lambda (test-name)
       (let* ((test-path    (conc *toppath* "/tests/" test-name))
	      (test-configf (conc test-path "/testconfig"))
	      (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
	      ;; read configs with tricks turned off (i.e. no system)
	      (test-conf    (if testexists (read-config test-configf #f #f)(make-hash-table))))
	 ;; use the cdb:remote-run instead of passing in db
	 (runs:update-test_meta test-name test-conf)))
     test-names)))

;; This could probably be refactored into one complex query ...
(define (runs:rollup-run keys runname user keyvals)
  (debug:print 4 "runs:rollup-run, keys: " keys " :runname " runname " user: " user)
  (let* ((db              #f)
	 (new-run-id      (cdb:remote-run db:register-run #f keyvals runname "new" "n/a" user))
	 (prev-tests      (cdb:remote-run test:get-matching-previous-test-run-records db new-run-id "%" "%"))
	 (curr-tests      (mt:get-tests-for-run new-run-id "%/%" '() '()))
	 (curr-tests-hash (make-hash-table)))
    (cdb:remote-run db:update-run-event_time db new-run-id)
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
	      (test-steps    (cdb:remote-run db:get-steps-for-test db (db:test-get-id testdat)))
	      (new-test-record #f))
	 ;; replace these with insert ... select
	 (apply sqlite3:execute 
		db 
		(conc "INSERT OR REPLACE INTO tests (run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment) "
		      "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?);")
		new-run-id (cddr (vector->list testdat)))
	 (set! new-testdat (car (mt:get-tests-for-run new-run-id (conc testname "/" item-path) '() '())))
	 (hash-table-set! curr-tests-hash full-name new-testdat) ;; this could be confusing, which record should go into the lookup table?
	 ;; Now duplicate the test steps
	 (debug:print 4 "Copying records in test_steps from test_id=" (db:test-get-id testdat) " to " (db:test-get-id new-testdat))
	 (cdb:remote-run 
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
	 
     
