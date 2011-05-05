;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "common.scm")
(define megatest-version 1.01)

(define help (conc "
Megatest, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2011

Usage: megatest [options]
  -h                      : this help

Process and test running
  -runall                 : run all tests that are not state COMPLETED and status PASS, 
                            CHECK or KILLED
  -runtests tst1,tst2 ... : run tests

Run status updates (these require that you are in a test directory
                    and you have sourced the \"megatest.csh\" or
                    \"megatest.sh\" file.)
  -step stepname
  -test-status            : set the state and status of a test (use :state and :status)
  -setlog logfname        : set the path/filename to the final log relative to the test
                            directory. may be used with -test-status
  -m comment              : insert a comment for this test

Run data
  :runname                : required, name for this particular test run
  :state                  : required if updating step state; e.g. start, end, completed
  :status                 : required if updating step status; e.g. pass, fail, n/a

Queries
  -list-runs patt         : list runs matching pattern \"patt\", % is the wildcard
  -testpatt patt          : in list-runs show only these tests, % is the wildcard
  -itempatt patt          : in list-runs show only tests with items that match patt
  -showkeys               : show the keys used in this megatest setup

Misc 
  -force                  : override some checks
  -xterm                  : start an xterm instead of launching the test

Helpers
  -runstep stepname  ...  : take leftover params as comand and execute as stepname
                            log will be in stepname.log
  -logpro file            : with -exec apply logpro file to stepname.log, creates
                            stepname.html and sets log to same

Called as " (string-intersperse (argv) " ")))

;;  -gui                    : start a gui interface
;;  -config fname           : override the runconfig file with fname

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-runtests"  ;; run a specific test
			"-config"    ;; override the config file name
			"-execute"   ;; run the command encoded in the base64 parameter
			"-step"
			":runname"   
			":item"
			":runname"   
			":state"  
			":status"
			"-list-runs"
			"-testpatt" 
			"-itempatt"
			"-setlog"
			"-runstep"
			"-logpro"
			"-remove-run"
			) 
		 (list  "-h"
		        "-force"
		        "-xterm"
		        "-showkeys"
		        "-test-status"
		        "-gui"
			"-runall"    ;; run all tests
                        
		       )
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

(include "keys.scm")
(include "items.scm")
(include "db.scm")
(include "configf.scm")
(include "process.scm")
(include "launch.scm")
(include "runs.scm")
;; (include "gui.scm")

(define *didsomething* #f)

;;======================================================================
;; Query runs
;;======================================================================

(if (args:get-arg "-list-runs")
    (let* ((db       (begin
		       (setup-for-run)
		       (open-db)))
	   (runpatt  (args:get-arg "-list-runs"))
	   (testpatt (args:get-arg "-testpatt"))
	   (itempatt (args:get-arg "-itempatt"))
	   (runsdat  (db-get-runs db runpatt))
	   (runs     (db:get-rows runsdat))
	   (header   (db:get-header runsdat))
	   (keys     (db-get-keys db))
	   (keynames (map key:get-fieldname keys)))
      ;; Each run
      (for-each 
       (lambda (run)
	 (print "Run: "
		(string-intersperse (map (lambda (x)
					   (db-get-value-by-header run header x))
					 keynames) "/")
		"/"
		(db-get-value-by-header run header "runname"))
	 (let ((run-id (db-get-value-by-header run header "id")))
	   (let ((tests (db-get-tests-for-run db run-id testpatt itempatt)))
	     ;; Each test
	     (for-each 
	      (lambda (test)
		(format #t
			"  Test: ~25a State: ~15a Status: ~15a Runtime: ~5@as Time: ~22a Host: ~10a\n"
			(conc (db:test-get-testname test)
			      (if (equal? (db:test-get-item-path test) "")
				  "" 
				  (conc "(" (db:test-get-item-path test) ")")))
			(db:test-get-state test)
			(db:test-get-status test)
			(db:test-get-run_duration test)
			(db:test-get-event_time test)
			(db:test-get-host test))
 		(if (not (or (equal? (db:test-get-status test) "PASS")
			     (equal? (db:test-get-state test) "NOT_STARTED")))
		    (begin
		      (print "         cpuload:  " (db:test-get-cpuload test)
			     "\n         diskfree: " (db:test-get-diskfree test)
			     "\n         uname:    " (db:test-get-uname test)
			     "\n         rundir:   " (db:test-get-rundir test)
			     )
		      ;; Each test
		      (let ((steps (db-get-test-steps-for-run db (db:test-get-id test))))
			(for-each 
			 (lambda (step)
			   (format #t 
				   "    Step: ~20a State: ~10a Status: ~10a Time ~22a\n"
				   (db:step-get-stepname step)
				   (db:step-get-state step)
				   (db:step-get-status step)
				   (db:step-get-event_time step)))
			;;    (print "    Step: " (db:step-get-stepname step)
			;; 	  " " (db:step-get-state step)
			;; 	  " " (db:step-get-status step)
			;; 	  " " (db:step-get-event_time step)))
			 steps)))))
		tests))))
       runs)
      (set! *didsomething* #t)
      ))

;;======================================================================
;; full run
;;======================================================================

;; get lock in db for full run for this directory
;; for all tests with deps
;;   walk tree of tests to find head tasks
;;   add head tasks to task queue
;;   add dependant tasks to task queue 
;;   add remaining tasks to task queue
;; for each task in task queue
;;   if have adequate resources
;;     launch task
;;   else
;;     put task in deferred queue
;; if still ok to run tasks
;;   process deferred tasks per above steps

;; run all tests are are Not COMPLETED and PASS or CHECK
(if (args:get-arg "-runall")
    (if (not (args:get-arg ":runname"))
	(begin
	  (print "ERROR: Missing required parameter for -runtests, you must specify the run name with :runname runname")
	  (exit 2))
	(let* ((db      (if (setup-for-run)
			    (open-db)
			    (begin
			      (print "Failed to setup, exiting")
			      (exit 1)))))
	  (if (not (car *configinfo*))
	      (begin
		(print "ERROR: Attempted to run a test but run area config file not found")
		(exit 1))
	      ;; put test parameters into convenient variables
	      (let* ((test-names (get-all-legal-tests))) ;; "PROD" is ignored for now
		(print "INFO: Attempting to start the following tests...")
		(print "     " (string-intersperse test-names ","))
		(run-tests db test-names)))
	  (run-waiting-tests db)
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

;;======================================================================
;; run one test
;;======================================================================

;; 1. find the config file
;; 2. change to the test directory
;; 3. update the db with "test started" status, set running host
;; 4. process launch the test
;;    - monitor the process, update stats in the db every 2^n minutes
;; 5. as the test proceeds internally it calls megatest as each step is
;;    started and completed
;;    - step started, timestamp
;;    - step completed, exit status, timestamp
;; 6. test phone home
;;    - if test run time > allowed run time then kill job
;;    - if cannot access db > allowed disconnect time then kill job

(define (runtests)
  (if (not (args:get-arg ":runname"))
      (begin
	(print "ERROR: Missing required parameter for -runtests, you must specify the run name with :runname runname")
	(exit 2))
      (let ((db #f))
	(if (not (setup-for-run))
	    (begin 
	      (print "Failed to setup, exiting")
	      (exit 1)))
	(set! db (open-db))
	(if (not (car *configinfo*))
	    (begin
	      (print "ERROR: Attempted to run a test but run area config file not found")
	      (exit 1))
	    ;; put test parameters into convenient variables
	    (let* ((test-names   (string-split (args:get-arg "-runtests") ",")))
	      (run-tests db test-names)))
	;; run-waiting-tests db)
	(sqlite3:finalize! db)
	(run-waiting-tests #f)
	(set! *didsomething* #t))))
	  
(if (args:get-arg "-runtests")
    (runtests))

;;======================================================================
;; Remove old run(s)
;;======================================================================

(define (remove-runs)
  (if (not (args:get-arg ":runname"))
      (begin
	(print "ERROR: Missing required parameter for -remove-run, you must specify the run name with :runname runname")
	(exit 2))
      (let ((db #f))
	(if (not (setup-for-run))
	    (begin 
	      (print "Failed to setup, exiting")
	      (exit 1)))
	(set! db (open-db))
	(if (not (car *configinfo*))
	    (begin
	      (print "ERROR: Attempted to remove a test but run area config file not found")
	      (exit 1))
	    ;; put test parameters into convenient variables
	    (let* ((test-names   (string-split (args:get-arg "-remove-tests") ",")))
	      (run-tests db test-names)))
	;; run-waiting-tests db)
	(sqlite3:finalize! db)
	(run-waiting-tests #f)
	(set! *didsomething* #t))))
	  
(if (args:get-arg "-runtests")
    (runtests))

;;======================================================================
;; execute the test
;;    - gets called on remote host
;;    - receives info from the -execute param
;;    - passes info to steps via MT_CMDINFO env var (future is to use a dot file)
;;    - gathers host info and 
;;======================================================================

(if (args:get-arg "-execute")
    (let* ((cmdinfo   (read (open-input-string (base64:base64-decode (args:get-arg "-execute"))))))
      (setenv "MT_CMDINFO" (args:get-arg "-execute"))
      (if (list? cmdinfo) ;; ((testpath /tmp/mrwellan/jazzmind/src/example_run/tests/sqlitespeed) (test-name sqlitespeed) (runscript runscript.rb) (db-host localhost) (run-id 1))
	  (let* ((testpath  (assoc/default 'testpath  cmdinfo))
		 (work-area (assoc/default 'work-area cmdinfo))
		 (test-name (assoc/default 'test-name cmdinfo))
		 (runscript (assoc/default 'runscript cmdinfo))
		 (db-host   (assoc/default 'db-host   cmdinfo))
		 (run-id    (assoc/default 'run-id    cmdinfo))
		 (itemdat   (assoc/default 'itemdat   cmdinfo))
		 (mt-bindir-path (assoc/default 'mt-bindir-path cmdinfo))
		 (fullrunscript (conc testpath "/" runscript))
		 (db        #f))
	    (print "Exectuing " test-name " on " (get-host-name))
	    (change-directory testpath)
	    (setenv "MT_TEST_RUN_DIR" testpath)
	    (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path))
	    (if (not (setup-for-run))
		(begin
		  (print "Failed to setup, exiting") 
		  (exit 1)))
	    ;; now can find our db
	    (set! db (open-db))
	    (change-directory work-area) 
	    (let ((runconfigf (conc  *toppath* "/runconfigs.config")))
	      (if (file-exists? runconfigf)
		  (setup-env-defaults db runconfigf run-id)
		  (print "WARNING: You do not have a run config file: " runconfigf)))
	    (set-megatest-env-vars db run-id)
	    (set-item-env-vars itemdat)
            (save-environment-as-files "megatest")
	    (test-set-meta-info db run-id test-name itemdat)
	    (test-set-status! db run-id test-name "REMOTEHOSTSTART" "n/a" itemdat (args:get-arg "-m"))
	    (if (args:get-arg "-xterm")
		(set! fullrunscript "xterm")
                (if (not (file-execute-access? fullrunscript))
                    (system (conc "chmod ug+x " fullrunscript))))
	    ;; We are about to actually kick off the test
	    ;; so this is a good place to remove the records for 
	    ;; any previous runs
	    ;; (db:test-remove-steps db run-id testname itemdat)
	    
	    ;; from here on out we will open and close the db
	    ;; on every access to reduce the probablitiy of 
	    ;; contention or stuck access on nfs.
	    (sqlite3:finalize! db)

	    (let* ((m            (make-mutex))
		   (kill-job?    #f)
		   (exit-info    (make-vector 3))
		   (runit        (lambda ()
				   (let-values
				    (((pid exit-status exit-code)
				      (run-n-wait fullrunscript)))
				    (mutex-lock! m)
				    (vector-set! exit-info 0 pid)
				    (vector-set! exit-info 1 exit-status)
				    (vector-set! exit-info 2 exit-code)
				    (mutex-unlock! m))))
		   (monitorjob   (lambda ()
				   (let* ((start-seconds (current-seconds))
					  (calc-minutes  (lambda ()
							   (inexact->exact 
							    (round 
							     (- 
							      (current-seconds) 
							      start-seconds))))))
				     (let loop ((minutes   (calc-minutes)))
				       (let ((db    (open-db)))
					 (set! kill-job? (test-get-kill-request db run-id test-name itemdat))
					 (test-update-meta-info db run-id test-name itemdat minutes)
					 (if kill-job? (process-signal (vector-ref exit-info 0) signal/term))
					 (sqlite3:finalize! db)
					 (thread-sleep! (+ 8 (random 4))) ;; add some jitter to the call home time to spread out the db accesses
					 (loop (calc-minutes)))))))
		   (th1          (make-thread monitorjob))
		   (th2          (make-thread runit)))
	      (thread-start! th1)
	      (thread-start! th2)
	      (thread-join! th2)
	      (mutex-lock! m)
	      (set! db (open-db))
	      (let* ((testinfo (runs:get-test-info db run-id test-name (item-list->path itemdat))))
		(if (not (equal? (db:test-get-state testinfo) "COMPLETED"))
		    (begin
		      (print "Test NOT logged as COMPLETED, (state=" (db:test-get-state testinfo) "), updating result")
		      (test-set-status! db run-id test-name
					(if kill-job? "KILLED" "COMPLETED")
					(if (vector-ref exit-info 1) ;; look at the exit-status
					    (if (eq? (vector-ref exit-info 2) 0)
						"PASS"
						"FAIL")
					    "FAIL") itemdat (args:get-arg "-m")))))
	      (mutex-unlock! m)
	      ;; (exec-results (cmd-run->list fullrunscript)) ;;  (list ">" (conc test-name "-run.log"))))
	      ;; (success      exec-results)) ;; (eq? (cadr exec-results) 0)))
	      (print "Output from running " fullrunscript ", pid " (vector-ref exit-info 0) " in work area " 
		     work-area ":\n====\n exit code " (vector-ref exit-info 2) "\n" "====\n")
	      (sqlite3:finalize! db)
	      (if (not (vector-ref exit-info 1))
		  (exit 4)))))
      (set! *didsomething* #t)))

(if (args:get-arg "-step")
    (if (not (getenv "MT_CMDINFO"))
	(begin
	  (print "ERROR: MT_CMDINFO env var not set, -step must be called *inside* a megatest invoked environment!")
	  (exit 5))
	(let* ((step      (args:get-arg "-step"))
	       (cmdinfo   (read (open-input-string (base64:base64-decode (getenv "MT_CMDINFO")))))
	       (testpath  (assoc/default 'testpath  cmdinfo))
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (db-host   (assoc/default 'db-host   cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (db        #f)
	       (state    (args:get-arg ":state"))
	       (status   (args:get-arg ":status")))
	  (change-directory testpath)
	  (if (not (setup-for-run))
	      (begin
		(print "Failed to setup, exiting")
		(exit 1)))
	  (set! db (open-db))
	  (if (and state status)
	      (teststep-set-status! db run-id test-name step state status itemdat)
	      (begin
		(print "ERROR: You must specify :state and :status with every call to -step")
		(exit 6)))
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

(if (or (args:get-arg "-setlog")       ;; since setting up is so costly lets piggyback on -test-status
	(args:get-arg "-test-status")
	(args:get-arg "-runstep"))
    (if (not (getenv "MT_CMDINFO"))
	(begin
	  (print "ERROR: MT_CMDINFO env var not set, commands -test-status, -runstep and -setlog must be called *inside* a megatest environment!")
	  (exit 5))
	(let* ((cmdinfo   (read (open-input-string (base64:base64-decode (getenv "MT_CMDINFO")))))
	       (testpath  (assoc/default 'testpath  cmdinfo))
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (db-host   (assoc/default 'db-host   cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (db        #f)
	       (state     (args:get-arg ":state"))
	       (status    (args:get-arg ":status")))
	  (change-directory testpath)
	  (if (not (setup-for-run))
	      (begin
		(print "Failed to setup, exiting")
		(exit 1)))
	  (set! db (open-db))
	  (if (args:get-arg "-setlog")
	      (test-set-log! db run-id test-name itemdat (args:get-arg "-setlog")))
	  (if (args:get-arg "-test-status")
	      (test-set-status! db run-id test-name state status itemdat (args:get-arg "-m"))
	      (if (and state status)
		  (if (not (args:get-arg "-setlog"))
		      (begin
			(print "ERROR: You must specify :state and :status with every call to -test-status\n" help)
			(sqlite3:finalize! db)
			(exit 6)))))
	  (if (args:get-arg "-run-step")
	      (if (null? remargs)
		  (begin
		    (print "ERROR: nothing specified to run!")
		    (sqlite3:finalize! db)
		    (exit 6))
		  (let* ((logprofile (args:get-arg "-logpro"))
			 (cmd        (if (null? remargs) #f (car remargs)))
			 (params     (if cmd (cdr remargs) #f))
			 (exitstat   #f))
		    ;; mark the start of the test
		    (test-set-status! db run-id test-name "start" "n/a" itemdat (args:get-arg "-m"))
		    ;; close the db
		    (sqlite3:finalize! db)
		    ;; run the test step
		    (set! exitstat (process-run cmd params))
		    ;; re-open the db
		    (set! db (open-db)) 
		    ;; run logpro if applicable
		    (if logpro
			(let ((logfile (conc test-name ".html")))
			  (set! exitstat (process-run "logpro" logpro logfile))
			  (test-set-log! db run-id test-name itemdat logfile)))
		    (test-set-status! db run-id test-name "end" exitstat itemdat (args:get-arg "-m"))
		    (sqlite3:finalize! db)
		    (exit exitstat)
		    ;; open the db
		;; mark the end of the test
		)))
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

(if (args:get-arg "-showkeys")
    (let ((db #f)
	  (keys #f))
      (if (not (setup-for-run))
	  (begin
	    (print "Failed to setup, exiting")
	    (exit 1)))
      (set! db (open-db))
      (set! keys (db-get-keys db))
      (print "Keys: " (string-intersperse (map key:get-fieldname keys) ", "))
      (sqlite3:finalize! db)
      (set! *didsomething* #t)))

(if (args:get-arg "-gui")
    (begin
      (print "Look at the dashboard for now")
      ;; (megatest-gui)
      (set! *didsomething* #t)))

(if (not *didsomething*)
    (print help))
