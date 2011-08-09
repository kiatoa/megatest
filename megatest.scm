;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(include "common.scm")
(include "megatest-version.scm")

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
  -set-toplog logfname    : set the overall log for a suite of sub-tests
  -summarize-items        : for an itemized test create a summary html 
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
  -remove-runs            : remove the data for a run, requires all fields be specified
                            and :runname ,-testpatt and -itempatt
                            and -testpatt
  -keepgoing              : continue running until no jobs are \"LAUNCHED\" or
                            \"NOT_STARTED\"
  -rerun FAIL,WARN...     : re-run if called on a test that previously ran (nullified
                            if -keepgoing is also specified)
  -rebuild-db             : bring the database schema up to date
  -rollup N               : fill run (set by :runname)  with latest test(s) from
                            past N days, requires keys
  -rename-run <runb>      : rename run (set by :runname) to <runb>, requires keys

Helpers
  -runstep stepname  ...  : take remaining params as comand and execute as stepname
                            log will be in stepname.log. Best to put command in quotes
  -logpro file            : with -exec apply logpro file to stepname.log, creates
                            stepname.html and sets log to same
                            If using make use stepname_logpro.log as your target

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
			"-set-toplog"
			"-runstep"
			"-logpro"
			"-m"
			"-rerun"
			"-days"
			"-rename-run"
			"-to"
			"-debug" ;; for *verbosity* > 2
			) 
		 (list  "-h"
		        "-force"
		        "-xterm"
		        "-showkeys"
		        "-test-status"
			"-summarize-items"
		        "-gui"
			"-runall"    ;; run all tests
			"-remove-runs"
			"-keepgoing"
			"-usequeue"
			"-rebuild-db"
			"-rollup"
			"-v" ;; verbose 2, more than normal (normal is 1)
			"-q" ;; quiet 0, errors/warnings only
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
(include "runconfig.scm")

(define *didsomething* #f)

;;======================================================================
;; Misc setup stuff
;;======================================================================

(set! *verbosity* (cond
		   ((args:get-arg "-debug")(string->number (args:get-arg "-debug")))
		   ((args:get-arg "-v")    2)
		   ((args:get-arg "-q")    0)
		   (else                   1)))

;;======================================================================
;; Remove old run(s)
;;======================================================================

;; since several actions can be specified on the command line the removal
;; is done first
(define (remove-runs)
  (cond
   ((not (args:get-arg ":runname"))
    (debug:print 0 "ERROR: Missing required parameter for -remove-runs, you must specify the run name pattern with :runname patt")
    (exit 2))
   ((not (args:get-arg "-testpatt"))
    (debug:print 0 "ERROR: Missing required parameter for -remove-runs, you must specify the test pattern with -testpatt")
    (exit 3))
   ((not (args:get-arg "-itempatt"))
    (print "ERROR: Missing required parameter for -remove-runs, you must specify the items with -itempatt")
    (exit 4))
   ((let ((db #f))
      (if (not (setup-for-run))
	  (begin 
	    (debug:print 0 print "Failed to setup, exiting")
	    (exit 1)))
      (set! db (open-db))
      (if (not (car *configinfo*))
	  (begin
	    (debug:print 0 "ERROR: Attempted to remove test(s) but run area config file not found")
	    (exit 1))
	  ;; put test parameters into convenient variables
	  (runs:remove-runs db
			    (args:get-arg ":runname")
			    (args:get-arg "-testpatt")
			    (args:get-arg "-itempatt")))
      (sqlite3:finalize! db)
      (set! *didsomething* #t)))))
	  
(if (args:get-arg "-remove-runs")
    (remove-runs))

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
	 (debug:print 2 "Run: "
		(string-intersperse (map (lambda (x)
					   (db:get-value-by-header run header x))
					 keynames) "/")
		"/"
		(db:get-value-by-header run header "runname"))
	 (let ((run-id (db:get-value-by-header run header "id")))
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
			     (equal? (db:test-get-status test) "WARN")
			     (equal? (db:test-get-state test)  "NOT_STARTED")))
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
    (general-run-call 
     "-runall"
     "run all tests"
     (lambda (db keys keynames keyvallst)
       (let* ((test-names (get-all-legal-tests))) ;; "PROD" is ignored for now
	 (debug:print 1 "INFO: Attempting to start the following tests...")
	 (debug:print 1 "     " (string-intersperse test-names ","))
	 (run-tests db test-names)))))

;;======================================================================
;; Rollup into a run
;;======================================================================
(if (args:get-arg "-rollup")
    (general-run-call 
     "-rollup" 
     "rollup tests" 
     (lambda (db keys keynames keyvallst)
       (let ((n (args:get-arg "-rollup")))
	 (runs:rollup db keys keynames keyvallst n)))))

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
  (general-run-call 
   "-runtests" 
   "run a test" 
   (lambda (db keys keynames keyvallst)
     (let ((test-names (string-split (args:get-arg "-runtests") ",")))
       (run-tests db test-names)))))

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
		 (env-ovrd  (assoc/default 'env-ovrd  cmdinfo))
		 (runname   (assoc/default 'runname   cmdinfo))
		 (megatest  (assoc/default 'megatest  cmdinfo))
		 (mt-bindir-path (assoc/default 'mt-bindir-path cmdinfo))
		 (fullrunscript (conc testpath "/" runscript))
		 (db        #f))
	    (debug:print 2 "Exectuing " test-name " on " (get-host-name))
	    (change-directory testpath)
	    (setenv "MT_TEST_RUN_DIR" work-area)
	    (setenv "MT_TEST_NAME" test-name)
	    (setenv "MT_ITEM_INFO" (conc itemdat))
	    (setenv "MT_RUNNAME"   runname)
	    (setenv "MT_MEGATEST"  megatest)
	    (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path))
	    
	    (if (not (setup-for-run))
		(begin
		  (debug:print 0 "Failed to setup, exiting") 
		  (exit 1)))
	    ;; now can find our db
	    (set! db (open-db))
	    (change-directory work-area) 
	    (set-run-config-vars db run-id)
            ;; environment overrides are done *before* the remaining critical envars.
            (alist->env-vars env-ovrd)
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
		   (job-thread   #f)
		   (runit        (lambda ()
				   ;; (let-values
				   ;;  (((pid exit-status exit-code)
				   ;;    (run-n-wait fullrunscript)))
				   (let ((pid (process-run fullrunscript)))
				     (let loop ((i 0))
				       (let-values
					(((pid-val exit-status exit-code) (process-wait pid #t)))
					(mutex-lock! m)
					(vector-set! exit-info 0 pid)
					(vector-set! exit-info 1 exit-status)
					(vector-set! exit-info 2 exit-code)
					(mutex-unlock! m)
					(if (eq? pid-val 0)
					    (begin
					      (thread-sleep! 2)
					      (loop (+ i 1)))
					    ))))))
		   (monitorjob   (lambda ()
				   (let* ((start-seconds (current-seconds))
					  (calc-minutes  (lambda ()
							   (inexact->exact 
							    (round 
							     (- 
							      (current-seconds) 
							      start-seconds)))))
					  (kill-tries 0))
				     (let loop ((minutes   (calc-minutes)))
				       (let ((db    (open-db)))
					 (set! kill-job? (test-get-kill-request db run-id test-name itemdat))
					 (test-update-meta-info db run-id test-name itemdat minutes)
					 (if kill-job? 
					     (begin
					       (mutex-lock! m)
					       (let* ((pid (vector-ref exit-info 0)))
						 (if (number? pid)
						     (begin
						       (debug:print 0 "WARNING: Request received to kill job (attempt # " kill-tries ")")
						       ;;(cond
						       ;;((>   kill-tries 0) ; 2)
						       (let ((processes (cmd-run->list (conc "pgrep -l -P " pid))))
							 (for-each 
							  (lambda (p)
							    (let* ((parts  (string-split p))
								   (p-id   (if (> (length parts) 0)
									       (string->number (car parts))
									       #f)))
							      (if p-id
								  (begin
								    (debug:print 0 "Killing " (cadr parts) "; kill -9  " p-id)
								    (system (conc "kill -9 " p-id))))))
							  (car processes))
							 (system (conc "kill -9 " pid))))
						     ;;(let* ((ppid (process-group-id pid))
						     ;;       (kcmd (conc "pkill -9 -g " ppid)))
						     ;;  ;; (process-signal pid signal/term)
						     ;;  ;; (process-signal pid signal/kill)
						     ;;  (debug:print 0 "Attempting to kill pid " pid " and children in process group " ppid " with command:\n    " kcmd)
						     ;;  (debug:print 0 "Children:")
						     ;;  (system (conc "pgrep -g -l " ppid))
						     ;;  (system kcmd)
						     ;;  (sleep 1) ;; give it a rest
						     ;;  (test-set-status! db run-id test-name "KILLED"  "FAIL"
						     ;;       	     itemdat (args:get-arg "-m"))
						     ;;  (sqlite3:finalize! db)
						     ;;  (exit 1)))))
						     (begin
						       (debug:print 0 "WARNING: Request received to kill job but problem with process, attempting to kill manager process")
						       (test-set-status! db run-id test-name "KILLED"  "FAIL"
									 itemdat (args:get-arg "-m"))
						       (sqlite3:finalize! db)
						       (exit 1))))
					       ;;     (thread-terminate! job-thread)))
					       (set! kill-tries (+ 1 kill-tries))
					       (mutex-unlock! m)))
					 ;; (handle-exceptions
					       ;;  exn
					       ;;  (begin
					       ;;    (debug:print 0 "ERROR: Problem killing process " (vector-ref exit-info 0))
					       ;;    (abort exn))
					       ;;  (let* ((pid   (vector-ref exit-info 0))
					       ;;         ;; (pgid  (process-group-id pid))
					       ;;         ;; (cmd  (conc "pkill -9 -P " pgid))
					       ;;         )
					       ;;    ;; (debug:print 0 "Running \"" cmd "\"")
					       ;;    ;; (system cmd)
					       ;;    (debug:print 0 "Running \"kill -9 " pid "\"")
					       ;;    (system (conc "kill -9 " pid))
					       ;;    ;; (process-signal (vector-ref exit-info 0) signal/kill)
					       ;;    ))))
					 (sqlite3:finalize! db)
					 (thread-sleep! (+ 8 (random 4))) ;; add some jitter to the call home time to spread out the db accesses
					 (loop (calc-minutes)))))))
		   (th1          (make-thread monitorjob))
		   (th2          (make-thread runit)))
	      (set! job-thread th2)
	      (thread-start! th1)
	      (thread-start! th2)
	      (thread-join! th2)
	      (mutex-lock! m)
	      (set! db (open-db))
	      (let* ((item-path (item-list->path itemdat))
		     (testinfo  (db:get-test-info db run-id test-name item-path)))
		(if (not (equal? (db:test-get-state testinfo) "COMPLETED"))
		    (begin
		      (debug:print 2 "Test NOT logged as COMPLETED, (state=" (db:test-get-state testinfo) "), updating result")
		      (test-set-status! db run-id test-name
					(if kill-job? "KILLED" "COMPLETED")
					(if (vector-ref exit-info 1) ;; look at the exit-status
					    (if (and (not kill-job?) 
						     (eq? (vector-ref exit-info 2) 0))
						"PASS"
						"FAIL")
					    "FAIL") itemdat (args:get-arg "-m"))))
		;; for automated creation of the rollup html file this is a good place...
		(if (not (equal? item-path ""))
		   (tests:summarize-items db run-id test-name #f)) ;; don't force - just update if no
		)
	      (mutex-unlock! m)
	      ;; (exec-results (cmd-run->list fullrunscript)) ;;  (list ">" (conc test-name "-run.log"))))
	      ;; (success      exec-results)) ;; (eq? (cadr exec-results) 0)))
	      (debug:print 2 "Output from running " fullrunscript ", pid " (vector-ref exit-info 0) " in work area " 
		     work-area ":\n====\n exit code " (vector-ref exit-info 2) "\n" "====\n")
	      (sqlite3:finalize! db)
	      (if (not (vector-ref exit-info 1))
		  (exit 4)))))
      (set! *didsomething* #t)))

(if (args:get-arg "-step")
    (if (not (getenv "MT_CMDINFO"))
	(begin
	  (debug:print 0 "ERROR: MT_CMDINFO env var not set, -step must be called *inside* a megatest invoked environment!")
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
		(debug:print 0 "Failed to setup, exiting")
		(exit 1)))
	  (set! db (open-db))
	  (if (and state status)
	      (teststep-set-status! db run-id test-name step state status itemdat (args:get-arg "-m"))
	      (begin
		(debug:print 0 "ERROR: You must specify :state and :status with every call to -step")
		(exit 6)))
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

(if (or (args:get-arg "-setlog")       ;; since setting up is so costly lets piggyback on -test-status
	(args:get-arg "-set-toplog")
	(args:get-arg "-test-status")
	(args:get-arg "-runstep")
	(args:get-arg "-summarize-items"))
    (if (not (getenv "MT_CMDINFO"))
	(begin
	  (debug:print 0 "ERROR: MT_CMDINFO env var not set, commands -test-status, -runstep and -setlog must be called *inside* a megatest environment!")
	  (exit 5))
	(let* ((startingdir (current-directory))
	       (cmdinfo   (read (open-input-string (base64:base64-decode (getenv "MT_CMDINFO")))))
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
		(debug:print 0 "Failed to setup, exiting")
		(exit 1)))
	  (set! db (open-db))
	  (if (args:get-arg "-setlog")
	      (test-set-log! db run-id test-name itemdat (args:get-arg "-setlog")))
	  (if (args:get-arg "-set-toplog")
	      (test-set-toplog! db run-id test-name (args:get-arg "-set-toplog")))
	  (if (args:get-arg "-summarize-items")
	      (tests:summarize-items db run-id test-name #t)) ;; do force here
	  (if (args:get-arg "-runstep")
	      (if (null? remargs)
		  (begin
		    (debug:print 0 "ERROR: nothing specified to run!")
		    (sqlite3:finalize! db)
		    (exit 6))
		  (let* ((stepname   (args:get-arg "-runstep"))
			 (logprofile (args:get-arg "-logpro"))
			 (logfile    (conc stepname ".log"))
			 (cmd        (if (null? remargs) #f (car remargs)))
			 (params     (if cmd (cdr remargs) '()))
			 (exitstat   #f)
			 (shell      (last (string-split (get-environment-variable "SHELL") "/")))
			 (redir      (case (string->symbol shell)
				       ((tcsh csh ksh)    ">&")
				       ((zsh bash sh ash) "2>&1 >")))
			 (fullcmd    (conc "(" (string-intersperse 
						(cons cmd params) " ")
					   ") " redir " " logfile)))
		    ;; mark the start of the test
		    (teststep-set-status! db run-id test-name stepname "start" "n/a" itemdat (args:get-arg "-m"))
		    ;; close the db
		    (sqlite3:finalize! db)
		    ;; run the test step
		    (debug:print 2 "INFO: Running \"" fullcmd "\"")
		    (change-directory startingdir)
		    (set! exitstat (system fullcmd)) ;; cmd params))
		    (set! *globalexitstatus* exitstat)
		    (change-directory testpath)
		    ;; re-open the db
		    (set! db (open-db)) 
		    ;; run logpro if applicable ;; (process-run "ls" (list "/foo" "2>&1" "blah.log"))
		    (if logprofile
			(let* ((htmllogfile (conc stepname ".html"))
			       (oldexitstat exitstat)
			       (cmd         (string-intersperse (list "logpro" logprofile htmllogfile "<" logfile ">" (conc stepname "_logpro.log")) " ")))
			  (debug:print 2 "INFO: running \"" cmd "\"")
			  (change-directory startingdir)
			  (set! exitstat (system cmd))
			  (set! *globalexitstatus* exitstat) ;; no necessary
			  (change-directory testpath)
			  (test-set-log! db run-id test-name itemdat htmllogfile)))
		    (teststep-set-status! db run-id test-name stepname "end" exitstat itemdat (args:get-arg "-m"))
		    (sqlite3:finalize! db)
		    (if (not (eq? exitstat 0))
			(exit 254)) ;; (exit exitstat) doesn't work?!?
		  ;; open the db
		  ;; mark the end of the test
		  )))
	  (if (args:get-arg "-test-status")
	      (let ((newstatus (cond
				((number? status)       (if (equal? status 0) "PASS" "FAIL"))
				((string->number status)(if (equal? (string->number status) 0) "PASS" "FAIL"))
				(else status))))
		(test-set-status! db run-id test-name state newstatus itemdat (args:get-arg "-m")))
	      (if (and state status)
		  (if (not (args:get-arg "-setlog"))
		      (begin
			(debug:print 0 "ERROR: You must specify :state and :status with every call to -test-status\n" help)
			(sqlite3:finalize! db)
			(exit 6)))))
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

(if (args:get-arg "-showkeys")
    (let ((db #f)
	  (keys #f))
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting")
	    (exit 1)))
      (set! db (open-db))
      (set! keys (db-get-keys db))
      (debug:print 1 "Keys: " (string-intersperse (map key:get-fieldname keys) ", "))
      (sqlite3:finalize! db)
      (set! *didsomething* #t)))

(if (args:get-arg "-gui")
    (begin
      (debug:print 0 "Look at the dashboard for now")
      ;; (megatest-gui)
      (set! *didsomething* #t)))

;;======================================================================
;; Update the database schema on request
;;======================================================================

(if (args:get-arg "-rebuild-db")
    (begin
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
	    (exit 1)))
      ;; now can find our db
      (set! db (open-db))
      (patch-db db)
      (sqlite3:finalize! db)
      (set! *didsomething* #t)))

(if (not *didsomething*)
    (debug:print 0 help))

(if (not (eq? *globalexitstatus* 0))
    (if (or (args:get-arg "-runtests")(args:get-arg "-runall"))
        (begin
           (debug:print 0 "NOTE: Subprocesses with non-zero exit code detected: " *globalexitstatus*)
           (exit 0))
        (case *globalexitstatus*
         ((0)(exit 0))
         ((1)(exit 1))
         ((2)(exit 2))
         (else (exit 3)))))
