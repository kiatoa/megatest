;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (include "common.scm")
;; (include "megatest-version.scm")

(use sqlite3 srfi-1 posix regex regex-case srfi-69 base64 format)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
(declare (uses runs))
(declare (uses launch))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")

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
  -target key1/key2/...   : run for key1, key2, etc.
  -reqtarg key1/key2/...  : run for key1, key2, etc. but key1/key2 must be in runconfig
  :runname                : required, name for this particular test run
  :state                  : required if updating step state; e.g. start, end, completed
  :status                 : required if updating step status; e.g. pass, fail, n/a

Values and record errors and warnings
  -set-values             : update or set values in the megatest db 
  :category               : set the category field (optional)
  :variable               : set the variable name (optional)
  :value                  : value measured (required)
  :expected               : value expected (required)
  :tol                    : |value-expect| <= tol (required, can be <, >, >=, <= or number)
  :units                  : name of the units for value, expected_value etc. (optional)

Arbitrary test data loading
  -load-test-data         : read test specific data for storage in the test_data table
                            from standard in. Each line is comma delimited with four
                            fields category,variable,value,comment

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
  -rollup                 : fill run (set by :runname)  with latest test(s) from
                            prior runs with same keys
  -update-meta            : update the tests metadata for all tests
  -env2file fname         : write the environment to fname.csh and fname.sh

Spreadsheet generation
  -extract-ods            : extract an open document spreadsheet from the database
  -pathmod path           : insert path, i.e. path/runame/itempath/logfile.html
                            will clear the field if no rundir/testname/itempath/logfile
                            if it contains forward slashes the path will be converted
                            to windows style

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
			"-target"
			"-reqtarg"
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
			;; values and messages
			":category"
			":variable"
			":value"
			":expected"
			":tol"
			":units"
			;; misc
			"-extract-ods"
			"-pathmod"
			"-env2file"
			"-debug" ;; for *verbosity* > 2
			) 
		 (list  "-h"
		        "-force"
		        "-xterm"
		        "-showkeys"
		        "-test-status"
			"-set-values"
			"-load-test-data"
			"-summarize-items"
		        "-gui"
			"-runall"    ;; run all tests
			"-remove-runs"
			"-keepgoing"
			"-usequeue"
			"-rebuild-db"
			"-rollup"
			"-update-meta"
			"-v" ;; verbose 2, more than normal (normal is 1)
			"-q" ;; quiet 0, errors/warnings only
		       )
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

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
;; Misc general calls
;;======================================================================

(if (args:get-arg "-env2file")
    (begin
      (save-environment-as-files (args:get-arg "-env2file"))
      (set! *didsomething* #t)))

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
	   (runsdat  (db:get-runs db runpatt #f #f '()))
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
	   (let ((tests (db-get-tests-for-run db run-id testpatt itempatt '() '())))
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
		      (let ((steps (db:get-steps-for-test db (db:test-get-id test))))
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
       (runs:rollup-run db keys))))

;;======================================================================
;; Extract a spreadsheet from the runs database
;;======================================================================

(if (args:get-arg "-extract-ods")
    (general-run-call
     "-extract-ods"
     "Make ods spreadsheet"
     (lambda (db keys keynames keyvallst)
       (let ((outputfile (args:get-arg "-extract-ods"))
	     (runspatt   (args:get-arg ":runname"))
	     (pathmod    (args:get-arg "-pathmod"))
	     (keyvalalist (keys->alist keys "%")))
	 (db:extract-ods-file db outputfile keyvalalist (if runspatt runspatt "%") pathmod)))))

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

(if (args:get-arg "-runtests")
  (general-run-call 
   "-runtests" 
   "run a test" 
   (lambda (db keys keynames keyvallst)
     (let ((test-names (string-split (args:get-arg "-runtests") ",")))
       (run-tests db test-names)))))

;;======================================================================
;; execute the test
;;    - gets called on remote host
;;    - receives info from the -execute param
;;    - passes info to steps via MT_CMDINFO env var (future is to use a dot file)
;;    - gathers host info and 
;;======================================================================

(if (args:get-arg "-execute")
    (begin
      (launch:execute (args:get-arg "-execute"))
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
	       (status   (args:get-arg ":status"))
	       (logfile  (args:get-arg "-setlog")))
	  (change-directory testpath)
	  (if (not (setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, exiting")
		(exit 1)))
	  (set! db (open-db))
	  (if (and state status)
	      (teststep-set-status! db run-id test-name step state status itemdat (args:get-arg "-m") logfile)
	      (begin
		(debug:print 0 "ERROR: You must specify :state and :status with every call to -step")
		(exit 6)))
	  (sqlite3:finalize! db)
	  (set! *didsomething* #t))))

(if (or (args:get-arg "-setlog")       ;; since setting up is so costly lets piggyback on -test-status
	(args:get-arg "-set-toplog")
	(args:get-arg "-test-status")
	(args:get-arg "-set-values")
	(args:get-arg "-load-test-data")
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
	  (if (args:get-arg "-load-test-data")
	      (db:load-test-data db run-id test-name itemdat))
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
		    (teststep-set-status! db run-id test-name stepname "start" "n/a" itemdat (args:get-arg "-m") logfile)
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
		    (teststep-set-status! db run-id test-name stepname "end" exitstat itemdat (args:get-arg "-m") logfile)
		    (sqlite3:finalize! db)
		    (if (not (eq? exitstat 0))
			(exit 254)) ;; (exit exitstat) doesn't work?!?
		  ;; open the db
		  ;; mark the end of the test
		  )))
	  (if (or (args:get-arg "-test-status")
		  (args:get-arg "-set-values"))
	      (let ((newstatus (cond
				((number? status)       (if (equal? status 0) "PASS" "FAIL"))
				((and (string? status)
				      (string->number status))(if (equal? (string->number status) 0) "PASS" "FAIL"))
				(else status)))
		    ;; transfer relevant keys into a hash to be passed to test-set-status!
		    ;; could use an assoc list I guess. 
		    (otherdata (let ((res (make-hash-table)))
				 (for-each (lambda (key)
					     (if (args:get-arg key)
						 (hash-table-set! res key (args:get-arg key))))
					   (list ":value" ":tol" ":expected" ":first_err" ":first_warn" ":units" ":category" ":variable"))
				 res)))
		(if (and (args:get-arg "-test-status")
			 (or (not state)
			     (not status)))
		    (begin
		      (debug:print 0 "ERROR: You must specify :state and :status with every call to -test-status\n" help)
		      (sqlite3:finalize! db)
		      (exit 6)))
		(test-set-status! db run-id test-name state newstatus itemdat (args:get-arg "-m") otherdata)))
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

;;======================================================================
;; Update the tests meta data from the testconfig files
;;

(if (args:get-arg "-update-meta")
    (begin
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
	    (exit 1)))
      ;; now can find our db
      (set! db (open-db))
      (runs:update-all-test_meta db)
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
