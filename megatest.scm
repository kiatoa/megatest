;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (include "common.scm")
;; (include "megatest-version.scm")

(use sqlite3 srfi-1 posix regex regex-case srfi-69 base64 format readline apropos zmq) ;; (srfi 18) extras)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
(declare (uses runs))
(declare (uses launch))
(declare (uses server))
(declare (uses tests))
(declare (uses genexample))

(define *db* #f) ;; this is only for the repl, do not use in general!!!!

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "megatest-fossil-hash.scm")

(define help (conc "
Megatest, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2012

Usage: megatest [options]
  -h                      : this help
  -version                : print megatest version (currently " megatest-version ")

Launching and managing runs
  -runall                 : run all tests that are not state COMPLETED and status PASS, 
                            CHECK or KILLED
  -runtests tst1,tst2 ... : run tests
  -remove-runs            : remove the data for a run, requires :runname and -testpatt
                            Optionally use :state and :status
  -set-state-status X,Y   : set state to X and status to Y, requires controls per -remove-runs
  -rerun FAIL,WARN...     : force re-run for tests with specificed status(s)
  -rollup                 : (currently disabled) fill run (set by :runname)  with latest test(s)
                            from prior runs with same keys
  -lock                   : lock run specified by target and runname
  -unlock                 : unlock run specified by target and runname

Selectors (e.g. use for -runtests, -remove-runs, -set-state-status, -list-runs etc.)
  -target key1/key2/...   : run for key1, key2, etc.
  -reqtarg key1/key2/...  : run for key1, key2, etc. but key1/key2 must be in runconfig
  -testpatt patt1/patt2,patt3/...  : % is wildcard
  :runname                : required, name for this particular test run
  :state                  : Applies to runs, tests or steps depending on context
  :status                 : Applies to runs, tests or steps depending on context

Test helpers (for use inside tests)
  -step stepname
  -test-status            : set the state and status of a test (use :state and :status)
  -setlog logfname        : set the path/filename to the final log relative to the test
                            directory. may be used with -test-status
  -set-toplog logfname    : set the overall log for a suite of sub-tests
  -summarize-items        : for an itemized test create a summary html 
  -m comment              : insert a comment for this test

Test data capture
  -set-values             : update or set values in the testdata table
  :category               : set the category field (optional)
  :variable               : set the variable name (optional)
  :value                  : value measured (required)
  :expected               : value expected (required)
  :tol                    : |value-expect| <= tol (required, can be <, >, >=, <= or number)
  :units                  : name of the units for value, expected_value etc. (optional)
  -load-test-data         : read test specific data for storage in the test_data table
                            from standard in. Each line is comma delimited with four
                            fields category,variable,value,comment

Queries
  -list-runs patt         : list runs matching pattern \"patt\", % is the wildcard
  -showkeys               : show the keys used in this megatest setup
  -test-files targpatt     : get the most recent test path/file matching targpatt e.g. %/%... 
                            returns list sorted by age ascending, see examples below
  -test-paths             : get the test paths matching target, runname, item and test
                            patterns.

Misc 
  -rebuild-db             : bring the database schema up to date
  -update-meta            : update the tests metadata for all tests
  -env2file fname         : write the environment to fname.csh and fname.sh
  -setvars VAR1=val1,VAR2=val2 : Add environment variables to a run NB// these are
                                 overwritten by values set in config files.
  -server -|hostname      : start the server (reduces contention on megatest.db), use
                            - to automatically figure out hostname
  -listservers            : list the servers 
  -killserver host:port|pid : kill server specified by host:port or pid
  -repl                   : start a repl (useful for extending megatest)

Spreadsheet generation
  -extract-ods fname.ods  : extract an open document spreadsheet from the database
  -pathmod path           : insert path, i.e. path/runame/itempath/logfile.html
                            will clear the field if no rundir/testname/itempath/logfile
                            if it contains forward slashes the path will be converted
                            to windows style
Getting started
  -gen-megatest-area      : create a skeleton megatest area. You will be prompted for paths
  -gen-megatest-test      : create a skeleton megatest test. You will be prompted for info

Examples

# Get test path, use '.' to get a single path or a specific path/file pattern
megatest -test-files 'logs/*.log' -target ubuntu/n%/no% :runname w49% -testpatt test_mt%

Called as " (string-intersperse (argv) " ") "
Built from " megatest-fossil-hash ))

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
			"-server"
			"-killserver"
			"-port"
			"-extract-ods"
			"-pathmod"
			"-env2file"
			"-setvars"
			"-set-state-status"
			"-debug" ;; for *verbosity* > 2
			"-gen-megatest-test"
			"-override-timeout"
			"-test-files"  ;; -test-paths is for listing all
			) 
		 (list  "-h"
			"-version"
		        "-force"
		        "-xterm"
		        "-showkeys"
		        "-test-status"
			"-set-values"
			"-load-test-data"
			"-summarize-items"
		        "-gui"
			;; misc
			"-archive"
			"-repl"
			"-lock"
			"-unlock"
			"-listservers"
			;; queries
			"-test-paths" ;; get path(s) to a test, ordered by youngest first

			"-runall"    ;; run all tests
			"-remove-runs"
			"-usequeue"
			"-rebuild-db"
			"-rollup"
			"-update-meta"
			"-gen-megatest-area"

			"-logging"
			"-v" ;; verbose 2, more than normal (normal is 1)
			"-q" ;; quiet 0, errors/warnings only
		       )
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

(if (args:get-arg "-version")
    (begin
      (print megatest-version)
      (exit)))

(define *didsomething* #f)

;;======================================================================
;; Misc setup stuff
;;======================================================================

(debug:setup)

(if (args:get-arg "-logging")(set! *logging* #t))

(if (debug:debug-mode 3) ;; we are obviously debugging
    (set! open-run-close open-run-close-no-exception-handling))

;; a,b,c % => a/%,b/%,c/%
(define (tack-on-patt srcstr patt)
  (let ((strlst (string-split srcstr ",")))
    (string-intersperse 
     (map (lambda (str)
	    (if (not (substring-index "/" str))
		(conc str "/" patt)
		str))
	  strlst)
	   ",")))

;; to try and not burden Kim too much...
(if (args:get-arg "-itempatt")
    (let ((old-testpatt (args:get-arg "-testpatt")))
      ;; (debug:print 0 "ERROR: parameter \"-itempatt\" has been deprecated. For now I will tweak your -testpatt for you")
      (if (args:get-arg "-testpatt")
	  (hash-table-set! args:arg-hash "-testpatt" (tack-on-patt old-testpatt (args:get-arg "-itempatt"))))
      ;; (debug:print 0 "    old: " old-testpatt ", new: " (args:get-arg "-testpatt"))
      (if (args:get-arg "-runtests")
	  (begin
	    ;; (debug:print 0 "NOTE: Also modifying -runtests")
	    (hash-table-set! args:arg-hash "-runtests" (tack-on-patt (args:get-arg "-runtests")
								     (args:get-arg "-itempatt")))))
      ))

;;======================================================================
;; Misc general calls
;;======================================================================

(if (args:get-arg "-env2file")
    (begin
      (save-environment-as-files (args:get-arg "-env2file"))
      (set! *didsomething* #t)))

;;======================================================================
;; Start the server - can be done in conjunction with -runall or -runtests (one day...)
;;   we start the server if not running else start the client thread
;;======================================================================

(if (args:get-arg "-server")
    (begin
      (debug:print 1 "Launching server...")
      (server:launch)))

(if (or (args:get-arg "-listservers")
	(args:get-arg "-killserver"))
    (let ((tl (setup-for-run)))
      (if tl 
	  (let ((servers (open-run-close tasks:get-all-servers tasks:open-db))
		(fmtstr  "~5a~8a~8a~20a~20a~10a~20a~10a~10a\n")
		(servers-to-kill '()))
	    (format #t fmtstr "Id" "MTver" "Pid" "Host" "Interface" "Port" "Time" "Priority" "State")
	    (format #t fmtstr "==" "=====" "===" "====" "=========" "====" "====" "========" "=====")
	    (for-each 
	     (lambda (server)
	       (let* ((killinfo   (args:get-arg "-killserver"))
		      (khost-port (if killinfo (if (substring-index ":" killinfo)(string-split ":") #f) #f))
		      (kpid       (if killinfo (if (substring-index ":" killinfo) #f (string->number killinfo)) #f))
		      (id         (vector-ref server 0))
		      (pid        (vector-ref server 1))
		      (hostname   (vector-ref server 2))
		      (interface  (vector-ref server 3))
		      (port       (vector-ref server 4))
		      (start-time (vector-ref server 5))
		      (priority   (vector-ref server 6))
		      (state      (vector-ref server 7))
		      (mt-ver     (vector-ref server 8))
		      (status     (open-run-close tasks:server-alive? tasks:open-db #f hostname: hostname port: port))
		      (killed     #f)
		      (zmq-socket (if status (server:client-connect hostname port) #f)))
		 ;; no need to login as status of #t indicates we are connecting to correct 
		 ;; server
		 (if (not status)    ;; no point in keeping dead records in the db
		      (open-run-close tasks:server-deregister tasks:open-db hostname port: port pid: pid))

		 (if (and khost-port ;; kill by host/port
			  (equal? hostname (car khost-port))
			  (equal? port (string->number (cadr khost-port))))
		     (tasks:kill-server status hostname port pid))

		 (if (and kpid
			  (equal? hostname (get-host-name))
			  (equal? kpid pid)) ;;; YEP, ALL WITH PID WILL BE KILLED!!!
		     (tasks:kill-server status hostname #f pid))

		 (format #t fmtstr id mt-ver pid hostname interface port start-time priority 
			 (if status "alive" "dead"))))
	     servers)
	    (debug:print-info 1 "Done with listservers")
	    (set! *didsomething* #t)
	    (exit) ;; must do, would have to add checks to many/all calls below
	    )
	  (exit)))
    ;; if not list or kill then start a client (if appropriate)
    (if (or (args-defined? "-h" "-version" "-gen-megatest-area" "-gen-megatest-test")
	    (eq? (length (hash-table-keys args:arg-hash)) 0))
	(debug:print-info 1 "Server connection not needed")
	(server:client-launch do-ping: #t)))

;;======================================================================
;; Remove old run(s)
;;======================================================================

;; since several actions can be specified on the command line the removal
;; is done first
(define (operate-on action)
  (cond
   ((not (args:get-arg ":runname"))
    (debug:print 0 "ERROR: Missing required parameter for " action ", you must specify the run name pattern with :runname patt")
    (exit 2))
   ((not (args:get-arg "-testpatt"))
    (debug:print 0 "ERROR: Missing required parameter for " action ", you must specify the test pattern with -testpatt")
    (exit 3))
   (else
    (if (not (car *configinfo*))
	(begin
	  (debug:print 0 "ERROR: Attempted " action "on test(s) but run area config file not found")
	  (exit 1))
	;; put test parameters into convenient variables
	(runs:operate-on  action
			  (args:get-arg ":runname")
			  (args:get-arg "-testpatt")
			  state: (args:get-arg ":state") 
			  status: (args:get-arg ":status")
			  new-state-status: (args:get-arg "-set-state-status")))
    (set! *didsomething* #t))))
	  
(if (args:get-arg "-remove-runs")
    (general-run-call 
     "-remove-runs"
     "remove runs"
     (lambda (target runname keys keynames keyvallst)
       (operate-on 'remove-runs))))

(if (args:get-arg "-set-state-status")
    (general-run-call 
     "-set-state-status"
     "set state and status"
     (lambda (target runname keys keynames keyvallst)
       (operate-on 'set-state-status))))

;;======================================================================
;; Query runs
;;======================================================================

(if (args:get-arg "-list-runs")
    (if (setup-for-run)
	(let* ((db       #f)
	       (runpatt  (args:get-arg "-list-runs"))
	       (testpatt (args:get-arg "-testpatt"))
	       (runsdat  (open-run-close db:get-runs db runpatt #f #f '()))
	       (runs     (db:get-rows runsdat))
	       (header   (db:get-header runsdat))
	       (keys     (open-run-close db:get-keys db))
	       (keynames (map key:get-fieldname keys)))
	  ;; Each run
	  (for-each 
	   (lambda (run)
	     (debug:print 1 "Run: "
			  (string-intersperse (map (lambda (x)
						     (db:get-value-by-header run header x))
						   keynames) "/")
			  "/"
			  (db:get-value-by-header run header "runname")
			  " status: " (db:get-value-by-header run header "state"))
	     (let ((run-id (open-run-close db:get-value-by-header run header "id")))
	       (let ((tests (open-run-close db:get-tests-for-run db run-id testpatt '() '())))
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
			  (let ((steps (open-run-close db:get-steps-for-test db (db:test-get-id test))))
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
	  (set! *didsomething* #t))
	(exit)))

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
     (lambda (target runname keys keynames keyvallst)
       (runs:run-tests target
		       runname
		       (if (args:get-arg "-testpatt")
			   (args:get-arg "-testpatt")
			   "%/%")
		       user
		       args:arg-hash))))

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
   (lambda (target runname keys keynames keyvallst)
     (runs:run-tests target
		     runname
		     (args:get-arg "-runtests")
		     user
		     args:arg-hash))))

;;======================================================================
;; Rollup into a run
;;======================================================================

(if (args:get-arg "-rollup")
    (begin
      (debug:print 0 "ERROR: Rollup is currently not working. If you need it please submit a ticket at http://www.kiatoa.com/fossils/megatest")
      (exit 4)))
;;     (general-run-call 
;;      "-rollup" 
;;      "rollup tests" 
;;      (lambda (target runname keys keynames keyvallst)
;;        (runs:rollup-run keys
;; 			(keys->alist keys "na")
;; 			(args:get-arg ":runname") 
;; 			user))))

;;======================================================================
;; Lock or unlock a run
;;======================================================================

(if (or (args:get-arg "-lock")(args:get-arg "-unlock"))
    (general-run-call 
     (if (args:get-arg "-lock") "-lock" "-unlock")
     "lock/unlock tests" 
     (lambda (target runname keys keynames keyvallst)
       (runs:handle-locking 
		  target
		  keys
		  (args:get-arg ":runname") 
		  (args:get-arg "-lock")
		  (args:get-arg "-unlock")
		  user))))

;;======================================================================
;; Get paths to tests
;;======================================================================
;; Get test paths matching target, runname, testpatt, and itempatt
(if (or (args:get-arg "-test-files")(args:get-arg "-test-paths"))
    ;; if we are in a test use the MT_CMDINFO data
    (if (getenv "MT_CMDINFO")
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
	       (status    (args:get-arg ":status"))
	       (target    (args:get-arg "-target"))
	       (toppath   (assoc/default 'toppath   cmdinfo)))
	  (change-directory toppath)
	  (if (not target)
	      (begin
		(debug:print 0 "ERROR: -target is required.")
		(exit 1)))
	  (if (not (setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, giving up on -test-paths or -test-files, exiting")
		(exit 1)))
	  (let* ((keys     (open-run-close db:get-keys db))
		 (keynames (map key:get-fieldname keys))
		 (paths    (open-run-close db:test-get-paths-matching db keynames target (args:get-arg "-test-files"))))
	    (set! *didsomething* #t)
	    (for-each (lambda (path)
			(print path))
		      paths)))
	;; else do a general-run-call
	(general-run-call 
	 "-test-files"
	 "Get paths to test"
	 (lambda (target runname keys keynames keyvallst)
	   (let* ((db       #f)
		  (itempatt (args:get-arg "-itempatt"))
		  (paths    (open-run-close db:test-get-paths-matching db keynames target (args:get-arg "-test-files"))))
	     (for-each (lambda (path)
			 (print path))
		       paths))))))

;;======================================================================
;; Archive tests
;;======================================================================
;; Archive tests matching target, runname, testpatt, and itempatt
(if (args:get-arg "-archive")
    ;; if we are in a test use the MT_CMDINFO data
    (if (getenv "MT_CMDINFO")
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
	       (status    (args:get-arg ":status"))
	       (target    (args:get-arg "-target")))
	  (change-directory testpath)
	  (if (not target)
	      (begin
		(debug:print 0 "ERROR: -target is required.")
		(exit 1)))
	  (if (not (setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, giving up on -archive, exiting")
		(exit 1)))
	  (let* ((itempatt (args:get-arg "-itempatt"))
		 (keys     (open-run-close db:get-keys db))
		 (keynames (map key:get-fieldname keys))
		 (paths    (open-run-close db:test-get-paths-matching db keynames target)))
	    (set! *didsomething* #t)
	    (for-each (lambda (path)
			(print path))
		      paths)))
	;; else do a general-run-call
	(general-run-call 
	 "-test-paths"
	 "Get paths to tests"
	 (lambda (target runname keys keynames keyvallst)
	   (let* ((db       #f)
		  (itempatt (args:get-arg "-itempatt"))
		  (paths    (open-run-close db:test-get-paths-matching db keynames target)))
	     (for-each (lambda (path)
			 (print path))
		       paths))))))

;;======================================================================
;; Extract a spreadsheet from the runs database
;;======================================================================

(if (args:get-arg "-extract-ods")
    (general-run-call
     "-extract-ods"
     "Make ods spreadsheet"
     (lambda (target runname keys keynames keyvallst)
       (let ((db         #f)
	     (outputfile (args:get-arg "-extract-ods"))
	     (runspatt   (args:get-arg ":runname"))
	     (pathmod    (args:get-arg "-pathmod"))
	     (keyvalalist (keys->alist keys "%")))
	 (debug:print 2 "Extract ods, outputfile: " outputfile " runspatt: " runspatt " keyvalalist: " keyvalalist)
	 (open-run-close db:extract-ods-file db outputfile keyvalalist (if runspatt runspatt "%") pathmod)))))

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

;;======================================================================
;; Test commands (i.e. for use inside tests)
;;======================================================================

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
	       (test-id   (assoc/default 'test-id   cmdinfo))
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
	  (if (and state status)
	      (open-run-close db:teststep-set-status! db test-id step state status (args:get-arg "-m") logfile)
	      (begin
		(debug:print 0 "ERROR: You must specify :state and :status with every call to -step")
		(exit 6)))
	  (if db (sqlite3:finalize! db))
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
	       (test-id   (assoc/default 'test-id   cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (db        #f)
	       (state     (args:get-arg ":state"))
	       (status    (args:get-arg ":status")))
	  (change-directory testpath)
	  (if (not (setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, exiting")
		(exit 1)))

	  ;; can setup as client for server mode now
	  (server:client-setup)

	  (if (args:get-arg "-load-test-data")
	      ;; has sub commands that are rdb:
	      (open-run-close db:load-test-data db test-id))
	  (if (args:get-arg "-setlog")
	      (let ((logfname (args:get-arg "-setlog")))
		(cdb:test-set-log! *runremote* test-id logfname)))
	  (if (args:get-arg "-set-toplog")
	      (open-run-close tests:test-set-toplog! db run-id test-name (args:get-arg "-set-toplog")))
	  (if (args:get-arg "-summarize-items")
	      (open-run-close tests:summarize-items db run-id test-name #t)) ;; do force here
	  (if (args:get-arg "-runstep")
	      (if (null? remargs)
		  (begin
		    (debug:print 0 "ERROR: nothing specified to run!")
		    (if db (sqlite3:finalize! db))
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
				       ((zsh bash sh ash) "2>&1 >")
				       (else ">&")))
			 (fullcmd    (conc "(" (string-intersperse 
						(cons cmd params) " ")
					   ") " redir " " logfile)))
		    ;; mark the start of the test
		    (open-run-close db:teststep-set-status! db test-id stepname "start" "n/a" (args:get-arg "-m") logfile)
		    ;; run the test step
		    (debug:print-info 2 "Running \"" fullcmd "\"")
		    (change-directory startingdir)
		    (set! exitstat (system fullcmd)) ;; cmd params))
		    (set! *globalexitstatus* exitstat)
		    (change-directory testpath)
		    ;; run logpro if applicable ;; (process-run "ls" (list "/foo" "2>&1" "blah.log"))
		    (if logprofile
			(let* ((htmllogfile (conc stepname ".html"))
			       (oldexitstat exitstat)
			       (cmd         (string-intersperse (list "logpro" logprofile htmllogfile "<" logfile ">" (conc stepname "_logpro.log")) " ")))
			  (debug:print-info 2 "running \"" cmd "\"")
			  (change-directory startingdir)
			  (set! exitstat (system cmd))
			  (set! *globalexitstatus* exitstat) ;; no necessary
			  (change-directory testpath)
			  (cdb:test-set-log! *runremote* test-id htmllogfile)))
		    (let ((msg (args:get-arg "-m")))
		      (open-run-close db:teststep-set-status! db test-id stepname "end" exitstat msg logfile))
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
		      ;; (sqlite3:finalize! db)
		      (exit 6)))
		(let* ((msg    (args:get-arg "-m"))
		       (numoth (length (hash-table-keys otherdata))))
		  ;; Convert to rpc inside the tests:test-set-status! call, not here
		  (tests:test-set-status! test-id state newstatus msg otherdata))))
	  (if db (sqlite3:finalize! db))
	  (set! *didsomething* #t))))

;;======================================================================
;; Various helper commands can go below here
;;======================================================================

(if (args:get-arg "-showkeys")
    (let ((db #f)
	  (keys #f))
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting")
	    (exit 1)))
      (set! keys (open-run-close db:get-keys db))
      (debug:print 1 "Keys: " (string-intersperse (map key:get-fieldname keys) ", "))
      (if db (sqlite3:finalize! db))
      (set! *didsomething* #t)))

(if (args:get-arg "-gui")
    (begin
      (debug:print 0 "Look at the dashboard for now")
      ;; (megatest-gui)
      (set! *didsomething* #t)))

(if (args:get-arg "-gen-megatest-area")
    (begin
      (genexample:mk-megatest.config)
      (set! *didsomething* #t)))

(if (args:get-arg "-gen-megatest-test")
    (let ((testname (args:get-arg "-gen-megatest-test")))
      (genexample:mk-megatest-test testname)
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
      (open-run-close patch-db #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Update the tests meta data from the testconfig files
;;======================================================================

(if (args:get-arg "-update-meta")
    (begin
      (if (not (setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
	    (exit 1)))
      ;; now can find our db
      (open-run-close runs:update-all-test_meta db)
      (set! *didsomething* #t)))

;;======================================================================
;; Start a repl
;;======================================================================

(if (args:get-arg "-repl")
    (let* ((toppath (setup-for-run))
	   (db      (if toppath (open-db) #f)))
      (if db
	  (begin
	    (set! *db* db)
	    (set! *client-non-blocking-mode* #t)
	    ;; (server:client-setup)
	    (import readline)
	    (import apropos)
	    (gnu-history-install-file-manager
	     (string-append
	      (or (get-environment-variable "HOME") ".") "/.megatest_history"))
	    (current-input-port (make-gnu-readline-port "megatest> "))
	    (repl))
	  (exit))
      (set! *didsomething* #t)))

;;======================================================================
;; Exit and clean up
;;======================================================================

;; this is the socket if we are a client
;; (if (and *runremote*
;; 	 (socket? *runremote*))
;;     (close-socket *runremote*))

(if (not *didsomething*)
    (debug:print 0 help))

;; (if *runremote* (rpc:close-all-connections!))
    
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
