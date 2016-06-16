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

;; fake out readline usage of toplevel-command
(define (toplevel-command . a) #f)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 base64 readline apropos json http-client directory-utils rpc ;; (srfi 18) extras)
     http-client srfi-18 extras format) ;;  zmq extras)

;; Added for csv stuff - will be removed
;;
(use sparse-vectors)

(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))
(import (prefix rpc rpc:))
(require-library mutils)

;; (use zmq)

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
(declare (uses runs))
(declare (uses launch))
(declare (uses server))
(declare (uses client))
(declare (uses tests))
(declare (uses genexample))
(declare (uses daemon))
(declare (uses db))

(declare (uses tdb))
(declare (uses mt))
(declare (uses api))
(declare (uses tasks)) ;; only used for debugging.
(declare (uses env))

(define *db* #f) ;; this is only for the repl, do not use in general!!!!

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "megatest-fossil-hash.scm")

(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.megatestrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

;; Disabled help items
;;  -rollup                 : (currently disabled) fill run (set by :runname)  with latest test(s)
;;                            from prior runs with same keys

(define help (conc "
Megatest, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2015

Usage: megatest [options]
  -h                      : this help
  -version                : print megatest version (currently " megatest-version ")

Launching and managing runs
  -runall                 : run all tests or as specified by -testpatt
  -remove-runs            : remove the data for a run, requires -runname and -testpatt
                            Optionally use :state and :status
  -set-state-status X,Y   : set state to X and status to Y, requires controls per -remove-runs
  -rerun FAIL,WARN...     : force re-run for tests with specificed status(s)
  -rerun-clean            : set all tests not COMPLETED+PASS,WARN,WAIVED to NOT_STARTED,n/a
                            and then run the specified testpatt with -preclean
  -lock                   : lock run specified by target and runname
  -unlock                 : unlock run specified by target and runname
  -set-run-status status  : sets status for run to status, requires -target and -runname
  -get-run-status         : gets status for run specified by target and runname
  -run-wait               : wait on run specified by target and runname
  -preclean               : remove the existing test directory before running the test
  -clean-cache            : remove the cached megatest.config and runconfig.config files

Selectors (e.g. use for -runtests, -remove-runs, -set-state-status, -list-runs etc.)
  -target key1/key2/...   : run for key1, key2, etc.
  -reqtarg key1/key2/...  : run for key1, key2, etc. but key1/key2 must be in runconfig
  -testpatt patt1/patt2,patt3/...  : % is wildcard
  -runname                : required, name for this particular test run
  -state                  : Applies to runs, tests or steps depending on context
  -status                 : Applies to runs, tests or steps depending on context

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
  -show-keys              : show the keys used in this megatest setup
  -test-files targpatt    : get the most recent test path/file matching targpatt e.g. %/%... 
                            returns list sorted by age ascending, see examples below
  -test-paths             : get the test paths matching target, runname, item and test
                            patterns.
  -list-disks             : list the disks available for storing runs
  -list-targets           : list the targets in runconfigs.config
  -list-db-targets        : list the target combinations used in the db
  -show-config            : dump the internal representation of the megatest.config file
  -show-runconfig         : dump the internal representation of the runconfigs.config file
  -dumpmode MODE          : dump in MODE format instead of sexpr, MODE=json,ini,sexp etc.
  -show-cmdinfo           : dump the command info for a test (run in test environment)
  -section sectionName
  -var varName            : for config and runconfig lookup value for sectionName varName
  -since N                : get list of runs changed since time N (Unix seconds)
  -fields fieldspec       : fields to include in json dump; runs:id,runame+tests:testname+steps
  -sort fieldname         : in -list-runs sort tests by this field

Misc 
  -start-dir path         : switch to this directory before running megatest
  -rebuild-db             : bring the database schema up to date
  -cleanup-db             : remove any orphan records, vacuum the db
  -import-megatest.db     : migrate a database from v1.55 series to v1.60 series
  -sync-to-megatest.db    : migrate data back to megatest.db
  -update-meta            : update the tests metadata for all tests
  -setvars VAR1=val1,VAR2=val2 : Add environment variables to a run NB// these are
                                 overwritten by values set in config files.
  -server -|hostname      : start the server (reduces contention on megatest.db), use
                            - to automatically figure out hostname
  -transport http|zmq     : use http or zmq for transport (default is http) 
  -daemonize              : fork into background and disconnect from stdin/out
  -log logfile            : send stdout and stderr to logfile
  -list-servers           : list the servers 
  -stop-server id         : stop server specified by id (see output of -list-servers), use
                            0 to kill all
  -repl                   : start a repl (useful for extending megatest)
  -load file.scm          : load and run file.scm
  -mark-incompletes       : find and mark incomplete tests
  -ping run-id|host:port  : ping server, exit with 0 if found
  -debug N|N,M,O...       : enable debug 0-N or N and M and O ...

Utilities
  -env2file fname         : write the environment to fname.csh and fname.sh
  -envcap fname=context   : save current variables labeled as context in file fname
  -refdb2dat refdb        : convert refdb to sexp or to format specified by -dumpmode
                            formats: perl, ruby, sqlite3, csv (for csv the -o param
                            will substitute %s for the sheet name in generating 
                            multiple sheets)
  -o                      : output file for refdb2dat (defaults to stdout)
  -archive cmd            : archive runs specified by selectors to one of disks specified
                            in the [archive-disks] section.
                            cmd: keep-html, restore, save, save-remove

Spreadsheet generation
  -extract-ods fname.ods  : extract an open document spreadsheet from the database
  -pathmod path           : insert path, i.e. path/runame/itempath/logfile.html
                            will clear the field if no rundir/testname/itempath/logfile
                            if it contains forward slashes the path will be converted
                            to windows style
Getting started
  -gen-megatest-area       : create a skeleton megatest area. You will be prompted for paths
  -gen-megatest-test tname : create a skeleton megatest test. You will be prompted for info

Examples

# Get test path, use '.' to get a single path or a specific path/file pattern
megatest -test-files 'logs/*.log' -target ubuntu/n%/no% -runname w49% -testpatt test_mt%

Called as " (string-intersperse (argv) " ") "
Version " megatest-version ", built from " megatest-fossil-hash ))

;;  -gui                    : start a gui interface
;;  -config fname           : override the runconfig file with fname

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-runtests"  ;; run a specific test
			"-config"    ;; override the config file name
			"-execute"   ;; run the command encoded in the base64 parameter
			"-step"
			"-target"
			"-reqtarg"
			":runname"
			"-runname"
			":state"  
			"-state"
			":status"
			"-status"
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
			"-start-dir"
			"-server"
			"-stop-server"
			"-transport"
			"-kill-server"
			"-port"
			"-extract-ods"
			"-pathmod"
			"-env2file"
			"-envcap"
			"-envdelta"
			"-setvars"
			"-set-state-status"
			"-set-run-status"
			"-debug" ;; for *verbosity* > 2
			"-gen-megatest-test"
			"-override-timeout"
			"-test-files"  ;; -test-paths is for listing all
			"-load"        ;; load and exectute a scheme file
			"-section"
			"-var"
			"-dumpmode"
			"-run-id"
			"-ping"
			"-refdb2dat"
			"-o"
			"-log"
			"-archive"
			"-since"
			"-fields"
			"-recover-test" ;; run-id,test-id - used internally to recover a test stuck in RUNNING state
			"-sort"
			) 
		 (list  "-h" "-help" "--help"
			"-version"
		        "-force"
		        "-xterm"
		        "-showkeys"
		        "-show-keys"
		        "-test-status"
			"-set-values"
			"-load-test-data"
			"-summarize-items"
		        "-gui"
			"-daemonize"
			"-preclean"
			"-rerun-clean"
			"-clean-cache"

			;; misc
			"-repl"
			"-lock"
			"-unlock"
			"-list-servers"
                        "-run-wait"      ;; wait on a run to complete (i.e. no RUNNING)
			"-local"         ;; run some commands using local db access

			;; misc queries
			"-list-disks"
			"-list-targets"
			"-list-db-targets"
			"-show-runconfig"
			"-show-config"
			"-show-cmdinfo"
			"-get-run-status"

			;; queries
			"-test-paths" ;; get path(s) to a test, ordered by youngest first

			"-runall"    ;; run all tests, respects -testpatt, defaults to %
			"-run"       ;; alias for -runall
			"-remove-runs"
			"-rebuild-db"
			"-cleanup-db"
			"-rollup"
			"-update-meta"
			"-gen-megatest-area"
			"-mark-incompletes"

			"-convert-to-norm"
			"-convert-to-old"
			"-import-megatest.db"
			"-sync-to-megatest.db"

			"-logging"
			"-v" ;; verbose 2, more than normal (normal is 1)
			"-q" ;; quiet 0, errors/warnings only
		       )
		 args:arg-hash
		 0))

;; Add args that use remargs here
;;
(if (and (not (null? remargs))
	 (not (or
	       (args:get-arg "-runstep")
	       (args:get-arg "-envcap")
	       (args:get-arg "-envdelta")
	       )
	      ))
    (debug:print 0 #f "ERROR: Unrecognised arguments: " (string-intersperse (if (list? remargs) remargs (argv))  " ")))

;; immediately set MT_TARGET if -reqtarg or -target are available
;;
(let ((targ (or (args:get-arg "-reqtarg")(args:get-arg "-target"))))
  (if targ (setenv "MT_TARGET" targ)))

;; The watchdog is to keep an eye on things like db sync etc.
;;
(define *time-zero* (current-seconds))
(define *watchdog*
  (make-thread 
   (lambda ()
     (thread-sleep! 0.05) ;; delay for startup
     (let ((legacy-sync (common:legacy-sync-required))
	   (debug-mode  (debug:debug-mode 1))
	   (last-time   (current-seconds)))
       (if (common:legacy-sync-recommended)
	   (let loop ()
	     ;; sync for filesystem local db writes
	     ;;
	     (let ((start-time      (current-seconds))
		   (servers-started (make-hash-table)))
	       (for-each 
		(lambda (run-id)
		  (mutex-lock! *db-multi-sync-mutex*)
		  (if (and legacy-sync 
			   (hash-table-ref/default *db-local-sync* run-id #f))
		      ;; (if (> (- start-time last-write) 5) ;; every five seconds
		      (begin ;; let ((sync-time (- (current-seconds) start-time)))
			(db:multi-db-sync (list run-id) 'new2old)
			(let ((sync-time (- (current-seconds) start-time)))
			  (debug:print-info 3 #f "Sync of newdb to olddb for run-id " run-id " completed in " sync-time " seconds")
			  (if (common:low-noise-print 30 "sync new to old")
			      (debug:print-info 0 #f "Sync of newdb to olddb for run-id " run-id " completed in " sync-time " seconds")))
			;; (if (> sync-time 10) ;; took more than ten seconds, start a server for this run
			;;     (begin
			;;       (debug:print-info 0 #f "Sync is taking a long time, start up a server to assist for run " run-id)
			;;       (server:kind-run run-id)))))
			(hash-table-delete! *db-local-sync* run-id)))
		  (mutex-unlock! *db-multi-sync-mutex*))
		(hash-table-keys *db-local-sync*))
	       (if (and debug-mode
			(> (- start-time last-time) 60))
		   (begin
		     (set! last-time start-time)
		     (debug:print-info 4 #f "timestamp -> " (seconds->time-string (current-seconds)) ", time since start -> " (seconds->hr-min-sec (- (current-seconds) *time-zero*))))))
	     
	     ;; keep going unless time to exit
	     ;;
	     (if (not *time-to-exit*)
		 (let delay-loop ((count 0))
		   (if (and (not *time-to-exit*)
			    (< count 11)) ;; aprox 5-6 seconds
		       (begin
			 (thread-sleep! 1)
			 (delay-loop (+ count 1))))
		   (loop)))
	     (if (common:low-noise-print 30)
		 (debug:print-info 0 #f "Exiting watchdog timer, *time-to-exit* = " *time-to-exit*)))))
     "Watchdog thread")))

(thread-start! *watchdog*)


(if (args:get-arg "-log")
    (let ((oup (open-output-file (args:get-arg "-log"))))
      (debug:print-info 0 #f "Sending log output to " (args:get-arg "-log"))
      (current-error-port oup)
      (current-output-port oup)))

(if (or (args:get-arg "-h")
	(args:get-arg "-help")
	(args:get-arg "--help"))
    (begin
      (print help)
      (exit)))

(if (args:get-arg "-start-dir")
    (if (file-exists? (args:get-arg "-start-dir"))
	(change-directory (args:get-arg "-start-dir"))
	(begin
	  (debug:print 0 #f "ERROR: non-existant start dir " (args:get-arg "-start-dir") " specified, exiting.")
	  (exit 1))))

(if (args:get-arg "-version")
    (begin
      (print (common:version-signature)) ;; (print megatest-version)
      (exit)))

(define *didsomething* #f)

;; Overall exit handling setup immediately
;;
(if (or (args:get-arg "-process-reap"))
        ;; (args:get-arg "-runtests")
	;; (args:get-arg "-execute")
	;; (args:get-arg "-remove-runs")
	;; (args:get-arg "-runstep"))
    (let ((original-exit (exit-handler)))
      (exit-handler (lambda (#!optional (exit-code 0))
		      (printf "Preparing to exit with exit code ~A ...\n" exit-code)
		      (for-each 
		       (lambda (pid)
			 (handle-exceptions
			  exn
			  #t
			  (let-values (((pid-val exit-status exit-code) (process-wait pid #t)))
				      (if (or (eq? pid-val pid)
					      (eq? pid-val 0))
					  (begin
					    (printf "Sending signal/term to ~A\n" pid)
					    (process-signal pid signal/term))))))
		       (process:children #f))
		      (original-exit exit-code)))))

;;======================================================================
;; Misc setup stuff
;;======================================================================

(debug:setup)

(if (args:get-arg "-logging")(set! *logging* #t))

(if (debug:debug-mode 3) ;; we are obviously debugging
    (set! open-run-close open-run-close-no-exception-handling))

(if (args:get-arg "-itempatt")
    (let ((newval (conc (args:get-arg "-testpatt") "/" (args:get-arg "-itempatt"))))
      (debug:print 0 #f "WARNING: -itempatt has been deprecated, please use -testpatt testpatt/itempatt method, new testpatt is "newval)
      (hash-table-set! args:arg-hash "-testpatt" newval)
      (hash-table-delete! args:arg-hash "-itempatt")))

(if (args:get-arg "-runtests")
    (debug:print 0 #f "WARNING: \"-runtests\" is deprecated. Use \"-run\" with \"-testpatt\" instead"))

(on-exit std-exit-procedure)

;;======================================================================
;; Misc general calls
;;======================================================================

;; handle a clean-cache request as early as possible
;;
(if (args:get-arg "-clean-cache")
    (begin
      (set! *didsomething* #t) ;; suppress the help output.
      (if (getenv "MT_TARGET") ;; no point in trying if no target
	  (if (args:get-arg "-runname")
	      (let* ((toppath  (launch:setup))
		     (linktree (if toppath (configf:lookup *configdat* "setup" "linktree")))
		     (runtop   (conc linktree "/" (getenv "MT_TARGET") "/" (args:get-arg "-runname")))
		     (files    (if (file-exists? runtop)
				   (append (glob (conc runtop "/.megatest*"))
					   (glob (conc runtop "/.runconfig*")))
				   '())))
		(if (null? files)
		    (debug:print-info 0 #f "No cached megatest or runconfigs files found. None removed.")
		    (begin
		      (debug:print-info 0 #f "Removing cached files:\n    " (string-intersperse files "\n    "))
		      (for-each 
		       (lambda (f)
			 (handle-exceptions
			     exn
			     (debug:print 0 #f "WARNING: Failed to remove file " f)
			   (delete-file f)))
		       files))))
	      (debug:print 0 #f "ERROR: -clean-cache requires -runname."))
	  (debug:print 0 #f "ERROR: -clean-cache requires -target or -reqtarg"))))
	    
	  
(if (args:get-arg "-env2file")
    (begin
      (save-environment-as-files (args:get-arg "-env2file"))
      (set! *didsomething* #t)))

(if (args:get-arg "-list-disks")
    (let ((toppath (launch:setup)))
      (print 
       (string-intersperse 
	(map (lambda (x)
	       (string-intersperse 
		x
		" => "))
	     (common:get-disks *configdat*))
	"\n"))
      (set! *didsomething* #t)))

;; csv processing record
(define (make-refdb:csv)
  (vector 
   (make-sparse-array)
   (make-hash-table)
   (make-hash-table)
   0
   0))
(define-inline (refdb:csv-get-svec     vec)    (vector-ref  vec 0))
(define-inline (refdb:csv-get-rows     vec)    (vector-ref  vec 1))
(define-inline (refdb:csv-get-cols     vec)    (vector-ref  vec 2))
(define-inline (refdb:csv-get-maxrow   vec)    (vector-ref  vec 3))
(define-inline (refdb:csv-get-maxcol   vec)    (vector-ref  vec 4))
(define-inline (refdb:csv-set-svec!    vec val)(vector-set! vec 0 val))
(define-inline (refdb:csv-set-rows!    vec val)(vector-set! vec 1 val))
(define-inline (refdb:csv-set-cols!    vec val)(vector-set! vec 2 val))
(define-inline (refdb:csv-set-maxrow!  vec val)(vector-set! vec 3 val))
(define-inline (refdb:csv-set-maxcol!  vec val)(vector-set! vec 4 val))

(define (get-dat results sheetname)
  (or (hash-table-ref/default results sheetname #f)
      (let ((tmp-vec  (make-refdb:csv)))
	(hash-table-set! results sheetname tmp-vec)
	tmp-vec)))

(if (args:get-arg "-refdb2dat")
    (let* ((input-db (args:get-arg "-refdb2dat"))
	   (out-file (args:get-arg "-o"))
	   (out-fmt  (or (args:get-arg "-dumpmode") "scheme"))
	   (out-port (if (and out-file 
			      (not (member out-fmt '("sqlite3" "csv"))))
			 (open-output-file out-file)
			 (current-output-port)))
	   (res-data (configf:read-refdb input-db))
	   (data     (car res-data))
	   (msg      (cadr res-data)))
      (if (not data)
	  (debug:print 0 #f "Bad input? data=" data) ;; some error occurred
	  (with-output-to-port out-port
	    (lambda ()
	      (case (string->symbol out-fmt)
		((scheme)(pp data))
		((perl)
		 ;; (print "%hash = (")
		 ;;        key1 => 'value1',
		 ;;        key2 => 'value2',
		 ;;        key3 => 'value3',
		 ;; );
		 (configf:map-all-hier-alist 
		  data 
		  (lambda (sheetname sectionname varname val)
		    (print "$data{\"" sheetname "\"}{\"" sectionname "\"}{\"" varname "\"} = \"" val "\";"))))
		((python ruby)
		 (print "data={}")
		 (configf:map-all-hier-alist
		  data
		  (lambda (sheetname sectionname varname val)
		    (print "data[\"" sheetname "\"][\"" sectionname "\"][\"" varname "\"] = \"" val "\""))
		  initproc1:
		  (lambda (sheetname)
		    (print "data[\"" sheetname "\"] = {}"))
		  initproc2:
		  (lambda (sheetname sectionname)
		    (print "data[\"" sheetname "\"][\"" sectionname "\"] = {}"))))
		((csv)
		 (let* ((results  (make-hash-table)) ;; (make-sparse-array)))
			(row-cols (make-hash-table))) ;; hash of hashes where section => ht { row-<name> => num or col-<name> => num
		   ;; (print "data=")
		   ;; (pp data)
		   (configf:map-all-hier-alist
		    data
		    (lambda (sheetname sectionname varname val)
		      ;; (print "sheetname: " sheetname ", sectionname: " sectionname ", varname: " varname ", val: " val)
		      (let* ((dat      (get-dat results sheetname))
			     (vec      (refdb:csv-get-svec dat))
			     (rownames (refdb:csv-get-rows dat))
			     (colnames (refdb:csv-get-cols dat))
			     (currrown (hash-table-ref/default rownames varname #f))
			     (currcoln (hash-table-ref/default colnames sectionname #f))
			     (rown     (or currrown 
					   (let* ((lastn   (refdb:csv-get-maxrow dat))
						  (newrown (+ lastn 1)))
					     (refdb:csv-set-maxrow! dat newrown)
					     newrown)))
			     (coln     (or currcoln 
					   (let* ((lastn   (refdb:csv-get-maxcol dat))
						  (newcoln (+ lastn 1)))
					     (refdb:csv-set-maxcol! dat newcoln)
					     newcoln))))
			(if (not (sparse-array-ref vec 0 coln)) ;; (eq? rown 0)
			    (begin
			      (sparse-array-set! vec 0 coln sectionname)
			      ;; (print "sparse-array-ref " 0 "," coln "=" (sparse-array-ref vec 0 coln))
			      ))
			(if (not (sparse-array-ref vec rown 0)) ;; (eq? coln 0)
			    (begin
			      (sparse-array-set! vec rown 0 varname)
			      ;; (print "sparse-array-ref " rown "," 0 "=" (sparse-array-ref vec rown 0))
			      ))
			(if (not currrown)(hash-table-set! rownames varname rown))
			(if (not currcoln)(hash-table-set! colnames sectionname coln))
			;; (print "dat=" dat ", rown=" rown ", coln=" coln)
			(sparse-array-set! vec rown coln val)
			;; (print "sparse-array-ref " rown "," coln "=" (sparse-array-ref vec rown coln))
			)))
		   (for-each
		    (lambda (sheetname)
		      (let* ((sheetdat (get-dat results sheetname))
			     (svec     (refdb:csv-get-svec sheetdat))
			     (maxrow   (refdb:csv-get-maxrow sheetdat))
			     (maxcol   (refdb:csv-get-maxcol sheetdat))
			     (fname    (if out-file 
					   (string-substitute "%s" sheetname out-file) ;; "/foo/bar/%s.csv")
					   (conc sheetname ".csv"))))
			(with-output-to-file fname
			  (lambda ()
			    ;; (print "Sheetname: " sheetname)
			    (let loop ((row       0)
				       (col       0)
				       (curr-row '())
				       (result   '()))
			      (let* ((val (sparse-array-ref svec row col))
				     (disp-val (if val
						   (conc "\"" val "\"")
						   "")))
				(if (> col 0)(display ","))
				(display disp-val)
				(cond
				 ((> row maxrow)(display "\n") result)
				 ((>= col maxcol)
				  (display "\n")
				  (loop (+ row 1) 0 '() (append result (list curr-row))))
				 (else
				  (loop row (+ col 1) (append curr-row (list val)) result)))))))))
		    (hash-table-keys results))))
		((sqlite3)
		 (let* ((db-file   (or out-file (pathname-file input-db)))
			(db-exists (file-exists? db-file))
			(db        (sqlite3:open-database db-file)))
		   (if (not db-exists)(sqlite3:execute db "CREATE TABLE data (sheet,section,var,val);"))
		   (configf:map-all-hier-alist
		    data
		    (lambda (sheetname sectionname varname val)
		      (sqlite3:execute db
				       "INSERT OR REPLACE INTO data (sheet,section,var,val) VALUES (?,?,?,?);"
				       sheetname sectionname varname val)))
		   (sqlite3:finalize! db)))
		(else
		 (pp data))))))
      (if out-file (close-output-port out-port))
      (exit) ;; yes, bending the rules here - need to exit since this is a utility
      ))

(if (args:get-arg "-ping")
    (let* ((run-id        (string->number (args:get-arg "-run-id")))
	   (host:port     (args:get-arg "-ping")))
      (server:ping run-id host:port)))

;;======================================================================
;; Capture, save and manipulate environments
;;======================================================================

;; NOTE: Keep these above the section where the server or client code is setup

(let ((envcap (args:get-arg "-envcap")))
  (if envcap
      (let* ((db      (env:open-db (if (null? remargs) "envdat.db" (car remargs)))))
	(env:save-env-vars db envcap)
	(env:close-database db)
	(set! *didsomething* #t))))

;; delta "language" will eventually be res=a+b-c but for now it is just res=a-b 
;;
(let ((envdelta (args:get-arg "-envdelta")))
  (if envdelta
      (let ((match (string-split envdelta "-")));; (string-match "([a-z0-9_]+)=([a-z0-9_\\-,]+)" envdelta)))
	(if (not (null? match))
	    (let* ((db        (env:open-db (if (null? remargs) "envdat.db" (car remargs))))
		   ;; (resctx    (cadr match))
		   ;; (equn      (caddr match))
		   (parts     match) ;; (string-split equn "-"))
		   (minuend   (car parts))
		   (subtraend (cadr parts))
		   (added     (env:get-added   db minuend subtraend))
		   (removed   (env:get-removed db minuend subtraend))
		   (changed   (env:get-changed db minuend subtraend)))
	      ;; (pp (hash-table->alist added))
	      ;; (pp (hash-table->alist removed))
	      ;; (pp (hash-table->alist changed))
	      (if (args:get-arg "-o")
		  (with-output-to-file
		      (args:get-arg "-o")
		    (lambda ()
		      (env:print added removed changed)))
		  (env:print added removed changed))
	      (env:close-database db)
	      (set! *didsomething* #t))
	    (debug:print 0 #f "ERROR: Parameter to -envdelta should be new=star-end")))))

;;======================================================================
;; Start the server - can be done in conjunction with -runall or -runtests (one day...)
;;   we start the server if not running else start the client thread
;;======================================================================

(if (args:get-arg "-server")

    ;; Server? Start up here.
    ;;
    (let ((tl        (launch:setup))
	  (run-id    (and (args:get-arg "-run-id")
			  (string->number (args:get-arg "-run-id")))))
      (if run-id
	  (begin
	    (server:launch run-id)
	    (set! *didsomething* #t))
	  (debug:print 0 #f "ERROR: server requires run-id be specified with -run-id")))

    ;; Not a server? This section will decide how to communicate
    ;;
    ;;  Setup client for all expect listed here
    (if (null? (lset-intersection 
		equal?
		(hash-table-keys args:arg-hash)
		'("-list-servers"
		  "-stop-server"
		  "-show-cmdinfo"
		  "-list-runs"
		  "-ping")))
	(if (launch:setup)
	    (let ((run-id    (and (args:get-arg "-run-id")
				  (string->number (args:get-arg "-run-id")))))
	      ;; (set! *fdb*   (filedb:open-db (conc *toppath* "/db/paths.db")))
	      ;; if not list or kill then start a client (if appropriate)
	      (if (or (args-defined? "-h" "-version" "-gen-megatest-area" "-gen-megatest-test")
		      (eq? (length (hash-table-keys args:arg-hash)) 0))
		  (debug:print-info 1 #f "Server connection not needed")
		  (begin
		    ;; (if run-id 
		    ;;     (client:launch run-id) 
		    ;;     (client:launch 0)      ;; without run-id we'll start a server for "0"
		    #t
		    ))))))

;; MAY STILL NEED THIS
;;		       (set! *megatest-db* (make-dbr:dbstruct path: *toppath* local: #t))))))))))

(if (or (args:get-arg "-list-servers")
	(args:get-arg "-stop-server"))
    (let ((tl (launch:setup)))
      (if tl 
	  (let* ((tdbdat  (tasks:open-db))
		 (servers (tasks:get-all-servers (db:delay-if-busy tdbdat)))
		 (fmtstr  "~5a~12a~8a~20a~24a~10a~10a~10a~10a\n")
		 (servers-to-kill '())
		 (killinfo   (args:get-arg "-stop-server"))
		 (khost-port (if killinfo (if (substring-index ":" killinfo)(string-split ":") #f) #f))
		 (sid        (if killinfo (if (substring-index ":" killinfo) #f (string->number killinfo)) #f)))
	    (format #t fmtstr "Id" "MTver" "Pid" "Host" "Interface:OutPort" "InPort" "LastBeat" "State" "Transport")
	    (format #t fmtstr "==" "=====" "===" "====" "=================" "======" "========" "=====" "=========")
	    (for-each 
	     (lambda (server)
	       (let* ((id         (vector-ref server 0))
		      (pid        (vector-ref server 1))
		      (hostname   (vector-ref server 2))
		      (interface  (vector-ref server 3)) 
		      (pullport   (vector-ref server 4))
		      (pubport    (vector-ref server 5))
		      (start-time (vector-ref server 6))
		      (priority   (vector-ref server 7))
		      (state      (vector-ref server 8))
		      (mt-ver     (vector-ref server 9))
		      (last-update (vector-ref server 10)) 
		      (transport  (vector-ref server 11))
		      (killed     #f)
		      (status     (< last-update 20)))
		 ;;   (zmq-sockets (if status (server:client-connect hostname port) #f)))
		 ;; no need to login as status of #t indicates we are connecting to correct 
		 ;; server
		 (if (equal? state "dead")
		     (if (> last-update (* 25 60 60)) ;; keep records around for slighly over a day.
			 (tasks:server-deregister (db:delay-if-busy tdbdat) hostname pullport: pullport pid: pid action: 'delete))
		     (if (> last-update 20)        ;; Mark as dead if not updated in last 20 seconds
			 (tasks:server-deregister (db:delay-if-busy tdbdat) hostname pullport: pullport pid: pid)))
		 (format #t fmtstr id mt-ver pid hostname (conc interface ":" pullport) pubport last-update
			 (if status "alive" "dead") transport)
		 (if (or (equal? id sid)
			 (equal? sid 0)) ;; kill all/any
		     (begin
		       (debug:print-info 0 #f "Attempting to stop server with pid " pid)
		       (tasks:kill-server status hostname pullport pid transport)))))
	     servers)
	    (debug:print-info 1 #f "Done with listservers")
	    (set! *didsomething* #t)
	    (exit)) ;; must do, would have to add checks to many/all calls below
	  (exit))))

;;======================================================================
;; Weird special calls that need to run *after* the server has started?
;;======================================================================

(if (args:get-arg "-list-targets")
    (let ((targets (common:get-runconfig-targets)))
      (debug:print 1 #f "Found "(length targets) " targets")
      (case (string->symbol (or (args:get-arg "-dumpmode") "alist"))
	((alist)
	 (for-each (lambda (x)
		     ;; (print "[" x "]"))
		     (print x))
		   targets))
	((json)
	 (json-write targets))
	(else
	 (debug:print 0 #f "ERROR: dump output format " (args:get-arg "-dumpmode") " not supported for -list-targets")))
      (set! *didsomething* #t)))

;; cache the runconfigs in $MT_LINKTREE/$MT_TARGET/$MT_RUNNAME/.runconfig
;;
(define (full-runconfigs-read)
;; in the envprocessing branch the below code replaces the further below code
;;  (if (eq? *configstatus* 'fulldata)
;;      *runconfigdat*
;;      (begin
;;	(launch:setup)
;;	*runconfigdat*)))

  (let* ((rundir (if (and (getenv "MT_LINKTREE")(getenv "MT_TARGET")(getenv "MT_RUNNAME"))
		     (conc (getenv "MT_LINKTREE") "/" (getenv "MT_TARGET") "/" (getenv "MT_RUNNAME"))
		     #f))
	 (cfgf   (if rundir (conc rundir "/.runconfig." megatest-version "-" megatest-fossil-hash) #f)))
    (if (and cfgf
	     (file-exists? cfgf)
	     (file-write-access? cfgf))
	(configf:read-alist cfgf)
	(let* ((keys   (rmt:get-keys))
	       (target (common:args-get-target))
	       (key-vals (if target (keys:target->keyval keys target) #f))
	       (sections (if target (list "default" target) #f))
	       (data     (begin
			   (setenv "MT_RUN_AREA_HOME" *toppath*)
			   (if key-vals
			       (for-each (lambda (kt)
					   (setenv (car kt) (cadr kt)))
					 key-vals))
			   (read-config (conc *toppath* "/runconfigs.config") #f #t sections: sections))))
	  (if (and rundir ;; have all needed variabless
		   (directory-exists? rundir)
		   (file-write-access? rundir))
	      (begin
		(configf:write-alist data cfgf)
		;; force re-read of megatest.config - this resolves circular references between megatest.config
		(launch:setup force: #t)
		(launch:cache-config))) ;; we can safely cache megatest.config since we have a valid runconfig
	  data))))

(if (args:get-arg "-show-runconfig")
    (let ((tl (launch:setup)))
      (push-directory *toppath*)
      (let ((data (full-runconfigs-read)))
	;; keep this one local
	(cond
	 ((and (args:get-arg "-section")
	       (args:get-arg "-var"))
	  (let ((val (or (configf:lookup data (args:get-arg "-section")(args:get-arg "-var"))
			 (configf:lookup data "default" (args:get-arg "-var")))))
	    (if val (print val))))
	 ((not (args:get-arg "-dumpmode"))
	  (pp (hash-table->alist data)))
	 ((string=? (args:get-arg "-dumpmode") "json")
	  (json-write data))
	 ((string=? (args:get-arg "-dumpmode") "ini")
	  (configf:config->ini data))
	 (else
	  (debug:print 0 #f "ERROR: -dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
	(set! *didsomething* #t))
      (pop-directory)))

(if (args:get-arg "-show-config")
    (let ((tl   (launch:setup))
	  (data *configdat*)) ;; (read-config "megatest.config" #f #t)))
      (push-directory *toppath*)
      ;; keep this one local
      (cond 
       ((and (args:get-arg "-section")
	     (args:get-arg "-var"))
	(let ((val (configf:lookup data (args:get-arg "-section")(args:get-arg "-var"))))
	  (if val (print val))))

       ;; print just a section if only -section

       ((not (args:get-arg "-dumpmode"))
	(pp (hash-table->alist data)))
       ((string=? (args:get-arg "-dumpmode") "json")
	(json-write data))
       ((string=? (args:get-arg "-dumpmode") "ini")
	(configf:config->ini data))
       (else
	(debug:print 0 #f "ERROR: -dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
      (set! *didsomething* #t)
      (pop-directory)))

(if (args:get-arg "-show-cmdinfo")
    (if (or (args:get-arg ":value")(getenv "MT_CMDINFO"))
	(let ((data (common:read-encoded-string (or (args:get-arg ":value")(getenv "MT_CMDINFO")))))
	  (if (equal? (args:get-arg "-dumpmode") "json")
	      (json-write data)
	      (pp data))
	  (set! *didsomething* #t))
	(debug:print-info 0 #f "environment variable MT_CMDINFO is not set")))

;;======================================================================
;; Remove old run(s)
;;======================================================================

;; since several actions can be specified on the command line the removal
;; is done first
(define (operate-on action)
  (let* ((runrec (runs:runrec-make-record))
	 (target (common:args-get-target)))
    (cond
     ((not target)
      (debug:print 0 #f "ERROR: Missing required parameter for " action ", you must specify -target or -reqtarg")
      (exit 1))
     ((not (or (args:get-arg ":runname")
	       (args:get-arg "-runname")))
      (debug:print 0 #f "ERROR: Missing required parameter for " action ", you must specify the run name pattern with -runname patt")
      (exit 2))
     ((not (args:get-arg "-testpatt"))
      (debug:print 0 #f "ERROR: Missing required parameter for " action ", you must specify the test pattern with -testpatt")
      (exit 3))
     (else
      (if (not (car *configinfo*))
	  (begin
	    (debug:print 0 #f "ERROR: Attempted " action "on test(s) but run area config file not found")
	    (exit 1))
	  ;; put test parameters into convenient variables
	  (begin
	    ;; check for correct version, exit with message if not correct
	    (common:exit-on-version-changed)
	    (runs:operate-on  action
			      target
			      (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
			      (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
			      state: (common:args-get-state)
			      status: (common:args-get-status)
			      new-state-status: (args:get-arg "-set-state-status"))))
      (set! *didsomething* #t)))))

(if (args:get-arg "-remove-runs")
    (general-run-call 
     "-remove-runs"
     "remove runs"
     (lambda (target runname keys keyvals)
       (operate-on 'remove-runs))))

(if (args:get-arg "-set-state-status")
    (general-run-call 
     "-set-state-status"
     "set state and status"
     (lambda (target runname keys keyvals)
       (operate-on 'set-state-status))))

(if (or (args:get-arg "-set-run-status")
	(args:get-arg "-get-run-status"))
    (general-run-call
     "-set-run-status"
     "set run status"
     (lambda (target runname keys keyvals)
       (let* ((runsdat  (rmt:get-runs-by-patt keys runname 
					(common:args-get-target)
					#f #f #f))
	      (header   (vector-ref runsdat 0))
	      (rows     (vector-ref runsdat 1)))
	 (if (null? rows)
	     (begin
	       (debug:print-info 0 #f "No matching run found.")
	       (exit 1))
	     (let* ((row      (car (vector-ref runsdat 1)))
		    (run-id   (db:get-value-by-header row header "id")))
	       (if (args:get-arg "-set-run-status")
		   (rmt:set-run-status run-id (args:get-arg "-set-run-status") msg: (args:get-arg "-m"))
		   (print (rmt:get-run-status run-id))
		   )))))))

;;======================================================================
;; Query runs
;;======================================================================

;; -fields runs:id,target,runname,comment+tests:id,testname,item_path+steps
;;
;; csi> (extract-fields-constraints "runs:id,target,runname,comment+tests:id,testname,item_path+steps")
;;         => (("runs" "id" "target" "runname" "comment") ("tests" "id" "testname" "item_path") ("steps"))
;;
;;   NOTE: remember that the cdr will be the list you expect (cdr ("runs" "id" "target" "runname" "comment")) => ("id" "target" "runname" "comment")
;;         and so alist-ref will yield what you expect
;;
(define (extract-fields-constraints fields-spec)
  (map (lambda (table-spec) ;; runs:id,target,runname
	 (let ((dat (string-split table-spec ":"))) ;; ("runs" "id,target,runname")
	   (if (> (length dat) 1)
	       (cons (car dat)(string-split (cadr dat) ",")) ;; "id,target,runname"
	       dat)))
       (string-split fields-spec "+")))

(define (get-value-by-fieldname datavec test-field-index fieldname)
  (let ((indx (hash-table-ref/default test-field-index fieldname #f)))
    (if indx
	(if (>= indx (vector-length datavec))
	    #f ;; index to high, should raise an error I suppose
	    (vector-ref datavec indx))
	#f)))

;; NOTE: list-runs and list-db-targets operate on local db!!!
;;
;; IDEA: megatest list -runname blah% ...
;;
(if (or (args:get-arg "-list-runs")
	(args:get-arg "-list-db-targets"))
    (if (launch:setup)
	(let* (;; (dbstruct    (make-dbr:dbstruct path: *toppath* local: (args:get-arg "-local")))
	       (runpatt     (args:get-arg "-list-runs"))
	       (testpatt    (common:args-get-testpatt #f))
	       ;; (if (args:get-arg "-testpatt") 
	       ;;  	        (args:get-arg "-testpatt") 
	       ;;  	        "%"))
	       (keys        (rmt:get-keys)) ;; (db:get-keys dbstruct))
	       ;; (runsda   t  (db:get-runs dbstruct runpatt #f #f '()))
	       (runsdat     (rmt:get-runs-by-patt keys (or runpatt "%") (common:args-get-target) ;; (db:get-runs-by-patt dbstruct keys (or runpatt "%") (common:args-get-target)
			           	 #f #f '("id" "runname" "state" "status" "owner" "event_time" "comment")))
	       (runstmp     (db:get-rows runsdat))
	       (header      (db:get-header runsdat))
	       ;; this is "-since" support. This looks at last mod times of <run-id>.db files
	       ;; and collects those modified since the -since time.
	       (runs        (if (and (not (null? runstmp))
				     (args:get-arg "-since"))
				(let ((changed-ids (db:get-changed-run-ids (string->number (args:get-arg "-since")))))
				  (let loop ((hed (car runstmp))
					     (tal (cdr runstmp))
					     (res '()))
				    (let ((new-res (if (member (db:get-value-by-header hed header "id") changed-ids)
						       (cons hed res)
						       res)))
				      (if (null? tal)
					  (reverse new-res)
					  (loop (car tal)(cdr tal) new-res)))))
				runstmp))
	       (db-targets  (args:get-arg "-list-db-targets"))
	       (seen        (make-hash-table))
	       (dmode       (let ((d (args:get-arg "-dumpmode")))
			      (if d (string->symbol d) #f)))
	       (data        (make-hash-table))
	       (fields-spec (if (args:get-arg "-fields")
				(extract-fields-constraints (args:get-arg "-fields"))
				(list (cons "runs" (append keys (list "id" "runname" "state" "status" "owner" "event_time" "comment" "fail_count" "pass_count")))
				      (cons "tests"  db:test-record-fields) ;; "id" "testname" "test_path")
				      (list "steps" "id" "stepname"))))
	       (runs-spec   (let ((r (alist-ref "runs"  fields-spec equal?))) ;; the check is now unnecessary
			      (if (and r (not (null? r))) r (list "id" ))))
	       (tests-spec  (let ((t (alist-ref "tests" fields-spec equal?)))
			      (if (and t (null? t)) ;; all fields
				  db:test-record-fields
				  t)))
	       (adj-tests-spec (delete-duplicates (if tests-spec (cons "id" tests-spec) db:test-record-fields))) ;; '("id"))))
	       (steps-spec  (alist-ref "steps" fields-spec equal?))
	       (test-field-index (make-hash-table)))
	  (if (and tests-spec (not (null? tests-spec))) ;; do some validation and processing of the test-spec
	      (let ((invalid-tests-spec (filter (lambda (x)(not (member x db:test-record-fields))) tests-spec)))
		(if (null? invalid-tests-spec)
		    ;; generate the lookup map test-field-name => index-number
		    (let loop ((hed (car adj-tests-spec))
			       (tal (cdr adj-tests-spec))
			       (idx 0))
		      (hash-table-set! test-field-index hed idx)
		      (if (not (null? tal))(loop (car tal)(cdr tal)(+ idx 1))))
		    (begin
		      (debug:print 0 #f "ERROR: Invalid test fields specified: " (string-intersperse invalid-tests-spec ", "))
		      (exit)))))

	  ;; Each run
	  (for-each 
	   (lambda (run)
	     (let ((targetstr (string-intersperse (map (lambda (x)
							 (db:get-value-by-header run header x))
						       keys) "/")))
	       (if db-targets
		   (if (not (hash-table-ref/default seen targetstr #f))
		       (begin
			 (hash-table-set! seen targetstr #t)
			 ;; (print "[" targetstr "]"))))
			 (if (not dmode)
			     (print targetstr)
			     (hash-table-set! data "targets" (cons targetstr (hash-table-ref/default data "targets" '())))
			     )))
		   (let* ((run-id  (db:get-value-by-header run header "id"))
			  (runname (db:get-value-by-header run header "runname")) 
			  (states  (string-split (or (args:get-arg "-state") "") ","))
			  (statuses (string-split (or (args:get-arg "-status") "") ","))
			  (tests   (if tests-spec
				       (rmt:get-tests-for-run run-id testpatt states statuses #f #f #f 'testname 'asc ;; (db:get-tests-for-run dbstruct run-id testpatt '() '() #f #f #f 'testname 'asc 
							     ;; use qryvals if test-spec provided
							     (if tests-spec
								 (string-intersperse adj-tests-spec ",")
								 ;; db:test-record-fields
								 #f)
							     #f
							     'normal)
				       '())))
		     (case dmode
		       ((json ods)
			(if runs-spec
			    (for-each 
			     (lambda (field-name)
			       (mutils:hierhash-set! data (conc (db:get-value-by-header run header field-name)) targetstr runname "meta" field-name))
			     runs-spec)))
			;; (mutils:hierhash-set! data (db:get-value-by-header run header "status")     targetstr runname "meta" "status"     )
			;; (mutils:hierhash-set! data (db:get-value-by-header run header "state")      targetstr runname "meta" "state"      )
			;; (mutils:hierhash-set! data (conc (db:get-value-by-header run header "id"))  targetstr runname "meta" "id"         )
			;; (mutils:hierhash-set! data (db:get-value-by-header run header "event_time") targetstr runname "meta" "event_time" )
			;; (mutils:hierhash-set! data (db:get-value-by-header run header "comment")    targetstr runname "meta" "comment"    )
			;; ;; add last entry twice - seems to be a bug in hierhash?
			;; (mutils:hierhash-set! data (db:get-value-by-header run header "comment")    targetstr runname "meta" "comment"    )
		       (else
			(if (null? runs-spec)
			    (print "Run: " targetstr "/" runname 
				   " status: " (db:get-value-by-header run header "state")
				   " run-id: " run-id ", number tests: " (length tests)
				   " event_time: " (db:get-value-by-header run header "event_time"))
			    (begin
			      (if (not (member "target" runs-spec))
			          ;; (display (conc "Target: " targetstr))
			          (display (conc "Run: " targetstr "/" runname " ")))
			      (for-each
			       (lambda (field-name)
				 (if (equal? field-name "target")
				     (display (conc "target: " targetstr " "))
				     (display (conc field-name ": " (db:get-value-by-header run header (conc field-name)) " "))))
			       runs-spec)
			      (newline)))))
		       
		     (for-each 
		      (lambda (test)
		      	(handle-exceptions
			 exn
			 (begin
			   (debug:print 0 #f "ERROR: Bad data in test record? " test)
			   (print "exn=" (condition->list exn))
			   (debug:print 0 #f " message: " ((condition-property-accessor 'exn 'message) exn))
			   (print-call-chain (current-error-port)))
			 (let* ((test-id      (if (member "id"           tests-spec)(get-value-by-fieldname test test-field-index "id"          ) #f)) ;; (db:test-get-id         test))
				(testname     (if (member "testname"     tests-spec)(get-value-by-fieldname test test-field-index "testname"    ) #f)) ;; (db:test-get-testname   test))
				(itempath     (if (member "item_path"    tests-spec)(get-value-by-fieldname test test-field-index "item_path"   ) #f)) ;; (db:test-get-item-path  test))
				(comment      (if (member "comment"      tests-spec)(get-value-by-fieldname test test-field-index "comment"     ) #f)) ;; (db:test-get-comment    test))
				(tstate       (if (member "state"        tests-spec)(get-value-by-fieldname test test-field-index "state"       ) #f)) ;; (db:test-get-state      test))
				(tstatus      (if (member "status"       tests-spec)(get-value-by-fieldname test test-field-index "status"      ) #f)) ;; (db:test-get-status     test))
				(event-time   (if (member "event_time"   tests-spec)(get-value-by-fieldname test test-field-index "event_time"  ) #f)) ;; (db:test-get-event_time test))
				(rundir       (if (member "rundir"       tests-spec)(get-value-by-fieldname test test-field-index "rundir"      ) #f)) ;; (db:test-get-rundir     test))
				(final_logf   (if (member "final_logf"   tests-spec)(get-value-by-fieldname test test-field-index "final_logf"  ) #f)) ;; (db:test-get-final_logf test))
				(run_duration (if (member "run_duration" tests-spec)(get-value-by-fieldname test test-field-index "run_duration") #f)) ;; (db:test-get-run_duration test))
				(fullname     (conc testname
						    (if (equal? itempath "")
							"" 
							(conc "(" itempath ")")))))
			   (case dmode
			     ((json ods)
			      (if tests-spec
				  (for-each
				   (lambda (field-name)
				     (mutils:hierhash-set! data  (get-value-by-fieldname test test-field-index field-name) targetstr runname "data" (conc test-id) field-name))
				   tests-spec)))
			     ;; ;; (mutils:hierhash-set! data  fullname   targetstr runname "data" (conc test-id) "tname"     )
			     ;;  (mutils:hierhash-set! data  testname   targetstr runname "data" (conc test-id) "testname"  )
			     ;;  (mutils:hierhash-set! data  itempath   targetstr runname "data" (conc test-id) "itempath"  )
			     ;;  (mutils:hierhash-set! data  comment    targetstr runname "data" (conc test-id) "comment"   )
			     ;;  (mutils:hierhash-set! data  tstate     targetstr runname "data" (conc test-id) "state"     )
			     ;;  (mutils:hierhash-set! data  tstatus    targetstr runname "data" (conc test-id) "status"    )
			     ;;  (mutils:hierhash-set! data  rundir     targetstr runname "data" (conc test-id) "rundir"    )
			     ;;  (mutils:hierhash-set! data  final_logf targetstr runname "data" (conc test-id) "final_logf")
			     ;;  (mutils:hierhash-set! data  run_duration targetstr runname "data" (conc test-id) "run_duration")
			     ;;  (mutils:hierhash-set! data  event-time targetstr runname "data" (conc test-id) "event_time")
			     ;;  ;; add last entry twice - seems to be a bug in hierhash?
			     ;;  (mutils:hierhash-set! data  event-time targetstr runname "data" (conc test-id) "event_time")
			     ;;  )
			     (else
			      (if (and tstate tstatus event-time)
				  (format #t
					  "  Test: ~25a State: ~15a Status: ~15a Runtime: ~5@as Time: ~22a Host: ~10a\n"
					  (if fullname fullname "")
					  (if tstate   tstate   "")
					  (if tstatus  tstatus  "")
					  (get-value-by-fieldname test test-field-index "run_duration");;(if test     (db:test-get-run_duration test) "")
					  (if event-time event-time "")
					  (get-value-by-fieldname test test-field-index "host")) ;;(if test (db:test-get-host test)) "")
				  (print "  Test: " fullname
					 (if tstate  (conc " State: "  tstate)  "")
					 (if tstatus (conc " Status: " tstatus) "")
					 (if (get-value-by-fieldname test test-field-index "run_duration")
					     (conc " Runtime: " (get-value-by-fieldname test test-field-index "run_duration"))
					     "")
					 (if event-time (conc " Time: " event-time) "")
					 (if (get-value-by-fieldname test test-field-index "host")
					     (conc " Host: " (get-value-by-fieldname test test-field-index "host"))
					     "")))
			      (if (not (or (equal? (get-value-by-fieldname test test-field-index "status") "PASS")
					   (equal? (get-value-by-fieldname test test-field-index "status") "WARN")
					   (equal? (get-value-by-fieldname test test-field-index "state")  "NOT_STARTED")))
				  (begin
				    (print   (if (get-value-by-fieldname test test-field-index "cpuload")
						 (conc "         cpuload:  "   (get-value-by-fieldname test test-field-index "cpuload"))
						 "") ;; (db:test-get-cpuload test)
					     (if (get-value-by-fieldname test test-field-index "diskfree")
						 (conc "\n         diskfree: " (get-value-by-fieldname test test-field-index "diskfree")) ;; (db:test-get-diskfree test)
						 "")
					     (if (get-value-by-fieldname test test-field-index "uname")
						 (conc "\n         uname:    " (get-value-by-fieldname test test-field-index "uname")) ;; (db:test-get-uname test)
						 "")
					     (if (get-value-by-fieldname test test-field-index "rundir")
						 (conc "\n         rundir:   " (get-value-by-fieldname test test-field-index "rundir")) ;; (db:test-get-rundir test)
						 "")
;;					     "\n         rundir:   " (get-value-by-fieldname test test-field-index "") ;; (sdb:qry 'getstr ;; (filedb:get-path *fdb* 
;; 					     (db:test-get-rundir test) ;; )
					     )
				    ;; Each test
				    ;; DO NOT remote run
				    (let ((steps (rmt:get-steps-for-test run-id (db:test-get-id test)))) ;; (db:get-steps-for-test dbstruct run-id (db:test-get-id test))))
				      (for-each 
				       (lambda (step)
					 (format #t 
						 "    Step: ~20a State: ~10a Status: ~10a Time ~22a\n"
						 (tdb:step-get-stepname step)
						 (tdb:step-get-state step)
						 (tdb:step-get-status step)
						 (tdb:step-get-event_time step)))
				       steps)))))))))
		      (if (args:get-arg "-sort")
			  (sort tests
				(lambda (a-test b-test)
				  (let* ((key    (args:get-arg "-sort"))
					 (first  (get-value-by-fieldname a-test test-field-index key))
					 (second (get-value-by-fieldname b-test test-field-index key)))
				    ((cond 
				      ((and (number? first)(number? second)) <)
				      ((and (string? first)(string? second)) string<=?)
				      (else equal?))
				     first second))))
			  tests))))))
	   runs)
	  (if (eq? dmode 'json)(json-write data))
	  (let* ((metadat-fields (delete-duplicates
				  (append keys '( "runname" "time" "owner" "pass_count" "fail_count" "state" "status" "comment" "id"))))
		 (run-fields    '(
				  "testname"
				  "item_path"
				  "state"
				  "status"
				  "comment"
				  "event_time"
				  "host"
				  "run_id"
				  "run_duration"
				  "attemptnum"
				  "id"
				  "archived"
				  "diskfree"
				  "cpuload"
				  "final_logf"
				  "shortdir"
				  "rundir"
				  "uname"
				  )
				)
		 (newdat          (common:to-alist data))
		 (allrundat       (if (null? newdat)
				      '()
				      (car (map cdr newdat)))) ;; (car (map cdr (car (map cdr newdat)))))
		 (runs            (append
				   (list "runs" ;; sheetname
					 metadat-fields)
				   (map (lambda (run)
					  ;; (print "run: " run)
					  (let* ((runname (car run))
						 (rundat  (cdr run))
						 (metadat (let ((tmp (assoc "meta" rundat)))
							    (if tmp (cdr tmp) #f))))
					    ;; (print "runname: " runname "\n\nrundat: " )(pp rundat)(print "\n\nmetadat: ")(pp metadat)
					    (if metadat
						(map (lambda (field)
						       (let ((tmp (assoc field metadat)))
							 (if tmp (cdr tmp) "")))
						     metadat-fields)
						(begin
						  (debug:print 0 #f "WARNING: meta data for run " runname " not found")
						  '()))))
					allrundat)))
		 ;; '( ( "target" ( "runname" ( "data" ( "runid" ( "id . "37" ) ( ... ))))
		 (run-pages      (map (lambda (targdat)
					(let* ((target  (car targdat))
					       (runsdat (cdr targdat)))
					  (if runsdat
					      (map (lambda (rundat)
						     (let* ((runname  (car rundat))
							    (rundat   (cdr rundat))
							    (testsdat (let ((tmp (assoc "data" rundat)))
									(if tmp (cdr tmp) #f))))
						       (if testsdat
							   (let ((tests (map (lambda (test)
									       (let* ((test-id  (car test))
										      (test-dat (cdr test)))
										 (map (lambda (field)
											(let ((tmp (assoc field test-dat)))
											  (if tmp (cdr tmp) "")))
										      run-fields)))
									     testsdat)))
							     ;; (print "Target: " target "/" runname " tests:")
							     ;; (pp tests)
							     (cons (conc target "/" runname)
								   (cons (list (conc target "/" runname))
									 (cons '()
									       (cons run-fields tests)))))
							   (begin
							     (debug:print 0 #f "WARNING: run " target "/" runname " appears to have no data")
							     ;; (pp rundat)
							     '()))))
						   runsdat)
					      '())))
				      newdat)) ;; we use newdat to get target
		 (sheets         (filter (lambda (x)
					   (not (null? x)))
					 (cons runs (map car run-pages)))))
	    ;; (print "allrundat:")
	    ;; (pp allrundat)
	    ;; (print "runs:")
	    ;; (pp runs)
	    ;(print "sheets: ")
	    ;; (pp sheets)
	    (if (eq? dmode 'ods)
		(let* ((tempdir    (conc "/tmp/" (current-user-name) "/" (random 10000) "_" (current-process-id)))
		       (outputfile (or (args:get-arg "-o") "out.ods"))
		       (ouf        (if (string-match (regexp "^[/~]+.*") outputfile) ;; full path?
				       outputfile
				       (begin
					 (debug:print 0 #f "WARNING: path given, " outputfile " is relative, prefixing with current directory")
					 (conc (current-directory) "/" outputfile)))))
		  (create-directory tempdir #t)
		  (ods:list->ods tempdir ouf sheets))))
	  ;; (system (conc "rm -rf " tempdir))
	  (set! *didsomething* #t))))

;; Don't think I need this. Incorporated into -list-runs instead
;;
;; (if (and (args:get-arg "-since")
;; 	 (launch:setup))
;;     (let* ((since-time (string->number (args:get-arg "-since")))
;; 	   (run-ids    (db:get-changed-run-ids since-time)))
;;       ;; (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
;;       (print (sort run-ids <))
;;       (set! *didsomething* #t)))
      
      
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
(if (or (args:get-arg "-runall")
	(args:get-arg "-run")
	(args:get-arg "-rerun-clean")
	(args:get-arg "-runtests"))
    (general-run-call 
     "-runall"
     "run all tests"
     (lambda (target runname keys keyvals)
       (if (args:get-arg "-rerun-clean") ;; first set states/statuses correct
	   (let ((states   (or (configf:lookup *configdat* "validvalues" "cleanrerun-states")
			       "KILLREQ,KILLED,UNKNOWN,INCOMPLETE,STUCK,NOT_STARTED"))
		 (statuses (or (configf:lookup *configdat* "validvalues" "cleanrerun-statuses")
			       "FAIL,INCOMPLETE,ABORT,CHECK")))
	     (hash-table-set! args:arg-hash "-preclean" #t)
	     (runs:operate-on 'set-state-status
			      target
			      (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
			      "%" ;; (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
			      state:  states
			      ;; status: statuses
			      new-state-status: "NOT_STARTED,n/a")
	     (runs:operate-on 'set-state-status
			      target
			      (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
			      "%" ;; (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
			      ;; state:  states
			      status: statuses
			      new-state-status: "NOT_STARTED,n/a")))
       (runs:run-tests target
		       runname
		       #f ;; (common:args-get-testpatt #f)
		       ;; (or (args:get-arg "-testpatt")
		       ;;     "%")
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

;; == duplicated == (if (or (args:get-arg "-run")(args:get-arg "-runtests"))
;; == duplicated ==   (general-run-call 
;; == duplicated ==    "-runtests" 
;; == duplicated ==    "run a test" 
;; == duplicated ==    (lambda (target runname keys keyvals)
;; == duplicated ==      ;;
;; == duplicated ==      ;; May or may not implement it this way ...
;; == duplicated ==      ;;
;; == duplicated ==      ;; Insert this run into the tasks queue
;; == duplicated ==      ;; (open-run-close tasks:add tasks:open-db 
;; == duplicated ==      ;;    	     "runtests" 
;; == duplicated ==      ;;    	     user
;; == duplicated ==      ;;    	     target
;; == duplicated ==      ;;    	     runname
;; == duplicated ==      ;;    	     (args:get-arg "-runtests")
;; == duplicated ==      ;;    	     #f))))
;; == duplicated ==      (runs:run-tests target
;; == duplicated == 		     runname
;; == duplicated == 		     (common:args-get-testpatt #f) ;; (args:get-arg "-runtests")
;; == duplicated == 		     user
;; == duplicated == 		     args:arg-hash))))

;;======================================================================
;; Rollup into a run
;;======================================================================

(if (args:get-arg "-rollup")
    (general-run-call 
     "-rollup" 
     "rollup tests" 
     (lambda (target runname keys keyvals)
       (runs:rollup-run keys
			keyvals
			(or (args:get-arg "-runname")(args:get-arg ":runname") )
			user))))

;;======================================================================
;; Lock or unlock a run
;;======================================================================

(if (or (args:get-arg "-lock")(args:get-arg "-unlock"))
    (general-run-call 
     (if (args:get-arg "-lock") "-lock" "-unlock")
     "lock/unlock tests" 
     (lambda (target runname keys keyvals)
       (runs:handle-locking 
		  target
		  keys
		  (or (args:get-arg "-runname")(args:get-arg ":runname") )
		  (args:get-arg "-lock")
		  (args:get-arg "-unlock")
		  user))))

;;======================================================================
;; Get paths to tests
;;======================================================================
;; Get test paths matching target, runname, and testpatt
(if (or (args:get-arg "-test-files")(args:get-arg "-test-paths"))
    ;; if we are in a test use the MT_CMDINFO data
    (if (getenv "MT_CMDINFO")
	(let* ((startingdir (current-directory))
	       (cmdinfo   (common:read-encoded-string (getenv "MT_CMDINFO")))
	       (transport (assoc/default 'transport cmdinfo))
	       (testpath  (assoc/default 'testpath  cmdinfo))
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (db-host   (assoc/default 'db-host   cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (state     (args:get-arg ":state"))
	       (status    (args:get-arg ":status"))
	       (target    (args:get-arg "-target"))
	       (toppath   (assoc/default 'toppath   cmdinfo)))
	  (change-directory toppath)
	  (if (not target)
	      (begin
		(debug:print 0 #f "ERROR: -target is required.")
		(exit 1)))
	  (if (not (launch:setup))
	      (begin
		(debug:print 0 #f "Failed to setup, giving up on -test-paths or -test-files, exiting")
		(exit 1)))
	  (let* ((keys     (rmt:get-keys))
		 ;; db:test-get-paths must not be run remote
		 (paths    (tests:test-get-paths-matching keys target (args:get-arg "-test-files"))))
	    (set! *didsomething* #t)
	    (for-each (lambda (path)
			(print path))
		      paths)))
	;; else do a general-run-call
	(general-run-call 
	 "-test-files"
	 "Get paths to test"
	 (lambda (target runname keys keyvals)
	   (let* ((db       #f)
		  ;; DO NOT run remote
		  (paths    (tests:test-get-paths-matching keys target (args:get-arg "-test-files"))))
	     (for-each (lambda (path)
			 (print path))
		       paths))))))

;;======================================================================
;; Archive tests
;;======================================================================
;; Archive tests matching target, runname, and testpatt
(if (args:get-arg "-archive")
    ;; else do a general-run-call
    (general-run-call 
     "-archive"
     "Archive"
     (lambda (target runname keys keyvals)
       (operate-on 'archive))))

;;======================================================================
;; Extract a spreadsheet from the runs database
;;======================================================================

(if (args:get-arg "-extract-ods")
    (general-run-call
     "-extract-ods"
     "Make ods spreadsheet"
     (lambda (target runname keys keyvals)
       (let ((dbstruct   (make-dbr:dbstruct path: *toppath* local: #t))
	     (outputfile (args:get-arg "-extract-ods"))
	     (runspatt   (or (args:get-arg "-runname")(args:get-arg ":runname")))
	     (pathmod    (args:get-arg "-pathmod")))
	     ;; (keyvalalist (keys->alist keys "%")))
	 (debug:print 2 #f "Extract ods, outputfile: " outputfile " runspatt: " runspatt " keyvals: " keyvals)
	 (db:extract-ods-file dbstruct outputfile keyvals (if runspatt runspatt "%") pathmod)
	 (db:close-all dbstruct)
	 (set! *didsomething* #t)))))

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
;; recover from a test where the managing mtest was killed but the underlying
;; process might still be salvageable
;;======================================================================

(if (args:get-arg "-recover-test")
    (let* ((params (string-split (args:get-arg "-recover-test") ",")))
      (if (> (length params) 1) ;; run-id and test-id
	  (let ((run-id (string->number (car params)))
		(test-id (string->number (cadr params))))
	    (if (and run-id test-id)
		(begin
		  (launch:recover-test run-id test-id)
		  (set! *didsomething* #t))
		(begin
		  (debug:print 0 #f "ERROR: bad run-id or test-id, must be integers")
		  (exit 1)))))))

;;======================================================================
;; Test commands (i.e. for use inside tests)
;;======================================================================

(define (megatest:step step state status logfile msg)
  (if (not (getenv "MT_CMDINFO"))
      (begin
	(debug:print 0 #f "ERROR: MT_CMDINFO env var not set, -step must be called *inside* a megatest invoked environment!")
	(exit 5))
      (let* ((cmdinfo   (common:read-encoded-string (getenv "MT_CMDINFO")))
	     (transport (assoc/default 'transport cmdinfo))
	     (testpath  (assoc/default 'testpath  cmdinfo))
	     (test-name (assoc/default 'test-name cmdinfo))
	     (runscript (assoc/default 'runscript cmdinfo))
	     (db-host   (assoc/default 'db-host   cmdinfo))
	     (run-id    (assoc/default 'run-id    cmdinfo))
	     (test-id   (assoc/default 'test-id   cmdinfo))
	     (itemdat   (assoc/default 'itemdat   cmdinfo))
	     (work-area (assoc/default 'work-area cmdinfo))
	     (db        #f))
	(change-directory testpath)
	(if (not (launch:setup))
	    (begin
	      (debug:print 0 #f "Failed to setup, exiting")
	      (exit 1)))
	(if (and state status)
	    (let ((comment (launch:load-logpro-dat run-id test-id step)))
	      ;; (rmt:test-set-log! run-id test-id (conc stepname ".html"))))
	      (rmt:teststep-set-status! run-id test-id step state status (or comment msg) logfile))
	    (begin
	      (debug:print 0 #f "ERROR: You must specify :state and :status with every call to -step")
	      (exit 6))))))

(if (args:get-arg "-step")
    (begin
      (megatest:step 
       (args:get-arg "-step")
       (or (args:get-arg "-state")(args:get-arg ":state"))
       (or (args:get-arg "-status")(args:get-arg ":status"))
       (args:get-arg "-setlog")
       (args:get-arg "-m"))
      ;; (if db (sqlite3:finalize! db))
      (set! *didsomething* #t)))
    
(if (or (args:get-arg "-setlog")       ;; since setting up is so costly lets piggyback on -test-status
	;;     (not (args:get-arg "-step")))  ;; -setlog may have been processed already in the "-step" previous
	;;     NEW POLICY - -setlog sets test overall log on every call.
	(args:get-arg "-set-toplog")
	(args:get-arg "-test-status")
	(args:get-arg "-set-values")
	(args:get-arg "-load-test-data")
	(args:get-arg "-runstep")
	(args:get-arg "-summarize-items"))
    (if (not (getenv "MT_CMDINFO"))
	(begin
	  (debug:print 0 #f "ERROR: MT_CMDINFO env var not set, commands -test-status, -runstep and -setlog must be called *inside* a megatest environment!")
	  (exit 5))
	(let* ((startingdir (current-directory))
	       (cmdinfo   (common:read-encoded-string (getenv "MT_CMDINFO")))
	       (transport (assoc/default 'transport cmdinfo))
	       (testpath  (assoc/default 'testpath  cmdinfo))
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (db-host   (assoc/default 'db-host   cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (test-id   (assoc/default 'test-id   cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (work-area (assoc/default 'work-area cmdinfo))
	       (db        #f) ;; (open-db))
	       (state     (args:get-arg ":state"))
	       (status    (args:get-arg ":status"))
	       (stepname  (args:get-arg "-step")))
	  (if (not (launch:setup))
	      (begin
		(debug:print 0 #f "Failed to setup, exiting")
		(exit 1)))

	  (if (args:get-arg "-runstep")(debug:print-info 1 #f "Running -runstep, first change to directory " work-area))
	  (change-directory work-area)
	  ;; can setup as client for server mode now
	  ;; (client:setup)

	  (if (args:get-arg "-load-test-data")
	      ;; has sub commands that are rdb:
	      ;; DO NOT put this one into either rmt: or open-run-close
	      (tdb:load-test-data run-id test-id))
	  (if (args:get-arg "-setlog")
	      (let ((logfname (args:get-arg "-setlog")))
		(rmt:test-set-log! run-id test-id logfname)))
	  (if (args:get-arg "-set-toplog")
	      ;; DO NOT run remote
	      (tests:test-set-toplog! run-id test-name (args:get-arg "-set-toplog")))
	  (if (args:get-arg "-summarize-items")
	      ;; DO NOT run remote
	      (tests:summarize-items run-id test-id test-name #t)) ;; do force here
	  (if (args:get-arg "-runstep")
	      (if (null? remargs)
		  (begin
		    (debug:print 0 #f "ERROR: nothing specified to run!")
		    (if db (sqlite3:finalize! db))
		    (exit 6))
		  (let* ((stepname   (args:get-arg "-runstep"))
			 (logprofile (args:get-arg "-logpro"))
			 (logfile    (conc stepname ".log"))
			 (cmd        (if (null? remargs) #f (car remargs)))
			 (params     (if cmd (cdr remargs) '()))
			 (exitstat   #f)
			 (shell      (let ((sh (get-environment-variable "SHELL") ))
				       (if sh 
					   (last (string-split sh "/"))
					   "bash")))
			 (redir      (case (string->symbol shell)
				       ((tcsh csh ksh)    ">&")
				       ((zsh bash sh ash) "2>&1 >")
				       (else ">&")))
			 (fullcmd    (conc "(" (string-intersperse 
						(cons cmd params) " ")
					   ") " redir " " logfile)))
		    ;; mark the start of the test
		    (rmt:teststep-set-status! run-id test-id stepname "start" "n/a" (args:get-arg "-m") logfile)
		    ;; run the test step
		    (debug:print-info 2 #f "Running \"" fullcmd "\" in directory \"" startingdir)
		    (change-directory startingdir)
		    (set! exitstat (system fullcmd))
		    (set! *globalexitstatus* exitstat)
		    ;; (change-directory testpath)
		    ;; run logpro if applicable ;; (process-run "ls" (list "/foo" "2>&1" "blah.log"))
		    (if logprofile
			(let* ((htmllogfile (conc stepname ".html"))
			       (oldexitstat exitstat)
			       (cmd         (string-intersperse (list "logpro" logprofile htmllogfile "<" logfile ">" (conc stepname "_logpro.log")) " ")))
			  (debug:print-info 2 #f "running \"" cmd "\"")
			  (change-directory startingdir)
			  (set! exitstat (system cmd))
			  (set! *globalexitstatus* exitstat) ;; no necessary
			  (change-directory testpath)
			  (rmt:test-set-log! run-id test-id htmllogfile)))
		    (let ((msg (args:get-arg "-m")))
		      (rmt:teststep-set-status! run-id test-id stepname "end" exitstat msg logfile))
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
		      (debug:print 0 #f "ERROR: You must specify :state and :status with every call to -test-status\n" help)
		      (if (sqlite3:database? db)(sqlite3:finalize! db))
		      (exit 6)))
		(let* ((msg    (args:get-arg "-m"))
		       (numoth (length (hash-table-keys otherdata))))
		  ;; Convert to rpc inside the tests:test-set-status! call, not here
		  (tests:test-set-status! run-id test-id state newstatus msg otherdata work-area: work-area))))
	  (if (sqlite3:database? db)(sqlite3:finalize! db))
	  (set! *didsomething* #t))))

;;======================================================================
;; Various helper commands can go below here
;;======================================================================

(if (or (args:get-arg "-showkeys")
        (args:get-arg "-show-keys"))
    (let ((db #f)
	  (keys #f))
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting")
	    (exit 1)))
      (set! keys (rmt:get-keys)) ;;  db))
      (debug:print 1 #f "Keys: " (string-intersperse keys ", "))
      (if (sqlite3:database? db)(sqlite3:finalize! db))
      (set! *didsomething* #t)))

(if (args:get-arg "-gui")
    (begin
      (debug:print 0 #f "Look at the dashboard for now")
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
;; Update the database schema, clean up the db
;;======================================================================

(if (args:get-arg "-rebuild-db")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting") 
	    (exit 1)))
      ;; keep this one local
      (open-run-close patch-db #f)
      (set! *didsomething* #t)))

(if (args:get-arg "-cleanup-db")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting") 
	    (exit 1)))
      ;; keep this one local
      ;; (open-run-close db:clean-up #f)
      (db:multi-db-sync 
       #f ;; do all run-ids
       ;; 'new2old
       'killservers
       'dejunk
       ;; 'adj-testids
       ;; 'old2new
       'new2old
       )
      (if (common:version-changed?)
	  (common:set-last-run-version))
      (set! *didsomething* #t)))

(if (args:get-arg "-mark-incompletes")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting")
	    (exit 1)))
      (open-run-close db:find-and-mark-incomplete #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Update the tests meta data from the testconfig files
;;======================================================================

(if (args:get-arg "-update-meta")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting") 
	    (exit 1)))
      ;; now can find our db
      ;; keep this one local
      (open-run-close runs:update-all-test_meta #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Start a repl
;;======================================================================

;; fakeout readline

(if (or (getenv "MT_RUNSCRIPT")
	(args:get-arg "-repl")
	(args:get-arg "-load"))
    (let* ((toppath (launch:setup))
	   (dbstruct (if toppath (make-dbr:dbstruct path: toppath local: (args:get-arg "-local")) #f)))
      (if dbstruct
	  (cond
	   ((getenv "MT_RUNSCRIPT")
	    ;; How to run megatest scripts
	    ;;
	    ;; #!/bin/bash
	    ;;
	    ;; export MT_RUNSCRIPT=yes
	    ;; megatest << EOF
	    ;; (print "Hello world")
	    ;; (exit)
	    ;; EOF

	    (repl))
	   (else
	    (begin
	      (set! *db* dbstruct)
	      (set! *client-non-blocking-mode* #t)
	      (import extras) ;; might not be needed
	      ;; (import csi)
	      (import readline)
	      (import apropos)
	      ;; (import (prefix sqlite3 sqlite3:)) ;; doesn't work ...
	      (include "readline-fix.scm")
	      (gnu-history-install-file-manager
	       (string-append
		(or (get-environment-variable "HOME") ".") "/.megatest_history"))
	      (current-input-port (make-gnu-readline-port "megatest> "))
	      (if (args:get-arg "-repl")
		  (repl)
		  (load (args:get-arg "-load")))
	      (db:close-all dbstruct))
	    (exit)))
	  (set! *didsomething* #t))))

;;======================================================================
;; Wait on a run to complete
;;======================================================================

(if (and (args:get-arg "-run-wait")
	 (not (or (args:get-arg "-run")
		  (args:get-arg "-runtests")))) ;; run-wait is built into runtests now
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 #f "Failed to setup, exiting") 
	    (exit 1)))
      (operate-on 'run-wait)
      (set! *didsomething* #t)))

;; ;; ;; redo me ;; Not converted to use dbstruct yet
;; ;; ;; redo me ;;
;; ;; ;; redo me (if (args:get-arg "-convert-to-norm")
;; ;; ;; redo me     (let* ((toppath (setup-for-run))
;; ;; ;; redo me 	   (dbstruct (if toppath (make-dbr:dbstruct path: toppath local: #t))))
;; ;; ;; redo me       (for-each 
;; ;; ;; redo me        (lambda (field)
;; ;; ;; redo me 	 (let ((dat '()))
;; ;; ;; redo me 	   (debug:print-info 0 #f "Getting data for field " field)
;; ;; ;; redo me 	   (sqlite3:for-each-row
;; ;; ;; redo me 	    (lambda (id val)
;; ;; ;; redo me 	      (set! dat (cons (list id val) dat)))
;; ;; ;; redo me 	    (db:get-db db run-id)
;; ;; ;; redo me 	    (conc "SELECT id," field " FROM tests;"))
;; ;; ;; redo me 	   (debug:print-info 0 #f "found " (length dat) " items for field " field)
;; ;; ;; redo me 	   (let ((qry (sqlite3:prepare db (conc "UPDATE tests SET " field "=? WHERE id=?;"))))
;; ;; ;; redo me 	     (for-each
;; ;; ;; redo me 	      (lambda (item)
;; ;; ;; redo me 		(let ((newval ;; (sdb:qry 'getid 
;; ;; ;; redo me 		       (cadr item))) ;; )
;; ;; ;; redo me 		  (if (not (equal? newval (cadr item)))
;; ;; ;; redo me 		      (debug:print-info 0 #f "Converting " (cadr item) " to " newval " for test #" (car item)))
;; ;; ;; redo me 		  (sqlite3:execute qry newval (car item))))
;; ;; ;; redo me 	      dat)
;; ;; ;; redo me 	     (sqlite3:finalize! qry))))
;; ;; ;; redo me        (db:close-all dbstruct)
;; ;; ;; redo me        (list "uname" "rundir" "final_logf" "comment"))
;; ;; ;; redo me       (set! *didsomething* #t)))

(if (args:get-arg "-import-megatest.db")
    (begin
      (db:multi-db-sync 
       #f ;; do all run-ids
       'killservers
       'dejunk
       'adj-testids
       'old2new
       ;; 'new2old
       )
      (set! *didsomething* #t)))

(if (args:get-arg "-sync-to-megatest.db")
    (begin
      (db:multi-db-sync 
       #f ;; do all run-ids
       'new2old
       )
      (set! *didsomething* #t)))

;;======================================================================
;; Exit and clean up
;;======================================================================

(if *runremote* (close-all-connections!))

(if (not *didsomething*)
    (debug:print 0 #f help))

(set! *time-to-exit* #t)
(thread-join! *watchdog*)

(if (not (eq? *globalexitstatus* 0))
    (if (or (args:get-arg "-run")(args:get-arg "-runtests")(args:get-arg "-runall"))
        (begin
           (debug:print 0 #f "NOTE: Subprocesses with non-zero exit code detected: " *globalexitstatus*)
           (exit 0))
        (case *globalexitstatus*
         ((0)(exit 0))
         ((1)(exit 1))
         ((2)(exit 2))
         (else (exit 3)))))
