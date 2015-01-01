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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 base64 format readline apropos json http-client directory-utils rpc ;; (srfi 18) extras)
     http-client srfi-18) ;;  zmq extras)

;; Added for csv stuff - will be removed
;;
(use sparse-vectors)

(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))
(import (prefix rpc rpc:))

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
  license GPL, Copyright Matt Welland 2006-2012

Usage: megatest [options]
  -h                      : this help
  -version                : print megatest version (currently " megatest-version ")

Launching and managing runs
  -runall                 : run all tests that are not state COMPLETED and status PASS, 
                            CHECK or KILLED
  -runtests tst1,tst2 ... : run tests
  -remove-runs            : remove the data for a run, requires -runname and -testpatt
                            Optionally use :state and :status
  -set-state-status X,Y   : set state to X and status to Y, requires controls per -remove-runs
  -rerun FAIL,WARN...     : force re-run for tests with specificed status(s)
  -lock                   : lock run specified by target and runname
  -unlock                 : unlock run specified by target and runname
  -set-run-status status  : sets status for run to status, requires -target and -runname
  -get-run-status         : gets status for run specified by target and runname
  -run-wait               : wait on run specified by target and runname
  -preclean               : remove the existing test directory before running the test

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
  -dumpmode json          : dump in json format instead of sexpr
  -show-cmdinfo           : dump the command info for a test (run in test environment)
  -section sectionName
  -var varName            : for config and runconfig lookup value for sectionName varName

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

Utilities
  -env2file fname         : write the environment to fname.csh and fname.sh
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
			;; misc
			"-repl"
			"-lock"
			"-unlock"
			"-list-servers"
                        "-run-wait"      ;; wait on a run to complete (i.e. no RUNNING)

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

			"-runall"    ;; run all tests
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

;; The watchdog is to keep an eye on things like db sync etc.
;;
(define *time-zero* (current-seconds))
(define *watchdog*
  (make-thread 
   (lambda ()
     (thread-sleep! 0.05) ;; delay for startup
     (let ((legacy-sync (configf:lookup *configdat* "setup" "megatest-db"))
	   (debug-mode  (debug:debug-mode 1))
	   (last-time   (current-seconds)))
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
		    (if (common:low-noise-print 30 "sync new to old")
			(let ((sync-time (- (current-seconds) start-time)))
			  (debug:print-info 0 "Sync of newdb to olddb for run-id " run-id " completed in " sync-time " seconds")))
		    ;; (if (> sync-time 10) ;; took more than ten seconds, start a server for this run
		    ;;     (begin
		    ;;       (debug:print-info 0 "Sync is taking a long time, start up a server to assist for run " run-id)
		    ;;       (server:kind-run run-id)))))
		    (hash-table-delete! *db-local-sync* run-id)))
	      (mutex-unlock! *db-multi-sync-mutex*))
	    (hash-table-keys *db-local-sync*))
	   (if (and debug-mode
		    (> (- start-time last-time) 60))
	       (begin
		 (set! last-time start-time)
		 (debug:print-info 1 "timestamp -> " (seconds->time-string (current-seconds)) ", time since start -> " (seconds->hr-min-sec (- (current-seconds) *time-zero*))))))
	 
	 ;; keep going unless time to exit
	 ;;
	 (if (not *time-to-exit*)
	     (let delay-loop ((count 0))
	       (if (and (not *time-to-exit*)
			(< count 11)) ;; aprox 5-6 seconds
		   (begin
		     (thread-sleep! 1)
		     (delay-loop (+ count 1))))
	       (loop))))))
   "Watchdog thread"))

(thread-start! *watchdog*)

(if (args:get-arg "-log")
    (let ((oup (open-output-file (args:get-arg "-log"))))
      (debug:print-info 0 "Sending log output to " (args:get-arg "-log"))
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
	  (debug:print 0 "ERROR: non-existant start dir " (args:get-arg "-start-dir") " specified, exiting.")
	  (exit 1))))

(if (args:get-arg "-version")
    (begin
      (print megatest-version)
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
      (debug:print 0 "WARNING: -itempatt has been deprecated, please use -testpatt testpatt/itempatt method, new testpatt is "newval)
      (hash-table-set! args:arg-hash "-testpatt" newval)
      (hash-table-delete! args:arg-hash "-itempatt")))

(on-exit std-exit-procedure)

;;======================================================================
;; Misc general calls
;;======================================================================

(if (args:get-arg "-env2file")
    (begin
      (save-environment-as-files (args:get-arg "-env2file"))
      (set! *didsomething* #t)))

(if (args:get-arg "-list-disks")
    (let ((toppath (launch:setup-for-run)))
      (print 
       (string-intersperse 
	(map (lambda (x)
	       (string-intersperse 
		x
		" => "))
	     (common:get-disks *configdat*))
	"\n"))
      (set! *didsomething* #t)))

(define (make-sparse-array)
  (let ((a (make-sparse-vector)))
    (sparse-vector-set! a 0 (make-sparse-vector))
    a))

(define (sparse-array? a)
  (and (sparse-vector? a)
       (sparse-vector? (sparse-vector-ref a 0))))

(define (sparse-array-ref a x y)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-ref row y)
	#f)))

(define (sparse-array-set! a x y val)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-set! row y val)
	(let ((new-row (make-sparse-vector)))
	  (sparse-vector-set! a x new-row)
	  (sparse-vector-set! new-row y val)))))

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
	  (debug:print 0 "Bad input? data=" data) ;; some error occurred
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

;;       (set! *did-something* #t)
;; 	      (begin
;; 		(print ((rpc:procedure 'testing (car host-port)(cadr host-port))))
;; 		(case (server:get-transport)
;; 		  ((http)(http:ping run-id host-port))
;; 		  ((rpc) (rpc:procedure 'server:login (car host-port)(cadr host-port));;  *toppath*)) ;; (rpc-transport:ping  run-id (car host-port)(cadr host-port)))
;; 		  (else  (debug:print 0 "ERROR: No transport set")(exit)))))

;;======================================================================
;; Start the server - can be done in conjunction with -runall or -runtests (one day...)
;;   we start the server if not running else start the client thread
;;======================================================================

(if (args:get-arg "-server")

    ;; Server? Start up here.
    ;;
    (let ((tl        (launch:setup-for-run))
	  (run-id    (and (args:get-arg "-run-id")
			  (string->number (args:get-arg "-run-id")))))
      (if run-id
	  (begin
	    (server:launch run-id)
	    (set! *didsomething* #t))
	  (debug:print 0 "ERROR: server requires run-id be specified with -run-id")))

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
	(if (launch:setup-for-run)
	    (let ((run-id    (and (args:get-arg "-run-id")
				  (string->number (args:get-arg "-run-id")))))
	      ;; (set! *fdb*   (filedb:open-db (conc *toppath* "/db/paths.db")))
	      ;; if not list or kill then start a client (if appropriate)
	      (if (or (args-defined? "-h" "-version" "-gen-megatest-area" "-gen-megatest-test")
		      (eq? (length (hash-table-keys args:arg-hash)) 0))
		  (debug:print-info 1 "Server connection not needed")
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
    (let ((tl (launch:setup-for-run)))
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
		       (debug:print-info 0 "Attempting to stop server with pid " pid)
		       (tasks:kill-server status hostname pullport pid transport)))))
	     servers)
	    (debug:print-info 1 "Done with listservers")
	    (set! *didsomething* #t)
	    (exit)) ;; must do, would have to add checks to many/all calls below
	  (exit))))

;;======================================================================
;; Weird special calls that need to run *after* the server has started?
;;======================================================================

(if (args:get-arg "-list-targets")
    (let ((targets (common:get-runconfig-targets)))
      (print "Found "(length targets) " targets")
      (for-each (lambda (x)
		  ;; (print "[" x "]"))
		  (print x))
		targets)
      (set! *didsomething* #t)))

(define (full-runconfigs-read)
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
    data))


(if (args:get-arg "-show-runconfig")
    (let ((tl (launch:setup-for-run)))
      (push-directory *toppath*)
      (let ((data (full-runconfigs-read)))
	;; keep this one local
	(cond
	 ((and (args:get-arg "-section")
	       (args:get-arg "-var"))
	  (let ((val (configf:lookup data (args:get-arg "-section")(args:get-arg "-var"))))
	    (if val (print val))))
	 ((not (args:get-arg "-dumpmode"))
	  (pp (hash-table->alist data)))
	 ((string=? (args:get-arg "-dumpmode") "json")
	(json-write data))
	 (else
	  (debug:print 0 "ERROR: -dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
	(set! *didsomething* #t))
      (pop-directory)))

(if (args:get-arg "-show-config")
    (let ((tl   (launch:setup-for-run))
	  (data *configdat*)) ;; (read-config "megatest.config" #f #t)))
      (push-directory *toppath*)
      ;; keep this one local
      (cond 
       ((and (args:get-arg "-section")
	     (args:get-arg "-var"))
	(let ((val (configf:lookup data (args:get-arg "-section")(args:get-arg "-var"))))
	  (if val (print val))))
       ((not (args:get-arg "-dumpmode"))
	(pp (hash-table->alist data)))
       ((string=? (args:get-arg "-dumpmode") "json")
	(json-write data))
       (else
	(debug:print 0 "ERROR: -dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
      (set! *didsomething* #t)
      (pop-directory)))

(if (args:get-arg "-show-cmdinfo")
    (if (getenv "MT_CMDINFO")
	(let ((data (common:read-encoded-string (getenv "MT_CMDINFO"))))
	  (if (equal? (args:get-arg "-dumpmode") "json")
	      (json-write data)
	      (pp data))
	  (set! *didsomething* #t))
	(debug:print-info 0 "environment variable MT_CMDINFO is not set")))

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
      (debug:print 0 "ERROR: Missing required parameter for " action ", you must specify -target or -reqtarg")
      (exit 1))
     ((not (or (args:get-arg ":runname")
	       (args:get-arg "-runname")))
      (debug:print 0 "ERROR: Missing required parameter for " action ", you must specify the run name pattern with -runname patt")
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
			    target
			    (or (args:get-arg "-runname")(args:get-arg ":runname"))
			    (args:get-arg "-testpatt")
			    state: (or (args:get-arg "-state")(args:get-arg ":state") )
			    status: (or (args:get-arg "-status")(args:get-arg ":status"))
			    new-state-status: (args:get-arg "-set-state-status")))
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
					#f #f))
	      (header   (vector-ref runsdat 0))
	      (rows     (vector-ref runsdat 1)))
	 (if (null? rows)
	     (begin
	       (debug:print-info 0 "No matching run found.")
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

;; NOTE: list-runs and list-db-targets operate on local db!!!
;;
(if (or (args:get-arg "-list-runs")
	(args:get-arg "-list-db-targets"))
    (if (launch:setup-for-run)
	(let* ((dbstruct (make-dbr:dbstruct path: *toppath* local: #t))
	       (runpatt  (args:get-arg "-list-runs"))
	       (testpatt (if (args:get-arg "-testpatt") 
			     (args:get-arg "-testpatt") 
			     "%"))
	       (keys     (db:get-keys dbstruct))
	       ;; (runsdat  (db:get-runs dbstruct runpatt #f #f '()))
	       (runsdat  (db:get-runs-by-patt dbstruct keys (or runpatt "%") (common:args-get-target)
					 #f #f))
		;; (cdb:remote-run db:get-runs #f runpatt #f #f '()))
	       (runs     (db:get-rows runsdat))
	       (header   (db:get-header runsdat))
	       (db-targets (args:get-arg "-list-db-targets"))
	       (seen     (make-hash-table)))
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
			 (print targetstr))))
	       (if (not db-targets)
		   (let* ((run-id (db:get-value-by-header run header "id"))
			  (tests  (db:get-tests-for-run dbstruct run-id testpatt '() '() #f #f #f 'testname 'asc #f)))
		     (print "Run: " targetstr "/" (db:get-value-by-header run header "runname") 
			    " status: " (db:get-value-by-header run header "state")
			    " run-id: " run-id ", number tests: " (length tests))
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
			      (print   "         cpuload:  " (db:test-get-cpuload test)
				     "\n         diskfree: " (db:test-get-diskfree test)
				     "\n         uname:    " ;; (sdb:qry 'getstr 
				     (db:test-get-uname test) ;; )
				     "\n         rundir:   " ;; (sdb:qry 'getstr ;; (filedb:get-path *fdb* 
				     (db:test-get-rundir test) ;; )
				     )
			      ;; Each test
			      ;; DO NOT remote run
			      (let ((steps (db:get-steps-for-test dbstruct run-id (db:test-get-id test))))
				(for-each 
				 (lambda (step)
				   (format #t 
					   "    Step: ~20a State: ~10a Status: ~10a Time ~22a\n"
					   (tdb:step-get-stepname step)
					   (tdb:step-get-state step)
					   (tdb:step-get-status step)
					   (tdb:step-get-event_time step)))
				 steps)))))
		      tests)))))
	     runs)
	  ;; (db:close-all dbstruct)
	  (set! *didsomething* #t))))

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
     (lambda (target runname keys keyvals)
       (runs:run-tests target
		       runname
		       (args:get-arg "-testpatt")
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
   (lambda (target runname keys keyvals)
     ;;
     ;; May or may not implement it this way ...
     ;;
     ;; Insert this run into the tasks queue
     ;; (open-run-close tasks:add tasks:open-db 
     ;;    	     "runtests" 
     ;;    	     user
     ;;    	     target
     ;;    	     runname
     ;;    	     (args:get-arg "-runtests")
     ;;    	     #f))))
     (runs:run-tests target
		     runname
		     (args:get-arg "-runtests")
		     user
		     args:arg-hash))))

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
		(debug:print 0 "ERROR: -target is required.")
		(exit 1)))
	  (if (not (launch:setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, giving up on -test-paths or -test-files, exiting")
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
	 (debug:print 2 "Extract ods, outputfile: " outputfile " runspatt: " runspatt " keyvals: " keyvals)
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
;; Test commands (i.e. for use inside tests)
;;======================================================================

(define (megatest:step step state status logfile msg)
  (if (not (getenv "MT_CMDINFO"))
      (begin
	(debug:print 0 "ERROR: MT_CMDINFO env var not set, -step must be called *inside* a megatest invoked environment!")
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
	(if (not (launch:setup-for-run))
	    (begin
	      (debug:print 0 "Failed to setup, exiting")
	      (exit 1)))
	(if (and state status)
	    (rmt:teststep-set-status! run-id test-id step state status msg logfile)
	    (begin
	      (debug:print 0 "ERROR: You must specify :state and :status with every call to -step")
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
	  (debug:print 0 "ERROR: MT_CMDINFO env var not set, commands -test-status, -runstep and -setlog must be called *inside* a megatest environment!")
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
	       (status    (args:get-arg ":status")))
	  (if (not (launch:setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, exiting")
		(exit 1)))

	  (if (args:get-arg "-runstep")(debug:print-info 1 "Running -runstep, first change to directory " work-area))
	  (change-directory work-area)
	  ;; can setup as client for server mode now
	  ;; (client:setup)

	  (if (args:get-arg "-load-test-data")
	      ;; has sub commands that are rdb:
	      ;; DO NOT put this one into either cdb:remote-run or open-run-close
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
		    (debug:print 0 "ERROR: nothing specified to run!")
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
		    (debug:print-info 2 "Running \"" fullcmd "\" in directory \"" startingdir)
		    (change-directory startingdir)
		    (set! exitstat (system fullcmd))
		    (set! *globalexitstatus* exitstat)
		    ;; (change-directory testpath)
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
		      (debug:print 0 "ERROR: You must specify :state and :status with every call to -test-status\n" help)
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
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting")
	    (exit 1)))
      (set! keys (cdb:remote-run db:get-keys db))
      (debug:print 1 "Keys: " (string-intersperse keys ", "))
      (if (sqlite3:database? db)(sqlite3:finalize! db))
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
;; Update the database schema, clean up the db
;;======================================================================

(if (args:get-arg "-rebuild-db")
    (begin
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
	    (exit 1)))
      ;; keep this one local
      (open-run-close patch-db #f)
      (set! *didsomething* #t)))

(if (args:get-arg "-cleanup-db")
    (begin
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
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
      (set! *didsomething* #t)))

(if (args:get-arg "-mark-incompletes")
    (begin
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") b
	    (exit 1)))
      (open-run-close db:find-and-mark-incomplete #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Update the tests meta data from the testconfig files
;;======================================================================

(if (args:get-arg "-update-meta")
    (begin
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
	    (exit 1)))
      ;; now can find our db
      ;; keep this one local
      (open-run-close runs:update-all-test_meta #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Start a repl
;;======================================================================

(if (or (args:get-arg "-repl")
	(args:get-arg "-load"))
    (let* ((toppath (launch:setup-for-run))
	   (dbstruct (if toppath (make-dbr:dbstruct path: toppath local: #t) #f)))
      (if dbstruct
	  (begin
	    (set! *db* dbstruct)
	    (set! *client-non-blocking-mode* #t)
	    (import readline)
	    (import apropos)
	    ;; (import (prefix sqlite3 sqlite3:)) ;; doesn't work ...
	    (gnu-history-install-file-manager
	     (string-append
	      (or (get-environment-variable "HOME") ".") "/.megatest_history"))
	    (current-input-port (make-gnu-readline-port "megatest> "))
	    (if (args:get-arg "-repl")
		(repl)
		(load (args:get-arg "-load")))
	    (db:close-all dbstruct))
	  (exit))
      (set! *didsomething* #t)))

;;======================================================================
;; Wait on a run to complete
;;======================================================================

(if (and (args:get-arg "-run-wait")
	 (not (args:get-arg "-runtests"))) ;; run-wait is built into runtests now
    (begin
      (if (not (launch:setup-for-run))
	  (begin
	    (debug:print 0 "Failed to setup, exiting") 
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
;; ;; ;; redo me 	   (debug:print-info 0 "Getting data for field " field)
;; ;; ;; redo me 	   (sqlite3:for-each-row
;; ;; ;; redo me 	    (lambda (id val)
;; ;; ;; redo me 	      (set! dat (cons (list id val) dat)))
;; ;; ;; redo me 	    (db:get-db db run-id)
;; ;; ;; redo me 	    (conc "SELECT id," field " FROM tests;"))
;; ;; ;; redo me 	   (debug:print-info 0 "found " (length dat) " items for field " field)
;; ;; ;; redo me 	   (let ((qry (sqlite3:prepare db (conc "UPDATE tests SET " field "=? WHERE id=?;"))))
;; ;; ;; redo me 	     (for-each
;; ;; ;; redo me 	      (lambda (item)
;; ;; ;; redo me 		(let ((newval ;; (sdb:qry 'getid 
;; ;; ;; redo me 		       (cadr item))) ;; )
;; ;; ;; redo me 		  (if (not (equal? newval (cadr item)))
;; ;; ;; redo me 		      (debug:print-info 0 "Converting " (cadr item) " to " newval " for test #" (car item)))
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
    (debug:print 0 help))

(set! *time-to-exit* #t)
(thread-join! *watchdog*)

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
