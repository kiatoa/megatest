;; Copyright 2006-2017, Matthew Welland.
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

(use (prefix sqlite3 sqlite3:) srfi-1 posix regex regex-case srfi-69 (prefix base64 base64:)
     readline apropos json http-client directory-utils typed-records
     http-client srfi-18 extras format)

;; Added for csv stuff - will be removed
;;
(use sparse-vectors)

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
;; (declare (uses dcommon))

(declare (uses tdb))
(declare (uses mt))
(declare (uses api))
(declare (uses tasks)) ;; only used for debugging.
(declare (uses env))
(declare (uses diff-report))

(define *db* #f) ;; this is only for the repl, do not use in general!!!!

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "megatest-fossil-hash.scm")

(define *usage-log-file* #f)    ;; put path to file for logging usage in this var in the ~/.megatestrc file
(define *usage-use-seconds* #t) ;; for Epoc seconds in usage logging change this to #t in ~/.megatestrc file

;; load the ~/.megatestrc file, put (use trace)(trace-call-sites #t)(trace function-you-want-to-trace) in this file
;;
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.megatestrc")))
  (if (common:file-exists? debugcontrolf)
      (load debugcontrolf)))

;; usage logging, careful with this, it is not designed to deal with all real world challenges!
;;
(if (and *usage-log-file*
         (file-write-access? *usage-log-file*))
    (with-output-to-file
        *usage-log-file*
      (lambda ()
        (print
         (if *usage-use-seconds*
             (current-seconds)
             (time->string
              (seconds->local-time (current-seconds))
              "%Yww%V.%w %H:%M:%S"))
         " "
         (current-user-name) " "
         (current-directory) " "
         "\"" (string-intersperse (argv) " ") "\""))
      #:append))

;; Disabled help items
;;  -rollup                 : (currently disabled) fill run (set by :runname)  with latest test(s)
;;                            from prior runs with same keys
;;  -daemonize              : fork into background and disconnect from stdin/out

(define help (conc "
Megatest, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2017

Usage: megatest [options]
  -h                      : this help
  -manual                 : show the Megatest user manual
  -version                : print megatest version (currently " megatest-version ")

Launching and managing runs
  -run                    : run all tests or as specified by -testpatt
  -remove-runs            : remove the data for a run, requires -runname and -testpatt
                            Optionally use :state and :status, use -keep-records to remove only
                            the run data.
  -set-state-status X,Y   : set state to X and status to Y, requires controls per -remove-runs
  -rerun FAIL,WARN...     : force re-run for tests with specificed status(s)
  -rerun-clean            : set all tests not COMPLETED+PASS,WARN,WAIVED to NOT_STARTED,n/a
                            and then run the specified testpatt with -preclean
  -rerun-all              : set all tests to NOT_STARTED,n/a and run with -preclean
  -lock                   : lock run specified by target and runname
  -unlock                 : unlock run specified by target and runname
  -set-run-status status  : sets status for run to status, requires -target and -runname
  -get-run-status         : gets status for run specified by target and runname
  -run-wait               : wait on run specified by target and runname
  -preclean               : remove the existing test directory before running the test
  -clean-cache            : remove the cached megatest.config and runconfigs.config files
  -no-cache               : do not use the cached config files. 

Selectors (e.g. use for -runtests, -remove-runs, -set-state-status, -list-runs etc.)
  -target key1/key2/...   : run for key1, key2, etc.
  -reqtarg key1/key2/...  : run for key1, key2, etc. but key1/key2 must be in runconfigs
  -testpatt patt1/patt2,patt3/...  : % is wildcard
  -runname                : required, name for this particular test run
  -state                  : Applies to runs, tests or steps depending on context
  -status                 : Applies to runs, tests or steps depending on context
  --modepatt key          : load testpatt from <key> in runconfigs instead of default TESTPATT if -testpatt and -tagexpr are not specified
  -tagexpr tag1,tag2%,..  : select tests with tags matching expression
  

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
  -test-files targpatt    : get the most recent test path/file matching targpatt e.g. %/% or '*.log'
                            returns list sorted by age ascending, see examples below
  -test-paths             : get the test paths matching target, runname, item and test
                            patterns.
  -list-disks             : list the disks available for storing runs
  -list-targets           : list the targets in runconfigs.config
  -list-db-targets        : list the target combinations used in the db
  -show-config            : dump the internal representation of the megatest.config file
  -show-runconfig         : dump the internal representation of the runconfigs.config file
  -dumpmode MODE          : dump in MODE format instead of sexpr, MODE=json,ini,sexp etc. (add -debug 0,9 to see which file contributes each line)
  -show-cmdinfo           : dump the command info for a test (run in test environment)
  -section sectionName
  -var varName            : for config and runconfig lookup value for sectionName varName
  -since N                : get list of runs changed since time N (Unix seconds)
  -fields fieldspec       : fields to include in json dump; runs:id,runame+tests:testname+steps
  -sort fieldname         : in -list-runs sort tests by this field
  -testdata-csv [categorypatt/]varpatt  : dump testdata for given category

Misc 
  -start-dir path         : switch to this directory before running megatest
  -contour cname          : add a level of hierarcy to the linktree and run paths
  -rebuild-db             : bring the database schema up to date
  -cleanup-db             : remove any orphan records, vacuum the db
  -import-megatest.db     : push data from megatest.db to cache db files in /tmp/$USER
  -sync-to-megatest.db    : pull data from cache files in /tmp/$USER to megatest.db
  -sync-to dest           : sync to new postgresql central style database
  -update-meta            : update the tests metadata for all tests
  -setvars VAR1=val1,VAR2=val2 : Add environment variables to a run NB// these are
                                 overwritten by values set in config files.
  -server -|hostname      : start the server (reduces contention on megatest.db), use
                            - to automatically figure out hostname
  -transport http|rpc     : use http or rpc for transport (default is http) 
  -log logfile            : send stdout and stderr to logfile
  -list-servers           : list the servers 
  -kill-servers           : kill all servers
  -repl                   : start a repl (useful for extending megatest)
  -load file.scm          : load and run file.scm
  -mark-incompletes       : find and mark incomplete tests
  -ping run-id|host:port  : ping server, exit with 0 if found
  -debug N|N,M,O...       : enable debug 0-N or N and M and O ...
  -config fname           : override the megatest.config file with fname
  -append-config fname    : append fname to the megatest.config file

Utilities
  -env2file fname         : write the environment to fname.csh and fname.sh
  -envcap a               : save current variables labeled as context 'a' in file envdat.db
  -envdelta a-b           : output enviroment delta from context a to context b to -o fname
                            set the output mode with -dumpmode csh, bash or ini
                            note: ini format will use calls to use curr and minimize path
  -refdb2dat refdb        : convert refdb to sexp or to format specified by s-dumpmode
                            formats: perl, ruby, sqlite3, csv (for csv the -o param
                            will substitute %s for the sheet name in generating 
                            multiple sheets)
  -o                      : output file for refdb2dat (defaults to stdout)
  -archive cmd            : archive runs specified by selectors to one of disks specified
                            in the [archive-disks] section.
                            cmd: keep-html, restore, save, save-remove
  -generate-html          : create a simple html tree for browsing your runs

Diff report
  -diff-rep               : generate diff report (must include -src-target, -src-runname, -target, -runname
                                                  and either -diff-email or -diff-html)
  -src-target <target>
  -src-runname <target>
  -diff-email <emails>    : comma separated list of email addresses to send diff report
  -diff-html  <rep.html>  : path to html file to generate

Spreadsheet generation
  -extract-ods fname.ods  : extract an open document spreadsheet from the database
  -pathmod path           : insert path, i.e. path/runame/itempath/logfile.html
                            will clear the field if no rundir/testname/itempath/logfile
                            if it contains forward slashes the path will be converted
                            to windows style
Getting started
  -create-megatest-area       : create a skeleton megatest area. You will be prompted for paths
  -create-test testname       : create a skeleton megatest test. You will be prompted for info

Examples

# Get test path, use '.' to get a single path or a specific path/file pattern
megatest -test-files 'logs/*.log' -target ubuntu/n%/no% -runname w49% -testpatt test_mt%

Called as " (string-intersperse (argv) " ") "
Version " megatest-version ", built from " megatest-fossil-hash ))

;;  -gui                    : start a gui interface
;;  -config fname           : override the runconfigs file with fname

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-runtests"  ;; run a specific test
			"-config"    ;; override the config file name
			"-append-config"
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
                        "-testdata-csv"
			"-testpatt"
                        "--modepatt"
                        "-tagexpr"
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
			"-contour"
			"-server"
			"-transport"
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
			"-create-test"
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
			"-target-db"
			"-source-db"
			"-prefix-target"

                        "-src-target"
                        "-src-runname"
                        "-diff-email"
			"-sync-to"			
			"-pgsync"
                        "-diff-html"
			)
 		 (list  "-h" "-help" "--help"
			"-manual"
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
			"-rerun-all"
			"-clean-cache"
			"-no-cache"
			"-cache-db"
                        "-use-db-cache"
                        "-prepend-contour"
			;; misc
			"-repl"
			"-lock"
			"-unlock"
			"-list-servers"
			"-kill-servers"
                        "-run-wait"      ;; wait on a run to complete (i.e. no RUNNING)
			"-local"         ;; run some commands using local db access
                        "-generate-html"

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
                        "-keep-records" ;; use with -remove-runs to remove only the run data
			"-rebuild-db"
			"-cleanup-db"
			"-rollup"
			"-update-meta"
			"-create-megatest-area"
			"-mark-incompletes"

			"-convert-to-norm"
			"-convert-to-old"
			"-import-megatest.db"
			"-sync-to-megatest.db"
			
			"-logging"
			"-v" ;; verbose 2, more than normal (normal is 1)
			"-q" ;; quiet 0, errors/warnings only

                        "-diff-rep"
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
    (debug:print-error 0 *default-log-port* "Unrecognised arguments: " (string-intersperse (if (list? remargs) remargs (argv))  " ")))

;; before doing anything else change to the start-dir if provided
;;
(if (args:get-arg "-start-dir")
    (if (common:file-exists? (args:get-arg "-start-dir"))
        (let ((fullpath (common:real-path (args:get-arg "-start-dir"))))
          (setenv "PWD" fullpath)
          (change-directory fullpath))
	(begin
	  (debug:print-error 0 *default-log-port* "non-existant start dir " (args:get-arg "-start-dir") " specified, exiting.")
	  (exit 1))))

;; immediately set MT_TARGET if -reqtarg or -target are available
;;
(let ((targ (or (args:get-arg "-reqtarg")(args:get-arg "-target"))))
  (if targ (setenv "MT_TARGET" targ)))

;; The watchdog is to keep an eye on things like db sync etc.
;;

;; TODO: for multiple areas, we will have multiple watchdogs; and multiple threads to manage
(define *watchdog* (make-thread
		    (lambda ()
		      (handle-exceptions
			  exn
			  (begin
			    (print-call-chain)
			    (print " message: " ((condition-property-accessor 'exn 'message) exn)))
			(common:watchdog)))
		    "Watchdog thread"))

;;(if (not (args:get-arg "-server"))
;;    (thread-start! *watchdog*)) ;; if starting a server; wait till we get to running state before kicking off watchdog
(let* ((no-watchdog-args
       '("-list-runs"
         "-testdata-csv"
         "-list-servers"
         "-server"
         "-list-disks"
         "-list-targets"
         "-show-runconfig"
         ;;"-list-db-targets"
         "-show-runconfig"
         "-show-config"
         "-show-cmdinfo"
	 "-cleanup-db"))
       (no-watchdog-args-vals (filter (lambda (x) x)
                                      (map args:get-arg no-watchdog-args)))
       (start-watchdog (null? no-watchdog-args-vals)))
  ;;(BB> "no-watchdog-args="no-watchdog-args "no-watchdog-args-vals="no-watchdog-args-vals) 
  (if start-watchdog
      (thread-start! *watchdog*)))


;; bracket open-output-file with code to make leading directory if it does not exist and handle exceptions
(define (open-logfile logpath)
  (condition-case
   (let* ((log-dir (or (pathname-directory logpath) ".")))
     (if (not (directory-exists? log-dir))
         (system (conc "mkdir -p " log-dir)))
     (open-output-file logpath))
   (exn ()
        (debug:print-error 0 *default-log-port* "Could not open log file for write: "logpath)
        (define *didsomething* #t)  
        (exit 1))))

;; this segment will run launch:setup only if -log is not set. This is fairly safe as servers are not
;; manually started and thus should never be started in a non-megatest area. Thus no need to handle situation
;; where (launch:setup) returns #f?
;;
(if (or (args:get-arg "-log")(args:get-arg "-server")) ;; redirect the log always when a server
    (handle-exceptions
	exn
	(begin
	  (print "ERROR: Failed to switch to log output. " ((condition-property-accessor 'exn 'message) exn))
	  )
      (let* ((tl   (or (args:get-arg "-log")(launch:setup)))   ;; run launch:setup if -server, ensure we do NOT run launch:setup if -log specified
	     (logf (or (args:get-arg "-log") ;; use -log unless we are a server, then craft a logfile name
		       (conc tl "/logs/server-" (current-process-id) "-" (get-host-name) ".log")))
	     (oup  (open-logfile logf)))
	(if (not (args:get-arg "-log"))
	    (hash-table-set! args:arg-hash "-log" logf)) ;; fake out future queries of -log
	(debug:print-info 0 *default-log-port* "Sending log output to " logf)
	(set! *default-log-port* oup))))

(if (or (args:get-arg "-h")
	(args:get-arg "-help")
	(args:get-arg "--help"))
    (begin
      (print help)
      (exit)))

(if (args:get-arg "-manual")
    (let* ((htmlviewercmd (or (configf:lookup *configdat* "setup" "htmlviewercmd")
			      (common:which '("firefox" "arora"))))
	   (install-home  (common:get-install-area))
	   (manual-html   (conc install-home "/share/docs/megatest_manual.html")))
      (if (and install-home
	       (common:file-exists? manual-html))
	  (system (conc "(" htmlviewercmd " " manual-html " ) &"))
	  (system (conc "(" htmlviewercmd " http://www.kiatoa.com/cgi-bin/fossils/megatest/doc/tip/docs/manual/megatest_manual.html ) &")))
      (exit)))

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

;; for some switches always print the command to stderr
;;
(if (args:any? "-run" "-runall" "-remove-runs" "-set-state-status")
    (debug:print 0 *default-log-port* (string-intersperse (argv) " ")))

;; some switches imply homehost. Exit here if not on homehost
;;
(let ((homehost-required  (list "-cleanup-db" "-server")))
  (if (apply args:any? homehost-required)
      (if (not (common:on-homehost?))
	  (for-each
	   (lambda (switch)
	     (if (args:get-arg switch)
		 (begin
		   (debug:print 0 *default-log-port* "ERROR: you must be on the homehost to run with " switch
				", you can move homehost by removing the .homehost file but this will disrupt any runs in progress.")
		   (exit 1))))
	   homehost-required))))

;;======================================================================
;; Misc setup stuff
;;======================================================================

(debug:setup)

(if (args:get-arg "-logging")(set! *logging* #t))

(if (debug:debug-mode 3) ;; we are obviously debugging
    (set! open-run-close open-run-close-no-exception-handling))

(if (args:get-arg "-itempatt")
    (let ((newval (conc (args:get-arg "-testpatt") "/" (args:get-arg "-itempatt"))))
      (debug:print 0 *default-log-port* "WARNING: -itempatt has been deprecated, please use -testpatt testpatt/itempatt method, new testpatt is "newval)
      (hash-table-set! args:arg-hash "-testpatt" newval)
      (hash-table-delete! args:arg-hash "-itempatt")))

(if (args:get-arg "-runtests")
    (debug:print 0 *default-log-port* "WARNING: \"-runtests\" is deprecated. Use \"-run\" with \"-testpatt\" instead"))

(on-exit std-exit-procedure)

;;======================================================================
;; Misc general calls
;;======================================================================

(if (and (args:get-arg "-cache-db")
         (args:get-arg "-source-db"))
    (let* ((temp-dir (or (args:get-arg "-target-db") (create-directory (conc "/tmp/" (getenv "USER") "/" (string-translate (current-directory) "/" "_")))))
           (target-db (conc temp-dir "/cached.db"))
           (source-db (args:get-arg "-source-db")))        
      (db:cache-for-read-only source-db target-db)
      (set! *didsomething* #t)))

;; handle a clean-cache request as early as possible
;;
(if (args:get-arg "-clean-cache")
    (let ((toppath  (launch:setup)))
      (set! *didsomething* #t) ;; suppress the help output.
      (runs:clean-cache (or (getenv "MT_TARGET")
			    (args:get-arg "-target")
			    (args:get-arg "-remtarg"))
			(args:get-arg "-runname")
			toppath)))
	  
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
	  (debug:print 0 *default-log-port* "Bad input? data=" data) ;; some error occurred
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
			(db-exists (common:file-exists? db-file))
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
    (let* ((server-id     (string->number (args:get-arg "-ping"))) ;; extract run-id (i.e. no ":"
	   (host:port     (args:get-arg "-ping")))
      (server:ping (or server-id host:port) do-exit: #t)))

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
	    (debug:print-error 0 *default-log-port* "Parameter to -envdelta should be new=star-end")))))

;;======================================================================
;; Start the server - can be done in conjunction with -runall or -runtests (one day...)
;;   we start the server if not running else start the client thread
;;======================================================================

;; Server? Start up here.
;;
(if (args:get-arg "-server")
    (let ((tl        (launch:setup))
          (transport-type (string->symbol (or (args:get-arg "-transport") "http"))))
      (server:launch 0 transport-type)
      (set! *didsomething* #t)))

(if (or (args:get-arg "-list-servers")
        (args:get-arg "-kill-servers"))
    (let ((tl (launch:setup)))
      (if tl ;; all roads from here exit
	  (let* ((servers (server:get-list *toppath*))
		 (fmtstr  "~8a~22a~20a~20a~8a\n"))
	    (format #t fmtstr "pid" "Interface:port" "age (hms)" "Last mod" "State")
	    (format #t fmtstr "===" "==============" "=========" "========" "=====")
	    (for-each ;;  ( mod-time host port start-time pid )
	     (lambda (server)
	       (let* ((mtm (any->number (car server)))
		      (mod (if mtm (- (current-seconds) mtm) "unk"))
		      (age (- (current-seconds)(or (any->number (list-ref server 3)) (current-seconds))))
		      (url (conc (cadr server) ":" (caddr server)))
		      (pid (list-ref server 4))
		      (alv (if (number? mod)(< mod 10) #f)))
		 (format #t
			 fmtstr
			 pid
			 url
			 (seconds->hr-min-sec age)
			 (seconds->hr-min-sec mod)
			 (if alv "alive" "dead"))
		 (if (and alv
			  (args:get-arg "-kill-servers"))
		     (begin
		       (debug:print-info 0 *default-log-port* "Attempting to kill server with pid " pid)
		       (server:kill server)))))
	     (sort servers (lambda (a b)
			     (let ((ma (or (any->number (car a)) 9e9))
				   (mb (or (any->number (car b)) 9e9)))
			       (> ma mb)))))
	    ;; (debug:print-info 1 *default-log-port* "Done with listservers")
	    (set! *didsomething* #t)
	    (exit))
	  (exit))))
      ;; must do, would have to add checks to many/all calls below

;;======================================================================
;; Weird special calls that need to run *after* the server has started?
;;======================================================================

(if (args:get-arg "-list-targets")
    (if (launch:setup)
        (let ((targets (common:get-runconfig-targets)))
          ;; (debug:print 1 *default-log-port* "Found "(length targets) " targets")
          (case (string->symbol (or (args:get-arg "-dumpmode") "alist"))
            ((alist)
             (for-each (lambda (x)
                         ;; (print "[" x "]"))
                         (print x))
                       targets))
            ((json)
             (json-write targets))
            (else
             (debug:print-error 0 *default-log-port* "dump output format " (args:get-arg "-dumpmode") " not supported for -list-targets")))
          (set! *didsomething* #t))))

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
	     (common:file-exists? cfgf)
	     (file-write-access? cfgf)
	     (common:use-cache?))
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
			   ;; (read-config (conc *toppath* "/runconfigs.config") #f #t sections: sections))))
                           (runconfig:read (conc *toppath* "/runconfigs.config") target #f))))
	  (if (and rundir ;; have all needed variabless
		   (directory-exists? rundir)
		   (file-write-access? rundir))
	      (begin
                (if (not (common:in-running-test?))
                    (configf:write-alist data cfgf))
		;; force re-read of megatest.config - this resolves circular references between megatest.config
		(launch:setup force-reread: #t)
		;; (launch:cache-config) ;; there are two independent config cache locations, turning this one off for now. MRW.
		)) ;; we can safely cache megatest.config since we have a valid runconfig
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
	 ((or (not (args:get-arg "-dumpmode"))
              (string=? (args:get-arg "-dumpmode") "ini"))
	  (configf:config->ini data))
	 ((string=? (args:get-arg "-dumpmode") "sexp")
	  (pp (hash-table->alist data)))
	 ((string=? (args:get-arg "-dumpmode") "json")
	  (json-write data))
	 (else
	  (debug:print-error 0 *default-log-port* "-dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
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
	(debug:print-error 0 *default-log-port* "-dumpmode of " (args:get-arg "-dumpmode") " not recognised")))
      (set! *didsomething* #t)
      (pop-directory)
      (set! *time-to-exit* #t)))

(if (args:get-arg "-show-cmdinfo")
    (if (or (args:get-arg ":value")(getenv "MT_CMDINFO"))
	(let ((data (common:read-encoded-string (or (args:get-arg ":value")(getenv "MT_CMDINFO")))))
	  (if (equal? (args:get-arg "-dumpmode") "json")
	      (json-write data)
	      (pp data))
	  (set! *didsomething* #t))
	(debug:print-info 0 *default-log-port* "environment variable MT_CMDINFO is not set")))

;;======================================================================
;; Remove old run(s)
;;======================================================================

;; since several actions can be specified on the command line the removal
;; is done first
(define (operate-on action #!key (mode #f)) ;; #f is "use default"
  (let* ((runrec (runs:runrec-make-record))
	 (target (common:args-get-target)))
    (cond
     ((not target)
      (debug:print-error 0 *default-log-port* "Missing required parameter for " action ", you must specify -target or -reqtarg")
      (exit 1))
     ((not (or (args:get-arg ":runname")
	       (args:get-arg "-runname")))
      (debug:print-error 0 *default-log-port* "Missing required parameter for " action ", you must specify the run name pattern with -runname patt")
      (exit 2))
     ((not (args:get-arg "-testpatt"))
      (debug:print-error 0 *default-log-port* "Missing required parameter for " action ", you must specify the test pattern with -testpatt")
      (exit 3))
     (else
      (if (not (car *configinfo*))
	  (begin
	    (debug:print-error 0 *default-log-port* "Attempted " action "on test(s) but run area config file not found")
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
			      new-state-status: (args:get-arg "-set-state-status")
                              mode: mode)))
      (set! *didsomething* #t)))))

(if (args:get-arg "-remove-runs")
    (general-run-call 
     "-remove-runs"
     "remove runs"
     (lambda (target runname keys keyvals)
       (operate-on 'remove-runs mode: (if (args:get-arg "-keep-records")
                                          'remove-data-only
                                          'remove-all)))))

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
					#f #f #f #f))
	      (header   (vector-ref runsdat 0))
	      (rows     (vector-ref runsdat 1)))
	 (if (null? rows)
	     (begin
	       (debug:print-info 0 *default-log-port* "No matching run found.")
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
	    #f ;; index too high, should raise an error I suppose
	    (vector-ref datavec indx))
	#f)))





(when (args:get-arg "-testdata-csv")
  (if (launch:setup)
      (let* ((keys        (rmt:get-keys)) ;; (db:get-keys dbstruct))
             (runpatt     (or (args:get-arg "-runname") "%"))
             (testpatt    (common:args-get-testpatt #f))
             (datapatt    (args:get-arg "-testdata-csv"))
             (match-data  (string-match "^([^/]+)/(.*)" (args:get-arg "-testdata-csv")))
             (categorypatt (if match-data (list-ref match-data 1) "%"))
             (setvarpatt  (if match-data
                              (list-ref match-data 2)
                              (args:get-arg "-testdata-csv")))
             (runsdat     (rmt:get-runs-by-patt keys (or runpatt "%") 
                                                (common:args-get-target) #f #f '("id" "runname" "state" "status" "owner" "event_time" "comment") 0))
             (header      (db:get-header runsdat))
             (access-mode (db:get-access-mode))
             (testpatt    (common:args-get-testpatt #f))
             (fields-spec (if (args:get-arg "-fields")
                              (extract-fields-constraints (args:get-arg "-fields"))
                              (list (cons "runs" (append keys (list "id" "runname" "state" "status" "owner" "event_time" "comment" "fail_count" "pass_count")))
                                    (cons "tests"  db:test-record-fields) ;; "id" "testname" "test_path")
                                    (list "steps" "id" "stepname"))))
             (tests-spec  (let ((t (alist-ref "tests" fields-spec equal?)))
                            (if (and t (null? t)) ;; all fields
                                db:test-record-fields
                                t)))
             (adj-tests-spec (delete-duplicates (if tests-spec (cons "id" tests-spec) db:test-record-fields))) 
             (test-field-index (make-hash-table))
             (runs (db:get-rows runsdat))
             )
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
                    (debug:print-error 0 *default-log-port* "Invalid test fields specified: " (string-intersperse invalid-tests-spec ", "))
                    (exit)))))
        (let* ((table-header (string-split "target,run,test,itempath,category,var,value,comment" ","))
               (table-rows
                (apply append (map  
                               (lambda (run)
                                 (let* ((target (string-intersperse (map (lambda (x)
							 (db:get-value-by-header run header x))
						       keys) "/"))
                                        (statuses (string-split (or (args:get-arg "-status") "") ","))
                                        (run-id  (db:get-value-by-header run header "id"))
                                        (runname (db:get-value-by-header run header "runname")) 
                                        (states  (string-split (or (args:get-arg "-state") "") ","))
                                        (tests   (if tests-spec
                                                     (db:dispatch-query access-mode rmt:get-tests-for-run db:get-tests-for-run run-id testpatt states statuses #f #f #f 'testname 'asc ;; (db:get-tests-for-run dbstruct run-id testpatt '() '() #f #f #f 'testname 'asc 
                                                                        ;; use qryvals if test-spec provided
                                                                        (if tests-spec
                                                                            (string-intersperse adj-tests-spec ",")
                                                                            ;; db:test-record-fields
                                                                            #f)
                                                                        #f
                                                                        'normal)
                                                     '())))
                                   (apply append
                                          (map
                                           (lambda (test)
                                             (let* (
                                                    (test-id      (if (member "id"           tests-spec)(get-value-by-fieldname test test-field-index "id"          ) #f)) ;; (db:test-get-id         test))
                                                    (testname     (if (member "testname"     tests-spec)(get-value-by-fieldname test test-field-index "testname"    ) #f)) ;; (db:test-get-testname   test))
                                                    (itempath     (if (member "item_path"    tests-spec)(get-value-by-fieldname test test-field-index "item_path"   ) #f)) ;; (db:test-get-item-path  test))
                                                    (fullname     (conc testname
                                                                        (if (equal? itempath "")
                                                                            "" 
                                                                            (conc "/" itempath ))))
                                                    (testdat-raw (map vector->list (rmt:read-test-data* run-id test-id categorypatt setvarpatt)))
                                                    (testdat (filter
                                                              (lambda (x)
                                                                (not (equal? "logpro"
                                                                             (list-ref x 10))))
                                                              testdat-raw)))
                                               (map 
                                                (lambda (item)
                                                  (receive (id test_id category
                                                               variable value expected
                                                               tol units comment status type)
                                                      (apply values item)
                                                    (list target runname testname itempath category variable value comment)))
                                                testdat)))
                                           tests))))
                               runs))))
          (print (string-join table-header ","))
          (for-each (lambda(table-row)
                      (print (string-join (map ->string table-row) ",")))

                    
                            table-rows))))
  (set! *didsomething* #t)
  (set! *time-to-exit* #t))



;; NOTE: list-runs and list-db-targets operate on local db!!!
;;
;; IDEA: megatest list -runname blah% ...
;;
(if (or (args:get-arg "-list-runs")
	(args:get-arg "-list-db-targets"))
    (if (launch:setup)
	(let* (;; (dbstruct    (make-dbr:dbstruct path: *toppath* local: (args:get-arg "-local")))
	       (runpatt     (args:get-arg "-list-runs"))
               (access-mode (db:get-access-mode))
	       (testpatt    (common:args-get-testpatt #f))
	       ;; (if (args:get-arg "-testpatt") 
	       ;;  	        (args:get-arg "-testpatt") 
	       ;;  	        "%"))
	       (keys        (rmt:get-keys)) ;; (db:get-keys dbstruct))
	       ;; (runsdat  (db:get-runs dbstruct runpatt #f #f '()))
	;; (runsdat     (rmt:get-runs-by-patt keys (or runpatt "%") (common:args-get-target) ;; (db:get-runs-by-patt dbstruct keys (or runpatt "%") (common:args-get-target)
	;; 		           	 #f #f '("id" "runname" "state" "status" "owner" "event_time" "comment") 0))
	       (runsdat     (rmt:get-runs-by-patt keys (or runpatt "%") 
                                                  (common:args-get-target) #f #f '("id" "runname" "state" "status" "owner" "event_time" "comment") 0))
	       (runstmp     (db:get-rows runsdat))
	       (header      (db:get-header runsdat))
	       ;; this is "-since" support. This looks at last mod times of <run-id>.db files
	       ;; and collects those modified since the -since time.
	       (runs        runstmp)
                        ;; (if (and (not (null? runstmp))
			;;        (args:get-arg "-since"))
			;;   (let ((changed-ids (db:get-changed-run-ids (string->number (args:get-arg "-since")))))
			;;     (let loop ((hed (car runstmp))
			;;   	     (tal (cdr runstmp))
			;;   	     (res '()))
			;;       (let ((new-res (if (member (db:get-value-by-header hed header "id") changed-ids)
			;;   		       (cons hed res)
			;;   		       res)))
			;;         (if (null? tal)
			;;   	  (reverse new-res)
			;;   	  (loop (car tal)(cdr tal) new-res)))))
			;;   runstmp))
	       (db-targets  (args:get-arg "-list-db-targets"))
	       (seen        (make-hash-table))
	       (dmode       (let ((d (args:get-arg "-dumpmode"))) ;; json, sexpr
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
		      (debug:print-error 0 *default-log-port* "Invalid test fields specified: " (string-intersperse invalid-tests-spec ", "))
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
				       (db:dispatch-query access-mode rmt:get-tests-for-run db:get-tests-for-run run-id testpatt states statuses #f #f #f 'testname 'asc ;; (db:get-tests-for-run dbstruct run-id testpatt '() '() #f #f #f 'testname 'asc 
							     ;; use qryvals if test-spec provided
							     (if tests-spec
								 (string-intersperse adj-tests-spec ",")
								 ;; db:test-record-fields
								 #f)
							     #f
							     'normal)
				       '())))
		     (case dmode
		       ((json ods sexpr)
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
		      	(common:debug-handle-exceptions #f
			 exn
			 (begin
			   (debug:print-error 0 *default-log-port* "Bad data in test record? " test)
			   (debug:print-error 5 *default-log-port* "exn=" (condition->list exn))
			   (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
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
			     ((json ods sexpr)
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
				    (let ((steps (db:dispatch-query access-mode rmt:get-steps-for-test db:get-steps-for-test run-id (db:test-get-id test)))) ;; (db:get-steps-for-test dbstruct run-id (db:test-get-id test))))
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
	  (case dmode
	    ((json)  (json-write data))
	    ((sexpr) (pp (common:to-alist data))))
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
						  (debug:print 0 *default-log-port* "WARNING: meta data for run " runname " not found")
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
							     (debug:print 4 *default-log-port* "WARNING: run " target "/" runname " appears to have no data")
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
					 (debug:print 0 *default-log-port* "WARNING: path given, " outputfile " is relative, prefixing with current directory")
					 (conc (current-directory) "/" outputfile)))))
		  (create-directory tempdir #t)
		  (ods:list->ods tempdir ouf sheets))))
	  ;; (system (conc "rm -rf " tempdir))
	  (set! *didsomething* #t)
          (set! *time-to-exit* #t)
          ) ;; end if true branch (end of a let)
        ) ;; end if
    ) ;; end if -list-runs

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
	(args:get-arg "-rerun-all")
	(args:get-arg "-runtests"))
    (let ((need-clean (or (args:get-arg "-rerun-clean")
                          (args:get-arg "-rerun-all"))))
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
               (runs:clean-cache target runname *toppath*)
               (runs:operate-on 'set-state-status
                                target
                                (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
                                "%" ;; (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
                                ;; state:  states
                                status: statuses
                                new-state-status: "NOT_STARTED,n/a")))
         ;; RERUN ALL
         (if (args:get-arg "-rerun-all") ;; first set states/statuses correct
             (begin
               (hash-table-set! args:arg-hash "-preclean" #t)
               (runs:operate-on 'set-state-status
                                target
                                (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
                                "%" ;; (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
                                state:  #f
                                ;; status: statuses
                                new-state-status: "NOT_STARTED,n/a")
               (runs:clean-cache target runname *toppath*)
               (runs:operate-on 'set-state-status
                                target
                                (common:args-get-runname)  ;; (or (args:get-arg "-runname")(args:get-arg ":runname"))
                                "%" ;; (common:args-get-testpatt #f) ;; (args:get-arg "-testpatt")
                                ;; state:  states
                                status: #f
                                new-state-status: "NOT_STARTED,n/a")))
         (runs:run-tests target
                         runname
                         #f ;; (common:args-get-testpatt #f)
                         ;; (or (args:get-arg "-testpatt")
                         ;;     "%")
                         user
                         args:arg-hash)))))

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
		(debug:print-error 0 *default-log-port* "-target is required.")
		(exit 1)))
	  (if (not (launch:setup))
	      (begin
		(debug:print 0 *default-log-port* "Failed to setup, giving up on -test-paths or -test-files, exiting")
		(exit 1)))
	  (let* ((keys     (rmt:get-keys))
		 ;; db:test-get-paths must not be run remote
		 (paths    (tests:test-get-paths-matching keys target (args:get-arg "-test-files"))))
	    (set! *didsomething* #t)
	    (for-each (lambda (path)
			(if (common:file-exists? path)
			(print path)))	
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
	 (debug:print 2 *default-log-port* "Extract ods, outputfile: " outputfile " runspatt: " runspatt " keyvals: " keyvals)
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
		  (debug:print-error 0 *default-log-port* "bad run-id or test-id, must be integers")
		  (exit 1)))))))

;;======================================================================
;; Test commands (i.e. for use inside tests)
;;======================================================================

(define (megatest:step step state status logfile msg)
  (if (not (getenv "MT_CMDINFO"))
      (begin
	(debug:print-error 0 *default-log-port* "MT_CMDINFO env var not set, -step must be called *inside* a megatest invoked environment!")
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
	      (debug:print 0 *default-log-port* "Failed to setup, exiting")
	      (exit 1)))
	(if (and state status)
	    (let ((comment (launch:load-logpro-dat run-id test-id step)))
	      ;; (rmt:test-set-log! run-id test-id (conc stepname ".html"))))
	      (rmt:teststep-set-status! run-id test-id step state status (or comment msg) logfile))
	    (begin
	      (debug:print-error 0 *default-log-port* "You must specify :state and :status with every call to -step")
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
	  (debug:print-error 0 *default-log-port* "MT_CMDINFO env var not set, commands -test-status, -runstep and -setlog must be called *inside* a megatest environment!")
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
		(debug:print 0 *default-log-port* "Failed to setup, exiting")
		(exit 1)))

	  (if (args:get-arg "-runstep")(debug:print-info 1 *default-log-port* "Running -runstep, first change to directory " work-area))
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
		    (debug:print-error 0 *default-log-port* "nothing specified to run!")
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
		    (debug:print-info 2 *default-log-port* "Running \"" fullcmd "\" in directory \"" startingdir)
		    (change-directory startingdir)
		    (set! exitstat (system fullcmd))
		    (set! *globalexitstatus* exitstat)
		    ;; (change-directory testpath)
		    ;; run logpro if applicable ;; (process-run "ls" (list "/foo" "2>&1" "blah.log"))
		    (if logprofile
			(let* ((htmllogfile (conc stepname ".html"))
			       (oldexitstat exitstat)
			       (cmd         (string-intersperse (list "logpro" logprofile htmllogfile "<" logfile ">" (conc stepname "_logpro.log")) " ")))
			  (debug:print-info 2 *default-log-port* "running \"" cmd "\"")
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
		      (debug:print-error 0 *default-log-port* "You must specify :state and :status with every call to -test-status\n" help)
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
	    (debug:print 0 *default-log-port* "Failed to setup, exiting")
	    (exit 1)))
      (set! keys (rmt:get-keys)) ;;  db))
      (debug:print 1 *default-log-port* "Keys: " (string-intersperse keys ", "))
      (if (sqlite3:database? db)(sqlite3:finalize! db))
      (set! *didsomething* #t)))

(if (args:get-arg "-gui")
    (begin
      (debug:print 0 *default-log-port* "Look at the dashboard for now")
      ;; (megatest-gui)
      (set! *didsomething* #t)))

(if (args:get-arg "-create-megatest-area")
    (begin
      (genexample:mk-megatest.config)
      (set! *didsomething* #t)))

(if (args:get-arg "-create-test")
    (let ((testname (args:get-arg "-create-test")))
      (genexample:mk-megatest-test testname)
      (set! *didsomething* #t)))

;;======================================================================
;; Update the database schema, clean up the db
;;======================================================================

(if (args:get-arg "-rebuild-db")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 *default-log-port* "Failed to setup, exiting") 
	    (exit 1)))
      ;; keep this one local
      ;; (open-run-close patch-db #f)
      (let ((dbstruct (db:setup #f areapath: *toppath*)))
        (common:cleanup-db dbstruct full: #t))
      (set! *didsomething* #t)))

(if (args:get-arg "-cleanup-db")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 *default-log-port* "Failed to setup, exiting") 
	    (exit 1)))
      (let ((dbstruct (db:setup #f areapath: *toppath*)))
        (common:cleanup-db dbstruct))
      (set! *didsomething* #t)))

(if (args:get-arg "-mark-incompletes")
    (begin
      (if (not (launch:setup))
	  (begin
	    (debug:print 0 *default-log-port* "Failed to setup, exiting")
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
	    (debug:print 0 *default-log-port* "Failed to setup, exiting") 
	    (exit 1)))
      (runs:update-all-test_meta #f)
      (set! *didsomething* #t)))

;;======================================================================
;; Start a repl
;;======================================================================

;; fakeout readline
(include "readline-fix.scm")


(when (args:get-arg "-diff-rep")
  (when (and
         (not (args:get-arg "-diff-html"))
         (not (args:get-arg "-diff-email")))
    (debug:print 0 *default-log-port* "Must specify -diff-html or -diff-email with -diff-rep")
    (set! *didsomething* 1)
    (exit 1))
  
  (let* ((toppath (launch:setup)))
    (do-diff-report
     (args:get-arg "-src-target")
     (args:get-arg "-src-runname")
     (args:get-arg "-target")
     (args:get-arg "-runname")
     (args:get-arg "-diff-html")
     (args:get-arg "-diff-email"))
    (set! *didsomething* #t)
    (exit 0)))

(if (or (getenv "MT_RUNSCRIPT")
	(args:get-arg "-repl")
	(args:get-arg "-load"))
    (let* ((toppath (launch:setup))
	   (dbstruct (if (and toppath
                              (common:on-homehost?))
                         (db:setup #t)
                         #f))) ;; make-dbr:dbstruct path: toppath local: (args:get-arg "-local")) #f)))
      (if *toppath*
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
	      (import extras) ;; might not be needed
	      ;; (import csi)
	      (import readline)
	      (import apropos)
	      ;; (import (prefix sqlite3 sqlite3:)) ;; doesn't work ...

	      (if *use-new-readline*
		  (begin
		    (install-history-file (get-environment-variable "HOME") ".megatest_history") ;;  [homedir] [filename] [nlines])
		    (current-input-port (make-readline-port "megatest> ")))
		  (begin
		    (gnu-history-install-file-manager
		     (string-append
		      (or (get-environment-variable "HOME") ".") "/.megatest_history"))
		    (current-input-port (make-gnu-readline-port "megatest> "))))
	      (if (args:get-arg "-repl")
		  (repl)
		  (load (args:get-arg "-load")))
	      ;; (db:close-all dbstruct) <= taken care of by on-exit call
	      )
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
	    (debug:print 0 *default-log-port* "Failed to setup, exiting") 
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
;; ;; ;; redo me 	   (debug:print-info 0 *default-log-port* "Getting data for field " field)
;; ;; ;; redo me 	   (sqlite3:for-each-row
;; ;; ;; redo me 	    (lambda (id val)
;; ;; ;; redo me 	      (set! dat (cons (list id val) dat)))
;; ;; ;; redo me 	    (db:get-db db run-id)
;; ;; ;; redo me 	    (conc "SELECT id," field " FROM tests;"))
;; ;; ;; redo me 	   (debug:print-info 0 *default-log-port* "found " (length dat) " items for field " field)
;; ;; ;; redo me 	   (let ((qry (sqlite3:prepare db (conc "UPDATE tests SET " field "=? WHERE id=?;"))))
;; ;; ;; redo me 	     (for-each
;; ;; ;; redo me 	      (lambda (item)
;; ;; ;; redo me 		(let ((newval ;; (sdb:qry 'getid 
;; ;; ;; redo me 		       (cadr item))) ;; )
;; ;; ;; redo me 		  (if (not (equal? newval (cadr item)))
;; ;; ;; redo me 		      (debug:print-info 0 *default-log-port* "Converting " (cadr item) " to " newval " for test #" (car item)))
;; ;; ;; redo me 		  (sqlite3:execute qry newval (car item))))
;; ;; ;; redo me 	      dat)
;; ;; ;; redo me 	     (sqlite3:finalize! qry))))
;; ;; ;; redo me        (db:close-all dbstruct)
;; ;; ;; redo me        (list "uname" "rundir" "final_logf" "comment"))
;; ;; ;; redo me       (set! *didsomething* #t)))

(if (args:get-arg "-import-megatest.db")
    (begin
      (db:multi-db-sync 
       (db:setup #f)
       'killservers
       'dejunk
       'adj-testids
       'old2new
       ;; 'new2old
       )
      (set! *didsomething* #t)))

(if (args:get-arg "-sync-to-megatest.db")
    (let ((res (db:multi-db-sync 
                (db:setup #f)
                'new2old)))
      (print "Synced " res " records to megatest.db")
      (set! *didsomething* #t)))

(if (args:get-arg "-sync-to")
    (let ((toppath (launch:setup)))
      (tasks:sync-to-postgres *configdat* (args:get-arg "-sync-to"))
      (set! *didsomething* #t)))

(if (args:get-arg "-generate-html")
    (let* ((toppath (launch:setup)))
      (if (tests:create-html-tree #f)
          (debug:print-info 0 *default-log-port* "HTML output created in " toppath "/lt/page#.html")
          (debug:print 0 *default-log-port* "Failed to create HTML output in " toppath "/lt/runs-index.html"))
      (set! *didsomething* #t)))

;;======================================================================
;; Exit and clean up
;;======================================================================

(if (not *didsomething*)
    (debug:print 0 *default-log-port* help)
    (set! *time-to-exit* #t)
    )
;;(debug:print-info 13 *default-log-port* "thread-join! watchdog")

;; join the watchdog thread if it has been thread-start!ed  (it may not have been started in the case of a server that never enters running state)
;;   (symbols returned by thread-state: created ready running blocked suspended sleeping terminated dead)
;; TODO: for multiple areas, we will have multiple watchdogs; and multiple threads to manage
(if (thread? *watchdog*)
    (case (thread-state *watchdog*)
      ((ready running blocked sleeping terminated dead)
       (thread-join! *watchdog*))))

(set! *time-to-exit* #t)

(if (not (eq? *globalexitstatus* 0))
    (if (or (args:get-arg "-run")(args:get-arg "-runtests")(args:get-arg "-runall"))
        (begin
           (debug:print 0 *default-log-port* "NOTE: Subprocesses with non-zero exit code detected: " *globalexitstatus*)
           (exit 0))
        (case *globalexitstatus*
         ((0)(exit 0))
         ((1)(exit 1))
         ((2)(exit 2))
         (else (exit 3)))))
