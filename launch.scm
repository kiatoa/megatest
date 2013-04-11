
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;======================================================================
;; launch a task - this runs on the originating host, tests themselves
;;
;;======================================================================

(use regex regex-case base64 sqlite3 srfi-18)
(import (prefix base64 base64:))
(import (prefix sqlite3 sqlite3:))

(declare (unit launch))
(declare (uses common))
(declare (uses configf))
(declare (uses db))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")

;;======================================================================
;; ezsteps
;;======================================================================

;; ezsteps were going to be coded as
;; stepname[,predstep1,predstep2 ...] [{VAR1=first,second,third}] command to execute
;;   BUT
;; now are
;; stepname {VAR=first,second,third ...} command ...
;; where the {VAR=first,second,third ...} is optional.

;; given an exit code and whether or not logpro was used calculate OK/BAD
;; return #t if we are ok, #f otherwise
(define (steprun-good? logpro exitcode)
  (or (eq? exitcode 0)
      (and logpro (eq? exitcode 2))))

;; if handed a string, process it, else look for MT_CMDINFO
(define (launch:get-cmdinfo-assoc-list #!key (encoded-cmd #f))
  (let ((enccmd (if encoded-cmd encoded-cmd (getenv "MT_CMDINFO"))))
    (if enccdm
	(read (open-input-string (base64:base64-decode enccmd)))
	'())))

(define (launch:execute encoded-cmd)
  (let* ((cmdinfo   (read (open-input-string (base64:base64-decode encoded-cmd)))))
    (setenv "MT_CMDINFO" encoded-cmd)
    (if (list? cmdinfo) ;; ((testpath /tmp/mrwellan/jazzmind/src/example_run/tests/sqlitespeed)
	;; (test-name sqlitespeed) (runscript runscript.rb) (db-host localhost) (run-id 1))
	(let* ((testpath  (assoc/default 'testpath  cmdinfo))  ;; How is testpath different from work-area ??
	       (top-path  (assoc/default 'toppath   cmdinfo))
	       (work-area (assoc/default 'work-area cmdinfo))
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (ezsteps   (assoc/default 'ezsteps   cmdinfo))
	       ;; (runremote (assoc/default 'runremote cmdinfo))
	       (transport (assoc/default 'transport cmdinfo))
	       (serverinf (assoc/default 'serverinf cmdinfo))
	       (port      (assoc/default 'port      cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (test-id   (assoc/default 'test-id   cmdinfo))
	       (target    (assoc/default 'target    cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (env-ovrd  (assoc/default 'env-ovrd  cmdinfo))
	       (set-vars  (assoc/default 'set-vars  cmdinfo)) ;; pre-overrides from -setvar
	       (runname   (assoc/default 'runname   cmdinfo))
	       (megatest  (assoc/default 'megatest  cmdinfo))
	       (mt-bindir-path (assoc/default 'mt-bindir-path cmdinfo))
	       (keys      #f)
	       (keyvals   #f)
	       (fullrunscript (if (not runscript)
                                  #f
                                  (if (substring-index "/" runscript)
                                      runscript ;; use unadultered if contains slashes
                                      (let ((fulln (conc testpath "/" runscript)))
	                                  (if (and (file-exists? fulln)
                                                   (file-execute-access? fulln))
                                              fulln
                                              runscript))))) ;; assume it is on the path
	       (rollup-status 0))
	  (change-directory top-path)
	  (debug:print 2 "Exectuing " test-name " (id: " test-id ") on " (get-host-name))
	  ;; Setup the *runremote* global var
	  (if *runremote* (debug:print 2 "ERROR: I'm not expecting *runremote* to be set at this time"))
	  ;; (set! *runremote* runremote)
	  (set! *transport-type* (string->symbol transport))
	  (set! keys       (cdb:remote-run db:get-keys #f))
	  (set! keyvals    (if run-id (cdb:remote-run db:get-key-vals #f run-id) #f))
	  ;; apply pre-overrides before other variables. The pre-override vars must not
	  ;; clobbers things from the official sources such as megatest.config and runconfigs.config
	  (if (string? set-vars)
	      (let ((varpairs (string-split set-vars ",")))
		(debug:print 4 "varpairs: " varpairs)
		(map (lambda (varpair)
		       (let ((varval (string-split varpair "=")))
			 (if (eq? (length varval) 2)
			     (let ((var (car varval))
				   (val (cadr varval)))
			       (debug:print 1 "Adding pre-var/val " var " = " val " to the environment")
			       (setenv var val)))))
		     varpairs)))
	  (setenv "MT_TEST_RUN_DIR" work-area)
	  (setenv "MT_TEST_NAME" test-name)
	  (setenv "MT_ITEM_INFO" (conc itemdat))
	  (setenv "MT_RUNNAME"   runname)
	  (setenv "MT_MEGATEST"  megatest)
	  (setenv "MT_TARGET"    target)
	  (if mt-bindir-path (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path)))
	  ;; (change-directory top-path)
	  (if (not (setup-for-run))
	      (begin
		(debug:print 0 "Failed to setup, exiting") 
		;; (sqlite3:finalize! db)
		;; (sqlite3:finalize! tdb)
		(exit 1)))
	  ;; Can setup as client for server mode now
	  ;; (client:setup)

	  (change-directory *toppath*) 
	  (set-megatest-env-vars run-id) ;; these may be needed by the launching process
	  (change-directory work-area) 

	  (open-run-close set-run-config-vars #f run-id keys keyvals)
	  ;; environment overrides are done *before* the remaining critical envars.
	  (alist->env-vars env-ovrd)
	  (set-megatest-env-vars run-id)
	  (set-item-env-vars itemdat)
	  (save-environment-as-files "megatest")
	  ;; open-run-close not needed for test-set-meta-info
	  (test-set-meta-info #f test-id run-id test-name itemdat 0)
	  (tests:test-set-status! test-id "REMOTEHOSTSTART" "n/a" (args:get-arg "-m") #f)
	  (if (args:get-arg "-xterm")
	      (set! fullrunscript "xterm")
	      (if (and fullrunscript (not (file-execute-access? fullrunscript)))
		  (system (conc "chmod ug+x " fullrunscript))))
	  ;; We are about to actually kick off the test
	  ;; so this is a good place to remove the records for 
	  ;; any previous runs
	  ;; (db:test-remove-steps db run-id testname itemdat)
	  
	  (let* ((m            (make-mutex))
		 (kill-job?    #f)
		 (exit-info    (vector #t #t #t))
		 (job-thread   #f)
		 (runit        (lambda ()
				 ;; (let-values
				 ;;  (((pid exit-status exit-code)
				 ;;    (run-n-wait fullrunscript)))
				 (tests:test-set-status! test-id "RUNNING" "n/a" #f #f)
				 ;; if there is a runscript do it first
				 (if fullrunscript
				     (let ((pid (process-run fullrunscript)))
				       (let loop ((i 0))
					 (let-values
					  (((pid-val exit-status exit-code) (process-wait pid #t)))
					  (mutex-lock! m)
					  (vector-set! exit-info 0 pid)
					  (vector-set! exit-info 1 exit-status)
					  (vector-set! exit-info 2 exit-code)
					  (set! rollup-status exit-code) 
					  (mutex-unlock! m)
					  (if (eq? pid-val 0)
					      (begin
						(thread-sleep! 2)
						(loop (+ i 1)))
					      )))))
				 ;; then, if runscript ran ok (or did not get called)
				 ;; do all the ezsteps (if any)
				 (if ezsteps
				     (let* ((testconfig (read-config (conc work-area "/testconfig") #f #t environ-patt: "pre-launch-env-vars")) ;; FIXME??? is allow-system ok here?
					    (ezstepslst (hash-table-ref/default testconfig "ezsteps" '())))
				       (if (not (file-exists? ".ezsteps"))(create-directory ".ezsteps"))
				       ;; if ezsteps was defined then we are sure to have at least one step but check anyway
				       (if (not (> (length ezstepslst) 0))
					   (debug:print 0 "ERROR: ezsteps defined but ezstepslst is zero length")
					   (let loop ((ezstep (car ezstepslst))
						      (tal    (cdr ezstepslst))
						      (prevstep #f))
					     ;; check exit-info (vector-ref exit-info 1)
					     (if (vector-ref exit-info 1)
						 (let* ((stepname  (car ezstep))  ;; do stuff to run the step
							(stepinfo  (cadr ezstep))
							(stepparts (string-match (regexp "^(\\{([^\\}]*)\\}\\s*|)(.*)$") stepinfo))
							(stepparms (list-ref stepparts 2)) ;; for future use, {VAR=1,2,3}, run step for each 
							(stepcmd   (list-ref stepparts 3))
							(script    "") ; "#!/bin/bash\n") ;; yep, we depend on bin/bash FIXME!!!
							(logpro-used #f))
						   ;; NB// can safely assume we are in test-area directory
						   (debug:print 4 "ezsteps:\n stepname: " stepname " stepinfo: " stepinfo " stepparts: " stepparts
								" stepparms: " stepparms " stepcmd: " stepcmd)
						   
						   (if (file-exists? (conc stepname ".logpro"))(set! logpro-used #t))

						   ;; ;; first source the previous environment
						   ;; (let ((prev-env (conc ".ezsteps/" prevstep (if (string-search (regexp "csh") 
						   ;;      							 (get-environment-variable "SHELL")) ".csh" ".sh"))))
						   ;;   (if (and prevstep (file-exists? prev-env))
						   ;;       (set! script (conc script "source " prev-env))))
						   
						   ;; call the command using mt_ezstep
						   (set! script (conc "mt_ezstep " stepname " " (if prevstep prevstep "-") " " stepcmd))

						   (debug:print 4 "script: " script)
						   ;; DO NOT remote
						   (db:teststep-set-status! #f test-id stepname "start" "-" #f #f)
						   ;; now launch
						   (let ((pid (process-run script)))
						     (let processloop ((i 0))
						       (let-values (((pid-val exit-status exit-code)(process-wait pid #t)))
								   (mutex-lock! m)
								   (vector-set! exit-info 0 pid)
								   (vector-set! exit-info 1 exit-status)
								   (vector-set! exit-info 2 exit-code)
								   (mutex-unlock! m)
								   (if (eq? pid-val 0)
								       (begin
									 (thread-sleep! 2)
									 (processloop (+ i 1))))
								   ))
                                                     (let ((exinfo (vector-ref exit-info 2))
                                                           (logfna (if logpro-used (conc stepname ".html") "")))
						       ;; testing if procedures called in a remote call cause problems (ans: no or so I suspect)
						       (db:teststep-set-status! #f test-id stepname "end" exinfo #f logfna))
						     (if logpro-used
							 (cdb:test-set-log! *runremote*  test-id (conc stepname ".html")))
						     ;; set the test final status
						     (let* ((this-step-status (cond
									       ((and (eq? (vector-ref exit-info 2) 2) logpro-used) 'warn)
									       ((eq? (vector-ref exit-info 2) 0)                   'pass)
									       (else 'fail)))
							    (overall-status   (cond
									       ((eq? rollup-status 2) 'warn)
									       ((eq? rollup-status 0) 'pass)
									       (else 'fail)))
							    (next-status      (cond 
									       ((eq? overall-status 'pass) this-step-status)
									       ((eq? overall-status 'warn)
										(if (eq? this-step-status 'fail) 'fail 'warn))
									       (else 'fail))))
						       (debug:print 4 "Exit value received: " (vector-ref exit-info 2) " logpro-used: " logpro-used 
								    " this-step-status: " this-step-status " overall-status: " overall-status 
								    " next-status: " next-status " rollup-status: " rollup-status)
						       (case next-status
							 ((warn)
							  (set! rollup-status 2)
							  ;; NB// test-set-status! does rdb calls under the hood
							  (tests:test-set-status! test-id "RUNNING" "WARN" 
									  (if (eq? this-step-status 'warn) "Logpro warning found" #f)
									  #f))
							 ((pass)
							  (tests:test-set-status! test-id "RUNNING" "PASS" #f #f))
							 (else ;; 'fail
							  (set! rollup-status 1) ;; force fail
							  (tests:test-set-status! test-id "RUNNING" "FAIL" (conc "Failed at step " stepname) #f)
							  ))))
						   (if (and (steprun-good? logpro-used (vector-ref exit-info 2))
							    (not (null? tal)))
						       (loop (car tal) (cdr tal) stepname)))
						 (debug:print 4 "WARNING: a prior step failed, stopping at " ezstep))))))))
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
				     (begin
				       (set! kill-job? (test-get-kill-request test-id)) ;; run-id test-name itemdat))
				       ;; open-run-close not needed for test-set-meta-info
				       (test-set-meta-info #f test-id run-id test-name itemdat minutes)
				       (if kill-job? 
					   (begin
					     (mutex-lock! m)
					     (let* ((pid (vector-ref exit-info 0)))
					       (if (number? pid)
						   (begin
						     (debug:print 0 "WARNING: Request received to kill job (attempt # " kill-tries ")")
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
						       (system (conc "kill -9 -" pid))))
						   (begin
						     (debug:print 0 "WARNING: Request received to kill job but problem with process, attempting to kill manager process")
						     (tests:test-set-status! test-id "KILLED"  "FAIL"
								     (args:get-arg "-m") #f)
						     (sqlite3:finalize! tdb)
						     (exit 1))))
					     (set! kill-tries (+ 1 kill-tries))
					     (mutex-unlock! m)))
				       ;; (sqlite3:finalize! db)
				       (thread-sleep! (+ 10 (random 10))) ;; add some jitter to the call home time to spread out the db accesses
				       (loop (calc-minutes)))))))
		 (th1          (make-thread monitorjob))
		 (th2          (make-thread runit)))
	    (set! job-thread th2)
	    (thread-start! th1)
	    (thread-start! th2)
	    (thread-join! th2)
	    (mutex-lock! m)
	    (let* ((item-path (item-list->path itemdat))
		   (testinfo  (cdb:get-test-info-by-id *runremote* test-id))) ;; )) ;; run-id test-name item-path)))
	      ;; Am I completed?
	      (if (not (equal? (db:test-get-state testinfo) "COMPLETED"))
		  (begin
		    (debug:print 2 "Test NOT logged as COMPLETED, (state=" (db:test-get-state testinfo) "), updating result, rollup-status is " rollup-status)
		    (tests:test-set-status! test-id 
				    (if kill-job? "KILLED" "COMPLETED")
				    (cond
				     ((not (vector-ref exit-info 1)) "FAIL") ;; job failed to run
				     ((eq? rollup-status 0)
				      ;; if the current status is AUTO the defer to the calculated value (i.e. leave this AUTO)
				      (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO" "PASS"))
				     ((eq? rollup-status 1) "FAIL")
				     ((eq? rollup-status 2)
				      ;; if the current status is AUTO the defer to the calculated value but qualify (i.e. make this AUTO-WARN)
				      (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO-WARN" "WARN"))
				     (else "FAIL"))
				    (args:get-arg "-m") #f)))
	      ;; for automated creation of the rollup html file this is a good place...
	      (if (not (equal? item-path ""))
		  (tests:summarize-items run-id test-name #f)) ;; don't force - just update if no
	      )
	    (mutex-unlock! m)
	    ;; (exec-results (cmd-run->list fullrunscript)) ;;  (list ">" (conc test-name "-run.log"))))
	    ;; (success      exec-results)) ;; (eq? (cadr exec-results) 0)))
	    (debug:print 2 "Output from running " fullrunscript ", pid " (vector-ref exit-info 0) " in work area " 
			 work-area ":\n====\n exit code " (vector-ref exit-info 2) "\n" "====\n")
	    ;; (sqlite3:finalize! db)
	    ;; (sqlite3:finalize! tdb)
	    (if (not (vector-ref exit-info 1))
		(exit 4)))))))

;; set up the very basics needed for doing anything here.
(define (setup-for-run)
  ;; would set values for KEYS in the environment here for better support of env-override but 
  ;; have chicken/egg scenario. need to read megatest.config then read it again. Going to 
  ;; pass on that idea for now
  ;; special case
  (set! *configinfo* (find-and-read-config 
		      (if (args:get-arg "-config")(args:get-arg "-config") "megatest.config")
		      environ-patt: "env-override"
		      given-toppath: (get-environment-variable "MT_RUN_AREA_HOME")
		      pathenvvar: "MT_RUN_AREA_HOME"))
  (set! *configdat*  (if (car *configinfo*)(car *configinfo*) #f))
  (set! *toppath*    (if (car *configinfo*)(cadr *configinfo*) #f))
  (if *toppath*
      (setenv "MT_RUN_AREA_HOME" *toppath*) ;; to be deprecated
      (debug:print 0 "ERROR: failed to find the top path to your run setup."))
  *toppath*)

(define (get-best-disk confdat)
  (let* ((disks    (hash-table-ref/default confdat "disks" #f))
	 (best     #f)
	 (bestsize 0))
    (if disks 
	(for-each 
	 (lambda (disk-num)
	   (let* ((dirpath    (cadr (assoc disk-num disks)))
		  (freespc    (if (and (directory? dirpath)
				       (file-write-access? dirpath))
				  (get-df dirpath)
				  (begin
				    (debug:print 0 "WARNING: path " dirpath " in [disks] section not valid or writable")
				    0))))
	     (if (> freespc bestsize)
		 (begin
		   (set! best     dirpath)
		   (set! bestsize freespc)))))
	 (map car disks)))
    (if best
	best
	(begin
	  (debug:print 0 "ERROR: No valid disks found in megatest.config. Please add some to your [disks] section")
	  (exit 1)))))

;; Desired directory structure:
;;
;;  <linkdir> - <target> - <testname> -.
;;                                     |
;;                                     v
;;  <rundir>  -  <target>  -    <testname> -|- <itempath(s)>
;;
;;  dir stored in test is:
;; 
;;  <linkdir> - <target> - <testname> [ - <itempath> ]
;; 
;; All log file links should be stored relative to the top of link path
;;  
;; <target> - <testname> [ - <itempath> ] 
;;
(define (create-work-area db run-id test-id test-src-path disk-path testname itemdat)
  (let* ((run-info (cdb:remote-run db:get-run-info #f run-id))
	 (item-path (item-list->path itemdat))
	 (runname  (db:get-value-by-header (db:get-row run-info)
					   (db:get-header run-info)
					   "runname"))
	 ;; convert back to db: from rdb: - this is always run at server end
	 (key-vals (cdb:remote-run db:get-key-vals #f run-id))
	 (target   (string-intersperse key-vals "/"))

	 (not-iterated  (equal? "" item-path))

	 ;; all tests are found at <rundir>/test-base or <linkdir>/test-base
	 (testtop-base (conc target "/" runname "/" testname))
	 (test-base    (conc testtop-base (if not-iterated "" "/") item-path))

	 ;; nb// if itempath is not "" then it is prefixed with "/"
	 (toptest-path (conc disk-path "/" testtop-base))
	 (test-path    (conc disk-path "/" test-base))

	 ;; ensure this exists first as links to subtests must be created there
	 (linktree  (let ((rd (config-lookup *configdat* "setup" "linktree")))
		      (if rd rd (conc *toppath* "/runs"))))

	 (lnkbase  (conc linktree "/" target "/" runname))
	 (lnkpath  (conc lnkbase "/" testname))
	 (lnkpathf (conc lnkpath (if not-iterated "" "/") item-path)))

    ;; Update the rundir path in the test record for all
    (cdb:test-set-rundir-by-test-id *runremote* test-id 
				    (if (equal? (configf:lookup *configdat* "setup" "testpath") "runpath")
					test-path
					lnkpathf))

    (debug:print 2 "INFO:\n       lnkbase=" lnkbase "\n       lnkpath=" lnkpath "\n  toptest-path=" toptest-path "\n     test-path=" test-path)
    (if (not (file-exists? linktree))
	(begin
	  (debug:print 0 "WARNING: linktree did not exist! Creating it now at " linktree)
	  (create-directory linktree #t))) ;; (system (conc "mkdir -p " linktree))))
    ;; create the directory for the tests dir links, this is needed no matter what...
    (if (not (directory-exists? lnkbase))
	(create-directory lnkbase #t))
    
    ;; update the toptest record with its location rundir, cache the path
    ;; This wass highly inefficient, one db write for every subtest, potentially
    ;; thousands of unnecessary updates, cache the fact it was set and don't set it 
    ;; again. 

    ;; NB - This is not working right - some top tests are not getting the path set!!!

    (if (not (hash-table-ref/default *toptest-paths* testname #f))
	(let* ((testinfo       (cdb:get-test-info-by-id *runremote* test-id)) ;;  run-id testname item-path))
	       (curr-test-path (if testinfo (db:test-get-rundir testinfo) #f)))
	  (hash-table-set! *toptest-paths* testname curr-test-path)
	  ;; NB// Was this for the test or for the parent in an iterated test?
	  (cdb:test-set-rundir! *runremote* run-id testname "" 
				(if (equal? (configf:lookup *configdat* "setup" "testpath") "runpath")
				    toptest-path
				    lnkpath))
	  (if (or (not curr-test-path)
		  (not (directory-exists? toptest-path)))
	      (begin
		(debug:print-info 2 "Creating " toptest-path " and link " lnkpath)
		(create-directory toptest-path #t)
		(hash-table-set! *toptest-paths* testname toptest-path)))))

    ;; Now create the link from the test path to the link tree, however
    ;; if the test is iterated it is necessary to create the parent path
    ;; to the iteration. use pathname-directory to trim the path by one
    ;; level
    (if (not not-iterated) ;; i.e. iterated
	(let ((iterated-parent  (pathname-directory (conc lnkpath "/" item-path))))
	  (debug:print-info 2 "Creating iterated parent " iterated-parent)
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 0 "ERROR:  Failed to create directory " iterated-parent ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit 1))
	   (create-directory iterated-parent #t))))

    (if (symbolic-link? lnkpath) 
	(handle-exceptions
	 exn
	 (begin
	   (debug:print 0 "ERROR:  Failed to remove symlink " lnkpath ((condition-property-accessor 'exn 'message) exn) ", exiting")
	   (exit 1))
	 (delete-file lnkpath)))

    (if (not (or (file-exists? lnkpath)
		 (symbolic-link? lnkpath)))
	(handle-exceptions
	 exn
	 (begin
	   (debug:print 0 "ERROR:  Failed to create symlink " lnkpath ((condition-property-accessor 'exn 'message) exn) ", exiting")
	   (exit 1))
	 (create-symbolic-link toptest-path lnkpath)))
    
    ;; The toptest path has been created, the link to the test in the linktree has
    ;; been created. Now, if this is an iterated test the real test dir must be created
    (if (not not-iterated) ;; this is an iterated test
	(let ((lnktarget (conc lnkpath "/" item-path)))
	  (debug:print 2 "Setting up sub test run area")
	  (debug:print 2 " - creating run area in " test-path)
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 0 "ERROR:  Failed to create directory " test-path ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit 1))
	   (create-directory test-path #t))
	  (debug:print 2 
		       " - creating link from: " test-path "\n"
		       "                   to: " lnktarget)

	  ;; If there is already a symlink delete it and recreate it.
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print 0 "ERROR:  Failed to re-create link " linktarget ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit))
	   (if (symbolic-link? lnktarget)     (delete-file lnktarget))
	   (if (not (file-exists? lnktarget)) (create-symbolic-link test-path lnktarget)))))

    ;; I suspect this section was deleting test directories under some 
    ;; wierd sitations? This doesn't make sense - reenabling the rm -f 
    ;; I honestly don't remember *why* this chunk was needed...
    ;; (let ((testlink (conc lnkpath "/" testname)))
    ;;   (if (and (file-exists? testlink)
    ;;            (or (regular-file? testlink)
    ;;     	   (symbolic-link? testlink)))
    ;;       (system (conc "rm -f " testlink)))
    ;;   (system  (conc "ln -sf " test-path " " testlink)))
    (if (directory? test-path)
	(begin
	  (let* ((ovrcmd (let ((cmd (config-lookup *configdat* "setup" "testcopycmd")))
			   (if cmd
			       ;; substitute the TEST_SRC_PATH and TEST_TARG_PATH
			       (string-substitute "TEST_TARG_PATH" test-path
						  (string-substitute "TEST_SRC_PATH" test-src-path cmd))
			       #f)))
		 (cmd    (if ovrcmd 
			     ovrcmd
			     (conc "rsync -av" (if (debug:debug-mode 1) "" "q") " " test-src-path "/ " test-path "/")))
		 (status (system cmd)))
	    (if (not (eq? status 0))
		(debug:print 2 "ERROR: problem with running \"" cmd "\"")))
	  (list lnkpathf lnkpath ))
	(list #f #f))))

;; 1. look though disks list for disk with most space
;; 2. create run dir on disk, path name is meaningful
;; 3. create link from run dir to megatest runs area 
;; 4. remotely run the test on allocated host
;;    - could be ssh to host from hosts table (update regularly with load)
;;    - could be netbatch
;;      (launch-test db (cadr status) test-conf))
(define (launch-test db run-id runname test-conf keyvallst test-name test-path itemdat params)
  (change-directory *toppath*)
  (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
   (list ;; (list "MT_TEST_RUN_DIR" work-area)
    (list "MT_RUN_AREA_HOME" *toppath*)
    (list "MT_TEST_NAME" test-name)
    ;; (list "MT_ITEM_INFO" (conc itemdat)) 
    (list "MT_RUNNAME"   runname)
    ;; (list "MT_TARGET"    mt_target)
    ))
  (let* ((useshell   (config-lookup *configdat* "jobtools"     "useshell"))
	 (launcher   (config-lookup *configdat* "jobtools"     "launcher"))
	 (runscript  (config-lookup test-conf   "setup"        "runscript"))
	 (ezsteps    (> (length (hash-table-ref/default test-conf "ezsteps" '())) 0)) ;; don't send all the steps, could be big
	 (diskspace  (config-lookup test-conf   "requirements" "diskspace"))
	 (memory     (config-lookup test-conf   "requirements" "memory"))
	 (hosts      (config-lookup *configdat* "jobtools"     "workhosts"))
	 (remote-megatest (config-lookup *configdat* "setup" "executable"))
	 ;; FIXME SOMEDAY: not good how this is so obtuse, this hack is to 
	 ;;                allow running from dashboard. Extract the path
	 ;;                from the called megatest and convert dashboard
	 ;;             	  or dboard to megatest
	 (local-megatest  (let* ((lm  (car (argv)))
				 (dir (pathname-directory lm))
				 (exe (pathname-strip-directory lm)))
			    (conc (if dir (conc dir "/") "")
				  (case (string->symbol exe)
				    ((dboard)    "megatest")
				    ((mtest)     "megatest")
				    ((dashboard) "megatest")
				    (else exe)))))
	 (test-sig   (conc test-name ":" (item-list->path itemdat))) ;; test-path is the full path including the item-path
	 (work-area  #f)
	 (toptest-work-area #f) ;; for iterated tests the top test contains data relevant for all
	 (diskpath   #f)
	 (cmdparms   #f)
	 (fullcmd    #f) ;; (define a (with-output-to-string (lambda ()(write x))))
	 (mt-bindir-path #f)
	 (item-path (item-list->path itemdat))
	 (test-id    (cdb:remote-run db:get-test-id #f run-id test-name item-path))
	 (testinfo   (cdb:get-test-info-by-id *runremote* test-id))
	 (mt_target  (string-intersperse (map cadr keyvallst) "/"))
	 (debug-param (append (if (args:get-arg "-debug")  (list "-debug" (args:get-arg "-debug")) '())
			      (if (args:get-arg "-logging")(list "-logging") '()))))
    (if hosts (set! hosts (string-split hosts)))
    ;; set the megatest to be called on the remote host
    (if (not remote-megatest)(set! remote-megatest local-megatest)) ;; "megatest"))
    (set! mt-bindir-path (pathname-directory remote-megatest))
    (if launcher (set! launcher (string-split launcher)))
    ;; set up the run work area for this test
    (set! diskpath (get-best-disk *configdat*))
    (if diskpath
	(let ((dat  (open-run-close create-work-area db run-id test-id test-path diskpath test-name itemdat)))
	  (set! work-area (car dat))
	  (set! toptest-work-area (cadr dat))
	  (debug:print-info 2 "Using work area " work-area))
	(begin
	  (set! work-area (conc test-path "/tmp_run"))
	  (create-directory work-area #t)
	  (debug:print 0 "WARNING: No disk work area specified - running in the test directory under tmp_run")))
    (set! cmdparms (base64:base64-encode 
		    (with-output-to-string
		      (lambda () ;; (list 'hosts     hosts)
			(write (list (list 'testpath  test-path)
				     ;; (list 'runremote *runremote*)
				     (list 'transport (conc *transport-type*))
				     (list 'serverinf *server-info*)
				     (list 'toppath   *toppath*)
				     (list 'work-area work-area)
				     (list 'test-name test-name) 
				     (list 'runscript runscript) 
				     (list 'run-id    run-id   )
				     (list 'test-id   test-id  )
				     (list 'itemdat   itemdat  )
				     (list 'megatest  remote-megatest)
				     (list 'ezsteps   ezsteps) 
				     (list 'target    mt_target)
				     (list 'env-ovrd  (hash-table-ref/default *configdat* "env-override" '())) 
				     (list 'set-vars  (if params (hash-table-ref/default params "-setvars" #f)))
				     (list 'runname   runname)
				     (list 'mt-bindir-path mt-bindir-path))))))) ;; (string-intersperse keyvallst " "))))
    ;; clean out step records from previous run if they exist
    ;; (debug:print-info 4 "FIXMEEEEE!!!! This can be removed some day, perhaps move all test records to the test db?")
    ;; (open-run-close db:delete-test-step-records db test-id)
    (change-directory work-area) ;; so that log files from the launch process don't clutter the test dir
    (tests:test-set-status! test-id "LAUNCHED" "n/a" #f #f) ;; (if launch-results launch-results "FAILED"))
    (cond
     ((and launcher hosts) ;; must be using ssh hostname
      (set! fullcmd (append launcher (car hosts)(list remote-megatest test-sig "-execute" cmdparms) debug-param)))
     ;; (set! fullcmd (append launcher (car hosts)(list remote-megatest test-sig "-execute" cmdparms))))
     (launcher
      (set! fullcmd (append launcher (list remote-megatest test-sig "-execute" cmdparms) debug-param)))
     ;; (set! fullcmd (append launcher (list remote-megatest test-sig "-execute" cmdparms))))
     (else
      (if (not useshell)(debug:print 0 "WARNING: internal launching will not work well without \"useshell yes\" in your [jobtools] section"))
      (set! fullcmd (append (list remote-megatest test-sig "-execute" cmdparms) debug-param (list (if useshell "&" ""))))))
    ;; (set! fullcmd (list remote-megatest test-sig "-execute" cmdparms (if useshell "&" "")))))
    (if (args:get-arg "-xterm")(set! fullcmd (append fullcmd (list "-xterm"))))
    (debug:print 1 "Launching " work-area)
    ;; set pre-launch-env-vars before launching, keep the vars in prevvals and put the envionment back when done
    (debug:print 4 "fullcmd: " fullcmd)
    (let* ((commonprevvals (alist->env-vars
			    (hash-table-ref/default *configdat* "env-override" '())))
	   (testprevvals   (alist->env-vars
			    (hash-table-ref/default test-conf "pre-launch-env-overrides" '())))
	   (miscprevvals   (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
			    (append (list (list "MT_TEST_RUN_DIR" work-area)
					  (list "MT_TEST_NAME" test-name)
					  (list "MT_ITEM_INFO" (conc itemdat)) 
					  (list "MT_RUNNAME"   runname)
					  (list "MT_TARGET"    mt_target)
					  )
				    itemdat)))
	   (launch-results (apply cmd-run-with-stderr->list ;; cmd-run-proc-each-line
				  (if useshell
				      (string-intersperse fullcmd " ")
				      (car fullcmd))
				  ;; conc
				  (if useshell
				      '()
				      (cdr fullcmd))))) ;;  launcher fullcmd)));; (apply cmd-run-proc-each-line launcher print fullcmd))) ;; (cmd-run->list fullcmd))
      (with-output-to-file "mt_launch.log"
	(lambda ()
	  (apply print launch-results)))
      (debug:print 2 "Launching completed, updating db")
      (debug:print 2 "Launch results: " launch-results)
      (if (not launch-results)
	  (begin
	    (print "ERROR: Failed to run " (string-intersperse fullcmd " ") ", exiting now")
	    ;; (sqlite3:finalize! db)
	    ;; good ole "exit" seems not to work
	    ;; (_exit 9)
	    ;; but this hack will work! Thanks go to Alan Post of the Chicken email list
	    ;; NB// Is this still needed? Should be safe to go back to "exit" now?
	    (process-signal (current-process-id) signal/kill)
	    ))
      (alist->env-vars miscprevvals)
      (alist->env-vars testprevvals)
      (alist->env-vars commonprevvals)
      launch-results))
  (change-directory *toppath*))

