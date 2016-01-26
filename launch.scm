
;; Copyright 2006-2013, Matthew Welland.
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

(use regex regex-case base64 sqlite3 srfi-18 directory-utils posix-extras z3 call-with-environment-variables)
(use defstruct)

(import (prefix base64 base64:))
(import (prefix sqlite3 sqlite3:))

(declare (unit launch))
(declare (uses common))
(declare (uses configf))
(declare (uses db))
;; (declare (uses sdb))
(declare (uses tdb))
;; (declare (uses filedb))

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
    (if enccmd
	(common:read-encoded-string enccmd)
	'())))

;;                       0           1              2              3
(defstruct launch:einf (pid #t)(exit-status #t)(exit-code #t)(rollup-status 0))

(define (launch:runstep ezstep run-id test-id exit-info m tal testconfig)
  (let* ((stepname       (car ezstep))  ;; do stuff to run the step
	 (stepinfo       (cadr ezstep))
	 (stepparts      (string-match (regexp "^(\\{([^\\}]*)\\}\\s*|)(.*)$") stepinfo))
	 (stepparms      (list-ref stepparts 2)) ;; for future use, {VAR=1,2,3}, run step for each 
	 (stepcmd        (list-ref stepparts 3))
	 (script         "") ; "#!/bin/bash\n") ;; yep, we depend on bin/bash FIXME!!!\
	 (logpro-file    (conc stepname ".logpro"))
	 (html-file      (conc stepname ".html"))
	 (tconfig-logpro (configf:lookup testconfig "logpro" stepname))
	 (logpro-used    (file-exists? logpro-file)))

    (if (and tconfig-logpro
	     (not logpro-used)) ;; no logpro file found but have a defn in the testconfig
	(begin
	  (with-output-to-file logpro-file
	    (lambda ()
	      (print ";; logpro file extracted from testconfig\n"
		     ";;")
	      (print tconfig-logpro)))
	  (set! logpro-used #t)))
    
    ;; NB// can safely assume we are in test-area directory
    (debug:print 4 "ezsteps:\n stepname: " stepname " stepinfo: " stepinfo " stepparts: " stepparts
		 " stepparms: " stepparms " stepcmd: " stepcmd)
    
    ;; ;; first source the previous environment
    ;; (let ((prev-env (conc ".ezsteps/" prevstep (if (string-search (regexp "csh") 
    ;;      							 (get-environment-variable "SHELL")) ".csh" ".sh"))))
    ;;   (if (and prevstep (file-exists? prev-env))
    ;;       (set! script (conc script "source " prev-env))))
    
    ;; call the command using mt_ezstep
    ;; (set! script (conc "mt_ezstep " stepname " " (if prevstep prevstep "x") " " stepcmd))
    
    (debug:print 4 "script: " script)
    (rmt:teststep-set-status! run-id test-id stepname "start" "-" #f #f)
    ;; now launch the actual process
    (call-with-environment-variables 
     (list (cons "PATH" (conc (get-environment-variable "PATH") ":.")))
     (lambda () ;; (process-run "/bin/bash" "-c" "exec ls -l /tmp/foobar > /tmp/delme-more.log 2>&1")
       (let* ((cmd (conc stepcmd " > " stepname ".log 2>&1")) ;; >outfile 2>&1 
	      (pid (process-run "/bin/bash" (list "-c" cmd))))
	 (rmt:test-set-top-process-pid run-id test-id pid)
	 (let processloop ((i 0))
	   (let-values (((pid-val exit-status exit-code)(process-wait pid #t)))
		       (mutex-lock! m)
		       (launch:einf-pid-set!         exit-info pid)         ;; (vector-set! exit-info 0 pid)
		       (launch:einf-exit-status-set! exit-info exit-status) ;; (vector-set! exit-info 1 exit-status)
		       (launch:einf-exit-code-set!   exit-info exit-code)   ;; (vector-set! exit-info 2 exit-code)
		       (mutex-unlock! m)
		       (if (eq? pid-val 0)
			   (begin
			     (thread-sleep! 2)
			     (processloop (+ i 1))))
		       )))))
    (debug:print-info 0 "step " stepname " completed with exit code " (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
    ;; now run logpro if needed
    (if logpro-used
	(let ((pid (process-run (conc "logpro " logpro-file " " (conc stepname ".html") " < " stepname ".log"))))
	  (let processloop ((i 0))
	    (let-values (((pid-val exit-status exit-code)(process-wait pid #t)))
			(mutex-lock! m)
			;; (make-launch:einf pid: pid exit-status: exit-status exit-code: exit-code)
			(launch:einf-pid-set!         exit-info pid)         ;; (vector-set! exit-info 0 pid)
			(launch:einf-exit-status-set! exit-info exit-status) ;; (vector-set! exit-info 1 exit-status)
			(launch:einf-exit-code-set!   exit-info exit-code)   ;; (vector-set! exit-info 2 exit-code)
			(mutex-unlock! m)
			(if (eq? pid-val 0)
			    (begin
			      (thread-sleep! 2)
			      (processloop (+ i 1)))))
	    (debug:print-info 0 "logpro for step " stepname " exited with code " (launch:einf-exit-code exit-info))))) ;; (vector-ref exit-info 2)))))
    
    (let ((exinfo (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
	  (logfna (if logpro-used (conc stepname ".html") "")))
      (rmt:teststep-set-status! run-id test-id stepname "end" exinfo #f logfna))
    (if logpro-used
	(rmt:test-set-log! run-id test-id (conc stepname ".html")))
    ;; set the test final status
    (let* ((process-exit-status (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
	   (this-step-status (cond
			      ((and (eq? process-exit-status 2) logpro-used) 'warn)  ;; logpro 2 = warnings
			      ((and (eq? process-exit-status 3) logpro-used) 'check) ;; logpro 3 = check
			      ((and (eq? process-exit-status 4) logpro-used) 'waived) ;; logpro 4 = abort			      
			      ((and (eq? process-exit-status 5) logpro-used) 'abort) ;; logpro 4 = abort
			      ((eq? process-exit-status 0)                   'pass)  ;; logpro 0 = pass
			      (else 'fail)))
	   (overall-status   (cond
			      ((eq? (launch:einf-rollup-status exit-info) 2) 'warn) ;; rollup-status (vector-ref exit-info 3)
			      ((eq? (launch:einf-rollup-status exit-info) 0) 'pass) ;; (vector-ref exit-info 3)
			      (else 'fail)))
	   (next-status      (cond 
			      ((eq? overall-status 'pass) this-step-status)
			      ((eq? overall-status 'warn)
			       (if (eq? this-step-status 'fail) 'fail 'warn))
			      ((eq? overall-status 'abort) 'abort)
			      (else 'fail)))
	   (next-state       ;; "RUNNING") ;; WHY WAS THIS CHANGED TO NOT USE (null? tal) ??
	    (cond
	     ((null? tal) ;; more to run?
	      "COMPLETED")
	     (else "RUNNING")))
	   )
      (debug:print 4 "Exit value received: " (launch:einf-exit-code exit-info) " logpro-used: " logpro-used 
		   " this-step-status: " this-step-status " overall-status: " overall-status 
		   " next-status: " next-status " rollup-status: "  (launch:einf-rollup-status exit-info)) ;; (vector-ref exit-info 3))
      (case next-status
	((warn)
	 (launch:einf-rollup-status-set! exit-info 2) ;; (vector-set! exit-info 3 2) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "WARN" 
				 (if (eq? this-step-status 'warn) "Logpro warning found" #f)
				 #f))
	((check)
	 (launch:einf-rollup-status-set! exit-info 3) ;; (vector-set! exit-info 3 3) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "CHECK" 
				 (if (eq? this-step-status 'check) "Logpro check found" #f)
				 #f))
	((abort)
	 (launch:einf-rollup-status-set! exit-info 4) ;; (vector-set! exit-info 3 4) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "ABORT" 
				 (if (eq? this-step-status 'abort) "Logpro abort found" #f)
				 #f))
	((pass)
	 (tests:test-set-status! run-id test-id next-state "PASS" #f #f))
	(else ;; 'fail
	 (launch:einf-rollup-status-set! exit-info 1) ;; (vector-set! exit-info 3 1) ;; force fail, this used to be next-state but that doesn't make sense. should always be "COMPLETED" 
	 (tests:test-set-status! run-id test-id "COMPLETED" "FAIL" (conc "Failed at step " stepname) #f)
	 )))
    logpro-used))

(define (launch:execute encoded-cmd)
  
   (let* ((cmdinfo    (common:read-encoded-string encoded-cmd))
	  (tconfigreg (tests:get-all)))
    (setenv "MT_CMDINFO" encoded-cmd)
    (if (list? cmdinfo) ;; ((testpath /tmp/mrwellan/jazzmind/src/example_run/tests/sqlitespeed)
	;; (test-name sqlitespeed) (runscript runscript.rb) (db-host localhost) (run-id 1))
	(let* ((testpath  (assoc/default 'testpath  cmdinfo))  ;; testpath is the test spec area
	       (top-path  (assoc/default 'toppath   cmdinfo))
	       (work-area (assoc/default 'work-area cmdinfo))  ;; work-area is the test run area
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (ezsteps   (assoc/default 'ezsteps   cmdinfo))
	       ;; (runremote (assoc/default 'runremote cmdinfo))
	       (transport (assoc/default 'transport cmdinfo))
	       ;; (serverinf (assoc/default 'serverinf cmdinfo))
	       (port      (assoc/default 'port      cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (test-id   (assoc/default 'test-id   cmdinfo))
	       (target    (assoc/default 'target    cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (env-ovrd  (assoc/default 'env-ovrd  cmdinfo))
	       (set-vars  (assoc/default 'set-vars  cmdinfo)) ;; pre-overrides from -setvar
	       (runname   (assoc/default 'runname   cmdinfo))
	       (megatest  (assoc/default 'megatest  cmdinfo))
	       (runtlim   (assoc/default 'runtlim   cmdinfo))
	       (item-path (item-list->path itemdat))
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
	       ;; (rollup-status 0)
	       )

	  ;; NFS might not have propagated the directory meta data to the run host - give it time if needed
	  (let loop ((count 0))
	    (if (or (file-exists? top-path)
		    (> count 10))
		(change-directory top-path)
		(begin
		  (debug:print 0 "INFO: Not starting job yet - directory " top-path " not found")
		  (thread-sleep! 10)
		  (loop (+ count 1)))))

	  (let ((sighand (lambda (signum)
			   ;; (signal-mask! signum) ;; to mask or not? seems to cause issues in exiting
			   (if (eq? signum signal/stop)
			 (debug:print 0 "ERROR: attempt to STOP process. Exiting."))
			   (set! *time-to-exit* #t)
			   (print "Received signal " signum ", cleaning up before exit. Please wait...")
			   (let ((th1 (make-thread (lambda ()
						     (tests:test-force-state-status! run-id test-id "INCOMPLETE" "KILLED")
						     (print "Killed by signal " signum ". Exiting")
						     (thread-sleep! 1)
						     (exit 1))))
				 (th2 (make-thread (lambda ()
						     (thread-sleep! 2)
						     (debug:print 0 "Done")
						     (exit 4)))))
			     (thread-start! th2)
			     (thread-start! th1)
			     (thread-join! th2)))))
	    (set-signal-handler! signal/int sighand)
	    (set-signal-handler! signal/term sighand)
	    (set-signal-handler! signal/stop sighand))
	  
	  ;; (set-signal-handler! signal/int (lambda ()
					    
	  ;; Do not run the test if it is REMOVING, RUNNING, KILLREQ or REMOTEHOSTSTART,
	  ;; Mark the test as REMOTEHOSTSTART *IMMEDIATELY*
	  ;;
	  (let ((test-info (rmt:get-testinfo-state-status run-id test-id)))
	    (cond
	     ((member (db:test-state test-info) '("INCOMPLETE" "KILLED" "UNKNOWN" "KILLREQ" "STUCK")) ;; prior run of this test didn't complete, go ahead and try to rerun
	      (debug:print 0 "INFO: test is INCOMPLETE or KILLED, treat this execute call as a rerun request")
	      (tests:test-force-state-status! run-id test-id "REMOTEHOSTSTART" "n/a")) ;; prime it for running
	     ((not (member (db:test-state test-info) '("REMOVING" "REMOTEHOSTSTART" "RUNNING" "KILLREQ")))
	      (tests:test-force-state-status! run-id test-id "REMOTEHOSTSTART" "n/a"))
	     (else ;; (member (db:test-state test-info) '("REMOVING" "REMOTEHOSTSTART" "RUNNING" "KILLREQ"))
	      (debug:print 0 "ERROR: test state is " (db:test-state test-info) ", cannot proceed")
	      (exit))))
	  
	  (debug:print 2 "Exectuing " test-name " (id: " test-id ") on " (get-host-name))
	  (set! keys       (rmt:get-keys))
	  ;; (runs:set-megatest-env-vars run-id inkeys: keys inkeyvals: keyvals) ;; these may be needed by the launching process
	  ;; one of these is defunct/redundant ...
	  (if (not (launch:setup-for-run force: #t))
	      (begin
		(debug:print 0 "Failed to setup, exiting") 
		;; (sqlite3:finalize! db)
		;; (sqlite3:finalize! tdb)
		(exit 1)))
	  (change-directory *toppath*) 

	  ;; NOTE: Current order is to process runconfigs *before* setting the MT_ vars. This 
	  ;;       seems non-ideal but could well break stuff
	  ;;    BUG? BUG? BUG?

	  (let ((rconfig (full-runconfigs-read))) ;; (read-config (conc  *toppath* "/runconfigs.config") #f #t sections: (list "default" target))))
	    ;; (setup-env-defaults (conc *toppath* "/runconfigs.config") run-id (make-hash-table) keyvals target)
	    ;; (set-run-config-vars run-id keyvals target) ;; (db:get-target db run-id))
	    ;; Now have runconfigs data loaded, set environment vars
	    (for-each (lambda (section)
			(for-each (lambda (varval)
				    (let ((var (car varval))
					  (val (cadr varval)))
				      (if (and (string? var)(string? val))
					  (begin
					    (setenv var (config:eval-string-in-environment val))) ;; val)
					  (debug:print 0 "ERROR: bad variable spec, " var "=" val))))
				  (configf:get-section rconfig section)))
		      (list "default" target)))

	  ;; NFS might not have propagated the directory meta data to the run host - give it time if needed
	  (let loop ((count 0))
	    (if (or (file-exists? work-area)
		    (> count 10))
		(change-directory work-area)
		(begin
		  (debug:print 0 "INFO: Not starting job yet - directory " work-area " not found")
		  (thread-sleep! 10)
		  (loop (+ count 1)))))

	  ;; (change-directory work-area) 
	  (set! keyvals    (keys:target->keyval keys target))
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
	  (for-each
	   (lambda (varval)
	     (let ((var (car varval))
		   (val (cadr varval)))
	       (if val
		   (setenv var val)
		   (begin
		     (debug:print 0 "ERROR: required variable " var " does not have a valid value. Exiting")
		     (exit)))))
	     (list 
	      (list  "MT_TEST_RUN_DIR" work-area)
	      (list  "MT_TEST_NAME" test-name)
	      (list  "MT_ITEM_INFO" (conc itemdat))
	      (list  "MT_ITEMPATH"  item-path)
	      (list  "MT_RUNNAME"   runname)
	      (list  "MT_MEGATEST"  megatest)
	      (list  "MT_TARGET"    target)
	      (list  "MT_LINKTREE"  (configf:lookup *configdat* "setup" "linktree"))
	      (list  "MT_TESTSUITENAME" (common:get-testsuite-name))))

	  (if mt-bindir-path (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path)))
	  ;; (change-directory top-path)
	  ;; Can setup as client for server mode now
	  ;; (client:setup)

	  
	  ;; environment overrides are done *before* the remaining critical envars.
	  (alist->env-vars env-ovrd)
	  (runs:set-megatest-env-vars run-id inkeys: keys inkeyvals: keyvals)
	  (set-item-env-vars itemdat)
	  (save-environment-as-files "megatest")
	  ;; open-run-close not needed for test-set-meta-info
	  ;; (tests:set-full-meta-info #f test-id run-id 0 work-area)
	  ;; (tests:set-full-meta-info test-id run-id 0 work-area)
	  (tests:set-full-meta-info #f test-id run-id 0 work-area 10)

	  (thread-sleep! 0.3) ;; NFS slowness has caused grief here

	  (if (args:get-arg "-xterm")
	      (set! fullrunscript "xterm")
	      (if (and fullrunscript 
		       (file-exists? fullrunscript)
		       (not (file-execute-access? fullrunscript)))
		  (system (conc "chmod ug+x " fullrunscript))))

	  ;; We are about to actually kick off the test
	  ;; so this is a good place to remove the records for 
	  ;; any previous runs
	  ;; (db:test-remove-steps db run-id testname itemdat)
	  
	  (let* ((m            (make-mutex))
		 (kill-job?    #f)
		 (exit-info    (make-launch:einf pid: #t exit-status: #t exit-code: #t rollup-status: 0)) ;; pid exit-status exit-code (i.e. process was successfully run) rollup-status
		 (job-thread   #f)
		 (keep-going   #t)
		 (runit        (lambda ()
				 ;; (let-values
				 ;;  (((pid exit-status exit-code)
				 ;;    (run-n-wait fullrunscript)))
				 ;; (tests:test-set-status! test-id "RUNNING" "n/a" #f #f)
				 ;; Since we should have a clean slate at this time there is no need to do 
				 ;; any of the other stuff that tests:test-set-status! does. Let's just 
				 ;; force RUNNING/n/a
				 

				 ;; (thread-sleep! 0.3)
				 (tests:test-force-state-status! run-id test-id "RUNNING" "n/a")
				 (rmt:roll-up-pass-fail-counts run-id test-name item-path #f "RUNNING")
				 ;; (thread-sleep! 0.3) ;; NFS slowness has caused grief here

				 ;; if there is a runscript do it first
				 (if fullrunscript
				     (let ((pid (process-run fullrunscript)))
				       (rmt:test-set-top-process-pid run-id test-id pid)
				       (let loop ((i 0))
					 (let-values
					  (((pid-val exit-status exit-code) (process-wait pid #t)))
					  (mutex-lock! m)
					  (launch:einf-pid-set!           exit-info  pid)         ;; (vector-set! exit-info 0 pid)
					  (launch:einf-exit-status-set!   exit-info  exit-status) ;; (vector-set! exit-info 1 exit-status)
					  (launch:einf-exit-code-set!     exit-info  exit-code)   ;; (vector-set! exit-info 2 exit-code)
					  (launch:einf-rollup-status-set! exit-info  exit-code)   ;; (vector-set! exit-info 3 exit-code)  ;; rollup status
					  (mutex-unlock! m)
					  (if (eq? pid-val 0)
					      (begin
						(thread-sleep! 2)
						(loop (+ i 1)))
					      )))))
				 ;; then, if runscript ran ok (or did not get called)
				 ;; do all the ezsteps (if any)
				 (if ezsteps
				     (let* ((testconfig ;; (read-config (conc work-area "/testconfig") #f #t environ-patt: "pre-launch-env-vars")) ;; FIXME??? is allow-system ok here?
					     ;; NOTE: it is tempting to turn off force-create of testconfig but dynamic
					     ;;       ezstep names need a full re-eval here.
					     (tests:get-testconfig test-name tconfigreg #t force-create: #t)) ;; 'return-procs)))
					    (ezstepslst (hash-table-ref/default testconfig "ezsteps" '())))
				       (hash-table-set! *testconfigs* test-name testconfig) ;; cached for lazy reads later ...
				       (if (not (file-exists? ".ezsteps"))(create-directory ".ezsteps"))
				       ;; if ezsteps was defined then we are sure to have at least one step but check anyway
				       (if (not (> (length ezstepslst) 0))
					   (debug:print 0 "ERROR: ezsteps defined but ezstepslst is zero length")
					   (let loop ((ezstep (car ezstepslst))
						      (tal    (cdr ezstepslst))
						      (prevstep #f))
					     ;; check exit-info (vector-ref exit-info 1)
					     (if (launch:einf-exit-status exit-info) ;; (vector-ref exit-info 1)
						 (let ((logpro-used (launch:runstep ezstep run-id test-id exit-info m tal testconfig)))
						   (if (and (steprun-good? logpro-used (launch:einf-exit-code exit-info))
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
				   ;; (tests:set-full-meta-info #f test-id run-id (calc-minutes) work-area)
				   ;; (tests:set-full-meta-info test-id run-id (calc-minutes) work-area)
				   (tests:set-full-meta-info #f test-id run-id (calc-minutes) work-area 10)
				   (let loop ((minutes   (calc-minutes))
					      (cpu-load  (get-cpu-load))
					      (disk-free (get-df (current-directory))))
				     (let ((new-cpu-load (let* ((load  (get-cpu-load))
								(delta (abs (- load cpu-load))))
							   (if (> delta 0.6) ;; don't bother updating with small changes
							       load
							       #f)))
					   (new-disk-free (let* ((df    (get-df (current-directory)))
								 (delta (abs (- df disk-free))))
							    (if (> delta 200) ;; ignore changes under 200 Meg
								df
								#f))))
				       (set! kill-job? (or (test-get-kill-request run-id test-id) ;; run-id test-name itemdat))
							   (and runtlim (let* ((run-seconds   (- (current-seconds) start-seconds))
									       (time-exceeded (> run-seconds runtlim)))
									  (if time-exceeded
									      (begin
										(debug:print-info 0 "KILLING TEST DUE TO TIME LIMIT EXCEEDED! Runtime=" run-seconds " seconds, limit=" runtlim)
										#t)
									      #f)))))
				       (tests:update-central-meta-info run-id test-id new-cpu-load new-disk-free (calc-minutes) #f #f)
				       (if kill-job? 
					   (begin
					     (mutex-lock! m)
					     ;; NOTE: The pid can change as different steps are run. Do we need handshaking between this
					     ;;       section and the runit section? Or add a loop that tries three times with a 1/4 second
					     ;;       between tries?
					     (let* ((pid1 (launch:einf-pid exit-info)) ;; (vector-ref exit-info 0))
						    (pid2 (rmt:test-get-top-process-pid run-id test-id))
						    (pids (delete-duplicates (filter number? (list pid1 pid2)))))
					       (if (not (null? pids))
						   (begin
						     (for-each
						      (lambda (pid)
							(handle-exceptions
							 exn
							 (begin
							   (debug:print-info 0 "Unable to kill process with pid " pid ", possibly already killed.")
							   (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn)))
							 (debug:print 0 "WARNING: Request received to kill job " pid) ;;  " (attempt # " kill-tries ")")
							 (debug:print-info 0 "Signal mask=" (signal-mask))
							 ;; (if (process:alive? pid)
							 ;;     (begin
							 (map (lambda (pid-num)
								(process-signal pid-num signal/term))
							      (process:get-sub-pids pid))
							 (thread-sleep! 5)
							 ;; (if (process:process-alive? pid)
							 (map (lambda (pid-num)
								(handle-exceptions
								 exn
								 #f
								 (process-signal pid-num signal/kill)))
							      (process:get-sub-pids pid))))
							 ;;    (debug:print-info 0 "not killing process " pid " as it is not alive"))))
						      pids)
						     (tests:test-set-status! run-id test-id "KILLED"  "KILLED" (args:get-arg "-m") #f))
						   (begin
						     (debug:print 0 "ERROR: Nothing to kill, pid1=" pid1 ", pid2=" pid2)
						     (tests:test-set-status! run-id test-id "KILLED"  "FAILED TO KILL" (args:get-arg "-m") #f)
						     )))
					     (mutex-unlock! m)
					     ;; no point in sticking around. Exit now.
					     (exit)))
				       (if keep-going
					   (begin
					     (thread-sleep! 3) ;; (+ 3 (random 6))) ;; add some jitter to the call home time to spread out the db accesses
					     (if keep-going    ;; keep originals for cpu-load and disk-free unless they change more than the allowed delta
						 (loop (calc-minutes) (or new-cpu-load cpu-load) (or new-disk-free disk-free)))))))
				   (tests:update-central-meta-info run-id test-id (get-cpu-load) (get-df (current-directory))(calc-minutes) #f #f)))) ;; NOTE: Checking twice for keep-going is intentional
		 (th1          (make-thread monitorjob "monitor job"))
		 (th2          (make-thread runit "run job")))
	    (set! job-thread th2)
	    (thread-start! th1)
	    (thread-start! th2)
	    (thread-join! th2)
	    (debug:print-info 0 "Megatest exectute of test " test-name ", item path " item-path " complete. Notifying the db ...")
	    (set! keep-going #f)
	    (thread-join! th1)
	    (thread-sleep! 1)       ;; givbe thread th1 a chance to be done TODO: Verify this is needed. At 0.1 I was getting fail to stop, increased to total of 1.1 sec.
	    (mutex-lock! m)
	    (let* ((item-path (item-list->path itemdat))
		   ;; only state and status needed - use lazy routine
		   (testinfo  (rmt:get-testinfo-state-status run-id test-id)))
	      ;; Am I completed?
	      (if (member (db:test-state testinfo) '("REMOTEHOSTSTART" "RUNNING")) ;; NOTE: It should *not* be REMOTEHOSTSTART but for reasons I don't yet understand it sometimes gets stuck in that state ;; (not (equal? (db:test-state testinfo) "COMPLETED"))
		  (let ((new-state  (if kill-job? "KILLED" "COMPLETED") ;; (if (eq? (vector-ref exit-info 2) 0) ;; exited with "good" status
				                                        ;; "COMPLETED"
							                ;; (db:test-state testinfo)))   ;; else preseve the state as set within the test
				    )
			(new-status (cond
				     ((not (launch:einf-exit-status exit-info)) "FAIL") ;; job failed to run ... (vector-ref exit-info 1)
				     ((eq? (launch:einf-rollup-status exit-info) 0)     ;; (vector-ref exit-info 3)
				      ;; if the current status is AUTO then defer to the calculated value (i.e. leave this AUTO)
				      (if (equal? (db:test-status testinfo) "AUTO") "AUTO" "PASS"))
				     ((eq? (launch:einf-rollup-status exit-info) 1) "FAIL")  ;; (vector-ref exit-info 3)
				     ((eq? (launch:einf-rollup-status exit-info) 2)	     ;;	(vector-ref exit-info 3)
				      ;; if the current status is AUTO the defer to the calculated value but qualify (i.e. make this AUTO-WARN)
				      (if (equal? (db:test-status testinfo) "AUTO") "AUTO-WARN" "WARN"))
				     (else "FAIL")))) ;; (db:test-status testinfo)))
		    (debug:print-info 1 "Test exited in state=" (db:test-state testinfo) ", setting state/status based on exit code of " (launch:einf-exit-status exit-info) " and rollup-status of " (launch:einf-rollup-status exit-info))
		    (tests:test-set-status! run-id 
					    test-id 
					    new-state
					    new-status
					    (args:get-arg "-m") #f)
		    ;; need to update the top test record if PASS or FAIL and this is a subtest
		    ;; NO NEED TO CALL roll-up-pass-fail-counts HERE, THIS IS DONE IN roll-up-pass-fail-counts called by tests:test-set-status!
		    ))
	      ;; for automated creation of the rollup html file this is a good place...
	      (if (not (equal? item-path ""))
		  (tests:summarize-items run-id test-id test-name #f))
	      (tests:summarize-test run-id test-id)  ;; don't force - just update if no
	      )
	    (mutex-unlock! m)
	    (debug:print 2 "Output from running " fullrunscript ", pid " (launch:einf-pid exit-info) " in work area " 
			 work-area ":\n====\n exit code " (launch:einf-exit-code exit-info) "\n" "====\n")
	    (if (not (launch:einf-exit-status exit-info))
		(exit 4)))))))

;; set up the very basics needed for doing anything here.
(define (launch:setup-for-run #!key (force #f))
  ;; would set values for KEYS in the environment here for better support of env-override but 
  ;; have chicken/egg scenario. need to read megatest.config then read it again. Going to 
  ;; pass on that idea for now
  ;; special case
  (if (or force (not (hash-table? *configdat*)))  ;; no need to re-open on every call
      (begin
	(set! *configinfo* (or (if (get-environment-variable "MT_CMDINFO") ;; we are inside a test - do not reprocess configs
				   (let ((alistconfig (conc (get-environment-variable "MT_LINKTREE") "/"
							    (get-environment-variable "MT_TARGET")   "/"
							    (get-environment-variable "MT_RUNNAME")  "/"
							    ".megatest.cfg-"  megatest-version "-" megatest-fossil-hash)))
				     (if (file-exists? alistconfig)
					 (list (configf:read-alist alistconfig)
					       (get-environment-variable "MT_RUN_AREA_HOME"))
					 #f))
				   #f) ;; no config cached - give up
			       (let ((runname (or (args:get-arg "-runname")(args:get-arg ":runname"))))
				 (if runname (setenv "MT_RUNNAME" runname))
				 (find-and-read-config 
				  (if (args:get-arg "-config")(args:get-arg "-config") "megatest.config")
				  environ-patt: "env-override"
				  given-toppath: (get-environment-variable "MT_RUN_AREA_HOME")
				  pathenvvar: "MT_RUN_AREA_HOME"))))
	(set! *configdat*  (if (car *configinfo*)(car *configinfo*) #f))
	(set! *toppath*    (if (car *configinfo*)(cadr *configinfo*) #f))
	(let* ((tmptransport (configf:lookup *configdat* "server" "transport"))
	       (transport    (if tmptransport (string->symbol tmptransport) 'http)))
	  (if (member transport '(http rpc nmsg))
	      (set! *transport-type* transport)
	      (begin
		(debug:print 0 "ERROR: Unrecognised transport " transport)
		(exit))))
	(let ((linktree (configf:lookup *configdat* "setup" "linktree"))) ;; link tree is critical
	  (if linktree
	      (if (not (file-exists? linktree))
		  (begin
		    (handle-exceptions
		     exn
		     (begin
		       (debug:print 0 "ERROR: Something went wrong when trying to create linktree dir at " linktree)
		       (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
		       (exit 1))
		     (create-directory linktree #t))))
	      (begin
		(debug:print 0 "ERROR: linktree not defined in [setup] section of megatest.config")
		(exit 1)))
	  (if linktree
	      (let ((dbdir (conc linktree "/.db")))
		(handle-exceptions
		 exn
		 (begin
		   (debug:print 0 "ERROR: failed to create the " dbdir " area for your database files")
		   (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn)))
		 (if (not (directory-exists? dbdir))(create-directory dbdir)))
		(setenv "MT_LINKTREE" linktree))
	      (begin
		(debug:print 0 "ERROR: linktree is required in your megatest.config [setup] section")
		(exit 1)))
	  (if (and *toppath*
		   (directory-exists? *toppath*))
	      (setenv "MT_RUN_AREA_HOME" *toppath*)
	      (begin
		(debug:print 0 "ERROR: failed to find the top path to your Megatest area.")
		(exit 1)))
	  )))
  *toppath*)

(define (launch:cache-config)
  ;; if we have a linktree and -runtests and -target and the directory exists dump the config
  ;; to megatest-(current-seconds).cfg and symlink it to megatest.cfg
  (if (and *configdat* 
	   (or (args:get-arg "-run")
	       (args:get-arg "-runtests")))
      (let* ((linktree (get-environment-variable "MT_LINKTREE"))
	     (target   (common:args-get-target))
	     (runname  (or (args:get-arg "-runname")
			   (args:get-arg ":runname")))
	     (fulldir  (conc linktree "/"
			     target "/"
			     runname)))
	(debug:print-info 0 "Have -runtests with target=" target ", runname=" runname ", fulldir=" fulldir ", testpatt=" (or (args:get-arg "-testpatt") "%"))
	(if (file-exists? linktree) ;; can't proceed without linktree
	    (begin
	      (if (not (file-exists? fulldir))
		  (create-directory fulldir #t)) ;; need to protect with exception handler 
	      (if (and target
		       runname
		       (file-exists? fulldir))
		  (let ((tmpfile  (conc fulldir "/.megatest.cfg." (current-seconds)))
			(targfile (conc fulldir "/.megatest.cfg-"  megatest-version "-" megatest-fossil-hash)))
		    (debug:print-info 0 "Caching megatest.config in " fulldir "/.megatest.cfg")
		    (configf:write-alist *configdat* tmpfile)
		    (system (conc "ln -sf " tmpfile " " targfile))
		    )))))))

(define (get-best-disk confdat testconfig)
  (let* ((disks   (or (and testconfig (hash-table-ref/default testconfig "disks" #f))
		      (hash-table-ref/default confdat "disks" #f)))
	 (minspace (let ((m (configf:lookup confdat "setup" "minspace")))
		     (string->number (or m "10000")))))
    (if disks 
	(let ((res (common:get-disk-with-most-free-space disks minspace))) ;; min size of 1000, seems tad dumb
	  (if res
	      (cdr res)
	      (begin
		(if (common:low-noise-print 20 "no valid disks")
		    (debug:print 0 "ERROR: No valid disks found in megatest.config. Please add some to your [disks] section and ensure the directory exists!"))
		(exit 1)))))))

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
(define (create-work-area run-id run-info keyvals test-id test-src-path disk-path testname itemdat #!key (remtries 2))
  (let* ((item-path (if (string? itemdat) itemdat (item-list->path itemdat))) ;; if pass in string - just use it
	 (runname   (if (string? run-info) ;; if we pass in a string as run-info use it as run-name.
			run-info
			(db:get-value-by-header (db:get-rows run-info)
						(db:get-header run-info)
						"runname")))
	 ;; convert back to db: from rdb: - this is always run at server end
	 (target   (string-intersperse (map cadr keyvals) "/"))

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

	 (lnkbase   (conc linktree "/" target "/" runname))
	 (lnkpath   (conc lnkbase "/" testname))
	 (lnkpathf  (conc lnkpath (if not-iterated "" "/") item-path))
	 (lnktarget (conc lnkpath "/" item-path)))

    ;; Update the rundir path in the test record for all, rundir=physical, shortdir=logical
    ;;                                                 rundir   shortdir
    (rmt:general-call 'test-set-rundir-shortdir run-id lnkpathf test-path testname item-path)

    (debug:print 2 "INFO:\n       lnkbase=" lnkbase "\n       lnkpath=" lnkpath "\n  toptest-path=" toptest-path "\n     test-path=" test-path)
    (if (not (file-exists? linktree))
	(begin
	  (debug:print 0 "WARNING: linktree did not exist! Creating it now at " linktree)
	  (create-directory linktree #t))) ;; (system (conc "mkdir -p " linktree))))
    ;; create the directory for the tests dir links, this is needed no matter what...
    (if (and (not (directory-exists? lnkbase))
	     (not (file-exists? lnkbase)))
	(handle-exceptions
	 exn
	 (begin
	   (debug:print "ERROR: Problem creating linktree base at " lnkbase)
	   (print-error-message exn (current-error-port)))
	 (create-directory lnkbase #t)))
    
    ;; update the toptest record with its location rundir, cache the path
    ;; This wass highly inefficient, one db write for every subtest, potentially
    ;; thousands of unnecessary updates, cache the fact it was set and don't set it 
    ;; again. 

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
    
    ;; NB - This was not working right - some top tests are not getting the path set!!!
    ;;
    ;; Do the setting of this record after the paths are created so that the shortdir can 
    ;; be set to the real directory location. This is safer for future clean up if the link
    ;; tree is damaged or lost.
    ;; 
    (if (not (hash-table-ref/default *toptest-paths* testname #f))
	(let* ((testinfo       (rmt:get-test-info-by-id run-id test-id)) ;;  run-id testname item-path))
	       (curr-test-path (if testinfo ;; (filedb:get-path *fdb*
							     ;; (db:get-path dbstruct
				   ;; (rmt:sdb-qry 'getstr 
				   (db:test-rundir testinfo) ;; ) ;; )
				   #f)))
	  (hash-table-set! *toptest-paths* testname curr-test-path)
	  ;; NB// Was this for the test or for the parent in an iterated test?
	  (rmt:general-call 'test-set-rundir-shortdir run-id lnkpath 
			    (if (file-exists? lnkpath)
				(resolve-pathname lnkpath)
				lnkpath)
			    testname "")
	  ;; (rmt:general-call 'test-set-rundir run-id lnkpath testname "") ;; toptest-path)
	  (if (or (not curr-test-path)
		  (not (directory-exists? toptest-path)))
	      (begin
		(debug:print-info 2 "Creating " toptest-path " and link " lnkpath)
		(handle-exceptions
		 exn
		 #f ;; don't care to catch and deal with errors here for now.
		 (create-directory toptest-path #t))
		(hash-table-set! *toptest-paths* testname toptest-path)))))

    ;; The toptest path has been created, the link to the test in the linktree has
    ;; been created. Now, if this is an iterated test the real test dir must be created
    (if (not not-iterated) ;; this is an iterated test
	(begin ;; (let ((lnktarget (conc lnkpath "/" item-path)))
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
	     (debug:print 0 "ERROR:  Failed to re-create link " lnktarget ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit))
	   (if (symbolic-link? lnktarget)     (delete-file lnktarget))
	   (if (not (file-exists? lnktarget)) (create-symbolic-link test-path lnktarget)))))

    (if (not (directory? test-path))
	(create-directory test-path #t)) ;; this is a hack, I don't know why out of the blue this path does not exist sometimes

    (if (and test-src-path (directory? test-path))
	(begin
	  (let* ((ovrcmd (let ((cmd (config-lookup *configdat* "setup" "testcopycmd")))
			   (if cmd
			       ;; substitute the TEST_SRC_PATH and TEST_TARG_PATH
			       (string-substitute "TEST_TARG_PATH" test-path
						  (string-substitute "TEST_SRC_PATH" test-src-path cmd #t) #t)
			       #f)))
		 (cmd    (if ovrcmd 
			     ovrcmd
			     (conc "rsync -av" (if (debug:debug-mode 1) "" "q") " " test-src-path "/ " test-path "/"
				   " >> " test-path "/mt_launch.log 2>> " test-path "/mt_launch.log")))
		 (status (system cmd)))
	    (if (not (eq? status 0))
		(debug:print 2 "ERROR: problem with running \"" cmd "\"")))
	  (list lnkpathf lnkpath ))
	(if (and test-src-path (> remtries 0))
	    (begin
	      (debug:print 0 "ERROR: Failed to create work area at " test-path " with link at " lnktarget ", remaining attempts " remtries)
	      ;; 
	      (create-work-area run-id run-info keyvals test-id test-src-path disk-path testname itemdat remtries: (- remtries 1)))
	    (list #f #f)))))

;; 1. look though disks list for disk with most space
;; 2. create run dir on disk, path name is meaningful
;; 3. create link from run dir to megatest runs area 
;; 4. remotely run the test on allocated host
;;    - could be ssh to host from hosts table (update regularly with load)
;;    - could be netbatch
;;      (launch-test db (cadr status) test-conf))
(define (launch-test test-id run-id run-info keyvals runname test-conf test-name test-path itemdat params)
  (change-directory *toppath*)
  (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
   (list ;; (list "MT_TEST_RUN_DIR" work-area)
    (list "MT_RUN_AREA_HOME" *toppath*)
    (list "MT_TEST_NAME" test-name)
    ;; (list "MT_ITEM_INFO" (conc itemdat)) 
    (list "MT_RUNNAME"   runname)
    ;; (list "MT_TARGET"    mt_target)
    ))
  (let* ((tregistry       (tests:get-all))
	 (item-path       (let ((ip (item-list->path itemdat)))
			    (alist->env-vars (list (list "MT_ITEMPATH" ip)))
			    ip))
	 (tconfig         (or (tests:get-testconfig test-name tregistry #t force-create: #t)
			      test-conf)) ;; force re-read now that all vars are set
	 (useshell        (let ((ush (config-lookup *configdat* "jobtools"     "useshell")))
			    (if ush 
				(if (equal? ush "no") ;; must use "no" to NOT use shell
				    #f
				    ush)
				#t)))     ;; default is yes
	 (runscript       (config-lookup tconfig   "setup"        "runscript"))
	 (ezsteps         (> (length (hash-table-ref/default tconfig "ezsteps" '())) 0)) ;; don't send all the steps, could be big
	 (diskspace       (config-lookup tconfig   "requirements" "diskspace"))
	 (memory          (config-lookup tconfig   "requirements" "memory"))
	 (hosts           (config-lookup *configdat* "jobtools"     "workhosts"))
	 (remote-megatest (config-lookup *configdat* "setup" "executable"))
	 (run-time-limit  (or (configf:lookup  tconfig   "requirements" "runtimelim")
			      (configf:lookup  *configdat* "setup" "runtimelim")))
	 ;; FIXME SOMEDAY: not good how this is so obtuse, this hack is to 
	 ;;                allow running from dashboard. Extract the path
	 ;;                from the called megatest and convert dashboard
	 ;;             	  or dboard to megatest
	 (local-megatest  (let* ((lm  (car (argv)))
				 (dir (pathname-directory lm))
				 (exe (pathname-strip-directory lm)))
			    (conc (if dir (conc dir "/") "")
				  (case (string->symbol exe)
				    ((dboard)    "../megatest")
				    ((mtest)     "../megatest")
				    ((dashboard) "megatest")
				    (else exe)))))
	 (launcher        (common:get-launcher *configdat* test-name item-path)) ;; (config-lookup *configdat* "jobtools"     "launcher"))
	 (test-sig   (conc (common:get-testsuite-name) ":" test-name ":" item-path)) ;; (item-list->path itemdat))) ;; test-path is the full path including the item-path
	 (work-area  #f)
	 (toptest-work-area #f) ;; for iterated tests the top test contains data relevant for all
	 (diskpath   #f)
	 (cmdparms   #f)
	 (fullcmd    #f) ;; (define a (with-output-to-string (lambda ()(write x))))
	 (mt-bindir-path #f)
	 (testinfo   (rmt:get-test-info-by-id run-id test-id))
	 (mt_target  (string-intersperse (map cadr keyvals) "/"))
	 (debug-param (append (if (args:get-arg "-debug")  (list "-debug" (args:get-arg "-debug")) '())
			      (if (args:get-arg "-logging")(list "-logging") '()))))

    (setenv "MT_ITEMPATH" item-path)
    (if hosts (set! hosts (string-split hosts)))
    ;; set the megatest to be called on the remote host
    (if (not remote-megatest)(set! remote-megatest local-megatest)) ;; "megatest"))
    (set! mt-bindir-path (pathname-directory remote-megatest))
    (if launcher (set! launcher (string-split launcher)))
    ;; set up the run work area for this test
    (if (and (args:get-arg "-preclean") ;; user has requested to preclean for this run
	     (not (member (db:test-rundir testinfo)(list "n/a" "/tmp/badname")))) ;; n/a is a placeholder and thus not a read dir
	(begin
	  (debug:print-info 0 "attempting to preclean directory " (db:test-rundir testinfo) " for test " test-name "/" item-path)
	  (runs:remove-test-directory testinfo 'remove-data-only))) ;; remove data only, do not perturb the record

    ;; prevent overlapping actions - set to LAUNCHED as early as possible
    ;;
    (tests:test-set-status! run-id test-id "LAUNCHED" "n/a" #f #f) ;; (if launch-results launch-results "FAILED"))
    (rmt:roll-up-pass-fail-counts run-id test-name item-path #f "LAUNCHED")
    (set! diskpath (get-best-disk *configdat* tconfig))
    (if diskpath
	(let ((dat  (create-work-area run-id run-info keyvals test-id test-path diskpath test-name itemdat)))
	  (set! work-area (car dat))
	  (set! toptest-work-area (cadr dat))
	  (debug:print-info 2 "Using work area " work-area))
	(begin
	  (set! work-area (conc test-path "/tmp_run"))
	  (create-directory work-area #t)
	  (debug:print 0 "WARNING: No disk work area specified - running in the test directory under tmp_run")))
    (set! cmdparms (base64:base64-encode 
		    (z3:encode-buffer 
		     (with-output-to-string
		       (lambda () ;; (list 'hosts     hosts)
			 (write (list (list 'testpath  test-path)
				      (list 'transport (conc *transport-type*))
				      ;; (list 'serverinf *server-info*)
				      (list 'toppath   *toppath*)
				      (list 'work-area work-area)
				      (list 'test-name test-name) 
				      (list 'runscript runscript) 
				      (list 'run-id    run-id   )
				      (list 'test-id   test-id  )
				      ;; (list 'item-path item-path )
				      (list 'itemdat   itemdat  )
				      (list 'megatest  remote-megatest)
				      (list 'ezsteps   ezsteps) 
				      (list 'target    mt_target)
				      (list 'runtlim   (if run-time-limit (common:hms-string->seconds run-time-limit) #f))
				      (list 'env-ovrd  (hash-table-ref/default *configdat* "env-override" '())) 
				      (list 'set-vars  (if params (hash-table-ref/default params "-setvars" #f)))
				      (list 'runname   runname)
				      (list 'mt-bindir-path mt-bindir-path))))))))

    ;; clean out step records from previous run if they exist
    ;; (rmt:delete-test-step-records run-id test-id)
    ;; if the dir does not exist we may have a itempath where individual variables are a path, launch anyway
    (if (file-exists? work-area)
	(change-directory work-area)) ;; so that log files from the launch process don't clutter the test dir
    (cond
     ((and launcher hosts) ;; must be using ssh hostname
      (set! fullcmd (append launcher (car hosts)(list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param)))
     ;; (set! fullcmd (append launcher (car hosts)(list remote-megatest test-sig "-execute" cmdparms))))
     (launcher
      (set! fullcmd (append launcher (list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param)))
     ;; (set! fullcmd (append launcher (list remote-megatest test-sig "-execute" cmdparms))))
     (else
      (if (not useshell)(debug:print 0 "WARNING: internal launching will not work well without \"useshell yes\" in your [jobtools] section"))
      (set! fullcmd (append (list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param (list (if useshell "&" ""))))))
    ;; (set! fullcmd (list remote-megatest test-sig "-execute" cmdparms (if useshell "&" "")))))
    (if (args:get-arg "-xterm")(set! fullcmd (append fullcmd (list "-xterm"))))
    (debug:print 1 "Launching " work-area)
    ;; set pre-launch-env-vars before launching, keep the vars in prevvals and put the envionment back when done
    (debug:print 4 "fullcmd: " fullcmd)
    (let* ((commonprevvals (alist->env-vars
			    (hash-table-ref/default *configdat* "env-override" '())))
	   (testprevvals   (alist->env-vars
			    (hash-table-ref/default tconfig "pre-launch-env-overrides" '())))
	   (miscprevvals   (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
			    (append (list (list "MT_TEST_RUN_DIR" work-area)
					  (list "MT_TEST_NAME" test-name)
					  (list "MT_ITEM_INFO" (conc itemdat)) 
					  (list "MT_RUNNAME"   runname)
					  (list "MT_TARGET"    mt_target)
					  (list "MT_ITEMPATH"  item-path)
					  )
				    itemdat)))
	   ;; Launchwait defaults to true, must override it to turn off wait
	   (launchwait     (if (equal? (configf:lookup *configdat* "setup" "launchwait") "no") #f #t))
	   (launch-results (apply (if launchwait
				      cmd-run-with-stderr->list
				      process-run)
				  (if useshell
				      (let ((cmdstr (string-intersperse fullcmd " ")))
					(if launchwait
					    cmdstr
					    (conc cmdstr " >> mt_launch.log 2>&1")))
				      (car fullcmd))
				  (if useshell
				      '()
				      (cdr fullcmd)))))
      (if (not launchwait) ;; give the OS a little time to allow the process to start
	  (thread-sleep! 0.01))
      (with-output-to-file "mt_launch.log"
	(lambda ()
	  (print "LAUNCHCMD: " (string-intersperse fullcmd " "))
	  (if (list? launch-results)
	      (apply print launch-results)
	      (print "NOTE: launched \"" fullcmd "\"\n  but did not wait for it to proceed. Add the following to megatest.config \n[setup]\nlaunchwait yes\n  if you have problems with this"))
	  #:append))
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

;; recover a test where the top controlling mtest may have died
;;
(define (launch:recover-test run-id test-id)
  ;; this function is called on the test run host via ssh
  ;;
  ;; 1. look at the process from pid
  ;;    - is it owned by calling user
  ;;    - it it's run directory correct for the test
  ;;    - is there a controlling mtest (maybe stuck)
  ;; 2. if recovery is needed watch pid
  ;;    - when it exits take the exit code and do the needful
  ;;
  (let* ((pid (rmt:test-get-top-process-id run-id test-id))
	 (psres (with-input-from-pipe
		 (conc "ps -F -u " (current-user-name) " | grep -E '" pid " ' | grep -v 'grep -E " pid "'")
		 (lambda ()
		   (read-line))))
	 (rundir (if (string? psres) ;; real process owned by user
		     (read-symbolic-link (conc "/proc/" pid "/cwd"))
		     #f)))
    ;; now wait on that process if all is correct
    ;; periodically update the db with runtime
    ;; when the process exits look at the db, if still RUNNING after 10 seconds set
    ;; state/status appropriately
    (process-wait pid)))
