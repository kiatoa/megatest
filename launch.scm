
;; Copyright 2006-2017, Matthew Welland.
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

(use regex regex-case base64 sqlite3 srfi-18 directory-utils posix-extras z3 call-with-environment-variables csv)
(use typed-records pathname-expand matchable)

(import (prefix base64 base64:))
(import (prefix sqlite3 sqlite3:))

(declare (unit launch))
(declare (uses subrun))
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
    (if enccmd
	(common:read-encoded-string enccmd)
	'())))

;;                       0           1              2              3
(defstruct launch:einf (pid #t)(exit-status #t)(exit-code #t)(rollup-status 0))

;; return (conc status ": " comment) from the final section so that
;;   the comment can be set in the step record in launch.scm
;;
(define (launch:load-logpro-dat run-id test-id stepname)
  (let ((cname (conc stepname ".dat")))
    (if (common:file-exists? cname)
	(let* ((dat  (read-config cname #f #f))
	       (csvr (db:logpro-dat->csv dat stepname))
	       (csvt (let-values (((fmt-cell fmt-record fmt-csv) (make-format ",")))
		       (fmt-csv (map list->csv-record csvr))))
	       (status (configf:lookup dat "final" "exit-status"))
	       (msg     (configf:lookup dat "final" "message")))
          (if csvt  ;; this if blocked stack dump caused by .dat file from logpro being 0-byte.  fixed by upgrading logpro
              (rmt:csv->test-data run-id test-id csvt)
	      (debug:print 0 *default-log-port* "ERROR: no csvdat exists for run-id: " run-id " test-id: " test-id " stepname: " stepname ", check that logpro version is 1.15 or newer"))
	  ;;  (debug:print-info 13 *default-log-port* "Error: run-id/test-id/stepname="run-id"/"test-id"/"stepname" => bad csvr="csvr)
	  ;;  )
	  (cond
	   ((equal? status "PASS") "PASS") ;; skip the message part if status is pass
	   (status (conc (configf:lookup dat "final" "exit-status") ": " (if msg msg "no message")))
	   (else #f)))
	#f)))

(define (launch:runstep ezstep run-id test-id exit-info m tal testconfig)
  (let* ((stepname       (car ezstep))  ;; do stuff to run the step
	 (stepinfo       (cadr ezstep))
	;; (let ((info (cadr ezstep)))
	;; 		   (if (proc? info) "" info)))
	;; (stepproc       (let ((info (cadr ezstep)))
	;; 		   (if (proc? info) info #f)))
	 (stepparts      (string-match (regexp "^(\\{([^\\}\\{]*)\\}\\s*|)(.*)$") stepinfo))
	 (stepparams     (list-ref stepparts 2)) ;; for future use, {VAR=1,2,3}, run step for each
	 (paramparts     (if (string? stepparams)
			     (map (lambda (x)(string-split x "=")) (string-split-fields "[^;]*=[^;]*" stepparams))
			     '()))
	 (subrun         (alist-ref "subrun" paramparts equal?))
	 (stepcmd        (list-ref stepparts 3))
	 (script         "") ; "#!/bin/bash\n") ;; yep, we depend on bin/bash FIXME!!!\
	 (logpro-file    (conc stepname ".logpro"))
	 (html-file      (conc stepname ".html"))
	 (dat-file       (conc stepname ".dat"))
	 (tconfig-logpro (configf:lookup testconfig "logpro" stepname))
	 (logpro-used    (common:file-exists? logpro-file)))

    (debug:print 0 *default-log-port* "stepparts: " stepparts ", stepparams: " stepparams
                 ", paramparts: " paramparts ", subrun: " subrun ", stepcmd: " stepcmd)
    
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
    (debug:print 4 *default-log-port* "ezsteps:\n stepname: " stepname " stepinfo: " stepinfo " stepparts: " stepparts
		 " stepparams: " stepparams " stepcmd: " stepcmd)
    
    ;; ;; first source the previous environment
    ;; (let ((prev-env (conc ".ezsteps/" prevstep (if (string-search (regexp "csh") 
    ;;      							 (get-environment-variable "SHELL")) ".csh" ".sh"))))
    ;;   (if (and prevstep (common:file-exists? prev-env))
    ;;       (set! script (conc script "source " prev-env))))
    
    ;; call the command using mt_ezstep
    ;; (set! script (conc "mt_ezstep " stepname " " (if prevstep prevstep "x") " " stepcmd))
    
    (debug:print 4 *default-log-port* "script: " script)
    (rmt:teststep-set-status! run-id test-id stepname "start" "-" #f #f)
    ;; now launch the actual process
    (call-with-environment-variables 
     (list (cons "PATH" (conc (get-environment-variable "PATH") ":.")))
     (lambda () ;; (process-run "/bin/bash" "-c" "exec ls -l /tmp/foobar > /tmp/delme-more.log 2>&1")
       (let* ((cmd (conc stepcmd " > " stepname ".log 2>&1")) ;; >outfile 2>&1 
	      (pid #f))
	 (let ((proc (lambda ()
		       (set! pid (process-run "/bin/bash" (list "-c" cmd))))))
	   (if subrun
               (begin
                 (debug:print-info 0 *default-log-port* "Running without MT_.* environment variables.")
                 (common:without-vars proc "^MT_.*"))
	       (proc)))
	 
         (with-output-to-file "Makefile.ezsteps"
           (lambda ()
             (print stepname ".log :")
             (print "\t" cmd)
             (if (common:file-exists? (conc stepname ".logpro"))
                 (print "\tlogpro " stepname ".logpro " stepname ".html < " stepname ".log"))
             (print)
             (print stepname " : " stepname ".log")
             (print))
           #:append)

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
    (debug:print-info 0 *default-log-port* "step " stepname " completed with exit code " (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
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
	    (debug:print-info 0 *default-log-port* "logpro for step " stepname " exited with code " (launch:einf-exit-code exit-info))))) ;; (vector-ref exit-info 2)))))
    
    (let ((exinfo (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
	  (logfna (if logpro-used (conc stepname ".html") ""))
	  (comment #f))
      (if logpro-used
	  (let ((datfile (conc stepname ".dat")))
	    ;; load the .dat file into the test_data table if it exists
	    (if (common:file-exists? datfile)
		(set! comment (launch:load-logpro-dat run-id test-id stepname)))
	    (rmt:test-set-log! run-id test-id (conc stepname ".html"))))
      (rmt:teststep-set-status! run-id test-id stepname "end" exinfo comment logfna))
    ;; set the test final status
    (let* ((process-exit-status (launch:einf-exit-code exit-info)) ;; (vector-ref exit-info 2))
	   (this-step-status (cond
			      ((and (eq? process-exit-status 2) logpro-used) 'warn)   ;; logpro 2 = warnings
			      ((and (eq? process-exit-status 3) logpro-used) 'check)  ;; logpro 3 = check
			      ((and (eq? process-exit-status 4) logpro-used) 'waived) ;; logpro 4 = waived
			      ((and (eq? process-exit-status 5) logpro-used) 'abort)  ;; logpro 5 = abort
			      ((and (eq? process-exit-status 6) logpro-used) 'skip)   ;; logpro 6 = skip
			      ((eq? process-exit-status 0)                   'pass)   ;; logpro 0 = pass
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
	     (else "RUNNING"))))
      (debug:print 4 *default-log-port* "Exit value received: " (launch:einf-exit-code exit-info) " logpro-used: " logpro-used 
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
	((waived)
	 (launch:einf-rollup-status-set! exit-info 4) ;; (vector-set! exit-info 3 3) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "WAIVED" 
				 (if (eq? this-step-status 'check) "Logpro waived found" #f)
				 #f))
	((abort)
	 (launch:einf-rollup-status-set! exit-info 5) ;; (vector-set! exit-info 3 4) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "ABORT" 
				 (if (eq? this-step-status 'abort) "Logpro abort found" #f)
				 #f))
	((skip)
	 (launch:einf-rollup-status-set! exit-info 6) ;; (vector-set! exit-info 3 4) ;; rollup-status
	 ;; NB// test-set-status! does rdb calls under the hood
	 (tests:test-set-status! run-id test-id next-state "SKIP" 
				 (if (eq? this-step-status 'skip) "Logpro skip found" #f)
				 #f))
	((pass)
	 (tests:test-set-status! run-id test-id next-state "PASS" #f #f))
	(else ;; 'fail
	 (launch:einf-rollup-status-set! exit-info 1) ;; (vector-set! exit-info 3 1) ;; force fail, this used to be next-state but that doesn't make sense. should always be "COMPLETED" 
	 (tests:test-set-status! run-id test-id "COMPLETED" "FAIL" (conc "Failed at step " stepname) #f)
	 )))
    logpro-used))

(define (launch:manage-steps run-id test-id item-path fullrunscript ezsteps subrun test-name tconfigreg exit-info m)
  ;; (let-values
  ;;  (((pid exit-status exit-code)
  ;;    (run-n-wait fullrunscript)))
  ;; (tests:test-set-status! test-id "RUNNING" "n/a" #f #f)
  ;; Since we should have a clean slate at this time there is no need to do 
  ;; any of the other stuff that tests:test-set-status! does. Let's just 
  ;; force RUNNING/n/a

  ;; (thread-sleep! 0.3)
  ;; (tests:test-force-state-status! run-id test-id "RUNNING" "n/a")
  (rmt:set-state-status-and-roll-up-items run-id test-name item-path "RUNNING" #f #f) 
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
  (if (or ezsteps subrun)
      (let* ((test-run-dir (tests:get-test-path-from-environment))
             (testconfig ;; (read-config (conc work-area "/testconfig") #f #t environ-patt: "pre-launch-env-vars")) ;; FIXME??? is allow-system ok here?
	      ;; NOTE: it is tempting to turn off force-create of testconfig but dynamic
	      ;;       ezstep names need a full re-eval here.
	      (tests:get-testconfig test-name item-path tconfigreg #t force-create: #t)) ;; 'return-procs)))
	     (ezstepslst (if (hash-table? testconfig)
			     (hash-table-ref/default testconfig "ezsteps" '())
			     #f)))
	(if testconfig
	    (hash-table-set! *testconfigs* test-name testconfig) ;; cached for lazy reads later ...
	    (begin
	      (launch:setup)
	      (debug:print 0 *default-log-port* "WARNING: no testconfig found for " test-name " in search path:\n  "
			   (string-intersperse (tests:get-tests-search-path *configdat*) "\n  "))))
	;; after all that, still no testconfig? Time to abort
	(if (not testconfig)
	    (begin
	      (debug:print-error 0 *default-log-port* "Failed to resolve megatest.config, runconfigs.config and testconfig issues. Giving up now")
	      (exit 1)))

	;; create a proc for the subrun if requested, save that proc in the ezsteps table as the last entry
	;; 1. get section [runarun]
	;; 2. unset MT_* vars
	;; 3. fix target
	;; 4. fix runname
	;; 5. fix testpatt or calculate it from contour
	;; 6. launch the run
	;; 7. roll up the run result and or roll up the logpro processed result
	(when (configf:lookup testconfig "subrun" "runwait") ;; we use runwait as the flag that a subrun is requested
            (subrun:initialize-toprun-test testconfig test-run-dir)
	    (let* ((mt-cmd (subrun:launch-cmd test-run-dir)))
              (debug:print-info 0 *default-log-port* "Subrun command is \"" mt-cmd "\"")
              (set! ezsteps #t) ;; set the needed flag
	      (set! ezstepslst
                    (append (or ezstepslst '())
                            (list (list "subrun" (conc "{subrun=true} " mt-cmd)))))))

	;; process the ezsteps
	(if ezsteps
	    (begin
	      (if (not (common:file-exists? ".ezsteps"))(create-directory ".ezsteps"))
	      ;; if ezsteps was defined then we are sure to have at least one step but check anyway
	      (if (not (> (length ezstepslst) 0))
		  (debug:print-error 0 *default-log-port* "ezsteps defined but ezstepslst is zero length")
		  (let loop ((ezstep (car ezstepslst))
			     (tal    (cdr ezstepslst))
			     (prevstep #f))
                    (debug:print-info 0 *default-log-port* "Processing ezstep \"" (string-intersperse ezstep " ") "\"")
		    ;; check exit-info (vector-ref exit-info 1)
		    (if (launch:einf-exit-status exit-info) ;; (vector-ref exit-info 1)
			(let ((logpro-used (launch:runstep ezstep run-id test-id exit-info m tal testconfig))
			      (stepname    (car ezstep)))
			  ;; if logpro-used read in the stepname.dat file
			  (if (and logpro-used (common:file-exists? (conc stepname ".dat")))
			      (launch:load-logpro-dat run-id test-id stepname))
			  (if (steprun-good? logpro-used (launch:einf-exit-code exit-info))
			      (if (not (null? tal))
				  (loop (car tal) (cdr tal) stepname))
			      (debug:print 0 *default-log-port* "WARNING: step " (car ezstep) " failed. Stopping")))
			(debug:print 0 *default-log-port* "WARNING: a prior step failed, stopping at " ezstep)))))))))

(define (launch:monitor-job run-id test-id item-path fullrunscript ezsteps test-name tconfigreg exit-info m work-area runtlim misc-flags)
  (let* ((update-period (string->number (or (configf:lookup *configdat* "setup" "test-stats-update-period") "30")))
         (start-seconds (current-seconds))
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
	       (cpu-load  (alist-ref 'adj-core-load (common:get-normalized-cpu-load #f)))
	       (disk-free (get-df (current-directory)))
               (last-sync (current-seconds)))
      (let* ((over-time     (> (current-seconds) (+ last-sync update-period)))
             (new-cpu-load  (let* ((load  (alist-ref 'adj-core-load (common:get-normalized-cpu-load #f)))
                                   (delta (abs (- load cpu-load))))
                              (if (> delta 0.1) ;; don't bother updating with small changes
                                  load
                                  #f)))
             (new-disk-free (let* ((df    (if over-time ;; only get df every 30 seconds
                                              (get-df (current-directory))
                                              disk-free))
                                   (delta (abs (- df disk-free))))
                              (if (and (> df 0)
                                       (> (/ delta df) 0.1)) ;; (> delta 200) ;; ignore changes under 200 Meg
                                  df
                                  #f)))
             (do-sync       (or new-cpu-load new-disk-free over-time)))
        (debug:print 4 *default-log-port* "cpu: " new-cpu-load " disk: " new-disk-free " last-sync: " last-sync " do-sync: " do-sync)
	(set! kill-job? (or (test-get-kill-request run-id test-id) ;; run-id test-name itemdat))
			    (and runtlim (let* ((run-seconds   (- (current-seconds) start-seconds))
						(time-exceeded (> run-seconds runtlim)))
					   (if time-exceeded
					       (begin
						 (debug:print-info 0 *default-log-port* "KILLING TEST DUE TO TIME LIMIT EXCEEDED! Runtime=" run-seconds " seconds, limit=" runtlim)
						 #t)
					       #f)))))
        (if do-sync
            (tests:update-central-meta-info run-id test-id new-cpu-load new-disk-free (calc-minutes) #f #f))
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
			    (debug:print-info 0 *default-log-port* "Unable to kill process with pid " pid ", possibly already killed.")
			    (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn)))
			  (debug:print 0 *default-log-port* "WARNING: Request received to kill job " pid) ;;  " (attempt # " kill-tries ")")
			  (debug:print-info 0 *default-log-port* "Signal mask=" (signal-mask))
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
		       ;;    (debug:print-info 0 *default-log-port* "not killing process " pid " as it is not alive"))))
		       pids)
		      (tests:test-set-status! run-id test-id "KILLED"  "KILLED" (args:get-arg "-m") #f))
		    (begin
		      (debug:print-error 0 *default-log-port* "Nothing to kill, pid1=" pid1 ", pid2=" pid2)
		      (tests:test-set-status! run-id test-id "KILLED"  "FAILED TO KILL" (args:get-arg "-m") #f)
		      )))
	      (mutex-unlock! m)
	      ;; no point in sticking around. Exit now.
	      (exit)))
	(if (hash-table-ref/default misc-flags 'keep-going #f)
	    (begin
	      (thread-sleep! 3) ;; (+ 3 (random 6))) ;; add some jitter to the call home time to spread out the db accesses
	      (if (hash-table-ref/default misc-flags 'keep-going #f)  ;; keep originals for cpu-load and disk-free unless they change more than the allowed delta
		  (loop (calc-minutes)
                        (or new-cpu-load cpu-load)
                        (or new-disk-free disk-free)
                        (if do-sync (current-seconds) last-sync)))))))
    (tests:update-central-meta-info run-id test-id (get-cpu-load) (get-df (current-directory))(calc-minutes) #f #f))) ;; NOTE: Checking twice for keep-going is intentional


(define (launch:execute encoded-cmd)
  (let* ((cmdinfo    (common:read-encoded-string encoded-cmd))
	 (tconfigreg #f))
    (setenv "MT_CMDINFO" encoded-cmd)
    ;;(bb-check-path msg: "launch:execute incoming")
    (if (list? cmdinfo) ;; ((testpath /tmp/mrwellan/jazzmind/src/example_run/tests/sqlitespeed)
	;; (test-name sqlitespeed) (runscript runscript.rb) (db-host localhost) (run-id 1))
	(let* ((testpath  (assoc/default 'testpath  cmdinfo))  ;; testpath is the test spec area
	       (top-path  (assoc/default 'toppath   cmdinfo))
	       (work-area (assoc/default 'work-area cmdinfo))  ;; work-area is the test run area
	       (test-name (assoc/default 'test-name cmdinfo))
	       (runscript (assoc/default 'runscript cmdinfo))
	       (ezsteps   (assoc/default 'ezsteps   cmdinfo))
	       (subrun    (assoc/default 'subrun    cmdinfo))
	       ;; (runremote (assoc/default 'runremote cmdinfo))
	       ;; (transport (assoc/default 'transport cmdinfo))  ;; not used
	       ;; (serverinf (assoc/default 'serverinf cmdinfo))
	       ;; (port      (assoc/default 'port      cmdinfo))
	       (serverurl (assoc/default 'serverurl cmdinfo))
	       (homehost  (assoc/default 'homehost  cmdinfo))
	       (run-id    (assoc/default 'run-id    cmdinfo))
	       (test-id   (assoc/default 'test-id   cmdinfo))
	       (target    (assoc/default 'target    cmdinfo))
	       (areaname  (assoc/default 'areaname  cmdinfo))
	       (itemdat   (assoc/default 'itemdat   cmdinfo))
	       (env-ovrd  (assoc/default 'env-ovrd  cmdinfo))
	       (set-vars  (assoc/default 'set-vars  cmdinfo)) ;; pre-overrides from -setvar
	       (runname   (assoc/default 'runname   cmdinfo))
	       (megatest  (assoc/default 'megatest  cmdinfo))
	       (runtlim   (assoc/default 'runtlim   cmdinfo))
	       (contour   (assoc/default 'contour   cmdinfo))
	       (item-path (item-list->path itemdat))
	       (mt-bindir-path (assoc/default 'mt-bindir-path cmdinfo))
	       (keys      #f)
	       (keyvals   #f)
	       (fullrunscript (if (not runscript)
                                  #f
                                  (if (substring-index "/" runscript)
                                      runscript ;; use unadultered if contains slashes
                                      (let ((fulln (conc work-area "/" runscript)))
	                                  (if (and (common:file-exists? fulln)
                                                   (file-execute-access? fulln))
                                              fulln
                                              runscript))))) ;; assume it is on the path
               (check-work-area           (lambda ()
                                            ;; NFS might not have propagated the directory meta data to the run host - give it time if needed
                                            (let loop ((count 0))
                                              (if (or (common:directory-exists? work-area)
                                                      (> count 10))
                                                  (change-directory work-area)
                                                  (begin
                                                    (debug:print 0 *default-log-port* "INFO: Not starting job yet - directory " work-area " not found")
                                                    (thread-sleep! 10)
                                                    (loop (+ count 1)))))

                                            (if (not (string=?  (common:real-path work-area)(common:real-path (current-directory))))
                                                (begin
                                                  (debug:print 0 *default-log-port*
                                                               "INFO: we are expecting to be in directory " work-area "\n"
                                                               "     but we are actually in the directory " (current-directory) "\n"
                                                               "     doing another change dir.")
                                                  (change-directory work-area)))
                                            
                                            ;; spot check that the files in testpath are available. Too often NFS delays cause problems here.
                                            (let ((files      (glob (conc testpath "/*")))
                                                  (bad-files '()))
                                              (for-each
                                               (lambda (fullname)
                                                 (let* ((fname (pathname-strip-directory fullname))
                                                        (targn (conc work-area "/" fname)))
                                                   (if (not (file-exists? targn))
                                                       (set! bad-files (cons fname bad-files)))))
                                               files)
                                              (if (not (null? bad-files))
                                                  (begin
                                                    (debug:print 0 *default-log-port* "INFO: test data from " testpath " not copied properly or filesystem problems causing data to not be found. Re-running the copy command.")
                                                    (debug:print 0 *default-log-port* "INFO: missing files from " work-area ": " (string-intersperse bad-files ", "))
                                                    (launch:test-copy testpath work-area))))
                                            ;; one more time, change to the work-area directory
                                            (change-directory work-area)))
	       ) ;; let*

	  (if contour (setenv "MT_CONTOUR" contour))
	  
	  ;; immediated set some key variables from CMDINFO data, yes, these will be set again below ...
	  ;;
	  (setenv "MT_TESTSUITENAME" areaname)
	  (setenv "MT_RUN_AREA_HOME" top-path)
	  (set! *toppath* top-path)
          (change-directory *toppath*) ;; temporarily switch to the run area home
	  (setenv "MT_TEST_RUN_DIR"  work-area)

	  (launch:setup) ;; should be properly in the run area home now

	  (if contour (setenv "MT_CONTOUR" contour))
	  
	  ;; immediated set some key variables from CMDINFO data, yes, these will be set again below ...
	  ;;
	  (setenv "MT_TESTSUITENAME" areaname)
	  (setenv "MT_RUN_AREA_HOME" top-path)
	  (set! *toppath* top-path)
          (change-directory *toppath*) ;; temporarily switch to the run area home
	  (setenv "MT_TEST_RUN_DIR"  work-area)

	  (launch:setup) ;; should be properly in the run area home now
          
	  (set! tconfigreg (tests:get-all)) ;; mapping of testname => test source path
	  (let ((sighand (lambda (signum)
			   ;; (signal-mask! signum) ;; to mask or not? seems to cause issues in exiting
			   (if (eq? signum signal/stop)
			       (debug:print-error 0 *default-log-port* "attempt to STOP process. Exiting."))
			   (set! *time-to-exit* #t)
			   (print "Received signal " signum ", cleaning up before exit. Please wait...")
			   (let ((th1 (make-thread (lambda ()
						     (rmt:test-set-state-status run-id test-id "INCOMPLETE" "KILLED" #f)
						     (print "Killed by signal " signum ". Exiting")
						     (thread-sleep! 1)
						     (exit 1))))
				 (th2 (make-thread (lambda ()
						     (thread-sleep! 2)
						     (debug:print 0 *default-log-port* "Done")
						     (exit 4)))))
			     (thread-start! th2)
			     (thread-start! th1)
			     (thread-join! th2)))))
	    (set-signal-handler! signal/int sighand)
	    (set-signal-handler! signal/term sighand)
	    ) ;; (set-signal-handler! signal/stop sighand)
	  
	  ;; Do not run the test if it is REMOVING, RUNNING, KILLREQ or REMOTEHOSTSTART,
	  ;; Mark the test as REMOTEHOSTSTART *IMMEDIATELY*
	  ;;
	  (let* ((test-info (rmt:get-test-info-by-id run-id test-id))
		 (test-host (if test-info
				(db:test-get-host        test-info)
				(begin
				  (debug:print 0 *default-log-port* "ERROR: failed to find a record for test-id " test-id ", exiting.")
				  (exit))))
		 (test-pid  (db:test-get-process_id  test-info)))
	    (cond
             ;; -mrw- I'm removing KILLREQ from this list so that a test in KILLREQ state is treated as a "do not run" flag.
	     ((member (db:test-get-state test-info) '("INCOMPLETE" "KILLED" "UNKNOWN" "STUCK")) ;; prior run of this test didn't complete, go ahead and try to rerun
	      (debug:print 0 *default-log-port* "INFO: test is INCOMPLETE or KILLED, treat this execute call as a rerun request")
	      ;; (tests:test-force-state-status! run-id test-id "REMOTEHOSTSTART" "n/a")
	      (rmt:test-set-state-status run-id test-id "REMOTEHOSTSTART" "n/a" #f)
	      ) ;; prime it for running
	     ((member (db:test-get-state test-info) '("RUNNING" "REMOTEHOSTSTART"))
	      (if (process:alive-on-host? test-host test-pid)
		  (debug:print-error 0 *default-log-port* "test state is "  (db:test-get-state test-info) " and process " test-pid " is still running on host " test-host ", cannot proceed")
		  ;; (tests:test-force-state-status! run-id test-id "REMOTEHOSTSTART" "n/a")
		  (rmt:test-set-state-status run-id test-id "REMOTEHOSTSTART" "n/a" #f)
		  ))
	     ((not (member (db:test-get-state test-info) '("REMOVING" "REMOTEHOSTSTART" "RUNNING" "KILLREQ")))
	      ;; (tests:test-force-state-status! run-id test-id "REMOTEHOSTSTART" "n/a")
	      (rmt:test-set-state-status run-id test-id "REMOTEHOSTSTART" "n/a" #f)
	      )
	     (else ;; (member (db:test-get-state test-info) '("REMOVING" "REMOTEHOSTSTART" "RUNNING" "KILLREQ"))
	      (debug:print-error 0 *default-log-port* "test state is " (db:test-get-state test-info) ", cannot proceed")
	      (exit))))
	  
	  (debug:print 2 *default-log-port* "Exectuing " test-name " (id: " test-id ") on " (get-host-name))
	  (set! keys       (rmt:get-keys))
	  ;; (runs:set-megatest-env-vars run-id inkeys: keys inkeyvals: keyvals) ;; these may be needed by the launching process
	  ;; one of these is defunct/redundant ...
	  (if (not (launch:setup force-reread: #t))
	      (begin
		(debug:print 0 *default-log-port* "Failed to setup, exiting") 
		;; (sqlite3:finalize! db)
		;; (sqlite3:finalize! tdb)
		(exit 1)))
          ;; validate that the test run area is available
          (check-work-area)
          
          ;; still need to go back to run area home for next couple steps
	  (change-directory *toppath*) 

	  ;; NOTE: Current order is to process runconfigs *before* setting the MT_ vars. This 
	  ;;       seems non-ideal but could well break stuff
	  ;;    BUG? BUG? BUG?
	  
	  (let ((rconfig (full-runconfigs-read)) ;; (read-config (conc  *toppath* "/runconfigs.config") #f #t sections: (list "default" target))))
		(wconfig (read-config "waivers.config" #f #t sections: `( "default" ,target )))) ;; read the waivers config if it exists
	    ;; (setup-env-defaults (conc *toppath* "/runconfigs.config") run-id (make-hash-table) keyvals target)
	    ;; (set-run-config-vars run-id keyvals target) ;; (db:get-target db run-id))
	    ;; Now have runconfigs data loaded, set environment vars
	    (for-each
	     (lambda (section)
	       (for-each
		(lambda (varval)
		  (let ((var (car varval))
			(val (cadr varval)))
		    (if (and (string? var)(string? val))
			(begin
			  (setenv var (config:eval-string-in-environment val))) ;; val)
			(debug:print-error 0 *default-log-port* "bad variable spec, " var "=" val))))
		(configf:get-section rconfig section)))
	     (list "default" target)))
          ;;(bb-check-path msg: "launch:execute post block 1")

	  ;; NFS might not have propagated the directory meta data to the run host - give it time if needed
	  (let loop ((count 0))
	    (if (or (common:file-exists? work-area)
		    (> count 10))
		(change-directory work-area)
		(begin
		  (debug:print 0 *default-log-port* "INFO: Not starting job yet - directory " work-area " not found")
		  (thread-sleep! 10)
		  (loop (+ count 1)))))

          ;; now we can switch to the work-area?
          (change-directory work-area)
          ;;(bb-check-path msg: "launch:execute post block 1.5")
	  ;; (change-directory work-area) 
	  (set! keyvals    (keys:target->keyval keys target))
	  ;; apply pre-overrides before other variables. The pre-override vars must not
	  ;; clobbers things from the official sources such as megatest.config and runconfigs.config
	  (if (string? set-vars)
	      (let ((varpairs (string-split set-vars ",")))
		(debug:print 4 *default-log-port* "varpairs: " varpairs)
		(map (lambda (varpair)
		       (let ((varval (string-split varpair "=")))
			 (if (eq? (length varval) 2)
			     (let ((var (car varval))
				   (val (cadr varval)))
			       (debug:print 1 *default-log-port* "Adding pre-var/val " var " = " val " to the environment")
			       (setenv var val)))))
		     varpairs)))
          ;;(bb-check-path msg: "launch:execute post block 2")
	  (for-each
	   (lambda (varval)
	     (let ((var (car varval))
		   (val (cadr varval)))
	       (if val
		   (setenv var val)
		   (begin
		     (debug:print-error 0 *default-log-port* "required variable " var " does not have a valid value. Exiting")
		     (exit)))))
	     (list 
	      (list  "MT_TEST_RUN_DIR" work-area)
	      (list  "MT_TEST_NAME" test-name)
	      (list  "MT_ITEM_INFO" (conc itemdat))
	      (list  "MT_ITEMPATH"  item-path)
	      (list  "MT_RUNNAME"   runname)
	      (list  "MT_MEGATEST"  megatest)
	      (list  "MT_TARGET"    target)
	      (list  "MT_LINKTREE"  (common:get-linktree)) ;; (configf:lookup *configdat* "setup" "linktree"))
	      (list  "MT_TESTSUITENAME" (common:get-testsuite-name))))
          ;;(bb-check-path msg: "launch:execute post block 3")

	  (if mt-bindir-path (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path)))
          ;;(bb-check-path msg: "launch:execute post block 4")
	  ;; (change-directory top-path)
	  ;; Can setup as client for server mode now
	  ;; (client:setup)

	  
	  ;; environment overrides are done *before* the remaining critical envars.
	  (alist->env-vars env-ovrd)
          ;;(bb-check-path msg: "launch:execute post block 41")
	  (runs:set-megatest-env-vars run-id inkeys: keys inkeyvals: keyvals)
          ;;(bb-check-path msg: "launch:execute post block 42")
	  (set-item-env-vars itemdat)
          ;;(bb-check-path msg: "launch:execute post block 43")
          (let ((blacklist (configf:lookup *configdat* "setup" "blacklistvars")))
            (if blacklist
                (save-environment-as-files "megatest" ignorevars: (string-split blacklist))
                (save-environment-as-files "megatest")))
          ;;(bb-check-path msg: "launch:execute post block 44")
	  ;; open-run-close not needed for test-set-meta-info
	  ;; (tests:set-full-meta-info #f test-id run-id 0 work-area)
	  ;; (tests:set-full-meta-info test-id run-id 0 work-area)
	  (tests:set-full-meta-info #f test-id run-id 0 work-area 10)

	  ;; (thread-sleep! 0.3) ;; NFS slowness has caused grief here

	  (if (args:get-arg "-xterm")
	      (set! fullrunscript "xterm")
	      (if (and fullrunscript 
		       (common:file-exists? fullrunscript)
		       (not (file-execute-access? fullrunscript)))
		  (system (conc "chmod ug+x " fullrunscript))))

	  ;; We are about to actually kick off the test
	  ;; so this is a good place to remove the records for 
	  ;; any previous runs
	  ;; (db:test-remove-steps db run-id testname itemdat)
	  ;; now is also a good time to write the .testconfig file
	  (let* ((tconfig-fname   (conc work-area "/.testconfig"))
		 (tconfig-tmpfile (conc tconfig-fname ".tmp"))
		 (tconfig         (tests:get-testconfig test-name item-path tconfigreg #t force-create: #t))) ;; 'return-procs)))
	    (configf:write-alist tconfig tconfig-tmpfile)
	    (file-move tconfig-tmpfile tconfig-fname #t))
	  ;; 
	  (let* ((m            (make-mutex))
		 (kill-job?    #f)
		 (exit-info    (make-launch:einf pid: #t exit-status: #t exit-code: #t rollup-status: 0)) ;; pid exit-status exit-code (i.e. process was successfully run) rollup-status
		 (job-thread   #f)
		 ;; (keep-going   #t)
		 (misc-flags   (let ((ht (make-hash-table)))
				 (hash-table-set! ht 'keep-going #t)
				 ht))
		 (runit        (lambda ()
				 (launch:manage-steps run-id test-id item-path fullrunscript ezsteps subrun test-name tconfigreg exit-info m)))
		 (monitorjob   (lambda ()
				 (launch:monitor-job  run-id test-id item-path fullrunscript ezsteps test-name tconfigreg exit-info m work-area runtlim misc-flags)))
		 (th1          (make-thread monitorjob "monitor job"))
		 (th2          (make-thread runit "run job")))
	    (set! job-thread th2)
	    (thread-start! th1)
	    (thread-start! th2)
	    (thread-join! th2)
	    (debug:print-info 0 *default-log-port* "Megatest exectute of test " test-name ", item path " item-path " complete. Notifying the db ...")
	    (hash-table-set! misc-flags 'keep-going #f)
	    (thread-join! th1)
	    (thread-sleep! 1)       ;; givbe thread th1 a chance to be done TODO: Verify this is needed. At 0.1 I was getting fail to stop, increased to total of 1.1 sec.
	    (mutex-lock! m)
	    (let* ((item-path (item-list->path itemdat))
		   ;; only state and status needed - use lazy routine
		   (testinfo  (rmt:get-testinfo-state-status run-id test-id)))
	      ;; Am I completed?
	      (if (member (db:test-get-state testinfo) '("REMOTEHOSTSTART" "RUNNING")) ;; NOTE: It should *not* be REMOTEHOSTSTART but for reasons I don't yet understand it sometimes gets stuck in that state ;; (not (equal? (db:test-get-state testinfo) "COMPLETED"))
		  (let ((new-state  (if kill-job? "KILLED" "COMPLETED") ;; (if (eq? (vector-ref exit-info 2) 0) ;; exited with "good" status
				                                        ;; "COMPLETED"
							                ;; (db:test-get-state testinfo)))   ;; else preseve the state as set within the test
				    )
			(new-status (cond
				     ((not (launch:einf-exit-status exit-info)) "FAIL") ;; job failed to run ... (vector-ref exit-info 1)
				     ((eq? (launch:einf-rollup-status exit-info) 0)     ;; (vector-ref exit-info 3)
				      ;; if the current status is AUTO then defer to the calculated value (i.e. leave this AUTO)
				      (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO" "PASS"))
				     ((eq? (launch:einf-rollup-status exit-info) 1) "FAIL")  ;; (vector-ref exit-info 3)
				     ((eq? (launch:einf-rollup-status exit-info) 2)	     ;;	(vector-ref exit-info 3)
				      ;; if the current status is AUTO the defer to the calculated value but qualify (i.e. make this AUTO-WARN)
				      (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO-WARN" "WARN"))
				     ((eq? (launch:einf-rollup-status exit-info) 3) "CHECK")
				     ((eq? (launch:einf-rollup-status exit-info) 4) "WAIVED")
				     ((eq? (launch:einf-rollup-status exit-info) 5) "ABORT")
				     ((eq? (launch:einf-rollup-status exit-info) 6) "SKIP")
				     (else "FAIL")))) ;; (db:test-get-status testinfo)))
		    (debug:print-info 1 *default-log-port* "Test exited in state=" (db:test-get-state testinfo) ", setting state/status based on exit code of " (launch:einf-exit-status exit-info) " and rollup-status of " (launch:einf-rollup-status exit-info))
		    (tests:test-set-status! run-id 
					    test-id 
					    new-state
					    new-status
					    (args:get-arg "-m") #f)
		    ;; need to update the top test record if PASS or FAIL and this is a subtest
		    ;; NO NEED TO CALL set-state-status-and-roll-up-items HERE, THIS IS DONE IN set-state-status-and-roll-up-items called by tests:test-set-status!
		    ))
	      ;; for automated creation of the rollup html file this is a good place...
	      (if (not (equal? item-path ""))
		  (tests:summarize-items run-id test-id test-name #f))
	      (tests:summarize-test run-id test-id)  ;; don't force - just update if no
	      (rmt:update-run-stats run-id (rmt:get-raw-run-stats run-id)))
	    (mutex-unlock! m)
	    (debug:print 2 *default-log-port* "Output from running " fullrunscript ", pid " (launch:einf-pid exit-info) " in work area " 
			 work-area ":\n====\n exit code " (launch:einf-exit-code exit-info) "\n" "====\n")
	    (if (not (launch:einf-exit-status exit-info))
		(exit 4))))
        )))

;; DO NOT USE - caching of configs is handled in launch:setup now.
;;
(define (launch:cache-config)
  ;; if we have a linktree and -runtests and -target and the directory exists dump the config
  ;; to megatest-(current-seconds).cfg and symlink it to megatest.cfg
  (if (and *configdat* 
	   (or (args:get-arg "-run")
	       (args:get-arg "-runtests")
	       (args:get-arg "-execute")))
      (let* ((linktree (common:get-linktree)) ;; (get-environment-variable "MT_LINKTREE"))
	     (target   (common:args-get-target exit-if-bad: #t))
	     (runname  (or (args:get-arg "-runname")
			   (args:get-arg ":runname")
			   (getenv "MT_RUNNAME")))
	     (fulldir  (conc linktree "/"
			     target "/"
			     runname)))
	(if (and linktree (common:file-exists? linktree)) ;; can't proceed without linktree
	    (begin
	      (debug:print-info 0 *default-log-port* "Have -run with target=" target ", runname=" runname ", fulldir=" fulldir ", testpatt=" (or (args:get-arg "-testpatt") "%"))
	      (if (not (common:file-exists? fulldir))
		  (create-directory fulldir #t)) ;; need to protect with exception handler 
	      (if (and target
		       runname
		       (common:file-exists? fulldir))
		  (let ((tmpfile  (conc fulldir "/.megatest.cfg." (current-seconds)))
			(targfile (conc fulldir "/.megatest.cfg-"  megatest-version "-" megatest-fossil-hash))
			(rconfig  (conc fulldir "/.runconfig." megatest-version "-" megatest-fossil-hash)))
		    (if (common:file-exists? rconfig) ;; only cache megatest.config AFTER runconfigs has been cached
			(begin
			  (debug:print-info 0 *default-log-port* "Caching megatest.config in " tmpfile)
                          (if (not (common:in-running-test?))
                              (configf:write-alist *configdat* tmpfile))
			  (system (conc "ln -sf " tmpfile " " targfile))))
		    )))
	    (debug:print-info 1 *default-log-port* "No linktree yet, no caching configs.")))))


;; gather available information, if legit read configs in this order:
;;
;;   if have cache;
;;      read it a return it
;;   else
;;     megatest.config     (do not cache)
;;     runconfigs.config   (cache if all vars avail)
;;     megatest.config     (cache if all vars avail)
;;   returns:
;;     *toppath*
;;   side effects:
;;     sets; *configdat*    (megatest.config info)
;;           *runconfigdat* (runconfigs.config info)
;;           *configstatus* (status of the read data)
;;
(define (launch:setup #!key (force-reread #f) (areapath #f))
  (mutex-lock! *launch-setup-mutex*)
  (if (and *toppath*
	   (eq? *configstatus* 'fulldata) (not force-reread)) ;; got it all
      (begin
	(debug:print 2 *default-log-port* "NOTE: skipping launch:setup-body call since we have fulldata")
	(mutex-unlock! *launch-setup-mutex*)
	*toppath*)
      (let ((res (launch:setup-body force-reread: force-reread areapath: areapath)))
	(mutex-unlock! *launch-setup-mutex*)
	res)))

;; return paths depending on what info is available.
;;
(define (launch:get-cache-file-paths areapath toppath target mtconfig)
  (let* ((use-cache (common:use-cache?))
         (runname  (common:args-get-runname))
         (linktree (common:get-linktree))
         (testname (common:get-full-test-name))
         (rundir   (if (and runname target linktree)
                       (common:directory-writable? (conc linktree "/" target "/" runname))
                       #f))
         (testdir  (if (and rundir testname)
                       (common:directory-writable? (conc rundir "/" testname))
                       #f))
         (cachedir (or testdir rundir))
         (mtcachef (and cachedir (conc cachedir "/" ".megatest.cfg-"  megatest-version "-" megatest-fossil-hash)))
         (rccachef (and cachedir (conc cachedir "/" ".runconfigs.cfg-"  megatest-version "-" megatest-fossil-hash))))
    (debug:print-info 6 *default-log-port* 
                      "runname=" runname 
                      "\n  linktree=" linktree
                      "\n  testname=" testname
                      "\n  rundir=" rundir 
                      "\n  testdir=" testdir 
                      "\n  cachedir=" cachedir
                      "\n  mtcachef=" mtcachef
                      "\n  rccachef=" rccachef)
    (cons mtcachef rccachef)))

(define (launch:setup-body #!key (force-reread #f) (areapath #f))
  (if (and (eq? *configstatus* 'fulldata)
	   *toppath*
	   (not force-reread)) ;; no need to reprocess
      *toppath*   ;; return toppath
      (let* ((use-cache (common:use-cache?)) ;; BB- use-cache checks *configdat* for use-cache setting.  We do not have *configdat*.  Bootstrapping problem here.
	     (toppath  (or *toppath* areapath (getenv "MT_RUN_AREA_HOME"))) ;; preserve toppath
	     (target   (common:args-get-target))
	     (sections (if target (list "default" target) #f)) ;; for runconfigs
	     (mtconfig (or (args:get-arg "-config") "megatest.config")) ;; allow overriding megatest.config 
             (cachefiles (launch:get-cache-file-paths areapath toppath target mtconfig))
	     ;; checking for null cachefiles should not be necessary, I was seeing error car of '(), might be a chicken bug or a red herring ...
	     (mtcachef   (if (null? cachefiles)
			     #f
			     (car cachefiles))) ;; (and cachedir (conc cachedir "/" ".megatest.cfg-"  megatest-version "-" megatest-fossil-hash)))
	     (rccachef   (if (null? cachefiles)
			     #f
			     (cdr cachefiles)))) ;; (and cachedir (conc cachedir "/" ".runconfigs.cfg-"  megatest-version "-" megatest-fossil-hash)))
	      ;; (cancreate (and cachedir (common:file-exists? cachedir)(file-write-access? cachedir) (not (common:in-running-test?)))))
	(set! *toppath* toppath) ;; This is needed when we are running as a test using CMDINFO as a datasource
        ;;(BB> "launch:setup-body -- cachefiles="cachefiles)
	(cond
	 ;; if mtcachef exists just read it, however we need to assume toppath is available in $MT_RUN_AREA_HOME
	 ((and (not force-reread)
	       mtcachef  rccachef
	       use-cache
	       (get-environment-variable "MT_RUN_AREA_HOME")
	       (common:file-exists? mtcachef)
	       (common:file-exists? rccachef))
          ;;(BB> "launch:setup-body -- cond branch 1 - use-cache")
          (set! *configdat*    (configf:read-alist mtcachef))
          ;;(BB> "launch:setup-body -- 1 set! *configdat*="*configdat*)
	  (set! *runconfigdat* (configf:read-alist rccachef))
	  (set! *configinfo*   (list *configdat*  (get-environment-variable "MT_RUN_AREA_HOME")))
	  (set! *configstatus* 'fulldata)
	  (set! *toppath*      (get-environment-variable "MT_RUN_AREA_HOME"))
	  *toppath*)
	 ;; there are no existing cached configs, do full reads of the configs and cache them
	 ;; we have all the info needed to fully process runconfigs and megatest.config
	 ((and ;; (not force-reread) ;; force-reread is irrelevant in the AND, could however OR it?
	       mtcachef
	       rccachef) ;; BB- why are we doing this without asking if caching is desired?
          ;;(BB> "launch:setup-body -- cond branch 2")
	  (let* ((first-pass    (find-and-read-config        ;; NB// sets MT_RUN_AREA_HOME as side effect
				 mtconfig
				 environ-patt: "env-override"
				 given-toppath: toppath
				 pathenvvar: "MT_RUN_AREA_HOME"))
		 (first-rundat  (let ((toppath (if toppath 
						   toppath
						   (car first-pass))))
				  (read-config ;; (conc toppath "/runconfigs.config") ;; this should be converted to runconfig:read but it is non-trivial, leaving it for now.
				   (conc (if (string? toppath)
					     toppath
					     (get-environment-variable "MT_RUN_AREA_HOME"))
					 "/runconfigs.config")
				   *runconfigdat* #t 
				   sections: sections))))
	    (set! *runconfigdat* first-rundat)
	    (if first-pass  ;; 
		(begin
                  ;;(BB> "launch:setup-body -- \"first-pass\"=first-pass")
		  (set! *configdat*  (car first-pass))
                  ;;(BB> "launch:setup-body -- 2 set! *configdat*="*configdat*)
		  (set! *configinfo* first-pass)
		  (set! *toppath*    (or toppath (cadr first-pass))) ;; use the gathered data unless already have it
		  (set! toppath      *toppath*)
		  (if (not *toppath*)
		      (begin
			(debug:print-error 0 *default-log-port* "you are not in a megatest area!")
			(exit 1)))
		  (setenv "MT_RUN_AREA_HOME" *toppath*)
		  ;; the seed read is done, now read runconfigs, cache it then read megatest.config one more time and cache it
		  (let* ((keys         (rmt:get-keys))
			 (key-vals     (keys:target->keyval keys target))
			 (linktree     (common:get-linktree)) ;; (or (getenv "MT_LINKTREE")(if *configdat* (configf:lookup *configdat* "setup" "linktree") #f)))
					;     (if *configdat*
					; 	   (configf:lookup *configdat* "setup" "linktree")
					; 	   (conc *toppath* "/lt"))))
			 (second-pass  (find-and-read-config
					mtconfig
					environ-patt: "env-override"
					given-toppath: toppath
					pathenvvar: "MT_RUN_AREA_HOME"))
			 (runconfigdat (begin     ;; this read of the runconfigs will see any adjustments made by re-reading megatest.config
					 (for-each (lambda (kt)
						     (setenv (car kt) (cadr kt)))
						   key-vals)
					 (read-config (conc toppath "/runconfigs.config") *runconfigdat* #t ;; consider using runconfig:read some day ...
						      sections: sections)))
                         (cachefiles   (launch:get-cache-file-paths areapath toppath target mtconfig))
                         (mtcachef     (car cachefiles))
                         (rccachef     (cdr cachefiles)))
                    ;;  trap exception due to stale NFS handle -- Error: (open-output-file) cannot open file - Stale NFS file handle: "/p/fdk/gwa/lefkowit/mtTesting/qa/primbeqa/links/p1222/11/PDK_r1.1.1/prim/clean/pcell_testgen/.runconfigs.cfg-1.6427-7d1e789cb3f62f9cde719a4865bb51b3c17ea853" - ticket 220546342
                    ;; TODO - consider 1) using simple-lock to bracket cache write
                    ;;                 2) cache in hash on server, since need to do rmt: anyway to lock.

		    (if rccachef
                        (common:fail-safe
                         (lambda ()
                           (configf:write-alist runconfigdat rccachef))
                         (conc "Could not write cache file - "rccachef)))
                    (if mtcachef
                        (common:fail-safe
                         (lambda ()
                           (configf:write-alist *configdat* mtcachef))
                         (conc "Could not write cache file - "mtcachef)))
		    (set! *runconfigdat* runconfigdat)
		    (if (and rccachef mtcachef) (set! *configstatus* 'fulldata))))
		;; no configs found? should not happen but let's try to recover gracefully, return an empty hash-table
		(set! *configdat* (make-hash-table))
		)))

	 ;; else read what you can and set the flag accordingly
	 ;; here we don't have either mtconfig or rccachef
	 (else
          ;;(BB> "launch:setup-body -- cond branch 3 - else")
	  (let* ((cfgdat   (find-and-read-config 
			    (or (args:get-arg "-config") "megatest.config")
			    environ-patt: "env-override"
			    given-toppath: (get-environment-variable "MT_RUN_AREA_HOME")
			    pathenvvar: "MT_RUN_AREA_HOME")))

            (if (and cfgdat (list? cfgdat) (> (length cfgdat) 0) (hash-table? (car cfgdat)))
		(let* ((toppath  (or (get-environment-variable "MT_RUN_AREA_HOME")(cadr cfgdat)))
		       (rdat     (read-config (conc toppath  ;; convert this to use runconfig:read!
						    "/runconfigs.config") *runconfigdat* #t sections: sections)))
		  (set! *configinfo*   cfgdat)
		  (set! *configdat*    (car cfgdat))
		  (set! *runconfigdat* rdat)
		  (set! *toppath*      toppath)
		  (set! *configstatus* 'partial))
		(begin
		  (debug:print-error 0 *default-log-port* "No " mtconfig " file found. Giving up.")
		  (exit 2))))))
	;; COND ends here.
	
	;; additional house keeping
	(let* ((linktree (or (common:get-linktree)
			     (conc *toppath* "/lt"))))
	  (if linktree
	      (begin
		(if (not (common:file-exists? linktree))
		    (begin
		      (handle-exceptions
			  exn
			  (begin
			    (debug:print-error 0 *default-log-port* "Something went wrong when trying to create linktree dir at " linktree)
			    (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
			    (exit 1))
			(create-directory linktree #t))))
		(handle-exceptions
		    exn
		    (begin
		      (debug:print-error 0 *default-log-port* "Something went wrong when trying to create link to linktree at " *toppath*)
		      (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn)))
		  (let ((tlink (conc *toppath* "/lt")))
		    (if (not (common:file-exists? tlink))
			(create-symbolic-link linktree tlink)))))
	      (begin
		(debug:print-error 0 *default-log-port* "linktree not defined in [setup] section of megatest.config")
		)))
	(if (and *toppath*
		 (directory-exists? *toppath*))
	    (begin
	      (setenv "MT_RUN_AREA_HOME" *toppath*)
	      (setenv "MT_TESTSUITENAME" (common:get-testsuite-name)))
	    (begin
	      (debug:print-error 0 *default-log-port* "failed to find the top path to your Megatest area.")
	      (set! *toppath* #f) ;; force it to be false so we return #f
	      #f))
	
        ;; one more attempt to cache the configs for future reading
        (let* ((cachefiles   (launch:get-cache-file-paths areapath toppath target mtconfig))
               (mtcachef     (car cachefiles))
               (rccachef     (cdr cachefiles)))

          ;; trap exception due to stale NFS handle -- Error: (open-output-file) cannot open file - Stale NFS file handle: "...somepath.../.runconfigs.cfg-1.6427-7d1e789cb3f62f9cde719a4865bb51b3c17ea853" - ticket 220546342
          ;; TODO - consider 1) using simple-lock to bracket cache write
          ;;                 2) cache in hash on server, since need to do rmt: anyway to lock.
          (if (and rccachef *runconfigdat* (not (common:file-exists? rccachef)))
              (common:fail-safe
               (lambda ()
                 (configf:write-alist *runconfigdat* rccachef))
               (conc "Could not write cache file - "rccachef))
              )
          (if (and mtcachef *configdat*    (not (common:file-exists? mtcachef)))
              (common:fail-safe
               (lambda ()
                 (configf:write-alist *configdat* mtcachef))
               (conc "Could not write cache file - "mtcachef))
              )
          (if (and rccachef mtcachef *runconfigdat* *configdat*)
              (set! *configstatus* 'fulldata)))

	;; if have -append-config then read and append here
	(let ((cfname (args:get-arg "-append-config")))
	  (if (and cfname
		   (file-read-access? cfname))
	      (read-config cfname *configdat* #t))) ;; values are added to the hash, no need to do anything special.
	*toppath*)))

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
		(if (common:low-noise-print 20 "No valid disks or no disk with enough space")
		    (debug:print-error 0 *default-log-port* "No valid disks found in megatest.config. Please add some to your [disks] section and ensure the directory exists and has enough space!\n    You can change minspace in the [setup] section of megatest.config. Current setting is: " minspace))
		(exit 1))))))) ;; TODO - move the exit to the calling location and return #f

(define (launch:test-copy test-src-path test-path)
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
	(debug:print 2 *default-log-port* "ERROR: problem with running \"" cmd "\""))))


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
	 (contour   #f) ;; NOT READY FOR THIS (args:get-arg "-contour"))
	 ;; convert back to db: from rdb: - this is always run at server end
	 (target   (string-intersperse (map cadr keyvals) "/"))

	 (not-iterated  (equal? "" item-path))

	 ;; all tests are found at <rundir>/test-base or <linkdir>/test-base
	 (testtop-base (conc target "/" runname "/" testname))
	 (test-base    (conc testtop-base (if not-iterated "" "/") item-path))

	 ;; nb// if itempath is not "" then it is prefixed with "/"
	 (toptest-path (conc disk-path (if contour (conc "/" contour) "") "/" testtop-base))
	 (test-path    (conc disk-path (if contour (conc "/" contour) "") "/" test-base))

	 ;; ensure this exists first as links to subtests must be created there
	 (linktree  (common:get-linktree))
	 ;; WAS: (let ((rd (config-lookup *configdat* "setup" "linktree")))
	 ;;         (if rd rd (conc *toppath* "/runs"))))
	 ;; which seems wrong ...

	 (lnkbase   (conc linktree (if contour (conc "/" contour) "") "/" target "/" runname))
	 (lnkpath   (conc lnkbase "/" testname))
	 (lnkpathf  (conc lnkpath (if not-iterated "" "/") item-path))
	 (lnktarget (conc lnkpath "/" item-path)))

    ;; Update the rundir path in the test record for all, rundir=physical, shortdir=logical
    ;;                                                 rundir   shortdir
    (rmt:general-call 'test-set-rundir-shortdir run-id lnkpathf test-path testname item-path run-id)

    (debug:print 2 *default-log-port* "INFO:\n       lnkbase=" lnkbase "\n       lnkpath=" lnkpath "\n  toptest-path=" toptest-path "\n     test-path=" test-path)
    (if (not (common:file-exists? linktree))
	(begin
	  (debug:print 0 *default-log-port* "WARNING: linktree did not exist! Creating it now at " linktree)
	  (create-directory linktree #t))) ;; (system (conc "mkdir -p " linktree))))
    ;; create the directory for the tests dir links, this is needed no matter what...
    (if (and (not (common:directory-exists? lnkbase))
	     (not (common:file-exists? lnkbase)))
	(handle-exceptions
	 exn
	 (begin
	   (debug:print-error 0 *default-log-port* "Problem creating linktree base at " lnkbase)
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
	  (debug:print-info 2 *default-log-port* "Creating iterated parent " iterated-parent)
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print-error 0 *default-log-port* " Failed to create directory " iterated-parent ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit 1))
	   (create-directory iterated-parent #t))))

    (if (symbolic-link? lnkpath) 
	(handle-exceptions
	 exn
	 (begin
	   (debug:print-error 0 *default-log-port* " Failed to remove symlink " lnkpath ((condition-property-accessor 'exn 'message) exn) ", exiting")
	   (exit 1))
	 (delete-file lnkpath)))

    (if (not (or (common:file-exists? lnkpath)
		 (symbolic-link? lnkpath)))
	(handle-exceptions
	 exn
	 (begin
	   (debug:print-error 0 *default-log-port* " Failed to create symlink " lnkpath ((condition-property-accessor 'exn 'message) exn) ", exiting")
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
				   (db:test-get-rundir testinfo) ;; ) ;; )
				   #f)))
	  (hash-table-set! *toptest-paths* testname curr-test-path)
	  ;; NB// Was this for the test or for the parent in an iterated test?
	  (rmt:general-call 'test-set-rundir-shortdir run-id lnkpath 
			    (if (common:file-exists? lnkpath)
				;; (resolve-pathname lnkpath)
				(common:nice-path lnkpath)
				lnkpath)
			    testname "" run-id)
	  ;; (rmt:general-call 'test-set-rundir run-id lnkpath testname "") ;; toptest-path)
	  (if (or (not curr-test-path)
		  (not (directory-exists? toptest-path)))
	      (begin
		(debug:print-info 2 *default-log-port* "Creating " toptest-path " and link " lnkpath)
		(handle-exceptions
		 exn
		 #f ;; don't care to catch and deal with errors here for now.
		 (create-directory toptest-path #t))
		(hash-table-set! *toptest-paths* testname toptest-path)))))

    ;; The toptest path has been created, the link to the test in the linktree has
    ;; been created. Now, if this is an iterated test the real test dir must be created
    (if (not not-iterated) ;; this is an iterated test
	(begin ;; (let ((lnktarget (conc lnkpath "/" item-path)))
	  (debug:print 2 *default-log-port* "Setting up sub test run area")
	  (debug:print 2 *default-log-port* " - creating run area in " test-path)
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print-error 0 *default-log-port* " Failed to create directory " test-path ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit 1))
	   (create-directory test-path #t))
	  (debug:print 2 *default-log-port* 
		       " - creating link from: " test-path "\n"
		       "                   to: " lnktarget)

	  ;; If there is already a symlink delete it and recreate it.
	  (handle-exceptions
	   exn
	   (begin
	     (debug:print-error 0 *default-log-port* " Failed to re-create link " lnktarget ((condition-property-accessor 'exn 'message) exn) ", exiting")
	     (exit))
	   (if (symbolic-link? lnktarget)     (delete-file lnktarget))
	   (if (not (common:file-exists? lnktarget)) (create-symbolic-link test-path lnktarget)))))

    (if (not (directory? test-path))
	(create-directory test-path #t)) ;; this is a hack, I don't know why out of the blue this path does not exist sometimes

    (if (and test-src-path (directory? test-path))
	(begin
	  (launch:test-copy test-src-path test-path)
	  (list lnkpathf lnkpath ))
	(if (and test-src-path (> remtries 0))
	    (begin
	      (debug:print-error 0 *default-log-port* "Failed to create work area at " test-path " with link at " lnktarget ", remaining attempts " remtries)
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

(define (launch-test . args)
  (let ((child-pid (process-fork)))
    (if (zero? child-pid)
        (begin
          (set! *common:local-force-server* 'always)
          (apply launch-test-inner args))
        #t)))

(define (launch-test-inner test-id run-id run-info keyvals runname test-conf test-name test-path itemdat params)
  (mutex-lock! *launch-setup-mutex*) ;; setting variables and processing the testconfig is NOT thread-safe, reuse the launch-setup mutex
  (let* ( ;; (lock-key        (conc "test-" test-id))
	;; (got-lock        (let loop ((lock        (rmt:no-sync-get-lock lock-key))
	;; 			     (expire-time (+ (current-seconds) 15))) ;; give up on getting the lock and steal it after 15 seconds
	;; 		    (if (car lock)
	;; 			#t
	;; 			(if (> (current-seconds) expire-time)
	;; 			    (begin
	;; 			      (debug:print-info 0 *default-log-port* "Timed out waiting for a lock to launch test " keyvals " " runname " " test-name " " test-path)
	;; 			      (rmt:no-sync-del! lock-key) ;; destroy the lock
	;; 			      (loop (rmt:no-sync-get-lock lock-key) expire-time)) ;; 
	;; 			    (begin
	;; 			      (thread-sleep! 1)
	;; 			      (loop (rmt:no-sync-get-lock lock-key) expire-time))))))
	 (item-path       (item-list->path itemdat))
	 (contour         #f)) ;; NOT READY FOR THIS (args:get-arg "-contour")))
    (let loop ((delta        (- (current-seconds) *last-launch*))
	       (launch-delay (configf:lookup-number *configdat* "setup" "launch-delay" default: 1)))
      (if (> launch-delay delta)
	  (begin
	    (if (common:low-noise-print 1200 "test launch delay") ;; every two hours or so remind the user about launch delay.
		(debug:print-info 0 *default-log-port* "NOTE: test launches are delayed by " launch-delay " seconds. See megatest.config launch-delay setting to adjust.")) ;; launch of " test-name " for " (- launch-delay delta) " seconds"))
	    (thread-sleep! (- launch-delay delta))
	    (loop (- (current-seconds) *last-launch*) launch-delay))))
    (change-directory *toppath*)
    (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute", *maybe* - the longer they are set the longer each launch takes (must be non-overlapping with the vars)
     (append
      (list
       (list "MT_RUN_AREA_HOME" *toppath*)
       (list "MT_TEST_NAME" test-name)
       (list "MT_RUNNAME"   runname)
       (list "MT_ITEMPATH"  item-path)
       (list "MT_CONTOUR"   contour)
       )
      itemdat))
    (let* ((tregistry       (tests:get-all)) ;; third param (below) is system-allowed
           ;; for tconfig, why do we allow fallback to test-conf?
	   (tconfig         (or (tests:get-testconfig test-name item-path tregistry #t force-create: #t)
				(begin
                                  (debug:print 0 *default-log-port* "WARNING: falling back to pre-calculated testconfig. This is likely not desired.")
                                  test-conf))) ;; force re-read now that all vars are set
	   (useshell        (let ((ush (config-lookup *configdat* "jobtools"     "useshell")))
			      (if ush 
				  (if (equal? ush "no") ;; must use "no" to NOT use shell
				      #f
				      ush)
				  #t)))     ;; default is yes
	   (runscript       (config-lookup tconfig   "setup"        "runscript"))
	   (ezsteps         (> (length (hash-table-ref/default tconfig "ezsteps" '())) 0)) ;; don't send all the steps, could be big, just send a flag
	   (subrun          (> (length (hash-table-ref/default tconfig "subrun"  '())) 0)) ;; send a flag to process a subrun
	   ;; (diskspace       (config-lookup tconfig   "requirements" "diskspace"))
	   ;; (memory          (config-lookup tconfig   "requirements" "memory"))
	   ;; (hosts           (config-lookup *configdat* "jobtools"     "workhosts")) ;; I'm pretty sure this was never completed
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
	   (test-sig        (conc (common:get-testsuite-name) ":" test-name ":" item-path)) ;; (item-list->path itemdat))) ;; test-path is the full path including the item-path
	   (work-area       #f)
	   (toptest-work-area #f) ;; for iterated tests the top test contains data relevant for all
	   (diskpath   #f)
	   (cmdparms   #f)
	   (fullcmd    #f) ;; (define a (with-output-to-string (lambda ()(write x))))
	   (mt-bindir-path #f)
	   (testinfo   (rmt:get-test-info-by-id run-id test-id))
	   (mt_target  (string-intersperse (map cadr keyvals) "/"))
	   (debug-param (append (if (args:get-arg "-debug")  (list "-debug" (args:get-arg "-debug")) '())
				(if (args:get-arg "-logging")(list "-logging") '()))))
      ;; (if hosts (set! hosts (string-split hosts)))
      ;; set the megatest to be called on the remote host
      (if (not remote-megatest)(set! remote-megatest local-megatest)) ;; "megatest"))
      (set! mt-bindir-path (pathname-directory remote-megatest))
      (if launcher (set! launcher (string-split launcher)))
      ;; set up the run work area for this test
      (if (and (args:get-arg "-preclean") ;; user has requested to preclean for this run
	       (not (member (db:test-get-rundir testinfo)(list "n/a" "/tmp/badname")))) ;; n/a is a placeholder and thus not a read dir
	  (begin
	    (debug:print-info 0 *default-log-port* "attempting to preclean directory " (db:test-get-rundir testinfo) " for test " test-name "/" item-path)
	    (runs:remove-test-directory testinfo 'remove-data-only))) ;; remove data only, do not perturb the record
      
      ;; prevent overlapping actions - set to LAUNCHED as early as possible
      ;;
      ;; the following call handles waiver propogation. cannot yet condense into roll-up-pass-fail
      (tests:test-set-status! run-id test-id "LAUNCHED" "n/a" #f #f) ;; (if launch-results launch-results "FAILED"))
      (rmt:set-state-status-and-roll-up-items run-id test-name item-path #f "LAUNCHED" #f)
      ;; (pp (hash-table->alist tconfig))
      (set! diskpath (get-best-disk *configdat* tconfig))
      (if diskpath
	  (let ((dat  (create-work-area run-id run-info keyvals test-id test-path diskpath test-name itemdat)))
	    (set! work-area (car dat))
	    (set! toptest-work-area (cadr dat))
	    (debug:print-info 2 *default-log-port* "Using work area " work-area))
	  (begin
	    (set! work-area (conc test-path "/tmp_run"))
	    (create-directory work-area #t)
	    (debug:print 0 *default-log-port* "WARNING: No disk work area specified - running in the test directory under tmp_run")))
      (set! cmdparms (base64:base64-encode 
		      (z3:encode-buffer 
		       (with-output-to-string
			 (lambda () ;; (list 'hosts     hosts)
			   (write (list (list 'testpath  test-path)
					;; (list 'transport (conc *transport-type*))
					;; (list 'serverinf *server-info*)
					(list 'homehost  (let* ((hhdat (common:get-homehost)))
							   (if hhdat
							       (car hhdat)
							       #f)))
					(list 'serverurl (if *runremote*
							     (remote-server-url *runremote*)
							     #f)) ;;
					(list 'areaname  (common:get-testsuite-name))
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
					(list 'subrun    subrun)
					(list 'target    mt_target)
					(list 'contour   contour)
					(list 'runtlim   (if run-time-limit (common:hms-string->seconds run-time-limit) #f))
					(list 'env-ovrd  (hash-table-ref/default *configdat* "env-override" '())) 
					(list 'set-vars  (if params (hash-table-ref/default params "-setvars" #f)))
					(list 'runname   runname)
					(list 'mt-bindir-path mt-bindir-path))))))))
      
      ;; clean out step records from previous run if they exist
      ;; (rmt:delete-test-step-records run-id test-id)
      ;; if the dir does not exist we may have a itempath where individual variables are a path, launch anyway
      (if (common:file-exists? work-area)
	  (change-directory work-area)) ;; so that log files from the launch process don't clutter the test dir
      (cond
       ;; ((and launcher hosts) ;; must be using ssh hostname
       ;;    (set! fullcmd (append launcher (car hosts)(list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param)))
       ;; (set! fullcmd (append launcher (car hosts)(list remote-megatest test-sig "-execute" cmdparms))))
       (launcher
	(set! fullcmd (append launcher (list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param)))
       ;; (set! fullcmd (append launcher (list remote-megatest test-sig "-execute" cmdparms))))
       (else
	(if (not useshell)(debug:print 0 *default-log-port* "WARNING: internal launching will not work well without \"useshell yes\" in your [jobtools] section"))
	(set! fullcmd (append (list remote-megatest "-m" test-sig "-execute" cmdparms) debug-param (list (if useshell "&" ""))))))
      ;; (set! fullcmd (list remote-megatest test-sig "-execute" cmdparms (if useshell "&" "")))))
      (if (args:get-arg "-xterm")(set! fullcmd (append fullcmd (list "-xterm"))))
      (debug:print 1 *default-log-port* "Launching " work-area)
      ;; set pre-launch-env-vars before launching, keep the vars in prevvals and put the envionment back when done
      (debug:print 4 *default-log-port* "fullcmd: " fullcmd)
      (set! *last-launch* (current-seconds)) ;; all that junk above takes time, set this as late as possible.
      (let* ((commonprevvals (alist->env-vars
			      (hash-table-ref/default *configdat* "env-override" '())))
	     (miscprevvals   (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
			      (append (list (list "MT_TEST_RUN_DIR" work-area)
					    (list "MT_TEST_NAME" test-name)
					    (list "MT_ITEM_INFO" (conc itemdat)) 
					    (list "MT_RUNNAME"   runname)
					    (list "MT_TARGET"    mt_target)
					    (list "MT_ITEMPATH"  item-path)
					    )
				      itemdat)))
	     (testprevvals   (alist->env-vars
			      (hash-table-ref/default tconfig "pre-launch-env-overrides" '())))
	     ;; Launchwait defaults to true, must override it to turn off wait
	     (launchwait     (if (equal? (configf:lookup *configdat* "setup" "launchwait") "no") #f #t))
	     (launch-results-prev (apply (if launchwait ;; BB: TODO: refactor this to examine return code of launcher, if nonzero, set state to launch failed.
					process:cmd-run-with-stderr-and-exitcode->list
					process-run)
				    (if useshell
					(let ((cmdstr (string-intersperse fullcmd " ")))
					  (if launchwait
					      cmdstr
					      (conc cmdstr " >> mt_launch.log 2>&1 &")))
					(car fullcmd))
				    (if useshell
					'()
					(cdr fullcmd))))
             (success        (if launchwait (equal? 0 (cadr launch-results-prev)) #t))
             (launch-results (if launchwait (car launch-results-prev) launch-results-prev)))
        (if (not success)
            (tests:test-set-status! run-id test-id "COMPLETED" "DEAD" "launcher failed; exited non-zero; check mt_launch.log" #f)) ;; (if launch-results launch-results "FAILED"))
        (mutex-unlock! *launch-setup-mutex*) ;; yes, really should mutex all the way to here. Need to put this entire process into a fork.
	;; (rmt:no-sync-del! lock-key)         ;; release the lock for starting this test
	(if (not launchwait) ;; give the OS a little time to allow the process to start
	    (thread-sleep! 0.01))
	(with-output-to-file "mt_launch.log"
	  (lambda ()
	    (print "LAUNCHCMD: " (string-intersperse fullcmd " "))
	    (if (list? launch-results)
		(apply print launch-results)
		(print "NOTE: launched \"" fullcmd "\"\n  but did not wait for it to proceed. Add the following to megatest.config \n[setup]\nlaunchwait yes\n  if you have problems with this"))
	    #:append))
	(debug:print 2 *default-log-port* "Launching completed, updating db")
	(debug:print 2 *default-log-port* "Launch results: " launch-results)
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
    (change-directory *toppath*)))

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
