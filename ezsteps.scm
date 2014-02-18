
;; Copyright 2006-2012, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking directory-utils)
(import (prefix sqlite3 sqlite3:))

(declare (unit ezsteps))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
;; (declare (uses sdb))
;; (declare (uses filedb))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")

(define (ezsteps:run-from testdat start-step-name run-one)
  (let* ((test-run-dir  ;; (filedb:get-path *fdb* 
	  (db:test-get-rundir testdat)) ;; )
	 (testconfig    (read-config (conc test-run-dir "/testconfig") #f #t environ-patt: "pre-launch-env-vars"))
	 (ezstepslst    (hash-table-ref/default testconfig "ezsteps" '()))
	 (run-mutex     (make-mutex))
	 (rollup-status 0)
	 (exit-info     (vector #t #t #t))
	 (test-id       (db:test-get-id testdat))
	 (run-id        (db:test-get-run_id testdat))
	 (test-name     (db:test-get-testname testdat))
	 (kill-job      #f)) ;; for future use (on re-factoring with launch.scm code
    (let loop ((count 5))
      (if (file-exists? test-run-dir)
	  (push-directory test-run-dir)
	  (if (> count 0)
	      (begin
		(debug:print 0 "WARNING: ezsteps attempting to run but test run directory " test-run-dir " is not there. Waiting and trying again " count " more times")
		(sleep 3)
		(loop (- count 1))))))
    (debug:print-info 0 "Running in directory " test-run-dir)
    (if (not (file-exists? ".ezsteps"))(create-directory ".ezsteps"))
    ;; if ezsteps was defined then we are sure to have at least one step but check anyway
    (if (not (> (length ezstepslst) 0))
	(message-window "ERROR: You can only re-run steps defined via ezsteps")
	(begin
	  (let loop ((ezstep   (car ezstepslst))
		     (tal      (cdr ezstepslst))
		     (prevstep #f)
		     (runflag  #f)) ;; flag used to skip steps when not starting at the beginning
	    (if (vector-ref exit-info 1)
		(let* ((stepname  (car ezstep))  ;; do stuff to run the step
		       (stepinfo  (cadr ezstep))
		       (stepparts (string-match (regexp "^(\\{([^\\}]*)\\}\\s*|)(.*)$") stepinfo))
		       (stepparms (list-ref stepparts 2)) ;; for future use, {VAR=1,2,3}, run step for each 
		       (stepcmd   (list-ref stepparts 3))
		       (script    "") ; "#!/bin/bash\n") ;; yep, we depend on bin/bash FIXME!!!
		       (logpro-used #f))

		  ;; Skip steps until hit start-step-name
		  ;;
		  (if (and start-step-name
			   (not runflag))
		      (if (equal? stepname start-step-name)
			  (set! runflag #t) ;; and continue
			  (if (not (null? tal))
			      (loop (car tal)(cdr tal) stepname #f))))

		  (debug:print 4 "ezsteps:\n stepname: " stepname " stepinfo: " stepinfo " stepparts: " stepparts
			       " stepparms: " stepparms " stepcmd: " stepcmd)
		  
		  (if (file-exists? (conc stepname ".logpro"))(set! logpro-used #t))
		  
		  ;; call the command using mt_ezstep
		  (set! script (conc "mt_ezstep " stepname " " (if prevstep prevstep "-") " " stepcmd))
		  
		  (debug:print 4 "script: " script)
		  (rmt:teststep-set-status! run-id test-id stepname "start" "-" #f #f)
		  ;; now launch
		  (let ((pid (process-run script)))
		    (let processloop ((i 0))
		      (let-values (((pid-val exit-status exit-code)(process-wait pid #t)))
				  (mutex-lock! run-mutex)
				  (vector-set! exit-info 0 pid)
				  (vector-set! exit-info 1 exit-status)
				  (vector-set! exit-info 2 exit-code)
				  (mutex-unlock! run-mutex)
				  (if (eq? pid-val 0)
				      (begin
					(thread-sleep! 1)
					(processloop (+ i 1))))
				  ))
		    (let ((exinfo (vector-ref exit-info 2))
			  (logfna (if logpro-used (conc stepname ".html") "")))
		      (rmt:teststep-set-status! run-id test-id stepname "end" exinfo #f logfna))
		    (if logpro-used
			(rmt:test-set-log! test-id (conc stepname ".html")))
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
		      (if (not run-one) ;; if we got here we completed the step, if run-one is true, stop
			  (loop (car tal) (cdr tal) stepname runflag))))
		(debug:print 4 "WARNING: a prior step failed, stopping at " ezstep)))
	  
	  ;; Once done with step/steps update the test record
	  ;;
	  (let* ((item-path (db:test-get-item-path testdat)) ;; (item-list->path itemdat))
		 (testinfo  (rmt:get-testinfo-by-id run-id test-id))) ;; refresh the testdat, call it iteminfo in case need prev/curr
	    ;; Am I completed?
	    (if (equal? (db:test-get-state testinfo) "RUNNING") ;; (not (equal? (db:test-get-state testinfo) "COMPLETED"))
		(let ((new-state  (if kill-job "KILLED" "COMPLETED") ;; (if (eq? (vector-ref exit-info 2) 0) ;; exited with "good" status
				  ;; "COMPLETED"
				  ;; (db:test-get-state testinfo)))   ;; else preseve the state as set within the test
				  )
		      (new-status (cond
				   ((not (vector-ref exit-info 1)) "FAIL") ;; job failed to run
				   ((eq? rollup-status 0)
				    ;; if the current status is AUTO the defer to the calculated value (i.e. leave this AUTO)
				    (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO" "PASS"))
				   ((eq? rollup-status 1) "FAIL")
				   ((eq? rollup-status 2)
				    ;; if the current status is AUTO the defer to the calculated value but qualify (i.e. make this AUTO-WARN)
				    (if (equal? (db:test-get-status testinfo) "AUTO") "AUTO-WARN" "WARN"))
				   (else "FAIL")))) ;; (db:test-get-status testinfo)))
		  (debug:print-info 2 "Test NOT logged as COMPLETED, (state=" (db:test-get-state testinfo) "), updating result, rollup-status is " rollup-status)
		  (tests:test-set-status! test-id 
					  new-state
					  new-status
					  (args:get-arg "-m") #f)
		  ;; need to update the top test record if PASS or FAIL and this is a subtest
		  (if (not (equal? item-path ""))
		      (cdb:roll-up-pass-fail-counts *runremote* run-id test-name item-path new-status))))
	    ;; for automated creation of the rollup html file this is a good place...
	    (if (not (equal? item-path ""))
		(tests:summarize-items #f run-id test-id test-name #f)) ;; don't force - just update if no
	    )))
    (pop-directory)
    rollup-status))
