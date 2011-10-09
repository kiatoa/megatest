
;; Copyright 2006-2011, Matthew Welland.
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

(use regex regex-case base64 sqlite3)
(import (prefix base64 base64:))
(import (prefix sqlite3 sqlite3:))

(declare (unit launch))
(declare (uses common))
(declare (uses configf))
(declare (uses db))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")

(define (launch:execute encoded-cmd)
  (let* ((cmdinfo   (read (open-input-string (base64:base64-decode encoded-cmd)))))
    (setenv "MT_CMDINFO" encoded-cmd)
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
	  (if mt-bindir-path (setenv "PATH" (conc (getenv "PATH") ":" mt-bindir-path)))
	  
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
	  (test-set-status! db run-id test-name "REMOTEHOSTSTART" "n/a" itemdat (args:get-arg "-m") #f)
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
				     (let* ((db       (open-db))
					    (cpuload  (get-cpu-load))
					    (diskfree (get-df (current-directory)))
					    (tmpfree  (get-df "/tmp")))
				       (if (not cpuload)  (begin (debug:print 0 "WARNING: CPULOAD not found.")  (set! cpuload "n/a")))
				       (if (not diskfree) (begin (debug:print 0 "WARNING: DISKFREE not found.") (set! diskfree "n/a")))
				       (set! kill-job? (test-get-kill-request db run-id test-name itemdat))
				       (test-update-meta-info db run-id test-name itemdat minutes cpuload diskfree tmpfree)
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
						       (system (conc "kill -9 " pid))))
						   (begin
						     (debug:print 0 "WARNING: Request received to kill job but problem with process, attempting to kill manager process")
						     (test-set-status! db run-id test-name "KILLED"  "FAIL"
								       itemdat (args:get-arg "-m") #f)
						     (sqlite3:finalize! db)
						     (exit 1))))
					     (set! kill-tries (+ 1 kill-tries))
					     (mutex-unlock! m)))
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
					  "FAIL") itemdat (args:get-arg "-m") #f)))
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
		(exit 4)))))))

(define (setup-for-run)
  (set! *configinfo* (find-and-read-config (if (args:get-arg "-config")(args:get-arg "-config") "megatest.config")))
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
		  (freespc    (if (directory? dirpath)
				  (get-df dirpath)
				  (begin
				    (debug:print 0 "WARNING: path " dirpath " in [disks] section not valid")
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

(define (create-work-area db run-id test-path disk-path testname itemdat)
  (let* ((run-info (db:get-run-info db run-id))
	 (item-path (let ((ip (item-list->path itemdat)))
		      (if (equal? ip "") "" (conc "/" ip))))
	 (runname  (db:get-value-by-header (db:get-row run-info)
					   (db:get-header run-info)
					   "runname"))
	 (key-vals (get-key-vals db run-id))
	 (key-str  (string-intersperse key-vals "/"))
	 (dfullp   (conc disk-path "/" key-str "/" runname "/" testname
			 item-path))
	 (toptest-path (conc disk-path "/" key-str "/" runname "/" testname))
	 (runsdir  (config-lookup *configdat* "setup" "runsdir"))
	 (lnkpath  (conc (if runsdir runsdir (conc *toppath* "/runs"))
			 "/" key-str "/" runname item-path)))
    ;; since this is an iterated test this is as good a place as any to
    ;; update the toptest record with its location rundir
    (if (not (equal? item-path ""))
	(db:test-set-rundir! db run-id testname "" toptest-path))
    (debug:print 2 "Setting up test run area")
    (debug:print 2 " - creating run area in " dfullp)
    (system  (conc "mkdir -p " dfullp))
    (debug:print 2 " - creating link from " dfullp "/" testname " to " lnkpath)
    (system  (conc "mkdir -p " lnkpath))

;; I suspect this section was deleting test directories under some 
;; wierd sitations

;;    (if (file-exists? (conc lnkpath "/" testname))
;;	(system (conc "rm -f " lnkpath "/" testname)))
    (system  (conc "ln -sf " dfullp " " lnkpath "/" testname))
    (if (directory? dfullp)
	(begin
	  (let* ((cmd    (conc "rsync -av" (if (> *verbosity* 1) "" "q") " " test-path "/ " dfullp "/"))
		 (status (system cmd)))
	    (if (not (eq? status 0))
		(debug:print 2 "ERROR: problem with running \"" cmd "\"")))
	  (list dfullp toptest-path))
	(list #f #f))))

;; 1. look though disks list for disk with most space
;; 2. create run dir on disk, path name is meaningful
;; 3. create link from run dir to megatest runs area 
;; 4. remotely run the test on allocated host
;;    - could be ssh to host from hosts table (update regularly with load)
;;    - could be netbatch
;;      (launch-test db (cadr status) test-conf))
(define (launch-test db run-id test-conf keyvallst test-name test-path itemdat)
  (change-directory *toppath*)
  (let ((useshell   (config-lookup *configdat* "jobtools"     "useshell"))
	(launcher   (config-lookup *configdat* "jobtools"     "launcher"))
	(runscript  (config-lookup test-conf   "setup"        "runscript"))
	(diskspace  (config-lookup test-conf   "requirements" "diskspace"))
	(memory     (config-lookup test-conf   "requirements" "memory"))
	(hosts      (config-lookup *configdat* "jobtools"     "workhosts"))
	(remote-megatest (config-lookup *configdat* "setup" "executable"))
	(local-megatest  (car (argv)))
	;; (item-path  (item-list->path itemdat)) test-path is the full path including the item-path
	(work-area  #f)
	(toptest-work-area #f) ;; for iterated tests the top test contains data relevant for all
	(diskpath   #f)
	(cmdparms   #f)
	(fullcmd    #f) ;; (define a (with-output-to-string (lambda ()(write x))))
	(mt-bindir-path #f))
    (if hosts (set! hosts (string-split hosts)))
    (if (not remote-megatest)(set! remote-megatest local-megatest)) ;; "megatest"))
    (set! mt-bindir-path (pathname-directory remote-megatest))
    (if launcher (set! launcher (string-split launcher)))
    ;; set up the run work area for this test
    (set! diskpath (get-best-disk *configdat*))
    (if diskpath
	(let ((dat  (create-work-area db run-id test-path diskpath test-name itemdat)))
	  (set! work-area (car dat))
	  (set! toptest-work-area (cadr dat)))
	(begin
	  (set! work-area test-path)
	  (debug:print 0 "WARNING: No disk work area specified - running in the test directory")))
    (set! cmdparms (base64:base64-encode (with-output-to-string
				    (lambda () ;; (list 'hosts     hosts)
				      (write (list (list 'testpath  test-path)
						   (list 'work-area work-area)
						   (list 'test-name test-name) 
						   (list 'runscript runscript) 
						   (list 'run-id    run-id   )
						   (list 'itemdat   itemdat  )
						   (list 'megatest  remote-megatest)
						   (list 'env-ovrd  (hash-table-ref/default *configdat* "env-override" '()))
						   (list 'runname   (args:get-arg ":runname"))
						   (list 'mt-bindir-path mt-bindir-path))))))) ;; (string-intersperse keyvallst " "))))
    ;; clean out step records from previous run if they exist
    (db:delete-test-step-records db run-id test-name itemdat)
    (change-directory work-area) ;; so that log files from the launch process don't clutter the test dir
    (cond
     ((and launcher hosts) ;; must be using ssh hostname
      (set! fullcmd (append launcher (car hosts)(list remote-megatest "-execute" cmdparms))))
     (launcher
      (set! fullcmd (append launcher (list remote-megatest "-execute" cmdparms))))
     (else
      (set! fullcmd (list remote-megatest "-execute" cmdparms))))
    (if (args:get-arg "-xterm")(set! fullcmd (append fullcmd (list "-xterm"))))
    (debug:print 1 "Launching megatest for test " test-name " in " work-area" ...")
    (test-set-status! db run-id test-name "LAUNCHED" "n/a" itemdat #f #f) ;; (if launch-results launch-results "FAILED"))
    ;; set 
    ;; set pre-launch-env-vars before launching, keep the vars in prevvals and put the envionment back when done
    (debug:print 4 "fullcmd: " fullcmd)
    (let* ((commonprevvals (alist->env-vars
			    (hash-table-ref/default *configdat* "env-override" '())))
	   (testprevvals   (alist->env-vars
			    (hash-table-ref/default test-conf "pre-launch-env-overrides" '())))
	   (miscprevvals   (alist->env-vars ;; consolidate this code with the code in megatest.scm for "-execute"
			    (append (list (list "MT_TEST_NAME" test-name)
					  (list "MT_ITEM_INFO" (conc itemdat)) 
					  (list "MT_RUNNAME"   (args:get-arg ":runname")))
				    itemdat)))
	   (launch-results (apply cmd-run-proc-each-line
				  (if useshell
				      (string-intersperse fullcmd " ")
				      (car fullcmd))
				  print
				  (if useshell
				      '()
				      (cdr fullcmd))))) ;;  launcher fullcmd)));; (apply cmd-run-proc-each-line launcher print fullcmd))) ;; (cmd-run->list fullcmd))
      (debug:print 2 "Launching completed, updating db")
      (debug:print 4 "Launch results: " launch-results)
      (if (not launch-results)
	  (begin
	    (print "ERROR: Failed to run " (string-intersperse fullcmd " ") ", exiting now")
	    (sqlite3:finalize! db)
	    ;; good ole "exit" seems not to work
	    ;; (_exit 9)
	    ;; but this hack will work! Thanks go to Alan Post of the Chicken email list
	    ;; NB// Is this still needed? Should be safe to go back to "exit" now?
	    (process-signal (current-process-id) signal/kill)
	    ))
      (alist->env-vars miscprevvals)
      (alist->env-vars testprevvals)
      (alist->env-vars commonprevvals)
      launch-results)))

