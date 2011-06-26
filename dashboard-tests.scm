;;======================================================================
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;;
;;======================================================================
(define (examine-test db test-id other-thread) ;; run-id run-key origtest)
  (let* ((testdat       (db:get-test-data-by-id db test-id))
	 (run-id        (if testdat (db:test-get-run_id testdat) #f))
	 (keydat        (if testdat (keys:get-key-val-pairs db run-id) #f))
	 (rundat        (if testdat (db:get-run-info db run-id) #f))
	 (runname       (if testdat (db:get-value-by-header (db:get-row rundat)
							    (db:get-header rundat)
							    "runname") #f))
	 (teststeps     (if testdat (db:get-steps-for-test db test-id) #f))
	 (logfile       "/this/dir/better/not/exist")
	 (rundir        logfile)
	 (testfullname  (if testdat (db:test-get-fullname testdat) "Gathering data ..."))
	 (viewlog    (lambda (x)
		       (if (file-exists? logfile)
			   (system (conc "firefox " logfile "&"))
			   (message-window (conc "File " logfile " not found")))))
	 (xterm      (lambda (x)
		       (if (directory-exists? rundir)
			   (let ((shell (if (get-environment-variable "SHELL") 
					    (conc "-e " (get-environment-variable "SHELL"))
					    "")))
			     (system (conc "cd " rundir 
					   ";xterm -T \"" (string-translate testfullname "()" "  ") "\" " shell "&")))
			   (message-window  (conc "Directory " rundir " not found")))))
	 (refreshdat (lambda ()
		       (let ((newtestdat (db:get-test-data-by-id db test-id)))
			 (if newtestdat 
			     (begin
			       (set! testdat newtestdat)
			       (set! teststeps    (db:get-steps-for-test db test-id))
			       (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
			       (set! rundir       (db:test-get-rundir testdat))
			       (set! testfullname (db:test-get-fullname testdat)))
			     (begin
			       (sqlite3:finalize! db)
			       (exit 0))))))
	 (widgets      (make-hash-table))
	 (self         #f)
	 (store-label  (lambda (name lbl cmd)
			 (hash-table-set! widgets name (lambda ()
							 (iup:attribute-set! lbl "TITLE" (cmd))))
			 lbl))
	 (store-button store-label))
    (cond
     ((not testdat)(begin (print "ERROR: bad test info for " test-id)(exit 1)))
     ((not rundat)(begin (print "ERROR: found test info but there is a problem with the run info for " run-id)(exit 1)))
     (else
      ;;  (test-set-status! db run-id test-name state status itemdat)
      (set! self 
	    (iup:dialog
	     #:title testfullname
	     (iup:hbox  #:expand "BOTH" ;; Need a full height box for all the test steps
	      (iup:vbox #:expand "VERTICAL"
               ;; The run and test info
	       (iup:hbox  #:expand "BOTH"
		(iup:frame #:title "Megatest Run Info" #:expand "VERTICAL"
			   (iup:hbox #:expand "VERTICAL"
			    (apply iup:vbox #:expand "VERTICAL"
				   (append (map (lambda (keyval)
						  (iup:label (conc (car keyval) " ") #:expand "HORIZONTAL"))
						keydat)
					   (list (iup:label "runname "))))
			    (apply iup:vbox
				   (append (map (lambda (keyval)
						  (iup:label (cadr keyval) #:expand "HORIZONTAL"))
						keydat)
					   (list (iup:label runname)(iup:label "" #:expand "VERTICAL"))))))
		(iup:frame #:title "Test Info" #:expand "VERTICAL"
			   (iup:hbox #:expand "VERTICAL"
			    (apply iup:vbox #:expand "VERTICAL"
				   (append (map (lambda (val)
						  (iup:label val #:expand "HORIZONTAL"))
						(list "Testname: "
						      "Item path: "
						      "Current state: "
						      "Current status: "
						      "Test comment: "))
					   (list (iup:label "" #:expand "VERTICAL"))))
			    (apply iup:vbox  #:expand "BOTH"
				   (list 
				    (iup:label (db:test-get-testname  testdat) #:expand "HORIZONTAL")
				    (iup:label (db:test-get-item-path testdat) #:expand "HORIZONTAL")
				    (store-label "teststate" 
						 (iup:label (db:test-get-state testdat) #:expand "HORIZONTAL")
						 (lambda ()
						   (db:test-get-state testdat)))
				    (let ((lbl   (iup:button (db:test-get-status testdat) #:expand "HORIZONTAL"))
					  (color (get-color-for-state-status (db:test-get-state testdat)
									     (db:test-get-status testdat))))
				      (hash-table-set! widgets "teststatus"
						       (lambda ()
							 (iup:attribute-set! lbl "BGCOLOR" color)
							 (db:test-get-status testdat)))
				      lbl)
				    (store-label "testcomment"
						 (iup:label "TestComment                             "
							    #:expand "HORIZONTAL")
						 (lambda ()
						   (db:test-get-comment testdat))))))))
	       ;; The run host info
	       (iup:frame #:title "Remote host and Test Run Info" #:expand "HORIZONTAL"
	        (iup:hbox #:expand "HORIZONTAL"
                 (apply iup:vbox #:expand "VERTICAL" ;; The heading labels
			(append (map (lambda (val)
				       (iup:label val #:expand "HORIZONTAL"))
				     (list "Hostname: "
					   "Uname -a: "
					   "Disk free: "
					   "CPU Load: "
					   "Run duration: "
					   "Logfile: "))
				(iup:label "" #:expand "VERTICAL")))
		 (apply iup:vbox #:expand "VERTICAL"
			(list
			 ;; NOTE: Yes, the host can change!
			 (store-label "HostName"
				      (iup:label (db:test-get-host testdat) #:expand "HORIZONTAL")
				      (lambda ()(db:test-get-host testdat)))
			 (store-label "Uname"
				      (iup:label "                                                   " #:expand "HORIZONTAL")
				      (lambda ()(db:test-get-uname testdat)))
			 (store-label "DiskFree"
				      (iup:label (conc (db:test-get-diskfree testdat)) #:expand "HORIZONTAL")
				      (lambda ()(conc (db:test-get-diskfree testdat))))
			 (store-label "CPULoad"
				      (iup:label (conc (db:test-get-cpuload testdat)) #:expand "HORIZONTAL")
				      (lambda ()(conc (db:test-get-cpuload testdat))))
			 (store-label "RunDuration"
				      (iup:label (conc (db:test-get-run_duration testdat)) #:expand "HORIZONTAL")
				      (lambda ()(conc (db:test-get-run_duration testdat))))
			 (store-label "CPULoad"
				      (iup:label (conc (db:test-get-final_logf testdat)) #:expand "HORIZONTAL")
				      (lambda ()(conc (db:test-get-final_logf testdat))))))))
	       ))))
      (iup:show self)
      ;; Now start keeping the gui updated from the db
      (let loop ((i 0))
	(thread-sleep! 0.1)
	(refreshdat) ;; update from the db here
	(thread-suspend! other-thread)
	;; update the gui elements here
	(for-each 
	 (lambda (key)
	   ;; (print "Updating " key)
	   ((hash-table-ref widgets key)))
	 (hash-table-keys widgets))
	(thread-resume! other-thread)
	(loop i))))))

;;
;;		    (iup:frame (iup:label (conc "TESTNAME:\n" testfullname) #:expand "YES")))
;;		   (iup:frame #:title "Actions" #:expand "YES"
;;			      (iup:hbox ;; the actions box
;;			       (iup:button "View Log"    #:action viewlog  #:expand "YES")
;;			       (iup:button "Start Xterm" #:action xterm  #:expand "YES")))
;;		   (iup:frame #:title "Set fields"
;;			      (iup:vbox
;;			       (iup:hbox 
;;				(iup:vbox ;; the state
;;				 (iup:label "STATE:" #:size "30x")
;;				 (let ((lb (iup:listbox #:action (lambda (val a b c)
;;								   ;; (print val " a: " a " b: " b " c: " c)
;;								   (set! newstate a))
;;							#:editbox "YES"
;;							#:expand "YES")))
;;				   (iuplistbox-fill-list lb
;;							 (list "COMPLETED" "NOT_STARTED" "RUNNING" "REMOTEHOSTSTART" "KILLED" "KILLREQ")
;;							 currstate)
;;				   lb))
;;				(iup:vbox ;; the status
;;				 (iup:label "STATUS:" #:size "30x")
;;				 (let ((lb (iup:listbox #:action (lambda (val a b c)
;;								   (set! newstatus a))
;;							#:editbox "YES"
;;							#:value currstatus
;;							#:expand "YES")))
;;				   (iuplistbox-fill-list lb
;;							 (list "PASS" "WARN" "FAIL" "CHECK" "n/a")
;;							 currstatus)
;;				   lb)))
;;			       (iup:hbox (iup:label "Comment:")
;;					 (iup:textbox #:action (lambda (val a b)
;;								 (set! currcomment b))
;;						      #:value currcomment 
;;						      #:expand "YES"))
;;			       (iup:button "Apply"
;;					   #:expand "YES"
;;					   #:action (lambda (x)
;;						      (test-set-status! *db* run-id testname newstate newstatus itempath currcomment)))
;;			       (iup:hbox (iup:button "Apply and close"
;;						     #:expand "YES"
;;						     #:action (lambda (x)
;;								(hash-table-delete! *examine-test-dat* testkey)
;;								(test-set-status! *db* run-id testname newstate newstatus itempath currcomment)
;;								(iup:destroy! self)))
;;					 (iup:button "Cancel and close"
;;						     #:expand "YES"
;;						     #:action (lambda (x)
;;								(hash-table-delete! *examine-test-dat* testkey)
;;								(iup:destroy! self))))
;;			       )))
;;		  (iup:hbox ;; the test steps are tracked here
;;		   (let ((stepsdat (iup:label "Test steps ........................................." #:expand "YES")))
;;		     (hash-table-set! widgets "Test Steps" stepsdat)
;;		     stepsdat)
;;		   ))))
