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
(define (examine-test db test-id mx1) ;; run-id run-key origtest)
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
			       (mutex-lock! mx1)
			       (set! testdat newtestdat)
			       (set! teststeps    (db:get-steps-for-test db test-id))
			       (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
			       (set! rundir       (db:test-get-rundir testdat))
			       (set! testfullname (db:test-get-fullname testdat))
			       (mutex-unlock! mx1))
			     (begin
			       (sqlite3:finalize! db)
			       (exit 0))))))
	 (widgets      (make-hash-table))
	 (self         #f)
	 (store-label  (lambda (name lbl cmd)
			 (hash-table-set! widgets name 
					  (lambda ()
					    (let ((newval (cmd))
						  (oldval (iup:attribute lbl "TITLE")))
					      (if (not (equal? newval oldval))
						  (begin
						    (mutex-lock! mx1)
						    (iup:attribute-set! lbl "TITLE" (cmd))
						    (mutex-unlock! mx1))))))
			 lbl))
	 (store-button store-label)
	 ;; Place for new values from the gui
	 (newstatus     #f)
	 (newstate      #f)
	 (newcomment    #f)
	 
	 )
    (cond
     ((not testdat)(begin (print "ERROR: bad test info for " test-id)(exit 1)))
     ((not rundat)(begin (print "ERROR: found test info but there is a problem with the run info for " run-id)(exit 1)))
     (else
      ;;  (test-set-status! db run-id test-name state status itemdat)
      (set! self ; 
	    (iup:dialog #:close_cb (lambda (a)(exit)) ; #:expand "YES"
	     #:title testfullname
	     (iup:hbox ; #:expand "YES" ;; Need a full height box for all the test steps
	      (iup:vbox ; #:expand "YES"
               ;; The run and test info
	       (iup:hbox  ; #:expand "YES"
		(iup:frame #:title "Megatest Run Info" ; #:expand "YES"
			   (iup:hbox ; #:expand "YES"
			    (apply iup:vbox ; #:expand "YES"
				   (append (map (lambda (keyval)
						  (iup:label (conc (car keyval) " ") ; #:expand "HORIZONTAL"
							     ))
						keydat)
					   (list (iup:label "runname "))))
			    (apply iup:vbox
				   (append (map (lambda (keyval)
						  (iup:label (cadr keyval) #:expand "HORIZONTAL"))
						keydat)
					   (list (iup:label runname)(iup:label "" #:expand "VERTICAL"))))))
		(iup:frame #:title "Test Info" ; #:expand "YES"
			   (iup:hbox ; #:expand "YES"
			    (apply iup:vbox ; #:expand "YES"
				   (append (map (lambda (val)
						  (iup:label val ; #:expand "HORIZONTAL"
							     ))
						(list "Testname: "
						      "Item path: "
						      "Current state: "
						      "Current status: "
						      "Test comment: "
						      "Test id: "))
					   (list (iup:label "" #:expand "VERTICAL"))))
			    (apply iup:vbox  ; #:expand "YES"
				   (list 
				    (iup:label (db:test-get-testname  testdat) #:expand "HORIZONTAL")
				    (iup:label (db:test-get-item-path testdat) #:expand "HORIZONTAL")
				    (store-label "teststate" 
						 (iup:label (db:test-get-state testdat) #:expand "HORIZONTAL")
						 (lambda ()
						   (db:test-get-state testdat)))
				    (let ((lbl   (iup:label (db:test-get-status testdat) #:expand "HORIZONTAL")))
				      (hash-table-set! widgets "teststatus"
						       (lambda ()
							 (let ((newstatus (db:test-get-status testdat))
							       (oldstatus (iup:attribute lbl "TITLE")))
							   (if (not (equal? oldstatus newstatus))
							       (begin
								 (iup:attribute-set! lbl "FGCOLOR" (get-color-for-state-status (db:test-get-state testdat)
															       (db:test-get-status testdat)))
								 (iup:attribute-set! lbl "TITLE" (db:test-get-status testdat)))))))
				      lbl)
				    (store-label "testcomment"
						 (iup:label "TestComment                             "
							    #:expand "HORIZONTAL")
						 (lambda ()
						   (db:test-get-comment testdat)))
				    (store-label "testid"
						 (iup:label "TestId                             "
							    #:expand "HORIZONTAL")
						 (lambda ()
						   (db:test-get-id testdat))))))))
	       ;; The run host info
	       (iup:frame #:title "Remote host and Test Run Info" ; #:expand "YES"
	        (iup:hbox ; #:expand "YES"
                 (apply iup:vbox ; #:expand "YES" ;; The heading labels
			(append (map (lambda (val)
				       (iup:label val ; #:expand "HORIZONTAL"
						  ))
				     (list "Hostname: "
					   "Uname -a: "
					   "Disk free: "
					   "CPU Load: "
					   "Run duration: "
					   "Logfile: "))
				(iup:label "" #:expand "VERTICAL")))
		 (apply iup:vbox ; #:expand "YES"
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
	       ;; The controls
	       (iup:frame #:title "Actions" ; #:expand "HORIZONTAL"
			  (iup:hbox ; #:expand "HORIZONTAL" ;; the actions box
			   (iup:button "View Log"    #:action viewlog #:expand "YES"
				       )
			   (iup:button "Start Xterm" #:action xterm  #:expand "YES")))
	       (iup:frame #:title "Set fields"
			  (iup:vbox
			   ;(iup:hbox ; #:expand "HORIZONTAL"
			   (iup:hbox (iup:label "Comment:")
				     (iup:textbox #:action (lambda (val a b)
							     (set! newcomment b))
						  #:value (db:test-get-comment testdat)
						  #:expand "YES"
						  ))
			   (iup:hbox 
			    (iup:vbox ; for the state and status controls
			     (iup:hbox ; #:expand "HORIZONTAL" ;; the state
			      (iup:label "STATE:" ; #:size "30x" ; #:expand "HORIZONTAL"
					 )
			      (let ((lb (iup:listbox #:action (lambda (val a b c)
								;; (print val " a: " a " b: " b " c: " c)
								(set! newstate a))
						     ;; #:editbox "YES"
						     #:dropdown "YES"
						     ;#:expand "HORIZONTAL"
						     )))
			       (iuplistbox-fill-list lb
						     (list "Set state" "COMPLETED" "NOT_STARTED" "RUNNING" "REMOTEHOSTSTART" "KILLED" "KILLREQ")
						     "Set state" ) ; (db:test-get-state testdat))
			       lb))
			     (iup:hbox ; #:expand "HORIZONTAL" ;; the status
			      (iup:label "STATUS:" ; #:size "30x" #:expand "HORIZONTAL"
					 )
			      (let ((lb (iup:listbox #:action (lambda (val a b c)
								(set! newstatus a))
						     ;; #:editbox "YES"
						     ;; #:value currstatus
						     #:dropdown "YES"
						     ;#:expand "HORIZONTAL"
						     )))
				(iuplistbox-fill-list lb
						      (list "Set status" "PASS" "WARN" "FAIL" "CHECK" "n/a")
						      "Set status" ) ; (db:test-get-status testdat))
				lb)))
			    (iup:vbox
			     (iup:button "Apply"
					 #:expand "YES"
					 #:action (lambda (x)
						    (db:test-set-state-status-by-id *db* test-id newstate newstatus newcomment)
						    ))
			     (iup:hbox;  #:expand "YES"
			      (iup:vbox
			       (iup:button "Apply and close"
					; #:expand "YES"
					   #:action (lambda (x)
						      (db:test-set-state-status-by-id *db* test-id newstate newstatus newcomment)
						      (exit))))
			      (iup:vbox
			       (iup:button "Cancel and close"
					; #:expand "YES"
					   #:action (lambda (x)
						      (exit)))))))
			     ))))))
      (iup:show self)
      ;; Now start keeping the gui updated from the db
      (let loop ((i 0))
	(thread-sleep! 0.1)
	(refreshdat) ;; update from the db here
	;(thread-suspend! other-thread)
	;; update the gui elements here
	(for-each 
	 (lambda (key)
	   ;; (print "Updating " key)
	   ((hash-table-ref widgets key)))
	 (hash-table-keys widgets))
	;(thread-resume! other-thread)
	; (iup:refresh self)
	(iup:main-loop-flush)
	(if *exit-started*
	    (set! *exit-started* 'ok)
	    (loop i)))))))

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
