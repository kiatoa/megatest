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
		       (set! testdat      (db:get-test-data-by-id db test-id))
		       (set! teststeps    (db:get-steps-for-test db test-id))
		       (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
		       (set! rundir       (db:test-get-rundir testdat))
		       (set! testfullname (db:test-get-fullname testdat))))
	 (widgets      (make-hash-table))
	 (self         #f)
	 (store-label  (lambda (name lbl cmd)
			 (hash-table-set! widgets name (lambda ()
							 (iup:attribute-set! lbl "TITLE" (cmd))))
			 lbl))
	 (store-button (lambda (name btn cmd)
			 (hash-table-set! widgets name (lambda (cmd)
							 (iup:attribute-set! btn "TITLE" (cmd))))
			 btn))
	 )
    (cond
     ((not testdat)(begin (print "ERROR: bad test info for " test-id)(exit 1)))
     ((not rundat)(begin (print "ERROR: found test info but there is a problem with the run info for " run-id)(exit 1)))
     (else
      ;;  (test-set-status! db run-id test-name state status itemdat)
      (set! self 
	    (iup:dialog
	     #:title testfullname
	     (iup:hbox  #:expand "BOTH" ;; Need a full height box for all the test steps
	      (iup:vbox #:expand "BOTH"
	       (iup:hbox  #:expand "BOTH"
		(iup:frame #:title "Run Info" #:expand "VERTICAL"
			   (iup:hbox #:expand "BOTH"
			    (apply iup:vbox #:expand "BOTH"
				   (append (map (lambda (keyval)
						  (iup:label (conc (car keyval) " ") #:expand "HORIZONTAL"))
						keydat)
					   (list (iup:label "runname "))))
			    (apply iup:vbox
				   (append (map (lambda (keyval)
						  (iup:label (cadr keyval) #:expand "HORIZONTAL"))
						keydat)
					   (list (iup:label runname))))))
		(iup:frame #:title "Test Info" #:expand "VERTICAL"
			   (iup:hbox #:expand "BOTH"
			    (apply iup:vbox #:expand "BOTH"
				   (map (lambda (val)
					  (iup:label val #:expand "HORIZONTAL"))
					(list "Testname: "
					      "Item path: "
					      "Current state: "
					      "Current status: "
					      "Test comment: ")))
			    (apply iup:vbox  #:expand "BOTH"
				   (list 
				    (iup:label (db:test-get-testname  testdat) #:expand "BOTH")
				    (iup:label (db:test-get-item-path testdat) #:expand "BOTH")
				    (store-label "teststate" 
						 (iup:label "TestState" #:expand "BOTH")
						 (lambda ()
						   (db:test-get-state testdat)))
				    (store-label "teststatus"
						 (iup:label "TestStatus" #:expand "BOTH")
						 (lambda ()
						   (db:test-get-status    testdat)))
				    (store-label "testcomment"
						 (iup:label "TestComment" #:expand "BOTH")
						 (lambda ()
						   (db:test-get-comment   testdat))))))))))))
      (iup:show self)
      ;; Now start keeping the gui updated from the db
      (let loop ((i 0))
	(thread-sleep! 0.1)
	(refreshdat) ;; update from the db here
	(thread-suspend! other-thread)
	;; update the gui elements here
	(for-each 
	 (lambda (key)
	   (print "Updating " key)
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
