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
(define (examine-test db test-id) ;; run-id run-key origtest)
  (let* ((testdat   (db:get-test-data-by-id db test-id))
	 (run-id    (if testdat (db:test-get-run_id testdat) #f))
	 (rundat    (if testdat (db:get-run-info db run-id)))
	 (teststeps (if testdat (db:get-steps-for-test db test-id))))
    (cond
     ((not testdat)(begin (print "ERROR: bad test info for " test-id)(exit 1)))
     ((not rundat)(begin (print "ERROR: found test info but there is a problem with the run info for " run-id)(exit 1)))
     (else
      (let* ((widgets      (make-hash-table)) ;; put the widgets to update in this hashtable
	     (logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
	     (viewlog      (lambda (x)
			     (if (file-exists? logfile)
				 (system (conc "firefox " logfile "&"))
				 (message-window (conc "File " logfile " not found")))))
	     (xterm        (lambda (x)
			     (if (directory-exists? rundir)
				 (let ((shell (if (get-environment-variable "SHELL") 
						  (conc "-e " (get-environment-variable "SHELL"))
						  "")))
				   (system (conc "cd " rundir 
						 ";xterm -T \"" (string-translate testfullname "()" "  ") "\" " shell "&")))
				 (message-window  (conc "Directory " rundir " not found")))))
	     (self         #f))
	
	  (hash-table-set! widgets "testdat" testdat)
	  (hash-table-set! widgets "rundat"  rundat)
	  
	  ;;  (test-set-status! db run-id test-name state status itemdat)
	  (set! self 
		(iup:dialog
		 #:title "testfullname"
		 (iup:hbox ;; Need a full height box for all the test steps
		  (iup:vbox
		   (iup:hbox 
		    (iup:frame (iup:label "BLAH (was run-key)")))))))
	  (iup:show self)
	  )))))

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
