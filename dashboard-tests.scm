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
;; Test info panel
;;======================================================================
(define (test-info-panel testdat store-label widgets)
  (iup:frame 
   #:title "Test Info" ; #:expand "YES"
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
			      "Test id: "
			      "Value: "
			      "Expected value: "
			      "Tolerance: "
			      "Units: "))
		   (list (iup:label "" #:expand "VERTICAL"))))
    (apply iup:vbox  ; #:expand "YES"
	   (list 
	    (store-label "testname"
			 (iup:label (db:test-get-testname  testdat) #:expand "HORIZONTAL")
			 (lambda (testdat)(db:test-get-testname testdat)))
	    (store-label "item-path"
			 (iup:label (db:test-get-item-path testdat) #:expand "HORIZONTAL")
			 (lambda (testdat)(db:test-get-item-path testdat)))
	    (store-label "teststate" 
			 (iup:label (db:test-get-state testdat) #:expand "HORIZONTAL")
			 (lambda (testdat)
			   (db:test-get-state testdat)))
	    (let ((lbl   (iup:label (db:test-get-status testdat) #:expand "HORIZONTAL")))
	      (hash-table-set! widgets "teststatus"
			       (lambda (testdat)
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
			 (lambda (testdat)
			   (db:test-get-comment testdat)))
	    (store-label "testid"
			 (iup:label "TestId                             "
				    #:expand "HORIZONTAL")
			 (lambda (testdat)
			   (db:test-get-id testdat)))
;;	    (store-label "testvalue"
;;			 (iup:label "TestValue                          "
;;				    #:expand "HORIZONTAL")
;;			 (lambda (testdat)
;;			   (db:test-get-value testdat)))
;;	    (store-label "testexpectedvalue"
;;			 (iup:label "TestExpectedValue                  "
;;				    #:expand "HORIZONTAL")
;;			 (lambda (testdat)
;;			   (db:test-get-expected_value testdat)))
;;	    (store-label "testtol"
;;			 (iup:label "TestTol                             "
;;				    #:expand "HORIZONTAL")
;;			 (lambda (testdat)
;;			   (db:test-get-tol testdat)))
;;	    (store-label "testunits"
;;			 (iup:label "TestUnits                          "
;;				    #:expand "HORIZONTAL")
;;			 (lambda (testdat)
;;			   (db:test-get-units testdat)))
	    )))))

;;======================================================================
;; Test meta panel
;;======================================================================
(define (test-meta-panel testmeta store-meta)
  (iup:frame 
   #:title "Test Meta Data" ; #:expand "YES"
   (iup:hbox ; #:expand "YES"
    (apply iup:vbox ; #:expand "YES"
	   (append (map (lambda (val)
			  (iup:label val ; #:expand "HORIZONTAL"
				     ))
			(list "Author: "
			      "Owner: "
			      "Reviewed: "
			      "Tags: "
			      "Description: "
			      ))
		   (list (iup:label "" #:expand "VERTICAL"))))
    (apply iup:vbox  ; #:expand "YES"
	   (list 
	    (store-meta "author"
			 (iup:label (db:testmeta-get-author testmeta) #:expand "HORIZONTAL")
			 (lambda (testmeta)(db:testmeta-get-author testmeta)))
	    (store-meta "owner"
			 (iup:label (db:testmeta-get-owner testmeta) #:expand "HORIZONTAL")
			 (lambda (testmeta)(db:testmeta-get-owner testmeta)))
	    (store-meta "reviewed" 
			 (iup:label (db:testmeta-get-reviewed testmeta) #:expand "HORIZONTAL")
			 (lambda (testmeta)(db:testmeta-get-reviewed testmeta)))
	    (store-meta "tags" 
			 (iup:label (db:testmeta-get-tags testmeta) #:expand "HORIZONTAL")
			 (lambda (testmeta)(db:testmeta-get-tags testmeta)))
	    (store-meta "description" 
			 (iup:label (db:testmeta-get-description testmeta) #:expand "HORIZONTAL")
			 (lambda (testmeta)(db:testmeta-get-description testmeta)))
	    )))))


;;======================================================================
;; Run info panel
;;======================================================================
(define (run-info-panel keydat testdat runname)
  (iup:frame 
   #:title "Megatest Run Info" ; #:expand "YES"
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
		   (list (iup:label runname)(iup:label "" #:expand "VERTICAL")))))))
  
;;======================================================================
;; Host info panel
;;======================================================================
(define (host-info-panel testdat store-label)
  (iup:frame
   #:title "Remote host and Test Run Info" ; #:expand "YES"
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
			 (lambda (testdat)(db:test-get-host testdat)))
	    (store-label "Uname"
			 (iup:label "                                                   " #:expand "HORIZONTAL")
			 (lambda (testdat)(db:test-get-uname testdat)))
	    (store-label "DiskFree"
			 (iup:label (conc (db:test-get-diskfree testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-diskfree testdat))))
	    (store-label "CPULoad"
			 (iup:label (conc (db:test-get-cpuload testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-cpuload testdat))))
	    (store-label "RunDuration"
			 (iup:label (conc (seconds->hr-min-sec (db:test-get-run_duration testdat))) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (seconds->hr-min-sec (db:test-get-run_duration testdat)))))
	    (store-label "CPULoad"
			 (iup:label (conc (db:test-get-final_logf testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-final_logf testdat)))))))))

;; use a global for setting the buttons colors
;;                           state status teststeps
(define *state-status* (vector #f #f #f))
(define (update-state-status-buttons testdat)
  (let* ((state  (db:test-get-state  testdat))
	 (status (db:test-get-status testdat))
	 (color  (get-color-for-state-status state status)))
    ((vector-ref *state-status* 0) state color)
    ((vector-ref *state-status* 1) status color)))

;;======================================================================
;; Set fields 
;;======================================================================
(define (set-fields-panel test-id testdat)
  (let ((newcomment #f)
	(newstatus  #f)
	(newstate   #f))
    (iup:frame
     #:title "Set fields"
     (iup:vbox
      (iup:hbox (iup:label "Comment:")
		(iup:textbox #:action (lambda (val a b)
					(db:test-set-state-status-by-id *db* test-id #f #f b)
					(set! newcomment b))
			     #:value (db:test-get-comment testdat)
			     #:expand "YES"))
      (apply iup:hbox
	     (iup:label "STATE:" #:size "30x")
	     (let* ((btns  (map (lambda (state)
				  (let ((btn (iup:button state
							 #:expand "YES" #:size "70x"
							 #:action (lambda (x)
								    (db:test-set-state-status-by-id *db* test-id state #f #f)
								    (db:test-set-state! testdat state)))))
				    btn))
				(list "COMPLETED" "NOT_STARTED" "RUNNING" "REMOTEHOSTSTART" "KILLED" "KILLREQ"))))
	       (vector-set! *state-status* 0
			    (lambda (state color)
			      (for-each 
			       (lambda (btn)
				 (let* ((name     (iup:attribute btn "TITLE"))
					(newcolor (if (equal? name state) color "192 192 192")))
				   (if (not (colors-similar? newcolor (iup:attribute btn "BGCOLOR")))
				       (iup:attribute-set! btn "BGCOLOR" newcolor))))
			       btns)))
	       btns))
      (apply iup:hbox
	     (iup:label "STATUS:" #:size "30x")
	     (let* ((btns  (map (lambda (status)
				  (let ((btn (iup:button status
							 #:expand "YES" #:size "70x"
							 #:action (lambda (x)
								    (db:test-set-state-status-by-id *db* test-id #f status #f)
								    (db:test-set-status! testdat status)))))
				    btn))
				(list  "PASS" "WARN" "FAIL" "CHECK" "n/a" "WAIVED"))))
	       (vector-set! *state-status* 1
			    (lambda (status color)
			      (for-each 
			       (lambda (btn)
				 (let* ((name     (iup:attribute btn "TITLE"))
					(newcolor (if (equal? name status) color "192 192 192")))
				   (if (not (colors-similar? newcolor (iup:attribute btn "BGCOLOR")))
				       (iup:attribute-set! btn "BGCOLOR" newcolor))))
			       btns)))
	       btns))))))


;;======================================================================
;;
;;======================================================================
(define (examine-test db test-id) ;; run-id run-key origtest)
  (let* ((testdat       (db:get-test-data-by-id db test-id))
	 (run-id        (if testdat (db:test-get-run_id testdat) #f))
	 (keydat        (if testdat (keys:get-key-val-pairs db run-id) #f))
	 (rundat        (if testdat (db:get-run-info db run-id) #f))
	 (runname       (if testdat (db:get-value-by-header (db:get-row rundat)
							    (db:get-header rundat)
							    "runname") #f))
	 ;(teststeps     (if testdat (db:get-steps-for-test db test-id) #f))
	 (logfile       "/this/dir/better/not/exist")
	 (rundir        logfile)
	 (testfullname  (if testdat (db:test-get-fullname testdat) "Gathering data ..."))
	 (testname      (if testdat (db:test-get-testname testdat) "n/a"))
	 (testmeta      (if testdat 
			    (let ((tm (db:testmeta-get-record db testname)))
			      (if tm tm (make-db:testmeta)))
			    (make-db:testmeta)))
	 (viewlog    (lambda (x)
		       (if (file-exists? logfile)
			   ;(system (conc "firefox " logfile "&"))
			   (iup:send-url logfile)
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
			       ;(mutex-lock! mx1)
			       (set! testdat newtestdat)
			       (set! teststeps    (db:get-steps-for-test db test-id))
			       (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
			       (set! rundir       (db:test-get-rundir testdat))
			       (set! testfullname (db:test-get-fullname testdat))
			       ;(mutex-unlock! mx1)
			       )
			     (begin
			       (db:test-set-testname! testdat "DEAD OR DELETED TEST"))))))
	 (widgets      (make-hash-table))
	 (meta-widgets (make-hash-table))
	 (self         #f)
	 (store-label  (lambda (name lbl cmd)
			 (hash-table-set! widgets name 
					  (lambda (testdat)
					    (let ((newval (cmd testdat))
						  (oldval (iup:attribute lbl "TITLE")))
					      (if (not (equal? newval oldval))
						  (begin
						    ;(mutex-lock! mx1)
						    (iup:attribute-set! lbl "TITLE" newval)
						    ;(mutex-unlock! mx1)
						    )))))
			 lbl))
	 (store-meta  (lambda (name lbl cmd)
			 (hash-table-set! meta-widgets name 
					  (lambda (testmeta)
					    (let ((newval (cmd testmeta))
						  (oldval (iup:attribute lbl "TITLE")))
					      (if (not (equal? newval oldval))
						  (begin
						    ;(mutex-lock! mx1)
						    (iup:attribute-set! lbl "TITLE" newval)
						    ;(mutex-unlock! mx1)
						    )))))
			 lbl))
	 (store-button store-label))
    (cond
     ((not testdat)(begin (print "ERROR: bad test info for " test-id)(exit 1)))
     ((not rundat)(begin (print "ERROR: found test info but there is a problem with the run info for " run-id)(exit 1)))
     (else
      ;;  (test-set-status! db run-id test-name state status itemdat)
      (set! self ; 
	    (iup:dialog #:close_cb (lambda (a)(exit)) ; #:expand "YES"
	     #:title testfullname
	     (iup:vbox ; #:expand "YES"
               ;; The run and test info
	       (iup:hbox  ; #:expand "YES"
		(run-info-panel keydat testdat runname)
		(test-info-panel testdat store-label widgets)
		(test-meta-panel testmeta store-meta))
	       (host-info-panel testdat store-label)
	       ;; The controls
	       (iup:frame #:title "Actions" 
			  (iup:hbox 
			   (iup:button "View Log"    #:action viewlog #:size "120x")
			   (iup:button "Start Xterm" #:action xterm   #:size "120x")
			   (iup:button "Close"       #:action (lambda (x)(exit)) #:size "120x")))
	       (set-fields-panel test-id testdat)
	       (iup:frame 
		#:title "Test Steps"
		(let ((stepsdat (iup:label "Test steps ........................................." 
					   #:expand "YES" 
					   #:size "200x150"
					   #:alignment "ALEFT:ATOP")))
		  (hash-table-set! widgets "Test Steps" (lambda (testdat)
							  (let* ((currval (iup:attribute stepsdat "TITLE"))
								 (fmtstr  "~25a~10a~10a~15a~20a")
								 (comprsteps (db:get-steps-table db test-id))
								 (newval  (string-intersperse 
									   (append
									    (list 
									     (format #f fmtstr "Stepname" "Start" "End"    "Status" "Time")
									     (format #f fmtstr "========" "=====" "======" "======" "=========="))
									    (map (lambda (x)
										   ;; take advantage of the \n on time->string
										   (format #f fmtstr
											   (vector-ref x 0)
											   (let ((s (vector-ref x 1)))
											     (if (number? s)(seconds->time-string s) s))
											   (let ((s (vector-ref x 2)))
											     (if (number? s)(seconds->time-string s) s))
											   (vector-ref x 3)
											   (vector-ref x 4)))
										 (sort (hash-table-values comprsteps)
										       (lambda (a b)
											 (if (and (number? a)(number? b))
											     (< (vector-ref a 1)(vector-ref b 1))
											     #t)))))
									   "\n")))
							    (if (not (equal? currval newval))
								(iup:attribute-set! stepsdat "TITLE" newval)))))
		  stepsdat)))))
      (iup:show self)
      (iup:callback-set! *tim* "ACTION_CB"
			 (lambda (x)
			   ;; Now start keeping the gui updated from the db
			   (refreshdat) ;; update from the db here
					;(thread-suspend! other-thread)
			   ;; update the gui elements here
			   (for-each 
			    (lambda (key)
			      ;; (print "Updating " key)
			      ((hash-table-ref widgets key) testdat))
			    (hash-table-keys widgets))
			   (update-state-status-buttons testdat)
					; (iup:refresh self)
			   (if *exit-started*
			       (set! *exit-started* 'ok))))))))

