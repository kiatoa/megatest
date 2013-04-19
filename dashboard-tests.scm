;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
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

(use format fmt)
(require-library iup)
(import (prefix iup iup:))

(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit dashboard-tests))
(declare (uses common))
(declare (uses db))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")

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
			      "Test id: "))
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
					 (iup:attribute-set! lbl "FGCOLOR" (common:get-color-for-state-status (db:test-get-state testdat)
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
	    )))))

;;======================================================================
;; Test meta panel
;;======================================================================

(define (test-meta-panel-get-description testmeta)
  (fmt #f (with-width 40 (wrap-lines (db:testmeta-get-description testmeta)))))

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
			 (iup:label (test-meta-panel-get-description testmeta) #:size "x50"); #:expand "HORIZONTAL")
			 (lambda (testmeta)
			   (test-meta-panel-get-description testmeta)))
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
		   (list (iup:label "runname ")(iup:label "run-id"))))
    (apply iup:vbox
	   (append (map (lambda (keyval)
			  (iup:label (cadr keyval) #:expand "HORIZONTAL"))
			keydat)
		   (list (iup:label runname)
			 (iup:label (conc (db:test-get-run_id testdat)))
			 (iup:label "" #:expand "VERTICAL")))))))
  
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
	 (color  (common:get-color-for-state-status state status)))
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
					(open-run-close db:test-set-state-status-by-id #f test-id #f #f b)
					(set! newcomment b))
			     #:value (db:test-get-comment testdat)
			     #:expand "HORIZONTAL"))
      (apply iup:hbox
	     (iup:label "STATE:" #:size "30x")
	     (let* ((btns  (map (lambda (state)
				  (let ((btn (iup:button state
							 #:expand "HORIZONTAL" #:size "50x" #:font "Courier New, -10"
							 #:action (lambda (x)
								    (open-run-close db:test-set-state-status-by-id #f test-id state #f #f)
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
							 #:expand "HORIZONTAL" #:size "50x" #:font "Courier New, -10"
							 #:action (lambda (x)
								    (open-run-close db:test-set-state-status-by-id #f test-id #f status #f)
								    (db:test-set-status! testdat status)))))
				    btn))
				(list  "PASS" "WARN" "FAIL" "CHECK" "n/a" "WAIVED" "SKIP"))))
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
(define (examine-test test-id) ;; run-id run-key origtest)
  (let* ((testdat       (open-run-close db:get-test-info-by-id #f test-id))
	 (db-path       (conc *toppath* "/megatest.db"))
	 (db-mod-time   0) ;; (file-modification-time db-path))
	 (last-update   0) ;; (current-seconds))
	 (request-update #t)
	 (db             #f))
    (if (not testdat)
	(begin
	  (debug:print 0 "ERROR: No test data found for test " test-id ", exiting")
	  (exit 1))
	(let* ((run-id        (if testdat (db:test-get-run_id testdat) #f))
	       (keydat        (if testdat (open-run-close db:get-key-val-pairs #f run-id) #f))
	       (rundat        (if testdat (open-run-close db:get-run-info #f run-id) #f))
	       (runname       (if testdat (db:get-value-by-header (db:get-row rundat)
								  (db:get-header rundat)
								  "runname") #f))
	       (teststeps     (if testdat (db:get-compressed-steps test-id) '()))
	       (logfile       "/this/dir/better/not/exist")
	       (rundir        logfile)
	       (testfullname  (if testdat (db:test-get-fullname testdat) "Gathering data ..."))
	       (testname      (if testdat (db:test-get-testname testdat) "n/a"))
	       (testmeta      (if testdat 
				  (let ((tm (open-run-close db:testmeta-get-record #f testname)))
				    (if tm tm (make-db:testmeta)))
				  (make-db:testmeta)))

	       (keystring  (string-intersperse 
			    (map (lambda (keyval)
				   ;; (conc ":" (car keyval) " " (cadr keyval)))
				   (cadr keyval))
				 keydat)
			    "/"))
	       (item-path  (db:test-get-item-path testdat))
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
			     (let* ((curr-mod-time (file-modification-time db-path))
				    (need-update   (or (and (> curr-mod-time db-mod-time)
							    (> (current-seconds) (+ last-update 2))) ;; every two seconds if db touched
						       request-update))
				    (newtestdat (if need-update (open-run-close db:get-test-info-by-id #f test-id))))
			       (cond
				((and need-update newtestdat)
				 (set! testdat newtestdat)
				 (set! teststeps    (db:get-compressed-steps test-id))
				 (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
				 (set! rundir       (db:test-get-rundir testdat))
				 (set! testfullname (db:test-get-fullname testdat))
				 ;; (debug:print 0 "INFO: teststeps=" (intersperse teststeps "\n    "))
				 )
				(need-update ;; if this was true and yet there is no data ....
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
	       (store-button store-label)
	       (command-text-box (iup:textbox #:expand "HORIZONTAL" #:font "Courier New, -10"))
	       (command-launch-button (iup:button "Execute!" #:action (lambda (x)
									(let ((cmd (iup:attribute command-text-box "VALUE")))
									  (system (conc cmd "  &"))))))
	       (run-test  (lambda (x)
			    (iup:attribute-set! 
			     command-text-box "VALUE"
			     (conc "xterm -geometry 180x20 -e \"megatest -target " keystring " :runname " runname 
				   " -runtests " (conc testname "/" (if (equal? item-path "")
									"%" 
									item-path))
				   ";echo Press any key to continue;bash -c 'read -n 1 -s'\""))))
	       (remove-test (lambda (x)
			      (iup:attribute-set!
			       command-text-box "VALUE"
			       (conc "xterm -geometry 180x20 -e \"megatest -remove-runs -target " keystring " :runname " runname
				     " -testpatt " (conc testname "/" (if (equal? item-path "")
									  "%"
									  item-path))
				     " -v;echo Press any key to continue;bash -c 'read -n 1 -s'\"")))))
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
					  (iup:vbox
					   (iup:hbox 
					    (iup:button "View Log"    #:action viewlog     #:size "80x")
					    (iup:button "Start Xterm" #:action xterm       #:size "80x")
					    (iup:button "Run Test"    #:action run-test    #:size "80x")
					    (iup:button "Clean Test"  #:action remove-test #:size "80x")
					    (iup:button "Close"       #:action (lambda (x)(exit)) #:size "80x"))
					   (apply 
					    iup:hbox
					    (list command-text-box command-launch-button))))
			       (set-fields-panel test-id testdat)
			       (let ((tabs 
				      (iup:tabs
				       ;; Replace here with matrix
				       (let ((steps-matrix (iup:matrix
							    #:font   "Courier New, -8"
							    #:expand "YES"
							    #:scrollbar "YES"
							    #:numcol 6
							    #:numlin 30
							    #:numcol-visible 6
							    #:numlin-visible 5
							    #:click-cb (lambda (obj lin col status)
									 (if (equal? col 6)
									     (let ((fname (iup:attribute obj (conc lin ":" col))))
									       (viewlog fname)
									       (print "obj: " obj " lin: " lin " col: " col " status: " status)))))
							   ))
					 ;; (let loop ((count 0))
					 ;;   (iup:attribute-set! steps-matrix "FITTOTEXT" (conc "L" count))
					 ;;   (if (< count 30)
					 ;;       (loop (+ count 1))))
					 (iup:attribute-set! steps-matrix "0:1" "Step Name")
					 (iup:attribute-set! steps-matrix "0:2" "Start")
					 (iup:attribute-set! steps-matrix "0:3" "End")
					 (iup:attribute-set! steps-matrix "WIDTH3" "50")
					 (iup:attribute-set! steps-matrix "0:4" "Status")
					 (iup:attribute-set! steps-matrix "WIDTH4" "50")
					 (iup:attribute-set! steps-matrix "0:5" "Duration")
					 (iup:attribute-set! steps-matrix "0:6" "Log File")
					 (iup:attribute-set! steps-matrix "ALIGNMENT1" "ALEFT")
					 ;; (iup:attribute-set! steps-matrix "FIXTOTEXT" "C1")
					 (iup:attribute-set! steps-matrix "RESIZEMATRIX" "YES")
					 (let ((proc
						(lambda (testdat)
						  (if (not (null? teststeps))
						      (let loop ((hed    (car teststeps))
								 (tal    (cdr teststeps))
								 (rownum 1)
								 (colnum 1))
							(let ((val (vector-ref hed (- colnum 1))))
							  (iup:attribute-set! steps-matrix  (conc rownum ":" colnum)(if val (conc val) ""))
							  (if (< colnum 6)
							      (loop hed tal rownum (+ colnum 1))
							      (if (not (null? tal))
								  (loop (car tal)(cdr tal)(+ rownum 1) 1)))))))))
					   (hash-table-set! widgets "StepsMatrix" proc)
					   (proc testdat))
					 steps-matrix)
				       ;; populate the Test Data panel
				       (iup:frame
					#:title "Test Data"
					(let ((test-data
					       (iup:textbox  ;; #:action (lambda (obj char val)
						;;   	#f)
						#:expand "YES"
						#:multiline "YES"
						#:font "Courier New, -10"
						#:size "100x100")))
					  (hash-table-set! widgets "Test Data"
							   (lambda (testdat) ;; 
							     (let* ((currval (iup:attribute test-data "VALUE")) ;; "TITLE"))
								    (fmtstr  "~10a~10a~10a~10a~7a~7a~6a~6a~a") ;; category,variable,value,expected,tol,units,type,comment
								    (newval  (string-intersperse 
									      (append
									       (list 
										(format #f fmtstr "Category" "Variable" "Value" "Expected" "Tol" "Status" "Units" "Type" "Comment")
										(format #f fmtstr "========" "========" "=====" "========" "===" "======" "=====" "====" "======="))
									       (map (lambda (x)
										      (format #f fmtstr
											      (db:test-data-get-category x)
											      (db:test-data-get-variable x)
											      (db:test-data-get-value    x)
											      (db:test-data-get-expected x)
											      (db:test-data-get-tol      x)
											      (db:test-data-get-status   x)
											      (db:test-data-get-units    x)
											      (db:test-data-get-type     x)
											      (db:test-data-get-comment  x)))
										    (open-run-close db:read-test-data #f test-id "%")))
									      "\n")))
							       (if (not (equal? currval newval))
								   (iup:attribute-set! test-data "VALUE" newval ))))) ;; "TITLE" newval)))))
					  test-data))
				       (dashboard:run-controls))))
				 (iup:attribute-set! tabs "TABTITLE0" "Steps")
				 (iup:attribute-set! tabs "TABTITLE1" "Test Data")
				 tabs))))
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
				     (set! *exit-started* 'ok))))))))))

