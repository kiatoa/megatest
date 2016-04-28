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
(declare (uses gutils))
(declare (uses rmt))
(declare (uses ezsteps))
;; (declare (uses sdb))
;; (declare (uses filedb))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")

;;======================================================================
;; C O M M O N
;;======================================================================

(define *dashboard-comment-share-slot* #f)

(define (dtests:get-pre-command #!key (default-override #f))
  (let ((cfg-ovrd (configf:lookup *configdat* "dashboard" "pre-command")))
    (or cfg-ovrd default-override "xterm -geometry 180x20 -e \"")))

(define (dtests:get-post-command #!key (default-override #f))
  (let ((cfg-ovrd (configf:lookup *configdat* "dashboard" "post-command")))
    (or cfg-ovrd default-override ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))


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
			      "Test date: "))
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
					 (iup:attribute-set! lbl "FGCOLOR" (car (gutils:get-color-for-state-status (db:test-get-state testdat)
														   (db:test-get-status testdat))))
					 (iup:attribute-set! lbl "TITLE" (db:test-get-status testdat)))))))
	      lbl)
	    (store-label "testcomment"
			 (iup:label "TestComment                             "
				    #:expand "HORIZONTAL")
			 (lambda (testdat)
			   (let ((newcomment (db:test-get-comment testdat)))
			     (if *dashboard-comment-share-slot*
				 (if (not (equal? (iup:attribute *dashboard-comment-share-slot* "VALUE")
						  newcomment))
				     (iup:attribute-set! *dashboard-comment-share-slot*
							 "VALUE"
							 newcomment)))
			     newcomment)))
	    (store-label "testid"
			 (iup:label "TestId                             "
				    #:expand "HORIZONTAL")
			 (lambda (testdat)
			   (db:test-get-id testdat)))
	    (store-label "testdate" 
			 (iup:label "TestDate                           "
				    #:expand "HORIZONTAL")
			 (lambda (testdat)
			   (seconds->work-week/day-time (db:test-get-event_time testdat))))
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
			      "Description: "))
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
(define (run-info-panel db keydat testdat runname)
  (let* ((run-id     (db:test-get-run_id testdat))
	 (rundat     (db:get-run-info db run-id))
	 (header     (db:get-header rundat))
	 (event_time (db:get-value-by-header (db:get-rows rundat)
					     (db:get-header rundat)
					     "event_time")))
    (iup:frame 
     #:title "Megatest Run Info" ; #:expand "YES"
     (iup:hbox ; #:expand "YES"
      (apply iup:vbox ; #:expand "YES"
	     (append (map (lambda (keyval)
			    (iup:label (conc (car keyval) " ")))
			  keydat)
		     (list (iup:label "runname ")
			   (iup:label "run-id")
			   (iup:label "run-date"))))
      (apply iup:vbox
	     (append (map (lambda (keyval)
			    (iup:label (cadr keyval) #:expand "HORIZONTAL"))
			  keydat)
		     (list (iup:label runname)
			   (iup:label (conc run-id))
			   (iup:label (seconds->year-work-week/day-time event_time))
			   (iup:label "" #:expand "VERTICAL"))))))))
  
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
			      "Disk free: "
			      "CPU Load: "
			      "Run duration: "
			      "Logfile: "
			      "Top process id: "
			      "Uname -a: "))
		   (iup:label "" #:expand "VERTICAL")))
    (apply iup:vbox ; #:expand "YES"
	   (list
	    ;; NOTE: Yes, the host can change!
	    (store-label "HostName"
			 (iup:label ;; (sdb:qry 'getstr 
			  (db:test-get-host testdat) ;; )
			  #:expand "HORIZONTAL")
			 (lambda (testdat)(db:test-get-host testdat)))
	    (store-label "DiskFree"
			 (iup:label (conc (db:test-get-diskfree testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-diskfree testdat))))
	    (store-label "CPULoad"
			 (iup:label (conc (db:test-get-cpuload testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-cpuload testdat))))
	    (store-label "RunDuration"
			 (iup:label (conc (seconds->hr-min-sec (db:test-get-run_duration testdat))) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (seconds->hr-min-sec (db:test-get-run_duration testdat)))))
	    (store-label "LogFile"
			 (iup:label (conc (db:test-get-final_logf testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-final_logf testdat))))
	    (store-label "ProcessId"
			 (iup:label (conc (db:test-get-process_id testdat)) #:expand "HORIZONTAL")
			 (lambda (testdat)(conc (db:test-get-process_id testdat))))
	    (store-label "Uname"
			 (iup:label "                                                   " #:expand "HORIZONTAL") ;;  #:wordwrap "YES")
			 (lambda (testdat) ;; (sdb:qry 'getstr 
			   (db:test-get-uname testdat))) ;; )
	    )))))

;; if there is a submegatest create a button to launch dashboard in that area
;;
(define (submegatest-panel dbstruct keydat testdat runname testconfig)
  (let* ((subarea (configf:lookup testconfig "setup" "submegatest"))
	 (area-exists (and subarea (file-exists? subarea))))
    ;; (debug:print-info 0 "Megatest subarea=" subarea ", area-exists=" area-exists)
    (if subarea
	(iup:frame 
	 #:title "Megatest Run Info" ; #:expand "YES"
	 (iup:button
	  "Launch Dashboard"
	  #:action (lambda (obj)
		     (system (conc "cd " subarea ";env -i PATH=$PATH DISPLAY=$DISPLAY HOME=$HOME USER=$USER dashboard &")))))
	(iup:vbox))))

;; use a global for setting the buttons colors
;;                           state status teststeps
(define *state-status* (vector #f #f #f))
(define (update-state-status-buttons testdat)
  (let* ((state  (db:test-get-state  testdat))
	 (status (db:test-get-status testdat))
	 (color  (car (gutils:get-color-for-state-status state status))))
    ((vector-ref *state-status* 0) state color)
    ((vector-ref *state-status* 1) status color)))

(define *dashboard-test-db* #t)
(define *dashboard-comment-share-slot* #f)

;;======================================================================
;; Set fields 
;;======================================================================
(define (set-fields-panel dbstruct run-id test-id testdat #!key (db #f))
  (let ((newcomment #f)
	(newstatus  #f)
	(newstate   #f)
	(wtxtbox    #f))
    (iup:frame
     #:title "Set fields"
     (iup:vbox
      (iup:hbox (iup:label "Comment:")
		(let ((txtbox (iup:textbox #:action (lambda (val a b)
						      (rmt:test-set-state-status-by-id run-id test-id #f #f b)
						      ;; IDEA: Just set a variable with the proc to call?
						      (rmt:test-set-state-status-by-id run-id test-id #f #f b)
						      (set! newcomment b))
					   #:value (db:test-get-comment testdat)
					   #:expand "HORIZONTAL")))
		  (set! wtxtbox txtbox)
		  txtbox))
		  
      (apply iup:hbox
	     (iup:label "STATE:" #:size "30x")
	     (let* ((btns  (map (lambda (state)
				  (let ((btn (iup:button state
							 #:expand "HORIZONTAL" #:size "50x" #:font "Courier New, -10"
							 #:action (lambda (x)
								    (rmt:test-set-state-status-by-id run-id test-id state #f #f)
								    (db:test-set-state! testdat state)))))
				    btn))
				(map cadr *common:std-states*)))) ;; (list "COMPLETED" "NOT_STARTED" "RUNNING" "REMOTEHOSTSTART" "LAUNCHED" "KILLED" "KILLREQ"))))
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
								    (let ((t (iup:attribute x "TITLE")))
								      (if (equal? t "WAIVED")
									  (iup:show (dashboard-tests:waiver run-id testdat 
													    (if wtxtbox (iup:attribute wtxtbox "VALUE") #f)
													    (lambda (c)
													      (set! newcomment c)
													      (if wtxtbox 
														  (begin
														    (iup:attribute-set! wtxtbox "VALUE" c)
														    (if (not *dashboard-comment-share-slot*)
															(set! *dashboard-comment-share-slot* wtxtbox)))
														  ))))
									  (begin
									    (rmt:test-set-state-status-by-id run-id test-id #f status #f)
									    (db:test-set-status! testdat status))))))))
				    btn))
				(map cadr *common:std-statuses*)))) ;; (list  "PASS" "WARN" "FAIL" "CHECK" "n/a" "WAIVED" "SKIP"))))
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

(define (dashboard-tests:run-html-viewer lfilename)
  (let ((htmlviewercmd (configf:lookup *configdat* "setup" "htmlviewercmd")))
    (if htmlviewercmd
	(system (conc "(" htmlviewercmd " " lfilename " ) &")) 
	(iup:send-url lfilename))))

(define (dashboard-tests:run-a-step info)
  #t)

(define (dashboard-tests:step-run-control testdat stepname testconfig)
  (iup:dialog ;; #:close_cb (lambda (a)(exit)) ; #:expand "YES"
   #:title stepname
   (iup:vbox ; #:expand "YES"
    (iup:label (conc "Step: " stepname "\nNB// These buttons only run the test step\nfor the purpose of debugging.\nNot all database updates are done."))
    (iup:button "Re-run"            
		#:expand "HORIZONTAL" 
		#:action (lambda (obj)
			   (thread-start! 
			    (make-thread (lambda ()
					   (ezsteps:run-from testdat stepname #t))
					 (conc "ezstep run single step " stepname)))))
    (iup:button "Re-run and continue"         
		#:expand "HORIZONTAL" 
		#:action (lambda (obj)
			   (thread-start!
			    (make-thread (lambda ()
					   (ezsteps:run-from testdat stepname #f))
					 (conc "ezstep run from step " stepname)))))
    ;; (iup:button "Refresh test data"
    ;;     	#:expand "HORIZONTAL"
    ;;     	#:action (lambda (obj)
    ;;     		   (print "Refresh test data " stepname))
    )))

(define (dashboard-tests:waiver run-id testdat ovrdval cmtcmd)
  (let* ((wpatt (configf:lookup *configdat* "setup" "waivercommentpatt"))
	 (wregx (if (string? wpatt)(regexp wpatt) #f))
	 (wmesg (iup:label (if wpatt (conc "Comment must match pattern " wpatt) "")))
	 (comnt (iup:textbox #:action (lambda (val a b)
					(if wpatt
					    (if (string-match wregx b)
						(iup:attribute-set! wmesg "TITLE" (conc "Comment matches " wpatt))
						(iup:attribute-set! wmesg "TITLE" (conc "Comment does not match " wpatt))
						)))
			     #:value (if ovrdval ovrdval (db:test-get-comment testdat))
			     #:expand "HORIZONTAL"))
	 (dlog  #f))
    (set! dlog (iup:dialog ;; #:close_cb (lambda (a)(exit)) ; #:expand "YES"
		#:title "SET WAIVER"
		(iup:vbox ; #:expand "YES"
		 (iup:label (conc "Enter justification for waiving test "
				  (db:test-get-testname testdat)
				  (if (equal? (db:test-get-item-path testdat) "") 
				      ""
				      (conc "/" (db:test-get-item-path testdat)))))
		 wmesg ;; the informational msg on whether it matches
		 comnt
		 (iup:hbox
		  (iup:button "Apply and Close "
			      #:expand "HORIZONTAL"
			      #:action (lambda (obj)
					 (let ((comment (iup:attribute comnt "VALUE"))
					       (test-id (db:test-get-id testdat)))
					   (if (or (not wpatt)
						   (string-match wregx comment))
					       (begin
						 (rmt:test-set-state-status-by-id run-id test-id #f "WAIVED" comment)
						 (db:test-set-status! testdat "WAIVED")
						 (cmtcmd comment)
						 (iup:destroy! dlog))))))
		  (iup:button "Cancel"
			      #:expand "HORIZONTAL" 
			      #:action (lambda (obj)
					 (iup:destroy! dlog)))))))
    dlog))


;;======================================================================
;;
;;======================================================================
(define (examine-test run-id test-id) ;; run-id run-key origtest)
  (let* ((db-path       (db:dbfile-path run-id)) ;; (conc (configf:lookup *configdat* "setup" "linktree") "/db/" run-id ".db"))
	 (dbstruct      (make-dbr:dbstruct path:  (db:dbfile-path #f) ;; (configf:lookup *configdat* "setup" "linktree") 
					   local: #t))
	 (testdat        (rmt:get-test-info-by-id run-id test-id)) ;; (db:get-test-info-by-id dbstruct run-id test-id))
	 (db-mod-time   0) ;; (file-modification-time db-path))
	 (last-update   0) ;; (current-seconds))
	 (request-update #t))
    (if (not testdat)
	(begin
	  (debug:print 2 "ERROR: No test data found for test " test-id ", exiting")
	  (exit 1))
	(let* (;; (run-id        (if testdat (db:test-get-run_id testdat) #f))
	       (test-registry (tests:get-all))
	       (keydat        (if testdat (rmt:get-key-val-pairs run-id) #f))
	       (rundat        (if testdat (rmt:get-run-info run-id) #f))
	       (runname       (if testdat (db:get-value-by-header (db:get-rows rundat)
								  (db:get-header rundat)
								  "runname") #f))
	       ;; (tdb           (tdb:open-test-db-by-test-id-local dbstruct run-id test-id))
	       ;; These next two are intentional bad values to ensure errors if they should not
	       ;; get filled in properly.
	       (logfile       "/this/dir/better/not/exist")
	       (rundir        (if testdat 
				  (db:test-get-rundir testdat)
				  logfile))
	       ;; (testdat-path  (conc rundir "/testdat.db")) ;; this gets recalculated until found 
	       (teststeps     (if testdat (tests:get-compressed-steps #f run-id test-id) '()))
	       (testfullname  (if testdat (db:test-get-fullname testdat) "Gathering data ..."))
	       (testname      (if testdat (db:test-get-testname testdat) "n/a"))
	       ;; (tests:get-testconfig testdat testname 'return-procs))
	       (testmeta      (if testdat 
				  (let ((tm (rmt:testmeta-get-record testname)))
				    (if tm tm (make-db:testmeta)))
				  (make-db:testmeta)))

	       (keystring  (string-intersperse 
			    (map (lambda (keyval)
				   ;; (conc ":" (car keyval) " " (cadr keyval)))
				   (cadr keyval))
				 keydat)
			    "/"))
	       (item-path  (db:test-get-item-path testdat))
	       ;; this next block was added to fix a bug where variables were
               ;; needed. Revisit this.
	       (runconfig  (let ((runconfigf (conc  *toppath* "/runconfigs.config")))
	 		     (if (file-exists? runconfigf)
	 			 (handle-exceptions
                                   exn
                                   #f  ;; do nothing, just keep on trucking ....
                                   (setup-env-defaults runconfigf run-id (make-hash-table) keydat environ-patt: keystring))
	 			 (make-hash-table))))
	       (testconfig    (begin
				;; (runs:set-megatest-env-vars run-id inrunname: runname testname: test-name itempath: item-path)
				(runs:set-megatest-env-vars run-id inkeyvals: keydat inrunname: runname intarget: keystring testname: testname itempath: item-path) ;; these may be needed by the launching process
				(handle-exceptions
				 exn
				 (tests:get-testconfig (db:test-get-testname testdat) test-registry #f)
				 (tests:get-testconfig (db:test-get-testname testdat) test-registry #t))))
	       (viewlog    (lambda (x)
			     (if (file-exists? logfile)
					;(system (conc "firefox " logfile "&"))
				 (dashboard-tests:run-html-viewer logfile)
				 (message-window (conc "File " logfile " not found")))))
	       (view-a-log (lambda (lfile) 
			     (let ((lfilename (conc rundir "/" lfile)))
			       ;; (print "lfilename: " lfilename)
			       (if (file-exists? lfilename)
					;(system (conc "firefox " logfile "&"))
				   (dashboard-tests:run-html-viewer lfilename)
				   (message-window (conc "File " lfilename " not found"))))))
	       (xterm      (lambda (x)
			     (if (directory-exists? rundir)
				 (let ((shell (if (get-environment-variable "SHELL") 
						  (conc "-e " (get-environment-variable "SHELL"))
						  "")))
				   (common:without-vars
				    (conc "cd " rundir 
					  ";mt_xterm -T \"" (string-translate testfullname "()" "  ") "\" " shell "&")
				    "MT_.*"))
				 (message-window  (conc "Directory " rundir " not found")))))
	       (widgets    (make-hash-table))
	       (refreshdat (lambda ()
			     (let* ((curr-mod-time (file-modification-time db-path))
				                   ;;     (max ..... (if (file-exists? testdat-path)
						   ;;      	      (file-modification-time testdat-path)
						   ;;      	      (begin
						   ;;      		(set! testdat-path (conc rundir "/testdat.db"))
						   ;;      		0))))
				    (need-update   (or (and (>= curr-mod-time db-mod-time)
							    (> (current-milliseconds)(+ last-update 250))) ;; every half seconds if db touched
						       (> (current-milliseconds)(+ last-update 10000))     ;; force update even 10 seconds
						       request-update))
				    (newtestdat (if need-update 
						    ;; NOTE: BUG HIDER, try to eliminate this exception handler
						    (handle-exceptions
						     exn 
						     (debug:print-info 0 "test db access issue in examine test for run-id " run-id ", test-id " test-id ": " ((condition-property-accessor 'exn 'message) exn))
						     (rmt:get-test-info-by-id run-id test-id )))))
			       ;; (debug:print-info 0 "need-update= " need-update " curr-mod-time = " curr-mod-time)
			       (cond
				((and need-update newtestdat)
				 (set! testdat newtestdat)
				 (set! teststeps    (tests:get-compressed-steps #f run-id test-id))
				 (set! logfile      (conc (db:test-get-rundir testdat) "/" (db:test-get-final_logf testdat)))
				 (set! rundir       ;; (filedb:get-path *fdb* 
				       (db:test-get-rundir testdat)) ;; )
				 (set! testfullname (db:test-get-fullname testdat))
				 ;; (debug:print 0 "INFO: teststeps=" (intersperse teststeps "\n    "))
				 
				 ;; I don't see why this was implemented this way. Please comment it ...
				 ;; (if (eq? curr-mod-time db-mod-time) ;; do only once if same
				 ;;     (set! db-mod-time (+ curr-mod-time 1))
				 ;;     (set! db-mod-time curr-mod-time))

				 (if (not (eq? curr-mod-time db-mod-time))
				     (set! db-mod-time curr-mod-time))
				 (set! last-update (current-milliseconds))
				 (set! request-update #f) ;; met the need ...
				 )
				(need-update ;; if this was true and yet there is no data ....
				 (db:test-set-testname! testdat "DEAD OR DELETED TEST")))
			       (if need-update
				   (begin
				     ;; update the gui elements here
				     (for-each 
				      (lambda (key)
					;; (print "Updating " key)
					((hash-table-ref widgets key) testdat))
				      (hash-table-keys widgets))
				     (update-state-status-buttons testdat)))
			       ;; (iup:refresh self)
			       )))
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
	       (command-proc (lambda (command-text-box)
			       (let* ((cmd     (iup:attribute command-text-box "VALUE"))
				      (fullcmd (conc (dtests:get-pre-command)
						     cmd 
						     (dtests:get-post-command))))
				 (debug:print-info 02 "Running command: " fullcmd)
				 (common:without-vars fullcmd "MT_.*"))))
	       (command-text-box (iup:textbox
				  #:expand "HORIZONTAL"
				  #:font "Courier New, -10"
				  #:action (lambda (obj cnum val)
					     ;; (print "cnum=" cnum)
					     (if (eq? cnum 13)
						 (command-prox obj)))
				  ))
	       (command-launch-button (iup:button "Execute!" #:action (lambda (x)
									(command-proc command-text-box))))
	;; (lambda (x)
	;; 								(let* ((cmd     (iup:attribute command-text-box "VALUE"))
	;; 								       (fullcmd (conc (dtests:get-pre-command)
	;; 										      cmd 
	;; 										      (dtests:get-post-command))))
	;; 								  (debug:print-info 02 "Running command: " fullcmd)
	;; 								  (common:without-vars fullcmd "MT_.*")))))
	       (kill-jobs (lambda (x)
			    (iup:attribute-set! 
			     command-text-box "VALUE"
			     (conc "megatest -target " keystring " -runname "  runname 
				   " -set-state-status KILLREQ,n/a -testpatt %/% "
				   " -state RUNNING"))))
	       (run-test  (lambda (x)
			    (iup:attribute-set! 
			     command-text-box "VALUE"
			     (conc "megatest -target " keystring " -runname " runname 
				   " -run -testpatt " (conc testname "/" (if (equal? item-path "")
									"%" 
									item-path))
				   ))))
	       (remove-test (lambda (x)
			      (iup:attribute-set!
			       command-text-box "VALUE"
			       (conc "megatest -remove-runs -target " keystring " -runname " runname
				     " -testpatt " (conc testname "/" (if (equal? item-path "")
									  "%"
									  item-path))
				     " -v"))))
	       (clean-run-execute  (lambda (x)
				     (let ((cmd (conc "megatest -remove-runs -target " keystring " -runname " runname
						      " -testpatt " (conc testname "/" (if (equal? item-path "")
						       					   "%"
						       					   item-path))
						      ";megatest -target " keystring " -runname " runname 
						      " -run -preclean -testpatt " (conc testname "/" (if (equal? item-path "")
											   "%" 
											   item-path))
						      )))
				       (common:without-vars
					(conc (dtests:get-pre-command)
					      cmd 
					      (dtests:get-post-command))
					"MT_.*"))))
	       (remove-test (lambda (x)
			      (iup:attribute-set!
			       command-text-box "VALUE"
			       (conc "megatest -remove-runs -target " keystring " -runname " runname
				     " -testpatt " (conc testname "/" (if (equal? item-path "")
									  "%"
									  item-path))
				     " -v"))))
	       (archive-test  (lambda (x)
				(iup:attribute-set! 
				 command-text-box "VALUE"
				 (conc "megatest -target " keystring " -runname " runname 
				       " -archive save-remove -testpatt " (conc testname "/" (if (equal? item-path "")
												 "%" 
												 item-path))
				       )))))
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
				(run-info-panel dbstruct keydat testdat runname)
				(test-info-panel testdat store-label widgets)
				(test-meta-panel testmeta store-meta))
			       (iup:hbox
				(host-info-panel testdat store-label)
				(submegatest-panel dbstruct keydat testdat runname testconfig))
			       ;; The controls
			       (iup:frame #:title "Actions" 
					  (iup:vbox
					   (iup:hbox 
					    (iup:button "View Log"      #:action viewlog      #:size "80x")
					    (iup:button "Start Xterm"   #:action xterm        #:size "80x")
					    (iup:button "Run Test"      #:action run-test     #:size "80x")
					    (iup:button "Clean Test"    #:action remove-test  #:size "80x")
					    (iup:button "CleanRunExecute!"    #:action clean-run-execute #:size "80x")
					    (iup:button "Kill All Jobs" #:action kill-jobs    #:size "80x")
					    (iup:button "Archive Test"  #:action archive-test #:size "80x")
					    (iup:button "Close"         #:action (lambda (x)(exit)) #:size "80x"))
					   (apply 
					    iup:hbox
					    (list command-text-box command-launch-button))))
			       (set-fields-panel dbstruct run-id test-id testdat)
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
									 ;; (if (equal? col 6)
									 (let* ((mtrx-rc (conc lin ":" 6))
										(fname   (iup:attribute obj mtrx-rc))) ;; col))))
									   (if (eq? col 6)
									       (view-a-log fname)
									       (iup:show
										(dashboard-tests:step-run-control 
										 testdat
										 (iup:attribute obj (conc lin ":" 1)) 
										 teststeps))))))))
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
						  (dcommon:populate-steps teststeps steps-matrix))))
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
										    (rmt:read-test-data run-id test-id "%")))
									      "\n")))
							       (if (not (equal? currval newval))
								   (iup:attribute-set! test-data "VALUE" newval ))))) ;; "TITLE" newval)))))
					  test-data))
				       ;;(dashboard:run-controls)
				       )))
				 (iup:attribute-set! tabs "TABTITLE0" "Steps")
				 (iup:attribute-set! tabs "TABTITLE1" "Test Data")
				 tabs))))
	    (iup:show self)
	    (iup:callback-set! *tim* "ACTION_CB"
			       (lambda (x)
				 ;; Now start keeping the gui updated from the db
				 (refreshdat) ;; update from the db here
					;(thread-suspend! other-thread)
				 (if *exit-started*
				     (set! *exit-started* 'ok))))))))))

