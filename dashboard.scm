;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(use format numbers)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (uses margs))
(declare (uses launch))
(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses server))
(declare (uses synchash))
(declare (uses dcommon))
(declare (uses tree))
(declare (uses configf))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

(define help (conc 
	      "Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2011

Usage: dashboard [options]
  -h                : this help
  -group groupname  : display this group of areas
  -test testid      : control test identified by testid
  -guimonitor       : control panel for runs

Misc
  -rows N         : set number of rows
"))

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-group" ;; display this group of areas
			"-debug"
			) 
		 (list  "-h"
			"-v"
			"-q"
			)
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

;; (if (args:get-arg "-host")
;;     (begin
;;       (set! (common:get-remote remote) (string-split (args:get-arg "-host" ":")))
;;       (client:launch))
;;     (client:launch))

;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(define *runremote* #f)

(debug:setup)

(define *tim* (iup:timer))
(define *ord* #f)

(iup:attribute-set! *tim* "TIME" 300)
(iup:attribute-set! *tim* "RUN" "YES")

(define (message-window msg)
  (iup:show
   (iup:dialog
    (iup:vbox 
     (iup:label msg #:margin "40x40")))))

(define (iuplistbox-fill-list lb items . default)
  (let ((i 1)
	(selected-item (if (null? default) #f (car default))))
    (iup:attribute-set! lb "VALUE" (if selected-item selected-item ""))
    (for-each (lambda (item)
		(iup:attribute-set! lb (number->string i) item)
		(if selected-item
		    (if (equal? selected-item item)
			(iup:attribute-set! lb "VALUE" item))) ;; (number->string i))))
		(set! i (+ i 1)))
	      items)
    i))

(define (pad-list l n)(append l (make-list (- n (length l)))))


(define (mkstr . x)
  (string-intersperse (map conc x) ","))

(define (update-search x val)
  (hash-table-set! *searchpatts* x val))

;; mtest is actually the megatest.config file
;;
(define (mtest window-id area-dat)
  (let* ((toppath          (megatest:area-path area-dat))
	 (curr-row-num     0)
	 (rawconfig        (read-config (conc toppath "/megatest.config") #f 'return-string))
	 (keys-matrix      (dcommon:keys-matrix rawconfig))
	 (setup-matrix     (dcommon:section-matrix rawconfig "setup" "Varname" "Value"))
	 (jobtools-matrix  (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 5
			    #:numcol-visible 1
			    #:numlin-visible 3))
	 (validvals-matrix (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 2
			    #:numcol-visible 1
			    #:numlin-visible 2))
	 (envovrd-matrix   (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 20
			    #:numcol-visible 1
			    #:numlin-visible 8))
	 (disks-matrix     (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 20
			    #:numcol-visible 1
			    #:numlin-visible 8))
	 )
    (iup:attribute-set! disks-matrix "0:0" "Disk Name")
    (iup:attribute-set! disks-matrix "0:1" "Disk Path")
    (iup:attribute-set! disks-matrix "WIDTH1" "120")
    (iup:attribute-set! disks-matrix "WIDTH0" "100")
    (iup:attribute-set! disks-matrix "ALIGNMENT1" "ALEFT")
    (iup:attribute-set! disks-matrix "FIXTOTEXT" "C1")
    (iup:attribute-set! disks-matrix "RESIZEMATRIX" "YES")

    ;; fill in existing info
    (for-each 
     (lambda (mat fname)
       (set! curr-row-num 1)
       (for-each
	(lambda (var)
	  (iup:attribute-set! mat (conc curr-row-num ":0") var)
	  (iup:attribute-set! mat (conc curr-row-num ":1") (config-lookup rawconfig fname var))
	  (set! curr-row-num (+ curr-row-num 1)))
	(configf:section-vars rawconfig fname)))
     (list setup-matrix jobtools-matrix validvals-matrix envovrd-matrix disks-matrix)
     (list "setup"      "jobtools"      "validvalues"      "env-override" "disks"))

    (for-each
     (lambda (mat)
       (iup:attribute-set! mat "0:1" "Value")
       (iup:attribute-set! mat "0:0" "Var")
       (iup:attribute-set! mat "ALIGNMENT1" "ALEFT")
       (iup:attribute-set! mat "FIXTOTEXT" "C1")
       (iup:attribute-set! mat "RESIZEMATRIX" "YES")
       (iup:attribute-set! mat "WIDTH1" "120")
       (iup:attribute-set! mat "WIDTH0" "100")
       )
     (list setup-matrix jobtools-matrix validvals-matrix envovrd-matrix))

    (iup:attribute-set! validvals-matrix "WIDTH1" "290")
    (iup:attribute-set! envovrd-matrix   "WIDTH1" "290")

    (iup:vbox
     (iup:hbox
      
      (iup:vbox
       (let ((tabs (iup:tabs 
		    ;; The required tab
		    (iup:hbox
		     ;; The keys
		     (iup:frame 
		      #:title "Keys (required)"
		      (iup:vbox
		       (iup:label (conc "Set the fields for organising your runs\n"
					"here. Note: can only be changed before\n"
					"running the first run when megatest.db\n"
					"is created."))
		       keys-matrix))
		     (iup:vbox
		      ;; The setup section
		      (iup:frame
		       #:title "Setup"
		       (iup:vbox
			(iup:label (conc "max_concurrent_jobs : limits total concurrent jobs (optional)\n"
					 "linktree : directory where linktree will be created."))
			setup-matrix))
		      ;; The jobtools
		      (iup:frame
		       #:title "Jobtools"
		       (iup:vbox 
			(iup:label (conc "launcher : tool or script to run jobs (try nbfake)\n"
					 "useshell : use system to run your launcher\n"
					 "workhosts : spread jobs out on these hosts"))
			jobtools-matrix))
		      ;; The disks
		      (iup:frame
		       #:title "Disks"
		       (iup:vbox
			(iup:label (conc "Enter names and existing paths of locations to run tests")) 
			disks-matrix))))
		    ;; The optional tab
		    (iup:vbox
		     ;; The Environment Overrides
		     (iup:frame 
		      #:title "Env override"
		      envovrd-matrix)
		     ;; The valid values
		     (iup:frame
		      #:title "Validvalues"
		      validvals-matrix)
		     ))))
	 (iup:attribute-set! tabs "TABTITLE0" "Required settings")
	 (iup:attribute-set! tabs "TABTITLE1" "Optional settings")
	 tabs))
      ))))

;; The runconfigs.config file
;;
(define (rconfig window-id)
  (iup:vbox
   (iup:frame #:title "Default")))

;;======================================================================
;; T E S T S
;;======================================================================

(define (tree-path->test-id path)
  (if (not (null? path))
      (hash-table-ref/default (dboard:data-get-path-test-ids *data*) path #f)
      #f))

(define (test-panel window-id)
  (let* ((curr-row-num 0)
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
	 (command-text-box (iup:textbox #:expand "HORIZONTAL" #:font "Courier New, -12"))
	 (command-launch-button (iup:button "Execute!" 
					    ;; #:expand "HORIZONTAL"
					    #:size "50x"
					    #:action (lambda (x)
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
			       " -v;echo Press any key to continue;bash -c 'read -n 1 -s'\""))))
    	 (run-info-matrix  (iup:matrix 		            
			    #:expand "YES"
			    ;; #:scrollbar "YES"
			    #:numcol 1
			    #:numlin 4
			    #:numcol-visible 1
			    #:numlin-visible 4
			    #:click-cb (lambda (obj lin col status)
					 (print "obj: " obj " lin: " lin " col: " col " status: " status))))
	 (test-info-matrix (iup:matrix
		            #:expand "YES"
		            #:numcol 1
		            #:numlin 7
		            #:numcol-visible 1
		            #:numlin-visible 7))
	 (test-run-matrix  (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 5
			    #:numcol-visible 1
			    #:numlin-visible 5))
	 (meta-dat-matrix  (iup:matrix
			    #:expand "YES"
			    #:numcol 1
			    #:numlin 5
			    #:numcol-visible 1
			    #:numlin-visible 5))
	 (steps-matrix     (iup:matrix
			    #:expand "YES"
			    #:numcol 6
			    #:numlin 50
			    #:numcol-visible 6
			    #:numlin-visible 8))
	 (data-matrix      (iup:matrix
			    #:expand "YES"
			    #:numcol 8
			    #:numlin 50
			    #:numcol-visible 8
			    #:numlin-visible 8))
	 (updater          (lambda (testdat)
			     (test-update window-id testdat run-info-matrix test-info-matrix test-run-matrix meta-dat-matrix steps-matrix data-matrix))))

    ;; Set the updater in updaters
    (hash-table-set! (dboard:data-get-updaters *data*) window-id updater)
    ;; 
    (for-each
     (lambda (mat)
       ;; (iup:attribute-set! mat "0:1" "Value")
       ;; (iup:attribute-set! mat "0:0" "Var")
       (iup:attribute-set! mat "HEIGHT0" 0)
       (iup:attribute-set! mat "ALIGNMENT1" "ALEFT")
       ;; (iup:attribute-set! mat "FIXTOTEXT" "C1")
       (iup:attribute-set! mat "RESIZEMATRIX" "YES"))
     ;; (iup:attribute-set! mat "WIDTH1" "120")
     ;; (iup:attribute-set! mat "WIDTH0" "100"))
     (list run-info-matrix test-info-matrix test-run-matrix meta-dat-matrix))

    ;; Steps matrix
    (iup:attribute-set! steps-matrix "0:1" "Step Name")
    (iup:attribute-set! steps-matrix "0:2" "Start")
    (iup:attribute-set! steps-matrix "WIDTH2" "40")
    (iup:attribute-set! steps-matrix "0:3" "End")
    (iup:attribute-set! steps-matrix "WIDTH3" "40")
    (iup:attribute-set! steps-matrix "0:4" "Status")
    (iup:attribute-set! steps-matrix "WIDTH4" "40")
    (iup:attribute-set! steps-matrix "0:5" "Duration")
    (iup:attribute-set! steps-matrix "WIDTH5" "40")
    (iup:attribute-set! steps-matrix "0:6" "Log File")
    (iup:attribute-set! steps-matrix "ALIGNMENT1" "ALEFT")
    ;; (iup:attribute-set! steps-matrix "FIXTOTEXT" "C1")
    (iup:attribute-set! steps-matrix "RESIZEMATRIX" "YES")
    ;; (iup:attribute-set! steps-matrix "WIDTH1" "120")
    ;; (iup:attribute-set! steps-matrix "WIDTH0" "100")

    ;; Data matrix
    ;; 
    (let ((rownum 1))
      (for-each
       (lambda (x)
	 (iup:attribute-set! data-matrix (conc "0:" rownum) x)
	 (iup:attribute-set! data-matrix (conc "WIDTH" rownum) "50")
	 (set! rownum (+ rownum 1)))
       (list "Category" "Variable" "Value" "Expected" "Tolerance"  "Status" "Units" "Type" "Comment")))
    (iup:attribute-set! data-matrix "REDRAW" "ALL")
    
    (for-each 
     (lambda (data)
       (let ((mat    (car data))
	     (keys   (cadr data))
	     (rownum 1))
	 (for-each
	  (lambda (key)
	    (iup:attribute-set! mat (conc rownum ":0") key)
	    (set! rownum (+ rownum 1)))
	  keys)
	 (iup:attribute-set! mat "REDRAW" "ALL")))
     (list
      (list run-info-matrix  '("Run Id"  "Target"   "Runname" "Run Start Time" ))
      (list test-info-matrix '("Test Id" "Testname" "Itempath" "State"   "Status" "Test Start Time" "Comment"))
      (list test-run-matrix  '("Hostname" "Host info" "Disk Free" "CPU Load" "Run Duration"))
      (list meta-dat-matrix  '("Author"   "Owner"     "Last Reviewed" "Tags" "Description"))))
    
    (iup:split
     #:orientation "HORIZONTAL"
     (iup:vbox
      (iup:hbox
       (iup:vbox
	run-info-matrix
	test-info-matrix)
       ;; test-info-matrix)
       (iup:vbox
	test-run-matrix
	meta-dat-matrix))
      (iup:vbox
       (iup:vbox
	(iup:hbox 
	 (iup:button "View Log"    #:action viewlog      #:size "60x" )   ;; #:size "30x" 
	 (iup:button "Start Xterm" #:action xterm        #:size "60x" ))	 ;; #:size "30x" 
	(iup:hbox
	 (iup:button "Run Test"    #:action run-test    #:size "60x" )	 ;; #:size "30x" 
	 (iup:button "Clean Test"  #:action remove-test #:size "60x" )))	 ;; #:size "30x" 
       (iup:hbox
	;; hiup:split ;; hbox
	;; #:orientation "HORIZONTAL"
	;; #:value 300
	command-text-box
	command-launch-button)))
     (iup:vbox
      (let ((tabs (iup:tabs
		   steps-matrix
		   data-matrix)))
	(iup:attribute-set! tabs "TABTITLE0" "Test Steps")
	(iup:attribute-set! tabs "TABTITLE1" "Test Data")
	tabs)))))

;; Test browser
(define (tree-browser data adat window-id)
  ;; (iup:split
  (let* ((tb      (iup:treebox
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((tree-path (tree:node->path obj id))
			    (area      (car tree-path))
			    (area-path (cdr tree-path)))
		       #f
		       ;; (test-id  (tree-path->test-id (cdr run-path))))
		       ;; (if test-id
		       ;;    (hash-table-set! (dboard:data-get-curr-test-ids *data*)
		       ;;		     window-id test-id))
		       ;; (print "path: " (tree:node->path obj id) " test-id: " test-id))))))
		       )))))
    ;; (iup:attribute-set! tb "VALUE" "0")
    ;; (iup:attribute-set! tb "NAME" "Runs")
    ;; (iup:attribute-set! tb "ADDEXPANDED" "NO")
    ;; (dboard:data-set-tests-tree! *data* tb)
    tb))
;;   (test-panel window-id)))

;; The function to update the fields in the test view panel
(define (test-update window-id testdat run-info-matrix test-info-matrix test-run-matrix meta-dat-matrix steps-matrix data-matrix)
  ;; get test-id
  ;; then get test record
  (if testdat
      (let* ((test-id      (hash-table-ref/default (dboard:data-get-curr-test-ids *data*) window-id #f))
	     (test-data    (hash-table-ref/default testdat test-id #f))
	     (run-id       (db:test-get-run_id test-data))
	     (targ/runname (hash-table-ref/default (dboard:data-get-run-keys *data*) 
						   run-id
						   '()))
	     (target       (if (null? targ/runname) "" (string-intersperse (reverse (cdr (reverse targ/runname))) "/")))
	     (runname      (if (null? targ/runname) "" (car (cdr targ/runname))))
	     (steps-dat    (tests:get-compressed-steps *dbstruct-local* run-id test-id)))
	
	(if test-data
	    (begin
	      ;; 
	      (for-each 
	       (lambda (data)
		 (let ((mat    (car data))
		       (vals   (cadr data))
		       (rownum 1))
		   (for-each 
		    (lambda (key)
		      (let ((cell   (conc rownum ":1")))
			(if (not (equal? (iup:attribute mat cell)(conc key)))
			    (begin
			      ;; (print "setting cell " cell " in matrix " mat " to value " key)
			      (iup:attribute-set! mat cell (conc key))
			      (iup:attribute-set! mat "REDRAW" cell)))
			(set! rownum (+ rownum 1))))
		    vals)))
	       (list 
		(list run-info-matrix
		      (if test-id
			  (list (db:test-get-run_id test-data)
				target
				runname
				"n/a")
			  (make-list 4 "")))
		(list test-info-matrix
		      (if test-id
			  (list test-id
				(db:test-get-testname test-data)
				(db:test-get-item-path test-data)
				(db:test-get-state    test-data)
				(db:test-get-status   test-data)
				(seconds->string (db:test-get-event_time test-data))
				(db:test-get-comment  test-data))
			  (make-list 7 "")))
		(list test-run-matrix
		      (if test-id
			  (list (db:test-get-host     test-data)
				(db:test-get-uname    test-data)
				(db:test-get-diskfree test-data)
				(db:test-get-cpuload  test-data)
				(seconds->hr-min-sec (db:test-get-run_duration test-data)))
			  (make-list 5 "")))
		))
	      (dcommon:populate-steps steps-dat steps-matrix))))))
;;(list meta-dat-matrix
;;      (if test-id
;;	  (list (


;;======================================================================
;; R U N   C O N T R O L
;;======================================================================

;; General displayer
;;
(define (area-display data adat window-id)
  (let* ((view-matrix     (iup:matrix
			   #:expand "YES"
			   ;; #:fittosize "YES"
			   #:scrollbar "YES"
			   #:numcol 100
			   #:numlin 100
			   #:numcol-visible 3
			   #:numlin-visible 3
			   #:click-cb (lambda (obj lin col status)
					(print "obj: " obj " lin: " lin " col: " col " status: " status)))))

    (iup:attribute-set! view-matrix "RESIZEMATRIX" "YES")
    (iup:attribute-set! view-matrix "WIDTH0" "100")
    ;; (dboard:data-set-runs-matrix! *data* runs-matrix)
    (iup:hbox
     (iup:frame 
      #:title "Runs browser"
      (iup:vbox
       view-matrix)))))

;; Browse and control a single run
;;
(define (runcontrol window-id)
  (iup:hbox))

;;======================================================================
;; A R E A S
;;======================================================================

(define (dashboard:init-area data area-name apath)
  (let* ((mtconffile  (conc area-name "/megatest.config"))
	 (mtconf      (read-config mtconffile (make-hash-table) #f)) ;; megatest.config
	 (area-dat    (let ((ad (make-megatest:area
			       area-name ;; area name
			       apath     ;; path to area
			       'http     ;; transport
			       (list apath mtconf) ;; configinfo (legacy)
			       mtconf    ;; megatest.config
			       (make-hash-table) ;; denoise hash
			       #f        ;; client-signature
			       #f        ;; remote connections
			       #f        ;; run keys
			       (make-hash-table) ;; run-id -> (hash of test-ids => dat)
			       (and (file-exists? apath)(file-write-access? apath)) ;; read-only
			       )))
		      (hash-table-set! (dboard:data-areas data) area-name ad)
		      ad)))
    area-dat))

;;======================================================================
;; D A S H B O A R D
;;======================================================================

;; Main Panel
;;
(define (main-panel data window-id)
  (iup:dialog
   #:title "Megatest Control Panel"
   #:menu (dcommon:main-menu data)
   #:shrink "YES"
   (iup:vbox
    (let* ((area-names  (hash-table-keys (dboard:data-cfgdat data)))
	   (area-panels (map (lambda (aname)
			       (let* ((apath      (configf:lookup (dboard:data-cfgdat data) aname "path")) ;;  (hash-table-ref (dboard:data-cfgdat data) area-name))
				      ;;          (hash-table-ref (dboard:data-cfgdat data) aname))
				      (area-dat   (dashboard:init-area data aname apath))
				      (tb         (tree-browser data area-dat window-id)) ;; (dboard:areas-tree-browser data)
				      (ad         (area-display data area-dat window-id))
				      (areas      (dboard:data-areas data))
				      (dboard-dat (make-dboard:area 
						   #f           ;; tree
						   #f           ;; matrix
						   area-dat     ;;
						   #f           ;; view path
						   'default     ;; view type
						   #f           ;; matrix
						   #f           ;; controls
						   #f           ;; cached data
						   #f           ;; filters
						   #f           ;; the run-id
						   (make-hash-table) ;; run-id -> test-id, for current test id
						   ""
						   )))
				 (hash-table-set! (dboard:data-areas data) aname dboard-dat)
				 (dboard:area-tree-set!   dboard-dat tb)
				 (dboard:area-matrix-set! dboard-dat ad)
				 (iup:split
				  #:value 200
				  tb ad)))
			     area-names))
	   (tabtop      (apply iup:tabs area-panels)))
      (let loop ((index 0)
		 (hed   (car area-names))
		 (tal   (cdr area-names)))
	(debug:print 0 "Adding area " hed " with index " index " to dashboard")
	(iup:attribute-set! tabtop (conc "TABTITLE" index) hed)
	(if (not (null? tal))
	    (loop (+ index 1)(car tal)(cdr tal))))
      tabtop))))

(define (newdashboard data window-id)
  (let* (;; (keys     (db:get-keys *dbstruct-local* *area-dat*))
	 ;; (runname  "%")
	 ;; (testpatt "%")
	 ;; (keypatts (map (lambda (k)(list k "%")) keys))
	 ;; (states   '())
	 ;; (statuses '())
	 (nextmintime (current-milliseconds)))
    (dboard:data-current-window-id-set! data (+ 1 (dboard:data-current-window-id data)))
    ;; (dboard:data-set-runs! *data* data) ;; make this data available to the rest of the application
    (iup:show (main-panel data (dboard:data-current-window-id data)))
    ;; Yes, running iup:show will pop up a new panel
    ;; (iup:show (main-panel my-window-id))
    (iup:callback-set! *tim*
		       "ACTION_CB"
		       (lambda (x)
			 (let ((starttime (current-milliseconds)))
			   ;; Want to dedicate no more than 50% of the time to this so skip if
			   ;; 2x delta time has not passed since last query
			   ;; (if (< (inexact->exact nextmintime)(inexact->exact starttime))
			   ;;     (let* ((changes   (dcommon:run-update data)) ;;keys data runname keypatts testpatt states statuses 'full my-window-id))
			   ;;            (endtime   (current-milliseconds)))
			   ;;       (set! nextmintime (+ endtime (* 2.0 (- endtime starttime))))
			   ;;       ;; (debug:print 11 "CHANGE(S): " (car changes) "...")
			   ;;       )
			   ;;     (debug:print-info 11 "Server overloaded")))))))
			   (dcommon:run-update data))))))

;;; main. Theoretically could have multiple windows (each with a group of tags, thus window-id
;;;
(let* ((window-id 0)
       (groupn    (or (args:get-arg "-group") "default"))
       (cfname    (conc (getenv "HOME") "/.megatest/" groupn ".dat"))
       (cfgdat    (if (file-exists? cfname)(read-config cfname (make-hash-table) #t)))
       (data      (make-dboard:data
		   cfgdat ;; this is the data from ~/.megatest for the selected group
		   (make-hash-table) ;; areaname -> area-rec
		   0 
		   )))
  (newdashboard data window-id)
  (iup:main-loop))
