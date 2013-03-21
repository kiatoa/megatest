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

(use format)
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

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

(define help (conc 
"Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2011

Usage: dashboard [options]
  -h                : this help
  -server host:port : connect to host:port instead of db access
  -test testid      : control test identified by testid
  -guimonitor       : control panel for runs

Misc
  -rows N         : set number of rows
"))

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-rows"
			"-run"
			"-test"
			"-debug"
			"-host" 
			) 
		 (list  "-h"
			"-guimonitor"
			"-main"
			"-v"
			"-q"
		       )
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

(if (not (setup-for-run))
    (begin
      (print "Failed to find megatest.config, exiting") 
      (exit 1)))

(if (args:get-arg "-host")
    (begin
      (set! *runremote* (string-split (args:get-arg "-host" ":")))
      (client:launch))
    (client:launch))


(debug:setup)

(define *tim* (iup:timer))
(define *ord* #f)

(define *data* (make-vector 10 #f))
(define-inline (dboard:data-get-runs          vec)    (vector-ref  vec 0))
(define-inline (dboard:data-get-tests         vec)    (vector-ref  vec 1))
(define-inline (dboard:data-get-runs-matrix   vec)    (vector-ref  vec 2))
(define-inline (dboard:data-get-tests-tree    vec)    (vector-ref  vec 3))
(define-inline (dboard:data-get-run-keys      vec)    (vector-ref  vec 4))
(define-inline (dboard:data-get-curr-test-ids vec)    (vector-ref  vec 5))
;; (define-inline (dboard:data-get-test-details  vec)    (vector-ref  vec 6))
(define-inline (dboard:data-get-path-test-ids vec)    (vector-ref  vec 7))
(define-inline (dboard:data-get-updaters      vec)    (vector-ref  vec 8))

(define-inline (dboard:data-set-runs!          vec val)(vector-set! vec 0 val))
(define-inline (dboard:data-set-tests!         vec val)(vector-set! vec 1 val))
(define-inline (dboard:data-set-runs-matrix!   vec val)(vector-set! vec 2 val))
(define-inline (dboard:data-set-tests-tree!    vec val)(vector-set! vec 3 val))
(define-inline (dboard:data-set-run-keys!      vec val)(vector-set! vec 4 val))
(define-inline (dboard:data-set-curr-test-ids! vec val)(vector-set! vec 5 val))
;; (define-inline (dboard:data-set-test-details!  vec val)(vector-set! vec 6 val))
(define-inline (dboard:data-set-path-test-ids! vec val)(vector-set! vec 7 val))
(define-inline (dboard:data-set-updaters!      vec val)(vector-set! vec 8 val))

(dboard:data-set-run-keys! *data* (make-hash-table))

;; List of test ids being viewed in various panels
(dboard:data-set-curr-test-ids! *data* (make-hash-table))

;; Look up test-ids by (key1 key2 ... testname [itempath])
(dboard:data-set-path-test-ids! *data* (make-hash-table))

;; Each test panel has an updater, only call when the tab is exposed
(dboard:data-set-updaters!      *data* (make-hash-table))

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

(define (main-menu)
  (iup:menu ;; a menu is a special attribute to a dialog (think Gnome putting the menu at screen top)
   (iup:menu-item "Files" (iup:menu   ;; Note that you can use either #:action or action: for options
		       (iup:menu-item "Open"  action: (lambda (obj)
							(iup:show (iup:file-dialog))
							(print "File->open " obj)))
		       (iup:menu-item "Save"  #:action (lambda (obj)(print "File->save " obj)))
		       (iup:menu-item "Exit"  #:action (lambda (obj)(exit)))))
   (iup:menu-item "Tools" (iup:menu
		       (iup:menu-item "Create new blah" #:action (lambda (obj)(print "Tools->new blah")))
		       ;; (iup:menu-item "Show dialog"     #:action (lambda (obj)
		       ;;  					   (show message-window
		       ;;  					     #:modal? #t
		       ;;  					     ;; set positon using coordinates or center, start, top, left, end, bottom, right, parent-center, current
		       ;;  					     ;; #:x 'mouse
		       ;;  					     ;; #:y 'mouse
		       ;;  )					     
		       ))))

;; mtest is actually the megatest.config file
;;
(define (mtest window-id)
  (let* ((curr-row-num     0)
	 (rawconfig        (read-config (conc *toppath* "/megatest.config") #f 'return-string))
	 (keys-matrix      (iup:matrix
		            #:expand "VERTICAL"
		            ;; #:scrollbar "YES"
		            #:numcol 1
		            #:numlin 20
		            #:numcol-visible 1
		            #:numlin-visible 5
		            #:click-cb (lambda (obj lin col status)
					 (print "obj: " obj " lin: " lin " col: " col " status: " status))))
	 (setup-matrix     (iup:matrix
		            #:expand "YES"
		            #:numcol 1
		            #:numlin 5
		            #:numcol-visible 1
		            #:numlin-visible 3))
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
    (iup:attribute-set! keys-matrix "0:0" "Field Num")
    (iup:attribute-set! keys-matrix "0:1" "Field Name")
    (iup:attribute-set! keys-matrix "WIDTH1" "100")
    (iup:attribute-set! disks-matrix "0:0" "Disk Name")
    (iup:attribute-set! disks-matrix "0:1" "Disk Path")
    (iup:attribute-set! disks-matrix "WIDTH1" "120")
    (iup:attribute-set! disks-matrix "WIDTH0" "100")
    (iup:attribute-set! disks-matrix "ALIGNMENT1" "ALEFT")
    (iup:attribute-set! disks-matrix "FIXTOTEXT" "C1")
    (iup:attribute-set! disks-matrix "RESIZEMATRIX" "YES")
    ;; fill in keys
    (set! curr-row-num 1)
    (for-each 
     (lambda (var)
       (iup:attribute-set! keys-matrix (conc curr-row-num ":0") curr-row-num)
       (iup:attribute-set! keys-matrix (conc curr-row-num ":1") var)
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
     (configf:section-vars rawconfig "fields"))

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
;; T R E E   S T U F F 
;;======================================================================

;; path is a list of nodes, each the child of the previous
;; this routine returns the id so another node can be added
;; either as a leaf or as a branch
;;
;; BUG: This needs a stop sensor for when a branch is exhausted
;;
(define (tree-find-node obj path)
  ;; start at the base of the tree
  (if (null? path)
      #f ;; or 0 ????
      (let loop ((hed      (car path))
		 (tal      (cdr path))
		 (depth    0)
		 (nodenum  0))
	;; nodes in iup tree are 100% sequential so iterate over nodenum
	(if (iup:attribute obj (conc "DEPTH" nodenum)) ;; end when no more nodes
	    (let ((node-depth (string->number (iup:attribute obj (conc "DEPTH" nodenum))))
		  (node-title (iup:attribute obj (conc "TITLE" nodenum))))
	      (if (and (equal? depth node-depth)
		       (equal? hed   node-title)) ;; yep, this is the one!
		  (if (null? tal) ;; end of the line
		      nodenum
		      (loop (car tal)(cdr tal)(+ depth 1)(+ 1 nodenum)))
		  ;; this is the case where we found part of the hierarchy but not 
		  ;; all of it, i.e. the node-depth went from deep to less deep
		  (if (> depth node-depth) ;; (+ 1 node-depth))
		      #f
		      (loop hed tal depth (+ nodenum 1)))))
	    #f))))

;; top is the top node name zeroeth node VALUE=0
(define (tree-add-node obj top nodelst #!key (userdata #f))
  (if (not (iup:attribute obj "TITLE0"))
      (iup:attribute-set! obj "ADDBRANCH0" top))
  (cond
   ((not (string=? top (iup:attribute obj "TITLE0")))
    (print "ERROR: top name " top " doesn't match " (iup:attribute obj "TITLE0")))
   ((null? nodelst))
   (else
    (let loop ((hed      (car nodelst))
	       (tal      (cdr nodelst))
	       (depth    1)
	       (pathl    (list top)))
      ;; Because the tree dialog changes node numbers when
      ;; nodes are added or removed we must look up nodes
      ;; each and every time. 0 is the top node so default
      ;; to that.
      (let* ((newpath    (append pathl (list hed)))
	     (parentnode (tree-find-node obj pathl))
	     (nodenum    (tree-find-node obj newpath)))
	;; Add the branch under lastnode if not found
	(if (not nodenum)
	    (begin
	      (iup:attribute-set! obj (conc "ADDBRANCH" parentnode) hed)
	      (if userdata
		  (iup:attribute-set! obj (conc "USERDATA"   parentnode) userdata))
	      (if (null? tal)
		  #t
		  ;; reset to top
		  (loop (car nodelst)(cdr nodelst) 1 (list top)))) 
	    (if (null? tal) ;; if null here then this path has already been added
		#t
		(loop (car tal)(cdr tal)(+ depth 1) newpath))))))))

(define (tree-node->path obj nodenum)
  ;; (print "\ncurrnode  nodenum  depth  node-depth  node-title   path")
  (let loop ((currnode 0)
	     (depth    0)
	     (path     '()))
    (let ((node-depth (iup:attribute obj (conc "DEPTH" currnode)))
	  (node-title (iup:attribute obj (conc "TITLE" currnode))))
      ;; (display (conc "\n   "currnode "        " nodenum "       " depth "         " node-depth "          " node-title "         " path))
      (if (> currnode nodenum)
	  path
	  (if (not node-depth) ;; #f if we are out of nodes
	      '()
	      (let ((ndepth (string->number node-depth)))
		(if (eq? ndepth depth)
		    ;; This next is the match condition depth == node-depth
		    (if (eq? currnode nodenum)
			(begin
			  ;; (display " <X>")
			  (append path (list node-title)))
			(loop (+ currnode 1)
			      (+ depth 1)
			      (append path (list node-title))))
		    ;; didn't match, reset to base path and keep looking
		    ;; due to more iup odditys we don't reset to base
		    (begin 
		      ;; (display " <L>")
		      (loop (+ 1 currnode)
			    2
			    (append (take path ndepth)(list node-title)))))))))))

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
			    #:numcol 5
			    #:numlin 50
			    #:numcol-visible 5
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
    (iup:attribute-set! steps-matrix "0:3" "End")
    (iup:attribute-set! steps-matrix "WIDTH3" "50")
    (iup:attribute-set! steps-matrix "0:4" "Status")
    (iup:attribute-set! steps-matrix "WIDTH4" "50")
    (iup:attribute-set! steps-matrix "0:5" "Log File")
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
	    
    (iup:vbox
     (iup:hbox
      run-info-matrix
      test-info-matrix)
     (iup:hbox
      test-run-matrix
      meta-dat-matrix)
     (iup:vbox
      (iup:vbox
       (iup:hbox 
	(iup:button "View Log"    #:action viewlog     #:size "80x")
	(iup:button "Start Xterm" #:action xterm       #:size "80x")
	(iup:button "Run Test"    #:action run-test    #:size "80x")
	(iup:button "Clean Test"  #:action remove-test #:size "80x"))
       (apply 
	iup:hbox
	(list command-text-box command-launch-button))))
     (iup:vbox
      (let ((tabs (iup:tabs
		   steps-matrix
		   data-matrix)))
	(iup:attribute-set! tabs "TABTITLE0" "Test Steps")
	(iup:attribute-set! tabs "TABTITLE1" "Test Data")
	tabs)))))
       
;; Test browser
(define (tests window-id)
  (iup:hbox 
   (let* ((tb      (iup:treebox
		    #:selection-cb
		    (lambda (obj id state)
		      ;; (print "obj: " obj ", id: " id ", state: " state)
		      (let* ((run-path (tree-node->path obj id))
			     (test-id  (tree-path->test-id (cdr run-path))))
			(if test-id
			    (hash-table-set! (dboard:data-get-curr-test-ids *data*)
					     window-id test-id))
			(print "path: " (tree-node->path obj id) " test-id: " test-id))))))
     (iup:attribute-set! tb "VALUE" "0")
     (iup:attribute-set! tb "NAME" "Runs")
     ;;(iup:attribute-set! tb "ADDEXPANDED" "NO")
     (dboard:data-set-tests-tree! *data* tb)
     tb)
   (test-panel window-id)))

;; The function to update the fields in the test view panel
(define (test-update window-id testdat run-info-matrix test-info-matrix test-run-matrix meta-dat-matrix steps-matrix data-matrix)
  ;; get test-id
  ;; then get test record
  (if testdat
      (let* ((test-id      (hash-table-ref/default (dboard:data-get-curr-test-ids *data*) window-id #f))
	     (test-data    (hash-table-ref/default testdat test-id #f))
	     (targ/runname (hash-table-ref/default (dboard:data-get-run-keys *data*) 
						   (db:test-get-run_id test-data) '()))
	     (target       (if (null? targ/runname) "" (string-intersperse (reverse (cdr (reverse targ/runname))) "/")))
	     (runname      (if (null? targ/runname) "" (car (cdr targ/runname)))))
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
		;;(list meta-dat-matrix
		;;      (if test-id
		;;	  (list (
		)))))))

  
;; db:test-get-id           
;; db:test-get-run_id       
;; db:test-get-testname     
;; db:test-get-state        
;; db:test-get-status       
;; db:test-get-event_time   
;; db:test-get-host         
;; db:test-get-cpuload      
;; db:test-get-diskfree     
;; db:test-get-uname        
;; db:test-get-rundir       
;; db:test-get-item-path    
;; db:test-get-run_duration 
;; db:test-get-final_logf   
;; db:test-get-comment      
;; db:test-get-fullname     	  


;;======================================================================
;; R U N   C O N T R O L
;;======================================================================

;; Overall runs browser
;;
(define (runs window-id)
  (let* ((runs-matrix     (iup:matrix
			   #:expand "YES"
			   ;; #:fittosize "YES"
			   #:scrollbar "YES"
			   #:numcol 100
			   #:numlin 100
			   #:numcol-visible 7
			   #:numlin-visible 7
			   #:click-cb (lambda (obj lin col status)
					(print "obj: " obj " lin: " lin " col: " col " status: " status)))))

    (iup:attribute-set! runs-matrix "RESIZEMATRIX" "YES")
    (iup:attribute-set! runs-matrix "WIDTH0" "100")

    (dboard:data-set-runs-matrix! *data* runs-matrix)
    (iup:hbox
     (iup:frame 
      #:title "Runs browser"
      (iup:vbox
       runs-matrix)))))

;; Browse and control a single run
;;
(define (runcontrol window-id)
  (iup:hbox))

;;======================================================================
;; P R O C E S S   R U N S
;;======================================================================

;; MOVE THIS INTO *data*
(define *cachedata* (make-hash-table))
(hash-table-set! *cachedata* "runid-to-col"    (make-hash-table))
(hash-table-set! *cachedata* "testname-to-row" (make-hash-table))

;; TO-DO
;;  1. Make "data" hash-table hierarchial store of all displayed data
;;  2. Update synchash to understand "get-runs", "get-tests" etc.
;;  3. Add extraction of filters to synchash calls
;;
;; Mode is 'full or 'incremental for full refresh or incremental refresh
(define (run-update keys data runname keypatts testpatt states statuses mode window-id)
  (let* (;; count and offset => #f so not used
	 ;; the synchash calls modify the "data" hash
	 (get-runs-sig    (conc (client:get-signature) " get-runs"))
	 (get-tests-sig   (conc (client:get-signature) " get-tests"))
	 (get-details-sig (conc (client:get-signature) " get-test-details"))

	 ;; test-ids to get and display are indexed on window-id in curr-test-ids hash
	 (test-ids        (hash-table-values (dboard:data-get-curr-test-ids *data*)))

 	 (run-changes     (synchash:client-get 'db:get-runs get-runs-sig (length keypatts) data runname #f #f keypatts))
	 (tests-detail-changes (if (not (null? test-ids))
				   (synchash:client-get 'db:get-test-info-by-ids get-details-sig 0  data test-ids)
				   '()))

	 ;; Now can calculate the run-ids
	 (run-hash    (hash-table-ref/default data get-runs-sig #f))
	 (run-ids     (if run-hash (filter number? (hash-table-keys run-hash)) '()))

	 (test-changes (synchash:client-get 'db:get-tests-for-runs-mindata get-tests-sig 0 data run-ids testpatt states statuses))
	 (runs-hash    (hash-table-ref/default data get-runs-sig #f))
	 (header       (hash-table-ref/default runs-hash "header" #f))
	 (run-ids      (sort (filter number? (hash-table-keys runs-hash))
			     (lambda (a b)
			       (let* ((record-a (hash-table-ref runs-hash a))
				      (record-b (hash-table-ref runs-hash b))
				      (time-a   (db:get-value-by-header record-a header "event_time"))
				      (time-b   (db:get-value-by-header record-b header "event_time")))
				 (> time-a time-b)))
			     ))
	 (runid-to-col    (hash-table-ref *cachedata* "runid-to-col"))
	 (testname-to-row (hash-table-ref *cachedata* "testname-to-row")) 
	 (colnum       1)
	 (rownum       0)) ;; rownum = 0 is the header
;; (debug:print 0 "test-ids " test-ids ", tests-detail-changes " tests-detail-changes)
    
	 ;; tests related stuff
	 ;; (all-testnames (delete-duplicates (map db:test-get-testname test-changes))))

    ;; Given a run-id and testname/item_path calculate a cell R:C

    ;; NOTE: Also build the test tree browser and look up table
    ;;
    ;; Each run is unique on its keys and runname or run-id, store in hash on colnum
    (for-each (lambda (run-id)
		(let* ((run-record (hash-table-ref/default runs-hash run-id #f))
		       (key-vals   (map (lambda (key)(db:get-value-by-header run-record header key))
					  (map key:get-fieldname keys)))
		       (run-name   (db:get-value-by-header run-record header "runname"))
		       (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
		       (run-path   (append key-vals (list run-name))))
		  (hash-table-set! (dboard:data-get-run-keys *data*) run-id run-path)
		  (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
				      (conc rownum ":" colnum) col-name)
		  (hash-table-set! runid-to-col run-id (list colnum run-record))
		  ;; Here we update the tests treebox and tree keys
		  (tree-add-node (dboard:data-get-tests-tree *data*) "Runs" (append key-vals (list run-name))
				 userdata: (conc "run-id: " run-id))
		  (set! colnum (+ colnum 1))))
	      run-ids)

    ;; Scan all tests to be displayed and organise all the test names, respecting what is in the hash table
    ;; Do this analysis in the order of the run-ids, the most recent run wins
    (for-each (lambda (run-id)
		(let* ((run-path       (hash-table-ref (dboard:data-get-run-keys *data*) run-id))
		       (new-test-dat   (car test-changes))
		       (removed-tests  (cadr test-changes))
		       (tests          (sort (map cadr (filter (lambda (testrec)
								 (eq? run-id (db:mintest-get-run_id (cadr testrec))))
							       new-test-dat))
					     (lambda (a b)
					       (let ((time-a (db:mintest-get-event_time a))
						     (time-b (db:mintest-get-event_time b)))
						 (> time-a time-b)))))
		       ;; test-changes is a list of (( id record ) ... )
		       ;; Get list of test names sorted by time, remove tests
		       (test-names (delete-duplicates (map (lambda (t)
							     (let ((i (db:mintest-get-item_path t))
								   (n (db:mintest-get-testname  t)))
							       (if (string=? i "")
								   (conc "   " i)
								   n)))
							   tests)))
		       (colnum     (car (hash-table-ref runid-to-col run-id))))
		  ;; for each test name get the slot if it exists and fill in the cell
		  ;; or take the next slot and fill in the cell, deal with items in the
		  ;; run view panel? The run view panel can have a tree selector for
		  ;; browsing the tests/items

		  ;; SWITCH THIS TO USING CHANGED TESTS ONLY
		  (for-each (lambda (test)
			      (let* ((test-id   (db:mintest-get-id test))
				     (state     (db:mintest-get-state test))
				     (status    (db:mintest-get-status test))
				     (testname  (db:mintest-get-testname test))
				     (itempath  (db:mintest-get-item_path test))
				     (fullname  (conc testname "/" itempath))
				     (dispname  (if (string=? itempath "") testname (conc "   " itempath)))
				     (rownum    (hash-table-ref/default testname-to-row fullname #f))
				     (test-path (append run-path (if (equal? itempath "") 
								     (list testname)
								     (list testname itempath)))))
				(tree-add-node (dboard:data-get-tests-tree *data*) "Runs" 
					       test-path
					       userdata: (conc "test-id: " test-id))
				(hash-table-set! (dboard:data-get-path-test-ids *data*) test-path test-id)
				(if (not rownum)
				    (let ((rownums (hash-table-values testname-to-row)))
				      (set! rownum (if (null? rownums)
						       1
						       (+ 1 (apply max rownums))))
				      (hash-table-set! testname-to-row fullname rownum)
				      ;; create the label
				      (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
							  (conc rownum ":" 0) dispname)
				      ))
				;; set the cell text and color
				;; (debug:print 2 "rownum:colnum=" rownum ":" colnum ", state=" status)
				(iup:attribute-set! (dboard:data-get-runs-matrix *data*)
						    (conc rownum ":" colnum)
						    (if (string=? state "COMPLETED")
							status
							state))
				(iup:attribute-set! (dboard:data-get-runs-matrix *data*)
						    (conc "BGCOLOR" rownum ":" colnum)
						    (gutils:get-color-for-state-status state status))
				))
			    tests)))
	      run-ids)

    (let ((updater (hash-table-ref/default  (dboard:data-get-updaters *data*) window-id #f)))
      (if updater (updater (hash-table-ref/default data get-details-sig #f))))

    (iup:attribute-set! (dboard:data-get-runs-matrix *data*) "REDRAW" "ALL")
    ;; (debug:print 2 "run-changes: " run-changes)
    ;; (debug:print 2 "test-changes: " test-changes)
    (list run-changes test-changes)))

;;======================================================================
;; D A S H B O A R D
;;======================================================================

;; Main Panel
(define (main-panel window-id)
  (iup:dialog
   #:title "Megatest Control Panel"
   #:menu (main-menu)
   (let ((tabtop (iup:tabs 
		  (runs window-id)
		  (tests window-id)
		  (runcontrol window-id)
		  (mtest window-id) 
		  (rconfig window-id)
		  )))
     (iup:attribute-set! tabtop "TABTITLE0" "Runs")
     (iup:attribute-set! tabtop "TABTITLE1" "Tests")
     (iup:attribute-set! tabtop "TABTITLE2" "Run Control")
     (iup:attribute-set! tabtop "TABTITLE3" "megatest.config") 
     (iup:attribute-set! tabtop "TABTITLE4" "runconfigs.config")
     tabtop)))

(define *current-window-id* 0)

(define (newdashboard)
  (let* ((data     (make-hash-table))
	 (keys     (cdb:remote-run db:get-keys #f))
	 (runname  "%")
	 (testpatt "%")
	 (keypatts (map (lambda (k)(list (vector-ref k 0) "%")) keys))
	 (states   '())
	 (statuses '())
	 (nextmintime (current-milliseconds))
	 (my-window-id *current-window-id*))
    (set! *current-window-id* (+ 1 *current-window-id*))
    (dboard:data-set-runs! *data* data) ;; make this data available to the rest of the application
    (iup:show (main-panel my-window-id))
    ;; Yes, running iup:show will pop up a new panel
    ;; (iup:show (main-panel my-window-id))
    (iup:callback-set! *tim*
		       "ACTION_CB"
		       (lambda (x)
			 ;; Want to dedicate no more than 50% of the time to this so skip if
			 ;; 2x delta time has not passed since last query
			 (if (< nextmintime (current-milliseconds))
			     (let* ((starttime (current-milliseconds))
				    (changes   (run-update keys data runname keypatts testpatt states statuses 'full my-window-id))
				    (endtime   (current-milliseconds)))
			       (set! nextmintime (+ endtime (* 2 (- endtime starttime))))
			       (debug:print 11 "CHANGE(S): " (car changes) "..."))
			     (debug:print-info 11 "Server overloaded"))))))

(newdashboard)    
(iup:main-loop)
