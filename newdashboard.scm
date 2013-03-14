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
							(show (iup:file-dialog))
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



(define (mtest)
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

(define (rconfig)
  (iup:vbox
   (iup:frame #:title "Default")))

(define (tests)
  (iup:hbox 
   (iup:frame #:title "Tests browser")))

(define (runs)
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
;;     (iup:attribute-set! keys-matrix "0:0" "Field Num")
;;     (iup:attribute-set! keys-matrix "0:1" "Field Name")
;;     (iup:attribute-set! keys-matrix "WIDTH1" "100")
;;     (iup:attribute-set! disks-matrix "0:0" "Disk Name")
;;     (iup:attribute-set! disks-matrix "0:1" "Disk Path")
;;     (iup:attribute-set! disks-matrix "WIDTH1" "120")
;;     (iup:attribute-set! disks-matrix "WIDTH0" "100")
;;     (iup:attribute-set! disks-matrix "ALIGNMENT1" "ALEFT")
;;     (iup:attribute-set! disks-matrix "FIXTOTEXT" "C1")
;;     (iup:attribute-set! disks-matrix "RESIZEMATRIX" "YES")
    ;; fill in keys
;;     (set! curr-row-num 1)
;;     (for-each 
;;      (lambda (var)
;;        (iup:attribute-set! keys-matrix (conc curr-row-num ":0") curr-row-num)
;;        (iup:attribute-set! keys-matrix (conc curr-row-num ":1") var)
;;        (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
;;      (configf:section-vars rawconfig "fields"))

    ;; fill in existing info
;;    (for-each 
;;     (lambda (mat fname)
;;       (set! curr-row-num 1)
;;       (for-each
;;	(lambda (var)
;;	  (iup:attribute-set! mat (conc curr-row-num ":0") var)
;;	  (iup:attribute-set! mat (conc curr-row-num ":1") (config-lookup rawconfig fname var))
;;	  (set! curr-row-num (+ curr-row-num 1)))
;;	(configf:section-vars rawconfig fname)))
;;     (list setup-matrix jobtools-matrix validvals-matrix envovrd-matrix disks-matrix)
;;     (list "setup"      "jobtools"      "validvalues"      "env-override" "disks"))

    (for-each
     (lambda (mat)
       (iup:attribute-set! mat "0:1" "ubuntu\nnfs\nnone")
       (iup:attribute-set! mat "0:0" "Test")
       (iup:attribute-set! mat "ALIGNMENT1" "ALEFT")
       ;; (iup:attribute-set! mat "FIXTOTEXT" "C1")
       (iup:attribute-set! mat "RESIZEMATRIX" "YES")
       (iup:attribute-set! mat "WIDTH1" "120")
       (iup:attribute-set! mat "WIDTH0" "100")
       )
     (list runs-matrix))

;;    (iup:attribute-set! validvals-matrix "WIDTH1" "290")
;;    (iup:attribute-set! envovrd-matrix   "WIDTH1" "290")
    
    (iup:hbox
     (iup:frame 
      #:title "Runs browser"
      (iup:vbox
       runs-matrix)))))

(define (main-panel)
  (iup:dialog
   #:title "Menu Test"
   #:menu (main-menu)
   (let ((tabtop (iup:tabs 
		  (runs)
		  (mtest) 
		  (rconfig)
		  (tests)
		  )))
     (iup:attribute-set! tabtop "TABTITLE0" "Runs")
     (iup:attribute-set! tabtop "TABTITLE3" "Tests")
     (iup:attribute-set! tabtop "TABTITLE1" "megatest.config") 
     (iup:attribute-set! tabtop "TABTITLE2" "runconfigs.config")
     tabtop)))

;;======================================================================
;; Process runs
;;======================================================================

;; TO-DO
;;  1. Make "data" hash-table hierarchial store of all displayed data
;;  2. Update synchash to understand "get-runs", "get-tests" etc.
;;  3. Add extraction of filters to synchash calls
;;
(define (run-update data runname keypatts testpatt states statuses)
  (let* (;; count and offset => #f so not used
	 ;; the synchash calls modify the "data" hash
	 (run-changes (synchash:client-get 'db:get-runs  "get-runs" (length keypatts) data runname #f #f keypatts))
	 ;; Now can calculate the run-ids
	 (run-hash    (hash-table-ref/default data "get-runs" #f))
	 (run-ids     (if run-hash (filter number? (hash-table-keys run-hash)) '()))
	 ;; (debug:print-info 2 "run-hash-keys: " (hash-table-keys run-hash))
	 ;; (debug:print-info 2 "run-hash: ")(pp (hash-table->alist run-hash))
	 ;; (debug:print-info 2 "run-ids: " run-ids)
	 (test-changes (synchash:client-get 'db:get-tests-for-runs "get-tests-for-runs" 0 data run-ids testpatt states statuses)))
    (list run-changes test-changes)))

(define (newdashboard)
  (let* ((data     (make-hash-table))
	 (keys     (cdb:remote-run db:get-keys #f))
	 (runname  "%")
	 (testpatt "%")
	 (keypatts (map (lambda (k)(list (vector-ref k 0) "%")) keys))
	 (states   '())
	 (statuses '()))
    (iup:show (main-panel))
    (iup:callback-set! *tim*
		       "ACTION_CB"
		       (lambda (x)
			 (let ((changes (run-update data runname keypatts testpatt states statuses)))
			   (debug:print 0 "CHANGES: " changes))))))

(newdashboard)    
(iup:main-loop)
