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

(use format)
(require-library iup)
(import (prefix iup iup:))

(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))

(declare (unit dashboard-guimonitor))
(declare (uses common))
(declare (uses keys))
(declare (uses db))
(declare (uses tasks))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "task_records.scm")

(define (control-panel db tdb keys)
  (let* ((var-params (make-hash-table)) ;; register all the widgets here for querying on run, rollup, remove?
	 (key-params (make-hash-table))
	 (monitordat '()) ;; list of monitor records
	 (keyentries (iup:frame 
		      #:title "Keys"
		      (apply
		       iup:vbox
		       (map (lambda (key)
			      (iup:hbox (iup:label (vector-ref key 0) #:size "60x15") ; #:expand "HORIZONTAL")
					(iup:textbox #:expand "HORIZONTAL"
						     #:action (lambda (obj a val)
								(hash-table-set! key-params (vector-ref key 0) val)))))
			    keys))))
	 (othervars  (iup:frame
		      #:title "Run Vars"
		      (apply
		       iup:vbox
		       (map (lambda (var)
			      (iup:hbox (iup:label var #:size "60x15")
					(iup:textbox   #:expand "HORIZONTAL"
						       #:action (lambda (obj a val)
								  (hash-table-set! var-params var val)))))
			    (list "runname" "testpatts" "itempatts")))))
	 (controls   (iup:frame
		      #:title "Controls"
		      (iup:hbox 
		       (iup:frame
			#:title "Runs"
			(iup:hbox 
			 (iup:button "Start"  
				     #:expand "HORIZONTAL"
				     #:action (lambda (obj)
						(tasks:add-from-params tdb "run" keys key-params var-params)
						(print "Launch Run")))
			 (iup:button "Remove" 
				     #:expand "HORIZONTAL"
				     #:action (lambda (obj)
						(print "Remove Run")))))
		       (iup:frame 
			#:title "Misc"
			(iup:hbox
			 (iup:button "Quit" 
				     #:expand "HORIZONTAL"
				     #:action (lambda (obj)
						(sqlite3:finalize! db)
						(sqlite3:finalize! tdb)
						(exit))))))))
	 (monitors     (iup:textbox 
			#:expand "YES" ; HORIZONTAL"
			; #:size   "x40"
			#:multiline "YES"
			#:font "Courier New, -10"
			#:value "None..............................................."))
	 (actions      (iup:textbox
			#:expand "YES"
			#:multiline "YES"
			#:font "Courier New, -10"
			#:value "None..............................................."))
	 (lastmodtime 0)
	 (next-touch  0) ;; the last time the "last_update" field was updated
	 (refreshdat (lambda ()
		       (let* ((monitordbpath  (conc *toppath* "/monitor.db"))
			      (megatestdbpath (conc *toppath* "/megatest.db"))
			      (modtime        (max (file-modification-time megatestdbpath)
						   (file-modification-time monitordbpath))))
			 ;; do stuff here when the db is updated by some other process
			 (if (> modtime lastmodtime)
			     (let ((tlst (tasks:get-tasks tdb '() '()))
				   (mlst (tasks:get-monitors tdb)))
			       (set! tasksdat tlst)
			       (set! monitorsdat mlst)
			       (iup:attribute-set! monitors "VALUE" (tasks:monitors->text-table mlst))
			       (iup:attribute-set! actions  "VALUE" (tasks:tasks->text tlst))
			       (tasks:process-queue db tdb)
			       (set! lastmodtime (max (file-modification-time megatestdbpath)
						      (file-modification-time monitordbpath)))
			       (tasks:reset-stuck-tasks tdb)))
			 ;; stuff to do every 10 seconds
			 (if (> (current-seconds) next-touch)
			     (begin
			       ;; (tasks:process-queue db tdb monitordbpath)
			       (tasks:monitors-update tdb)
			       (tasks:reset-stuck-tasks tdb)
			       (set! monitorsdat (tasks:get-monitors tdb))
			       (set! next-touch (+ (current-seconds) 10))
			       )))))
	 (topdialog  #f))
    (set! topdialog (iup:dialog 
		     #:close_cb (lambda (a)(exit))
		     #:title "Run Controls"
		     (iup:vbox
		      (iup:hbox keyentries othervars)
		      controls
		      (let ((tabtop (iup:tabs 
				     monitors
				     (iup:vbox 
				       (let* ((tb (iup:textbox #:expand "HORIZONTAL"))
					      (bt (iup:button "Remove tasks by id"
							      #:action (lambda (obj)
									 (let ((val (iup:attribute tb "VALUE")))
									   (tasks:remove-queue-entries tdb val)))))
					      (lb (iup:label "(comma separated)")))
					 (iup:hbox bt tb lb))
				      actions))))
			(iup:attribute-set! tabtop "TABTITLE0" "Monitors")
			(iup:attribute-set! tabtop "TABTITLE1" "Actions")
			tabtop)
		      )))
		      ; (iup:frame
		      ;  #:title "Monitors"
		      ;  monitors)
		      ; (iup:frame
		      ;  #:title "Actions"
		      ;  actions))))

    (iup:show topdialog)
    (iup:callback-set! *tim* "ACTION_CB"
		       (lambda (x)
			 (refreshdat)
			 (if *exit-started*
			     (set! *exit-started* 'ok))))))

(define (main-window setuptab fsltab collateraltab toolstab)
  (iup:show
   (iup:dialog #:title "FSL Power Window" #:size "290x190" ; #:expand "YES"
               (let ((tabtop (iup:tabs setuptab collateraltab fsltab toolstab)))
                 (iup:attribute-set! tabtop "TABTITLE0" "Setup") 
                 (iup:attribute-set! tabtop "TABTITLE1" "Collateral")
                 (iup:attribute-set! tabtop "TABTITLE2" "Fossil")
                 (iup:attribute-set! tabtop "TABTITLE3" "Tools")
                 tabtop))))

(on-exit (lambda ()
	   (let ((tdb (tasks:open-db)))
	     (print "On-exit called")
	     (tasks:remove-monitor-record tdb)
	     (sqlite3:finalize! tdb))))

(define (gui-monitor db)
  (let ((keys (get-keys db))
	(tdb  (tasks:open-db)))
    (tasks:register-monitor db tdb) ;;; let the other monitors know we are here
    (control-panel db tdb keys)
    ;(tasks:remove-monitor-record db)
    ;(sqlite3:finalize! db)
   ))
	
