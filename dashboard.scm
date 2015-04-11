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


;;======================================================================
;; T E S T S
;;======================================================================


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
