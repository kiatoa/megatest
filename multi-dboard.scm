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

(use sql-de-lite srfi-1 posix regex regex-case srfi-69)
;; (import (prefix sqlite3 sqlite3:))

;; (declare (unit multi-dboard))
(declare (uses margs))
;; (declare (uses launch))
(declare (uses megatest-version))
(declare (uses gutils))
;; (declare (uses db))
;; (declare (uses server))
;; (declare (uses synchash))
;; (declare (uses dcommon))
(declare (uses tree))
(declare (uses configf))

(include "common_records.scm")
;; (include "db_records.scm")
;; (include "key_records.scm")

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
;; R E C O R D S
;;======================================================================

;; data for an area (regression or testsuite)
;;
(define-record area
  name               ;; area name
  path               ;; mt run area home
  configdat          ;; megatest config
  denoise            ;; focal point for not putting out same messages over and over
  client-signature   ;; key for client-server conversation
  remote             ;; hash of all the client side connnections
  run-keys           ;; target keys for this area
  rundat             ;; used in dashboard
  read-only          ;; can I write to this area?
  )

;; general data for the dboard application
;;
(define-record data
  cfgdat             ;; data from ~/.megatest/<group>.dat
  areas              ;; hash of areaname -> area-rec
  current-window-id  ;; 
  current-tab-id     ;; 
  update-needed      ;; flag to indicate that the tab pointed to by current tab id needs refreshing immediately
  tab-ids            ;; hash of tab-id -> areaname
  )

;; all the components of an area display, all fits into a tab but
;; parts may be swapped in/out as needed
;;
(define-record tab
  tree
  matrix    ;; the spreadsheet 
  area-dat  ;; the one-structure (one day dbstruct will be put in here)
  view-path ;; <target/path>/<runname>/...
  view-type ;; standard, etc.
  controls  ;; the controls
  data      ;; all the data kept in sync with db
  filters   ;; user filters, alist name -> filter record, eventually store these in ~/.megatest/<group>.dat?
  run-id    ;; the current run-id
  test-ids  ;; the current test id hash, run-id => test-id
  command   ;; the command from the entry field
  )

(define-record filter
  target    ;; hash of widgets for the target
  runname   ;; the runname widget
  testpatt  ;; the testpatt widget
  )

;;======================================================================
;; T R E E 
;;======================================================================

;; <area> - <target - ... > - <runname> - <test> - <itempath - ...>

(define (dashboard:tree-browser data adat window-id)
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

;;======================================================================
;; M A I N   M A T R I X
;;======================================================================

;; General displayer
;;
(define (dashboard:main-matrix data adat window-id)
  (let* ((view-matrix     (iup:matrix
			   ;; (runs-for-targ (db:get-runs-by-patt *dbstruct-local* *keys* "%" target #f #f #f))
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

;;======================================================================
;; A R E A S
;;======================================================================

(define (dashboard:init-area data area-name apath)
  (let* ((mtconffile  (conc area-name "/megatest.config"))
	 (mtconf      (read-config mtconffile (make-hash-table) #f)) ;; megatest.config
	 (area-dat    (let ((ad (make-area
			       area-name ;; area name
			       apath     ;; path to area
			       ;; 'http     ;; transport
			       mtconf    ;; megatest.config
			       (make-hash-table) ;; denoise hash
			       #f        ;; client-signature
			       #f        ;; remote connections
			       #f        ;; run keys
			       (make-hash-table) ;; run-id -> (hash of test-ids => dat)
			       (and (file-exists? apath)(file-write-access? apath)) ;; read-only
			       )))
		      (hash-table-set! (data-areas data) area-name ad)
		      ad)))
    area-dat))

;;======================================================================
;; D A S H B O A R D
;;======================================================================

(define (dashboard:area-panel aname data window-id)
  (let* ((apath      (configf:lookup (data-cfgdat data) aname "path")) ;;  (hash-table-ref (dboard:data-cfgdat data) area-name))
	 ;;          (hash-table-ref (dboard:data-cfgdat data) aname))
	 (area-dat   (dashboard:init-area data aname apath))
	 (tb         (dashboard:tree-browser data area-dat window-id)) ;; (dboard:areas-tree-browser data)
	 (ad         (dashboard:main-matrix  data area-dat window-id))
	 (areas      (data-areas data))
	 (dboard-dat (make-tab
		      #f           ;; tree
		      #f           ;; matrix
		      area-dat     ;;
		      #f           ;; view path
		      'default     ;; view type
		      #f           ;; controls
		      #f           ;; cached data
		      #f           ;; filters
		      #f           ;; the run-id
		      (make-hash-table) ;; run-id -> test-id, for current test id
		      ""
		      )))
    (hash-table-set! (data-areas data) aname dboard-dat)
    (tab-tree-set!   dboard-dat tb)
    (tab-matrix-set! dboard-dat ad)
    (iup:split
     #:value 200
     tb ad)))


;; Main Panel
;;
(define (dashboard:main-panel data window-id)
  (iup:dialog
   #:title "Megatest Control Panel"
;;   #:menu (dcommon:main-menu data)
   #:shrink "YES"
   (iup:vbox
    (let* ((area-names  (hash-table-keys (data-cfgdat data)))
	   (area-panels (map (lambda (aname)
			       (dashboard:area-panel aname data window-id))
			     area-names))
	   (tabtop      (apply iup:tabs  
			       #:tabchangepos-cb (lambda (obj curr prev)
						   (data-current-tab-id-set! data curr)
						   (data-update-needed-set!  data #t)
						   (print "Tab is: " curr ", prev was " prev))
			       area-panels))
	   (tab-ids     (data-tab-ids data)))
      (if (not (null? area-names))
	  (let loop ((index 0)
		     (hed   (car area-names))
		     (tal   (cdr area-names)))
	    (hash-table-set! tab-ids index hed)
	    (debug:print 0 "Adding area " hed " with index " index " to dashboard")
	    (iup:attribute-set! tabtop (conc "TABTITLE" index) hed)
	    (if (not (null? tal))
		(loop (+ index 1)(car tal)(cdr tal)))))
      tabtop))))

;;======================================================================
;; C O N F I G U R A T I O N 
;;======================================================================

;; Get the configuration file for a group name, if the group name is "default" and it doesn't 
;; exist, create it and add the current path if it contains megatest.config
;;
(define (dboard:get-config group-name)
  (let* ((fname (conc (getenv "HOME") "/.megatest/" group-name ".dat")))
    (if (file-exists? fname)
	(read-config fname (make-hash-table) #t)
	(if (dboard:create-config fname)
	    (dboard:get-config group-name)
	    (make-hash-table)))))

(define (dboard:create-config fname)
  ;; (handle-exceptions
  ;;  exn
  ;;  
  ;;  #f ;; failed to create - just give up
   (let* ((dirname       (pathname-directory fname))
	  (file-name     (pathname-strip-directory fname))
	  (curr-mtcfgdat (find-config "megatest.config"
				      toppath: (or (get-environment-variable "MT_RUN_AREA_HOME")(current-directory))))
	  (curr-mtcfg    (if (and curr-mtcfgdat (not (null? curr-mtcfgdat)))(cadr curr-mtcfgdat) #f))
	  (curr-mtpath   (if curr-mtcfg (car curr-mtcfgdat) #f)))
     (if curr-mtpath
	 (begin
	   (debug:print-info 0 "Creating config file " fname)
	   (if (not (file-exists? dirname))
	       (create-directory dirname #t))
	   (with-output-to-file fname
	     (lambda ()
	       (let ((aname (pathname-strip-directory curr-mtpath)))
		 (print "[" aname "]")
		 (print  "path " curr-mtpath))))
	   #t)
	 (begin
	   (debug:print-info 0 "Need to create a config but no megatest.config found: " curr-mtcfgdat)
	   #f))))
;; )

;;; main. Theoretically could have multiple windows (each with a group of tags, thus window-id
;;;
(define (dboard:make-window window-id)
  (let* (;; (window-id 0)
	 (groupn    (or (args:get-arg "-group") "default"))
	 (cfgdat    (dboard:get-config groupn))
	 ;; (cfgdat    (if (file-exists? cfname)(read-config cfname (make-hash-table) #t)(make-hash-table)))
	 (data      (make-data
		     cfgdat ;; this is the data from ~/.megatest for the selected group
		     (make-hash-table) ;; areaname -> area-rec
		     0                 ;; current window id
		     0                 ;; current tab id
		     #f                ;; redraw needed for current tab id
		     (make-hash-table) ;; tab-id -> areaname
		     )))
    (iup:show (dashboard:main-panel data window-id))
    (iup:main-loop)))



;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(dboard:make-window 0)

