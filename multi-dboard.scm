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

(use format numbers sql-de-lite srfi-1 posix regex regex-case srfi-69 nanomsg srfi-18 call-with-environment-variables)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)

(declare (uses margs))
(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses tree))
(declare (uses configf))
(declare (uses portlogger))
(declare (uses keys))
(declare (uses common))

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
(define *windows* (make-hash-table))
(define *changed-main* (make-hash-table)) ;; set path/... => #t
(define *changed-mutex* (make-mutex))     ;; use for all incoming change requests
(define *searchpatts*   (make-hash-table))

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

;; NOTE: Consider switching to defstruct.

;; data for an area (regression or testsuite)
;;
(define-record areadat
  name               ;; area name
  path               ;; mt run area home
  configdat          ;; megatest config
  denoise            ;; focal point for not putting out same messages over and over
  client-signature   ;; key for client-server conversation
  remote             ;; hash of all the client side connnections
  run-keys           ;; target keys for this area
  runs               ;; used in dashboard, hash of run-ids -> rundat
  read-only          ;; can I write to this area?
  monitordb          ;; db handle for monitor.db
  maindb             ;; db handle for main.db
  )

;; rundat, basic run data
;;
(define-record rundat
  id                 ;; the run-id
  target             ;; val1/val2 ... corrosponding to run-keys in areadat
  runname
  state              ;; state of the run, symbol 
  status             ;; status of the run, symbol
  event-time         ;; when the run was initiated
  tests              ;; hash of test-id -> testdat, QUESTION: separate by run-id?
  db                 ;; db handle
  )

;; testdat, basic test data
(define-record testdat
  run-id             ;; what run is this from
  id                 ;; test id
  testname           ;; test name
  itempath           ;; item path
  state              ;; test state, symbol
  status             ;; test status, symbol
  event-time         ;; when the test started
  duration           ;; how long the test took
  )

;; general data for the dboard application
;;
(define-record data
  cfgdat             ;; data from ~/.megatest/<group>.dat
  areas              ;; hash of areaname -> area-rec
  current-window-id  ;; 
  current-tab-id     ;; 
  update-needed      ;; flag to indicate that the tab pointed to by current tab id needs refreshing immediately
  tabs               ;; hash of tab-id -> areaname (??) should be of type "tab"
  )

;; all the components of an area display, all fits into a tab but
;; parts may be swapped in/out as needed
;;
(define-record tab
  tree
  matrix    ;; the spreadsheet 
  areadat   ;; the one-structure (one day dbstruct will be put in here)
  view-path ;; <target/path>/<runname>/...
  view-type ;; standard, etc.
  controls  ;; the controls
  data      ;; all the data kept in sync with db
  filters   ;; user filters, alist name -> filter record, eventually store these in ~/.megatest/<group>.dat?
  run-id    ;; the current run-id
  test-ids  ;; the current test id hash, run-id => test-id
  command   ;; the command from the entry field
  headers   ;; hash of header  -> colnum
  rows      ;; hash of rowname -> rownum
  )

(define-record filter
  target    ;; hash of widgets for the target
  runname   ;; the runname widget
  testpatt  ;; the testpatt widget
  )

;;======================================================================
;; D B
;;======================================================================

;; These are all using sql-de-lite and independent of area so cannot use stuff 
;; from db.scm

;; NB// run-id=#f => return dbdir only
;;
(define (areadb:dbfile-path areadat run-id)
  (let* ((cfgdat  (areadat-configdat areadat))
	 (dbdir   (or (configf:lookup cfgdat "setup" "dbdir")
		      (conc (configf:lookup cfgdat "setup" "linktree") "/.db")))
	 (fname   (if run-id
		      (case run-id
			((-1) "monitor.db")
			((0) "main.db")
			(else (conc run-id ".db")))
		      #f)))
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "ERROR: Couldn't create path to " dbdir)
       (exit 1))
     (if (not (directory? dbdir))(create-directory dbdir #t)))
    (if fname
	(conc dbdir "/" fname)
	dbdir)))

;; -1 => monitor.db
;;  0 => main.db
;; >1 => <run-id>.db
;;
(define (areadb:open areadat run-id)
  (let* ((runs   (areadat-runs areadat))
	 (rundat (if (> run-id 0) ;; it is a run
		     (hash-table-ref/default runs run-id #f)
		     #f))
	 (db     (case run-id ;; if already opened, get the db and return it
		   ((-1) (areadat-monitordb areadat))
		   ((0)  (areadat-maindb    areadat))
		   (else (if rundat
			     (rundat-db rundat)
			     #f)))))
    (if db
	db ;; merely return the already opened db
	(let* ((dbfile (areadb:dbfile-path areadat run-id)) ;; not already opened, so open it
	       (db     (if (file-exists? dbfile)
			   (open-database dbfile)
			   (begin
			     (debug:print 0 "ERROR: I was asked to open " dbfile ", but file does not exist or is not readable.")
			     #f))))
	  (case run-id
	    ((-1)(areadat-monitordb-set! areadat db))
	    ((0) (areadat-maindb-set!    areadat db))
	    (else (rundat-db-set!        rundat  db)))
	  db))))

;; populate the areadat tests info, does NOT fill the tests data itself unless asked
;;
(define (areadb:populate-run-info areadat)
  (let* ((runs   (or (areadat-runs areadat) (make-hash-table)))
	 (keys   (areadat-run-keys areadat))
	 (maindb (areadb:open areadat 0)))
    (if maindb
	(query (for-each-row (lambda (row)
			       (let ((id  (list-ref row 0))
				     (dat (apply make-rundat (append row (list #f #f))))) ;; add placeholders for tests and db
				 (print row)
				 (hash-table-set! runs id dat))))
	       (sql maindb (conc "SELECT id,"
				 (string-intersperse keys "||'/'||")
				 ",runname,state,status,event_time FROM runs WHERE state != 'deleted';")))
	(debug:print 0 "ERROR: no main.db found at "  (areadb:dbfile-path areadat 0)))
    areadat))

;; given an areadat and target/runname patt fill up runs data
;;
;; ?????/

;; given a list of run-ids refresh/retrieve runs data into areadat
;;
(define (areadb:fill-tests areadat #!key (run-ids #f))
  (let* ((runs   (or (areadat-runs areadat) (make-hash-table))))
    (for-each
     (lambda (run-id)
       (let* ((rundat (hash-table-ref/default runs run-id #f))
	      (tests  (if (and rundat
			       (rundat-tests rundat)) ;; re-use existing hash table?
			  (rundat-tests rundat)
			  (let ((ht (make-hash-table)))
			    (rundat-tests-set! rundat ht)
			    ht)))
	      (rundb  (areadb:open areadat run-id)))
	 (query (for-each-row (lambda (row)
				(let* ((id         (list-ref row 0))
				       (testname   (list-ref row 1))
				       (itempath   (list-ref row 2))
				       (state      (list-ref row 3))
				       (status     (list-ref row 4))
				       (eventtim   (list-ref row 5))
				       (duration   (list-ref row 6)))
				  (hash-table-set! tests id
						   (make-testdat run-id id testname itempath state status eventtim duration)))))
		(sql rundb "SELECT id,testname,item_path,state,status,event_time,run_duration FROM tests WHERE state != 'DELETED';"))))
     (or run-ids (hash-table-keys runs)))
    areadat))
    

;; initialize and refresh data
;;		
(define (dboard:general-updater con port)
  (for-each
   (lambda (window-id)
     ;; (print "Processing for window-id " window-id)
     (let* ((window-dat     (hash-table-ref *windows* window-id))
	    (areas          (data-areas     window-dat))
	    ;; (keys           (areadat-run-keys area-dat))
	    (tabs           (data-tabs      window-dat))
	    (tab-ids        (hash-table-keys tabs))
	    (current-tab    (if (null? tab-ids)
				#f
				(hash-table-ref tabs (car tab-ids))))
	    (current-tree   (if (null? tab-ids) #f (tab-tree   current-tab)))
	    (current-node   (if (null? tab-ids) 0  (string->number (iup:attribute current-tree "VALUE"))))
	    (current-path   (if (eq? current-node 0)
				"Areas"
				(string-intersperse (tree:node->path current-tree current-node) "/")))
	    (current-matrix (if (null? tab-ids) #f (tab-matrix current-tab)))
	    (seen-nodes     (make-hash-table))
	    (path-changed   (if current-tab
				(equal? current-path (tab-view-path current-tab))
				#t)))
       ;; (debug:print-info 0 "Current path: " current-path)
       ;; now for each area in the window gather the data
       (if path-changed
	   (begin
	     (debug:print-info 0 "clearing matrix - path changed")
	     (dboard:clear-matrix current-tab)))
       (for-each
	(lambda (area-name)
	  ;; (print "Processing for area-name " area-name)
	  (let* ((area-dat  (hash-table-ref areas area-name))
		 (area-path (areadat-path   area-dat))
		 (runs      (areadat-runs   area-dat)))
	    (if (hash-table-ref/default *changed-main* area-path 'processed)
		(begin
		  (print "Processing " area-dat " for area-name " area-name)
		  (hash-table-set! *changed-main* area-path #f)
		  (areadb:populate-run-info area-dat)
		  (for-each 
		   (lambda (run-id)
		     (let* ((run     (hash-table-ref runs run-id))
			    (target  (rundat-target run))
			    (runname (rundat-runname run)))
		       (if current-tree
			   (let* ((partial-path (append (string-split target "/")(list runname)))
				  (full-path    (cons area-name partial-path)))
			     (if (not (hash-table-exists? seen-nodes full-path))
				 (begin
				   (print "INFO: Adding node " partial-path " to section " area-name)
				   (tree:add-node current-tree "Areas" full-path)
				   (areadb:fill-tests area-dat run-ids: (list run-id))))
				   (hash-table-set! seen-nodes full-path #t)))))
		   (hash-table-keys runs))))
	    (if (or (equal? "Areas" current-path)
		    (string-match (conc "^Areas/" area-name "(|\\/.*)$") current-path))
		(dboard:redraw-area area-name area-dat current-tab current-matrix current-path))))
	(hash-table-keys areas))))
   (hash-table-keys *windows*)))

;;======================================================================
;; D A S H B O A R D   D B 
;;======================================================================

;; All moved to common.scm		

;;======================================================================
;; T R E E 
;;======================================================================

;; <area> - <target - ... > - <runname> - <test> - <itempath - ...>

(define (dashboard:tree-browser data adat window-id)
  ;; (iup:split
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:title "Areas"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((tree-path (tree:node->path obj id))
			    (area      (car tree-path))
			    (areadat-path (cdr tree-path)))
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
  (let* (;; (tab-dat         (areadat-
	 (view-matrix     (iup:matrix
			   ;; (runs-for-targ (db:get-runs-by-patt *dbstruct-local* *keys* "%" target #f #f #f))
			   #:expand "YES"
			   ;; #:fittosize "YES"
			   #:resizematrix "YES"
			   #:scrollbar "YES"
			   #:numcol 100
			   #:numlin 100
			   #:numcol-visible 3
			   #:numlin-visible 20
			   #:click-cb (lambda (obj lin col status)
					(print "obj: " obj " lin: " lin " col: " col " status: " status " value: " (iup:attribute obj "VALUE"))))))
    
    ;; (iup:attribute-set! view-matrix "RESIZEMATRIX" "YES")
    (iup:attribute-set! view-matrix "WIDTH0" "100")
    ;; (dboard:data-set-runs-matrix! *data* runs-matrix)
    ;; (iup:hbox
    ;;  (iup:frame 
    ;;   #:title "Runs browser"
    ;;   (iup:vbox
    view-matrix))

;;======================================================================
;; A R E A S
;;======================================================================

(define (dashboard:init-area data area-name apath)
  (let* ((mtconf      (dboard:read-mtconf apath))
	 (area-dat    (let ((ad (make-areadat
				 area-name ;; area name
				 apath     ;; path to area
				 ;; 'http     ;; transport
				 mtconf    ;; megatest.config
				 (make-hash-table) ;; denoise hash
				 #f        ;; client-signature
				 #f        ;; remote connections
				 (keys:config-get-fields mtconf) ;; run keys
				 (make-hash-table) ;; run-id -> (hash of test-ids => dat)
				 (and (file-exists? apath)(file-write-access? apath)) ;; read-only
				 #f
				 #f
				 )))
			(hash-table-set! (data-areas data) area-name ad)
			ad)))
    area-dat))

;; given the keys for an area and a path from the tree browser
;; return the level: areas area runs run tests test
;;
(define (dboard:get-view-type keys current-path)
  (let* ((path-parts (string-split current-path "/"))
	 (path-len   (length path-parts)))
    (cond
     ((equal? current-path "Areas")     'areas)
     ((eq? path-len 2)                  'area)
     ((<= (+ (length keys) 2) path-len) 'runs)
     (else                              'run))))

(define (dboard:clear-matrix tab)
  (if tab
      (begin
	(iup:attribute-set! (tab-matrix tab) "CLEARVALUE" "ALL")
	(tab-headers-set! tab (make-hash-table))
	(tab-rows-set!    tab (make-hash-table)))))

;; full redraw of a given area
;;
(define (dboard:redraw-area area-name area-dat tab-dat current-matrix current-path)
  (let* ((keys      (areadat-run-keys area-dat))
	 (runs      (areadat-runs     area-dat))
	 (headers   (tab-headers   tab-dat))
	 (rows      (tab-rows      tab-dat))
	 (used-cols (hash-table-values headers))
	 (used-rows (hash-table-values rows))
	 (touched   (make-hash-table)) ;; (vector row col) ==> true, touched cell
	 (view-type (dboard:get-view-type keys current-path))
	 (changed   #f)
	 (state-statuses  (list "PASS" "FAIL" "WARN" "CHECK" "SKIP" "RUNNING" "LAUNCHED")))
    ;; (debug:print 0 "current-matrix=" current-matrix)
    (case view-type
      ((areas) ;; find row for this area, if not found, create new entry
       (let* ((curr-rownum (hash-table-ref/default rows area-name #f))
	      (next-rownum (+ (apply max (cons 0 used-rows)) 1))
	      (rownum      (or curr-rownum next-rownum))
	      (coord       (conc rownum ":0")))
	 (if (not curr-rownum)(hash-table-set! rows area-name rownum))
	 (if (not (equal? (iup:attribute current-matrix coord) area-name))
	     (begin
	       (let loop ((hed  (car state-statuses))
			  (tal  (cdr state-statuses))
			  (count 1))
		 (if (not (equal? (iup:attribute current-matrix (conc "0:" count)) hed))
		     (iup:attribute-set! current-matrix (conc "0:" count) hed))
		 (iup:attribute-set! current-matrix (conc rownum ":" count) "0")
		 (if (not (null? tal))
		     (loop (car tal)(cdr tal)(+ count 1))))
	       (debug:print-info 0 "view-type=" view-type ", rownum=" rownum ", curr-rownum=" curr-rownum ", next-rownum=" next-rownum ", coord=" coord ", area-name=" area-name)
	       (iup:attribute-set! current-matrix coord area-name)
	       (set! changed #t))))))
    (if changed (iup:attribute-set! current-matrix "REDRAW" "ALL"))))
	     

       
   ;; (dboard:clear-matrix current-matrix used-cols used-rows touched) ;; clear all
    
	
  
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
		      (make-hash-table) ;; cached data? not sure how to use this yet :)
		      #f           ;; filters
		      #f           ;; the run-id
		      (make-hash-table) ;; run-id -> test-id, for current test id
		      ""
		      (make-hash-table) ;; headername -> colnum
		      (make-hash-table) ;; rowname    -> rownum
		      )))
    (hash-table-set! (data-areas data) aname area-dat) ;; dboard-dat)
    (hash-table-set! (data-tabs data) window-id dboard-dat)
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
	   (tabs     (data-tabs data)))
      (if (not (null? area-names))
	  (let loop ((index 0)
		     (hed   (car area-names))
		     (tal   (cdr area-names)))
	    ;; (hash-table-set! tabs index hed)
	    (debug:print 0 "Adding area " hed " with index " index " to dashboard")
	    (iup:attribute-set! tabtop (conc "TABTITLE" index) hed)
	    (if (not (null? tal))
		(loop (+ index 1)(car tal)(cdr tal)))))
      tabtop))))


;;======================================================================
;; N A N O M S G   S E R V E R
;;======================================================================

(define (dboard:server-service soc port)
  (print "server starting")
  (let loop ((msg-in (nn-recv soc))
	     (count  0))
    (if (eq? 0 (modulo count 1000))
	(print "server received: " msg-in ", count=" count))
    (cond
     ;;
     ;; quit
     ;;
     ((equal? msg-in "quit")
      (nn-send soc "Ok, quitting"))
     ;;
     ;; ping
     ;;
     ((and (>= (string-length msg-in) 4)
	   (equal? (substring msg-in 0 4) "ping"))
      (nn-send soc (conc (current-process-id)))
      (loop (nn-recv soc)(+ count 1)))
     ;;
     ;; main changed
     ;;
     ((and (>= (string-length msg-in) 4)
	   (equal? (substring msg-in 0 4) "main"))
      (let ((parts (string-split msg-in " ")))
	(hash-table-set! *changed-main* (cadr parts) #t)
	(nn-send soc "got it!")))
     ;;
     ;; ??
     ;;
     (else
      (nn-send soc "hello " msg-in " you got to the else clause!")))
    (loop (nn-recv soc)(if (> count 20000000)
			   0
			   (+ count 1)))))

(define (dboard:one-time-ping-receive soc port)
  (let ((msg-in (nn-recv soc)))
    (if (and (>= (string-length msg-in) 4)
	     (equal? (substring msg-in 0 4) "ping"))
	(nn-send soc (conc (current-process-id))))))

(define (dboard:server-start given-port #!key (num-tries 200))
  (let* ((rep (nn-socket 'rep))
	 (port (or given-port  (portlogger:main "find")))
	 (con (conc "tcp://*:" port)))
    ;; register this connect here ....
    (nn-bind rep con)
    (thread-start! 
     (make-thread (lambda ()
		    (dboard:one-time-ping-receive rep port))
		  "one time receive thread"))
    (if (dboard:ping-self "localhost" port)
	(begin
	  (print "INFO: dashboard nanomsg server started on " port)
	  (values rep port))
	(begin
	  (print "WARNING: couldn't create server on port " port)
	  (portlogger:main "set" "failed")
	  (if (> num-tries 0)
	      (dboard:server-start #f (- num-tries 1))
	      (begin
		(print "ERROR: failed to start nanomsg server")
		(values #f #f)))))))

(define (dboard:server-close con port)
  (nn-close con)
  (portlogger:main "set" port "released"))

(define (dboard:ping-self host port #!key (return-socket #t))
  ;; send a random number along with pid and check that we get it back
  (let* ((req     (nn-socket 'req))
	 (key     "ping")
	 (success #f)
	 (keepwaiting #t)
	 (ping    (make-thread
		   (lambda ()
		     (print "ping: sending string \"" key "\", expecting " (current-process-id))
		     (nn-send req key)
		     (let ((result  (nn-recv req)))
		       (if (equal? (conc (current-process-id)) result)
			   (begin
			     (print "ping, success: received \"" result "\"")
			     (set! success #t))
			   (begin
			     (print "ping, failed: received key \"" result "\"")
			     (set! keepwaiting #f)
			     (set! success #f)))))
		   "ping"))
	 (timeout (make-thread (lambda ()
				 (let loop ((count 0))
				   (thread-sleep! 1)
				   (print "still waiting after " count " seconds...")
				   (if (and keepwaiting (< count 10))
				       (loop (+ count 1))))
				 (if keepwaiting
				     (begin
				       (print "timeout waiting for ping")
				       (thread-terminate! ping))))
			       "timeout")))
    (nn-connect req (conc "tcp://" host ":" port))
    (handle-exceptions
     exn
     (begin
       (print-call-chain)
       (print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (print "exn=" (condition->list exn))
       (print "ping failed to connect to " host ":" port))
     (thread-start! timeout)
     (thread-start! ping)
     (thread-join! ping)
     (if success (thread-terminate! timeout)))
    (if return-socket
	(if success req #f)
	(begin
	  (nn-close req)
	  success))))

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

(define (dboard:read-mtconf apath)
  (let* ((mtconffile  (conc apath "/megatest.config")))
    (call-with-environment-variables
     (list (cons "MT_RUN_AREA_HOME" apath))
     (lambda ()
       (read-config mtconffile (make-hash-table) #f)) ;; megatest.config
     )))
	 

;;======================================================================
;; G U I   S T U F F 
;;======================================================================

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
    (hash-table-set! *windows* window-id data)
    (iup:show (dashboard:main-panel data window-id))
    (iup:main-loop)))

;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(define (main)
  (let-values 
      (((con port)(dboard:server-start #f)))
    (let ((portnum   (if (string? port)(string->number port) port)))
      ;; got here, monitor/dashboard was started
      (mddb:register-dashboard portnum)
      (thread-start! (make-thread (lambda ()(dboard:server-service con portnum)) "server service"))
      (thread-start! (make-thread (lambda ()
				    (let loop ()
				      (dboard:general-updater con portnum)
				      (thread-sleep! 1)
				      (loop))) "general updater"))
      (dboard:make-window 0)
      (mddb:unregister-dashboard (get-host-name) portnum)
      (dboard:server-close con port))))

