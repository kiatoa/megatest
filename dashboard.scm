;;======================================================================
;; Copyright 2006-2016, Matthew Welland.
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
(import canvas-draw-iup)

(use sqlite3 srfi-1 posix regex regex-case srfi-69 typed-records sparse-vectors) ;; defstruct
(import (prefix sqlite3 sqlite3:))

(declare (uses common))
(declare (uses margs))
(declare (uses keys))
(declare (uses items))
(declare (uses db))
(declare (uses configf))
(declare (uses process))
(declare (uses launch))
(declare (uses runs))
(declare (uses dashboard-tests))
(declare (uses dashboard-guimonitor))
(declare (uses tree))
(declare (uses dcommon))
(declare (uses vg))

;; (declare (uses dashboard-main))
(declare (uses megatest-version))
(declare (uses mt))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "task_records.scm")
(include "megatest-fossil-hash.scm")
(include "vg_records.scm")

(define help (conc 
	      "Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2012-2016

Usage: dashboard [options]
  -h                    : this help
  -test run-id,test-id  : control test identified by testid
  -skip-version-check   : skip the version check

Misc
  -rows N         : set number of rows
"))

;;   -server host:port     : connect to host:port instead of db access
;;   -xterm run-id,test-id : Start a new xterm with specified run-id and test-id
;;   -guimonitor           : control panel for runs

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-rows"
			"-run"
			"-test"
                        "-xterm"
			"-debug"
			"-host" 
			"-transport"
			) 
		 (list  "-h"
			"-use-server"
			"-guimonitor"
			"-main"
			"-v"
			"-q"
			"-use-local"
			"-skip-version-check"
			)
		 args:arg-hash
		 0))

(if (args:get-arg "-h")
    (begin
      (print help)
      (exit)))

(if (not (launch:setup))
    (begin
      (print "Failed to find megatest.config, exiting") 
      (exit 1)))

;; data common to all tabs goes here
;;
(defstruct dboard:commondat
  curr-tab-num
  please-update  
  tabdats
  update-mutex
  updaters 
  updating
  uidat ;; needs to move to tabdat at some time
  hide-not-hide-tabs
  )

(define (dboard:commondat-make)
  (make-dboard:commondat
   curr-tab-num:         0
   tabdats:              (make-hash-table)
   please-update:        #t
   update-mutex:         (make-mutex)
   updaters:             (make-hash-table)
   updating:             #f
   hide-not-hide-tabs:   #f
   ))

(define (dboard:common-get-tabdat commondat #!key (tab-num #f))
  (hash-table-ref/default 
   (dboard:commondat-tabdats commondat)
   (or tab-num (dboard:commondat-curr-tab-num commondat))
   #f))

(define (dboard:common-set-tabdat! commondat tabnum tabdat)
  (hash-table-set!
   (dboard:commondat-tabdats commondat)
   tabnum
   tabdat))

;; gets and calls updater based on curr-tab-num
(define (dboard:common-run-curr-updaters commondat #!key (tab-num #f))
  (if (dboard:common-get-tabdat commondat tab-num: tab-num) ;; only update if there is a tabdat
      (let* ((tnum     (or tab-num (dboard:commondat-curr-tab-num commondat)))
	     (updaters (hash-table-ref/default (dboard:commondat-updaters commondat)
					       tnum
					       '())))
	(debug:print 4 *default-log-port* "Found these updaters: " updaters " for tab-num: " tnum)
	(for-each
	 (lambda (updater)
	   (debug:print 3 *default-log-port* "Running " updater)
	   (updater)
	   )

	 updaters))))

;; if tab-num passed in then use it, otherwise look in commondat at curr-tab-num
;;
(define (dboard:commondat-add-updater commondat updater #!key (tab-num #f))
  (let* ((tnum          (or tab-num
			     (dboard:commondat-curr-tab-num commondat)))
	 (curr-updaters (hash-table-ref/default (dboard:commondat-updaters commondat) tnum '())))
    (hash-table-set! (dboard:commondat-updaters commondat)
		     tnum
		     (cons updater curr-updaters))))

;; data for each specific tab goes here
;;
(defstruct dboard:tabdat 
  ;; runs
  ((allruns         '())                 : list)        ;; list of dboard:rundat records
  ((allruns-by-id    (make-hash-table))  : hash-table)  ;; hash of run-id -> dboard:rundat records
  ((done-runs       '())                 : list)        ;; list of runs already drawn
  ((not-done-runs   '())                 : list)        ;; list of runs not yet drawn
  (header            #f)                                ;; header for decoding the run records
  (keys              #f)                                ;; keys for this run (i.e. target components)
  ((numruns          16)                 : number)      ;; 
  ((tot-runs          0)                 : number)
  ((last-data-update  0)                 : number)      ;; last time the data in allruns was updated
  (runs-mutex         (make-mutex))                     ;; use to prevent parallel access to draw objects

  ;; Runs view
  ((buttondat         (make-hash-table)) : hash-table)  ;;     
  ((item-test-names  '())                : list)        
  ((run-keys          (make-hash-table)) : hash-table)
  (runs-matrix        #f)                               ;; used in newdashboard
  ((start-run-offset   0)                : number)      ;; left-right slider value
  ((start-test-offset  0)                : number)      ;; up-down slider value

  ;; Canvas and drawing data
  (cnv                #f)
  (cnv-obj            #f)
  (drawing            #f)
  ((run-start-row     0)                 : number)
  ((max-row           0)                 : number)
  ((running-layout    #f)                : boolean)
  (originx            #f)
  (originy            #f)
  ((layout-update-ok  #t)                : boolean)
  ((compact-layout    #t)                : boolean)

  ;; Controls used to launch runs etc.
  ((command          "")                 : string)      ;; for run control this is the command being built up
  (command-tb        #f)			         
  (key-listboxes     #f)			         
  (key-lbs           #f)			         
  run-name                                              ;; from run name setting widget
  states                                                ;; states for -state s1,s2 ...
  statuses                                              ;; statuses for -status s1,s2 ...
						         
  ;; Selector variables				         
  curr-run-id                                           ;; current row to display in Run summary view
  curr-test-ids                                         ;; used only in dcommon:run-update which is used in newdashboard
  ((filters-changed  #f)                 : boolean)     ;; to to indicate that the user changed filters for this tab
  ((last-filter-str  "")                 : string)      ;; conc the target runname and testpatt for a signature of changed filters
  ((hide-empty-runs  #f)                 : boolean)     
  ((hide-not-hide    #t)                 : boolean)     ;; toggle for hide/not hide empty runs
  (hide-not-hide-button #f)
  ((searchpatts      (make-hash-table))  : hash-table)  ;;
  ((state-ignore-hash (make-hash-table)) : hash-table)  ;; hash of  STATE => #t/#f for display control
  ((status-ignore-hash (make-hash-table)) : hash-table)  ;; hash of STATUS => #t/#f
  (target              #f)
  (test-patts          #f)

  ;; db info to file the .db files for the area
  (dbdir               #f)
  (dbfpath             #f)
  (dbkeys              #f)
  ((last-db-update     0)                : number)      ;; last db file timestamp
  (monitor-db-path     #f)                              ;; where to find monitor.db
  ro                                                    ;; is the database read-only?

  ;; tests data
  ((num-tests          10)               : number)      ;; total number of tests to show (used in the old runs display)

  ;; runs tree
  ((path-run-ids       (make-hash-table)) : hash-table) ;; path (target / runname) => id
  (runs-tree           #f)

  ;; tab data
  ((view-changed       #t)                : boolean)   
  ((xadj               0)                 : number)     ;; x slider number (if using canvas)
  ((yadj               0)                 : number)     ;; y slider number (if using canvas)

  tests-tree       ;; used in newdashboard
  )

(define (dboard:tabdat-target-string vec)
  (let ((targ (dboard:tabdat-target vec)))
    (if (list? targ)(string-intersperse targ "/") "no-target-specified")))

(define (dboard:tabdat-test-patts-use vec)    
  (let ((val (dboard:tabdat-test-patts vec)))(if val val "")))

;; additional setters for dboard:data
(define (dboard:tabdat-test-patts-set!-use    vec val)
  (dboard:tabdat-test-patts-set! vec (if (equal? val "") #f val)))

(define (dboard:tabdat-make-data)
  (let ((dat (make-dboard:tabdat)))
    (dboard:setup-tabdat dat)
    (dboard:setup-num-rows dat)
    dat))

(define (dboard:setup-tabdat tabdat)
  (dboard:tabdat-dbdir-set! tabdat (db:dbfile-path #f)) ;; (conc (configf:lookup *configdat* "setup" "linktree") "/.db"))
  (dboard:tabdat-dbfpath-set! tabdat (db:dbfile-path 0))
  (dboard:tabdat-monitor-db-path-set! tabdat (conc (dboard:tabdat-dbdir tabdat) "/monitor.db"))

  ;; HACK ALERT: this is a hack, please fix.
  (dboard:tabdat-ro-set! tabdat (not (file-read-access? (dboard:tabdat-dbfpath tabdat))))
  
  (dboard:tabdat-keys-set! tabdat (rmt:get-keys))
  (dboard:tabdat-dbkeys-set! tabdat (append (dboard:tabdat-keys tabdat) (list "runname")))
  (dboard:tabdat-tot-runs-set! tabdat (rmt:get-num-runs "%"))
  )

;; data for runs, tests etc. was used in run summary?
;;
(defstruct dboard:runsdat
  ;; new system
  runs-index    ;; target/runname => colnum
  tests-index   ;; testname/itempath => rownum
  matrix-dat    ;; vector of vectors rows/cols
  )

(define (dboard:runsdat-make-init)
  (make-dboard:runsdat
   runs-index: (make-hash-table)
   tests-index: (make-hash-table)
   matrix-dat: (make-sparse-array)))

;; used to keep the rundata from rmt:get-tests-for-run
;; in sync. 
;;
(defstruct dboard:rundat
  run
  tests-drawn    ;; list of id's already drawn on screen
  tests-notdrawn ;; list of id's NOT already drawn
  rowsused       ;; hash of lists covering what areas used - replace with quadtree
  hierdat        ;; put hierarchial sorted list here
  tests          ;; hash of id => testdat
  tests-by-name  ;; hash of testfullname => testdat
  key-vals
  last-update    ;; last query to db got records from before last-update
  data-changed
  )

(define (dboard:rundat-make-init #!key (run #f)(key-vals #f)(tests #f)(last-update -100));; -100 is before time began
  (make-dboard:rundat 
   run: run
   tests: (or tests (make-hash-table))
   tests-by-name: (make-hash-table)
   key-vals: key-vals 
   last-update: last-update
   data-changed: #t
   )) 

(define (dboard:rundat-copy-tests-to-by-name rundat)
  (let ((src-ht (dboard:rundat-tests rundat))
	(trg-ht (dboard:rundat-tests-by-name rundat)))
    (if (and (hash-table? src-ht)(hash-table? trg-ht))
	(for-each
	 (lambda (testdat)
	   (hash-table-set! trg-ht (test:test-get-fullname testdat) testdat))
	 (hash-table-values src-ht))
	(debug:print 0 *default-log-port* "WARNING: src-ht " src-ht " trg-ht " trg-ht))))
  
(defstruct dboard:testdat
  id       ;; testid
  state    ;; test state
  status   ;; test status
  )

(define (dboard:runsdat-get-col-num dat target runname force-set)
  (let* ((runs-index (dboard:runsdat-runs-index dat))
	 (col-name   (conc target "/" runname))
	 (res        (hash-table-ref/default runs-index col-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-col-num (+ 1 (apply max -1 (hash-table-values runs-index)))))
	      (hash-table-set! runs-index col-name max-col-num)
	      max-col-num)))))

(define (dboard:runsdat-get-row-num dat testname itempath force-set)
  (let* ((tests-index (dboard:runsdat-runs-index dat))
	 (row-name    (conc testname "/" itempath))
	 (res         (hash-table-ref/default runs-index row-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-row-num (+ 1 (apply max -1 (hash-table-values tests-index)))))
	      (hash-table-set! runs-index row-name max-row-num)
	      max-row-num)))))

;; default is to NOT set the cell if the column and row names are not pre-existing
;;
(define (dboard:runsdat-set-test-cell dat target runname testname itempath test-id state status #!key (force-set #f))
  (let* ((col-num  (dboard:runsdat-get-col-num dat target runname force-set))
	 (row-num  (dboard:runsdat-get-row-num dat testname itempath force-set)))
    (if (and row-num col-num)
	(let ((tdat (dboard:testdat 
		     id: test-id
		     state: state
		     status: status)))
	  (sparse-array-set! (dboard:runsdat-matrix-dat dat) col-num row-num tdat)
	  tdat)
	#f)))

(define *dashboard-mode* (string->symbol (or (configf:lookup *configdat* "dashboard" "mode") "dashboard")))
  

(define *exit-started* #f)

;; sorting global data (would apply to many testsuites so leave it global for now)
;;
(define *tests-sort-options* (vector (vector "Sort +a" 'testname   "ASC")
				     (vector "Sort -a" 'testname   "DESC")
				     (vector "Sort +t" 'event_time "ASC")
				     (vector "Sort -t" 'event_time "DESC")
				     (vector "Sort +s" 'statestatus "ASC")
				     (vector "Sort -s" 'statestatus "DESC")
				     (vector "Sort +a" 'testname   "ASC")))

(define *tests-sort-type-index* '(("+testname" 0)
				  ("-testname" 1)
				  ("+event_time" 2)
				  ("-event_time" 3)
				  ("+statestatus" 4)
				  ("-statestatus" 5)))

;; Don't forget to adjust the >= below if you add to the sort-options above
(define (next-sort-option)
  (if (>= *tests-sort-reverse* 5)
      (set! *tests-sort-reverse* 0)
      (set! *tests-sort-reverse* (+ *tests-sort-reverse* 1)))
  *tests-sort-reverse*)

(define *tests-sort-reverse* 
  (let ((t-sort (assoc (configf:lookup *configdat* "dashboard" "testsort") *tests-sort-type-index*)))
    (if t-sort
	(cadr t-sort)
	3)))

(define (get-curr-sort)
  (vector-ref *tests-sort-options* *tests-sort-reverse*))

(debug:setup)

;; (define uidat #f)

(define-inline (dboard:uidat-get-keycol  vec)(vector-ref vec 0))
(define-inline (dboard:uidat-get-lftcol  vec)(vector-ref vec 1))
(define-inline (dboard:uidat-get-header  vec)(vector-ref vec 2))
(define-inline (dboard:uidat-get-runsvec vec)(vector-ref vec 3))

(if (get-environment-variable "MT_RUN_AREA_HOME")(change-directory (get-environment-variable "MT_RUN_AREA_HOME")))

(define (message-window msg)
  (iup:show
   (iup:dialog
    (iup:vbox 
     (iup:label msg #:margin "40x40")))))

(define (iuplistbox-fill-list lb items #!key (selected-item #f))
  (let ((i 1))
    (for-each (lambda (item)
		(iup:attribute-set! lb (number->string i) item)
		(if selected-item
		    (if (equal? selected-item item)
			(iup:attribute-set! lb "VALUE" i))) ;; (number->string i))))
		(set! i (+ i 1)))
	      items)
    ;; (iup:attribute-set! lb "VALUE" (if selected-item selected-item ""))
    i))

(define (pad-list l n)(append l (make-list (- n (length l)))))

(define (colors-similar? color1 color2)
  (let* ((c1    (map string->number (string-split color1)))
	 (c2    (map string->number (string-split color2)))
	 (delta (map (lambda (a b)(abs (- a b))) c1 c2)))
    (null? (filter (lambda (x)(> x 3)) delta))))

(define (dboard:compare-tests test1 test2)
  (let* ((test-name1  (db:test-get-testname  test1))
	 (item-path1  (db:test-get-item-path test1))
	 (eventtime1  (db:test-get-event_time test1))
	 (test-name2  (db:test-get-testname  test2))
	 (item-path2  (db:test-get-item-path test2))
	 (eventtime2  (db:test-get-event_time test2))
	 (same-name   (equal? test-name1 test-name2))
	 (test1-top   (equal? item-path1 ""))
	 (test2-top   (equal? item-path2 ""))
	 (test1-older (> eventtime1 eventtime2))
	 (same-time   (equal? eventtime1 eventtime2)))			 
    (if same-name
	(if same-time
	    (string>? item-path1 item-path2)
	    test1-older)
	(if same-time
	    (string>? test-name1 test-name2)
	    test1-older))))

;; This is roughly the same as dboard:get-tests-dat, should merge them if possible
;;
;; gets all the tests for run-id that match testnamepatt and key-vals, merges them
;;
;;    NOTE: Yes, this is used
;;
(define (dboard:get-tests-for-run-duplicate tabdat run-id run testnamepatt key-vals)
  (let* ((states      (hash-table-keys (dboard:tabdat-state-ignore-hash tabdat)))
	 (statuses    (hash-table-keys (dboard:tabdat-status-ignore-hash tabdat)))
	 (sort-info   (get-curr-sort))
	 (sort-by     (vector-ref sort-info 1))
	 (sort-order  (vector-ref sort-info 2))
	 (bubble-type (if (member sort-order '(testname))
			  'testname
			  'itempath))
	 ;; note: the rundat is normally created in "update-rundat". 
	 (run-dat    (let ((rec (hash-table-ref/default (dboard:tabdat-allruns-by-id tabdat) run-id #f)))
			(if rec 
			    rec
			    (let ((rd (dboard:rundat-make-init run: run key-vals: key-vals)))
			      (hash-table-set! (dboard:tabdat-allruns-by-id tabdat) run-id rd)
			      rd))))
	 ;; (prev-tests  (dboard:rundat-tests prev-dat)) ;; (vector-ref prev-dat 1))
	 (last-update (dboard:rundat-last-update run-dat)) ;; (vector-ref prev-dat 3))
	 (tmptests    (rmt:get-tests-for-run run-id testnamepatt states statuses  ;; run-id testpatt states statuses
					     #f #f                                ;; offset limit 
					     (dboard:tabdat-hide-not-hide tabdat) ;; no-in
					     sort-by                              ;; sort-by
					     sort-order                           ;; sort-order
					     #f ;; 'shortlist                           ;; qrytype
					     (if (dboard:tabdat-filters-changed tabdat) 
						 0
						 last-update) ;; last-update
					     *dashboard-mode*)) ;; use dashboard mode
	 (use-new    (dboard:tabdat-hide-not-hide tabdat))
	 (tests-ht   (dboard:rundat-tests run-dat))
	 (start-time (current-seconds)))
    (for-each 
     (lambda (tdat)
       (let ((test-id (db:test-get-id tdat))
	     (state   (db:test-get-state tdat)))
	 (dboard:rundat-data-changed-set! run-dat #t)
	 (if (equal? state "DELETED")
	     (hash-table-delete! tests-ht test-id)
	     (hash-table-set! tests-ht test-id tdat))))
     tmptests)
    (dboard:rundat-last-update-set! run-dat (- (current-seconds) 10)) ;; go back two seconds in time to ensure all changes are captured.
    tests-ht))

;; tmptests   - new tests data
;; prev-tests - old tests data
;;
;; (define (dashboard:merge-changed-tests tabdat tests tmptests) ;;  use-new prev-tests) 
;;   (let* ((newdat     (filter
;; 		      (lambda (x)
;; 			(not (equal? (db:test-get-state x) "DELETED"))) ;; remove deleted tests but do it after merging
;; 		      (delete-duplicates (if use-new ;; (dboard:tabdat-filters-changed tabdat)
;; 					     tmptests
;; 					     (append tmptests prev-tests))
;; 					 (lambda (a b)
;; 					   (eq? (db:test-get-id a)(db:test-get-id b)))))))
;;     (print "Time took: " (- (current-seconds) start-time))
;;     (if (eq? *tests-sort-reverse* 3) ;; +event_time
;; 	(sort newdat dboard:compare-tests)
;; 	newdat)))

;; this calls dboard:get-tests-for-run-duplicate for each run
;;
;; create a virtual table of all the tests
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;;
(define (update-rundat tabdat runnamepatt numruns testnamepatt keypatts)
  (let* ((allruns     (rmt:get-runs runnamepatt numruns (dboard:tabdat-start-run-offset tabdat) keypatts))
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns))
	 (start-time  (current-seconds)))
    (dboard:tabdat-header-set! tabdat header)
    ;; 
    ;; trim runs to only those that are changing often here
    ;; 
    (if (not (null? runs))
	(let loop ((run      (car runs))
		   (tal      (cdr runs))
		   (res     '())
		   (maxtests 0))
	  (let* ((run-id       (db:get-value-by-header run header "id"))
		 (key-vals     (rmt:get-key-vals run-id))
		 (tests-ht     (dboard:get-tests-for-run-duplicate tabdat run-id run testnamepatt key-vals))
		 (all-test-ids (hash-table-keys tests-ht))
		 (num-tests    (length all-test-ids)))
	    ;; NOTE: bubble-up also sets the global (dboard:tabdat-item-test-names tabdat)
	    ;; (tests       (bubble-up tmptests priority: bubble-type))
	    ;; NOTE: 11/01/2013 This routine is *NOT* getting called excessively.
	    ;; (debug:print 0 *default-log-port* "Getting data for run " run-id " with key-vals=" key-vals)
	    ;; Not sure this is needed?
	    (if (not (null? all-test-ids))
		(let* ((newmaxtests (max num-tests maxtests))
		       (last-update (- (current-seconds) 10))
		       (run-struct  (dboard:rundat-make-init
				     run:         run 
				     tests:       tests-ht
				     key-vals:    key-vals
				     last-update: last-update))
		       (new-res     (cons run-struct res))
		       (elapsed-time (- (current-seconds) start-time)))
		  (hash-table-set! (dboard:tabdat-allruns-by-id tabdat) run-id run-struct)
		  (if (or (null? tal)
			  (> elapsed-time 5)) ;; stop loading data after 5 seconds, on the next call more data *should* be loaded since get-tests-for-run uses last update
		      (begin
			(if (> elapsed-time 5)(print "WARNING: timed out in update-testdat " elapsed-time "s"))
			(dboard:tabdat-allruns-set! tabdat new-res)
			maxtests)
		      (loop (car tal)(cdr tal) new-res newmaxtests)))))))))

(define *collapsed* (make-hash-table))

(define (toggle-hide lnum uidat) ; fulltestname)
  (let* ((btn (vector-ref (dboard:uidat-get-lftcol uidat) lnum))
	 (fulltestname (iup:attribute btn "TITLE"))
	 (parts        (string-split fulltestname "("))
	 (basetestname (if (null? parts) "" (car parts))))
					;(print "Toggling " basetestname " currently " (hash-table-ref/default *collapsed* basetestname #f))
    (if (hash-table-ref/default *collapsed* basetestname #f)
	(begin
					;(iup:attribute-set! btn "FGCOLOR" "0 0 0")
	  (hash-table-delete! *collapsed* basetestname))
	(begin
					;(iup:attribute-set! btn "FGCOLOR" "0 192 192")
	  (hash-table-set! *collapsed* basetestname #t)))))

(define blank-line-rx (regexp "^\\s*$"))

(define (run-item-name->vectors lst)
  (map (lambda (x)
	 (let ((splst (string-split x "("))
	       (res   (vector "" "")))
	   (vector-set! res 0 (car splst))
	   (if (> (length splst) 1)
	       (vector-set! res 1 (car (string-split (cadr splst) ")"))))
	   res))
       lst))

(define (collapse-rows tabdat inlst)
  (let* ((sort-info   (get-curr-sort))
	 (sort-by     (vector-ref sort-info 1))
	 (sort-order  (vector-ref sort-info 2))
	 (bubble-type (if (member sort-order '(testname))
			  'testname
			  'itempath))
	 (newlst      (filter (lambda (x)
				(let* ((tparts    (string-split x "("))
				       (basetname (if (null? tparts) x (car tparts))))
					;(print "x " x " tparts: " tparts " basetname: " basetname)
				  (cond
				   ((string-match blank-line-rx x) #f)
				   ((equal? x basetname) #t)
				   ((hash-table-ref/default *collapsed* basetname #f) 
					;(print "Removing " basetname " from items")
				    #f)
				   (else #t))))
			      inlst))
	 (vlst         (run-item-name->vectors newlst))
	 (vlst2        (bubble-up tabdat vlst priority: bubble-type)))
    (map (lambda (x)
	   (if (equal? (vector-ref x 1) "")
	       (vector-ref x 0)
	       (conc (vector-ref x 0) "(" (vector-ref x 1) ")")))
	 vlst2)))

(define (update-labels uidat)
  (let* ((rown    0)
	 (keycol  (dboard:uidat-get-keycol uidat))
	 (lftcol  (dboard:uidat-get-lftcol uidat))
	 (numcols (vector-length lftcol))
	 (maxn    (- numcols 1))
	 (allvals (make-vector numcols "")))
    (for-each (lambda (name)
		(if (<= rown maxn)
		    (vector-set! allvals rown name)) ;)
		(set! rown (+ 1 rown)))
	      *alltestnamelst*)
    (let loop ((i 0))
      (let* ((lbl    (vector-ref lftcol i))
	     (keyval (vector-ref keycol i))
	     (oldval (iup:attribute lbl "TITLE"))
	     (newval (vector-ref allvals i)))
	(if (not (equal? oldval newval))
	    (let ((munged-val (let ((parts (string-split newval "(")))
				(if (> (length parts) 1)(conc "  " (car (string-split (cadr parts) ")"))) newval))))
	      (vector-set! keycol i newval)
	      (iup:attribute-set! lbl "TITLE" munged-val)))
	(iup:attribute-set! lbl "FGCOLOR" (if (hash-table-ref/default *collapsed* newval #f) "0 112 112" "0 0 0"))
	(if (< i maxn)
	    (loop (+ i 1)))))))

;; 
(define (get-itemized-tests test-dats)
  (let ((tnames '()))
    (for-each (lambda (tdat)
		(let ((tname (vector-ref tdat 0))  ;; (db:test-get-testname tdat))
		      (ipath (vector-ref tdat 1))) ;; (db:test-get-item-path tdat)))
		  (if (not (equal? ipath ""))
		      (if (and (list? tnames)
			       (string? tname)
			       (not (member tname tnames)))
			  (set! tnames (append tnames (list tname)))))))
	      test-dats)
    tnames))

;; Bubble up the top tests to above the items, collect the items underneath
;; all while preserving the sort order from the SQL query as best as possible.
;;
(define (bubble-up tabdat test-dats #!key (priority 'itempath))
  (if (null? test-dats)
      test-dats
      (begin
	(let* ((tnames   '())                ;; list of names used to reserve order
	       (tests    (make-hash-table))  ;; hash of lists, used to build as we go
	       (itemized (get-itemized-tests test-dats)))
	  (for-each 
	   (lambda (testdat)
	     (let* ((tname (vector-ref testdat 0))  ;; db:test-get-testname testdat))
		    (ipath (vector-ref testdat 1))) ;; db:test-get-item-path testdat)))
	       ;;   (seen  (hash-table-ref/default tests tname #f)))
	       (if (not (member tname tnames))
		   (if (or (and (eq? priority 'itempath)
				(not (equal? ipath "")))
			   (and (eq? priority 'testname)
				(equal? ipath ""))
			   (not (member tname itemized)))
		       (set! tnames (append tnames (list tname)))))
	       (if (equal? ipath "")
		   ;; This a top level, prepend it
		   (hash-table-set! tests tname (cons testdat (hash-table-ref/default tests tname '())))
		   ;; This is item, append it
		   (hash-table-set! tests tname (append (hash-table-ref/default tests tname '())(list testdat))))))
	   test-dats)
	  ;; Set all tests with items 
	  (dboard:tabdat-item-test-names-set! tabdat (append (if (null? tnames)
							      '()
							      (filter (lambda (tname)
									(let ((tlst (hash-table-ref tests tname)))
									  (and (list tlst)
									       (> (length tlst) 1))))
								      tnames))
							  (dboard:tabdat-item-test-names tabdat)))
	  (let loop ((hed (car tnames))
		     (tal (cdr tnames))
		     (res '()))
	    (let ((newres (append res (hash-table-ref tests hed))))
	      (if (null? tal)
		  newres
		  (loop (car tal)(cdr tal) newres))))))))

;; optimized to get runs constrained by what is visible on the screen
;;  - not appropriate for where all the runs are needed
;;
(define (update-buttons tabdat uidat numruns numtests)
  (let* ((runs        (if (> (length (dboard:tabdat-allruns tabdat)) numruns)
			  (take-right (dboard:tabdat-allruns tabdat) numruns)
			  (pad-list (dboard:tabdat-allruns tabdat) numruns)))
	 (lftcol      (dboard:uidat-get-lftcol uidat))
	 (tableheader (dboard:uidat-get-header uidat))
	 (table       (dboard:uidat-get-runsvec uidat))
	 (coln        0))
    (set! *alltestnamelst* '())
    ;; create a concise list of test names
    (for-each
     (lambda (rundat)
       (if rundat
	   (let* ((testdats  (dboard:rundat-tests rundat))
		  (testnames (map test:test-get-fullname (hash-table-values testdats)))
		  (alltests-by-name (make-hash-table)))
	     (dboard:rundat-copy-tests-to-by-name rundat)
	     ;; for the normalized list of testnames (union of all runs)
	     (if (not (and (dboard:tabdat-hide-empty-runs tabdat)
			   (null? testnames)))
		 (for-each (lambda (testname)
			     (if (not (member testname *alltestnamelst*))
				 (begin
				   (set! *alltestnamelst* (append *alltestnamelst* (list testname))))))
			   testnames)))))
     runs)

    ;; need alltestnames to enable lining up all tests from all runs
    (set! *alltestnamelst* (collapse-rows tabdat *alltestnamelst*)) ;;; argh. please clean up this sillyness
    (set! *alltestnamelst* (let ((xl (if (> (length *alltestnamelst*) (dboard:tabdat-start-test-offset tabdat))
					 (drop *alltestnamelst* (dboard:tabdat-start-test-offset tabdat))
					 '())))
			     (append xl (make-list (- (dboard:tabdat-num-tests tabdat) (length xl)) ""))))
    (update-labels uidat)
    (for-each
     (lambda (rundat)
       (if (or (not rundat) ;; handle padded runs
	       (not (dboard:rundat-run rundat)))
	   ;;           ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration
	   (set! rundat (dboard:rundat-make-init
			 key-vals: (map (lambda (x) "")(dboard:tabdat-keys tabdat)))))
       (let* ((run              (dboard:rundat-run rundat))
	      (testsdat-by-name (dboard:rundat-tests-by-name rundat))
	      (key-val-dat      (dboard:rundat-key-vals rundat))
	      (run-id           (db:get-value-by-header run (dboard:tabdat-header tabdat) "id"))
	      (key-vals         (append key-val-dat
					(list (let ((x (db:get-value-by-header run (dboard:tabdat-header tabdat) "runname")))
						(if x x "")))))
	      (run-key          (string-intersperse key-vals "\n")))
	 
	 ;; fill in the run header key values
	 (let ((rown      0)
	       (headercol (vector-ref tableheader coln)))
	   (for-each (lambda (kval)
		       (let* ((labl      (vector-ref headercol rown)))
			 (if (not (equal? kval (iup:attribute labl "TITLE")))
			     (iup:attribute-set! (vector-ref headercol rown) "TITLE" kval))
			 (set! rown (+ rown 1))))
		     key-vals))
	 
	 ;; For this run now fill in the buttons for each test
	 (let ((rown 0)
	       (columndat  (vector-ref table coln)))
	   (for-each
	    (lambda (testname)
	      (let ((buttondat  (hash-table-ref/default (dboard:tabdat-buttondat tabdat) (mkstr coln rown) #f)))
		(if (and buttondat
			 (hash-table? testsdat-by-name))
		    (let* ((testdat      (let ((matching (hash-table-ref/default testsdat-by-name testname #f)))
					   ;; (filter 
					   ;;   (lambda (x)(equal? (test:test-get-fullname x) testname))
					   ;;     testsdat)))
					   (if (not matching)
					       (vector -1 -1 "" "" "" 0 "" "" 0 "" "" "" 0 "" "")
					       ;; (car matching))))
					       matching)))
			   (testname     (db:test-get-testname   testdat))
			   (itempath     (db:test-get-item-path  testdat))
			   (testfullname (test:test-get-fullname testdat))
			   (teststatus   (db:test-get-status     testdat))
			   (teststate    (db:test-get-state      testdat))
			   ;;(teststart  (db:test-get-event_time test))
			   ;;(runtime    (db:test-get-run_duration test))
			   (buttontxt    (cond
					  ((member teststate '("COMPLETED" "ARCHIVED")) teststatus)
					  ((and (equal? teststate "NOT_STARTED")
						(member teststatus '("ZERO_ITEMS" "BLOCKED" "PREQ_FAIL" "PREQ_DISCARDED" "TIMED_OUT" "KEEP_TRYING" "TEN_STRIKES")))
					   teststatus)
					  (else
					   teststate)))
			   (button     (vector-ref columndat rown))
			   (color      (car (gutils:get-color-for-state-status teststate teststatus)))
			   (curr-color (vector-ref buttondat 1)) ;; (iup:attribute button "BGCOLOR"))
			   (curr-title (vector-ref buttondat 2))) ;; (iup:attribute button "TITLE")))
		      (if (not (equal? curr-color color))
			  (iup:attribute-set! button "BGCOLOR" color))
		      (if (not (equal? curr-title buttontxt))
			  (iup:attribute-set! button "TITLE"   buttontxt))
		      (vector-set! buttondat 0 run-id)
		      (vector-set! buttondat 1 color)
		      (vector-set! buttondat 2 buttontxt)
		      (vector-set! buttondat 3 testdat)
		      (vector-set! buttondat 4 run-key)))
		(set! rown (+ rown 1))))
	    *alltestnamelst*))
	 (set! coln (+ coln 1))))
     runs)))

(define (mkstr . x)
  (string-intersperse (map conc x) ","))

(define (set-bg-on-filter commondat tabdat)
  (let ((search-changed (not (null? (filter (lambda (key)
					      (not (equal? (hash-table-ref (dboard:tabdat-searchpatts tabdat) key) "%")))
					    (hash-table-keys (dboard:tabdat-searchpatts tabdat))))))
	(state-changed  (not (null? (hash-table-keys (dboard:tabdat-state-ignore-hash tabdat)))))
	(status-changed (not (null? (hash-table-keys (dboard:tabdat-status-ignore-hash tabdat))))))
    (iup:attribute-set! (dboard:commondat-hide-not-hide-tabs commondat) "BGCOLOR"
			(if (or search-changed
				state-changed
				status-changed)
			    "190 180 190"
			    "190 190 190"
			    ))
    (dboard:tabdat-filters-changed-set! tabdat #t)))

(define (update-search commondat tabdat x val)
  (hash-table-set! (dboard:tabdat-searchpatts tabdat) x val)
  (dboard:tabdat-filters-changed-set! tabdat #t)
  (set-bg-on-filter commondat tabdat))

(define (mark-for-update tabdat)
  (dboard:tabdat-filters-changed-set! tabdat #t)
  (dboard:tabdat-last-db-update-set! tabdat 0))

;;======================================================================
;; R U N C O N T R O L
;;======================================================================

;; target populating logic
;;  
;; lb            = <vector curr-label-object next-label-object>
;; field         = target field name for this dropdown
;; referent-vals = selected value in the left dropdown
;; targets       = list of targets to use to build the dropdown
;; 
;; each node is chained: key1 -> key2 -> key3
;;
;; must select values from only apropriate targets
;;   a b c
;;   a d e
;;   a b f
;;        a/b => c f
;;
(define (dashboard:populate-target-dropdown lb referent-vals targets) ;;  runconf-targs)
  ;; is the current value in the new list? choose new default if not
  (let* ((remvalues  (map (lambda (row)
			    (common:list-is-sublist referent-vals (vector->list row)))
			  targets))
	 (values     (delete-duplicates (map car (filter list? remvalues))))
	 (sel-valnum (iup:attribute lb "VALUE"))
	 (sel-val    (iup:attribute lb sel-valnum))
	 (val-num    1))
    ;; first check if the current value is in the new list, otherwise replace with 
    ;; first value from values
    (iup:attribute-set! lb "REMOVEITEM" "ALL")
    (for-each (lambda (val)
		;; (iup:attribute-set! lb "APPENDITEM" val)
		(iup:attribute-set! lb (conc val-num) val)
		(if (equal? sel-val val)
		    (iup:attribute-set! lb "VALUE" val-num))
		(set! val-num (+ val-num 1)))
	      values)
    (let ((val (iup:attribute lb "VALUE")))
      (if val
	  val
	  (if (not (null? values))
	      (let ((newval (car values)))
		(iup:attribute-set! lb "VALUE" newval)
		newval))))))

(define (dashboard:update-target-selector tabdat #!key (action-proc #f))
  (let* ((runconf-targs (common:get-runconfig-targets))
	 (key-lbs       (dboard:tabdat-key-listboxes tabdat))
	 (db-target-dat (rmt:get-targets))
	 (header        (vector-ref db-target-dat 0))
	 (db-targets    (vector-ref db-target-dat 1))
	 (munge-target  (lambda (x)            ;; create a target vector from a string. Pad with na if needed.
			  (list->vector
			   (take (append (string-split x "/")
					 (make-list (length header) "na"))
				 (length header)))))
	 (all-targets   (append (list (munge-target (string-intersperse 
						     (map (lambda (x) "%") header)
						     "/")))
				db-targets
				(map munge-target
				     runconf-targs)
				))
	 (key-listboxes (if key-lbs key-lbs (make-list (length header) #f))))
    (if (not (dboard:tabdat-key-listboxes tabdat))(dboard:tabdat-key-listboxes-set! tabdat key-listboxes))
    (let loop ((key     (car header))
	       (remkeys (cdr header))
	       (refvals '())
	       (indx    0)
	       (lbs     '()))
      (let* ((lb (let ((lb (list-ref key-listboxes indx)))
		   (if lb
		       lb
		       (iup:listbox 
			#:size "x60" 
			#:fontsize "10"
			#:expand "YES" ;; "VERTICAL"
			;; #:dropdown "YES"
			#:editbox "YES"
			#:action (lambda (obj a b c)
				   (debug:catch-and-dump action-proc "update-target-selector"))
			#:caret_cb (lambda (obj a b c)
				     (debug:catch-and-dump action-proc "update-target-selector"))
			))))
	     ;; loop though all the targets and build the list for this dropdown
	     (selected-value (dashboard:populate-target-dropdown lb refvals all-targets)))
	(if (null? remkeys)
	    ;; return a list of the listbox items and an iup:hbox with the labels and listboxes
	    (let* ((listboxes (append lbs (list lb)))
		   (res       (list listboxes
				    (map (lambda (htxt lb)
					   (iup:vbox
					    (iup:label htxt) 
					    lb))
					 header
					 listboxes))))
	      (dboard:tabdat-key-listboxes-set! tabdat res)
	      res)
	    (loop (car remkeys)
		  (cdr remkeys)
		  (append refvals (list selected-value))
		  (+ indx 1)
		  (append lbs (list lb))))))))

;; Make a vertical list of toggles using items, when toggled call proc with the conc'd string 
;; interspersed with commas
;;
(define (dashboard:text-list-toggle-box items proc)
  (let ((alltgls (make-hash-table)))
    (apply iup:vbox
	   (map (lambda (item)
		  (iup:toggle 
		   item
		   #:fontsize 8
		   #:expand "YES"
		   #:action (lambda (obj tstate)
			       (debug:catch-and-dump 
				(lambda ()
				  (if (eq? tstate 0)
				      (hash-table-delete! alltgls item)
				      (hash-table-set! alltgls item #t))
				  (let ((all (hash-table-keys alltgls)))
				    (proc all)))
				"text-list-toggle-box"))))
		items))))

;; Extract the various bits of data from tabdat and create the command line equivalent that will be displayed
;;
(define (dashboard:update-run-command tabdat)
  (let* ((cmd-tb       (dboard:tabdat-command-tb tabdat))
	 (cmd          (dboard:tabdat-command    tabdat))
	 (test-patt    (let ((tp (dboard:tabdat-test-patts tabdat)))
			 (if (equal? tp "") "%" tp)))
	 (states       (dboard:tabdat-states     tabdat))
	 (statuses     (dboard:tabdat-statuses   tabdat))
	 (target       (let ((targ-list (dboard:tabdat-target     tabdat)))
			 (if targ-list (string-intersperse targ-list "/") "no-target-selected")))
	 (run-name     (dboard:tabdat-run-name   tabdat))
	 (states-str   (if (or (not states)
			       (null? states))
			   ""
			   (conc " -state "  (string-intersperse states ","))))
	 (statuses-str (if (or (not statuses)
			       (null? statuses))
			   ""
			   (conc " -status " (string-intersperse statuses ","))))
	 (full-cmd  "megatest"))
    (case (string->symbol cmd)
      ((run)
       (set! full-cmd (conc full-cmd 
			    " -run"
			    " -testpatt "
			    test-patt
			    " -target "
			    target
			    " -runname "
			    run-name
			    " -clean-cache"
			    )))
      ((remove-runs)
       (set! full-cmd (conc full-cmd
			    " -remove-runs -runname "
			    run-name
			    " -target " 
			    target
			    " -testpatt "
			    test-patt
			    states-str
			    statuses-str
			    )))
      (else (set! full-cmd " no valid command ")))
    (iup:attribute-set! cmd-tb "VALUE" full-cmd)))

;; Display the tests as rows of boxes on the test/task pane
;;
(define (dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames test-records)
  (canvas-clear! cnv)
  (canvas-font-set! cnv "Helvetica, -10")
  (let-values (((sizex sizey sizexmm sizeymm) (canvas-size cnv))
	       ((originx originy)             (canvas-origin cnv)))
    ;; (print "originx: " originx " originy: " originy)
    ;; (canvas-origin-set! cnv 0 (- (/ sizey 2)))
    (if (hash-table-ref/default tests-draw-state 'first-time #t)
	(begin
	  (hash-table-set! tests-draw-state 'first-time #f)
	  (hash-table-set! tests-draw-state 'scalef 1)
	  (hash-table-set! tests-draw-state 'tests-info (make-hash-table))
	  (hash-table-set! tests-draw-state 'selected-tests (make-hash-table))
	  ;; set these 
	  (dcommon:initial-draw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames test-records))
	(dcommon:redraw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames test-records))
    ))

;;======================================================================
;; R U N   C O N T R O L S
;;======================================================================
;;
;; A gui for launching tests
;;

(define (dboard:target-updater tabdat) ;;  key-listboxes)
  (let ((targ (map (lambda (x)
		     (iup:attribute x "VALUE"))
		   (car (dashboard:update-target-selector tabdat))))
	(curr-runname (dboard:tabdat-run-name tabdat)))
    (dboard:tabdat-target-set! tabdat targ)
    ;; (if (dboard:tabdat-updater-for-runs tabdat)
    ;; 	 ((dboard:tabdat-updater-for-runs tabdat)))
    (if (or (not (equal? curr-runname (dboard:tabdat-run-name tabdat)))
	    (equal? (dboard:tabdat-run-name tabdat) ""))
	(dboard:tabdat-run-name-set! tabdat curr-runname))
    (dashboard:update-run-command tabdat)))

(define (dashboard:run-controls commondat tabdat #!key (tab-num #f))
  (let* ((targets       (make-hash-table))
	 (test-records  (make-hash-table))
	 (all-tests-registry (tests:get-all)) ;; (tests:get-valid-tests *toppath* '()))
	 (test-names    (hash-table-keys all-tests-registry))
	 (sorted-testnames #f)
	 (action        "-run")
	 (cmdln         "")
	 (runlogs       (make-hash-table))
	 ;;; (key-listboxes #f)
	 (update-keyvals (lambda () ;; gets called in dashboard:update-target-selector as "action-proc"
			   (dboard:target-updater (dboard:tabdat-key-listboxes tabdat))))
	 (tests-draw-state (make-hash-table)) ;; use for keeping state of the test canvas
	 (test-patterns-textbox  #f))
    (hash-table-set! tests-draw-state 'first-time #t)
    ;; (hash-table-set! tests-draw-state 'scalef 1)
    (tests:get-full-data test-names test-records '() all-tests-registry)
    (set! sorted-testnames (tests:sort-by-priority-and-waiton test-records))
    
    ;; refer to (dboard:tabdat-keys tabdat), (dboard:tabdat-dbkeys tabdat) for keys
    (iup:vbox
     (dcommon:command-execution-control tabdat)
     (iup:split
      #:orientation "VERTICAL" ;; "HORIZONTAL"
      #:value 200
;; 
;;       (iup:split
;;        #:value 300

       ;; Target, testpatt, state and status input boxes
       ;;
       (iup:vbox
	;; Command to run, placed over the top of the canvas
	(dcommon:command-action-selector commondat tabdat tab-num: tab-num)
	(dcommon:command-runname-selector commondat tabdat tab-num: tab-num)
	(dcommon:command-testname-selector commondat tabdat update-keyvals)) ;;  key-listboxes))
       
       (dcommon:command-tests-tasks-canvas tabdat test-records sorted-testnames tests-draw-state))
       
 ;;(iup:frame
 ;; #:title "Logs" ;; To be replaced with tabs
 ;; (let ((logs-tb (iup:textbox #:expand "YES"
 ;;				   #:multiline "YES")))
 ;;	 (dboard:tabdat-logs-textbox-set! tabdat logs-tb)
 ;;	 logs-tb))
      )))

;;======================================================================
;; R U N   C O N T R O L S
;;======================================================================
;;
;; A gui for launching tests
;;
(define (dashboard:run-times commondat tabdat #!key (tab-num #f))
  (let* ((drawing               (vg:drawing-new))
	 (run-times-tab-updater (lambda ()	
				  (debug:catch-and-dump 
				   (lambda ()
				     (let ((tabdat        (dboard:common-get-tabdat commondat tab-num: tab-num)))
				       (if tabdat
					   (let ((last-data-update (dboard:tabdat-last-data-update tabdat))
						 (now-time         (current-seconds)))
					     (dashboard:run-times-tab-canvas-updater commondat tabdat tab-num)
					     (if (> (- now-time last-data-update) 5)
						 (if (not (dboard:tabdat-running-layout tabdat))
						     (begin
						       (dashboard:run-times-tab-run-data-updater commondat tabdat tab-num)
						       (dboard:tabdat-last-data-update-set! tabdat now-time)
						       (thread-start! (make-thread
								       (lambda ()
									 (dboard:tabdat-running-layout-set! tabdat #t)
									 (dashboard:run-times-tab-layout-updater commondat tabdat tab-num)
									 (dboard:tabdat-running-layout-set! tabdat #f))
								       "run-times-tab-layout-updater")))
						     ))))))
				   "dashboard:run-times-tab-updater")))
	 (key-listboxes #f) ;; 
	 (update-keyvals (lambda ()
			   (dboard:target-updater tabdat))))
    (dboard:tabdat-drawing-set! tabdat drawing)
    (dboard:commondat-add-updater commondat run-times-tab-updater tab-num: tab-num)
    (iup:split
     #:orientation "VERTICAL" ;; "HORIZONTAL"
     #:value 150
     (iup:vbox
      (let* ((tb      (iup:treebox
		       #:value 0
		       #:name "Runs"
		       #:expand "YES"
		       #:addexpanded "NO"
		       #:selection-cb
		       (lambda (obj id state)
			 (debug:catch-and-dump
			  (lambda ()
			    (let* ((run-path (tree:node->path obj id))
				   (run-id    (tree-path->run-id tabdat (cdr run-path))))
			      (dboard:tabdat-target-set! tabdat (cdr run-path)) ;; (print "run-path: " run-path)			    
			      (dboard:tabdat-layout-update-ok-set! tabdat #f)
			      (if (number? run-id)
				  (begin
				    (dboard:tabdat-curr-run-id-set! tabdat run-id)
				    (dboard:tabdat-view-changed-set! tabdat #t))
				  (debug:print-error 0 *default-log-port* "tree-path->run-id returned non-number " run-id))))
			  "treebox"))
		       ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		       )))
	(dboard:tabdat-runs-tree-set! tabdat tb)
	tb)
      (iup:hbox
       (iup:toggle 
	"Compact layout"
	#:fontsize 8
	#:expand "YES"
	#:value 1
	#:action (lambda (obj tstate)
		   (debug:catch-and-dump 
		    (lambda ()
		      (print "tstate: " tstate)
		      (if (eq? tstate 0)
			  (dboard:tabdat-compact-layout-set! tabdat #f)
			  (dboard:tabdat-compact-layout-set! tabdat #t))
		      (dboard:tabdat-last-filter-str-set! tabdat "")
		      )
		    "text-list-toggle-box"))))
      (dcommon:command-runname-selector commondat tabdat tab-num: tab-num)
      (dcommon:command-testname-selector commondat tabdat update-keyvals))
     (iup:vbox
      (let* ((cnv-obj (iup:canvas 
		       ;; #:size "500x400"
		       #:expand "YES"
		       #:scrollbar "YES"
		       #:posx "0.5"
		       #:posy "0.5"
		       #:action (make-canvas-action
				  (lambda (c xadj yadj)
				    (debug:catch-and-dump
				     (lambda ()
				       (if (not (dboard:tabdat-cnv tabdat))
					   (let ((cnv     (dboard:tabdat-cnv tabdat)))
					     (dboard:tabdat-cnv-set! tabdat c)
					     (vg:drawing-cnv-set! (dboard:tabdat-drawing tabdat)
								  (dboard:tabdat-cnv tabdat))))
				       (let ((drawing (dboard:tabdat-drawing tabdat))
					     (old-xadj (dboard:tabdat-xadj   tabdat))
					     (old-yadj (dboard:tabdat-yadj   tabdat)))
					 (if (not (and (eq? xadj old-xadj)(eq? yadj old-yadj)))
					     (begin
					       (print  "xadj: " xadj " yadj: " yadj "changed: "(eq? xadj old-xadj) " " (eq? yadj old-yadj))
					       (dboard:tabdat-view-changed-set! tabdat #t)
					       (dboard:tabdat-xadj-set! tabdat (* -2000 (- xadj 0.5)))
					       (dboard:tabdat-yadj-set! tabdat (*  2000 (- yadj 0.5)))
					       ))))
				     "iup:canvas action")))
		       #:wheel-cb  (lambda (obj step x y dir) ;; dir is 4 for up and 5 for down. I think.
				     (debug:catch-and-dump
				      (lambda ()
					(let* ((drawing (dboard:tabdat-drawing tabdat))
					       (scalex  (vg:drawing-scalex drawing)))
					  (dboard:tabdat-view-changed-set! tabdat #t)
					  (print "step: " step " x: " x " y: " y " dir: " dir " scalex: " scalex)
					  (vg:drawing-scalex-set! drawing
								  (+ scalex
								     (if (> step 0)
									 (* scalex  0.02)
									 (* scalex -0.02))))))
				      "wheel-cb"))
		       )))
	cnv-obj)))))

;;======================================================================
;; S U M M A R Y 
;;======================================================================
;;
;; General info about the run(s) and megatest area
(define (dashboard:summary commondat tabdat #!key (tab-num #f))
  (let* ((rawconfig        (read-config (conc *toppath* "/megatest.config") #f #f)) ;; changed to #f since I want #{} to be expanded by [system ...] to NOT be expanded. WAS: 'return-string)))
	 (changed          #f))
    (iup:vbox
     (iup:split
      #:value 500
      (iup:frame 
       #:title "General Info"
       (iup:vbox
	(iup:hbox
	 (iup:label "Area Path")
	 (iup:textbox #:value *toppath* #:expand "HORIZONTAL"))
	(iup:hbox 
	 (dcommon:keys-matrix rawconfig)
	 (dcommon:general-info)
	 )))
      (iup:frame
       #:title "Server"
       (dcommon:servers-table commondat tabdat)))
     (iup:frame 
      #:title "Megatest config settings"
      (iup:hbox
       (dcommon:section-matrix rawconfig "setup" "Varname" "Value")
       (iup:vbox
	(dcommon:section-matrix rawconfig "server" "Varname" "Value")
	;; (iup:frame
	;; #:title "Disks Areas"
	(dcommon:section-matrix rawconfig "disks" "Disk area" "Path"))))
     (iup:frame
      #:title "Run statistics"
      (dcommon:run-stats commondat tabdat tab-num: tab-num)))))

;;======================================================================
;; R U N
;;======================================================================
;;
;; display and manage a single run at a time

(define (tree-path->run-id tabdat path)
  (if (not (null? path))
      (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) path #f)
      #f))

;; (define dashboard:update-run-summary-tab #f)
;; (define dashboard:update-new-view-tab #f)

(define (dboard:get-tests-dat tabdat run-id last-update)
  (let ((tdat (if run-id (rmt:get-tests-for-run run-id 
					     (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
					     (hash-table-keys (dboard:tabdat-state-ignore-hash tabdat))  ;; '()
					     (hash-table-keys (dboard:tabdat-status-ignore-hash tabdat)) ;; '()
					     #f #f                                                       ;; offset limit
					     (dboard:tabdat-hide-not-hide tabdat)                        ;; not-in
					     #f #f                                                       ;; sort-by sort-order
					     #f ;; get all? "id,testname,item_path,state,status,event_time,run_duration"                        ;; qryval
					     (if (dboard:tabdat-filters-changed tabdat)
						 0
						 last-update)
					     *dashboard-mode*)
		  '()))) ;; get 'em all
    (debug:print 0 *default-log-port* "dboard:get-tests-dat: got " (length tdat) " test records for run " run-id)
    (sort tdat (lambda (a b)
		 (let* ((aval (vector-ref a 2))
			(bval (vector-ref b 2))
			(anum (string->number aval))
			(bnum (string->number bval)))
		   (if (and anum bnum)
		       (< anum bnum)
		       (string<= aval bval)))))))

(define (dashboard:safe-cadr-assoc name lst)
  (let ((res (assoc name lst)))
    (if (and res (> (length res) 1))
	(cadr res)
	#f)))

(define (dashboard:one-run-updater commondat tabdat tb cell-lookup run-matrix)
  (let* ((runs-dat     (rmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f))
	 (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
	 (run-id       (dboard:tabdat-curr-run-id tabdat))
	 (last-update  0) ;; fix me
	 (tests-dat    (dboard:get-tests-dat tabdat run-id last-update))
	 (tests-mindat (dcommon:minimize-test-data tests-dat))
	 (indices      (common:sparse-list-generate-index tests-mindat)) ;;  proc: set-cell))
	 (row-indices  (cadr indices))
	 (col-indices  (car indices))
	 (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
	 (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
	 (max-visible  (max (- (dboard:tabdat-num-tests tabdat) 15) 3)) ;; (dboard:tabdat-num-tests tabdat) is proportional to the size of the window
	 (numrows      1)
	 (numcols      1)
	 (changed      #f)
	 (runs-hash    (let ((ht (make-hash-table)))
			 (for-each (lambda (run)
				     (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
				   (vector-ref runs-dat 1))
			 ht))
	 (run-ids      (sort (filter number? (hash-table-keys runs-hash))
			     (lambda (a b)
			       (let* ((record-a (hash-table-ref runs-hash a))
				      (record-b (hash-table-ref runs-hash b))
				      (time-a   (db:get-value-by-header record-a runs-header "event_time"))
				      (time-b   (db:get-value-by-header record-b runs-header "event_time")))
				 (< time-a time-b))))))
    (dboard:tabdat-filters-changed-set! tabdat #f)
    (let loop ((pass-num 0)
	       (changed  #f))
      ;; (iup:attribute-set! tb "VALUE" "0")
      ;; (iup:attribute-set! tb "NAME" "Runs")
      ;; Update the runs tree
      (for-each (lambda (run-id)
		  (let* ((run-record (hash-table-ref/default runs-hash run-id #f))
			 (key-vals   (map (lambda (key)(db:get-value-by-header run-record runs-header key))
					  (dboard:tabdat-keys tabdat)))
			 (run-name   (db:get-value-by-header run-record runs-header "runname"))
			 (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
			 (run-path   (append key-vals (list run-name)))
			 (existing   (tree:find-node tb run-path)))
		    (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
			(begin
			  (hash-table-set! (dboard:tabdat-run-keys tabdat) run-id run-path)
			  ;; (iup:attribute-set! (dboard:tabdat-runs-matrix tabdat)
			  ;;    		 (conc rownum ":" colnum) col-name)
			  ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
			  ;; Here we update the tests treebox and tree keys
			  (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
					 userdata: (conc "run-id: " run-id))
			  (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path run-id)
			  ;; (set! colnum (+ colnum 1))
			  ))))
		run-ids)
      (if (eq? pass-num 1)
	  (begin ;; big reset
	    (iup:attribute-set! run-matrix "CLEARVALUE" "ALL") ;; NOTE: Was CONTENTS
	    (iup:attribute-set! run-matrix "CLEARATTRIB" "CONTENTS")
	    (iup:attribute-set! run-matrix "RESIZEMATRIX" "YES")
	    (iup:attribute-set! run-matrix "NUMCOL" max-col )
	    (iup:attribute-set! run-matrix "NUMLIN" (if (< max-row max-visible) max-visible max-row)))) ;; min of 20
      
      ;; (iup:attribute-set! run-matrix "NUMCOL_VISIBLE" max-col)
      ;; (iup:attribute-set! run-matrix "NUMLIN_VISIBLE" (if (> max-row max-visible) max-visible max-row))
      
      ;; Row labels
      (for-each (lambda (ind)
		  (let* ((name (car ind))
			 (num  (cadr ind))
			 (key  (conc num ":0")))
		    (if (not (and (eq? pass-num 0) changed))
			(set! changed (dcommon:modify-if-different run-matrix key name changed)))))
		row-indices)
      
      (print "row-indices: " row-indices " col-indices: " col-indices)
      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass

      ;; Cell contents
      (for-each (lambda (entry)
		  (let* ((row-name  (cadr entry))
			 (col-name  (car entry))
			 (valuedat  (caddr entry))
			 (test-id   (list-ref valuedat 0))
			 (test-name row-name) ;; (list-ref valuedat 1))
			 (item-path col-name) ;; (list-ref valuedat 2))
			 (state     (list-ref valuedat 1))
			 (status    (list-ref valuedat 2))
			 (value     (let ((res (gutils:get-color-for-state-status state status)))
				      (if (and (list? res)
					       (> (length res) 1))
					  res
					  #f)))) ;; (list "n/a" "256 256 256"))))
		    (print "value: " value " row-name: " (cadr value) " row-color: " (car value))
		    (print "(assoc row-name row-indices): " (assoc row-name row-indices) "  (assoc col-name col-indices): "  (assoc col-name col-indices))
		    (if value
			(let* ((row-name  (cadr value))
			       (row-color (car value))
			       (row-num   (dashboard:safe-cadr-assoc row-name row-indices)) ;; (cadr (assoc row-name row-indices)))
			       (col-num   (dashboard:safe-cadr-assoc col-name col-indices))
			       (key       (conc row-num ":" col-num)))
			  (if (and row-num col-num)
			      (begin
				(hash-table-set! cell-lookup key test-id)
				(set! changed (dcommon:modify-if-different run-matrix key row-name changed))
				(set! changed (dcommon:modify-if-different run-matrix (conc "BGCOLOR" key) row-color changed)))
			      (print "ERROR: row-num=" row-num " col-num=" col-num))))
			  ))
		tests-mindat)
      
      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass due to contents changing

      ;; Col labels - do after setting Cell contents so they are accounted for in the size calc.

      (for-each (lambda (ind)
		  (print "ind: " ind)
		  (let* ((name (car ind))
			 (num  (cadr ind))
			 (key  (conc "0:" num)))
		    (set! changed (dcommon:modify-if-different run-matrix key name changed))
		    (if changed (iup:attribute-set! run-matrix "FITTOTEXT" (conc "C" num)))))
		col-indices)

      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass due to column labels changing

      ;; (debug:print 0 *default-debug-port* "one-run-updater, changed: " changed " pass-num: " pass-num)
      (print "one-run-updater, changed: " changed " pass-num: " pass-num)
      (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL")))))

;; This is the Run Summary tab
;; 
(define (dashboard:one-run commondat tabdat #!key (tab-num #f))
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id tabdat (cdr run-path))))
		       (if (number? run-id)
			   (begin
			     (dboard:tabdat-curr-run-id-set! tabdat run-id)
			     (dboard:tabdat-layout-update-ok-set! tabdat #f)
			     ;; (dashboard:update-run-summary-tab)
			     )
			   (debug:print-error 0 *default-log-port* "tree-path->run-id returned non-number " run-id)))
		     ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		     )))
	 (cell-lookup (make-hash-table))
	 (run-matrix (iup:matrix
		      #:expand "YES"
		      #:click-cb
		      (lambda (obj lin col status)
			(let* ((toolpath (car (argv)))
			       (key      (conc lin ":" col))
			       (test-id  (hash-table-ref/default cell-lookup key -1))
			       (cmd      (conc toolpath " -test " (dboard:tabdat-curr-run-id tabdat) "," test-id "&")))
			  (system cmd)))))
	 (one-run-updater  (lambda ()
			     (print "Got here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
			     (if  (dashboard:database-changed? commondat tabdat)
				  (dashboard:one-run-updater commondat tabdat tb cell-lookup run-matrix)))))
    (dboard:commondat-add-updater commondat one-run-updater tab-num: tab-num)
    (iup:vbox
     (let* ((cnv-obj (iup:canvas 
		     ;; #:size "500x400"
		     #:expand "YES"
		     #:scrollbar "YES"
		     #:posx "0.5"
		     #:posy "0.5"
		     #:action (make-canvas-action
			       (lambda (c xadj yadj)
				 (debug:catch-and-dump
				  (lambda ()
				    (if (not (dboard:tabdat-cnv tabdat))
					(dboard:tabdat-cnv-set! tabdat c))
				    (let ((drawing (dboard:tabdat-drawing tabdat))
					  (old-xadj (dboard:tabdat-xadj   tabdat))
					  (old-yadj (dboard:tabdat-yadj   tabdat)))
				      (if (not (and (eq? xadj old-xadj)(eq? yadj old-yadj)))
					  (begin
					    (print  "xadj: " xadj " yadj: " yadj "changed: "(eq? xadj old-xadj) " " (eq? yadj old-yadj))
					    (dboard:tabdat-view-changed-set! tabdat #t)
					    (dboard:tabdat-xadj-set! tabdat (* -500 (- xadj 0.5)))
					    (dboard:tabdat-yadj-set! tabdat (*  500 (- yadj 0.5)))
					    ))))
				  "iup:canvas action dashboard:one-run")))
		     #:wheel-cb (lambda (obj step x y dir) ;; dir is 4 for up and 5 for down. I think.
				  (debug:catch-and-dump
				   (lambda ()
				     (let* ((drawing (dboard:tabdat-drawing tabdat))
					    (scalex  (vg:drawing-scalex drawing)))
				       (dboard:tabdat-view-changed-set! tabdat #t)
				       (print "step: " step " x: " x " y: " y " dir: " dir " scalex: " scalex)
				       (vg:drawing-scalex-set! drawing
							       (+ scalex
								  (if (> step 0)
								      (* scalex  0.02)
								      (* scalex -0.02))))))
				   "dashboard:one-run wheel-cb"))
		     )))
       cnv-obj))))

;;======================================================================
;; S U M M A R Y 
;;======================================================================
;;
;; General info about the run(s) and megatest area
(define (dashboard:summary commondat tabdat #!key (tab-num #f))
  (let* ((rawconfig        (read-config (conc *toppath* "/megatest.config") #f #f)) ;; changed to #f since I want #{} to be expanded by [system ...] to NOT be expanded. WAS: 'return-string)))
	 (changed          #f))
    (iup:vbox
     (iup:split
      #:value 500
      (iup:frame 
       #:title "General Info"
       (iup:vbox
	(iup:hbox
	 (iup:label "Area Path")
	 (iup:textbox #:value *toppath* #:expand "HORIZONTAL"))
	(iup:hbox 
	 (dcommon:keys-matrix rawconfig)
	 (dcommon:general-info)
	 )))
      (iup:frame
       #:title "Server"
       (dcommon:servers-table commondat tabdat)))
     (iup:frame 
      #:title "Megatest config settings"
      (iup:hbox
       (dcommon:section-matrix rawconfig "setup" "Varname" "Value")
       (iup:vbox
	(dcommon:section-matrix rawconfig "server" "Varname" "Value")
	;; (iup:frame
	;; #:title "Disks Areas"
	(dcommon:section-matrix rawconfig "disks" "Disk area" "Path"))))
     (iup:frame
      #:title "Run statistics"
      (dcommon:run-stats commondat tabdat tab-num: tab-num)))))

;;======================================================================
;; R U N
;;======================================================================
;;
;; display and manage a single run at a time

(define (tree-path->run-id tabdat path)
  (if (not (null? path))
      (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) path #f)
      #f))

;; (define dashboard:update-run-summary-tab #f)
;; (define dashboard:update-new-view-tab #f)

(define (dboard:get-tests-dat tabdat run-id last-update)
  (let ((tdat (if run-id (rmt:get-tests-for-run run-id 
					     (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
					     (hash-table-keys (dboard:tabdat-state-ignore-hash tabdat))  ;; '()
					     (hash-table-keys (dboard:tabdat-status-ignore-hash tabdat)) ;; '()
					     #f #f                                                       ;; offset limit
					     (dboard:tabdat-hide-not-hide tabdat)                        ;; not-in
					     #f #f                                                       ;; sort-by sort-order
					     #f ;; get all? "id,testname,item_path,state,status,event_time,run_duration"                        ;; qryval
					     (if (dboard:tabdat-filters-changed tabdat)
						 0
						 last-update)
					     *dashboard-mode*)
		  '()))) ;; get 'em all
    (debug:print 0 *default-log-port* "dboard:get-tests-dat: got " (length tdat) " test records for run " run-id)
    (sort tdat (lambda (a b)
		 (let* ((aval (vector-ref a 2))
			(bval (vector-ref b 2))
			(anum (string->number aval))
			(bnum (string->number bval)))
		   (if (and anum bnum)
		       (< anum bnum)
		       (string<= aval bval)))))))

(define (dashboard:safe-cadr-assoc name lst)
  (let ((res (assoc name lst)))
    (if (and res (> (length res) 1))
	(cadr res)
	#f)))

(define (dashboard:one-run-updater commondat tabdat tb cell-lookup run-matrix)
  (let* ((runs-dat     (rmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f))
	 (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
	 (run-id       (dboard:tabdat-curr-run-id tabdat))
	 (last-update  0) ;; fix me
	 (tests-dat    (dboard:get-tests-dat tabdat run-id last-update))
	 (tests-mindat (dcommon:minimize-test-data tests-dat))
	 (indices      (common:sparse-list-generate-index tests-mindat)) ;;  proc: set-cell))
	 (row-indices  (cadr indices))
	 (col-indices  (car indices))
	 (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
	 (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
	 (max-visible  (max (- (dboard:tabdat-num-tests tabdat) 15) 3)) ;; (dboard:tabdat-num-tests tabdat) is proportional to the size of the window
	 (numrows      1)
	 (numcols      1)
	 (changed      #f)
	 (runs-hash    (let ((ht (make-hash-table)))
			 (for-each (lambda (run)
				     (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
				   (vector-ref runs-dat 1))
			 ht))
	 (run-ids      (sort (filter number? (hash-table-keys runs-hash))
			     (lambda (a b)
			       (let* ((record-a (hash-table-ref runs-hash a))
				      (record-b (hash-table-ref runs-hash b))
				      (time-a   (db:get-value-by-header record-a runs-header "event_time"))
				      (time-b   (db:get-value-by-header record-b runs-header "event_time")))
				 (< time-a time-b))))))
    (dboard:tabdat-filters-changed-set! tabdat #f)
    (let loop ((pass-num 0)
	       (changed  #f))
      ;; (iup:attribute-set! tb "VALUE" "0")
      ;; (iup:attribute-set! tb "NAME" "Runs")
      ;; Update the runs tree
      (for-each (lambda (run-id)
		  (let* ((run-record (hash-table-ref/default runs-hash run-id #f))
			 (key-vals   (map (lambda (key)(db:get-value-by-header run-record runs-header key))
					  (dboard:tabdat-keys tabdat)))
			 (run-name   (db:get-value-by-header run-record runs-header "runname"))
			 (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
			 (run-path   (append key-vals (list run-name)))
			 (existing   (tree:find-node tb run-path)))
		    (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
			(begin
			  (hash-table-set! (dboard:tabdat-run-keys tabdat) run-id run-path)
			  ;; (iup:attribute-set! (dboard:tabdat-runs-matrix tabdat)
			  ;;    		 (conc rownum ":" colnum) col-name)
			  ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
			  ;; Here we update the tests treebox and tree keys
			  (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
					 userdata: (conc "run-id: " run-id))
			  (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path run-id)
			  ;; (set! colnum (+ colnum 1))
			  ))))
		run-ids)
      (if (eq? pass-num 1)
	  (begin ;; big reset
	    (iup:attribute-set! run-matrix "CLEARVALUE" "ALL") ;; NOTE: Was CONTENTS
	    (iup:attribute-set! run-matrix "CLEARATTRIB" "CONTENTS")
	    (iup:attribute-set! run-matrix "RESIZEMATRIX" "YES")
	    (iup:attribute-set! run-matrix "NUMCOL" max-col )
	    (iup:attribute-set! run-matrix "NUMLIN" (if (< max-row max-visible) max-visible max-row)))) ;; min of 20
      
      ;; (iup:attribute-set! run-matrix "NUMCOL_VISIBLE" max-col)
      ;; (iup:attribute-set! run-matrix "NUMLIN_VISIBLE" (if (> max-row max-visible) max-visible max-row))
      
      ;; Row labels
      (for-each (lambda (ind)
		  (let* ((name (car ind))
			 (num  (cadr ind))
			 (key  (conc num ":0")))
		    (if (not (and (eq? pass-num 0) changed))
			(set! changed (dcommon:modify-if-different run-matrix key name changed)))))
		row-indices)
      
      (print "row-indices: " row-indices " col-indices: " col-indices)
      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass

      ;; Cell contents
      (for-each (lambda (entry)
		  (let* ((row-name  (cadr entry))
			 (col-name  (car entry))
			 (valuedat  (caddr entry))
			 (test-id   (list-ref valuedat 0))
			 (test-name row-name) ;; (list-ref valuedat 1))
			 (item-path col-name) ;; (list-ref valuedat 2))
			 (state     (list-ref valuedat 1))
			 (status    (list-ref valuedat 2))
			 (value     (let ((res (gutils:get-color-for-state-status state status)))
				      (if (and (list? res)
					       (> (length res) 1))
					  res
					  #f)))) ;; (list "n/a" "256 256 256"))))
		    (print "value: " value " row-name: " (cadr value) " row-color: " (car value))
		    (print "(assoc row-name row-indices): " (assoc row-name row-indices) "  (assoc col-name col-indices): "  (assoc col-name col-indices))
		    (if value
			(let* ((row-name  (cadr value))
			       (row-color (car value))
			       (row-num   (dashboard:safe-cadr-assoc row-name row-indices)) ;; (cadr (assoc row-name row-indices)))
			       (col-num   (dashboard:safe-cadr-assoc col-name col-indices))
			       (key       (conc row-num ":" col-num)))
			  (if (and row-num col-num)
			      (begin
				(hash-table-set! cell-lookup key test-id)
				(set! changed (dcommon:modify-if-different run-matrix key row-name changed))
				(set! changed (dcommon:modify-if-different run-matrix (conc "BGCOLOR" key) row-color changed)))
			      (print "ERROR: row-num=" row-num " col-num=" col-num))))
			  ))
		tests-mindat)
      
      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass due to contents changing

      ;; Col labels - do after setting Cell contents so they are accounted for in the size calc.

      (for-each (lambda (ind)
		  (print "ind: " ind)
		  (let* ((name (car ind))
			 (num  (cadr ind))
			 (key  (conc "0:" num)))
		    (set! changed (dcommon:modify-if-different run-matrix key name changed))
		    (if changed (iup:attribute-set! run-matrix "FITTOTEXT" (conc "C" num)))))
		col-indices)

      (if (and (eq? pass-num 0) changed)
	  (loop 1 #t)) ;; force second pass due to column labels changing

      ;; (debug:print 0 *default-debug-port* "one-run-updater, changed: " changed " pass-num: " pass-num)
      (print "one-run-updater, changed: " changed " pass-num: " pass-num)
      (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL")))))

;; This is the Run Summary tab
;; 
(define (dashboard:one-run commondat tabdat #!key (tab-num #f))
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id tabdat (cdr run-path))))
		       (if (number? run-id)
			   (begin
			     (dboard:tabdat-curr-run-id-set! tabdat run-id)
			     (dboard:tabdat-layout-update-ok-set! tabdat #f)
			     ;; (dashboard:update-run-summary-tab)
			     )
			   (debug:print-error 0 *default-log-port* "tree-path->run-id returned non-number " run-id)))
		     ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		     )))
	 (cell-lookup (make-hash-table))
	 (run-matrix (iup:matrix
		      #:expand "YES"
		      #:click-cb
		      (lambda (obj lin col status)
			(let* ((toolpath (car (argv)))
			       (key      (conc lin ":" col))
			       (test-id  (hash-table-ref/default cell-lookup key -1))
			       (cmd      (conc toolpath " -test " (dboard:tabdat-curr-run-id tabdat) "," test-id "&")))
			  (system cmd)))))
	 (one-run-updater  (lambda ()
			     (print "Got here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
			     (if  (dashboard:database-changed? commondat tabdat)
				  (dashboard:one-run-updater commondat tabdat tb cell-lookup run-matrix)))))
    (dboard:commondat-add-updater commondat one-run-updater tab-num: tab-num)
    (dboard:tabdat-runs-tree-set! tabdat tb)
    (iup:split
     tb
     run-matrix)))

;; This is the New View tab
;; 
(define (dashboard:new-view db commondat tabdat #!key (tab-num #f))
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id tabdat (cdr run-path))))
		       (if (number? run-id)
			   (begin
			     (dboard:tabdat-curr-run-id-set! tabdat run-id)
			     ;; (dashboard:update-new-view-tab)
			     (dboard:tabdat-layout-update-ok-set! tabdat #f)
			     )
			   (debug:print-error 0 *default-log-port* "tree-path->run-id returned non-number " run-id)))
		     ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		     )))
	 (cell-lookup (make-hash-table))
	 (run-matrix (iup:matrix
		      #:expand "YES"
		      #:click-cb
		      (lambda (obj lin col status)
			(let* ((toolpath (car (argv)))
			       (key      (conc lin ":" col))
			       (test-id  (hash-table-ref/default cell-lookup key -1))
			       (cmd      (conc toolpath " -test " (dboard:tabdat-curr-run-id tabdat) "," test-id "&")))
			  (system cmd)))))
	 (new-view-updater  (lambda ()
			      (if  (dashboard:database-changed? commondat tabdat)
				   (let* ((runs-dat     (rmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f))
					  (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
					  (run-id       (dboard:tabdat-curr-run-id tabdat))
					  (last-update  0) ;; fix me
					  (tests-dat    (dboard:get-tests-dat tabdat run-id last-update))
					  (tests-mindat (dcommon:minimize-test-data tests-dat))
					  (indices      (common:sparse-list-generate-index tests-mindat)) ;;  proc: set-cell))
					  (row-indices  (cadr indices))
					  (col-indices  (car indices))
					  (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
					  (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
					  (max-visible  (max (- (dboard:tabdat-num-tests tabdat) 15) 3)) ;; (dboard:tabdat-num-tests tabdat) is proportional to the size of the window
					  (numrows      1)
					  (numcols      1)
					  (changed      #f)
					  (runs-hash    (let ((ht (make-hash-table)))
							  (for-each (lambda (run)
								      (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
								    (vector-ref runs-dat 1))
							  ht))
					  (run-ids      (sort (filter number? (hash-table-keys runs-hash))
							      (lambda (a b)
								(let* ((record-a (hash-table-ref runs-hash a))
								       (record-b (hash-table-ref runs-hash b))
								       (time-a   (db:get-value-by-header record-a runs-header "event_time"))
								       (time-b   (db:get-value-by-header record-b runs-header "event_time")))
								  (< time-a time-b))))))
				     ;; (iup:attribute-set! tb "VALUE" "0")
				     ;; (iup:attribute-set! tb "NAME" "Runs")
				     ;; Update the runs tree
				     (for-each (lambda (run-id)
						 (let* ((run-record (hash-table-ref/default runs-hash run-id #f))
							(key-vals   (map (lambda (key)(db:get-value-by-header run-record runs-header key))
									 (dboard:tabdat-keys tabdat)))
							(run-name   (db:get-value-by-header run-record runs-header "runname"))
							(col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
							(run-path   (append key-vals (list run-name)))
							(existing   (tree:find-node tb run-path)))
						   (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
						       (begin
							 (hash-table-set! (dboard:tabdat-run-keys tabdat) run-id run-path)
							 ;; (iup:attribute-set! (dboard:tabdat-runs-matrix tabdat)
							 ;;    		 (conc rownum ":" colnum) col-name)
							 ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
							 ;; Here we update the tests treebox and tree keys
							 (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
									userdata: (conc "run-id: " run-id))
							 (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path run-id)
							 ;; (set! colnum (+ colnum 1))
							 ))))
					       run-ids)
				     (iup:attribute-set! run-matrix "CLEARVALUE" "ALL") ;; NOTE: Was CONTENTS
				     (iup:attribute-set! run-matrix "CLEARATTRIB" "CONTENTS")
				     (iup:attribute-set! run-matrix "RESIZEMATRIX" "YES")
				     (iup:attribute-set! run-matrix "NUMCOL" max-col )
				     (iup:attribute-set! run-matrix "NUMLIN" (if (< max-row max-visible) max-visible max-row)) ;; min of 20
				     ;; (iup:attribute-set! run-matrix "NUMCOL_VISIBLE" max-col)
				     ;; (iup:attribute-set! run-matrix "NUMLIN_VISIBLE" (if (> max-row max-visible) max-visible max-row))
				     
				     ;; Row labels
				     (for-each (lambda (ind)
						 (let* ((name (car ind))
							(num  (cadr ind))
							(key  (conc num ":0")))
						   (if (not (equal? (iup:attribute run-matrix key) name))
						       (begin
							 (set! changed #t)
							 (iup:attribute-set! run-matrix key name)))))
					       row-indices)

				     
				     ;; Cell contents
				     (for-each (lambda (entry)
						 (let* ((row-name  (cadr entry))
							(col-name  (car entry))
							(valuedat  (caddr entry))
							(test-id   (list-ref valuedat 0))
							(test-name row-name) ;; (list-ref valuedat 1))
							(item-path col-name) ;; (list-ref valuedat 2))
							(state     (list-ref valuedat 1))
							(status    (list-ref valuedat 2))
							(value     (gutils:get-color-for-state-status state status))
							(row-num   (cadr (assoc row-name row-indices)))
							(col-num   (cadr (assoc col-name col-indices)))
							(key       (conc row-num ":" col-num)))
						   (hash-table-set! cell-lookup key test-id)
						   (if (not (equal? (iup:attribute run-matrix key) (cadr value)))
						       (begin
							 (set! changed #t)
							 (iup:attribute-set! run-matrix key (cadr value))
							 (iup:attribute-set! run-matrix (conc "BGCOLOR" key) (car value))))))
					       tests-mindat)
				     
				     ;; Col labels - do after setting Cell contents so they are accounted for in the size calc.

				     (for-each (lambda (ind)
						 (let* ((name (car ind))
							(num  (cadr ind))
							(key  (conc "0:" num)))
						   (if (not (equal? (iup:attribute run-matrix key) name))
						       (begin
							 (set! changed #t)
							 (iup:attribute-set! run-matrix key name)
							 (iup:attribute-set! run-matrix "FITTOTEXT" (conc "C" num))))))
					       col-indices)
				     (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL")))))))
    (dboard:commondat-add-updater commondat new-view-updater tab-num: tab-num)
    (dboard:tabdat-runs-tree-set! tabdat tb)
    (iup:split
     tb
     run-matrix)))

;;======================================================================
;; R U N S 
;;======================================================================

(define (dboard:make-controls commondat tabdat)
	  (iup:hbox
	   (iup:vbox
	    (iup:frame 
	     #:title "filter test and items"
	     (iup:hbox
	      (iup:vbox
	       (iup:textbox #:size "120x15" #:fontsize "10" #:value "%"
			    #:action (lambda (obj unk val)
				       (debug:catch-and-dump
					(lambda ()
					  (mark-for-update tabdat)
					  (update-search commondat tabdat "test-name" val))
					"make-controls")))
	       (iup:hbox
		(iup:button "Quit"      #:action (lambda (obj)
						   ;; (if (dboard:tabdat-dblocal tabdat) (db:close-all (dboard:tabdat-dblocal tabdat)))
						   (exit)))
		(iup:button "Refresh"   #:action (lambda (obj)
						   (mark-for-update tabdat)))
		(iup:button "Collapse"  #:action (lambda (obj)
						   (debug:catch-and-dump 
						    (lambda ()
						      (let ((myname (iup:attribute obj "TITLE")))
							(if (equal? myname "Collapse")
							    (begin
							      (for-each (lambda (tname)
									  (hash-table-set! *collapsed* tname #t))
									(dboard:tabdat-item-test-names tabdat))
							      (iup:attribute-set! obj "TITLE" "Expand"))
							    (begin
							      (for-each (lambda (tname)
									  (hash-table-delete! *collapsed* tname))
									(hash-table-keys *collapsed*))
							      (iup:attribute-set! obj "TITLE" "Collapse"))))
						      (mark-for-update tabdat))
						    "make-controls collapse button"))))
	       )
	      (iup:vbox
	       ;; (iup:button "Sort -t"   #:action (lambda (obj)
	       ;;   				 (next-sort-option)
	       ;;   				 (iup:attribute-set! obj "TITLE" (vector-ref (vector-ref *tests-sort-options* *tests-sort-reverse*) 0))
	       ;;   				 (mark-for-update tabdat)))
	       
	       (let* ((hide #f)
		      (show #f)
		      (hide-empty #f)
		      (sel-color    "180 100 100")
		      (nonsel-color "170 170 170")
		      (cmds-list '("+testname" "-testname" "+event_time" "-event_time" "+statestatus" "-statestatus"))
		      (sort-lb    (iup:listbox #:expand "HORIZONTAL"
					       #:dropdown "YES"
					       #:action (lambda (obj val index lbstate)
							  (set! *tests-sort-reverse* index)
							  (mark-for-update tabdat))))
		      (default-cmd (car (list-ref *tests-sort-type-index* *tests-sort-reverse*))))
		 (iuplistbox-fill-list sort-lb cmds-list selected-item: default-cmd)
		 
		 (set! hide-empty (iup:button "HideEmpty"
					      #:expand "YES"
					      #:action (lambda (obj)
							 (dboard:tabdat-hide-empty-runs-set! tabdat (not (dboard:tabdat-hide-empty-runs tabdat)))
							 (iup:attribute-set! obj "TITLE" (if (dboard:tabdat-hide-empty-runs tabdat) "+HideE" "-HideE"))
							 (mark-for-update tabdat))))
		 (set! hide (iup:button "Hide"
					#:expand "YES"
					#:action (lambda (obj)
						   (dboard:tabdat-hide-not-hide-set! tabdat #t) ;; (not (dboard:tabdat-hide-not-hide tabdat)))
						   ;; (iup:attribute-set! obj "TITLE" (if (dboard:tabdat-hide-not-hide tabdat) "HideTests" "NotHide"))
						   (iup:attribute-set! hide "BGCOLOR" sel-color)
						   (iup:attribute-set! show "BGCOLOR" nonsel-color)
						   (mark-for-update tabdat))))
		 (set! show (iup:button "Show"
					#:expand "YES"
					#:action (lambda (obj)
						   (dboard:tabdat-hide-not-hide-set! tabdat #f) ;; (not (dboard:tabdat-hide-not-hide tabdat)))
						   (iup:attribute-set! show "BGCOLOR" sel-color)
						   (iup:attribute-set! hide "BGCOLOR" nonsel-color)
						   (mark-for-update tabdat))))
		 (iup:attribute-set! hide "BGCOLOR" sel-color)
		 (iup:attribute-set! show "BGCOLOR" nonsel-color)
		 ;; (dboard:tabdat-hide-not-hide-button-set! tabdat hideit) ;; never used, can eliminate ...
		 (iup:vbox
		  (iup:hbox hide show)
		  hide-empty sort-lb)))
	       )))
	   (iup:frame 
	    #:title "state/status filter"
	    (iup:vbox
	     (apply 
	      iup:hbox
	      (map (lambda (status)
		     (iup:toggle (conc status "  ")
				 #:action   (lambda (obj val)
					      (mark-for-update tabdat)
					      (if (eq? val 1)
						  (hash-table-set! (dboard:tabdat-status-ignore-hash tabdat) status #t)
						  (hash-table-delete! (dboard:tabdat-status-ignore-hash tabdat) status))
					      (set-bg-on-filter commondat tabdat))))
		   (map cadr *common:std-statuses*))) ;; '("PASS" "FAIL" "WARN" "CHECK" "WAIVED" "STUCK/DEAD" "n/a" "SKIP")))
	     (apply 
	      iup:hbox
	      (map (lambda (state)
		     (iup:toggle (conc state "  ")
				 #:action   (lambda (obj val)
					      (mark-for-update tabdat)
					      (if (eq? val 1)
						  (hash-table-set! (dboard:tabdat-state-ignore-hash tabdat) state #t)
						  (hash-table-delete! (dboard:tabdat-state-ignore-hash tabdat) state))
					      (set-bg-on-filter commondat tabdat))))
		   (map cadr *common:std-states*))) ;; '("RUNNING" "COMPLETED" "INCOMPLETE" "LAUNCHED" "NOT_STARTED" "KILLED" "DELETED")))
	     (iup:valuator #:valuechanged_cb (lambda (obj)
					       (let ((val (inexact->exact (round (/ (string->number (iup:attribute obj "VALUE")) 10))))
						     (oldmax   (string->number (iup:attribute obj "MAX")))
						     (maxruns  (dboard:tabdat-tot-runs tabdat)))
						 (dboard:tabdat-start-run-offset-set! tabdat val)
						 (mark-for-update tabdat)
						 (debug:print 6 *default-log-port* "(dboard:tabdat-start-run-offset tabdat) " (dboard:tabdat-start-run-offset tabdat) " maxruns: " maxruns ", val: " val " oldmax: " oldmax)
						 (iup:attribute-set! obj "MAX" (* maxruns 10))))
			   #:expand "HORIZONTAL"
			   #:max (* 10 (length (dboard:tabdat-allruns tabdat)))
			   #:min 0
			   #:step 0.01)))
					;(iup:button "inc rows" #:action (lambda (obj)(dboard:tabdat-num-tests-set! tabdat (+ (dboard:tabdat-num-tests tabdat) 1))))
					;(iup:button "dec rows" #:action (lambda (obj)(dboard:tabdat-num-tests-set! tabdat (if (> (dboard:tabdat-num-tests tabdat) 0)(- (dboard:tabdat-num-tests tabdat) 1) 0))))
	   ))

(define (dashboard:popup-menu buttndat run-id test-id target runname test-name testpatt)
  (iup:menu 
   (iup:menu-item
    "Run"
    (iup:menu              
     (iup:menu-item
      (conc "Rerun " testpatt)
      #:action
      (lambda (obj)
        ;;(print "buttndat: " buttndat " run-id: " run-id " test-id: " test-id " target: " target " runname: " runname " test-name: " test-name " testpatt: " testpatt)
	(common:run-a-command
	 (conc "megatest -run -target " target
	       " -runname " runname
	       " -testpatt " testpatt
	       " -preclean -clean-cache")
	 )))
     (iup:menu-item
      "Rerun Complete Run"
      #:action
      (lambda (obj)
        (common:run-a-command
         (conc "megatest -set-state-status NOT_STARTED,n/a -run -target " target
               " -runname " runname
               " -testpatt % "
               " -preclean -clean-cache"))))
     (iup:menu-item
      "Clean Complete Run"
      #:action
      (lambda (obj)
        (common:run-a-command
         (conc "megatest -remove-runs -target " target
               " -runname " runname
               " -testpatt % "))))))
   (iup:menu-item
    "Test"
    (iup:menu 
     (iup:menu-item
      (conc "Rerun " test-name)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -set-state-status NOT_STARTED,n/a -run -target " target
               " -runname " runname
	       " -testpatt " test-name
	       " -preclean -clean-cache"))))
     (iup:menu-item
      (conc "Kill " test-name)
      #:action
      (lambda (obj)
        ;; (rmt:test-set-state-status-by-id run-id test-id "KILLREQ" #f #f)
	(common:run-a-command
	 (conc "megatest -set-state-status KILLREQ,n/a -target " target
               " -runname " runname
	       " -testpatt " test-name
	       " -state RUNNING,REMOTEHOSTSTART,LAUNCHED"))))
     (iup:menu-item
      (conc "Clean " test-name)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -remove-runs -target " target
               " -runname " runname
	       " -testpatt " test-name))))
     (iup:menu-item
      "Start xterm"
      #:action
      (lambda (obj)
        (dcommon:examine-xterm run-id test-id)))
	;;(let* ((cmd (conc (car (argv)) " -xterm " run-id "," test-id "&")))
	;; (system cmd))))
     (iup:menu-item
      "Edit testconfig"
      #:action
      (lambda (obj)
	(let* ((all-tests (tests:get-all))
	       (editor-rx (or (configf:lookup *configdat* "setup" "editor-regex") 
			      "\\b(vim?|nano|pico)\\b"))
	       (editor (or (configf:lookup *configdat* "setup" "editor")
			   (get-environment-variable "VISUAL")
			   (get-environment-variable "EDITOR") "vi"))
	       (tconfig (conc (hash-table-ref all-tests test-name) "/testconfig"))
	       (cmd (conc (if (string-search editor-rx editor)
			      (conc "xterm -e " editor)
			      editor)
			  " " tconfig " &")))
	  (system cmd))))
     ))))

(define (make-dashboard-buttons commondat) ;;  runs-sum-dat new-view-dat)
  (let* ((stats-dat       (dboard:tabdat-make-data))
	 (runs-dat        (dboard:tabdat-make-data))
	 (onerun-dat      (dboard:tabdat-make-data))
	 (runcontrols-dat (dboard:tabdat-make-data))
	 (runtimes-dat    (dboard:tabdat-make-data))
	 (nruns           (dboard:tabdat-numruns runs-dat))
	 (ntests          (dboard:tabdat-num-tests runs-dat))
	 (keynames        (dboard:tabdat-dbkeys runs-dat))
	 (nkeys           (length keynames))
	 (runsvec         (make-vector nruns))
	 (header          (make-vector nruns))
	 (lftcol          (make-vector ntests))
	 (keycol          (make-vector ntests))
	 (controls        '())
	 (lftlst          '())
	 (hdrlst          '())
	 (bdylst          '())
	 (result          '())
	 (i               0))
    ;; controls (along bottom)
    (set! controls (dboard:make-controls commondat runs-dat))
    
    ;; create the left most column for the run key names and the test names 
    (set! lftlst (list (iup:hbox
			(iup:label) ;; (iup:valuator)
			(apply iup:vbox 
			       (map (lambda (x)		
				      (let ((res (iup:hbox #:expand "HORIZONTAL"
							   (iup:label x #:size "x15" #:fontsize "10" #:expand "HORIZONTAL")
							   (iup:textbox #:size "x15" #:fontsize "10" #:value "%" #:expand "HORIZONTAL"
									#:action (lambda (obj unk val)
										   (mark-for-update runs-dat)
										   (update-search commondat runs-dat x val))))))
					(set! i (+ i 1))
					res))
				    keynames)))))
    (let loop ((testnum  0)
	       (res      '()))
      (cond
       ((>= testnum ntests)
	;; now lftlst will be an hbox with the test keys and the test name labels
	(set! lftlst (append lftlst (list (iup:hbox  #:expand "HORIZONTAL"
						     (iup:valuator #:valuechanged_cb (lambda (obj)
										       (let ((val (string->number (iup:attribute obj "VALUE")))
											     (oldmax  (string->number (iup:attribute obj "MAX")))
											     (newmax  (* 10 (length *alltestnamelst*))))
											 (dboard:commondat-please-update-set! commondat #t)
											 (dboard:tabdat-start-test-offset-set! runs-dat (inexact->exact (round (/ val 10))))
											 (debug:print 6 *default-log-port* "(dboard:tabdat-start-test-offset runs-dat) " (dboard:tabdat-start-test-offset runs-dat) " val: " val " newmax: " newmax " oldmax: " oldmax)
											 (if (< val 10)
											     (iup:attribute-set! obj "MAX" newmax))
											 ))
								   #:expand "VERTICAL" 
								   #:orientation "VERTICAL"
								   #:min 0
								   #:step 0.01)
						     (apply iup:vbox (reverse res)))))))
       (else
	(let ((labl  (iup:button "" 
				 #:flat "YES" 
				 #:alignment "ALEFT"
					; #:image img1
					; #:impress img2
				 #:size "x15"
				 #:expand "HORIZONTAL"
				 #:fontsize "10"
				 #:action (lambda (obj)
					    (mark-for-update tabdat)
					    (toggle-hide testnum uidat))))) ;; (iup:attribute obj "TITLE"))))
	  (vector-set! lftcol testnum labl)
	  (loop (+ testnum 1)(cons labl res))))))
    ;; 
    (let loop ((runnum  0)
	       (keynum  0)
	       (keyvec  (make-vector nkeys))
	       (res    '()))
      (cond ;; nb// no else for this approach.
       ((>= runnum nruns) #f)
       ((>= keynum nkeys) 
	(vector-set! header runnum keyvec)
	(set! hdrlst (cons (apply iup:vbox (reverse res)) hdrlst))
	(loop (+ runnum 1) 0 (make-vector nkeys) '()))
       (else
	(let ((labl  (iup:label "" #:size "60x15" #:fontsize "10" #:expand "HORIZONTAL"))) ;; #:expand "HORIZONTAL"
	  (vector-set! keyvec keynum labl)
	  (loop runnum (+ keynum 1) keyvec (cons labl res))))))
    ;; By here the hdrlst contains a list of vboxes containing nkeys labels
    (let loop ((runnum  0)
	       (testnum 0)
	       (testvec  (make-vector ntests))
	       (res    '()))
      (cond
       ((>= runnum nruns) #f) ;;  (vector tableheader runsvec))
       ((>= testnum ntests) 
	(vector-set! runsvec runnum testvec)
	(set! bdylst (cons (apply iup:vbox (reverse res)) bdylst))
	(loop (+ runnum 1) 0 (make-vector ntests) '()))
       (else
	(let* ((button-key (mkstr runnum testnum))
	       (butn       (iup:button
			    "" ;; button-key 
			    #:size "60x15" 
			    #:expand "HORIZONTAL"
			    #:fontsize "10"
			    #:button-cb
			    (lambda (obj a pressed x y btn . rem)
			      ;; (print "pressed= " pressed " x= " x " y= " y " rem=" rem " btn=" btn " string? " (string? btn))
			      (if  (substring-index "3" btn)
				   (if (eq? pressed 1)
				       (let* ((toolpath (car (argv)))
					      (buttndat (hash-table-ref (dboard:tabdat-buttondat runs-dat) button-key))
					      (test-id  (db:test-get-id (vector-ref buttndat 3)))
					      (run-id   (db:test-get-run_id (vector-ref buttndat 3)))
					      (run-info (rmt:get-run-info run-id))
					      (target   (rmt:get-target run-id))
					      (runname  (db:get-value-by-header (db:get-rows run-info)
										(db:get-header run-info) "runname"))
					      (test-name (db:test-get-testname (rmt:get-test-info-by-id run-id test-id)))
					      (testpatt  (let ((tlast (rmt:tasks-get-last target runname)))
							   (if tlast
							       (let ((tpatt (tasks:task-get-testpatt tlast)))
								 (if (member tpatt '("0" 0)) ;; known bad historical value - remove in 2017
								     "%"
								     tpatt))
							       "%"))))
					 (iup:show (dashboard:popup-menu buttndat run-id test-id target runname test-name testpatt) ;; popup-menu
						   #:x 'mouse
						   #:y 'mouse
						   #:modal? "NO")
					 ;; (print "got here")
					 ))
				   (if (eq? pressed 0)
				       (let* ((toolpath (car (argv)))
					      (buttndat (hash-table-ref (dboard:tabdat-buttondat runs-dat) button-key))
					      (test-id  (db:test-get-id (vector-ref buttndat 3)))
					      (run-id   (db:test-get-run_id (vector-ref buttndat 3)))
					      (cmd  (conc toolpath " -test " run-id "," test-id "&")))
					 (system cmd)))
				   )))))
	  (hash-table-set! (dboard:tabdat-buttondat runs-dat) button-key (vector 0 "100 100 100" button-key #f #f)) 
	  (vector-set! testvec testnum butn)
	  (loop runnum (+ testnum 1) testvec (cons butn res))))))
    ;; now assemble the hdrlst and bdylst and kick off the dialog
    (iup:show
     (iup:dialog 
      #:title (conc "Megatest dashboard " (current-user-name) ":" *toppath*)
      #:menu (dcommon:main-menu)
      (let* ((runs-view (iup:vbox
			 (apply iup:hbox 
				(cons (apply iup:vbox lftlst)
				      (list 
				       (iup:vbox
					;; the header
					(apply iup:hbox (reverse hdrlst))
					(apply iup:hbox (reverse bdylst))))))
			 ;; controls
			 ))
	     ;; (data (dboard:tabdat-init (make-d:data)))
	     (tabs (iup:tabs
		    #:tabchangepos-cb (lambda (obj curr prev)
					(debug:catch-and-dump
					 (lambda ()
					   (let* ((tab-num (dboard:commondat-curr-tab-num commondat))
						  (tabdat  (dboard:common-get-tabdat commondat tab-num: tab-num)))
					     (dboard:tabdat-layout-update-ok-set! tabdat #f))
					   (dboard:commondat-curr-tab-num-set! commondat curr)
					   (let* ((tab-num (dboard:commondat-curr-tab-num commondat))
						  (tabdat  (dboard:common-get-tabdat commondat tab-num: tab-num)))
					     (dboard:commondat-please-update-set! commondat #t)
					     (dboard:tabdat-layout-update-ok-set! tabdat #t)))
					 "tabchangepos"))
		    (dashboard:summary commondat stats-dat tab-num: 0)
		    runs-view
		    (dashboard:one-run commondat onerun-dat tab-num: 2)
		    ;; (dashboard:new-view db data new-view-dat tab-num: 3)
		    (dashboard:run-controls commondat runcontrols-dat tab-num: 3)
		    (dashboard:run-times commondat runtimes-dat tab-num: 4)
		    )))
	;; (set! (iup:callback tabs tabchange-cb:) (lambda (a b c)(print "SWITCHED TO TAB: " a " " b " " c)))
	(iup:attribute-set! tabs "TABTITLE0" "Summary")
	(iup:attribute-set! tabs "TABTITLE1" "Runs")
	(iup:attribute-set! tabs "TABTITLE2" "Run Summary")
	(iup:attribute-set! tabs "TABTITLE3" "Run Control")
	(iup:attribute-set! tabs "TABTITLE4" "Run Times")
	;; (iup:attribute-set! tabs "TABTITLE3" "New View")
	;; (iup:attribute-set! tabs "TABTITLE4" "Run Control")
	(iup:attribute-set! tabs "BGCOLOR" "190 190 190")
	;; make the iup tabs object available (for changing color for example)
	(dboard:commondat-hide-not-hide-tabs-set! commondat tabs)
	;; now set up the tabdat lookup
	(dboard:common-set-tabdat! commondat 0 stats-dat)
	(dboard:common-set-tabdat! commondat 1 runs-dat)
	(dboard:common-set-tabdat! commondat 2 onerun-dat)
	(dboard:common-set-tabdat! commondat 3 runcontrols-dat)
	(dboard:common-set-tabdat! commondat 4 runtimes-dat)
	(iup:vbox
	 tabs
	 ;; controls
	 ))))
    (vector keycol lftcol header runsvec)))

(define (dboard:setup-num-rows tabdat)
  (if (or (args:get-arg "-rows")
	  (get-environment-variable "DASHBOARDROWS" ))
      (begin
	(dboard:tabdat-num-tests-set! tabdat (string->number
					      (or (args:get-arg "-rows")
						  (get-environment-variable "DASHBOARDROWS"))))
	(update-rundat tabdat "%" (dboard:tabdat-numruns tabdat) "%/%" '()))
      (dboard:tabdat-num-tests-set! tabdat (min (max (update-rundat tabdat "%" (dboard:tabdat-numruns tabdat) "%/%" '()) 8) 20))))
  
(define *tim* (iup:timer))
(define *ord* #f)
(iup:attribute-set! *tim* "TIME" 300)
(iup:attribute-set! *tim* "RUN" "YES")

(define *last-recalc-ended-time* 0)

(define (dashboard:been-changed)
  (> (file-modification-time (dboard:tabdat-dbfpath tabdat)) (dboard:tabdat-last-db-update tabdat)))

(define (dashboard:set-db-update-time)
  (dboard:tabdat-last-db-update-set! tabdat (file-modification-time (dboard:tabdat-dbfpath tabdat))))

(define (dashboard:recalc modtime please-update-buttons last-db-update-time)
  (or please-update-buttons
      (and (> (current-milliseconds)(+ *last-recalc-ended-time* 150))
	   (> modtime last-db-update-time)
	   (> (current-seconds)(+ last-db-update-time 1)))))

;; (define *monitor-db-path* #f)
(define *last-monitor-update-time* 0)

;; Force creation of the db in case it isn't already there.
(tasks:open-db)

(define (dashboard:get-youngest-run-db-mod-time tabdat)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 *default-log-port* "WARNING: error in accessing databases in get-youngest-run-db-mod-time: " ((condition-property-accessor 'exn 'message) exn))
     (current-seconds)) ;; something went wrong - just print an error and return current-seconds
   (apply max (map (lambda (filen)
		     (file-modification-time filen))
		   (glob (conc (dboard:tabdat-dbdir tabdat) "/*.db"))))))

(define (dashboard:monitor-changed? commondat tabdat)
  (let* ((run-update-time (current-seconds))
	 (monitor-db-path (dboard:tabdat-monitor-db-path tabdat))
	 (monitor-modtime (if (and monitor-db-path (file-exists? monitor-db-path))
			      (file-modification-time monitor-db-path)
			      -1)))
    (if (and (eq? (dboard:commondat-curr-tab-num commondat) 0)
	     (or (> monitor-modtime *last-monitor-update-time*)
		 (> (- run-update-time *last-monitor-update-time*) 5))) ;; update every 1/2 minute just in case
	(begin
	  (set! *last-monitor-update-time* run-update-time) ;; monitor-modtime)
	  #t)
	#f)))

(define (dashboard:database-changed? commondat tabdat)
  (let* ((run-update-time (current-seconds))
	 (modtime         (dashboard:get-youngest-run-db-mod-time tabdat)) ;; NOTE: ensure this is tabdat!! 
	 (recalc          (dashboard:recalc modtime (dboard:commondat-please-update commondat) (dboard:tabdat-last-db-update tabdat))))
     (dboard:commondat-please-update-set! commondat #f)
     recalc))

;; point inside line
;;
(define-inline (dashboard:px-between px lx1 lx2)
  (and (< lx1 px)(> lx2 px)))

;; can a bar be placed in row "rownum" covering x1 to x2 without overlapping with existing 
;; bars? Use num-rows to check that a block will fit from rownum to (+ rownum num-rows)
;;
(define (dashboard:row-collision rowhash rownum x1 x2 #!key (num-rows #f))
  (let ((lastrow   (if num-rows (+ rownum num-rows) rownum)))
    (let loop ((i      0)
	       (rowdat (hash-table-ref/default rowhash rownum '())))
      (if (null? rowdat)
	  #f
	  (let rowloop ((bar (car rowdat))
			(tal (cdr rowdat)))
	    (let ((bx1 (car bar))
		  (bx2 (cdr bar)))
	      (cond
	       ;; newbar x1 inside bar
	       ((dashboard:px-between x1 bx1 bx2) #t)
	       ((dashboard:px-between x2 bx1 bx2) #t)
	       ((and (<= x1 bx1)(>= x2 bx2))      #t)
	       (else (if (null? tal)
			 (if (< i lastrow)
			     (loop (+ i 1)
				   (hash-table-ref/default rowhash (+ rownum i) '()))
			     #f)
			 (rowloop (car tal)(cdr tal)))))))))))

(define (dashboard:add-bar rowhash rownum x1 x2 #!key (num-rows 0))
  (let loop ((i 0))
    (hash-table-set! rowhash 
		     (+ i rownum)
		     (cons (cons x1 x2) 
			   (hash-table-ref/default rowhash (+ i rownum) '())))
    (if (< i num-rows)
	(loop (+ i 1)))))

;; get min or max, use > for max and < for min, this works around the limits on apply
;;
(define (dboard:min-max comp lst)
  (if (null? lst)
      #f ;; better than an exception for my needs
      (fold (lambda (a b)
	      (if (comp a b) a b))
	    (car lst)
	    lst)))

;; sort a list of test-ids by the event _time using a hash table of id => testdat
;;
(define-inline (dboard:sort-testsdat-by-event-time test-ids tests-ht)
  (sort test-ids
	(lambda (a b)
	  (< (db:test-get-event_time (hash-table-ref tests-ht a))
	     (db:test-get-event_time (hash-table-ref tests-ht b))))))

;; first group items into lists, then sort by time
;; finally sort by first item time
;; 
;; NOTE: we are returning lists of lists of ids!
;;
(define (dboard:tests-sort-by-time-group-by-item testsdat)
  (let ((test-ids (hash-table-keys testsdat)))
    (if (null? test-ids)
	test-ids
	;; now group all tests by testname tname => (id1 id2 ...), tname2 => ( ...
	(let* ((test-ids-by-name
		(let ((ht (make-hash-table)))
		  (for-each
		   (lambda (tdat)
		     (let ((testname (db:test-get-testname tdat))
			   (test-id  (db:test-get-id tdat)))
		       (hash-table-set! 
			ht 
			testname
			(cons test-id (hash-table-ref/default ht testname '())))))
		   (hash-table-values testsdat))
		  ht)))
	;; remove toplevel tests from iterated tests, sort tests in the list by event time
	(for-each 
	 (lambda (testname)
	   (let ((tests-id-lst (hash-table-ref test-ids-by-name testname)))
	     (if (> (length tests-id-lst) 1) ;; must be iterated
		 (let ((item-tests (filter (lambda (tid) ;; filter out toplevel tests
					     (let ((tdat (hash-table-ref testsdat tid)))
					       (not (equal? (db:test-get-item-path tdat) ""))))
					   tests-id-lst)))
		   (if (not (null? item-tests)) ;; resist bad data, generally should not fail this condition
		       (hash-table-set! test-ids-by-name 
					testname 
					(dboard:sort-testsdat-by-event-time item-tests testsdat)))))))
	 (hash-table-keys test-ids-by-name))
	;; finally sort by the event time of the first test
	(sort (hash-table-values test-ids-by-name)
	      (lambda (a b)
		(< (db:test-get-event_time (hash-table-ref testsdat (car a)))
		   (db:test-get-event_time (hash-table-ref testsdat (car b))))))))))

;; run times tab data updater
;;
(define (dashboard:run-times-tab-run-data-updater commondat tabdat tab-num)
  (let* ((runs-dat      (rmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f))
	 (runs-header   (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
	 (runs-hash     (let ((ht (make-hash-table)))
			  (for-each (lambda (run)
				      (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
				    (vector-ref runs-dat 1))
			  ht))
	 (run-ids       (sort (filter number? (hash-table-keys runs-hash))
			      (lambda (a b)
				(let* ((record-a (hash-table-ref runs-hash a))
				       (record-b (hash-table-ref runs-hash b))
				       (time-a   (db:get-value-by-header record-a runs-header "event_time"))
				       (time-b   (db:get-value-by-header record-b runs-header "event_time")))
				  (< time-a time-b)))))
	 (tb            (dboard:tabdat-runs-tree tabdat))
	 (num-runs      (length (hash-table-keys runs-hash)))
	 (update-start-time (current-seconds))
	 (inc-mode      #f))
    ;; fill in the tree
    (if (and tb 
	     (not inc-mode))
	(for-each
	 (lambda (run-id)
	   (let* ((run-record (hash-table-ref/default runs-hash run-id #f))
		  (key-vals   (map (lambda (key)(db:get-value-by-header run-record runs-header key))
				   (dboard:tabdat-keys tabdat)))
		  (run-name   (db:get-value-by-header run-record runs-header "runname"))
		  (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
		  (run-path   (append key-vals (list run-name)))
		  (existing   (tree:find-node tb run-path)))
	     (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
		 (begin
		   (hash-table-set! (dboard:tabdat-run-keys tabdat) run-id run-path)
		   ;; Here we update the tests treebox and tree keys
		   (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
				  userdata: (conc "run-id: " run-id))
		   (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path run-id)
		   ;; (set! colnum (+ colnum 1))
		   ))))
	 run-ids))
    (print "Updating rundat")
    (if (dboard:tabdat-keys tabdat) ;; have keys yet?
	(let* ((num-keys (length (dboard:tabdat-keys tabdat)))
	       (targpatt (map (lambda (k v)
				(list k v))
			      (dboard:tabdat-keys tabdat)
			      (take (append (or (dboard:tabdat-target tabdat);; (string-split (dboard: "/")
						'("%" "%"))
					    (make-list num-keys "%"))
				    num-keys)
			      ))
	       (runpatt   (if (dboard:tabdat-target tabdat)
			     (last (dboard:tabdat-target tabdat))
			     "%"))
	       (testpatt  (or (dboard:tabdat-test-patts tabdat) "%"))
	       (filtrstr  (conc targpatt "/" runpatt "/" testpatt)))
	  (print "targpatt: " targpatt " runpatt: " runpatt " testpatt: " testpatt)

	  (if (not (equal? (dboard:tabdat-last-filter-str tabdat) filtrstr))
	      (let ((dwg (dboard:tabdat-drawing tabdat)))
		(print "reseting drawing")
		(dboard:tabdat-layout-update-ok-set! tabdat #f)
		(vg:drawing-libs-set! dwg (make-hash-table))
		(vg:drawing-insts-set! dwg (make-hash-table))
		(vg:drawing-cache-set! dwg '())
		(dboard:tabdat-allruns-by-id-set! tabdat (make-hash-table))
		;; (dboard:tabdat-allruns-set! tabdat '())
		(dboard:tabdat-max-row-set! tabdat 0)
		(dboard:tabdat-last-filter-str-set! tabdat filtrstr)))
	  (update-rundat tabdat
			 runpatt
			 ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "runname" "%") 
			 10  ;; (dboard:tabdat-numruns tabdat)
			 testpatt ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
			 ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "item-name" "%")
			 
			 targpatt
			 
			 ;; old method 
			 ;; (let ((res '()))
			 ;;   (for-each (lambda (key)
			 ;;      	 (if (not (equal? key "runname"))
			 ;;      	     (let ((val (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) key #f)))
			 ;;      	       (if val (set! res (cons (list key val) res))))))
			 ;;             (dboard:tabdat-dbkeys tabdat))
			 ;;   res)
			 )))))

;; run times canvas updater
;;
(define (dashboard:run-times-tab-canvas-updater commondat tabdat tab-num)
  (let ((cnv (dboard:tabdat-cnv tabdat))
	(dwg (dboard:tabdat-drawing tabdat))
	(mtx (dboard:tabdat-runs-mutex tabdat))
	(vch (dboard:tabdat-view-changed tabdat)))
    (if (and cnv dwg vch)
	(begin
	  (vg:drawing-xoff-set! dwg (dboard:tabdat-xadj tabdat))
	  (vg:drawing-yoff-set! dwg (dboard:tabdat-yadj tabdat))
	  (mutex-lock! mtx)
	  (canvas-clear! cnv)
	  (vg:draw dwg tabdat)
	  (mutex-unlock! mtx)
	  (dboard:tabdat-view-changed-set! tabdat #f)))))
  
;; doesn't work.
;;
;;(define (gotoescape tabdat escape)
;;  (or (dboard:tabdat-layout-update-ok tabdat)
;;      (escape #t)))

(define (dboard:graph-db-open dbstr)
  (let* ((parts (string-split dbstr ":"))
	 (dbpth (if (< (length parts) 2) ;; assume then a filename was provided
		    dbstr
		    (if (equal? (car parts) "sqlite3")
			(cadr parts)
			(begin
			  (debug:print 0 *default-log-port* "ERROR: I only know sqlite3 databases for now: " dbstr)
			  #f)))))
    (if (and dbpth (file-read-access? dbpth))
	(let ((db (sqlite3:open-database dbpth))) ;; (open-database dbpth)))
	  (sqlite3:set-busy-handler! db (make-busy-timeout 10000))
	  db)
	#f)))

;; sqlite3:path tablename timefieldname varfieldname field1 field2 ...
;;
(define (dboard:graph-read-data cmdstring tstart tend)
  (let* ((parts (string-split cmdstring))) ;; spaces not allowed
    (if (< (length parts) 6) ;; sqlite3:path tablename timefieldname varfname valfname field1 field2 ...
	(debug:print 0 *default-log-port* "ERROR: malformed graph line: " cmdstring)
	(let* ((dbdef  (list-ref parts 0))
	       (tablen (list-ref parts 1))
	       (timef  (list-ref parts 2))
	       (varfn  (list-ref parts 3))
	       (valfn  (list-ref parts 4))
	       (fields (cdr  (cddddr parts)))
	       (db     (dboard:graph-db-open dbdef))
	       (res-ht (make-hash-table)))
	  (if db
	      (begin
		(for-each
		 (lambda (fieldname) ;; fields
		   (let ((all-dat-qrystr (conc "SELECT " timef "," varfn "," valfn " FROM " tablen " WHERE " varfn "='" fieldname "' AND " timef " >= " tstart " AND " timef " <= " tend " ORDER BY " timef " ASC"))
			 (zeroth-point   (conc "SELECT " timef "," varfn "," valfn " FROM " tablen " WHERE " varfn "='" fieldname "' AND " timef " < " tstart " LIMIT 1")))
		     (print "all-dat-qrystr: " all-dat-qrystr)
		     (hash-table-set! res-ht fieldname ;; (fetch-rows (sql db qrystr)))))
				      (reverse
				       (sqlite3:fold-row
					(lambda (res t var val)
					  (cons (vector t var val) res))
					'() db all-dat-qrystr)))
		     (let ((zeropt (handle-exceptions
				    exn
				    #f
				    (sqlite3:first-row db all-dat-qrystr))))
		       (if zeropt ;; NOTE: Add zeropt to the beginning of the list as the list was reversed above.
			   (hash-table-set! res-ht
					    fieldname
					    (cons
					     (apply vector tstart (cdr zeropt))						    
					     (hash-table-ref/default res-ht fieldname '())))))))
		 fields)
		res-ht)
	      #f)))))
  
;; graph data 
;;  tsc=timescale, tfn=function; time->x
;;
(define (dboard:graph commondat tabdat tabnum llx lly ulx uly tstart tend tsc tfn compname cmargin)
  (let* ((dwg      (dboard:tabdat-drawing tabdat))
	 (lib      (vg:get/create-lib dwg "runslib"))
	 (cnv      (dboard:tabdat-cnv tabdat))
	 (dur      (- tstart tend)) ;; time duration
	 (cmp      (vg:get-component dwg "runslib" compname))
	 (cfg      (configf:get-section *configdat* "graph"))
	 (stdcolor (vg:rgb->number 120 130 140))
	 (delta-y  (- uly lly)))
    (vg:add-obj-to-comp
     cmp 
     (vg:make-rect-obj llx lly ulx uly))
    (vg:add-obj-to-comp
     cmp
     (vg:make-text-obj (- (tfn tstart) 10)(- lly 10)(seconds->year-week/day-time tstart)))
    (let*-values (((span timeunit time-blk first timesym) (common:find-start-mark-and-mark-delta tstart tend)))
		 (let loop ((mark  first)
			    (count 0))
		   (let* ((smark (tfn mark))           ;; scale the mark
			  (mark-delta (quotient (- mark tstart) time-blk)) ;; how far from first mark
			  (label      (conc (* count span) timesym))) ;; was mark-delta
		     (if (> count 2)
			 (begin
			   (vg:add-obj-to-comp
			    cmp
			    (vg:make-rect-obj (- smark 1)(- lly 2)(+ smark 1) lly))
			   (vg:add-obj-to-comp
			    cmp
			    (vg:make-text-obj (- smark 1)(- lly 10) label))))
		     (if (< mark (- tend time-blk))
			 (loop (+ mark time-blk)(+ count 1))))))
    (for-each 
     (lambda (cf)
       (let* ((alldat  (dboard:graph-read-data (cadr cf) tstart tend)))
	 (if alldat
	     (for-each
	      (lambda (fieldn)
		(let* ((dat     (hash-table-ref alldat fieldn ))
		       (vals    (map (lambda (x)(vector-ref x 2)) dat)))
                  (print "RA : dat " dat " vals " vals " fieldn " fieldn " lly " lly " llx " llx)
		  (if (not (null? vals))
		      (let* ((maxval   (apply max vals))
			     (minval   (apply min vals))
			     (yoff     (- minval lly)) ;;  minval))
			     (deltaval (- maxval minval))
			     (yscale   (/ delta-y (if (eq? deltaval 0) 1 deltaval)))
			     (yfunc    (lambda (y)(+ lly (* yscale (- y minval)))))) ;; (lambda (y)(* (+ y yoff) yscale))))
                        (print (car cf) "; maxval: " maxval " minval: " minval " deltaval: " deltaval " yscale: " yscale)
;; (lambda (y)(* (+ y yoff) yscale))))
			;; (print (car cf) "; maxval: " maxval " minval: " minval " deltaval: " deltaval " yscale: " yscale)
			(vg:add-obj-to-comp
			 cmp 
			 (vg:make-text-obj (- llx 10)(yfunc maxval) (conc maxval)))
			(vg:add-obj-to-comp
			 cmp 
			 (vg:make-text-obj (- llx 10)(yfunc minval) (conc minval)))
			(fold 
			 (lambda (next prev)  ;; #(time ? val) #(time ? val)
			   (if prev
			       (let* ((yval       (vector-ref prev 2))
				      (last-tval  (tfn   (vector-ref prev 0)))
				      (last-yval  (yfunc yval)) ;; (+ lly (* yscale (vector-ref prev 2))))
				      (curr-tval  (tfn   (vector-ref next 0))))
				 (if (>= curr-tval last-tval)
				     (vg:add-obj-to-comp
				      cmp 
				      (vg:make-rect-obj last-tval lly curr-tval last-yval ;; (- stval 2) lly (+ stval 2)(+ lly (* yval yscale))
							fill-color: stdcolor
							line-color: stdcolor))
				     (print "ERROR: curr-tval is not > last-tval; curr-tval " curr-tval ", last-tval " last-tval))))
			   next)
			 ;; for init create vector tstart,0
			 #f ;; (vector tstart minval minval)
			 dat)
			   
			 ;; (for-each
			;;  (lambda (dpt)
			;;    (let* ((tval  (vector-ref dpt 0))
			 ;; 	  (yval  (vector-ref dpt 2))
			;; 	  (stval (tfn tval))
			;; 	  (syval (yfunc yval)))
			;;      (vg:add-obj-to-comp
			;;       cmp 
			;;       (vg:make-rect-obj (- stval 2) lly (+ stval 2)(+ lly (* yval yscale))
			;; 			fill-color: stdcolor))))
			;;  dat)
			)))) ;; for each data point in the series
	      (hash-table-keys alldat)))))
     cfg)))
	 
;; run times tab
;;
(define (dashboard:run-times-tab-layout-updater commondat tabdat tab-num)
  ;; each test is an object in the run component
  ;; each run is a component
  ;; all runs stored in runslib library
  (let escapeloop ((escape #f))
    (if (and (not escape)
	     tabdat)
	(let* ((canvas-margin 10)
	       (not-done-runs (dboard:tabdat-not-done-runs tabdat))
	       (mtx           (dboard:tabdat-runs-mutex tabdat))
	       (drawing      (dboard:tabdat-drawing tabdat))
	       (runslib      (vg:get/create-lib drawing "runslib")) ;; creates and adds lib
	       (allruns      (dboard:tabdat-allruns tabdat))
	       (num-runs     (length allruns))
	       (cnv          (dboard:tabdat-cnv tabdat))
	       (compact-layout (dboard:tabdat-compact-layout tabdat))
	       (row-height     (if compact-layout 2 10))
	       (graph-height 120)
	       (run-to-run-margin 25))
	  (dboard:tabdat-layout-update-ok-set! tabdat #t)
	  (if (canvas? cnv)
	      (let*-values (((sizex sizey sizexmm sizeymm) (canvas-size cnv))
			    ((originx originy)             (canvas-origin cnv))
			    ((calc-y)                      (lambda (rownum)
							     (- (/ sizey 2)
								(* rownum row-height))))
			    ((fixed-originx)               (if (dboard:tabdat-originx tabdat)
							       (dboard:tabdat-originx tabdat)
							       (begin
							 	 (dboard:tabdat-originx-set! tabdat originx)
							 	 originx)))
			    ((fixed-originy)               (if (dboard:tabdat-originy tabdat)
							       (dboard:tabdat-originy tabdat)
							       (begin
								 (dboard:tabdat-originy-set! tabdat originy)
								 originy))))
		;; (print "allruns: " allruns)
		(let runloop ((rundat   (car allruns))
			      (runtal   (cdr allruns))
			      (run-num   1)
			      (doneruns '()))
		  (let* ((run         (dboard:rundat-run rundat))
			 (rowhash     (make-hash-table)) ;; store me in tabdat
			 (key-val-dat (dboard:rundat-key-vals rundat))
			 (run-id      (db:get-value-by-header run (dboard:tabdat-header tabdat) "id"))
			 (key-vals    (append key-val-dat
					      (list (let ((x (db:get-value-by-header run (dboard:tabdat-header tabdat) "runname")))
						      (if x x "")))))
			 (run-key  (string-intersperse key-vals "\n"))
			 (run-full-name (string-intersperse key-vals "/"))
			 (curr-run-start-row  (dboard:tabdat-max-row tabdat)))
		    ;; (print "run: " run-full-name " curr-run-start-row: " curr-run-start-row)
		    (if (not (vg:lib-get-component runslib run-full-name))
			(let* ((hierdat   (if (or (dboard:rundat-data-changed rundat) ;; attempt to not sort when possible.
						  (not (dboard:rundat-hierdat rundat)))
					      (let ((hd (dboard:tests-sort-by-time-group-by-item (dboard:rundat-tests rundat)))) ;; hierarchial list of ids
						(dboard:rundat-hierdat-set! rundat hd)
						hd)
					      (dboard:rundat-hierdat rundat)))
			       (tests-ht  (dboard:rundat-tests rundat))
			       (all-tids  (hash-table-keys   tests-ht)) ;; (apply append hierdat)) ;; was testsdat
			       (testsdat  (hash-table-values tests-ht))
			       (runcomp   (vg:comp-new));; new component for this run
			       (rows-used (make-hash-table)) ;; keep track of what parts of the rows are used here row1 = (obj1 obj2 ...)
			       ;; (row-height 4)
			       (run-start  (dboard:min-max < (map db:test-get-event_time testsdat)))
			       (run-end    (dboard:min-max > (map (lambda (t)(+ (db:test-get-event_time t)(db:test-get-run_duration t))) testsdat)))
			       (timeoffset (- run-start)) ;; (+ fixed-originx canvas-margin) run-start))
			       (run-duration (- run-end run-start))
			       (timescale  (/ (- sizex (* 2 canvas-margin))
					      (if (> run-duration 0)
						  run-duration
						  (current-seconds)))) ;; a least lously guess
			       (maptime    (lambda (tsecs)(* timescale (+ tsecs timeoffset))))
			       (num-tests  (length hierdat))
			       (tot-tests  (length testsdat))
			       (width      (* timescale run-duration))
			       (graph-lly  (calc-y (/ -50 row-height)))
			       (graph-uly  (- (calc-y 0) canvas-margin))
			       (sec-per-50pt (/ 50 timescale))
			       )
			  (print "timeoffset: " timeoffset " timescale: " timescale " run-duration: " (seconds->hr-min-sec run-duration) " width: " width " sec-per-50pt: " sec-per-50pt)
			  ;; (print "timescale: " timescale " timeoffset: " timeoffset " sizex: " sizex " originx: " originx)
			  (mutex-lock! mtx)
			  (vg:add-comp-to-lib runslib run-full-name runcomp)
			  ;; Have to keep moving the instantiated box as it is anchored at the lower left
			  ;; this should have worked for x in next statement? (maptime run-start)
			  ;; add 60 to make room for the graph
			  (vg:instantiate drawing "runslib" run-full-name run-full-name 8 (- (calc-y curr-run-start-row) (+ 5 graph-height run-to-run-margin)))
			  (mutex-unlock! mtx)
			  ;; (set! run-start-row (+ max-row 2))
			  ;; (dboard:tabdat-start-row-set! tabdat (+ new-run-start-row 1))
			  ;; get tests in list sorted by event time ascending
			  (let testsloop ((test-ids  (car hierdat))              ;; loop on tests (NOTE: not items!)
					  (tests-tal (cdr hierdat))
					  (test-num  1))
			    (let ((iterated     (> (length test-ids) 1))
				  (first-rownum #f)
				  (num-items    (length test-ids)))
			      (let testitemloop ((test-id  (car test-ids))    ;; loop on test or test items
						 (tidstal  (cdr test-ids))
						 (item-num 1)
						 (test-objs '()))
				(let* ((testdat      (hash-table-ref tests-ht test-id))
				       (event-time   (maptime (db:test-get-event_time   testdat)))
				       (test-duration (* timescale (db:test-get-run_duration testdat)))
				       (end-time     (+ event-time test-duration))
				       (test-name    (db:test-get-testname     testdat))
				       (item-path    (db:test-get-item-path    testdat))
				       (state         (db:test-get-state       testdat))
				       (status        (db:test-get-status      testdat))
				       (test-fullname (conc test-name "/" item-path))
				       (name-color    (gutils:get-color-for-state-status state status))
				       (new-test-objs 
					(let loop ((rownum 0)) ;;  new-run-start-row)) ;; (+ start-row 1)))
					  (if (dashboard:row-collision rowhash rownum event-time end-time)
					      (loop (+ rownum 1))
					      (let* ((title   (if iterated (if compact-layout #f item-path) test-name))
						     (lly     (calc-y rownum)) ;; (- sizey (* rownum row-height)))
						     (uly     (+ lly row-height))
						     (use-end (if (< (- end-time event-time) 3)(+ event-time 3) end-time)) ;; if short grow it a little to give the user something to click on
						     (obj     (vg:make-rect-obj event-time lly use-end uly
										fill-color: (vg:iup-color->number (car name-color))
										text: title
										font: "Helvetica -10")) 
						     (bar-end (+ 5 (max use-end
									(+ 3 event-time 
									   (if compact-layout
									       0
									       (* (string-length title) 10))))))) ;; 8 pixels per letter
						;; (if iterated
						;;     (dashboard:add-bar rowhash (- rownum 1) event-time end-time num-rows: (+ 1 num-items))
						;; (if (not first-rownum)
						;;     (begin
						;;       (dashboard:row-collision rowhash (- rownum 1) event-time end-time num-rows: num-items)
						;;       (set! first-rownum rownum)))
						(dboard:tabdat-max-row-set! tabdat (max (+ curr-run-start-row rownum)
											(dboard:tabdat-max-row tabdat))) ;; track the max row used
						;; bar-end has some margin for text - accounting for text in extents not yet working.
						(dashboard:add-bar rowhash rownum event-time bar-end) ;; (+ end-time 5))
						(vg:add-obj-to-comp runcomp obj)
						;; (vg:instance-move drawing run-full-name 0 (calc-y (dboard:tabdat-max-row tabdat)))
						(dboard:tabdat-view-changed-set! tabdat #t)
						(cons obj test-objs))))))
				  ;; (print "event_time: " (db:test-get-event_time   testdat) " mapped event_time: " event-time)
				  ;; (print "run-duration: "  (db:test-get-run_duration testdat) " mapped run_duration: " run-duration)
				  (if (> item-num 50)
				      (if (eq? 0 (modulo item-num 50))
					  (print "processing " run-num " of " num-runs " runs " item-num " of " num-items " of test " test-name ", " test-num " of " num-tests " tests")))
				  ;; (print "test-name: " test-name " event-time: " event-time " run-duration: " run-duration)
				  (let ((newdoneruns (cons rundat doneruns)))
				    (if (null? tidstal)
					(if iterated
					    (let* ((xtents (vg:get-extents-for-objs drawing new-test-objs))
						   (llx (- (car xtents)  10))
						   (lly (- (cadr xtents) 10))
						   (ulx (+ 5 (caddr xtents)))
						   (uly (+ 10 (cadddr xtents))))
					      ;; (dashboard:add-bar rowhash 0 llx ulx num-rows:  num-items)
					      ;; This is the box around the tests of an iterated test
					      (vg:add-obj-to-comp runcomp (vg:make-rect-obj llx lly ulx uly
											    text:  (db:test-get-testname (hash-table-ref tests-ht (car test-ids)))
											    line-color:  (vg:rgb->number  0 0 255 a: 128)
											    font: "Helvetica -10"))
					      ;; (vg:instance-move drawing run-full-name 0 (dboard:tabdat-max-row tabdat))
					      (dboard:tabdat-view-changed-set! tabdat #t))) ;; trigger a redraw
					(if (dboard:tabdat-layout-update-ok tabdat)
					    (testitemloop (car tidstal)(cdr tidstal)(+ item-num 1) new-test-objs)
					    (escapeloop #t) ;; (dboard:tabdat-layout-update-ok tabdat)
					    )))))
			      ;; If it is an iterated test put box around it now.
			      (if (not (null? tests-tal))
				  (if #f ;; (> (- (current-seconds) update-start-time) 5)
				      (print "drawing runs taking too long")
				      (if (dboard:tabdat-layout-update-ok tabdat)
					  (testsloop  (car tests-tal)(cdr tests-tal)(+ test-num 1))
					  (escapeloop #t) ;; (dboard:tabdat-layout-update-ok tabdat)
					  )))))
			  ;; placeholder box
			  (dboard:tabdat-max-row-set! tabdat (+ (dboard:tabdat-max-row tabdat) 1))
			  ;; (let ((y  (calc-y (dboard:tabdat-max-row tabdat)))) ;;  (- sizey (* (dboard:tabdat-max-row tabdat) row-height))))
			  ;;   (vg:add-obj-to-comp runcomp (vg:make-rect-obj 0 y 0 y)))
			  ;; instantiate the component
			  (let* ((extents   (vg:components-get-extents drawing runcomp))
				 (new-xtnts (apply vg:grow-rect 5 5 extents))
				 (llx       (list-ref new-xtnts 0))
				 (lly       (list-ref new-xtnts 1))
				 (ulx       (list-ref new-xtnts 2))
				 (uly       (list-ref new-xtnts 3))
				 (outln     (vg:make-rect-obj -5 lly ulx uly 
							      text: run-full-name
							      line-color:  (vg:rgb->number  255 0 255 a: 128))))
					;  (vg:components-get-extents d1 c1)))
			    ;; this is the box around the run
			    (mutex-lock! mtx)
			    (vg:add-obj-to-comp runcomp outln)
			    (mutex-unlock! mtx)
			    ;; this is where we have enough info to place the graph
			    (dboard:graph commondat tabdat tab-num -5 (+ uly 10) ulx (+ uly graph-height 3) run-start run-end timescale maptime run-full-name canvas-margin)
			    (dboard:tabdat-max-row-set! tabdat (+ (dboard:tabdat-max-row tabdat)(quotient (+ graph-height 40 3) row-height)))
			    ;; (vg:instance-move drawing run-full-name 0 (dboard:tabdat-max-row tabdat))
			    ))
			;; end of the run handling loop 
			(if (not (dboard:tabdat-layout-update-ok tabdat))
			    (escapeloop #t) ;; (dboard:tabdat-layout-update-ok tabdat)
			    (let ((newdoneruns (cons rundat doneruns)))
			      (if (null? runtal)
				  (begin
				    (dboard:rundat-data-changed-set! rundat #f) 
				    (dboard:tabdat-not-done-runs-set! tabdat '())
				    (dboard:tabdat-done-runs-set! tabdat allruns))
				  (if #f ;; (> (- (current-seconds) update-start-time) 5)
				      (begin
					(print "drawing runs taking too long....  have " (length runtal) " remaining")
					;; (dboard:tabdat-done-runs-set! tabdat newdoneruns) ;; taking too long? stop here!
					;; (time (vg:draw (dboard:tabdat-drawing tabdat) #t))
					(dboard:tabdat-not-done-runs-set! tabdat runtal))
				      (begin
					(if (dboard:tabdat-layout-update-ok tabdat)
					    (runloop (car runtal)(cdr runtal) (+ run-num 1) newdoneruns)
					    (escapeloop #t) ;; (dboard:tabdat-layout-update-ok tabdat)
					    ))))))))) ;;  new-run-start-row
		)))
	(debug:print 2 *default-log-port* "no tabdat for run-times-tab-updater"))))

(define (dashboard:runs-tab-updater commondat tab-num)
  (debug:catch-and-dump 
   (lambda ()
     (let ((tabdat (dboard:common-get-tabdat commondat tab-num: tab-num)))
       (update-rundat tabdat (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "runname" "%") (dboard:tabdat-numruns tabdat)
		      (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
		      ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "item-name" "%")
		      (let ((res '()))
			(for-each (lambda (key)
				    (if (not (equal? key "runname"))
					(let ((val (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) key #f)))
					  (if val (set! res (cons (list key val) res))))))
				  (dboard:tabdat-dbkeys tabdat))
			res))
       (let ((uidat (dboard:commondat-uidat commondat)))
	 (update-buttons tabdat uidat (dboard:tabdat-numruns tabdat) (dboard:tabdat-num-tests tabdat)))
       ))
   "dashboard:runs-tab-updater"))

;; ((2)
;;  (dashboard:update-run-summary-tab))
;; ((3)
;;  (dashboard:update-new-view-tab))
;; (else
;;  (dboard:common-run-curr-updater commondat)))
;; (set! *last-recalc-ended-time* (current-milliseconds))))))))

;;======================================================================
;; The heavy lifting starts here
;;======================================================================

;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(define (main)
  (if (not (args:get-arg "-skip-version-check"))(common:exit-on-version-changed))
  (let* ((commondat       (dboard:commondat-make)))
    ;; Move this stuff to db.scm? I'm not sure that is the right thing to do...
    (cond 
     ((args:get-arg "-test") ;; run-id,test-id
      (let* ((dat     (let ((d (map string->number (string-split (args:get-arg "-test") ","))))
			(if (> (length d) 1)
			    d
			    (list #f #f))))
	     (run-id  (car dat))
	     (test-id (cadr dat)))
	(if (and (number? run-id)
		 (number? test-id)
		 (>= test-id 0))
	    (examine-test run-id test-id)
	    (begin
	      (debug:print 3 *default-log-port* "INFO: tried to open test with invalid run-id,test-id. " (args:get-arg "-test"))
	      (exit 1)))))
     ;; ((args:get-arg "-guimonitor")
     ;;  (gui-monitor (dboard:tabdat-dblocal tabdat)))
     (else
      (dboard:commondat-uidat-set! commondat (make-dashboard-buttons commondat)) ;; (dboard:tabdat-dblocal data)
					  ;; (dboard:tabdat-numruns tabdat)
					  ;; (dboard:tabdat-num-tests tabdat)
					  ;; (dboard:tabdat-dbkeys tabdat)
					  ;; runs-sum-dat new-view-dat))
      ;; legacy setup of updaters for summary tab and runs tab
      ;; summary tab
      ;; (dboard:commondat-add-updater 
      ;;  commondat 
      ;;  (lambda ()
      ;; 	 (dashboard:summary-tab-updater commondat 0))
      ;;  tab-num: 0)
      ;; runs tab
      (dboard:commondat-curr-tab-num-set! commondat 0)
      ;; this next call is working and doing what it should
      (dboard:commondat-add-updater 
       commondat 
       (lambda ()
      	 (dashboard:runs-tab-updater commondat 1))
       tab-num: 1)
      (iup:callback-set! *tim*
			 "ACTION_CB"
			 (lambda (time-obj)
			   (let ((update-is-running #f))
			     (mutex-lock! (dboard:commondat-update-mutex commondat))
			     (set! update-is-running (dboard:commondat-updating commondat))
			     (if (not update-is-running)
				 (dboard:commondat-updating-set! commondat #t))
			     (mutex-unlock! (dboard:commondat-update-mutex commondat))
			     (if (not update-is-running) ;; we know that the update was not running and we now have a lock on doing an update
				 (begin
				   (dboard:common-run-curr-updaters commondat) ;; (dashboard:run-update commondat)
				   (mutex-lock! (dboard:commondat-update-mutex commondat))
				   (dboard:commondat-updating-set! commondat #f)
				   (mutex-unlock! (dboard:commondat-update-mutex commondat)))
				 ))
			   1))))
    
    (let ((th1 (make-thread (lambda ()
			      (thread-sleep! 1)
			      (dboard:common-run-curr-updaters commondat 0) ;; force update of summary tab 
			      (dboard:commondat-please-update-set! commondat #t)
			      ;; (dashboard:run-update commondat)
			      ) "update buttons once"))
	  (th2 (make-thread iup:main-loop "Main loop")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th2))))

(main)
