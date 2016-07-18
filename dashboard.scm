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

(use sqlite3 srfi-1 posix regex regex-case srfi-69 defstruct sparse-vectors)
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

(define help (conc 
	      "Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2012-2016

Usage: dashboard [options]
  -h                   : this help
  -server host:port    : connect to host:port instead of db access
  -test run-id,test-id : control test identified by testid
  -xterm run-id,test-id : Start a new xterm with specified run-id and test-id
  -guimonitor          : control panel for runs

Misc
  -rows N         : set number of rows
"))

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
	(debug:print 0 *default-log-port* "Found these updaters: " updaters " for tab-num: " tnum)
	(for-each
	 (lambda (updater)
	   (debug:print 0 *default-log-port* "Running " updater)
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
  allruns 
  allruns-by-id
  buttondat 
  cnv
  cnv-obj
  command
  command-tb 
  curr-run-id 
  curr-test-ids 
  db
  dbdir
  dbfpath
  dbkeys 
  drawing
  filters-changed
  header      
  hide-empty-runs
  hide-not-hide  ;; toggle for hide/not hide
  hide-not-hide-button
  item-test-names
  keys
  last-db-update 
  logs-textbox
  monitor-db-path
  num-tests
  numruns
  path-run-ids
  ro
  run-keys
  run-name
  runs
  runs-listbox
  runs-matrix 
  runs-tree
  searchpatts
  start-run-offset
  start-test-offset
  state-ignore-hash
  states 
  status-ignore-hash
  statuses
  target
  test-patts
  tests
  tests-tree
  tot-runs
  view-changed
  xadj
  yadj
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
  (let ((dat (make-dboard:tabdat
	      allruns-by-id:        (make-hash-table)
	      allruns:              '()
	      buttondat:            (make-hash-table)
	      curr-test-ids:        (make-hash-table)
	      dbdir:                #f
	      filters-changed:      #f
	      header:               #f 
	      hide-empty-runs:      #f
	      hide-not-hide-button: #f
	      hide-not-hide:        #t
	      item-test-names:      '()
	      last-db-update:       0
	      num-tests:            15
	      numruns:              16
	      path-run-ids:         (make-hash-table)
	      run-ids:              (make-hash-table)
	      run-keys:             (make-hash-table)
	      searchpatts:          (make-hash-table)
	      start-run-offset:     0
	      start-test-offset:    0
	      state-ignore-hash:    (make-hash-table)
	      status-ignore-hash:   (make-hash-table)
	      xadj:                 0
	      yadj:                 0
	      view-changed:         #t
	      )))
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

;; data for runs, tests etc
;;
(defstruct dboard:rundat
  ;; new system
  runs-index    ;; target/runname => colnum
  tests-index   ;; testname/itempath => rownum
  matrix-dat    ;; vector of vectors rows/cols
  )

(define (dboard:rundat-make-init)
  (make-dboard:rundat
   runs-index: (make-hash-table)
   tests-index: (make-hash-table)
   matrix-dat: (make-sparse-array)))

(defstruct dboard:testdat
  id       ;; testid
  state    ;; test state
  status   ;; test status
  )

(define (dboard:rundat-get-col-num dat target runname force-set)
  (let* ((runs-index (dboard:rundat-runs-index dat))
	 (col-name   (conc target "/" runname))
	 (res        (hash-table-ref/default runs-index col-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-col-num (+ 1 (apply max -1 (hash-table-values runs-index)))))
	      (hash-table-set! runs-index col-name max-col-num)
	      max-col-num)))))

(define (dboard:rundat-get-row-num dat testname itempath force-set)
  (let* ((tests-index (dboard:rundat-runs-index dat))
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
(define (dboard:rundat-set-test-cell dat target runname testname itempath test-id state status #!key (force-set #f))
  (let* ((col-num  (dboard:rundat-get-col-num dat target runname force-set))
	 (row-num  (dboard:rundat-get-row-num dat testname itempath force-set)))
    (if (and row-num col-num)
	(let ((tdat (dboard:testdat 
		     id: test-id
		     state: state
		     status: status)))
	  (sparse-array-set! (dboard:rundat-matrix-dat dat) col-num row-num tdat)
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
	 (prev-dat    (let ((rec (hash-table-ref/default (dboard:tabdat-allruns-by-id tabdat) run-id #f)))
			(if rec rec (vector run '() key-vals -100)))) ;; -100 is before time began
	 (prev-tests  (vector-ref prev-dat 1))
	 (last-update (vector-ref prev-dat 3))
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
	 (tests      (dashboard:merge-changed-tests prev-tests tmptests  (dboard:tabdat-hide-not-hide tabdat) prev-tests)))
    (vector-set! prev-dat 3 (- (current-seconds) 2)) ;; go back two seconds in time to ensure all changes are captured.
    ;; (debug:print 0 *default-log-port* "(dboard:get-tests-for-run-duplicate: filters-changed=" (dboard:tabdat-filters-changed tabdat) " last-update=" last-update " got " (length tmptests) " test records for run " run-id)
    tests))

;; tmptests   - new tests data
;; prev-tests - old tests data
;;
(define (dashboard:merge-changed-tests tests tmptests use-new prev-tests) 
  (let ((newdat (filter
		 (lambda (x)
		   (not (equal? (db:test-get-state x) "DELETED"))) ;; remove deleted tests but do it after merging
		 (delete-duplicates (if use-new ;; (dboard:tabdat-filters-changed tabdat)
					tmptests
					(append tmptests prev-tests))
				    (lambda (a b)
				      (eq? (db:test-get-id a)(db:test-get-id b)))))))
    (if (eq? *tests-sort-reverse* 3) ;; +event_time
	(sort newdat dboard:compare-tests)
	newdat)))

;; create a virtual table of all the tests
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
(define (update-rundat tabdat runnamepatt numruns testnamepatt keypatts)
  (let* ((referenced-run-ids '())
	 (allruns     (rmt:get-runs runnamepatt numruns (dboard:tabdat-start-run-offset tabdat) keypatts))
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns))
	 (result      '())
	 (maxtests    0))
    ;; 
    ;; trim runs to only those that are changing often here
    ;; 
    (for-each (lambda (run)
		(let* ((run-id      (db:get-value-by-header run header "id"))
		       (key-vals    (rmt:get-key-vals run-id))
		       (tests       (dboard:get-tests-for-run-duplicate tabdat run-id run testnamepatt key-vals)))
		  ;; NOTE: bubble-up also sets the global (dboard:tabdat-item-test-names tabdat)
		  ;; (tests       (bubble-up tmptests priority: bubble-type))
		  ;; NOTE: 11/01/2013 This routine is *NOT* getting called excessively.
		  ;; (debug:print 0 *default-log-port* "Getting data for run " run-id " with key-vals=" key-vals)
		  ;; Not sure this is needed?
		  (if (not (null? tests))
		      (begin
			(set! referenced-run-ids (cons run-id referenced-run-ids))
			(if (> (length tests) maxtests)
			    (set! maxtests (length tests)))
			(if (or (not (dboard:tabdat-hide-empty-runs tabdat)) ;; this reduces the data burden when set
				(not (null? tests)))
			    (let ((dstruct (vector run tests key-vals (- (current-seconds) 10))))
			      (hash-table-set! (dboard:tabdat-allruns-by-id tabdat) run-id dstruct)
			      (set! result (cons dstruct result))))))))
	      runs)

    (dboard:tabdat-header-set! tabdat header)
    (dboard:tabdat-allruns-set! tabdat result)
    (debug:print-info 6 *default-log-port* "(dboard:tabdat-allruns tabdat) has " (length (dboard:tabdat-allruns tabdat)) " runs")
    maxtests))

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
       (if (vector? rundat)
	   (let* ((testdat   (vector-ref rundat 1))
		  (testnames (map test:test-get-fullname testdat)))
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
       (if (not rundat) ;; handle padded runs
	   ;;           ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration
	   (set! rundat (vector (make-vector 20 #f) '() (map (lambda (x) "") (dboard:tabdat-keys tabdat)))));; 3)))
       (let* ((run      (vector-ref rundat 0))
	      (testsdat (vector-ref rundat 1))
	      (key-val-dat (vector-ref rundat 2))
	      (run-id   (db:get-value-by-header run (dboard:tabdat-header tabdat) "id"))
	      (key-vals (append key-val-dat
				(list (let ((x (db:get-value-by-header run (dboard:tabdat-header tabdat) "runname")))
					(if x x "")))))
	      (run-key  (string-intersperse key-vals "\n")))
	 
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
		(if buttondat
		    (let* ((test       (let ((matching (filter 
							(lambda (x)(equal? (test:test-get-fullname x) testname))
							testsdat)))
					 (if (null? matching)
					     (vector -1 -1 "" "" "" 0 "" "" 0 "" "" "" 0 "" "")
					     (car matching))))
			   (testname   (db:test-get-testname  test))
			   (itempath   (db:test-get-item-path test))
			   (testfullname (test:test-get-fullname test))
			   (teststatus (db:test-get-status   test))
			   (teststate  (db:test-get-state    test))
			   ;;(teststart  (db:test-get-event_time test))
			   ;;(runtime    (db:test-get-run_duration test))
			   (buttontxt  (cond
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
		      (vector-set! buttondat 3 test)
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

(define (dashboard:update-target-selector key-lbs #!key (action-proc #f))
  (let* ((runconf-targs (common:get-runconfig-targets))
	 (db-target-dat (rmt:get-targets))
	 (header        (vector-ref db-target-dat 0))
	 (db-targets    (vector-ref db-target-dat 1))
	 (all-targets   (append db-targets
				(map (lambda (x)
				       (list->vector
					(take (append (string-split x "/")
						      (make-list (length header) "na"))
					      (length header))))
				     runconf-targs)))
	 (key-listboxes (if key-lbs key-lbs (make-list (length header) #f))))
    (let loop ((key     (car header))
	       (remkeys (cdr header))
	       (refvals '())
	       (indx    0)
	       (lbs     '()))
      (let* ((lb (let ((lb (list-ref key-listboxes indx)))
		   (if lb
		       lb
		       (iup:listbox 
			#:size "45x50" 
			#:fontsize "10"
			#:expand "YES" ;; "VERTICAL"
			;; #:dropdown "YES"
			#:editbox "YES"
			#:action (lambda (obj a b c)
				   (action-proc))
			#:caret_cb (lambda (obj a b c)(action-proc))
			))))
	     ;; loop though all the targets and build the list for this dropdown
	     (selected-value (dashboard:populate-target-dropdown lb refvals all-targets)))
	(if (null? remkeys)
	    ;; return a list of the listbox items and an iup:hbox with the labels and listboxes
	    (let ((listboxes (append lbs (list lb))))
	      (list listboxes
		    (map (lambda (htxt lb)
			   (iup:vbox
			    (iup:label htxt) 
			    lb))
			 header
			 listboxes)))
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
		   #:expand "YES"
		   #:action (lambda (obj tstate)
			      (if (eq? tstate 0)
				  (hash-table-delete! alltgls item)
				  (hash-table-set! alltgls item #t))
			      (let ((all (hash-table-keys alltgls)))
				(proc all)))))
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

(define (dashboard:run-controls commondat tabdat #!key (tab-num #f))
  (let* ((targets       (make-hash-table))
	 (test-records  (make-hash-table))
	 (all-tests-registry (tests:get-all)) ;; (tests:get-valid-tests *toppath* '()))
	 (test-names    (hash-table-keys all-tests-registry))
	 (sorted-testnames #f)
	 (action        "-run")
	 (cmdln         "")
	 (runlogs       (make-hash-table))
	 (key-listboxes #f)
	 (update-keyvals (lambda ()
			   (let ((targ (map (lambda (x)
					      (iup:attribute x "VALUE"))
					    (car (dashboard:update-target-selector key-listboxes))))
				 (curr-runname (dboard:tabdat-run-name tabdat)))
			     (dboard:tabdat-target-set! tabdat targ)
			;; (if (dboard:tabdat-updater-for-runs tabdat)
			;; 	 ((dboard:tabdat-updater-for-runs tabdat)))
			     (if (or (not (equal? curr-runname (dboard:tabdat-run-name tabdat)))
				     (equal? (dboard:tabdat-run-name tabdat) ""))
				 (dboard:tabdat-run-name-set! tabdat curr-runname))
			     (dashboard:update-run-command tabdat))))
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
	(dcommon:command-testname-selector commondat tabdat update-keyvals key-listboxes))
       
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
  ;; (dashboard:run-times-tab-updater commondat tab-num)
  (let ((drawing               (vg:drawing-new))
	(run-times-tab-updater (lambda ()
				 (dashboard:run-times-tab-updater commondat tab-num))))
    (dboard:tabdat-drawing-set! tabdat drawing)
    (dboard:commondat-add-updater commondat run-times-tab-updater tab-num: tab-num)
    (iup:vbox
     (let* ((cnv-obj (iup:canvas 
		     ;; #:size "500x400"
		     #:expand "YES"
		     #:scrollbar "YES"
		     #:posx "0.5"
		     #:posy "0.5"
		     #:action (make-canvas-action
			       (lambda (c xadj yadj)
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
					 )))))
		     #:wheel-cb (lambda (obj step x y dir) ;; dir is 4 for up and 5 for down. I think.
				  (let* ((drawing (dboard:tabdat-drawing tabdat))
					 (scalex  (vg:drawing-scalex drawing)))
				    (dboard:tabdat-view-changed-set! tabdat #t)
				    (print "step: " step " x: " x " y: " y " dir: " dir " scalex: " scalex)
				    (vg:drawing-scalex-set! drawing
							    (+ scalex
							       (if (> step 0)
								   (* scalex  0.02)
								   (* scalex -0.02))))))
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
				       (mark-for-update tabdat)
				       (update-search commondat tabdat "test-name" val)))
	       (iup:hbox
		(iup:button "Quit"      #:action (lambda (obj)
						   ;; (if (dboard:tabdat-dblocal tabdat) (db:close-all (dboard:tabdat-dblocal tabdat)))
						   (exit)))
		(iup:button "Refresh"   #:action (lambda (obj)
						   (mark-for-update tabdat)))
		(iup:button "Collapse"  #:action (lambda (obj)
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
						   (mark-for-update tabdat))))
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
	(common:run-a-command
	 (conc "megatest -run -target " target
	       " -runname " runname
	       " -testpatt " testpatt
	       " -preclean -clean-cache")
	 )))))
   (iup:menu-item
    "Test"
    (iup:menu 
     (iup:menu-item
      (conc "Rerun " test-name)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -run -target " target
	       " -runname " runname
	       " -testpatt " test-name
	       " -preclean -clean-cache"))))
     (iup:menu-item
      "Start xterm"
      #:action
      (lambda (obj)
	(let* ((cmd (conc (car (argv)) " -xterm " run-id "," test-id "&")))
	  (system cmd))))
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
					(dboard:commondat-please-update-set! commondat #t)
					(dboard:commondat-curr-tab-num-set! commondat curr))
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
	 controls))))
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
;; bars?
;;
(define (dashboard:row-collision rowhash rownum x1 x2)
  (let ((rowdat    (hash-table-ref/default rowhash rownum '()))
	(collision #f))
    (for-each
     (lambda (bar)
       (let ((bx1 (car bar))
	     (bx2 (cdr bar)))
	 (cond
	  ;; newbar x1 inside bar
	  ((dashboard:px-between x1 bx1 bx2)(set! collision #t))
	  ((dashboard:px-between x2 bx1 bx2)(set! collision #t))
	  ((and (<= x1 bx1)(>= x2 bx2))(set! collision #t)))))
     rowdat)
    collision))

(define-inline (dashboard:add-bar rowhash rownum x1 x2)
  (hash-table-set! rowhash rownum (cons (cons x1 x2) 
					(hash-table-ref/default rowhash rownum '()))))

(define (dashboard:run-times-tab-updater commondat tab-num)
  ;; each test is an object in the run component
  ;; each run is a component
  ;; all runs stored in runslib library
  (let ((tabdat        (dboard:common-get-tabdat commondat tab-num: tab-num))
	(canvas-margin 10)
	(start-row     0) ;; each run starts in this row
	(row-height    10))
    (if (and tabdat
	     (dboard:tabdat-view-changed tabdat))
	(let* ((drawing    (dboard:tabdat-drawing tabdat))
	       (runslib    (vg:get/create-lib drawing "runslib"))) ;; creates and adds lib
	  (vg:drawing-xoff-set! drawing (dboard:tabdat-xadj tabdat))
	  (vg:drawing-yoff-set! drawing (dboard:tabdat-yadj tabdat))
	  (update-rundat tabdat
			 "%" ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "runname" "%") 
			 100  ;; (dboard:tabdat-numruns tabdat)
			 "%" ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
			 ;; (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "item-name" "%")
			 (let ((res '()))
			   (for-each (lambda (key)
				       (if (not (equal? key "runname"))
					   (let ((val (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) key #f)))
					     (if val (set! res (cons (list key val) res))))))
				     (dboard:tabdat-dbkeys tabdat))
			   res))
	  (let ((allruns (dboard:tabdat-allruns tabdat))
		(rowhash (make-hash-table)) ;; store me in tabdat
		(cnv     (dboard:tabdat-cnv tabdat)))
	    (let-values (((sizex sizey sizexmm sizeymm) (canvas-size cnv))
			 ((originx originy)             (canvas-origin cnv)))
	      ;; (print "allruns: " allruns)
	      (for-each
	       (lambda (rundat)
		 (if (vector? rundat)
		     (let* ((run      (vector-ref rundat 0))
			    (testsdat  (sort (vector-ref rundat 1)
					     (lambda (a b)
					       (< (db:test-get-event_time a)
						  (db:test-get-event_time b)))))
			    (key-val-dat (vector-ref rundat 2))
			    (run-id   (db:get-value-by-header run (dboard:tabdat-header tabdat) "id"))
			    (key-vals (append key-val-dat
					      (list (let ((x (db:get-value-by-header run (dboard:tabdat-header tabdat) "runname")))
						      (if x x "")))))
			    (run-key  (string-intersperse key-vals "\n"))
			    (run-full-name (string-intersperse key-vals "/"))
			    (runcomp  (vg:comp-new));; new component for this run
			    (rows-used (make-hash-table)) ;; keep track of what parts of the rows are used here row1 = (obj1 obj2 ...)
			    ;; (row-height 4)
			    (run-start  (apply min (map db:test-get-event_time testsdat)))
			    (run-end    (apply max (map (lambda (t)(+ (db:test-get-event_time t)(db:test-get-run_duration t))) testsdat)))
			    (timeoffset (- (+ originx canvas-margin) run-start))
			    (run-duration (- run-end run-start))
			    (timescale  (/ (- sizex (* 2 canvas-margin))
					   (if (> run-duration 0)
					       run-duration
					       (current-seconds)))) ;; a least lously guess
			    (maptime    (lambda (tsecs)(* timescale (+ tsecs timeoffset)))))
		       ;; (print "timescale: " timescale " timeoffset: " timeoffset " sizex: " sizex " originx: " originx)
		       (vg:add-comp-to-lib runslib run-full-name runcomp)
		       (set! start-row (+ start-row 1))
		       (let ((x 10)
			     (y (- sizey (* start-row row-height))))
			 (vg:add-objs-to-comp runcomp (vg:make-text x y run-full-name font: "Helvetica -10"))
			 (dashboard:add-bar rowhash start-row x (+ x 100)))
		       (set! start-row (+ start-row 1))
		       ;; get tests in list sorted by event time ascending
		       (for-each 
			(lambda (testdat)
			  (let* ((event-time   (maptime (db:test-get-event_time   testdat)))
				 (run-duration (* timescale (db:test-get-run_duration testdat)))
				 (end-time     (+ event-time run-duration))
				 (test-name    (db:test-get-testname     testdat))
				 (item-path    (db:test-get-item-path    testdat))
				 (state         (db:test-get-state       testdat))
				 (status        (db:test-get-status      testdat))
				 (test-fullname (conc test-name "/" item-path))
				 (name-color    (gutils:get-color-for-state-status state status)))
			    ;; (print "event_time: " (db:test-get-event_time   testdat) " mapped event_time: " event-time)
			    ;; (print "run-duration: "  (db:test-get-run_duration testdat) " mapped run_duration: " run-duration)
			    (let loop ((rownum start-row)) ;; (+ start-row 1)))
			      (set! start-row (max rownum start-row)) ;; track the max row used
			      (if (dashboard:row-collision rowhash rownum event-time end-time)
				  (loop (+ rownum 1))
				  (let* ((lly (- sizey (* rownum row-height)))
					 (uly (+ lly row-height)))
				    (dashboard:add-bar rowhash rownum event-time end-time)
				    (vg:add-objs-to-comp runcomp
							 (vg:make-rect event-time lly end-time uly
								       fill-color: (vg:iup-color->number (car name-color))
								       text: (conc test-name "/" item-path)
								       font: "Helvetica -10")
							 ;; (vg:make-text (+ event-time 2)
							 ;;               (+ lly 2)
							 ;;               (conc test-name "/" item-path)
							 ;;               font: "Helvetica -10")
							 ))))
			    ;; (print "test-name: " test-name " event-time: " event-time " run-duration: " run-duration)
			    ))
			testsdat)
		       ;; instantiate the component 
		       (let* ((extents (vg:components-get-extents drawing runcomp))
			      (llx     (list-ref extents 0))
			      (lly     (list-ref extents 1))
			      (ulx     (list-ref extents 2))
			      (uly     (list-ref extents 3))
			      ;; move the following into mapping functions in vg.scm
			      (deltax  (- llx ulx))
			      (scalex  (if (> deltax 0)(/ sizex deltax) 1))
			      (sllx    (* scalex llx))
			      (offx    (- sllx originx)))
			 (print "llx: " llx " lly: " lly "ulx: " ulx " uly: " uly " deltax: " deltax " scalex: " scalex " sllx: " sllx " offx: " offx)
			 (print " run-full-name: " run-full-name)
			 ;; (vg:instantiate drawing "runslib" run-full-name "wrongname" offx 0))))) 
			 (vg:instantiate drawing "runslib" run-full-name run-full-name 0 0))))) 
			;;		 scalex: scalex scaley: 1)))))
	       allruns)
	      (vg:drawing-cnv-set! (dboard:tabdat-drawing tabdat)(dboard:tabdat-cnv tabdat)) ;; cnv-obj)
	      (canvas-clear! (dboard:tabdat-cnv tabdat)) ;; -obj)
	      (print "Number of objs: " (length (vg:draw (dboard:tabdat-drawing tabdat) #t)))
	      (dboard:tabdat-view-changed-set! tabdat #f)
	      )))
	(print "no tabdat for run-times-tab-updater"))))

(define (dashboard:runs-tab-updater commondat tab-num)
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
  (common:exit-on-version-changed)
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
