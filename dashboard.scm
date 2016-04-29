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

;; (declare (uses dashboard-main))
(declare (uses megatest-version))
(declare (uses mt))

(include "common_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "megatest-fossil-hash.scm")

(define help (conc 
"Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2012-2016

Usage: dashboard [options]
  -h                   : this help
  -server host:port    : connect to host:port instead of db access
  -test run-id,test-id : control test identified by testid
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

;; create a stuct for all the miscellaneous state
;;
(defstruct d:alldat 
  allruns 
  allruns-by-id
  buttondat 
  curr-tab-num
  dbdir
  dbfpath
  dbkeys 
  dblocal
  header      
  hide-empty-runs
  hide-not-hide  ;; toggle for hide/not hide
  hide-not-hide-button
  hide-not-hide-tabs
  item-test-names
  keys
  last-db-update 
  num-tests
  numruns
  please-update  
  ro
  searchpatts
  start-run-offset
  start-test-offset
  state-ignore-hash
  status-ignore-hash
  tot-runs   
  update-mutex
  updaters
  updating
  useserver  
 )

(define *alldat* (make-d:alldat
		  header: #f 
		  allruns: '()
		  allruns-by-id: (make-hash-table)
		  buttondat: (make-hash-table)
		  searchpatts: (make-hash-table)
		  numruns: 16
		  last-db-update: 0
		  please-update: #t
		  updating: #f
		  update-mutex: (make-mutex)
		  item-test-names: '()
		  num-tests: 15
		  start-run-offset: 0
		  start-test-offset: 0
		  status-ignore-hash: (make-hash-table)
		  state-ignore-hash: (make-hash-table)
		  hide-empty-runs: #f
		  hide-not-hide: #t
		  hide-not-hide-button: #f
		  hide-not-hide-tabs: #f
		  curr-tab-num: 0
		  updaters: (make-hash-table)
		  ))

;; simple two dimentional sparse array
;;
(define (make-sparse-array)
  (let ((a (make-sparse-vector)))
    (sparse-vector-set! a 0 (make-sparse-vector))
    a))

(define (sparse-array? a)
  (and (sparse-vector? a)
       (sparse-vector? (sparse-vector-ref a 0))))

(define (sparse-array-ref a x y)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-ref row y)
	#f)))

(define (sparse-array-set! a x y val)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-set! row y val)
	(let ((new-row (make-sparse-vector)))
	  (sparse-vector-set! a x new-row)
	  (sparse-vector-set! new-row y val)))))

;; data for runs, tests etc
;;
(defstruct d:rundat
  ;; new system
  runs-index    ;; target/runname => colnum
  tests-index   ;; testname/itempath => rownum
  matrix-dat    ;; vector of vectors rows/cols
  )

(define (d:rundat-make-init)
  (make-d:rundat
   runs-index: (make-hash-table)
   tests-index: (make-hash-table)
   matrix-dat: (make-sparse-array)))

(defstruct d:testdat
  id       ;; testid
  state    ;; test state
  status   ;; test status
  )

(define (d:rundat-get-col-num dat target runname force-set)
  (let* ((runs-index (d:rundat-runs-index dat))
	 (col-name   (conc target "/" runname))
	 (res        (hash-table-ref/default runs-index col-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-col-num (+ 1 (apply max -1 (hash-table-values runs-index)))))
	      (hash-table-set! runs-index col-name max-col-num)
	      max-col-num)))))

(define (d:rundat-get-row-num dat testname itempath force-set)
  (let* ((tests-index (d:rundat-runs-index dat))
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
(define (d:rundat-set-test-cell dat target runname testname itempath test-id state status #!key (force-set #f))
  (let* ((col-num  (d:rundat-get-col-num dat target runname force-set))
	 (row-num  (d:rundat-get-row-num dat testname itempath force-set)))
    (if (and row-num col-num)
	(let ((tdat (d:testdat 
		     id: test-id
		     state: state
		     status: status)))
	  (sparse-array-set! (d:rundat-matrix-dat dat) col-num row-num tdat)
	  tdat)
	#f)))





(d:alldat-useserver-set! *alldat* (cond
				   ((args:get-arg "-use-local") #f)
				   ((configf:lookup *configdat* "dashboard" "use-server")
				    (let ((ans (config:lookup *configdat* "dashboard" "use-server")))
				      (if (equal? ans "yes") #t #f)))
				   (else #t)))

(d:alldat-dbdir-set! *alldat* (db:dbfile-path #f)) ;; (conc (configf:lookup *configdat* "setup" "linktree") "/.db"))
(d:alldat-dblocal-set! *alldat* (make-dbr:dbstruct path:  (d:alldat-dbdir *alldat*)
						   local: #t))
(d:alldat-dbfpath-set! *alldat* (db:dbfile-path 0))

;; HACK ALERT: this is a hack, please fix.
(d:alldat-ro-set! *alldat* (not (file-read-access? (d:alldat-dbfpath *alldat*))))

(d:alldat-keys-set! *alldat* (if (d:alldat-useserver *alldat*)
				 (rmt:get-keys)
				 (db:get-keys (d:alldat-dblocal *alldat*))))
(d:alldat-dbkeys-set! *alldat* (append (d:alldat-keys *alldat*) (list "runname")))
(d:alldat-tot-runs-set! *alldat* (if (d:alldat-useserver *alldat*)
				     (rmt:get-num-runs "%")
				     (db:get-num-runs (d:alldat-dblocal *alldat*) "%")))
;;
(define *exit-started* #f)
;; *updaters* (make-hash-table))

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

(define uidat #f)

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

(define (compare-tests test1 test2)
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
    
;; create a virtual table of all the tests
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
(define (update-rundat data runnamepatt numruns testnamepatt keypatts)
  (let* ((referenced-run-ids '())
	 (allruns     (if (d:alldat-useserver data)
			  (rmt:get-runs runnamepatt numruns (d:alldat-start-run-offset data) keypatts)
			  (db:get-runs (d:alldat-dblocal data) runnamepatt numruns ;; (+ numruns 1) ;; (/ numruns 2))
				      (d:alldat-start-run-offset data) keypatts)))
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns))
	 (result      '())
	 (maxtests    0)
	 (states      (hash-table-keys (d:alldat-state-ignore-hash data)))
	 (statuses    (hash-table-keys (d:alldat-status-ignore-hash data)))
	 (sort-info   (get-curr-sort))
	 (sort-by     (vector-ref sort-info 1))
	 (sort-order  (vector-ref sort-info 2))
	 (bubble-type (if (member sort-order '(testname))
			  'testname
			  'itempath)))
    ;; 
    ;; trim runs to only those that are changing often here
    ;; 
    (for-each (lambda (run)
		(let* ((run-id      (db:get-value-by-header run header "id"))
		       (key-vals    (if (d:alldat-useserver data) 
					(rmt:get-key-vals run-id)
					(db:get-key-vals (d:alldat-dblocal data) run-id)))
		       (prev-dat    (let ((rec (hash-table-ref/default (d:alldat-allruns-by-id data) run-id #f)))
				      (if rec rec (vector run '() key-vals -100)))) ;; -100 is before time began
		       (prev-tests  (vector-ref prev-dat 1))
		       (last-update (vector-ref prev-dat 3))
		       (tmptests    (if (d:alldat-useserver data)
					(rmt:get-tests-for-run run-id testnamepatt states statuses
							       #f #f
							       (d:alldat-hide-not-hide data)
							       sort-by
							       sort-order
							       'shortlist
							       last-update)
					(db:get-tests-for-run (d:alldat-dblocal data) run-id testnamepatt states statuses
							      #f #f
							      (d:alldat-hide-not-hide data)
							      sort-by
							      sort-order
							      'shortlist
							      last-update)))
		       (tests       (let ((newdat (filter
						   (lambda (x)
						     (not (equal? (db:test-get-state x) "DELETED"))) ;; remove deleted tests but do it after merging
						   (delete-duplicates (append tmptests prev-tests)
								      (lambda (a b)
									(eq? (db:test-get-id a)(db:test-get-id b)))))))
				      (if (eq? *tests-sort-reverse* 3) ;; +event_time
					(sort newdat compare-tests)
					newdat))))
		  ;; NOTE: bubble-up also sets the global (d:alldat-item-test-names data)
		  ;; (tests       (bubble-up tmptests priority: bubble-type))
		  ;; NOTE: 11/01/2013 This routine is *NOT* getting called excessively.
		  ;; (debug:print 0 "Getting data for run " run-id " with key-vals=" key-vals)
		  ;; Not sure this is needed?
		  (set! referenced-run-ids (cons run-id referenced-run-ids))
		  (if (> (length tests) maxtests)
		      (set! maxtests (length tests)))
		  (if (or (not (d:alldat-hide-empty-runs data)) ;; this reduces the data burden when set
			  (not (null? tests)))
		      (let ((dstruct (vector run tests key-vals (- (current-seconds) 10))))
			(hash-table-set! (d:alldat-allruns-by-id data) run-id dstruct)
			(set! result (cons dstruct result))))))
	      runs)

    (d:alldat-header-set! data header)
    (d:alldat-allruns-set! data result)
    (debug:print-info 6 "(d:alldat-allruns data) has " (length (d:alldat-allruns data)) " runs")
    maxtests))

(define *collapsed* (make-hash-table))
; (define *row-lookup* (make-hash-table)) ;; testname => (rownum lableobj)

(define (toggle-hide lnum) ; fulltestname)
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

(define (collapse-rows inlst)
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
	 (vlst2        (bubble-up vlst priority: bubble-type)))
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
(define (bubble-up test-dats #!key (priority 'itempath))
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
	  (d:alldat-item-test-names-set! *alldat* (append (if (null? tnames)
							      '()
							      (filter (lambda (tname)
									(let ((tlst (hash-table-ref tests tname)))
									  (and (list tlst)
									       (> (length tlst) 1))))
								      tnames))
							  (d:alldat-item-test-names *alldat*)))
	  (let loop ((hed (car tnames))
		     (tal (cdr tnames))
		     (res '()))
	    (let ((newres (append res (hash-table-ref tests hed))))
	      (if (null? tal)
		  newres
		  (loop (car tal)(cdr tal) newres))))))))
      
(define (update-buttons uidat numruns numtests)
  (let* ((runs        (if (> (length (d:alldat-allruns *alldat*)) numruns)
			  (take-right (d:alldat-allruns *alldat*) numruns)
			  (pad-list (d:alldat-allruns *alldat*) numruns)))
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
	     (if (not (and (d:alldat-hide-empty-runs *alldat*)
			   (null? testnames)))
		 (for-each (lambda (testname)
			     (if (not (member testname *alltestnamelst*))
				 (begin
				   (set! *alltestnamelst* (append *alltestnamelst* (list testname))))))
			   testnames)))))
     runs)

    (set! *alltestnamelst* (collapse-rows *alltestnamelst*)) ;;; argh. please clean up this sillyness
    (set! *alltestnamelst* (let ((xl (if (> (length *alltestnamelst*) (d:alldat-start-test-offset *alldat*))
					 (drop *alltestnamelst* (d:alldat-start-test-offset *alldat*))
					 '())))
			     (append xl (make-list (- (d:alldat-num-tests *alldat*) (length xl)) ""))))
    (update-labels uidat)
    (for-each
     (lambda (rundat)
       (if (not rundat) ;; handle padded runs
	   ;;           ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration
	   (set! rundat (vector (make-vector 20 #f) '() (map (lambda (x) "") (d:alldat-keys *alldat*)))));; 3)))
       (let* ((run      (vector-ref rundat 0))
	      (testsdat (vector-ref rundat 1))
	      (key-val-dat (vector-ref rundat 2))
	      (run-id   (db:get-value-by-header run (d:alldat-header *alldat*) "id"))
	      (key-vals (append key-val-dat
				(list (let ((x (db:get-value-by-header run (d:alldat-header *alldat*) "runname")))
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
	      (let ((buttondat  (hash-table-ref/default (d:alldat-buttondat *alldat*) (mkstr coln rown) #f)))
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

(define (set-bg-on-filter)
  (let ((search-changed (not (null? (filter (lambda (key)
					      (not (equal? (hash-table-ref (d:alldat-searchpatts *alldat*) key) "%")))
					    (hash-table-keys (d:alldat-searchpatts *alldat*))))))
	(state-changed  (not (null? (hash-table-keys (d:alldat-state-ignore-hash *alldat*)))))
	(status-changed (not (null? (hash-table-keys (d:alldat-status-ignore-hash *alldat*))))))
    (iup:attribute-set! (d:alldat-hide-not-hide-tabs *alldat*) "BGCOLOR"
			(if (or search-changed
				state-changed
				status-changed)
			    "190 180 190"
			    "190 190 190"
			    ))))

(define (update-search x val)
  (hash-table-set! (d:alldat-searchpatts *alldat*) x val)
  (set-bg-on-filter))

(define (mark-for-update)
  (d:alldat-last-db-update-set! *alldat* 0))

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
	 (db-target-dat (if (d:alldat-useserver *alldat*) 
			    (rmt:get-targets)
			    (db:get-targets (d:alldat-dblocal *alldat*))))
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

;; Extract the various bits of data from *data* and create the command line equivalent that will be displayed
;;
(define (dashboard:update-run-command)
  (let* ((cmd-tb       (dboard:data-get-command-tb *data*))
	 (cmd          (dboard:data-get-command    *data*))
	 (test-patt    (let ((tp (dboard:data-get-test-patts *data*)))
			 (if (equal? tp "") "%" tp)))
	 (states       (dboard:data-get-states     *data*))
	 (statuses     (dboard:data-get-statuses   *data*))
	 (target       (let ((targ-list (dboard:data-get-target     *data*)))
			 (if targ-list (string-intersperse targ-list "/") "no-target-selected")))
	 (run-name     (dboard:data-get-run-name   *data*))
	 (states-str   (if (or (not states)
			       (null? states))
			   ""
			   (conc " :state "  (string-intersperse states ","))))
	 (statuses-str (if (or (not statuses)
			       (null? statuses))
			   ""
			   (conc " :status " (string-intersperse statuses ","))))
	 (full-cmd  "megatest"))
    (case (string->symbol cmd)
      ((runtests)
       (set! full-cmd (conc full-cmd 
			    " -runtests "
			    test-patt
			    " -target "
			    target
			    " -runname "
			    run-name
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
(define (dashboard:run-controls)
  (let* ((targets       (make-hash-table))
	 (test-records  (make-hash-table))
	 (all-tests-registry (tests:get-all)) ;; (tests:get-valid-tests *toppath* '()))
	 (test-names    (hash-table-keys all-tests-registry))
	 (sorted-testnames #f)
	 (action        "-runtests")
	 (cmdln         "")
	 (runlogs       (make-hash-table))
	 (key-listboxes #f)
	 (updater-for-runs #f)
	 (update-keyvals (lambda ()
			   (let ((targ (map (lambda (x)
					      (iup:attribute x "VALUE"))
					    (car (dashboard:update-target-selector key-listboxes)))))
			     (dboard:data-set-target! *data* targ)
			     (if updater-for-runs (updater-for-runs))
			     (dashboard:update-run-command))))
	 (tests-draw-state (make-hash-table)) ;; use for keeping state of the test canvas
	 (test-patterns-textbox  #f))
    (hash-table-set! tests-draw-state 'first-time #t)
    ;; (hash-table-set! tests-draw-state 'scalef 1)
    (tests:get-full-data test-names test-records '() all-tests-registry)
    (set! sorted-testnames (tests:sort-by-priority-and-waiton test-records))
    
    ;; refer to (d:alldat-keys *alldat*), (d:alldat-dbkeys *alldat*) for keys
    (iup:vbox
     ;; The command line display/exectution control
     (iup:frame
      #:title "Command to be exectuted"
      (iup:hbox
       (iup:label "Run on" #:size "40x")
       (iup:radio 
	(iup:hbox
	 (iup:toggle "Local" #:size "40x")
	 (iup:toggle "Server" #:size "40x")))
       (let ((tb (iup:textbox 
		  #:value "megatest "
		  #:expand "HORIZONTAL"
		  #:readonly "YES"
		  #:font "Courier New, -12"
		  )))
	 (dboard:data-set-command-tb! *data* tb)
	 tb)
       (iup:button "Execute" #:size "50x"
		   #:action (lambda (obj)
			      (let ((cmd (conc "xterm -geometry 180x20 -e \""
					       (iup:attribute (dboard:data-get-command-tb *data*) "VALUE")
					       ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))
				(system cmd))))))

     (iup:split
      #:orientation "HORIZONTAL"
      
      (iup:split
       #:value 300

       ;; Target, testpatt, state and status input boxes
       ;;
       (iup:vbox
	;; Command to run
	(iup:frame
	 #:title "Set the action to take"
	 (iup:hbox
	  ;; (iup:label "Command to run" #:expand "HORIZONTAL" #:size "70x" #:alignment "LEFT:ACENTER")
	  (let* ((cmds-list '("runtests" "remove-runs" "set-state-status" "lock-runs" "unlock-runs"))
		 (lb         (iup:listbox #:expand "HORIZONTAL"
					  #:dropdown "YES"
					  #:action (lambda (obj val index lbstate)
						     ;; (print obj " " val " " index " " lbstate)
						     (dboard:data-set-command! *data* val)
						     (dashboard:update-run-command))))
		 (default-cmd (car cmds-list)))
	    (iuplistbox-fill-list lb cmds-list selected-item: default-cmd)
	    (dboard:data-set-command! *data* default-cmd)
	    lb)))

	(iup:frame
	 #:title "Runname"
	 (let* ((default-run-name (seconds->work-week/day (current-seconds)))
		(tb (iup:textbox #:expand "HORIZONTAL"
				 #:action (lambda (obj val txt)
					    ;; (print "obj: " obj " val: " val " unk: " unk)
					    (dboard:data-set-run-name! *data* txt) ;; (iup:attribute obj "VALUE"))
					    (dashboard:update-run-command))
				 #:value default-run-name))
		(lb (iup:listbox #:expand "HORIZONTAL"
				 #:dropdown "YES"
				 #:action (lambda (obj val index lbstate)
					    (iup:attribute-set! tb "VALUE" val)
					    (dboard:data-set-run-name! *data* val)
					    (dashboard:update-run-command))))
		(refresh-runs-list (lambda ()
				     (let* ((target        (dboard:data-get-target-string *data*))
					    (runs-for-targ (if (d:alldat-useserver *alldat*)
							       (rmt:get-runs-by-patt (d:alldat-keys *alldat*) "%" target #f #f #f)
							       (db:get-runs-by-patt (d:alldat-dblocal *alldat*) (d:alldat-keys *alldat*) "%" target #f #f #f)))
					    (runs-header   (vector-ref runs-for-targ 0))
					    (runs-dat      (vector-ref runs-for-targ 1))
					    (run-names     (cons default-run-name 
								 (map (lambda (x)
									(db:get-value-by-header x runs-header "runname"))
								      runs-dat))))
				       (iup:attribute-set! lb "REMOVEITEM" "ALL")
				       (iuplistbox-fill-list lb run-names selected-item: default-run-name)))))
	   (set! updater-for-runs refresh-runs-list)
	   (refresh-runs-list)
	   (dboard:data-set-run-name! *data* default-run-name)
	   (iup:hbox
	    tb
	    lb)))

	(iup:frame
	 #:title "SELECTORS"
	 (iup:vbox
	  ;; Text box for test patterns
	  (iup:frame
	   #:title "Test patterns (one per line)"
	   (let ((tb (iup:textbox #:action (lambda (val a b)
					     (dboard:data-set-test-patts!
					      *data*
					      (dboard:lines->test-patt b))
					     (dashboard:update-run-command))
				  #:value (dboard:test-patt->lines
					   (dboard:data-get-test-patts *data*))
				  #:expand "YES"
				  #:size "x50"
				  #:multiline "YES")))
	     (set! test-patterns-textbox tb)
	     tb))
	  (iup:frame
	   #:title "Target"
	   ;; Target selectors
	   (apply iup:hbox
		  (let* ((dat      (dashboard:update-target-selector key-listboxes action-proc: update-keyvals))
			 (key-lb   (car dat))
			 (combos   (cadr dat)))
		    (set! key-listboxes key-lb)
		    combos)))
	  (iup:hbox
	   ;; Text box for STATES
	   (iup:frame
	    #:title "States"
	    (dashboard:text-list-toggle-box 
	     ;; Move these definitions to common and find the other useages and replace!
	     (map cadr *common:std-states*) ;; '("COMPLETED" "RUNNING" "STUCK" "INCOMPLETE" "LAUNCHED" "REMOTEHOSTSTART" "KILLED")
	     (lambda (all)
	       (dboard:data-set-states! *data* all)
	       (dashboard:update-run-command))))
	   ;; Text box for STATES
	   (iup:frame
	    #:title "Statuses"
	    (dashboard:text-list-toggle-box 
	     (map cadr *common:std-statuses*) ;; '("PASS" "FAIL" "n/a" "CHECK" "WAIVED" "SKIP" "DELETED" "STUCK/DEAD")
	     (lambda (all)
	       (dboard:data-set-statuses! *data* all)
	       (dashboard:update-run-command))))))))
      
       (iup:frame
	#:title "Tests and Tasks"
	(let* ((updater #f)
	       (last-xadj 0)
	       (last-yadj 0)
	       (the-cnv   #f)
	       (canvas-obj 
                (iup:canvas #:action (make-canvas-action
				      (lambda (cnv xadj yadj)
					(if (not updater)
					    (set! updater (lambda (xadj yadj)
							    ;; (print "cnv: " cnv " xadj: " xadj " yadj: " yadj)
							    (dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames test-records)
							    (set! last-xadj xadj)
							    (set! last-yadj yadj))))
					(updater xadj yadj)
					(set! the-cnv cnv)
					))
			    ;; Following doesn't work 
			    #:wheel-cb (lambda (obj step x y dir) ;; dir is 4 for up and 5 for down. I think.
					 (let ((scalef (hash-table-ref tests-draw-state 'scalef)))
					   (hash-table-set! tests-draw-state 'scalef (+ scalef
											(if (> step 0)
											    (* scalef 0.01)
											    (* scalef -0.01))))
					   (if the-cnv
					       (dashboard:draw-tests the-cnv last-xadj last-yadj tests-draw-state sorted-testnames test-records))
					   ))
			    ;; #:size "50x50"
			    #:expand "YES"
			    #:scrollbar "YES"
			    #:posx "0.5"
			    #:posy "0.5"
			    #:button-cb (lambda (obj btn pressed x y status)
					  ;; (print "obj: " obj ", pressed " pressed ", status " status)
					  ; (print "canvas-origin: " (canvas-origin the-cnv))
					  ;; (let-values (((xx yy)(canvas-origin the-cnv)))
					    ;; (canvas-transform-set! the-cnv #f)
					    ;; (print "canvas-origin: " xx " " yy " click at " x " " y))
					  (let* ((tests-info     (hash-table-ref tests-draw-state 'tests-info))
						 (selected-tests (hash-table-ref tests-draw-state 'selected-tests))
						 (scalef         (hash-table-ref tests-draw-state 'scalef))
						 (sizey          (hash-table-ref tests-draw-state 'sizey))
						 (xoffset        (dcommon:get-xoffset tests-draw-state #f #f))
						 (yoffset        (dcommon:get-yoffset tests-draw-state #f #f))
						 (new-y          (- sizey y)))
					    ;; (print "xoffset=" xoffset ", yoffset=" yoffset)
					    ;; (print "\tx\ty\tllx\tlly\turx\tury")
					    (for-each (lambda (test-name)
							(let* ((rec-coords (hash-table-ref tests-info test-name))
							       (llx        (dcommon:x->canvas (list-ref rec-coords 0) scalef xoffset))
							       (lly        (dcommon:y->canvas (list-ref rec-coords 1) scalef yoffset))
							       (urx        (dcommon:x->canvas (list-ref rec-coords 2) scalef xoffset))
							       (ury        (dcommon:y->canvas (list-ref rec-coords 3) scalef yoffset)))
							  ;; (if (eq? pressed 1)
							  ;;    (print "\tx=" x "\ty=" y "\tnew-y=" new-y "\tllx=" llx "\tlly=" lly "\turx=" urx "\tury=" ury "\t" test-name " "))
							  (if (and (eq? pressed 1)
								   (>= x llx)
								   (>= new-y lly)
								   (<= x urx)
								   (<= new-y ury))
							      (let ((patterns (string-split (iup:attribute test-patterns-textbox "VALUE"))))
								(let* ((selected     (not (member test-name patterns)))
								       (newpatt-list (if selected
											 (cons test-name patterns)
											 (delete test-name patterns)))
								       (newpatt      (string-intersperse newpatt-list "\n")))
								  (iup:attribute-set! obj "REDRAW" "ALL")
								  (hash-table-set! selected-tests test-name selected)
								  (iup:attribute-set! test-patterns-textbox "VALUE" newpatt)
								  (dboard:data-set-test-patts! *data* (dboard:lines->test-patt newpatt))
								  (dashboard:update-run-command)
								  (if updater (updater last-xadj last-yadj)))))))
						      (hash-table-keys tests-info)))))))
	  canvas-obj)))
       
      (iup:frame
       #:title "Logs" ;; To be replaced with tabs
       (let ((logs-tb (iup:textbox #:expand "YES"
				   #:multiline "YES")))
	 (dboard:data-set-logs-textbox! *data* logs-tb)
	 logs-tb))))))


;; (trace dashboard:populate-target-dropdown
;;        common:list-is-sublist)
;; 
;;       ;; key1 key2 key3 ...
;;       ;; target entry (wild cards allowed)
;;       
;;       ;; The action
;;       (iup:hbox
;;        ;; label Action | action selector
;;        ))
;;      ;; Test/items selector
;;      (iup:hbox
;;       ;; tests
;;       ;; items
;;       ))
;;     ;; The command line
;;     (iup:hbox
;;      ;; commandline entry
;;      ;; GO button
;;      )
;;     ;; The command log monitor
;;     (iup:tabs
;;      ;; log monitor
;;      )))

;;======================================================================
;; S U M M A R Y 
;;======================================================================
;;
;; General info about the run(s) and megatest area
(define (dashboard:summary data)
  (let* ((db               (d:alldat-dblocal data))
	 (rawconfig        (read-config (conc *toppath* "/megatest.config") #f #f))) ;; changed to #f since I want #{} to be expanded by [system ...] to NOT be expanded. WAS: 'return-string)))
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
       (dcommon:servers-table)))
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
      (dcommon:run-stats db)))))

;;======================================================================
;; R U N
;;======================================================================
;;
;; display and manage a single run at a time

(define (tree-path->run-id data path)
  (if (not (null? path))
      (hash-table-ref/default (d:data-path-run-ids data) path #f)
      #f))

(define dashboard:update-run-summary-tab #f)

;; This is the Run Summary tab
;; 
(define (dashboard:one-run db data)
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id data (cdr run-path))))
		       (if (number? run-id)
			   (begin
			     (d:data-curr-run-id-set! data run-id)
			     (dashboard:update-run-summary-tab))
			   (debug:print 0 "ERROR: tree-path->run-id returned non-number " run-id)))
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
			       (cmd      (conc toolpath " -test " (d:data-curr-run-id data) "," test-id "&")))
			  (system cmd)))))
	 (updater  (lambda ()
		     (let* ((runs-dat     (if (d:alldat-useserver *alldat*)
					      (rmt:get-runs-by-patt (d:alldat-keys *alldat*) "%" #f #f #f #f)
					      (db:get-runs-by-patt db (d:alldat-keys *alldat*) "%" #f #f #f #f)))
			    (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
			    (run-id       (d:data-curr-run-id data))
			    (last-update  0) ;; fix me
			    (tests-dat    (let ((tdat (if run-id
							  (if (d:alldat-useserver *alldat*)
							      (rmt:get-tests-for-run run-id 
										     (hash-table-ref/default (d:alldat-searchpatts *alldat*) "test-name" "%/%")
										     (hash-table-keys (d:alldat-state-ignore-hash *alldat*)) ;; '()
										     (hash-table-keys (d:alldat-status-ignore-hash *alldat*)) ;; '()
										     #f #f
										     (d:alldat-hide-not-hide *alldat*)
										     #f #f
										     "id,testname,item_path,state,status"
										     last-update) ;; get 'em all
							      (db:get-tests-for-run db run-id 
										    (hash-table-ref/default (d:alldat-searchpatts *alldat*) "test-name" "%/%")
										    (hash-table-keys (d:alldat-state-ignore-hash *alldat*)) ;; '()
										    (hash-table-keys (d:alldat-status-ignore-hash *alldat*)) ;; '()
										    #f #f
										    (d:alldat-hide-not-hide *alldat*)
										    #f #f
										    "id,testname,item_path,state,status"
										    last-update))
							  '()))) ;; get 'em all
					    (sort tdat (lambda (a b)
							 (let* ((aval (vector-ref a 2))
								(bval (vector-ref b 2))
								(anum (string->number aval))
								(bnum (string->number bval)))
							   (if (and anum bnum)
							       (< anum bnum)
							       (string<= aval bval)))))))
			    (tests-mindat (dcommon:minimize-test-data tests-dat))
			    (indices      (common:sparse-list-generate-index tests-mindat)) ;;  proc: set-cell))
			    (row-indices  (cadr indices))
			    (col-indices  (car indices))
			    (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
			    (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
			    (max-visible  (max (- (d:alldat-num-tests *alldat*) 15) 3)) ;; (d:alldat-num-tests *alldat*) is proportional to the size of the window
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
							   (d:alldat-keys *alldat*)))
					  (run-name   (db:get-value-by-header run-record runs-header "runname"))
					  (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
					  (run-path   (append key-vals (list run-name)))
					  (existing   (tree:find-node tb run-path)))
				     (if (not (hash-table-ref/default (d:data-path-run-ids data) run-path #f))
					 (begin
					   (hash-table-set! (d:data-run-keys data) run-id run-path)
					   ;; (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
					   ;;    		 (conc rownum ":" colnum) col-name)
					   ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
					   ;; Here we update the tests treebox and tree keys
					   (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
							  userdata: (conc "run-id: " run-id))
					   (hash-table-set! (d:data-path-run-ids data) run-path run-id)
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
		       (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL"))))))
    
    (set! dashboard:update-run-summary-tab updater)
    (d:data-runs-tree-set! data tb)
    (iup:split
     tb
     run-matrix)))

;; This is the New View tab
;; 
(define (dashboard:new-view db data)
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:addexpanded "NO"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id data (cdr run-path))))
		       (if (number? run-id)
			   (begin
			     (d:data-curr-run-id-set! data run-id)
			     (dashboard:update-run-summary-tab))
			   (debug:print 0 "ERROR: tree-path->run-id returned non-number " run-id)))
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
			       (cmd      (conc toolpath " -test " (d:data-curr-run-id data) "," test-id "&")))
			  (system cmd)))))
	 (updater  (lambda ()
		     (let* ((runs-dat     (if (d:alldat-useserver *alldat*)
					      (rmt:get-runs-by-patt (d:alldat-keys *alldat*) "%" #f #f #f #f)
					      (db:get-runs-by-patt db (d:alldat-keys *alldat*) "%" #f #f #f #f)))
			    (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
			    (run-id       (d:data-curr-run-id data))
			    (last-update  0) ;; fix me
			    (tests-dat    (let ((tdat (if run-id
							  (if (d:alldat-useserver *alldat*)
							      (rmt:get-tests-for-run run-id 
										     (hash-table-ref/default (d:alldat-searchpatts *alldat*) "test-name" "%/%")
										     (hash-table-keys (d:alldat-state-ignore-hash *alldat*)) ;; '()
										     (hash-table-keys (d:alldat-status-ignore-hash *alldat*)) ;; '()
										     #f #f
										     (d:alldat-hide-not-hide *alldat*)
										     #f #f
										     "id,testname,item_path,state,status"
										     last-update) ;; get 'em all
							      (db:get-tests-for-run db run-id 
										    (hash-table-ref/default (d:alldat-searchpatts *alldat*) "test-name" "%/%")
										    (hash-table-keys (d:alldat-state-ignore-hash *alldat*)) ;; '()
										    (hash-table-keys (d:alldat-status-ignore-hash *alldat*)) ;; '()
										    #f #f
										    (d:alldat-hide-not-hide *alldat*)
										    #f #f
										    "id,testname,item_path,state,status"
										    last-update))
							  '()))) ;; get 'em all
					    (sort tdat (lambda (a b)
							 (let* ((aval (vector-ref a 2))
								(bval (vector-ref b 2))
								(anum (string->number aval))
								(bnum (string->number bval)))
							   (if (and anum bnum)
							       (< anum bnum)
							       (string<= aval bval)))))))
			    (tests-mindat (dcommon:minimize-test-data tests-dat))
			    (indices      (common:sparse-list-generate-index tests-mindat)) ;;  proc: set-cell))
			    (row-indices  (cadr indices))
			    (col-indices  (car indices))
			    (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
			    (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
			    (max-visible  (max (- (d:alldat-num-tests *alldat*) 15) 3)) ;; (d:alldat-num-tests *alldat*) is proportional to the size of the window
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
							   (d:alldat-keys *alldat*)))
					  (run-name   (db:get-value-by-header run-record runs-header "runname"))
					  (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
					  (run-path   (append key-vals (list run-name)))
					  (existing   (tree:find-node tb run-path)))
				     (if (not (hash-table-ref/default (d:data-path-run-ids data) run-path #f))
					 (begin
					   (hash-table-set! (d:data-run-keys data) run-id run-path)
					   ;; (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
					   ;;    		 (conc rownum ":" colnum) col-name)
					   ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
					   ;; Here we update the tests treebox and tree keys
					   (tree:add-node tb "Runs" run-path ;; (append key-vals (list run-name))
							  userdata: (conc "run-id: " run-id))
					   (hash-table-set! (d:data-path-run-ids data) run-path run-id)
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
		       (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL"))))))
    
    (set! dashboard:update-run-summary-tab updater)
    (d:data-runs-tree-set! data tb)
    (iup:split
     tb
     run-matrix)))

;;======================================================================
;; R U N S 
;;======================================================================

(define (make-dashboard-buttons data nruns ntests keynames runs-sum-dat new-view-dat)
  (let* ((db      (d:alldat-dblocal data))
	 (nkeys   (length keynames))
	 (runsvec (make-vector nruns))
	 (header  (make-vector nruns))
	 (lftcol  (make-vector ntests))
	 (keycol  (make-vector ntests))
	 (controls '())
	 (lftlst  '())
	 (hdrlst  '())
	 (bdylst  '())
	 (result  '())
	 (i       0))
    ;; controls (along bottom)
    (set! controls
	  (iup:hbox
	   (iup:vbox
	    (iup:frame 
	     #:title "filter test and items"
	     (iup:hbox
	      (iup:textbox #:size "120x15" #:fontsize "10" #:value "%"
			   #:action (lambda (obj unk val)
				      (mark-for-update)
				      (update-search "test-name" val)))
	      ;;(iup:textbox #:size "60x15" #:fontsize "10" #:value "%"
	      ;;  	   #:action (lambda (obj unk val)
	      ;;  		      (mark-for-update)
	      ;;  		      (update-search "item-name" val))
	      ))
	    (iup:vbox
	     (iup:hbox
	      (let* ((cmds-list '("+testname" "-testname" "+event_time" "-event_time" "+statestatus" "-statestatus"))
		     (lb         (iup:listbox #:expand "HORIZONTAL"
					      #:dropdown "YES"
					      #:action (lambda (obj val index lbstate)
							 (set! *tests-sort-reverse* index)
							 (mark-for-update))))
		     (default-cmd (car (list-ref *tests-sort-type-index* *tests-sort-reverse*))))
		(iuplistbox-fill-list lb cmds-list selected-item: default-cmd)
		(mark-for-update)
		;; (set! *tests-sort-reverse* *tests-sort-reverse*0)
		lb)
	      ;; (iup:button "Sort -t"   #:action (lambda (obj)
	      ;;   				 (next-sort-option)
	      ;;   				 (iup:attribute-set! obj "TITLE" (vector-ref (vector-ref *tests-sort-options* *tests-sort-reverse*) 0))
	      ;;   				 (mark-for-update)))
	      (iup:button "HideEmpty" #:action (lambda (obj)
						 (d:alldat-hide-empty-runs-set! *alldat* (not (d:alldat-hide-empty-runs *alldat*)))
						 (iup:attribute-set! obj "TITLE" (if (d:alldat-hide-empty-runs *alldat*) "+HideE" "-HideE"))
						 (mark-for-update)))
	      (let ((hideit (iup:button "HideTests" #:action (lambda (obj)
							       (d:alldat-hide-not-hide-set! *alldat* (not (d:alldat-hide-not-hide *alldat*)))
							       (iup:attribute-set! obj "TITLE" (if (d:alldat-hide-not-hide *alldat*) "HideTests" "NotHide"))
							       (mark-for-update)))))
		(d:alldat-hide-not-hide-button-set! *alldat* hideit) ;; never used, can eliminate ...
		hideit))
	     (iup:hbox
	      (iup:button "Quit"      #:action (lambda (obj)
						 ;; (if (d:alldat-dblocal *alldat*) (db:close-all (d:alldat-dblocal *alldat*)))
						 (exit)))
	      (iup:button "Refresh"   #:action (lambda (obj)
						 (mark-for-update)))
	      (iup:button "Collapse"  #:action (lambda (obj)
						 (let ((myname (iup:attribute obj "TITLE")))
						   (if (equal? myname "Collapse")
						       (begin
							 (for-each (lambda (tname)
								     (hash-table-set! *collapsed* tname #t))
								   (d:alldat-item-test-names *alldat*))
							 (iup:attribute-set! obj "TITLE" "Expand"))
						       (begin
							 (for-each (lambda (tname)
								     (hash-table-delete! *collapsed* tname))
								   (hash-table-keys *collapsed*))
							 (iup:attribute-set! obj "TITLE" "Collapse"))))
						 (mark-for-update))))))
	   (iup:frame 
	    #:title "state/status filter"
	    (iup:vbox
	     (apply 
	      iup:hbox
	      (map (lambda (status)
		     (iup:toggle status  #:action   (lambda (obj val)
						      (mark-for-update)
						      (if (eq? val 1)
							  (hash-table-set! (d:alldat-status-ignore-hash *alldat*) status #t)
							  (hash-table-delete! (d:alldat-status-ignore-hash *alldat*) status))
						      (set-bg-on-filter))))
		   (map cadr *common:std-statuses*))) ;; '("PASS" "FAIL" "WARN" "CHECK" "WAIVED" "STUCK/DEAD" "n/a" "SKIP")))
	     (apply 
	      iup:hbox
	      (map (lambda (state)
		     (iup:toggle state   #:action   (lambda (obj val)
						      (mark-for-update)
						      (if (eq? val 1)
							  (hash-table-set! (d:alldat-state-ignore-hash *alldat*) state #t)
							  (hash-table-delete! (d:alldat-state-ignore-hash *alldat*) state))
						      (set-bg-on-filter))))
		   (map cadr *common:std-states*))) ;; '("RUNNING" "COMPLETED" "INCOMPLETE" "LAUNCHED" "NOT_STARTED" "KILLED" "DELETED")))
	     (iup:valuator #:valuechanged_cb (lambda (obj)
					       (let ((val (inexact->exact (round (/ (string->number (iup:attribute obj "VALUE")) 10))))
						     (oldmax   (string->number (iup:attribute obj "MAX")))
						     (maxruns  (d:alldat-tot-runs *alldat*)))
						 (d:alldat-start-run-offset-set! *alldat* val)
						 (mark-for-update)
						 (debug:print 6 "(d:alldat-start-run-offset *alldat*) " (d:alldat-start-run-offset *alldat*) " maxruns: " maxruns ", val: " val " oldmax: " oldmax)
						 (iup:attribute-set! obj "MAX" (* maxruns 10))))
			   #:expand "HORIZONTAL"
			   #:max (* 10 (length (d:alldat-allruns *alldat*)))
			   #:min 0
			   #:step 0.01)))
					;(iup:button "inc rows" #:action (lambda (obj)(d:alldat-num-tests-set! *alldat* (+ (d:alldat-num-tests *alldat*) 1))))
					;(iup:button "dec rows" #:action (lambda (obj)(d:alldat-num-tests-set! *alldat* (if (> (d:alldat-num-tests *alldat*) 0)(- (d:alldat-num-tests *alldat*) 1) 0))))
	   )
	  )
    
    ;; create the left most column for the run key names and the test names 
    (set! lftlst (list (iup:hbox
			(iup:label) ;; (iup:valuator)
			(apply iup:vbox 
			       (map (lambda (x)		
				      (let ((res (iup:hbox #:expand "HORIZONTAL"
							   (iup:label x #:size "x15" #:fontsize "10" #:expand "HORIZONTAL")
							   (iup:textbox #:size "x15" #:fontsize "10" #:value "%" #:expand "HORIZONTAL"
									#:action (lambda (obj unk val)
										   (mark-for-update)
										   (update-search x val))))))
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
											 (d:alldat-please-update-set! *alldat* #t)
											 (d:alldat-start-test-offset-set! *alldat* (inexact->exact (round (/ val 10))))
											 (debug:print 6 "(d:alldat-start-test-offset *alldat*) " (d:alldat-start-test-offset *alldat*) " val: " val " newmax: " newmax " oldmax: " oldmax)
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
					    (mark-for-update)
					    (toggle-hide testnum))))) ;; (iup:attribute obj "TITLE"))))
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
	       (butn       (iup:button "" ;; button-key 
				       #:size "60x15" 
				       #:expand "HORIZONTAL"
				       #:fontsize "10" 
				       #:action (lambda (x)
						  (let* ((toolpath (car (argv)))
							 (buttndat (hash-table-ref (d:alldat-buttondat *alldat*) button-key))
							 (test-id  (db:test-get-id (vector-ref buttndat 3)))
							 (run-id   (db:test-get-run_id (vector-ref buttndat 3)))
							 (cmd  (conc toolpath " -test " run-id "," test-id "&")))
					;(print "Launching " cmd)
						    (system cmd))))))
	  (hash-table-set! (d:alldat-buttondat *alldat*) button-key (vector 0 "100 100 100" button-key #f #f)) 
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
			 controls))
	     (data (d:data-init (make-d:data)))
	     (tabs (iup:tabs
		    #:tabchangepos-cb (lambda (obj curr prev)
					(d:alldat-please-update-set! *alldat* #t)
					(d:alldat-curr-tab-num-set! *alldat* curr))
		    (dashboard:summary *alldat*)
		    runs-view
		    (dashboard:one-run db  runs-sum-dat)
		    (dashboard:new-view db new-view-dat)
		    (dashboard:run-controls)
		    )))
	;; (set! (iup:callback tabs tabchange-cb:) (lambda (a b c)(print "SWITCHED TO TAB: " a " " b " " c)))
	(iup:attribute-set! tabs "TABTITLE0" "Summary")
	(iup:attribute-set! tabs "TABTITLE1" "Runs")
	(iup:attribute-set! tabs "TABTITLE2" "Run Summary")
	(iup:attribute-set! tabs "TABTITLE3" "New View")
	(iup:attribute-set! tabs "TABTITLE4" "Run Control")
	(iup:attribute-set! tabs "BGCOLOR" "190 190 190")
	(d:alldat-hide-not-hide-tabs-set! *alldat* tabs)
	tabs)))
    (vector keycol lftcol header runsvec)))

(if (or (args:get-arg "-rows")
	(get-environment-variable "DASHBOARDROWS" ))
    (begin
      (d:alldat-num-tests-set! *alldat* (string->number
					 (or (args:get-arg "-rows")
					     (get-environment-variable "DASHBOARDROWS"))))
      (update-rundat *alldat* "%" (d:alldat-numruns *alldat*) "%/%" '()))
    (d:alldat-num-tests-set! *alldat* (min (max (update-rundat *alldat* "%" (d:alldat-numruns *alldat*) "%/%" '()) 8) 20)))

(define *tim* (iup:timer))
(define *ord* #f)
(iup:attribute-set! *tim* "TIME" 300)
(iup:attribute-set! *tim* "RUN" "YES")

;; Move this stuff to db.scm? I'm not sure that is the right thing to do...
;;
(d:alldat-last-db-update-set! *alldat* (file-modification-time (d:alldat-dbfpath *alldat*))) ;; (conc *toppath* "/db/main.db")))
(define *last-recalc-ended-time* 0)

(define (dashboard:been-changed)
  (> (file-modification-time (d:alldat-dbfpath *alldat*)) (d:alldat-last-db-update *alldat*)))

(define (dashboard:set-db-update-time)
  (d:alldat-last-db-update-set! *alldat* (file-modification-time (d:alldat-dbfpath *alldat*))))

(define (dashboard:recalc modtime please-update-buttons last-db-update-time)
  (or please-update-buttons
      (and (> (current-milliseconds)(+ *last-recalc-ended-time* 150))
	   (> modtime last-db-update-time)
	   (> (current-seconds)(+ last-db-update-time 1)))))

(define *monitor-db-path* (conc (d:alldat-dbdir *alldat*) "/monitor.db"))
(define *last-monitor-update-time* 0)

;; Force creation of the db in case it isn't already there.
(tasks:open-db)

(define (dashboard:get-youngest-run-db-mod-time)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 "WARNING: error in accessing databases in get-youngest-run-db-mod-time: " ((condition-property-accessor 'exn 'message) exn))
     (current-seconds)) ;; something went wrong - just print an error and return current-seconds
   (apply max (map (lambda (filen)
		     (file-modification-time filen))
		   (glob (conc (d:alldat-dbdir *alldat*) "/*.db"))))))

(define (dashboard:run-update x)
  (let* ((modtime         (dashboard:get-youngest-run-db-mod-time)) ;; (file-modification-time (d:alldat-dbfpath *alldat*)))
	 (monitor-modtime (if (file-exists? *monitor-db-path*)
			      (file-modification-time *monitor-db-path*)
			      -1))
	 (run-update-time (current-seconds))
	 (recalc          (dashboard:recalc modtime (d:alldat-please-update *alldat*) (d:alldat-last-db-update *alldat*))))
    (if (and (eq? (d:alldat-curr-tab-num *alldat*) 0)
	     (or (> monitor-modtime *last-monitor-update-time*)
		 (> (- run-update-time *last-monitor-update-time*) 5))) ;; update every 1/2 minute just in case
	(begin
	  (set! *last-monitor-update-time* run-update-time) ;; monitor-modtime)
	  (if dashboard:update-servers-table (dashboard:update-servers-table))))
    (if recalc
	(begin	
	  (case (d:alldat-curr-tab-num *alldat*) 
	    ((0) 
	     (if dashboard:update-summary-tab (dashboard:update-summary-tab)))
	    ((1) ;; The runs table is active
	     (update-rundat *alldat* (hash-table-ref/default (d:alldat-searchpatts *alldat*) "runname" "%") (d:alldat-numruns *alldat*)
			    (hash-table-ref/default (d:alldat-searchpatts *alldat*) "test-name" "%/%")
			    ;; (hash-table-ref/default (d:alldat-searchpatts *alldat*) "item-name" "%")
			    (let ((res '()))
			      (for-each (lambda (key)
					  (if (not (equal? key "runname"))
					      (let ((val (hash-table-ref/default (d:alldat-searchpatts *alldat*) key #f)))
						(if val (set! res (cons (list key val) res))))))
					(d:alldat-dbkeys *alldat*))
			      res))
	     (update-buttons uidat (d:alldat-numruns *alldat*) (d:alldat-num-tests *alldat*)))
	    ((2)
	     (dashboard:update-run-summary-tab))
	    ((3)
	     (dashboard:update-run-summary-tab))
	    (else
	     (let ((updater (hash-table-ref/default (d:alldat-updaters *alldat*)
						    (d:alldat-curr-tab-num *alldat*) #f)))
	       (if updater (updater)))))
	  (d:alldat-please-update-set! *alldat* #f)
	  (d:alldat-last-db-update-set! *alldat* modtime)
	  (set! *last-recalc-ended-time* (current-milliseconds))))))

;;======================================================================
;; The heavy lifting starts here
;;======================================================================

;; ease debugging by loading ~/.dashboardrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.dashboardrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(define (main)
  (let ((runs-sum-dat (d:data-init (make-d:data))) ;; data for run-summary tab
	(new-view-dat (d:data-init (make-d:data))))
    (cond 
     ((args:get-arg "-run")
      (let ((runid (string->number (args:get-arg "-run"))))
	(if runid
	    (begin
	      (lambda (x)
		(on-exit std-exit-procedure)
		(examine-run (d:alldat-dblocal *alldat*) runid)))
	    (begin
	      (print "ERROR: runid is not a number " (args:get-arg "-run"))
	      (exit 1)))))
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
	      (debug:print 3 "INFO: tried to open test with invalid run-id,test-id. " (args:get-arg "-test"))
	      (exit 1)))))
     ((args:get-arg "-guimonitor")
      (gui-monitor (d:alldat-dblocal *alldat*)))
     (else
      (set! uidat (make-dashboard-buttons *alldat* ;; (d:alldat-dblocal *alldat*)
					  (d:alldat-numruns *alldat*)
					  (d:alldat-num-tests *alldat*)
					  (d:alldat-dbkeys *alldat*)
					   runs-sum-dat new-view-dat))
      (iup:callback-set! *tim*
			 "ACTION_CB"
			 (lambda (x)
			   (let ((update-is-running #f))
			     (mutex-lock! (d:alldat-update-mutex *alldat*))
			     (set! update-is-running (d:alldat-updating *alldat*))
			     (if (not update-is-running)
				 (d:alldat-updating-set! *alldat* #t))
			     (mutex-unlock! (d:alldat-update-mutex *alldat*))
			     (if (not update-is-running)
				 (begin
				   (dashboard:run-update x)
				   (mutex-lock! (d:alldat-update-mutex *alldat*))
				   (d:alldat-updating-set! *alldat* #f)
				   (mutex-unlock! (d:alldat-update-mutex *alldat*)))))
			   1))))
    
    (let ((th1 (make-thread (lambda ()
			      (thread-sleep! 1)
			      (d:alldat-please-update-set! *alldat* #t)
			      (dashboard:run-update 1)) "update buttons once"))
	  (th2 (make-thread iup:main-loop "Main loop")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th2))))

(main)
