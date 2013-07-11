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
(import canvas-draw-iup)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)
(import (prefix sqlite3 sqlite3:))
(use trace)

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

(define help (conc 
"Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright (C) Matt Welland 2013

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
			"-use-server"
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

(define *db* #f) ;; (open-db))

(if (args:get-arg "-host")
    (begin
      (set! *runremote* (string-split (args:get-arg "-host" ":")))
      (client:launch))
    (if (not (args:get-arg "-use-server"))
	(set! *transport-type* 'fs) ;; force fs access
	(client:launch)))

;; HACK ALERT: this is a hack, please fix.
(define *read-only* (not (file-read-access? (conc *toppath* "/megatest.db"))))
;; (client:setup *db*)

(define toplevel #f)
(define dlg      #f)
(define max-test-num 0)
;; (define *keys*   (open-run-close db:get-keys #f))
(define *keys*   (cdb:remote-run db:get-keys #f))
;; (define *keys*   (db:get-keys   *db*))

(define *dbkeys*  (append *keys* (list "runname")))

(define *header*       #f)
(define *allruns*     '())
(define *allruns-by-id* (make-hash-table)) ;; 
(define *runchangerate* (make-hash-table))

(define *buttondat*    (make-hash-table)) ;; <run-id color text test run-key>
(define *alltestnamelst* '())
(define *searchpatts*  (make-hash-table))
(define *num-runs*      8)
(define *tot-run-count* (cdb:remote-run db:get-num-runs #f "%"))
;; (define *tot-run-count* (db:get-num-runs *db* "%"))
(define *last-update*   (current-seconds))
(define *last-db-update-time* 0)
(define *please-update-buttons* #t)
(define *delayed-update* 0)

(define *num-tests*     15)
(define *start-run-offset*  0)
(define *start-test-offset* 0)
(define *examine-test-dat* (make-hash-table))
(define *exit-started* #f)
(define *status-ignore-hash* (make-hash-table))
(define *state-ignore-hash*  (make-hash-table))

(define *db-file-path* (conc *toppath* "/megatest.db"))

(define *tests-sort-reverse* #f)
(define *hide-empty-runs* #f)

(define *current-tab-number* 0)
(define *updaters* (make-hash-table))

(debug:setup)

(define uidat #f)

(define-inline (dboard:uidat-get-keycol  vec)(vector-ref vec 0))
(define-inline (dboard:uidat-get-lftcol  vec)(vector-ref vec 1))
(define-inline (dboard:uidat-get-header  vec)(vector-ref vec 2))
(define-inline (dboard:uidat-get-runsvec vec)(vector-ref vec 3))


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
  (let* ((c1 (map string->number (string-split color1)))
	 (c2 (map string->number (string-split color2)))
	 (delta (map (lambda (a b)(abs (- a b))) c1 c2)))
    (null? (filter (lambda (x)(> x 3)) delta))))

;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
(define (update-rundat runnamepatt numruns testnamepatt keypatts)
  (let* ((referenced-run-ids '())
	 (allruns     (cdb:remote-run db:get-runs #f runnamepatt numruns ;; (+ numruns 1) ;; (/ numruns 2))
				      *start-run-offset* keypatts))
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns))
	 (result      '())
	 (maxtests    0)
	 (states      (hash-table-keys *state-ignore-hash*))
	 (statuses    (hash-table-keys *status-ignore-hash*)))
    ;; 
    ;; trim runs to only those that are changing often here
    ;; 
    (for-each (lambda (run)
		(let* ((run-id   (db:get-value-by-header run header "id"))
		       (tests    (let ((tsts (mt:get-tests-for-run run-id testnamepatt states statuses)))
				   (if *tests-sort-reverse* (reverse tsts) tsts)))
		       (key-vals (cdb:remote-run db:get-key-vals #f run-id)))
		  ;; Not sure this is needed?
		  (set! referenced-run-ids (cons run-id referenced-run-ids))
		  (if (> (length tests) maxtests)
		      (set! maxtests (length tests)))
		  (if (or (not *hide-empty-runs*) ;; this reduces the data burden when set
			  (not (null? tests)))
		      (let ((dstruct (vector run tests key-vals)))
			;;
			;; compare the tests with the tests in *allruns-by-id* same run-id 
			;; if different then increment value in *runchangerate*
			;;
			(hash-table-set! *allruns-by-id* run-id dstruct)
			(set! result (cons dstruct result))))))
	      runs)

    (set! *header*  header)
    (set! *allruns* result)
    (debug:print-info 6 "*allruns* has " (length *allruns*) " runs")
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
  (let* ((newlst (filter (lambda (x)
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
	 (vlst  (run-item-name->vectors newlst))
	 ;; sort by second field
	 (vlst-s1 (sort vlst (lambda (a b)
			       (let ((astr (vector-ref a 1))
				     (bstr (vector-ref b 1)))
				 (if (string=? astr "") #f #t)))))
			;; (>= (string-length (vector-ref a 1))(string-length (vector-ref b 1))))))
	 (vlst-s2 (sort vlst-s1 (lambda (a b)
			   	  (string>= (vector-ref a 0)(vector-ref b 0))))))
    (map (lambda (x)
	   (if (equal? (vector-ref x 1) "")
	       (vector-ref x 0)
	       (conc (vector-ref x 0) "(" (vector-ref x 1) ")")))
	 vlst-s2)))
    
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

(define (update-buttons uidat numruns numtests)
  (let* ((runs        (if (> (length *allruns*) numruns)
			  (take-right *allruns* numruns)
			  (pad-list *allruns* numruns)))
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
	     (if (not (and *hide-empty-runs*
			   (null? testnames)))
		 (for-each (lambda (testname)
			     (if (not (member testname *alltestnamelst*))
				 (begin
				   (set! *alltestnamelst* (append *alltestnamelst* (list testname))))))
			   testnames)))))
     runs)

    (set! *alltestnamelst* (collapse-rows *alltestnamelst*)) ;;; argh. please clean up this sillyness
    (set! *alltestnamelst* (let ((xl (if (> (length *alltestnamelst*) *start-test-offset*)
					 (drop *alltestnamelst* *start-test-offset*)
					 '())))
			     (append xl (make-list (- *num-tests* (length xl)) ""))))
    (update-labels uidat)
    (for-each
     (lambda (rundat)
       (if (not rundat) ;; handle padded runs
	   ;;           ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration
	   (set! rundat (vector (make-vector 20 #f) '() (map (lambda (x) "") *keys*))));; 3)))
       (let* ((run      (vector-ref rundat 0))
	      (testsdat (vector-ref rundat 1))
	      (key-val-dat (vector-ref rundat 2))
	      (run-id   (db:get-value-by-header run *header* "id"))
	      (key-vals (append key-val-dat
				(list (let ((x (db:get-value-by-header run *header* "runname")))
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
	      (let ((buttondat  (hash-table-ref/default *buttondat* (mkstr coln rown) #f)))
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
			   (teststart  (db:test-get-event_time test))
			   (runtime    (db:test-get-run_duration test))
			   (buttontxt  (if (equal? teststate "COMPLETED") teststatus teststate))
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

(define (update-search x val)
  ;; (print "Setting search for " x " to " val)
  (hash-table-set! *searchpatts* x val))

(define (mark-for-update)
  (set! *last-db-update-time* 0)
  (set! *delayed-update* 1))

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
	 (db-target-dat (open-run-close db:get-targets #f))
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
			;; #:size "x10" 
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
			    " :runname "
			    run-name
			    )))
      ((remove-runs)
       (set! full-cmd (conc full-cmd
			    " -remove-runs :runname "
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
(define (dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames)
  (canvas-clear! cnv)
  (canvas-font-set! cnv "Helvetica, -10")
  (let-values (((sizex sizey sizexmm sizeymm) (canvas-size cnv))
	       ((originx originy)             (canvas-origin cnv)))
      ;; (print "originx: " originx " originy: " originy)
      ;; (canvas-origin-set! cnv 0 (- (/ sizey 2)))
      (if (hash-table-ref/default tests-draw-state 'first-time #t)
	  (begin
	    (hash-table-set! tests-draw-state 'first-time #f)
	    (hash-table-set! tests-draw-state 'scalef 8)
	    (hash-table-set! tests-draw-state 'tests-info (make-hash-table))
	    (hash-table-set! tests-draw-state 'selected-tests (make-hash-table))
	    ;; set these 
	    (hash-table-set! tests-draw-state 'test-browse-xoffset 20) ;; (- 0 (* (/ sizex 2) (* 8 xadj))))
	    (hash-table-set! tests-draw-state 'test-browse-yoffset 20))) ;; (- 0 (* (/ sizey 2) (* 8 (- 1 yadj)))))))
      (let* ((scalef (hash-table-ref/default tests-draw-state 'scalef 8))
	     (test-browse-xoffset (hash-table-ref tests-draw-state 'test-browse-xoffset))
	     (test-browse-yoffset (hash-table-ref tests-draw-state 'test-browse-yoffset))
	     (xtorig (+ test-browse-xoffset (* (/ sizex 2) scalef (- 0.5 xadj)))) ;;  (- xadj 1))))
	     (ytorig (+ test-browse-yoffset (* (/ sizey 2) scalef (- yadj 0.5))))
	     (boxw   90)
	     (boxh   25)
	     (gapx   20)
	     (gapy   30)
	     (tests-hash     (hash-table-ref tests-draw-state 'tests-info))
	     (selected-tests (hash-table-ref tests-draw-state 'selected-tests )))
	;; (print "sizex: " sizex " sizey: " sizey " font: " (canvas-font cnv) " originx: " originx " originy: " originy " xtorig: " xtorig " ytorig: " ytorig " xadj: " xadj " yadj: " yadj)
	(let loop ((hed (car (reverse sorted-testnames)))
		   (tal (cdr (reverse sorted-testnames)))
		   (llx xtorig)
		   (lly ytorig)
		   (urx (+ xtorig boxw))
		   (ury (+ ytorig boxh)))
	  ; (print "hed " hed " llx " llx " lly " lly " urx " urx " ury " ury)
	  (canvas-text! cnv (+ llx 5)(+ lly 5) hed) ;; (conc testname " (" xtorig "," ytorig ")"))
	  (canvas-rectangle! cnv llx urx lly ury)
	  (if (hash-table-ref/default selected-tests hed #f)
	      (canvas-box! cnv llx (+ llx 5) lly (+ lly 5)))
	  (hash-table-set! tests-hash hed (list llx urx (- sizey ury)(- sizey lly))) ;; NB// Swap ury and lly
	  (if (not (null? tal))
	      ;; leave a column of space to the right to list items
	      (let ((have-room 
		     (if #t ;; put "auto" here where some form of auto rearanging can be done
			 (> (* 3 (+ boxw gapx)) (- urx xtorig))
			 (< urx (- sizex boxw gapx boxw)))))  ;; is there room for another column?
		(loop (car tal)
		      (cdr tal)
		      (if have-room (+ llx boxw gapx) xtorig) ;; have room, 
		      (if have-room lly (+ lly boxh gapy))
		      (if have-room (+ urx boxw gapx) (+ xtorig boxw))
		      (if have-room ury (+ ury boxh gapy)))))))))

;;======================================================================
;; R U N   C O N T R O L S
;;======================================================================
;;
;; A gui for launching tests
;;
(define (dashboard:run-controls)
  (let* ((targets       (make-hash-table))
	 (test-records  (make-hash-table))
	 (test-names    (tests:get-valid-tests *toppath* '()))
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
    (hash-table-set! tests-draw-state 'scalef 8)
    (tests:get-full-data test-names test-records '())
    (set! sorted-testnames (tests:sort-by-priority-and-waiton test-records))
    
    ;; refer to *keys*, *dbkeys* for keys
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
	 (let* ((default-run-name (conc "ww" (seconds->work-week/day (current-seconds))))
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
					    (runs-for-targ (mt:get-runs-by-patt *keys* "%" target))
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
	     '("COMPLETED" "RUNNING" "STUCK" "INCOMPLETE" "LAUNCHED" "REMOTEHOSTSTART" "KILLED")
	     (lambda (all)
	       (dboard:data-set-states! *data* all)
	       (dashboard:update-run-command))))
	   ;; Text box for STATES
	   (iup:frame
	    #:title "Statuses"
	    (dashboard:text-list-toggle-box 
	     '("PASS" "FAIL" "n/a" "CHECK" "WAIVED" "SKIP" "DELETED" "STUCK/DEAD")
	     (lambda (all)
	       (dboard:data-set-statuses! *data* all)
	       (dashboard:update-run-command))))))))
      
       (iup:frame
	#:title "Tests and Tasks"
	(let* ((updater #f)
	       (last-xadj 0)
	       (last-yadj 0)
	       (canvas-obj   
	(iup:canvas #:action (make-canvas-action
			      (lambda (cnv xadj yadj)
				(if (not updater)
				    (set! updater (lambda (xadj yadj)
						    ;; (print "cnv: " cnv " x: " x " y: " y)
						    (dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames))))
				(updater xadj yadj)
				(set! last-xadj xadj)
				(set! last-yadj yadj)))
		    ;; Following doesn't work 
		    ;; #:wheel-cb (make-canvas-action
		    ;;           (lambda (cnv xadj yadj)
		    ;;    	 ;; (print "cnv: " cnv " x: " x " y: " y)
		    ;;    	 (dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames)))
		    #:size "150x150"
		    #:expand "YES"
		    #:scrollbar "YES"
		    #:posx "0.5"
		    #:posy "0.5"
		    #:button-cb (lambda (obj btn pressed x y status)
				  ;; (print "obj: " obj)
				  (let ((tests-info     (hash-table-ref tests-draw-state  'tests-info))
					(selected-tests (hash-table-ref tests-draw-state  'selected-tests)))
				    ;; (print "x\ty\tllx\tlly\turx\tury")
				    (for-each (lambda (test-name)
						(let* ((rec-coords (hash-table-ref tests-info test-name))
						       (llx        (list-ref rec-coords 0))
						       (urx        (list-ref rec-coords 1))
						       (lly        (list-ref rec-coords 2))
						       (ury        (list-ref rec-coords 3)))
						  ;; (print x "\t" y "\t" llx "\t" lly "\t" urx "\t" ury "\t" test-name " "
						  (if (and (eq? pressed 1)
							   (> x llx)
							   (> y lly)
							   (< x urx)
							   (< y ury))
						      (let ((patterns (string-split (iup:attribute test-patterns-textbox "VALUE"))))
							(let* ((selected     (not (member test-name patterns)))
							       (newpatt-list (if selected
										 (cons test-name patterns)
										 (delete test-name patterns)))
							       (newpatt      (string-intersperse newpatt-list "\n")))
							  ;; (if cnv-obj
							  ;;    (dashboard:draw-tests cnv-obj 0 0 tests-draw-state sorted-testnames))
							  (iup:attribute-set! obj "REDRAW" "ALL")
							  (hash-table-set! selected-tests test-name selected)
							  (iup:attribute-set! test-patterns-textbox "VALUE" newpatt)
							  (dboard:data-set-test-patts! *data* (dboard:lines->test-patt newpatt))
							  (dashboard:update-run-command)
							  (if updater (updater last-xadj last-yadj)))))))
					      (hash-table-keys tests-info)))))))
	  canvas-obj)))
      ;; (print "obj: " obj " btn: " btn " pressed: " pressed " x: " x " y: " y " status: " status))
      
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
(define (dashboard:summary)
  (let ((rawconfig        (read-config (conc *toppath* "/megatest.config") #f 'return-string)))
    (iup:vbox
     (iup:split
      ;; #:value 500
      (iup:frame 
       #:title "General Info"
       (iup:hbox 
	(dcommon:keys-matrix rawconfig)
	(dcommon:general-info)
	))
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
      (dcommon:run-stats)))))

;;======================================================================
;; R U N
;;======================================================================
;;
;; display and manage a single run at a time

(define (tree-path->run-id path)
  (if (not (null? path))
      (hash-table-ref/default (dboard:data-get-path-run-ids *data*) path #f)
      #f))

(define dashboard:update-run-summary-tab #f)

;; (define (tests window-id)
(define (dashboard:one-run)
  (let* ((tb      (iup:treebox
		   #:value 0
		   #:name "Runs"
		   #:expand "YES"
		   #:selection-cb
		   (lambda (obj id state)
		     ;; (print "obj: " obj ", id: " id ", state: " state)
		     (let* ((run-path (tree:node->path obj id))
			    (run-id   (tree-path->run-id (cdr run-path))))
		       (if run-id
			   (begin
			     (dboard:data-set-curr-run-id! *data* run-id)
			     (dashboard:update-run-summary-tab)))
		       ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		       ))))
	 (run-matrix (iup:matrix
		      #:expand "YES"))
	 (updater  (lambda ()
		     (let* ((runs-dat     (mt:get-runs-by-patt *keys* "%" #f))
			    (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
			    (run-id       (dboard:data-get-curr-run-id *data*))
			    (tests-dat    (let ((tdat (mt:get-tests-for-run run-id "%" '() '()
									    qryvals: "id,testname,item_path,state,status"))) ;; get 'em all
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
			    (max-visible  (max (- *num-tests* 15) 3)) ;; *num-tests* is proportional to the size of the window
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
							   *keys*))
					  (run-name   (db:get-value-by-header run-record runs-header "runname"))
					  (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
					  (run-path   (append key-vals (list run-name))))
				     (hash-table-set! (dboard:data-get-run-keys *data*) run-id run-path)
				     ;; (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
				     ;;    		 (conc rownum ":" colnum) col-name)
				     ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
				     ;; Here we update the tests treebox and tree keys
				     (tree:add-node tb "Runs" (append key-vals (list run-name))
						    userdata: (conc "run-id: " run-id))
				     (let ((path ;;(string-intersperse "/" 
					    (append key-vals (list run-name))))
				       (hash-table-set! (dboard:data-get-path-run-ids *data*) path run-id))
				     ;; (set! colnum (+ colnum 1))
				     ))
				 run-ids)
		       (iup:attribute-set! run-matrix "CLEARVALUE" "CONTENTS")
		       (iup:attribute-set! run-matrix "CLEARATTRIB" "CONTENTS")
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
		       
		       ;; Col labels
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
				     (if (not (equal? (iup:attribute run-matrix key) (cadr value)))
					 (begin
					   (set! changed #t)
					   (iup:attribute-set! run-matrix key (cadr value))
					   (iup:attribute-set! run-matrix (conc "BGCOLOR" key) (car value))))))
				 tests-mindat)
		       (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL"))))))
    (set! dashboard:update-run-summary-tab updater)
    (dboard:data-set-runs-tree! *data* tb)
    (iup:split
     tb
     run-matrix)))

;;======================================================================
;; R U N S 
;;======================================================================

(define (make-dashboard-buttons nruns ntests keynames)
  (let* ((nkeys   (length keynames))
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
	      (iup:button "Sort" #:action (lambda (obj)
					    (set! *tests-sort-reverse* (not *tests-sort-reverse*))
					    (iup:attribute-set! obj "TITLE" (if *tests-sort-reverse* "+Sort" "-Sort"))
					    (mark-for-update)))
	      (iup:button "HideEmpty" #:action (lambda (obj)
						 (set! *hide-empty-runs* (not *hide-empty-runs*))
						 (iup:attribute-set! obj "TITLE" (if *hide-empty-runs* "+Hide" "-Hide"))
						 (mark-for-update)))
	      (iup:button "Refresh"   #:action (lambda (obj)
						 (mark-for-update))))
	     (iup:hbox
	      (iup:button "Quit" #:action (lambda (obj)(if *db* (sqlite3:finalize! *db*))(exit)))
	      (iup:button "Monitor" #:action (lambda (obj)(system (conc (car (argv))" -guimonitor &")))))
	     ))
	   ;; (iup:button "<-  Left" #:action (lambda (obj)(set! *start-run-offset*  (+ *start-run-offset* 1))))
	   ;; (iup:button "Up     ^" #:action (lambda (obj)(set! *start-test-offset* (if (> *start-test-offset* 0)(- *start-test-offset* 1) 0))))
	   ;; (iup:button "Down   v" #:action (lambda (obj)(set! *start-test-offset* (if (>= *start-test-offset* (length *alltestnamelst*))(length *alltestnamelst*)(+ *start-test-offset* 1)))))
	   ;; (iup:button "Right ->" #:action (lambda (obj)(set! *start-run-offset*  (if (> *start-run-offset* 0)(- *start-run-offset* 1) 0))))
	   (iup:frame 
	    #:title "hide"
	    (iup:vbox
	     (apply 
	      iup:hbox
	      (map (lambda (status)
		     (iup:toggle status  #:action   (lambda (obj val)
						      (mark-for-update)
						      (if (eq? val 1)
							  (hash-table-set! *status-ignore-hash* status #t)
							  (hash-table-delete! *status-ignore-hash* status)))))
		   '("PASS" "FAIL" "WARN" "CHECK" "WAIVED" "STUCK/DEAD" "n/a" "SKIP")))
	     (apply 
	      iup:hbox
	      (map (lambda (state)
		     (iup:toggle state   #:action   (lambda (obj val)
						      (mark-for-update)
						      (if (eq? val 1)
							  (hash-table-set! *state-ignore-hash* state #t)
							  (hash-table-delete! *state-ignore-hash* state)))))
		   '("RUNNING" "COMPLETED" "INCOMPLETE" "LAUNCHED" "NOT_STARTED" "KILLED" "DELETED")))
	     (iup:valuator #:valuechanged_cb (lambda (obj)
					       (let ((val (inexact->exact (round (/ (string->number (iup:attribute obj "VALUE")) 10))))
						     (oldmax   (string->number (iup:attribute obj "MAX")))
						     (maxruns  *tot-run-count*))
						 (set! *start-run-offset* val)
						 (mark-for-update)
						 (debug:print 6 "*start-run-offset* " *start-run-offset* " maxruns: " maxruns ", val: " val " oldmax: " oldmax)
						 (iup:attribute-set! obj "MAX" (* maxruns 10))))
			   #:expand "YES"
			   #:max (* 10 (length *allruns*)))))
					;(iup:button "inc rows" #:action (lambda (obj)(set! *num-tests* (+ *num-tests* 1))))
					;(iup:button "dec rows" #:action (lambda (obj)(set! *num-tests* (if (> *num-tests* 0)(- *num-tests* 1) 0))))
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
											 (set! *please-update-buttons* #t)
											 (set! *start-test-offset* (inexact->exact (round (/ val 10))))
											 (debug:print 6 "*start-test-offset* " *start-test-offset* " val: " val " newmax: " newmax " oldmax: " oldmax)
											 (if (< val 10)
											     (iup:attribute-set! obj "MAX" newmax))
											 ))
								   #:expand "VERTICAL" 
								   #:orientation "VERTICAL")
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
							 (buttndat (hash-table-ref *buttondat* button-key))
							 (test-id  (db:test-get-id (vector-ref buttndat 3)))
							 (cmd  (conc toolpath " -test " test-id "&")))
					;(print "Launching " cmd)
						    (system cmd))))))
	  (hash-table-set! *buttondat* button-key (vector 0 "100 100 100" button-key #f #f)) 
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
	     (tabs (iup:tabs
		    #:tabchangepos-cb (lambda (obj curr prev)
					(set! *please-update-buttons* #t)
					(set! *current-tab-number* curr))
		    (dashboard:summary)
		    runs-view
		    (dashboard:one-run)
		    (dashboard:run-controls)
		    )))
	;; (set! (iup:callback tabs tabchange-cb:) (lambda (a b c)(print "SWITCHED TO TAB: " a " " b " " c)))
	(iup:attribute-set! tabs "TABTITLE0" "Summary")
	(iup:attribute-set! tabs "TABTITLE1" "Runs")
	(iup:attribute-set! tabs "TABTITLE2" "Run Summary")
	(iup:attribute-set! tabs "TABTITLE3" "Run Control")
	tabs)))
    (vector keycol lftcol header runsvec)))

(if (or (args:get-arg "-rows")
	(get-environment-variable "DASHBOARDROWS" ))
    (begin
      (set! *num-tests* (string->number (or (args:get-arg "-rows")
					    (get-environment-variable "DASHBOARDROWS"))))
      (update-rundat "%" *num-runs* "%/%" '()))
    (set! *num-tests* (min (max (update-rundat "%" *num-runs* "%/%" '()) 8) 20)))

(define *tim* (iup:timer))
(define *ord* #f)
(iup:attribute-set! *tim* "TIME" 300)
(iup:attribute-set! *tim* "RUN" "YES")

;; Move this stuff to db.scm? I'm not sure that is the right thing to do...
;;
(define *last-db-update-time* (file-modification-time (conc *toppath* "/megatest.db")))

(define (dashboard:been-changed)
  (> (file-modification-time (conc *toppath* "/megatest.db")) *last-db-update-time*))

(define (dashboard:set-db-update-time)
  (set! *last-db-update-time* (file-modification-time (conc *toppath* "/megatest.db"))))

(define (dashboard:recalc modtime please-update-buttons last-db-update-time)
  (or please-update-buttons
      (and (> modtime last-db-update-time)
	   (> (current-seconds)(+ last-db-update-time 1)))))

(define *monitor-db-path* (conc *toppath* "/monitor.db"))
(define *last-monitor-update-time* 0)

(define (dashboard:run-update x)
  (let* ((modtime         (file-modification-time *db-file-path*))
	 (monitor-modtime (file-modification-time *monitor-db-path*))
	 (run-update-time (current-seconds))
	 (recalc          (dashboard:recalc modtime *please-update-buttons* *last-db-update-time*)))
    (if (and (eq? *current-tab-number* 0)
	     (> monitor-modtime *last-monitor-update-time*))
	(begin
	  (set! *last-monitor-update-time* monitor-modtime)
	  (if dashboard:update-servers-table (dashboard:update-servers-table))))
    (if recalc
	(begin	
	  (case *current-tab-number* 
	    ((0) 
	     (if dashboard:update-summary-tab (dashboard:update-summary-tab)))
	    ((1) ;; The runs table is active
	     (update-rundat (hash-table-ref/default *searchpatts* "runname" "%") *num-runs*
			    (hash-table-ref/default *searchpatts* "test-name" "%/%")
			    ;; (hash-table-ref/default *searchpatts* "item-name" "%")
			    (let ((res '()))
			      (for-each (lambda (key)
					  (if (not (equal? key "runname"))
					      (let ((val (hash-table-ref/default *searchpatts* key #f)))
						(if val (set! res (cons (list key val) res))))))
					*dbkeys*)
			      res))
	     (update-buttons uidat *num-runs* *num-tests*))
	    ((2)
	     (dashboard:update-run-summary-tab))
	    (else
	     (let ((updater (hash-table-ref/default *updaters* *current-tab-number* #f)))
	       (if updater (updater)))))
	  (set! *please-update-buttons* #f)
	  (set! *last-db-update-time* modtime)
	  (set! *last-update* run-update-time)))))

;;======================================================================
;; The heavy lifting starts here
;;======================================================================

;; ease debugging by loading ~/.megatestrc
(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.megatestrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(cond 
 ((args:get-arg "-run")
  (let ((runid (string->number (args:get-arg "-run"))))
    (if runid
	(begin
	  (lambda (x)
	    (on-exit (lambda ()
		       (if *db* (sqlite3:finalize! *db*))))
	    (cdb:remote-run examine-run *db* runid)))
	(begin
	  (print "ERROR: runid is not a number " (args:get-arg "-run"))
	  (exit 1)))))
 ((args:get-arg "-test")
  (let ((testid (string->number (args:get-arg "-test"))))
    (if testid
	(examine-test testid)
	(begin
	  (print "ERROR: testid is not a number " (args:get-arg "-test"))
	  (exit 1)))))
 ((args:get-arg "-guimonitor")
  (gui-monitor *db*))
 (else
  (set! uidat (make-dashboard-buttons *num-runs* *num-tests* *dbkeys*))
  (iup:callback-set! *tim*
		     "ACTION_CB"
		     (lambda (x)
		       (dashboard:run-update x)
		       1))))

(iup:main-loop)
