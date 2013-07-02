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
(define *num-tests*     15)
(define *start-run-offset*  0)
(define *start-test-offset* 0)
(define *examine-test-dat* (make-hash-table))
(define *exit-started* #f)
(define *status-ignore-hash* (make-hash-table))
(define *state-ignore-hash*  (make-hash-table))

(define *last-db-update-time* 0)
(define *please-update-buttons* #t)
(define *delayed-update* 0)

(define *db-file-path* (conc *toppath* "/megatest.db"))

(define *tests-sort-reverse* #f)
(define *hide-empty-runs* #f)

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

(define (colors-similar? color1 color2)
  (let* ((c1 (map string->number (string-split color1)))
	 (c2 (map string->number (string-split color2)))
	 (delta (map (lambda (a b)(abs (- a b))) c1 c2)))
    (null? (filter (lambda (x)(> x 3)) delta))))

;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
(define (update-rundat runnamepatt numruns testnamepatt keypatts)
  (let ((modtime             (file-modification-time *db-file-path*))
	(referenced-run-ids '()))
    (if (or (and (> modtime *last-db-update-time*)
		 (> (current-seconds)(+ *last-db-update-time* 5)))
	    (> *delayed-update* 0))
	;;
	;; Run this stuff only when the megatest.db file has changed
	;;
	(let ((full-run (> (random 100) 75))) ;; 25% of the time do a full refresh
	  (debug:print-info 4 "update-rundat runnamepatt: " runnamepatt " numruns: " numruns " testnamepatt: " testnamepatt " keypatts: " keypatts)
	  (set! *please-update-buttons* #t)
	  (set! *last-db-update-time* modtime)
	  (set! *delayed-update* (- *delayed-update* 1))
	  (let* ((allruns     (cdb:remote-run db:get-runs #f runnamepatt numruns ;; (+ numruns 1) ;; (/ numruns 2))
					   *start-run-offset* keypatts))
		 (header      (db:get-header allruns))
		 (runs        (db:get-rows   allruns))
		 (result      '())
		 (maxtests    0)
		 (states      (hash-table-keys *state-ignore-hash*))
		 (statuses    (hash-table-keys *status-ignore-hash*)))
	    ;; (thread-sleep! 0.1) ;; give some time to other threads
	    (debug:print 6 "update-rundat, got " (length runs) " runs")
	    (if (> (+ *last-update* 300) (current-seconds)) ;; every five minutes
		(begin
		  (set! *last-update* (current-seconds))
		  (set! *tot-run-count* (length runs))))
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
	    
	    ;;
	    ;; if full-run use referenced-run-ids to delete data in *all-runs-by-id* and *runchangerate*
	    ;;

	    (set! *header*  header)
	    (set! *allruns* result)
	    (debug:print 6 "*allruns* has " (length *allruns*) " runs")
	    ;; (set! *tot-run-count* (+ 1 (length *allruns*)))
	    maxtests))
	;; 
	;; Run this if the megatest.db file did not get touched
	;;
	(begin
	  
	  *num-tests*)))) ;; FIXME, naughty coding eh?

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
  (if *please-update-buttons*
      (let* ((runs        (if (> (length *allruns*) numruns)
			      (take-right *allruns* numruns)
			      (pad-list *allruns* numruns)))
	     (lftcol      (dboard:uidat-get-lftcol uidat))
	     (tableheader (dboard:uidat-get-header uidat))
	     (table       (dboard:uidat-get-runsvec uidat))
	     (coln        0))
	(set! *please-update-buttons* #f)
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
			       (color      (common:get-color-for-state-status teststate teststatus))
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
	 runs))))

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
(define (dashboard:populate-target-dropdown lb referent-vals targets)
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
  (let* ((db-target-dat (open-run-close db:get-targets #f))
	 (header        (vector-ref db-target-dat 0))
	 (db-targets    (vector-ref db-target-dat 1))
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
			#:size "x10" 
			#:fontsize "10"
			#:expand "VERTICAL"
			;; #:dropdown "YES"
			#:editbox "YES"
			#:action action-proc
			))))
	     ;; loop though all the targets and build the list for this dropdown
	     (selected-value (dashboard:populate-target-dropdown lb refvals db-targets)))
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

(define (dashboard:draw-tests cnv xadj yadj test-draw-state sorted-testnames)
  (canvas-clear! cnv)
  (canvas-font-set! cnv "Courier New, -10")
  (let-values (((sizex sizey sizexmm sizeymm) (canvas-size cnv))
	       ((originx originy)             (canvas-origin cnv)))
      (if (hash-table-ref/default test-draw-state 'first-time #t)
	  (begin
	    (hash-table-set! test-draw-state 'first-time #f)
	    (hash-table-set! test-draw-state 'scalef 8)
	    ;; set these 
	    (hash-table-set! test-draw-state 'test-browse-xoffset 20) ;; (- 0 (* (/ sizex 2) (* 8 xadj))))
	    (hash-table-set! test-draw-state 'test-browse-yoffset 20))) ;; (- 0 (* (/ sizey 2) (* 8 (- 1 yadj)))))))
      (let* ((scalef (hash-table-ref/default test-draw-state 'scalef 8))
	     (test-browse-xoffset (hash-table-ref test-draw-state 'test-browse-xoffset))
	     (test-browse-yoffset (hash-table-ref test-draw-state 'test-browse-yoffset))
	     (xtorig (+ test-browse-xoffset (* (/ sizex 2) scalef (- 0.5 xadj)))) ;;  (- xadj 1))))
	     (ytorig (+ test-browse-yoffset (* (/ sizey 2) scalef (- yadj 0.5))))
	     (boxw   90)
	     (boxh   25)
	     (gapx   20)
	     (gapy   30))
	(print "sizex: " sizex " sizey: " sizey " font: " (canvas-font cnv) " originx: " originx " originy: " originy " xtorig: " xtorig " ytorig: " ytorig " xadj: " xadj " yadj: " yadj)
	(let loop ((hed (car (reverse sorted-testnames)))
		   (tal (cdr (reverse sorted-testnames)))
		   (llx xtorig)
		   (lly ytorig)
		   (urx (+ xtorig boxw))
		   (ury (+ ytorig boxh)))
	  ; (print "hed " hed " llx " llx " lly " lly " urx " urx " ury " ury)
	  (canvas-text! cnv (+ llx 5)(+ lly 5) hed) ;; (conc testname " (" xtorig "," ytorig ")"))
	  (canvas-rectangle! cnv llx urx lly ury)
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
	 (runconf-targs (common:get-runconfig-targets))
	 (test-records  (make-hash-table))
	 (test-names    (tests:get-valid-tests *toppath* '()))
	 (sorted-testnames #f)
	 (action        "-runtests")
	 (cmdln         "")
	 (runlogs       (make-hash-table))
	 (key-listboxes #f)
	 (update-keyvals (lambda (obj b c d)
			   ;; (print "obj: " obj ", b " b ", c " c ", d " d)
			   (dashboard:update-target-selector key-listboxes)
			   ))
	 (tests-draw-state (make-hash-table))) ;; use for keeping state of the test canvas
    (hash-table-set! tests-draw-state 'first-time #t)
    (hash-table-set! tests-draw-state 'scalef 8)
    (tests:get-full-data test-names test-records '())
    (set! sorted-testnames (tests:sort-by-priority-and-waiton test-records))
    
    ;; refer to *keys*, *dbkeys* for keys
    (iup:vbox
     (iup:hbox
       ;; Target and action
      (iup:frame
       #:title "Target"
       (iup:vbox
        ;; Target selectors
        (apply iup:hbox
	       (let* ((dat      (dashboard:update-target-selector key-listboxes action-proc: update-keyvals))
		      (key-lb   (car dat))
		      (combos   (cadr dat)))
		 (set! key-listboxes key-lb)
		 combos))))
      (iup:frame
       #:title "Tests and Tasks"
       (iup:vbox
	(iup:canvas #:action (make-canvas-action
			      (lambda (cnv xadj yadj)
				;; (print "cnv: " cnv " x: " x " y: " y)
				(dashboard:draw-tests cnv xadj yadj tests-draw-state sorted-testnames)))
		    #:size "150x150"
		    #:expand "YES"
		    #:scrollbar "YES"
		    #:posx "0.5"
		    #:posy "0.5")))))))


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
     (iup:frame 
      #:title "General Info"
      (iup:hbox 
       (dcommon:general-info)
       (dcommon:keys-matrix rawconfig)))
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
      #:title (conc "Megatest dashboard " *toppath*)
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
		    (dashboard:summary)
		    runs-view
		    (dashboard:run-controls)
		    )))
	(iup:attribute-set! tabs "TABTITLE0" "Summary")
	(iup:attribute-set! tabs "TABTITLE1" "Runs")
	(iup:attribute-set! tabs "TABTITLE2" "Run Control")
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

(define (dashboard:run-update x)
  (update-buttons uidat *num-runs* *num-tests*)
  ;; (if (dashboard:been-changed)
  (begin
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
    ; (dashboard:set-db-update-time)
    ))

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
		       (dashboard:run-update x)))))
		       ;(print x)))))

(iup:main-loop)
