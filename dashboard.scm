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

(use format)
(require-library iup)
(import (prefix iup iup:))

(use canvas-draw)

(use sqlite3 srfi-1 posix regex regex-case srfi-69)

(import (prefix sqlite3 sqlite3:))

(include "common.scm")
(include "margs.scm")
(include "keys.scm")
(include "items.scm")
(include "db.scm")
(include "configf.scm")
(include "process.scm")
(include "launch.scm")
(include "runs.scm")
(include "gui.scm")
(include "dashboard-tests.scm")

(define help "
Megatest Dashboard, documentation at http://www.kiatoa.com/fossils/megatest
  version 0.2
  license GPL, Copyright Matt Welland 2011

Usage: dashboard [options]
  -h              : this help
  -run runid      : control run identified by runid
  -test testid    : control test identified by testid

Misc
  -rows N         : set number of rows
")

;; process args
(define remargs (args:get-args 
		 (argv)
		 (list  "-rows"
			"-run"
			"-test"
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

(if (not (setup-for-run))
    (begin
      (print "Failed to find megatest.config, exiting") 
      (exit 1)))

(define *db* (open-db))

(define toplevel #f)
(define dlg      #f)
(define max-test-num 0)
(define *keys*   (get-keys   *db*))
(define dbkeys   (map (lambda (x)(vector-ref x 0))
		      (append *keys* (list (vector "runname" "blah")))))
(define *header*       #f)
(define *allruns*     '())
(define *buttondat*    (make-hash-table)) ;; <run-id color text test run-key>
(define *alltestnamelst* '())
(define *searchpatts*  (make-hash-table))
(define *num-runs*      10)
(define *num-tests*     15)
(define *start-run-offset*  0)
(define *start-test-offset* 0)
(define *examine-test-dat* (make-hash-table))
(define *exit-started* #f)

(define *verbosity* (cond
		     ((args:get-arg "-debug")(string->number (args:get-arg "-debug")))
		     ((args:get-arg "-v")    2)
		     ((args:get-arg "-q")    0)
		     (else                   1)))

(define uidat #f)
;; (megatest-dashboard)

;(define img1 (iup:image/palette 16 16 (u8vector->blob (u8vector
;				   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 
;				   1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 
;				   1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 
;				   1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 0 2 0 2 2 0 2 2 2 0 0 
;				   2 2 2 0 2 0 0 2 0 0 2 0 2 0 2 2 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 2 2 2 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 2 0 0 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 0 2 1))))
;
;(define img2 (iup:image/palette 16 16 (u8vector->blob (u8vector
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 
;				   1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 
;				   1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 2 2 2 2 2 2 2 2 2 2 2 
;				   2 2 2 0 2 0 2 0 2 2 0 2 2 2 0 0 
;				   2 2 2 0 2 0 0 2 0 0 2 0 2 0 2 2 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 2 2 2 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 2 0 0 
;				   2 2 2 0 2 0 2 2 0 2 2 0 2 0 2 1))))
;
;(iup:handle-name-set! img1 "img1")
;(iup:attribute-set! img1 "0" "0 0 0")
;(iup:attribute-set! img1 "1" "BGCOLOR")
;(iup:attribute-set! img1 "2" "255 0 0")
;
;(iup:handle-name-set! img2 "img2")
;(iup:attribute-set! img2 "0" "0 0 0")
;(iup:attribute-set! img2 "1" "BGCOLOR")
;(iup:attribute-set! img2 "2" "255 0 0")

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

(define (update-rundat runnamepatt numruns testnamepatt itemnamepatt)
  (let* ((allruns     (db-get-runs *db* runnamepatt numruns *start-run-offset*))
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns))
	 (result      '())
	 (maxtests    0))
    (for-each (lambda (run)
		(let* ((run-id   (db:get-value-by-header run header "id"))
		       (tests    (db-get-tests-for-run *db* run-id testnamepatt itemnamepatt))
		       (key-vals (get-key-vals *db* run-id)))
		  (if (> (length tests) maxtests)
		      (set! maxtests (length tests)))
		  (set! result (cons (vector run tests key-vals) result))))
	      runs)
    (set! *header*  header)
    (set! *allruns* result)
    maxtests))

(define *collapsed* (make-hash-table))
; (define *row-lookup* (make-hash-table)) ;; testname => (rownum lableobj)

(define (toggle-hide lnum) ; fulltestname)
  (let* ((btn (vector-ref (vector-ref uidat 0) lnum))
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

(define (collapse-rows inlst)
  (let ((newlst (filter (lambda (x)
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
			inlst)))
    ;; special sort to push the test(item) to after test
    (sort newlst (lambda (a b)
		   (let* ((partsa (string-split a "("))
			  (partsb (string-split b "("))
			  (lena   (length partsa))
			  (lenb   (length partsb)))
		     (if (or (and (eq? lena 1)(> lenb 1))
			     (and (eq? lenb 1)(> lena 1)))
			 (if (equal? (car partsa)(car partsb)) ;; same test
			     (> lenb lena)
			     #t)
			 #t))))))
			     
(define (update-labels uidat)
  (let* ((rown    0)
	 (lftcol  (vector-ref uidat 0))
	 (numcols (vector-length lftcol))
	 (maxn    (- numcols 1))
	 (allvals (make-vector numcols "")))
    (for-each (lambda (name)
		(if (<= rown maxn)
		    (let ((labl (vector-ref lftcol rown)))
		      (vector-set! allvals rown name)))
		(set! rown (+ 1 rown)))
	      *alltestnamelst*)
	;      (if (> (length *alltestnamelst*) *start-test-offset*)
	;	  (drop *alltestnamelst* *start-test-offset*)
	;	  '()))
    (let loop ((i 0))
      (let* ((lbl    (vector-ref lftcol i))
	     (oldval (iup:attribute lbl "TITLE"))
	     (newval (vector-ref allvals i)))
	(if (not (equal? oldval newval))
	    (iup:attribute-set! lbl "TITLE" newval))
	(iup:attribute-set! lbl "FGCOLOR" (if (hash-table-ref/default *collapsed* newval #f) "0 112 112" "0 0 0"))
	(if (< i maxn)
	    (loop (+ i 1)))))))

(define (get-color-for-state-status state status)
  (case (string->symbol state)
    ((COMPLETED)
     (if (equal? status "PASS")
	 "70 249 73"
	 (if (or (equal? status "WARN")
		 (equal? status "WAIVED"))
	     "255 172 13"
	     "223 33 49"))) ;; greenish orangeish redish
    ((LAUNCHED)         "101 123 142")
    ((CHECK)            "255 100 50")
    ((REMOTEHOSTSTART)  "50 130 195")
    ((RUNNING)          "9 131 232")
    ((KILLREQ)          "39 82 206")
    ((KILLED)           "234 101 17")
    ((NOT_STARTED)      "240 240 240")
    (else               "192 192 192")))

(define (update-buttons uidat numruns numtests)
  (let* ((runs        (if (> (length *allruns*) numruns)
			  (take-right *allruns* numruns)
			  (pad-list *allruns* numruns)))
	 (lftcol      (vector-ref uidat 0))
	 (tableheader (vector-ref uidat 1))
	 (table       (vector-ref uidat 2))
	 (coln        0))
    (set! *alltestnamelst* '())
    ;; create a concise list of test names
    (for-each (lambda (rundat)
		(if (vector? rundat)
		    (let* ((testdat   (vector-ref rundat 1))
			   (testnames (map test:test-get-fullname testdat)))
		      (for-each (lambda (testname)
				  (if (not (member testname *alltestnamelst*))
				      (begin
					(set! *alltestnamelst* (append *alltestnamelst* (list testname))))))
				testnames))))
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
			   (color      (get-color-for-state-status teststate teststatus))
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

(define (make-dashboard-buttons nruns ntests keynames)
  (let* ((nkeys   (length keynames))
	 (runsvec (make-vector nruns))
	 (header  (make-vector nruns))
	 (lftcol  (make-vector ntests))
	 (controls '())
	 (lftlst  '())
	 (hdrlst  '())
	 (bdylst  '())
	 (result  '())
	 (i       0))
    ;; controls (along bottom)
    (set! controls
	  (iup:hbox
	   (iup:textbox #:size "60x15" #:fontsize "10" #:value "%"
			#:action (lambda (obj unk val)
				   (update-search "test-name" val)))
	   (iup:textbox #:size "60x15" #:fontsize "10" #:value "%"
			#:action (lambda (obj unk val)
				   (update-search "item-name" val)))
	   (iup:button "Quit" #:action (lambda (obj)(sqlite3:finalize! *db*)(exit)))
	   (iup:button "<-  Left" #:action (lambda (obj)(set! *start-run-offset*  (+ *start-run-offset* 1))))
	   (iup:button "Up     ^" #:action (lambda (obj)(set! *start-test-offset* (if (> *start-test-offset* 0)(- *start-test-offset* 1) 0))))
	   (iup:button "Down   v" #:action (lambda (obj)(set! *start-test-offset* (if (>= *start-test-offset* (length *alltestnamelst*))(length *alltestnamelst*)(+ *start-test-offset* 1)))))
	   (iup:button "Right ->" #:action (lambda (obj)(set! *start-run-offset*  (if (> *start-run-offset* 0)(- *start-run-offset* 1) 0))))
	   ;(iup:button "inc rows" #:action (lambda (obj)(set! *num-tests* (+ *num-tests* 1))))
	   ;(iup:button "dec rows" #:action (lambda (obj)(set! *num-tests* (if (> *num-tests* 0)(- *num-tests* 1) 0))))
	   )
	  )
    
    ;; create the left most column for the run key names and the test names 
    (set! lftlst (list (apply iup:vbox 
			      (map (lambda (x)		
				     (let ((res (iup:hbox
						 (iup:label x #:size "40x15" #:fontsize "10") ;;  #:expand "HORIZONTAL")
						 (iup:textbox #:size "60x15" #:fontsize "10" #:value "%" ;; #:expand "HORIZONTAL"
							      #:action (lambda (obj unk val)
									 (update-search x val))))))
				       (set! i (+ i 1))
				       res))
				   keynames))))
    (let loop ((testnum  0)
	       (res      '()))
      (cond
       ((>= testnum ntests)
	;; now lftlst will be an hbox with the test keys and the test name labels
	(set! lftlst (append lftlst (list (apply iup:vbox (reverse res))))))
       (else
	(let ((labl  (iup:button "" 
				 #:flat "YES" 
				 ; #:image img1
				 ; #:impress img2
				 #:size "100x15"
				 #:fontsize "10"
				 #:action (lambda (obj)
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
	(let ((labl  (iup:label "" #:size "60x15" #:fontsize "10"))) ;; #:expand "HORIZONTAL"
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
				       ;; #:expand "HORIZONTAL"
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
      #:title "Megatest dashboard"
      (iup:vbox
	(apply iup:hbox 
	       (cons (apply iup:vbox lftlst)
		     (list 
		      (iup:vbox
		       ;; the header
		       (apply iup:hbox (reverse hdrlst))
		       (apply iup:hbox (reverse bdylst))))))
       controls)))
    (vector lftcol header runsvec)))

(if (or (args:get-arg "-rows")
	(get-environment-variable "DASHBOARDROWS" ))
    (begin
        (set! *num-tests* (string->number (or (args:get-arg "-rows")
					      (get-environment-variable "DASHBOARDROWS"))))
	(update-rundat "%" *num-runs* "%" "%"))
    (set! *num-tests* (min (max (update-rundat "%" *num-runs* "%" "%") 8) 20)))

(define (run-update mtx1)
  (let loop ((i 0))
    (thread-sleep! 0.05)
    (mutex-lock! mtx1)
    (update-buttons uidat *num-runs* *num-tests*)
    (mutex-unlock! mtx1)
    (iup:main-loop-flush)
    (mutex-lock! mtx1)
    (update-rundat (hash-table-ref/default *searchpatts* "runname" "%") *num-runs*
		   (hash-table-ref/default *searchpatts* "test-name" "%")
		   (hash-table-ref/default *searchpatts* "item-name" "%"))
    (mutex-unlock! mtx1)
    (loop i)))

(define *job* #f)

(cond 
 ((args:get-arg "-run")
  (let ((runid (string->number (args:get-arg "-run"))))
    (if runid
	(set! *job* (lambda (mx1)
		      (on-exit (lambda ()
				 (sqlite3:finalize! *db*)))
		      (examine-run *db* runid)))
	(begin
	  (print "ERROR: runid is not a number " (args:get-arg "-run"))
	  (exit 1)))))
 ((args:get-arg "-test")
    (let ((testid (string->number (args:get-arg "-test"))))
    (if testid
	(set! *job* (lambda (mx1)
		      (examine-test *db* testid mx1)))
	(begin
	  (print "ERROR: testid is not a number " (args:get-arg "-test"))
	  (exit 1)))))
 (else
  (set! uidat (make-dashboard-buttons *num-runs* *num-tests* dbkeys))
  (set! *job* (lambda (mtx1)(run-update mtx1)))))


(let* ((mx1 (make-mutex))
       (th2 (make-thread iup:main-loop))
       (th1 (make-thread (*job* mx1))))
  (thread-start! th1)
  (thread-start! th2)
  (thread-join! th2))
