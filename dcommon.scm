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

(use format)
(require-library iup)
(import (prefix iup iup:))
(use canvas-draw)
(use regex)

(declare (unit dcommon))

(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses synchash))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")

;; yes, this is non-ideal 
(define dashboard:update-summary-tab #f)
(define dashboard:update-servers-table #f)

;;======================================================================
;; C O M M O N   D A T A   S T R U C T U R E
;;======================================================================
;; 
;; A single data structure for all the data used in a dashboard for
;; all areas tracked.
;;


(define-record dboard:data
  cfgdat            ;; data from ~/.megatest/<group>.dat
  areas             ;; hash of areaname -> area-rec
  current-window-id
  )

(define-record dboard:area
  tree
  matrix
  read-only ;; #t => can't write
  area-dat  ;; the one-structure (one day dbstruct will be put in here)
  name      ;; name for this area
  mpath     ;; path to the megatest home (MT_RUN_AREA_HOME)
  view-path ;; <target/path>/<runname>/...
  view-type ;; standard, etc.
  matrix    ;; the spreadsheet 
  controls  ;; the controls
  data      ;; all the data kept in sync with db
  filters   ;; user filters 
  run-id    ;; the current run-id
  test-ids  ;; the current test id hash, run-id => test-id
  command   ;; the command from the entry field
  ;; dbstruct ;; not needed
  )

(define-record dboard:filter
  target    ;; hash of widgets for the target
  runname   ;; the runname widget
  testpatt  ;; the testpatt widget
  )

;; Use megatest:area from common.scm for an area record

;;======================================================================
;; D O T F I L E
;;======================================================================

;; write a sexp list to fname
;;
(define (dcommon:write-dotfile fname dat)
  (with-output-to-file fname
    (lambda ()
      (pp dat))))

(define (dcommon:read-dotfile fname)
  (if (file-exists? fname)
      (with-input-from-file fname
	(lambda ()
	  (read)))
      '()))       

;; gets the name for the file ~/.megatest/<name>
;; creates .megatest dir if not there
;;
(define (dcommon:get-dot-file-pathn name)
  (let* ((dot-dir (conc (get-environment-variable "HOME") "/.megatest"))
	 (dfile   (conc dot-dir "/" name)))
    (if (not (file-exists? dot-dir))
	(create-directory dot-dir))
    dfile))

;; dat is the top level data stucture that contains all the info being 
;; displayed in all runs etc.
;;
(define (dcommon:dotfiles-save-areas data)
  (let* ((areas-dat   (dcommon:data-get-areas data))
	 (areas-dfile (dcommon:get-dot-file-pathn "areas")))
    (dcommon:write-dotfile areas-dfile areas-dat)))

;; returns alist of area => path
;;
(define (dcommon:data-get-areas data)
  (let ((area-names (hash-table-keys data)))
    (map (lambda (area-name)
	   (cons area-name 
		 (dboard:data-get-area-path (hash-table-ref data area-name))))
	 area-names)))

;; Fill the hash table data with area => area-record
;;
(define (dcommon:read-areas-init-data data)
  (let* ((dfile       (dcommon:get-dot-file-pathn "areas"))
	 (areas-dfile (dcommon:read-dotfile dfile)))
    (for-each
     (lambda (area)
       (let ((rec (vector 25 #f)))
	 (dboard:data-set-area-path! rec (cdr area))
	 (dboard:data-set-updaters!  rec (make-hash-table))
	 (hash-table-set! data (car area) rec)))
     areas-dfile)))

;;======================================================================
;; TARGET AND PATTERN MANIPULATIONS
;;======================================================================

;; Convert to and from list of lines (for a text box)
;; "," => "\n"
(define (dboard:test-patt->lines test-patt)
  (string-substitute (regexp ",") "\n" test-patt))

(define (dboard:lines->test-patt lines)
  (string-substitute (regexp "\n") "," lines #t))

;;======================================================================
;; P R O C E S S   R U N S
;;======================================================================

;; TO-DO
;;  1. Make "data" hash-table hierarchial store of all displayed data
;;  2. Update synchash to understand "get-runs", "get-tests" etc.
;;  3. Add extraction of filters to synchash calls
;;
;; Mode is 'full or 'incremental for full refresh or incremental refresh
(define (dcommon:run-update data)
  (thread-sleep! 0.25))

;;  (let* (;; count and offset => #f so not used
;; 	 ;; the synchash calls modify the "data" hash
;; 	 (get-runs-sig    (conc (client:get-signature) " get-runs"))
;; 	 (get-tests-sig   (conc (client:get-signature) " get-tests"))
;; 	 (get-details-sig (conc (client:get-signature) " get-test-details"))
;; 
;; 	 ;; test-ids to get and display are indexed on window-id in curr-test-ids hash
;; 	 (test-ids        (hash-table-values (dboard:data-get-curr-test-ids *data*)))
;; 	 ;; run-id is #f in next line to send the query to server 0
;;  	 (run-changes     (synchash:client-get *area-dat* 'db:get-runs get-runs-sig (length keypatts) data #f runname #f #f keypatts))
;; 	 (tests-detail-changes (if (not (null? test-ids))
;; 				   (synchash:client-get *area-dat* 'db:get-test-info-by-ids get-details-sig 0  data #f test-ids)
;; 				   '()))
;; 
;; 	 ;; Now can calculate the run-ids
;; 	 (run-hash    (hash-table-ref/default data get-runs-sig #f))
;; 	 (run-ids     (if run-hash (filter number? (hash-table-keys run-hash)) '()))
;; 
;; 	 (all-test-changes (let ((res (make-hash-table)))
;; 			     (for-each (lambda (run-id)
;; 					 (if (> run-id 0)
;; 					     (hash-table-set! res run-id (synchash:client-get *area-dat* 'db:get-tests-for-run-mindata get-tests-sig 0 data run-id 1 testpatt states statuses #f))))
;; 				       run-ids)
;; 			     res))
;; 	 (runs-hash    (hash-table-ref/default data get-runs-sig #f))
;; 	 (header       (hash-table-ref/default runs-hash "header" #f))
;; 	 (run-ids      (sort (filter number? (hash-table-keys runs-hash))
;; 			     (lambda (a b)
;; 			       (let* ((record-a (hash-table-ref runs-hash a))
;; 				      (record-b (hash-table-ref runs-hash b))
;; 				      (time-a   (db:get-value-by-header record-a header "event_time"))
;; 				      (time-b   (db:get-value-by-header record-b header "event_time")))
;; 				 (> time-a time-b)))
;; 			     ))
;; 	 (runid-to-col    (hash-table-ref *cachedata* "runid-to-col"))
;; 	 (testname-to-row (hash-table-ref *cachedata* "testname-to-row")) 
;; 	 (colnum       1)
;; 	 (rownum       0)) ;; rownum = 0 is the header
;; ;; (debug:print 0 "test-ids " test-ids ", tests-detail-changes " tests-detail-changes)
;;     
;; 	 ;; tests related stuff
;; 	 ;; (all-testnames (delete-duplicates (map db:test-get-testname test-changes))))
;; 
;;     ;; Given a run-id and testname/item_path calculate a cell R:C
;; 
;;     ;; NOTE: Also build the test tree browser and look up table
;;     ;;
;;     ;; Each run is unique on its keys and runname or run-id, store in hash on colnum
;;     (for-each (lambda (run-id)
;; 		(let* ((run-record (hash-table-ref/default runs-hash run-id #f))
;; 		       (key-vals   (map (lambda (key)(db:get-value-by-header run-record header key))
;; 					keys))
;; 		       (run-name   (db:get-value-by-header run-record header "runname"))
;; 		       (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
;; 		       (run-path   (append key-vals (list run-name))))
;; 		  (hash-table-set! (dboard:data-get-run-keys *data*) run-id run-path)
;; 		  (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
;; 				      (conc rownum ":" colnum) col-name)
;; 		  (hash-table-set! runid-to-col run-id (list colnum run-record))
;; 		  ;; Here we update the tests treebox and tree keys
;; 		  (tree:add-node (dboard:data-get-tests-tree *data*) "Runs" (append key-vals (list run-name))
;; 				 userdata: (conc "run-id: " run-id))
;; 		  (set! colnum (+ colnum 1))))
;; 	      run-ids)
;; 
;;     ;; Scan all tests to be displayed and organise all the test names, respecting what is in the hash table
;;     ;; Do this analysis in the order of the run-ids, the most recent run wins
;;     (for-each (lambda (run-id)
;; 		(let* ((run-path       (hash-table-ref (dboard:data-get-run-keys *data*) run-id))
;; 		       (test-changes   (hash-table-ref all-test-changes run-id))
;; 		       (new-test-dat   (car test-changes))
;; 		       (removed-tests  (cadr test-changes))
;; 		       (tests          (sort (map cadr (filter (lambda (testrec)
;; 								 (eq? run-id (db:mintest-get-run_id (cadr testrec))))
;; 							       new-test-dat))
;; 					     (lambda (a b)
;; 					       (let ((time-a (db:mintest-get-event_time a))
;; 						     (time-b (db:mintest-get-event_time b)))
;; 						 (> time-a time-b)))))
;; 		       ;; test-changes is a list of (( id record ) ... )
;; 		       ;; Get list of test names sorted by time, remove tests
;; 		       (test-names (delete-duplicates (map (lambda (t)
;; 							     (let ((i (db:mintest-get-item_path t))
;; 								   (n (db:mintest-get-testname  t)))
;; 							       (if (string=? i "")
;; 								   (conc "   " i)
;; 								   n)))
;; 							   tests)))
;; 		       (colnum     (car (hash-table-ref runid-to-col run-id))))
;; 		  ;; for each test name get the slot if it exists and fill in the cell
;; 		  ;; or take the next slot and fill in the cell, deal with items in the
;; 		  ;; run view panel? The run view panel can have a tree selector for
;; 		  ;; browsing the tests/items
;; 
;; 		  ;; SWITCH THIS TO USING CHANGED TESTS ONLY
;; 		  (for-each (lambda (test)
;; 			      (let* ((test-id   (db:mintest-get-id test))
;; 				     (state     (db:mintest-get-state test))
;; 				     (status    (db:mintest-get-status test))
;; 				     (testname  (db:mintest-get-testname test))
;; 				     (itempath  (db:mintest-get-item_path test))
;; 				     (fullname  (conc testname "/" itempath))
;; 				     (dispname  (if (string=? itempath "") testname (conc "   " itempath)))
;; 				     (rownum    (hash-table-ref/default testname-to-row fullname #f))
;; 				     (test-path (append run-path (if (equal? itempath "") 
;; 								     (list testname)
;; 								     (list testname itempath))))
;; 				     (tb         (dboard:data-get-tests-tree *data*)))
;; 				(print "INFONOTE: run-path: " run-path)
;; 				(tree:add-node (dboard:data-get-tests-tree *data*) "Runs" 
;; 					       test-path
;; 					       userdata: (conc "test-id: " test-id))
;; 				(let ((node-num (tree:find-node tb (cons "Runs" test-path)))
;; 				      (color    (car (gutils:get-color-for-state-status state status))))
;; 				  (debug:print 0 "node-num: " node-num ", color: " color)
;; 				  (iup:attribute-set! tb (conc "COLOR" node-num) color))
;; 				(hash-table-set! (dboard:data-get-path-test-ids *data*) test-path test-id)
;; 				(if (not rownum)
;; 				    (let ((rownums (hash-table-values testname-to-row)))
;; 				      (set! rownum (if (null? rownums)
;; 						       1
;; 						       (+ 1 (apply max rownums))))
;; 				      (hash-table-set! testname-to-row fullname rownum)
;; 				      ;; create the label
;; 				      (iup:attribute-set! (dboard:data-get-runs-matrix *data*)
;; 							  (conc rownum ":" 0) dispname)
;; 				      ))
;; 				;; set the cell text and color
;; 				;; (debug:print 2 "rownum:colnum=" rownum ":" colnum ", state=" status)
;; 				(iup:attribute-set! (dboard:data-get-runs-matrix *data*)
;; 						    (conc rownum ":" colnum)
;; 						    (if (member state '("ARCHIVED" "COMPLETED"))
;; 							status
;; 							state))
;; 				(iup:attribute-set! (dboard:data-get-runs-matrix *data*)
;; 						    (conc "BGCOLOR" rownum ":" colnum)
;; 						    (car (gutils:get-color-for-state-status state status)))
;; 				))
;; 			    tests)))
;; 	      run-ids)
;; 
;;     (let ((updater (hash-table-ref/default  (dboard:data-get-updaters *data*) window-id #f)))
;;       (if updater (updater (hash-table-ref/default data get-details-sig #f))))
;; 
;;     (iup:attribute-set! (dboard:data-get-runs-matrix *data*) "REDRAW" "ALL")
;;     ;; (debug:print 2 "run-changes: " run-changes)
;;     ;; (debug:print 2 "test-changes: " test-changes)
;;     (list run-changes all-test-changes)))

;;======================================================================
;; TESTS DATA
;;======================================================================

;; Produce a list of lists ready for common:sparse-list-generate-index
;;
(define (dcommon:minimize-test-data tests-dat)
  (if (null? tests-dat) 
      '()
      (let loop ((hed (car tests-dat))
		 (tal (cdr tests-dat))
		 (res '()))
	(let* ((test-id    (vector-ref hed 0)) ;; look at the tests-dat spec for locations
	       (test-name  (vector-ref hed 1))
	       (item-path  (vector-ref hed 2))
	       (state      (vector-ref hed 3))
	       (status     (vector-ref hed 4))
	       (newitem    (list test-name item-path (list test-id state status))))
	  (if (null? tal)
	      (reverse (cons newitem res))
	      (loop (car tal)(cdr tal)(cons newitem res)))))))
	  

;;======================================================================
;; D A T A   T A B L E S
;;======================================================================

;; Table of keys
(define (dcommon:keys-matrix rawconfig)
  (let* ((curr-row-num 1)
	 (key-vals     (configf:section-vars rawconfig "fields"))
	 (keys-matrix  (iup:matrix
			#:alignment1 "ALEFT"
			#:expand "YES" ;; "HORIZONTAL" ;; "VERTICAL"
			;; #:scrollbar "YES"
			#:numcol 1
			#:numlin (length key-vals)
			#:numcol-visible 1
			#:numlin-visible (length key-vals)
			#:click-cb (lambda (obj lin col status)
				     (print "obj: " obj " lin: " lin " col: " col " status: " status)))))
    ;; (iup:attribute-set! keys-matrix "0:0" "Run Keys")
    (iup:attribute-set! keys-matrix "WIDTH0" 0)
    (iup:attribute-set! keys-matrix "0:1" "Key Name")
    ;; (iup:attribute-set! keys-matrix "WIDTH1" "100")
    ;; fill in keys
    (for-each 
     (lambda (var)
       ;; (iup:attribute-set! keys-matrix "ADDLIN" (conc curr-row-num))
       (iup:attribute-set! keys-matrix (conc curr-row-num ":0") curr-row-num)
       (iup:attribute-set! keys-matrix (conc curr-row-num ":1") var)
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup configdat "fields" var)))
     key-vals)
    (iup:attribute-set! keys-matrix "WIDTHDEF" "40")
    keys-matrix))

;; Section to table
(define (dcommon:section-matrix rawconfig sectionname varcolname valcolname #!key (title #f))
  (let* ((curr-row-num    1)
	 (key-vals        (configf:section-vars rawconfig sectionname))
	 (section-matrix  (iup:matrix
			   #:alignment1 "ALEFT"
			   #:expand "YES" ;; "HORIZONTAL"
			   #:numcol 1
			   #:numlin (length key-vals)
			   #:numcol-visible 1
			   #:numlin-visible (length key-vals)
			   #:scrollbar "YES")))
    (iup:attribute-set! section-matrix "0:0" varcolname)
    (iup:attribute-set! section-matrix "0:1" valcolname)
    (iup:attribute-set! section-matrix "WIDTH1" "200")
    ;; fill in keys
    (for-each 
     (lambda (var)
       ;; (iup:attribute-set! keys-matrix "ADDLIN" (conc curr-row-num))
       (iup:attribute-set! section-matrix (conc curr-row-num ":0") var)
       (iup:attribute-set! section-matrix (conc curr-row-num ":1") (configf:lookup rawconfig sectionname var))
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup configdat "fields" var)))
     key-vals)
    (iup:vbox
     (iup:label (if title title (conc "Settings from [" sectionname "]"))  
         	;; #:size   "5x"
         	#:expand "HORIZONTAL"
         	)
     section-matrix)))
    
;; General data
;;
(define (dcommon:general-info)
  (let ((general-matrix (iup:matrix
			 #:alignment1 "ALEFT"
			 #:expand "YES" ;; "HORIZONTAL"
			 #:numcol 1
			 #:numlin 2
			 #:numcol-visible 1
			 #:numlin-visible 2)))
    (iup:attribute-set! general-matrix "WIDTH1" "150")
    (iup:attribute-set! general-matrix "0:1" "About this Megatest area") 
    ;; User (this is not always obvious - it is common to run as a different user
    (iup:attribute-set! general-matrix "1:0" "User")
    (iup:attribute-set! general-matrix "1:1" (current-user-name))
    ;; Megatest area
    ;; (iup:attribute-set! general-matrix "2:0" "Area")
    ;; (iup:attribute-set! general-matrix "2:1" toppath)
    ;; Megatest version
    (iup:attribute-set! general-matrix "2:0" "Version")
    (iup:attribute-set! general-matrix "2:1" (conc megatest-version "-" (substring megatest-fossil-hash 0 4)))

    general-matrix))

(define (dcommon:run-stats dbstruct)
  (let* ((stats-matrix (iup:matrix expand: "YES"))
	 (changed      #f)
	 (updater      (lambda ()
			 (let* ((run-stats    (db:get-run-stats dbstruct))
				(indices      (common:sparse-list-generate-index run-stats)) ;;  proc: set-cell))
				(row-indices  (car indices))
				(col-indices  (cadr indices))
				(max-row      (if (null? row-indices) 1 (apply max (map cadr row-indices))))
				(max-col      (if (null? col-indices) 1 
						  (apply max (map cadr col-indices))))
				(max-visible  (max (- *num-tests* 15) 3))
				(max-col-vis  (if (> max-col 10) 10 max-col))
				(numrows      1)
				(numcols      1))
			   (iup:attribute-set! stats-matrix "CLEARVALUE" "CONTENTS")
			   (iup:attribute-set! stats-matrix "NUMCOL" max-col )
			   (iup:attribute-set! stats-matrix "NUMLIN" (if (< max-row max-visible) max-visible max-row)) ;; min of 20
			   (iup:attribute-set! stats-matrix "NUMCOL_VISIBLE" max-col-vis)
			   (iup:attribute-set! stats-matrix "NUMLIN_VISIBLE" (if (> max-row max-visible) max-visible max-row))

			   ;; Row labels
			   (for-each (lambda (ind)
				       (let* ((name (car ind))
					      (num  (cadr ind))
					      (key  (conc num ":0")))
					 (if (not (equal? (iup:attribute stats-matrix key) name))
					     (begin
					       (set! changed #t)
					       (iup:attribute-set! stats-matrix key name)))))
				     row-indices)

			   ;; Col labels
			   (for-each (lambda (ind)
				       (let* ((name (car ind))
					      (num  (cadr ind))
					      (key  (conc "0:" num)))
					 (if (not (equal? (iup:attribute stats-matrix key) name))
					     (begin
					       (set! changed #t)
					       (iup:attribute-set! stats-matrix key name)))))
				     col-indices)

			   ;; Cell contents
			   (for-each (lambda (entry)
				       (let* ((row-name (car entry))
					      (col-name (cadr entry))
					      (value    (caddr entry))
					      (row-num  (cadr (assoc row-name row-indices)))
					      (col-num  (cadr (assoc col-name col-indices)))
					      (key      (conc row-num ":" col-num)))
					 (if (not (equal? (iup:attribute stats-matrix key) value))
					     (begin
					       (set! changed #t)
					       (iup:attribute-set! stats-matrix key value)))))
				     run-stats)
			   (if changed (iup:attribute-set! stats-matrix "REDRAW" "ALL"))))))
    (updater)
    (set! dashboard:update-summary-tab updater)
    (iup:attribute-set! stats-matrix "WIDTHDEF" "40")
    (iup:vbox
     ;; (iup:label "Run statistics"  #:expand "HORIZONTAL")
     stats-matrix)))

(define (dcommon:servers-table area-dat)
  (let* ((tdbdat         (tasks:open-db area-dat))
	 (colnum         0)
	 (rownum         0)
	 (servers-matrix (iup:matrix #:expand "YES"
				     #:numcol 7
				     #:numcol-visible 7
				     #:numlin-visible 5
				     ))
	 (colnames       (list "Id" "MTver" "Pid" "Host" "Interface:OutPort" "RunTime" "State" "RunId"))
	 (updater        (lambda ()
			   (let ((servers (tasks:get-all-servers (db:delay-if-busy tdbdat area-dat))))
			     (iup:attribute-set! servers-matrix "NUMLIN" (length servers))
			     ;; (set! colnum 0)
			     ;; (for-each (lambda (colname)
			     ;;    	 ;; (print "colnum: " colnum " colname: " colname)
			     ;;    	 (iup:attribute-set! servers-matrix (conc "0:" colnum) colname)
			     ;;    	 (set! colnum (+ 1 colnum)))
			     ;;           colnames)
			     (set! rownum 1)
			     (for-each 
			      (lambda (server)
				(set! colnum 0)
				(let* ((vals (list (vector-ref server 0) ;; Id
						   (vector-ref server 9) ;; MT-Ver
						   (vector-ref server 1) ;; Pid
						   (vector-ref server 2) ;; Hostname
						   (conc (vector-ref server 3) ":" (vector-ref server 4)) ;; IP:Port
						   (seconds->hr-min-sec (- (current-seconds)(vector-ref server 6)))
						   ;; (vector-ref server 5) ;; Pubport
						   ;; (vector-ref server 10) ;; Last beat
						   ;; (vector-ref server 6) ;; Start time
						   ;; (vector-ref server 7) ;; Priority
						   ;; (vector-ref server 8) ;; State
						   (vector-ref server 8) ;; State
						   (vector-ref server 12)  ;; RunId
						   )))
				  (for-each (lambda (val)
					      (let* ((row-col (conc rownum ":" colnum))
						     (curr-val (iup:attribute servers-matrix row-col)))
						(if (not (equal? (conc val) curr-val))
						    (begin
						      (iup:attribute-set! servers-matrix row-col val)
						      (iup:attribute-set! servers-matrix "FITTOTEXT" (conc "C" colnum))))
						(set! colnum (+ 1 colnum))))
					    vals)
				  (set! rownum (+ rownum 1)))
				 (iup:attribute-set! servers-matrix "REDRAW" "ALL"))
			      servers)))))
    (set! colnum 0)
    (for-each (lambda (colname)
		(iup:attribute-set! servers-matrix (conc "0:" colnum) colname)
		(iup:attribute-set! servers-matrix "FITTOTEXT" (conc "C" colnum))
		(set! colnum (+ colnum 1)))
	      colnames)
    (set! dashboard:update-servers-table updater) 
    ;; (iup:attribute-set! servers-matrix "WIDTHDEF" "40")
   ;;  (iup:hbox
   ;;   (iup:vbox
   ;;    (iup:button "Start"
   ;;      	  ;; #:size "50x"
   ;;      	  #:expand "YES"
   ;;      	  #:action (lambda (obj)
   ;;      		     (let ((cmd (conc ;; "xterm -geometry 180x20 -e \""
   ;;      				      "megatest -server - &")))
   ;;      				      ;; ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))
   ;;      		       (system cmd))))
   ;;    (iup:button "Stop"
   ;;      	  #:expand "YES"
   ;;      	  ;; #:size "50x"
   ;;      	  #:action (lambda (obj)
   ;;      		     (let ((cmd (conc ;; "xterm -geometry 180x20 -e \""
   ;;      				      "megatest -stop-server 0 &")))
   ;;      				      ;; ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))
   ;;      		       (system cmd))))
   ;;    (iup:button "Restart"
   ;;      	  #:expand "YES"
   ;;      	  ;; #:size "50x"
   ;;      	  #:action (lambda (obj)
   ;;      		     (let ((cmd (conc ;; "xterm -geometry 180x20 -e \""
   ;;      				      "megatest -stop-server 0;megatest -server - &")))
   ;;      				      ;; ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))
   ;;      		       (system cmd)))))
   ;;    servers-matrix
   ;;   )))
    servers-matrix
    ))

;; The main menu
(define (dcommon:main-menu data)
  (iup:menu ;; a menu is a special attribute to a dialog (think Gnome putting the menu at screen top)
   (iup:menu-item "Files" (iup:menu   ;; Note that you can use either #:action or action: for options
			   (iup:menu-item "Open"  action: (lambda (obj)
							    (let* ((area-name (iup:textbox #:expand "HORIZONTAL"))
								   (fd        (iup:file-dialog #:dialogtype "DIR"))
								   (top       (iup:show fd #:modal? "YES")))
							      (iup:attribute-set! source-tb "VALUE"
										  (iup:attribute fd "VALUE"))
							      (iup:destroy! fd))))
			   ;; (lambda (obj)
			   ;;  (iup:show (iup:file-dialog))
			   ;;  (print "File->open " obj)))
			   (iup:menu-item "Save"  #:action (lambda (obj)(print "File->save " obj)))
			   (iup:menu-item "Exit"  #:action (lambda (obj)(exit)))))
   (iup:menu-item "Tools" (iup:menu
			   (iup:menu-item "Create new blah" #:action (lambda (obj)(print "Tools->new blah")))
			   ;; (iup:menu-item "Show dialog"     #:action (lambda (obj)
			   ;;  					   (show message-window
			   ;;  					     #:modal? #t
			   ;;  					     ;; set positon using coordinates or center, start, top, left, end, bottom, right, parent-center, current
			   ;;  					     ;; #:x 'mouse
			   ;;  					     ;; #:y 'mouse
			   ;;  )					     
			   ))))

;;======================================================================
;; CANVAS STUFF FOR TESTS
;;======================================================================

(define (dcommon:draw-test cnv x y w h name selected)
  (let* ((llx x)
	 (lly y)
	 (urx (+ x w))
	 (ury (+ y h)))
    (canvas-text! cnv (+ llx 5)(+ lly 5) name) ;; (conc testname " (" xtorig "," ytorig ")"))
    (canvas-rectangle! cnv llx urx lly ury)
    (if selected (canvas-box! cnv llx (+ llx 5) lly (+ lly 5)))))

(define (dcommon:initial-draw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames)
      (let* ((scalef (hash-table-ref/default tests-draw-state 'scalef 8))
	     (test-browse-xoffset (hash-table-ref tests-draw-state 'test-browse-xoffset))
	     (test-browse-yoffset (hash-table-ref tests-draw-state 'test-browse-yoffset))
	     (xtorig (+ test-browse-xoffset (* (/ sizex 2) scalef (- 0.5 xadj)))) ;;  (- xadj 1))))
	     (ytorig (+ test-browse-yoffset (* (/ sizey 2) scalef (- yadj 0.5))))
	     (boxw   90) ;; default, overriden by length estimate below
	     (boxh   25)
	     (gapx   20)
	     (gapy   30)
	     (tests-hash     (hash-table-ref tests-draw-state 'tests-info))
	     (selected-tests (hash-table-ref tests-draw-state 'selected-tests )))
	(hash-table-set! tests-draw-state 'xtorig xtorig)
	(hash-table-set! tests-draw-state 'ytorig ytorig)
	(let ((longest-str   (if (null? sorted-testnames) "         " (car (sort sorted-testnames (lambda (a b)(>= (string-length a)(string-length b))))))))
	  (let-values (((x-max y-max) (canvas-text-size cnv longest-str)))
             (if (> x-max boxw)(set! boxw (+ 10 x-max)))))
	;; (print "sizex: " sizex " sizey: " sizey " font: " (canvas-font cnv) " originx: " originx " originy: " originy " xtorig: " xtorig " ytorig: " ytorig " xadj: " xadj " yadj: " yadj)
	(if (not (null? sorted-testnames))
	    (let loop ((hed (car (reverse sorted-testnames)))
		       (tal (cdr (reverse sorted-testnames)))
		       (llx xtorig)
		       (lly ytorig)
		       (urx (+ xtorig boxw))
		       (ury (+ ytorig boxh)))
					; (print "hed " hed " llx " llx " lly " lly " urx " urx " ury " ury)
	      (dcommon:draw-test cnv llx lly boxw boxh hed (hash-table-ref/default selected-tests hed #f))
	      ;; data used by mouse click calc. keep the wacky order for now.
	      (hash-table-set! tests-hash hed  (list llx urx (- sizey ury)(- sizey lly) lly boxw boxh)) 
	      ;; (list llx lly boxw boxh)) ;; NB// Swap ury and lly
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

(define (dcommon:redraw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames)
  (let* ((scalef (hash-table-ref/default tests-draw-state 'scalef 8))
	 (test-browse-xoffset (hash-table-ref tests-draw-state 'test-browse-xoffset))
	 (test-browse-yoffset (hash-table-ref tests-draw-state 'test-browse-yoffset))
	 (xtorig (+ test-browse-xoffset (* (/ sizex 2) scalef (- 0.5 xadj)))) ;;  (- xadj 1))))
	 (ytorig (+ test-browse-yoffset (* (/ sizey 2) scalef (- yadj 0.5))))
	 (xdelta (- (hash-table-ref tests-draw-state 'xtorig) xtorig))
	 (ydelta (- (hash-table-ref tests-draw-state 'ytorig) ytorig))
	 (tests-hash     (hash-table-ref tests-draw-state 'tests-info))
	 (selected-tests (hash-table-ref tests-draw-state 'selected-tests )))
    (hash-table-set! tests-draw-state 'xtorig xtorig)
    (hash-table-set! tests-draw-state 'ytorig ytorig)
    (if (not (null? sorted-testnames))
	(let loop ((hed (car (reverse sorted-testnames)))
		   (tal (cdr (reverse sorted-testnames))))
	  (let* ((tvals (hash-table-ref tests-hash hed))
		 (llx   (+ xdelta (list-ref tvals 0)))
		 (lly   (+ ydelta (list-ref tvals 4)))
		 (boxw  (list-ref tvals 5))
		 (boxh  (list-ref tvals 6))
		 (urx   (+ llx boxw))
		 (ury   (+ lly boxh)))
	    (dcommon:draw-test cnv llx lly boxw boxh hed (hash-table-ref/default selected-tests hed #f))
	    (hash-table-set! tests-hash hed (list llx urx (- sizey ury)(- sizey lly) lly boxw boxh))
	    (if (not (null? tal))
		;; leave a column of space to the right to list items
		(loop (car tal)
		      (cdr tal))))))))

;;======================================================================
;;  S T E P S
;;======================================================================

(define (dcommon:populate-steps teststeps steps-matrix)
  (let ((max-row 0))
    (if (null? teststeps)
	(iup:attribute-set! steps-matrix "CLEARVALUE" "CONTENTS")
	(let loop ((hed    (car teststeps))
		   (tal    (cdr teststeps))
		   (rownum 1)
		   (colnum 1))
	  (if (> rownum max-row)(set! max-row rownum))
	  (let ((val     (vector-ref hed (- colnum 1)))
		(mtrx-rc (conc rownum ":" colnum)))
	    (iup:attribute-set! steps-matrix  mtrx-rc (if val (conc val) ""))
	    (if (< colnum 6)
		(loop hed tal rownum (+ colnum 1))
		(if (not (null? tal))
		    (loop (car tal)(cdr tal)(+ rownum 1) 1))))))
    (if (> max-row 0)
	(begin
	  ;; we are going to speculatively clear rows until we find a row that is already cleared
	  (let loop ((rownum  (+ max-row 1))
		     (colnum  0)
		     (deleted #f))
	    ;; (debug:print-info 0 "cleaning " rownum ":" colnum)
	    (let* ((next-row (if (eq? colnum 6) (+ rownum 1) rownum))
		   (next-col (if (eq? colnum 6) 1 (+ colnum 1)))
		   (mtrx-rc  (conc rownum ":" colnum))
		   (curr-val (iup:attribute steps-matrix mtrx-rc)))
	      ;; (debug:print-info 0 "cleaning " rownum ":" colnum " currval= " curr-val)
	      (if (and (string? curr-val)
		       (not (equal? curr-val "")))
		  (begin
		    (iup:attribute-set! steps-matrix mtrx-rc "")
		    (loop next-row next-col #t))
		  (if (eq? colnum 6) ;; not done, didn't get a full blank row
		      (if deleted (loop next-row next-col #f)) ;; exit on this not met
		      (loop next-row next-col deleted)))))
	  (iup:attribute-set! steps-matrix "REDRAW" "ALL")))))
