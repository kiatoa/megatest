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
(import canvas-draw-iup)
(use regex typed-records matchable)

(declare (unit dcommon))

(declare (uses megatest-version))
(declare (uses gutils))
(declare (uses db))
(declare (uses synchash))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

;; yes, this is non-ideal 
(define dashboard:update-summary-tab #f)
(define dashboard:update-servers-table #f)

;;======================================================================
;; C O M M O N   D A T A   S T R U C T U R E
;;======================================================================
;; 
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
  ((numruns          (string->number (or (args:get-arg "-cols") "10")))                 : number)      ;; 
  ((tot-runs          0)                 : number)
  ((last-data-update  0)                 : number)      ;; last time the data in allruns was updated
  ((last-runs-update  0)                 : number)      ;; last time we pulled the runs info to update the tree
  (runs-mutex         (make-mutex))                     ;; use to prevent parallel access to draw objects
  ((run-update-times  (make-hash-table)) : hash-table)  ;; update times indexed by run-id
  ((last-test-dat      (make-hash-table)) : hash-table)  ;; cache last tests dat by run-id
  ((run-db-paths      (make-hash-table)) : hash-table)  ;; cache the paths to the run db files

  ;; Runs view
  ((buttondat         (make-hash-table)) : hash-table)  ;;     
  ((item-test-names  '())                : list)        ;; list of itemized tests
  ((run-keys          (make-hash-table)) : hash-table)
  (runs-matrix        #f)                               ;; used in newdashboard
  ((start-run-offset   0)                : number)      ;; left-right slider value
  ((start-test-offset  0)                : number)      ;; up-down slider value
  ((runs-btn-height    (or (configf:lookup *configdat* "dashboard" "btn-height") "x14")) : string)  ;; was 12
  ((runs-btn-fontsz    (or (configf:lookup *configdat* "dashboard" "btn-fontsz") "10")) : string)   ;; was 8
  ((runs-cell-width    (or (configf:lookup *configdat* "dashboard" "cell-width") "60")) : string)   ;; was 50
  ((all-test-names     '())              : list)
  
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

  ;; Run times layout
  ;; (graph-button-box #f) ;; RA => Think it is not referenced anywhere
  (graph-matrix     #f)
  ((graph-matrix-table (make-hash-table)) : hash-table) ;; graph-dats referenced thru graph name info
  ((graph-cell-table (make-hash-table)) : hash-table) ;; graph-dats referenced thru matrix cell info
  ((graph-matrix-row 1) : number)
  ((graph-matrix-col 1) : number)

  ;; Controls used to launch runs etc.
  ((command          "")                 : string)      ;; for run control this is the command being built up
  (command-tb        #f)	                        ;; widget for the type of command; run, remove-runs etc.
  (test-patterns-textbox #f)                            ;; text box widget for editing a list of test patterns
  (key-listboxes     #f)			         
  (key-lbs           #f)			         
  run-name                                              ;; from run name setting widget
  states                                                ;; states for -state s1,s2 ...
  statuses                                              ;; statuses for -status s1,s2 ...
						         
  ;; Selector variables				         
  curr-run-id                                           ;; current row to display in Run summary view
  prev-run-id                                           ;; previous runid selected before current runid was selected (used in xor-two-runs runs summary mode
  curr-test-ids                                         ;; used only in dcommon:run-update which is used in newdashboard
  ((filters-changed  #t)                  : boolean)    ;; to to indicate that the user changed filters for this tab
  ((last-filter-str  "")                  : string)      ;; conc the target runname and testpatt for a signature of changed filters
  ((hide-empty-runs  #f)                  : boolean)     
  ((hide-not-hide    #t)                  : boolean)     ;; toggle for hide/not hide empty runs
  (hide-not-hide-button #f)
  ((searchpatts        (make-hash-table)) : hash-table)  ;;
  ((state-ignore-hash  (make-hash-table)) : hash-table)  ;; hash of  STATE => #t/#f for display control
  ((status-ignore-hash (make-hash-table)) : hash-table)  ;; hash of STATUS => #t/#f
  (target              #f)
  (test-patts          #f)

  ;; db info to file the .db files for the area
  (access-mode        (db:get-access-mode))             ;; use cached db or not
  (dbdir               #f)
  (dbfpath             #f)
  (dbkeys              #f)
  ((last-db-update     (make-hash-table)) : hash-table) ;; last db file timestamp
  (monitor-db-path     #f)                              ;; where to find monitor.db
  ro                                                    ;; is the database read-only?

  ;; tests data
  ((num-tests          10)               : number)      ;; total number of tests to show (used in the old runs display)

  ;; runs tree
  ((path-run-ids       (make-hash-table)) : hash-table) ;; path (target / runname) => id
  (runs-tree           #f)
  ((runs-tree-ht       (make-hash-table)) : hash-table) ;; track which targets added to tree (merge functionality with path-run-ids?)

  ;; tab data
  ((view-changed       #t)                : boolean)   
  ((xadj               0)                 : number)     ;; x slider number (if using canvas)
  ((yadj               0)                 : number)     ;; y slider number (if using canvas)
  ;; runs-summary tab state
  ((runs-summary-modes '((one-run . "Show One Run") (xor-two-runs . "XOR Two Runs") (xor-two-runs-hide-clean . "XOR; Hide Clean")) )   : list)
  ((runs-summary-mode-buttons '())               : list)
  ((runs-summary-mode  'one-run)            : symbol)
  ((runs-summary-mode-change-callbacks '()) : list)
  (runs-summary-source-runname-label #f)
  (runs-summary-dest-runname-label #f)
  ;; runs summary view
  
  tests-tree       ;; used in newdashboard
  )

;;======================================================================
;; D O T F I L E
;;======================================================================

(define (dcommon:write-dotfile fname dat)
  (with-output-to-file fname
    (lambda ()
      (pp dat))))

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

;; MOVE THIS INTO *data*
(define *cachedata* (make-hash-table))
(hash-table-set! *cachedata* "runid-to-col"    (make-hash-table))
(hash-table-set! *cachedata* "testname-to-row" (make-hash-table))

;; modify a cell if the data is changed, return #t or-ed with previous if modified, #f elsewise
;;
(define (dcommon:modifiy-if-different mtrx cell-name new-val prev-changed)
  (let ((curr-val (iup:attribute mtrx cell-name)))
    (if (not (equal? curr-val new-val)) 
	(begin
	  (iup:attribute-set! mtrx cell-name col-name)
	  #t) ;; need a re-draw
	prev-changed)))


;; TO-DO
;;  1. Make "data" hash-table hierarchial store of all displayed data
;;  2. Update synchash to understand "get-runs", "get-tests" etc.
;;  3. Add extraction of filters to synchash calls
;;
;;    NOTE: Used in newdashboard
;;
;; Mode is 'full or 'incremental for full refresh or incremental refresh
(define (dcommon:run-update keys data runname keypatts testpatt states statuses mode window-id)
  (let* (;; count and offset => #f so not used
	 ;; the synchash calls modify the "data" hash
	 (changed         #f)
	 (get-runs-sig    (conc (client:get-signature) " get-runs"))
	 (get-tests-sig   (conc (client:get-signature) " get-tests"))
	 (get-details-sig (conc (client:get-signature) " get-test-details"))

	 ;; test-ids to get and display are indexed on window-id in curr-test-ids hash
	 (test-ids        (hash-table-values (dboard:tabdat-curr-test-ids data)))
	 ;; run-id is #f in next line to send the query to server 0
 	 (run-changes     (synchash:client-get 'db:get-runs get-runs-sig (length keypatts) data #f runname #f #f keypatts))
	 (tests-detail-changes (if (not (null? test-ids))
				   (synchash:client-get 'db:get-test-info-by-ids get-details-sig 0  data #f test-ids)
				   '()))

	 ;; Now can calculate the run-ids
	 (run-hash    (hash-table-ref/default data get-runs-sig #f))
	 (run-ids     (if run-hash (filter number? (hash-table-keys run-hash)) '()))

	 (all-test-changes (let ((res (make-hash-table)))
			     (for-each (lambda (run-id)
					 (if (> run-id 0)
					     (hash-table-set! res run-id (synchash:client-get 'db:get-tests-for-run-mindata get-tests-sig 0 data run-id 1 testpatt states statuses #f))))
				       run-ids)
			     res))
	 (runs-hash    (hash-table-ref/default data get-runs-sig #f))
	 (header       (hash-table-ref/default runs-hash "header" #f))
	 (run-ids      (sort (filter number? (hash-table-keys runs-hash))
			     (lambda (a b)
			       (let* ((record-a (hash-table-ref runs-hash a))
				      (record-b (hash-table-ref runs-hash b))
				      (time-a   (db:get-value-by-header record-a header "event_time"))
				      (time-b   (db:get-value-by-header record-b header "event_time")))
				 (> time-a time-b)))
			     ))
	 (runid-to-col    (hash-table-ref *cachedata* "runid-to-col"))
	 (testname-to-row (hash-table-ref *cachedata* "testname-to-row")) 
	 (colnum       1)
	 (rownum       0)
	 (cellname (conc rownum ":" colnum))) ;; rownum = 0 is the header
;; (debug:print 0 *default-log-port* "test-ids " test-ids ", tests-detail-changes " tests-detail-changes)
    
	 ;; tests related stuff
	 ;; (all-testnames (delete-duplicates (map db:test-get-testname test-changes))))

    ;; Given a run-id and testname/item_path calculate a cell R:C

    ;; NOTE: Also build the test tree browser and look up table
    ;;
    ;; Each run is unique on its keys and runname or run-id, store in hash on colnum
    (for-each (lambda (run-id)
		(let* ((run-record (hash-table-ref/default runs-hash run-id #f))
		       (key-vals   (map (lambda (key)(db:get-value-by-header run-record header key))
					keys))
		       (run-name   (db:get-value-by-header run-record header "runname"))
		       (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
		       (run-path   (append key-vals (list run-name))))
		  (hash-table-set! (dboard:tabdat-run-keys data) run-id run-path)
		  ;; modify cell - but only if changed
		  (set! changed (dcommon:modifiy-if-different (dboard:tabdat-runs-matrix data) cellname col-name changed))
		  (hash-table-set! runid-to-col run-id (list colnum run-record))
		  ;; Here we update the tests treebox and tree keys
		  (tree:add-node (dboard:tabdat-tests-tree data) "Runs" (append key-vals (list run-name))
				 userdata: (conc "run-id: " run-id))
		  (set! colnum (+ colnum 1))))
	      run-ids)

    ;; Scan all tests to be displayed and organise all the test names, respecting what is in the hash table
    ;; Do this analysis in the order of the run-ids, the most recent run wins
    (for-each (lambda (run-id)
		(let* ((run-path       (hash-table-ref (dboard:tabdat-run-keys data) run-id))
		       (test-changes   (hash-table-ref all-test-changes run-id))
		       (new-test-dat   (car test-changes))
		       (removed-tests  (cadr test-changes))
		       (tests          (sort (map cadr (filter (lambda (testrec)
								 (eq? run-id (db:mintest-get-run_id (cadr testrec))))
							       new-test-dat))
					     (lambda (a b)
					       (let ((time-a (db:mintest-get-event_time a))
						     (time-b (db:mintest-get-event_time b)))
						 (> time-a time-b)))))
		       ;; test-changes is a list of (( id record ) ... )
		       ;; Get list of test names sorted by time, remove tests
		       (test-names (delete-duplicates (map (lambda (t)
							     (let ((i (db:mintest-get-item_path t))
								   (n (db:mintest-get-testname  t)))
							       (if (string=? i "")
								   (conc "   " i)
								   n)))
							   tests)))
		       (colnum     (car (hash-table-ref runid-to-col run-id))))
		  ;; for each test name get the slot if it exists and fill in the cell
		  ;; or take the next slot and fill in the cell, deal with items in the
		  ;; run view panel? The run view panel can have a tree selector for
		  ;; browsing the tests/items

		  ;; SWITCH THIS TO USING CHANGED TESTS ONLY
		  (for-each (lambda (test)
			      (let* ((test-id   (db:mintest-get-id test))
				     (state     (db:mintest-get-state test))
				     (status    (db:mintest-get-status test))
				     (testname  (db:mintest-get-testname test))
				     (itempath  (db:mintest-get-item_path test))
				     (fullname  (conc testname "/" itempath))
				     (dispname  (if (string=? itempath "") testname (conc "   " itempath)))
				     (rownum    (hash-table-ref/default testname-to-row fullname #f))
				     (test-path (append run-path (if (equal? itempath "") 
								     (list testname)
								     (list testname itempath))))
				     (tb         (dboard:tabdat-tests-tree data)))
				(print "INFONOTE: run-path: " run-path)
				(tree:add-node (dboard:tabdat-tests-tree data) "Runs" 
					       test-path
					       userdata: (conc "test-id: " test-id))
				(let ((node-num (tree:find-node tb (cons "Runs" test-path)))
				      (color    (car (gutils:get-color-for-state-status state status))))
				  (debug:print 0 *default-log-port* "node-num: " node-num ", color: " color)

				  (set! changed (dcommon:modifiy-if-different 
						 tb
						 (conc "COLOR" node-num)
						 color changed))

				  ;; (iup:attribute-set! tb (conc "COLOR" node-num) color)
				  )
				(hash-table-set! (dboard:tabdat-path-test-ids data) test-path test-id)
				(if (not rownum)
				    (let ((rownums (hash-table-values testname-to-row)))
				      (set! rownum (if (null? rownums)
						       1
						       (+ 1 (common:max rownums))))
				      (hash-table-set! testname-to-row fullname rownum)
				      ;; create the label
				      (set! changed (dcommon:modifiy-if-different 
						     (dboard:tabdat-runs-matrix data)
						     (conc rownum ":" 0)
						     dispname
						     changed))
				      ;; (iup:attribute-set! (dboard:tabdat-runs-matrix data)
				      ;;   		  (conc rownum ":" 0) dispname)
				      ))
				;; set the cell text and color
				;; (debug:print 2 *default-log-port* "rownum:colnum=" rownum ":" colnum ", state=" status)
				(set! changed (dcommon:modifiy-if-different 
						     (dboard:tabdat-runs-matrix data)
						     (conc rownum ":" colnum)
						     (if (member state '("ARCHIVED" "COMPLETED"))
							 status
							 state)
						     changed))
				;; (iup:attribute-set! (dboard:tabdat-runs-matrix data)
				;; 		    (conc rownum ":" colnum)
				;; 		    (if (member state '("ARCHIVED" "COMPLETED"))
				;; 			status
				;; 			state))
				(set! changed (dcommon:modifiy-if-different 
					       (dboard:tabdat-runs-matrix data)
					       (conc "BGCOLOR" rownum ":" colnum)
					       (car (gutils:get-color-for-state-status state status))
					       changed))
				;; (iup:attribute-set! (dboard:tabdat-runs-matrix data)
				;; 		    (conc "BGCOLOR" rownum ":" colnum)
				;; 		    (car (gutils:get-color-for-state-status state status)))
				))
			    tests)))
	      run-ids)

    (let ((updater (hash-table-ref/default  (dboard:commondat-updaters commondat) window-id #f)))
      (if updater (updater (hash-table-ref/default data get-details-sig #f))))

    (if changed (iup:attribute-set! (dboard:tabdat-runs-matrix data) "REDRAW" "ALL"))
    ;; (debug:print 2 *default-log-port* "run-changes: " run-changes)
    ;; (debug:print 2 *default-log-port* "test-changes: " test-changes)
    (list run-changes all-test-changes)))

(define (dcommon:runsdat-get-col-num dat target runname force-set)
  (let* ((runs-index (dboard:runsdat-runs-index dat))
	 (col-name   (conc target "/" runname))
	 (res        (hash-table-ref/default runs-index col-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-col-num (+ 1 (common:max (cons-1 (hash-table-values runs-index))))))
	      (hash-table-set! runs-index col-name max-col-num)
	      max-col-num)))))

(define (dcommon:runsdat-get-row-num dat testname itempath force-set)
  (let* ((tests-index (dboard:runsdat-runs-index dat))
	 (row-name    (conc testname "/" itempath))
	 (res         (hash-table-ref/default runs-index row-name #f)))
    (if res
	res
	(if force-set
	    (let ((max-row-num (+ 1 (common:max (cons -1 (hash-table-values tests-index))))))
	      (hash-table-set! runs-index row-name max-row-num)
	      max-row-num)))))

(define (dcommon:rundat-copy-tests-to-by-name rundat)
  (let ((src-ht (dboard:rundat-tests rundat))
	(trg-ht (dboard:rundat-tests-by-name rundat)))
    (if (and (hash-table? src-ht)(hash-table? trg-ht))
	(begin
	  (hash-table-clear! trg-ht)
	  (for-each
	   (lambda (testdat)
	     (hash-table-set! trg-ht (test:test-get-fullname testdat) testdat))
	   (hash-table-values src-ht)))
	(debug:print 0 *default-log-port* "WARNING: src-ht " src-ht " trg-ht " trg-ht))))
  

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
	(let* ((test-id    (db:test-get-id hed)) ;; look at the tests-dat spec for locations
	       (test-name  (db:test-get-testname hed))
	       (item-path  (db:test-get-item-path hed))
	       (state      (db:test-get-state hed))
	       (status     (db:test-get-status hed))
	       (newitem    (list test-name item-path (list test-id state status))))
	  (if (null? tal)
	      (reverse (cons newitem res))
	      (loop (car tal)(cdr tal)(cons newitem res)))))))

(define (dcommon:tests-mindat->hash tests-mindat)
  (let* ((res (make-hash-table)))
    (for-each
     (lambda (item)
       (let* ((test-name+item-path (cons (list-ref item 0) (list-ref item 1)))
              (value (list-ref item 2)))
         (hash-table-set! res test-name+item-path value)))
     tests-mindat)
    res))

;; return 1 if status1 is better
;; return 0 if status1 and 2 are equally good
;; return -1 if status2 is better
(define (dcommon:status-compare3 status1 status2)
  (let*
      ((status-goodness-ranking  (list "PASS" "WARN" "WAIVED" "SKIP" "FAIL" "ABORT" #f))
       (mem1 (member status1 status-goodness-ranking))
       (mem2 (member status2 status-goodness-ranking))
       )
    (cond
     ((and (not mem1) (not mem2)) 0)
     ((not mem1) -1)
     ((not mem2) 1)
     ((= (length mem1) (length mem2)) 0)
     ((> (length mem1) (length mem2)) 1)
     (else -1))))
     
(define (dcommon:xor-tests-mindat src-tests-mindat dest-tests-mindat #!key (hide-clean #f))
  (let* ((src-hash (dcommon:tests-mindat->hash src-tests-mindat))
         (dest-hash (dcommon:tests-mindat->hash dest-tests-mindat))
         (all-keys
          (reverse (sort 
           (delete-duplicates
            (append (hash-table-keys src-hash) (hash-table-keys dest-hash)))

           (lambda (a b) 
             (cond
              ((< 0 (string-compare3 (car a) (car b))) #t)
              ((> 0 (string-compare3 (car a) (car b))) #f)
              ((< 0 (string-compare3 (cdr a) (cdr b))) #t)
              (else #f)))

           ))))
    (let ((res
           (map ;; TODO: rename xor to delta globally in dcommon and dashboard
            (lambda (key)
              (let* ((test-name (car key))
                     (item-path (cdr key))

                     (dest-value (hash-table-ref/default dest-hash key #f)) ;; (list test-id state status)
                     (dest-test-id  (if dest-value (list-ref dest-value 0) #f))
                     (dest-state    (if dest-value (list-ref dest-value 1) #f))
                     (dest-status   (if dest-value (list-ref dest-value 2) #f))

                     (src-value     (hash-table-ref/default src-hash key #f))   ;; (list test-id state status)
                     (src-test-id   (if src-value (list-ref src-value 0) #f))
                     (src-state     (if src-value (list-ref src-value 1) #f))
                     (src-status    (if src-value (list-ref src-value 2) #f))

                     (incomplete-statuses '("DELETED" "INCOMPLETE" "STUCK/DEAD" "N/A")) ;; if any of these statuses apply, treat test as incomplete

                     (dest-complete
                      (and dest-value dest-state dest-status
                           (equal? dest-state "COMPLETED")
                           (not (member dest-status incomplete-statuses))))
                     (src-complete
                      (and src-value src-state src-status
                           (equal? src-state "COMPLETED")
                           (not (member src-status incomplete-statuses))))
                     (status-compare-result (dcommon:status-compare3 src-status dest-status))
                     (xor-new-item
                      (cond
                       ;; complete, for this case means: state=compelte AND status not in ( deleted uncomplete stuck/dead n/a )
                       ;; neither complete -> bad

                       ;; src !complete, dest complete -> better
                       ((and (not dest-complete) (not src-complete))
                        (list dest-test-id "BOTH-BAD" "BOTH-INCOMPLETE"))
                       ((not dest-complete)
                        (list src-test-id "DIFF-MISSING" "DEST-INCOMPLETE"))  
                       ((not src-complete)
                        (list dest-test-id "DIFF-NEW" "SRC-INCOMPLETE"))      
                       ((and
                         (equal? src-state dest-state)
                         (equal? src-status dest-status))
                        (list dest-test-id  (conc "CLEAN") (conc "CLEAN-" dest-status) )) 
                       ;;    better or worse: pass > warn > waived > skip > fail > abort
                       ;;     pass > warn > waived > skip > fail > abort
                       
                       ((= 1 status-compare-result) ;; src is better, dest is worse
                        (list dest-test-id "DIRTY-WORSE" (conc src-status "->" dest-status)))
                       (else
                        (list dest-test-id "DIRTY-BETTER" (conc src-status "->" dest-status)))
                       )))
                (list test-name item-path  xor-new-item)))
            all-keys)))

      (if hide-clean
          (filter
           (lambda (item)
             ;;(print item)
             (not
              (equal?
               "CLEAN"
               (list-ref (list-ref item 2) 1))))
           res)
          res))))

(define (dcommon:examine-xterm run-id test-id)
  (let* ((testdat (rmt:get-test-info-by-id run-id test-id)))
    (if (not testdat)
        (begin
          (debug:print 2 "ERROR: No test data found for test " test-id ", exiting")
          (exit 1))
        (let*
            ((rundir        (if testdat 
                                (db:test-get-rundir testdat)
                                logfile))
             (testfullname  (if testdat (db:test-get-fullname testdat) "Gathering data ..."))
             (xterm      (lambda ()
                           (if (directory-exists? rundir)
                               (let* ((shell (if (get-environment-variable "SHELL") 
                                                 (conc "-e " (get-environment-variable "SHELL"))
                                                 ""))
                                      (command (conc "cd " rundir 
                                                     ";mt_xterm -T \"" (string-translate testfullname "()" "  ") "\" " shell "&")))
                                 (print "Command =" command)
                                 (common:without-vars
                                  command
                                  "MT_.*"))
                               (message-window  (conc "Directory " rundir " not found"))))))
          (xterm)
          (print "Adding xterm code")))))

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
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
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
                           #:numlin-visible (min 10 (length key-vals))
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
       (set! curr-row-num (+ 1 curr-row-num))) ;; (config-lookup *configdat* "fields" var)))
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
    ;; (iup:attribute-set! general-matrix "2:1" *toppath*)
    ;; Megatest version
    (iup:attribute-set! general-matrix "2:0" "Version")
    (iup:attribute-set! general-matrix "2:1" (conc megatest-version "-" (substring megatest-fossil-hash 0 4)))

    general-matrix))

(define (dcommon:run-stats commondat tabdat #!key (tab-num #f))
  (let* ((stats-matrix (iup:matrix expand: "YES"))
	 (changed      #f)
	 (stats-updater (lambda ()
			 (if (dashboard:database-changed? commondat tabdat context-key: 'run-stats)
			     (let* ((run-stats    (rmt:get-run-stats))
				    (indices      (common:sparse-list-generate-index run-stats)) ;;  proc: set-cell))
				    (row-indices  (car indices))
				    (col-indices  (cadr indices))
				    (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
				    (max-col      (if (null? col-indices) 1 
						      (common:max (map cadr col-indices))))
				    (max-visible  (max (- (dboard:tabdat-num-tests tabdat) 15) 3))
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
			       (if changed (iup:attribute-set! stats-matrix "REDRAW" "ALL")))
                             ))))
    ;; (dboard:commondat-please-update-set! commondat #t) ;; force redraw on first pass 
    ;; (mark-for-update tabdat)
    ;; (stats-updater)
    (dboard:commondat-add-updater commondat stats-updater tab-num: tab-num)
    ;; (set! dashboard:update-summary-tab updater)
    (iup:attribute-set! stats-matrix "WIDTHDEF" "40")
    (iup:vbox
     ;; (iup:label "Run statistics"  #:expand "HORIZONTAL")
     stats-matrix)))

(define (dcommon:servers-table commondat tabdat)
  (let* ((tdbdat         (tasks:open-db))
	 (colnum         0)
	 (rownum         0)
	 (servers-matrix (iup:matrix #:expand "YES"
				     #:numcol 7
				     #:numcol-visible 7
				     #:numlin-visible 5
				     ))
	 (colnames       (list "Id" "MTver" "Pid" "Host" "Interface:OutPort" "RunTime" "State" "RunId"))
	 (updater        (lambda ()
			   (if (dashboard:monitor-changed? commondat tabdat)
			       (let ((servers  (server:get-list *toppath* limit: 10)))
				 ;; (tasks:get-all-servers (db:delay-if-busy tdbdat))))
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
				    (match-let (((mod-time host port start-time pid)
						 server))
				      (let* ((uptime  (- (current-seconds) mod-time))
					     (runtime (if start-time
							  (- mod-time start-time)
							  0))
					     (vals (list "-"  ;; (vector-ref server 0) ;; Id
							 "-"  ;; (vector-ref server 9) ;; MT-Ver
							 pid  ;; (vector-ref server 1) ;; Pid
							 host ;; (vector-ref server 2) ;; Hostname
							 (conc host ":" port) ;; (conc (vector-ref server 3) ":" (vector-ref server 4)) ;; IP:Port
							 (seconds->hr-min-sec runtime) ;; (- (current-seconds) start-time)) ;; (vector-ref server 6)))
							 (cond
							  ((< uptime 5)  "alive")
							  ((< uptime 16) "probably alive");; less than 15 seconds since mod, call it alive (vector-ref server 8) ;; State
							  (else "dead"))
							 "-" ;; (vector-ref server 12)  ;; RunId
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
				      (iup:attribute-set! servers-matrix "REDRAW" "ALL")))
				    (sort servers (lambda (a b)(> (car a)(car b))))))))))
    (set! colnum 0)
    (for-each (lambda (colname)
		(iup:attribute-set! servers-matrix (conc "0:" colnum) colname)
		(iup:attribute-set! servers-matrix "FITTOTEXT" (conc "C" colnum))
		(set! colnum (+ colnum 1)))
	      colnames)
    ;; (set! dashboard:update-servers-table updater) 
    (dboard:commondat-add-updater commondat updater)
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
(define (dcommon:main-menu)
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

(define (dcommon:draw-test cnv xoffset yoffset scalef x y w h name selected)
  (let* ((llx (dcommon:x->canvas x scalef xoffset))
	 (lly (dcommon:y->canvas y scalef yoffset))
	 (urx (dcommon:x->canvas (+ x w) scalef xoffset))
	 (ury (dcommon:y->canvas (+ y h) scalef yoffset)))
    (canvas-text! cnv (+ llx 5)(+ lly 5) name)
    (canvas-rectangle! cnv llx urx lly ury)
    (if selected (canvas-box! cnv llx (+ llx 5) lly (+ lly 5)))))

(define (dcommon:draw-arrow cnv test-box-center waiton-center)
  (let* ((test-box-center-x (vector-ref test-box-center 0))
	 (test-box-center-y (vector-ref test-box-center 1))
	 (waiton-center-x   (vector-ref waiton-center   0))
	 (waiton-center-y   (vector-ref waiton-center   1))
	 (delta-y           (- waiton-center-y test-box-center-y))
	 (delta-x           (- waiton-center-x test-box-center-x))
	 (abs-delta-x       (abs delta-x))
	 (abs-delta-y       (abs delta-y))
	 (use-delta-x       (> abs-delta-x abs-delta-y)) ;; use the larger one
	 (delta-ratio       (if use-delta-x
				(if (> abs-delta-x 0)
				    (/ abs-delta-y abs-delta-x)
				    1)
				(if (> abs-delta-y 0)
				    (/ abs-delta-x abs-delta-y)
				    1)))
	 (x-adj             (if use-delta-x
				8
				(* delta-ratio 8)))
	 (y-adj             (if use-delta-x
				(* x-adj delta-ratio)
				8))
	 (new-waiton-x      (inexact->exact
			     (round (if (> delta-x 0) ;; have positive x
					(- waiton-center-x x-adj)
					(+ waiton-center-x x-adj)))))
	 (new-waiton-y      (inexact->exact
			     (round (if (> delta-y 0)
					(- waiton-center-y y-adj)
					(+ waiton-center-y y-adj))))))
  ;; (canvas-line-width-set! cnv 5)
  (canvas-line! cnv
		test-box-center-x
		test-box-center-y
		new-waiton-x
		new-waiton-y
		)
  (canvas-mark! cnv new-waiton-x new-waiton-y)))

(define (dcommon:get-box-center box)
  (let* ((llx  (list-ref box 0))
	 (lly  (list-ref box 1))
	 (boxw (list-ref box 4))
	 (boxh (list-ref box 5)))
    (vector (+ llx (/ boxw 2))
	    (+ lly (/ boxh 2)))))

(define-inline (num->int num)
  (inexact->exact (round num)))

(define (dcommon:draw-edges cnv xoffset yoffset scalef edges)
  (for-each
   (lambda (e)
     (let loop ((x1 (car e))
		(y1 (cadr e))
		(x2 #f)
		(y2 #f)
		(tal (cddr e)))
       (if (and x1 y1 x2 y2)
	   (canvas-line! 
	    cnv 
	    (num->int (dcommon:x->canvas x1 scalef xoffset))
	    (num->int (dcommon:y->canvas y1 scalef yoffset))
	    (num->int (dcommon:x->canvas x2 scalef xoffset))
	    (num->int (dcommon:y->canvas y2 scalef yoffset)))) ;; (num->int x1)(num->int y1)(num->int x2)(num->int y2)))
       (if (< (length tal) 2)
	   (canvas-mark! cnv
			 (num->int (dcommon:x->canvas x1 scalef xoffset))
			 (num->int (dcommon:y->canvas y1 scalef yoffset))) ;; (num->int x1)(num->int y1))
	   (loop (car tal)(cadr tal) x1 y1 (cddr tal)))))
   ;; (map (lambda (e)(map (lambda (x)(num->int (* x scalef))) e)) edges)))
   edges))


(define (dcommon:draw-arrows cnv testname tests-hash test-records)
  (let* ((test-box-info   (hash-table-ref tests-hash testname))
	 (test-box-center (dcommon:get-box-center test-box-info))
	 (test-record     (hash-table-ref test-records testname))
	 (waitons         (vector-ref test-record 2)))
    (for-each
     (lambda (waiton)
       (let* ((waiton-box-info (hash-table-ref/default tests-hash waiton #f))
	      (waiton-center   (dcommon:get-box-center (or waiton-box-info test-box-info))))
	 (dcommon:draw-arrow cnv test-box-center waiton-center)))
     waitons)
    ;; (debug:print 0 *default-log-port* "test-box-info=" test-box-info)
    ;; (debug:print 0 *default-log-port* "test-record=" test-record)
    ))

(define (dcommon:estimate-scale sizex sizey originx originy nodes)
  ;; (print "sizex: " sizex " sizey: " sizey " originx: " originx " originy: " originy " nodes: " nodes)
  (let* ((maxx 1)
	 (maxy 1))
    (for-each
     (lambda (node)
       (if (equal? (car node) "node")
	   (let ((x (string->number (list-ref node 2)))
		 (y (string->number (list-ref node 3))))
	     (if (and x (> x maxx))(set! maxx x))
	     (if (and y (> y maxy))(set! maxy y)))))
     nodes)
    (let ((scalex (/ sizex maxx))
	  (scaley (/ sizey maxy)))
      ;; (print "maxx: " maxx " maxy: " maxy " scalex: " scalex " scaley: " scaley)
      (min scalex scaley))))

(define (dcommon:get-xoffset tests-draw-state sizex-in xadj-in)
  (let ((xadj  (or xadj-in  (hash-table-ref/default tests-draw-state 'xadj 0)))
	(sizex (or sizex-in (hash-table-ref/default tests-draw-state 'sizex 500))))
    (hash-table-set! tests-draw-state 'xadj xadj) ;; for use in de-scaling when handling mouse clicks
    (hash-table-set! tests-draw-state 'sizex sizex)
    (* (/ sizex 2) (- 0.5 xadj))))

(define (dcommon:get-yoffset tests-draw-state sizey-in yadj-in)
  (let ((yadj  (or yadj-in  (hash-table-ref/default tests-draw-state 'yadj 0)))
	(sizey (or sizey-in (hash-table-ref/default tests-draw-state 'sizey 500))))
    (hash-table-set! tests-draw-state 'yadj yadj) ;; for use in de-scaling when handling mouse clicks
    (hash-table-set! tests-draw-state 'sizey sizey)
    (* (/ sizey 2) (- yadj 0.5))))

(define (dcommon:x->canvas x scalef xoffset)
  (+ xoffset (* x scalef)))

(define (dcommon:y->canvas y scalef yoffset)
  (+ yoffset (* y scalef)))

;; sizex, sizey     - canvas size
;; originx, originy - canvas origin
;;
(define (dcommon:initial-draw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames test-records)
  (let* ((dot-data ;; (map cdr (filter
		   ;; 	  (lambda (x)(equal? "node" (car x)))
	  (map string-split (tests:lazy-dot test-records "plain" sizex sizey))) ;; (tests:easy-dot test-records "plain")))
	 (xoffset	 (dcommon:get-xoffset tests-draw-state sizex xadj))
	 (yoffset        (dcommon:get-yoffset tests-draw-state sizey yadj))
	 (no-dot         (configf:lookup *configdat* "setup" "nodot"))
	 (boxh           15)
	 (boxw           10)
	 (margin         5)
	 (tests-info     (hash-table-ref tests-draw-state 'tests-info))
	 (selected-tests (hash-table-ref tests-draw-state 'selected-tests ))
	 (scalef         (if no-dot
			     1
			     (dcommon:estimate-scale sizex sizey originx originy dot-data)))
	 (sorted-testnames (if no-dot
			       (sort sorted-testnames string>=?)
			       sorted-testnames))
	 (curr-x         0)  ;; NB// NOT screen units
	 (curr-y         (/ (- sizey boxh margin) scalef)) ;; used when no-dot
	 (scaled-sizex   (/ sizex scalef)))

    (hash-table-set! tests-draw-state 'scalef scalef)
    
    (let ((longest-str   (if (null? sorted-testnames) "         " (car (sort sorted-testnames (lambda (a b)(>= (string-length a)(string-length b))))))))
      (let-values (((x-max y-max) (canvas-text-size cnv longest-str)))
	(if (> x-max boxw)(set! boxw (+ 10 x-max)))))
    ;; (print "sizex: " sizex " sizey: " sizey " font: " (canvas-font cnv) " originx: " originx " originy: " originy " xtorig: " xtorig " ytorig: " ytorig " xadj: " xadj " yadj: " yadj)
    (if (not (null? sorted-testnames))
	(let loop ((hed (car (reverse sorted-testnames)))
		   (tal (cdr (reverse sorted-testnames))))
	  (let* ((nodedat (if no-dot
			      #f
			      (let ((tmpres (filter (lambda (x)
						      (if (and (not (null? x))
							       (equal? (car x) "node"))
							  (equal? hed (cadr x))
							  #f))
						    dot-data)))
				(if (null? tmpres)
				    ;;           llx  lly boxw boxh
				    (list "0" "1" "1" (conc (length tal)) "2" "0.5") ;; return some placeholder junk if no dat found
				    (car tmpres)))))
		 (edgedat (if no-dot
			      '()
			      (let ((edges (filter (lambda (x)  ;; filter for edge
						     (if (and (not (null? x))
							      (equal? (car x) "edge"))
							 (equal? hed (cadr x))
							 #f))
						   dot-data)))
				(map (lambda (inlst)
				       (dcommon:process-polyline 
					(map (lambda (instr)
					       (string->number instr)) ;; convert to number and scale
					     (let ((il (cddddr inlst)))
					       (take il (- (length il) 2))))
					(lambda (x y)
					  (list (+ x 0)   ;; xtorig)
						(+ y 0))) ;; ytorig)))
					#f #f)) ;; process polyline
				     edges))))
		 (llx  (if no-dot
			   curr-x
			   (string->number (list-ref nodedat 2))))
		 (lly  (if no-dot
			   curr-y
			   (string->number (list-ref nodedat 3))))
		 (boxw (if no-dot
			   boxw
			   (string->number (list-ref nodedat 4))))
		 (boxh (if no-dot
			   boxh
			   (string->number (list-ref nodedat 5))))
		 (urx  (+ llx boxw))
		 (ury  (+ lly boxh)))

	    ;; if we are in no-dot mode then increment curr-x and curr-y as needed
	    (if no-dot
		(begin
		  (cond 
		   ((< curr-x (- scaled-sizex boxw boxw margin))
		    (set! curr-x (+ curr-x boxw margin)))
		   ((> curr-x (- scaled-sizex boxw boxw margin))
		    (set! curr-x 0)
		    (set! curr-y (- curr-y (+ boxh margin)))))))
					; (print "hed " hed " llx " llx " lly " lly " urx " urx " ury " ury)
	    (dcommon:draw-test cnv xoffset yoffset scalef llx lly boxw boxh hed (hash-table-ref/default selected-tests hed #f))
	    ;; (dcommon:draw-arrows cnv testname tests-info test-records))
	    (dcommon:draw-edges cnv xoffset yoffset scalef edgedat)
	    
	    ;; data used by mouse click calc. keep the wacky order for now.
	    (hash-table-set! tests-info hed  (list llx lly urx ury boxw boxh edgedat)) 
	    (if (not (null? tal))
		(loop (car tal)
		      (cdr tal))))))
    ))

;; per-point-proc required, remainder optional
;;
(define (dcommon:process-polyline line per-point-proc per-segment-proc last-segment-proc)
  (if (< (length line) 2)
      '()
      (let loop ((x1   (car  line))
		 (y1   (cadr line))
		 (x2   #f)
		 (y2   #f)
		 (tal  (cddr line))
		 (res  '()))
	(if (and x1 y1 x2 y2 per-segment-proc)
	    (per-segment-proc x1 y1 x2 y2))
	(if (< (length tal) 2)
	    (begin
	      (if last-segment-proc (last-segment-proc x1 y1 x2 y2))
	      (append res (per-point-proc x1 y1)))
	    (loop (car tal)(cadr tal) x1 y1 (cddr tal) (append res (per-point-proc x1 y1)))))))

(define (dcommon:redraw-tests cnv xadj yadj sizex sizey sizexmm sizeymm originx originy tests-draw-state sorted-testnames test-records)
  (let* ((scalef              (hash-table-ref tests-draw-state 'scalef))
	 (xoffset             (dcommon:get-xoffset tests-draw-state sizex xadj))
	 (yoffset             (dcommon:get-yoffset tests-draw-state sizey yadj))
	 (tests-info          (hash-table-ref tests-draw-state 'tests-info))
	 (selected-tests      (hash-table-ref tests-draw-state 'selected-tests )))
    (if (not (null? sorted-testnames))
	(let loop ((hed (car (reverse sorted-testnames)))
		   (tal (cdr (reverse sorted-testnames))))
	  (let* ((tvals (hash-table-ref tests-info hed))
		 (llx   (list-ref tvals 0))
		 (lly   (list-ref tvals 1))
		 (boxw  (list-ref tvals 4))
		 (boxh  (list-ref tvals 5))
		 (edges (map (lambda (pline)
			       (dcommon:process-polyline pline
							 (lambda (x1 y1)
							   (list x1 y1))
							 #f #f))
			     (list-ref tvals 6)))
		 (urx   (+ llx boxw))
		 (ury   (+ lly boxh)))
	    (dcommon:draw-test cnv xoffset yoffset scalef llx lly boxw boxh hed (hash-table-ref/default selected-tests hed #f))
	    (dcommon:draw-edges cnv xoffset yoffset scalef edges)
	    (if (not (null? tal))
		;; leave a column of space to the right to list items
		(loop (car tal)
		      (cdr tal))))))))

;;======================================================================
;; RUN CONTROLS
;;======================================================================

(define (dcommon:command-execution-control data)
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
      (dboard:tabdat-command-tb-set! data tb)
      tb)
    (iup:button "Execute" #:size "50x"
		#:action (lambda (obj)
			   ;; (let ((cmd (conc ;; "xterm -geometry 180x20 -e \""
                           (common:run-a-command (iup:attribute (dboard:tabdat-command-tb data) "VALUE")))))))
    ;; ";echo Press any key to continue;bash -c 'read -n 1 -s'\" &")))
    ;; (system cmd)))))))

(define (dcommon:command-action-selector commondat tabdat #!key (tab-num #f))
  (iup:frame
   #:title "Set the action to take"
   (iup:hbox
    ;; (iup:label "Command to run" #:expand "HORIZONTAL" #:size "70x" #:alignment "LEFT:ACENTER")
    (let* ((cmds-list '("run" "remove-runs")) ;;  "set-state-status" "lock-runs" "unlock-runs"))
	   (lb         (iup:listbox #:expand "HORIZONTAL"
				    #:dropdown "YES"
				    #:action (lambda (obj val index lbstate)
					       ;; (print obj " " val " " index " " lbstate)
					       (dboard:tabdat-command-set! tabdat val)
					       (dashboard:update-run-command tabdat))))
	   (default-cmd (car cmds-list)))
      (iuplistbox-fill-list lb cmds-list selected-item: default-cmd)
      (dboard:tabdat-command-set! tabdat default-cmd)
      lb))))

(define (dcommon:command-runname-selector commondat tabdat #!key (tab-num #f)) ;; alldat data)
  (iup:frame
   #:title "Runname"
   (let* ((default-run-name (seconds->work-week/day (current-seconds)))
	  (tb (iup:textbox #:expand "HORIZONTAL"
			   #:action (lambda (obj val txt)
				      (debug:catch-and-dump
				       (lambda ()
					 ;; (print "obj: " obj " val: " val " unk: " unk)
					 (dboard:tabdat-run-name-set! tabdat txt) ;; (iup:attribute obj "VALUE"))
					 (dashboard:update-run-command tabdat))
				       "command-runname-selector tb action"))
			   #:value (or default-run-name (dboard:tabdat-run-name tabdat))))
	  (lb (iup:listbox #:expand "HORIZONTAL"
			   #:dropdown "YES"
			   #:action (lambda (obj val index lbstate)
				      (debug:catch-and-dump
				       (lambda ()
					 (if (not (equal? val ""))
					     (begin
					       (iup:attribute-set! tb "VALUE" val)
					       (dboard:tabdat-run-name-set! tabdat val)
					       (dashboard:update-run-command tabdat))))
				       "command-runname-selector lb action"))))
	  (refresh-runs-list (lambda ()
			       (if (dashboard:database-changed? commondat tabdat context-key: 'runname-selector-runs-list)
				   (let* (;; (target        (dboard:tabdat-target-string tabdat))
					  (runs-for-targ (rmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f 0))
					  (runs-header   (vector-ref runs-for-targ 0))
					  (runs-dat      (vector-ref runs-for-targ 1))
					  (run-names     (cons default-run-name 
							       (map (lambda (x)
								      (db:get-value-by-header x runs-header "runname"))
								    runs-dat))))
				     ;; (print "DEBUGINFO: run-names=" run-names)
				     ;; (iup:attribute-set! lb "REMOVEITEM" "ALL")
				     (iuplistbox-fill-list lb run-names selected-item: default-run-name))))))
     ;; (dboard:tabdat-updater-for-runs-set! tabdat refresh-runs-list)
     (dboard:commondat-add-updater commondat refresh-runs-list tab-num: tab-num)
     ;; (refresh-runs-list)
     (dboard:tabdat-run-name-set! tabdat default-run-name)
     (iup:hbox
      tb
      lb))))

(define (dcommon:command-testname-selector commondat tabdat update-keyvals) ;;  key-listboxes)
  (iup:vbox
   ;; Text box for test patterns
   (iup:frame
    #:title "Test patterns (one per line)"
    (let ((tb (iup:textbox #:action (lambda (val a b)
				      (debug:catch-and-dump
				       (lambda ()
					 (dboard:tabdat-test-patts-set!-use
					  tabdat
					  (dboard:lines->test-patt b))
					 (dashboard:update-run-command tabdat))
				       "command-testname-selector tb action"))
			   #:value (dboard:test-patt->lines
				    (dboard:tabdat-test-patts-use tabdat))
			   #:expand "YES"
			   #:size "x30" ;; was 10x30
			   #:multiline "YES")))
      (set! test-patterns-textbox tb)
      (dboard:tabdat-test-patterns-textbox-set! tabdat tb)
      tb))
;; (iup:frame
;;  #:title "Target"
;;  ;; Target selectors
;;  (apply iup:hbox
;; 	   (let* ((dat      (dashboard:update-target-selector tabdat action-proc: update-keyvals))
;; 		  (key-lb   (car dat))
;; 		  (combos   (cadr dat)))
;; 	     combos)))
   (iup:hbox
    ;; Text box for STATES
    (iup:frame
     #:title "States"
     (dashboard:text-list-toggle-box 
      ;; Move these definitions to common and find the other useages and replace!
      (map cadr *common:std-states*) ;; '("COMPLETED" "RUNNING" "STUCK" "INCOMPLETE" "LAUNCHED" "REMOTEHOSTSTART" "KILLED")
      (lambda (all)
	(dboard:tabdat-states-set! tabdat all)
	(dashboard:update-run-command tabdat))))
    ;; Text box for STATES
    (iup:frame
     #:title "Statuses"
     (dashboard:text-list-toggle-box 
      (map cadr *common:std-statuses*) ;; '("PASS" "FAIL" "n/a" "CHECK" "WAIVED" "SKIP" "DELETED" "STUCK/DEAD")
      (lambda (all)
	(dboard:tabdat-statuses-set! tabdat all)
	(dashboard:update-run-command tabdat)))))))

(define (dcommon:command-tests-tasks-canvas tabdat test-records sorted-testnames tests-draw-state)
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
					    (new-y          (- sizey y))
					    (test-patterns-textbox (dboard:tabdat-test-patterns-textbox tabdat)))
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
							 (let* ((box-patterns (string-split (iup:attribute test-patterns-textbox "VALUE")))
                                                                (test-patts   (string-split (or (dboard:tabdat-test-patts tabdat)
                                                                                                "")
                                                                                            ","))
                                                                (patterns     (delete-duplicates (append box-patterns test-patts)))) 
							   (let* ((selected     (not (member test-name patterns)))
								  (newpatt-list (if selected
										    (cons test-name patterns)
										    (delete test-name patterns)))
								  (newpatt      (string-intersperse newpatt-list "\n")))
							     (iup:attribute-set! test-patterns-textbox "VALUE" newpatt)
							     (iup:attribute-set! obj "REDRAW" "ALL")
							     (hash-table-set! selected-tests test-name selected)
							     (dboard:tabdat-test-patts-set!-use tabdat (dboard:lines->test-patt newpatt))
							     (dashboard:update-run-command tabdat)
							     (if updater (updater last-xadj last-yadj)))))))
						 (hash-table-keys tests-info)))))))
     canvas-obj)))

;;======================================================================
;;  S T E P S
;;======================================================================

(define (dcommon:populate-steps teststeps steps-matrix)
  (let ((max-row 0)
	(max-col 7))
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
	    (if (< colnum max-col)
		(loop hed tal rownum (+ colnum 1))
		(if (not (null? tal))
		    (loop (car tal)(cdr tal)(+ rownum 1) 1))))))
    (if (> max-row 0)
	(begin
	  ;; we are going to speculatively clear rows until we find a row that is already cleared
	  (let loop ((rownum  (+ max-row 1))
		     (colnum  0)
		     (deleted #f))
	    ;; (debug:print-info 0 *default-log-port* "cleaning " rownum ":" colnum)
	    (let* ((next-row (if (eq? colnum max-col) (+ rownum 1) rownum))
		   (next-col (if (eq? colnum max-col) 1 (+ colnum 1)))
		   (mtrx-rc  (conc rownum ":" colnum))
		   (curr-val (iup:attribute steps-matrix mtrx-rc)))
	      ;; (debug:print-info 0 *default-log-port* "cleaning " rownum ":" colnum " currval= " curr-val)
	      (if (and (string? curr-val)
		       (not (equal? curr-val "")))
		  (begin
		    (iup:attribute-set! steps-matrix mtrx-rc "")
		    (loop next-row next-col #t))
		  (if (eq? colnum max-col) ;; not done, didn't get a full blank row
		      (if deleted (loop next-row next-col #f)) ;; exit on this not met
		      (loop next-row next-col deleted)))))
	  (iup:attribute-set! steps-matrix "REDRAW" "ALL")))))
