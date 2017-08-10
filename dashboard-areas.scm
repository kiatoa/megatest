;;======================================================================
;; AREAS
;;======================================================================

(define (dashboard:areas-summary-updater commondat tabdat tb cell-lookup run-matrix)
  (dashboard:areas-do-update-rundat tabdat) ;; )
  (dboard:areas-summary-control-panel-updater tabdat)
  (let* ((last-runs-update  (dboard:tabdat-last-runs-update tabdat))
	 (runs-dat     (mrmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f last-runs-update))
	 (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
         (runs         (vector-ref runs-dat 1))
	 (run-id       (dboard:tabdat-curr-run-id tabdat))
         (runs-hash (dashboard:areas-get-runs-hash tabdat))
         ;; (runs-hash    (let ((ht (make-hash-table)))
	 ;;        	 (for-each (lambda (run)
	 ;;        		     (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
	 ;;        		   runs)
	 ;;        	 ht))
         )
    (if (dashboard:areas-database-changed? commondat tabdat context-key: 'runs-summary-tree)
        (dboard:areas-update-tree tabdat runs-hash runs-header tb))
    (if run-id
        (let* ((matrix-content
                (case (dboard:tabdat-runs-summary-mode tabdat) 
                  ((one-run) (dashboard:areas-run-id->tests-mindat run-id tabdat runs-hash))
                  ((xor-two-runs) (dashboard:areas-runs-summary-xor-matrix-content tabdat runs-hash))
                  ((xor-two-runs-hide-clean) (dashboard:areas-runs-summary-xor-matrix-content tabdat runs-hash hide-clean: #t))
                  (else (dashboard:areas-run-id->tests-mindat run-id tabdat runs-hash)))))
          (when matrix-content
            (let* ((indices      (common:sparse-list-generate-index matrix-content)) ;;  proc: set-cell))
                   (row-indices  (cadr indices))
                   (col-indices  (car indices))
                   (max-row      (if (null? row-indices) 1 (common:max (map cadr row-indices))))
                   (max-col      (if (null? col-indices) 1 (common:max (map cadr col-indices))))
                   (max-visible  (max (- (dboard:tabdat-num-tests tabdat) 15) 3)) ;; (dboard:tabdat-num-tests tabdat) is proportional to the size of the window
                   (numrows      1)
                   (numcols      1)
                   (changed      #f)
                   )
              
              (dboard:tabdat-filters-changed-set! tabdat #f)
              (let loop ((pass-num 0)
                         (changed  #f))
                (if (eq? pass-num 1)
                    (begin ;; big reset
                      (iup:attribute-set! run-matrix "CLEARVALUE" "ALL") ;; NOTE: Was CONTENTS
                      (iup:attribute-set! run-matrix "CLEARATTRIB" "CONTENTS")
                      (iup:attribute-set! run-matrix "RESIZEMATRIX" "YES")))

                (if (> max-col (string->number (iup:attribute run-matrix "NUMCOL")))
                    (iup:attribute-set! run-matrix "NUMCOL" max-col ))

                (let ((effective-max-row (if (< max-row max-visible) max-visible max-row)))
                  (if (> effective-max-row (string->number (iup:attribute run-matrix "NUMLIN")))
                      (iup:attribute-set! run-matrix "NUMLIN" effective-max-row )))
                
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
                ;; (print "row-indices: " row-indices " col-indices: " col-indices)
                (if (and (eq? pass-num 0) changed)
                    (loop 1 #t)) ;; force second pass
                
                ;; Cell contents
                (for-each (lambda (entry)
                            ;; (print "entry: " entry)
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
                          matrix-content)
                
                ;; Col labels - do after setting Cell contents so they are accounted for in the size calc.
                
                (for-each (lambda (ind)
                            (let* ((name (car ind))
                                   (num  (cadr ind))
                                   (key  (conc "0:" num)))
                              (if (not (equal? (iup:attribute run-matrix key) name))
                                  (begin
                                    (set! changed #t)
                                    (iup:attribute-set! run-matrix key name)
                                    (if (<= num max-col)
                                        (iup:attribute-set! run-matrix "FITTOTEXT" (conc "C" num)))))))
                          col-indices)
                
                (if (and (eq? pass-num 0) changed)
                    (loop 1 #t)) ;; force second pass due to column labels changing
                
                ;; (debug:print 0 *default-log-port* "runs-summary-updater, changed: " changed " pass-num: " pass-num)
                ;; (print "runs-summary-updater, changed: " changed " pass-num: " pass-num)
                (if changed (iup:attribute-set! run-matrix "REDRAW" "ALL")))))))))

(define (dboard:areas-make-matrix commondat tabdat )
  (iup:matrix
   #:expand "YES"
   #:click-cb
   
   (lambda (obj lin col status)
     (debug:catch-and-dump
      (lambda ()
	
	;; Bummer - we dont have the global get/set api mapped in chicken
	;; (let* ((modkeys (iup:global "MODKEYSTATE")))
	;;   (BB> "modkeys="modkeys))
	
	(debug:print-info 13 *default-log-port* "click-cb: obj="obj" lin="lin" col="col" status="status)
	;; status is corrupted on Brandon's home machine.  will have to wait until after shutdown to see if it is still broken in PDX SLES
	(let* ((toolpath (car (argv)))
	       (key      (conc lin ":" col))
	       (test-id   (hash-table-ref/default cell-lookup key -1))
	       (run-id   (dboard:tabdat-curr-run-id tabdat))
	       (run-info (mrmt:get-run-info run-id))
	       (target   (mrmt:get-target run-id))
	       (runname  (db:get-value-by-header (db:get-rows run-info)
						 (db:get-header run-info) "runname"))
	       (test-info  (mrmt:get-test-info-by-id run-id test-id))
	       (test-name (db:test-get-testname test-info))
	       (testpatt  (let ((tlast (mrmt:tasks-get-last target runname)))
			    (if tlast
				(let ((tpatt (tasks:task-get-testpatt tlast)))
				  (if (member tpatt '("0" 0)) ;; known bad historical value - remove in 2017
				      "%"
				      tpatt))
				"%")))
	       (item-path (db:test-get-item-path (mrmt:get-test-info-by-id run-id test-id)))
	       (item-test-path (conc test-name "/" (if (equal? item-path "")
						       "%" 
						       item-path)))
	       (status-chars (char-set->list (string->char-set status)))
	       (testpanel-cmd      (conc toolpath " -test " (dboard:tabdat-curr-run-id tabdat) "," test-id " &")))
	  (debug:print-info 13 *default-log-port* "status-chars=["status-chars"] status=["status"]")
	  (cond
	   ((member #\1 status-chars) ;; 1 is left mouse button
	    (system testpanel-cmd))
	   
	   ((member #\2 status-chars) ;; 2 is middle mouse button
	    
	    (debug:print-info 13 *default-log-port* "mmb- test-name="test-name" testpatt="testpatt)
	    (iup:show (dashboard:areas-popup-menu run-id test-id target runname test-name testpatt item-test-path test-info) ;; popup-menu
		      #:x 'mouse
		      #:y 'mouse
		      #:modal? "NO")
	    )
	   (else
	    (debug:print-info 13 *default-log-port* "unhandled status in run-summary-click-cb.  Doing right click action. (status is corrupted on Brandon's ubuntu host - bad/buggy  iup install??" )
	    (iup:show (dashboard:areas-popup-menu run-id test-id target runname test-name testpatt item-test-path test-info) ;; popup-menu
		      #:x 'mouse
		      #:y 'mouse
		      #:modal? "NO")
	    )))) "runs-summary-click-callback"))))

;; This is the Areas Summary tab
;; 
(define (dashboard:areas-summary commondat tabdat #!key (tab-num #f))
  (let* ((update-mutex (dboard:commondat-update-mutex commondat))
	 (tb      (iup:treebox
		   #:value 0
		   #:name "Areas"
		   #:expand "YES"
		   #:addexpanded "YES"
		   #:selection-cb
		   (lambda (obj id state)
		     (debug:catch-and-dump
		      (lambda ()
			;; (print "obj: " obj ", id: " id ", state: " state)
			(let* ((run-path (tree:node->path obj id))
			       (run-id   (tree-path->run-id tabdat (cdr run-path))))
			  (if (number? run-id)
			      (begin
                                (dboard:tabdat-prev-run-id-set!
                                 tabdat
                                 (dboard:tabdat-curr-run-id tabdat))

				(dboard:tabdat-curr-run-id-set! tabdat run-id)
				(dboard:tabdat-layout-update-ok-set! tabdat #f)
				;; (dashboard:update-run-summary-tab)
				)
			      ;; (debug:print-error 0 *default-log-port* "tree-path->run-id returned non-number " run-id)
			      )))
		      "selection-cb in areas-summary")
		     ;; (print "path: " (tree:node->path obj id) " run-id: " run-id)
		     )))
	 (cell-lookup            (make-hash-table))
	 (areas-matrix           (dboard:areas-make-matrix commondat tabdat))
	 (areas-summary-updater  (lambda ()
				   (mutex-lock! update-mutex)
				   (if  (or (dashboard:areas-database-changed? commondat tabdat context-key: 'runs-summary-updater)
					    (dboard:tabdat-view-changed tabdat))
					(debug:catch-and-dump
					 (lambda () ;; check that areas-matrix is initialized before calling the updater
					   (if areas-matrix 
					       (dashboard:areas-summary-updater commondat tabdat tb cell-lookup areas-matrix)))
					 "dashboard:areas-summary-updater")
					)
				   (mutex-unlock! update-mutex)))
         (runs-summary-control-panel (dashboard:areas-summary-control-panel tabdat)))
    (dboard:commondat-add-updater commondat areas-summary-updater tab-num: tab-num)
    (dboard:tabdat-runs-tree-set! tabdat tb)
    (iup:vbox
     (iup:split
      #:value 200
      tb
      areas-matrix)
     (dboard:make-controls commondat tabdat extra-widget: runs-summary-control-panel))))

;; this calls dboard:get-tests-for-run-duplicate for each run
;;
;; create a virtual table of all the tests
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;;
(define (dboard:areas-update-rundat tabdat runnamepatt numruns testnamepatt keypatts)
  (let* ((access-mode      (dboard:tabdat-access-mode tabdat))
         (keys             (dboard:tabdat-keys tabdat))
	 (last-runs-update (- (dboard:tabdat-last-runs-update tabdat) 2))
         (allruns          (mrmt:get-runs runnamepatt numruns (dboard:tabdat-start-run-offset tabdat) keypatts))
         ;;(allruns-tree (mrmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f))
         (allruns-tree    (mrmt:get-runs-by-patt keys "%" #f #f #f #f 0)) ;; last-runs-update));;'("id" "runname")
	 (header      (db:get-header allruns))
	 (runs        (db:get-rows   allruns)) ;; RA => Filtered as per runpatt selected
         (runs-tree   (db:get-rows   allruns-tree)) ;; RA => Returns complete list of runs
	 (start-time  (current-seconds))
	 (runs-hash   (let ((ht (make-hash-table)))
			 (for-each (lambda (run)
				     (hash-table-set! ht (db:get-value-by-header run header "id") run))
				   runs-tree) ;; (vector-ref runs-dat 1))
			 ht))
	 (tb          (dboard:tabdat-runs-tree tabdat)))
    (dboard:tabdat-last-runs-update-set! tabdat (- (current-seconds) 2))
    (dboard:tabdat-header-set! tabdat header)
    ;; 
    ;; trim runs to only those that are changing often here
    ;; 
    (if (null? runs)
	(begin
	  (dboard:tabdat-allruns-set! tabdat '())
	  (dboard:tabdat-all-test-names-set! tabdat '())
	  (dboard:tabdat-item-test-names-set! tabdat '())
	  (hash-table-clear! (dboard:tabdat-allruns-by-id tabdat)))
	(let loop ((run      (car runs))
		   (tal      (cdr runs))
		   (res     '())
		   (maxtests 0))
	  (let* ((run-id       (db:get-value-by-header run header "id"))
		 (run-struct   (hash-table-ref/default (dboard:tabdat-allruns-by-id tabdat) run-id #f))
		 ;; (last-update  (if run-struct (dboard:rundat-last-update run-struct) 0))
		 (key-vals     (mrmt:get-key-vals run-id))
		 (tests-ht     (dboard:get-tests-for-run-duplicate tabdat run-id run testnamepatt key-vals))
		 ;; GET RID OF dboard:get-tests-dat - it is superceded by dboard:get-tests-for-run-duplicate
		 ;;  dboard:get-tests-for-run-duplicate - returns a hash table
		 ;;  (dboard:get-tests-dat tabdat run-id last-update))
		 (all-test-ids (hash-table-keys tests-ht))
		 (num-tests    (length all-test-ids)))
	    ;; (print "run-struct: " run-struct)
	    ;; NOTE: bubble-up also sets the global (dboard:tabdat-item-test-names tabdat)
	    ;; (tests       (bubble-up tmptests priority: bubble-type))
	    ;; NOTE: 11/01/2013 This routine is *NOT* getting called excessively.
	    ;; (debug:print 0 *default-log-port* "Getting data for run " run-id " with key-vals=" key-vals)
	    ;; Not sure this is needed?
	    (let* ((newmaxtests (max num-tests maxtests))
		   ;; (last-update (- (current-seconds) 10))
		   (run-struct  (or run-struct
				    (dboard:rundat-make-init
				     run:         run 
				     tests:       tests-ht
				     key-vals:    key-vals)))
		   (new-res     (if (null? all-test-ids)
                                    res
                                    (delete-duplicates
                                     (cons run-struct res)
                                     (lambda (a b)
                                       (eq? (db:get-value-by-header (dboard:rundat-run a) header "id")
                                            (db:get-value-by-header (dboard:rundat-run b) header "id"))))))
		   (elapsed-time (- (current-seconds) start-time)))
	      (if (null? all-test-ids)
		  (hash-table-delete! (dboard:tabdat-allruns-by-id tabdat) run-id)
		  (hash-table-set!    (dboard:tabdat-allruns-by-id tabdat) run-id run-struct))
	      (if (or (null? tal)
		      (> elapsed-time 2)) ;; stop loading data after 5 seconds, on the next call more data *should* be loaded since get-tests-for-run uses last update
		  (begin
		    (when (> elapsed-time 2)   
                      (debug:print 0 *default-log-port* "NOTE: updates are taking a long time, " elapsed-time "s elapsed.")
                      (let* ((old-val (iup:attribute *tim* "TIME"))
                             (new-val (number->string (inexact->exact (floor (* 2  (string->number old-val)))))))
                        (debug:print 0 *default-log-port* "NOTE: increasing poll interval from "old-val" to "new-val)
                        (iup:attribute-set! *tim* "TIME" new-val))


                      )
		    (dboard:tabdat-allruns-set! tabdat new-res)
		    maxtests)
		  (if (> (dboard:rundat-run-data-offset run-struct) 0)
		      (loop run tal new-res newmaxtests) ;; not done getting data for this run
		      (loop (car tal)(cdr tal) new-res newmaxtests)))))))
    (dboard:tabdat-filters-changed-set! tabdat #f)
    (dboard:areas-update-tree tabdat runs-hash header tb)))

;; runs update-rundat using the various filters from the gui
;;
(define (dashboard:areas-do-update-rundat tabdat)
  (dboard:areas-update-rundat
   tabdat
   (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "runname" "%")
   (dboard:tabdat-numruns tabdat)
   (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) "test-name" "%/%")
   ;; generate key patterns from the target stored in tabdat
   (let* ((dbkeys (dboard:tabdat-dbkeys tabdat)))
     (let ((fres   (if (dboard:tabdat-target tabdat)
                       (let ((ptparts (append (dboard:tabdat-target tabdat)(make-list (length dbkeys) "%"))))
                         (map (lambda (k v)(list k v)) dbkeys ptparts))
                       (let ((res '()))
                         (for-each (lambda (key)
                                     (if (not (equal? key "runname"))
                                         (let ((val (hash-table-ref/default (dboard:tabdat-searchpatts tabdat) key #f)))
                                           (if val (set! res (cons (list key val) res))))))
                                   dbkeys)
                         res))))
       fres))))

(define (dashboard:areas-get-runs-hash tabdat)
  (let* ((access-mode       (dboard:tabdat-access-mode tabdat))
         (last-runs-update  0);;(dboard:tabdat-last-runs-update tabdat))
	 (runs-dat     (mrmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f last-runs-update))
	 (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
         (runs         (vector-ref runs-dat 1))
	 (run-id       (dboard:tabdat-curr-run-id tabdat))
         (runs-hash    (let ((ht (make-hash-table)))
			 (for-each (lambda (run)
				     (hash-table-set! ht (db:get-value-by-header run runs-header "id") run))
				   runs) ht)))
    runs-hash))
         
;; DOES NOT WORK RELIABLY WITH /tmp WAL mode files. Timestamps only change when the db
;; is closed (I think). If db dir starts with /tmp always return true
;;
(define (dashboard:areas-database-changed? commondat tabdat #!key (context-key 'default))
  (let* ((run-update-time (current-seconds))
	 (dbdir           (dboard:tabdat-dbdir tabdat))
	 (modtime         (dashboard:areas-get-youngest-run-db-mod-time dbdir))
	 (recalc          (dashboard:areas-recalc modtime 
					    (dboard:commondat-please-update commondat) 
					    (dboard:get-last-db-update tabdat context-key))))
    ;; (dboard:tabdat-last-db-update tabdat))))
    (if recalc 
	(dboard:set-last-db-update! tabdat context-key run-update-time))
    (dboard:commondat-please-update-set! commondat #f)
    recalc))

(define (dboard:areas-update-tree tabdat runs-hash runs-header tb)
  (let* ((access-mode   (dboard:tabdat-access-mode tabdat))
         (run-ids (sort (filter number? (hash-table-keys runs-hash))
			(lambda (a b)
			  (let* ((record-a (hash-table-ref runs-hash a))
				 (record-b (hash-table-ref runs-hash b))
				 (time-a   (db:get-value-by-header record-a runs-header "event_time"))
				 (time-b   (db:get-value-by-header record-b runs-header "event_time")))
			    (< time-a time-b)))))
         (changed      #f)
	 (last-runs-update  (dboard:tabdat-last-runs-update tabdat))
	 (runs-dat     (mrmt:get-runs-by-patt (dboard:tabdat-keys tabdat) "%" #f #f #f #f 0)) ;; last-runs-update))
	 (runs-header  (vector-ref runs-dat 0)) ;; 0 is header, 1 is list of records
         (runs         (vector-ref runs-dat 1))
	 (new-run-ids  (map (lambda (run)
			      (db:get-value-by-header run runs-header "id"))
			    runs))
	 (areas        (configf:get-section *configdat* "areas")))
    (dboard:tabdat-last-runs-update-set! tabdat (- (current-seconds) 2))
    (for-each
     (lambda (area)
       (let ((run-path (list area)))
	 (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
	     (begin
	       (tree:add-node tb "Areas" run-path)
	       (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path 0)))))
     (map car areas))
    ;; here the local area
    (for-each
     (lambda (run-id)
       (let* ((run-record (hash-table-ref/default runs-hash run-id #f))
	      (key-vals   (map (lambda (key)(db:get-value-by-header run-record runs-header key))
			       (dboard:tabdat-keys tabdat)))
	      (run-name   (db:get-value-by-header run-record runs-header "runname"))
	      (col-name   (conc (string-intersperse key-vals "\n") "\n" run-name))
	      (run-path   (cons "local " (append key-vals (list run-name)))))
	 (if (not (hash-table-ref/default (dboard:tabdat-path-run-ids tabdat) run-path #f))
	     ;; (let ((existing   (tree:find-node tb run-path)))
	     ;;   (if (not existing)
	     (begin
	       (hash-table-set! (dboard:tabdat-run-keys tabdat) run-id run-path)
	       ;; (iup:attribute-set! (dboard:tabdat-runs-matrix tabdat)
	       ;;    		 (conc rownum ":" colnum) col-name)
	       ;; (hash-table-set! runid-to-col run-id (list colnum run-record))
	       ;; Here we update the tests treebox and tree keys
	       (tree:add-node tb "Areas" run-path) ;; (append key-vals (list run-name))
	       ;;                                             userdata: (conc "run-id: " run-id))))
	       (hash-table-set! (dboard:tabdat-path-run-ids tabdat) run-path run-id)
	       ;; (set! colnum (+ colnum 1))
	       ))))
     (append new-run-ids run-ids)))) ;; for-each run-id
  
(define (dashboard:areas-run-id->tests-mindat run-id tabdat runs-hash)
  (let* ((run          (hash-table-ref/default runs-hash run-id #f))
         (key-vals     (mrmt:get-key-vals run-id))
         (testnamepatt (or (dboard:tabdat-test-patts tabdat) "%/%"))
         (tests-ht     (dboard:get-tests-for-run-duplicate tabdat run-id run testnamepatt key-vals))
         (tests-dat    (dashboard:tests-ht->tests-dat tests-ht)) 
         (tests-mindat (dcommon:minimize-test-data tests-dat)))  ;; reduces data for display
    (dboard:tabdat-last-runs-update-set! tabdat (- (current-seconds) 2))
    (hash-table-set! (dboard:tabdat-last-test-dat tabdat) run-id tests-dat)
    (hash-table-set! (dboard:tabdat-run-update-times tabdat) run-id (- (current-seconds) 10))
    (when (not run)
        (debug:print-info 13 *default-log-port* "ERROR: NO RUN FOR RUN-ID run-id="run-id)
        (debug:print-info 13 *default-log-port* "runs-hash-> " (hash-table->alist runs-hash))
        )
    tests-mindat))

(define (dashboard:areas-runs-summary-xor-matrix-content tabdat runs-hash #!key (hide-clean #f))
  (let* ((src-run-id (dboard:tabdat-prev-run-id tabdat))
         (dest-run-id (dboard:tabdat-curr-run-id tabdat)))
    (if (and src-run-id dest-run-id)
        (dcommon:xor-tests-mindat 
         (dashboard:run-id->tests-mindat src-run-id tabdat runs-hash)
         (dashboard:run-id->tests-mindat dest-run-id tabdat runs-hash)
         hide-clean: hide-clean)
        #f)))

(define (dashboard:areas-popup-menu  run-id test-id target runname test-name testpatt item-test-path test-info)
  (iup:menu 
   (iup:menu-item
    "Test Control Panel"
    #:action
    (lambda (obj)
      (let* ((toolpath (car (argv)))
             (testpanel-cmd
              (conc toolpath " -test " run-id "," test-id " &")))
        (system testpanel-cmd)
        )))
   
   (iup:menu-item
    (conc "View Log " item-test-path)
    #:action
    (lambda (obj)
      (let* ((rundir    (db:test-get-rundir      test-info))
	     (logf      (db:test-get-final_logf  test-info))
	     (fullfile  (conc rundir "/" logf)))
	(if (common:file-exists? fullfile)
	    (dcommon:run-html-viewer fullfile)
	    (message-window (conc "file " fullfile " not found.")))))
    )
   (let* ((steps (tests:get-compressed-steps run-id test-id))   ;; #<stepname start end status Duration Logfile Comment id>
	  (rundir (db:test-get-rundir test-info)))
     (iup:menu-item
      "Step logs"
      (apply iup:menu
	     (map (lambda (step)
		    (let ((stepname (vector-ref step 0))
			  (logfile  (vector-ref step 5))
			  (status   (vector-ref step 3)))
		      (iup:menu-item
		       (conc stepname "/" (if (string=? logfile "") "no log!" logfile) " (" status ")")
		       #:action (lambda (obj)
				  (let ((fullfile (conc rundir "/" logfile)))
				    (if (common:file-exists? fullfile)
					(dcommon:run-html-viewer fullfile)
					(message-window (conc "file " fullfile " not found"))))))))
		  steps))))
   (iup:menu-item
    (conc "Rerun " item-test-path)
    #:action
    (lambda (obj)
      (common:run-a-command
       (conc "megatest -set-state-status NOT_STARTED,n/a -run -target " target
             " -runname " runname
             " -testpatt " item-test-path
             " -preclean -clean-cache"))))
   
   (iup:menu-item
    "Start xterm"
    #:action
    (lambda (obj)
      (dcommon:examine-xterm run-id test-id)))

   (iup:menu-item
    (conc "Kill " item-test-path)
    #:action
    (lambda (obj)
      ;; (mrmt:test-set-state-status-by-id run-id test-id "KILLREQ" #f #f)
      (common:run-a-command
       (conc "megatest -set-state-status KILLREQ,n/a -target " target
             " -runname " runname
             " -testpatt " item-test-path 
             " -state RUNNING,REMOTEHOSTSTART,LAUNCHED,NOT_STARTED"))))

   
   (iup:menu-item
    "Run"
    (iup:menu              
     (iup:menu-item
      (conc "Rerun " testpatt)
      #:action
      (lambda (obj)
        ;; (print  " run-id: " run-id " test-id: " test-id " target: " target " runname: " runname " test-name: " test-name " testpatt: " testpatt "item-path : " item-path)
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
               " -testpatt % "))))
     (iup:menu-item 
      "Kill Complete Run"
      #:action
      (lambda (obj)
        (common:run-a-command
         (conc "megatest -set-state-status KILLREQ,n/a -target " target
               " -runname " runname
               " -testpatt % "
               "  -state RUNNING,REMOTEHOSTSTART,LAUNCHED,NOT_STARTED"))))
     (iup:menu-item 
      "Delete Run Data"
      #:action
      (lambda (obj)
        (common:run-a-command
         (conc "megatest -remove-runs -target " target
               " -runname " runname
               " -testpatt % "
               "  -keep-records"))))))
   (iup:menu-item
    "Test"
    (iup:menu 
     (iup:menu-item
      (conc "Rerun " item-test-path)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -set-state-status NOT_STARTED,n/a -run -target " target
               " -runname " runname
	       " -testpatt " item-test-path
	       " -preclean -clean-cache"))))
     (iup:menu-item
      (conc "Kill " item-test-path)
      #:action
      (lambda (obj)
        ;; (mrmt:test-set-state-status-by-id run-id test-id "KILLREQ" #f #f)
	(common:run-a-command
	 (conc "megatest -set-state-status KILLREQ,n/a -target " target
               " -runname " runname
	       " -testpatt " item-test-path 
	       " -state RUNNING,REMOTEHOSTSTART,LAUNCHED"))))
     (iup:menu-item
      (conc "Delete data : " item-test-path)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -remove-runs -target " target
               " -runname " runname
	       " -testpatt " item-test-path 
	       " -keep-records"))))
     (iup:menu-item
      (conc "Clean "item-test-path)
      #:action
      (lambda (obj)
	(common:run-a-command
	 (conc "megatest -remove-runs -target " target
               " -runname " runname
	       " -testpatt " item-test-path))))
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


(define (dashboard:areas-get-youngest-run-db-mod-time dbdir)
  (handle-exceptions
   exn
   (begin
     (debug:print 2 *default-log-port* "WARNING: error in accessing databases in get-youngest-run-db-mod-time: " ((condition-property-accessor 'exn 'message) exn) " db-dir="dbdir)
     (current-seconds)) ;; something went wrong - just print an error and return current-seconds
   (common:max (map (lambda (filen)
		      (file-modification-time filen))
		    (glob (conc dbdir "/*.db*"))))))

(define (dashboard:areas-recalc modtime please-update-buttons last-db-update-time)
  (or please-update-buttons
      (and ;; (> (current-milliseconds)(+ *last-recalc-ended-time* 150)) ;; can't use this - it needs to be tab specific
	   (> modtime (- last-db-update-time 3)) ;; add three seconds of margin
	   (> (current-seconds)(+ last-db-update-time 1)))))

;; setup buttons and callbacks to switch between modes in runs summary tab
;;
(define (dashboard:areas-summary-control-panel tabdat)
  (let* ((summary-buttons ;; build buttons
          (map
           (lambda (mode-item)
             (let* ((this-mode (car mode-item))
                    (this-mode-label (cdr mode-item)))
               (iup:button this-mode-label
                           #:action
                           (lambda (obj)
                             (debug:catch-and-dump
                              (lambda ()
                                (dboard:tabdat-runs-summary-mode-set! tabdat this-mode)
                                (dboard:areas-summary-control-panel-updater tabdat))
                              "runs summary control panel updater")))))
           (dboard:tabdat-runs-summary-modes tabdat)))
         (summary-buttons-hbox (apply iup:hbox summary-buttons))
         (xor-runname-labels-hbox
          (iup:hbox
           (let ((temp-label
                  (iup:label "" #:size "125x15" #:fontsize "10" )))
             (dboard:tabdat-runs-summary-source-runname-label-set! tabdat temp-label)
             temp-label
             )
           (let ((temp-label
                  (iup:label "" #:size "125x15" #:fontsize "10")))
             (dboard:tabdat-runs-summary-dest-runname-label-set! tabdat temp-label)
             temp-label))))
    (dboard:tabdat-runs-summary-mode-buttons-set! tabdat summary-buttons)

    ;; maybe wrap in a frame
    (let ((res (iup:vbox summary-buttons-hbox xor-runname-labels-hbox )))
      (dboard:areas-summary-control-panel-updater tabdat)
      res
      )))

(define (dboard:areas-summary-control-panel-updater tabdat)
  (dboard:areas-summary-xor-labels-updater tabdat)
  (dboard:areas-summary-buttons-updater tabdat))

(define (dboard:areas-summary-xor-labels-updater tabdat)
  (let ((source-runname-label (dboard:tabdat-runs-summary-source-runname-label tabdat))
        (dest-runname-label (dboard:tabdat-runs-summary-dest-runname-label tabdat))
        (mode (dboard:tabdat-runs-summary-mode tabdat)))
    (when (and source-runname-label dest-runname-label)
      (case mode
        ((xor-two-runs xor-two-runs-hide-clean)
         (let* ((curr-run-id          (dboard:tabdat-curr-run-id tabdat))
                (prev-run-id          (dboard:tabdat-prev-run-id tabdat))
                (curr-runname (if curr-run-id
                                  (mrmt:get-run-name-from-id curr-run-id)
                                  "None"))
                (prev-runname (if prev-run-id
                                  (mrmt:get-run-name-from-id prev-run-id)
                                  "None")))
           (iup:attribute-set! source-runname-label "TITLE" (conc " SRC: "prev-runname"  "))
           (iup:attribute-set! dest-runname-label "TITLE" (conc "DEST: "curr-runname"  "))))
        (else
         (iup:attribute-set! source-runname-label "TITLE" "")
         (iup:attribute-set! dest-runname-label "TITLE" ""))))))

(define (dboard:areas-summary-buttons-updater tabdat)
  (let loop ((buttons-left (dboard:tabdat-runs-summary-mode-buttons tabdat))
             (modes-left (dboard:tabdat-runs-summary-modes tabdat)))
    (if (or (null? buttons-left) (null? modes-left))
        #t
        (let* ((this-button (car buttons-left))
               (mode-item (car modes-left))
               (this-mode (car mode-item))
               (sel-color    "180 100 100")
               (nonsel-color "170 170 170")
               (current-mode (dboard:tabdat-runs-summary-mode tabdat)))
          (if (eq? this-mode current-mode)
              (iup:attribute-set! this-button "BGCOLOR" sel-color)
              (iup:attribute-set! this-button "BGCOLOR" nonsel-color))
          (loop (cdr buttons-left) (cdr modes-left))))))

