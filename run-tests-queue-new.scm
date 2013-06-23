
;; test-records is a hash table testname:item_path => vector < testname testconfig waitons priority items-info ... >
(define (runs:run-tests-queue-new run-id runname test-records keyvals flags test-patts required-tests reglen-in)
  ;; At this point the list of parent tests is expanded 
  ;; NB// Should expand items here and then insert into the run queue.
  (debug:print 5 "test-records: " test-records ", flags: " (hash-table->alist flags))
  (let ((run-info              (cdb:remote-run db:get-run-info #f run-id))
	(tests-info            (mt:get-tests-for-run run-id #f '() '())) ;;  qryvals: "id,testname,item_path"))
	(sorted-test-names     (tests:sort-by-priority-and-waiton test-records))
	(test-registry         (make-hash-table))
	(registry-mutex        (make-mutex))
	(num-retries           0)
	(max-retries           (config-lookup *configdat* "setup" "maxretries"))
	(max-concurrent-jobs   (let ((mcj (config-lookup *configdat* "setup"     "max_concurrent_jobs")))
				 (if (and mcj (string->number mcj))
				     (string->number mcj)
				     1))) ;; length of the register queue ahead
	(reglen                (if (number? reglen-in) reglen-in 1)))
    ;; Initialize the test-registery hash with tests that already have a record
    (for-each (lambda (trec)
		(let ((id (db:test-get-id        trec))
		      (tn (db:test-get-testname  trec))
		      (ip (db:test-get-item-path trec))
		      (st (db:test-get-state     trec)))
		  (hash-table-set! test-registry (runs:make-full-test-name tn ip) (string->symbol st))))
	      tests-info)
    (set! max-retries (if (and max-retries (string->number max-retries))(string->number max-retries) 100))
    (if (not (null? sorted-test-names))
	(let loop ((hed         (car sorted-test-names))
		   (tal         (cdr sorted-test-names))
		   (reg         '()) ;; registered, put these at the head of tal 
		   (reruns      '()))
	  (if (not (null? reruns))(debug:print-info 4 "reruns=" reruns))
	  ;; (print "Top of loop, hed=" hed ", tal=" tal " ,reruns=" reruns)
	  (let* ((test-record (hash-table-ref test-records hed))
		 (test-name   (tests:testqueue-get-testname test-record))
		 (tconfig     (tests:testqueue-get-testconfig test-record))
		 (jobgroup    (config-lookup tconfig "requirements" "jobgroup"))
		 (testmode    (let ((m (config-lookup tconfig "requirements" "mode")))
				(if m (string->symbol m) 'normal)))
		 (waitons     (tests:testqueue-get-waitons    test-record))
		 (priority    (tests:testqueue-get-priority   test-record))
		 (itemdat     (tests:testqueue-get-itemdat    test-record)) ;; itemdat can be a string, list or #f
		 (items       (tests:testqueue-get-items      test-record))
		 (item-path   (item-list->path itemdat))
		 (tfullname   (runs:make-full-test-name test-name item-path))
		 (newtal      (append tal (list hed)))
		 (regfull     (> (length reg) reglen)))

	    ;; Fast skip of tests that are already "COMPLETED"
	    (if (equal? (hash-table-ref/default test-registry tfullname #f) 'COMPLETED)
		(begin
		  (debug:print-info 0 "Skipping COMPLETED test " tfullname)
		  (if (not (null? tal))
		      (loop (car tal)(cdr tal) reg reruns))))
	    ;; (if (> (length reg) 10)
	    ;;     (begin
	    ;;       (set! tal (cons hed tal))
	    ;;       (set! hed (car reg))
	    ;;       (set! reg (cdr reg))
	    ;;       (set! newtal tal)))
	    (debug:print 6
			 "test-name: " test-name
			 "\n  hed:         " hed
			 "\n  itemdat:     " itemdat
			 "\n  items:       " items
			 "\n  item-path:   " item-path
			 "\n  waitons:     " waitons
			 "\n  num-retries: " num-retries
			 "\n  tal:         " tal
			 "\n  reruns:      " reruns)

	    ;; check for hed in waitons => this would be circular, remove it and issue an
	    ;; error
	    (if (member test-name waitons)
		(begin
		  (debug:print 0 "ERROR: test " test-name " has listed itself as a waiton, please correct this!")
		  (set! waiton (filter (lambda (x)(not (equal? x hed))) waitons))))

	    (cond ;; OUTER COND
	     ((not items) ;; when false the test is ok to be handed off to launch (but not before)
	      (if (and (not (tests:match test-patts (tests:testqueue-get-testname test-record) item-path required: required-tests))
	               (not (null? tal)))
               ;; This was (car tal)(cdr tal) in new but (car newtal)(cdr newtal) in classic
	          (loop (car newtal)(cdr newtal) reg reruns))
	      (let* ((run-limits-info         (cdb:remote-run runs:can-run-more-tests #f jobgroup max-concurrent-jobs)) ;; look at the test jobgroup and tot jobs running
		      ;; (open-run-close runs:can-run-more-tests #f jobgroup max-concurrent-jobs)) ;; look at the test jobgroup and tot jobs running
		     (have-resources          (car run-limits-info))
		     (num-running             (list-ref run-limits-info 1))
		     (num-running-in-jobgroup (list-ref run-limits-info 2))
		     (max-concurrent-jobs     (list-ref run-limits-info 3))
		     (job-group-limit         (list-ref run-limits-info 4))
		     (prereqs-not-met         (mt:get-prereqs-not-met run-id waitons item-path mode: testmode))
		     (fails                   (runs:calc-fails prereqs-not-met))
		     (non-completed           (runs:calc-not-completed prereqs-not-met)))
		(debug:print-info 8 "have-resources: " have-resources " prereqs-not-met: " 
			     (string-intersperse 
			      (map (lambda (t)
				     (if (vector? t)
					 (conc (db:test-get-state t) "/" (db:test-get-status t))
					 (conc " WARNING: t is not a vector=" t )))
				   prereqs-not-met) ", ") " fails: " fails)
		(debug:print-info 4 "hed=" hed "\n  test-record=" test-record "\n  test-name: " test-name "\n  item-path: " item-path "\n  test-patts: " test-patts)

		;; Don't know at this time if the test have been launched at some time in the past
		;; i.e. is this a re-launch?
		(debug:print-info 4 "run-limits-info = " run-limits-info)
		(cond ;; INNER COND #1 for a launchable test
		 ;; Check item path against item-patts
		 ((not (tests:match test-patts (tests:testqueue-get-testname test-record) item-path required: required-tests)) ;; This test/itempath is not to be run
		  ;; else the run is stuck, temporarily or permanently
		  ;; but should check if it is due to lack of resources vs. prerequisites
		  (debug:print-info 1 "Skipping " (tests:testqueue-get-testname test-record) " " item-path " as it doesn't match " test-patts)
		  ;; (thread-sleep! *global-delta*)
		  (if (not (null? tal))
		      (loop (runs:queue-next-hed tal reg reglen regfull)
			    (runs:queue-next-tal tal reg reglen regfull)
			    (runs:queue-next-reg tal reg reglen regfull)
			    reruns)))
		 ;; Registry has been started for this test but has not yet completed
		 ;; this should be rare, the case where there are only a couple of tests and the db is slow
		 ;; delay a short while and continue
		 ;; ((eq? (hash-table-ref/default test-registry (runs:make-full-test-name test-name item-path) #f) 'start)
		 ;;  (thread-sleep! 0.01)
		 ;;  (loop (car newtal)(cdr newtal) reruns))
		 ;; count number of 'done, if more than 100 then skip on through.
		 ((not (hash-table-ref/default test-registry (runs:make-full-test-name test-name item-path) #f)) ;; ) ;; too many changes required. Implement later.
		  (debug:print-info 4 "Pre-registering test " test-name "/" item-path " to create placeholder" )
		  (let ((th (make-thread (lambda ()
		        		   (mutex-lock! registry-mutex)
		        		   (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'start)
		        		   (mutex-unlock! registry-mutex)
					   ;; If haven't done it before register a top level test if this is an itemized test
					   (if (not (eq? (hash-table-ref/default test-registry (runs:make-full-test-name test-name "") #f) 'done))
					       (cdb:tests-register-test *runremote* run-id test-name ""))
					   (cdb:tests-register-test *runremote* run-id test-name item-path)
		        		   (mutex-lock! registry-mutex)
					   (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'done)
		        		   (mutex-unlock! registry-mutex))
		        		 (conc test-name "/" item-path))))
		    (thread-start! th))
		  (cdb:remote-run runs:shrink-can-run-more-tests-count #f)   ;; DELAY TWEAKER (still needed?)
		  (if (and (null? tal)(null? reg))
              ;; What is the logic here? Why redo the loop with the same variable contents?
		      (loop hed tal reg reruns)
		      (loop (runs:queue-next-hed tal reg reglen regfull)
			    (runs:queue-next-tal tal reg reglen regfull)
			    (let ((newl (append reg (list hed))))
			      (if regfull 
				  (cdr newl)
				  newl))
			    reruns)))
		 ;; At this point hed test registration must be completed.
		 ((eq? (hash-table-ref/default test-registry (runs:make-full-test-name test-name item-path) #f)
		       'start)
		  (debug:print-info 0 "Waiting on test registration(s): " (string-intersperse 
									   (filter (lambda (x)
										     (eq? (hash-table-ref/default test-registry x #f) 'start))
										   (hash-table-keys test-registry))
									   ", "))
		  (thread-sleep! 0.1)
		  (loop hed tal reg reruns))
		 ((not have-resources) ;; simply try again after waiting a second
		  (debug:print-info 1 "no resources to run new tests, waiting ...")
		  ;; Have gone back and forth on this but db starvation is an issue.
		  ;; wait one second before looking again to run jobs.
		  (thread-sleep! 1) ;; (+ 2 *global-delta*))
		  ;; could have done hed tal here but doing car/cdr of newtal to rotate tests
		  (loop (car newtal)(cdr newtal) reg reruns))
		 ((and have-resources
		       (or (null? prereqs-not-met)
			   (and (eq? testmode 'toplevel)
				(null? non-completed))))
		  (run:test run-id run-info keyvals runname test-record flags #f)
		  (hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'running)
		  (cdb:remote-run runs:shrink-can-run-more-tests-count #f)  ;; DELAY TWEAKER (still needed?)
		  ;; (thread-sleep! *global-delta*)
		  (if (not (null? tal))
		      (loop (runs:queue-next-hed tal reg reglen regfull)
			    (runs:queue-next-tal tal reg reglen regfull)
			    (runs:queue-next-reg tal reg reglen regfull)
			    reruns)))
		 (else ;; must be we have unmet prerequisites
		  (debug:print 4 "FAILS: " fails)
		  ;; If one or more of the prereqs-not-met are FAIL then we can issue
		  ;; a message and drop hed from the items to be processed.
		  (if (null? fails)
		      (begin
			;; couldn't run, take a breather
			(debug:print-info 4 "Shouldn't really get here, race condition? Unable to launch more tests at this moment, killing time ...")
			;; (thread-sleep! (+ 0.01 *global-delta*)) ;; long sleep here - no resources, may as well be patient
			;; we made new tal by sticking hed at the back of the list
			(loop (car newtal)(cdr newtal) reg reruns))
		      ;; the waiton is FAIL so no point in trying to run hed ever again
		      (if (not (null? tal))
			  (if (vector? hed)
			      (begin 
				(debug:print 1 "WARN: Dropping test " (db:test-get-testname hed) "/" (db:test-get-item-path hed)
					     " from the launch list as it has prerequistes that are FAIL")
				(cdb:remote-run runs:shrink-can-run-more-tests-count #f) ;; DELAY TWEAKER (still needed?)
				;; (thread-sleep! *global-delta*)
				(hash-table-set! test-registry (runs:make-full-test-name test-name item-path) 'removed)
				(loop (runs:queue-next-hed tal reg reglen regfull)
				      (runs:queue-next-tal tal reg reglen regfull)
				      (runs:queue-next-reg tal reg reglen regfull)
				      (cons hed reruns)))
			      (begin
				(debug:print 1 "WARN: Test not processed correctly. Could be a race condition in your test implementation? " hed) ;;  " as it has prerequistes that are FAIL. (NOTE: hed is not a vector)")
				(cdb:remote-run runs:shrink-can-run-more-tests-count #f) ;; DELAY TWEAKER (still needed?)
				;; (thread-sleep! (+ 0.01 *global-delta*))
				(loop hed tal reg reruns))))))))) ;; END OF INNER COND
	     
	     ;; case where an items came in as a list been processed
	     ((and (list? items)     ;; thus we know our items are already calculated
		   (not   itemdat)) ;; and not yet expanded into the list of things to be done
	      (if (and (debug:debug-mode 1) ;; (>= *verbosity* 1)
		       (> (length items) 0)
		       (> (length (car items)) 0))
		  (pp items))
	      (for-each
	       (lambda (my-itemdat)
		 (let* ((new-test-record (let ((newrec (make-tests:testqueue)))
					   (vector-copy! test-record newrec)
					   newrec))
			(my-item-path (item-list->path my-itemdat)))
		   (if (tests:match test-patts hed my-item-path required: required-tests) ;; (patt-list-match my-item-path item-patts)           ;; yes, we want to process this item, NOTE: Should not need this check here!
		       (let ((newtestname (runs:make-full-test-name hed my-item-path)))    ;; test names are unique on testname/item-path
			 (tests:testqueue-set-items!     new-test-record #f)
			 (tests:testqueue-set-itemdat!   new-test-record my-itemdat)
			 (tests:testqueue-set-item_path! new-test-record my-item-path)
			 (hash-table-set! test-records newtestname new-test-record)
			 (set! tal (cons newtestname tal)))))) ;; since these are itemized create new test names testname/itempath
	       items)
	      (if (not (null? tal))
		  (begin
		    (debug:print-info 4 "End of items list, looping with next after short delay")
		    ;; (thread-sleep! (+ 0.01 *global-delta*))
		    (loop (runs:queue-next-hed tal reg reglen regfull)
			  (runs:queue-next-tal tal reg reglen regfull)
			  (runs:queue-next-reg tal reg reglen regfull)
			  reruns))))

	     ;; if items is a proc then need to run items:get-items-from-config, get the list and loop 
	     ;;    - but only do that if resources exist to kick off the job
	     ((or (procedure? items)(eq? items 'have-procedure))
	      (let ((can-run-more    (cdb:remote-run runs:can-run-more-tests #f jobgroup max-concurrent-jobs)))
		(if (and (list? can-run-more)
			 (car can-run-more))
		    (let* ((prereqs-not-met (mt:get-prereqs-not-met run-id waitons item-path mode: testmode))
			   (fails           (runs:calc-fails prereqs-not-met))
			   (non-completed   (runs:calc-not-completed prereqs-not-met)))
		      (debug:print-info 8 "can-run-more: " can-run-more
					"\n testname:        " hed
					"\n prereqs-not-met: " (runs:pretty-string prereqs-not-met)
					"\n non-completed:   " (runs:pretty-string non-completed) 
					"\n fails:           " (runs:pretty-string fails)
					"\n testmode:        " testmode
					"\n num-retries:     " num-retries
					"\n (eq? testmode 'toplevel): " (eq? testmode 'toplevel)
					"\n (null? non-completed):    " (null? non-completed)
					"\n reruns:          " reruns
					"\n items:           " items
					"\n can-run-more:    " can-run-more)
		      ;; (thread-sleep! (+ 0.01 *global-delta*))
		      (cond ;; INNER COND #2
		       ((or (null? prereqs-not-met) ;; all prereqs met, fire off the test
			    ;; or, if it is a 'toplevel test and all prereqs not met are COMPLETED then launch
			    (and (eq? testmode 'toplevel)
				 (null? non-completed)))
			(let ((test-name (tests:testqueue-get-testname test-record)))
			  (setenv "MT_TEST_NAME" test-name) ;; 
			  (setenv "MT_RUNNAME"   runname)
			  (set-megatest-env-vars run-id inrunname: runname) ;; these may be needed by the launching process
			  (let ((items-list (items:get-items-from-config tconfig)))
			    (if (list? items-list)
				(begin
				  (tests:testqueue-set-items! test-record items-list)
				  ;; (thread-sleep! *global-delta*)
				  (loop hed tal reg reruns))
				(begin
				  (debug:print 0 "ERROR: The proc from reading the setup did not yield a list - please report this")
				  (exit 1))))))
		       ((null? fails)
			(debug:print-info 4 "fails is null, moving on in the queue but keeping " hed " for now")
			;; only increment num-retries when there are no tests runing
			(if (eq? 0 (list-ref can-run-more 1))
			    (begin
			      ;; TRY (if (> num-retries 100) ;; first 100 retries are low time cost
			      ;; TRY     (thread-sleep! (+ 2 *global-delta*))
			      ;; TRY     (thread-sleep! (+ 0.01 *global-delta*)))
			      (set! num-retries (+ num-retries 1))))
			(if (> num-retries  max-retries)
			    (if (not (null? tal))
				(loop (runs:queue-next-hed tal reg reglen regfull)
				      (runs:queue-next-tal tal reg reglen regfull)
				      (runs:queue-next-reg tal reg reglen regfull)
				      reruns))
			    (loop (car newtal)(cdr newtal) reg reruns))) ;; an issue with prereqs not yet met?
		       ((and (not (null? fails))(eq? testmode 'normal))
			(debug:print-info 1 "test "  hed " (mode=" testmode ") has failed prerequisite(s); "
				     (string-intersperse (map (lambda (t)(conc (db:test-get-testname t) ":" (db:test-get-state t)"/"(db:test-get-status t))) fails) ", ")
				     ", removing it from to-do list")
			(if (not (null? tal))
			    (begin
			      ;; (thread-sleep! *global-delta*)
			      (loop (runs:queue-next-hed tal reg reglen regfull)
				    (runs:queue-next-tal tal reg reglen regfull)
				    (runs:queue-next-reg tal reg reglen regfull)
				    (cons hed reruns)))))
		       (else
			(debug:print 8 "ERROR: No handler for this condition.")
			;; TRY (thread-sleep! (+ 1 *global-delta*))
			(loop (car newtal)(cdr newtal) reg reruns)))) ;; END OF IF CAN RUN MORE

		    ;; if can't run more just loop with next possible test
		    (begin
		      (debug:print-info 4 "processing the case with a lambda for items or 'have-procedure. Moving through the queue without dropping " hed)
		      ;; (thread-sleep! (+ 2 *global-delta*))
		      (loop (car newtal)(cdr newtal) reg reruns))))) ;; END OF (or (procedure? items)(eq? items 'have-procedure))
	     
	     ;; this case should not happen, added to help catch any bugs
	     ((and (list? items) itemdat)
	      (debug:print 0 "ERROR: Should not have a list of items in a test and the itemspath set - please report this")
	      (exit 1))
	     ((not (null? reruns))
	      (let* ((newlst (tests:filter-non-runnable run-id tal test-records)) ;; i.e. not FAIL, WAIVED, INCOMPLETE, PASS, KILLED,
		     (junked (lset-difference equal? tal newlst)))
		(debug:print-info 4 "full drop through, if reruns is less than 100 we will force retry them, reruns=" reruns ", tal=" tal)
		(if (< num-retries max-retries)
		    (set! newlst (append reruns newlst)))
		(set! num-retries (+ num-retries 1))
		;; (thread-sleep! (+ 1 *global-delta*))
		(if (not (null? newlst))
		    ;; since reruns have been tacked on to newlst create new reruns from junked
		    (loop (car newlst)(cdr newlst) reg (delete-duplicates junked)))))
	     ((not (null? tal))
	      (debug:print-info 4 "I'm pretty sure I shouldn't get here."))
	     ((not (null? reg)) ;; could we get here with leftovers?
	      (debug:print-info 0 "Have leftovers!")
	      (loop (car reg)(cdr reg) '() reruns))
	     (else
	      (debug:print-info 4 "Exiting loop with...\n  hed=" hed "\n  tal=" tal "\n  reruns=" reruns))
	     )))) ;; LET* ((test-record

    ;; we get here on "drop through" - loop for next test in queue
    ;; FIXME!!!! THIS SHOULD NOT REQUIRE AN EXIT!!!!!!!
    
    (debug:print-info 1 "All tests launched")
    (thread-sleep! 0.5)
    ;; FIXME! This harsh exit should not be necessary....
    ;; (if (not *runremote*)(exit)) ;; 
    #f)) ;; return a #f as a hint that we are done
;; Here we need to check that all the tests remaining to be run are eligible to run
;; and are not blocked by failed

