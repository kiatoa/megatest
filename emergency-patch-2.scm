(include "test_records.scm")

(define (common:wait-for-cpuload maxload numcpus waitdelay #!key (count 1000) (msg #f)(remote-host #f))
  (let* ((loadavg (common:get-cpu-load remote-host))
	 (first   (car loadavg))
	 (next    (cadr loadavg))
	 (adjload (* maxload numcpus))
	 (loadjmp (- first next)))
    (cond
     ((and (> first adjload)
	   (> count 0))
      (debug:print-info 0 *default-log-port* "waiting " waitdelay " seconds due to load " first " exceeding max of " adjload " " (if msg msg ""))
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1)))
     ((and (> loadjmp numcpus)
	   (> count 0))
      (debug:print-info 0 *default-log-port* "waiting " waitdelay " seconds due to load jump " loadjmp " > numcpus " numcpus (if msg msg ""))
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1))))))

(define (common:wait-for-homehost-load maxload msg)
  (let* ((hh-dat (if (common:on-homehost?) ;; if we are on the homehost then pass in #f so the calls are local.
                     #f
                     (common:get-homehost)))
         (hh     (if hh-dat (car hh-dat) #f))
         (numcpus (common:get-num-cpus hh)))
    (common:wait-for-normalized-load maxload msg: msg remote-host: hh)))

;; wait for normalized cpu load to drop below maxload
;;
(define (common:wait-for-normalized-load maxload #!key (msg #f)(remote-host #f))
  (let ((num-cpus (common:get-num-cpus remote-host)))
    (common:wait-for-cpuload maxload num-cpus 15 msg: msg remote-host: remote-host)))

;;  hed tal reg reruns reglen regfull test-record runname test-name item-path jobgroup max-concurrent-jobs run-id waitons item-path testmode test-patts required-tests test-registry registry-mutex flags keyvals run-info newtal all-tests-registry itemmaps)
(define (runs:process-expanded-tests runsdat testdat)
  ;; unroll the contents of runsdat and testdat (due to ongoing refactoring).
  (let* ((hed                    (runs:testdat-hed testdat))
	 (tal                    (runs:testdat-tal testdat))
	 (reg                    (runs:testdat-reg testdat))
	 (reruns                 (runs:testdat-reruns testdat))
	 (test-name              (runs:testdat-test-name testdat))
	 (item-path              (runs:testdat-item-path testdat))
	 (jobgroup               (runs:testdat-jobgroup testdat))
	 (waitons                (runs:testdat-waitons testdat))
	 (item-path              (runs:testdat-item-path testdat))
	 (testmode               (runs:testdat-testmode testdat))
	 (newtal                 (runs:testdat-newtal testdat))
	 (itemmaps               (runs:testdat-itemmaps testdat))
	 (test-record            (runs:testdat-test-record testdat))
	 (prereqs-not-met        (runs:testdat-prereqs-not-met testdat))

	 (reglen                 (runs:dat-reglen runsdat))
	 (regfull                (runs:dat-regfull runsdat))
	 (runname                (runs:dat-runname runsdat))
	 (max-concurrent-jobs    (runs:dat-max-concurrent-jobs runsdat))
	 (run-id                 (runs:dat-run-id runsdat))
	 (test-patts             (runs:dat-test-patts runsdat))
	 (required-tests         (runs:dat-required-tests runsdat))
	 (test-registry          (runs:dat-test-registry runsdat))
	 (registry-mutex         (runs:dat-registry-mutex runsdat))
	 (flags                  (runs:dat-flags runsdat))
	 (keyvals                (runs:dat-keyvals runsdat))
	 (run-info               (runs:dat-run-info runsdat))
	 (all-tests-registry     (runs:dat-all-tests-registry runsdat))
	 (run-limits-info        (runs:dat-can-run-more-tests runsdat))
	 ;; (runs:can-run-more-tests run-id jobgroup max-concurrent-jobs)) ;; look at the test jobgroup and tot jobs running
	 (have-resources         (car run-limits-info))
	 (num-running            (list-ref run-limits-info 1))
	 (num-running-in-jobgroup(list-ref run-limits-info 2)) 
	 (max-concurrent-jobs    (list-ref run-limits-info 3))
	 (job-group-limit        (list-ref run-limits-info 4))
	 ;; (prereqs-not-met        (rmt:get-prereqs-not-met run-id waitons hed item-path mode: testmode itemmaps: itemmaps))
	 ;; (prereqs-not-met         (mt:lazy-get-prereqs-not-met run-id waitons item-path mode: testmode itemmap: itemmap))
	 (fails                  (if (list? prereqs-not-met)
				      (runs:calc-fails prereqs-not-met)
				      (begin
					(debug:print-error 0 *default-log-port* "prereqs-not-met is not a list! " prereqs-not-met)
					'())))
	 (non-completed           (filter (lambda (x)             ;; remove hed from not completed list, duh, of course it is not completed!
					    (not (equal? x hed)))
					  (runs:calc-not-completed prereqs-not-met)))
	 (loop-list               (list hed tal reg reruns))
	 ;; configure the load runner
	 (numcpus                 (common:get-num-cpus #f))
	 (maxload                 (string->number (or (configf:lookup *configdat* "jobtools" "maxload") "3.0")))         ;; use a non-number string to disable
         (maxhomehostload         (string->number (or (configf:lookup *configdat* "jobtools" "maxhomehostload") "1.2"))) ;; use a non-number string to disable
         (waitdelay               (string->number (or (configf:lookup *configdat* "jobtools" "waitdelay") "60"))))
    (debug:print-info 4 *default-log-port* "have-resources: " have-resources " prereqs-not-met: (" 
		      (string-intersperse 
		       (map (lambda (t)
			      (if (vector? t)
				  (conc (db:test-get-state t) "/" (db:test-get-status t))
				  (conc " WARNING: t is not a vector=" t )))
			    prereqs-not-met)
		       ", ") ") fails: " fails
		       "\nregistered? " (hash-table-ref/default test-registry (db:test-make-full-name test-name item-path) #f))
			    

    
    (if (and (not (null? prereqs-not-met))
	     (runs:lownoise (conc "waiting on tests " prereqs-not-met hed) 60))
	(debug:print-info 2 *default-log-port* "waiting on tests; " (string-intersperse (runs:mixed-list-testname-and-testrec->list-of-strings prereqs-not-met) ", ")))

    ;; Don't know at this time if the test have been launched at some time in the past
    ;; i.e. is this a re-launch?
    (debug:print-info 4 *default-log-port* "run-limits-info = " run-limits-info)
    
    (cond
     
     ;; Check item path against item-patts, 
     ;;
     ((not (tests:match test-patts (tests:testqueue-get-testname test-record) item-path required: required-tests)) ;; This test/itempath is not to be run
      ;; else the run is stuck, temporarily or permanently
      ;; but should check if it is due to lack of resources vs. prerequisites
      (debug:print-info 1 *default-log-port* "Skipping " (tests:testqueue-get-testname test-record) " " item-path " as it doesn't match " test-patts)
      (if (or (not (null? tal))(not (null? reg)))
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		(runs:queue-next-reg tal reg reglen regfull)
		reruns)
	  #f))
     
     ;; Register tests 
     ;;
     ((not (hash-table-ref/default test-registry (db:test-make-full-name test-name item-path) #f))
      (debug:print-info 4 *default-log-port* "Pre-registering test " test-name "/" item-path " to create placeholder" )
      ;; always do firm registration now in v1.60 and greater ;; (eq? *transport-type* 'fs) ;; no point in parallel registration if use fs
      (let register-loop ((numtries 15))
	(rmt:register-test run-id test-name item-path)
	(if (rmt:get-test-id run-id test-name item-path)
	    (hash-table-set! test-registry (db:test-make-full-name test-name item-path) 'done)
	    (if (> numtries 0)
		(begin
		  (thread-sleep! 0.5)
		  (register-loop (- numtries 1)))
		(debug:print-error 0 *default-log-port* "failed to register test " (db:test-make-full-name test-name item-path)))))
      (if (not (eq? (hash-table-ref/default test-registry (db:test-make-full-name test-name "") #f) 'done))
	  (begin
	    (rmt:register-test run-id test-name "")
	    (if (rmt:get-test-id run-id test-name "")
		(hash-table-set! test-registry (db:test-make-full-name test-name "") 'done))))
      (runs:shrink-can-run-more-tests-count runsdat)   ;; DELAY TWEAKER (still needed?)
      (if (and (null? tal)(null? reg))
	  (list hed tal (append reg (list hed)) reruns)
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		;; NB// Here we are building reg as we register tests
		;; if regfull we must pop the front item off reg
		(if regfull
		    (append (cdr reg) (list hed))
		    (append reg (list hed)))
		reruns)))
     
     ;; At this point hed test registration must be completed.
     ;;
     ((eq? (hash-table-ref/default test-registry (db:test-make-full-name test-name item-path) #f)
	   'start)
      (debug:print-info 0 *default-log-port* "Waiting on test registration(s): "
			(string-intersperse 
			 (filter (lambda (x)
				   (eq? (hash-table-ref/default test-registry x #f) 'start))
				 (hash-table-keys test-registry))
			 ", "))
      (thread-sleep! 0.051)
      (list hed tal reg reruns))
     
     ;; If no resources are available just kill time and loop again
     ;;
     ((not have-resources) ;; simply try again after waiting a second
      (if (runs:lownoise "no resources" 60)
	  (debug:print-info 1 *default-log-port* "no resources to run new tests, waiting ..."))
      ;; Have gone back and forth on this but db starvation is an issue.
      ;; wait one second before looking again to run jobs.
      (thread-sleep! 1)
      ;; could have done hed tal here but doing car/cdr of newtal to rotate tests
      (list (car newtal)(cdr newtal) reg reruns))
     
     ;; This is the final stage, everything is in place so launch the test
     ;;
     ((and have-resources
	   (or (null? prereqs-not-met)
	       (and (member 'toplevel testmode) ;;  'toplevel)
		    (null? non-completed)
		    (not (member 'exclusive testmode)))))
      ;; (hash-table-delete! *max-tries-hash* (db:test-make-full-name test-name item-path))
      ;; we are going to reset all the counters for test retries by setting a new hash table
      ;; this means they will increment only when nothing can be run
      (set! *max-tries-hash* (make-hash-table))
      ;; well, first lets see if cpu load throttling is enabled. If so wait around until the
      ;; average cpu load is under the threshold before continuing
      (if maxload ;; only gate if maxload is specified
          (common:wait-for-cpuload maxload numcpus waitdelay))
      (if maxhomehostload
          (common:wait-for-homehost-load maxhomehostload (conc "Waiting for homehost load to drop below normalized value of " maxhomehostload)))
      
      (run:test run-id run-info keyvals runname test-record flags #f test-registry all-tests-registry)
      (runs:incremental-print-results run-id)
      (hash-table-set! test-registry (db:test-make-full-name test-name item-path) 'running)
      (runs:shrink-can-run-more-tests-count runsdat)  ;; DELAY TWEAKER (still needed?)
      ;; (thread-sleep! *global-delta*)
      (if (or (not (null? tal))(not (null? reg)))
	  (list (runs:queue-next-hed tal reg reglen regfull)
		(runs:queue-next-tal tal reg reglen regfull)
		(runs:queue-next-reg tal reg reglen regfull)
		reruns)
	  #f))
     
     ;; must be we have unmet prerequisites
     ;;
     (else
      (debug:print 4 *default-log-port* "FAILS: " fails)
      ;; If one or more of the prereqs-not-met are FAIL then we can issue
      ;; a message and drop hed from the items to be processed.
      ;; (runs:mixed-list-testname-and-testrec->list-of-strings prereqs-not-met)
      (if (and (not (null? prereqs-not-met))
	       (runs:lownoise (conc "waiting on tests " prereqs-not-met hed) 60))
	  (debug:print-info 1 *default-log-port* "waiting on tests; " (string-intersperse 
						    (runs:mixed-list-testname-and-testrec->list-of-strings 
						     prereqs-not-met) ", ")))
      (if (or (null? fails)
	      (member 'toplevel testmode))
	  (begin
	    ;; couldn't run, take a breather
	    (if  (runs:lownoise "Waiting for more work to do..." 60)
		 (debug:print-info 0 *default-log-port* "Waiting for more work to do..."))
	    (thread-sleep! 1)
	    (list (car newtal)(cdr newtal) reg reruns))
	  ;; the waiton is FAIL so no point in trying to run hed ever again
	  (if (or (not (null? reg))(not (null? tal)))
	      (if (vector? hed)
		  (begin
		    (debug:print 1 *default-log-port* "WARNING: Dropping test " test-name "/" item-path
				 " from the launch list as it has prerequistes that are FAIL")
		    (let ((test-id (rmt:get-test-id run-id hed "")))
		      (if test-id (mt:test-set-state-status-by-id run-id test-id "NOT_STARTED" "PREQ_FAIL" "Failed to run due to failed prerequisites")))
		    (runs:shrink-can-run-more-tests-count runsdat) ;; DELAY TWEAKER (still needed?)
		    ;; (thread-sleep! *global-delta*)
		    ;; This next is for the items
		    (mt:test-set-state-status-by-testname run-id test-name item-path "NOT_STARTED" "BLOCKED" #f)
		    (hash-table-set! test-registry (db:test-make-full-name test-name item-path) 'removed)
		    (list (runs:queue-next-hed tal reg reglen regfull)
			  (runs:queue-next-tal tal reg reglen regfull)
			  (runs:queue-next-reg tal reg reglen regfull)
			  reruns ;; WAS: (cons hed reruns) ;; but that makes no sense?
			  ))
		  (let ((nth-try (hash-table-ref/default test-registry hed 0)))
		    (cond
		     ((member "RUNNING" (map db:test-get-state prereqs-not-met))
		      (if (runs:lownoise (conc "possible RUNNING prerequistes " hed) 60)
			  (debug:print 0 *default-log-port* "WARNING: test " hed " has possible RUNNING prerequisites, don't give up on it yet."))
		      (thread-sleep! 4)
		      (list (runs:queue-next-hed newtal reg reglen regfull)
			    (runs:queue-next-tal newtal reg reglen regfull)
			    (runs:queue-next-reg newtal reg reglen regfull)
			    reruns))
		     ((or (not nth-try)
			  (and (number? nth-try)
			       (< nth-try 10)))
		      (hash-table-set! test-registry hed (if (number? nth-try)
							     (+ nth-try 1)
							     0))
		      (if (runs:lownoise (conc "not removing test " hed) 60)
			  (debug:print 1 *default-log-port* "WARNING: not removing test " hed " from queue although it may not be runnable due to FAILED prerequisites"))
		      ;; may not have processed correctly. Could be a race condition in your test implementation? Dropping test " hed) ;;  " as it has prerequistes that are FAIL. (NOTE: hed is not a vector)")
		      (runs:shrink-can-run-more-tests-count runsdat) ;; DELAY TWEAKER (still needed?)
		      ;; (list hed tal reg reruns)
		      ;; (list (car newtal)(cdr newtal) reg reruns)
		      ;; (hash-table-set! test-registry hed 'removed)
		      (list (runs:queue-next-hed newtal reg reglen regfull)
			    (runs:queue-next-tal newtal reg reglen regfull)
			    (runs:queue-next-reg newtal reg reglen regfull)
			    reruns))
		     ((symbol? nth-try)
		      (if (eq? nth-try 'removed) ;; removed is removed - drop it NOW
			  (if (null? tal)
			      #f ;; yes, really
			      (list (car tal)(cdr tal) reg reruns))
			  (begin
			    (if (runs:lownoise (conc "FAILED prerequisites or other issue" hed) 60)
				(debug:print 0 *default-log-port* "WARNING: test " hed " has FAILED prerequisites or other issue. Internal state " nth-try " will be overridden and we'll retry."))
			    (mt:test-set-state-status-by-testname run-id test-name item-path "NOT_STARTED" "KEEP_TRYING" #f)
			    (hash-table-set! test-registry hed 0)
			    (list (runs:queue-next-hed newtal reg reglen regfull)
				  (runs:queue-next-tal newtal reg reglen regfull)
				  (runs:queue-next-reg newtal reg reglen regfull)
				  reruns))))
		     (else
		      (if (runs:lownoise (conc "FAILED prerequitests and we tried" hed) 60)
			  (debug:print 0 *default-log-port* "WARNING: test " hed " has FAILED prerequitests and we've tried at least 10 times to run it. Giving up now."))
		      ;; (debug:print 0 *default-log-port* "         prereqs: " prereqs-not-met)
		      (hash-table-set! test-registry hed 'removed)
		      (mt:test-set-state-status-by-testname run-id test-name item-path "NOT_STARTED" "TEN_STRIKES" #f)
		      ;; I'm unclear on if this roll up is needed - it may be the root cause of the "all set to FAIL" bug.
		      (rmt:set-state-status-and-roll-up-items run-id test-name item-path #f "FAIL" #f) ;; treat as FAIL
		      (list (if (null? tal)(car newtal)(car tal))
			    tal
			    reg
			    reruns)))))
	      ;; can't drop this - maybe running? Just keep trying
	      (let ((runable-tests (runs:runable-tests prereqs-not-met)))
		(if (null? runable-tests)
		    #f   ;; I think we are truly done here
		    (list (runs:queue-next-hed newtal reg reglen regfull)
			    (runs:queue-next-tal newtal reg reglen regfull)
			    (runs:queue-next-reg newtal reg reglen regfull)
			    reruns)))))))))
