;; register a test run with the db
(define (register-run db keys) ;; test-name)
  (let* ((keystr    (keys->keystr keys))
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (keyvallst (keys->vallist keys)) ;; extracts the values from remainder of (argv)
	 (runname   (get-with-default ":runname" #f))
	 (state     (get-with-default ":state" "no"))
	 (status    (get-with-default ":status" "n/a"))
	 (allvals   (append (list runname state status user) keyvallst))
	 (qryvals   (append (list runname) keyvallst))
	 (key=?str  (string-intersperse (map (lambda (k)(conc (key:get-fieldname k) "=?")) keys) " AND ")))
    (debug:print 3 "keys: " keys " allvals: " allvals " keyvallst: " keyvallst)
    (debug:print 2 "NOTE: using key " (string-intersperse keyvallst "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvallst))) ;; there must be a better way to "apply and"
	(let ((res #f))
	  (apply sqlite3:execute db (conc "INSERT OR IGNORE INTO runs (runname,state,status,owner,event_time" comma keystr ") VALUES (?,?,?,?,strftime('%s','now')" comma valslots ");")
		 allvals)
	  (apply sqlite3:for-each-row 
	   (lambda (id)
	     (set! res id))
	   db
	   (let ((qry (conc "SELECT id FROM runs WHERE (runname=? " andstr key=?str ");")))
	     ;(debug:print 4 "qry: " qry) 
	     qry)
	   qryvals)
	  (sqlite3:execute db "UPDATE runs SET state=?,status=? WHERE id=?;" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
	  #f))))

;; This is original run-tests, this routine is deprecated and we will transition to using runs:run-tests (see below)
;;
(define (run-tests db test-names)
  (let* ((keys        (db-get-keys db))
	 (keyvallst   (keys->vallist keys #t))
	 (run-id      (register-run db keys))  ;;  test-name)))
	 (deferred    '()) ;; delay running these since they have a waiton clause
	 (runconfigf   (conc  *toppath* "/runconfigs.config"))
	 (required-tests '()))

    ;; now add non-directly referenced dependencies (i.e. waiton)
    ;; could cache all these since they need to be read again ...
    ;; FIXME SOMEDAY
    (if (not (null? test-names))
	(let loop ((hed (car test-names))
		   (tal (cdr test-names)))
	  (let* ((config  (test:get-testconfig hed #f))
		 (waitons (string-split (let ((w (config-lookup config "requirements" "waiton")))
					  (if w w "")))))
	    (for-each 
	     (lambda (waiton)
	       (if (and waiton (not (member waiton test-names)))
		   (begin
		     (set! required-tests (cons waiton required-tests))
		     (set! test-names (append test-names (list waiton))))))
	     waitons)
	    (let ((remtests (delete-duplicates (append waitons tal))))
	      (if (not (null? remtests))
		  (loop (car remtests)(cdr remtests)))))))

    (if (not (null? required-tests))
	(debug:print 1 "INFO: Adding " required-tests " to the run queue")
	(debug:print 1 "INFO: No prerequisites added"))

    ;; on the first pass or call to run-tests set FAILS to NOT_STARTED if
    ;; -keepgoing is specified

    (set-megatest-env-vars db run-id) ;; these may be needed by the launching process
    
    (if (file-exists? runconfigf)
	(setup-env-defaults db runconfigf run-id *already-seen-runconfig-info* environ-patt: ".*")
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))

    (if (and (eq? *passnum* 0)
	     (args:get-arg "-keepgoing"))
	(begin
	  ;; have to delete test records where NOT_STARTED since they can cause -keepgoing to 
	  ;; get stuck due to becoming inaccessible from a failed test. I.e. if test B depends 
	  ;; on test A but test B reached the point on being registered as NOT_STARTED and test
	  ;; A failed for some reason then on re-run using -keepgoing the run can never complete.
	  (db:delete-tests-in-state db run-id "NOT_STARTED")
	  (db:set-tests-state-status db run-id test-names #f "FAIL" "NOT_STARTED" "FAIL")))
    (set! *passnum* (+ *passnum* 1))
    (let loop ((numtimes 0))
      (for-each 
       (lambda (test-name)
	 (if (runs:can-run-more-tests db)
	     (run-one-test db run-id test-name keyvallst)
	     ;; add some delay 
	     ;(sleep 2)
	     ))
       (tests:sort-by-priority-and-waiton test-names))
      ;; (run-waiting-tests db)
      (if (args:get-arg "-keepgoing")
	  (let ((estrem (db:estimated-tests-remaining db run-id)))
	    (if (and (> estrem 0)
		     (eq? *globalexitstatus* 0))
		(begin
		  (debug:print 1 "Keep going, estimated " estrem " tests remaining to run, will continue in 3 seconds ...")
		  (thread-sleep! 3)
		  (run-waiting-tests db)
		  (loop (+ numtimes 1)))))))))
	  
;; VERY INEFFICIENT! Move stuff that should be done once up to calling proc
(define (run-one-test db run-id test-name keyvallst)
  (debug:print 1 "Launching test " test-name)
  ;; All these vars might be referenced by the testconfig file reader
  (setenv "MT_TEST_NAME" test-name) ;; 
  (setenv "MT_RUNNAME"   (args:get-arg ":runname"))

  ;; (set-megatest-env-vars db run-id) ;; these may be needed by the launching process

  (change-directory *toppath*)
  (let* ((test-path    (conc *toppath* "/tests/" test-name)) ;; could use test:get-testconfig here ...
	 (test-configf (conc test-path "/testconfig"))
	 (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
	 (test-conf    (if testexists (read-config test-configf #f #t) (make-hash-table)))
	 (waiton       (let ((w (config-lookup test-conf "requirements" "waiton")))
			 (if (string? w)(string-split w)'())))
	 (tags         (let ((t (config-lookup test-conf "setup" "tags")))
			 ;; we want our tags to be separated by commas and fully delimited by commas
			 ;; so that queries with "like" can tie to the commas at either end of each tag
			 ;; while also allowing the end user to freely use spaces and commas to separate tags
			 (if (string? t)(string-substitute (regexp "[,\\s]+") "," (conc "," t ",") #t)
			     '()))))
    (if (not testexists)
	(begin
	  (debug:print 0 "ERROR: Can't find config file " test-configf)
	  (exit 2))
	;; put top vars into convenient variables and open the db
	(let* (;; db is always at *toppath*/db/megatest.db
	       (items       (hash-table-ref/default test-conf "items" '()))
	       (itemstable  (hash-table-ref/default test-conf "itemstable" '()))
	       (allitems    (if (or (not (null? items))(not (null? itemstable)))
				(append (item-assoc->item-list items)
					(item-table->item-list itemstable))
				'(())))) ;; a list with one null list is a test with no items
;; 	  (runconfigf  (conc  *toppath* "/runconfigs.config")))
	  (debug:print 1 "items: ")
	  (if (>= *verbosity* 1)(pp allitems))
	  (if (>= *verbosity* 5)
	      (begin
		(print "items: ")(pp (item-assoc->item-list items))
		(print "itestable: ")(pp (item-table->item-list itemstable))))
	  (if (args:get-arg "-m")
	      (db:set-comment-for-run db run-id (args:get-arg "-m")))

	  ;; Here is where the test_meta table is best updated
	  (runs:update-test_meta db test-name test-conf)

	  ;; braindead work-around for poorly specified allitems list BUG!!! FIXME
	  (if (null? allitems)(set! allitems '(())))
	  (let loop ((itemdat (car allitems))
		     (tal     (cdr allitems)))
	    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
	    ;; Handle lists of items
	    (let* ((item-path     (item-list->path itemdat)) ;; (string-intersperse (map cadr itemdat) "/"))
		   (new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
		   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
		   (testdat   #f)
		   (num-running (db:get-count-tests-running db))
		   (max-concurrent-jobs (config-lookup *configdat* "setup" "max_concurrent_jobs"))
		   (parent-test (and (not (null? items))(equal? item-path "")))
		   (single-test (and (null? items) (equal? item-path "")))
		   (item-test   (not (equal? item-path "")))
		   (item-patt   (args:get-arg "-itempatt"))
		   (patt-match  (if item-patt
				    (string-search (glob->regexp
						   (string-translate item-patt "%" "*"))
						  item-path)
				    #t)))
	      (debug:print 3 "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	      (if (and patt-match (runs:can-run-more-tests db))
		  (begin
		    (let loop2 ((ts (db:get-test-info db run-id test-name item-path)) ;; #f)
				(ct 0))
		      (if (and (not ts)
			       (< ct 10))
			  (begin
			    (register-test db run-id test-name item-path)
			    (db:test-set-comment db run-id test-name item-path "")
			    (loop2 (db:get-test-info db run-id test-name item-path)
				   (+ ct 1)))
			  (if ts
			      (set! testdat ts)
			      (begin
				(debug:print 0 "WARNING: Couldn't register test " test-name " with item path " item-path ", skipping")
				(if (not (null? tal))
				    (loop (car tal)(cdr tal)))))))
		    (change-directory test-path)
		    ;; this block is here only to inform the user early on
		    
		    ;; NB// Moving the setting of runconfig.config vars to *before* the 
		    ;; the calling of each test.
		    ;; (if (file-exists? runconfigf)
		    ;;     (setup-env-defaults db runconfigf run-id *already-seen-runconfig-info*)
		    ;;     (debug:print 0 "WARNING: You do not have a run config file: " runconfigf))
		    (debug:print 4 "run-id: " run-id " test-name: " test-name " item-path: " item-path " testdat: " (test:get-status testdat) " test-state: " (test:get-state testdat))
		    (case (if (args:get-arg "-force")
			      'NOT_STARTED
			      (if testdat
				  (string->symbol (test:get-state testdat))
				  'failed-to-insert))
		      ((failed-to-insert)
		       (debug:print 0 "ERROR: Failed to insert the record into the db"))
		      ((NOT_STARTED COMPLETED)
		       (debug:print 6 "Got here, " (test:get-state testdat))
		       (let ((runflag #f))
			 (cond
			  ;; i.e. this is the parent test to a suite of items, never "run" it
			  (parent-test
			   (set! runflag #f))
			  ;; -force, run no matter what
			  ((args:get-arg "-force")(set! runflag #t))
			  ;; NOT_STARTED, run no matter what
			  ((equal? (test:get-state testdat) "NOT_STARTED")(set! runflag #t))
			  ;; not -rerun and PASS, WARN or CHECK, do no run
			  ((and (or (not (args:get-arg "-rerun"))
				    (args:get-arg "-keepgoing"))
				(member (test:get-status testdat) '("PASS" "WARN" "CHECK")))
			   (set! runflag #f))
			  ;; -rerun and status is one of the specifed, run it
			  ((and (args:get-arg "-rerun")
				(let ((rerunlst (string-split (args:get-arg "-rerun") ","))) ;; FAIL,
				  (member (test:get-status testdat) rerunlst)))
			   (set! runflag #t))
			  ;; -keepgoing, do not rerun FAIL
			  ((and (args:get-arg "-keepgoing")
				(member (test:get-status testdat) '("FAIL")))
			   (set! runflag #f))
			  ((and (not (args:get-arg "-rerun"))
				(member (test:get-status testdat) '("FAIL" "n/a")))
			   (set! runflag #t))
			  (else (set! runflag #f)))
			 (debug:print 6 "RUNNING => runflag: " runflag " STATE: " (test:get-state testdat) " STATUS: " (test:get-status testdat))
			 (if (not runflag)
			     (if (not parent-test)
				 (debug:print 1 "NOTE: Not starting test " new-test-name " as it is state \"COMPLETED\" and status \"" (test:get-status testdat) "\", use -force to override"))
			     (let* ((get-prereqs-cmd (lambda ()
						       (db-get-prereqs-not-met db run-id waiton))) ;; check before running ....
				    (launch-cmd      (lambda ()
						       (launch-test db run-id (args:get-arg ":runname") test-conf keyvallst test-name test-path itemdat args:arg-hash)))
				    (testrundat      (list get-prereqs-cmd launch-cmd)))
			       (if (or (args:get-arg "-force")
				       (let ((preqs-not-yet-met ((car testrundat))))
					 (debug:print 2 "Preqrequesites for " test-name ": " preqs-not-yet-met)
					 (null? preqs-not-yet-met))) ;; are there any tests that must be run before this one...
				   (if (not ((cadr testrundat))) ;; this is the line that launches the test to the remote host
				       (begin
					 (print "ERROR: Failed to launch the test. Exiting as soon as possible")
					 (set! *globalexitstatus* 1) ;; 
					 (process-signal (current-process-id) signal/kill)
					 ;(exit 1)
					 ))
				   (if (not (args:get-arg "-keepgoing"))
				       (hash-table-set! *waiting-queue* new-test-name testrundat)))))))
		      ((KILLED) 
		       (debug:print 1 "NOTE: " new-test-name " is already running or was explictly killed, use -force to launch it."))
		      ((LAUNCHED REMOTEHOSTSTART RUNNING)  
		       (if (> (- (current-seconds)(+ (db:test-get-event_time testdat)
						     (db:test-get-run_duration testdat)))
			      100) ;; i.e. no update for more than 100 seconds
			   (begin
			     (debug:print 0 "WARNING: Test " test-name " appears to be dead. Forcing it to state INCOMPLETE and status STUCK/DEAD")
			     (test-set-status! db run-id test-name "INCOMPLETE" "STUCK/DEAD" itemdat "Test is stuck or dead" #f))
			   (debug:print 2 "NOTE: " test-name " is already running")))
		      (else       (debug:print 0 "ERROR: Failed to launch test " new-test-name ". Unrecognised state " (test:get-state testdat))))))
	      (if (not (null? tal))
		  (loop (car tal)(cdr tal)))))))))

(define (run-waiting-tests db)
  (let ((numtries           0)
	(last-try-time      (current-seconds))
	(times              (list 1))) ;; minutes to wait before trying again to kick off runs
    ;; BUG this hack of brute force retrying works quite well for many cases but 
    ;;     what is needed is to check the db for tests that have failed less than
    ;;     N times or never been started and kick them off again
    (let loop ((waiting-test-names (hash-table-keys *waiting-queue*)))
      (cond
       ((not (runs:can-run-more-tests db))
	(thread-sleep! 2)
	(loop waiting-test-names))
       ((null? waiting-test-names)
	(debug:print 1 "All tests launched"))
       (else
	(set! numtries (+ numtries 1))
	(for-each (lambda (testname)
		    (if (runs:can-run-more-tests db)
			(let* ((testdat (hash-table-ref *waiting-queue* testname))
			       (prereqs ((car testdat)))
			       (ldb     (if db db (open-db))))
			  (debug:print 2 "prereqs remaining: " prereqs)
			  (if (null? prereqs)
			      (begin
				(debug:print 2 "Prerequisites met, launching " testname)
				((cadr testdat))
				(hash-table-delete! *waiting-queue* testname)))
			  (if (not db)
			      (sqlite3:finalize! ldb)))))
		  waiting-test-names)
	;; (sleep 10) ;; no point in rushing things at this stage?
	(loop (hash-table-keys *waiting-queue*)))))))
