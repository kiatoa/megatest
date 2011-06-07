
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;;  strftime('%m/%d/%Y %H:%M:%S','now','localtime')

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
    ;; (print "keys: " keys " allvals: " allvals " keyvallst: " keyvallst)
    (print "NOTE: using key " (string-intersperse keyvallst "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvallst))) ;; there must be a better way to "apply and"
	(let ((res #f))
	  (apply sqlite3:execute db (conc "INSERT OR IGNORE INTO runs (runname,state,status,owner,event_time" comma keystr ") VALUES (?,?,?,?,strftime('%s','now')" comma valslots ");")
		 allvals)
	  (apply sqlite3:for-each-row 
	   (lambda (id)
	     (set! res id))
	   db
	   (let ((qry (conc "SELECT id FROM runs WHERE (runname=? " andstr key=?str ");")))
	     ;; (print "qry: " qry) 
	     qry)
	   qryvals)
	  (sqlite3:execute db "UPDATE runs SET state=?,status=? WHERE id=?;" state status res)
	  res) 
	(begin
	  (print "ERROR: Called without all necessary keys")
	  #f))))

;; runs:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
;;  to extract info from the structure returned
;;
(define (runs:get-runs-by-patt db keys runnamepatt . params) ;; test-name)
  (let* ((keyvallst (keys->vallist keys))
	 (tmp      (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time")))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (res     '())
	 (key-patt ""))
    (for-each (lambda (keyval)
		(let* ((key    (vector-ref keyval 0))
		       (fulkey (conc ":" key))
		       (patt   (args:get-arg fulkey)))
		  (if patt
		      (set! key-patt (conc key-patt " AND " key " like '" patt "'"))
		      (begin
			(print "ERROR: searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keys)
    (sqlite3:for-each-row 
     (lambda (a . r)
       (set! res (cons (list->vector (cons a r)) res)))
     db 
     (conc "SELECT " keystr " FROM runs WHERE runname like ? " key-patt ";")
     runnamepatt)
    (vector header res)))

(define (register-test db run-id test-name item-path)
  (let ((item-paths (if (equal? item-path "")
			(list item-path)
			(list item-path ""))))
    (for-each 
     (lambda (pth)
       (sqlite3:execute db "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path) VALUES (?,?,strftime('%s','now'),?);" run-id test-name pth))
     item-paths)))

;;  (define db (open-db))
;;  (test-set-status! db 2 "runfirst" "COMPLETED" "PASS" "summer")

(define (test-set-status! db run-id test-name state status itemdat-or-path . comment)
  (let ((item-path (if (string? itemdat-or-path) itemdat-or-path (item-list->path itemdat-or-path))))
    (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
		     state status run-id test-name item-path)
    (if (and (not (equal? item-path "")) ;; need to update the top test record if PASS or FAIL and this is a subtest
	     (or (equal? status "PASS")
		 (equal? status "WARN")
		 (equal? status "FAIL")))
	(begin
	  (sqlite3:execute 
	   db
	   "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND status='FAIL'),
                 pass_count=(SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND (status='PASS' OR status='WARN'))
             WHERE run_id=? AND testname=? AND item_path='';"
	   run-id test-name run-id test-name run-id test-name)
	  (sqlite3:execute
	   db
	   "UPDATE tests
             SET state='COMPLETED',
                status=CASE WHEN fail_count > 0 THEN 'FAIL' WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' ELSE 'UNKNOWN' END
             WHERE run_id=? AND testname=? AND item_path='';"
	   run-id test-name)))
    (if (and (not (null? comment))
	     (car comment))
	(sqlite3:execute db "UPDATE tests SET comment=? WHERE run_id=? AND testname=? AND item_path=?;"
			 (car comment) run-id test-name item-path))))

(define (test-set-log! db run-id test-name itemdat logf) 
  (let ((item-path (item-list->path itemdat)))
    (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path=?;" 
		     logf run-id test-name item-path)))

(define (test-set-toplog! db run-id test-name logf) 
  (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';" 
		   logf run-id test-name))

;; ;; TODO: Converge this with db:get-test-info
;; (define (runs:get-test-info db run-id test-name item-path)
;;   (let ((res #f)) ;; (vector #f #f #f #f #f #f)))
;;     (sqlite3:for-each-row 
;;      (lambda (id run-id test-name state status)
;;        (set! res (vector id run-id test-name state status item-path)))
;;      db "SELECT id,run_id,testname,state,status FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
;;      run-id test-name item-path)
;;     res))

(define-inline (test:get-id vec)       (vector-ref vec 0))
(define-inline (test:get-run_id vec)   (vector-ref vec 1))
(define-inline (test:get-test-name vec)(vector-ref vec 2))
(define-inline (test:get-state vec)    (vector-ref vec 3))
(define-inline (test:get-status vec)   (vector-ref vec 4))
(define-inline (test:get-item-path vec)(vector-ref vec 5))

(define (runs:test-get-full-path test)
  (let* ((testname (db:test-get-testname   test))
	 (itempath (db:test-get-item-path test)))
    (conc testname (if (equal? itempath "") "" (conc "(" itempath ")")))))

(define-inline (test:test-get-fullname test)
   (conc (db:test-get-testname test)
	 (if (equal? (db:test-get-item-path test) "")
	     ""
	     (conc "(" (db:test-get-item-path test) ")"))))

(define (check-valid-items class item)
  (let ((valid-values (let ((s (config-lookup *configdat* "validvalues" class)))
			(if s (string-split s) #f))))
    (if valid-values
	(if (member item valid-values)
	    item #f)
	item)))

(define (teststep-set-status! db run-id test-name teststep-name state-in status-in itemdat comment)
  ;; (print "run-id: " run-id " test-name: " test-name)
  (let* ((state     (check-valid-items "state" state-in))
	 (status    (check-valid-items "status" status-in))
	 (item-path (item-list->path itemdat))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    ;; (print "testdat: " testdat)
    (if (and testdat ;; if the section exists then force specification BUG, I don't like how this works.
	     (or (not state)(not status)))
	(print "WARNING: Invalid " (if status "status" "state")
	       " value \"" (if status status-in state-in) "\", update your validstates section in megatest.config"))
    (if testdat
	(let ((test-id (test:get-id testdat)))
	  (sqlite3:execute db 
			"INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment) VALUES(?,?,?,?,strftime('%s','now'),?);"
			test-id teststep-name state status (if comment comment "")))
	(print "ERROR: Can't update " test-name " for run " run-id " -> no such test in db"))))

(define (test-get-kill-request db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat   (db:get-test-info db run-id test-name item-path)))
    (equal? (test:get-state testdat) "KILLREQ")))

(define (test-set-meta-info db run-id testname itemdat)
  (let ((item-path (item-list->path itemdat))
	(cpuload  (get-cpu-load))
	(hostname (get-host-name))
	(diskfree (get-df (current-directory)))
	(uname    (get-uname "-srvpio"))
	(runpath  (current-directory)))
    (sqlite3:execute db "UPDATE tests SET host=?,cpuload=?,diskfree=?,uname=?,rundir=? WHERE run_id=? AND testname=? AND item_path=?;"
		  hostname
		  cpuload
		  diskfree
		  uname
		  runpath
		  run-id
		  testname
		  item-path)))

(define (test-update-meta-info db run-id testname itemdat minutes)
  (let ((item-path (item-list->path itemdat))
	(cpuload  (get-cpu-load))
	(diskfree (get-df (current-directory))))
    (if (not cpuload)  (begin (print "WARNING: CPULOAD not found.")  (set! cpuload "n/a")))
    (if (not diskfree) (begin (print "WARNING: DISKFREE not found.") (set! diskfree "n/a")))
    (if (not item-path)(begin (print "WARNING: ITEMPATH not set.")   (set! item-path "")))
    ;; (let ((testinfo (db:get-test-info db run-id testname item-path)))
    ;;   (if (and (not (equal? (db:test-get-status testinfo) "COMPLETED"))
    ;;            (not (equal? (db:test-get-status testinfo) "KILLREQ"))
    (sqlite3:execute
     db
     "UPDATE tests SET cpuload=?,diskfree=?,run_duration=?,state='RUNNING' WHERE run_id=? AND testname=? AND item_path=? AND state NOT IN ('COMPLETED','KILLREQ','KILLED');"
     cpuload
     diskfree
     minutes
     run-id
     testname
     item-path)))

(define (set-megatest-env-vars db run-id)
  (let ((keys (db-get-keys db)))
    (for-each (lambda (key)
		(sqlite3:for-each-row
		 (lambda (val)
		   (print "setenv " (key:get-fieldname key) " " val)
		   (setenv (key:get-fieldname key) val))
		 db 
		 (conc "SELECT " (key:get-fieldname key) " FROM runs WHERE id=?;")
		 run-id))
	      keys)))

(define (set-item-env-vars itemdat)
  (for-each (lambda (item)
	      (print "setenv " (car item) " " (cadr item))
	      (setenv (car item) (cadr item)))
	    itemdat))

(define (get-all-legal-tests)
  (let* ((tests  (glob (conc *toppath* "/tests/*")))
	 (res    '()))
    ;; (print "INFO: Looking at tests " (string-intersperse tests ","))
    (for-each (lambda (testpath)
		(if (file-exists? (conc testpath "/testconfig"))
		    (set! res (cons (last (string-split testpath "/")) res))))
	      tests)
    res))

(define (run-tests db test-names)
  (let* ((keys        (db-get-keys db))
	 (keyvallst   (keys->vallist keys #t))
	 (run-id      (register-run db keys))) ;;  test-name)))
    (for-each 
     (lambda (test-name)
       (let ((num-running (db:get-count-tests-running db))
	     (max-concurrent-jobs (config-lookup *configdat* "setup" "max_concurrent_jobs")))
	 (print "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	 (if (or (not max-concurrent-jobs)
		 (and max-concurrent-jobs
		      (string->number max-concurrent-jobs)
		      (not (> num-running (string->number max-concurrent-jobs)))))
	     (run-one-test db run-id test-name keyvallst)
	     (print "WARNING: Max running jobs exceeded, current number running: " num-running 
		    ", max_concurrent_jobs: " max-concurrent-jobs))))
     test-names)))

;; VERY INEFFICIENT! Move stuff that should be done once up to calling proc
(define (run-one-test db run-id test-name keyvallst)
  (print "Launching test " test-name)
  ;; All these vars might be referenced by the testconfig file reader
  (setenv "MT_TEST_NAME" test-name) ;; 
  (setenv "MT_RUNNAME"   (args:get-arg ":runname"))
  (set-megatest-env-vars db run-id) ;; these may be needed by the launching process
  (change-directory *toppath*)
  (let* ((test-path    (conc *toppath* "/tests/" test-name))
	 (test-configf (conc test-path "/testconfig"))
	 (testexists   (and (file-exists? test-configf)(file-read-access? test-configf)))
	 (test-conf    (if testexists (read-config test-configf) (make-hash-table)))
	 (waiton       (let ((w (config-lookup test-conf "requirements" "waiton")))
			 (if (string? w)(string-split w)'()))))
    (if (not testexists)
	(begin
	  (print "ERROR: Can't find config file " test-configf)
	  (exit 2))
	;; put top vars into convenient variables and open the db
	(let* (;; db is always at *toppath*/db/megatest.db
	       (items       (hash-table-ref/default test-conf "items" '()))
	       (itemstable  (hash-table-ref/default test-conf "itemstable" '()))
	       (allitems    (if (or (not (null? items))(not (null? itemstable)))
				(append (item-assoc->item-list items)
					(item-table->item-list itemstable))
				'(()))) ;; a list with one null list is a test with no items
	       (runconfigf  (conc  *toppath* "/runconfigs.config")))
	  (print "items: ")(pp allitems)
	  (if (args:get-arg "-m")
	      (db:set-comment-for-run db run-id (args:get-arg "-m")))
	  (let loop ((itemdat (car allitems))
		     (tal     (cdr allitems)))
	    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
	    (let* ((item-path     (item-list->path itemdat)) ;; (string-intersperse (map cadr itemdat) "/"))
		   (new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
		   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
		   (test-status   #f)
		   (num-running (db:get-count-tests-running db))
		   (max-concurrent-jobs (config-lookup *configdat* "setup" "max_concurrent_jobs")))
	      (print "max-concurrent-jobs: " max-concurrent-jobs ", num-running: " num-running)
	      (if (not (or (not max-concurrent-jobs)
			   (and max-concurrent-jobs
				(string->number max-concurrent-jobs)
				(not (> num-running (string->number max-concurrent-jobs))))))
		  (print "WARNING: Max running jobs exceeded, current number running: " num-running 
			 ", max_concurrent_jobs: " max-concurrent-jobs)
		  (begin
		    (let loop2 ((ts #f)
				(ct 0))
		      (if (and (not ts)
			       (< ct 10))
			  (begin
			    (register-test db run-id test-name item-path)
			    (db:test-set-comment db run-id test-name item-path "")
			    ;; (test-set-status! db run-id test-name "NOT_STARTED" "n/a" itemdat "")
			    ;; (db:set-comment-for-test db run-id test-name item-path "")
			    (db:delete-test-step-records db run-id test-name) ;; clean out if this is a re-run
			    (loop2 (db:get-test-info db run-id test-name item-path)
				   (+ ct 1)))
			  (if ts
			      (set! test-status ts)
			      (begin
				(print "WARNING: Couldn't register test " test-name " with item path " item-path ", skipping")
				(if (not (null? tal))
				    (loop (car tal)(cdr tal)))))))
		    (change-directory test-path)
		    ;; this block is here only to inform the user early on
		    (if (file-exists? runconfigf)
			(setup-env-defaults db runconfigf run-id *already-seen-runconfig-info*)
			(print "WARNING: You do not have a run config file: " runconfigf))
		    ;; (print "run-id: " run-id " test-name: " test-name " item-path: " item-path " test-status: " (test:get-status test-status) " test-state: " (test:get-state test-status))
		    (case (if (args:get-arg "-force")
			      'NOT_STARTED
			      (if test-status
				  (string->symbol (test:get-state test-status))
				  'failed-to-insert))
		      ((failed-to-insert)
		       (print "ERROR: Failed to insert the record into the db"))
		      ((NOT_STARTED COMPLETED) ;; (cadr status is the row id for the run record)
		       (if (and (equal? (test:get-state test-status)  "COMPLETED")
				(or (equal? (test:get-status test-status) "PASS")
				    (equal? (test:get-status test-status) "WARN")
				    (equal? (test:get-status test-status) "CHECK"))
				(not (args:get-arg "-force")))
			   (print "NOTE: Not starting test " new-test-name " as it is state \"COMPLETED\" and status \"" (test:get-status test-status) "\", use -force to override")
			   (let* ((get-prereqs-cmd (lambda ()
						     (db-get-prereqs-not-met db run-id waiton))) ;; check before running ....
				  (launch-cmd      (lambda ()
						     (launch-test db run-id test-conf keyvallst test-name test-path itemdat)))
				  (testrundat      (list get-prereqs-cmd launch-cmd)))
			     (if (or (args:get-arg "-force")
				     (null? ((car testrundat)))) ;; are there any tests that must be run before this one...
				 ((cadr testrundat)) ;; this is the line that launches the test to the remote host
				 (hash-table-set! *waiting-queue* new-test-name testrundat)))))
		      ((KILLED) 
		       (print "NOTE: " new-test-name " is already running or was explictly killed, use -force to launch it."))
		      ((LAUNCHED REMOTEHOSTSTART RUNNING)  
		       (if (> (- (current-seconds)(+ (db:test-get-event_time test-status)
						     (db:test-get-run_duration test-status)))
			      100) ;; i.e. no update for more than 100 seconds
			   (begin
			     (print "WARNING: Test " test-name " appears to be dead.")
			     (test-set-status! db run-id test-name "INCOMPLETE" "STUCK/DEAD" itemdat "Test is stuck or dead"))
			   (print "NOTE: " test-name " is already running")))
		      (else       (print "ERROR: Failed to launch test " new-test-name ". Unrecognised state " (test:get-state test-status))))))
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
       ((null? waiting-test-names)
	(print "All tests launched"))
       ((> numtries 4)
	(print "NOTE: Tried launching four times, perhaps run megatest again in a few minutes"))
       (else
	(set! numtries (+ numtries 1))
	(for-each (lambda (testname)
		    (let* ((testdat (hash-table-ref *waiting-queue* testname))
			   (prereqs ((car testdat)))
			   (ldb     (if db db (open-db))))
		      ;; (print "prereqs remaining: " prereqs)
		      (if (null? prereqs)
			  (begin
			    (print "Prerequisites met, launching " testname)
			    ((cadr testdat))
			    (hash-table-delete! *waiting-queue* testname)))
		      (if (not db)
			  (sqlite3:finalize! ldb))))
		  waiting-test-names)
	(sleep 10) ;; no point in rushing things at this stage?
	(loop (hash-table-keys *waiting-queue*)))))))

(define (get-dir-up-one dir) 
  (let ((dparts  (string-split dir "/")))
    (conc "/" (string-intersperse 
	       (take dparts (- (length dparts) 1))
	       "/"))))
;; Remove runs
;; fields are passing in through 
(define (runs:remove-runs db runnamepatt testpatt itempatt)
  (let* ((keys        (db-get-keys db))
	 (rundat      (runs:get-runs-by-patt db keys runnamepatt))
	 (header      (vector-ref rundat 0))
	 (runs        (vector-ref rundat 1)))
    (print "Header: " header)
    (for-each
     (lambda (run)
       (let ((runkey (string-intersperse (map (lambda (k)
						(db-get-value-by-header run header (vector-ref k 0))) keys) "/")))
	 (let* ((run-id (db-get-value-by-header run header "id") )
		(tests  (db-get-tests-for-run db (db-get-value-by-header run header "id") testpatt itempatt))
		(lasttpath "/does/not/exist/I/hope"))
	   (if (not (null? tests))
	       (begin
		 (print "Removing tests for run: " runkey " " (db-get-value-by-header run header "runname"))
		 (for-each
		  (lambda (test)
		    (print "  " (db:test-get-testname test) " id: " (db:test-get-id test) " " (db:test-get-item-path test))
		    (db:delete-test-records db (db:test-get-id test))
		    (if (> (string-length (db:test-get-rundir test)) 5) ;; bad heuristic but should prevent /tmp /home etc.
			(let ((fullpath (db:test-get-rundir test))) ;; "/" (db:test-get-item-path test))))
			  (set! lasttpath fullpath)
			  (print "rm -rf " fullpath)
			  (system (conc "rm -rf " fullpath))
			  (let ((cmd (conc "rmdir -p " (get-dir-up-one fullpath))))
			    (print cmd)
			    (system cmd))
			  )))
		  tests)))
	   (let ((remtests (db-get-tests-for-run db (db-get-value-by-header run header "id"))))
	     (if (null? remtests) ;; no more tests remaining
		 (let* ((dparts  (string-split lasttpath "/"))
			(runpath (conc "/" (string-intersperse 
					    (take dparts (- (length dparts) 1))
					    "/"))))
		   (print "Removing run: " runkey " " (db-get-value-by-header run header "runname"))
		   (db:delete-run db run-id)
		   ;; need to figure out the path to the run dir and remove it if empty
		;;    (if (null? (glob (conc runpath "/*")))
		;;        (begin
		;; 	 (print "Removing run dir " runpath)
		;; 	 (system (conc "rmdir -p " runpath))))
		   )))
		 )))
     runs)))

