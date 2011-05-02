
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
	 (keyvallst (keys->vallist keys))
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

(define (register-test db run-id test-name item-path)
  (sqlite3:execute db "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path) VALUES (?,?,strftime('%s','now'),?);" run-id test-name item-path))

(define (test-set-status! db run-id test-name state status itemdat-or-path . comment)
  (let ((item-path (if (string? itemdat-or-path) itemdat-or-path (item-list->path itemdat-or-path))))
    (sqlite3:execute db "UPDATE tests SET state=?,status=?,event_time=strftime('%s','now') WHERE run_id=? AND testname=? AND item_path=?;" 
		     state status run-id test-name item-path)
    (if (and (not (null? comment))
	     (car comment))
	(sqlite3:execute db "UPDATE tests SET comment=? WHERE run_id=? AND testname=? AND item_path=?;"
			 (car comment) run-id test-name item-path))))

(define (test-set-log! db run-id test-name itemdat logf) 
  (let ((item-path (item-list->path itemdat)))
    (sqlite3:execute db "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path=?;" 
		     logf run-id test-name item-path)))

;; TODO: Converge this with db:get-test-info
(define (runs:get-test-info db run-id test-name item-path)
  (let ((res #f)) ;; (vector #f #f #f #f #f #f)))
    (sqlite3:for-each-row 
     (lambda (id run-id test-name state status)
       (set! res (vector id run-id test-name state status item-path)))
     db "SELECT id,run_id,testname,state,status FROM tests WHERE run_id=? AND testname=? AND item_path=?;"
     run-id test-name item-path)
    res))

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

(define (teststep-set-status! db run-id test-name teststep-name state-in status-in itemdat)
  ;; (print "run-id: " run-id " test-name: " test-name)
  (let* ((state     (check-valid-items "state" state-in))
	 (status    (check-valid-items "status" status-in))
	 (item-path (item-list->path itemdat))
	 (testdat   (runs:get-test-info db run-id test-name item-path)))
    ;; (print "testdat: " testdat)
    (if (and testdat ;; if the section exists then force specification BUG, I don't like how this works.
	     (or (not state)(not status)))
	(print "WARNING: Invalid " (if status "status" "state")
	       " value \"" (if status status-in state-in) "\", update your validstates section in megatest.config"))
    (if testdat
	(let ((test-id (test:get-id testdat)))
	  (sqlite3:execute db 
			"INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time) VALUES(?,?,?,?,strftime('%s','now'));"
			test-id teststep-name state status))
	(print "ERROR: Can't update " test-name " for run " run-id " -> no such test in db"))))

(define (test-get-kill-request db run-id test-name itemdat)
  (let* ((item-path (item-list->path itemdat))
	 (testdat   (runs:get-test-info db run-id test-name item-path)))
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
  (for-each 
   (lambda (test-name)
     (run-one-test db test-name))
   test-names))

(define (run-one-test db test-name)
  (print "Launching test " test-name)
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
	       (keys        (db-get-keys db))
	       (keyvallst   (keys->vallist keys #t))
	       (items       (hash-table-ref/default test-conf "items" #f))
	       (allitems    (item-assoc->item-list items))
	       (run-id      (register-run db keys)) ;;  test-name)))
	       (runconfigf  (conc  *toppath* "/runconfigs.config")))
	  ;; (print "items: ")(pp allitems)
	  (let loop ((itemdat (car allitems))
		     (tal     (cdr allitems)))
	    ;; (lambda (itemdat) ;;; ((ripeness "overripe") (temperature "cool") (season "summer"))
	    (let* ((item-path     (item-list->path itemdat)) ;; (string-intersperse (map cadr itemdat) "/"))
		   (new-test-path (string-intersperse (cons test-path (map cadr itemdat)) "/"))
		   (new-test-name (if (equal? item-path "") test-name (conc test-name "/" item-path))) ;; just need it to be unique
		   (test-status   #f))
	      (let loop2 ((ts #f)
			 (ct 0))
		(if (and (not ts)
			 (< ct 10))
		    (begin
		      (register-test db run-id test-name item-path)
		      (loop2 (runs:get-test-info db run-id test-name item-path)
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
		 (if (and (equal? (test:get-state test-status) "COMPLETED")
			  (equal? (test:get-status test-status) "PASS")
			  (not (args:get-arg "-force")))
		     (print "NOTE: Not starting test " new-test-name " as it is state \"COMPLETED\" and status \"PASS\", use -force to override")
		     (let* ((get-prereqs-cmd (lambda ()
					       (db-get-prereqs-not-met db run-id waiton))) ;; check before running ....
			    (launch-cmd      (lambda ()
					       (launch-test db run-id test-conf keyvallst test-name test-path itemdat)))
			    (testrundat      (list get-prereqs-cmd launch-cmd)))
		       (if (or (args:get-arg "-force")
			       (null? ((car testrundat)))) ;; are there any tests that must be run before this one...
			   ((cadr testrundat)) ;; this is the line that launches the test to the remote host
			   (hash-table-set! *waiting-queue* new-test-name testrundat)))))
		((LAUNCHED REMOTEHOSTSTART KILLED) 
		 (print "NOTE: " new-test-name " is already running or was explictly killed, use -force to launch it."))
		((RUNNING)  (print "NOTE: " test-name " is already running"))
		(else       (print "ERROR: Failed to launch test " new-test-name ". Unrecognised state " (test:get-state test-status)))))
	    (if (not (null? tal))
		(loop (car tal)(cdr tal))))))))

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
  
