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

(use json format)

(declare (unit rmt))
(declare (uses api))
(declare (uses tdb))
(declare (uses http-transport))

;;
;; THESE ARE ALL CALLED ON THE CLIENT SIDE!!!
;;

;; ;; For debugging add the following to ~/.megatestrc
;;
;; (require-library trace)
;; (import trace)
;; (trace
;; rmt:send-receive
;; api:execute-requests
;; )


;;======================================================================
;;  S U P P O R T   F U N C T I O N S
;;======================================================================

;; #t means - please start a server!
;;
(define (rmt:write-frequency-over-limit? cmd run-id)
  (and (not (member cmd api:read-only-queries))
       (let* ((tmprec (hash-table-ref/default *write-frequency* run-id #f))
	      (record (if tmprec tmprec 
			  (let ((v (vector (current-seconds) 0)))
			    (hash-table-set! *write-frequency* run-id v)
			    v)))
	      (count  (+ 1 (vector-ref record 1)))
	      (start  (vector-ref record 0))
	      (queries-per-second (/ (* count 1.0)
				     (max (- (current-seconds) start) 1))))
	 (vector-set! record 1 count)
	 (if (and (> count 10)
		  (> queries-per-second 10))
	     (begin
	       (debug:print-info 1 "db write rate too high, starting a server, count=" count " start=" start " run-id=" run-id " queries-per-second=" queries-per-second)
	       #t)
	     #f))))

;; cmd is a symbol
;; vars is a json string encoding the parameters for the call
;;
(define (rmt:send-receive cmd rid params #!key (attemptnum 0))
  ;; clean out old connections
  (mutex-lock! *db-multi-sync-mutex*)
  (let ((expire-time (- (current-seconds) 60)))
    (for-each 
     (lambda (run-id)
       (let ((connection (hash-table-ref/default *runremote* run-id #f)))
	 (if (and connection 
		  (< (http-transport:server-dat-get-last-access connection) expire-time))
	     (begin
	       (debug:print-info 0 "Discarding connection to server for run-id " run-id ", too long between accesses")
	       (hash-table-delete! *runremote* run-id)))))
     (hash-table-keys *runremote*)))
  (mutex-unlock! *db-multi-sync-mutex*)
  (let* ((run-id          (if rid rid 0))
	 (connection-info (let ((cinfo (hash-table-ref/default *runremote* run-id #f)))
			    (if cinfo
				cinfo
				;; NB// can cache the answer for server running for 10 seconds ...
				;;  ;; (and (not (rmt:write-frequency-over-limit? cmd run-id))
				(if (tasks:server-running-or-starting? (db:delay-if-busy
									(tasks:open-db))
								       run-id)
				    (let ((res (client:setup run-id)))
				      (if res 
					  (hash-table-ref/default *runremote* run-id #f) ;; client:setup filled this in (hopefully)
					  #f))
				    #f))))
	 (jparams         (db:obj->string params)))
    (if connection-info
	(let ((res             (http-transport:client-api-send-receive run-id connection-info cmd jparams)))
	  (http-transport:server-dat-update-last-access connection-info)
	  (if res
	      (db:string->obj res)
	      (begin ;; let ((new-connection-info (client:setup run-id)))
		(debug:print 0 "WARNING: Communication failed, trying call to http-transport:client-api-send-receive again.")
		(hash-table-delete! *runremote* run-id) ;; don't keep using the same connection

		;; no longer killing the server in http-transport:client-api-send-receive
		;; may kill it here but what are the criteria?
		;; start with three calls then kill server
		(if (eq? attemptnum 3)(tasks:kill-server-run-id run-id))

		(rmt:send-receive cmd run-id params attemptnum: (+ attemptnum 1)))))
	(let ((max-avg-qry (string->number (or (configf:lookup *configdat* "server" "server-query-threshold") "10"))))
	  (debug:print-info 4 "no server and read-only query, bypassing normal channel")
	  ;; (if (rmt:write-frequency-over-limit? cmd run-id)(server:kind-run run-id))
	  (let ((curr-max (rmt:get-max-query-average run-id)))
	    (if (> (cdr curr-max) max-avg-qry)
		(if (common:low-noise-print 10 "start server due to max average query too long")
		      (begin
			(debug:print-info 0 "Max average query, " (inexact->exact (round (cdr curr-max))) "ms (" (car curr-max) ") exceeds " max-avg-qry ", try starting server ...")
			(server:kind-run run-id)
			(debug:print-info 3 "Max average query, " (inexact->exact (round (cdr curr-max))) "ms (" (car curr-max) ") below " max-avg-qry ", not starting server...")))))
	  (rmt:open-qry-close-locally cmd run-id params)))))

(define (rmt:update-db-stats run-id rawcmd params duration)
  (mutex-lock! *db-stats-mutex*)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 "WARNING: stats collection failed in update-db-stats")
     (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
     (print "exn=" (condition->list exn))
     #f) ;; if this fails we don't care, it is just stats
   (let* ((cmd      (conc "run-id=" run-id " " (if (eq? rawcmd 'general-call) (car params) rawcmd)))
	  (stat-vec (hash-table-ref/default *db-stats* cmd #f)))
     (if (not stat-vec)
	 (let ((newvec (vector 0 0)))
	   (hash-table-set! *db-stats* cmd newvec)
	   (set! stat-vec newvec)))
     (vector-set! stat-vec 0 (+ (vector-ref stat-vec 0) 1))
     (vector-set! stat-vec 1 (+ (vector-ref stat-vec 1) duration))))
  (mutex-unlock! *db-stats-mutex*))


(define (rmt:print-db-stats)
  (let ((fmtstr "~40a~7-d~9-d~20,2-f")) ;; "~20,2-f"
    (debug:print 18 "DB Stats\n========")
    (debug:print 18 (format #f "~40a~8a~10a~10a" "Cmd" "Count" "TotTime" "Avg"))
    (for-each (lambda (cmd)
		(let ((cmd-dat (hash-table-ref *db-stats* cmd)))
		  (debug:print 18 (format #f fmtstr cmd (vector-ref cmd-dat 0) (vector-ref cmd-dat 1) (/ (vector-ref cmd-dat 1)(vector-ref cmd-dat 0))))))
	      (sort (hash-table-keys *db-stats*)
		    (lambda (a b)
		      (> (vector-ref (hash-table-ref *db-stats* a) 0)
			 (vector-ref (hash-table-ref *db-stats* b) 0)))))))

(define (rmt:get-max-query-average run-id)
  (mutex-lock! *db-stats-mutex*)
  (let* ((runkey (conc "run-id=" run-id " "))
	 (cmds   (filter (lambda (x)
			   (substring-index runkey x))
			 (hash-table-keys *db-stats*)))
	 (res    (if (null? cmds)
		     (cons 'none 0)
		     (let loop ((cmd (car cmds))
				(tal (cdr cmds))
				(max-cmd (car cmds))
				(res 0))
		       (let* ((cmd-dat (hash-table-ref *db-stats* cmd))
			      (tot     (vector-ref cmd-dat 0))
			      (curravg (/ (vector-ref cmd-dat 1) (vector-ref cmd-dat 0))) ;; count is never zero by construction
			      (currmax (max res curravg))
			      (newmax-cmd (if (> curravg res) cmd max-cmd)))
			 (if (null? tal)
			     (if (> tot 10)
				 (cons newmax-cmd currmax)
				 (cons 'none 0))
			     (loop (car tal)(cdr tal) newmax-cmd currmax)))))))
    (mutex-unlock! *db-stats-mutex*)
    res))
	  
(define (rmt:open-qry-close-locally cmd run-id params)
  (let* ((dbstruct-local (if *dbstruct-db*
			     *dbstruct-db*
			     (let* ((dbdir (conc    (configf:lookup *configdat* "setup" "linktree") "/.db"))
				    (db (make-dbr:dbstruct path:  dbdir local: #t)))
			       (set! *dbstruct-db* db)
			       db)))
	 (db-file-path   (db:dbfile-path 0)))
    ;; (read-only      (not (file-read-access? db-file-path)))
    (let* ((start         (current-milliseconds))
	   (res           (api:execute-requests dbstruct-local (symbol->string cmd) params))
	   (duration      (- (current-milliseconds) start)))
      (rmt:update-db-stats run-id cmd params duration)
      ;; mark this run as dirty if this was a write
      (if (not (member cmd api:read-only-queries))
	  (let ((start-time (current-seconds)))
	    (mutex-lock! *db-multi-sync-mutex*)
	    ;; (if (not (hash-table-ref/default *db-local-sync* run-id #f))
	    ;; just set it every time. Is a write more expensive than a read and does it matter?
	    (hash-table-set! *db-local-sync* run-id start-time) ;; the oldest "write"
	    (mutex-unlock! *db-multi-sync-mutex*)))
      res)))

(define (rmt:send-receive-no-auto-client-setup connection-info cmd run-id params)
  (let* ((run-id   (if run-id run-id 0))
	 (jparams         (db:obj->string params)) ;; (rmt:dat->json-str params))
	 (res (http-transport:client-api-send-receive run-id connection-info cmd jparams)))
    (if res
	(db:string->obj res)
	res)))

;; Wrap json library for strings (why the ports crap in the first place?)
(define (rmt:dat->json-str dat)
  (with-output-to-string 
    (lambda ()
      (json-write dat))))

(define (rmt:json-str->dat json-str)
  (with-input-from-string json-str
    (lambda ()
      (json-read))))

;;======================================================================
;;
;; A C T U A L   A P I   C A L L S  
;;
;;======================================================================

;;======================================================================
;;  S E R V E R
;;======================================================================

(define (rmt:kill-server run-id)
  (rmt:send-receive 'kill-server run-id (list run-id)))

(define (rmt:start-server run-id)
  (rmt:send-receive 'start-server 0 (list run-id)))

;;======================================================================
;;  M I S C
;;======================================================================

(define (rmt:login run-id)
  (rmt:send-receive 'login run-id (list *toppath* megatest-version run-id *my-client-signature*)))

;; This login does no retries under the hood - it acts a bit like a ping.
;;
(define (rmt:login-no-auto-client-setup connection-info run-id)
  (rmt:send-receive-no-auto-client-setup connection-info 'login run-id (list *toppath* megatest-version run-id *my-client-signature*)))
  
;; hand off a call to one of the db:queries statements
;; added run-id to make looking up the correct db possible 
;;
(define (rmt:general-call stmtname run-id . params)
  (rmt:send-receive 'general-call run-id (append (list stmtname run-id) params)))

(define (rmt:sync-inmem->db run-id)
  (rmt:send-receive 'sync-inmem->db run-id '()))

(define (rmt:sdb-qry qry val run-id)
  ;; add caching if qry is 'getid or 'getstr
  (rmt:send-receive 'sdb-qry run-id (list qry val)))

;; NOT COMPLETED
(define (rmt:runtests user run-id testpatt params)
  (rmt:send-receive 'runtests run-id testpatt))

;;======================================================================
;;  K E Y S 
;;======================================================================

;; These require run-id because the values come from the run!
;;
(define (rmt:get-key-val-pairs run-id)
  (rmt:send-receive 'get-key-val-pairs run-id (list run-id)))

(define (rmt:get-keys)
  (rmt:send-receive 'get-keys #f '()))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (rmt:get-test-id run-id testname item-path)
  (rmt:send-receive 'get-test-id run-id (list run-id testname item-path)))

(define (rmt:get-test-info-by-id run-id test-id)
  (if (and (number? run-id)(number? test-id))
      (rmt:send-receive 'get-test-info-by-id run-id (list run-id test-id))
      (begin
	(debug:print 0 "WARNING: Bad data handed to rmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
	(print-call-chain)
	#f)))

(define (rmt:test-get-rundir-from-test-id run-id test-id)
  (rmt:send-receive 'test-get-rundir-from-test-id run-id (list run-id test-id)))

(define (rmt:open-test-db-by-test-id run-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id run-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (rmt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment)
  (rmt:send-receive 'test-set-state-status-by-id run-id (list run-id test-id newstate newstatus newcomment)))

(define (rmt:set-tests-state-status run-id testnames currstate currstatus newstate newstatus)
  (rmt:send-receive 'set-tests-state-status run-id (list run-id testnames currstate currstatus newstate newstatus)))

(define (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals)
  (if (number? run-id)
      (rmt:send-receive 'get-tests-for-run run-id (list run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals))
      (begin
	(debug:print "ERROR: rmt:get-tests-for-run called with bad run-id=" run-id)
	(print-call-chain)
	'())))

(define (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
  (let ((run-id-list (if run-ids
			 run-ids
			 (rmt:get-all-run-ids))))
    (apply append (map (lambda (run-id)
			 (rmt:send-receive 'get-tests-for-run-mindata run-id (list run-ids testpatt states status not-in)))
		       run-id-list))))

(define (rmt:delete-test-records run-id test-id)
  (rmt:send-receive 'delete-test-records run-id (list run-id test-id)))

;; This is not needed as test steps are deleted on test delete call
;;
;; (define (rmt:delete-test-step-records run-id test-id)
;;   (rmt:send-receive 'delete-test-step-records run-id (list run-id test-id)))

(define (rmt:test-set-status-state run-id test-id status state msg)
  (rmt:send-receive 'test-set-status-state run-id (list run-id test-id status state msg)))

(define (rmt:test-toplevel-num-items run-id test-name)
  (rmt:send-receive 'test-toplevel-num-items run-id (list run-id test-name)))

;; (define (rmt:get-previous-test-run-record run-id test-name item-path)
;;   (rmt:send-receive 'get-previous-test-run-record run-id (list run-id test-name item-path)))

(define (rmt:get-matching-previous-test-run-records run-id test-name item-path)
  (rmt:send-receive 'get-matching-previous-test-run-records run-id (list run-id test-name item-path)))

(define (rmt:test-get-logfile-info run-id test-name)
  (rmt:send-receive 'test-get-logfile-info run-id (list run-id test-name)))

(define (rmt:test-get-records-for-index-file run-id test-name)
  (rmt:send-receive 'test-get-records-for-index-file run-id (list run-id test-name)))

(define (rmt:get-testinfo-state-status run-id test-id)
  (rmt:send-receive 'get-testinfo-state-status run-id (list run-id test-id)))

(define (rmt:test-set-log! run-id test-id logf)
  (if (string? logf)(rmt:general-call 'test-set-log run-id logf test-id)))

(define (rmt:test-set-top-process-pid run-id test-id pid)
  (rmt:send-receive 'test-set-top-process-pid run-id (list run-id test-id pid)))

(define (rmt:test-get-top-process-pid run-id test-id)
  (rmt:send-receive 'test-get-top-process-pid run-id (list run-id test-id)))

(define (rmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt)
  (rmt:send-receive 'get-run-ids-matching-target #f (list keynames target res runname testpatt statepatt statuspatt)))

;; NOTE: This will open and access ALL run databases. 
;;
(define (rmt:test-get-paths-matching-keynames-target-new keynames target res testpatt statepatt statuspatt runname)
  (let ((run-ids (rmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt)))
    (apply append 
	   (map (lambda (run-id)
		  (rmt:send-receive 'test-get-paths-matching-keynames-target-new run-id (list run-id keynames target res testpatt statepatt statuspatt runname)))
	   run-ids))))

(define (rmt:get-run-ids-matching keynames target res)
  (rmt:send-receive #f 'get-run-ids-matching (list keynames target res)))

(define (rmt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode '(normal)))
  (rmt:send-receive 'get-prereqs-not-met run-id (list run-id waitons ref-item-path mode)))

(define (rmt:get-count-tests-running-for-run-id run-id)
  (rmt:send-receive 'get-count-tests-running-for-run-id run-id (list run-id)))

;; Statistical queries

(define (rmt:get-count-tests-running run-id)
  (rmt:send-receive 'get-count-tests-running run-id (list run-id)))

(define (rmt:get-count-tests-running-in-jobgroup run-id jobgroup)
  (rmt:send-receive 'get-count-tests-running-in-jobgroup run-id (list run-id jobgroup)))

(define (rmt:roll-up-pass-fail-counts run-id test-name item-path status)
  (rmt:send-receive 'roll-up-pass-fail-counts run-id (list run-id test-name item-path status)))

(define (rmt:update-pass-fail-counts run-id test-name)
  (rmt:general-call 'update-fail-pass-counts run-id (list run-id test-name run-id test-name run-id test-name)))

;;======================================================================
;;  R U N S
;;======================================================================

(define (rmt:get-run-info run-id)
  (rmt:send-receive 'get-run-info run-id (list run-id)))

;; Use the special run-id == #f scenario here since there is no run yet
(define (rmt:register-run keyvals runname state status user)
  (rmt:send-receive 'register-run #f (list keyvals runname state status user)))
    
(define (rmt:get-run-name-from-id run-id)
  (rmt:send-receive 'get-run-name-from-id run-id (list run-id)))

(define (rmt:delete-run run-id)
  (rmt:send-receive 'delete-run run-id (list run-id)))

(define (rmt:delete-old-deleted-test-records)
  (rmt:send-receive 'delete-old-deleted-test-records #f '()))

(define (rmt:get-runs runpatt count offset keypatts)
  (rmt:send-receive 'get-runs #f (list runpatt count offset keypatts)))

(define (rmt:get-runs runpatt count offset keypatts)
  (rmt:send-receive 'get-runs #f (list runpatt count offset keypatts)))

(define (rmt:get-all-run-ids)
  (rmt:send-receive 'get-all-run-ids #f '()))

(define (rmt:get-prev-run-ids run-id)
  (rmt:send-receive 'get-prev-run-ids #f (list run-id)))

(define (rmt:lock/unlock-run run-id lock unlock user)
  (rmt:send-receive 'lock/unlock-run #f (list run-id lock unlock user)))

;; set/get status
(define (rmt:get-run-status run-id)
  (rmt:send-receive 'get-run-status #f (list run-id)))

(define (rmt:set-run-status run-id run-status #!key (msg #f))
  (rmt:send-receive 'set-run-status #f (list run-id run-status msg)))

(define (rmt:update-run-event_time run-id)
  (rmt:send-receive 'update-run-event_time #f (list run-id)))

(define (rmt:get-runs-by-patt  keys runnamepatt targpatt offset limit)
  (rmt:send-receive 'get-runs-by-patt #f (list keys runnamepatt targpatt offset limit)))

(define (rmt:find-and-mark-incomplete run-id ovr-deadtime)
  (rmt:send-receive 'find-and-mark-incomplete run-id (list run-id ovr-deadtime)))

;;======================================================================
;; M U L T I R U N   Q U E R I E S
;;======================================================================

;; Need to move this to multi-run section and make associated changes
(define (rmt:find-and-mark-incomplete-all-runs #!key (ovr-deadtime #f))
  (let ((run-ids (rmt:get-all-run-ids)))
    (for-each (lambda (run-id)
	       (rmt:find-and-mark-incomplete run-id ovr-deadtime))
	     run-ids)))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
;; 
;; Run this at the client end since we have to connect to multiple run-id dbs
;;
(define (rmt:get-previous-test-run-record run-id test-name item-path)
  (let* ((keyvals (rmt:get-key-val-pairs run-id))
	 (keys    (rmt:get-keys))
	 (selstr  (string-intersperse  keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND ")))
    (if (not keyvals)
	#f
	(let ((prev-run-ids (rmt:get-prev-run-ids run-id)))
	  ;; for each run starting with the most recent look to see if there is a matching test
	  ;; if found then return that matching test record
	  (debug:print 4 "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) #f
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (rmt:get-tests-for-run hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f)))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))

;;======================================================================
;;  S T E P S
;;======================================================================

;; Getting steps is more complicated.
;;
;; If given work area 
;;  1. Find the testdat.db file
;;  2. Open the testdat.db file and do the query
;; If not given the work area
;;  1. Do a remote call to get the test path
;;  2. Continue as above
;; 
(define (rmt:get-steps-for-test run-id test-id)
  (rmt:send-receive 'get-steps-data run-id (list test-id)))

(define (rmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile)
  (let* ((state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (rmt:send-receive 'teststep-set-status! run-id (list run-id test-id teststep-name state-in status-in comment logfile))))

(define (rmt:get-steps-for-test run-id test-id)
  (rmt:send-receive 'get-steps-for-test run-id (list test-id)))

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (rmt:read-test-data run-id test-id categorypatt #!key (work-area #f)) 
  (let ((tdb  (rmt:open-test-db-by-test-id run-id test-id work-area: work-area)))
    (if tdb
	(tdb:read-test-data tdb test-id categorypatt)
	'())))

(define (rmt:testmeta-add-record testname)
  (rmt:send-receive 'testmeta-add-record #f (list testname)))

(define (rmt:testmeta-get-record testname)
  (rmt:send-receive 'testmeta-get-record #f (list testname)))

(define (rmt:testmeta-update-field test-name fld val)
  (rmt:send-receive 'testmeta-update-field #f (list test-name fld val)))

(define (rmt:test-data-rollup run-id test-id status)
  (rmt:send-receive 'test-data-rollup run-id (list run-id test-id status)))

(define (rmt:csv->test-data run-id test-id csvdata)
  (rmt:send-receive 'csv->test-data run-id (list run-id test-id csvdata)))