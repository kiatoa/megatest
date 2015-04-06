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
(declare (uses nmsg-transport))

;;
;; THESE ARE ALL CALLED ON THE CLIENT SIDE!!!
;;

;; ;; For debugging add the following to ~/.megatestrc
;;a
;; (require-library trace)
;; (import trace)
;; (trace
;; rmt:send-receive
;; api:execute-requests
;; )

;; generate entries for ~/.megatestrc with the following
;;
;;  grep define ../rmt.scm | grep rmt: |perl -pi -e 's/\(define\s+\((\S+)\W.*$/\1/'|sort -u


;;======================================================================
;;  S U P P O R T   F U N C T I O N S
;;======================================================================

;; NOT USED?
;;
;; (define (rmt:call-transport run-id connection-info cmd jparams)
;;   (case (server:get-transport)
;;     ((rpc)  ( rpc-transport:client-api-send-receive run-id connection-info cmd jparams))
;;     ((http) (http-transport:client-api-send-receive run-id connection-info cmd jparams))
;;     ((fs)   ( fs-transport:client-api-send-receive run-id connection-info cmd jparams))
;;     ((zmq)  (zmq-transport:client-api-send-receive run-id connection-info cmd jparams))
;;     (else   ( rpc-transport:client-api-send-receive run-id connection-info cmd jparams))))

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

;; if a server is either running or in the process of starting call client:setup
;; else return #f to let the calling proc know that there is no server available
;;
(define (rmt:get-connection-info run-id area-dat #!key (remote #f))
  (let ((cinfo (common:get-remote remote run-id)))
    (if cinfo
	cinfo
	;; NB// can cache the answer for server running for 10 seconds ...
	;;  ;; (and (not (rmt:write-frequency-over-limit? cmd run-id))
	(if (tasks:server-running-or-starting? (db:delay-if-busy (tasks:open-db area-dat) area-dat) run-id)
	    (client:setup run-id area-dat remote: remote)
	    #f))))

(define (rmt:discard-old-connections area-dat)
  ;; clean out old connections
  (mutex-lock! *db-multi-sync-mutex*)
  (let ((remote      (megatest:area-remote area-dat))
	(expire-time (- (current-seconds) (server:get-timeout area-dat) 10))) ;; don't forget the 10 second margin
    (for-each 
     (lambda (run-id)
       (let ((connection (common:get-remote remote run-id)))
         (if (and (vector? connection)
        	  (< (http-transport:server-dat-get-last-access connection) expire-time))
             (begin
               (debug:print-info 0 "Discarding connection to server for run-id " run-id ", too long between accesses")
               ;; SHOULD CLOSE THE CONNECTION HERE
	       (case (megatest:area-transport area-dat)
		 ((nmsg)(nn-close (http-transport:server-dat-get-socket 
				   (common:get-remote remote run-id)))))
               (common:del-remote! remote run-id)))))
     (common:get-remote-all remote)))
  (mutex-unlock! *db-multi-sync-mutex*))

(define *send-receive-mutex* (make-mutex)) ;; should have separate mutex per run-id

(define (rmt:send-receive cmd rid params area-dat #!key (attemptnum 1)(remote #f)) ;; start attemptnum at 1 so the modulo below works as expected
  (rmt:discard-old-connections area-dat)
  ;; (mutex-lock! *send-receive-mutex*)
  (let* ((transport-type  (megatest:area-transport area-dat))
	 (run-id          (if rid rid 0))
	 (configdat       (megatest:area-configdat area-dat))
	 (connection-info (rmt:get-connection-info run-id area-dat)))
    ;; the nmsg method does the encoding under the hood (the http method should be changed to do this also)
    (if connection-info
	;; use the server if have connection info
	(let* ((dat     (case transport-type
			  ((http)(condition-case
				  (http-transport:client-api-send-receive run-id connection-info cmd params)
				  ((commfail)(vector #f "communications fail"))
				  ((exn)(vector #f "other fail"))))
			  ((nmsg)(condition-case
				  (nmsg-transport:client-api-send-receive run-id connection-info cmd params)
				  ((timeout)(vector #f "timeout talking to server"))))
			  (else  (exit))))
	       (success (if (vector? dat) (vector-ref dat 0) #f))
	       (res     (if (vector? dat) (vector-ref dat 1) #f)))
	  (if (vector? connection-info)(http-transport:server-dat-update-last-access connection-info))
	  (if success
	      (begin
		;; (mutex-unlock! *send-receive-mutex*)
		(case 
		  ((http) res) ;; (db:string->obj res))
		  ((nmsg) res))) ;; (vector-ref res 1)))
	      (begin ;; let ((new-connection-info (client:setup run-id)))
		(debug:print 0 "WARNING: Communication failed, trying call to rmt:send-receive again.")
		;; (case *transport-type*
		;;   ((nmsg)(nn-close (http-transport:server-dat-get-socket connection-info))))
		(common:del-remote! remote run-id) ;; don't keep using the same connection
		;; NOTE: killing server causes this process to block forever. No idea why. Dec 2. 
		;; (if (eq? (modulo attemptnum 5) 0)
		;;     (tasks:kill-server-run-id run-id tag: "api-send-receive-failed"))
		;; (mutex-unlock! *send-receive-mutex*) ;; close the mutex here to allow other threads access to communications
		(tasks:start-and-wait-for-server (tasks:open-db area-dat) run-id 15)
		;; (nmsg-transport:client-api-send-receive run-id connection-info cmd param remtries: (- remtries 1))))))

		;; no longer killing the server in http-transport:client-api-send-receive
		;; may kill it here but what are the criteria?
		;; start with three calls then kill server
		;; (if (eq? attemptnum 3)(tasks:kill-server-run-id run-id))
		;; (thread-sleep! 2)
		(rmt:send-receive cmd run-id params area-dat attemptnum: (+ attemptnum 1)))))
	;; no connection info? try to start a server, or access locally if no
	;; server and the query is read-only
	;;
	;; Note: The tasks db was checked for a server in starting mode in the rmt:get-connection-info call
	;;
	(if (and (< attemptnum 15)
		 (member cmd api:write-queries))
	    (let ((faststart (configf:lookup configdat "server" "faststart")))
	      (common:del-remote! remote run-id)
	      ;; (mutex-unlock! *send-receive-mutex*)
	      (if (and faststart (equal? faststart "no"))
		  (begin
		    (tasks:start-and-wait-for-server (db:delay-if-busy (tasks:open-db area-dat)) run-id 10)
		    (thread-sleep! (random 5)) ;; give some time to settle and minimize collison?
		    (rmt:send-receive cmd rid params area-dat attemptnum: (+ attemptnum 1)))
		  (begin
		    (server:kind-run run-id area-dat)
		    (rmt:open-qry-close-locally cmd run-id area-dat params area-dat))))
	    (begin
	      ;; (debug:print 0 "ERROR: Communication failed!")
	      ;; (mutex-unlock! *send-receive-mutex*)
	      ;; (exit)
	      (rmt:open-qry-close-locally cmd run-id area-dat params area-dat)
	      )))))

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
     (if (not (vector? stat-vec))
	 (let ((newvec (vector 0 0)))
	   (hash-table-set! *db-stats* cmd newvec)
	   (set! stat-vec newvec)))
     (vector-set! stat-vec 0 (+ (vector-ref stat-vec 0) 1))
     (vector-set! stat-vec 1 (+ (vector-ref stat-vec 1) duration))))
  (mutex-unlock! *db-stats-mutex*))


(define (rmt:print-db-stats area-dat)
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
	  
(define (rmt:open-qry-close-locally cmd run-id area-dat params #!key (remretries 5))
  (let* ((dbstruct-local (if *dbstruct-db*
			     *dbstruct-db*
			     (let* ((dbdir (db:dbfile-path #f area-dat))
				    (db (make-dbr:dbstruct path:  dbdir local: #t)))
			       (set! *dbstruct-db* db)
			       db)))
	 (db-file-path   (db:dbfile-path 0 area-dat))
	 ;; (read-only      (not (file-read-access? db-file-path)))
	 (start          (current-milliseconds))
	 (resdat         (api:execute-requests dbstruct-local area-dat (vector (symbol->string cmd) params)))
	 (success        (vector-ref resdat 0))
	 (res            (vector-ref resdat 1))
	 (duration       (- (current-milliseconds) start)))
    (if (not success)
	(if (> remretries 0)
	    (begin
	      (debug:print 0 "ERROR: local query failed. Trying again.")
	      (thread-sleep! (/ (random 5000) 1000)) ;; some random delay 
	      (rmt:open-qry-close-locally cmd run-id area-dat params remretries: (- remretries 1)))
	    (begin
	      (debug:print 0 "ERROR: too many retries in rmt:open-qry-close-locally, giving up")
	      #f))
	(begin
	  (rmt:update-db-stats run-id cmd params duration)
	  ;; mark this run as dirty if this was a write
	  (if (not (member cmd api:read-only-queries))
	      (let ((start-time (current-seconds)))
		(mutex-lock! *db-multi-sync-mutex*)
		;; (if (not (hash-table-ref/default *db-local-sync* run-id #f))
		;; just set it every time. Is a write more expensive than a read and does it matter?
		(hash-table-set! *db-local-sync* (or run-id 0) start-time) ;; the oldest "write"
		(mutex-unlock! *db-multi-sync-mutex*)))
	  res))))

(define (rmt:send-receive-no-auto-client-setup connection-info cmd run-id params area-dat)
  (let* ((run-id   (if run-id run-id 0))
	 ;; (jparams  (db:obj->string params)) ;; (rmt:dat->json-str params))
	 (res  	   (handle-exceptions
		    exn
		    #f
		    (http-transport:client-api-send-receive run-id connection-info cmd params))))
;;		    ((commfail) (vector #f "communications fail")))))
    (if (and res (vector-ref res 0))
	(vector-ref res 1) ;;; YES!! THIS IS CORRECT!! CHANGE IT HERE, THEN CHANGE rmt:send-receive ALSO!!!
	#f)))
;; 	(db:string->obj (vector-ref dat 1))
;; 	(begin
;; 	  (debug:print 0 "ERROR: rmt:send-receive-no-auto-client-setup failed, attempting to continue. Got " dat)
;; 	  dat))))

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
  (rmt:send-receive 'kill-server run-id (list run-id) area-dat))

(define (rmt:start-server run-id)
  (rmt:send-receive 'start-server 0 (list run-id) area-dat))

;;======================================================================
;;  M I S C
;;======================================================================

(define (rmt:login run-id area-dat)
  (rmt:send-receive 'login run-id (list (megatest:area-path area-dat) megatest-version run-id *my-client-signature*) area-dat))

;; This login does no retries under the hood - it acts a bit like a ping.
;; Deprecated for nmsg-transport.
;;
(define (rmt:login-no-auto-client-setup connection-info run-id area-dat)
  (let ((transport (megatest:area-transport area-dat))
	(toppath   (megatest:area-path      area-dat)))
    (case transport
      ((http)(rmt:send-receive-no-auto-client-setup connection-info 'login run-id  (list toppath megatest-version run-id *my-client-signature*) area-dat))
      ((nmsg)(nmsg-transport:client-api-send-receive run-id connection-info 'login (list toppath megatest-version run-id *my-client-signature*) area-dat)))))

;; hand off a call to one of the db:queries statements
;; added run-id to make looking up the correct db possible 
;;
(define (rmt:general-call stmtname run-id area-dat . params)
  (rmt:send-receive 'general-call run-id (append (list stmtname run-id) params) area-dat))

(define (rmt:sync-inmem->db run-id area-dat)
  (rmt:send-receive 'sync-inmem->db run-id '() area-dat))

(define (rmt:sdb-qry qry val run-id area-dat)
  ;; add caching if qry is 'getid or 'getstr
  (rmt:send-receive 'sdb-qry run-id (list qry val) area-dat))

;; NOT COMPLETED
(define (rmt:runtests user run-id testpatt params area-dat)
  (rmt:send-receive 'runtests run-id testpatt area-dat))

;;======================================================================
;;  K E Y S 
;;======================================================================

;; These require run-id because the values come from the run!
;;
(define (rmt:get-key-val-pairs run-id area-dat)
  (rmt:send-receive 'get-key-val-pairs run-id (list run-id) area-dat))

(define (rmt:get-keys area-dat)
  (rmt:send-receive 'get-keys #f '() area-dat))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (rmt:get-test-id run-id testname item-path area-dat)
  (rmt:send-receive 'get-test-id run-id (list run-id testname item-path) area-dat))

(define (rmt:get-test-info-by-id run-id test-id area-dat)
  (if (and (number? run-id)(number? test-id))
      (rmt:send-receive 'get-test-info-by-id run-id (list run-id test-id) area-dat)
      (begin
	(debug:print 0 "WARNING: Bad data handed to rmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
	(print-call-chain (current-error-port))
	#f)))

(define (rmt:test-get-rundir-from-test-id run-id test-id area-dat)
  (rmt:send-receive 'test-get-rundir-from-test-id run-id (list run-id test-id) area-dat))

(define (rmt:open-test-db-by-test-id run-id test-id area-dat #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id run-id test-id area-dat))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path area-dat)))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (rmt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment area-dat)
  (rmt:send-receive 'test-set-state-status-by-id run-id (list run-id test-id newstate newstatus newcomment) area-dat))

(define (rmt:set-tests-state-status run-id testnames currstate currstatus newstate newstatus area-dat)
  (rmt:send-receive 'set-tests-state-status run-id (list run-id testnames currstate currstatus newstate newstatus) area-dat))

(define (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals area-dat)
  (if (number? run-id)
      (rmt:send-receive 'get-tests-for-run run-id (list run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals) area-dat)
      (begin
	(debug:print "ERROR: rmt:get-tests-for-run called with bad run-id=" run-id)
	(print-call-chain (current-error-port))
	'())))

;; get stuff via synchash 
(define (rmt:synchash-get run-id proc synckey keynum params area-dat)
  (rmt:send-receive 'synchash-get run-id (list run-id proc synckey keynum params) area-dat))

;; IDEA: Threadify these - they spend a lot of time waiting ...
;;
(define (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in area-dat)
  (let ((multi-run-mutex (make-mutex))
	(run-id-list (if run-ids
			 run-ids
			 (rmt:get-all-run-ids)))
	(result      '()))
    (if (null? run-id-list)
	'()
	(let loop ((hed     (car run-id-list))
		   (tal     (cdr run-id-list))
		   (threads '()))
	  (if (> (length threads) 5)
	      (loop hed tal (filter (lambda (th)(not (member (thread-state th) '(terminated dead)))) threads))
	      (let* ((newthread (make-thread
				 (lambda ()
				   (let ((res (rmt:send-receive 'get-tests-for-run-mindata hed (list hed testpatt states status not-in) area-dat)))
				     (if (list? res)
					 (begin
					   (mutex-lock! multi-run-mutex)
					   (set! result (append result res))
					   (mutex-unlock! multi-run-mutex))
					 (debug:print 0 "ERROR: get-tests-for-run-mindata failed for run-id " hed ", testpatt " testpatt ", states " states ", status " status ", not-in " not-in))))
				 (conc "multi-run-thread for run-id " hed)))
		     (newthreads (cons newthread threads)))
		(thread-start! newthread)
		(thread-sleep! 0.5) ;; give that thread some time to start
		(if (null? tal)
		    newthreads
		    (loop (car tal)(cdr tal) newthreads))))))
    result))

;; ;; IDEA: Threadify these - they spend a lot of time waiting ...
;; ;;
;; (define (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
;;   (let ((run-id-list (if run-ids
;; 			 run-ids
;; 			 (rmt:get-all-run-ids))))
;;     (apply append (map (lambda (run-id)
;; 			 (rmt:send-receive 'get-tests-for-run-mindata run-id (list run-ids testpatt states status not-in)))
;; 		       run-id-list))))

(define (rmt:delete-test-records run-id test-id area-dat)
  (rmt:send-receive 'delete-test-records run-id (list run-id test-id) area-dat))

;; This is not needed as test steps are deleted on test delete call
;;
;; (define (rmt:delete-test-step-records run-id test-id)
;;   (rmt:send-receive 'delete-test-step-records run-id (list run-id test-id)))

(define (rmt:test-set-status-state run-id test-id status state msg area-dat)
  (rmt:send-receive 'test-set-status-state run-id (list run-id test-id status state msg) area-dat))

(define (rmt:test-toplevel-num-items run-id test-name area-dat)
  (rmt:send-receive 'test-toplevel-num-items run-id (list run-id test-name) area-dat))

;; (define (rmt:get-previous-test-run-record run-id test-name item-path)
;;   (rmt:send-receive 'get-previous-test-run-record run-id (list run-id test-name item-path)))

(define (rmt:get-matching-previous-test-run-records run-id test-name item-path area-dat)
  (rmt:send-receive 'get-matching-previous-test-run-records run-id (list run-id test-name item-path) area-dat))

(define (rmt:test-get-logfile-info run-id test-name area-dat)
  (rmt:send-receive 'test-get-logfile-info run-id (list run-id test-name) area-dat))

(define (rmt:test-get-records-for-index-file run-id test-name area-dat)
  (rmt:send-receive 'test-get-records-for-index-file run-id (list run-id test-name) area-dat))

(define (rmt:get-testinfo-state-status run-id test-id area-dat)
  (rmt:send-receive 'get-testinfo-state-status run-id (list run-id test-id) area-dat))

(define (rmt:test-set-log! run-id test-id logf area-dat)
  (if (string? logf)(rmt:general-call 'test-set-log run-id logf test-id area-dat)))

(define (rmt:test-set-top-process-pid run-id test-id pid area-dat)
  (rmt:send-receive 'test-set-top-process-pid run-id (list run-id test-id pid) area-dat))

(define (rmt:test-get-top-process-pid run-id test-id area-dat)
  (rmt:send-receive 'test-get-top-process-pid run-id (list run-id test-id) area-dat))

(define (rmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt area-dat)
  (rmt:send-receive 'get-run-ids-matching-target #f (list keynames target res runname testpatt statepatt statuspatt) area-dat))

;; NOTE: This will open and access ALL run databases. 
;;
(define (rmt:test-get-paths-matching-keynames-target-new keynames target res testpatt statepatt statuspatt runname area-dat)
  (let ((run-ids (rmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt area-dat)))
    (apply append 
	   (map (lambda (run-id)
		  (rmt:send-receive 'test-get-paths-matching-keynames-target-new run-id (list run-id keynames target res testpatt statepatt statuspatt runname) area-dat))
	   run-ids))))

(define (rmt:get-run-ids-matching keynames target res area-dat)
  (rmt:send-receive #f 'get-run-ids-matching (list keynames target res) area-dat) area-dat)

(define (rmt:get-prereqs-not-met run-id waitons ref-item-path area-dat #!key (mode '(normal)))
  (rmt:send-receive 'get-prereqs-not-met run-id (list run-id waitons ref-item-path mode) area-dat))

(define (rmt:get-count-tests-running-for-run-id run-id area-dat)
  (rmt:send-receive 'get-count-tests-running-for-run-id run-id (list run-id) area-dat))

;; Statistical queries

(define (rmt:get-count-tests-running run-id area-dat)
  (rmt:send-receive 'get-count-tests-running run-id (list run-id) area-dat))

(define (rmt:get-count-tests-running-for-testname run-id testname area-dat)
  (rmt:send-receive 'get-count-tests-running-for-testname run-id (list run-id testname) area-dat))

(define (rmt:get-count-tests-running-in-jobgroup run-id jobgroup area-dat)
  (rmt:send-receive 'get-count-tests-running-in-jobgroup run-id (list run-id jobgroup) area-dat))

(define (rmt:roll-up-pass-fail-counts run-id test-name item-path status area-dat)
  (rmt:send-receive 'roll-up-pass-fail-counts run-id (list run-id test-name item-path status) area-dat))

(define (rmt:update-pass-fail-counts run-id test-name area-dat)
  (rmt:general-call 'update-fail-pass-counts run-id (list run-id test-name run-id test-name run-id test-name) area-dat))

;;======================================================================
;;  R U N S
;;======================================================================

(define (rmt:get-run-info run-id area-dat)
  (rmt:send-receive 'get-run-info run-id (list run-id) area-dat))

;; Use the special run-id == #f scenario here since there is no run yet
(define (rmt:register-run keyvals runname state status user area-dat)
  (rmt:send-receive 'register-run #f (list keyvals runname state status user) area-dat))
    
(define (rmt:get-run-name-from-id run-id area-dat)
  (rmt:send-receive 'get-run-name-from-id run-id (list run-id) area-dat))

(define (rmt:delete-run run-id area-dat)
  (rmt:send-receive 'delete-run run-id (list run-id) area-dat))

(define (rmt:delete-old-deleted-test-records area-dat)
  (rmt:send-receive 'delete-old-deleted-test-records #f '() area-dat))

(define (rmt:get-runs runpatt count offset keypatts area-dat)
  (rmt:send-receive 'get-runs #f (list runpatt count offset keypatts) area-dat))

(define (rmt:get-runs runpatt count offset keypatts area-dat)
  (rmt:send-receive 'get-runs #f (list runpatt count offset keypatts) area-dat))

(define (rmt:get-all-run-ids area-dat)
  (rmt:send-receive 'get-all-run-ids #f '() area-dat))

(define (rmt:get-prev-run-ids run-id area-dat)
  (rmt:send-receive 'get-prev-run-ids #f (list run-id) area-dat))

(define (rmt:lock/unlock-run run-id lock unlock user area-dat)
  (rmt:send-receive 'lock/unlock-run #f (list run-id lock unlock user) area-dat))

;; set/get status
(define (rmt:get-run-status run-id area-dat)
  (rmt:send-receive 'get-run-status #f (list run-id) area-dat))

(define (rmt:set-run-status run-id run-status area-dat #!key (msg #f))
  (rmt:send-receive 'set-run-status #f (list run-id run-status msg) area-dat))

(define (rmt:update-run-event_time run-id area-dat)
  (rmt:send-receive 'update-run-event_time #f (list run-id) area-dat))

(define (rmt:get-runs-by-patt  keys runnamepatt targpatt offset limit area-dat)
  (rmt:send-receive 'get-runs-by-patt #f (list keys runnamepatt targpatt offset limit) area-dat))

(define (rmt:find-and-mark-incomplete run-id ovr-deadtime area-dat)
  (if (rmt:send-receive 'have-incompletes? run-id (list run-id ovr-deadtime) area-dat)
      (rmt:send-receive 'mark-incomplete run-id (list run-id ovr-deadtime) area-dat)))

;;======================================================================
;; M U L T I R U N   Q U E R I E S
;;======================================================================

;; Need to move this to multi-run section and make associated changes
(define (rmt:find-and-mark-incomplete-all-runs area-dat #!key (ovr-deadtime #f))
  (let ((run-ids (rmt:get-all-run-ids area-dat)))
    (for-each (lambda (run-id)
	       (rmt:find-and-mark-incomplete run-id ovr-deadtime area-dat))
	     run-ids)))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
;; 
;; Run this at the client end since we have to connect to multiple run-id dbs
;;
(define (rmt:get-previous-test-run-record run-id test-name item-path area-dat)
  (let* ((keyvals (rmt:get-key-val-pairs run-id area-dat))
	 (keys    (rmt:get-keys area-dat))
	 (selstr  (string-intersperse  keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND ")))
    (if (not keyvals)
	#f
	(let ((prev-run-ids (rmt:get-prev-run-ids run-id area-dat)))
	  ;; for each run starting with the most recent look to see if there is a matching test
	  ;; if found then return that matching test record
	  (debug:print 4 "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) #f
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (rmt:get-tests-for-run hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f area-dat)))
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
;;(define (rmt:get-steps-for-test run-id test-id)
;;  (rmt:send-receive 'get-steps-data run-id (list test-id)))

(define (rmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile area-dat)
  (let* ((state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (rmt:send-receive 'teststep-set-status! run-id (list run-id test-id teststep-name state-in status-in comment logfile) area-dat)))

(define (rmt:get-steps-for-test run-id test-id area-dat)
  (rmt:send-receive 'get-steps-for-test run-id (list run-id test-id) area-dat))

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (rmt:read-test-data run-id test-id categorypatt area-dat #!key (work-area #f)) 
  (let ((tdb  (rmt:open-test-db-by-test-id run-id test-id area-dat work-area: work-area)))
    (if tdb
	(tdb:read-test-data tdb test-id categorypatt area-dat)
	'())))

(define (rmt:testmeta-add-record testname area-dat)
  (rmt:send-receive 'testmeta-add-record #f (list testname) area-dat))

(define (rmt:testmeta-get-record testname area-dat)
  (rmt:send-receive 'testmeta-get-record #f (list testname) area-dat))

(define (rmt:testmeta-update-field test-name fld val area-dat)
  (rmt:send-receive 'testmeta-update-field #f (list test-name fld val) area-dat))

(define (rmt:test-data-rollup run-id test-id status area-dat)
  (rmt:send-receive 'test-data-rollup run-id (list run-id test-id status) area-dat))

(define (rmt:csv->test-data run-id test-id csvdata area-dat)
  (rmt:send-receive 'csv->test-data run-id (list run-id test-id csvdata) area-dat))

;;======================================================================
;;  T A S K S
;;======================================================================

(define (rmt:tasks-find-task-queue-records target run-name test-patt state-patt action-patt area-dat)
  (rmt:send-receive 'find-task-queue-records #f (list target run-name test-patt state-patt action-patt) area-dat))

(define (rmt:tasks-add action owner target runname testpatt params area-dat)
  (rmt:send-receive 'tasks-add #f (list action owner target runname testpatt params) area-dat))

(define (rmt:tasks-set-state-given-param-key param-key new-state area-dat)
  (rmt:send-receive 'tasks-set-state-given-param-key #f (list  param-key new-state) area-dat))

;;======================================================================
;; A R C H I V E S
;;======================================================================

(define (rmt:archive-get-allocations  testname itempath dneeded area-dat)
  (rmt:send-receive 'archive-get-allocations #f (list testname itempath dneeded) area-dat))

(define (rmt:archive-register-block-name bdisk-id archive-path area-dat)
  (rmt:send-receive 'archive-register-block-name #f (list bdisk-id archive-path) area-dat))

(define (rmt:archive-allocate-testsuite/area-to-block block-id testsuite-name areakey area-dat)
  (rmt:send-receive 'archive-allocate-test-to-block #f (list  block-id testsuite-name areakey) area-dat))

(define (rmt:archive-register-disk bdisk-name bdisk-path df area-dat)
  (rmt:send-receive 'archive-register-disk #f (list bdisk-name bdisk-path df) area-dat))

(define (rmt:test-set-archive-block-id run-id test-id archive-block-id area-dat)
  (rmt:send-receive 'test-set-archive-block-id run-id (list run-id test-id archive-block-id) area-dat))

(define (rmt:test-get-archive-block-info archive-block-id area-dat)
  (rmt:send-receive 'test-get-archive-block-info #f (list archive-block-id) area-dat))
