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

(use format typed-records) ;; RADT => purpose of json format??

(declare (unit rmt))
(declare (uses api))
(declare (uses tdb))
(declare (uses http-transport))
;;(declare (uses nmsg-transport))
(include "common_records.scm")

;;
;; THESE ARE ALL CALLED ON THE CLIENT SIDE!!!
;;

;; generate entries for ~/.megatestrc with the following
;;
;;  grep define ../rmt.scm | grep rmt: |perl -pi -e 's/\(define\s+\((\S+)\W.*$/\1/'|sort -u

;;======================================================================
;;  S U P P O R T   F U N C T I O N S
;;======================================================================

;; if a server is either running or in the process of starting call client:setup
;; else return #f to let the calling proc know that there is no server available
;;
(define (rmt:get-connection-info areapath #!key (area-dat #f)) ;; TODO: push areapath down.
  (let* ((runremote (or area-dat *runremote*))
	 (cinfo     (remote-conndat runremote))
        (run-id 0))
    (if cinfo
	cinfo
	(if (server:check-if-running areapath)
	    (client:setup areapath)
	    #f))))

(define *send-receive-mutex* (make-mutex)) ;; should have separate mutex per run-id

;; RA => e.g. usage (rmt:send-receive 'get-var #f (list varname))
;;
(define (rmt:send-receive cmd rid params #!key (attemptnum 1)(area-dat #f)) ;; start attemptnum at 1 so the modulo below works as expected

  ;; do all the prep locked under the rmt-mutex
  (mutex-lock! *rmt-mutex*)

  ;; 1. check if server is started IFF cmd is a write OR if we are not on the homehost, store in runremote
  ;; 2. check the age of the connections. refresh the connection if it is older than timeout-20 seconds.
  ;; 3. do the query, if on homehost use local access
  ;;
  (let* ((start-time (current-seconds)) ;; snapshot time so all use cases get same value
         (areapath *toppath*);; TODO - resolve from dbstruct to be compatible with multiple areas
         (dbfile (conc *toppath* "/megatest.db"))
         (readonly-mode (not (file-write-access? dbfile))) ;; TODO: use dbstruct or runremote to figure this out in future
	 (runremote  (or area-dat *runremote*)))
    ;;(print "BB> readonly-mode is "readonly-mode" dbfile is "dbfile)
    (cond
     ;; give up if more than 15 attempts
     ((> attemptnum 15)
      (debug:print 0 *default-log-port* "ERROR: 15 tries to start/connect to server. Giving up.")
      (exit 1))

     ;; readonly mode, read request-  handle it - case 20
     ((and readonly-mode
           (member cmd api:read-only-queries)) 
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case 20")
      (rmt:open-qry-close-locally cmd 0 params)
      )

     ;; readonly mode, write request.  Do nothing, return #f
     (readonly-mode
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case 21")
      (debug:print 0 *default-log-port* "WARNING: write transaction requested on a readonly area.  cmd="cmd" params="params)
      #f
      )

     ;; reset the connection if it has been unused too long
     ((and runremote
           (remote-conndat runremote)
	   (let ((expire-time (+ (- start-time (remote-server-timeout runremote))(random 30)))) ;; add 30 seconds of noise so that not all running tests expire at the same time causing a storm of server starts
	     (< (http-transport:server-dat-get-last-access (remote-conndat runremote)) expire-time)))
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  8")
      (remote-conndat-set! runremote #f)
      (mutex-unlock! *rmt-mutex*)
      (rmt:send-receive cmd rid params attemptnum: attemptnum))
     ;; ensure we have a record for our connection for given area
     ((not runremote)                     
      (set! *runremote* (make-remote))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  1")
      (rmt:send-receive cmd rid params attemptnum: attemptnum))
     ;; ensure we have a homehost record
     ((not (pair? (remote-hh-dat runremote)))  ;; not on homehost
      (thread-sleep! 0.1) ;; since we shouldn't get here, delay a little
      (remote-hh-dat-set! runremote (common:get-homehost))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  2")
      (rmt:send-receive cmd rid params attemptnum: attemptnum))
     ;; on homehost and this is a read
     ((and (cdr (remote-hh-dat runremote))   ;; on homehost
           (member cmd api:read-only-queries)) ;; this is a read
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  3")
      (rmt:open-qry-close-locally cmd 0 params))

     ;; on homehost and this is a write, we already have a server, but server has died
     ((and (cdr (remote-hh-dat runremote))         ;; on homehost
           (not (member cmd api:read-only-queries))  ;; this is a write
           (remote-server-url runremote)           ;; have a server
           (not (server:check-if-running *toppath*)))  ;; server has died.
      (set! *runremote* (make-remote))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  4.1")
      (rmt:send-receive cmd rid params attemptnum: attemptnum))

     ;; on homehost and this is a write, we already have a server
     ((and (cdr (remote-hh-dat runremote))         ;; on homehost
           (not (member cmd api:read-only-queries))  ;; this is a write
           (remote-server-url runremote))          ;; have a server
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  4")
      (rmt:open-qry-close-locally cmd 0 params))

     ;;  on homehost, no server contact made and this is a write, passively start a server 
     ((and (cdr (remote-hh-dat runremote)) ; new
           (not (remote-server-url runremote))
	   (not (member cmd api:read-only-queries)))
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  5")
      (let ((server-url  (server:check-if-running *toppath*))) ;; (server:read-dotserver->url *toppath*))) ;; (server:check-if-running *toppath*))) ;; Do NOT want to run server:check-if-running - very expensive to do for every write call
	(if server-url
	    (remote-server-url-set! runremote server-url) ;; the string can be consumed by the client setup if needed
	    (server:kind-run *toppath*)))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  5.1")
      (rmt:open-qry-close-locally cmd 0 params))

     ((and (not (cdr (remote-hh-dat runremote)))        ;; not on a homehost 
           (not (remote-conndat runremote)))            ;; and no connection
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  6  hh-dat: " (remote-hh-dat runremote) " conndat: " (remote-conndat runremote))
      (mutex-unlock! *rmt-mutex*)
      (server:start-and-wait *toppath*)
      (remote-conndat-set! runremote (rmt:get-connection-info *toppath*)) ;; calls client:setup which calls client:setup-http
      (rmt:send-receive cmd rid params attemptnum: attemptnum)) ;; TODO: add back-off timeout as
     ;; all set up if get this far, dispatch the query
     ((cdr (remote-hh-dat runremote)) ;; we are on homehost
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  7")
      (rmt:open-qry-close-locally cmd (if rid rid 0) params))

     ;; not on homehost, do server query
     (else
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "rmt:send-receive, case  9")
      (mutex-lock! *rmt-mutex*)
      (let* ((conninfo (remote-conndat runremote))
	     (dat      (case (remote-transport runremote)
			 ((http) (condition-case ;; handling here has caused a lot of problems. However it is needed to deal with attemtped communication to servers that have gone away
                                  (http-transport:client-api-send-receive 0 conninfo cmd params)
                                  ((commfail)(vector #f "communications fail"))
                                  ((exn)(vector #f "other fail" (print-call-chain)))))
			 (else
			  (debug:print 0 *default-log-port* "ERROR: transport " (remote-transport runremote) " not supported")
			  (exit))))
	     (success  (if (vector? dat) (vector-ref dat 0) #f))
	     (res      (if (vector? dat) (vector-ref dat 1) #f)))
	(if (vector? conninfo)(http-transport:server-dat-update-last-access conninfo)) ;; refresh access time
	;; (mutex-unlock! *rmt-mutex*)
        (debug:print-info 12 *default-log-port* "rmt:send-receive, case  9. conninfo=" conninfo " dat=" dat " runremote = "runremote)
	(if success
	    (case (remote-transport runremote)
	      ((http)
	       (mutex-unlock! *rmt-mutex*)
	       res)
	      (else
	       (debug:print 0 *default-log-port* "ERROR: transport " (remote-transport runremote) " is unknown")
	       (mutex-unlock! *rmt-mutex*)
	       (exit 1)))
	    (begin
	      (debug:print 0 *default-log-port* "WARNING: communication failed. Trying again, try num: " attemptnum)
	      (remote-conndat-set!    runremote #f)
	      (remote-server-url-set! runremote #f)
              (debug:print-info 12 *default-log-port* "rmt:send-receive, case  9.1")
	      (mutex-unlock! *rmt-mutex*)
	      (server:start-and-wait *toppath*)
	      (rmt:send-receive cmd rid params attemptnum: (+ attemptnum 1)))))))))

;; (define (rmt:update-db-stats run-id rawcmd params duration)
;;   (mutex-lock! *db-stats-mutex*)
;;   (handle-exceptions
;;    exn
;;    (begin
;;      (debug:print 0 *default-log-port* "WARNING: stats collection failed in update-db-stats")
;;      (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
;;      (print "exn=" (condition->list exn))
;;      #f) ;; if this fails we don't care, it is just stats
;;    (let* ((cmd      (conc "run-id=" run-id " " (if (eq? rawcmd 'general-call) (car params) rawcmd)))
;; 	  (stat-vec (hash-table-ref/default *db-stats* cmd #f)))
;;      (if (not (vector? stat-vec))
;; 	 (let ((newvec (vector 0 0)))
;; 	   (hash-table-set! *db-stats* cmd newvec)
;; 	   (set! stat-vec newvec)))
;;      (vector-set! stat-vec 0 (+ (vector-ref stat-vec 0) 1))
;;      (vector-set! stat-vec 1 (+ (vector-ref stat-vec 1) duration))))
;;   (mutex-unlock! *db-stats-mutex*))

(define (rmt:print-db-stats)
  (let ((fmtstr "~40a~7-d~9-d~20,2-f")) ;; "~20,2-f"
    (debug:print 18 *default-log-port* "DB Stats\n========")
    (debug:print 18 *default-log-port* (format #f "~40a~8a~10a~10a" "Cmd" "Count" "TotTime" "Avg"))
    (for-each (lambda (cmd)
		(let ((cmd-dat (hash-table-ref *db-stats* cmd)))
		  (debug:print 18 *default-log-port* (format #f fmtstr cmd (vector-ref cmd-dat 0) (vector-ref cmd-dat 1) (/ (vector-ref cmd-dat 1)(vector-ref cmd-dat 0))))))
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

(define (rmt:open-qry-close-locally cmd run-id params #!key (remretries 5))
  (let* ((qry-is-write   (not (member cmd api:read-only-queries)))
	 (db-file-path   (db:dbfile-path)) ;;  0))
	 (dbstruct-local (db:setup))  ;; make-dbr:dbstruct path:  dbdir local: #t)))
	 (read-only      (not (file-write-access? db-file-path)))
	 (start          (current-milliseconds))
	 (resdat         (if (not (and read-only qry-is-write))
			     (let ((v (api:execute-requests dbstruct-local (vector (symbol->string cmd) params))))
			       (handle-exceptions ;; there has been a long history of receiving strange errors from values returned by the client when things go wrong..
				exn               ;;  This is an attempt to detect that situation and recover gracefully
				(begin
				  (debug:print0 *default-log-port* "ERROR: bad data from server " v " message: "  ((condition-property-accessor 'exn 'message) exn))
				  (vector #t '())) ;; should always get a vector but if something goes wrong return a dummy
				(if (and (vector? v)
					 (> (vector-length v) 1))
				    (let ((newvec (vector (vector-ref v 0)(vector-ref v 1))))
				      newvec)           ;; by copying the vector while inside the error handler we should force the detection of a corrupted record
				    (vector #t '()))))  ;; we could also check that the returned types are valid
			     (vector #t '())))
	 (success        (vector-ref resdat 0))
	 (res            (vector-ref resdat 1))
	 (duration       (- (current-milliseconds) start)))
    (if (and read-only qry-is-write)
        (debug:print 0 *default-log-port* "ERROR: attempt to write to read-only database ignored. cmd=" cmd))
    (if (not success)
	(if (> remretries 0)
	    (begin
	      (debug:print-error 0 *default-log-port* "local query failed. Trying again.")
	      (thread-sleep! (/ (random 5000) 1000)) ;; some random delay 
	      (rmt:open-qry-close-locally cmd run-id params remretries: (- remretries 1)))
	    (begin
	      (debug:print-error 0 *default-log-port* "too many retries in rmt:open-qry-close-locally, giving up")
	      #f))
	(begin
	  ;; (rmt:update-db-stats run-id cmd params duration)
	  ;; mark this run as dirty if this was a write, the watchdog is responsible for syncing it
	  (if qry-is-write
	      (let ((start-time (current-seconds)))
		(mutex-lock! *db-multi-sync-mutex*)
/		(set! *db-last-access* start-time)  ;; THIS IS PROBABLY USELESS? (we are on a client)
                (mutex-unlock! *db-multi-sync-mutex*)))))
    res))

(define (rmt:send-receive-no-auto-client-setup connection-info cmd run-id params)
  (let* ((run-id   (if run-id run-id 0))
	 (res  	   (handle-exceptions
		    exn
		    #f
		    (http-transport:client-api-send-receive run-id connection-info cmd params))))
    (if (and res (vector-ref res 0))
	(vector-ref res 1) ;;; YES!! THIS IS CORRECT!! CHANGE IT HERE, THEN CHANGE rmt:send-receive ALSO!!!
	#f)))

;; ;; Wrap json library for strings (why the ports crap in the first place?)
;; (define (rmt:dat->json-str dat)
;;   (with-output-to-string 
;;     (lambda ()
;;       (json-write dat))))
;; 
;; (define (rmt:json-str->dat json-str)
;;   (with-input-from-string json-str
;;     (lambda ()
;;       (json-read))))

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
  (rmt:send-receive 'login run-id (list *toppath* megatest-version *my-client-signature*)))

;; This login does no retries under the hood - it acts a bit like a ping.
;; Deprecated for nmsg-transport.
;;
(define (rmt:login-no-auto-client-setup connection-info)
  (case *transport-type* ;; run-id of 0 is just a placeholder
    ((http)(rmt:send-receive-no-auto-client-setup connection-info 'login 0 (list *toppath* megatest-version *my-client-signature*)))
    ;;((nmsg)(nmsg-transport:client-api-send-receive run-id connection-info 'login (list *toppath* megatest-version run-id *my-client-signature*)))
    ))

;; hand off a call to one of the db:queries statements
;; added run-id to make looking up the correct db possible 
;;
(define (rmt:general-call stmtname run-id . params)
  (rmt:send-receive 'general-call run-id (append (list stmtname run-id) params)))


;; given a hostname, return a pair of cpu load and update time representing latest intelligence from tests running on that host
(define (rmt:get-latest-host-load hostname)
  (rmt:send-receive 'get-latest-host-load 0 (list hostname)))

;; (define (rmt:sync-inmem->db run-id)
;;   (rmt:send-receive 'sync-inmem->db run-id '()))

(define (rmt:sdb-qry qry val run-id)
  ;; add caching if qry is 'getid or 'getstr
  (rmt:send-receive 'sdb-qry run-id (list qry val)))

;; NOT COMPLETED
(define (rmt:runtests user run-id testpatt params)
  (rmt:send-receive 'runtests run-id testpatt))

;;======================================================================
;;  T E S T   M E T A 
;;======================================================================

(define (rmt:get-tests-tags)
  (rmt:send-receive 'get-tests-tags #f '()))

;;======================================================================
;;  K E Y S 
;;======================================================================

;; These require run-id because the values come from the run!
;;
(define (rmt:get-key-val-pairs run-id)
  (rmt:send-receive 'get-key-val-pairs run-id (list run-id)))

(define (rmt:get-keys)
  (if *db-keys* *db-keys* 
     (let ((res (rmt:send-receive 'get-keys #f '())))
       (set! *db-keys* res)
       res)))

(define (rmt:get-keys-write) ;; dummy query to force server start
  (let ((res (rmt:send-receive 'get-keys-write #f '())))
    (set! *db-keys* res)
    res))

;; we don't reuse run-id's (except possibly *after* a db cleanup) so it is safe
;; to cache the resuls in a hash
;;
(define (rmt:get-key-vals run-id)
  (or (hash-table-ref/default *keyvals* run-id #f)
      (let ((res (rmt:send-receive 'get-key-vals #f (list run-id))))
        (hash-table-set! *keyvals* run-id res)
        res)))

(define (rmt:get-targets)
  (rmt:send-receive 'get-targets #f '()))

(define (rmt:get-target run-id)
  (rmt:send-receive 'get-target run-id (list run-id)))

;;======================================================================
;;  T E S T S
;;======================================================================

;; Just some syntatic sugar
(define (rmt:register-test run-id test-name item-path)
  (rmt:general-call 'register-test run-id run-id test-name item-path))

(define (rmt:get-test-id run-id testname item-path)
  (rmt:send-receive 'get-test-id run-id (list run-id testname item-path)))

(define (rmt:get-test-info-by-id run-id test-id)
  (if (and (number? run-id)(number? test-id))
      (rmt:send-receive 'get-test-info-by-id run-id (list run-id test-id))
      (begin
	(debug:print 0 *default-log-port* "WARNING: Bad data handed to rmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
	(print-call-chain (current-error-port))
	#f)))

(define (rmt:test-get-rundir-from-test-id run-id test-id)
  (rmt:send-receive 'test-get-rundir-from-test-id run-id (list run-id test-id)))

(define (rmt:open-test-db-by-test-id run-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id run-id test-id))))
    (debug:print 3 *default-log-port* "TEST PATH: " test-path)
    (open-test-db test-path)))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (rmt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment)
  (rmt:send-receive 'test-set-state-status-by-id run-id (list run-id test-id newstate newstatus newcomment)))

(define (rmt:set-tests-state-status run-id testnames currstate currstatus newstate newstatus)
  (rmt:send-receive 'set-tests-state-status run-id (list run-id testnames currstate currstatus newstate newstatus)))

(define (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)
  (if (number? run-id)
      (rmt:send-receive 'get-tests-for-run run-id (list run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode))
      (begin
	(debug:print-error 0 *default-log-port* "rmt:get-tests-for-run called with bad run-id=" run-id)
	(print-call-chain (current-error-port))
	'())))

;; get stuff via synchash 
(define (rmt:synchash-get run-id proc synckey keynum params)
  (rmt:send-receive 'synchash-get run-id (list run-id proc synckey keynum params)))

;; IDEA: Threadify these - they spend a lot of time waiting ...
;;
(define (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
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
				   (let ((res (rmt:send-receive 'get-tests-for-run-mindata hed (list hed testpatt states status not-in))))
				     (if (list? res)
					 (begin
					   (mutex-lock! multi-run-mutex)
					   (set! result (append result res))
					   (mutex-unlock! multi-run-mutex))
					 (debug:print-error 0 *default-log-port* "get-tests-for-run-mindata failed for run-id " hed ", testpatt " testpatt ", states " states ", status " status ", not-in " not-in))))
				 (conc "multi-run-thread for run-id " hed)))
		     (newthreads (cons newthread threads)))
		(thread-start! newthread)
		(thread-sleep! 0.05) ;; give that thread some time to start
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

(define (rmt:delete-test-records run-id test-id)
  (rmt:send-receive 'delete-test-records run-id (list run-id test-id)))

;; This is not needed as test steps are deleted on test delete call
;;
;; (define (rmt:delete-test-step-records run-id test-id)
;;   (rmt:send-receive 'delete-test-step-records run-id (list run-id test-id)))

(define (rmt:test-set-state-status run-id test-id state status msg)
  (rmt:send-receive 'test-set-state-status run-id (list run-id test-id state status msg)))

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

;; (define (rmt:get-run-ids-matching keynames target res)
;;   (rmt:send-receive #f 'get-run-ids-matching (list keynames target res)))

(define (rmt:get-prereqs-not-met run-id waitons ref-test-name ref-item-path #!key (mode '(normal))(itemmaps #f))
  (rmt:send-receive 'get-prereqs-not-met run-id (list run-id waitons ref-test-name ref-item-path mode itemmaps)))

(define (rmt:get-count-tests-running-for-run-id run-id)
  (rmt:send-receive 'get-count-tests-running-for-run-id run-id (list run-id)))

;; Statistical queries

(define (rmt:get-count-tests-running run-id)
  (rmt:send-receive 'get-count-tests-running run-id (list run-id)))

(define (rmt:get-count-tests-running-for-testname run-id testname)
  (rmt:send-receive 'get-count-tests-running-for-testname run-id (list run-id testname)))

(define (rmt:get-count-tests-running-in-jobgroup run-id jobgroup)
  (rmt:send-receive 'get-count-tests-running-in-jobgroup run-id (list run-id jobgroup)))

;; state and status are extra hints not usually used in the calculation
;;
(define (rmt:set-state-status-and-roll-up-items run-id test-name item-path state status comment)
  (rmt:send-receive 'set-state-status-and-roll-up-items run-id (list run-id test-name item-path state status comment)))

(define (rmt:update-pass-fail-counts run-id test-name)
  (rmt:general-call 'update-pass-fail-counts run-id test-name test-name test-name))

(define (rmt:top-test-set-per-pf-counts run-id test-name)
  (rmt:send-receive 'top-test-set-per-pf-counts run-id (list run-id test-name)))

(define (rmt:get-raw-run-stats run-id)
  (rmt:send-receive 'get-raw-run-stats run-id (list run-id)))

;;======================================================================
;;  R U N S
;;======================================================================

(define (rmt:get-run-info run-id)
  (rmt:send-receive 'get-run-info run-id (list run-id)))

(define (rmt:get-num-runs runpatt)
  (rmt:send-receive 'get-num-runs #f (list runpatt)))

;; Use the special run-id == #f scenario here since there is no run yet
(define (rmt:register-run keyvals runname state status user)
  (rmt:send-receive 'register-run #f (list keyvals runname state status user)))
    
(define (rmt:get-run-name-from-id run-id)
  (rmt:send-receive 'get-run-name-from-id run-id (list run-id)))

(define (rmt:delete-run run-id)
  (rmt:send-receive 'delete-run run-id (list run-id)))

(define (rmt:update-run-stats run-id stats)
  (rmt:send-receive 'update-run-stats #f (list run-id stats)))

(define (rmt:delete-old-deleted-test-records)
  (rmt:send-receive 'delete-old-deleted-test-records #f '()))

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

(define (rmt:get-runs-by-patt  keys runnamepatt targpatt offset limit fields last-runs-update) ;; fields of #f uses default
  (rmt:send-receive 'get-runs-by-patt #f (list keys runnamepatt targpatt offset limit fields last-runs-update)))

(define (rmt:find-and-mark-incomplete run-id ovr-deadtime)
  ;; (if (rmt:send-receive 'have-incompletes? run-id (list run-id ovr-deadtime))
  (rmt:send-receive 'mark-incomplete run-id (list run-id ovr-deadtime))) ;; )

(define (rmt:get-main-run-stats run-id)
  (rmt:send-receive 'get-main-run-stats #f (list run-id)))

(define (rmt:get-var varname)
  (rmt:send-receive 'get-var #f (list varname)))

(define (rmt:set-var varname value)
  (rmt:send-receive 'set-var #f (list varname value)))

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
	  (debug:print 4 *default-log-port* "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) #f
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (rmt:get-tests-for-run hed (conc test-name "/" item-path) '() '() ;; run-id testpatt states statuses
						      #f #f #f               ;; offset limit not-in hide/not-hide
						      #f #f #f #f 'normal))) ;; sort-by sort-order qryvals last-update mode
		  (debug:print 4 *default-log-port* "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))

(define (rmt:get-run-stats)
  (rmt:send-receive 'get-run-stats #f '()))

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

(define (rmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile)
  (let* ((state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 *default-log-port* "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (rmt:send-receive 'teststep-set-status! run-id (list run-id test-id teststep-name state-in status-in comment logfile))))

(define (rmt:get-steps-for-test run-id test-id)
  (rmt:send-receive 'get-steps-for-test run-id (list run-id test-id)))

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (rmt:read-test-data run-id test-id categorypatt #!key (work-area #f)) 
  (rmt:send-receive 'read-test-data run-id (list run-id test-id categorypatt)))
;;   (let ((tdb  (rmt:open-test-db-by-test-id run-id test-id work-area: work-area)))
;;     (if tdb
;; 	(tdb:read-test-data tdb test-id categorypatt)
;; 	'())))

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

;;======================================================================
;;  T A S K S
;;======================================================================

(define (rmt:tasks-find-task-queue-records target run-name test-patt state-patt action-patt)
  (rmt:send-receive 'find-task-queue-records #f (list target run-name test-patt state-patt action-patt)))

(define (rmt:tasks-add action owner target runname testpatt params)
  (rmt:send-receive 'tasks-add #f (list action owner target runname testpatt params)))

(define (rmt:tasks-set-state-given-param-key param-key new-state)
  (rmt:send-receive 'tasks-set-state-given-param-key #f (list  param-key new-state)))

(define (rmt:tasks-get-last target runname)
  (rmt:send-receive 'tasks-get-last #f (list target runname)))

;;======================================================================
;; A R C H I V E S
;;======================================================================

(define (rmt:archive-get-allocations  testname itempath dneeded)
  (rmt:send-receive 'archive-get-allocations #f (list testname itempath dneeded)))

(define (rmt:archive-register-block-name bdisk-id archive-path)
  (rmt:send-receive 'archive-register-block-name #f (list bdisk-id archive-path)))

(define (rmt:archive-allocate-testsuite/area-to-block block-id testsuite-name areakey)
  (rmt:send-receive 'archive-allocate-test-to-block #f (list  block-id testsuite-name areakey)))

(define (rmt:archive-register-disk bdisk-name bdisk-path df)
  (rmt:send-receive 'archive-register-disk #f (list bdisk-name bdisk-path df)))

(define (rmt:test-set-archive-block-id run-id test-id archive-block-id)
  (rmt:send-receive 'test-set-archive-block-id run-id (list run-id test-id archive-block-id)))

(define (rmt:test-get-archive-block-info archive-block-id)
  (rmt:send-receive 'test-get-archive-block-info #f (list archive-block-id)))
