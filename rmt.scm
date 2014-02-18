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

(use json)

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

;; cmd is a symbol
;; vars is a json string encoding the parameters for the call
;;
(define (rmt:send-receive cmd rid params)
  (let* ((run-id  (if rid rid 0))
	 (connection-info (let ((cinfo (hash-table-ref/default *runremote* run-id #f)))
			    (if cinfo
				cinfo
				(let loop ((numtries 100))
				  (thread-sleep! 1)
				  (let ((res (client:setup run-id)))
				    (if res 
					res
					(if (> numtries 0)
					    (loop (- numtries 1))
					    (begin
					      (debug:print 0 "ERROR: 100 tries and no server, giving up")
					      (exit 1)))))))))
	 (jparams         (db:obj->string params))
	 (res (http-transport:client-api-send-receive run-id connection-info cmd jparams)))
    (if res
	(db:string->obj res) ;; (rmt:json-str->dat res)
	(let ((new-connection-info (client:setup run-id)))
	  (debug:print 0 "WARNING: Communication failed, trying call to http-transport:client-api-send-receive again.")
	  (rmt:send-receive cmd run-id params)))))

(define (rmt:send-receive-no-auto-client-setup connection-info cmd run-id params)
  (let* ((run-id   (if run-id run-id 0))
	 (jparams         (db:obj->string params)) ;; (rmt:dat->json-str params))
	 (res (http-transport:client-api-send-receive run-id connection-info cmd jparams numretries: 3)))
    (if res
	(db:string->obj res) ;; (rmt:json-str->dat res)
	;; this one does NOT keep trying
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
;;  M I S C
;;======================================================================

(define (rmt:login run-id)
  (rmt:send-receive 'login run-id (list *toppath* megatest-version run-id *my-client-signature*)))

;; This login does no retries under the hood - it acts a bit like a ping.
;;
(define (rmt:login-no-auto-client-setup connection-info run-id)
  (rmt:send-receive-no-auto-client-setup connection-info 'login run-id (list *toppath* megatest-version run-id *my-client-signature*)))
  
(define (rmt:kill-server run-id)
  (rmt:send-receive 'kill-server run-id (list run-id)))

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

;; These should not require run-id but it is more consistent to have it. 
;; run-id can theoretically be #f but how to handle that is not yet done.
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
	(debug:print 0 "ERROR: Bad data handed to rmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
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

(define (rmt:test-set-status-state run-id test-id status state msg)
  (rmt:send-receive 'test-set-status-state run-id (list run-id test-id status state msg)))

(define (rmt:get-previous-test-run-record run-id test-name item-path)
  (rmt:send-receive 'get-previous-test-run-record run-id (list run-id test-name item-path)))

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

(define (rmt:test-get-paths-matching-keynames-target-new keynames target res testpatt statepatt statuspatt runname)
  (let ((run-ids (rmt:get-run-ids-matching keynames target res)))
    (apply append (lambda (run-id)
		    (rmt:send-receive 'test-get-paths-matching-keynames-target-new (list keynames target res testpatt statepatt statuspatt runname)))
	   run-ids)))

(define (rmt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode 'normal))
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

(define (rmt:lock/unlock-run run-id lock unlock user)
  (rmt:send-receive 'lock/unlock-run #f (list run-id lock unlock user)))

(define (rmt:update-run-event_time run-id)
  (rmt:send-receive 'update-run-event_time #f (list run-id)))

(define (rmt:get-runs-by-patt  keys runnamepatt targpatt offset limit)
  (rmt:send-receive 'get-runs-by-patt #f (list keys runnamepatt targpatt offset limit)))

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