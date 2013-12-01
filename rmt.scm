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
(define (rmt:send-receive cmd params)
  (case *transport-type* 
    ((fs-aint-here)
     (debug:print 0 "ERROR: Not yet (re)supported")
     (exit 1))
    ((fs http)
     (let* ((jparams (db:obj->string params)) ;; (rmt:dat->json-str params))
	    (res (http-transport:client-api-send-receive *runremote* cmd jparams)))
       (if res
	   (db:string->obj res) ;; (rmt:json-str->dat res)
	   (begin
	     (debug:print 0 "ERROR: Bad value from http-transport:client-api-send-receive " res)
	     #f))
     ))
    (else
     (debug:print 0 "ERROR: Transport " *transport-type* " not yet (re)supported")
     (exit 1))))

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

(define (rmt:login)
  (rmt:send-receive 'login (list *toppath* megatest-version *my-client-signature*)))

(define (rmt:kill-server)
  (rmt:send-receive 'kill-server '()))

;; hand off a call to one of the db:queries statements
;; added run-id to make looking up the correct db possible 
;;
(define (rmt:general-call stmtname run-id . params)
  (rmt:send-receive 'general-call (append (list stmtname run-id) params)))

(define (rmt:sync-inmem->db)
  (rmt:send-receive 'sync-inmem->db '()))

(define (rmt:sdb-qry qry val)
  ;; add caching if qry is 'getid or 'getstr
  (rmt:send-receive 'sdb-qry (list qry val)))

;;======================================================================
;;  K E Y S 
;;======================================================================

(define (rmt:get-key-val-pairs run-id)
  (rmt:send-receive 'get-key-val-pairs (list run-id)))

(define (rmt:get-keys)
  (rmt:send-receive 'get-keys '()))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (rmt:get-test-id run-id testname item-path)
  (rmt:send-receive 'get-test-id (list run-id testname item-path)))

(define (rmt:get-test-info-by-id run-id test-id)
  (if (and (number? run-id)(number? test-id))
      (rmt:send-receive 'get-test-info-by-id (list run-id test-id))
      (begin
	(debug:print 0 "ERROR: Bad data handed to rmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
	(print-call-chain)
	#f)))

(define (rmt:test-get-rundir-from-test-id run-id test-id)
  (rmt:send-receive 'test-get-rundir-from-test-id (list run-id test-id)))

(define (rmt:open-test-db-by-test-id run-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(rmt:test-get-rundir-from-test-id run-id test-id))))
    (debug:print 3 "TEST PATH: " test-path)
    (open-test-db test-path)))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (rmt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment)
  (rmt:send-receive 'test-set-state-status-by-id (list run-id test-id newstate newstatus newcomment)))

(define (rmt:set-tests-state-status run-id testnames currstate currstatus newstate newstatus)
  (rmt:send-receive 'set-tests-state-status (list run-id testnames currstate currstatus newstate newstatus)))

(define (rmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals)
  (if (number? run-id)
      (rmt:send-receive 'get-tests-for-run (list run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals))
      (begin
	(debug:print "ERROR: rmt:get-tests-for-run called with bad run-id=" run-id)
	(print-call-chain)
	'())))

(define (rmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
  (rmt:send-receive 'get-tests-for-runs-mindata (list run-ids testpatt states status not-in)))

(define (rmt:delete-test-records run-id test-id)
  (rmt:send-receive 'delete-test-records (list run-id test-id)))

(define (rmt:test-set-status-state run-id test-id status state msg)
  (rmt:send-receive 'test-set-status-state (list run-id test-id status state msg)))

(define (rmt:get-previous-test-run-record run-id test-name item-path)
  (rmt:send-receive 'get-previous-test-run-record (list run-id test-name item-path)))

(define (rmt:get-matching-previous-test-run-records run-id test-name item-path)
  (rmt:send-receive 'get-matching-previous-test-run-records (list run-id test-name item-path)))

(define (rmt:test-get-logfileg-info run-id test-name)
  (rmt:send-receive 'test-get-logfile-info (list run-id test-name)))

(define (rmt:test-get-records-for-index-file run-id test-name)
  (rmt:send-receive 'test-get-records-for-index-file (list  run-id test-name)))

(define (rmt:get-testinfo-state-status run-id test-id)
  (rmt:send-receive 'get-testinfo-state-status (list run-id test-id)))

(define (rmt:test-set-log! run-id test-id logf)
  (if (string? logf)(rmt:general-call 'test-set-log run-id logf test-id)))

(define (rmt:test-get-paths-matching-keynames-target-new keynames target res testpatt statepatt statuspatt runname)
  (rmt:send-receive 'test-get-paths-matching-keynames-target-new (list keynames target res testpatt statepatt statuspatt runname)))

(define (rmt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode 'normal))
  (rmt:send-receive 'get-prereqs-not-met (list run-id waitons ref-item-path mode)))

(define (rmt:get-count-tests-running-for-run-id run-id)
  (rmt:send-receive 'get-count-tests-running-for-run-id (list run-id)))

;; Statistical queries

(define (rmt:get-count-tests-running run-id)
  (rmt:send-receive 'get-count-tests-running (list run-id)))

(define (rmt:get-count-tests-running-in-jobgroup run-id jobgroup)
  (rmt:send-receive 'get-count-tests-running-in-jobgroup (list run-id jobgroup)))

(define (rmt:roll-up-pass-fail-counts run-id test-name item-path status)
  (rmt:send-receive 'roll-up-pass-fail-counts (list run-id test-name item-path status)))

(define (rmt:update-pass-fail-counts run-id test-name)
  (rmt:general-call 'update-fail-pass-counts run-id test-name run-id test-name run-id test-name))

;;======================================================================
;;  R U N S
;;======================================================================

(define (rmt:get-run-info run-id)
  (rmt:send-receive 'get-run-info (list run-id)))

(define (rmt:register-run keyvals runname state status user)
  (rmt:send-receive 'register-run (list keyvals runname state status user)))
    
(define (rmt:get-run-name-from-id run-id)
  (rmt:send-receive 'get-run-name-from-id (list run-id)))

(define (rmt:delete-run run-id)
  (rmt:send-receive 'delete-run (list run-id)))

(define (rmt:delete-old-deleted-test-records)
  (rmt:send-receive 'delete-old-deleted-test-records '()))

(define (rmt:get-runs runpatt count offset keypatts)
  (rmt:send-receive 'get-runs (list runpatt count offset keypatts)))

(define (rmt:get-runs-by-patt keys runnamepatt targpatt offset limit)
  (rmt:send-receive 'get-runs-by-patt (list keys runnamepatt targpatt offset limit)))

(define (rmt:lock/unlock-run run-id lock unlock user)
  (rmt:send-receive 'lock/unlock-run (list run-id lock unlock user)))

(define (rmt:update-run-event_time run-id)
  (rmt:send-receive 'update-run-event_time (list run-id)))

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
(define (rmt:get-steps-for-test test-id)
  (rmt:send-receive 'get-steps-data (list test-id)))

(define (rmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile)
  (let* ((state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (rmt:send-receive 'teststep-set-status! (list run-id test-id teststep-name state-in status-in comment logfile))))

(define (rmt:get-steps-for-test test-id)
  (rmt:send-receive 'get-steps-for-test (list test-id)))

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (rmt:read-test-data test-id categorypatt #!key (work-area #f)) 
  (let ((tdb  (rmt:open-test-db-by-test-id test-id work-area: work-area)))
    (if tdb
	(tdb:read-test-data tdb test-id categorypatt)
	'())))

(define (rmt:testmeta-add-record testname)
  (rmt:send-receive 'testmeta-add-record (list testname)))

(define (rmt:testmeta-get-record testname)
  (rmt:send-receive 'testmeta-get-record (list testname)))

(define (rmt:testmeta-update-field test-name fld val)
  (rmt:send-receive 'testmeta-update-field (list test-name fld val)))

(define (rmt:test-data-rollup run-id test-id status)
  (rmt:send-receive 'test-data-rollup (list run-id test-id status)))

(define (rmt:csv->test-data run-id test-id csvdata)
  (rmt:send-receive 'csv->test-data (list run-id test-id csvdata)))