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

(declare (unit api))
(declare (uses rmt))
(declare (uses db))
(declare (uses tasks))

;; allow these queries through without starting a server
;;
(define api:read-only-queries
  '(get-key-val-pairs
    get-keys
    test-toplevel-num-items
    get-test-info-by-id
    test-get-rundir-from-test-id
    get-count-tests-running
    get-count-tests-running-in-jobgroup
    get-previous-test-run-record
    get-matching-previous-test-run-records
    test-get-logfile-info
    test-get-records-for-index-file
    get-testinfo-state-status
    test-get-top-process-pid
    test-get-paths-matching-keynames-target-new
    get-prereqs-not-met
    get-count-tests-running-for-run-id
    get-run-info
    get-run-status
    register-run
    get-tests-for-run
    get-test-id
    get-tests-for-runs-mindata
    get-run-name-from-id
    get-runs
    get-all-run-ids
    get-prev-run-ids
    get-run-ids-matching-target
    get-runs-by-patt
    get-steps-data
    login
    testmeta-get-record
    have-incompletes?
    ))

(define api:write-queries
  '(
    ;; SERVERS
    start-server
    kill-server

    ;; TESTS
    test-set-state-status-by-id
    delete-test-records
    delete-old-deleted-test-records
    test-set-status-state
    test-set-top-process-pid
    roll-up-pass-fail-counts
    update-fail-pass-counts

    ;; RUNS
    register-run
    set-tests-state-status
    delete-run
    lock/unlock-run
    update-run-event_time
    mark-incomplete

    ;; STEPS
    teststep-set-status!

    ;; TEST DATA
    test-data-rollup
    csv->test-data

    ;; MISC
    sync-inmem->db

    ;; TESTMETA
    testmeta-add-record
    testmeta-update-field

    ;; TASKS
    tasks-add
    tasks-set-state-given-param-key
    ))

;; These are called by the server on recipt of /api calls
;;    - keep it simple, only return the actual result of the call, i.e. no meta info here
;;
;;    - returns #( flag result )
;;
(define (api:execute-requests dbstruct dat)
  (handle-exceptions
   exn
   (let ((call-chain (get-call-chain)))
     (print-call-chain (current-error-port))
     (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))       
     (vector #f (vector exn call-chain dat))) ;; return some stuff for debug if an exception happens
   (if (not (vector? dat))                    ;; it is an error to not receive a vector
       (vector #f #f "remote must be called with a vector")       
       (vector                                   ;; return a vector + the returned data structure
	#t 
	(let ((cmd    (vector-ref dat 0))
	      (params (vector-ref dat 1)))
	  (case (if (symbol? cmd)
		    cmd
		    (string->symbol cmd))

	    ;;===============================================
	    ;; READ/WRITE QUERIES
	    ;;===============================================

	    ;; SERVERS
	    ((start-server)                    (apply server:kind-run params))
	    ((kill-server)                     (set! *server-run* #f))

	    ;; TESTS
	    ((test-set-state-status-by-id)     (apply db:test-set-state-status-by-id dbstruct params))
	    ((delete-test-records)             (apply db:delete-test-records dbstruct params))
	    ((delete-old-deleted-test-records) (apply db:delete-old-deleted-test-records dbstruct params))
	    ((test-set-status-state)           (apply db:test-set-status-state dbstruct params))
	    ((test-set-top-process-pid)        (apply db:test-set-top-process-pid dbstruct params))
	    ((roll-up-pass-fail-counts)        (apply db:roll-up-pass-fail-counts dbstruct params))
	    ((update-fail-pass-counts)         (apply db:general-call dbstruct 'update-pass-fail-counts params))

	    ;; RUNS
	    ((register-run)                 (apply db:register-run dbstruct params))
	    ((set-tests-state-status)       (apply db:set-tests-state-status dbstruct params))
	    ((delete-run)                   (apply db:delete-run dbstruct params))
	    ((lock/unlock-run)              (apply db:lock/unlock-run dbstruct params))
	    ((update-run-event_time)        (apply db:update-run-event_time dbstruct params))

	    ;; STEPS
	    ((teststep-set-status!)         (apply db:teststep-set-status! dbstruct params))

	    ;; TEST DATA
	    ((test-data-rollup)             (apply db:test-data-rollup dbstruct params))
	    ((csv->test-data)               (apply db:csv->test-data dbstruct params))

	    ;; MISC
	    ((sync-inmem->db)               (let ((run-id (car params)))
					      (db:sync-touched dbstruct run-id force-sync: #t)))
	    ((mark-incomplete)              (apply db:find-and-mark-incomplete dbstruct params))

	    ;; TESTMETA
	    ((testmeta-add-record)       (apply db:testmeta-add-record dbstruct params))
	    ((testmeta-update-field)     (apply db:testmeta-update-field dbstruct params))

	    ;; TASKS
	    ((tasks-add)                 (apply tasks:add dbstruct params))   
	    ((tasks-set-state-given-param-key) (apply tasks:set-state-given-param-key dbstruct params))

	    ;; ARCHIVES
	    ;; ((archive-get-allocations)   
	    ((archive-register-disk)     (apply db:archive-register-disk dbstruct params))
	    ((archive-register-block-name)(apply db:archive-register-block-name dbstruct params))
	    ((archive-allocate-testsuite/area-to-block)(apply db:archive-allocate-testsuite/area-to-block dbstruct block-id testsuite-name areakey))

	    ;;======================================================================
	    ;; READ ONLY QUERIES
	    ;;======================================================================

	    ;; KEYS
	    ((get-key-val-pairs)               (apply db:get-key-val-pairs dbstruct params))
	    ((get-keys)                        (db:get-keys dbstruct))

	    ;; TESTS
	    ((test-toplevel-num-items)         (apply db:test-toplevel-num-items dbstruct params))
	    ((get-test-info-by-id)	       (apply db:get-test-info-by-id dbstruct params))
	    ((test-get-rundir-from-test-id)    (apply db:test-get-rundir-from-test-id dbstruct params))
	    ((get-count-tests-running)         (apply db:get-count-tests-running dbstruct params))
	    ((get-count-tests-running-in-jobgroup) (apply db:get-count-tests-running-in-jobgroup dbstruct params))
	    ;; ((delete-test-step-records)        (apply db:delete-test-step-records dbstruct params))
	    ((get-previous-test-run-record)    (apply db:get-previous-test-run-record dbstruct params))
	    ((get-matching-previous-test-run-records)(apply db:get-matching-previous-test-run-records dbstruct params))
	    ((test-get-logfile-info)           (apply db:test-get-logfile-info dbstruct params))
	    ((test-get-records-for-index-file)  (apply db:test-get-records-for-index-file dbstruct params))
	    ((get-testinfo-state-status)       (apply db:get-testinfo-state-status dbstruct params))
	    ((test-get-top-process-pid)        (apply db:test-get-top-process-pid dbstruct params))
	    ((test-get-paths-matching-keynames-target-new) (apply db:test-get-paths-matching-keynames-target-new dbstruct params))
	    ((get-prereqs-not-met)             (apply db:get-prereqs-not-met dbstruct params))
	    ((get-count-tests-running-for-run-id) (apply db:get-count-tests-running-for-run-id dbstruct params))

	    ;; RUNS
	    ((get-run-info)                 (apply db:get-run-info dbstruct params))
	    ((get-run-status)               (apply db:get-run-status dbstruct params))
	    ((set-run-status)               (apply db:set-run-status dbstruct params))
	    ((get-tests-for-run)            (apply db:get-tests-for-run dbstruct params))
	    ((get-test-id)                  (apply db:get-test-id dbstruct params))
	    ((get-tests-for-run-mindata)    (apply db:get-tests-for-run-mindata dbstruct params))
	    ((get-runs)                     (apply db:get-runs dbstruct params))
	    ((get-all-run-ids)              (db:get-all-run-ids dbstruct))
	    ((get-prev-run-ids)             (apply db:get-prev-run-ids dbstruct params))
	    ((get-run-ids-matching-target)  (apply db:get-run-ids-matching-target dbstruct params))
	    ((get-runs-by-patt)             (apply db:get-runs-by-patt dbstruct params))
	    ((get-run-name-from-id)         (apply db:get-run-name-from-id dbstruct params))

	    ;; STEPS
	    ((get-steps-data)               (apply db:get-steps-data dbstruct params))

	    ;; MISC
	    ((have-incompletes?)            (apply db:have-incompletes? dbstruct params))
	    ((login)                        (apply db:login dbstruct params))
	    ((general-call)                 (let ((stmtname   (car params))
						  (run-id     (cadr params))
						  (realparams (cddr params)))
					      (db:with-db dbstruct run-id #t ;; these are all for modifying the db
							  (lambda (db)
							    (db:general-call db stmtname realparams)))))
	    ((sdb-qry)                      (apply sdb:qry params))
	    ((ping)                         (current-process-id))

	    ;; TESTMETA
	    ((testmeta-get-record)       (apply db:testmeta-get-record dbstruct params))

	    ;; TASKS 
	    ((find-task-queue-records)   (apply tasks:find-task-queue-records dbstruct params))))))))


;; http-server  send-response
;;                 api:process-request
;;                    db:*
;;
;; NB// Runs on the server as part of the server loop
;;
(define (api:process-request dbstruct $) ;; the $ is the request vars proc
  (let* ((cmd     ($ 'cmd))
	 (paramsj ($ 'params))
	 (params  (db:string->obj paramsj transport: 'http)) ;; (rmt:json-str->dat paramsj))
	 (resdat  (api:execute-requests dbstruct (vector cmd params))) ;; #( flag result )
	 (res     (vector-ref resdat 1)))

    ;; This can be here but needs controls to ensure it doesn't run more than every 4 seconds
    ;; (rmt:dat->json-str
    ;;  (if (or (string? res)
    ;;          (list?   res)
    ;;          (number? res)
    ;;          (boolean? res))
    ;;      res 
    ;;      (list "ERROR, not string, list, number or boolean" 1 cmd params res)))))
    (db:obj->string res transport: 'http)))

