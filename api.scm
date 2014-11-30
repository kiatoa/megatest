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
    testmeta-get-record))

;; These are called by the server on recipt of /api calls
;;    - keep it simple, only return the actual result of the call, i.e. no meta info here
;;
(define (api:execute-requests dbstruct cmd params)
  (case (string->symbol cmd)
    ;; SERVERS
    ((start-server)                 (apply server:kind-run params))
    ((kill-server)                  (set! *server-run* #f))

    ;; KEYS
    ((get-key-val-pairs)            (apply db:get-key-val-pairs dbstruct params))
    ((get-keys)                     (db:get-keys dbstruct))

    ;; TESTS
    ((test-toplevel-num-items)         (apply db:test-toplevel-num-items dbstruct params))
    ((get-test-info-by-id)	       (apply db:get-test-info-by-id dbstruct params))
    ((test-get-rundir-from-test-id)    (apply db:test-get-rundir-from-test-id dbstruct params))
    ((test-set-state-status-by-id)     (apply db:test-set-state-status-by-id dbstruct params))
    ((get-count-tests-running)         (apply db:get-count-tests-running dbstruct params))
    ((get-count-tests-running-in-jobgroup) (apply db:get-count-tests-running-in-jobgroup dbstruct params))
    ((delete-test-records)             (apply db:delete-test-records dbstruct params))
    ;; ((delete-test-step-records)        (apply db:delete-test-step-records dbstruct params))
    ((delete-old-deleted-test-records) (apply db:delete-old-deleted-test-records dbstruct params))
    ((test-set-status-state)           (apply db:test-set-status-state dbstruct params))
    ((get-previous-test-run-record)    (apply db:get-previous-test-run-record dbstruct params))
    ((get-matching-previous-test-run-records)(apply db:get-matching-previous-test-run-records dbstruct params))
    ((test-get-logfile-info)           (apply db:test-get-logfile-info dbstruct params))
    ((test-get-records-for-index-file)  (apply db:test-get-records-for-index-file dbstruct params))
    ((get-testinfo-state-status)       (apply db:get-testinfo-state-status dbstruct params))
    ((test-set-top-process-pid)        (apply db:test-set-top-process-pid dbstruct params))
    ((test-get-top-process-pid)        (apply db:test-get-top-process-pid dbstruct params))
    ((test-get-paths-matching-keynames-target-new) (apply db:test-get-paths-matching-keynames-target-new dbstruct params))
    ((get-prereqs-not-met)             (apply db:get-prereqs-not-met dbstruct params))
    ((roll-up-pass-fail-counts)        (apply db:roll-up-pass-fail-counts dbstruct params))
    ((update-fail-pass-counts)         (apply db:general-call dbstruct 'update-pass-fail-counts params))
    ((get-count-tests-running-for-run-id) (apply db:get-count-tests-running-for-run-id dbstruct params))

    ;; RUNS
    ((get-run-info)                 (apply db:get-run-info dbstruct params))
    ((get-run-status)               (apply db:get-run-status dbstruct params))
    ((set-run-status)               (apply db:set-run-status dbstruct params))
    ((register-run)                 (apply db:register-run dbstruct params))
    ((set-tests-state-status)       (apply db:set-tests-state-status dbstruct params))
    ((get-tests-for-run)            (apply db:get-tests-for-run dbstruct params))
    ((get-test-id)                  (apply db:get-test-id dbstruct params))
	    ((get-tests-for-run-mindata)    (apply db:get-tests-for-run-mindata dbstruct params))
    ((get-run-name-from-id)         (apply db:get-run-name-from-id dbstruct params))
    ((delete-run)                   (apply db:delete-run dbstruct params))
    ((get-runs)                     (apply db:get-runs dbstruct params))
    ((get-all-run-ids)              (db:get-all-run-ids dbstruct))
    ((get-prev-run-ids)             (apply db:get-prev-run-ids dbstruct params))
    ((get-run-ids-matching-target)  (apply db:get-run-ids-matching-target dbstruct params))
    ((get-runs-by-patt)             (apply db:get-runs-by-patt dbstruct params))
    ((lock/unlock-run)              (apply db:lock/unlock-run dbstruct params))
    ((update-run-event_time)        (apply db:update-run-event_time dbstruct params))
    ((find-and-mark-incomplete)     (apply db:find-and-mark-incomplete dbstruct params))

    ;; STEPS
    ((teststep-set-status!)         (apply db:teststep-set-status! dbstruct params))

    ;; TEST DATA
    ((test-data-rollup)             (apply db:test-data-rollup dbstruct params))
    ((csv->test-data)               (apply db:csv->test-data dbstruct params))
    ((get-steps-data)               (apply db:get-steps-data dbstruct params))

    ;; MISC
    ((login)                        (apply db:login dbstruct params))
    ((general-call)                 (let ((stmtname   (car params))
					  (run-id     (cadr params))
					  (realparams (cddr params)))
				      (db:with-db dbstruct run-id #t ;; these are all for modifying the db
						  (lambda (db)
						    (db:general-call db stmtname realparams)))))
    ((sync-inmem->db)               (db:sync-touched dbstruct run-id force-sync: #t))
    ((sdb-qry)                      (apply sdb:qry params))

    ;; TESTMETA
    ((testmeta-get-record)       (apply db:testmeta-get-record dbstruct params))
    ((testmeta-add-record)       (apply db:testmeta-add-record dbstruct params))
    ((testmeta-update-field)     (apply db:testmeta-update-field dbstruct params))
    (else
     (list "ERROR" 0))))

;; http-server  send-response
;;                 api:process-request
;;                    db:*
;;
;; NB// Runs on the server as part of the server loop
;;
(define (api:process-request dbstruct $) ;; the $ is the request vars proc
  (let* ((cmd     ($ 'cmd))
	 (paramsj ($ 'params))
	 (params  (db:string->obj paramsj)) ;; (rmt:json-str->dat paramsj))
	 (res     (api:execute-requests dbstruct cmd params)))

    ;; This can be here but needs controls to ensure it doesn't run more than every 4 seconds
    ;; (rmt:dat->json-str
    ;;  (if (or (string? res)
    ;;          (list?   res)
    ;;          (number? res)
    ;;          (boolean? res))
    ;;      res 
    ;;      (list "ERROR, not string, list, number or boolean" 1 cmd params res)))))
    (db:obj->string res)))

