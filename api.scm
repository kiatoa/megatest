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

;; These are called by the server on recipt of /api calls

(define (api:execute-requests dbstruct cmd params)
  (case (string->symbol cmd)
    ;; KEYS
    ((get-key-val-pairs)            (apply db:get-key-val-pairs dbstruct params))
    ((get-keys)                     (db:get-keys db))

    ;; TESTS
    ;; json doesn't do vectors, convert to list
    ((get-test-info-by-id)	       (apply db:get-test-info-by-id dbstruct params))
    ((test-get-rundir-from-test-id)    (apply db:test-get-rundir-from-test-id dbstruct params))
    ((test-set-state-status-by-id)     (apply db:test-set-state-status-by-id dbstruct params))
    ((get-count-tests-running)         (db:get-count-tests-running db))
    ((get-count-tests-running-in-jobgroup) (apply db:get-count-tests-running-in-jobgroup dbstruct params))
    ((delete-test-records)             (apply db:delete-test-records dbstruct params))
    ((delete-old-deleted-test-records) (db:delete-old-deleted-test-records db))
    ((test-set-status-state)           (apply db:test-set-status-state dbstruct params))
    ((get-previous-test-run-record)    (apply db:get-previous-test-run-record dbstruct params))
    ((get-matching-previous-test-run-records)(apply db:get-matching-previous-test-run-records dbstruct params))
    ((db:test-get-logfile-info)        (apply db:test-get-logfile-info dbstruct params))
    ((test-get-records-for-index-file  (apply db:test-get-records-for-index-file dbstruct params)))
    ((get-testinfo-state-status)       (apply db:get-testinfo-state-status dbstruct params))
    ((test-get-paths-matching-keynames-target-new) (apply db:test-get-paths-matching-keynames-target-new dbstruct params))
    ((get-prereqs-not-met)             (apply db:get-prereqs-not-met dbstruct params))
    ((roll-up-pass-fail-counts)        (apply db:roll-up-pass-fail-counts dbstruct params))
    ((update-fail-pass-counts)         (apply db:general-call dbstruct 'update-pass-fail-counts params))
    ((get-count-tests-running-for-run-id) (apply db:get-count-tests-running-for-run-id dbstruct params))

    ;; RUNS
    ((get-run-info)                 (apply db:get-run-info dbstruct params))
    ((register-run)                 (apply db:register-run dbstruct params))
    ((set-tests-state-status)       (apply db:set-tests-state-status dbstruct params))
    ((get-tests-for-run)            (apply db:get-tests-for-run dbstruct params))
    ((get-test-id)                  (apply db:get-test-id-not-cached dbstruct params))
    ((get-tests-for-runs-mindata)   (apply db:get-tests-for-runs-mindata dbstruct params))
    ((get-run-name-from-id)         (apply db:get-run-name-from-id dbstruct params))
    ((delete-run)                   (apply db:delete-run dbstruct params))
    ((get-runs)                     (apply db:get-runs dbstruct params))
    ((get-runs-by-patt)             (apply db:get-runs-by-patt dbstruct params))
    ((lock/unlock-run)              (apply db:lock/unlock-run dbstruct params))
    ((update-run-event_time)        (apply db:update-run-event_time dbstruct params))

    ;; STEPS
    ((teststep-set-status!)         (apply db:teststep-set-status! dbstruct params))

    ;; TEST DATA
    ((test-data-rollup)             (apply db:test-data-rollup dbstruct params))
    ((csv->test-data)               (apply db:csv->test-data dbstruct params))
    ((get-steps-data)               (apply db:get-steps-data dbstruct params))

    ;; MISC
    ((login)                        (apply db:login dbstruct params))
    ((general-call)                 (let ((stmtname   (car params))
					  (realparams (cdr params)))
				      (db:general-call dbstruct stmtname realparams)))
    ((sync-inmem->db)               (db:sync-back))
    ((kill-server)
     (db:sync-tables (db:tbls *inmemdb*) *inmemdb* *db*)  ;; (db:sync-to *inmemdb* *db*)
     (let ((hostname (car  *runremote*))
	   (port     (cadr *runremote*))
	   (pid      (if (null? params) #f (car params)))
	   (th1      (make-thread (lambda ()(thread-sleep! 3)(debug:print 0 "Server exiting!")(exit 0)) "Server exit thread")))
       (debug:print 0 "WARNING: Server on " hostname ":" port " going down by user request!")
       (debug:print-info 1 "current pid=" (current-process-id))
       (open-run-close tasks:server-deregister tasks:open-db 
		       hostname
		       port: port)
       (set! *server-run* #f)
       (thread-sleep! 3)
       (if pid 
	   (process-signal pid signal/kill)
	   (thread-start! th1))
       '(#t "exit process started")))

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

