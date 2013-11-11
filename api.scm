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

(define (api:execute-requests db cmd params)
  (case (string->symbol cmd)
    ;; KEYS
    ((get-key-val-pairs)            (apply db:get-key-val-pairs db params))
    ((get-keys)                     (db:get-keys db))

    ;; TESTS
    ;; json doesn't do vectors, convert to list
    ((get-test-info-by-id)	       (let ((res (apply db:get-test-info-by-id db params)))
					 (if (vector? res)(vector->list res) res)))
    ((test-get-rundir-from-test-id)    (apply db:test-get-rundir-from-test-id db params))
    ((testmeta-get-record)             (vector->list (apply db:testmeta-get-record db params)))
    ((test-set-state-status-by-id)     (apply db:test-set-state-status-by-id db params))
    ((get-count-tests-running)         (db:get-count-tests-running db))
    ((get-count-tests-running-in-jobgroup) (apply db:get-count-tests-running-in-jobgroup db params))
    ((delete-test-records)             (apply db:delete-test-records params))
    ((delete-old-deleted-test-records) (db:delete-old-deleted-test-records db))
    ((test-set-status-state)           (apply db:test-set-status-state params))
    ((get-previous-test-run-record)    (apply db:get-previous-test-run-record params))
    ((get-matching-previous-test-run-records)(map vector->list (apply db:get-matching-previous-test-run-records db params)))
    ((db:test-get-logfile-info)        (apply db:test-get-logfile-info db params))
    ((test-get-records-for-index-file  (apply db:test-get-records-for-index-file db params)))
    ((get-testinfo-state-status)       (apply db:get-testinfo-state-status db params))
    ((update-testdat-meta-info)        (apply db:update-testdat-meta-info db params))

    ;; RUNS
    ((get-run-info)                 (let ((res (apply db:get-run-info db params)))
				      (list (vector-ref res 0)
					    (vector->list (vector-ref res 1)))))
    ((register-run)                 (apply db:register-run db params))
    ((set-tests-state-status)       (apply db:set-state-status db params))
    ((get-tests-for-run)            (map vector->list (apply db:get-tests-for-run db params)))
    ((get-test-id)                  (apply db:get-test-id-not-cached db params))
    ((get-tests-for-runs-mindata)   (map vector->list (apply db:get-tests-for-runs-mindata db params)))
    ((get-run-name-from-id)         (apply db:get-run-name-from-id db params))
    ((delete-run)                   (apply db:delete-run db params))

    ;; MISC
    ((login)                        (apply db:login db params))
    ((general-call)                 (let ((stmtname   (car params))
					  (realparams (cdr params)))
				      (db:general-call db stmtname realparams)))
    (else
     (list "ERROR" 0))))

;; http-server  send-response
;;                 api:process-request
;;                    db:*
;;
;; NB// Runs on the server as part of the server loop
;;
(define (api:process-request db $) ;; the $ is the request vars proc
  (let* ((cmd     ($ 'cmd))
	 (paramsj ($ 'params))
	 (params  (rmt:json-str->dat paramsj))
	 (res     (api:execute-requests db cmd params)))

    ;; This can be here but needs controls to ensure it doesn't run more than every 4 seconds
    (db:sync-to *inmemdb* *db*)
    
    (rmt:dat->json-str
     (if (or (string? res)
	     (list?   res)
	     (number? res)
	     (boolean? res))
	 res 
	 (list "ERROR" 1 cmd params res)))))

