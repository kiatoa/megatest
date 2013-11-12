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
    ((test-set-state-status-by-id)     (apply db:test-set-state-status-by-id db params))
    ((get-count-tests-running)         (db:get-count-tests-running db))
    ((get-count-tests-running-in-jobgroup) (apply db:get-count-tests-running-in-jobgroup db params))
    ((delete-test-records)             (apply db:delete-test-records db params))
    ((delete-old-deleted-test-records) (db:delete-old-deleted-test-records db))
    ((test-set-status-state)           (apply db:test-set-status-state db params))
    ((get-previous-test-run-record)    (apply db:get-previous-test-run-record db params))
    ((get-matching-previous-test-run-records)(map vector->list (apply db:get-matching-previous-test-run-records db params)))
    ((db:test-get-logfile-info)        (apply db:test-get-logfile-info db params))
    ((test-get-records-for-index-file  (apply db:test-get-records-for-index-file db params)))
    ((get-testinfo-state-status)       (apply db:get-testinfo-state-status db params))
    ((test-get-paths-matching-keynames-target-new) (apply db:test-get-paths-matching-keynames-target-new db params))
    ((get-prereqs-not-met)             (let ((res (apply db:get-prereqs-not-met db params)))
					 (map (lambda (x)
						(if (vector? x)
						    (vector->list x)
						    x))
					      res)))
					 

    ;; RUNS
    ((get-run-info)                 (let ((res (apply db:get-run-info db params)))
				      (list (vector-ref res 0)
					    (vector->list (vector-ref res 1)))))
    ((register-run)                 (apply db:register-run db params))
    ((set-tests-state-status)       (apply db:set-tests-state-status db params))
    ((get-tests-for-run)            (map vector->list (apply db:get-tests-for-run db params)))
    ((get-test-id)                  (apply db:get-test-id-not-cached db params))
    ((get-tests-for-runs-mindata)   (map vector->list (apply db:get-tests-for-runs-mindata db params)))
    ((get-run-name-from-id)         (apply db:get-run-name-from-id db params))
    ((delete-run)                   (apply db:delete-run db params))
    ((get-runs)                     (let* ((res  (apply db:get-runs db params))
					   (hedr (vector-ref res 0))
					   (data (vector-ref res 1)))
				      (list hedr (map vector->list data))))
    ((get-runs-by-patt)             (let* ((res  (apply db:get-runs-by-patt db params))
					   (hedr (vector-ref res 0))
					   (data (vector-ref res 1)))
				      (list hedr (map vector->list data))))
    ((lock/unlock-run)              (apply db:lock/unlock-run db params))
    ((update-run-event_time)        (apply db:update-run-event_time db params))

    ;; MISC
    ((login)                        (apply db:login db params))
    ((general-call)                 (let ((stmtname   (car params))
					  (realparams (cdr params)))
				      (db:general-call db stmtname realparams)))
    ((kill-server)
     (db:sync-to *inmemdb* *db*)
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
    ((testmeta-get-record)       (let ((res (apply db:testmeta-get-record db params)))
				   (if (vector? res)
				       (vector->list res)
				       res)))
    ((testmeta-add-record)       (apply db:testmeta-add-record db params))
    ((testmeta-update-field)     (apply db:testmeta-update-field db params))
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
    (rmt:dat->json-str
     (if (or (string? res)
	     (list?   res)
	     (number? res)
	     (boolean? res))
	 res 
	 (list "ERROR" 1 cmd params res)))))

