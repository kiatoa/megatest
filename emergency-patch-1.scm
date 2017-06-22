

;; These are called by the server on recipt of /api calls
;;    - keep it simple, only return the actual result of the call, i.e. no meta info here
;;
;;    - returns #( flag result )
;;
(define (api:execute-requests dbstruct dat)
  (handle-exceptions
   exn
   (let ((call-chain (get-call-chain)))
     (debug:print 0 *default-log-port* "WARNING: api:execute-requests received an exception from peer, dat=" dat)
     (print-call-chain (current-error-port))
     (debug:print 0 *default-log-port* " message: "  ((condition-property-accessor 'exn 'message) exn))       
     (vector #f (vector exn call-chain dat))) ;; return some stuff for debug if an exception happens
   (cond
    ((not (vector? dat))                    ;; it is an error to not receive a vector
     (vector #f (vector #f "remote must be called with a vector")))
    ((> *api-process-request-count* 20) ;; 20)
     (debug:print 0 *default-log-port* "WARNING: api:execute-requests received an overloaded message.")
     (set! *server-overloaded* #t)
     (vector #f (vector #f 'overloaded))) ;; the inner vector is what gets returned. nope, don't know why. please refactor!
    (else  
     (let* ((cmd-in            (vector-ref dat 0))
            (cmd               (if (symbol? cmd-in)
				   cmd-in
				   (string->symbol cmd-in)))
            (params            (vector-ref dat 1))
            (start-t           (current-milliseconds))
            (readonly-mode     (dbr:dbstruct-read-only dbstruct))
            (readonly-command  (member cmd api:read-only-queries))
            (writecmd-in-readonly-mode (and readonly-mode (not readonly-command)))
            (res    
             (if writecmd-in-readonly-mode
                 (conc "attempt to run write command "cmd" on a read-only database")
                 (case cmd
                   ;;===============================================
                   ;; READ/WRITE QUERIES
                   ;;===============================================

                   ((get-keys-write)                        (db:get-keys dbstruct)) ;; force a dummy "write" query to force server; for debug in -repl
                   
                   ;; SERVERS
                   ((start-server)                    (apply server:kind-run params))
                   ((kill-server)                     (set! *server-run* #f))

                   ;; TESTS

                   ;;((test-set-state-status-by-id)     (apply mt:test-set-state-status-by-id dbstruct params))
                   ;;BB - commented out above because it was calling below, eventually, incorrectly (dbstruct passed to mt:test-set-state-status-by-id, which previosly did more, but now only passes thru to db:set-state-status-and-roll-up-items.
                   ((test-set-state-status-by-id)

                    ;; (define (db:set-state-status-and-roll-up-items dbstruct run-id test-name item-path state status comment)
                    (db:set-state-status-and-roll-up-items
                     dbstruct
                     (list-ref params 0) ; run-id
                     (list-ref params 1) ; test-name
                     #f                  ; item-path
                     (list-ref params 2) ; state
                     (list-ref params 3) ; status
                     (list-ref params 4) ; comment
                     ))
                   
                   ((delete-test-records)             (apply db:delete-test-records dbstruct params))
                   ((delete-old-deleted-test-records) (apply db:delete-old-deleted-test-records dbstruct params))
                   ((test-set-state-status)           (apply db:test-set-state-status dbstruct params))
                   ((test-set-top-process-pid)        (apply db:test-set-top-process-pid dbstruct params))
                   ((set-state-status-and-roll-up-items) (apply db:set-state-status-and-roll-up-items dbstruct params))
                   ((top-test-set-per-pf-counts)      (apply db:top-test-set-per-pf-counts dbstruct params))
                   ((test-set-archive-block-id)       (apply db:test-set-archive-block-id dbstruct params))

                   ;; RUNS
                   ((register-run)                 (apply db:register-run dbstruct params))
                   ((set-tests-state-status)       (apply db:set-tests-state-status dbstruct params))
                   ((delete-run)                   (apply db:delete-run dbstruct params))
                   ((lock/unlock-run)              (apply db:lock/unlock-run dbstruct params))
                   ((update-run-event_time)        (apply db:update-run-event_time dbstruct params))
                   ((update-run-stats)             (apply db:update-run-stats dbstruct params))
                   ((set-var)                      (apply db:set-var dbstruct params))
                   ((del-var)                      (apply db:del-var dbstruct params))

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
                   ((get-tests-tags)            (db:get-tests-tags dbstruct))

                   ;; TASKS
                   ((tasks-add)                 (apply tasks:add dbstruct params))   
                   ((tasks-set-state-given-param-key) (apply tasks:set-state-given-param-key dbstruct params))
                   ((tasks-get-last)            (apply tasks:get-last dbstruct params))

		   ;; NO SYNC DB
		   ((no-sync-set)               (apply db:no-sync-set         *no-sync-db* params))
		   ((no-sync-get/default)       (apply db:no-sync-get/default *no-sync-db* params))
		   ((no-sync-del!)              (apply db:no-sync-del!        *no-sync-db* params))
		 
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
                   ((get-key-vals)                    (apply db:get-key-vals dbstruct params))
                   ((get-target)                      (apply db:get-target dbstruct params))
                   ((get-targets)                     (db:get-targets dbstruct))

                   ;; ARCHIVES
                   ((test-get-archive-block-info)     (apply db:test-get-archive-block-info dbstruct params))
                   
                   ;; TESTS
                   ((test-toplevel-num-items)         (apply db:test-toplevel-num-items dbstruct params))
                   ((get-test-info-by-id)	       (apply db:get-test-info-by-id dbstruct params))
                   ((test-get-rundir-from-test-id)    (apply db:test-get-rundir-from-test-id dbstruct params))
                   ((get-count-tests-running-for-testname) (apply db:get-count-tests-running-for-testname dbstruct params))
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
                   ((synchash-get)                    (apply synchash:server-get dbstruct params))
                   ((get-raw-run-stats)               (apply db:get-raw-run-stats dbstruct params))

                   ;; RUNS
                   ((get-run-info)                 (apply db:get-run-info dbstruct params))
                   ((get-run-status)               (apply db:get-run-status dbstruct params))
                   ((set-run-status)               (apply db:set-run-status dbstruct params))
                   ((get-tests-for-run)            (apply db:get-tests-for-run dbstruct params))
                   ((get-test-id)                  (apply db:get-test-id dbstruct params))
                   ((get-tests-for-run-mindata)    (apply db:get-tests-for-run-mindata dbstruct params))
                   ((get-runs)                     (apply db:get-runs dbstruct params))
                   ((get-num-runs)                 (apply db:get-num-runs dbstruct params))
                   ((get-all-run-ids)              (db:get-all-run-ids dbstruct))
                   ((get-prev-run-ids)             (apply db:get-prev-run-ids dbstruct params))
                   ((get-run-ids-matching-target)  (apply db:get-run-ids-matching-target dbstruct params))
                   ((get-runs-by-patt)             (apply db:get-runs-by-patt dbstruct params))
                   ((get-run-name-from-id)         (apply db:get-run-name-from-id dbstruct params))
                   ((get-main-run-stats)           (apply db:get-main-run-stats dbstruct params))
                   ((get-var)                      (apply db:get-var dbstruct params))
                   ((get-run-stats)                (apply db:get-run-stats dbstruct params))

                   ;; STEPS
                   ((get-steps-data)               (apply db:get-steps-data dbstruct params))
                   ((get-steps-for-test)           (apply db:get-steps-for-test dbstruct params))

                   ;; TEST DATA
                   ((read-test-data)               (apply db:read-test-data dbstruct params))
                   ((read-test-data*)              (apply db:read-test-data* dbstruct params))

                   ;; MISC
                   ((get-latest-host-load)         (apply db:get-latest-host-load dbstruct params))
                   ((have-incompletes?)            (apply db:have-incompletes? dbstruct params))
                   ((login)                        (apply db:login dbstruct params))
                   ((general-call)                 (let ((stmtname   (car params))
                                                         (run-id     (cadr params))
                                                         (realparams (cddr params)))
                                                     (db:general-call dbstruct stmtname realparams)))
                   ((sdb-qry)                      (apply sdb:qry params))
                   ((ping)                         (current-process-id))
		   ((get-changed-record-ids)       (apply db:get-changed-record-ids dbstruct params))
		   
                   ;; TESTMETA
                   ((testmeta-get-record)       (apply db:testmeta-get-record dbstruct params))

                   ;; TASKS 
                   ((find-task-queue-records)   (apply tasks:find-task-queue-records dbstruct params))
		   (else
		    (debug:print 0 *default-log-port* "ERROR: bad api call " cmd)
		    (conc "ERROR: BAD api call " cmd))))))
       
       ;; save all stats
       (let ((delta-t (- (current-milliseconds)
			 start-t)))
	 (hash-table-set! *db-api-call-time* cmd
			  (cons delta-t (hash-table-ref/default *db-api-call-time* cmd '()))))
       (if writecmd-in-readonly-mode
	   (vector #f res)
           (vector #t res)))))))