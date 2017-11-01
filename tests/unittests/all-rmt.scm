
;;======================================================================
;;  A L L - R M T 
;;======================================================================

;; Run like this:
;;
;;  ./rununittest.sh all-rmt 1

;; Definitions:
;;   NTN - no test needed
;;   DEP - function is deprecated, no point in testing
;;   NED - function nested under others, no test needed.
;;   DEF - deferred

(print "start dir: " (current-directory))
       
(define toppath (current-directory))
(test #f #t (string?(server:start-and-wait *toppath*)))

(test "setup for run" #t (begin (launch:setup)
 				(string? (getenv "MT_RUN_AREA_HOME"))))
(test #f #t (vector? (client:setup toppath)))

(test #f #t (vector? (rmt:get-connection-info toppath))) ;; TODO: push areapath down.
(test #f #t (string? (server:check-if-running ".")))
;; DEF (test #f #f (rmt:send-receive-no-auto-client-setup *runremote* 'get-keys #f '()))
;; DEF (rmt:kill-server run-id)
;; DEF (rmt:start-server run-id)
(test #f '(#t "successful login")(rmt:login #f))
;; DEF (rmt:login-no-auto-client-setup connection-info)
(test #f #t (pair? (rmt:get-latest-host-load (get-host-name))))

;; get-latest-host-load does a lookup in the db, it won't return a useful value unless
;; a test ran recently on host
(test-batch rmt:get-latest-host-load
            "rmt:get-latest-host-load"
            (list (list "localhost"  #t (get-host-name))
                  (list "not-a-host" #t "not-a-host"  ))
            post-proc: pair?)
                                           
(test #f #t (list? (rmt:get-changed-record-ids 0)))

(test #f #f (begin (runs:update-all-test_meta #f) #f))

(test #f '("test1" "test2")(sort (alist-ref "tagtwo" (hash-table->alist (rmt:get-tests-tags)) equal?) string<=))

(test #f '() (rmt:get-key-val-pairs 0))
(test #f '("SYSTEM" "RELEASE") (rmt:get-keys))
(test #f '("SYSTEM" "RELEASE") (rmt:get-keys-write)) ;; dummy query to force server start
(test #f '() (rmt:get-key-vals 1))
(test #f (vector '("SYSTEM" "RELEASE") '()) (rmt:get-targets))
(test #f "" (rmt:get-target 1))
(test #f #t (rmt:register-test 1 "foo" ""))
(test #f 1  (rmt:get-test-id 1 "foo" ""))
(test #f "foo" (vector-ref (rmt:get-test-info-by-id 1 1) 2))
(test #f "/tmp/badname" (rmt:test-get-rundir-from-test-id 1 1))
(test #f '(1) (db:set-tests-state-status *db* 1 '("foo")  "COMPLETED" "PASS" "NOT_STARTED" "PASS"))
(test #f '(1) (rmt:set-tests-state-status 1 '("foo") "COMPLETED" "PASS" "NOT_STARTED" "PASS"))
(test #f #t (mt:test-set-state-status-by-id 1 1 "COMPLETED" "PASS" "Just testing!"))
(test #f #t (list? (rmt:get-tests-for-run 1 "%" '() '() #f #f #f #f #f #f 0 #f)))
(test #f #t (list? (rmt:get-tests-for-runs-mindata '(1) "%" '() '() #f)))
(test #f #f (begin (rmt:delete-test-records 1 2) #f))
(test #f #t (begin (rmt:test-set-state-status 1 1 "COMPLETED" "FAIL" "Another message") #t))
(test #f 0  (rmt:test-toplevel-num-items 1 "foo"))
(test #f '()(rmt:get-matching-previous-test-run-records 1 "foo" ""))
(test #f '("/tmp/badname" "logs/final.log") (rmt:test-get-logfile-info 1 "foo"))
(test #f '()(rmt:test-get-records-for-index-file 1 "foo"))
(test #f #t (vector? (rmt:get-testinfo-state-status 1 1)))
(test #f #t (rmt:test-set-log! 1 1 "/tmp/another/logfile/eh"))
(test #f #f (begin (rmt:test-set-top-process-pid 1 1 123) #f))
(test #f 123  (rmt:test-get-top-process-pid 1 1))
(define keys (rmt:get-keys))
(test #f '()(rmt:get-run-ids-matching-target keys "%/%" #f "%" "%" "%" "%"))
(test #f '()(rmt:test-get-paths-matching-keynames-target-new keys "%/%" #f "%" "%" "%" "%"))
(test #f '()(rmt:get-prereqs-not-met 1 '() "foo" ""))
(test #f 0 (rmt:get-count-tests-running-for-run-id 1))
(test #f 0 (rmt:get-count-tests-running 1))
(test #f 0 (rmt:get-count-tests-running-for-testname 1 "foo"))
(test #f 0 (rmt:get-count-tests-running-in-jobgroup 1 "nada"))
(test #f #f (begin (rmt:set-state-status-and-roll-up-items 1 "foo" "" "COMPLETED" "FAIL" "Just yet another message") #f))
(test #f #t (rmt:top-test-set-per-pf-counts 1 "foo"))
(test #f '() (rmt:get-raw-run-stats 1))
(test #f #t (vector? (rmt:get-run-info 1)))
(test #f 0 (rmt:get-num-runs "%"))
(define keypatts '(("SYSTEM" "ubuntu")("RELEASE" "v1.234")) )
(test #f 1 (rmt:register-run '(("SYSTEM" "ubuntu")("RELEASE" "v1.234")) "bar" "NEW" "JUSTFINE" "bobafett" "quick"))
(test #f "bar" (rmt:get-run-name-from-id 1))
(test #f #t (begin (rmt:delete-run 2) #t)) ;; delete a non-existant run
(test #f #t (begin (rmt:update-run-stats 1 '()) #t))
(test #f #t (begin (rmt:delete-old-deleted-test-records) #t))
(test #f #t (vector? (rmt:get-runs "%" 10 0 keypatts)))
(test #f '(1)(rmt:get-all-run-ids))
(test #f '()(rmt:get-prev-run-ids 1))
(test #f #t (begin (rmt:lock/unlock-run 1 #t #f "mikey") #t))
(test #f "JUSTFINE" (rmt:get-run-status 1))
(test #f #t (begin (rmt:set-run-status 1 "NOTFINE" msg: "A message") #t))
(test #f #t (begin (rmt:update-run-event_time 1) #t))

;; (rmt:get-runs-by-patt  keys runnamepatt targpatt offset limit fields last-runs-update) ;; fields of #f uses default
;;
(let ((keys (rmt:get-keys))
      (rnp  "%")    ;; run name patt
      (tpt  "%/%")) ;; target patt
  (test-batch rmt:get-runs-by-patt
              "rmt:get-runs-by-patt"
              (list (list "t=0" #t keys rnp tpt #f #f #f 0)
                    (list "t=current" #f keys rnp tpt #f #f #f (+ 100 (current-seconds))) ;; should be no records from the future
                    )
              post-proc: (lambda (res)
                           ;; (print "rmt:get-runs-by-patt returned: " res)
                           (and (vector? res)
                                (let ((rows (vector-ref res 1)))
                                  (> (length rows) 0))))))

;; (rmt:find-and-mark-incomplete run-id ovr-deadtime)
;; (rmt:get-main-run-stats run-id)
;; (rmt:get-var varname)
;; (rmt:set-var varname value)
;; (rmt:find-and-mark-incomplete-all-runs #!key (ovr-deadtime #f))
;; (rmt:get-previous-test-run-record run-id test-name item-path)
;; (rmt:get-run-stats)
;; (rmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile)
;; (rmt:get-steps-for-test run-id test-id)
;; (rmt:read-test-data run-id test-id categorypatt #!key (work-area #f)) 
;; (rmt:testmeta-add-record testname)
;; (rmt:testmeta-get-record testname)
;; (rmt:testmeta-update-field test-name fld val)
;; (rmt:test-data-rollup run-id test-id status)
;; (rmt:csv->test-data run-id test-id csvdata)
;; (rmt:tasks-find-task-queue-records target run-name test-patt state-patt action-patt)
;; (rmt:tasks-add action owner target runname testpatt params)
;; (rmt:tasks-set-state-given-param-key param-key new-state)
;; (rmt:tasks-get-last target runname)
;; (rmt:archive-get-allocations  testname itempath dneeded)
;; (rmt:archive-register-block-name bdisk-id archive-path)
;; (rmt:archive-allocate-testsuite/area-to-block block-id testsuite-name areakey)
;; (rmt:archive-register-disk bdisk-name bdisk-path df)
;; (rmt:test-set-archive-block-id run-id test-id archive-block-id)
;; (rmt:test-get-archive-block-info archive-block-id)
;; NED (rmt:open-qry-close-locally cmd run-id params #!key (remretries 5))
;; NED (rmt:send-receive cmd rid params #!key (attemptnum 1)(area-dat #f)) ;; start attemptnum at 1 so the modulo below works as expected
;; DEF (test #f #f (rmt:print-db-stats))
;; DEF (rmt:get-max-query-average run-id)
;; NED (rmt:general-call stmtname run-id . params)
;; DEP (rmt:sdb-qry qry val run-id)
;; DEF (rmt:runtests user run-id testpatt params)
;; DEP (rmt:open-test-db-by-test-id run-id test-id #!key (work-area #f))
;; DEP (rmt:synchash-get run-id proc synckey keynum params)
;; DEP (test #f #f (rmt:update-pass-fail-counts 1 "foo"))
