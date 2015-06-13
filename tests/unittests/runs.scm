(define keys (rmt:get-keys))

(test "get all legal tests" (list "test1" "test2") (sort (get-all-legal-tests) string<=?))

(test "register-run" #t (number?
			 (rmt:register-run 
					  '(("SYSTEM" "key1")("RELEASE" "key2"))
					  "myrun" 
					  "new"
					  "n/a" 
					  "bob")))

(test #f #t             (rmt:tests-register-test 1 "nada" ""))
(test #f 1              (rmt:get-test-id 1 "nada" ""))
(test #f "NOT_STARTED"  (vector-ref (rmt:get-test-info 1 "nada" "") 3))
(test #f "NOT_STARTED"  (vector-ref (rmt:get-test-info 1 "nada" "") 3))

(test #f "FOO LIKE 'abc%def'" (db:patt->like "FOO" "abc%def"))
(test #f "key2" (vector-ref (car (vector-ref (runs:get-runs-by-patt *db* '("SYSTEM" "RELEASE") "%" "key1/key2") 1)) 1))

(test #f "SYSTEM,RELEASE,id,runname,state,status,owner,event_time" (car (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time"))))
(test #f #t (runs:operate-on 'print "%" "%" "%"))

;;(test "update-test-info" #t (test-update-meta-info *db* 1 "nada" 
(setenv "BLAHFOO" "1234")
(unsetenv "NADAFOO")
(test "env temp overrides" "xyz" (let ((prevvals (alist->env-vars '(("BLAHFOO" 4321)("NADAFOO" xyz))))
				       (result   (get-environment-variable "NADAFOO")))
				    (alist->env-vars prevvals)
				    result))

(test "env restored" "1234" (get-environment-variable "BLAHFOO"))


(test "Items assoc" "Elephant" (cadar (cadr (item-assoc->item-list '(("ANIMAL" "Elephant Lion")("SEASON" "Spring Fall"))))))
(set! *verbosity* 6)
(test "Items assoc" '()(item-assoc->item-list '(("a" "a b c d")("b" "c d e")("c" "")("d"))))
(set! *verbosity* -1)
(test "Items assoc empty items" '()   (item-assoc->item-list '(("A"))))
(set! *verbosity* 1)
(test "Items table" "SEASON" (caadar (item-table->item-list '(("ANIMAL" "Elephant Lion")("SEASON" "Spring Winter")))))
(test "Items table empty items I" '() (item-table->item-list '(("A"))))
(test "Items table empty items II" '() (item-table->item-list '(("A" ""))))

;; Test out the steps code

(define test-id #f)

;; force keepgoing
; (hash-table-set! args:arg-hash "-keepgoing" #t)
(hash-table-set! args:arg-hash "-itempatt" "%")
(hash-table-set! args:arg-hash "-testpatt" "%")
(hash-table-set! args:arg-hash "-target" "ubuntu/r1.2")
(test "Setup for a run"       #t (begin (setup-for-run) #t))

(define *tdb* #f)
(define keyvals #f)
(test "target->keyval" #t (let ((kv (keys:target->keyval keys (args:get-arg "-target"))))
			    (set! keyvals kv)(list? keyvals)))

(define testdbpath (conc "/tmp/" (getenv "USER") "/megatest_testing"))
(system (conc "rm -f " testdbpath "/testdat.db;mkdir -p " testdbpath))

(print "Using " testdbpath " for test db")
(test #f #t (let ((db (open-test-db testdbpath)))
	      (set! *tdb* db)
	      (sqlite3#database? db)))
(sqlite3#finalize! *tdb*)

;; (test "Remove the rollup run" #t (begin (remove-runs) #t))
(define tconfig #f)
(test "get a testconfig" #t (let ((tconf (tests:get-testconfig "test1" 'return-procs)))
			      (set! tconfig tconf)
			      (hash-table? tconf)))
(db:clean-all-caches)

(test "set-megatest-env-vars"
      "ubuntu"
      (begin
	(set-megatest-env-vars 1 inkeys: keys)
	(get-environment-variable "SYSTEM")))
(test "setup-env-defaults"
      "see this variable"
      (begin
	(setup-env-defaults "runconfigs.config" 1 *already-seen-runconfig-info* keys keyvals "pre-launch-env-vars")
	(get-environment-variable "ALLTESTS")))

(test #f "ubuntu" (car (keys:target-set-args keys (args:get-arg "-target") args:arg-hash)))

(define rinfo #f)
(test "get-run-info"  #f (vector? (vector-ref (let ((rinf (cdb:remote-run db:get-run-info #f 1)))
						(set! rinfo rinf)
						rinf) 0)))
(test "get-key-vals"  "key1" (car (cdb:remote-run db:get-key-vals #f 1)))
(test "tests:sort-by" '() (tests:sort-by-priority-and-waiton (make-hash-table)))

(test "update-test_meta" "test1" (begin
				   (runs:update-test_meta "test1" tconfig)
				   (let ((dat (cdb:remote-run db:testmeta-get-record #f "test1")))
				     (vector-ref dat 1))))

(define test-path "tests/test1")
(define disk-path #f)
(test "get-best-disk"    #t (string? (file-exists? (let ((d (get-best-disk *configdat*)))
						     (set! disk-path d)
						     d))))
(test "create-work-area" #t (symbolic-link? (car (create-work-area 1 rinfo keyvals 1 test-path disk-path "test1" '()))))
(test #f "" (item-list->path '()))

(test "launch-test" #t (string? (file-exists? (launch-test 1 1 rinfo keyvals "run1" tconfig "test1" test-path '() (make-hash-table)))))


(test "Run a test" #t (general-run-call 
		       "-runtests" 
		       "run a test"
		       (lambda (target runname keys keyvallst)
			 (let ((test-patts "test%"))
			   ;; (runs:run-tests target runname test-patts user (make-hash-table))
			   ;; (run:test run-id run-info key-vals runname test-record flags parent-test)
			   ;; (set! *verbosity* 22) ;; (list 0 1 2))
			   (run:test 1 ;; run-id
				     #f        ;; run-info is yet only a dream
				     keyvallst ;; (keys:target->keyval keys target)
				     "run1"    ;; runname 
				     (vector            ;; test_records.scm tests:testqueue
				      "test1"           ;; testname
				      tconfig           ;; testconfig
				      '()               ;; waitons
				      0                 ;; priority
				      #f                ;; items
				      #f                ;; itemsdat
				      ""                ;; itempath
				      )
				     args:arg-hash      ;; flags (e.g. -itemspatt)
				     #f)
			   ;; (set! *verbosity* 0)
			   ))))





(test "server stop" #f (let ((hostname (car  *runremote*))
			     (port     (cadr *runremote*)))
			 (tasks:kill-server #t hostname port server-pid 'http)
			 (open-run-close tasks:get-best-server tasks:open-db)))

(exit 1)
;; (test "cache is coherent" #t (let ((cached-info (db:get-test-info-cached-by-id db 2))
;; 				   (non-cached  (db:get-test-info-not-cached-by-id db 2)))
;; 			       (print "\nCached:    " cached-info)
;; 			       (print "Noncached: " non-cached)
;; 			       (equal? cached-info non-cached)))

(change-directory test-work-dir)
(test "Add a step"  #t
      (begin
	(db:teststep-set-status! db 2 "step1" "start" 0 "This is a comment" "mylogfile.html")
	(sleep 2)
	(db:teststep-set-status! db 2 "step1" "end" "pass" "This is a different comment" "finallogfile.html")
	(set! test-id (db:test-get-id (car (cdb:remote-run db:get-tests-for-run #f 1 "test1" '() '()))))
	(number? test-id)))

(test "Get rundir"       #t (let ((rundir (cdb:remote-run db:test-get-rundir-from-test-id #f test-id)))
			      (print "Rundir " rundir)
			      (system (conc "mkdir -p " rundir))
			      (string? rundir)))
(test #f #t (sqlite3#database? (open-test-db "./")))
(test "Create a test db" "../simpleruns/key1/key2/myrun/test1/testdat.db"
      (let ((tdb (open-run-close db:open-test-db-by-test-id db test-id)))
	(if tdb (sqlite3#finalize! tdb))
	(file-exists? "../simpleruns/key1/key2/myrun/test1/testdat.db")))

(test "Get steps for test" #t (let ((steps (cdb:remote-run db:get-steps-for-test #f test-id)))
				(print steps)
				(> (length steps) 0)))
(test "Get nice table for steps" "2.0s"
      (begin
	(vector-ref (hash-table-ref (open-run-close db:get-steps-table #f test-id) "step1") 4)))

;; (exit)

(test #f "myrun" (cdb:remote-run db:get-run-name-from-id #f 1))

(test #f #f (cdb:remote-run db:roll-up-pass-fail-counts #f 1 "nada" "" "PASS"))

;;======================================================================
;; R E M O T E   C A L L S 
;;======================================================================

(define start-wait (current-seconds))
(print "Starting intensive cache and rpc test")
(for-each (lambda (params)
	    (print "Intensive: params=" params)
	    (cdb:tests-register-test *runremote* 1 (conc "test" (random 20)) "")
	    (apply cdb:test-set-status-state *runremote* test-id params)
	    (cdb:pass-fail-counts *runremote* test-id (random 100) (random 100))
	    (cdb:test-rollup-test_data-pass-fail *runremote* test-id)
	    (cdb:roll-up-pass-fail-counts *runremote* 1 "test1" "" (cadr params))
	    (thread-sleep! 0.01)) ;; cache ordering granularity is at the second level. Should really be at the ms level
	  '(("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("COMPLETED"    "PASS" #f)
	    ("NOT_STARTED"  "FAIL" "Just testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ("KILLED"       "UNKNOWN" "More testing")
	    ))

;; now set all tests to completed
(cdb:flush-queue *runremote*)
(let ((tests (cdb:remote-run db:get-tests-for-run #f 1 "%" '() '())))
  (print "Setting " (length tests) " to COMPLETED/PASS")
  (for-each
   (lambda (test)
     (cdb:test-set-status-state *runremote* (db:test-get-id test) "COMPLETED" "PASS" "Forced pass"))
   tests))

;; (process-wait server-pid)
;; (test "Server wait time" #t (let ((run-delta (- (current-seconds) start-wait)))
;; 			      (print "Server ran for " run-delta " seconds")
;; 			      (> run-delta 20)))

(test "Rollup the run(s)" #t (begin
			       (runs:rollup-run keys (keys->alist keys "na") "rollup" "matt")
			       #t))

(hash-table-set! args:arg-hash ":runname" "%")

(test "Remove the rollup run" #t (begin (operate-on 'remove-runs)))

(print "Waiting for server to be done, should be about 20 seconds")
(test "server stop" #f (let ((hostname (car  *runremote*))
			     (port     (cadr *runremote*)))
			 (tasks:kill-server #t hostname port server-pid 'http)
			 (open-run-close tasks:get-best-server tasks:open-db)))

;; (cdb:kill-server *runremote*)

;; (thread-join! th1 th2 th3)

;; ADD ME!!!! (db:get-prereqs-not-met *db* 1 '("runfirst") "" mode: 'normal)
;; ADD ME!!!! (rdb:get-tests-for-run *db* 1 "runfirst" #f '() '())
