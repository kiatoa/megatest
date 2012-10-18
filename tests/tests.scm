(require-extension test)
(require-extension regex)

(define test-work-dir (current-directory))

;; read in all the _record files
(let ((files (glob "*_records.scm")))
  (for-each
   (lambda (file)
     (print "Loading " file)
     (load file))
   files))

;;======================================================================
;; P R O C E S S E S
;;======================================================================

(test "cmd-run-with-stderr->list" '("No such file or directory")
      (let ((reslst (cmd-run-with-stderr->list "ls" "/tmp/ihadbetternotexist")))
	(string-search (regexp "No such file or directory")(car reslst))))

;;======================================================================
;; T E S T   M A T C H I N G
;;======================================================================

;; tests:glob-like-match
(test #f '("abc") (tests:glob-like-match "abc" "abc"))
(for-each 
 (lambda (patt str expected)
   (test (conc patt " " str "=>" expected) expected (tests:glob-like-match patt str)))
 (list "abc"    "~abc" "~abc" "a*c"  "a%c")
 (list "abc"    "abcd" "abc"  "ABC"  "ABC")
 (list '("abc")  #t      #f     #f '("ABC"))
 )

;; tests:match
(test #f #t (tests:match "abc/def" "abc" "def"))
(for-each 
 (lambda (patterns testname itempath expected)
   (test (conc patterns " " testname "/" itempath "=>" expected)
	 expected 
	 (tests:match patterns testname itempath)))
 (list "abc" "abc/%" "ab%/c%" "~abc/c%" "abc/~c%" "a,b/c,%/d" "%/,%/a" "%/,%/a" "%/,%/a" "%" "%" "%/" "%/")
 (list "abc" "abc"   "abcd"   "abc"     "abc"     "a"         "abc"     "def"    "ghi"   "a" "a"  "a"  "a")
 (list   ""  ""      "cde"    "cde"     "cde"     ""            ""      "a"       "b"    ""  "b"  ""   "b")
 (list   #t    #t       #t    #f           #f      #t           #t       #t       #f     #t  #t   #t    #f))

;; db:patt->like
(test #f "testname LIKE 't%'" (db:patt->like "testname" "t%" comparator: " AND "))
(test #f "testname LIKE 't%' AND testname LIKE '%t'" (db:patt->like "testname" "t%,%t" comparator: " AND "))
(test #f "item_path GLOB ''" (db:patt->like "item_path" ""))

;; test:match->sqlqry
(test #f "(testname GLOB 'a' AND item_path GLOB 'b') OR (testname LIKE 'a%' AND item_path LIKE '%') OR (testname GLOB '' AND item_path LIKE 'b%')"
      (tests:match->sqlqry "a/b,a%,/b%"))
(test #f "(testname GLOB 'a' AND item_path GLOB 'b') OR (testname LIKE 'a%' AND item_path LIKE '%') OR (testname LIKE '%' AND item_path LIKE 'b%')"
      (tests:match->sqlqry "a/b,a%,%/b%"))

;; (exit)

;;======================================================================
;; C O N F I G   F I L E S 
;;======================================================================

(define conffile #f)
(test "Read a config" #t (hash-table? (read-config "test.config" #f #f)))
(test "Read a config that doesn't exist" #t (hash-table? (read-config "nada.config" #f #f)))

(set! conffile (read-config "test.config" #f #f))
(test "Get available diskspace" #t (number? (get-df "./")))
(test "Get best dir" #t (let ((bestdir (get-best-disk conffile)))
			      (or (equal? "./"   bestdir)
				  (equal? "/tmp" bestdir))))
(test "Multiline variable" 4 (length (string-split (config-lookup conffile "metadata" "description") "\n")))

;; db
(define row    (vector "a" "b" "c" "blah"))
(define header (list "col1" "col2" "col3" "col4"))
(test "Get row by header" "blah" (db:get-value-by-header row header "col4"))

;; (define *toppath* "tests")
(define *db* #f)
(test "setup for run" #t (begin (setup-for-run)
				(string? (getenv "MT_RUN_AREA_HOME"))))
(test "open-db" #t (begin
		     (set! *db* (open-db))
		     (if *db* #t #f)))

;; quit wasting time, I'm changing *db* to db
(define db *db*)

(test "get cpu load" #t (number? (get-cpu-load)))
(test "get uname"    #t (string? (get-uname)))

(test "get validvalues as list" (list "start" "end" "completed")
      (string-split (config-lookup *configdat* "validvalues" "state")))

(for-each (lambda (item)
	    (test (conc "get valid items (" item ")")
		  item (check-valid-items "state" item)))
	  (list "start" "end" "completed"))

(for-each (lambda (item)
	    (test (conc "get valid items (" item ")")
		  item (check-valid-items "status" item)))
	  (list "pass" "fail" "n/a"))

(test "write env files" "nada.csh" (begin
                                      (save-environment-as-files "nada")
                                      (and (file-exists? "nada.sh")
    			                 (file-exists? "nada.csh"))))

(test "get all legal tests" (list "test1" "test2") (sort (get-all-legal-tests) string<=?))

(test "register-test, test info" "NOT_STARTED"
      (begin
	(rdb:tests-register-test *db* 1 "nada" "")
	;; (rdb:flush-queue)
	(vector-ref (db:get-test-info *db* 1 "nada" "") 3)))

(test #f "NOT_STARTED"    
      (begin
	(rdb:tests-register-test #f 1 "nada" "")
	;; (rdb:flush-queue)
	(vector-ref (open-run-close db:get-test-info #f 1 "nada" "") 3)))

(test "get-keys" "SYSTEM" (vector-ref (car (db:get-keys *db*)) 0));; (key:get-fieldname (car (sort (db-get-keys *db*)(lambda (a b)(string>=? (vector-ref a 0)(vector-ref b 0)))))))

(define remargs (args:get-args
		 '("bar" "foo" ":runname" "bob" ":SYSTEM" "ubuntu" ":RELEASE" "v1.2" ":datapath" "blah/foo" "nada")
		 (list ":runname" ":state" ":status")
		 (list "-h")
		 args:arg-hash
		 0))

(test "register-run" #t (number? (runs:register-run *db*
						    (db:get-keys *db*)
						    '(("SYSTEM" "key1")("RELEASE" "key2"))
						    "myrun" 
						    "new"
						    "n/a" 
						    "bob")))
(define keys (db:get-keys *db*))

;;======================================================================
;; D B
;;======================================================================
(test #f "FOO LIKE 'abc%def'" (db:patt->like "FOO" "abc%def"))
(test #f (vector '("SYSTEM" "RELEASE" "id" "runname" "state" "status" "owner" "event_time") '())
      (runs:get-runs-by-patt db keys "%"))
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
;; (set! *verbosity* 20)
(test "Run a test" #t (general-run-call 
		       "-runtests" 
		       "run a test"
		       (lambda (target runname keys keynames keyvallst)
			 (let ((test-patts "test%"))
			   ;; (runs:run-tests target runname test-patts user (make-hash-table))
			   (run:test 1 ;; run-id
				     (args:get-arg ":runname")
				     (keys:target->keyval keys target)
				     (vector
				      "test1"           ;; testname
				      tconfig           ;; testconfig
				      '()               ;; waitons
				      0                 ;; priority
				      #f                ;; items
				      #f                ;; itemsdat
				      #f                ;; spare
				      )
				     args:arg-hash      ;; flags (e.g. -itemspatt)
				     #f)))))

(test "cache is coherent" #t (let ((cached-info (db:get-test-info-cached-by-id db 2))
				   (non-cached  (db:get-test-info-not-cached-by-id db 2)))
			       (print "\nCached:    " cached-info)
			       (print "Noncached: " non-cached)
			       (equal? cached-info non-cached)))

(change-directory test-work-dir)
(test "Add a step"  #t
      (begin
	(db:teststep-set-status! db 2 "step1" "start" 0 "This is a comment" "mylogfile.html")
	(sleep 2)
	(db:teststep-set-status! db 2 "step1" "end" "pass" "This is a different comment" "finallogfile.html")
	(set! test-id (db:test-get-id (car (db:get-tests-for-run db 1 "test1" '() '()))))
	(number? test-id)))

(test "Get rundir"       #t (let ((rundir (db:test-get-rundir-from-test-id db test-id)))
			      (print "Rundir" rundir)
			      (string? rundir)))
(test "Create a test db" "../simpleruns/key1/key2/myrun/test1/testdat.db" (let ((tdb (db:open-test-db-by-test-id db test-id)))
			      (sqlite3#finalize! tdb)
			      (file-exists? "../simpleruns/key1/key2/myrun/test1/testdat.db")))
(test "Get steps for test" #t (> (length (db:get-steps-for-test db test-id)) 0))
(test "Get nice table for steps" "2.0s"
      (begin
	(vector-ref (hash-table-ref (db:get-steps-table db test-id) "step1") 4)))

;; (exit)

;;======================================================================
;; R E M O T E   C A L L S 
;;======================================================================

;; start a server process
(set! *verbosity* 10)
(define server-pid (process-run "../../bin/megatest" (list "-server" "-" "-debug" (conc *verbosity*))))
(sleep 2)
(define start-wait (current-seconds))
(server:client-setup)
(print "Starting intensive cache and rpc test")
(for-each (lambda (params)
	    ;;; (rdb:tests-register-test #f 1 (conc "test" (random 20)) "")
	    (apply rdb:test-set-status-state test-id params)
	    (rdb:pass-fail-counts test-id (random 100) (random 100))
	    (rdb:test-rollup-test_data-pass-fail test-id)
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
(rdb:flush-queue)
(let ((tests (open-run-close db:get-tests-for-run #f 1 "%" '() '())))
  (print "Setting " (length tests) " to COMPLETED/PASS")
  (for-each
   (lambda (test)
     (rdb:test-set-status-state (db:test-get-id test) "COMPLETED" "PASS" "Forced pass"))
   tests))

(print "Waiting for server to be done, should be about 20 seconds")
(process-wait server-pid)
(test "Server wait time" #t (let ((run-delta (- (current-seconds) start-wait)))
			      (print "Server ran for " run-delta " seconds")
			      (> run-delta 20)))

(test "Rollup the run(s)" #t (begin
			       (runs:rollup-run keys (keys->alist keys "na") "rollup" "matt")
			       #t))

(hash-table-set! args:arg-hash ":runname" "%")

(test "Remove the rollup run" #t (begin (operate-on 'remove-runs)))

;; ADD ME!!!! (db:get-prereqs-not-met *db* 1 '("runfirst") "" mode: 'normal)
;; ADD ME!!!! (rdb:get-tests-for-run *db* 1 "runfirst" #f '() '())
