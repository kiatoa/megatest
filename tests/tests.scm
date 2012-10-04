(require-extension test)

(define test-work-dir (current-directory))

;; read in all the _record files
(let ((files (glob "*_records.scm")))
  (for-each
   (lambda (file)
     (print "Loading " file)
     (load file))
   files))

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
	(db:tests-register-test *db* 1 "nada" "")
	(vector-ref (db:get-test-info *db* 1 "nada" "") 3)))
(test #f "NOT_STARTED"    
      (begin
	(open-run-close db:tests-register-test #f 1 "nada" "")
	(vector-ref (open-run-close db:get-test-info #f 1 "nada" "") 3)))

(test "get-keys" "SYSTEM" (vector-ref (car (db:get-keys *db*)) 0));; (key:get-fieldname (car (sort (db-get-keys *db*)(lambda (a b)(string>=? (vector-ref a 0)(vector-ref b 0)))))))

(define remargs (args:get-args
		 '("bar" "foo" ":runname" "bob" ":sysname" "ubuntu" ":fsname" "nfs" ":datapath" "blah/foo" "nada")
		 (list ":runname" ":state" ":status")
		 (list "-h")
		 args:arg-hash
		 0))

(test "register-run" #t (number? (runs:register-run *db*
						    (db:get-keys *db*)
						    '(("SYSTEM" "key1")("OS" "key2"))
						    "myrun" 
						    "new"
						    "n/a" 
						    "bob")))
(define keys (db:get-keys *db*))

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

;; (set! *verbosity* 20)
(test "Run a test" #t (general-run-call 
		       "-runtests" 
		       "run a test"
		       (lambda (target runname keys keynames keyvallst)
			 (let ((test-patts "test%"))
			   (runs:run-tests target runname test-patts user (make-hash-table))
			   ))))

(change-directory test-work-dir)
(test "Add a step"  #t
      (begin
	(db:teststep-set-status! db 2 "step1" "start" 0 "This is a comment" "mylogfile.html")
	(sleep 2)
	(db:teststep-set-status! db 2 "step1" "end" "pass" "This is a different comment" "finallogfile.html")
	(set! test-id (db:test-get-id (car (db:get-tests-for-run db 2 "test1" "" '() '()))))
	(number? test-id)))

(sleep 5)
(test "Get nice table for steps" "2s"
      (begin
	(vector-ref (hash-table-ref (db:get-steps-table db test-id) "step1") 4)))

(test "Rollup the run(s)" #t (begin
			       (runs:rollup-run keys (keys->alist keys "na") "rollup" "matt")
			       #t))

(hash-table-set! args:arg-hash ":runname" "%")

(test "Remove the rollup run" #t (begin (operate-on 'remove-runs)))

;; ADD ME!!!! (db:get-prereqs-not-met *db* 1 '("runfirst") "" mode: 'normal)
;; ADD ME!!!! (rdb:get-tests-for-run *db* 1 "runfirst" #f '() '())
