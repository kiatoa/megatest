(use test)
;; (require-library args)

(include "../common.scm")
(include "../keys.scm")
(include "../db.scm")
(include "../configf.scm")
(include "../process.scm")
(include "../launch.scm")
(include "../items.scm")
(include "../runs.scm")

(define conffile #f)
(test "Read a config" #t (hash-table? (read-config "test.config")))
(test "Read a config that doesn't exist" #t (hash-table? (read-config "nada.config")))

(set! conffile (read-config "test.config"))
(test "Get available diskspace" #t (number? (get-df "./")))
(test "Get best dir" #t (let ((bestdir (get-best-disk conffile)))
			      (or (equal? "./"   bestdir)
				  (equal? "/tmp" bestdir))))

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

(test "get all legal tests" (list "runfirst" "runwithfirst" "singletest" "singletest2" "sqlitespeed") (sort (get-all-legal-tests) string<=?))

(test "register-test, test info" "NOT_STARTED"
      (begin
	(register-test *db* 1 "nada" "")
	(test:get-state (db:get-test-info *db* 1 "nada" ""))))

(test "get-keys" "sysname" (key:get-fieldname (car (sort (db-get-keys *db*)(lambda (a b)(string>=? (vector-ref a 0)(vector-ref b 0)))))))

(define remargs (args:get-args
		 '("bar" "foo" ":runname" "bob" ":sysname" "ubuntu" ":fsname" "nfs" ":datapath" "blah/foo" "nada")
		 (list ":runname" ":state" ":status")
		 (list "-h")
		 args:arg-hash
		 0))

(test "register-run" #t (number? (register-run *db* (db-get-keys *db*))))

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