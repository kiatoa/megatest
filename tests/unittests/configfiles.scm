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
		  item (items:check-valid-items "state" item)))
	  (list "start" "end" "completed"))

(for-each (lambda (item)
	    (test (conc "get valid items (" item ")")
		  item (items:check-valid-items "status" item)))
	  (list "pass" "fail" "n/a"))

(test #f #f (items:check-valid-items "state" "blahfool"))

(test "write env files" "nada.csh" (begin
                                      (save-environment-as-files "nada")
                                      (and (file-exists? "nada.sh")
    			                 (file-exists? "nada.csh"))))

