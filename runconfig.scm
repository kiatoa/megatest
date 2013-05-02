;;======================================================================
;; read a config file, loading only the section pertinent
;; to this run field1val/field2val/field3val ...
;;======================================================================

(use format directory-utils)

(declare (unit runconfig))
(declare (uses common))

(include "common_records.scm")



;; (define (setup-env-defaults db fname run-id already-seen #!key (environ-patt #f)(change-env #t))
(define (setup-env-defaults fname run-id already-seen keys keyvals #!key (environ-patt #f)(change-env #t))
  (let* (;; (keys    (db:get-keys db))
	 ;; (keyvals (if run-id (db:get-key-vals db run-id) #f))
	 (thekey  (if keyvals (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/")
		      (if (args:get-arg "-reqtarg") 
			  (args:get-arg "-reqtarg")
			  (if (args:get-arg "-target")
			      (args:get-arg "-target")
			      (begin
				(debug:print 0 "ERROR: setup-env-defaults called with no run-id or -target or -reqtarg")
				"nothing matches this I hope")))))
	 ;; Why was system disallowed in the reading of the runconfigs file?
	 ;; NOTE: Should be setting env vars based on (target|default)
	 (confdat (read-config fname #f #t environ-patt: environ-patt sections: (list "default" thekey)))
	 (whatfound (make-hash-table))
	 (finaldat  (make-hash-table))
	 (sections (list "default" thekey)))
    (if (not *target*)(set! *target* thekey)) ;; may save a db access or two but repeats db:get-target code
    (debug:print 4 "Using key=\"" thekey "\"")

    (if change-env
	(for-each
	 (lambda (key val)
	   (setenv (vector-ref key 0) val))
	 keys keyvals))
	
    (for-each 
     (lambda (section)
       (let ((section-dat (hash-table-ref/default confdat section #f)))
	 (if section-dat
	     (for-each 
	      (lambda (envvar)
		(let ((val (cadr (assoc envvar section-dat))))
		(hash-table-set! whatfound section (+ (hash-table-ref/default whatfound section 0) 1))
		(if change-env (setenv envvar val))
		(hash-table-set! finaldat envvar val)))
	      (map car section-dat)))))
     sections)
    (if already-seen
	(begin
	  (debug:print 2 "Key settings found in runconfig.config:")
	  (for-each (lambda (fullkey)
		      (debug:print 2 (format #f "~20a ~a\n" fullkey (hash-table-ref/default whatfound fullkey 0))))
		    sections)
	  (debug:print 2 "---")
	  (set! *already-seen-runconfig-info* #t)))
    finaldat))

(define (set-run-config-vars run-id keys keyvals targ-from-db)
  (push-directory *toppath*)
  (let ((runconfigf (conc  *toppath* "/runconfigs.config"))
	(targ       (or (args:get-arg "-target")
			(args:get-arg "-reqtarg")
			targ-from-db)))
    (pop-directory)
    (if (file-exists? runconfigf)
	(setup-env-defaults runconfigf run-id #t keys keyvals
			    environ-patt: (conc "(default"
						(if targ
						    (conc "|" targ ")")
						    ")")))
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))))
 