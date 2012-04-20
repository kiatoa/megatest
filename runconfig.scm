;;======================================================================
;; read a config file, loading only the section pertinent
;; to this run field1val/field2val/field3val ...
;;======================================================================

(use format)

(declare (unit runconfig))
(declare (uses common))

(include "common_records.scm")



(define (setup-env-defaults db fname run-id already-seen #!key (environ-patt #f))
  (let* ((keys    (rdb:get-keys db))
	 (keyvals (rdb:get-key-vals db run-id))
	 (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/"))
	 ;; Why was system disallowed in the reading of the runconfigs file?
	 ;; NOTE: Should be setting env vars based on (target|default)
	 (confdat (read-config fname #f #t environ-patt: environ-patt sections: '("default" thekey)))
	 (whatfound (make-hash-table))
	 (sections (list "default" thekey)))
    (if (not *target*)(set! *target* thekey)) ;; may save a db access or two but repeats db:get-target code
    (debug:print 4 "Using key=\"" thekey "\"")

    (for-each
     (lambda (key val)
       (setenv (vector-ref key 0) val))
     keys keyvals)

    (for-each 
     (lambda (section)
       (let ((section-dat (hash-table-ref/default confdat section #f)))
	 (if section-dat
	     (for-each 
	      (lambda (envvar)
		(hash-table-set! whatfound section (+ (hash-table-ref/default whatfound section 0) 1))
		(setenv envvar (cadr (assoc envvar section-dat))))
	      (map car section-dat)))))
     sections)
    (if already-seen
	(begin
	  (debug:print 2 "Key settings found in runconfig.config:")
	  (for-each (lambda (fullkey)
		      (debug:print 2 (format #f "~20a ~a\n" fullkey (hash-table-ref/default whatfound fullkey 0))))
		    sections)
	  (debug:print 2 "---")
	  (set! *already-seen-runconfig-info* #t)))))

(define (set-run-config-vars db run-id)
  (let ((runconfigf (conc  *toppath* "/runconfigs.config"))
	(targ       (or (args:get-arg "-target")
			(args:get-arg "-reqtarg")
			(db:get-target db run-id))))
    (if (file-exists? runconfigf)
	(setup-env-defaults db runconfigf run-id #t environ-patt: (conc "(default"
									(if targ
									    (conc "|" targ ")")
									    ")")))
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))))
 