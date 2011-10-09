;;======================================================================
;; read a config file, loading only the section pertinent
;; to this run field1val/field2val/field3val ...
;;======================================================================

(use format)

(declare (unit runconfig))
(declare (uses common))

(include "common_records.scm")

(define (setup-env-defaults db fname run-id . already-seen)
  (let* ((keys    (get-keys db))
	 (keyvals (get-key-vals db run-id))
	 (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/"))
	 (confdat (read-config fname #f #f))
	 (whatfound (make-hash-table))
	 (sections (list "default" thekey)))
    (debug:print 4 "Using key=\"" thekey "\"")
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
    (if (and (not (null? already-seen))
	     (not (car already-seen)))
	(begin
	  (debug:print 2 "Key settings found in runconfig.config:")
	  (for-each (lambda (fullkey)
		      (debug:print 2 (format #f "~20a ~a\n" fullkey (hash-table-ref/default whatfound fullkey 0))))
		    sections)
	  (debug:print 2 "---")
	  (set! *already-seen-runconfig-info* #t)))))

(define (set-run-config-vars db run-id)
  (let ((runconfigf (conc  *toppath* "/runconfigs.config")))
    (if (file-exists? runconfigf)
	(setup-env-defaults db runconfigf run-id)
	(debug:print 0 "WARNING: You do not have a run config file: " runconfigf))))
  