;;======================================================================
;; read a config file, loading only the section pertinent
;; to this run field1val/field2val/field3val ...
;;======================================================================

(use format directory-utils)

(declare (unit runconfig))
(declare (uses common))

(include "common_records.scm")

(define (runconfig:read fname target environ-patt)
  (let ((ht (make-hash-table)))
    (if target (hash-table-set! ht target '()))
    (read-config fname ht #t environ-patt: environ-patt sections: (if target (list "default" target) #f))))

;; NB// to process a runconfig ensure to use environ-patt with target!
;;
(define (setup-env-defaults fname run-id already-seen keyvals #!key (environ-patt #f)(change-env #t))
  (let* ((keys    (map car keyvals))
	 (thekey  (if keyvals 
		      (string-intersperse (map (lambda (x)(if x x "-na-")) (map cadr keyvals)) "/")
		      (or (common:args-get-target)
			  (get-environment-variable "MT_TARGET")
			  (begin
			    (debug:print-error 0 *default-log-port* "setup-env-defaults called with no run-id or -target or -reqtarg")
			    "nothing matches this I hope"))))
	 ;; Why was system disallowed in the reading of the runconfigs file?
	 ;; NOTE: Should be setting env vars based on (target|default)
	 (confdat   (runconfig:read fname thekey environ-patt))
	 (whatfound (make-hash-table))
	 (finaldat  (make-hash-table))
	 (sections (list "default" thekey)))
    (if (not *target*)(set! *target* thekey)) ;; may save a db access or two but repeats db:get-target code
    (debug:print 4 *default-log-port* "Using key=\"" thekey "\"")

    (if change-env
	(for-each ;; NB// This can be simplified with new content of keyvals having all that is needed.
	 (lambda (keyval)
	   (safe-setenv (car keyval)(cadr keyval)))
	 keyvals))
	
    (for-each 
     (lambda (section)
       (let ((section-dat (hash-table-ref/default confdat section #f)))
	 (if section-dat
	     (for-each 
	      (lambda (envvar)
		(let ((val (cadr (assoc envvar section-dat))))
		(hash-table-set! whatfound section (+ (hash-table-ref/default whatfound section 0) 1))
		(if (and (string? envvar)
			 (string? val)
			 change-env)
		    (safe-setenv envvar val))
		(hash-table-set! finaldat envvar val)))
	      (map car section-dat)))))
     sections)
    (if already-seen
	(begin
	  (debug:print 2 *default-log-port* "Key settings found in runconfigs.config:")
	  (for-each (lambda (fullkey)
		      (debug:print 2 *default-log-port* (format #f "~20a ~a\n" fullkey (hash-table-ref/default whatfound fullkey 0))))
		    sections)
	  (debug:print 2 *default-log-port* "---")
	  (set! *already-seen-runconfig-info* #t)))
    ;; finaldat ;; was returning this "finaldat" which would be good but conflicts with other uses
    confdat
    ))

(define (set-run-config-vars run-id keyvals targ-from-db)
  (push-directory *toppath*) ;; the push/pop doesn't appear to do anything ...
  (let ((runconfigf (conc  *toppath* "/runconfigs.config"))
	(targ       (or (common:args-get-target)
			targ-from-db
			(get-environment-variable "MT_TARGET"))))
    (pop-directory)
    (if (file-exists? runconfigf)
	(setup-env-defaults runconfigf run-id #t keyvals
			    environ-patt: (conc "(default"
						(if targ
						    (conc "|" targ ")")
						    ")")))
	(debug:print 0 *default-log-port* "WARNING: You do not have a run config file: " runconfigf))))

;; given (a (b c) d) return ((a b d)(a c d))
;; NOTE: this feels like it has been done before - perhaps with items handling?
;;
(define (runconfig:combinations inlst)
  (let loop ((hed (car inlst))
	     (tal (cdr inlst))
	     (res '()))
    ;; (print "res: " res " hed: " hed)
    (if (list? hed)
	(let ((newres (if (null? res) ;; first time through convert incoming items to list of items
			  (map list hed)
			  (apply append
				 (map (lambda (r)  ;; iterate over items in res
					(map (lambda (h) ;; iterate over items in hed
					       (append r (list h)))
					     hed))
				      res)))))
	  ;; (print "newres1: " newres)
	  (if (null? tal)
	      newres
	      (loop (car tal)(cdr tal) newres)))
	(let ((newres (if (null? res)
			  (list (list hed))
			  (map (lambda (r)
				 (append r (list hed)))
			       res))))
	  ;; (print "newres2: " newres)
	  (if (null? tal)
	      newres
	      (loop (car tal)(cdr tal) newres))))))

;; multi-part expand
;; Given a/b,c,d/e,f return a/b/e a/b/f a/c/e a/c/f a/d/e a/d/f
;;
(define (runconfig:expand target)
  (let* ((parts (map (lambda (x)
		       (string-split x ","))
		     (string-split target "/"))))
    (map (lambda (x)
	   (string-intersperse x "/"))
	 (runconfig:combinations parts))))

;; multi-target expansion
;; a/b/c/x,y,z a/b/d/x,y => a/b/c/x a/b/c/y a/b/c/z a/b/d/x a/b/d/y
;; 
(define (runconfig:expand-target target-strs)
  (delete-duplicates
   (apply append (map runconfig:expand (string-split target-strs " ")))))

#|
  (if (null? target-strs)
      '()
      (let loop ((hed (car target-strs))
		 (tal (cdr target-strs))
		 (res '()))
	;; first break all parts into individual target patterns
	(if (string-index hed " ") ;; this is a multi-target target
	    (let ((newres (append (string-split hed " ") res)))
	      (runconfig:expand-target newres))
	    (if (string-index hed ",") ;; this is a multi-target where one or more parts are comma separated
		  
|#
