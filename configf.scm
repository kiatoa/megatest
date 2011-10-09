;;======================================================================
;; Copyright 2006-2011, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Config file handling
;;======================================================================

(use regex regex-case)
(declare (unit configf))
(declare (uses common))
(declare (uses process))

(include "common_records.scm")

;; return list (path fullpath configname)
(define (find-config configname)
  (let* ((cwd (string-split (current-directory) "/")))
    (let loop ((dir cwd))
      (let* ((path     (conc "/" (string-intersperse dir "/")))
	     (fullpath (conc path "/" configname)))
	(if (file-exists? fullpath)
	    (list path fullpath configname)
	    (let ((remcwd (take dir (- (length dir) 1))))
	      (if (null? remcwd)
		  (list #f #f #f) ;;  #f #f) 
		  (loop remcwd))))))))

(define (config:assoc-safe-add alist key val)
  (let ((newalist (filter (lambda (x)(not (equal? key (car x)))) alist)))
    (append newalist (list (list key val)))))

;; read a config file, returns hash table of alists
;; adds to ht if given (must be #f otherwise)
(define (read-config path ht allow-system)
  (if (not (file-exists? path))
      (if (not ht)(make-hash-table) ht)
      (let ((inp        (open-input-file path))
	    (res        (if (not ht)(make-hash-table) ht))
	    (include-rx (regexp "^\\[include\\s+(.*)\\]\\s*$"))
	    (section-rx (regexp "^\\[(.*)\\]\\s*$"))
	    (blank-l-rx (regexp "^\\s*$"))
	    (key-sys-pr (regexp "^(\\S+)\\s+\\[system\\s+(\\S+.*)\\]\\s*$"))
	    (key-val-pr (regexp "^(\\S+)\\s+(.*)$"))
	    (comment-rx (regexp "^\\s*#.*"))
	    (cont-ln-rx (regexp "^(\\s+)(\\S+.*)$")))
	(let loop ((inl               (read-line inp))
		   (curr-section-name "default")
		   (var-flag #f);; turn on for key-var-pr and cont-ln-rx, turn off elsewhere
		   (lead     #f))
	  (if (eof-object? inl) 
	      (begin
		(close-input-port inp)
		res)
	      (regex-case 
	       inl 
	       (comment-rx _                  (loop (read-line inp) curr-section-name #f #f))
	       (blank-l-rx _                  (loop (read-line inp) curr-section-name #f #f))
	       (include-rx ( x include-file ) (begin
						(read-config include-file res allow-system)
						(loop (read-line inp) curr-section-name #f #f)))
	       (section-rx ( x section-name ) (loop (read-line inp) section-name #f #f))
	       (key-sys-pr ( x key cmd      ) (if allow-system
						  (let ((alist (hash-table-ref/default res curr-section-name '()))
							(val   (let* ((cmdres  (cmd-run->list cmd))
								      (status  (cadr cmdres))
								      (res     (car  cmdres)))
								 (if (not (eq? status 0))
								     (begin
								       (debug:print 0 "ERROR: problem with " inl ", return code " status)
								       (exit 1)))
								 (if (null? res)
								     ""
								     (string-intersperse res " ")))))
						    (hash-table-set! res curr-section-name 
								     (config:assoc-safe-add alist key val))
						    (loop (read-line inp) curr-section-name #f #f))
						  (loop (read-line inp) curr-section-name #f #f)))
	       (key-val-pr ( x key val      ) (let ((alist (hash-table-ref/default res curr-section-name '())))
						(hash-table-set! res curr-section-name 
								 (config:assoc-safe-add alist key val))
						(loop (read-line inp) curr-section-name key #f)))
	       ;; if a continued line
	       (cont-ln-rx ( x whsp val     ) (let ((alist (hash-table-ref/default res curr-section-name '())))
						(if var-flag             ;; if set to a string then we have a continued var
						    (let ((newval (conc 
								   (config-lookup res curr-section-name var-flag) "\n"
								   ;; trim lead from the incoming whsp to support some indenting.
								   (if lead
								       (string-substitute (regexp lead) "" whsp)
								       "")
								   val)))
						      ;; (print "val: " val "\nnewval: \"" newval "\"\nvarflag: " var-flag)
						      (hash-table-set! res curr-section-name 
								       (config:assoc-safe-add alist var-flag newval))
						      (loop (read-line inp) curr-section-name var-flag (if lead lead whsp)))
						    (loop (read-line inp) curr-section-name #f #f))))
	       (else (debug:print 0 "ERROR: problem parsing " path ",\n   \"" inl "\"")
		     (set! var-flag #f)
		     (loop (read-line inp) curr-section-name #f #f))))))))
  
(define (find-and-read-config fname)
  (let* ((curr-dir   (current-directory))
         (configinfo (find-config fname))
	 (toppath    (car configinfo))
	 (configfile (cadr configinfo)))
    (if toppath (change-directory toppath)) 
    (let ((configdat  (if configfile (read-config configfile #f #t) #f))) ;; (make-hash-table))))
      (if toppath (change-directory curr-dir))
      (list configdat toppath configfile fname))))

(define (config-lookup cfgdat section var)
  (let ((sectdat (hash-table-ref/default cfgdat section '())))
    (if (null? sectdat)
	#f
	(let ((match (assoc var sectdat)))
	  (if match
	      (cadr match)
	      #f))
	)))

(define (setup)
  (let* ((configf (find-config))
	 (config  (if configf (read-config configf #f #t) #f)))
    (if config
	(setenv "RUN_AREA_HOME" (pathname-directory configf)))
    config))

