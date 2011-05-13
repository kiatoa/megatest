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
  (let ((newalist (filter (lambda (x)(not (equal? key x))) alist)))
    (append alist (list (list key val)))))

;; read a config file, returns two level hierarchial hash-table,
;; adds to ht if given (must be #f otherwise)
(define (read-config path . ht)
  (if (not (file-exists? path))
      (if (null? ht)(make-hash-table) (car ht))
      (let ((inp        (open-input-file path))
	    (res        (if (null? ht)(make-hash-table)(car ht)))
	    (include-rx (regexp "^\\[include\\s+(.*)\\]\\s*$"))
	    (section-rx (regexp "^\\[(.*)\\]\\s*$"))
	    (blank-l-rx (regexp "^\\s*$"))
	    (key-sys-pr (regexp "^(\\S+)\\s+\\[system\\s+(\\S+.*)\\]\\s*$"))
	    (key-val-pr (regexp "^(\\S+)\\s+(.*)$"))
	    (comment-rx (regexp "^\\s*#.*")))
	(let loop ((inl               (read-line inp))
		   (curr-section-name "default"))
	  (if (eof-object? inl) res
	      (regex-case 
	       inl 
	       (comment-rx _                  (loop (read-line inp) curr-section-name))
	       (blank-l-rx _                  (loop (read-line inp) curr-section-name))
	       (include-rx ( x include-file ) (begin
						(read-config include-file res)
						(loop (read-line inp) curr-section-name)))
	       (section-rx ( x section-name ) (loop (read-line inp) section-name))
	       (key-sys-pr ( x key cmd      ) (let ((alist (hash-table-ref/default res curr-section-name '()))
						    (val   (let ((res (car (cmd-run->list cmd))))
							     (if (null? res)
								 ""
								 (string-intersperse res " ")))))
						(hash-table-set! res curr-section-name 
								 (config:assoc-safe-add alist key val))
								 ;; (append alist (list (list key val))))
						(loop (read-line inp) curr-section-name)))
	       (key-val-pr ( x key val      ) (let ((alist (hash-table-ref/default res curr-section-name '())))
						(hash-table-set! res curr-section-name 
								 (config:assoc-safe-add alist key val))
								 ;; (append alist (list (list key val))))
						(loop (read-line inp) curr-section-name)))
	       (else (print "ERROR: Should not get here,\n   \"" inl "\"")
		     (loop (read-line inp) curr-section-name))))))))
  
(define (find-and-read-config fname)
  (let* ((configinfo (find-config fname))
	 (toppath    (car configinfo))
	 (configfile (cadr configinfo))
	 (configdat  (if configfile (read-config configfile) #f))) ;; (make-hash-table))))
    (list configdat toppath configfile fname)))

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
	 (config  (if configf (read-config configf) #f)))
    (if config
	(setenv "RUN_AREA_HOME" (pathname-directory configf)))
    config))

