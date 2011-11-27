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

(define (config:eval-string-in-environment str)
  (let ((cmdres (cmd-run->list (conc "echo " str))))
    (if (null? cmdres) ""
	(caar cmdres))))

;; read a config file, returns hash table of alists
;; adds to ht if given (must be #f otherwise)
;; envion-patt is a regex spec that identifies sections that will be eval'd
;; in the environment on the fly

(define (read-config path ht allow-system #!key (environ-patt #f))
  (debug:print 4 "INFO: read-config " path " allow-system " allow-system " environ-patt " environ-patt)
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
						(read-config include-file res allow-system environ-patt: environ-patt)
						(loop (read-line inp) curr-section-name #f #f)))
	       (section-rx ( x section-name ) (loop (read-line inp) section-name #f #f))
	       (key-sys-pr ( x key cmd      ) (if allow-system
						  (let ((alist (hash-table-ref/default res curr-section-name '()))
							(val-proc (lambda ()
								    (let* ((cmdres  (cmd-run->list cmd))
									   (status  (cadr cmdres))
									   (res     (car  cmdres)))
								      (if (not (eq? status 0))
									  (begin
									    (debug:print 0 "ERROR: problem with " inl ", return code " status)
									    (exit 1)))
								      (if (null? res)
									  ""
									  (string-intersperse res " "))))))
						    (hash-table-set! res curr-section-name 
								     (config:assoc-safe-add alist
											    key 
											    (if (eq? allow-system 'return-procs)
												val-proc
												(val-proc))))
						    (loop (read-line inp) curr-section-name #f #f))
						  (loop (read-line inp) curr-section-name #f #f)))
	       (key-val-pr ( x key val      ) (let* ((alist   (hash-table-ref/default res curr-section-name '()))
						     (envar   (and environ-patt (string-match (regexp environ-patt) curr-section-name)))
						     (realval (if envar
								 (config:eval-string-in-environment val)
								 val)))
						(if envar
						    (begin
						      (debug:print 4 "INFO: read-config key=" key ", val=" val ", realval=" realval)
						      (setenv key realval)))
						(hash-table-set! res curr-section-name 
								 (config:assoc-safe-add alist key realval))
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
  
(define (find-and-read-config fname #!key (environ-patt #f))
  (let* ((curr-dir   (current-directory))
         (configinfo (find-config fname))
	 (toppath    (car configinfo))
	 (configfile (cadr configinfo)))
    (if toppath (change-directory toppath)) 
    (let ((configdat  (if configfile (read-config configfile #f #t environ-patt: environ-patt) #f))) ;; (make-hash-table))))
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

(define (configf:section-vars cfgdat section)
  (let ((sectdat (hash-table-ref/default cfgdat section '())))
    (if (null? sectdat)
	'()
	(map car sectdat))))

(define (setup)
  (let* ((configf (find-config))
	 (config  (if configf (read-config configf #f #t) #f)))
    (if config
	(setenv "RUN_AREA_HOME" (pathname-directory configf)))
    config))

;;======================================================================
;; Non destructive writing of config file
;;======================================================================

(define (configf:compress-multi-lines fdat)
  ;; step 1.5 - compress any continued lines
  (if (null? fdat) fdat
	(let loop ((hed (car fdat))
		   (tal (cdr fdat))
		   (cur "")
		   (led #f)
		   (res '()))
	  ;; ALL WHITESPACE LEADING LINES ARE TACKED ON!!
	  ;;  1. remove led whitespace
	  ;;  2. tack on to hed with "\n"
	  (let ((match (string-match cont-ln-rx hed)))
	    (if match ;; blast! have to deal with a multiline
		(let* ((lead (cadr match))
		       (lval (caddr match))
		       (newl (conc cur "\n" lval)))
		  (if (not led)(set! led lead))
		  (if (null? tal) 
		      (set! fdat (append fdat (list newl)))
		      (loop (car tal)(cdr tal) newl led res))) ;; NB// not tacking newl onto res
		(let ((newres (if led 
				  (append res (list cur hed))
				  (append res (list hed)))))
		  ;; prev was a multiline
		  (if (null? tal)
		      newres
		      (loop (car tal)(cdr tal) "" #f newres))))))))

(define (configf:expand-multi-lines fdat)
  ;; step 1.5 - compress any continued lines
  (if (null? fdat) fdat
	(let loop ((hed (car fdat))
		   (tal (cdr fdat))
		   (cur "")
		   (led #f)
		   (res '()))
	  ;; ALL WHITESPACE LEADING LINES ARE TACKED ON!!
	  ;;  1. remove led whitespace
	  ;;  2. tack on to hed with "\n"
	  (let ((match (string-match cont-ln-rx hed)))
	    (if match ;; blast! have to deal with a multiline
		(let* ((lead (cadr match))
		       (lval (caddr match))
		       (newl (conc cur "\n" lval)))
		  (if (not led)(set! led lead))
		  (if (null? tal) 
		      (set! fdat (append fdat (list newl)))
		      (loop (car tal)(cdr tal) newl led res))) ;; NB// not tacking newl onto res
		(let ((newres (if led 
				  (append res (list cur hed))
				  (append res (list hed)))))
		  ;; prev was a multiline
		  (if (null? tal)
		      newres
		      (loop (car tal)(cdr tal) "" #f newres))))))))

(define (configf:file->list fname)
  (if (file-exists? fname)
      (let ((inp (open-input-file fname)))
	(let loop ((inl (read-line inp))
		   (res '()))
	  (if (eof-object? inl)
	      (begin
		(close-input-port inp)
		(reverse res))
	      (loop (read-line inp)(cons inl)))))
      '()))

;;======================================================================
;; Write a config
;;   0. Given a refererence data structure "indat"
;;   1. Open the output file and read it into a list
;;   2. Flatten any multiline entries
;;   3. Modify values per contents of "indat" and remove absent values
;;   4. Append new values to the section (immediately after last legit entry)
;;   5. Write out the new list 
;;======================================================================

(define (configf:write-config indat fname #!key (required-sections '()))
  (let* ((include-rx (regexp "^\\[include\\s+(.*)\\]\\s*$"))
	 (section-rx (regexp "^\\[(.*)\\]\\s*$"))
	 (blank-l-rx (regexp "^\\s*$"))
	 (key-sys-pr (regexp "^(\\S+)\\s+\\[system\\s+(\\S+.*)\\]\\s*$"))
	 (key-val-pr (regexp "^(\\S+)\\s+(.*)$"))
	 (comment-rx (regexp "^\\s*#.*"))
	 (cont-ln-rx (regexp "^(\\s+)(\\S+.*)$"))
	 ;; step 1: Open the output file and read it into a list
	 (fdat       (configf:file->list fname))
	 (refdat  (make-hash-table))
	 (sechash (make-hash-table)) ;; current section hash, init with hash for "default" section
	 (new     #f)) ;; put the line to be used in new, if it is to be deleted the set new to #f

    ;; step 2: Flatten multiline entries
    (if (not (null? fdat))(set! fdat (configf:compress-multi-line fdat)))

    ;; step 3: Modify values per contents of "indat" and remove absent values
    (if (not (null? fdat))
	(let loop ((hed  (car fdat))
		   (tal  (cadr fdat))
		   (res  '())
		   (sec  #f) ;; section
		   (lnum 0))
	  (regex-case 
	   hed
	   (comment-rx _                  (set! res (append res (list hed)))) ;; (loop (read-line inp) curr-section-name #f #f))
	   (blank-l-rx _                  (set! res (append res (list hed)))) ;; (loop (read-line inp) curr-section-name #f #f))
	   (section-rx ( x section-name ) (let ((section-hash (hash-table-ref/default refdat section-name #f)))
					    (if (not section-hash)
						(let ((newhash (make-hash-table)))
						  (hash-table-set! refhash section-name newhash)
						  (set! sechash newhash))
						(set! sechash section-hash))
					    (set! new hed) ;; will append this at the bottom of the loop
					    (set! sec section-name)
					    ))
	   ;; No need to process key cmd, let it fall though to key val
	   (key-val-pr ( x key val      )
		       (let ((newval (config-lookup indat sec key)))
			 ;; can handle newval == #f here => that means key is removed
			 (cond 
			  ((equal? newval val)
			   (set! res (append res (list hed))))
			  ((not newval) ;; key has been removed
			   (set! new #f))
			  ((not (equal? newval val))
			     (hash-table-set! sechash key newval)
			     (set! new (conc key " " newval)))
			  (else
			   (debug:print 0 "ERROR: problem parsing line number " lnum "\"" hed "\"")))))
	   (else
	    (debug:print 0 "ERROR: Problem parsing line num " lnum " :\n   " hed )))
	  (if (not (null? tal))
	      (loop (car tal)(cdr tal)(if new (append res (list new)) res)(+ lnum 1)))
	  ;; drop to here when done processing, res contains modified list of lines
	  (set! fdat res)))

    ;; step 4: Append new values to the section
    (for-each 
     (lambda (section)
       (let ((sdat   '()) ;; append needed bits here
	     (svars  (configf:section-vars indat section)))
	 (for-each 
	  (lambda (var)
	    (let ((val (config-lookup refdat section var)))
	      (if (not val) ;; this one is new
		  (begin
		    (if (null? sdat)(set! sdat (list (conc "[" section "]"))))
		    (set! sdat (append sdat (list (conc var " " val))))))))
	  svars)
	 (set! fdat (append fdat sdat))))
     (delete-duplicates (append require-sections (hash-table-keys indat))))

    ;; step 5: Write out new file
    (with-output-to-file fname 
      (lambda ()
	(for-each 
	 (lambda (line)
	   (print line))
	 (configf:expand-multi-lines fdat))))))

