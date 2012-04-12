;;======================================================================
;; Copyright 2006-2012, Matthew Welland.
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

;;======================================================================
;; Make the regexp's needed globally available
;;======================================================================

(define configf:include-rx (regexp "^\\[include\\s+(.*)\\]\\s*$"))
(define configf:section-rx (regexp "^\\[(.*)\\]\\s*$"))
(define configf:blank-l-rx (regexp "^\\s*$"))
(define configf:key-sys-pr (regexp "^(\\S+)\\s+\\[system\\s+(\\S+.*)\\]\\s*$"))
(define configf:key-val-pr (regexp "^(\\S+)\\s+(.*)$"))
(define configf:comment-rx (regexp "^\\s*#.*"))
(define configf:cont-ln-rx (regexp "^(\\s+)(\\S+.*)$"))

;; read a line and process any #{ ... } constructs

(define configf:var-expand-regex (regexp "^(.*)#\\{(scheme|system|shell|getenv|get|runconfigs-get)\\s+([^\\}\\{]*)\\}(.*)"))
(define (configf:process-line l ht)
  (let loop ((res l))
    (if (string? res)
	(let ((matchdat (string-search configf:var-expand-regex res)))
	  (if matchdat
	      (let* ((prestr  (list-ref matchdat 1))
		     (cmdtype (list-ref matchdat 2)) ;; eval, system, shell, getenv
		     (cmd     (list-ref matchdat 3))
		     (poststr (list-ref matchdat 4))
		     (result  #f)
		     (fullcmd (case (string->symbol cmdtype)
				((scheme)(conc "(lambda (ht)" cmd ")"))
				((system)(conc "(lambda (ht)(system \"" cmd "\"))"))
				((shell) (conc "(lambda (ht)(shell \""  cmd "\"))"))
				((getenv)(conc "(lambda (ht)(get-environment-variable \"" cmd "\"))"))
				((get)   
				 (let* ((parts (string-split cmd))
					(sect  (car parts))
					(var   (cadr parts)))
				   (conc "(lambda (ht)(config-lookup ht \"" sect "\" \"" var "\"))")))
				((runconfigs-get) (conc "(lambda (ht)(runconfigs-get ht \"" cmd "\"))"))
				(else "(lambda (ht)(print \"ERROR\") \"ERROR\")"))))
		;; (print "fullcmd=" fullcmd)
		(with-input-from-string fullcmd
		  (lambda ()
		    (set! result ((eval (read)) ht))))
		(loop (conc prestr result poststr)))
	      res))
	res)))

;; Run a shell command and return the output as a string
(define (shell cmd)
  (let* ((output (cmd-run->list cmd))
	 (res    (car output))
	 (status (cadr output)))
    (if (equal? status 0)
	(let ((outres (string-intersperse 
		       res
		       "\n")))
	  (debug:print 4 "INFO: shell result:\n" outres)
	  outres)
	(begin
	  (with-output-to-port (current-error-port)
	    (print "ERROR: " cmd " returned bad exit code " status))
	  ""))))

;; Lookup a value in runconfigs based on -reqtarg or -target
(define (runconfigs-get config var)
  (let ((targ (or (args:get-arg "-reqtarg")(args:get-arg "-target"))))
    (if targ
	(config-lookup config targ var)
	#f)))

(define-inline (configf:read-line p ht)
  (configf:process-line (read-line p) ht))

;; read a config file, returns hash table of alists

;; read a config file, returns hash table of alists
;; adds to ht if given (must be #f otherwise)
;; envion-patt is a regex spec that identifies sections that will be eval'd
;; in the environment on the fly

(define (read-config path ht allow-system #!key (environ-patt #f)(curr-section #f))
  (debug:print 4 "INFO: read-config " path " allow-system " allow-system " environ-patt " environ-patt " curr-section: " curr-section)
  (if (not (file-exists? path))
      (begin 
	(debug:print 4 "INFO: read-config - file not found " path " current path: " (current-directory))
	(if (not ht)(make-hash-table) ht))
      (let ((inp        (open-input-file path))
	    (res        (if (not ht)(make-hash-table) ht)))
	(let loop ((inl               (configf:read-line inp res)) ;; (read-line inp))
		   (curr-section-name (if curr-section curr-section "default"))
		   (var-flag #f);; turn on for key-var-pr and cont-ln-rx, turn off elsewhere
		   (lead     #f))
	  (if (eof-object? inl) 
	      (begin
		(close-input-port inp)
		res)
	      (regex-case 
	       inl 
	       (configf:comment-rx _                  (loop (configf:read-line inp res) curr-section-name #f #f))
	       (configf:blank-l-rx _                  (loop (configf:read-line inp res) curr-section-name #f #f))
	       (configf:include-rx ( x include-file ) (let ((curr-dir (current-directory))
							    (conf-dir  (pathname-directory path)))
							(if conf-dir (change-directory conf-dir))
							(read-config include-file res allow-system environ-patt: environ-patt curr-section: curr-section-name)
							(change-directory curr-dir)
							(loop (configf:read-line inp res) curr-section-name #f #f)))
	       (configf:section-rx ( x section-name ) (loop (configf:read-line inp res) section-name #f #f))
	       (configf:key-sys-pr ( x key cmd      ) (if allow-system
							  (let ((alist (hash-table-ref/default res curr-section-name '()))
								(val-proc (lambda ()
									    (let* ((cmdres  (cmd-run->list cmd))
										   (status  (cadr cmdres))
										   (res     (car  cmdres)))
									      (debug:print 4 "INFO: " inl "\n => " (string-intersperse res "\n"))
									      (if (not (eq? status 0))
										  (begin
										    (debug:print 0 "ERROR: problem with " inl ", return code " status
												 " output: " cmdres)
										    (exit 1)))
									      (if (null? res)
										  ""
										  (string-intersperse res " "))))))
							    (hash-table-set! res curr-section-name 
									     (config:assoc-safe-add alist
												    key 
												    (case allow-system
												      ((return-procs) val-proc)
												      ((return-string) cmd)
												      (else (val-proc)))))
							    (loop (configf:read-line inp res) curr-section-name #f #f))
							  (loop (configf:read-line inp res) curr-section-name #f #f)))
	       (configf:key-val-pr ( x key val      ) (let* ((alist   (hash-table-ref/default res curr-section-name '()))
							     (envar   (and environ-patt (string-search (regexp environ-patt) curr-section-name)))
							     (realval (if envar
									  (config:eval-string-in-environment val)
									  val)))
							(debug:print 6 "INFO: read-config env setting, envar: " envar " realval: " realval " val: " val " key: " key " curr-section-name: " curr-section-name)
							(if envar
							    (begin
							      ;; (debug:print 4 "INFO: read-config key=" key ", val=" val ", realval=" realval)
							      (setenv key realval)))
							(hash-table-set! res curr-section-name 
									 (config:assoc-safe-add alist key realval))
							(loop (configf:read-line inp res) curr-section-name key #f)))
	       ;; if a continued line
	       (configf:cont-ln-rx ( x whsp val     ) (let ((alist (hash-table-ref/default res curr-section-name '())))
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
						      (loop (configf:read-line inp res) curr-section-name var-flag (if lead lead whsp)))
						    (loop (configf:read-line inp res) curr-section-name #f #f))))
	       (else (debug:print 0 "ERROR: problem parsing " path ",\n   \"" inl "\"")
		     (set! var-flag #f)
		     (loop (configf:read-line inp res) curr-section-name #f #f))))))))
  
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
	  (if match ;; (and match (list? match)(> (length match) 1))
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
	  (let ((match (string-match configf:cont-ln-rx hed)))
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

;; note: I'm cheating a little here. I merely replace "\n" with "\n         "
(define (configf:expand-multi-lines fdat)
  ;; step 1.5 - compress any continued lines
  (if (null? fdat) fdat
      (let loop ((hed (car fdat))
		 (tal (cdr fdat))
		 (res '()))
	(let ((newres (append res (list (string-substitute (regexp "\n") "\n         " hed #t)))))
	  (if (null? tal)
	      newres
	      (loop (car tal)(cdr tal) newres))))))

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
  (let* (;; step 1: Open the output file and read it into a list
	 (fdat       (configf:file->list fname))
	 (refdat  (make-hash-table))
	 (sechash (make-hash-table)) ;; current section hash, init with hash for "default" section
	 (new     #f) ;; put the line to be used in new, if it is to be deleted the set new to #f
	 (secname #f))

    ;; step 2: Flatten multiline entries
    (if (not (null? fdat))(set! fdat (configf:compress-multi-line fdat)))

    ;; step 3: Modify values per contents of "indat" and remove absent values
    (if (not (null? fdat))
	(let loop ((hed  (car fdat))
		   (tal  (cadr fdat))
		   (res  '())
		   (lnum 0))
	  (regex-case 
	   hed
	   (configf:comment-rx _                  (set! res (append res (list hed)))) ;; (loop (read-line inp) curr-section-name #f #f))
	   (configf:blank-l-rx _                  (set! res (append res (list hed)))) ;; (loop (read-line inp) curr-section-name #f #f))
	   (configf:section-rx ( x section-name ) (let ((section-hash (hash-table-ref/default refdat section-name #f)))
					    (if (not section-hash)
						(let ((newhash (make-hash-table)))
						  (hash-table-set! refhash section-name newhash)
						  (set! sechash newhash))
						(set! sechash section-hash))
					    (set! new hed) ;; will append this at the bottom of the loop
					    (set! secname section-name)
					    ))
	   ;; No need to process key cmd, let it fall though to key val
	   (configf:key-val-pr ( x key val      )
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

