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

(use regex regex-case) ;;  directory-utils)
(declare (unit configf))
(declare (uses process))
(declare (uses env))
(declare (uses keys))

(include "common_records.scm")

;; return list (path fullpath configname)
(define (find-config configname #!key (toppath #f))
  (if toppath
      (let ((cfname (conc toppath "/" configname)))
	(if (file-exists? cfname)
	    (list toppath cfname configname)
	    (list #f      #f     #f)))
      (let* ((cwd (string-split (current-directory) "/")))
	(let loop ((dir cwd))
	  (let* ((path     (conc "/" (string-intersperse dir "/")))
		 (fullpath (conc path "/" configname)))
	    (if (file-exists? fullpath)
		(list path fullpath configname)
		(let ((remcwd (take dir (- (length dir) 1))))
		  (if (null? remcwd)
		      (list #f #f #f) ;;  #f #f) 
		  (loop remcwd)))))))))

(define (config:assoc-safe-add alist key val #!key (metadata #f))
  (let ((newalist (filter (lambda (x)(not (equal? key (car x)))) alist)))
    (append newalist (list (if metadata
			       (list key val metadata)
			       (list key val))))))

(define (configf:section-var-set! cfgdat section-name var value #!key (metadata #f))
  (hash-table-set! cfgdat section-name
		   (config:assoc-safe-add
		    (hash-table-ref/default cfgdat section-name '())
		    var value metadata: metadata)))

(define (config:eval-string-in-environment str)
  (handle-exceptions
   exn
   (begin
     (debug:print-error 0 *default-log-port* "problem evaluating \"" str "\" in the shell environment")
     #f)
   (let ((cmdres (process:cmd-run->list (conc "echo " str))))
     (if (null? cmdres) ""
	 (caar cmdres)))))

;;======================================================================
;; Make the regexp's needed globally available
;;======================================================================

(define configf:include-rx (regexp "^\\[include\\s+(.*)\\]\\s*$"))
(define configf:script-rx  (regexp "^\\[scriptinc\\s+(\\S+)([^\\]]*)\\]\\s*$")) ;; include output from a script
(define configf:section-rx (regexp "^\\[(.*)\\]\\s*$"))
(define configf:blank-l-rx (regexp "^\\s*$"))
(define configf:key-sys-pr (regexp "^(\\S+)\\s+\\[system\\s+(\\S+.*)\\]\\s*$"))
(define configf:key-val-pr (regexp "^(\\S+)(\\s+(.*)|())$"))
(define configf:key-no-val (regexp "^(\\S+)(\\s*)$"))
(define configf:comment-rx (regexp "^\\s*#.*"))
(define configf:cont-ln-rx (regexp "^(\\s+)(\\S+.*)$"))
(define configf:settings   (regexp "^\\[configf:settings\\s+(\\S+)\\s+(\\S+)]\\s*$"))

;; read a line and process any #{ ... } constructs

(define configf:var-expand-regex (regexp "^(.*)#\\{(scheme|system|shell|getenv|get|runconfigs-get|rget|scm|sh|rp|gv|g|mtrah)\\s+([^\\}\\{]*)\\}(.*)"))

(define (configf:process-line l ht allow-system #!key (linenum #f))
  (let loop ((res l))
    (if (string? res)
	(let ((matchdat (string-search configf:var-expand-regex res)))
	  (if matchdat
	      (let* ((prestr  (list-ref matchdat 1))
		     (cmdtype (list-ref matchdat 2)) ;; eval, system, shell, getenv
		     (cmd     (list-ref matchdat 3))
		     (poststr (list-ref matchdat 4))
		     (result  #f)
		     (start-time (current-seconds))
		     (cmdsym  (string->symbol cmdtype))
		     (fullcmd (case cmdsym
				((scheme scm) (conc "(lambda (ht)" cmd ")"))
				((system)     (conc "(lambda (ht)(system \"" cmd "\"))"))
				((shell sh)   (conc "(lambda (ht)(string-translate (shell \""  cmd "\") \"\n\" \" \"))"))
				((realpath rp)(conc "(lambda (ht)(common:nice-path \"" cmd "\"))"))
				((getenv gv)  (conc "(lambda (ht)(get-environment-variable \"" cmd "\"))"))
				((mtrah)      (conc "(lambda (ht)"
                                                    "    (let ((extra \"" cmd "\"))"
						    "       (conc (or *toppath* (get-environment-variable \"MT_RUN_AREA_HOME\"))"
						    "             (if (string-null? extra) \"\" \"/\")"
						    "             extra)))"))
				((get g)   
				 (let* ((parts (string-split cmd))
					(sect  (car parts))
					(var   (cadr parts)))
				   (conc "(lambda (ht)(config-lookup ht \"" sect "\" \"" var "\"))")))
				((runconfigs-get rget) (conc "(lambda (ht)(runconfigs-get ht \"" cmd "\"))"))
				;; ((rget)           (conc "(lambda (ht)(runconfigs-get ht \"" cmd "\"))"))
				(else "(lambda (ht)(print \"ERROR\") \"ERROR\")"))))
		;; (print "fullcmd=" fullcmd)
		(handle-exceptions
		 exn
		 (begin
		   (debug:print 0 *default-log-port* "WARNING: failed to process config input \"" l "\"")
		   (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
		   ;; (print "exn=" (condition->list exn))
		   (set! result (conc "#{( " cmdtype ") " cmd "}, full expansion: " fullcmd)))
		 (if (or allow-system
			 (not (member cmdtype '("system" "shell" "sh"))))
		     (with-input-from-string fullcmd
		       (lambda ()
			 (set! result ((eval (read)) ht))))
		     (set! result (conc "#{(" cmdtype ") "  cmd "}"))))
		(case cmdsym
		  ((system shell scheme)
		   (let ((delta (- (current-seconds) start-time)))
		     (if (> delta 2)
			 (debug:print-info 0 *default-log-port* "for line \"" l "\"\n command:  " cmd " took " delta " seconds to run with output:\n   " result)
			 (debug:print-info 9 *default-log-port* "for line \"" l "\"\n command:  " cmd " took " delta " seconds to run with output:\n   " result)))))
		(loop (conc prestr result poststr)))
	      res))
	res)))

;; Run a shell command and return the output as a string
(define (shell cmd)
  (let* ((output (process:cmd-run->list cmd))
	 (res    (car output))
	 (status (cadr output)))
    (if (equal? status 0)
	(let ((outres (string-intersperse 
		       res
		       "\n")))
	  (debug:print-info 4 *default-log-port* "shell result:\n" outres)
	  outres)
	(begin
	  (with-output-to-port (current-error-port)
	    (lambda ()
	      (print "ERROR: " cmd " returned bad exit code " status)))
	  ""))))

;; this was inline but I'm pretty sure that is a hold over from when it was *very* simple ...
;;
(define (configf:read-line p ht allow-processing settings)
  (let loop ((inl (read-line p)))
    (let ((cont-line (and (string? inl)
			  (not (string-null? inl))
			  (equal? "\\" (string-take-right inl 1)))))
      (if cont-line ;; last character is \ 
	  (let ((nextl (read-line p)))
	    (if (not (eof-object? nextl))
		(loop (string-append (if cont-line 
					 (string-take inl (- (string-length inl) 1))
					 inl)
				     nextl))))
	  (let ((res (case allow-processing ;; if (and allow-processing 
		       ;;	   (not (eq? allow-processing 'return-string)))
		       ((#t #f)
			(configf:process-line inl ht allow-processing))
		       ((return-string)
			inl)
		       (else
			(configf:process-line inl ht allow-processing)))))
	    (if (and (string? res)
		     (not (equal? (hash-table-ref/default settings "trim-trailing-spaces" "no") "no")))
		(string-substitute "\\s+$" "" res)
		res))))))
  
(define (calc-allow-system allow-system section sections)
  (if sections
      (and (or (equal? "default" section)
	       (member section sections))
	   allow-system) ;; account for sections and return allow-system as it might be a symbol such as return-strings
      allow-system))
    
;; given a config hash and a section name, apply that section to all matching sections (using wildcard % or regex if /..../)
;; remove the section when done so that there is no downstream clobbering
;;
(define (configf:apply-wildcards ht section-name)
  (if (hash-table-exists? ht section-name)
      (let* ((vars  (hash-table-ref ht section-name))
	     (rxstr (if (string-contains section-name "%")
			(string-substitute (regexp "%") ".*" section-name)
			(string-substitute (regexp "^/(.*)/$") "\\1" section-name)))
	     (rx    (regexp rxstr)))
	;; (print "\nsection-name: " section-name " rxstr: " rxstr)
        (for-each
         (lambda (section)
	   (if section
	       (let ((same-section (string=? section-name section))
		     (rx-match     (string-match rx section)))
		 ;; (print "section: " section " vars: " vars " same-section: " same-section " rx-match: " rx-match)
		 (if (and (not same-section) rx-match)
		     (for-each
		      (lambda (bundle)
			;; (print "bundle: " bundle)
			(let ((key  (car bundle))
			      (val  (cadr bundle))
			      (meta (if (> (length bundle) 2)(caddr bundle) #f)))
			  (hash-table-set! ht section (config:assoc-safe-add (hash-table-ref ht section) key val metadata: meta))))
		      vars)))))
         (hash-table-keys ht))))
  ht)

;; read a config file, returns hash table of alists

;; read a config file, returns hash table of alists
;; adds to ht if given (must be #f otherwise)
;; envion-patt is a regex spec that identifies sections that will be eval'd
;; in the environment on the fly
;; sections: #f => get all, else list of sections to gather
;; post-section-procs alist of section-pattern => proc, where: (proc section-name next-section-name ht curr-path)
;; apply-wildcards: #t/#f - apply vars from targets with % wildcards to all matching sections
;;
(define (read-config path ht allow-system #!key (environ-patt #f)            (curr-section #f)
		     (sections #f)              (settings (make-hash-table)) (keep-filenames #f)
		     (post-section-procs '())   (apply-wildcards #t))
  (debug:print 9 *default-log-port* "START: " path)
  (if *configdat*
      (common:save-pkt `((action . read-config)
			 (f      . path))
		       *configdat* #t add-only: #t))
  (if (and (not (port? path))
	   (not (file-exists? path))) ;; for case where we are handed a port
      (begin 
	(debug:print-info 1 *default-log-port* "read-config - file not found " path " current path: " (current-directory))
	;; WARNING: This is a risky change but really, we should not return an empty hash table if no file read?
	#f) ;; (if (not ht)(make-hash-table) ht))
      (let ((inp        (if (string? path)
			    (open-input-file path)
			      path)) ;; we can be handed a port
	    (res        (if (not ht)(make-hash-table) ht))
	    (metapath   (if (or (debug:debug-mode 9)
				keep-filenames)
			    path #f))
            (process-wildcards  (lambda (res curr-section-name)
                                  (if (and apply-wildcards
                                           (or (string-contains curr-section-name "%")   ;; wildcard
                                               (string-match "/.*/" curr-section-name))) ;; regex
                                      (begin
                                        (configf:apply-wildcards res curr-section-name)
                                        (hash-table-delete! res curr-section-name))))))  ;; NOTE: if the section is a wild card it will be REMOVED from res 
	(let loop ((inl               (configf:read-line inp res (calc-allow-system allow-system curr-section sections) settings)) ;; (read-line inp))
		   (curr-section-name (if curr-section curr-section "default"))
		   (var-flag #f);; turn on for key-var-pr and cont-ln-rx, turn off elsewhere
		   (lead     #f))
	  (debug:print-info 8 *default-log-port* "curr-section-name: " curr-section-name " var-flag: " var-flag "\n   inl: \"" inl "\"")
	  (if (eof-object? inl) 
	      (begin
                ;; process last section for wildcards
                (process-wildcards res curr-section-name)
		(if (string? path) ;; we received a path, not a port, thus we are responsible for closing it.
		    (close-input-port inp))
		(if (list? sections) ;; delete all sections except given when sections is provided
		    (for-each
		     (lambda (section)
		       (if (not (member section sections))
			   (hash-table-delete! res section))) ;; we are using "" as a dumping ground and must remove it before returning the ht
		     (hash-table-keys res)))
		(debug:print 9 *default-log-port* "END: " path)
		res)
	      (regex-case 
	       inl 
	       (configf:comment-rx _                  (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
	       (configf:blank-l-rx _                  (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
	       (configf:settings   ( x setting val  ) (begin
							(hash-table-set! settings setting val)
							(loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f)))
	       (configf:include-rx ( x include-file ) (let* ((curr-conf-dir (pathname-directory path))
							     (full-conf     (if (absolute-pathname? include-file)
										include-file
										(common:nice-path 
										 (conc (if curr-conf-dir
											   curr-conf-dir
											   ".")
										       "/" include-file)))))
							(if (file-exists? full-conf)
							    (begin
							      ;; (push-directory conf-dir)
							      (debug:print 9 *default-log-port* "Including: " full-conf)
							      (read-config full-conf res allow-system environ-patt: environ-patt curr-section: curr-section-name sections: sections settings: settings keep-filenames: keep-filenames)
							      ;; (pop-directory)
							      (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
							    (begin
							      (debug:print '(2 9) #f "INFO: include file " include-file " not found (called from " path ")")
							      (debug:print 2 *default-log-port* "        " full-conf)
							      (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f)))))
	       (configf:script-rx ( x include-script params);; handle-exceptions
                                  ;;    exn
                                  ;;    (begin
                                  ;;      (debug:print '(0 2 9) #f "INFO: include from script " include-script " failed.")
                                  ;;      (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
							 (if (and (file-exists? include-script)(file-execute-access? include-script))
							     (let* ((new-inp-port (open-input-pipe (conc include-script " " params))))
							       (debug:print '(2 9) *default-log-port* "Including from script output: " include-script)
							      ;;  (print "We got here, calling read-config next. Port is: " new-inp-port)
							       (read-config new-inp-port res allow-system environ-patt: environ-patt curr-section: curr-section-name sections: sections settings: settings keep-filenames: keep-filenames)
							       (close-input-port new-inp-port)
							       (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
							     (begin
							       (debug:print 0 *default-log-port* "Script not found or not exectutable: " include-script)
							       (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f)))
							 ) ;; )
	       (configf:section-rx ( x section-name ) (begin
							;; call post-section-procs
							(for-each 
							 (lambda (dat)
							   (let ((patt (car dat))
								 (proc (cdr dat)))
							     (if (string-match patt curr-section-name)
								 (proc curr-section-name section-name res path))))
							 post-section-procs)
                                                        ;; after gathering the vars for a section and if apply-wildcards is true and if there is a wildcard in the section name process wildcards
                                                        ;; NOTE: we are processing the curr-section-name, NOT section-name.
                                                        (process-wildcards res curr-section-name)
							(if (not (hash-table-ref/default res section-name #f))(hash-table-set! res section-name '())) ;; ensure that mere mention of a section is not lost
							(loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings)
							      ;; if we have the sections list then force all settings into "" and delete it later?
							      ;; (if (or (not sections) 
							      ;;	      (member section-name sections))
							      ;;	  section-name "") ;; stick everything into "". NOPE: We need new strategy. Put stuff in correct sections and then delete all sections later.
							      section-name
							      #f #f)))
	       (configf:key-sys-pr ( x key cmd      ) (if (calc-allow-system allow-system curr-section-name sections)
							  (let ((alist    (hash-table-ref/default res curr-section-name '()))
								(val-proc (lambda ()
									    (let* ((start-time (current-seconds))
										   (cmdres     (process:cmd-run->list cmd))
										   (delta      (- (current-seconds) start-time))
										   (status     (cadr cmdres))
										   (res        (car  cmdres)))
									      (debug:print-info 4 *default-log-port* "" inl "\n => " (string-intersperse res "\n"))
									      (if (not (eq? status 0))
										  (begin
										    (debug:print-error 0 *default-log-port* "problem with " inl ", return code " status
												 " output: " cmdres)))
									      (if (> delta 2)
										  (debug:print-info 0 *default-log-port* "for line \"" inl "\"\n  command: " cmd " took " delta " seconds to run with output:\n   " res)
										  (debug:print-info 9 *default-log-port* "for line \"" inl "\"\n  command: " cmd " took " delta " seconds to run with output:\n   " res))
									      (if (null? res)
										  ""
										  (string-intersperse res " "))))))
							    (hash-table-set! res curr-section-name 
									     (config:assoc-safe-add alist
									   			    key 
												    (case (calc-allow-system allow-system curr-section-name sections)
												      ((return-procs) val-proc)
												      ((return-string) cmd)
												      (else (val-proc)))
												    metadata: metapath))
							    (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))
							  (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f)))
	       (configf:key-no-val ( x key val)            (let* ((alist   (hash-table-ref/default res curr-section-name '()))
								  (fval    (or (if (string? val) val #f) ""))) ;; fval should be either "" or " " (one or more spaces)
							     (debug:print 10 *default-log-port* "   setting: [" curr-section-name "] " key " = #t")
							     (safe-setenv key fval)
							     (hash-table-set! res curr-section-name 
									      (config:assoc-safe-add alist key fval metadata: metapath))
							     (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name key #f)))
	       (configf:key-val-pr ( x key unk1 val unk2 ) (let* ((alist   (hash-table-ref/default res curr-section-name '()))
								  (envar   (and environ-patt (string-search (regexp environ-patt) curr-section-name)))
								  (realval (if envar
									       (config:eval-string-in-environment val)
									       val)))
							     (debug:print-info 6 *default-log-port* "read-config env setting, envar: " envar " realval: " realval " val: " val " key: " key " curr-section-name: " curr-section-name)
							     (if envar (safe-setenv key realval))
							     (debug:print 10 *default-log-port* "   setting: [" curr-section-name "] " key " = " val)
							     (hash-table-set! res curr-section-name 
									      (config:assoc-safe-add alist key realval metadata: metapath))
							     (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name key #f)))
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
								       (config:assoc-safe-add alist var-flag newval metadata: metapath))
						      (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name var-flag (if lead lead whsp)))
						    (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))))
	       (else (debug:print-error 0 *default-log-port* "problem parsing " path ",\n   \"" inl "\"")
		     (set! var-flag #f)
		     (loop (configf:read-line inp res (calc-allow-system allow-system curr-section-name sections) settings) curr-section-name #f #f))))))))
  
;; pathenvvar will set the named var to the path of the config
(define (find-and-read-config fname #!key (environ-patt #f)(given-toppath #f)(pathenvvar #f))
  (let* ((curr-dir   (current-directory))
         (configinfo (find-config fname toppath: given-toppath))
	 (toppath    (car configinfo))
	 (configfile (cadr configinfo))
	 (set-fields (lambda (curr-section next-section ht path)
		       (let ((field-names (if ht (common:get-fields ht) '()))
			     (target      (or (getenv "MT_TARGET")(args:get-arg "-reqtarg")(args:get-arg "-target"))))
			 (debug:print-info 9 *default-log-port* "set-fields with field-names=" field-names " target=" target " curr-section=" curr-section " next-section=" next-section " path=" path " ht=" ht)
			 (if (not (null? field-names))(keys:target-set-args field-names target #f))))))
    (if toppath (change-directory toppath)) 
    (if (and toppath pathenvvar)(setenv pathenvvar toppath))
    (let ((configdat  (if configfile 
			  (read-config configfile #f #t environ-patt: environ-patt post-section-procs: (list (cons "^fields$" set-fields)) #f))))
      (if toppath (change-directory curr-dir))
      (list configdat toppath configfile fname))))

(define (config-lookup cfgdat section var)
  (if (hash-table? cfgdat)
      (let ((sectdat (hash-table-ref/default cfgdat section '())))
	(if (null? sectdat)
	    #f
	    (let ((match (assoc var sectdat)))
	      (if match ;; (and match (list? match)(> (length match) 1))
		  (cadr match)
		  #f))
	    ))
      #f))

;; use to have definitive setting:
;;  [foo]
;;  var yes
;;
;;  (configf:var-is? cfgdat "foo" "var" "yes") => #t
;;
(define (configf:var-is? cfgdat section var expected-val)
  (equal? (configf:lookup cfgdat section var) expected-val))

(define configf:lookup config-lookup)
(define configf:read-file read-config)

;; safely look up a value that is expected to be a number, return
;; a default (#f unless provided)
;;
(define (configf:lookup-number cfdat section varname #!key (default #f))
  (let* ((val (configf:lookup *configdat* section varname))
         (res (if val
                  (string->number (string-substitute "\\s+" "" val #t))
                  #f)))
    (cond
     (res  res)
     (val  (debug:print 0 *default-log-port* "ERROR: no number found for [" section "], " varname ", got: " val))
     (else default))))

(define (configf:section-vars cfgdat section)
  (let ((sectdat (hash-table-ref/default cfgdat section '())))
    (if (null? sectdat)
	'()
	(map car sectdat))))

(define (configf:get-section cfgdat section)
  (hash-table-ref/default cfgdat section '()))

(define (setup)
  (let* ((configf (find-config "megatest.config"))
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
	      (loop (read-line inp)(cons inl res)))))
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
			   (debug:print-error 0 *default-log-port* "problem parsing line number " lnum "\"" hed "\"")))))
	   (else
	    (debug:print-error 0 *default-log-port* "Problem parsing line num " lnum " :\n   " hed )))
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

;;======================================================================
;; refdb
;;======================================================================

;; reads a refdb into an assoc array of assoc arrays
;;   returns (list dat msg)
(define (configf:read-refdb refdb-path)
  (let ((sheets-file  (conc refdb-path "/sheet-names.cfg")))
    (if (not (file-exists? sheets-file))
	(list #f (conc "ERROR: no refdb found at " refdb-path))
	(if (not (file-read-access? sheets-file))
	    (list #f (conc "ERROR: refdb file not readable at " refdb-path))
	    (let* ((sheets (with-input-from-file sheets-file
			     (lambda ()
			       (let loop ((inl (read-line))
					  (res '()))
				 (if (eof-object? inl)
				     (reverse res)
				     (loop (read-line)(cons inl res)))))))
		   (data   '()))
	      (for-each 
	       (lambda (sheet-name)
		 (let* ((dat-path  (conc refdb-path "/" sheet-name ".dat"))
			(ref-dat   (configf:read-file dat-path #f #t))
			(ref-assoc (map (lambda (key)
					  (list key (hash-table-ref ref-dat key)))
					(hash-table-keys ref-dat))))
				   ;; (hash-table->alist ref-dat)))
		   ;; (set! data (append data (list (list sheet-name ref-assoc))))))
		   (set! data (cons (list sheet-name ref-assoc) data))))
	       sheets)
	      (list data "NO ERRORS"))))))

;; map over all pairs in a three level hierarchial alist and apply a function to the keys/val
;;
(define (configf:map-all-hier-alist data proc #!key (initproc1 #f)(initproc2 #f)(initproc3 #f))
  (for-each 
   (lambda (sheetname)
     (let* ((sheettmp  (assoc sheetname data))
	    (sheetdat  (if sheettmp (cadr sheettmp) '())))
       (if initproc1 (initproc1 sheetname))
       (for-each 
	(lambda (sectionname)
	  (let* ((sectiontmp  (assoc sectionname sheetdat))
		 (sectiondat  (if sectiontmp (cadr sectiontmp) '())))
	    (if initproc2 (initproc2 sheetname sectionname))
	    (for-each
	     (lambda (varname)
	       (let* ((valtmp (assoc varname sectiondat))
		      (val    (if valtmp (cadr valtmp) "")))
		 (proc sheetname sectionname varname val)))
	     (map car sectiondat))))
	(map car sheetdat))))
   (map car data))
  data)

;;======================================================================
;;  C O N F I G   T O / F R O M   A L I S T
;;======================================================================

(define (configf:config->alist cfgdat)
  (hash-table->alist cfgdat))

(define (configf:alist->config adat)
  (let ((ht (make-hash-table)))
    (for-each
     (lambda (section)
       (hash-table-set! ht (car section)(cdr section)))
     adat)
    ht))

;; if 
(define (configf:read-alist fname)
  (handle-exceptions
      exn
      #f
    (configf:alist->config
     (with-input-from-file fname read))))

(define (configf:write-alist cdat fname)
    (if (common:faux-lock fname)
        (let* ((dat  (configf:config->alist cdat))
               (res
                (begin
                  (with-output-to-file fname ;; first write out the file
                    (lambda ()
                      (pp dat)))
                  
                  (if (common:file-exists? fname)   ;; now verify it is readable
                      (if (configf:read-alist fname)
                          #t ;; data is good.
                          (begin
                            (handle-exceptions
                             exn
                             #f
                             (debug:print 0 *default-log-port* "WARNING: content " dat " for cache " fname " is not readable. Deleting generated file.")
                             (delete-file fname))
                            #f))
                      #f))))
          
          (common:faux-unlock fname)
          res)
        (begin
          (debug:print 0 *default-log-port* "WARNING: could not get faux-lock on " fname)
          #f)))

;; convert hierarchial list to ini format
;;
(define (configf:config->ini data)
  (map 
   (lambda (section)
     (let ((section-name (car section))
	   (section-dat  (cdr section)))
       (print "\n[" section-name "]")
       (map (lambda (dat-pair)
	      (let* ((var (car dat-pair))
		     (val (cadr dat-pair))
		     (fname (if (> (length dat-pair) 2)(caddr dat-pair) #f)))
		(if fname (print "# " var "=>" fname))
		(print var " " val)))
	    section-dat))) ;;       (print "section-dat: " section-dat))
   (hash-table->alist data)))
