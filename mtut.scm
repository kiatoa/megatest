;; Copyright 2006-2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

;; (include "common.scm")
;; (include "megatest-version.scm")

;; fake out readline usage of toplevel-command
(define (toplevel-command . a) #f)

(use srfi-1 posix srfi-69 readline ;;  regex regex-case srfi-69 apropos json http-client directory-utils rpc typed-records;; (srfi 18) extras)
     srfi-18 extras format pkts pkts regex regex-case
     (prefix dbi dbi:)) ;;  zmq extras)

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
(declare (uses configf))
;; (declare (uses rmt))

(include "megatest-fossil-hash.scm")

(require-library stml)

(define *target-mappers*  (make-hash-table)) ;; '())
(define *runname-mappers* (make-hash-table)) ;; '())

;; this needs some thought regarding security implications.
;;
;;   i. Check that owner of the file and calling user are same?
;;  ii. Check that we are in a legal megatest area?
;; iii. Have some form of authentication or record of the md5sum or similar of the file?
;;  iv. Use compiled version in preference to .scm version. Thus there is a manual "blessing"
;;      required to use .mtutil.scm.
;;
(if (file-exists? "megatest.config")
    (if (file-exists? ".mtutil.so")
	(load ".mtutil.so")
	(if (file-exists? ".mtutil.scm")
	(load ".mtutil.scm"))))

;; Disabled help items
;;  -rollup                 : (currently disabled) fill run (set by :runname)  with latest test(s)
;;                            from prior runs with same keys
;; Contour actions
;;    import                  : import pkts
;;    dispatch                : dispatch queued run jobs from imported pkts
;;    rungen                  : look at input sense list in [rungen] and generate run pkts

(define help (conc "
mtutil, part of the Megatest tool suite, documentation at http://www.kiatoa.com/fossils/megatest
  version " megatest-version "
  license GPL, Copyright Matt Welland 2006-2017

Usage: mtutil action [options]
  -h                         : this help
  -manual                    : show the Megatest user manual
  -version                   : print megatest version (currently " megatest-version ")
			     
Actions:		     
   run                       : initiate runs
   remove                    : remove runs
   rerun                     : register action for processing
   set-ss                    : set state/status
   archive                   : compress and move test data to archive disk
   kill                      : stop tests or entire runs
   db                        : database utilities
   areas, contours, setup    : show areas, contours or setup section from megatest.config

Contour actions:
   process                   : runs import, rungen and dispatch 
			     
Selectors 		     
  -immediate                 : apply this action immediately, default is to queue up actions
  -area areapatt1,area2...   : apply this action only to the specified areas
  -target key1/key2/...      : run for key1, key2, etc.
  -test-patt p1/p2,p3/...    : % is wildcard
  -run-name                  : required, name for this particular test run
  -contour contourname       : run all targets for contourname, requires -run-name, -target
  -state-status c/p,c/f      : Specify a list of state and status patterns
  -tag-expr tag1,tag2%,..    : select tests with tags matching expression
  -mode-patt key             : load testpatt from <key> in runconfigs instead of default TESTPATT
                               if -testpatt and -tagexpr are not specified
  -new state/status          : specify new state/status for set-ss
			     
Misc 			     
  -start-dir path            : switch to this directory before running mtutil
  -set-vars V1=1,V2=2        : Add environment variables to a run NB// these are
                                   overwritten by values set in config files.
  -log logfile               : send stdout and stderr to logfile
  -repl                      : start a repl (useful for extending megatest)
  -load file.scm             : load and run file.scm
  -debug N|N,M,O...          : enable debug messages 0-N or N and M and O ...
			     
Utility			     
 db pgschema                 : emit postgresql schema; do \"mtutil db pgschema | psql -d mydb\"

Examples:

# Start a megatest run in the area \"mytests\"
mtutil run -area mytests -target v1.63/aa3e -mode-patt MYPATT -tag-expr quick

# Start a contour
mtutil run -contour quick -target v1.63/aa3e 

Called as " (string-intersperse (argv) " ") "
Version " megatest-version ", built from " megatest-fossil-hash ))

;; args and pkt key specs
;;
(define *arg-keys*
  '(("-area"       . G) ;; maps to group
    ("-target"     . t)
    ("-run-name"   . n)
    ("-state"      . e)
    ("-status"     . s)
    ("-contour"    . c)
    ("-test-patt"  . p)  ;; idea, enhance margs ("-test-patt" "-testpatt") => yields one value in "-test-patt"
    ("-mode-patt"  . o)
    ("-tag-expr"   . x)
    ("-item-patt"  . i)
    ("-sync-to"    . k)
    ("-append-config" . d)
    ;; misc
    ("-start-dir"  . S)
    ("-msg"        . M)
    ("-set-vars"   . v)
    ("-debug"      . #f)  ;; for *verbosity* > 2
    ("-load"       . #f)  ;; load and exectute a scheme file
    ("-log"        . #f)
    ))
(define *switch-keys*
  '(("-h"          . #f)
    ("-help"       . #f)
    ("--help"      . #f)
    ("-manual"     . #f)
    ("-version"    . #f)
    ;; misc
    ("-repl"       . #f)
    ("-immediate"  . I)
    ("-preclean"   . r)
    ("-rerun-all"  . u)
    ))

;; alist to map actions to old megatest commands
(define *action-keys*
  '((run         . "-run")
    (sync        . "")
    (archive     . "-archive")
    (set-ss      . "-set-state-status")))

;; inlst is an alternative input
;;
(define (lookup-param-by-key key #!key (inlst #f))
  (fold (lambda (a res)
	  (if (eq? (cdr a) key)
	      (car a)
	      res))
	#f
	(or inlst *arg-keys*)))

(define (lookup-action-by-key key)
  (alist-ref (string->symbol key) *action-keys*))

;;======================================================================
;;  U T I L S
;;======================================================================

;; given a mtutil param, return the old megatest equivalent
;;
(define (param-translate param)
  (or (alist-ref (string->symbol param)
		 '((-tag-expr  . "-tagexpr")
		   (-mode-patt . "--modepatt")
		   (-run-name  . "-runname")
		   (-test-patt . "-testpatt")
		   (-msg       . "-m")))
      param))

(define (val->alist val)
  (let ((val-list (string-split-fields ";\\s*" val #:infix)))
    (if val-list
	(map (lambda (x)
	       (let ((f (string-split-fields "\\s*=\\s*" x #:infix)))
		 (case (length f)
		   ((0) `(,#f))  ;; null string case
		   ((1) `(,(string->symbol (car f))))
		   ((2) `(,(string->symbol (car f)) . ,(cadr f)))
		   (else f))))
	     val-list)
	'())))

(define (push-run-spec torun contour runkey spec)
  (configf:section-var-set! torun contour runkey
			    (cons spec
				  (or (configf:lookup torun contour runkey)
				      '()))))

(define (fossil:clone-or-sync url name dest-dir)
  (let ((targ-file (conc dest-dir "/" name))) ;; do not force usage of .fossil extension
    (handle-exceptions
	exn
	(print "ERROR: failed to create directory " dest-dir " message: " ((condition-property-accessor 'exn 'message) exn))
      (create-directory dest-dir #t))
    (handle-exceptions
	exn
	(print "ERROR: failed to clone or sync 1ossil " url " message: " ((condition-property-accessor 'exn 'message) exn))
      (if (file-exists? targ-file)
	  (system (conc "fossil pull --once " url " -R " targ-file))
	  (system (conc "fossil clone " url " " targ-file))
	  ))))

(define (fossil:last-change-node-and-time fossils-dir fossil-name branch)
  (let* ((fossil-file   (conc fossils-dir "/" fossil-name))
	 (timeline-port (if (file-read-access? fossil-file)
			    (handle-exceptions
				exn
				(begin
				  (print "ERROR: failed to get timeline from " fossil-file " message: " ((condition-property-accessor 'exn 'message) exn))
				  #f)
			      (open-input-pipe (conc "fossil timeline -t ci -W 0 -n 0 -R " fossil-file)))
			    #f))
	 (get-line      (lambda ()
			  (handle-exceptions
			      exn
			      (begin
				(print "ERROR: failed to read from file " fossil-file " message: "  ((condition-property-accessor 'exn 'message) exn))
				#f)
			    (read-line timeline-port))))
	 (date-rx       (regexp "^=== (\\S+) ===$"))
	 (node-rx       (regexp "^(\\S+) \\[(\\S+)\\].*\\(.*tags:\\s+([^\\)]+)\\)$")))
    (let loop ((inl (get-line))
	       (date #f)
	       (node #f)
	       (time #f))
      (cond
       ((and date time node) ;; have all, return 'em
	(close-input-port timeline-port)
	(values (common:date-time->seconds (conc date " " time)) node))
       ((and inl (not (eof-object? inl))) ;; have a line to process
	(regex-case inl
	  (date-rx ( _ newdate ) (loop (get-line) newdate node time))
	  ;; 22:47:48 [a024d9e60f] Added *user-hash-data* - a global that can be used in -repl and #{scheme ...} calls by the end user (user: matt tags: v1.63)
	  (node-rx ( _ newtime newnode alltags )
		   (let ((tags (string-split-fields ",\\s*" alltags #:infix)))
		     (print "tags: " tags)
		     (if (member branch tags)
			 (loop (get-line) date newnode newtime)
			 (loop (get-line) date node time))))
	  (else ;; have some unrecognised junk? spit out error message
	   (print "ERROR: fossil timeline returned unrecognisable junk \"" inl "\"")
	   (loop (get-line) date node time))))
       (else ;; no more datat and last node on branch not found
	(close-input-port timeline-port)
	(values  (common:date-time->seconds (conc date " " time)) node))))))


;;======================================================================
;; GLOBALS
;;======================================================================

;; Card types:
;;
;; a action
;; u username (Unix)
;; D timestamp
;; T card type

;; process args
(define *action* (if (> (length (argv)) 1)
		     (cadr (argv))
		     #f))
(define remargs (args:get-args 
		 (if *action* (cdr (argv)) (argv)) ;; args:get-args dumps first in argv list (the program name)
		 (map car *arg-keys*)
		 (map car *switch-keys*)
		 args:arg-hash
		 0))

(if (or (member *action* '("-h" "-help" "help" "--help"))
	(args:any-defined? "-h" "-help" "--help"))
    (begin
      (print help)
      (exit 1)))

;; (print "*action*: " *action*)
;; (let-values (((uuid pkt)
;; 	      (command-line->pkt #f args:arg-hash)))
;;   (print pkt))

;; Add args that use remargs here
;;
(if (and (not (null? remargs))
	 (not (or
	       (args:get-arg "-runstep")
	       (args:get-arg "-envcap")
	       (args:get-arg "-envdelta")
	       (member *action* '("db"))   ;; very loose checks on db.
	       (equal? *action* "show")    ;; just keep going if list
	       )))
    (debug:print-error 0 *default-log-port* "Unrecognised arguments: " (string-intersperse (if (list? remargs) remargs (argv))  " ")))

(if (or (args:any? "-h" "help" "-help" "--help")
	(member *action* '("-h" "-help" "--help" "help")))
    (begin
      (print help)
      (exit 1)))

;;======================================================================
;; pkts
;;======================================================================

(define (with-queue-db mtconf proc)
  (let* ((pktsdirs (configf:lookup mtconf "setup"  "pktsdirs"))
	 (pktsdir  (if pktsdirs (car (string-split pktsdirs " ")) #f))
	 (toppath  (configf:lookup mtconf "dyndat" "toppath"))
	 (pdbpath  (or (configf:lookup mtconf "setup"  "pdbpath") pktsdir)))
    (if (not (and  pktsdir toppath pdbpath))
	(begin
	  (print "ERROR: settings are missing in your megatest.config for area management.")
	  (print "  you need to have pktsdir in the [setup] section."))
	(let* ((pdb  (open-queue-db pdbpath "pkts.db"
				    schema: '("CREATE TABLE groups (id INTEGER PRIMARY KEY,groupname TEXT, CONSTRAINT group_constraint UNIQUE (groupname));"))))
	  (proc pktsdirs pktsdir pdb)
	  (dbi:close pdb)))))

(define (load-pkts-to-db mtconf)
  (with-queue-db
   mtconf
   (lambda (pktsdirs pktsdir pdb)
     (for-each
      (lambda (pktsdir) ;; look at all
	(if (and (file-exists? pktsdir)
		 (directory? pktsdir)
		 (file-read-access? pktsdir))
	    (let ((pkts (glob (conc pktsdir "/*.pkt"))))
	      (for-each
	       (lambda (pkt)
		 (let* ((uuid    (cadr (string-match ".*/([0-9a-f]+).pkt" pkt)))
			(exists  (lookup-by-uuid pdb uuid #f)))
		   (if (not exists)
		       (let* ((pktdat (string-intersperse
				       (with-input-from-file pkt read-lines)
				       "\n"))
			      (apkt   (pkt->alist pktdat))
			      (ptype  (alist-ref 'T apkt)))
			 (add-to-queue pdb pktdat uuid (or ptype 'cmd) #f 0)
			 (debug:print 4 *default-log-port* "Added " uuid " of type " ptype " to queue"))
		       (debug:print 4 *default-log-port* "pkt: " uuid " exists, skipping...")
		       )))
	       pkts))))
      (string-split pktsdirs)))))

(define (get-pkt-alists pkts)
  (map (lambda (x)
	 (alist-ref 'apkt x)) ;; 'pkta pulls out the alist from the read pkt
       pkts))

;; given list of pkts (alist mode) return list of D cards as Unix epoch, sorted descending
;; also delete duplicates by target i.e. (car pkt)
(define (get-pkt-times pkts)
  (delete-duplicates
   (sort 
    (map (lambda (x)
	   `(,(alist-ref 't x) . ,(string->number (alist-ref 'D x))))
	 pkts)
    (lambda (a b)(> (cdr a)(cdr b))))      ;; sort descending
   (lambda (a b)(equal? (car a)(car b))))) ;; remove duplicates by target

;;======================================================================
;; Runs
;;======================================================================

;; make a runname
;;
(define (make-runname pre post)
 (time->string
  (seconds->local-time (current-seconds)) "%Yw%V.%w-%H%M"))

;; collect, translate, collate and assemble a pkt from the command-line
;;
;; sched => force the run start time to be recorded as sched Unix
;; epoch. This aligns times properly for triggers in some cases.
;;
(define (command-line->pkt action args-alist sched-in #!key (extra-dat '()))
  (let* ((sched     (cond
		     ((vector? sched-in)(local-time->seconds sched-in)) ;; we recieved a time
		     ((number? sched-in) sched-in)
		     (else     (current-seconds))))
	 (args-data (if args-alist
			(if (hash-table? args-alist) ;; seriously?
			    (hash-table->alist args-alist)
			    args-alist)
			(hash-table->alist args:arg-hash))) ;; if no args-alist then we assume this is a call driven directly by commandline
	 (alldat    (apply append (list 'T "cmd"
					'a action
					'U (current-user-name)
					'D sched)
                           extra-dat
			   (map (lambda (x)
				  (let* ((param (car x))
					 (value (cdr x))
					 (pmeta (assoc param *arg-keys*))    ;; translate the card key to a megatest switch or parameter
					 (smeta (assoc param *switch-keys*)) ;; first lookup the key in arg-keys or switch-keys
					 (meta  (if (or pmeta smeta)
						    (cdr (or pmeta smeta))   ;; found it?
						    #f)))
				    (if (or pmeta smeta)                     ;; construct the switch/param pair.
					(list meta value)
					'())))
				(filter cdr args-data)))))
;; (print  "Alldat: " alldat
;;         " args-data: " args-data)
    (add-z-card
     (apply construct-sdat alldat))))

(define (simple-setup start-dir-in)
  (let* ((start-dir (or start-dir-in "."))
	 (mtconfig  (or (args:get-arg "-config") "megatest.config"))
	 (mtconfdat (find-and-read-config        ;; NB// sets MT_RUN_AREA_HOME as side effect
		     mtconfig
		     ;; environ-patt: "env-override"
		     given-toppath: start-dir
		     ;; pathenvvar: "MT_RUN_AREA_HOME"
		     ))
	 (mtconf    (if mtconfdat (car mtconfdat) #f)))
    ;; we set some dynamic data in a section called "dyndata"
    (if mtconf
	(begin
	  (configf:section-var-set! mtconf "dyndat" "toppath" start-dir)))
    ;; (print "TOPPATH: " (configf:lookup mtconf "dyndat" "toppath"))
    mtconfdat))


;; NEED TIMESTAMP ON PKTS for efficient loading of packets into db.


;; make a run request pkt from basic data, this seriously needs to be refactored
;;   i. Take the code that builds the info to submit to create-run-pkt and have it
;;      generate the pkt keys directly.
;;  ii. Pass the pkt keys and values to this proc and go from there.
;; iii. Maybe have an abstraction alist with meaningful names for the pkt keys
;;
;; Override the run start time record with sched. Usually #f is fine.
;; 
(define (create-run-pkt mtconf action area runkey runname mode-patt tag-expr pktsdir reason contour sched dbdest append-conf runtrans)
  (let* ((good-val   (lambda (inval)(and inval (string? inval)(not (string-null? inval)))))
	 (area-dat   (val->alist (or (configf:lookup mtconf "areas" area) "")))
	 (area-path  (alist-ref 'path      area-dat))
	 (area-xlatr (alist-ref 'targtrans area-dat))
	 (new-runname (let* ((callname (if (string? runtrans)(string->symbol runtrans) #f))
			     (mapper   (if callname (hash-table-ref/default *runname-mappers* callname #f) #f)))
			;; (print "callname=" callname " runtrans=" runtrans " mapper=" mapper)
			(if (and callname
				 (not (equal? callname "auto"))
				 (not mapper))
			    (print "No mapper " callname " for area " area " using " callname " as the runname"))
			(if mapper
			    (handle-exceptions
				exn
				(begin
				  (print-call-chain)
				  (print "FAILED TO RUN RUNNAME MAPPER " callname " FOR AREA " area)
				  (print " message: " ((condition-property-accessor 'exn 'message) exn))
				  runname)
			      (print "(mapper " (string-intersperse (list runkey runname area area-path reason contour mode-patt) ", ") ")")
			      (mapper runkey runname area area-path reason contour mode-patt))
			    (case callname
			      ((auto) runname)
			      (else   runtrans)))))
	 (new-target (if area-xlatr 
			 (let ((xlatr-key (string->symbol area-xlatr)))
			   (if (hash-table-exists? *target-mappers* xlatr-key)
			       (begin
				 (print "Using target mapper: " area-xlatr)
				 (handle-exceptions
				     exn
				     (begin
				       (print "FAILED TO RUN TARGET MAPPER FOR " area ", called " area-xlatr)
				       (print "   function is: " (hash-table-ref/default *target-mappers* xlatr-key #f ) )
				       (print " message: " ((condition-property-accessor 'exn 'message) exn))
				       runkey)
				   ((hash-table-ref *target-mappers* xlatr-key)
				    runkey new-runname area area-path reason contour mode-patt)))
			       (begin
				 (print "ERROR: Failed to find named target translator " xlatr-key ", using original target.")
				 runkey)))
			 runkey)))
    ;; some hacks to remove switches not needed in certain cases
    (case (string->symbol (or action "run"))
      ((sync)
       (set! new-target #f)
       (set! runame     #f)))
    (print "area-path: " area-path " area-xlatr: " area-xlatr " orig-target: " runkey " new-target: " new-target)
    (let-values (((uuid pkt)
		  (command-line->pkt
		   (if action action "run")
		   (append 
		    `(("-start-dir"  . ,area-path)
		      ("-msg"        . ,reason)
		      ("-contour"    . ,contour))
		    (if (good-val new-runname) `(("-run-name"      . ,new-runname)) '())
		    (if (good-val new-target)  `(("-target"        . ,new-target))  '())
		    (if (good-val mode-patt)   `(("-mode-patt"     . ,mode-patt))   '())
		    (if (good-val tag-expr)    `(("-tag-expr"      . ,tag-expr))    '())
		    (if (good-val dbdest)      `(("-sync-to"       . ,dbdest))      '())
		    (if (good-val append-conf) `(("-append-config" . ,append-conf)) '())
		    (if (not (or mode-patt tag-expr))
			`(("-testpatt"  . "%"))
			'())
		    (if (or (not action)
			    (equal? action "run"))
			`(("-preclean"  . " ")
			  ("-rerun-all" . " "))      ;; if run we *always* want preclean set, use single space as placeholder
			'())
		    )
		   sched
                   extra-dat: `((a . ,runkey))  ;; we need the run key for marking the run as launched
                   )))
      (with-output-to-file
	  (conc pktsdir "/" uuid ".pkt")
	(lambda ()
	  (print pkt))))))


(define (val-alist->areas val-alist)
  (string-split (or (alist-ref 'areas val-alist) "") ","))

(define (area-allowed? area areas)
  (or (not areas)
      (null? areas)
      (member area areas)))

;; (use trace)(trace create-run-pkt)

;; collect all needed data and create run pkts for contours with changed inputs
;;
(define (generate-run-pkts mtconf toppath)
  (let ((std-runname (conc "sched"  (time->string (seconds->local-time (current-seconds)) "%M%H%d"))))
    (with-queue-db
     mtconf
     (lambda (pktsdirs pktsdir pdb)
       (let* ((rgconfdat (find-and-read-config (conc toppath "/runconfigs.config")))
	      (rgconf    (car rgconfdat))
	      (all-areas (map car (configf:get-section mtconf "areas")))
	      (contours  (configf:get-section mtconf "contours"))
	      (torun     (make-hash-table)) ;; target => ( ... info ... )
	      (rgentargs (hash-table-keys rgconf))) ;; these are the targets registered for automatically triggering

	 (print "rgentargs: " rgentargs)
	 
	 (for-each
	  (lambda (runkey)
	    (let* ((keydats   (configf:get-section rgconf runkey)))
	      (for-each
	       (lambda (sense) ;; these are the sense rules
		 (let* ((key        (car sense))
			(val        (cadr sense))
			(keyparts   (string-split key ":")) ;; contour:ruletype:action:optional
			(contour    (car keyparts))
			(len-key    (length keyparts))
			(ruletype   (if (> len-key 1)(cadr keyparts) #f))
			(action     (if (> len-key 2)(caddr keyparts) #f))
			(optional   (if (> len-key 3)(cadddr keyparts) #f))
			;; (val-list   (string-split-fields ";\\s*" val #:infix)) ;; (string-split val)) ;; runname-rule params
			(val-alist  (val->alist val))
			(runname    (make-runname "" ""))
			(runtrans   (alist-ref 'runtrans val-alist))
			
			(runstarts  (find-pkts pdb '(runstart) `((o . ,contour)
								 (t . ,runkey))))
			(rspkts     (get-pkt-alists runstarts))
			;; starttimes is for run start times and is used to know when the last run was launched
			(starttimes (get-pkt-times rspkts)) ;; sort by age (youngest first) and delete duplicates by target
			(last-run (if (null? starttimes) ;; if '() then it has never been run, else get the max
				      0
				      (apply max (map cdr starttimes))))
			;; synctimes is for figuring out the last time a sync was done
			(syncstarts   (find-pkts pdb '(syncstart) '())) ;; no qualifiers, a sync does all tarets etc.
			(sspkts       (get-pkt-alists syncstarts))
			(synctimes    (get-pkt-times  sspkts))
			(last-sync (if (null? synctimes) ;; if '() then it has never been run, else get the max
				      0
				      (apply max (map cdr synctimes))))
			)

		   (let ((delta (lambda (x)
				  (round (/ (- (current-seconds) x) 60)))))
		     (print "runkey: " runkey ", ruletype: " ruletype ", action: " action ", last-run: " last-run " time since; last-run: " (delta last-run) ", last-sync: " (delta last-sync)))

		   (print "val-alist=" val-alist " runtrans=" runtrans)
		   
		   ;; look in runstarts for matching runs by target and contour
		   ;; get the timestamp for when that run started and pass it
		   ;; to the rule logic here where "ruletype" will be applied
		   ;; if it comes back "changed" then proceed to register the runs
		   
		   (case (string->symbol (or ruletype "no-such-rule"))

		     ((no-such-rule) (print "ERROR: no such rule for " sense))

		     ((scheduled)
		      (if (not (alist-ref 'cron val-alist)) ;; gotta have cron spec
			  (print "ERROR: bad sense spec \"" (string-intersperse sense " ") "\" params: " val-alist)
			  (let* ((run-name (alist-ref 'run-name val-alist))
				 (target   (alist-ref 'target   val-alist))
				 (crontab  (alist-ref 'cron     val-alist))
                                 (areas    (val-alist->areas val-alist))
				 ;; (action   (alist-ref 'action   val-alist))
				 (cron-safe-string (string-translate (string-intersperse (string-split (alist-ref 'cron val-alist)) "-") "*" "X"))
				 (runname  std-runname)) ;; (conc "sched" (time->string (seconds->local-time (current-seconds)) "%M%H%d")))))
			    ;; (print "last-run: " last-run " need-run: " need-run)
			    ;; (if need-run
			    (case (string->symbol action)
			      ((sync)
			       (if (common:extended-cron crontab #f last-sync)
				   (push-run-spec torun contour runkey
						  `((message . ,(conc ruletype ":sync-" cron-safe-string))
						    (action  . ,action)
						    (dbdest  . ,(alist-ref 'dbdest val-alist))
						    (append  . ,(alist-ref 'appendconf val-alist))))))
			      ((run)
			       (if (common:extended-cron crontab #f last-run)
				   (push-run-spec torun contour runkey
						  `((message . ,(conc ruletype ":" cron-safe-string))
						    (runname . ,runname)
						    (runtrans . ,runtrans)
						    (action  . ,action)
						    (target  . ,target)))))
                              ((remove)
                               (push-run-spec torun contour runkey
						  `((message . ,(conc ruletype ":" cron-safe-string))
						    (runname . ,runname)
						    (runtrans . ,runtrans)
						    (action  . ,action)
						    (target  . ,target))))
			      (else
			       (print "ERROR: action \"" action "\" has no scheduled handler")
			       )))))

		     ((script)
		      ;; syntax is a little different here. It is a list of commands to run, "scriptname = extra_parameters;scriptname = ..."
		      ;; where scriptname may be repeated multiple times. The script must return unix-epoch of last change, new-target-name and new-run-name
		      ;; the script is called like this:  scriptname contour runkey std-runname action extra_param1 extra_param2 ...
		      (for-each
		       (lambda (cmd)
			 (print "cmd: " cmd)
			 (let* ((script (car cmd))
				(params (cdr cmd))
				(cmd    (conc script " " contour " " runkey " " std-runname " " action " " params))
				(res    (handle-exceptions
					    exn
					    #f
					  (print "Running " cmd)
					  (with-input-from-pipe cmd read-lines))))
			   (if (and res (not (null? res)))
			       (let* ((parts       (string-split (car res))) ;;
				      (rem-lines   (cdr res))
				      (num-parts   (length parts))
				      (last-change (string->number (if (> num-parts 0)(car parts) "abc")))  ;; force no run if not a number returned
				      (new-target  (if (> num-parts 1)
						       (cadr parts)
						       runkey))
				      (new-runname (if (> num-parts 2)
						       (caddr parts)
						       std-runname))
				      (message     (if (null? rem-lines)
						       cmd
						       (string-intersperse rem-lines "-")))
				      (need-run    (> last-change last-run)))
				 (print "last-run: " last-run " need-run: " need-run)
				 (if need-run
				     (let* ((key-msg    `((message  . ,(conc ruletype ":" message))
							  (runname  . ,runname)
							  (runtrans . ,runtrans)
							  (action   . ,action)
							  (target   . ,new-target))))
				       (print "key-msg: " key-msg)
				       (push-run-spec torun contour
						      (if optional  ;; we need to be able to differentiate same contour, different behavior. 
							  (conc runkey ":" optional)  ;; NOTE: NOT COMPLETELY IMPLEMENTED. DO NOT USE
							  runkey)
						      key-msg)))))))
		       val-alist)) ;; iterate over the param split by ;\s*

		     ((fossil)
		      (for-each
		       (lambda (fspec)
			 (print "fspec: " fspec)
			 (let* ((url         (symbol->string (car fspec))) ;; THIS COULD BE TROUBLE. Add option to reading line to return as string.
				(branch      (cdr fspec))
				(url-is-file (string-match "^(/|file:).*$" url))
				(fname       (conc (common:get-signature url) ".fossil"))
				(fdir        (conc "/tmp/" (current-user-name) "/mtutil_cache")))
			   ;; (if (not url-is-file) ;; need to sync first --- for now, clone 'em all.
			   (fossil:clone-or-sync url fname fdir) ;; )
			   (let-values (((datetime node)
					 (fossil:last-change-node-and-time fdir fname branch)))
			     (if (null? starttimes)
				 (push-run-spec torun contour runkey
						`((message . ,(conc "fossil:" branch "-neverrun"))
						  (runname . ,(conc runname "-" node))
						  (runtrans . ,runtrans)
						  (target  . ,runkey)))
				 (if (> datetime last-run) ;; change time is greater than last-run time
				     (push-run-spec torun contour runkey
						    `((message . ,(conc "fossil:" branch "-" node))
						      (runname . ,(conc runname "-" node))
						      (runtrans . ,runtrans)
						      (target  . ,runkey)))))
			     (print "Got datetime=" datetime " node=" node))))
		       val-alist))
		     
		     ((file file-or) ;; one or more files must be newer than the reference
		      (let* ((file-globs  (alist-ref 'glob val-alist))
                             (areas       (val-alist->areas val-alist))
			     (youngestdat (common:get-youngest (common:bash-glob file-globs)))
			     (youngestmod (car youngestdat)))
			;; (print "youngestmod: " youngestmod " starttimes: " starttimes)
			(if (null? starttimes) ;; this target has never been run
			    (push-run-spec torun contour runkey
					   `((message . "file:neverrun")
					     (action  . ,action)
					     (runtrans . ,runtrans)
					     (target  . ,runkey)
					     (runname . ,runname)))
			;; (for-each
			;;  (lambda (starttime) ;; look at the time the last run was kicked off for this contour
			;;    (if (> youngestmod (cdr starttime))
			;; 	   (begin
			;; 	     (print "starttime younger than youngestmod: " starttime " Youngestmod: " youngestmod)
			    (if (> youngestmod last-run)
				(push-run-spec torun contour runkey
					       `((message . ,(conc ruletype ":" (cadr youngestdat)))
						 (action  . ,action)
						 (target  . ,runkey)
						 (runtrans . ,runtrans)
						 (runname . ,runname)
						 ))))))
		      ;; starttimes))

		     ((file-and) ;; all files must be newer than the reference
		      (let* ((file-globs  (alist-ref 'glob val-alist))
			     (youngestdat (common:get-youngest file-globs))
			     (youngestmod (car youngestdat))
			     (success     #t)) ;; any cases of not true, set flag to #f for AND
			;; (print "youngestmod: " youngestmod " starttimes: " starttimes)
			(if (null? starttimes) ;; this target has never been run
			    (push-run-spec torun contour runkey
					   `((message . "file:neverrun")
					     (runname . ,runname)
					     (runtrans . ,runtrans)
					     (target  . ,runkey)
					     (action  . ,action)))
			    ;; NB// I think this is wrong. It should be looking at last-run only.
			    (if (> youngestmod last-run)
				
				;; 			    (for-each
				;; 			     (lambda (starttime) ;; look at the time the last run was kicked off for this contour
				;; 			       (if (< youngestmod (cdr starttime))
				;; 				   (set! success #f)))
				;; 			     starttimes))
				;; 			(if success
				;; 			    (begin
				;; 			      (print "starttime younger than youngestmod: " starttime " Youngestmod: " youngestmod)
				(push-run-spec torun contour runkey
					       `((message . ,(conc ruletype ":" (cadr youngestdat)))
						 (runname . ,runname)
						 (runtrans . ,runtrans)
						 (target  . ,runkey)
						 (action  . ,action)
						 ))))))
		     (else (print "ERROR: unrecognised rule \"" ruletype)))))
	       keydats))) ;; sense rules
	  (hash-table-keys rgconf))
	 
	 ;; now have to run populated
	 (for-each
	  (lambda (contour)
	    (print "contour: " contour)
	    (let* ((val       (or (configf:lookup mtconf "contours" contour) ""))
		   (val-alist (val->alist val))
		   (areas     (val-alist->areas val-alist))
		   (selector  (alist-ref 'selector val-alist))
		   (mode-tag  (and selector (string-split-fields "/" selector #:infix)))
		   (mode-patt (and mode-tag (if (eq? (length mode-tag) 2)(cadr mode-tag) #f)))
		   (tag-expr  (and mode-tag (if (null? mode-tag) #f (car mode-tag)))))
	      (for-each
	       (lambda (runkeydatset)
		 ;; (print "runkeydatset: ")(pp runkeydatset)
		 (let ((runkey     (car runkeydatset))
		       (runkeydats (cadr runkeydatset)))
		   (for-each
		    (lambda (runkeydat)
		      (for-each
		       (lambda (area)
                         (if (area-allowed? area areas) ;; is this area to be handled (from areas=a,b,c ...)
                             (let ((runname (alist-ref 'runname runkeydat))
                                   (runtrans (alist-ref 'runtrans runkeydat))
                                   (reason  (alist-ref 'message runkeydat))
                                   (sched   (alist-ref 'sched   runkeydat))
                                   (action  (alist-ref 'action  runkeydat))
                                   (dbdest  (alist-ref 'dbdest  runkeydat))
                                   (append  (alist-ref 'append  runkeydat))
                                   (target  (or (alist-ref 'target  runkeydat) runkey))) ;; override with target if forced
                               (print "Have: runkey=" runkey " contour=" contour " area=" area " action=" action " tag-expr=" tag-expr " mode-patt=" mode-patt " target=" target)
                               (if (case (or (and action (string->symbol action)) 'noaction)  ;; ensure we have the needed data to run this action
                                     ((noaction) #f)
                                     ((run)      (and runname reason))
                                     ((sync)     (and reason dbdest))
                                     (else       #f))
                                   ;; instead of unwrapping the runkeydat alist, pass it directly to create-run-pkt
                                   (create-run-pkt mtconf action area runkey runname mode-patt tag-expr pktsdir reason contour sched dbdest append runtrans) 
                                   (print "ERROR: Missing info to make a " action " call: runkey=" runkey " contour=" contour " area=" area  " tag-expr=" tag-expr " mode-patt=" mode-patt " dbdest=" dbdest)
                                   ))
                             (print "NOTE: skipping " runkeydat " for area, not in " areas)))
		       all-areas))
		    runkeydats)))
	       (let ((res (configf:get-section torun contour))) ;; each contour / target
		 ;; (print "res=" res)
		 res))))
	  (hash-table-keys torun)))))))

(define (pkt->cmdline pkta)
  (let ((action (or (lookup-action-by-key (alist-ref 'a pkta)) "noaction")))
    (fold (lambda (a res)
	    (let* ((key (car a)) ;; get the key name
		   (val (cdr a))
		   (par (or (lookup-param-by-key key)  ;; need to check also if it is a switch
			    (lookup-param-by-key key inlst: *switch-keys*))))
	      ;; (print "key: " key " val: " val " par: " par)
	      (if par
		  (conc res " " (param-translate par) " " val)
		  (if (member key '(a Z U D T)) ;; a is the action
		      res
		      (begin
			(print "ERROR: Unknown key in packet \"" key "\" with value \"" val "\"")
			res)))))
	  (conc "megatest " (if (not (member action '("sync")))
				(conc action " ")
				""))
	  pkta)))

;; (use trace)(trace pkt->cmdline)

(define (write-pkt pktsdir uuid pkt)
  (if pktsdir
      (with-output-to-file
	  (conc pktsdir "/" uuid ".pkt")
	(lambda ()
	  (print pkt)))
      (print "ERROR: cannot process commands without a pkts directory")))

;; collect all needed data and create run pkts for contours with changed inputs
;;
(define (dispatch-commands mtconf toppath)
  ;; we are expecting a directory "logs", check and create it, create the log in /tmp if not able to create logs dir
  (let ((logdir
	 (if (if (not (directory? "logs"))
		 (handle-exceptions
		     exn
		     #f
		   (create-directory "logs")
		   #t)
		 #t)
	     "logs"
	     "/tmp")))
    (with-queue-db
     mtconf
     (lambda (pktsdirs pktsdir pdb)
       (let* ((rgconfdat (find-and-read-config (conc toppath "/runconfigs.config")))
	      (rgconf    (car rgconfdat))
	      (areas     (configf:get-section mtconf "areas"))
	      (contours  (configf:get-section mtconf "contours"))
	      (pkts      (find-pkts pdb '(cmd) '()))
	      (torun     (make-hash-table)) ;; target => ( ... info ... )
	      (rgentargs (hash-table-keys rgconf))) ;; these are the targets registered for automatically triggering
	 (for-each
	  (lambda (pktdat)
	    (let* ((pkta    (alist-ref 'apkt pktdat))
		   (action  (alist-ref 'a pkta))
		   (cmdline (pkt->cmdline pkta))
		   (uuid    (alist-ref 'Z pkta))
		   (logf    (conc logdir "/" uuid "-run.log"))
		   (fullcmd (conc "NBFAKE_LOG=" logf " nbfake " cmdline)))
	      (print "RUNNING: " fullcmd)
	      (system fullcmd)
	      (mark-processed pdb (list (alist-ref 'id pktdat)))
	      (let-values (((ack-uuid ack-pkt)
			    (add-z-card
			     (construct-sdat 'P uuid
					     'T (case (string->symbol action)
						  ((run) "runstart")
						  ((sync) "syncstart")    ;; example of translating run -> runstart
						  (else   action))
					     'c (alist-ref 'o pkta) ;; THIS IS WRONG! SHOULD BE 'c
					     't (alist-ref 't pkta)))))
		(write-pkt pktsdir ack-uuid ack-pkt))))
	  pkts))))))
  
(define (get-pkts-dir mtconf)
  (let ((pktsdirs  (configf:lookup mtconf "setup" "pktsdirs"))
	(pktsdir   (if pktsdirs (car (string-split pktsdirs " ")) #f)))
    pktsdir))

(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.mtutilrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

(if *action*
    (case (string->symbol *action*)
      ((run remove rerun set-ss archive kill list)
       (let* ((mtconfdat (simple-setup (args:get-arg "-start-dir")))
	      (mtconf    (car mtconfdat))
	      (pktsdirs  (configf:lookup mtconf "setup" "pktsdirs"))
	      (pktsdir   (if pktsdirs (car (string-split pktsdirs " ")) #f))
	      (adjargs   (hash-table-copy args:arg-hash)))
	 ;; (for-each
	 ;;  (lambda (key)
	 ;;    (if (not (member key *legal-params*))
	 ;; 	(hash-table-delete! adjargs key))) ;; we need to delete any params intended for mtutil
	 ;;  (hash-table-keys adjargs))
	 (let-values (((uuid pkt)
		       (command-line->pkt *action* adjargs #f)))
	   (write-pkt pktsdir uuid pkt))))
      ((dispatch import rungen process)
       (let* ((mtconfdat (simple-setup (args:get-arg "-start-dir")))
	      (mtconf    (car mtconfdat))
	      (toppath   (configf:lookup mtconf "dyndat" "toppath")))
	 (case (string->symbol *action*)
	   ((process)  (begin
			 (load-pkts-to-db mtconf)
			 (generate-run-pkts mtconf toppath)
			 (load-pkts-to-db mtconf)
			 (dispatch-commands mtconf toppath)))
	   ((import)   (load-pkts-to-db mtconf)) ;; import pkts
	   ((rungen)   (generate-run-pkts mtconf toppath))
	   ((dispatch) (dispatch-commands mtconf toppath)))))
      ;; misc
      ((show)
       (if (> (length remargs) 0)
	   (let* ((mtconfdat (simple-setup (args:get-arg "-start-dir")))
		  (mtconf    (car mtconfdat))
		  (sect-dat (configf:get-section mtconf (car remargs))))
	     (if sect-dat
		 (for-each
		  (lambda (entry)
		    (if (> (length entry) 1)
			(print (car entry) "   " (cadr entry))
			(print (car entry))))
		  sect-dat)
		 (print "No section \"" (car remargs) "\" found")))
	   (print "ERROR: list requires section parameter; areas, setup or contours")))
      ((gendot)
       (let* ((mtconfdat (simple-setup (args:get-arg "-start-dir")))
	      (mtconf    (car mtconfdat)))
	 (with-queue-db
	  mtconf
	  (lambda (pktsdirs pktsdir conn)
	    (make-report "out.dot" conn (make-hash-table))))))
      ((db)
       (if (null? remargs)
	   (print "ERROR: missing sub command for db command")
	   (let ((subcmd (car remargs)))
	     (case (string->symbol subcmd)
	       ((pgschema)
		(let* ((install-home (common:get-install-area))
		       (schema-file  (conc install-home "/share/db/mt-pg.sql")))
		  (if (file-exists? schema-file)
		      (system (conc "/bin/cat " schema-file)))))
	       ((sqlite3schema)
		(let* ((install-home (common:get-install-area))
		       (schema-file  (conc install-home "/share/db/mt-sqlite3.sql")))
		  (if (file-exists? schema-file)
		      (system (conc "/bin/cat " schema-file)))))
	       ((junk)
		(rmt:get-keys))))))))

;; If HTTP_HOST is defined then we must be in the cgi environment
;; so run stml and exit
;;
(if (get-environment-variable "HTTP_HOST")
    (begin
      (stml:main #f)
      (exit)))

(if (or (args:get-arg "-repl")
	(args:get-arg "-load"))
    (begin
      (import extras) ;; might not be needed
      ;; (import csi)
      (import readline)
      (import apropos)
      ;; (import (prefix sqlite3 sqlite3:)) ;; doesn't work ...
      
      (install-history-file (get-environment-variable "HOME") ".mtutil_history") ;;  [homedir] [filename] [nlines])
      (current-input-port (make-readline-port "mtutil> "))
      (if (args:get-arg "-repl")
	  (repl)
	  (load (args:get-arg "-load")))))
