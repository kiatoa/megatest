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
     srfi-18 extras format pkts regex
     (prefix dbi dbi:)) ;;  zmq extras)

(declare (uses common))
(declare (uses megatest-version))
(declare (uses margs))
(declare (uses configf))
;; (declare (uses rmt))

(include "megatest-fossil-hash.scm")

(require-library stml)

(let ((debugcontrolf (conc (get-environment-variable "HOME") "/.mtutilrc")))
  (if (file-exists? debugcontrolf)
      (load debugcontrolf)))

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
  -h                       : this help
  -manual                  : show the Megatest user manual
  -version                 : print megatest version (currently " megatest-version ")

Actions:
   run                     : initiate runs
   remove                  : remove runs
   rerun                   : register action for processing
   set-ss                  : set state/status
   archive                 : compress and move test data to archive disk
   kill                    : stop tests or entire runs
   db                      : database utilities

Contour actions:
   process                 : runs import, rungen and dispatch 

Selectors 
  -immediate               : apply this action immediately, default is to queue up actions
  -area areapatt1,area2... : apply this action only to the specified areas
  -target key1/key2/...    : run for key1, key2, etc.
  -test-patt p1/p2,p3/...  : % is wildcard
  -run-name                : required, name for this particular test run
  -contour contourname     : run all targets for contourname, requires -run-name, -target
  -state-status c/p,c/f    : Specify a list of state and status patterns
  -tag-expr tag1,tag2%,..  : select tests with tags matching expression
  -mode-patt key           : load testpatt from <key> in runconfigs instead of default TESTPATT
                             if -testpatt and -tagexpr are not specified
  -new state/status        : specify new state/status for set-ss

Misc 
  -start-dir path          : switch to this directory before running mtutil
  -set-vars V1=1,V2=2      : Add environment variables to a run NB// these are
                                 overwritten by values set in config files.
  -log logfile             : send stdout and stderr to logfile
  -repl                    : start a repl (useful for extending megatest)
  -load file.scm           : load and run file.scm
  -debug N|N,M,O...        : enable debug messages 0-N or N and M and O ...

Utility
 db pgschema               : emit postgresql schema; do \"mtutil db pgschema | psql -d mydb\"

Examples:

# Start a megatest run in the area \"mytests\"
mtutil -area mytests -action run -target v1.63/aa3e -mode-patt MYPATT -tag-expr quick

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
    ))

(define (lookup-param-by-key key #!key (inlst #f))
  (fold (lambda (a res)
	  (if (eq? (cdr a) key)
	      (car a)
	      res))
	#f
	(or inlst *arg-keys*)))

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
			      (apkt   (convert-pkt->alist pktdat))
			      (ptype  (alist-ref 'T apkt)))
			 (add-to-queue pdb pktdat uuid (or ptype 'cmd) #f 0)
			 (debug:print 4 *default-log-port* "Added " uuid " of type " ptype " to queue"))
		       (debug:print 4 *default-log-port* "pkt: " uuid " exists, skipping...")
		       )))
	       pkts))))
      (string-split pktsdirs)))))

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
(define (command-line->pkt action args-alist sched-in)
  (let* ((sched     (cond
		     ((vector? sched-in)(local-time->seconds sched-in)) ;; we recieved a time
		     ((number? sched-in) sched-in)
		     (else     (current-seconds))))
	 (args-data (if args-alist
			args-alist
			(hash-table->alist args:arg-hash)))
	 (alldat    (apply append (list 'a action
					'U (current-user-name)
					'D sched)
			   (map (lambda (x)
				  (let* ((param (car x))
					 (value (cdr x))
					 (pmeta (assoc param *arg-keys*))
					 (smeta (assoc param *switch-keys*))
					 (meta  (if (or pmeta smeta)
						    (cdr (or pmeta smeta))
						    #f)))
				    (if (or pmeta smeta)
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


;; make a run request pkt from basic data
;;
(define (create-run-pkt mtconf area runkey runname mode-patt tag-expr pktsdir reason contour sched) 
  (let* ((area-dat   (string-split (or (configf:lookup mtconf "areas" area) "")))
	 (area-path  (car area-dat))
	 (area-xlatr (if (eq? (length area-dat) 2)(cadr area-dat) #f))
	 (new-target (if area-xlatr
			 (let ((xlatr-key (string->symbol area-xlatr)))
			   (if (alist-ref xlatr-key *target-mappers*)
			       (begin
				 (print "Using target mapper: " area-xlatr)
				 (handle-exceptions
				     exn
				     (begin
				       (print "FAILED TO RUN TARGET MAPPER FOR " area ", called " area-xlatr)
				       (print "   function is: " (alist-ref xlatr-key *target-mappers*))
				       (print " message: " ((condition-property-accessor 'exn 'message) exn))
				       runkey)
				   ((alist-ref xlatr-key *target-mappers*)
				    runkey runname area area-path reason contour mode-patt)))))
			 runkey)))
    (let-values (((uuid pkt)
		  (command-line->pkt
		   "run"
		   (append 
		    `(("-target"     . ,new-target)
		      ("-run-name"   . ,runname)
		      ("-start-dir"  . ,area-path)
		      ("-msg"        . ,reason)
		      ("-contour"    . ,contour))
		    (if mode-patt
			`(("-mode-patt"  . ,mode-patt))
			'())
		    (if tag-expr
			`(("-tag-expr"   . ,tag-expr))
			'())
		    (if (not (or mode-patt tag-expr))
			`(("-item-patt"  . "%"))
			'()))
		   sched)))
      (with-output-to-file
	  (conc pktsdir "/" uuid ".pkt")
	(lambda ()
	  (print pkt))))))

;; collect all needed data and create run pkts for contours with changed inputs
;;
(define (generate-run-pkts mtconf toppath)
  (with-queue-db
   mtconf
   (lambda (pktsdirs pktsdir pdb)
     (let* ((rgconfdat (find-and-read-config (conc toppath "/runconfigs.config")))
	    (rgconf    (car rgconfdat))
	    (areas     (map car (configf:get-section mtconf "areas")))
	    (contours  (configf:get-section mtconf "contours"))
	    (torun     (make-hash-table)) ;; target => ( ... info ... )
	    (rgentargs (hash-table-keys rgconf))) ;; these are the targets registered for automatically triggering
       
       (for-each
	(lambda (runkey)
	  (let* ((keydats   (configf:get-section rgconf runkey)))
	    (for-each
	     (lambda (sense) ;; these are the sense rules
	       (let* ((key        (car sense))
		      (val        (cadr sense))
		      (keyparts   (string-split key ":"))
		      (contour    (car keyparts))
		      (len-key    (length keyparts))
		      (ruletype   (if (> len-key 1)(cadr keyparts) #f))
		      (action     (if (> len-key 2)(caddr keyparts) #f))
		      (val-list   (string-split-fields ";\\s*" val #:infix)) ;; (string-split val)) ;; runname-rule params
		      (val-alist  (if val-list
				      (map (lambda (x)
					     (let ((f (string-split-fields "\\s*=\\s*" x #:infix)))
					       (case (length f)
						 ((0) `(,#f))  ;; null string case
						 ((1) `(,(string->symbol (car f))))
						 ((2) `(,(string->symbol (car f)) . ,(cadr f)))
						 (else f))))
					   val-list)
				      '()))
		      (runname    (make-runname "" ""))
		      (runstarts  (find-pkts pdb '(runstart) `((o . ,contour)
							       (t . ,runkey))))
		      (rspkts     (map (lambda (x)
					 (alist-ref 'pkta x))
				       runstarts))
		      (starttimes ;; sort by age (youngest first) and delete duplicates by target
		       (delete-duplicates
			(sort 
			 (map (lambda (x)
				`(,(alist-ref 't x) . ,(string->number (alist-ref 'D x))))
			      rspkts)
			 (lambda (a b)(> (cdr a)(cdr b))))      ;; sort descending
			(lambda (a b)(equal? (car a)(car b))))) ;; remove duplicates by target
		      )
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
			       (crontab  (alist-ref 'cron     val-alist))
			       (action   (alist-ref 'action   val-alist))
			       (last-run (if (null? starttimes) ;; never run
					     0
					     (apply max (map cdr starttimes))))
			       (need-run (common:cron-event crontab #f last-run))
			       (runname  (if need-run (conc "sched" (time->string (seconds->local-time (current-seconds)) "%M%H%d")))))
			  (print "last-run: " last-run " need-run: " need-run)
			  (if need-run
			      (configf:section-var-set! torun contour runkey `(,(conc ruletype ":" (string-intersperse (string-split (alist-ref 'cron val-alist)) "-"))
									       ,runname ,need-run ,action))))))
		   ((file file-or) ;; one or more files must be newer than the reference
		    (let* ((file-globs  (alist-ref 'glob val-alist))
			   (youngestdat (common:get-youngest (common:bash-glob file-globs)))
			   (youngestmod (car youngestdat)))
		      ;; (print "youngestmod: " youngestmod " starttimes: " starttimes)
		      (if (null? starttimes) ;; this target has never been run
			  (configf:section-var-set! torun contour runkey `("file:neverrun" ,runname))
			  (for-each
			   (lambda (starttime) ;; look at the time the last run was kicked off for this contour
			     (if (> youngestmod (cdr starttime))
				 (begin
				   (print "starttime younger than youngestmod: " starttime " Youngestmod: " youngestmod)
				   (configf:section-var-set! torun contour runkey `(,(conc ruletype ":" (cadr youngestdat)) ,runname #f)))))
			   starttimes))
		      ))
		   ((file-and) ;; all files must be newer than the reference
		    (let* ((file-globs  (alist-ref 'glob val-alist))
			   (youngestdat (common:get-youngest file-globs))
			   (youngestmod (car youngestdat))
			   (success     #t)) ;; any cases of not true, set flag to #f for AND
		      ;; (print "youngestmod: " youngestmod " starttimes: " starttimes)
		      (if (null? starttimes) ;; this target has never been run
			  (configf:section-var-set! torun contour runkey `("file:neverrun" ,runname #f))
			  (for-each
			   (lambda (starttime) ;; look at the time the last run was kicked off for this contour
			     (if (< youngestmod (cdr starttime))
				 (set! success #f)))
			   starttimes))
		      (if success
			  (begin
			    (print "starttime younger than youngestmod: " starttime " Youngestmod: " youngestmod)
			    (configf:section-var-set! torun contour runkey `(,(conc ruletype ":" (cadr youngestdat)) ,runname #f))))))
		   )))
	     keydats)))
	(hash-table-keys rgconf))
       
       ;; now have to run populated
       (for-each
	(lambda (contour)
	  (let* ((mode-tag  (string-split (or (configf:lookup mtconf "contours" contour) "") "/"))
		 (mode-patt (if (eq? (length mode-tag) 2)(cadr mode-tag) #f))
		 (tag-expr  (if (null? mode-tag) #f (car mode-tag))))
	    (for-each
	     (lambda (runkeydat)
	       (let* ((runkey (car runkeydat))
		      (info   (cadr runkeydat)))
		 (for-each
		  (lambda (area)
		    (if (< (length info) 3)
			(print "ERROR: bad info data for " contour ", " runkey ", " area)
			(let ((runname (cadr info))
			      (reason  (car  info))
			      (sched   (caddr info)))
			  (print "runkey: " runkey " contour: " contour " info: " info " area: " area  " tag-expr: " tag-expr " mode-patt: " mode-patt)
			  (create-run-pkt mtconf area runkey runname mode-patt tag-expr pktsdir reason contour sched))))
		  areas)))
	     (configf:get-section torun contour))))
	(hash-table-keys torun))))))


(define (pkt->cmdline pkta)
  (fold (lambda (a res)
	  (let* ((key (car a)) ;; get the key name
		 (val (cdr a))
		 (par (lookup-param-by-key key)))
	    ;; (print "key: " key " val: " val " par: " par)
	    (if par
		(conc res " " (param-translate par) " " val)
		res)))
	"megatest -run"
	pkta))

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
	  (let* ((pkta    (alist-ref 'pkta pktdat))
		 (cmdline (pkt->cmdline pkta))
		 (uuid    (alist-ref 'Z pkta))
		 (logf    (conc "logs/" uuid "-run.log")))
	    (system (conc "NBFAKE_LOG=" logf " nbfake " cmdline))
	    (mark-processed pdb (list (alist-ref 'id pktdat)))
	    (let-values (((ack-uuid ack-pkt)
			  (add-z-card
			   (construct-sdat 'P uuid
					   'T "runstart"
					   'c (alist-ref 'o pkta) ;; THIS IS WRONG! SHOULD BE 'c
					   't (alist-ref 't pkta)))))
	      (write-pkt pktsdir ack-uuid ack-pkt))))
	pkts)))))

(define (get-pkts-dir mtconf)
  (let ((pktsdirs  (configf:lookup mtconf "setup" "pktsdirs"))
	(pktsdir   (if pktsdirs (car (string-split pktsdirs " ")) #f)))
    pktsdir))

(if *action*
    (case (string->symbol *action*)
      ((run remove rerun set-ss archive kill)
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
