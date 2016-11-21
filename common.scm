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

(use srfi-1 posix regex-case base64 format dot-locking csv-xml z3 sql-de-lite hostinfo typed-records)
(require-extension regex posix)

(require-extension (srfi 18) extras tcp rpc)

(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (unit common))

(include "common_records.scm")

;; (require-library margs)
;; (include "margs.scm")

;; (define old-exit exit)
;; 
;; (define (exit . code)
;;   (if (null? code)
;;       (old-exit)
;;       (old-exit code)))

(define getenv get-environment-variable)
(define (safe-setenv key val)
  (if (and (string? val)(string? key))
      (handle-exceptions
       exn
       (debug:print-error 0 *default-log-port* "bad value for setenv, key=" key ", value=" val)
       (setenv key val))
      (debug:print-error 0 *default-log-port* "bad value for setenv, key=" key ", value=" val)))

(define home (getenv "HOME"))
(define user (getenv "USER"))

;; GLOBAL GLETCHES

(define *contexts* (make-hash-table))

;; Common data structure for 
(defstruct cxt
  (taskdb #f)
  (cmutex (make-mutex)))

;; safe method for accessing a context given a toppath
;;
(define (common:with-cxt toppath proc)
  (mutex-lock! *context-mutex*)
  (let ((cxt (hash-table-ref/default *contexts* toppath #f)))
    (if (not cxt)
        (set! cxt (let ((x (make-cxt)))(hash-table-set! *contexts* toppath x) x)))
    (let ((cxt-mutex (cxt-mutex cxt)))
      (mutex-unlock! *context-mutex*)
      (mutex-lock! cxt-mutex)
      (let ((res (proc cxt)))
        (mutex-unlock! cxt-mutex)
        res))))
        
(define *db-keys* #f)

(define *configinfo*   #f)   ;; raw results from setup, includes toppath and table from megatest.config
(define *runconfigdat* #f)   ;; run configs data
(define *configdat*    #f)   ;; megatest.config data
(define *configstatus* #f)   ;; status of data; 'fulldata : all processing done, #f : no data yet, 'partialdata : partial read done
(define *toppath*      #f)
(define *already-seen-runconfig-info* #f)

(define *waiting-queue*     (make-hash-table))
(define *test-meta-updated* (make-hash-table))
(define *globalexitstatus*  0) ;; attempt to work around possible thread issues
(define *passnum*           0) ;; when running track calls to run-tests or similar
(define *write-frequency*   (make-hash-table)) ;; run-id => (vector (current-seconds) 0))
(define *alt-log-file* #f)  ;; used by -log
(define *common:denoise*    (make-hash-table)) ;; for low noise printing
(define *default-log-port*  (current-error-port))

;; DATABASE
(define *dbstruct-db*  #f)
(define *db-stats*            (make-hash-table)) ;; hash of vectors < count duration-total >
(define *db-stats-mutex*      (make-mutex))
(define *db-sync-mutex*       (make-mutex))
(define *db-multi-sync-mutex* (make-mutex))
(define *db-local-sync*       (make-hash-table)) ;; used to record last touch of db
(define *megatest-db*         #f)
(define *last-db-access*      (current-seconds))  ;; update when db is accessed via server
(define *db-write-access*     #t)
(define *inmemdb*             #f)
(define *task-db*             #f) ;; (vector db path-to-db)
(define *db-access-allowed*   #t) ;; flag to allow access
(define *db-access-mutex*     (make-mutex))

;; SERVER
(define *my-client-signature* #f)
(define *transport-type*    'http)
(define *transport-type*    'http)             ;; override with [server] transport http|rpc|nmsg
(define *runremote*         (make-hash-table)) ;; if set up for server communication this will hold <host port>
(define *max-cache-size*    0)
(define *logged-in-clients* (make-hash-table))
(define *client-non-blocking-mode* #f)
(define *server-id*         #f)
(define *server-info*       #f)
(define *time-to-exit*      #f)
(define *received-response* #f)
(define *default-numtries*  10)
(define *server-run*        #t)
(define *run-id*            #f)
(define *server-kind-run*   (make-hash-table))

(define *target*            (make-hash-table)) ;; cache the target here; target is keyval1/keyval2/.../keyvalN
(define *keys*              (make-hash-table)) ;; cache the keys here
(define *keyvals*           (make-hash-table))
(define *toptest-paths*     (make-hash-table)) ;; cache toptest path settings here
(define *test-paths*        (make-hash-table)) ;; cache test-id to test run paths here
(define *test-ids*          (make-hash-table)) ;; cache run-id, testname, and item-path => test-id
(define *test-info*         (make-hash-table)) ;; cache the test info records, update the state, status, run_duration etc. from testdat.db

(define *run-info-cache*    (make-hash-table)) ;; run info is stable, no need to reget

;; Awful. Please FIXME
(define *env-vars-by-run-id* (make-hash-table))
(define *current-run-name*   #f)

;; Testconfig and runconfig caches. 
(define *testconfigs*       (make-hash-table)) ;; test-name => testconfig
(define *runconfigs*        (make-hash-table)) ;; target    => runconfig

;; This is a cache of pre-reqs met, don't re-calc in cases where called with same params less than
;; five seconds ago
(define *pre-reqs-met-cache* (make-hash-table))

;; cache of verbosity given string
;;
(define *verbosity-cache* (make-hash-table))

(define (common:clear-caches)
  (set! *target*             (make-hash-table))
  (set! *keys*               (make-hash-table))
  (set! *keyvals*            (make-hash-table))
  (set! *toptest-paths*      (make-hash-table))
  (set! *test-paths*         (make-hash-table))
  (set! *test-ids*           (make-hash-table))
  (set! *test-info*          (make-hash-table))
  (set! *run-info-cache*     (make-hash-table))
  (set! *env-vars-by-run-id* (make-hash-table))
  (set! *test-id-cache*      (make-hash-table)))

;; Generic string database
(define sdb:qry #f) ;; (make-sdb:qry)) ;;  'init #f)
;; Generic path database
(define *fdb* #f)

(define *last-launch* (current-seconds)) ;; use for throttling the launch rate. Would be better to use the db and last time of a test in LAUNCHED state.

;;======================================================================
;; V E R S I O N
;;======================================================================

(define (common:get-full-version)
  (conc megatest-version "-" megatest-fossil-hash))

(define (common:version-signature)
  (conc megatest-version "-" (substring megatest-fossil-hash 0 4)))

;; from metadat lookup MEGATEST_VERSION
;;
(define (common:get-last-run-version) ;; RADT => How does this work in send-receive function??; assume it is the value saved in some DB
  (rmt:get-var "MEGATEST_VERSION"))

(define (common:get-last-run-version-number)
  (string->number 
   (substring (common:get-last-run-version) 0 6)))

(define (common:set-last-run-version)
  (rmt:set-var "MEGATEST_VERSION" (common:version-signature)))

(define (common:version-changed?)
  (not (equal? (common:get-last-run-version)
	       (common:version-signature))))

;; Move me elsewhere ...
;; RADT => Why do we meed the version check here, this is called only if version misma
;;
(define (common:cleanup-db)
  (db:multi-db-sync 
   #f ;; do all run-ids
   ;; 'new2old
   'killservers
   'dejunk
   ;; 'adj-testids
   ;; 'old2new
   'new2old)
  (if (common:version-changed?)
      (common:set-last-run-version)))

;; Force a megatest cleanup-db if version is changed and skip-version-check not specified
;;
(define (common:exit-on-version-changed)
  (if (common:version-changed?)
      (let ((mtconf (conc (get-environment-variable "MT_RUN_AREA_HOME") "/megatest.config")))
        (debug:print 0 *default-log-port*
		     "WARNING: Version mismatch!\n"
		     "   expected: " (common:version-signature) "\n"
		     "   got:      " (common:get-last-run-version))
	(if (and (file-exists? mtconf)
		 (eq? (current-user-id)(file-owner mtconf))) ;; safe to run -cleanup-db
	    (begin
	      (debug:print 0 *default-log-port* "   I see you are the owner of megatest.config, attempting to cleanup and reset to new version")
	      (handle-exceptions
	       exn
	       (begin
		 (debug:print 0 *default-log-port* "Failed to switch versions.")
		 (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
		 (print-call-chain (current-error-port))
		 (exit 1))
	       (common:cleanup-db)))
	    (begin
	      (debug:print 0 *default-log-port* " to switch versions you can run: \"megatest -cleanup-db\"")
	      (exit 1))))))

;;======================================================================
;; S P A R S E   A R R A Y S
;;======================================================================

(define (make-sparse-array)
  (let ((a (make-sparse-vector)))
    (sparse-vector-set! a 0 (make-sparse-vector))
    a))

(define (sparse-array? a)
  (and (sparse-vector? a)
       (sparse-vector? (sparse-vector-ref a 0))))

(define (sparse-array-ref a x y)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-ref row y)
	#f)))

(define (sparse-array-set! a x y val)
  (let ((row (sparse-vector-ref a x)))
    (if row
	(sparse-vector-set! row y val)
	(let ((new-row (make-sparse-vector)))
	  (sparse-vector-set! a x new-row)
	  (sparse-vector-set! new-row y val)))))

;;======================================================================
;; L O C K E R S   A N D   B L O C K E R S 
;;======================================================================

;; block further accesses to databases. Call this before shutting db down
(define (common:db-block-further-queries)
  (mutex-lock! *db-access-mutex*)
  (set! *db-access-allowed* #f)
  (mutex-unlock! *db-access-mutex*))

(define (common:db-access-allowed?)
  (let ((val (begin
	       (mutex-lock! *db-access-mutex*)
	       *db-access-allowed*
	       (mutex-unlock! *db-access-mutex*))))
    val))

;;======================================================================
;; U S E F U L   S T U F F
;;======================================================================

;; convert things to an alist or assoc list, #f gets converted to ""
;;
(define (common:to-alist dat)
  (cond
   ((list? dat)   (map common:to-alist dat))
   ((vector? dat)
    (map common:to-alist (vector->list dat)))
   ((pair? dat)
    (cons (common:to-alist (car dat))
	  (common:to-alist (cdr dat))))
   ((hash-table? dat)
    (map common:to-alist (hash-table->alist dat)))
   (else
    (if dat
	dat
	""))))

(define (common:low-noise-print waitval . keys)
  (let* ((key      (string-intersperse (map conc keys) "-" ))
	 (lasttime (hash-table-ref/default *common:denoise* key 0))
	 (currtime (current-seconds)))
    (if (> (- currtime lasttime) waitval)
	(begin
	  (hash-table-set! *common:denoise* key currtime)
	  #t)
	#f)))

(define (common:get-megatest-exe)
  (or (getenv "MT_MEGATEST") "megatest"))

(define (common:read-encoded-string instr)
  (handle-exceptions
   exn
   (handle-exceptions
    exn
    (begin
      (debug:print-error 0 *default-log-port* "received bad encoded string \"" instr "\", message: " ((condition-property-accessor 'exn 'message) exn))
      (print-call-chain (current-error-port))
      #f)
    (read (open-input-string (base64:base64-decode instr))))
   (read (open-input-string (z3:decode-buffer (base64:base64-decode instr))))))

;; dot-locking egg seems not to work, using this for now
;; if lock is older than expire-time then remove it and try again
;; to get the lock
;;
(define (common:simple-file-lock fname #!key (expire-time 300))
  (if (file-exists? fname)
      (if (> (- (current-seconds)(file-modification-time fname)) expire-time)
	  (begin
	    (delete-file* fname)
	    (common:simple-file-lock fname expire-time: expire-time))
	  #f)
      (let ((key-string (conc (get-host-name) "-" (current-process-id))))
	(with-output-to-file fname
	  (lambda ()
	    (print key-string)))
	(thread-sleep! 0.25)
	(if (file-exists? fname)
	    (with-input-from-file fname
	      (lambda ()
		(equal? key-string (read-line))))
	    #f))))
	
(define (common:simple-file-release-lock fname)
  (delete-file* fname))

;;======================================================================
;; S T A T E S   A N D   S T A T U S E S
;;======================================================================

(define *common:std-states*   
  '((0 "COMPLETED")
    (1 "NOT_STARTED")
    (2 "RUNNING")
    (3 "REMOTEHOSTSTART")
    (4 "LAUNCHED")
    (5 "KILLED")
    (6 "KILLREQ")
    (7 "STUCK")
    (8 "ARCHIVED")))

(define *common:std-statuses*
  '((0 "PASS")
    (1 "WARN")
    (2 "FAIL")
    (3 "CHECK")
    (4 "n/a")
    (5 "WAIVED")
    (6 "SKIP")
    (7 "DELETED")
    (8 "STUCK/DEAD")
    (9 "ABORT")))

;; These are stopping conditions that prevent a test from being run
(define *common:cant-run-states-sym* 
  '(COMPLETED KILLED WAIVED UNKNOWN INCOMPLETE ABORT ARCHIVED))

;;======================================================================
;; D E B U G G I N G   S T U F F 
;;======================================================================

(define *verbosity*         1)
(define *logging*           #f)

(define (get-with-default val default)
  (let ((val (args:get-arg val)))
    (if val val default)))

(define (assoc/default key lst . default)
  (let ((res (assoc key lst)))
    (if res (cadr res)(if (null? default) #f (car default)))))

(define (common:get-testsuite-name)
  (or (configf:lookup *configdat* "setup" "testsuite" )
      (if *toppath* 
          (pathname-file *toppath*)
          (pathname-file (current-directory)))))

(define (common:get-db-tmp-area)
  (create-directory (conc "/tmp/" (current-user-name)
                          "/megatest/"
                          (common:get-testsuite-name) "/"
                          (string-translate *toppath* "/" "_")) #t))

;;======================================================================
;; E X I T   H A N D L I N G
;;======================================================================

(define (common:legacy-sync-recommended)
  (or (args:get-arg "-runtests")
      (args:get-arg "-run")
      (args:get-arg "-server")
      ;; (args:get-arg "-set-run-status")
      (args:get-arg "-remove-runs")
      ;; (args:get-arg "-get-run-status")
      ))

(define (common:legacy-sync-required)
  (configf:lookup *configdat* "setup" "megatest-db"))

(define (std-exit-procedure)
  (let ((no-hurry  (if *time-to-exit* ;; hurry up
		       #f
		       (begin
			 (set! *time-to-exit* #t)
			 #t))))
    (debug:print-info 4 *default-log-port* "starting exit process, finalizing databases.")
    (if (and no-hurry (debug:debug-mode 18))
	(rmt:print-db-stats))
    (let ((th1 (make-thread (lambda () ;; thread for cleaning up, give it five seconds
			      (let ((run-ids (hash-table-keys *db-local-sync*)))
				(if (and (not (null? run-ids))
					 (or (common:legacy-sync-recommended)
					     (configf:lookup *configdat* "setup" "megatest-db")))
				    (if no-hurry (db:multi-db-sync run-ids 'new2old))))
			      (if *dbstruct-db* (db:close-all *dbstruct-db*))
			      (if *inmemdb*     (db:close-all *inmemdb*))
			      (if (and *megatest-db*
				       (sqlite3:database? *megatest-db*))
				  (begin
				    (sqlite3:interrupt! *megatest-db*)
				    (sqlite3:finalize! *megatest-db* #t)
				    (set! *megatest-db* #f)))
			      (if *task-db*    
				  (let ((db (cdr *task-db*)))
				    (if (sqlite3:database? db)
					(begin
					  (sqlite3:interrupt! db)
					  (sqlite3:finalize! db #t)
					  (vector-set! *task-db* 0 #f)))))
			      (close-output-port *default-log-port*)
			      (set! *default-log-port* (current-error-port))) "Cleanup db exit thread"))
	  (th2 (make-thread (lambda ()
			      (debug:print 4 *default-log-port* "Attempting clean exit. Please be patient and wait a few seconds...")
			      (if no-hurry
				  (thread-sleep! 5) ;; give the clean up few seconds to do it's stuff
				  (thread-sleep! 2))
			      (debug:print 4 *default-log-port* " ... done")
			      )
			    "clean exit")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1))))

(define (std-signal-handler signum)
  ;; (signal-mask! signum)
  (set! *time-to-exit* #t)
  (debug:print-error 0 *default-log-port* "Received signal " signum " exiting promptly")
  ;; (std-exit-procedure) ;; shouldn't need this since we are exiting and it will be called anyway
  (exit))

(set-signal-handler! signal/int  std-signal-handler)  ;; ^C
(set-signal-handler! signal/term std-signal-handler)
;; (set-signal-handler! signal/stop std-signal-handler)  ;; ^Z NO, do NOT handle ^Z!

;;======================================================================
;; M I S C   U T I L S
;;======================================================================

;; one-of args defined
(define (args-defined? . param)
  (let ((res #f))
    (for-each 
     (lambda (arg)
       (if (args:get-arg arg)(set! res #t)))
     param)
    res))

;; convert stuff to a number if possible
(define (any->number val)
  (cond 
   ((number? val) val)
   ((string? val) (string->number val))
   ((symbol? val) (any->number (symbol->string val)))
   (else #f)))

(define (any->number-if-possible val)
  (let ((num (any->number val)))
    (if num num val)))

(define (patt-list-match item patts)
  (debug:print-info 8 *default-log-port* "patt-list-match item=" item " patts=" patts)
  (if (and item patts)  ;; here we are filtering for matches with item patterns
      (let ((res #f))   ;; look through all the item-patts if defined, format is patt1,patt2,patt3 ... wildcard is %
	(for-each 
	 (lambda (patt)
	   (let ((modpatt (string-substitute "%" ".*" patt #t)))
	     (debug:print-info 10 *default-log-port* "patt " patt " modpatt " modpatt)
	     (if (string-match (regexp modpatt) item)
		 (set! res #t))))
	 (string-split patts ","))
	res)
      #t))

;; (map print (map car (hash-table->alist (read-config "runconfigs.config" #f #t))))
(define (common:get-runconfig-targets #!key (configf #f))
  (let ((targs       (sort (map car (hash-table->alist
				     (or configf
					 (read-config (conc *toppath* "/runconfigs.config")
						      #f #t)
					 (make-hash-table))))
			   string<?))
	(target-patt (args:get-arg "-target")))
    (if target-patt
	(filter (lambda (x)
		  (patt-list-match x target-patt))
		targs)
	targs)))

;; '(print (string-intersperse (map cadr (hash-table-ref/default (read-config "megatest.config" \#f \#t) "disks" '"'"'("none" ""))) "\n"))'
(define (common:get-disks #!key (configf #f))
  (hash-table-ref/default 
   (or configf (read-config "megatest.config" #f #t))
   "disks" '("none" "")))

;; return first command that exists, else #f
;;
(define (common:which cmds)
  (if (null? cmds)
      #f
      (let loop ((hed (car cmds))
		 (tal (cdr cmds)))
	(let ((res (with-input-from-pipe (conc "which " hed) read-line)))
	  (if (and (string? res)
		   (file-exists? res))
	      res
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal))))))))
  
(define (common:get-install-area)
  (let ((exe-path (car (argv))))
    (if (file-exists? exe-path)
	(handle-exceptions
	 exn
	 #f
	 (pathname-directory
	  (pathname-directory 
	   (pathname-directory exe-path))))
	#f)))

;;======================================================================
;; T A R G E T S  ,   S T A T E ,   S T A T U S ,   
;;                    R U N N A M E    A N D   T E S T P A T T
;;======================================================================

;; Lookup a value in runconfigs based on -reqtarg or -target
(define (runconfigs-get config var)
  (let ((targ (common:args-get-target))) ;; (or (args:get-arg "-reqtarg")(args:get-arg "-target")(getenv "MT_TARGET"))))
    (if targ
	(or (configf:lookup config targ var)
	    (configf:lookup config "default" var))
	(configf:lookup config "default" var))))

(define (common:args-get-state)
  (or (args:get-arg "-state")(args:get-arg ":state")))

(define (common:args-get-status)
  (or (args:get-arg "-status")(args:get-arg ":status")))

(define (common:args-get-testpatt rconf)
  (let* ((rtestpatt     (if rconf (runconfigs-get rconf "TESTPATT") #f))
	 (args-testpatt (or (args:get-arg "-testpatt")
			    (args:get-arg "-runtests")
			    "%"))
	 (testpatt    (or (and (equal? args-testpatt "%")
			       rtestpatt)
			  args-testpatt)))
    (if rtestpatt (debug:print-info 0 *default-log-port* "TESTPATT from runconfigs: " rtestpatt))
    testpatt))

(define (common:get-linktree)
  (or (getenv "MT_LINKTREE")
      (if *configdat*
	  (configf:lookup *configdat* "setup" "linktree"))))

(define (common:args-get-runname)
  (let ((res (or (args:get-arg "-runname")
		 (args:get-arg ":runname")
		 (getenv "MT_RUNNAME"))))
    ;; (if res (set-environment-variable "MT_RUNNAME" res)) ;; not sure if this is a good idea. side effect and all ...
    res))

(define (common:args-get-target #!key (split #f))
  (let* ((keys    (if (hash-table? *configdat*) (keys:config-get-fields *configdat*) '()))
	 (numkeys (length keys))
	 (target  (or (args:get-arg "-reqtarg")
		      (args:get-arg "-target")
		      (getenv "MT_TARGET")))
	 (tlist   (if target (string-split target "/" #t) '()))
	 (valid   (if target
		      (or (null? keys) ;; probably don't know our keys yet
			  (and (not (null? tlist))
			       (eq? numkeys (length tlist))
			       (null? (filter string-null? tlist))))
		      #f)))
    (if valid
	(if split
	    tlist
	    target)
	(if target
	    (begin
	      (debug:print-error 0 *default-log-port* "Invalid target, spaces or blanks not allowed \"" target "\", target should be: " (string-intersperse keys "/") ", have " tlist " for elements")
	      #f)
	    #f))))

;; logic for getting homehost. Returns (host . at-home)
;;
(define (common:get-homehost)
  (let* ((currhost (get-host-name))
	 (bestadrs (server:get-best-guess-address currhost))
	 ;; first look in config, then look in file .homehost, create it if not found
	 (homehost (or (configf:lookup *configdat* "server" "homehost" )
		       (let ((hhf (conc *toppath* "/.homehost")))
			 (if (file-exists? hhf)
			     (with-input-from-file hhf read-line)
			     (if (file-write-access? *toppath*)
				 (begin
				   (with-output-to-file hhf
				     (lambda ()
				       (print bestadrs)))
				   (common:get-homehost))
				 #f)))))
	 (at-home  (or (equal? homehost currhost)
		       (equal? homehost bestadrs))))
    (cons homehost at-home)))

;;======================================================================
;; M I S C   L I S T S
;;======================================================================

;; items in lista are matched value and position in listb
;; return the remaining items in listb or #f
;;
(define (common:list-is-sublist lista listb)
  (if (null? lista)
      listb ;; all items in listb are "remaining"
      (if (> (length lista)(length listb)) 
	  #f
	  (let loop ((heda (car lista))
		     (tala (cdr lista))
		     (hedb (car listb))
		     (talb (cdr listb)))
	    (if (equal? heda hedb)
		(if (null? tala) ;; we are done
		    talb
		    (loop (car tala)
			  (cdr tala)
			  (car talb)
			  (cdr talb)))
		#f)))))

;; Needed for long lists to be sorted where (apply max ... ) dies
;;
(define (common:max inlst)
  (let loop ((max-val (car inlst))
	     (hed     (car inlst))
	     (tal     (cdr inlst)))
    (if (not (null? tal))
	(loop (max hed max-val)
	      (car tal)
	      (cdr tal))
	(max hed max-val))))

;; get min or max, use > for max and < for min, this works around the limits on apply
;;
(define (common:min-max comp lst)
  (if (null? lst)
      #f ;; better than an exception for my needs
      (fold (lambda (a b)
	      (if (comp a b) a b))
	    (car lst)
	    lst)))

;; path list to hash-table tree
;;   ((a b c)(a b d)(e b c)) => ((a (b (d) (c))) (e (b (c))))
;;
(define (common:list->htree lst)
  (let ((resh (make-hash-table)))
    (for-each
     (lambda (inlst)
       (let loop ((ht  resh)
		  (hed (car inlst))
		  (tal (cdr inlst)))
	 (if (hash-table-ref/default ht hed #f)
	     (if (not (null? tal))
		 (loop (hash-table-ref ht hed)
		       (car tal)
		       (cdr tal)))
	     (begin
	       (hash-table-set! ht hed (make-hash-table))
	       (loop ht hed tal)))))
     lst)
    resh))

;; hash-table tree to html list tree
;;
;;   tipfunc takes two parameters: y the tip value and path the path to that point
;;
(define (common:htree->html ht path tipfunc)
  (let ((datlist 	(sort (hash-table->alist ht)
                              (lambda (a b)
                                (string< (car a)(car b))))))
    (if (null? datlist)
    	(tipfunc #f path) ;; really shouldn't get here
	(s:ul
	 (map (lambda (x)
		(let* ((levelname (car x))
		       (y         (cdr x))
		       (newpath   (append path (list levelname)))
		       (leaf      (or (not (hash-table? y))
				      (null? (hash-table-keys y)))))
		  (if leaf
		      (s:li (tipfunc y newpath))
		      (s:li
		       (list 
			levelname
			(common:htree->html y newpath tipfunc))))))
	      datlist)))))

;; hash-table tree to alist tree
;;
(define (common:htree->atree ht)
  (map (lambda (x)
	 (cons (car x)
	       (let ((y (cdr x)))
		 (if (hash-table? y)
		     (common:htree->atree y)
		     y))))
       (hash-table->alist ht)))

;;======================================================================
;; M U N G E   D A T A   I N T O   N I C E   F O R M S
;;======================================================================

;; Generate an index for a sparse list of key values
;;   ( (rowname1 colname1 val1)(rowname2 colname2 val2) )
;;
;; => 
;;
;;   ( (rowname1 0)(rowname2 1))    ;; rownames -> num
;;     (colname1 0)(colname2 1)) )  ;; colnames -> num
;; 
;; optional apply proc to rownum colnum value
(define (common:sparse-list-generate-index data #!key (proc #f))
  (if (null? data)
      (list '() '())
      (let loop ((hed (car data))
		 (tal (cdr data))
		 (rownames '())
		 (colnames '())
		 (rownum   0)
		 (colnum   0))
	(let* ((rowkey          (car   hed))
	       (colkey          (cadr  hed))
	       (value           (caddr hed))
	       (existing-rowdat (assoc rowkey rownames))
	       (existing-coldat (assoc colkey colnames))
	       (curr-rownum     (if existing-rowdat rownum (+ rownum 1)))
	       (curr-colnum     (if existing-coldat colnum (+ colnum 1)))
	       (new-rownames    (if existing-rowdat rownames (cons (list rowkey curr-rownum) rownames)))
	       (new-colnames    (if existing-coldat colnames (cons (list colkey curr-colnum) colnames))))
	  ;; (debug:print-info 0 *default-log-port* "Processing record: " hed )
	  (if proc (proc curr-rownum curr-colnum rowkey colkey value))
	  (if (null? tal)
	      (list new-rownames new-colnames)
	      (loop (car tal)
		    (cdr tal)
		    new-rownames
		    new-colnames
		    (if (> curr-rownum rownum) curr-rownum rownum)
		    (if (> curr-colnum colnum) curr-colnum colnum)
		    ))))))

;;======================================================================
;; S Y S T E M   S T U F F
;;======================================================================

;; lazy-safe get file mod time. on any error (file not existing etc.) return 0
;;
(define (common:lazy-modification-time fpath)
  (handle-exceptions
   exn
   0
   (file-modification-time fpath)))

;; return a nice clean pathname made absolute
(define (common:nice-path dir)
  (let ((match (string-match "^(~[^\\/]*)(\\/.*|)$" dir)))
    (if match ;; using ~ for home?
	(common:nice-path (conc (common:read-link-f (cadr match)) "/" (caddr match)))
	(normalize-pathname (if (absolute-pathname? dir)
				dir
				(conc (current-directory) "/" dir))))))

;; make "nice-path" available in config files and the repl
(define nice-path common:nice-path)

(define (common:read-link-f path)
  (handle-exceptions
      exn
      (begin
	(debug:print-error 0 *default-log-port* "command \"/bin/readlink -f " path "\" failed.")
	path) ;; just give up
    (with-input-from-pipe
	(conc "/bin/readlink -f " path)
      (lambda ()
	(read-line)))))

(define (get-cpu-load #!key (remote-host #f))
  (car (common:get-cpu-load remote-host)))
;;   (let* ((load-res (process:cmd-run->list "uptime"))
;; 	 (load-rx  (regexp "load average:\\s+(\\d+)"))
;; 	 (cpu-load #f))
;;     (for-each (lambda (l)
;; 		(let ((match (string-search load-rx l)))
;; 		  (if match
;; 		      (let ((newval (string->number (cadr match))))
;; 			(if (number? newval)
;; 			    (set! cpu-load newval))))))
;; 	      (car load-res))
;;     cpu-load))

;; get cpu load by reading from /proc/loadavg, return all three values
;;
(define (common:get-cpu-load remote-host)
  (if remote-host
      (map (lambda (res)
	     (if (eof-object? res) 9e99 res))
	   (with-input-from-pipe 
	    (conc "ssh " remote-host " cat /proc/loadavg")
	    (lambda ()(list (read)(read)(read)))))
      (with-input-from-file "/proc/loadavg" 
	(lambda ()(list (read)(read)(read))))))

(define (common:wait-for-cpuload maxload numcpus waitdelay #!key (count 1000) (msg #f)(remote-host #f))
  (let* ((loadavg (common:get-cpu-load remote-host))
	 (first   (car loadavg))
	 (next    (cadr loadavg))
	 (adjload (* maxload numcpus))
	 (loadjmp (- first next)))
    (cond
     ((and (> first adjload)
	   (> count 0))
      (debug:print-info 0 *default-log-port* "waiting " waitdelay " seconds due to load " first " exceeding max of " adjload (if msg msg ""))
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1)))
     ((and (> loadjmp numcpus)
	   (> count 0))
      (debug:print-info 0 *default-log-port* "waiting " waitdelay " seconds due to load jump " loadjmp " > numcpus " numcpus (if msg msg ""))
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1))))))

(define (common:get-num-cpus remote-host)
  (let ((proc (lambda ()
		(let loop ((numcpu 0)
			   (inl    (read-line)))
		  (if (eof-object? inl)
		      numcpu
		      (loop (if (string-match "^processor\\s+:\\s+\\d+$" inl)
				(+ numcpu 1)
				numcpu)
			    (read-line)))))))
    (if remote-host
	(with-input-from-pipe 
	 (conc "ssh " remote-host " cat /proc/cpuinfo")
	 proc)
	(with-input-from-file "/proc/cpuinfo" proc))))

;; wait for normalized cpu load to drop below maxload
;;
(define (common:wait-for-normalized-load maxload #!key (msg #f)(remote-host #f))
  (let ((num-cpus (common:get-num-cpus remote-host)))
    (common:wait-for-cpuload maxload num-cpus 15 msg: msg)))

(define (get-uname . params)
  (let* ((uname-res (process:cmd-run->list (conc "uname " (if (null? params) "-a" (car params)))))
	 (uname #f))
    (if (null? (car uname-res))
	"unknown"
	(caar uname-res))))

;; for reasons I don't understand multiple calls to real-path in parallel threads
;; must be protected by mutexes
;;
(define (common:real-path inpath)
  ;; (process:cmd-run-with-stderr->list "readlink" "-f" inpath)) ;; cmd . params)
  ;; (let-values 
  ;;  (((inp oup pid) (process "readlink" (list "-f" inpath))))
  ;;  (with-input-from-port inp
  ;;    (let loop ((inl (read-line))
  ;;       	(res #f))
  ;;      (print "inl=" inl)
  ;;      (if (eof-object? inl)
  ;;          (begin
  ;;            (close-input-port inp)
  ;;            (close-output-port oup)
  ;;            ;; (process-wait pid)
  ;;            res)
  ;;          (loop (read-line) inl))))))
  (with-input-from-pipe (conc "readlink -f " inpath) read-line))

;;======================================================================
;; D I S K   S P A C E 
;;======================================================================

(define (common:get-disk-space-used fpath)
  (with-input-from-pipe (conc "/usr/bin/du -s " fpath) read))

;; given path get free space, allows override in [setup]
;; with free-space-script /path/to/some/script.sh
;;
(define (get-df path)
  (if (configf:lookup *configdat* "setup" "free-space-script")
      (with-input-from-pipe 
       (conc (configf:lookup *configdat* "setup" "free-space-script") " " path)
       (lambda ()
	 (let ((res (read-line)))
	   (if (string? res)
	       (string->number res)))))
      (get-unix-df path)))

(define (get-unix-df path)
  (let* ((df-results (process:cmd-run->list (conc "df " path)))
	 (space-rx   (regexp "([0-9]+)\\s+([0-9]+)%"))
	 (freespc    #f))
    ;; (write df-results)
    (for-each (lambda (l)
		(let ((match (string-search space-rx l)))
		  (if match 
		      (let ((newval (string->number (cadr match))))
			(if (number? newval)
			    (set! freespc newval))))))
	      (car df-results))
    freespc))

;; check space in dbdir
;; returns: ok/not dbspace required-space
;;
(define (common:check-db-dir-space)
  (let* ((dbdir    (db:get-dbdir))
	 (dbspace  (if (directory? dbdir)
		       (get-df dbdir)
		       0))
	 (required (string->number 
		    (or (configf:lookup *configdat* "setup" "dbdir-space-required")
			"100000"))))
    (list (> dbspace required)
	  dbspace
	  required
	  dbdir)))

;; check available space in dbdir, exit if insufficient
;;
(define (common:check-db-dir-and-exit-if-insufficient)
  (let* ((spacedat (common:check-db-dir-space))
	 (is-ok    (car spacedat))
	 (dbspace  (cadr spacedat))
	 (required (caddr spacedat))
	 (dbdir    (cadddr spacedat)))
    (if (not is-ok)
	(begin
	  (debug:print-error 0 *default-log-port* "Insufficient space in " dbdir ", require " required ", have " dbspace  ", exiting now.")
	  (exit 1)))))
  
;; paths is list of lists ((name path) ... )
;;
(define (common:get-disk-with-most-free-space disks minsize)
  (let ((best     #f)
	(bestsize 0))
    (for-each 
     (lambda (disk-num)
       (let* ((dirpath    (cadr (assoc disk-num disks)))
	      (freespc    (cond
			   ((not (directory? dirpath))
			    (if (common:low-noise-print 300 "disks not a dir " disk-num)
				(debug:print 0 *default-log-port* "WARNING: disk " disk-num " at path \"" dirpath "\" is not a directory - ignoring it."))
			    -1)
			   ((not (file-write-access? dirpath))
			    (if (common:low-noise-print 300 "disks not writeable " disk-num)
				(debug:print 0 *default-log-port* "WARNING: disk " disk-num " at path \"" dirpath "\" is not writeable - ignoring it."))
			    -1)
			   ((not (eq? (string-ref dirpath 0) #\/))
			    (if (common:low-noise-print 300 "disks not a proper path " disk-num)
				(debug:print 0 *default-log-port* "WARNING: disk " disk-num " at path \"" dirpath "\" is not a fully qualified path - ignoring it."))
			    -1)
			   (else
			    (get-df dirpath)))))
	 (if (> freespc bestsize)
	     (begin
	       (set! best     (cons disk-num dirpath))
	       (set! bestsize freespc)))))
     (map car disks))
    (if (and best (> bestsize minsize))
	best
	#f))) ;; #f means no disk candidate found

;;======================================================================
;; E N V I R O N M E N T   V A R S
;;======================================================================
	      
(define (save-environment-as-files fname #!key (ignorevars (list "USER" "HOME" "DISPLAY" "LS_COLORS" "XKEYSYMDB" "EDITOR" "MAKEFLAGS" "MAKEF" "MAKEOVERRIDES")))
  (let ((envvars (get-environment-variables))
        (whitesp (regexp "[^a-zA-Z0-9_\\-:,.\\/%$]"))
	(mungeval (lambda (val)
		    (cond
		     ((eq? val #t) "") ;; convert #t to empty string
		     ((eq? val #f) #f) ;; convert #f to itself (still thinking about this one
		     (else val)))))
     (with-output-to-file (conc fname ".csh")
       (lambda ()
          (for-each (lambda (keyval)
		      (let* ((key   (car keyval))
			     (val   (cdr keyval))
			     (delim (if (string-search whitesp val) 
					"\""
					"")))
			(print (if (member key ignorevars)
				   "# setenv "
				   "setenv ")
			       key " " delim (mungeval val) delim)))
		    envvars)))
     (with-output-to-file (conc fname ".sh")
       (lambda ()
          (for-each (lambda (keyval)
		      (let* ((key (car keyval))
			     (val (cdr keyval))
			     (delim (if (string-search whitesp val) 
					"\""
					"")))
			(print (if (member key ignorevars)
				   "# export "
				   "export ")
			       key "=" delim (mungeval val) delim)))
                    envvars)))))

;; set some env vars from an alist, return an alist with original values
;; (("VAR" "value") ...)
(define (alist->env-vars lst)
  (if (list? lst)
      (let ((res '()))
	(for-each (lambda (p)
		    (let* ((var (car  p))
			   (val (cadr p))
			   (prv (get-environment-variable var)))
		      (set! res (cons (list var prv) res))
		      (if val 
			  (setenv var (->string val))
			  (unsetenv var))))
		  lst)
	res)
      '()))

;; clear vars matching pattern, run proc, set vars back
;; if proc is a string run that string as a command with
;; system.
;;
(define (common:without-vars proc . var-patts)
  (let ((vars (make-hash-table)))
    (for-each
     (lambda (vardat) ;; each env var
       (for-each
	(lambda (var-patt)
	  (if (string-match var-patt (car vardat))
	      (let ((var (car vardat))
		    (val (cdr vardat)))
		(hash-table-set! vars var val)
		(unsetenv var))))
	var-patts))
     (get-environment-variables))
    (cond
     ((string? proc)(system proc))
     (proc          (proc)))
    (hash-table-for-each
     vars
     (lambda (var val)
       (setenv var val)))
    vars))

(define (common:run-a-command cmd #!key (with-vars #f))
  (let* ((pre-cmd  (dtests:get-pre-command))
         (post-cmd (dtests:get-post-command))
         (fullcmd  (if (or pre-cmd post-cmd)
                       (conc pre-cmd cmd post-cmd)
                       (conc "viewscreen " cmd))))
    (debug:print-info 02 *default-log-port* "Running command: " fullcmd)
    (if with-vars
        (common:without-vars cmd)
        (common:without-vars fullcmd "MT_.*"))))
		  
;;======================================================================
;; T I M E   A N D   D A T E
;;======================================================================

;; Convert strings like "5s 2h 3m" => 60x60x2 + 3x60 + 5
(define (common:hms-string->seconds tstr)
  (let ((parts     (string-split tstr))
	(time-secs 0)
	;; s=seconds, m=minutes, h=hours, d=days
	(trx       (regexp "(\\d+)([smhd])")))
    (for-each (lambda (part)
		(let ((match  (string-match trx part)))
		  (if match
		      (let ((val (string->number (cadr match)))
			    (unt (caddr match)))
			(if val 
			    (set! time-secs (+ time-secs (* val
							    (case (string->symbol unt)
							      ((s) 1)
							      ((m) 60)
							      ((h) (* 60 60))
							      ((d) (* 24 60 60))
							      (else 0))))))))))
	      parts)
    time-secs))
		       
(define (seconds->hr-min-sec secs)
  (let* ((hrs (quotient secs 3600))
	 (min (quotient (- secs (* hrs 3600)) 60))
	 (sec (- secs (* hrs 3600)(* min 60))))
    (conc (if (> hrs 0)(conc hrs "hr ") "")
	  (if (> min 0)(conc min "m ")  "")
	  sec "s")))

(define (seconds->time-string sec)
  (time->string 
   (seconds->local-time sec) "%H:%M:%S"))

(define (seconds->work-week/day-time sec)
  (time->string
   (seconds->local-time sec) "ww%V.%u %H:%M"))

(define (seconds->work-week/day sec)
  (time->string
   (seconds->local-time sec) "ww%V.%u"))

(define (seconds->year-work-week/day sec)
  (time->string
   (seconds->local-time sec) "%yww%V.%w"))

(define (seconds->year-work-week/day-time sec)
  (time->string
   (seconds->local-time sec) "%Yww%V.%w %H:%M"))

(define (seconds->year-week/day-time sec)
  (time->string
   (seconds->local-time sec) "%Yw%V.%w %H:%M"))

(define (seconds->quarter sec)
  (case (string->number
	 (time->string 
	  (seconds->local-time sec)
	  "%m"))
    ((1 2 3) 1)
    ((4 5 6) 2)
    ((7 8 9) 3)
    ((10 11 12) 4)
    (else #f)))

;; given span of seconds tstart to tend
;; find start time to mark and mark delta
;;
(define (common:find-start-mark-and-mark-delta tstart tend)
  (let* ((deltat   (- (max tend (+ tend 10)) tstart)) ;; can't handle runs of less than 4 seconds. Pad it to 10 seconds ...
	 (result   #f)
	 (min      60)
	 (hr       (* 60 60))
	 (day      (* 24 hr))
	 (yr       (* 365 day)) ;; year
	 (mo       (/ yr 12))
	 (wk       (* day 7)))
    (for-each
     (lambda (max-blks)
       (for-each
	(lambda (span) ;; 5 2 1
	  (if (not result)
	      (for-each 
	       (lambda (timeunit timesym) ;; year month day hr min sec
		 (if (not result)
		     (let* ((time-blk (* span timeunit))
			    (num-blks (quotient deltat time-blk)))
		       (if (and (> num-blks 4)(< num-blks max-blks))
			   (let ((first (* (quotient tstart time-blk) time-blk)))
			     (set! result (list span timeunit time-blk first timesym))
			     )))))
	       (list yr mo wk day hr min 1)
	       '(     y  mo w  d   h  m   s))))
	(list 8 6 5 2 1)))
     '(5 10 15 20 30 40 50 500))
    (if values
	(apply values result)
	(values 0 day 1 0 'd))))
	    
	  

;;======================================================================
;; C O L O R S
;;======================================================================
      
(define (common:name->iup-color name)
  (case (string->symbol (string-downcase name))
    ((red)    "223 33 49")
    ((grey)   "192 192 192")
    ((orange) "255 172 13")
    ((purple) "This is unfinished ...")))

;; (define (common:get-color-for-state-status state status)
;;   (case (string->symbol state)
;;     ((COMPLETED)
;;      (case (string->symbol status)
;;        ((PASS)        "70  249 73")
;;        ((WARN WAIVED) "255 172 13")
;;        ((SKIP)        "230 230 0")
;;        (else "223 33 49")))
;;     ((LAUNCHED)         "101 123 142")
;;     ((CHECK)            "255 100 50")
;;     ((REMOTEHOSTSTART)  "50  130 195")
;;     ((RUNNING)          "9   131 232")
;;     ((KILLREQ)          "39  82  206")
;;     ((KILLED)           "234 101 17")
;;     ((NOT_STARTED)      "240 240 240")
;;     (else               "192 192 192")))

(define (common:iup-color->rgb-hex instr)
  (string-intersperse 
   (map (lambda (x)
          (number->string x 16))
        (map string->number
             (string-split instr)))
   "/"))

(define (common:get-color-from-status status)
  (cond
   ((equal? status "PASS")    "green")
   ((equal? status "FAIL")    "red")
   ((equal? status "WARN")    "orange")
   ((equal? status "KILLED")  "orange")
   ((equal? status "KILLREQ") "purple")
   ((equal? status "RUNNING") "blue")
   ((equal? status "ABORT")   "brown")
   (else "black")))

;;======================================================================
;; N A N O M S G   C L I E N T
;;======================================================================

(define (server:get-best-guess-address hostname)
  (let ((res #f))
    (for-each 
     (lambda (adr)
       (if (not (eq? (u8vector-ref adr 0) 127))
	   (set! res adr)))
     ;; NOTE: This can fail when there is no mention of the host in /etc/hosts. FIXME
     (vector->list (hostinfo-addresses (hostname->hostinfo hostname))))
    (string-intersperse 
     (map number->string
	  (u8vector->list
	   (if res res (hostname->ip hostname)))) ".")))


(define (common:send-dboard-main-changed)
  (let* ((dashboard-ips (mddb:get-dashboards)))
    (for-each
     (lambda (ipadr)
       (let* ((soc (common:open-nm-req (conc "tcp://" ipadr)))
	      (msg (conc "main " *toppath*))
	      (res (common:nm-send-receive-timeout soc msg)))
	 (if (not res) ;; couldn't reach that dashboard - remove it from db
	     (print "ERROR: couldn't reach dashboard " ipadr))
	 res))
     dashboard-ips)))
    
    
;;======================================================================
;; D A S H B O A R D   D B 
;;======================================================================

(define (mddb:open-db)
  (let* ((db (open-database (conc (get-environment-variable "HOME") "/.dashboard.db"))))
    (set-busy-handler! db (busy-timeout 10000))
    (for-each
     (lambda (qry)
       (exec (sql db qry)))
     (list 
      "CREATE TABLE IF NOT EXISTS vars       (id INTEGER PRIMARY KEY,key TEXT, val TEXT, CONSTRAINT varsconstraint UNIQUE (key));"
      "CREATE TABLE IF NOT EXISTS dashboards (
          id         INTEGER PRIMARY KEY,
          pid        INTEGER,
          username   TEXT,
          hostname   TEXT,
          ipaddr     TEXT,
          portnum    INTEGER,
          start_time TIMESTAMP DEFAULT (strftime('%s','now')),
             CONSTRAINT hostport UNIQUE (hostname,portnum)
        );"
      ))
    db))

;; register a dashboard 
;;
(define (mddb:register-dashboard port)
  (let* ((pid      (current-process-id))
	 (hostname (get-host-name))
	 (ipaddr   (server:get-best-guess-address hostname))
	 (username (current-user-name)) ;; (car userinfo)))
	 (db      (mddb:open-db)))
    (print "Register monitor, pid: " pid ", hostname: " hostname ", port: " port ", username: " username)
    (exec (sql db "INSERT OR REPLACE INTO dashboards (pid,username,hostname,ipaddr,portnum) VALUES (?,?,?,?,?);")
	   pid username hostname ipaddr port)
    (close-database db)))

;; unregister a monitor
;;
(define (mddb:unregister-dashboard host port)
  (let* ((db      (mddb:open-db)))
    (print "Register unregister monitor, host:port=" host ":" port)
    (exec (sql db "DELETE FROM dashboards WHERE hostname=? AND portnum=?;") host port)
    (close-database db)))

;; get registered dashboards
;;
(define (mddb:get-dashboards)
  (let ((db (mddb:open-db)))
    (query fetch-column
	   (sql db "SELECT ipaddr || ':' || portnum FROM dashboards;"))))
    
;;======================================================================
;;  T E S T   L A U N C H I N G   P E R   I T E M   W I T H   H O S T   T Y P E S
;;======================================================================
;; 
;; [host-types]
;; general ssh #{getbgesthost general}
;; nbgeneral nbjob run JOBCOMMAND -log $MT_LINKTREE/$MT_TARGET/$MT_RUNNAME.$MT_TESTNAME-$MT_ITEM_PATH.lgo
;; 
;; [hosts]
;; general cubian xena
;; 
;; [launchers]
;; envsetup general
;; xor/%/n 4C16G
;; % nbgeneral
;; 
;; [jobtools]
;; launcher bsub
;; # if defined and not "no" flexi-launcher will bypass launcher unless there is no
;; # match.
;; flexi-launcher yes  

(define (common:get-launcher configdat testname itempath)
  (let ((fallback-launcher (configf:lookup configdat "jobtools" "launcher")))
    (if (and (configf:lookup configdat "jobtools" "flexi-launcher") ;; overrides launcher
	     (not (equal? (configf:lookup configdat "jobtools" "flexi-launcher") "no")))
	(let* ((launchers         (hash-table-ref/default configdat "launchers" '())))
	  (if (null? launchers)
	      fallback-launcher
	      (let loop ((hed (car launchers))
			 (tal (cdr launchers)))
		(let ((patt      (car hed))
		      (host-type (cadr hed)))
		  (if (tests:match patt testname itempath)
		      (begin
			(debug:print-info 2 *default-log-port* "Have flexi-launcher match for " testname "/" itempath " = " host-type)
			(let ((launcher (configf:lookup configdat "host-types" host-type)))
			  (if launcher
			      launcher
			      (begin
				(debug:print-info 0 *default-log-port* "WARNING: no launcher found for host-type " host-type)
				(if (null? tal)
				    fallback-launcher
				    (loop (car tal)(cdr tal)))))))
		      ;; no match, try again
		      (if (null? tal)
			  fallback-launcher
			  (loop (car tal)(cdr tal))))))))
	fallback-launcher)))
  
;;======================================================================
;; D A S H B O A R D   U S E R   V I E W S
;;======================================================================

;; first read ~/views.config if it exists, then read $MTRAH/views.config if it exists
;;
(define (common:load-views-config)
  (let* ((view-cfgdat    (make-hash-table))
	 (home-cfgfile   (conc (get-environment-variable "HOME") "/.mtviews.config"))
	 (mthome-cfgfile (conc *toppath* "/.mtviews.config")))
    (if (file-exists? mthome-cfgfile)
	(read-config mthome-cfgfile view-cfgdat #t))
    ;; we load the home dir file AFTER the MTRAH file so the user can clobber settings when running the dashboard in read-only areas
    (if (file-exists? home-cfgfile)
	(read-config home-cfgfile view-cfgdat #t))
    view-cfgdat))

