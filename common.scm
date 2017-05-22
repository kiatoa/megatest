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

(use srfi-1 posix regex-case base64 format dot-locking csv-xml z3 sql-de-lite hostinfo md5 message-digest typed-records directory-utils stack
     matchable pkts (prefix dbi dbi:)
     regex)

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
  (if (substring-index ":" key) ;; variables containing : are for internal use and cannot be environment variables.
      (debug:print-error 4 *default-log-port* "skip setting internal use only variables containing \":\"")
      (if (and (string? val)
	       (string? key))
	  (handle-exceptions
	      exn
	      (debug:print-error 0 *default-log-port* "bad value for setenv, key=" key ", value=" val)
	    (setenv key val))
	  (debug:print-error 0 *default-log-port* "bad value for setenv, key=" key ", value=" val))))

(define home (getenv "HOME"))
(define user (getenv "USER"))

;; GLOBAL GLETCHES

;; CONTEXTS
(defstruct cxt
  (taskdb #f)
  (cmutex (make-mutex)))
;; (define *contexts* (make-hash-table))
;; (define *context-mutex* (make-mutex))

;; ;; safe method for accessing a context given a toppath
;; ;;
;; (define (common:with-cxt toppath proc)
;;   (mutex-lock! *context-mutex*)
;;   (let ((cxt (hash-table-ref/default *contexts* toppath #f)))
;;     (if (not cxt)
;;         (set! cxt (let ((x (make-cxt)))(hash-table-set! *contexts* toppath x) x)))
;;     (let ((cxt-mutex (cxt-mutex cxt)))
;;       (mutex-unlock! *context-mutex*)
;;       (mutex-lock! cxt-mutex)
;;       (let ((res (proc cxt)))
;;         (mutex-unlock! cxt-mutex)
;;         res))))
        
;; A hash table that can be accessed by #{scheme ...} calls in
;; config files. Allows communicating between confgs
;;
(define *user-hash-data* (make-hash-table))

(define *db-keys* #f)

(define *pkts-info*    (make-hash-table)) ;; store stuff like the last parent here
(define *configinfo*   #f)   ;; raw results from setup, includes toppath and table from megatest.config
(define *runconfigdat* #f)   ;; run configs data
(define *configdat*    #f)   ;; megatest.config data
(define *configstatus* #f)   ;; status of data; 'fulldata : all processing done, #f : no data yet, 'partialdata : partial read done
(define *toppath*      #f)
(define *already-seen-runconfig-info* #f)

(define *test-meta-updated* (make-hash-table))
(define *globalexitstatus*  0) ;; attempt to work around possible thread issues
(define *passnum*           0) ;; when running track calls to run-tests or similar
;; (define *alt-log-file* #f)  ;; used by -log
(define *common:denoise*    (make-hash-table)) ;; for low noise printing
(define *default-log-port*  (current-error-port))
(define *time-zero* (current-seconds)) ;; for the watchdog

;; DATABASE
(define *dbstruct-db*         #f) ;; used to cache the dbstruct in db:setup. Goal is to remove this.
;; db stats
(define *db-stats*            (make-hash-table)) ;; hash of vectors < count duration-total >
(define *db-stats-mutex*      (make-mutex))
;; db access
(define *db-last-access*      (current-seconds)) ;; last db access, used in server
(define *db-write-access*     #t)
;; db sync
(define *db-last-sync*        0)                 ;; last time the sync to megatest.db happened
(define *db-sync-in-progress* #f)                ;; if there is a sync in progress do not try to start another
(define *db-multi-sync-mutex* (make-mutex))      ;; protect access to *db-sync-in-progress*, *db-last-sync*
;; task db
(define *task-db*             #f) ;; (vector db path-to-db)
(define *db-access-allowed*   #t) ;; flag to allow access
(define *db-access-mutex*     (make-mutex))
(define *db-transaction-mutex* (make-mutex))
(define *db-cache-path*       #f)
(define *db-with-db-mutex*    (make-mutex))
(define *db-api-call-time*    (make-hash-table)) ;; hash of command => (list of times)

;; SERVER
(define *my-client-signature* #f)
(define *transport-type*    'http)             ;; override with [server] transport http|rpc|nmsg
(define *runremote*         #f)                ;; if set up for server communication this will hold <host port>
;; (define *max-cache-size*    0)
(define *logged-in-clients* (make-hash-table))
;; (define *server-id*         #f)
(define *server-info*       #f)  ;; good candidate for easily convert to non-global
(define *time-to-exit*      #f)
(define *server-run*        #t)
(define *run-id*            #f)
(define *server-kind-run*   (make-hash-table))
(define *home-host*         #f)
;; (define *total-non-write-delay* 0)
(define *heartbeat-mutex*   (make-mutex))
(define *api-process-request-count* 0)
(define *max-api-process-requests* 0)

;; client
(define *rmt-mutex*         (make-mutex))     ;; remote access calls mutex 

;; RPC transport
(define *rpc:listener*      #f)

;; KEY info
(define *target*            (make-hash-table)) ;; cache the target here; target is keyval1/keyval2/.../keyvalN
(define *keys*              (make-hash-table)) ;; cache the keys here
(define *keyvals*           (make-hash-table))
(define *toptest-paths*     (make-hash-table)) ;; cache toptest path settings here
(define *test-paths*        (make-hash-table)) ;; cache test-id to test run paths here
(define *test-ids*          (make-hash-table)) ;; cache run-id, testname, and item-path => test-id
(define *test-info*         (make-hash-table)) ;; cache the test info records, update the state, status, run_duration etc. from testdat.db

(define *run-info-cache*     (make-hash-table)) ;; run info is stable, no need to reget
(define *launch-setup-mutex* (make-mutex))     ;; need to be able to call launch:setup often so mutex it and re-call the real deal only if *toppath* not set
(define *homehost-mutex*     (make-mutex))

(defstruct remote
  (hh-dat            (common:get-homehost)) ;; homehost record ( addr . hhflag )
  (server-url        (if *toppath* (server:check-if-running *toppath*))) ;; (server:check-if-running *toppath*) #f))
  (last-server-check 0)  ;; last time we checked to see if the server was alive
  (conndat           #f)
  (transport         *transport-type*)
  (server-timeout    (server:get-timeout)) ;; default from server:get-timeout
  (force-server      #f)
  (ro-mode           #f)  
  (ro-mode-checked   #f)) ;; flag that indicates we have checked for ro-mode

;; launching and hosts
(defstruct host
  (reachable    #f)
  (last-update  0)
  (last-used    0)
  (last-cpuload 1))

(define *host-loads*         (make-hash-table))

;; cache environment vars for each run here
(define *env-vars-by-run-id* (make-hash-table))

;; Testconfig and runconfig caches. 
(define *testconfigs*        (make-hash-table)) ;; test-name => testconfig
(define *runconfigs*         (make-hash-table)) ;; target    => runconfig

;; This is a cache of pre-reqs met, don't re-calc in cases where called with same params less than
;; five seconds ago
(define *pre-reqs-met-cache* (make-hash-table))

;; cache of verbosity given string
;;
(define *verbosity-cache*    (make-hash-table))

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

;; postive number if megatest version > db version
;; negative number if megatest version < db version
(define (common:version-db-delta)
         (- megatest-version (common:get-last-run-version-number)))

(define (common:version-changed?)
  (not (equal? (common:get-last-run-version)
               (common:version-signature))))

(define (common:api-changed?)
  (not (equal? (substring (->string megatest-version) 0 4)
               (substring (common:get-last-run-version) 0 4))))
  
;; Move me elsewhere ...
;; RADT => Why do we meed the version check here, this is called only if version misma
;;
(define (common:cleanup-db dbstruct)
  (db:multi-db-sync 
   dbstruct
   'schema
   ;; 'new2old
   'killservers
   'dejunk
   'adj-target
   ;; 'old2new
   'new2old
   )
  (if (common:api-changed?)
      (common:set-last-run-version)))

;; Rotate logs, logic: 
;;                 if > 500k and older than 1 week:
;;                     remove previous compressed log and compress this log
;; WARNING: This proc operates assuming that it is in the directory above the
;;          logs directory you wish to log-rotate.
;;
(define (common:rotate-logs)
  (if (not (directory-exists? "logs"))(create-directory "logs"))
  (directory-fold 
   (lambda (file rem)
     (handle-exceptions
      exn
      (debug:print-info 0 *default-log-port* "failed to rotate log " file ", probably handled by another process.")
      (let* ((fullname (conc "logs/" file))
             (file-age (- (current-seconds)(file-modification-time fullname))))
        (if (or (and (string-match "^.*.log" file)
                     (> (file-size fullname) 200000))
                (and (string-match "^server-.*.log" file)
                     (> (- (current-seconds) (file-modification-time fullname))
                        (* 8 60 60))))
            (let ((gzfile (conc fullname ".gz")))
              (if (file-exists? gzfile)
                  (begin
                    (debug:print-info 0 *default-log-port* "removing " gzfile)
                    (delete-file gzfile)))
              (debug:print-info 0 *default-log-port* "compressing " file)
              (system (conc "gzip " fullname)))
            (if (> file-age (* (string->number (or (configf:lookup *configdat* "setup" "log-expire-days") "30")) 24 3600))
                (handle-exceptions
                 exn
                 #f
                 (delete-file fullname)))))))
   '()
   "logs"))

;; Force a megatest cleanup-db if version is changed and skip-version-check not specified
;; Do NOT check if not on homehost!
;;
(define (common:exit-on-version-changed)
  (if (common:on-homehost?)
      (if (common:api-changed?)
	  (let* ((mtconf (conc (get-environment-variable "MT_RUN_AREA_HOME") "/megatest.config"))
                (dbfile (conc (get-environment-variable "MT_RUN_AREA_HOME") "/megatest.db"))
                (read-only (not (file-write-access? dbfile)))
                (dbstruct (db:setup #t)))
	    (debug:print 0 *default-log-port*
			 "WARNING: Version mismatch!\n"
			 "   expected: " (common:version-signature) "\n"
			 "   got:      " (common:get-last-run-version))
            (cond
             ((get-environment-variable "MT_SKIP_DB_MIGRATE") #t)
             ((and (file-exists? mtconf) (file-exists? dbfile) (not read-only)
                   (eq? (current-user-id)(file-owner mtconf))) ;; safe to run -cleanup-db
              (debug:print 0 *default-log-port* "   I see you are the owner of megatest.config, attempting to cleanup and reset to new version")
              (handle-exceptions
               exn
               (begin
                 (debug:print 0 *default-log-port* "Failed to switch versions.")
                 (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
                 (print-call-chain (current-error-port))
                 (exit 1))
               (common:cleanup-db dbstruct)))
             ((not (file-exists? mtconf))
              (debug:print 0 *default-log-port* "   megatest.config does not exist in this area.  Cannot proceed with megatest version migration.")
              (exit 1))
             ((not (file-exists? dbfile))
              (debug:print 0 *default-log-port* "   megatest.db does not exist in this area.  Cannot proceed with megatest version migration.")
              (exit 1))
             ((not (eq? (current-user-id)(file-owner mtconf)))
              (debug:print 0 *default-log-port* "   You do not own megatest.db in this area.  Cannot proceed with megatest version migration.")
              (exit 1))
             (read-only
              (debug:print 0 *default-log-port* "   You have read-only access to this area.  Cannot proceed with megatest version migration.")
              (exit 1))
             (else
              (debug:print 0 *default-log-port* " to switch versions you can run: \"megatest -cleanup-db\"")
              (exit 1)))))
      (begin
	(debug:print 0 *default-log-port* "ERROR: cannot migrate version unless on homehost. Exiting.")
	(exit 1))))

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
  (handle-exceptions
      exn
      #f ;; don't really care what went wrong right now. NOTE: I have not seen this one actually fail.
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
	      #f)))))

(define (common:simple-file-lock-and-wait fname #!key (expire-time 300))
  (let ((end-time (+ expire-time (current-seconds))))
    (let loop ((got-lock (common:simple-file-lock fname expire-time: expire-time)))
      (if got-lock
	  #t
	  (if (> end-time (current-seconds))
	      (begin
		(thread-sleep! 3)
		(loop (common:simple-file-lock fname expire-time: expire-time)))
	      #f)))))

(define (common:simple-file-release-lock fname)
  (handle-exceptions
      exn
      #f ;; I don't really care why this failed (at least for now)
    (delete-file* fname)))

;;======================================================================
;; S T A T E S   A N D   S T A T U S E S
;;======================================================================

;; BBnote: *common:std-states* - dashboard filter control and test control state buttons defined here; used in set-fields-panel and dboard:make-controls
(define *common:std-states*   ;; for toggle buttons in dashboard
  '((0 "ARCHIVED")
    (1 "STUCK")
    (2 "KILLREQ")
    (3 "KILLED")
    (4 "NOT_STARTED")
    (5 "COMPLETED")
    (6 "LAUNCHED")
    (7 "REMOTEHOSTSTART")
    (8 "RUNNING")
    ))

;; BBnote: *common:std-statuses* dashboard filter control and test control status buttons defined here; used in set-fields-panel and dboard:make-controls
(define *common:std-statuses*
  '(;; (0 "DELETED")
    (1 "n/a")
    (2 "PASS")
    (3 "SKIP")
    (4 "WARN")
    (5 "WAIVED")
    (6 "CHECK")
    (7 "STUCK/DEAD")
    (8 "FAIL")
    (9 "ABORT")))

(define *common:ended-states*       ;; states which indicate the test is stopped and will not proceed
  '("COMPLETED" "ARCHIVED" "KILLED" "KILLREQ" "STUCK" "INCOMPLETE"))

(define *common:badly-ended-states* ;; these roll up as CHECK, i.e. results need to be checked
  '("KILLED" "KILLREQ" "STUCK" "INCOMPLETE" "DEAD"))

;; BBnote: *common:running-states* used from db:set-state-status-and-roll-up-items
(define *common:running-states*     ;; test is either running or can be run
  '("RUNNING" "REMOTEHOSTSTART" "LAUNCHED" "STARTED"))

(define *common:cant-run-states*    ;; These are stopping conditions that prevent a test from being run
  '("COMPLETED" "KILLED" "UNKNOWN" "INCOMPLETE" "ARCHIVED"))

(define *common:not-started-ok-statuses* ;; if not one of these statuses when in not_started state treat as dead
  '("n/a" "na" "PASS" "FAIL" "WARN" "CHECK" "WAIVED" "DEAD" "SKIP"))

(define (common:special-sort items order comp)
  (let ((items-order (map reverse order))
        (acomp       (or comp >)))
    (sort items
        (lambda (a b)
          (let ((a-num (cadr (or (assoc a items-order) '(0 0))))
                (b-num (cadr (or (assoc b items-order) '(0 0)))))
            (acomp a-num b-num))))))

;; ;; given a toplevel with currstate, currstatus apply state and status
;; ;;  => (newstate . newstatus)
;; (define (common:apply-state-status currstate currstatus state status)
;;   (let* ((cstate  (string->symbol (string-downcase currstate)))
;;          (cstatus (string->symbol (string-downcase currstatus)))
;;          (sstate  (string->symbol (string-downcase state)))
;;          (sstatus (string->symbol (string-downcase status)))
;;          (nstate  #f)
;;          (nstatus #f))
;;     (set! nstate
;;           (case cstate
;;             ((completed not_started killed killreq stuck archived) 
;;              (case sstate ;; completed -> sstate
;;                ((completed killed killreq stuck archived) completed)
;;                ((running remotehoststart launched)        running)
;;                (else                                      unknown-error-1)))
;;             ((running remotehoststart launched)
;;              (case sstate
;;                ((completed killed killreq stuck archived) #f) ;; need to look at all items
;;                ((running remotehoststart launched)        running)
;;                (else                                      unknown-error-2)))
;;             (else unknown-error-3)))
;;     (set! nstatus
;;           (case sstatus
;;             ((pass)
;;              (case nstate
;;                ((pass n/a deleted)     pass)
;;                ((warn)                 warn)
;;                ((fail)                 fail)
;;                ((check)               check)
;;                ((waived)             waived)
;;                ((skip)                 skip)
;;                ((stuck/dead)          stuck)
;;                ((abort)               abort)
;;                (else        unknown-error-4)))
;;             ((warn)
;;              (case nstate
;;                ((pass warn n/a skip deleted)   warn)
;;                ((fail)                         fail)
;;                ((check)                       check)
;;                ((waived)                     waived)
;;                ((stuck/dead)                  stuck)
;;                (else                unknown-error-5)))
;;             ((fail)
;;              (case nstate
;;                ((pass warn fail check n/a waived skip deleted stuck/dead stuck)  fail)
;;                ((abort)                                                         abort)
;;                (else                                                  unknown-error-6)))
;;             (else    unknown-error-7)))
;;     (cons 
;;      (if nstate  (symbol->string nstate)  nstate)
;;      (if nstatus (symbol->string nstatus) nstatus))))
               
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
  (or (configf:lookup *configdat* "setup" "area-name") ;; megatest is a flexible tool, testsuite is too limiting a description.
      (configf:lookup *configdat* "setup" "testsuite" )
      (getenv "MT_TESTSUITE_NAME")
      (if (string? *toppath* )
          (pathname-file *toppath*)
          #f))) ;; (pathname-file (current-directory)))))

(define common:get-area-name common:get-testsuite-name)

(define (common:get-db-tmp-area . junk)
  (if *db-cache-path*
      *db-cache-path*
      (if *toppath* ;; common:get-create-writeable-dir
	  (handle-exceptions
	      exn
	      (begin
		(debug:print-error 0 *default-log-port* "Couldn't create path to " dbdir)
		(exit 1))
	    (let ((dbpath (common:get-create-writeable-dir
			   (list (conc "/tmp/" (current-user-name)
				       "/megatest_localdb/"
				       (common:get-testsuite-name) "/"
				       (string-translate *toppath* "/" ".")))))) ;;  #t))))
	      (set! *db-cache-path* dbpath)
	      dbpath))
	  #f)))

(define (common:get-area-path-signature)
  (message-digest-string (md5-primitive) *toppath*))

(define (common:get-signature str)
  (message-digest-string (md5-primitive) str))

;;======================================================================
;; E X I T   H A N D L I N G
;;======================================================================

(define (common:run-sync?)
    (and (common:on-homehost?)
	 (args:get-arg "-server")))

;;   (let ((ohh (common:on-homehost?))
;; 	(srv (args:get-arg "-server")))
;;     (and ohh srv)))
    ;; (debug:print-info 0 *default-log-port* "common:run-sync? ohh=" ohh ", srv=" srv)

;;;; run-ids
;;    if #f use *db-local-sync* : or 'local-sync-flags
;;    if #t use timestamps      : or 'timestamps
(define (common:sync-to-megatest.db dbstruct) 
  (let ((start-time         (current-seconds))
	(res                (db:multi-db-sync dbstruct 'new2old)))
    (let ((sync-time (- (current-seconds) start-time)))
      (debug:print-info 3 *default-log-port* "Sync of newdb to olddb completed in " sync-time " seconds pid="(current-process-id))
      (if (common:low-noise-print 30 "sync new to old")
	  (debug:print-info 0 *default-log-port* "Sync of newdb to olddb completed in " sync-time " seconds pid="(current-process-id))))
    res))




(define *wdnum* 0)
(define *wdnum*mutex (make-mutex))
;; currently the primary job of the watchdog is to run the sync back to megatest.db from the db in /tmp
;; if we are on the homehost and we are a server (by definition we are on the homehost if we are a server)
;;


(define (common:readonly-watchdog dbstruct)
  (thread-sleep! 0.05) ;; delay for startup
  (debug:print-info 13 *default-log-port* "common:readonly-watchdog entered.")
  ;; sync megatest.db to /tmp/.../megatst.db
  (let* ((sync-cool-off-duration   3)
        (golden-mtdb     (dbr:dbstruct-mtdb dbstruct))
        (golden-mtpath   (db:dbdat-get-path golden-mtdb))
        (tmp-mtdb        (dbr:dbstruct-tmpdb dbstruct))
        (tmp-mtpath      (db:dbdat-get-path tmp-mtdb)))
    (debug:print-info 0 *default-log-port* "Read-only periodic sync thread started.")
    (let loop ((last-sync-time 0))
      (debug:print-info 13 *default-log-port* "loop top tmp-mtpath="tmp-mtpath" golden-mtpath="golden-mtpath)
      (let* ((duration-since-last-sync (- (current-seconds) last-sync-time)))
        (debug:print-info 13 *default-log-port* "duration-since-last-sync="duration-since-last-sync)
        (if (and (not *time-to-exit*)
                 (< duration-since-last-sync sync-cool-off-duration))
            (thread-sleep! (- sync-cool-off-duration duration-since-last-sync)))
        (if (not *time-to-exit*)
            (let ((golden-mtdb-mtime (file-modification-time golden-mtpath))
                  (tmp-mtdb-mtime    (file-modification-time tmp-mtpath)))
              (if (> golden-mtdb-mtime tmp-mtdb-mtime)
                  (let ((res (db:multi-db-sync dbstruct 'old2new)))
                    (debug:print-info 13 *default-log-port* "rosync called, " res " records transferred.")))
              (loop (current-seconds)))
            #t)))
    (debug:print-info 0 *default-log-port* "Exiting readonly-watchdog timer, *time-to-exit* = " *time-to-exit*" pid="(current-process-id)" mtpath="golden-mtpath)))


        
(define (common:writable-watchdog dbstruct)
  (thread-sleep! 0.05) ;; delay for startup
  (let ((legacy-sync (common:run-sync?))
	(debug-mode  (debug:debug-mode 1))
	(last-time   (current-seconds))
        (this-wd-num     (begin (mutex-lock! *wdnum*mutex) (let ((x *wdnum*)) (set! *wdnum* (add1 *wdnum*)) (mutex-unlock! *wdnum*mutex) x))))
    (debug:print-info 2 *default-log-port* "Periodic sync thread started.")
    (debug:print-info 3 *default-log-port* "watchdog starting. legacy-sync is " legacy-sync" pid="(current-process-id)" this-wd-num="this-wd-num)
    (if (and legacy-sync (not *time-to-exit*))
	(let* (;;(dbstruct (db:setup))
	       (mtdb     (dbr:dbstruct-mtdb dbstruct))
	       (mtpath   (db:dbdat-get-path mtdb)))
	  (debug:print-info 0 *default-log-port* "Server running, periodic sync started.")
	  (let loop ()
	    ;; sync for filesystem local db writes
	    ;;
	    (mutex-lock! *db-multi-sync-mutex*)
	    (let* ((need-sync        (>= *db-last-access* *db-last-sync*)) ;; no sync since last write
		   (sync-in-progress *db-sync-in-progress*)
		   (should-sync      (and (not *time-to-exit*)
                                          (> (- (current-seconds) *db-last-sync*) 5))) ;; sync every five seconds minimum
		   (start-time       (current-seconds))
		   (mt-mod-time      (file-modification-time mtpath))
		   (recently-synced  (< (- start-time mt-mod-time) 4))
		   (will-sync        (and (or need-sync should-sync)
					  (not sync-in-progress)
					  (not recently-synced))))
              (debug:print-info 13 *default-log-port* "WD writable-watchdog top of loop.  need-sync="need-sync" sync-in-progress="sync-in-progress" should-sync="should-sync" start-time="start-time" mt-mod-time="mt-mod-time" recently-synced="recently-synced" will-sync="will-sync)
	      ;; (if recently-synced (debug:print-info 0 *default-log-port* "Skipping sync due to recently-synced flag=" recently-synced))
	      ;; (debug:print-info 0 *default-log-port* "need-sync: " need-sync " sync-in-progress: " sync-in-progress " should-sync: " should-sync " will-sync: " will-sync)
	      (if will-sync (set! *db-sync-in-progress* #t))
	      (mutex-unlock! *db-multi-sync-mutex*)
	      (if will-sync
		  (let ((res (common:sync-to-megatest.db dbstruct))) ;; did we sync any data? If so need to set the db touched flag to keep the server alive
		    (if (> res 0) ;; some records were transferred, keep the db alive
			(begin
			  (mutex-lock! *heartbeat-mutex*)
			  (set! *db-last-access* (current-seconds))
			  (mutex-unlock! *heartbeat-mutex*)
			  (debug:print-info 0 *default-log-port* "sync called, " res " records transferred."))
			(debug:print-info 2 *default-log-port* "sync called but zero records transferred"))))
	      (if will-sync
		  (begin
		    (mutex-lock! *db-multi-sync-mutex*)
		    (set! *db-sync-in-progress* #f)
		    (set! *db-last-sync* start-time)
		    (mutex-unlock! *db-multi-sync-mutex*)))
	      (if (and debug-mode
		       (> (- start-time last-time) 60))
		  (begin
		    (set! last-time start-time)
		    (debug:print-info 4 *default-log-port* "timestamp -> " (seconds->time-string (current-seconds)) ", time since start -> " (seconds->hr-min-sec (- (current-seconds) *time-zero*))))))
	    
	    ;; keep going unless time to exit
	    ;;
	    (if (not *time-to-exit*)
		(let delay-loop ((count 0))
                  ;;(debug:print-info 13 *default-log-port* "delay-loop top; count="count" pid="(current-process-id)" this-wd-num="this-wd-num" *time-to-exit*="*time-to-exit*)
                                                            
		  (if (and (not *time-to-exit*)
			   (< count 4)) ;; was 11, changing to 4. 
		      (begin
			(thread-sleep! 1)
			(delay-loop (+ count 1))))
		  (if (not *time-to-exit*) (loop))))
	    (if (common:low-noise-print 30)
		(debug:print-info 0 *default-log-port* "Exiting watchdog timer, *time-to-exit* = " *time-to-exit*" pid="(current-process-id)" this-wd-num="this-wd-num)))))))

;; TODO: for multiple areas, we will have multiple watchdogs; and multiple threads to manage
(define (common:watchdog)
  (debug:print-info 13 *default-log-port* "common:watchdog entered.")
  (if (common:on-homehost?)
      (let ((dbstruct (db:setup #t)))
	(debug:print-info 13 *default-log-port* "after db:setup with dbstruct="dbstruct)
	(cond
	 ((dbr:dbstruct-read-only dbstruct)
	  (debug:print-info 13 *default-log-port* "loading read-only watchdog")
	  (common:readonly-watchdog dbstruct))
	 (else
	  (debug:print-info 13 *default-log-port* "loading writable-watchdog.")
	  (common:writable-watchdog dbstruct)))
	(debug:print-info 13 *default-log-port* "watchdog done."))
      (debug:print-info 13 *default-log-port* "no need for watchdog on non-homehost")))


(define (std-exit-procedure)
  (on-exit (lambda () 0))
  ;;(debug:print-info 13 *default-log-port* "std-exit-procedure called; *time-to-exit*="*time-to-exit*)
  (let ((no-hurry  (if *time-to-exit* ;; hurry up
		       #f
		       (begin
			 (set! *time-to-exit* #t)
			 #t))))
    (debug:print-info 4 *default-log-port* "starting exit process, finalizing databases.")
    (if (and no-hurry (debug:debug-mode 18))
	(rmt:print-db-stats))
    (let ((th1 (make-thread (lambda () ;; thread for cleaning up, give it five seconds
                              (if *dbstruct-db* (db:close-all *dbstruct-db*)) ;; one second allocated
			      (if *task-db*    
				  (let ((db (cdr *task-db*)))
				    (if (sqlite3:database? db)
					(begin
					  (sqlite3:interrupt! db)
					  (sqlite3:finalize! db #t)
					  ;; (vector-set! *task-db* 0 #f)
					  (set! *task-db* #f)))))
                              (if (and *runremote*
                                       (remote-conndat *runremote*))
                                  (begin
                                    (http-client#close-all-connections!))) ;; for http-client
                              (if (not (eq? *default-log-port* (current-error-port)))
                                  (close-output-port *default-log-port*))
			      (set! *default-log-port* (current-error-port))) "Cleanup db exit thread"))
	  (th2 (make-thread (lambda ()
			      (debug:print 4 *default-log-port* "Attempting clean exit. Please be patient and wait a few seconds...")
			      (if no-hurry
                                  (begin
                                    (thread-sleep! 5)) ;; give the clean up few seconds to do it's stuff
                                  (begin
      				  (thread-sleep! 2)))
      			      (debug:print 4 *default-log-port* " ... done")
      			      )
			    "clean exit")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1)
      )
    )

  0)

(define (std-signal-handler signum)
  ;; (signal-mask! signum)
  (set! *time-to-exit* #t)
  ;;(debug:print-info 13 *default-log-port* "got signal "signum)
  (debug:print-error 0 *default-log-port* "Received signal " signum " exiting promptly")
  ;; (std-exit-procedure) ;; shouldn't need this since we are exiting and it will be called anyway
  (exit))

(set-signal-handler! signal/int  std-signal-handler)  ;; ^C
(set-signal-handler! signal/term std-signal-handler)
;; (set-signal-handler! signal/stop std-signal-handler)  ;; ^Z NO, do NOT handle ^Z!

;;======================================================================
;; M I S C   U T I L S
;;======================================================================

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

;; return first path that can be created or already exists and is writable
;;
(define (common:get-create-writeable-dir dirs)
  (if (null? dirs)
      #f
      (let loop ((hed (car dirs))
		 (tal (cdr dirs)))
	(let ((res (or (and (directory? hed)
			    (file-write-access? hed)
			    hed)
		       (handle-exceptions
			exn
			#f
			(create-directory hed #t)))))
	  (if (and (string? res)
		   (directory? res))
	      res
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal))))))))

;; return the youngest timestamp . filename
;;
(define (common:get-youngest glob-list)
  (let ((all-files (apply append
			  (map (lambda (patt)
				 (handle-exceptions
				     exn
				     '()
				   (glob patt)))
			       glob-list))))
    (fold (lambda (fname res)
	    (let ((last-mod (car res))
		  (curmod   (handle-exceptions
				exn
				0
			      (file-modification-time fname))))
	      (if (> curmod last-mod)
		  (list curmod fname)
		  res)))
	  '(0 "n/a")
	  all-files)))

;; use bash to expand a glob. Does NOT handle paths with spaces!
;;
(define (common:bash-glob instr)
  (string-split
   (with-input-from-pipe
       (conc "/bin/bash -c \"echo " instr "\"")
     read-line)))
  
;;======================================================================
;; T A R G E T S  ,   S T A T E ,   S T A T U S ,   
;;                    R U N N A M E    A N D   T E S T P A T T
;;======================================================================

;; (map print (map car (hash-table->alist (read-config "runconfigs.config" #f #t))))
;;
(define (common:get-runconfig-targets #!key (configf #f))
  (let ((targs       (sort (map car (hash-table->alist
				     (or configf ;; NOTE: There is no value in using runconfig:read here.
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

;; Lookup a value in runconfigs based on -reqtarg or -target
;; 
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
  (let* (;; (tagexpr       (args:get-arg "-tagexpr"))
         ;; (tags-testpatt (if tagexpr (string-join (runs:get-tests-matching-tags tagexpr) ",") #f))
         (testpatt-key  (if (args:get-arg "--modepatt") (args:get-arg "--modepatt") "TESTPATT"))
         (args-testpatt (or (args:get-arg "-testpatt") (args:get-arg "-runtests") "%"))
         (rtestpatt     (if rconf (runconfigs-get rconf testpatt-key) #f)))
    (cond
     ((args:get-arg "--modepatt") ;; modepatt is a forced setting, when set it MUST refer to an existing PATT in the runconfig
      (if rconf
	  (runconfigs-get rconf testpatt-key)
	  #f))     ;; We do NOT fall back to "%"
     ;; (tags-testpatt
     ;;  (debug:print-info 0 *default-log-port* "-tagexpr "tagexpr" selects testpatt "tags-testpatt)
     ;;  tags-testpatt)
     ((and (equal? args-testpatt "%") rtestpatt)
      (debug:print-info 0 *default-log-port* "testpatt defined in "testpatt-key" from runconfigs: " rtestpatt)
      rtestpatt)
     (else args-testpatt))))



(define (common:false-on-exception thunk #!key (message #f))
  (handle-exceptions exn
                     (begin
                       (if message
                           (debug:print-info 0 *default-log-port* message))
                       #f) (thunk) ))

(define (common:file-exists? path-string)
  ;; this avoids stack dumps in the case where 

  ;;;; TODO: catch permission denied exceptions and emit appropriate warnings, eg:  system error while trying to access file: "/nfs/pdx/disks/icf_env_disk001/bjbarcla/gwa/issues/mtdev/randy-slow/reproduce/q...
  (common:false-on-exception (lambda () (file-exists? path-string))
                             message: (conc "Unable to access path: " path-string)
                             ))

(define (common:directory-exists? path-string)
  ;;;; TODO: catch permission denied exceptions and emit appropriate warnings, eg:  system error while trying to access file: "/nfs/pdx/disks/icf_env_disk001/bjbarcla/gwa/issues/mtdev/randy-slow/reproduce/q...
  (common:false-on-exception (lambda () (directory-exists? path-string))
                             message: (conc "Unable to access path: " path-string)
                             ))

;; does the directory exist and do we have write access?
;;
;;    returns the directory or #f
;;
(define (common:directory-writable? path-string)
  (handle-exceptions
   exn
   #f
   (if (and (directory-exists? path-string)
            (file-write-access? path-string))
       path-string
       #f)))

(define (common:get-linktree)
  (or (getenv "MT_LINKTREE")
      (if *configdat*
	  (configf:lookup *configdat* "setup" "linktree")
	  (if *toppath*
	      (conc *toppath* "/lt")
	      #f))))

(define (common:args-get-runname)
  (let ((res (or (args:get-arg "-runname")
		 (args:get-arg ":runname")
		 (getenv "MT_RUNNAME"))))
    ;; (if res (set-environment-variable "MT_RUNNAME" res)) ;; not sure if this is a good idea. side effect and all ...
    res))

(define (common:get-fields cfgdat)
  (let ((fields (hash-table-ref/default cfgdat "fields" '())))
    (map car fields)))

(define (common:args-get-target #!key (split #f)(exit-if-bad #f))
  (let* ((keys    (if (hash-table? *configdat*) (common:get-fields *configdat*) '()))
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
	      (if exit-if-bad (exit 1))
	      #f)
	    #f))))

;; looking only (at least for now) at the MT_ variables craft the full testname
;;
(define (common:get-full-test-name)
  (if (getenv "MT_TEST_NAME")
      (if (and (getenv "MT_ITEMPATH")
               (not (equal? (getenv "MT_ITEMPATH") "")))
          (getenv "MT_TEST_NAME")
          (conc (getenv "MT_TEST_NAME") "/" (getenv "MT_ITEMPATH")))
      #f))

;; logic for getting homehost. Returns (host . at-home)
;; IF *toppath* is not set, wait up to five seconds trying every two seconds
;; (this is to accomodate the watchdog)
;;
(define (common:get-homehost #!key (trynum 5))
  ;; called often especially at start up. use mutex to eliminate collisions
  (mutex-lock! *homehost-mutex*)
  (cond
   (*home-host*
    (mutex-unlock! *homehost-mutex*)
    *home-host*)
   ((not *toppath*)
    (mutex-unlock! *homehost-mutex*)
    (launch:setup) ;; safely mutexed now
    (if (> trynum 0)
	(begin
	  (thread-sleep! 2)
	  (common:get-homehost trynum: (- trynum 1)))
	#f))
   (else
    (let* ((currhost (get-host-name))
	   (bestadrs (server:get-best-guess-address currhost))
	   ;; first look in config, then look in file .homehost, create it if not found
	   (homehost (or (configf:lookup *configdat* "server" "homehost" )
			 (handle-exceptions
			     exn
			     (if (> trynum 0)
				 (let ((delay-time (* (- 5 trynum) 5)))
				   (mutex-unlock! *homehost-mutex*)
				   (debug:print 0 *default-log-port* "ERROR: Failed to read .homehost file, delaying " delay-time " seconds and trying again, message: "  ((condition-property-accessor 'exn 'message) exn))
				   (thread-sleep! delay-time)
				   (common:get-homehost trynum: (- trynum 1)))
				 (begin
				   (mutex-unlock! *homehost-mutex*)
				   (debug:print 0 *default-log-port* "ERROR: Failed to read .homehost file after trying five times. Giving up and exiting, message: "  ((condition-property-accessor 'exn 'message) exn))
				   (exit 1)))
			   (let ((hhf (conc *toppath* "/.homehost")))
			     (if (file-exists? hhf)
				 (with-input-from-file hhf read-line)
				 (if (file-write-access? *toppath*)
				     (begin
				       (with-output-to-file hhf
					 (lambda ()
					   (print bestadrs)))
				       (begin
					 (mutex-unlock! *homehost-mutex*)
					 (car (common:get-homehost))))
				     #f))))))
	   (at-home  (or (equal? homehost currhost)
			 (equal? homehost bestadrs))))
      (set! *home-host* (cons homehost at-home))
      (mutex-unlock! *homehost-mutex*)
      *home-host*))))

;; am I on the homehost?
;;
(define (common:on-homehost?)
  (let ((hh (common:get-homehost)))
    (if hh
	(cdr hh)
	#f)))

;; do we honor the caches of the config files?
;;
(define (common:use-cache?)
  (let ((res #t)) ;; priority by order of evaluation
    (if *configdat*
	(if (equal? (configf:lookup *configdat* "setup" "use-cache") "no")
	    (set! res #f)
	    (if (equal? (configf:lookup *configdat* "setup" "use-cache") "yes")
		(set! res #t))))
    (if (args:get-arg "-no-cache")(set! res #f)) ;; overrides setting in "setup"
    (if (getenv "MT_USE_CACHE")
	(if (equal? (getenv "MT_USE_CACHE") "yes")
	    (set! res #t)
	    (if (equal? (getenv "MT_USE_CACHE") "no")
		(set! res #f))))    ;; overrides -no-cache switch
    res))
  
;; force use of server?
;;
(define (common:force-server?)
  (let* ((force-setting (configf:lookup *configdat* "server" "force"))
	 (force-type    (if force-setting (string->symbol force-setting) #f))
	 (force-result  (case force-type
			  ((#f)     #f)
			  ((always) #t)
			  ((test)   (if (args:get-arg "-execute") ;; we are in a test
					#t
					#f))
			  (else
			   (debug:print 0 *default-log-port* "ERROR: Bad server force setting " force-setting ", forcing server.")
			   #t)))) ;; default to requiring server
    (if force-result
	(begin
	  (debug:print-info 0 *default-log-port* "forcing use of server, force setting is \"" force-setting "\".")
	  #t)
	#f)))

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

;; get min or max, use > for max and < for min, this works around the limits on apply
;;
(define (common:sum lst)
  (if (null? lst)
      0
      (fold (lambda (a b)
	      (+ a b))
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

;; find timestamp of newest file associated with a sqlite db file
(define (common:lazy-sqlite-db-modification-time fpath)
  (let* ((glob-list (handle-exceptions
			exn
			`(,(conc "/no/such/file, message: " ((condition-property-accessor 'exn 'message) exn)))
		      (glob (conc fpath "*"))))
         (file-list (if (eq? 0 (length glob-list))
			'("/no/such/file")
			glob-list)))
  (apply max
   (map
    common:lazy-modification-time 
    file-list))))

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

;; get normalized cpu load by reading from /proc/loadavg and /proc/cpuinfo return all three values and the number of real cpus and the number of threads
;; returns alist '((adj-cpu-load . normalized-proc-load) ... etc.
;;  keys: adj-proc-load, adj-core-load, 1m-load, 5m-load, 15m-load
;;
(define (common:get-normalized-cpu-load remote-host)
  (let ((data (if remote-host
                  (with-input-from-pipe 
                   (conc "ssh " remote-host " cat /proc/loadavg;cat /proc/cpuinfo;echo end")
                   read-lines)
                  (append 
                   (with-input-from-file "/proc/loadavg" 
                     read-lines)
                   (with-input-from-file "/proc/cpuinfo"
                     read-lines)
                   (list "end"))))
        (load-rx  (regexp "^([\\d\\.]+)\\s+([\\d\\.]+)\\s+([\\d\\.]+)\\s+.*$"))
        (proc-rx  (regexp "^processor\\s+:\\s+(\\d+)\\s*$"))
        (core-rx  (regexp "^core id\\s+:\\s+(\\d+)\\s*$"))
        (phys-rx  (regexp "^physical id\\s+:\\s+(\\d+)\\s*$"))
        (max-num  (lambda (p n)(max (string->number p) n))))
    ;; (print "data=" data)
    (if (null? data) ;; something went wrong
        #f
        (let loop ((hed      (car data))
                   (tal      (cdr data))
                   (loads    #f)
                   (proc-num 0)  ;; processor includes threads
                   (phys-num 0)  ;; physical chip on motherboard
                   (core-num 0)) ;; core
          ;; (print hed ", " loads ", " proc-num ", " phys-num ", " core-num)
          (if (null? tal) ;; have all our data, calculate normalized load and return result
              (let* ((act-proc (+ proc-num 1))
                     (act-phys (+ phys-num 1))
                     (act-core (+ core-num 1))
                     (adj-proc-load (/ (car loads) act-proc))
                     (adj-core-load (/ (car loads) act-core)))
                (append (list (cons 'adj-proc-load adj-proc-load)
                              (cons 'adj-core-load adj-core-load))
                        (list (cons '1m-load (car loads))
                              (cons '5m-load (cadr loads))
                              (cons '15m-load (caddr loads)))
                        (list (cons 'proc act-proc)
                              (cons 'core act-core)
                              (cons 'phys act-phys))))
              (regex-case
               hed
               (load-rx  ( x l1 l5 l15 ) (loop (car tal)(cdr tal)(map string->number (list l1 l5 l15)) proc-num phys-num core-num))
               (proc-rx  ( x p         ) (loop (car tal)(cdr tal) loads           (max-num p proc-num) phys-num core-num))
               (phys-rx  ( x p         ) (loop (car tal)(cdr tal) loads           proc-num (max-num p phys-num) core-num))
               (core-rx  ( x c         ) (loop (car tal)(cdr tal) loads           proc-num phys-num (max-num c core-num)))
               (else 
                (begin
                  ;; (print "NO MATCH: " hed)
                  (loop (car tal)(cdr tal) loads proc-num phys-num core-num)))))))))

(define (common:unix-ping hostname)
  (let ((res (system (conc "ping -c 1 " hostname " > /dev/null"))))
    (eq? res 0)))

;; ideally put all this info into the db, no need to preserve it across moving homehost
;;
;; return list of
;;  ( reachable? cpuload update-time )
(define (common:get-host-info hostname)
  (let* ((loadinfo (rmt:get-latest-host-load hostname))
         (load (car loadinfo))
         (load-sample-time (cdr loadinfo))
         (load-sample-age (- (current-seconds) load-sample-time))
         (loadinfo-timeout-seconds 20)
         (host-last-update-timeout-seconds 10)
         (host-rec (hash-table-ref/default *host-loads* hostname #f))
         )
    (cond
     ((< load-sample-age loadinfo-timeout-seconds)
      (list #t
            load-sample-time
            load))
     ((and host-rec
           (< (current-seconds) (+ (host-last-update host-rec) host-last-update-timeout-seconds)))
      (list #t
            (host-last-update host-rec)
            (host-last-cpuload host-rec )))
     ((common:unix-ping hostname)
      (list #t
            (current-seconds)
            (alist-ref 'adj-core-load (common:get-normalized-cpu-load hostname))))
     (else
      (list #f 0 -1)))))
    
(define (common:update-host-loads-table hosts-raw)
  (let* ((hosts (filter (lambda (x)
                          (string-match (regexp "^\\S+$") x))
                        hosts-raw)))
    (for-each
     (lambda (hostname)
       (let* ((rec       (let ((h (hash-table-ref/default *host-loads* hostname #f)))
                          (if h
                              h
                              (let ((h (make-host)))
                                (hash-table-set! *host-loads* hostname h)
                                h))))
              (host-info         (common:get-host-info hostname))
              (is-reachable      (car host-info))
              (last-reached-time (cadr host-info))
              (load              (caddr host-info)))
         (host-reachable-set!    rec is-reachable)
         (host-last-update-set!  rec last-reached-time)
         (host-last-cpuload-set! rec load)))
     hosts)))

(define (common:get-least-loaded-host hosts-raw)
  (let* ((hosts (filter (lambda (x)
                          (string-match (regexp "^\\S+$") x))
                        hosts-raw))
         (best-host #f)
         (best-load 99999)
         (curr-time (current-seconds)))
    (common:update-host-loads-table hosts)
    (for-each
     (lambda (hostname)
       (let* ((rec
               (let ((h (hash-table-ref/default *host-loads* hostname #f)))
                 (if h
                     h
                     (let ((h (make-host)))
                       (hash-table-set! *host-loads* hostname h)
                       h))))
              (reachable (host-reachable rec))
              (load      (host-last-cpuload   rec)))
         (cond
          ((not reachable) #f)
          ((< (+ load (/ (random 250) 1000))         ;; add a random factor to keep from getting in a rut
              (+ best-load (/ (random 250) 1000))  )
           (set! best-load load)
           (set! best-host hostname)))))
     hosts)
    best-host))




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

(define (common:check-space-in-dir dirpath required)
  (let* ((dbspace  (if (directory? dirpath)
		       (get-df dirpath)
		       0)))
    (list (> dbspace required)
	  dbspace
	  required
	  dirpath)))

;; check space in dbdir and in megatest dir
;; returns: ok/not dbspace required-space
;;
(define (common:check-db-dir-space)
  (let* ((required (string->number 
		    (or (configf:lookup *configdat* "setup" "dbdir-space-required")
			"100000")))
	 (dbdir    (common:get-db-tmp-area)) ;; (db:get-dbdir))
	 (tdbspace (common:check-space-in-dir dbdir required))
	 (mdbspace (common:check-space-in-dir *toppath* required)))
    (sort (list tdbspace mdbspace) (lambda (a b)
				     (< (cadr a)(cadr b))))))
    
;; check available space in dbdir, exit if insufficient
;;
(define (common:check-db-dir-and-exit-if-insufficient)
  (let* ((spacedat (car (common:check-db-dir-space))) ;; look only at worst for now
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
(define (bb-check-path #!key (msg "check-path: "))
  (let ((path (or (get-environment-variable "PATH") "none")))
    (debug:print-info 0 *default-log-port* (conc msg" : $PATH="path))
    (if (string-match "^.*/isoenv-core/.*" path)
        (debug:print-error 0 *default-log-port* (conc msg" : !!ISOENV PRESENT!!")) ;; remove for prod
        (debug:print-info 1 *default-log-port* (conc msg" : **no isoenv present**")))))

	      
(define (save-environment-as-files fname #!key (ignorevars (list "USER" "HOME" "DISPLAY" "LS_COLORS" "XKEYSYMDB" "EDITOR" "MAKEFLAGS" "MAKEF" "MAKEOVERRIDES")))
  ;;(bb-check-path msg: "save-environment-as-files entry")
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
			(print (if (or (member key ignorevars)
				       (string-search ":" key)) ;; internal only values to be skipped.
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

;; basic ISO8601 format (e.g. "2017-02-28 06:02:54") date time => Unix epoch
;;
(define (common:date-time->seconds datetime)
  (local-time->seconds (string->time datetime "%Y-%m-%d %H:%M:%S")))

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

;; given x y lim return the cron expansion
;;
(define (common:expand-cron-slash x y lim)
  (let loop ((curr x)
	     (res  `()))
    (if (< curr lim)
	(loop (+ curr y) (cons curr res))
	(reverse res))))

;; expand a complex cron string to a list of cron strings
;;
;;  x/y   => x, x+y, x+2y, x+3y while x+Ny<max_for_field
;;  a,b,c => a, b ,c
;;
;;   NOTE: with flatten a lot of the crud below can be factored down.
;;
(define (common:cron-expand cron-str)
  (if (list? cron-str)
      (flatten
       (fold (lambda (x res)
	       (if (list? x)
		   (let ((newres (map common:cron-expand x)))
		     (append x newres))
		   (cons x res)))
	     '()
	     cron-str)) ;; (map common:cron-expand cron-str))
      (let ((cron-items (string-split cron-str))
	    (slash-rx   (regexp "(\\d+)/(\\d+)"))
	    (comma-rx   (regexp ".*,.*"))
	    (max-vals   '((min        . 60)
			  (hour       . 24)
			  (dayofmonth . 28) ;;; BUG!!!! This will be a bug for some combinations
			  (month      . 12)
			  (dayofweek  . 7))))
	(if (< (length cron-items) 5) ;; bad spec
	    cron-str ;; `(,cron-str)              ;; just return the string, something downstream will fix it
	    (let loop ((hed  (car cron-items))
		       (tal  (cdr cron-items))
		       (type 'min)
		       (type-tal '(hour dayofmonth month dayofweek))
		       (res  '()))
	      (regex-case
		  hed
		(slash-rx ( _ base incr ) (let* ((basen          (string->number base))
						 (incrn          (string->number incr))
						 (expanded-vals  (common:expand-cron-slash basen incrn (alist-ref type max-vals)))
						 (new-list-crons (fold (lambda (x myres)
									 (cons (conc (if (null? res)
											 ""
											 (conc (string-intersperse res " ") " "))
										     x " " (string-intersperse tal " "))
									       myres))
								       '() expanded-vals)))
					    ;; (print "new-list-crons: " new-list-crons)
					    ;; (fold (lambda (x res)
					    ;; 	    (if (list? x)
					    ;; 		(let ((newres (map common:cron-expand x)))
					    ;; 		  (append x newres))
					    ;; 		(cons x res)))
					    ;; 	  '()
					    (flatten (map common:cron-expand new-list-crons))))
		;;					    (map common:cron-expand (map common:cron-expand new-list-crons))))
		(else (if (null? tal)
			  cron-str
			  (loop (car tal)(cdr tal)(car type-tal)(cdr type-tal)(append res (list hed)))))))))))
		      
	    
;; given a cron string and the last time event was processed return #t to run or #f to not run
;;
;;  min    hour   dayofmonth month  dayofweek
;; 0-59    0-23   1-31       1-12   0-6          ### NOTE: dayofweek does not include 7
;;
;;  #t => yes, run the job
;;  #f => no, do not run the job
;;
(define (common:cron-event cron-str now-seconds-in last-done) ;; ref-seconds = #f is NOW.
  (let* ((cron-items     (map string->number (string-split cron-str)))
	 (now-seconds    (or now-seconds-in (current-seconds)))
	 (now-time       (seconds->local-time now-seconds))
	 (last-done-time (seconds->local-time last-done))
	 (all-times      (make-hash-table)))
    ;; (print "cron-items: " cron-items "(length cron-items): " (length cron-items))
    (if (not (eq? (length cron-items) 5)) ;; don't even try to figure out junk strings
	#f
	(match-let (((     cmin chour cdayofmonth cmonth    cdayofweek)
		     cron-items)
		    ;; 0     1    2        3         4    5      6
		    ((nsec nmin nhour ndayofmonth nmonth nyr ndayofweek n7 n8 n9)
		     (vector->list now-time))
		    ((lsec lmin lhour ldayofmonth lmonth lyr ldayofweek l7 l8 l9)
		     (vector->list last-done-time)))
	  ;; create all possible time slots
	  ;; remove invalid slots due to (for example) day of week
	  ;; get the start and end entries for the ref-seconds (current) time
	  ;; if last-done > ref-seconds => this is an ERROR!
	  ;; does the last-done time fall in the legit region?
	  ;;    yes => #f  do not run again this command
	  ;;    no  => #t  ok to run the command
	  (for-each ;; month
	   (lambda (month)
	     (for-each ;; dayofmonth
	      (lambda (dom)
		(for-each
		 (lambda (hr) ;; hour
		   (for-each
		    (lambda (minute) ;; minute
		      (let ((copy-now (apply vector (vector->list now-time))))
			(vector-set! copy-now 0 0) ;; force seconds to zero
			(vector-set! copy-now 1 minute)
			(vector-set! copy-now 2 hr)
			(vector-set! copy-now 3 dom)  ;; dom is already corrected for zero referenced
			(vector-set! copy-now 4 month)
			(let* ((copy-now-secs (local-time->seconds copy-now))
			       (new-copy      (seconds->local-time copy-now-secs))) ;; remake the time vector
			  (if (or (not cdayofweek)
				  (equal? (vector-ref new-copy 6)
					  cdayofweek)) ;; if the day is specified and a match OR if the day is NOT specified
			      (if (or (not cdayofmonth)
				      (equal? (vector-ref new-copy 3)
					      (+ 1 cdayofmonth))) ;; if the month is specified and a match OR if the month is NOT specified
				  (hash-table-set! all-times copy-now-secs new-copy))))))
		    (if cmin
			`(,cmin)  ;; if given cmin, have to use it
			(list (- nmin 1) nmin (+ nmin 1))))) ;; minute
		 (if chour
		     `(,chour)
		     (list (- nhour 1) nhour (+ nhour 1))))) ;; hour
	      (if cdayofmonth
		  `(,cdayofmonth)
		  (list (- ndayofmonth 1) ndayofmonth (+ ndayofmonth 1)))))
	   (if cmonth
	       `(,cmonth)
	       (list (- nmonth 1) nmonth (+ nmonth 1))))
	  (let ((before #f)
		(is-in  #f))
	    (for-each
	     (lambda (moment)
	       (if (and before
			(<= before now-seconds)
			(>= moment now-seconds))
		   (begin
		     ;; (print)
		     ;; (print "Before: " (time->string (seconds->local-time before)))
		     ;; (print "Now:    " (time->string (seconds->local-time now-seconds)))
		     ;; (print "After:  " (time->string (seconds->local-time moment)))
		     ;; (print "Last:   " (time->string (seconds->local-time last-done)))
		     (if (<  last-done before)
			 (set! is-in before))
		     ))
	       (set! before moment))
	     (sort (hash-table-keys all-times) <))
	    is-in)))))

(define (common:extended-cron  cron-str now-seconds-in last-done)
  (let ((expanded-cron (common:cron-expand cron-str)))
    (if (string? expanded-cron)
	(common:cron-event expanded-cron now-seconds-in last-done)
	(let loop ((hed (car expanded-cron))
		   (tal (cdr expanded-cron)))
	  (if (common:cron-event hed now-seconds-in last-done)
	      #t
	      (if (null? tal)
		  #f
		  (loop (car tal)(cdr tal))))))))

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

(define (common:faux-lock keyname)
  (if (rmt:get-var keyname)
      #f
      (begin
        (rmt:set-var keyname (conc (current-process-id)))
        (equal? (conc (current-process-id)) (conc (rmt:get-var keyname))))))

(define (common:faux-unlock keyname #!key (force #f))
  (if (or force (equal? (conc (current-process-id)) (conc (rmt:get-var keyname))))
      (begin
        (if (rmt:get-var keyname) (rmt:del-var keyname))
        #t)
      #f))

  
(define (common:in-running-test?)
  (and (args:get-arg "-execute") (get-environment-variable "MT_CMDINFO")))

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

;; ;;======================================================================
;; ;; N A N O M S G   C L I E N T
;; ;;======================================================================
;; 
;; 
;; 
;; (define (common:send-dboard-main-changed)
;;   (let* ((dashboard-ips (mddb:get-dashboards)))
;;     (for-each
;;      (lambda (ipadr)
;;        (let* ((soc (common:open-nm-req (conc "tcp://" ipadr)))
;; 	      (msg (conc "main " *toppath*))
;; 	      (res (common:nm-send-receive-timeout soc msg)))
;; 	 (if (not res) ;; couldn't reach that dashboard - remove it from db
;; 	     (print "ERROR: couldn't reach dashboard " ipadr))
;; 	 res))
;;      dashboard-ips)))
;;     
;;     
;; ;;======================================================================
;; ;; D A S H B O A R D   D B 
;; ;;======================================================================
;; 
;; (define (mddb:open-db)
;;   (let* ((db (open-database (conc (get-environment-variable "HOME") "/.dashboard.db"))))
;;     (set-busy-handler! db (busy-timeout 10000))
;;     (for-each
;;      (lambda (qry)
;;        (exec (sql db qry)))
;;      (list 
;;       "CREATE TABLE IF NOT EXISTS vars       (id INTEGER PRIMARY KEY,key TEXT, val TEXT, CONSTRAINT varsconstraint UNIQUE (key));"
;;       "CREATE TABLE IF NOT EXISTS dashboards (
;;           id         INTEGER PRIMARY KEY,
;;           pid        INTEGER,
;;           username   TEXT,
;;           hostname   TEXT,
;;           ipaddr     TEXT,
;;           portnum    INTEGER,
;;           start_time TIMESTAMP DEFAULT (strftime('%s','now')),
;;              CONSTRAINT hostport UNIQUE (hostname,portnum)
;;         );"
;;       ))
;;     db))
;; 
;; ;; register a dashboard 
;; ;;
;; (define (mddb:register-dashboard port)
;;   (let* ((pid      (current-process-id))
;; 	 (hostname (get-host-name))
;; 	 (ipaddr   (server:get-best-guess-address hostname))
;; 	 (username (current-user-name)) ;; (car userinfo)))
;; 	 (db      (mddb:open-db)))
;;     (print "Register monitor, pid: " pid ", hostname: " hostname ", port: " port ", username: " username)
;;     (exec (sql db "INSERT OR REPLACE INTO dashboards (pid,username,hostname,ipaddr,portnum) VALUES (?,?,?,?,?);")
;; 	   pid username hostname ipaddr port)
;;     (close-database db)))
;; 
;; ;; unregister a monitor
;; ;;
;; (define (mddb:unregister-dashboard host port)
;;   (let* ((db      (mddb:open-db)))
;;     (print "Register unregister monitor, host:port=" host ":" port)
;;     (exec (sql db "DELETE FROM dashboards WHERE hostname=? AND portnum=?;") host port)
;;     (close-database db)))
;; 
;; ;; get registered dashboards
;; ;;
;; (define (mddb:get-dashboards)
;;   (let ((db (mddb:open-db)))
;;     (query fetch-column
;; 	   (sql db "SELECT ipaddr || ':' || portnum FROM dashboards;"))))
    
;;======================================================================
;;  T E S T   L A U N C H I N G   P E R   I T E M   W I T H   H O S T   T Y P E S
;;======================================================================
;; 
;; [hosts]
;; arm cubie01 cubie02
;; x86_64 zeus xena myth01
;; allhosts #{g hosts arm} #{g hosts x86_64}
;; 
;; [host-types]
;; general #MTLOWESTLOAD #{g hosts allhosts}
;; arm     #MTLOWESTLOAD #{g hosts arm}
;; nbgeneral nbjob run JOBCOMMAND -log $MT_LINKTREE/$MT_TARGET/$MT_RUNNAME.$MT_TESTNAME-$MT_ITEM_PATH.lgo
;; 
;; [launchers]
;; envsetup general
;; xor/%/n 4C16G
;; % nbgeneral
;; 
;; [jobtools]
;; # if defined and not "no" flexi-launcher will bypass "launcher" unless no match.
;; flexi-launcher yes  
;; launcher nbfake
;;
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
			      (let* ((launcher-parts (string-split launcher))
				     (launcher-exe   (car launcher-parts)))
				(if (equal? launcher-exe "#MTLOWESTLOAD") ;; this is our special case, we will find the lowest load and craft a nbfake commandline
				    (let ((targ-host (common:get-least-loaded-host (cdr launcher-parts))))
				      (conc "remrun " targ-host))
				    launcher))
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

;;======================================================================
;; Manage pkts, used in servers, tests and likely other contexts so put
;; in common
;;======================================================================

(define common:pkts-spec
  '((default . ((parent    . P)))
    (server  . ((action    . a)
		(pid       . d)
		(ipaddr    . i)
		(port      . p)
		(parent    . P)))
    			  
    (test    . ((cpuuse    . c)
		(diskuse   . d)
		(item-path . i)
		(runname   . r)
		(state     . s)
		(target    . t)
		(status    . u)
		(parent    . P)))))

(define (common:get-pkts-dirs mtconf use-lt)
  (let* ((pktsdirs-str (or (configf:lookup mtconf "setup"  "pktsdirs")
			   (and use-lt
				(conc (or *toppath*
					  (current-directory))
				      "/lt/.pkts"))))
	 (pktsdirs  (if pktsdirs-str
			(string-split pktsdirs-str " ")
			#f)))
    pktsdirs))

(define (common:save-pkt pktalist-in mtconf use-lt)
  (let* ((parent   (hash-table-ref/default *pkts-info* 'last-parent #f))
	 (pktalist (if parent
		       (cons `(parent . ,parent)
			     pktalist-in)
		       pktalist-in)))
    (let-values (((uuid pkt)
		  (alist->pkt pktalist common:pkts-spec)))
      (hash-table-set! *pkts-info* 'last-parent uuid)
      (let ((pktsdir (or (hash-table-ref/default *pkts-info* 'pkts-dir #f)
			 (let* ((pktsdirs (common:get-pkts-dirs mtconf use-lt))
			       (pktsdir   (car pktsdirs))) ;; assume it is there
			   (hash-table-set! *pkts-info* 'pkts-dir pktsdir)
			   pktsdir))))
	(if (not (file-exists? pktsdir))
	    (create-directory pktsdir #t))
	(with-output-to-file
	    (conc pktsdir "/" uuid ".pkt")
	  (lambda ()
	    (print pkt)))))))
	
(define (common:with-queue-db mtconf proc #!key (use-lt #f)(toppath-in #f))
  (let* ((pktsdirs (common:get-pkts-dirs mtconf use-lt))
	 (pktsdir  (if pktsdirs (car pktsdirs) #f))
	 (toppath  (or (configf:lookup mtconf "scratchdat" "toppath")
		       toppath-in))
	 (pdbpath  (or (configf:lookup mtconf "setup"  "pdbpath") pktsdir)))
    (if (not (and  pktsdir toppath pdbpath))
	(begin
	  (print "ERROR: settings are missing in your megatest.config for area management.")
	  (print "  you need to have pktsdir in the [setup] section."))
	(let* ((pdb  (open-queue-db pdbpath "pkts.db"
				    schema: '("CREATE TABLE groups (id INTEGER PRIMARY KEY,groupname TEXT, CONSTRAINT group_constraint UNIQUE (groupname));"))))
	  (proc pktsdirs pktsdir pdb)
	  (dbi:close pdb)))))

(define (common:load-pkts-to-db mtconf #!key (use-lt #f))
  (common:with-queue-db
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
      pktsdirs))
   use-lt: use-lt))

(define (common:get-pkt-alists pkts)
  (map (lambda (x)
	 (alist-ref 'apkt x)) ;; 'pkta pulls out the alist from the read pkt
       pkts))

;; given list of pkts (alist mode) return list of D cards as Unix epoch, sorted descending
;; also delete duplicates by target i.e. (car pkt)
;;
(define (common:get-pkt-times pkts)
  (delete-duplicates
   (sort 
    (map (lambda (x)
	   `(,(alist-ref 't x) . ,(string->number (alist-ref 'D x))))
	 pkts)
    (lambda (a b)(> (cdr a)(cdr b))))      ;; sort descending
   (lambda (a b)(equal? (car a)(car b))))) ;; remove duplicates by target



