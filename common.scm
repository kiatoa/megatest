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

(use sqlite3 srfi-1 posix regex-case base64 format dot-locking csv-xml z3)
(require-extension sqlite3 regex posix)

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
       (debug:print 0 "ERROR: bad value for setenv, key=" key ", value=" val)
       (setenv key val))
      (debug:print 0 "ERROR: bad value for setenv, key=" key ", value=" val)))

(define home (getenv "HOME"))
(define user (getenv "USER"))

;; GLOBAL GLETCHES
(define-record megatest:area
  name               ;; area name
  path               ;; mt run area home
  transport          ;; defaults to http
  configinfo         ;; legacy config format
  configdat          ;; megatest config
  denoise            ;; focal point for not 
  client-signature   ;; key for client-server conversation
  remote             ;; hash of all the client side connnections
  run-keys           ;; target keys for this area
  runs               ;; used in dashboard
  read-only          ;; can I write to this area?
  )

(define *already-seen-runconfig-info* #f)
(define *waiting-queue*     (make-hash-table))
(define *test-meta-updated* (make-hash-table))
(define *globalexitstatus*  0) ;; attempt to work around possible thread issues
(define *passnum*           0) ;; when running track calls to run-tests or similar
(define *write-frequency*   (make-hash-table)) ;; run-id => (vector (current-seconds) 0))
(define *alt-log-file* #f)  ;; used by -log
(define *common:denoise*    (make-hash-table)) ;; for low noise printing

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
;; (define *transport-type*    'http)             ;; override with [server] transport http|rpc|nmsg
;; (define *runremote*         (make-hash-table)) ;; if set up for server communication this will hold <host port>

(define (common:get-remote remote run-id)
  (let ((ht (or remote *runremote*)))
    (if ht
	(hash-table-ref/default ht run-id #f)
	#f)))

(define (common:set-remote! remote run-id value)
  (let ((ht (or remote *runremote*)))
    (if ht
	(hash-table-set! ht run-id value))))

(define (common:del-remote! remote run-id)
  (let ((ht (or remote *runremote*)))
    (if ht
	(hash-table-delete! ht run-id))))

(define (common:get-remote-all remote)
  (let ((ht (or remote *runremote*)))
    (if ht
	(hash-table-keys ht)
	'())))

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
  (if (getenv "MT_MEGATEST") (getenv "MT_MEGATEST") "megatest"))

(define (common:read-encoded-string instr)
  (handle-exceptions
   exn
   (handle-exceptions
    exn
    (begin
      (debug:print 0 "ERROR: received bad encoded string \"" instr "\", message: " ((condition-property-accessor 'exn 'message) exn))
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
	(with-input-from-file fname
	  (lambda ()
	    (equal? key-string (read-line)))))))
	
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

(define (common:get-testsuite-name area-dat)
  (or (configf:lookup (megatest:area-configdat area-dat) "setup" "testsuite" )
       (pathname-file (megatest:area-path      area-dat))))

;;======================================================================
;; E X I T   H A N D L I N G
;;======================================================================

(define (common:legacy-sync-recommended)
  (or (args:get-arg "-runtests")
      (args:get-arg "-server")
      (args:get-arg "-set-run-status")
      (args:get-arg "-remove-runs")
      (args:get-arg "-get-run-status")
      ))

(define (common:legacy-sync-required)
  (configf:lookup *configdat* "setup" "megatest-db"))

(define (std-exit-procedure)
  (let ((no-hurry  (if *time-to-exit* ;; hurry up
		       #f
		       (begin
			 (set! *time-to-exit* #t)
			 #t)))
         (configdat (megatest:area-configdat area-dat))
	 (run-ids   (hash-table-keys *db-local-sync*)))
    (debug:print-info 4 "starting exit process, finalizing databases.")
    (if (and no-hurry (debug:debug-mode 18))
	(rmt:print-db-stats area-dat))
    (let ((th1 (make-thread (lambda () ;; thread for cleaning up, give it five seconds
			      (if (and (not (null? run-ids))
				       (configf:lookup configdat "setup" "megatest-db"))
				  (if no-hurry (db:multi-db-sync run-ids 'new2old)))
			      (if *dbstruct-db* (db:close-all *dbstruct-db* area-dat))
			      (if *inmemdb*     (db:close-all *inmemdb* area-dat))
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
					  (vector-set! *task-db* 0 #f)))))) "Cleanup db exit thread"))
	  (th2 (make-thread (lambda ()
			      (debug:print 4 "Attempting clean exit. Please be patient and wait a few seconds...")
			      (if no-hurry
				  (thread-sleep! 5) ;; give the clean up few seconds to do it's stuff
				  (thread-sleep! 2))
			      (debug:print 4 " ... done")
			      )
			    "clean exit")))
      (thread-start! th1)
      (thread-start! th2)
      (thread-join! th1))))

(define (std-signal-handler signum)
  ;; (signal-mask! signum)
  (set! *time-to-exit* #t)
  (debug:print 0 "ERROR: Received signal " signum " exiting promptly")
  ;; (std-exit-procedure) ;; shouldn't need this since we are exiting and it will be called anyway
  (exit))

(set-signal-handler! signal/int  std-signal-handler)  ;; ^C
(set-signal-handler! signal/term std-signal-handler)
(set-signal-handler! signal/stop std-signal-handler)  ;; ^Z

;;======================================================================
;; Misc utils
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
		       
(define (common:version-signature)
  (conc megatest-version "-" (substring megatest-fossil-hash 0 4)))

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
  (debug:print-info 8 "patt-list-match item=" item " patts=" patts)
  (if (and item patts)  ;; here we are filtering for matches with item patterns
      (let ((res #f))   ;; look through all the item-patts if defined, format is patt1,patt2,patt3 ... wildcard is %
	(for-each 
	 (lambda (patt)
	   (let ((modpatt (string-substitute "%" ".*" patt #t)))
	     (debug:print-info 10 "patt " patt " modpatt " modpatt)
	     (if (string-match (regexp modpatt) item)
		 (set! res #t))))
	 (string-split patts ","))
	res)
      #t))

;; (map print (map car (hash-table->alist (read-config "runconfigs.config" #f #t))))
(define (common:get-runconfig-targets #!key (configf #f))
  (sort (map car (hash-table->alist
		  (or configf
		      (read-config "runconfigs.config"
			       #f #t))))
	string<?))

;; '(print (string-intersperse (map cadr (hash-table-ref/default (read-config "megatest.config" \#f \#t) "disks" '"'"'("none" ""))) "\n"))'
(define (common:get-disks #!key (configf #f))
  (hash-table-ref/default 
   (or configf (read-config "megatest.config" #f #t))
   "disks" '("none" "")))

;;======================================================================
;; T A R G E T S
;;======================================================================

(define (common:args-get-target #!key (split #f))
  (let* ((target  (if (args:get-arg "-reqtarg")
		      (args:get-arg "-reqtarg")
		      (if (args:get-arg "-target")
			  (args:get-arg "-target")
			  (getenv "MT_TARGET"))))
	 (tlist   (if target (string-split target "/" #t) '()))
	 (valid   (if target
		      (and (not (null? tlist))
			   (null? (filter string-null? tlist)))
		      #f)))
    (if valid
	(if split
	    tlist
	    target)
	(if target
	    (begin
	      (debug:print 0 "ERROR: Invalid target, spaces or blanks not allowed \"" target "\"")
	      #f)
	    #f))))

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


;;======================================================================
;; Munge data into nice forms
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
	  ;; (debug:print-info 0 "Processing record: " hed )
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
;; System stuff
;;======================================================================

;; return a nice clean pathname made absolute
(define (nice-path dir)
  (normalize-pathname (if (absolute-pathname? dir)
			  dir
			  (conc (current-directory) "/" dir))))

(define (get-cpu-load)
  (car (common:get-cpu-load)))
;;   (let* ((load-res (cmd-run->list "uptime"))
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
(define (common:get-cpu-load)
  (with-input-from-file "/proc/loadavg" 
    (lambda ()(list (read)(read)(read)))))

(define (common:wait-for-cpuload maxload numcpus waitdelay #!key (count 1000))
  (let* ((loadavg (common:get-cpu-load))
	 (first   (car loadavg))
	 (next    (cadr loadavg))
	 (adjload (* maxload numcpus))
	 (loadjmp (- first next)))
    (cond
     ((and (> first adjload)
	   (> count 0))
      (debug:print-info 0 "waiting " waitdelay " seconds due to load " first " exceeding max of " adjload)
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1)))
     ((and (> loadjmp numcpus)
	   (> count 0))
      (debug:print-info 0 "waiting " waitdelay " seconds due to load jump " loadjmp " > numcpus " numcpus)
      (thread-sleep! waitdelay)
      (common:wait-for-cpuload maxload numcpus waitdelay count: (- count 1))))))

(define (common:get-num-cpus)
  (with-input-from-file "/proc/cpuinfo"
    (lambda ()
      (let loop ((numcpu 0)
		 (inl    (read-line)))
	(if (eof-object? inl)
	    numcpu
	    (loop (if (string-match "^processor\\s+:\\s+\\d+$" inl)
		      (+ numcpu 1)
		      numcpu)
		  (read-line)))))))

(define (get-uname . params)
  (let* ((uname-res (cmd-run->list (conc "uname " (if (null? params) "-a" (car params)))))
	 (uname #f))
    (if (null? (car uname-res))
	"unknown"
	(caar uname-res))))

;;======================================================================
;; D I S K   S P A C E 
;;======================================================================

(define (common:get-disk-space-used fpath)
  (with-input-from-pipe (conc "/usr/bin/du -s " fpath) read))

(define (get-df path)
  (let* ((df-results (cmd-run->list (conc "df " path)))
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
			    (if (common:low-noise-print 50 "disks not a dir " disk-num)
				(debug:print 0 "WARNING: disk " disk-num " at path " dirpath " is not a directory - ignoring it."))
			    -1)
			   ((not (file-write-access? dirpath))
			    (if (common:low-noise-print 50 "disks not writeable " disk-num)
				(debug:print 0 "WARNING: disk " disk-num " at path " dirpath " is not writeable - ignoring it."))
			    -1)
			   ((not (eq? (string-ref dirpath 0) #\/))
			    (if (common:low-noise-print 50 "disks not a proper path " disk-num)
				(debug:print 0 "WARNING: disk " disk-num " at path " dirpath " is not a fully qualified path - ignoring it."))
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
	      
(define (save-environment-as-files fname #!key (ignorevars (list "USER" "HOME" "DISPLAY" "LS_COLORS" "XKEYSYMDB" "EDITOR" "MAKEFLAGS" "MAKEF")))
  (let ((envvars (get-environment-variables))
        (whitesp (regexp "[^a-zA-Z0-9_\\-:,.\\/%$]")))
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
			       key " " delim val delim)))
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
			       key "=" delim val delim)))
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
		  
;;======================================================================
;; time and date nice to have stuff
;;======================================================================

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
   (seconds->local-time sec) "%yww%V.%w %H:%M"))

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

;;======================================================================
;; Colors
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
