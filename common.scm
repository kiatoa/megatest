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

(use srfi-1 posix regex-case base64 format dot-locking csv-xml z3 nanomsg sql-de-lite hostinfo)
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
       (debug:print 0 "ERROR: bad value for setenv, key=" key ", value=" val)
       (setenv key val))
      (debug:print 0 "ERROR: bad value for setenv, key=" key ", value=" val)))

(define home (getenv "HOME"))
(define user (getenv "USER"))

;; GLOBAL GLETCHES
(define *db-keys* #f)
(define *configinfo* #f)
(define *configdat*  #f)
(define *toppath*    #f)
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
  (or (getenv "MT_MEGATEST") "megatest"))

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

(define (common:get-testsuite-name)
  (or (configf:lookup *configdat* "setup" "testsuite" )
      (pathname-file *toppath*)))

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
			 #t))))
    (debug:print-info 4 "starting exit process, finalizing databases.")
    (if (and no-hurry (debug:debug-mode 18))
	(rmt:print-db-stats))
    (let ((th1 (make-thread (lambda () ;; thread for cleaning up, give it five seconds
			      (let ((run-ids (hash-table-keys *db-local-sync*)))
				(if (and (not (null? run-ids))
					 (configf:lookup *configdat* "setup" "megatest-db"))
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
;; M I S C   U T I L S
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
;; T A R G E T S  ,   S T A T E ,   S T A T U S ,   
;;                    R U N N A M E    A N D   T E S T P A T T
;;======================================================================

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
    (if rtestpatt (debug:print-info 0 "TESTPATT from runconfigs: " rtestpatt))
    testpatt))

(define (common:args-get-runname)
  (or (args:get-arg "-runname")
      (args:get-arg ":runname")))

(define (common:args-get-target #!key (split #f))
  (let* ((keys    (keys:config-get-fields *configdat*))
	 (numkeys (length keys))
	 (target  (if (args:get-arg "-reqtarg")
		      (args:get-arg "-reqtarg")
		      (if (args:get-arg "-target")
			  (args:get-arg "-target")
			  (getenv "MT_TARGET"))))
	 (tlist   (if target (string-split target "/" #t) '()))
	 (valid   (if target
		      (and (not (null? tlist))
			   (eq? numkeys (length tlist))
			   (null? (filter string-null? tlist)))
		      #f)))
    (if valid
	(if split
	    tlist
	    target)
	(if target
	    (begin
	      (debug:print 0 "ERROR: Invalid target, spaces or blanks not allowed \"" target "\", target should be: " (string-intersperse keys "/"))
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

;; for reasons I don't understand multiple calls to real-path in parallel threads
;; must be protected by mutexes
;;
(define (common:real-path inpath)
  ;; (cmd-run-with-stderr->list "readlink" "-f" inpath)) ;; cmd . params)
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
	      
(define (save-environment-as-files fname #!key (ignorevars (list "USER" "HOME" "DISPLAY" "LS_COLORS" "XKEYSYMDB" "EDITOR" "MAKEFLAGS" "MAKEF" "MAKEOVERRIDES")))
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

(define (common:open-nm-req addr)
  (let* ((req (nn-socket 'req))
	 (res (nn-connect req addr)))
    req))

;; (with-output-to-string (lambda ()(serialize obj)))
(define (common:nm-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))

(define (common:close-nm-req soc)
  (nn-close soc))

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
    
(define (common:nm-send-receive-timeout req msg)
  (let* ((key     "ping")
	 (success #f)
	 (keepwaiting #t)
	 (result  #f)
	 (sendrec (make-thread
		   (lambda ()
		     (nn-send req msg)
		     (set! result (nn-recv req))
		     (set! success #t))
		   "send-receive"))
	 (timeout (make-thread (lambda ()
				 (let loop ((count 0))
				   (thread-sleep! 1)
				   (print "still waiting after count seconds...")
				   (if (and keepwaiting (< count 10))
				       (loop (+ count 1))))
				 (if keepwaiting
				     (begin
				       (print "timeout waiting for reply")
				       (thread-terminate! sendrec))))
			       "timeout")))
    (handle-exceptions
     exn
     (begin
       (print-call-chain)
       (print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (print "exn=" (condition->list exn)))
     (thread-start! timeout)
     (thread-start! sendrec)
     (thread-join!  sendrec)
     (if success (thread-terminate! timeout)))
    result))
    
(define (common:ping-nm req)
  ;; send a random number and check that we get it back
  (let* ((key     "ping")
	 (success #f)
	 (keepwaiting #t)
	 (ping    (make-thread
		   (lambda ()
		     (print "ping: sending string \"" key "\", expecting " (current-process-id))
		     (nn-send req key)
		     (let ((result  (nn-recv req)))
		       (if (equal? (conc (current-process-id)) result)
			   (begin
			     (print "ping, success: received \"" result "\"")
			     (set! success #t))
			   (begin
			     (print "ping, failed: received key \"" result "\"")
			     (set! keepwaiting #f)
			     (set! success #f)))))
		   "ping"))
	 (timeout (make-thread (lambda ()
				 (let loop ((count 0))
				   (thread-sleep! 1)
				   (print "still waiting after count seconds...")
				   (if (and keepwaiting (< count 10))
				       (loop (+ count 1))))
				 (if keepwaiting
				     (begin
				       (print "timeout waiting for ping")
				       (thread-terminate! ping))))
			       "timeout")))
    (handle-exceptions
     exn
     (begin
       (print-call-chain)
       (print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
       (print "exn=" (condition->list exn))
       (print "ping failed to connect to tcp://" hostport))
     (thread-start! timeout)
     (thread-start! ping)
     (thread-join! ping)
     (if success (thread-terminate! timeout)))
    (if return-socket
	(if success req #f)
	(begin
	  (nn-close req)
	  success))))

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
			(debug:print-info 0 "Have flexi-launcher match for " testname "/" itempath " = " host-type)
			(let ((launcher (configf:lookup configdat "host-types" host-type)))
			  (if launcher
			      launcher
			      (begin
				(debug:print-info 0 "WARNING: no launcher found for host-type " host-type)
				(if (null? tal)
				    fallback-launcher
				    (loop (car tal)(cdr tal)))))))
		      ;; no match, try again
		      (if (null? tal)
			  fallback-launcher
			  (loop (car tal)(cdr tal))))))))
	fallback-launcher)))
  
