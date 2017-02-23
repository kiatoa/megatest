;;======================================================================
;; Copyright 2006-2016, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;;======================================================================
;; Database access
;;======================================================================

;; dbstruct vector containing all the relevant dbs like main.db, megatest.db, run.db etc

(use (srfi 18) extras tcp stack)
(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64 format dot-locking z3 typed-records matchable)
(import (prefix sqlite3 sqlite3:))
(import (prefix base64 base64:))

(declare (unit db))
(declare (uses common))
(declare (uses keys))
(declare (uses ods))
(declare (uses client))
(declare (uses mt))

(include "common_records.scm")
(include "db_records.scm")
(include "key_records.scm")
(include "run_records.scm")

(define *rundb-mutex* (make-mutex)) ;; prevent problems opening/closing rundb's
(define *number-of-writes* 0)
(define *number-non-write-queries* 0)

;;======================================================================
;;  R E C O R D S
;;======================================================================

;; each db entry is a pair ( db . dbfilepath )
;; I propose this record evolves into the area record
;;
(defstruct dbr:dbstruct 
  (tmpdb       #f)
  (dbstack     #f) ;; stack for tmp db handles, do not initialize with a stack
  (mtdb        #f)
  (refndb      #f)
  (homehost    #f) ;; not used yet
  (on-homehost #f) ;; not used yet
  (read-only   #f)
  )                ;; goal is to converge on one struct for an area but for now it is too confusing
  

;; record for keeping state,status and count for doing roll-ups in
;; iterated tests
;;
(defstruct dbr:counts
  (state #f)
  (status #f)
  (count  0)) 

;;======================================================================
;; SQLITE3 HELPERS
;;======================================================================

(define (db:general-sqlite-error-dump exn stmt . params)
  (let ((err-status ((condition-property-accessor 'sqlite3 'status #f) exn))) ;; RADT ... how does this work?
    ;; check for (exn sqlite3) ((condition-property-accessor 'exn 'message) exn)
    (print "err-status: " err-status)
    (debug:print-error 0 *default-log-port* " query " stmt " failed, params: " params ", error: " ((condition-property-accessor 'exn 'message) exn))
    (print-call-chain (current-error-port))))

;; convert to -inline 
;;
(define (db:first-result-default db stmt default . params)
  (handle-exceptions
   exn
   (let ((err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
     ;; check for (exn sqlite3) ((condition-property-accessor 'exn 'message) exn)
     (if (eq? err-status 'done)
	 default
	 (begin
	   (debug:print-error 0 *default-log-port* " query " stmt " failed, params: " params ", error: " ((condition-property-accessor 'exn 'message) exn))
	   (print-call-chain (current-error-port))
	   default)))
   (apply sqlite3:first-result db stmt params)))

;; Get/open a database
;;    if run-id => get run specific db
;;    if #f     => get main db
;;    if db already open - return inmem
;;    if db not open, open inmem, rundb and sync then return inmem
;;    inuse gets set automatically for rundb's
;;
(define (db:get-db dbstruct) ;;  run-id) 
  (if (stack? (dbr:dbstruct-dbstack dbstruct))
      (if (stack-empty? (dbr:dbstruct-dbstack dbstruct))
          (let ((newdb (db:open-megatest-db path: (db:dbfile-path))))
            ;; (stack-push! (dbr:dbstruct-dbstack dbstruct) newdb)
            newdb)
          (stack-pop! (dbr:dbstruct-dbstack dbstruct)))
      (db:open-db dbstruct)))

;; ;; legacy handling of structure for managing db's. Refactor this into dbr:?
(define (db:dbdat-get-db dbdat)
  (if (pair? dbdat)
      (car dbdat)
      dbdat))

(define (db:dbdat-get-path dbdat)
  (if (pair? dbdat)
      (cdr dbdat)
      #f))

;; mod-read:
;;     'mod   modified data
;;     'read  read data
;; Locks the mutex and depending on 'mod or 'read passed, sets the last timestamp in dbstruct
;;
;; (define (db:done-with dbstruct run-id mod-read)
;;   (if (not (sqlite3:database? dbstruct))
;;       (begin
;; 	(mutex-lock! *rundb-mutex*)
;; 	(if (eq? mod-read 'mod)
;; 	    (dbr:dbstruct-mtime-set! dbstruct (current-milliseconds))
;; 	    (dbr:dbstruct-rtime-set! dbstruct (current-milliseconds)))
;; 	(dbr:dbstruct-inuse-set! dbstruct #f)
;; 	(mutex-unlock! *rundb-mutex*))))

;; (db:with-db dbstruct run-id sqlite3:exec "select blah fgrom blaz;")
;; r/w is a flag to indicate if the db is modified by this query #t = yes, #f = no
;;
(define (db:with-db dbstruct run-id r/w proc . params)
  (let* ((have-struct (dbr:dbstruct? dbstruct))
         (dbdat (if have-struct 
                    (db:get-db dbstruct)
                    #f))
	 (db    (if have-struct
		    (db:dbdat-get-db dbdat)
		    dbstruct))
	 (use-mutex (> *api-process-request-count* 25)))
    (if (and use-mutex
	     (common:low-noise-print 120 "over-50-parallel-api-requests"))
	(debug:print-info 0 *default-log-port* *api-process-request-count* " parallel api requests being processed in process " (current-process-id) ", throttling access"))
    (if (common:low-noise-print 600 (conc "parallel-api-requests" *max-api-process-requests*))
	(debug:print-info 2 *default-log-port* "Parallel api request count: " *api-process-request-count* " max parallel requests: " *max-api-process-requests*))
    (handle-exceptions
     exn
     (begin
       (print-call-chain (current-error-port))
       (debug:print-error 0 *default-log-port* "sqlite3 issue in db:with-db, dbstruct=" dbstruct ", run-id=" run-id ", proc=" proc ", params=" params " error: " ((condition-property-accessor 'exn 'message) exn))
       ;; there is no recovering at this time. exit
       (exit 50))
     (if use-mutex (mutex-lock! *db-with-db-mutex*))
     (let ((res (apply proc db params)))
       (if use-mutex (mutex-unlock! *db-with-db-mutex*))
       ;; (if (vector? dbstruct)(db:done-with dbstruct run-id r/w))
       (if dbdat (stack-push! (dbr:dbstruct-dbstack dbstruct) dbdat))
       res))))

;;======================================================================
;; K E E P   F I L E D B   I N   dbstruct
;;======================================================================

;; (define (db:get-filedb dbstruct run-id)
;;   (let ((db (vector-ref dbstruct 2)))
;;     (if db
;; 	db
;; 	(let ((fdb (filedb:open-db (conc *toplevel* "/db/files.db"))))
;; 	  (vector-set! dbstruct 2 fdb)
;; 	  fdb))))
;; 
;; ;; Can also be used to save arbitrary strings
;; ;;
;; (define (db:save-path dbstruct path)
;;   (let ((fdb (db:get-filedb dbstruct)))b
;;     (filedb:register-path fdb path)))
;; 
;; ;; Use to get a path. To get an arbitrary string see next define
;; ;;
;; (define (db:get-path dbstruct id)
;;   (let ((fdb (db:get-filedb dbstruct)))
;;     (filedb:get-path db id)))

;; NB// #f => return dbdir only
;;      (was planned to be;  zeroth db with name=main.db)
;; 
;; If run-id is #f return to create and retrieve the path where the db will live.
;;
(define (db:dbfile-path . junk) ;;  run-id)
  (let* ((dbdir           (common:get-db-tmp-area)))
    (handle-exceptions
     exn
     (begin
       (debug:print-error 0 *default-log-port* "Couldn't create path to " dbdir)
       (exit 1))
     (if (not (directory? dbdir))(create-directory dbdir #t)))
    dbdir))

(define (db:set-sync db)
  (let ((syncprag (configf:lookup *configdat* "setup" "sychronous")))
    (sqlite3:execute db (conc "PRAGMA synchronous = " (or syncprag 0) ";")))) 

;; open an sql database inside a file lock
;; returns: db existed-prior-to-opening
;; RA => Returns a db handler; sets the lock if opened in writable mode
;;
(define (db:lock-create-open fname initproc)
  (let* ((parent-dir   (or (pathname-directory fname)(current-directory))) ;; no parent? go local
	 (dir-writable (file-write-access? parent-dir))
	 (file-exists  (file-exists? fname))
	 (file-write   (if file-exists
			   (file-write-access? fname)
			   dir-writable )))
    (if file-write ;; dir-writable
	(let (;; (lock    (obtain-dot-lock fname 1 5 10))
	      (db      (sqlite3:open-database fname)))
	  (sqlite3:set-busy-handler! db (make-busy-timeout 136000))
	  ;; (db:set-sync db)
	  (sqlite3:execute db "PRAGMA synchronous = 0;")
	  (if (not file-exists)
	      (begin
		(if (string-match "^/tmp/.*" fname) ;; this is a file in /tmp
		    (sqlite3:execute db "PRAGMA journal_mode=WAL;")
		    (print "Creating " fname " in NON-WAL mode."))
		(initproc db)))
	  ;; (release-dot-lock fname)
	  db)
	(begin
	  (debug:print 2 *default-log-port* "WARNING: opening db in non-writable dir " fname)
	  (sqlite3:open-database fname))))) ;; )

;; ;; This routine creates the db. It is only called if the db is not already opened
;; ;; 
;; (define (db:open-rundb dbstruct run-id #!key (attemptnum 0)(do-not-open #f)) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
;;   (let* ((dbfile       (db:dbfile-path run-id)) ;; (conc toppath "/db/" run-id ".db"))
;;          (dbexists     (file-exists? dbfile))
;;          (db           (db:lock-create-open dbfile (lambda (db)
;;                                                      (handle-exceptions
;;                                                       exn
;;                                                       (begin
;;                                                         ;; (release-dot-lock dbpath)
;;                                                         (if (> attemptnum 2)
;;                                                             (debug:print-error 0 *default-log-port* "tried twice, cannot create/initialize db for run-id " run-id ", at path " dbpath)
;;                                                             (db:open-rundb dbstruct run-id attemptnum (+ attemptnum 1))))
;;                                                       (db:initialize-run-id-db db)
;;                                                       (sqlite3:execute 
;;                                                        db
;;                                                        "INSERT OR IGNORE INTO tests (id,run_id,testname,event_time,item_path,state,status) VALUES (?,?,'bogustest',strftime('%s','now'),'nowherepath','DELETED','n/a');"
;;                                                        (* run-id 30000) ;; allow for up to 30k tests per run
;;                                                        run-id)
;;                                                       ;; do a dummy query to test that the table exists and the db is truly readable
;;                                                       (sqlite3:execute db "SELECT * FROM tests WHERE id=?;" (* run-id 30000))
;;                                                       )))) ;; add strings db to rundb, not in use yet
;;          (olddb        (if *megatest-db*
;;                            *megatest-db* 
;;                            (let ((db (db:open-megatest-db)))
;;                              (set! *megatest-db* db)
;;                              db)))
;;          (write-access (file-write-access? dbfile)))
;;     (if (and dbexists (not write-access))
;;         (set! *db-write-access* #f)) ;; only unset so other db's also can use this control
;;     (dbr:dbstruct-rundb-set!  dbstruct (cons db dbfile))
;;     (dbr:dbstruct-inuse-set!  dbstruct #t)
;;     (dbr:dbstruct-olddb-set!  dbstruct olddb)
;;     ;;; (mutex-unlock! *rundb-mutex*) ;;; why did we need a mutex on opening db's?
;;     (db:sync-tables db:sync-tests-only *megatest-db* db)
;;     db))

;; This routine creates the db if not already present. It is only called if the db is not already opened
;;
(define (db:open-db dbstruct #!key (areapath #f)) ;; TODO: actually use areapath
  (let ((tmpdb-stack (dbr:dbstruct-dbstack dbstruct))) ;; RA => Returns the first reference in dbstruct
    (if (stack? tmpdb-stack)
	(db:get-db tmpdb-stack) ;; get previously opened db (will create new db handle if all in the stack are already used
        (let* ((dbpath       (db:dbfile-path ))      ;; path to tmp db area
               (dbexists     (file-exists? dbpath))
	       (tmpdbfname   (conc dbpath "/megatest.db"))
	       (dbfexists    (file-exists? tmpdbfname)) ;; (conc dbpath "/megatest.db")))
               (tmpdb        (db:open-megatest-db path: dbpath)) ;; lock-create-open dbpath db:initialize-main-db))
               (mtdbexists   (file-exists? (conc *toppath* "/megatest.db")))
               (mtdb         (db:open-megatest-db))
               (mtdbpath     (db:dbdat-get-path mtdb))
               
               (refndb       (db:open-megatest-db path: dbpath name: "megatest_ref.db"))
               (write-access (file-write-access? mtdbpath))
	       (mtdbmodtime  (if mtdbexists (common:lazy-sqlite-db-modification-time mtdbpath)   #f))
	       (tmpdbmodtime (if dbfexists  (common:lazy-sqlite-db-modification-time tmpdbfname) #f))
	       (modtimedelta (and mtdbmodtime tmpdbmodtime (- mtdbmodtime tmpdbmodtime))))
	  
          ;;(debug:print-info 13 *default-log-port* "db:open-db>> mtdbpath="mtdbpath" mtdbexists="mtdbexists" and write-access="write-access)
          (if (and dbexists (not write-access))
              (begin
                (set! *db-write-access* #f)
                (dbr:dbstruct-read-only-set! dbstruct #t)))
          (dbr:dbstruct-mtdb-set!   dbstruct mtdb)
          (dbr:dbstruct-tmpdb-set!  dbstruct tmpdb)
          (dbr:dbstruct-dbstack-set! dbstruct (make-stack)) ;; BB: why a stack?  Why would the number of db's be indeterminate?  Is this a legacy of 1.db 2.db .. ?
          (stack-push! (dbr:dbstruct-dbstack dbstruct) tmpdb) ;; olddb is already a (cons db path)
          (dbr:dbstruct-refndb-set! dbstruct refndb)
          ;;	    (mutex-unlock! *rundb-mutex*)
          (if (or (not dbfexists)
                  (and modtimedelta
                       (> modtimedelta 10))) ;; if db in tmp is over ten seconds older than the file in MTRA then do a sync back
	      (begin
		(debug:print 0 *default-log-port* "filling db " (db:dbdat-get-path tmpdb) " with data \n    from " (db:dbdat-get-path mtdb) " mod time delta: " modtimedelta)
		(db:sync-tables (db:sync-all-tables-list dbstruct) #f mtdb refndb tmpdb)
                (debug:print-info 13 *default-log-port* "db:sync-all-tables-list done.")
                )
	      (debug:print 0 *default-log-port* " db, " (db:dbdat-get-path tmpdb) " already exists or fresh enough, not propogating data from\n     " (db:dbdat-get-path mtdb) " mod time delta: " modtimedelta) )
	  ;; (db:multi-db-sync dbstruct 'old2new))  ;; migrate data from megatest.db automatically
          tmpdb))))

;; Make the dbstruct, setup up auxillary db's and call for main db at least once
;;
;; called in http-transport and replicated in rmt.scm for *local* access. 
;;
(define (db:setup #!key (areapath #f))
  ;;

  (cond
   (*dbstruct-db* *dbstruct-db*);; TODO: when multiple areas are supported, this optimization will be a hazard
   (else ;;(common:on-homehost?)
    (debug:print-info 13 *default-log-port* "db:setup entered (first time, not cached.)")
    (let* ((dbstruct (make-dbr:dbstruct)))
      (when (not *toppath*)
        (debug:print-info 13 *default-log-port* "in db:setup, *toppath* not set; calling launch:setup")
        (launch:setup areapath: areapath))
      (debug:print-info 13 *default-log-port* "Begin db:open-db")
      (db:open-db dbstruct areapath: areapath)
      (debug:print-info 13 *default-log-port* "Done db:open-db")
      (set! *dbstruct-db* dbstruct)
      ;;(debug:print-info 13 *default-log-port* "new dbstruct = "(dbr:dbstruct->alist dbstruct))
      dbstruct))))
   ;; (else
   ;;  (debug:print 0 *default-log-port* "ERROR: attempt to open database when not on homehost. Exiting. Homehost: " (common:get-homehost))
   ;;  (exit 1))))

;; Open the classic megatest.db file (defaults to open in toppath)
;;
;;   NOTE: returns a dbdat not a dbstruct!
;;
(define (db:open-megatest-db #!key (path #f)(name #f))
  (let* ((dbpath       (conc (or path *toppath*) "/" (or name "megatest.db")))
	 (dbexists     (file-exists? dbpath))
	 (db           (db:lock-create-open dbpath
					    (lambda (db)
					      (db:initialize-main-db db)
					      (db:initialize-run-id-db db))))
	 (write-access (file-write-access? dbpath)))
    (debug:print-info 13 *default-log-port* "db:open-megatest-db "dbpath)
    (if (and dbexists (not write-access))
	(set! *db-write-access* #f))
    (cons db dbpath)))

;; sync run to disk if touched
;;
(define (db:sync-touched dbstruct run-id #!key (force-sync #f))
  (let ((tmpdb   (db:get-db dbstruct))
	(mtdb    (dbr:dbstruct-mtdb   dbstruct))
        (refndb  (dbr:dbstruct-refndb dbstruct))
	(start-t (current-seconds)))
    (debug:print-info 4 *default-log-port* "Syncing for run-id: " run-id)
    (mutex-lock! *db-multi-sync-mutex*)
    (let ((update_info (cons (if force-sync 0 *db-last-sync*) "last_update")))
      (mutex-unlock! *db-multi-sync-mutex*)
      (db:sync-tables (db:sync-all-tables-list dbstruct) update_info tmpdb refndb mtdb))
    (mutex-lock! *db-multi-sync-mutex*)
    (set! *db-last-sync* start-t)
    (set! *db-last-access* start-t)
    (mutex-unlock! *db-multi-sync-mutex*)
    (stack-push! (dbr:dbstruct-dbstack dbstruct) tmpdb)))

;; close all opened run-id dbs
(define (db:close-all dbstruct)
  (if (dbr:dbstruct? dbstruct)
      (begin
        ;; (db:sync-touched dbstruct 0 force-sync: #t) ;; NO. Do not do this here. Instead we rely on a server to be started when there are writes, even if the server itself is not going to be used as a server.
        (let ((tdbs (map db:dbdat-get-db 
                         (stack->list (dbr:dbstruct-dbstack dbstruct))))
              (mdb (db:dbdat-get-db (dbr:dbstruct-mtdb   dbstruct)))
              (rdb (db:dbdat-get-db (dbr:dbstruct-refndb dbstruct))))
          (map sqlite3:finalize! tdbs)
          (if mdb (sqlite3:finalize! mdb))
          (if rdb (sqlite3:finalize! rdb))))))
  
;;   (let ((locdbs (dbr:dbstruct-locdbs dbstruct)))
;;     (if (hash-table? locdbs)
;; 	(for-each (lambda (run-id)
;; 		    (db:close-run-db dbstruct run-id))
;; 		  (hash-table-keys locdbs)))))

;; (define (db:open-inmem-db)
;;   (let* ((db      (sqlite3:open-database ":memory:"))
;; 	 (handler (make-busy-timeout 3600)))
;;     (sqlite3:set-busy-handler! db handler)
;;     (db:initialize-run-id-db db)
;;     (cons db #f)))

;; just tests, test_steps and test_data tables
(define db:sync-tests-only
  (list
   ;; (list "strs"
   ;;       '("id"             #f)
   ;;       '("str"            #f))
   (list "tests" 
	 '("id"             #f)
	 '("run_id"         #f)
	 '("testname"       #f)
	 '("host"           #f)
	 '("cpuload"        #f)
	 '("diskfree"       #f)
	 '("uname"          #f)
	 '("rundir"         #f)
	 '("shortdir"       #f)
	 '("item_path"      #f)
	 '("state"          #f)
	 '("status"         #f)
	 '("attemptnum"     #f)
	 '("final_logf"     #f)
	 '("logdat"         #f)
	 '("run_duration"   #f)
	 '("comment"        #f)
	 '("event_time"     #f)
	 '("fail_count"     #f)
	 '("pass_count"     #f)
	 '("archived"       #f))
  (list "test_steps"
	 '("id"             #f)
	 '("test_id"        #f)
	 '("stepname"       #f)
	 '("state"          #f)
	 '("status"         #f)
	 '("event_time"     #f)
	 '("comment"        #f)
	 '("logfile"        #f))
   (list "test_data"
	 '("id"             #f)
	 '("test_id"        #f)
	 '("category"       #f)
	 '("variable"       #f)
	 '("value"          #f)
	 '("expected"       #f)
	 '("tol"            #f)
	 '("units"          #f)
	 '("comment"        #f)
	 '("status"         #f)
	 '("type"           #f))))

;; needs db to get keys, this is for syncing all tables
;;
(define (db:sync-main-list dbstruct)
  (let ((keys  (db:get-keys dbstruct)))
    (list
     (list "keys"
	   '("id"        #f)
	   '("fieldname" #f)
	   '("fieldtype" #f))
     (list "metadat" '("var" #f) '("val" #f))
     (append (list "runs" 
		   '("id"  #f))
	     (map (lambda (k)(list k #f))
		  (append keys
			  (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count" "pass_count"))))
     (list "test_meta"
	   '("id"             #f)
	   '("testname"       #f)
	   '("owner"          #f)
	   '("description"    #f)
	   '("reviewed"       #f)
	   '("iterated"       #f)
	   '("avg_runtime"    #f)
	   '("avg_disk"       #f)
	   '("tags"           #f)
	   '("jobgroup"       #f)))))

(define (db:sync-all-tables-list dbstruct)
  (append (db:sync-main-list dbstruct)
	  db:sync-tests-only))

;; use bunch of Unix commands to try to break the lock and recreate the db
;;
(define (db:move-and-recreate-db dbdat)
  (let* ((dbpath   (db:dbdat-get-path        dbdat))
	 (dbdir    (pathname-directory       dbpath))
	 (fname    (pathname-strip-directory dbpath))
	 (fnamejnl (conc fname "-journal"))
	 (tmpname  (conc fname "." (current-process-id)))
	 (tmpjnl   (conc fnamejnl "." (current-process-id))))
    (debug:print-error 0 *default-log-port* "" fname " appears corrupted. Making backup \"old/" fname "\"")
    (system (conc "cd " dbdir ";mkdir -p old;cat " fname " > old/" tmpname))
    (system (conc "rm -f " dbpath))
    (if (file-exists? fnamejnl)
	(begin
	  (debug:print-error 0 *default-log-port* "" fnamejnl " found, moving it to old dir as " tmpjnl)
	  (system (conc "cd " dbdir ";mkdir -p old;cat " fnamejnl " > old/" tmpjnl))
	  (system (conc "rm -f " dbdir "/" fnamejnl))))
    ;; attempt to recreate database
    (system (conc "cd " dbdir ";sqlite3 old/" tmpname " .dump | sqlite3 " fname))))
    
;; return #f to indicate the dbdat should be closed/reopened
;; else return dbdat
;;
(define (db:repair-db dbdat #!key (numtries 1))
  (let* ((dbpath   (db:dbdat-get-path        dbdat))
	 (dbdir    (pathname-directory       dbpath))
	 (fname    (pathname-strip-directory dbpath)))
    (debug:print-info 0 *default-log-port* "Checking db " dbpath " for errors.")
    (cond
     ((not (file-write-access? dbdir))
      (debug:print 0 *default-log-port* "WARNING: can't write to " dbdir ", can't fix " fname)
      #f)

     ;; handle special cases, megatest.db and monitor.db
     ;; 
     ;;  NOPE: apply this same approach to all db files
     ;;
     (else ;; ((equal? fname "megatest.db") ;; this file can be regenerated if needed
      (handle-exceptions
       exn
       (begin
	 ;; (db:move-and-recreate-db dbdat)
	 (if (> numtries 0)
	     (db:repair-db dbdat numtries: (- numtries 1))
	     #f)
	 (debug:print 0 *default-log-port* "FATAL: file " dbpath " was found corrupted, an attempt to fix has been made but you must start over.")
	 (debug:print 0 *default-log-port*
		      "   check the following:\n"
		      "      1. full directories, look in ~/ /tmp and " dbdir "\n"
		      "      2. write access to " dbdir "\n\n"
		      "   if the automatic recovery failed you may be able to recover data by doing \"" 
		      (if (member fname '("megatest.db" "monitor.db"))
			  "megatest -cleanup-db"
			  "megatest -import-megatest.db;megatest -cleanup-db")
		      "\"\n")
	 (exit) ;; we can not safely continue when a db was corrupted - even if fixed.
	 )
       ;; test read/write access to the database
       (let ((db (sqlite3:open-database dbpath)))
	 (cond
	  ((equal? fname "megatest.db")
	   (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED';"))
	  ((equal? fname "main.db")
	   (sqlite3:execute db "DELETE FROM runs WHERE state='deleted';"))
	  ((string-match "\\d.db" fname)
	   (sqlite3:execute db "UPDATE tests SET state='DELETED' WHERE state='DELETED';"))
	  ((equal? fname "monitor.db")
	   (sqlite3:execute "DELETE FROM servers WHERE state LIKE 'defunct%';"))
	  (else
	   (sqlite3:execute db "vacuum;")))
	 
	 (finalize! db)
	 #t))))))
    
;; tbls is ( ("tablename" ( "field1" [#f|proc1] ) ( "field2" [#f|proc2] ) .... ) )
;; db's are dbdat's
;;
;; if last-update specified ("field-name" . time-in-seconds)
;;    then sync only records where field-name >= time-in-seconds
;;    IFF field-name exists
;;
(define (db:sync-tables tbls last-update fromdb todb . slave-dbs)
  (handle-exceptions
   exn
   (begin
     (debug:print 0 *default-log-port* "EXCEPTION: database probably overloaded or unreadable in db:sync-tables.")
     (print-call-chain (current-error-port))
     (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
     (print "exn=" (condition->list exn))
     (debug:print 0 *default-log-port* " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
     (debug:print 0 *default-log-port* " src db:  " (db:dbdat-get-path fromdb))
     (for-each (lambda (dbdat)
		 (let ((dbpath (db:dbdat-get-path dbdat)))
		   (debug:print 0 *default-log-port* " dbpath:  " dbpath)
		   (if (not (db:repair-db dbdat))
		       (begin
			 (debug:print-error 0 *default-log-port* "Failed to rebuild " dbpath ", exiting now.")
			 (exit)))))
	       (cons todb slave-dbs))
     
     0)
   ;; this is the work to be done
   (cond
    ((not fromdb) (debug:print 3 *default-log-port* "WARNING: db:sync-tables called with fromdb missing")
     -1)
    ((not todb)   (debug:print 3 *default-log-port* "WARNING: db:sync-tables called with todb missing")
     -2)
    ((not (sqlite3:database? (db:dbdat-get-db fromdb)))
     (debug:print-error 0 *default-log-port* "db:sync-tables called with fromdb not a database " fromdb)
     -3)
    ((not (sqlite3:database? (db:dbdat-get-db todb)))
     (debug:print-error 0 *default-log-port* "db:sync-tables called with todb not a database " todb)
     -4)

    ((not (file-write-access? (db:dbdat-get-path todb)))
     (debug:print-error 0 *default-log-port* "db:sync-tables called with todb not a read-only database " todb)
     -5)
    ((not (null? (let ((readonly-slave-dbs
                        (filter
                         (lambda (dbdat)
                           (not (file-write-access? (db:dbdat-get-path todb))))
                         slave-dbs)))
                   (for-each
                    (lambda (bad-dbdat)
                      (debug:print-error
                       0 *default-log-port* "db:sync-tables called with todb not a read-only database " bad-dbdat))
                    readonly-slave-dbs)
                   readonly-slave-dbs))) -6)
    (else
     (let ((stmts       (make-hash-table)) ;; table-field => stmt
	   (all-stmts   '())              ;; ( ( stmt1 value1 ) ( stml2 value2 ))
	   (numrecs     (make-hash-table))
	   (start-time  (current-milliseconds))
	   (tot-count   0))
       (for-each ;; table
	(lambda (tabledat)
	  (let* ((tablename  (car tabledat))
		 (fields     (cdr tabledat))
		 (use-last-update  (if last-update
				       (if (pair? last-update)
					   (member (car last-update)    ;; last-update field name
						   (map car fields))
					   (begin
					     (debug:print 0 *default-log-port* "ERROR: parameter last-update for db:sync-tables must be a pair, received: " last-update) ;; found in fields
					     #f))
				       #f))
		 (num-fields (length fields))
		 (field->num (make-hash-table))
		 (num->field (apply vector (map car fields)))
		 (full-sel   (conc "SELECT " (string-intersperse (map car fields) ",") 
				   " FROM " tablename (if use-last-update ;; apply last-update criteria
							  (conc " " (car last-update) ">=" (cdr last-update))
							  "")
				   ";"))
		 (full-ins   (conc "INSERT OR REPLACE INTO " tablename " ( " (string-intersperse (map car fields) ",") " ) "
				   " VALUES ( " (string-intersperse (make-list num-fields "?") ",") " );"))
		 (fromdat    '())
		 (fromdats   '())
		 (totrecords 0)
		 (batch-len  (string->number (or (configf:lookup *configdat* "sync" "batchsize") "100")))
		 (todat      (make-hash-table))
		 (count      0))

	    ;; set up the field->num table
	    (for-each
	     (lambda (field)
	       (hash-table-set! field->num field count)
	       (set! count (+ count 1)))
	     fields)

	    ;; read the source table
	    (sqlite3:for-each-row
	     (lambda (a . b)
	       (set! fromdat (cons (apply vector a b) fromdat))
	       (if (> (length fromdat) batch-len)
		   (begin
		     (set! fromdats (cons fromdat fromdats))
		     (set! fromdat  '())
		     (set! totrecords (+ totrecords 1)))))
	     (db:dbdat-get-db fromdb)
	     full-sel)
	    
	    ;; tack on remaining records in fromdat
	    (if (not (null? fromdat))
		(set! fromdats (cons fromdat fromdats)))

	    (if (common:low-noise-print 120 "sync-records")
		(debug:print-info 4 *default-log-port* "found " totrecords " records to sync"))

	    ;; read the target table
	    (sqlite3:for-each-row
	     (lambda (a . b)
	       (hash-table-set! todat a (apply vector a b)))
	     (db:dbdat-get-db todb)
	     full-sel)

	    ;; first pass implementation, just insert all changed rows
	    (for-each 
	     (lambda (targdb)
	       (let* ((db     (db:dbdat-get-db targdb))
		      (stmth  (sqlite3:prepare db full-ins)))
		 (db:delay-if-busy targdb) ;; NO WAITING
		 (for-each
		  (lambda (fromdat-lst)
		    (sqlite3:with-transaction
		     db
		     (lambda ()
		       (for-each ;; 
			(lambda (fromrow)
			  (let* ((a    (vector-ref fromrow 0))
				 (curr (hash-table-ref/default todat a #f))
				 (same #t))
			    (let loop ((i 0))
			      (if (or (not curr)
				      (not (equal? (vector-ref fromrow i)(vector-ref curr i))))
				  (set! same #f))
			      (if (and same
				       (< i (- num-fields 1)))
				  (loop (+ i 1))))
			    (if (not same)
				(begin
				  (apply sqlite3:execute stmth (vector->list fromrow))
				  (hash-table-set! numrecs tablename (+ 1 (hash-table-ref/default numrecs tablename 0)))))))
			fromdat-lst))
		  ))
		  fromdats)
		 (sqlite3:finalize! stmth)))
	     (append (list todb) slave-dbs))))
	tbls)
       (let* ((runtime      (- (current-milliseconds) start-time))
	      (should-print (or (debug:debug-mode 12)
				(common:low-noise-print 120 "db sync" (> runtime 500))))) ;; low and high sync times treated as separate.
	 (if should-print (debug:print 3 *default-log-port* "INFO: db sync, total run time " runtime " ms"))
	 (for-each 
	  (lambda (dat)
	    (let ((tblname (car dat))
		  (count   (cdr dat)))
	      (set! tot-count (+ tot-count count))
	      (if (> count 0)
		  (if should-print (debug:print 0 *default-log-port* (format #f "    ~10a ~5a" tblname count))))))
	  (sort (hash-table->alist numrecs)(lambda (a b)(> (cdr a)(cdr b))))))
       tot-count)))))

(define (db:patch-schema-rundb frundb)
  ;;
  ;; remove this some time after September 2016 (added in version v1.6031
  ;;
  (for-each
   (lambda (table-name)
     (handle-exceptions
      exn
      (if (string-match ".*duplicate.*" ((condition-property-accessor 'exn 'message) exn))
          (debug:print 0 *default-log-port* "Column last_update already added to " table-name " table")
          (db:general-sqlite-error-dump exn "alter table " table-name " ..." #f "none"))
      (sqlite3:execute
       frundb
       (conc "ALTER TABLE " table-name " ADD COLUMN last_update INTEGER DEFAULT 0")))
     (sqlite3:execute
      frundb
      (conc "DROP TRIGGER IF EXISTS update_" table-name "_trigger;"))
     (sqlite3:execute
      frundb
      (conc "CREATE TRIGGER IF NOT EXISTS update_" table-name "_trigger AFTER UPDATE ON " table-name "
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE " table-name " SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;"))
     )
   '("tests" "test_steps" "test_data")))

(define (db:patch-schema-maindb maindb)
  ;;
  ;; remove all these some time after september 2016 (added in v1.6031
  ;;
  (handle-exceptions
   exn
   (if (string-match ".*duplicate.*" ((condition-property-accessor 'exn 'message) exn))
       (debug:print 0 *default-log-port* "Column last_update already added to runs table")
       (db:general-sqlite-error-dump exn "alter table runs ..." #f "none"))
   (sqlite3:execute
    maindb
    "ALTER TABLE runs ADD COLUMN last_update INTEGER DEFAULT 0"))
  ;; these schema changes don't need exception handling
  (sqlite3:execute
   maindb
   "CREATE TRIGGER IF NOT EXISTS update_runs_trigger AFTER UPDATE ON runs
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE runs SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
  (sqlite3:execute maindb "CREATE TABLE IF NOT EXISTS run_stats (
                              id     INTEGER PRIMARY KEY,
                              run_id INTEGER,
                              state  TEXT,
                              status TEXT,
                              count  INTEGER,
                              last_update INTEGER DEFAULT (strftime('%s','now')))")
  (sqlite3:execute maindb "CREATE TRIGGER  IF NOT EXISTS update_run_stats_trigger AFTER UPDATE ON run_stats
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE run_stats SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;"))

(define *global-db-store* (make-hash-table))

(define (db:get-access-mode)
  (if (args:get-arg "-use-db-cache") 'cached 'rmt))

;; Add db direct
;;
(define (db:dispatch-query access-mode rmt-cmd db-cmd . params)
  (if (eq? access-mode 'cached)
      (debug:print 2 *default-log-port* "not doing cached calls right now"))
;;      (apply db:call-with-cached-db db-cmd params)
      (apply rmt-cmd params))
;;)

;; return the target db handle so it can be used
;;
(define (db:cache-for-read-only source target #!key (use-last-update #f))
  (if (and (hash-table-ref/default *global-db-store* target #f)
	   (>= (file-modification-time target)(file-modification-time source)))
      (hash-table-ref *global-db-store* target)
      (let* ((toppath   (launch:setup))
	     (targ-db-last-mod (if (file-exists? target)
				   (file-modification-time target)
				   0))
	     (cache-db  (or (hash-table-ref/default *global-db-store* target #f)
			    (db:open-megatest-db path: target)))
	     (source-db (db:open-megatest-db path: source))
	     (curr-time (current-seconds))
	     (res      '())
	     (last-update (if use-last-update (cons "last_update" targ-db-last-mod) #f)))
	(db:sync-tables (db:sync-main-list source-db) last-update source-db cache-db)
	(db:sync-tables db:sync-tests-only last-update source-db cache-db)
	(hash-table-set! *global-db-store* target cache-db)
	cache-db)))

;; call a proc with a cached db
;;
(define (db:call-with-cached-db proc . params)
  ;; first cache the db in /tmp
  (let* ((cname-part (conc "megatest_cache/" (common:get-testsuite-name)))
	 (fname      (conc  (common:get-area-path-signature) ".db"))
	 (cache-dir  (common:get-create-writeable-dir
		      (list (conc "/tmp/" (current-user-name) "/" cname-part)
			    (conc "/tmp/" (current-user-name) "-" cname-part)
			     (conc "/tmp/" (current-user-name) "_" cname-part))))
	 (megatest-db (conc *toppath* "/megatest.db")))
    ;; (debug:print-info 0 *default-log-port* "Using cache dir " cache-dir)
    (if (not cache-dir)
	(begin
	  (debug:print 0 *default-log-port* "ERROR: Failed to find an area to write the cache db")
	  (exit 1))
	(let* ((th1      (make-thread
			  (lambda ()
			    (if (and (file-exists? megatest-db)
				     (file-write-access? megatest-db))
				(begin
				  (common:sync-to-megatest.db 'timestamps) ;; internally mutexes on *db-local-sync*
				  (debug:print-info 2 *default-log-port* "Done syncing to megatest.db"))))
			  "call-with-cached-db sync-to-megatest.db"))
	       (cache-db (db:cache-for-read-only
			  megatest-db
			  (conc cache-dir "/" fname)
			  use-last-update: #t)))
	  (thread-start! th1)
	  (apply proc cache-db params)
	  ))))

;; options:
;;
;;  'killservers  - kills all servers
;;  'dejunk       - removes junk records
;;  'adj-testids  - move test-ids into correct ranges
;;  'old2new      - sync megatest.db to /tmp/.../megatest.db and /tmp/.../megatest_ref.db
;;  'new2old      - sync /tmp/.../megatest.db to megatest.db and /tmp/.../megatest_ref.db (and update data_synced)
;;  'closeall     - close all opened dbs
;;  'schema       - attempt to apply schema changes
;;  run-ids: '(1 2 3 ...) or #f (for all)
;;
(define (db:multi-db-sync dbstruct . options)
  (if (not (launch:setup))
      (debug:print 0 *default-log-port* "ERROR: not able to setup up for megatest.")
      (let* ((mtdb     (dbr:dbstruct-mtdb dbstruct))
	     (tmpdb    (db:get-db dbstruct))
             (refndb   (dbr:dbstruct-refndb dbstruct))
	     (allow-cleanup #t) ;; (if run-ids #f #t))
	     ;; (tdbdat  (tasks:open-db))
	     (servers (server:get-list *toppath*)) ;; (tasks:get-all-servers (db:delay-if-busy tdbdat)))
	     (data-synced 0)) ;; count of changed records (I hope)
    
	;; kill servers
	(if (member 'killservers options)
	    (for-each
	     (lambda (server)
	       (match-let (((mod-time host port start-time pid) server))
		 (if (and host pid)
		     (tasks:kill-server host pid))))
	     servers))

	;; clear out junk records
	;;
	(if (member 'dejunk options)
	    (begin
	      (db:delay-if-busy mtdb) ;; ok to delay on mtdb
	      (db:clean-up mtdb)
	      (db:clean-up tmpdb)
              (db:clean-up refndb)))

	;; adjust test-ids to fit into proper range
	;;
	;; (if (member 'adj-testids options)
	;;     (begin
	;;       (db:delay-if-busy mtdb)
	;;       (db:prep-megatest.db-for-migration mtdb)))

	;; sync runs, test_meta etc.
	;;
	(if (member 'old2new options)
	    ;; (begin
            (set! data-synced
                  (+ (db:sync-tables (db:sync-all-tables-list dbstruct) #f mtdb tmpdb refndb)
                     data-synced)))
			      ;; (db:sync-main-list mtdb) mtdb (db:get-db dbstruct #f))
;; 	      (for-each 
;; 	       (lambda (run-id)
;; 		 (db:delay-if-busy mtdb)
;; 		 (let ((testrecs (db:get-all-tests-info-by-run-id mtdb run-id)))
;; ;;		       (dbstruct (if toppath (make-dbr:dbstruct path: toppath local: #t) #f)))
;; 		   (debug:print 0 *default-log-port* "INFO: Propagating " (length testrecs) " records for run-id=" run-id " to run specific db")
;; 		   (db:replace-test-records dbstruct run-id testrecs)
;; 		   (sqlite3:finalize! (db:dbdat-get-db (dbr:dbstruct-rundb dbstruct)))))
;; 	       run-ids)))

	;; now ensure all newdb data are synced to megatest.db
	;; do not use the run-ids list passed in to the function
	;;
	(if (member 'new2old options)
	    (set! data-synced
		  (+ (db:sync-tables (db:sync-all-tables-list dbstruct) #f tmpdb refndb mtdb)
		      data-synced)))



        (if (member 'fixschema options)
            (begin
              (db:patch-schema-maindb (db:dbdat-get-db mtdb))
              (db:patch-schema-maindb (db:dbdat-get-db tmpdb))
              (db:patch-schema-maindb (db:dbdat-get-db refndb))
              (db:patch-schema-rundb  (db:dbdat-get-db mtdb))
              (db:patch-schema-rundb  (db:dbdat-get-db tmpdb))
              (db:patch-schema-rundb  (db:dbdat-get-db refndb))))
              
	;; (let* ((maindb      (make-dbr:dbstruct path: toppath local: #t))
	;; 	   (src-run-ids (if run-ids run-ids (db:get-all-run-ids (db:dbdat-get-db (db:get-db maindb 0)))))
	;; 	   (all-run-ids (sort (delete-duplicates (cons 0 src-run-ids)) <))
	;; 	   (count       1)
	;; 	   (total       (length all-run-ids))
	;; 	   (dead-runs  '()))
	;;   ;; first fix schema if needed
	;;   (map
	;;    (lambda (th)
	;; 	 (thread-join! th))
	;;    (map
	;; 	(lambda (run-id)
	;; 	  (thread-start! 
	;; 	   (make-thread
	;; 	    (lambda ()
	;; 	      (let* ((fromdb (if toppath (make-dbr:dbstruct path: toppath local: #t) #f))
;;                    (if (member 'schema options)
	;; 		(if (eq? run-id 0)
	;; 		    (let ((maindb  (db:dbdat-get-db (db:get-db fromdb #f))))
	;; 		      (db:patch-schema-maindb run-id maindb))
	;; 		    (db:patch-schema-rundb run-id frundb)))
	;; 	      (set! count (+ count 1))
	;; 	      (debug:print 0 *default-log-port* "Finished patching schema for " (if (eq? run-id 0) " main.db " (conc run-id ".db")) ", " count " of " total)))))
	;; 	all-run-ids))
	;;   ;; Then sync and fix db's
	;;   (set! count 0)
	;;   (process-fork
	;;    (lambda ()
	;; 	 (map
	;; 	  (lambda (th)
	;; 	    (thread-join! th))
	;; 	  (map
	;; 	   (lambda (run-id)
	;; 	     (thread-start! 
	;; 	      (make-thread
	;; 	       (lambda ()
	;; 		 (let* ((fromdb (if toppath (make-dbr:dbstruct path: toppath local: #t) #f))
	;; 			(frundb (db:dbdat-get-db (db:get-db fromdb run-id))))
	;; 		   (if (eq? run-id 0)
	;; 		       (let ((maindb  (db:dbdat-get-db (db:get-db fromdb #f))))
;;                             (db:sync-tables (db:sync-main-list dbstruct) #f (db:get-db fromdb #f) mtdb)
	;; 			 (set! dead-runs (db:clean-up-maindb (db:get-db fromdb #f))))
	;; 		       (begin
	;; 			 ;; NB// must sync first to ensure deleted tests get marked as such in megatest.db
;;                             (db:sync-tables db:sync-tests-only #f (db:get-db fromdb run-id) mtdb)
	;; 			 (db:clean-up-rundb (db:get-db fromdb run-id)))))
	;; 		 (set! count (+ count 1))
	;; 		 (debug:print 0 *default-log-port* "Finished clean up of "
	;; 			      (if (eq? run-id 0)
	;; 				  " main.db " (conc run-id ".db")) ", " count " of " total)))))
	;; 	   all-run-ids))))

	;; removed deleted runs
;; (let ((dbdir (tasks:get-task-db-path)))
;;   (for-each (lambda (run-id)
;; 	      (let ((fullname (conc dbdir "/" run-id ".db")))
;; 		(if (file-exists? fullname)
;; 		    (begin
;; 		      (debug:print 0 *default-log-port* "Removing database file for deleted run " fullname)
;; 		      (delete-file fullname)))))
;; 	    dead-runs))))
;; 
	;; (db:close-all dbstruct)
	;; (sqlite3:finalize! mdb)
        (stack-push! (dbr:dbstruct-dbstack dbstruct) tmpdb)
	data-synced)))

;; keeping it around for debugging purposes only
(define (open-run-close-no-exception-handling  proc idb . params)
  (debug:print-info 11 *default-log-port* "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (print "I don't work anymore. open-run-close-no-exception-handling needs fixing or removing...")
  (exit)
  (if (or *db-write-access*
	  (not #t)) ;; was: (member proc * db:all-write-procs *)))
      (let* ((db (cond
		  ((pair? idb)                 (db:dbdat-get-db idb))
		  ((sqlite3:database? idb)     idb)
		  ((not idb)                   (debug:print-error 0 *default-log-port* "cannot open-run-close with #f anymore"))
		  ((procedure? idb)            (idb))
		  (else   	               (debug:print-error 0 *default-log-port* "cannot open-run-close with #f anymore"))))
	     (res #f))
	(set! res (apply proc db params))
	(if (not idb)(sqlite3:finalize! dbstruct))
	(debug:print-info 11 *default-log-port* "open-run-close-no-exception-handling END" )
	res)
      #f))

(define (open-run-close-exception-handling proc idb . params)
  (handle-exceptions
   exn
   (let ((sleep-time (random 30))
	 (err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
     (case err-status
       ((busy)
	(thread-sleep! sleep-time))
       (else
	(debug:print 0 *default-log-port* "EXCEPTION: database probably overloaded or unreadable.")
	(debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
	(print "exn=" (condition->list exn))
	(debug:print 0 *default-log-port* " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
	(print-call-chain (current-error-port))
	(thread-sleep! sleep-time)
	(debug:print-info 0 *default-log-port* "trying db call one more time....this may never recover, if necessary kill process " (current-process-id) " on host " (get-host-name) " to clean up")))
     (apply open-run-close-exception-handling proc idb params))
   (apply open-run-close-no-exception-handling proc idb params)))

;; (define open-run-close 
(define open-run-close open-run-close-exception-handling)
		;;	   open-run-close-no-exception-handling
;;			   open-run-close-exception-handling)
;;)

(define (db:initialize-main-db dbdat)
  (let* ((configdat (car *configinfo*))  ;; tut tut, global warning...
	 (keys     (keys:config-get-fields configdat))
	 (havekeys (> (length keys) 0))
	 (keystr   (keys->keystr keys))
	 (fieldstr (keys->key/field keys))
	 (db       (db:dbdat-get-db dbdat)))
    (for-each (lambda (key)
		(let ((keyn key))
		  (if (member (string-downcase keyn)
			      (list "runname" "state" "status" "owner" "event_time" "comment" "fail_count"
				    "pass_count"))
		      (begin
			(print "ERROR: your key cannot be named " keyn " as this conflicts with the same named field in the runs table, you must remove your megatest.db and <linktree>/.db before trying again.")
			(exit 1)))))
	      keys)
    (sqlite3:with-transaction
     db
     (lambda ()
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS keys (id INTEGER PRIMARY KEY, fieldname TEXT, fieldtype TEXT, CONSTRAINT keyconstraint UNIQUE (fieldname));")
       (for-each (lambda (key)
		   (sqlite3:execute db "INSERT OR REPLACE INTO keys (fieldname,fieldtype) VALUES (?,?);" key "TEXT"))
		 keys)
       (sqlite3:execute db (conc 
			    "CREATE TABLE IF NOT EXISTS runs (id INTEGER PRIMARY KEY, \n			 " 
			    fieldstr (if havekeys "," "") "
			 runname    TEXT DEFAULT 'norun',
			 state      TEXT DEFAULT '',
			 status     TEXT DEFAULT '',
			 owner      TEXT DEFAULT '',
			 event_time TIMESTAMP DEFAULT (strftime('%s','now')),
			 comment    TEXT DEFAULT '',
			 fail_count INTEGER DEFAULT 0,
			 pass_count INTEGER DEFAULT 0,
                         last_update INTEGER DEFAULT (strftime('%s','now')),
			 CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
       (sqlite3:execute db "CREATE TRIGGER IF NOT EXISTS update_runs_trigger AFTER UPDATE ON runs
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE runs SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS run_stats (
                              id     INTEGER PRIMARY KEY,
                              run_id INTEGER,
                              state  TEXT,
                              status TEXT,
                              count  INTEGER,
                              last_update INTEGER DEFAULT (strftime('%s','now')))")
       (sqlite3:execute db "CREATE TRIGGER  IF NOT EXISTS update_run_stats_trigger AFTER UPDATE ON run_stats
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE run_stats SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_meta (
                                     id          INTEGER PRIMARY KEY,
                                     testname    TEXT DEFAULT '',
                                     author      TEXT DEFAULT '',
                                     owner       TEXT DEFAULT '',
                                     description TEXT DEFAULT '',
                                     reviewed    TIMESTAMP,
                                     iterated    TEXT DEFAULT '',
                                     avg_runtime REAL,
                                     avg_disk    REAL,
                                     tags        TEXT DEFAULT '',
                                     jobgroup    TEXT DEFAULT 'default',
                                CONSTRAINT test_meta_constraint UNIQUE (testname));")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS tasks_queue (id INTEGER PRIMARY KEY,
                                action TEXT DEFAULT '',
                                owner TEXT,
                                state TEXT DEFAULT 'new',
                                target TEXT DEFAULT '',
                                name TEXT DEFAULT '',
                                testpatt TEXT DEFAULT '',
                                keylock TEXT,
                                params TEXT,
                                creation_time TIMESTAMP DEFAULT (strftime('%s','now')),
                                execution_time TIMESTAMP);")
       ;; archive disk areas, cached info from [archive-disks]
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS archive_disks (
                                id INTEGER PRIMARY KEY,
                                archive_area_name TEXT,
                                disk_path TEXT,
                                last_df INTEGER DEFAULT -1,
                                last_df_time TIMESTAMP DEFAULT (strftime('%s','now')),
                                creation_time TIMESTAMP DEFAULT (strftime('%','now')));")
       ;; individual bup (or tar) data chunks
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS archive_blocks (
                                id INTEGER PRIMARY KEY,
                                archive_disk_id INTEGER,
                                disk_path TEXT,
                                last_du INTEGER DEFAULT -1,
                                last_du_time TIMESTAMP DEFAULT (strftime('%s','now')),
                                creation_time TIMESTAMP DEFAULT (strftime('%','now')));")
       ;; tests allocated to what chunks. reusing a chunk for a test/item_path is very efficient
       ;; NB// the per run/test recording of where the archive is stored is done in the test
       ;;      record. 
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS archive_allocations (
                                id INTEGER PRIMARY KEY,
                                archive_block_id INTEGER,
                                testname TEXT,
                                item_path TEXT,
                                creation_time TIMESTAMP DEFAULT (strftime('%','now')));")
       ;; move this clean up call somewhere else
       (sqlite3:execute db "DELETE FROM tasks_queue WHERE state='done' AND creation_time < ?;" (- (current-seconds)(* 24 60 60))) ;; remove older than 24 hrs
       (sqlite3:execute db (conc "CREATE INDEX IF NOT EXISTS runs_index ON runs (runname" (if havekeys "," "") keystr ");"))
       ;; (sqlite3:execute db "CREATE VIEW runs_tests AS SELECT * FROM runs INNER JOIN tests ON runs.id=tests.run_id;")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS extradat (id INTEGER PRIMARY KEY, run_id INTEGER, key TEXT, val TEXT);")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS metadat (id INTEGER PRIMARY KEY, var TEXT, val TEXT,
                                  CONSTRAINT metadat_constraint UNIQUE (var));")
       (sqlite3:execute db "CREATE TABLE IF NOT EXISTS access_log (id INTEGER PRIMARY KEY, user TEXT, accessed TIMESTAMP, args TEXT);")
       ;; Must do this *after* running patch db !! No more. 
       ;; cannot use db:set-var since it will deadlock, hardwire the code here
       (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" "MEGATEST_VERSION" (common:version-signature))
       (debug:print-info 11 *default-log-port* "db:initialize END")))))

;;======================================================================
;; R U N   S P E C I F I C   D B 
;;======================================================================

(define (db:initialize-run-id-db db)
  (sqlite3:with-transaction 
   db
   (lambda ()
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS tests 
                    (id INTEGER PRIMARY KEY,
                     run_id       INTEGER   DEFAULT -1,
                     testname     TEXT      DEFAULT 'noname',
                     host         TEXT      DEFAULT 'n/a',
                     cpuload      REAL      DEFAULT -1,
                     diskfree     INTEGER   DEFAULT -1,
                     uname        TEXT      DEFAULT 'n/a', 
                     rundir       TEXT      DEFAULT '/tmp/badname',
                     shortdir     TEXT      DEFAULT '/tmp/badname',
                     item_path    TEXT      DEFAULT '',
                     state        TEXT      DEFAULT 'NOT_STARTED',
                     status       TEXT      DEFAULT 'FAIL',
                     attemptnum   INTEGER   DEFAULT 0,
                     final_logf   TEXT      DEFAULT 'logs/final.log',
                     logdat       TEXT      DEFAULT '', 
                     run_duration INTEGER   DEFAULT 0,
                     comment      TEXT      DEFAULT '',
                     event_time   TIMESTAMP DEFAULT (strftime('%s','now')),
                     fail_count   INTEGER   DEFAULT 0,
                     pass_count   INTEGER   DEFAULT 0,
                     archived     INTEGER   DEFAULT 0, -- 0=no, > 1=archive block id where test data can be found
                     last_update  INTEGER DEFAULT (strftime('%s','now')),
                        CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path));")
     (sqlite3:execute db "CREATE INDEX IF NOT EXISTS tests_index ON tests (run_id, testname, item_path, uname);")
     (sqlite3:execute db "CREATE TRIGGER  IF NOT EXISTS update_tests_trigger AFTER UPDATE ON tests
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE tests SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_steps 
                              (id INTEGER PRIMARY KEY,
                               test_id INTEGER, 
                               stepname TEXT, 
                               state TEXT DEFAULT 'NOT_STARTED', 
                               status TEXT DEFAULT 'n/a',
                               event_time TIMESTAMP,
                               comment TEXT DEFAULT '',
                               logfile TEXT DEFAULT '',
                               last_update  INTEGER DEFAULT (strftime('%s','now')),
                               CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
     (sqlite3:execute db "CREATE INDEX IF NOT EXISTS teststeps_index ON tests (run_id, testname, item_path);")
     (sqlite3:execute db "CREATE TRIGGER  IF NOT EXISTS update_teststeps_trigger AFTER UPDATE ON test_steps
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE test_steps SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data (id INTEGER PRIMARY KEY,
                                test_id INTEGER,
                                category TEXT DEFAULT '',
                                variable TEXT,
	                        value REAL,
	                        expected REAL,
	                        tol REAL,
                                units TEXT,
                                comment TEXT DEFAULT '',
                                status TEXT DEFAULT 'n/a',
                                type TEXT DEFAULT '',
                                last_update  INTEGER DEFAULT (strftime('%s','now')),
                              CONSTRAINT test_data_constraint UNIQUE (test_id,category,variable));")
     (sqlite3:execute db "CREATE INDEX IF NOT EXISTS test_data_index ON test_data (test_id);")
     (sqlite3:execute db "CREATE TRIGGER  IF NOT EXISTS update_test_data_trigger AFTER UPDATE ON test_data
                             FOR EACH ROW
                               BEGIN 
                                 UPDATE test_data SET last_update=(strftime('%s','now'))
                                   WHERE id=old.id;
                               END;")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_rundat (
                              id           INTEGER PRIMARY KEY,
                              test_id      INTEGER,
                              update_time  TIMESTAMP,
                              cpuload      INTEGER DEFAULT -1,
                              diskfree     INTEGER DEFAULT -1,
                              diskusage    INTGER DEFAULT -1,
                              run_duration INTEGER DEFAULT 0);")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS archives (
                              id           INTEGER PRIMARY KEY,
                              test_id      INTEGER,
                              state        TEXT DEFAULT 'new',
                              status       TEXT DEFAULT 'n/a',
                              archive_type TEXT DEFAULT 'bup',
                              du           INTEGER,
                              archive_path TEXT);")))
  db)

;;======================================================================
;; A R C H I V E S
;;======================================================================

;; dneeded is minimum space needed, scan for existing archives that 
;; are on disks with adequate space and already have this test/itempath
;; archived
;;
(define (db:archive-get-allocations dbstruct testname itempath dneeded)
  (let* ((dbdat        (db:get-db dbstruct)) ;; archive tables are in main.db
	 (db           (db:dbdat-get-db dbdat))
	 (res          '())
	 (blocks       '())) ;; a block is an archive chunck that can be added too if there is space
    (sqlite3:for-each-row
     (lambda (id archive-disk-id disk-path last-du last-du-time)
       (set! res (cons (vector id archive-disk-id disk-path last-du last-du-time) res)))
     db
     "SELECT b.id,b.archive_disk_id,b.disk_path,b.last_du,b.last_du_time FROM archive_blocks AS b
        INNER JOIN archive_allocations AS a ON a.archive_block_id=b.id
        WHERE a.testname=? AND a.item_path=?;" 
     testname itempath)
    ;; Now res has list of candidate paths, look in archive_disks for candidate with potential free space
    (if (null? res)
	'()
	(sqlite3:for-each-row
	 (lambda (id archive-area-name disk-path last-df last-df-time)
	   (set! blocks (cons (vector id archive-area-name disk-path last-df last-df-time) blocks)))
	 db 
	 (conc
	  "SELECT d.id,d.archive_area_name,disk_path,last_df,last_df_time FROM archive_disks AS d
             INNER JOIN archive_blocks AS b ON d.id=b.archive_disk_id
             WHERE b.id IN (" (string-intersperse (map conc res) ",") ") AND
         last_df > ?;")
	 dneeded))
    blocks))
    
;; returns id of the record, register a disk allocated to archiving and record it's last known
;; available space
;;
(define (db:archive-register-disk dbstruct bdisk-name bdisk-path df)
  (let* ((dbdat        (db:get-db dbstruct)) ;; archive tables are in main.db
	 (db           (db:dbdat-get-db dbdat))
	 (res          #f))
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     db
     "SELECT id FROM archive_disks WHERE archive_area_name=? AND disk_path=?;"
     bdisk-name bdisk-path)
    (if res ;; record exists, update df and return id
	(begin
	  (sqlite3:execute db "UPDATE archive_disks SET last_df=?,last_df_time=(strftime('%s','now'))
                                  WHERE archive_area_name=? AND disk_path=?;"
			   df bdisk-name bdisk-path)
          (stack-push! (dbr:dbstruct-dbstack dbstruct) dbdat)
	  res)
	(begin
	  (sqlite3:execute
	   db
	   "INSERT OR REPLACE INTO archive_disks (archive_area_name,disk_path,last_df)
                VALUES (?,?,?);"
	   bdisk-name bdisk-path df)
          (stack-push! (dbr:dbstruct-dbstack dbstruct) dbdat)
	  (db:archive-register-disk dbstruct bdisk-name bdisk-path df)))))

;; record an archive path created on a given archive disk (identified by it's bdisk-id)
;; if path starts with / then it is full, otherwise it is relative to the archive disk
;; preference is to store the relative path.
;;
(define (db:archive-register-block-name dbstruct bdisk-id archive-path #!key (du #f))
  (let* ((dbdat        (db:get-db dbstruct)) ;; archive tables are in main.db
	 (db           (db:dbdat-get-db dbdat))
	 (res          #f))
    ;; first look to see if this path is already registered
    (sqlite3:for-each-row
     (lambda (id)
       (set! res id))
     db
     "SELECT id FROM archive_blocks WHERE archive_disk_id=? AND disk_path=?;"
     bdisk-id archive-path)
    (if res ;; record exists, update du if applicable and return res
	(begin
	  (if du (sqlite3:exectute db "UPDATE archive_blocks SET last_du=?,last_du_time=(strftime('%s','now'))
                                          WHERE archive_disk_id=? AND disk_path=?;"
				   bdisk-id archive-path du))
	  res)
	(begin
	  (sqlite3:execute db "INSERT OR REPLACE INTO archive_blocks (archive_disk_id,disk_path,last_du)
                                                        VALUES (?,?,?);"
			   bdisk-id archive-path (or du 0))
	  (db:archive-register-block-name dbstruct bdisk-id archive-path du: du)))))


;; The "archived" field in tests is overloaded; 0 = not archived, > 0 archived in block with given id
;;
(define (db:test-set-archive-block-id dbstruct run-id test-id archive-block-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:execute db "UPDATE tests SET archived=? WHERE id=?;"
		      archive-block-id test-id))))
 
;; Look up the archive block info given a block-id
;;
(define (db:test-get-archive-block-info dbstruct archive-block-id)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row 
	;;        0         1           2        3          4           5
	(lambda (id archive-disk-id disk-path last-du last-du-time creation-time)
	  (set! res (vector id archive-disk-id disk-path last-du last-du-time creation-time)))
	db
	"SELECT id,archive_disk_id,disk_path,last_du,last_du_time,creation_time FROM archive_blocks WHERE id=?;"
	archive-block-id)
       res))))

;; (define (db:archive-allocate-testsuite/area-to-block block-id testsuite-name areakey)
;;   (let* ((dbdat        (db:get-db dbstruct #f)) ;; archive tables are in main.db
;; 	 (db           (db:dbdat-get-db dbdat))
;; 	 (res          '())
;; 	 (blocks       '())) ;; a block is an archive chunck that can be added too if there is space
;;     (sqlite3:for-each-row  #f)

;;======================================================================
;; L O G G I N G    D B 
;;======================================================================

(define (open-logging-db)
  (let* ((dbpath    (conc (if *toppath* (conc *toppath* "/") "") "logging.db")) ;; fname)
	 (dbexists  (file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath))
	 (handler   (make-busy-timeout (if (args:get-arg "-override-timeout")
					   (string->number (args:get-arg "-override-timeout"))
					   136000)))) ;; 136000)))
    (sqlite3:set-busy-handler! db handler)
    (if (not dbexists)
	(begin
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS log (id INTEGER PRIMARY KEY,event_time TIMESTAMP DEFAULT (strftime('%s','now')),logline TEXT,pwd TEXT,cmdline TEXT,pid INTEGER);")
	  (db:set-sync db) ;; (sqlite3:execute db (conc "PRAGMA synchronous = 0;"))
	  ))
    db))

(define (db:log-local-event . loglst)
  (let ((logline (apply conc loglst)))
    (db:log-event logline)))

(define (db:log-event logline)
  (let ((db (open-logging-db)))
    (sqlite3:execute db "INSERT INTO log (logline,pwd,cmdline,pid) VALUES (?,?,?,?);"
		     logline
		     (current-directory)
		     (string-intersperse (argv) " ")
		     (current-process-id))
    (sqlite3:finalize! db)
    logline))

;;======================================================================
;; D B   U T I L S
;;======================================================================

;;======================================================================
;; M A I N T E N A N C E
;;======================================================================

(define (db:have-incompletes? dbstruct run-id ovr-deadtime)
  (let* ((incompleted '())
	 (oldlaunched '())
	 (toplevels   '())
	 (deadtime-str (configf:lookup *configdat* "setup" "deadtime"))
	 (deadtime     (if (and deadtime-str
				(string->number deadtime-str))
			   (string->number deadtime-str)
			   7200))) ;; two hours
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (if (number? ovr-deadtime)(set! deadtime ovr-deadtime))
       
       ;; in RUNNING or REMOTEHOSTSTART for more than 10 minutes
       ;;
       ;; HOWEVER: this code in run:test seems to work fine
       ;;              (> (- (current-seconds)(+ (db:test-get-event_time testdat)
       ;;                     (db:test-get-run_duration testdat)))
       ;;                    600) 
       ;; (db:delay-if-busy dbdat)
       (sqlite3:for-each-row 
        (lambda (test-id run-dir uname testname item-path)
          (if (and (equal? uname "n/a")
                   (equal? item-path "")) ;; this is a toplevel test
              ;; what to do with toplevel? call rollup?
              (begin
                (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
                (debug:print-info 0 *default-log-port* "Found old toplevel test in RUNNING state, test-id=" test-id))
              (set! incompleted (cons (list test-id run-dir uname testname item-path run-id) incompleted))))
        db
        "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > (run_duration + ?) AND state IN ('RUNNING','REMOTEHOSTSTART');"
        run-id deadtime)

       ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
       ;;
       ;; (db:delay-if-busy dbdat)
       (sqlite3:for-each-row
        (lambda (test-id run-dir uname testname item-path)
          (if (and (equal? uname "n/a")
                   (equal? item-path "")) ;; this is a toplevel test
              ;; what to do with toplevel? call rollup?
              (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
              (set! oldlaunched (cons (list test-id run-dir uname testname item-path run-id) oldlaunched))))
        db
        "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > 86400 AND state IN ('LAUNCHED');"
        run-id)
       
       (debug:print-info 18 *default-log-port* "Found " (length oldlaunched) " old LAUNCHED items, " (length toplevels) " old LAUNCHED toplevel tests and " (length incompleted) " tests marked RUNNING but apparently dead.")
       (if (and (null? incompleted)
                (null? oldlaunched)
                (null? toplevels))
           #f
           #t)))))

;; given a launch delay (minimum time from last launch) return amount of time to wait
;;
;; (define (db:launch-delay-left dbstruct run-id launch-delay)
  

;;  select end_time-now from
;;      (select testname,item_path,event_time+run_duration as
;;                          end_time,strftime('%s','now') as now from tests where state in
;;      ('RUNNING','REMOTEHOSTSTART','LAUNCED'));

(define (db:find-and-mark-incomplete dbstruct run-id ovr-deadtime)
  (let* ((incompleted '())
	 (oldlaunched '())
	 (toplevels   '())
	 (deadtime-str (configf:lookup *configdat* "setup" "deadtime"))
	 (deadtime     (if (and deadtime-str
				(string->number deadtime-str))
			   (string->number deadtime-str)
			   7200))) ;; two hours
    (db:with-db 
     dbstruct #f #f
     (lambda (db)
       (if (number? ovr-deadtime)(set! deadtime ovr-deadtime))
       
       ;; in RUNNING or REMOTEHOSTSTART for more than 10 minutes
       ;;
       ;; HOWEVER: this code in run:test seems to work fine
       ;;              (> (- (current-seconds)(+ (db:test-get-event_time testdat)
       ;;                     (db:test-get-run_duration testdat)))
       ;;                    600) 
       ;; (db:delay-if-busy dbdat)
       (sqlite3:for-each-row 
        (lambda (test-id run-dir uname testname item-path)
          (if (and (equal? uname "n/a")
                   (equal? item-path "")) ;; this is a toplevel test
              ;; what to do with toplevel? call rollup?
              (begin
                (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
                (debug:print-info 0 *default-log-port* "Found old toplevel test in RUNNING state, test-id=" test-id))
              (set! incompleted (cons (list test-id run-dir uname testname item-path run-id) incompleted))))
        db
        "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > (run_duration + ?) AND state IN ('RUNNING','REMOTEHOSTSTART');"
        run-id deadtime)

       ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
       ;;
       ;; (db:delay-if-busy dbdat)
       (sqlite3:for-each-row
        (lambda (test-id run-dir uname testname item-path)
          (if (and (equal? uname "n/a")
                   (equal? item-path "")) ;; this is a toplevel test
              ;; what to do with toplevel? call rollup?
              (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
              (set! oldlaunched (cons (list test-id run-dir uname testname item-path run-id) oldlaunched))))
        db
        "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > 86400 AND state IN ('LAUNCHED');"
        run-id)
       
       (debug:print-info 18 *default-log-port* "Found " (length oldlaunched) " old LAUNCHED items, " (length toplevels) " old LAUNCHED toplevel tests and " (length incompleted) " tests marked RUNNING but apparently dead.")

       ;; These are defunct tests, do not do all the overhead of set-state-status. Force them to INCOMPLETE.
       ;;
       ;; (db:delay-if-busy dbdat)
       (let* (;; (min-incompleted (filter (lambda (x)
              ;;      		      (let* ((testpath (cadr x))
              ;;      			     (tdatpath (conc testpath "/testdat.db"))
              ;;      			     (dbexists (file-exists? tdatpath)))
              ;;      			(or (not dbexists) ;; if no file then something wrong - mark as incomplete
              ;;      			    (> (- (current-seconds)(file-modification-time tdatpath)) 600)))) ;; no change in 10 minutes to testdat.db - she's dead Jim
              ;;      		    incompleted))
              (min-incompleted-ids (map car incompleted)) ;; do 'em all
              (all-ids             (append min-incompleted-ids (map car oldlaunched))))
         (if (> (length all-ids) 0)
             (begin
               (debug:print 0 *default-log-port* "WARNING: Marking test(s); " (string-intersperse (map conc all-ids) ", ") " as INCOMPLETE")
               (for-each
                (lambda (test-id)
                  (db:test-set-state-status dbstruct run-id test-id "COMPLETE" "DEAD" "Test failed to complete"))
                all-ids))))))))

;; ALL REPLACED BY THE BLOCK ABOVE
;;
;; 	    (sqlite3:execute 
;; 	     db
;; 	     (conc "UPDATE tests SET state='INCOMPLETE' WHERE run_id=? AND id IN (" 
;; 		   (string-intersperse (map conc all-ids) ",")
;; 		   ");")
;;              run-id))))
;; 
;;     ;; Now do rollups for the toplevel tests
;;     ;;
;;     ;; (db:delay-if-busy dbdat)
;;     (for-each
;;      (lambda (toptest)
;;        (let ((test-name (list-ref toptest 3)))
;; ;;	     (run-id    (list-ref toptest 5)))
;; 	 (db:top-test-set-per-pf-counts dbstruct run-id test-name)))
;;      toplevels)))

;; BUG: Probably broken - does not explicitly use run-id in the query
;;
(define (db:top-test-set-per-pf-counts dbstruct run-id test-name)
  (db:general-call dbstruct 'top-test-set-per-pf-counts (list test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name)))

;; Clean out old junk and vacuum the database
;;
;; Ultimately do something like this:
;;
;; 1. Look at test records either deleted or part of deleted run:
;;    a. If test dir exists, set the the test to state='UNKNOWN', Set the run to 'unknown'
;;    b. If test dir gone, delete the test record
;; 2. Look at run records
;;    a. If have tests that are not deleted, set state='unknown'
;;    b. ....
;;
(define (db:clean-up dbdat)
  ;; (debug:print 0 *default-log-port* "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
  (let* ((db         (db:dbdat-get-db dbdat))
	 (count-stmt (sqlite3:prepare db "SELECT (SELECT count(id) FROM tests)+(SELECT count(id) FROM runs);"))
	(statements
	 (map (lambda (stmt)
		(sqlite3:prepare db stmt))
	      (list
	       ;; delete all tests that belong to runs that are 'deleted'
	       "DELETE FROM tests WHERE run_id in (SELECT id FROM runs WHERE state='deleted');"
	       ;; delete all tests that are 'DELETED'
	       "DELETE FROM tests WHERE state='DELETED';"
	       ;; delete all tests that have no run
	       "DELETE FROM tests WHERE run_id NOT IN (SELECT DISTINCT id FROM runs);"
	       ;; delete all runs that are state='deleted'
	       "DELETE FROM runs WHERE state='deleted';"
	       ;; delete empty runs
	       "DELETE FROM runs WHERE id NOT IN (SELECT DISTINCT r.id FROM runs AS r INNER JOIN tests AS t ON t.run_id=r.id);"
	       ))))
    ;; (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    ;; (db:delay-if-busy dbdat)
    (sqlite3:execute db "VACUUM;")))

;; Clean out old junk and vacuum the database
;;
;; Ultimately do something like this:
;;
;; 1. Look at test records either deleted or part of deleted run:
;;    a. If test dir exists, set the the test to state='UNKNOWN', Set the run to 'unknown'
;;    b. If test dir gone, delete the test record
;; 2. Look at run records
;;    a. If have tests that are not deleted, set state='unknown'
;;    b. ....
;;
(define (db:clean-up-rundb dbdat)
  ;; (debug:print 0 *default-log-port* "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
  (let* ((db         (db:dbdat-get-db dbdat))
	 (count-stmt (sqlite3:prepare db "SELECT (SELECT count(id) FROM tests);"))
	(statements
	 (map (lambda (stmt)
		(sqlite3:prepare db stmt))
	      (list
	       ;; delete all tests that belong to runs that are 'deleted'
	       ;; (conc "DELETE FROM tests WHERE run_id NOT IN (" (string-intersperse (map conc valid-runs) ",") ");")
	       ;; delete all tests that are 'DELETED'
	       "DELETE FROM tests WHERE state='DELETED';"
	       ))))
    ;; (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    ;; (db:delay-if-busy dbdat)
    (sqlite3:execute db "VACUUM;")))

;; Clean out old junk and vacuum the database
;;
;; Ultimately do something like this:
;;
;; 1. Look at test records either deleted or part of deleted run:
;;    a. If test dir exists, set the the test to state='UNKNOWN', Set the run to 'unknown'
;;    b. If test dir gone, delete the test record
;; 2. Look at run records
;;    a. If have tests that are not deleted, set state='unknown'
;;    b. ....
;;
(define (db:clean-up-maindb dbdat)
  ;; (debug:print 0 *default-log-port* "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
  (let* ((db         (db:dbdat-get-db dbdat))
	 (count-stmt (sqlite3:prepare db "SELECT (SELECT count(id) FROM runs);"))
	 (statements
	  (map (lambda (stmt)
		 (sqlite3:prepare db stmt))
	       (list
		;; delete all tests that belong to runs that are 'deleted'
		;; (conc "DELETE FROM tests WHERE run_id NOT IN (" (string-intersperse (map conc valid-runs) ",") ");")
		;; delete all tests that are 'DELETED'
		"DELETE FROM runs WHERE state='deleted';"
		)))
	 (dead-runs '()))
    (sqlite3:for-each-row
     (lambda (run-id)
       (set! dead-runs (cons run-id dead-runs)))
       db
       "SELECT id FROM runs WHERE state='deleted';")
    ;; (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 *default-log-port* "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    ;; (db:delay-if-busy dbdat)
    (sqlite3:execute db "VACUUM;")
    dead-runs))

;;======================================================================
;; M E T A   G E T   A N D   S E T   V A R S
;;======================================================================

;; returns number if string->number is successful, string otherwise
;; also updates *global-delta*
;;
(define (db:get-var dbstruct var)
  (let* ((res      #f))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
        (lambda (val)
          (set! res val))
        db
        "SELECT val FROM metadat WHERE var=?;" var)
       ;; convert to number if can
       (if (string? res)
           (let ((valnum (string->number res)))
             (if valnum (set! res valnum))))
       res))))

;; This was part of db:get-var. It was used to estimate the load on
;; the database files.
;;
;; scale by 10, average with current value.
;;     (set! *global-delta* (/ (+ *global-delta* (* (- (current-milliseconds) start-ms)
;; 						 (if throttle throttle 0.01)))
;; 			    2))
;;     (if (> (abs (- *last-global-delta-printed* *global-delta*)) 0.08) ;; don't print all the time, only if it changes a bit
;; 	(begin
;; 	  (debug:print-info 4 *default-log-port* "launch throttle factor=" *global-delta*)
;; 	  (set! *last-global-delta-printed* *global-delta*)))

(define (db:set-var dbstruct var val)
  (db:with-db dbstruct #f #t 
	      (lambda (db)
		(sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val))))

(define (db:del-var dbstruct var)
  (db:with-db dbstruct #f #t 
	      (lambda (db)
		(sqlite3:execute db "DELETE FROM metadat WHERE var=?;" var))))

;; use a global for some primitive caching, it is just silly to
;; re-read the db over and over again for the keys since they never
;; change

;; why get the keys from the db? why not get from the *configdat*
;; using keys:config-get-fields?

(define (db:get-keys dbstruct)
  (if *db-keys* *db-keys* 
      (let ((res '()))
	(db:with-db dbstruct #f #f
		    (lambda (db)
		      (sqlite3:for-each-row 
		       (lambda (key)
			 (set! res (cons key res)))
		       db
		       "SELECT fieldname FROM keys ORDER BY id DESC;")))
	(set! *db-keys* res)
	res)))

;; look up values in a header/data structure
(define (db:get-value-by-header row header field)
  (if (or (null? header) (not row))
      #f
      (let loop ((hed (car header))
		 (tal (cdr header))
		 (n   0))
	(if (equal? hed field)
	    (vector-ref row n)
	    (if (null? tal) #f (loop (car tal)(cdr tal)(+ n 1)))))))

;; Accessors for the header/data structure
;; get rows and header from 
(define (db:get-header vec)(vector-ref vec 0))
(define (db:get-rows   vec)(vector-ref vec 1))

;;======================================================================
;;  R U N S
;;======================================================================

(define (db:get-run-name-from-id dbstruct run-id)
  (db:with-db 
   dbstruct
   #f ;; this is for the main runs db
   #f ;; does not modify db
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row
	(lambda (runname)
	  (set! res runname))
	db
	"SELECT runname FROM runs WHERE id=?;"
	run-id)
       res))))

(define (db:get-run-key-val dbstruct run-id key)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row
	(lambda (val)
	  (set! res val))
	db
	(conc "SELECT " key " FROM runs WHERE id=?;")
	run-id)
       res))))

;; keys list to key1,key2,key3 ...
(define (runs:get-std-run-fields keys remfields)
  (let* ((header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (list keystr header)))

;; make a query (fieldname like 'patt1' OR fieldname 
(define (db:patt->like fieldname pattstr #!key (comparator " OR "))
  (let ((patts (if (string? pattstr)
		   (string-split pattstr ",")
		   '("%"))))
    (string-intersperse (map (lambda (patt)
			       (let ((wildtype (if (substring-index "%" patt) "LIKE" "GLOB")))
				 (conc fieldname " " wildtype " '" patt "'")))
			     (if (null? patts)
				 '("")
				 patts))
			comparator)))


;; register a test run with the db, this accesses the main.db and does NOT
;; use server api
;;
(define (db:register-run dbstruct keyvals runname state status user)
  (let* ((keys      (map car keyvals))
	 (keystr    (keys->keystr keys))	 
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (allvals   (append (list runname state status user) (map cadr keyvals)))
	 (qryvals   (append (list runname) (map cadr keyvals)))
	 (key=?str  (string-intersperse (map (lambda (k)(conc k "=?")) keys) " AND ")))
    (debug:print 3 *default-log-port* "keys: " keys " allvals: " allvals " keyvals: " keyvals " key=?str is " key=?str)
    (debug:print 2 *default-log-port* "NOTE: using target " (string-intersperse (map cadr keyvals) "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvals))) ;; there must be a better way to "apply and"
	(db:with-db
	 dbstruct #f #f
	 (lambda (db)
	   (let ((res #f))
	     (apply sqlite3:execute db (conc "INSERT OR IGNORE INTO runs (runname,state,status,owner,event_time" comma keystr ") VALUES (?,?,?,?,strftime('%s','now')" comma valslots ");")
		    allvals)
	     (apply sqlite3:for-each-row 
		    (lambda (id)
		      (set! res id))
		    db
		    (let ((qry (conc "SELECT id FROM runs WHERE (runname=? " andstr key=?str ");")))
		      qry)
		    qryvals)
	     (sqlite3:execute db "UPDATE runs SET state=?,status=?,event_time=strftime('%s','now') WHERE id=? AND state='deleted';" state status res)
	     res))) 
	(begin
	  (debug:print-error 0 *default-log-port* "Called without all necessary keys")
	  #f))))

;; replace header and keystr with a call to runs:get-std-run-fields
;;
;; keypatts: ( (KEY1 "abc%def")(KEY2 "%") )
;; runpatts: patt1,patt2 ...
;;
(define (db:get-runs dbstruct runpatt count offset keypatts)
  (let* ((res       '())
	 (keys       (db:get-keys dbstruct))
	 (runpattstr (db:patt->like "runname" runpatt))
	 (remfields  (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header     (append keys remfields))
	 (keystr     (conc (keys->keystr keys) ","
			   (string-intersperse remfields ",")))
	 (qrystr     (conc "SELECT " keystr " FROM runs WHERE (" runpattstr ") " ;; runname LIKE ? "
			   ;; Generate: " AND x LIKE 'keypatt' ..."
			   (if (null? keypatts) ""
			       (conc " AND "
				     (string-join 
				      (map (lambda (keypatt)
					     (let ((key  (car keypatt))
						   (patt (cadr keypatt)))
					       (db:patt->like key patt)))
					   keypatts)
				      " AND ")))
			   " AND state != 'deleted' ORDER BY event_time DESC "
			   (if (number? count)
			       (conc " LIMIT " count)
			       "")
			   (if (number? offset)
			       (conc " OFFSET " offset)
			       ""))))
    (debug:print-info 11 *default-log-port* "db:get-runs START qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (db:with-db dbstruct #f #f
		(lambda (db)		
		  (sqlite3:for-each-row
		   (lambda (a . x)
		     (set! res (cons (apply vector a x) res)))
		   db
		   qrystr
		   )))
    (debug:print-info 11 *default-log-port* "db:get-runs END qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (vector header res)))

;; TODO: Switch this to use max(update_time) from each run db? Then if using a server there is no disk traffic (using inmem db)
;;
(define (db:get-changed-run-ids since-time)
  (let* ((dbdir      (db:dbfile-path)) ;; (configf:lookup *configdat* "setup" "dbdir"))
	 (alldbs     (glob (conc dbdir "/[0-9]*.db")))
	 (changed    (filter (lambda (dbfile)
			       (> (file-modification-time dbfile) since-time))
			     alldbs)))
    (delete-duplicates
     (map (lambda (dbfile)
	    (let* ((res (string-match ".*\\/(\\d)*\\.db" dbfile)))
	      (if res
		  (string->number (cadr res))
		  (begin
		    (debug:print 2 *default-log-port* "WARNING: Failed to process " dbfile " for run-id")
		    0))))
	  changed))))

;; Get all targets from the db
;;
(define (db:get-targets dbstruct)
  (let* ((res       '())
	 (keys       (db:get-keys dbstruct))
	 (header     keys) ;; (map key:get-fieldname keys))
	 (keystr     (keys->keystr keys))
	 (qrystr     (conc "SELECT " keystr " FROM runs WHERE state != 'deleted';"))
	 (seen       (make-hash-table)))
    (db:with-db
     dbstruct
     #f
     #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (a . x)
	  (let ((targ (cons a x)))
	    (if (not (hash-table-ref/default seen targ #f))
		(begin
		  (hash-table-set! seen targ #t)
		  (set! res (cons (apply vector targ) res))))))
	db
	qrystr)
       (debug:print-info 11 *default-log-port* "db:get-targets END qrystr: " qrystr )
       (vector header res)))))

;; just get count of runs
(define (db:get-num-runs dbstruct runpatt)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (let ((numruns 0))
       (debug:print-info 11 *default-log-port* "db:get-num-runs START " runpatt)
       (sqlite3:for-each-row 
	(lambda (count)
	  (set! numruns count))
	db
	"SELECT COUNT(id) FROM runs WHERE runname LIKE ? AND state != 'deleted';" runpatt)
       (debug:print-info 11 *default-log-port* "db:get-num-runs END " runpatt)
       numruns))))

;; (sqlite3#fold-row proc3670 init3671 db-or-stmt3672 . params3673)>
;; 
(define (db:get-raw-run-stats dbstruct run-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:fold-row
	(lambda (res state status count)
	  (cons (list state status count) res))
	'()
	db
	"SELECT state,status,count(id) AS count FROM tests WHERE run_id=? AND NOT(uname='n/a' AND item_path='') GROUP BY state,status;"
	run-id))))

;; Update run_stats for given run_id
;; input data is a list (state status count)
;;
(define (db:update-run-stats dbstruct run-id stats)
  ;; (mutex-lock! *db-transaction-mutex*)
  (db:with-db
   dbstruct
   #f
   #f

   (lambda (db)
     ;; remove previous data
     
     (let* ((stmt1 (sqlite3:prepare db "DELETE FROM run_stats WHERE run_id=? AND state=? AND status=?;"))
	    (stmt2 (sqlite3:prepare db "INSERT INTO run_stats (run_id,state,status,count) VALUES (?,?,?,?);"))
	    (res
	     (sqlite3:with-transaction
	      db
	      (lambda ()
		(for-each
		 (lambda (dat)
		   (sqlite3:execute stmt1 run-id (car dat)(cadr dat))
		   (apply sqlite3:execute stmt2 run-id dat))
		 stats)))))
       (sqlite3:finalize! stmt1)
       (sqlite3:finalize! stmt2)
       ;; (mutex-unlock! *db-transaction-mutex*)
       res))))

(define (db:get-main-run-stats dbstruct run-id)
  (db:with-db
   dbstruct
   #f ;; this data comes from main
   #f
   (lambda (db)
     (sqlite3:fold-row
	(lambda (res state status count)
	  (cons (list state status count) res))
	'()
	db
	"SELECT state,status,count FROM run_stats WHERE run_id=? AND run_id IN (SELECT id FROM runs WHERE state NOT IN ('DELETED','deleted'));"
	run-id))))

(define (db:print-current-query-stats)
  ;; generate stats from *db-api-call-time*
  (let ((ordered-keys (sort (hash-table-keys *db-api-call-time*)
			    (lambda (a b)
			      (let ((sum-a (common:sum (hash-table-ref *db-api-call-time* a)))
				    (sum-b (common:sum (hash-table-ref *db-api-call-time* b))))
				(> sum-a sum-b))))))
    (for-each
     (lambda (cmd-key)
       (let* ((dat  (hash-table-ref *db-api-call-time* cmd-key))
	      (avg  (if (> (length dat) 0)
			(/ (common:sum dat)(length dat)))))
	 (debug:print-info 0 *default-log-port* cmd-key "\tavg: " avg " max: " (common:max dat) " min: " (common:min-max < dat) " num: " (length dat))))
     ordered-keys)))

(define (db:get-all-run-ids dbstruct)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (let ((run-ids '()))
       (sqlite3:for-each-row
	(lambda (run-id)
	  (set! run-ids (cons run-id run-ids)))
	db
	"SELECT id FROM runs WHERE state != 'deleted' ORDER BY event_time DESC;")
    (reverse run-ids)))))

;; get some basic run stats
;;
;; data structure:
;;
;; ( (runname (( state  count ) ... ))
;;   (   ...
;;
(define (db:get-run-stats dbstruct)
  (let* ((totals       (make-hash-table))
	 (curr         (make-hash-table))
	 (res          '())
	 (runs-info    '()))
    ;; First get all the runname/run-ids
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (run-id runname)
	  (set! runs-info (cons (list run-id runname) runs-info)))
	db
	"SELECT id,runname FROM runs WHERE state != 'deleted' ORDER BY event_time DESC;"))) ;; If you change this to the more logical ASC please adjust calls to db:get-run-stats
    ;; for each run get stats data
    (for-each
     (lambda (run-info)
       ;; get the net state/status counts for this run
       (let* ((run-id   (car  run-info))
	      (run-name (cadr run-info)))
	 (db:with-db
	  dbstruct
	  run-id
	  #f
	  (lambda (db)
	    (sqlite3:for-each-row
	     (lambda (state status count)
	       (let ((netstate (if (equal? state "COMPLETED") status state)))
		 (if (string? netstate)
		     (begin
		       (hash-table-set! totals netstate (+ (hash-table-ref/default totals netstate 0) count))
		       (hash-table-set! curr   netstate (+ (hash-table-ref/default curr   netstate 0) count))))))
	     db
	     "SELECT state,status,count(id) FROM tests AS t WHERE run_id=? GROUP BY state,status ORDER BY state,status DESC;"
             run-id)
	    ;; add the per run counts to res
	    (for-each (lambda (state)
			(set! res (cons (list run-name state (hash-table-ref curr state)) res)))
		      (sort (hash-table-keys curr) string>=))
	    (set! curr (make-hash-table))))))
     runs-info)
    (for-each (lambda (state)
		(set! res (cons (list "Totals" state (hash-table-ref totals state)) res)))
	      (sort (hash-table-keys totals) string>=))
    res))

;; db:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db:get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
;;  to extract info from the structure returned
;;
(define (db:get-runs-by-patt dbstruct keys runnamepatt targpatt offset limit fields last-update) ;; test-name)
  (let* ((tmp      (runs:get-std-run-fields keys (or fields '("id" "runname" "state" "status" "owner" "event_time"))))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (key-patt "")
	 (runwildtype (if (substring-index "%" runnamepatt) "like" "glob"))
	 (qry-str  #f)
	 (keyvals  (if targpatt (keys:target->keyval keys targpatt) '())))
    (for-each (lambda (keyval)
		(let* ((key    (car keyval))
		       (patt   (cadr keyval))
		       (fulkey (conc ":" key))
		       (wildtype (if (substring-index "%" patt) "like" "glob")))
		  (if patt
		      (set! key-patt (conc key-patt " AND " key " " wildtype " '" patt "'"))
		      (begin
			(debug:print-error 0 *default-log-port* "searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keyvals)
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt 
			(if last-update
			       (conc " AND last_update >= " last-update " ")
			       " ")
			" ORDER BY event_time "
			(if limit  (conc " LIMIT " limit)   "")
			(if offset (conc " OFFSET " offset) "")
			";"))
    (debug:print-info 4 *default-log-port* "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
    (vector header 
            (reverse
             (db:with-db dbstruct #f #f ;; reads db, does not write to it.
                         (lambda (db)
                           (sqlite3:fold-row
                            (lambda (res . r)
                              (cons (list->vector r) res))
                            '()
                            db
                            qry-str
                            runnamepatt)))))))

;; use (get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
(define (db:get-run-info dbstruct run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
  (let* ((res       (vector #f #f #f #f))
	 (keys      (db:get-keys dbstruct))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (debug:print-info 11 *default-log-port* "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (a . x)
	  (set! res (apply vector a x)))
	db 
	(conc "SELECT " keystr " FROM runs WHERE id=? AND state != 'deleted';")
	run-id)))
    (debug:print-info 11 *default-log-port* "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (let ((finalres (vector header res)))
      ;; (hash-table-set! *run-info-cache* run-id finalres)
      finalres)))

(define (db:set-comment-for-run dbstruct run-id comment)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment ;; (sdb:qry 'getid comment)
		      run-id))))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run dbstruct run-id)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (sqlite3:with-transaction
      db
      (lambda ()
        (sqlite3:execute db "DELETE FROM test_steps WHERE test_id IN (SELECT id FROM tests WHERE run_id=?);" run-id)
        (sqlite3:execute db "DELETE FROM test_data WHERE test_id IN (SELECT id FROM tests WHERE run_id=?);"  run-id)
        (sqlite3:execute db "UPDATE tests SET state='DELETED',comment='' WHERE run_id=?;" run-id)
        ;; (db:delay-if-busy dbdat)
        (sqlite3:execute db "UPDATE runs SET state='deleted',comment='' WHERE id=?;" run-id))))))

(define (db:update-run-event_time dbstruct run-id)
  (db:with-db
   dbstruct #f #t
   (lambda (db)
     (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id))))

(define (db:lock/unlock-run dbstruct run-id lock unlock user)
  (db:with-db
   dbstruct #f #t
   (lambda (db)
     (let ((newlockval (if lock "locked"
			   (if unlock
			       "unlocked"
			       "locked")))) ;; semi-failsafe
       (sqlite3:execute db "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
       (sqlite3:execute db "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
			user (conc newlockval " " run-id))
       (debug:print-info 1 *default-log-port* "" newlockval " run number " run-id)))))

(define (db:set-run-status dbstruct run-id status msg)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (if msg
         (sqlite3:execute db "UPDATE runs SET status=?,comment=? WHERE id=?;" status msg run-id)
         (sqlite3:execute db "UPDATE runs SET status=? WHERE id=?;" status run-id)))))

(define (db:get-run-status dbstruct run-id)
  (let ((res "n/a"))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (status)
	  (set! res status))
	db
	"SELECT status FROM runs WHERE id=?;" 
	run-id)
       res))))

;;======================================================================
;; K E Y S
;;======================================================================

;; get key val pairs for a given run-id
;; ( (FIELDNAME1 keyval1) (FIELDNAME2 keyval2) ... )
(define (db:get-key-val-pairs dbstruct run-id)
  (let* ((keys (db:get-keys dbstruct))
	 (res  '()))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (for-each 
	(lambda (key)
	  (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	    (sqlite3:for-each-row 
	     (lambda (key-val)
	       (set! res (cons (list key key-val) res)))
	     db qry run-id)))
	keys)))
       (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals dbstruct run-id)
  (let* ((keys (db:get-keys dbstruct))
	 (res  '()))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (for-each 
	(lambda (key)
	  (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	    ;; (db:delay-if-busy dbdat)
	    (sqlite3:for-each-row 
	     (lambda (key-val)
	       (set! res (cons key-val res)))
	     db qry run-id)))
	keys)))
    (let ((final-res (reverse res)))
      (hash-table-set! *keyvals* run-id final-res)
      final-res)))

;; The target is keyval1/keyval2..., cached in *target* as it is used often
(define (db:get-target dbstruct run-id)
  (let* ((keyvals (db:get-key-vals dbstruct run-id))
	 (thekey  (string-intersperse (map (lambda (x)(if x x "-na-")) keyvals) "/")))
    thekey))

;; Get run-ids for runs with same target but different runnames and NOT run-id
;;
(define (db:get-prev-run-ids dbstruct run-id)
  (let* ((keyvals (rmt:get-key-val-pairs run-id))
	 (kvalues (map cadr keyvals))
	 (keys    (rmt:get-keys))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND ")))
    (let ((prev-run-ids '()))
      (if (null? keyvals)
          '()
          (begin
            (db:with-db dbstruct #f #f ;; #f means work with the zeroth db - i.e. the runs db
                        (lambda (db)
                          (apply sqlite3:for-each-row
                                 (lambda (id)
                                   (set! prev-run-ids (cons id prev-run-ids)))
                                 db
                                 (conc "SELECT id FROM runs WHERE " qrystr " AND state != 'deleted' AND id != ?;")
                                 (append kvalues (list run-id)))))
            prev-run-ids)))))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
;; mode:
;;  'dashboard - use state = 'COMPLETED' AND status in ( statuses ) OR state in ( states )
;;
(define (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)
  (if (not (number? run-id))
      (begin ;; no need to treat this as an error by default
	(debug:print 4 *default-log-port* "WARNING: call to db:get-tests-for-run with bad run-id=" run-id)
	;; (print-call-chain (current-error-port))
	'())
      (let* ((qryvalstr       (case qryvals
				((shortlist) "id,run_id,testname,item_path,state,status")
				((#f)        db:test-record-qry-selector) ;; "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")
				(else        qryvals)))
	     (res            '())
	     ;; if states or statuses are null then assume match all when not-in is false
	     (states-qry      (if (null? states) 
				  #f
				  (conc " state "  
					(if (eq? mode 'dashboard)
					    " IN ('"
					    (if not-in
						" NOT IN ('"
						" IN ('")) 
					(string-intersperse states   "','")
					"')")))
	     (statuses-qry    (if (null? statuses)
				  #f
				  (conc " status "
					(if (eq? mode 'dashboard)
					    " IN ('"
					    (if not-in 
						" NOT IN ('"
						" IN ('") )
					(string-intersperse statuses "','")
					"')")))
	     (interim-qry       (conc " AND " (if not-in "NOT " "") "( state='COMPLETED' " (if statuses-qry (conc " AND " statuses-qry " ) ") " ) ")
				      (if states-qry
					  (conc (if not-in " AND " " OR ") states-qry ) ;; " ) ")
					  "")))
	     (states-statuses-qry 
	      (cond 
	       ((and states-qry statuses-qry)
		(case mode
		  ((dashboard) 
		   (if not-in
		       (conc " AND (state='COMPLETED' AND status NOT IN ('" (string-intersperse statuses "','") "')) "
			     " OR (state != 'COMPLETED' AND state NOT IN ('" (string-intersperse states "','") "')) ")
		       (conc " AND (state='COMPLETED' AND status IN ('" (string-intersperse statuses "','") "')) "
			     " OR (state NOT IN ('COMPLETED','DELETED') AND state IN ('" (string-intersperse states "','") "')) ")))
		  (else       (conc " AND ( " states-qry " AND " statuses-qry " ) "))))
	       (states-qry  
		(case mode
		  ((dashboard) (conc " AND " (if not-in "NOT " "") " state IN ('" (string-intersperse states    "','") "') ")) ;; interim-qry)
		  (else        (conc " AND " states-qry))))
	       (statuses-qry 
		(case mode
		  ((dashboard) (conc " AND " (if not-in "NOT " "") " status IN ('" (string-intersperse statuses "','") "') ")) ;; interim-qry)
		  (else        (conc " AND " statuses-qry))))
	       (else "")))
	     (tests-match-qry (tests:match->sqlqry testpatt))
	     (qry             (conc "SELECT " qryvalstr
				    " FROM tests WHERE run_id=? "
				    (if last-update " " " AND state != 'DELETED' ") ;; if using last-update we want deleted tests?
				    states-statuses-qry
				    (if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
				    (if last-update (conc " AND last_update >= " last-update " ") "")
				    (case sort-by
				      ((rundir)      " ORDER BY length(rundir) ")
				      ((testname)    (conc " ORDER BY testname " (if sort-order (conc sort-order ",") "") " item_path "))
				      ((statestatus) (conc " ORDER BY state " (if  sort-order (conc sort-order ",") "") " status "))
				      ((event_time)  " ORDER BY event_time ")
				      (else          (if (string? sort-by)
							 (conc " ORDER BY " sort-by " ")
							 " ")))
				    (if sort-order sort-order " ")
				    (if limit  (conc " LIMIT " limit)   " ")
				    (if offset (conc " OFFSET " offset) " ")
				    ";"
				    )))
	(debug:print-info 8 *default-log-port* "db:get-tests-for-run run-id=" run-id ", qry=" qry)
	(db:with-db dbstruct run-id #f
		    (lambda (db)
		      (sqlite3:for-each-row 
		       (lambda (a . b) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment)
			 (set! res (cons (apply vector a b) res))) ;; id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment) res)))
		       db
		       qry
		       run-id
		       )))
	(case qryvals
	  ((shortlist)(map db:test-short-record->norm res))
	  ((#f)       res)
	  (else       res)))))

(define (db:test-short-record->norm inrec)
  ;;  "id,run_id,testname,item_path,state,status"
  ;;  "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
  (vector (vector-ref inrec 0) ;; id
	  (vector-ref inrec 1) ;; run_id
	  (vector-ref inrec 2) ;; testname
	  (vector-ref inrec 4) ;; state
	  (vector-ref inrec 5) ;; status
	  -1 "" -1 -1 "" "-" 
	  (vector-ref inrec 3) ;; item-path
	  -1 "-" "-"))

(define (db:get-tests-for-run-state-status dbstruct run-id testpatt)
  (let* ((res            '())
	 (tests-match-qry (tests:match->sqlqry testpatt))
	 (qry             (conc "SELECT id,testname,item_path,state,status FROM tests WHERE run_id=? " 
				(if tests-match-qry (conc " AND (" tests-match-qry ") ") ""))))
    (debug:print-info 8 *default-log-port* "db:get-tests-for-run qry=" qry)
    (db:with-db dbstruct run-id #f
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (id testname item-path state status)
		     ;;                      id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
		     (set! res (cons (vector id run-id testname state status -1         ""     -1      -1       ""    "-"  item-path -1           "-"         "-") res)))
		   db 
		   qry
		   run-id)))
    res))

(define (db:get-testinfo-state-status dbstruct run-id test-id)
  (let ((res            #f))
    (db:with-db dbstruct run-id #f
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (run-id testname item-path state status)
		     ;; id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment
		     (set! res (vector test-id run-id testname state status -1 "" -1 -1 "" "-" item-path -1 "-" "-")))
		   db 
		   "SELECT run_id,testname,item_path,state,status FROM tests WHERE id=?;" 
		   test-id)))
    res))

;; get a useful subset of the tests data (used in dashboard
;; use db:mintest-get-{id ,run_id,testname ...}
;;
(define (db:get-tests-for-run-mindata dbstruct run-id testpatt states statuses not-in)
  (db:get-tests-for-run dbstruct run-id testpatt states statuses #f #f not-in #f #f "id,run_id,testname,state,status,event_time,item_path" 0 #f))

;; do not use.
;;
(define (db:get-tests-for-runs dbstruct run-ids testpatt states statuses #!key (not-in #f)(qryvals #f))
  ;; (db:delay-if-busy)
  (let ((res '()))
    (for-each 
     (lambda (run-id)
       (set! res (append 
		  res 
		  (db:get-tests-for-run dbstruct run-id testpatt states statuses #f #f not-in #f #f qryvals #f 'normal))))
     (if run-ids
	 run-ids
	 (db:get-all-run-ids dbstruct)))
    res))

;; Convert calling routines to get list of run-ids and loop, do not use the get-tests-for-runs
;;

(define (db:delete-test-records dbstruct run-id test-id)
  (db:general-call dbstruct 'delete-test-step-records (list test-id))
  (db:general-call dbstruct 'delete-test-data-records (list test-id))
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id))))

;; 
(define (db:delete-old-deleted-test-records dbstruct)
  (let (;; (run-ids  (db:get-all-run-ids dbstruct))
	(targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
    (db:with-db
     dbstruct
     0
     #t
     (lambda (db)
       (sqlite3:with-transaction
	db
	(lambda ()
	  (sqlite3:execute db "DELETE FROM test_steps WHERE test_id IN (SELECT id FROM tests WHERE state='DELETED' AND event_time<?);" targtime)
	  (sqlite3:execute db "DELETE FROM test_data WHERE test_id IN (SELECT id FROM tests WHERE state='DELETED' AND event_time<?);" targtime)
	  (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED' AND event_time<?;" targtime)))))))

;; set tests with state currstate and status currstatus to newstate and newstatus
;; use currstate = #f and or currstatus = #f to apply to any state or status respectively
;; WARNING: SQL injection risk. NB// See new but not yet used "faster" version below
;;
;;  AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
;;  (debug:print 0 *default-log-port* "QRY: " qry)
;;  (db:delay-if-busy)
;;
;; NB// This call only operates on toplevel tests. Consider replacing it with more general call
;;
(define (db:set-tests-state-status dbstruct run-id testnames currstate currstatus newstate newstatus)
  (for-each (lambda (testname)
	      (let ((qry (conc "UPDATE tests SET state=?,status=? WHERE "
			       (if currstate  (conc "state='" currstate "' AND ") "")
			       (if currstatus (conc "status='" currstatus "' AND ") "")
			       " run_id=? AND testname LIKE ?;"))
		    (test-id (db:get-test-id dbstruct run-id testname "")))
		(db:with-db
		 dbstruct
		 run-id
		 #t
		 (lambda (db)
		   (sqlite3:execute db qry newstate newstatus run-id testname)))
		(if test-id (mt:process-triggers dbstruct run-id test-id newstate newstatus))))
	    testnames))

;; ;; speed up for common cases with a little logic
;; ;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;; ;;
(define (db:test-set-state-status dbstruct run-id test-id newstate newstatus newcomment)
  (db:with-db
   dbstruct
   run-id
   #t
   (lambda (db)
     (cond
      ((and newstate newstatus newcomment)
       (sqlite3:execute db "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;" newstate newstatus newcomment ;; (sdb:qry 'getid newcomment)
			test-id))
      ((and newstate newstatus)
       (sqlite3:execute db "UPDATE tests SET state=?,status=? WHERE id=?;" newstate newstatus test-id))
      (else
       (if newstate   (sqlite3:execute db "UPDATE tests SET state=?   WHERE id=?;" newstate   test-id))
       (if newstatus  (sqlite3:execute db "UPDATE tests SET status=?  WHERE id=?;" newstatus  test-id))
       (if newcomment (sqlite3:execute db "UPDATE tests SET comment=? WHERE id=?;" newcomment ;; (sdb:qry 'getid newcomment)
				       test-id))))))
  (mt:process-triggers dbstruct run-id test-id newstate newstatus))

;; NEW BEHAVIOR: Count tests running in all runs!
;;
(define (db:get-count-tests-running dbstruct run-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:first-result 
      db
      ;; WARNING BUG EDIT ME - merged from v1.55 - not sure what is right here ...
      ;; AND run_id NOT IN (SELECT id FROM runs WHERE state='deleted')
      "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND NOT (uname = 'n/a' AND item_path = '');"
      ;; "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=?;" 
      ))))

;; NEW BEHAVIOR: Count tests running in only one run!
;;
(define (db:get-count-tests-actually-running dbstruct run-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:first-result
      db
      ;; WARNING BUG EDIT ME - merged from v1.55 - not sure what is right here ...
      ;; "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id NOT IN (SELECT id FROM runs WHERE state='deleted') AND NOT (uname = 'n/a' AND item_path = '');")
      "SELECT count(id) FROM tests WHERE state in ('RUNNING','REMOTEHOSTSTART') AND run_id=?;" 
      run-id)))) ;; NOT IN (SELECT id FROM runs WHERE state='deleted');")

;; NEW BEHAVIOR: Look only at single run with run-id
;; 
;; (define (db:get-running-stats dbstruct run-id)
(define (db:get-count-tests-running-for-run-id dbstruct run-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:first-result
      db
      "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=? AND NOT (uname = 'n/a' AND item_path = '');" run-id))))

;; For a given testname how many items are running? Used to determine
;; probability for regenerating html
;; 
(define (db:get-count-tests-running-for-testname dbstruct run-id testname)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:first-result
      db
      "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND run_id=? AND NOT (uname = 'n/a' AND item_path = '') AND testname=?;" run-id testname))))

(define (db:get-count-tests-running-in-jobgroup dbstruct run-id jobgroup)
  (if (not jobgroup)
      0 ;; 
      (let ((testnames '()))
	;; get the testnames
	(db:with-db
	 dbstruct #f #f
	 (lambda (db)
	   (sqlite3:for-each-row
	    (lambda (testname)
	      (set! testnames (cons testname testnames)))
	    db
	    "SELECT testname FROM test_meta WHERE jobgroup=?"
	    jobgroup)))
	;; get the jobcount NB// EXTEND THIS TO OPPERATE OVER ALL RUNS?
	(if (not (null? testnames))
	    (db:with-db
	     dbstruct
	     run-id
	     #f
	     (lambda (db)
	       (sqlite3:first-result
		db
		(conc "SELECT count(id) FROM tests WHERE state in ('RUNNING','LAUNCHED','REMOTEHOSTSTART') AND testname in ('"
		      (string-intersperse testnames "','")
		      "') AND NOT (uname = 'n/a' AND item_path='');")) ;; should this include the (uname = 'n/a' ...) ???
	       ))
	    0))))

;; tags: '("tag%" "tag2" "%ag6")
;;

;; done with run when:
;;   0 tests in LAUNCHED, NOT_STARTED, REMOTEHOSTSTART, RUNNING
(define (db:estimated-tests-remaining dbstruct run-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:first-result
      db
      "SELECT count(id) FROM tests WHERE state in ('LAUNCHED','NOT_STARTED','REMOTEHOSTSTART','RUNNING','KILLREQ') AND run_id=?;")
     run-id)))

;; map run-id, testname item-path to test-id
(define (db:get-test-id dbstruct run-id testname item-path)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (db:first-result-default
      db
      "SELECT id FROM tests WHERE testname=? AND item_path=? AND run_id=?;"
      #f ;; the default
      testname item-path run-id))))

;; overload the unused attemptnum field for the process id of the runscript or 
;; ezsteps step script in progress
;;
(define (db:test-set-top-process-pid dbstruct run-id test-id pid)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (sqlite3:execute db "UPDATE tests SET attemptnum=? WHERE id=?;"
		      pid test-id))))

(define (db:test-get-top-process-pid dbstruct run-id test-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (db:first-result-default 
      db
      "SELECT attemptnum FROM tests WHERE id=?;"
      #f
      test-id))))

(define db:test-record-fields '("id"           "run_id"        "testname"  "state"      "status"      "event_time"
				"host"         "cpuload"       "diskfree"  "uname"      "rundir"      "item_path"
                                "run_duration" "final_logf"    "comment"   "shortdir"   "attemptnum"  "archived"))

;; fields *must* be a non-empty list
;;
(define (db:field->number fieldname fields)
  (if (null? fields)
      #f
      (let loop ((hed  (car fields))
		 (tal  (cdr fields))
		 (indx 0))
	(if (equal? fieldname hed)
	    indx
	    (if (null? tal)
		#f
		(loop (car tal)(cdr tal)(+ indx 1)))))))

(define db:test-record-qry-selector (string-intersperse db:test-record-fields ","))


;; NOTE: Use db:test-get* to access records
;; NOTE: This needs rundir decoding? Decide, decode here or where used? For the moment decode where used.
(define (db:get-all-tests-info-by-run-id dbstruct run-id)
  (let* ((res '()))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir attemptnum archived)
	  ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14     15        16
	  (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir attemptnum archived)
			  res)))
	db
	(conc "SELECT " db:test-record-qry-selector " FROM tests WHERE state != 'DELETED' AND run_id=?;")
	run-id)))
    res))

(define (db:replace-test-records dbstruct run-id testrecs)
  (db:with-db dbstruct run-id #t 
	      (lambda (db)
		(let* ((qmarks (string-intersperse (make-list (length db:test-record-fields) "?") ","))
		       (qrystr (conc "INSERT OR REPLACE INTO tests (" db:test-record-qry-selector ") VALUES (" qmarks ") WHERE run_id=?;"))
		       (qry    (sqlite3:prepare db qrystr)))
		  (debug:print 0 *default-log-port* "INFO: migrating test records for run with id " run-id)
		  (sqlite3:with-transaction
		   db
		   (lambda ()
		     (for-each 
		      (lambda (rec)
			;; (debug:print 0 *default-log-port* "INFO: Inserting values: " (string-intersperse (map conc (vector->list rec)) ",") "\n")
			(apply sqlite3:execute qry (append (vector->list rec)(list run-id))))
		      testrecs)))
		  (sqlite3:finalize! qry)))))

;; map a test-id into the proper range
;;
(define (db:adj-test-id mtdb min-test-id test-id)
  (if (>= test-id min-test-id)
      test-id
      (let loop ((new-id min-test-id))
	(let ((test-id-found #f))
	  (sqlite3:for-each-row 
	   (lambda (id)
	     (set! test-id-found id))
	   (db:dbdat-get-db mtdb)
	   "SELECT id FROM tests WHERE id=?;"
	   new-id)
	  ;; if test-id-found then need to try again
	  (if test-id-found
	      (loop (+ new-id 1))
	      (begin
		(debug:print-info 0 *default-log-port* "New test id " new-id " selected for test with id " test-id)
		(sqlite3:execute mtdb "UPDATE tests SET id=? WHERE id=?;" new-id test-id)))))))

;; move test ids into the 30k * run_id range
;;
(define (db:prep-megatest.db-adj-test-ids mtdb run-id testrecs)
  (debug:print-info 0 *default-log-port* "Adjusting test ids in megatest.db for run " run-id)
  (let ((min-test-id (* run-id 30000)))
    (for-each 
     (lambda (testrec)
       (let* ((test-id (vector-ref testrec (db:field->number "id" db:test-record-fields))))
	 (db:adj-test-id (db:dbdat-get-db mtdb) min-test-id test-id)))
     testrecs)))
	
;; 1. move test ids into the 30k * run_id range
;; 2. move step ids into the 30k * run_id range
;;
(define (db:prep-megatest.db-for-migration mtdb)
  (let* ((run-ids (db:get-all-run-ids mtdb)))
    (for-each 
     (lambda (run-id)
       (let ((testrecs (db:get-all-tests-info-by-run-id mtdb run-id)))
	 (db:prep-megatest.db-adj-test-ids (db:dbdat-get-db mtdb) run-id testrecs)))
     run-ids)))

;; Get test data using test_id, run-id is not used
;; 
(define (db:get-test-info-by-id dbstruct run-id test-id)
  (db:with-db
   dbstruct
   #f ;; run-id
   #f
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row ;; attemptnum added to hold pid of top process (not Megatest) controlling a test
	(lambda (id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id attemptnum archived)
	  ;;             0    1       2      3      4        5       6      7        8     9     10      11          12          13           14         15          16
	  (set! res (vector id run-id testname state status event-time host cpuload diskfree uname rundir-id item-path run_duration final-logf-id comment short-dir-id attemptnum archived)))
	db
	(conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id=?;")
	test-id)
       res))))

;; Use db:test-get* to access
;; Get test data using test_ids. NB// Only works within a single run!!
;;
(define (db:get-test-info-by-ids dbstruct run-id test-ids)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let ((res '()))
       (sqlite3:for-each-row
	(lambda (a . b)
	  ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14
	  (set! res (cons (apply vector a b) res)))
	db
	(conc "SELECT " db:test-record-qry-selector " FROM tests WHERE id in ("
	      (string-intersperse (map conc test-ids) ",") ");"))
       res))))

(define (db:get-test-info dbstruct run-id test-name item-path)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row
	(lambda (a . b)
	  (set! res (apply vector a b)))
	db
	(conc "SELECT " db:test-record-qry-selector " FROM tests WHERE testname=? AND item_path=? AND run_id=?;")
	test-name item-path run-id)
       res))))

(define (db:test-get-rundir-from-test-id dbstruct run-id test-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (db:first-result-default
      db
      "SELECT rundir FROM tests WHERE id=?;"
      #f ;; default result
      test-id))))

;;======================================================================
;; S T E P S
;;======================================================================

(define (db:teststep-set-status! dbstruct run-id test-id teststep-name state-in status-in comment logfile)
  (db:with-db
   dbstruct
   run-id
   #t
   (lambda (db)
     (sqlite3:execute 
      db
      "INSERT OR REPLACE into test_steps (test_id,stepname,state,status,event_time,comment,logfile) VALUES(?,?,?,?,?,?,?);"
      test-id teststep-name state-in status-in (current-seconds)
      (if comment comment "")
      (if logfile logfile "")))))
   
;; db-get-test-steps-for-run
(define (db:get-steps-for-test dbstruct run-id test-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let* ((res '()))
       (sqlite3:for-each-row 
	(lambda (id test-id stepname state status event-time logfile comment)
	  (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "") comment) res)))
	db
	"SELECT id,test_id,stepname,state,status,event_time,logfile,comment FROM test_steps WHERE status != 'DELETED' AND test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
	test-id)
       (reverse res)))))

(define (db:get-steps-data dbstruct run-id test-id)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let ((res '()))
       (sqlite3:for-each-row 
	(lambda (id test-id stepname state status event-time logfile)
	  (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
	db
	"SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE status != 'DELETED' AND test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
	test-id)
       (reverse res)))))

;;======================================================================
;; T E S T  D A T A 
;;======================================================================

;; WARNING: Do NOT call this for the parent test on an iterated test
;; Roll up test_data pass/fail results
;; look at the test_data status field, 
;;    if all are pass (any case) and the test status is PASS or NULL or '' then set test status to PASS.
;;    if one or more are fail (any case) then set test status to PASS, non "pass" or "fail" are ignored
(define (db:test-data-rollup dbstruct run-id test-id status)
  (let* ((fail-count 0)
	 (pass-count 0))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (fcount pcount)
	  (set! fail-count fcount)
	  (set! pass-count pcount))
	db 
	"SELECT (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail') AS fail_count,
             (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass') AS pass_count;"
	test-id test-id)
       ;; Now rollup the counts to the central megatest.db
       (db:general-call dbstruct 'pass-fail-counts (list pass-count fail-count test-id))
       ;; if the test is not FAIL then set status based on the fail and pass counts.
       (db:general-call dbstruct 'test_data-pf-rollup (list test-id test-id test-id test-id))))))

;; each section is a rule except "final" which is the final result
;;
;; [rule-5]
;; operator in
;; section LogFileBody
;; desc Output voltage
;; status OK
;; expected 1.9
;; measured 1.8
;; type +/-
;; tolerance 0.1
;; pass 1
;; fail 0
;; 
;; [final]
;; exit-code 6
;; exit-status SKIP
;; message If flagged we are asking for this to exit with code 6
;;
;; recorded in steps table:
;;   category: stepname
;;   variable: rule-N
;;   value:    measured
;;   expected: expected
;;   tol:      tolerance
;;   units:    -
;;   comment:  desc or message
;;   status:   status
;;   type:     type
;; 
(define (db:logpro-dat->csv dat stepname)
  (let ((res '()))
    (for-each
     (lambda (entry-name)
       (if (equal? entry-name "final")
	   (set! res (append 
		      res
		      (list
		       (list stepname
			     entry-name
			     (configf:lookup dat entry-name "exit-code")    ;; 0 ;; Value
			     0                                              ;; 1 ;; Expected
			     0                                              ;; 2 ;; Tolerance
			     "n/a"					    ;; 3 ;; Units
			     (configf:lookup dat entry-name "message")      ;; 4 ;; Comment
			     (configf:lookup dat entry-name "exit-status")  ;; 5 ;; Status
			     "logpro"                                       ;; 6 ;; Type
			     ))))
	   (let* ((value     (or (configf:lookup dat entry-name "measured")  "n/a"))
		  (expected  (or (configf:lookup dat entry-name "expected")  "n/a"))
		  (tolerance (or (configf:lookup dat entry-name "tolerance") "n/a"))
		  (comment   (or (configf:lookup dat entry-name "comment")
				 (configf:lookup dat entry-name "desc")      "n/a"))
		  (status    (or (configf:lookup dat entry-name "status")    "n/a"))
		  (type      (or (configf:lookup dat entry-name "expected")  "n/a")))
	     (set! res (append
			res  
			(list (list stepname
				    entry-name 
				    value        ;; 0
				    expected     ;; 1
				    tolerance    ;; 2
				    "n/a"        ;; 3 Units
				    comment      ;; 4
				    status       ;; 5
				    type         ;; 6
				    )))))))
     (hash-table-keys dat))
    res))

;; $MT_MEGATEST -load-test-data << EOF
;; foo,bar,   1.2,  1.9, >
;; foo,rab, 1.0e9, 10e9, 1e9
;; foo,bla,   1.2,  1.9, <
;; foo,bal,   1.2,  1.2, <   ,     ,Check for overload
;; foo,alb,   1.2,  1.2, <=  , Amps,This is the high power circuit test
;; foo,abl,   1.2,  1.3, 0.1
;; foo,bra,   1.2, pass, silly stuff
;; faz,bar,    10,  8mA,     ,     ,"this is a comment"
;; EOF

(define (db:csv->test-data dbstruct run-id test-id csvdata)
  (debug:print 4 *default-log-port* "test-id " test-id ", csvdata: " csvdata)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (let* ((csvlist (csv->list (make-csv-reader
				 (open-input-string csvdata)
				 '((strip-leading-whitespace? #t)
				   (strip-trailing-whitespace? #t)))))) ;; (csv->list csvdata)))
       (for-each
	(lambda (csvrow)
	  (let* ((padded-row  (take (append csvrow (list #f #f #f #f #f #f #f #f #f)) 9))
		 (category    (list-ref padded-row 0))
		 (variable    (list-ref padded-row 1))
		 (value       (any->number-if-possible (list-ref padded-row 2)))
		 (expected    (any->number-if-possible (list-ref padded-row 3)))
		 (tol         (any->number-if-possible (list-ref padded-row 4))) ;; >, <, >=, <=, or a number
		 (units       (list-ref padded-row 5))
		 (comment     (list-ref padded-row 6))
		 (status      (let ((s (list-ref padded-row 7)))
				(if (and (string? s)(or (string-match (regexp "^\\s*$") s)
							(string-match (regexp "^n/a$") s)))
				    #f
				    s))) ;; if specified on the input then use, else calculate
		 (type        (list-ref padded-row 8)))
	    ;; look up expected,tol,units from previous best fit test if they are all either #f or ''
	    (debug:print 4 *default-log-port* "BEFORE: category: " category " variable: " variable " value: " value 
			 ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment " type: " type)
	    
	    (if (and (or (not expected)(equal? expected ""))
		     (or (not tol)     (equal? expected ""))
		     (or (not units)   (equal? expected "")))
		(let-values (((new-expected new-tol new-units)(tdb:get-prev-tol-for-test #f test-id category variable)))
		  (set! expected new-expected)
		  (set! tol      new-tol)
		  (set! units    new-units)))
	    
	    (debug:print 4 *default-log-port* "AFTER:  category: " category " variable: " variable " value: " value 
			 ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	    ;; calculate status if NOT specified
	    (if (and (not status)(number? expected)(number? value)) ;; need expected and value to be numbers
		(if (number? tol) ;; if tol is a number then we do the standard comparison
		    (let* ((max-val (+ expected tol))
			   (min-val (- expected tol))
			   (result  (and (>=  value min-val)(<= value max-val))))
		      (debug:print 4 *default-log-port* "max-val: " max-val " min-val: " min-val " result: " result)
		      (set! status (if result "pass" "fail")))
		    (set! status ;; NB// need to assess each one (i.e. not return operator since need to act if not valid op.
		      (case (string->symbol tol) ;; tol should be >, <, >=, <=
			((>)  (if (>  value expected) "pass" "fail"))
			((<)  (if (<  value expected) "pass" "fail"))
			((>=) (if (>= value expected) "pass" "fail"))
			((<=) (if (<= value expected) "pass" "fail"))
			(else (conc "ERROR: bad tol comparator " tol))))))
	    (debug:print 4 *default-log-port* "AFTER2: category: " category " variable: " variable " value: " value 
			 ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	    ;; (db:delay-if-busy dbdat)
	    (sqlite3:execute db "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status,type) VALUES (?,?,?,?,?,?,?,?,?,?);"
			     test-id category variable value expected tol units (if comment comment "") status type)))
	csvlist)))))

;; This routine moved from tdb.scm, tdb:read-test-data
;;
(define (db:read-test-data dbstruct run-id test-id categorypatt)
  (let* ((res '()))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (id test_id category variable value expected tol units comment status type)
	  (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
	db
	"SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
       (reverse res)))))

;;======================================================================
;; Misc. test related queries
;;======================================================================

(define (db:get-run-ids-matching-target dbstruct keynames target res runname testpatt statepatt statuspatt)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (let* ((row-ids '())
	    (keystr (string-intersperse 
		     (map (lambda (key val)
			    (conc key " like '" val "'"))
			  keynames 
			  (string-split target "/"))
		     " AND "))
	    ;; (testqry (tests:match->sqlqry testpatt))
	    (runsqry (sqlite3:prepare db (conc "SELECT id FROM runs WHERE " keystr " AND runname LIKE '" runname "';"))))
       ;; (debug:print 8 *default-log-port* "db:test-get-paths-matching-keynames-target-new\n  runsqry=" runsqry "\n  tstsqry=" testqry)
       (sqlite3:for-each-row
	(lambda (rid)
	  (set! row-ids (cons rid row-ids)))
	runsqry)
       (sqlite3:finalize! runsqry)
       row-ids))))

;; finds latest matching all patts for given run-id
;;
(define (db:test-get-paths-matching-keynames-target-new dbstruct run-id keynames target res testpatt statepatt statuspatt runname)
  (let* ((testqry (tests:match->sqlqry testpatt))
	 (tstsqry (conc "SELECT rundir FROM tests WHERE run_id=? AND " testqry " AND state LIKE '" statepatt "' AND status LIKE '" statuspatt "' ORDER BY event_time ASC;")))
    (db:with-db
     dbstruct
     run-id
     #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (p)
	  (set! res (cons p res)))
	db
	tstsqry
	run-id)
       res))))

(define (db:test-toplevel-num-items dbstruct run-id testname)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let ((res 0))
       (sqlite3:for-each-row
	(lambda (num-items)
	  (set! res num-items))
	db
	"SELECT count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND state NOT IN ('DELETED');"
	run-id
	testname)
       res))))

;;======================================================================
;; QUEUE UP META, TEST STATUS AND STEPS REMOTE ACCESS
;;======================================================================

;; NOTE: Can remove the regex and base64 encoding for zmq
(define (db:obj->string obj #!key (transport 'http))
  (case transport
    ;; ((fs) obj)
    ((http fs)
     (string-substitute
      (regexp "=") "_"
      (base64:base64-encode 
       (z3:encode-buffer
	(with-output-to-string
	  (lambda ()(serialize obj))))) ;; BB: serialize - this is what causes problems between different builds of megatest communicating.  serialize is sensitive to binary image of mtest.
      #t))
    ((zmq nmsg)(with-output-to-string (lambda ()(serialize obj))))
    (else obj))) ;; rpc

(define (db:string->obj msg #!key (transport 'http))
  (case transport
    ;; ((fs) msg)
    ((http fs)
     (if (string? msg)
	 (with-input-from-string 
	     (z3:decode-buffer
	      (base64:base64-decode
	       (string-substitute 
		(regexp "_") "=" msg #t)))
	   (lambda ()(deserialize)))
	 (begin
	   (debug:print-error 0 *default-log-port* "reception failed. Received \"" msg "\" but cannot translate it.")
           (print-call-chain (current-error-port))
	   msg))) ;; crude reply for when things go awry
    ((zmq nmsg)(with-input-from-string msg (lambda ()(deserialize))))
    (else msg))) ;; rpc

;; ; This is to be the big daddy call NOPE: Replaced by db:set-state-status-and-roll-up-items
;; ;
;; define (db:test-set-state-status dbstruct run-id test-id state status msg)
;;  (let ((dbdat  (db:get-db dbstruct run-id)))
;;    (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
;; 	(db:general-call dbdat 'set-test-start-time (list test-id)))
;;    ;; (if msg
;;    ;; 	(db:general-call dbdat 'state-status-msg (list state status msg test-id))
;;    ;; 	(db:general-call dbdat 'state-status     (list state status test-id)))
;;    (db:set-state-status-and-roll-up-items dbstruct run-id test-id #f state status msg)
;;    ;; process the test_data table
;;    (if (and test-id state status (equal? status "AUTO")) 
;; 	(db:test-data-rollup dbstruct run-id test-id status))
;;    (mt:process-triggers dbstruct run-id test-id state status)))

;; state is the priority rollup of all states
;; status is the priority rollup of all completed statesfu
;;
;; if test-name is an integer work off that instead of test-name test-path
;;
(define (db:set-state-status-and-roll-up-items dbstruct run-id test-name item-path state status comment)
  ;; establish info on incoming test followed by info on top level test
  (let* ((testdat      (if (number? test-name)
			   (db:get-test-info-by-id dbstruct run-id test-name) ;; test-name is actually a test-id
			   (db:get-test-info       dbstruct run-id test-name item-path)))
	 (test-id      (db:test-get-id testdat))
	 (test-name    (if (number? test-name)
			   (db:test-get-testname testdat)
			   test-name))
	 (item-path    (db:test-get-item-path testdat))
         (tl-testdat   (db:get-test-info dbstruct run-id test-name ""))
         (tl-test-id   (db:test-get-id tl-testdat)))
    (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
	(db:general-call dbstruct 'set-test-start-time (list test-id)))
    (mutex-lock! *db-transaction-mutex*)
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (let ((tr-res
              (sqlite3:with-transaction
               db
               (lambda ()
                 ;; NB// Pass the db so it is part fo the transaction
                 (db:test-set-state-status db run-id test-id state status comment)
                 (if (not (equal? item-path "")) ;; only roll up IF incoming test is an item
                     (let* ((state-status-counts  (db:get-all-state-status-counts-for-test dbstruct run-id test-name item-path)) ;; item-path is used to exclude current state/status of THIS test
                            (running              (length (filter (lambda (x)
                                                                    (member (dbr:counts-state x) *common:running-states*))
                                                                  state-status-counts)))
                            (bad-not-started      (length (filter (lambda (x)
                                                                    (and (equal? (dbr:counts-state x) "NOT_STARTED")
                                                                         (not (member (dbr:counts-status x)
                                                                                      *common:not-started-ok-statuses*))))
								  state-status-counts)))
                            (all-curr-states   (common:special-sort  ;; worst -> best (sort of)
                                                (delete-duplicates
                                                 (cons state (map dbr:counts-state state-status-counts)))
                                                *common:std-states* >))
                            (all-curr-statuses (common:special-sort  ;; worst -> best
                                                (delete-duplicates
                                                 (cons status (map dbr:counts-status state-status-counts)))
                                                *common:std-statuses* >))
                            (newstate          (if (> running 0)
                                                   "RUNNING"
                                                   (if (> bad-not-started 0)
                                                       "COMPLETED"
                                                       (car all-curr-states))))
                            (newstatus         (if (> bad-not-started 0)
                                                   "CHECK"
                                                   (car all-curr-statuses))))
                       ;; NB// Pass the db so it is part of the transaction
                       (db:test-set-state-status db run-id tl-test-id newstate newstatus #f)))))))
         (mutex-unlock! *db-transaction-mutex*)
         (if (and test-id state status (equal? status "AUTO")) 
             (db:test-data-rollup dbstruct run-id test-id status))
         tr-res)))))

(define (db:get-all-state-status-counts-for-test dbstruct run-id test-name item-path)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (sqlite3:map-row
      (lambda (state status count)
	(make-dbr:counts state: state status: status count: count))
      db
      "SELECT state,status,count(id) FROM tests WHERE run_id=? AND testname=? AND item_path != '' AND item_path !=? GROUP BY state,status;"
      run-id test-name item-path))))

;; (define (db:get-all-item-states db run-id test-name)
;;   (sqlite3:map-row 
;;    (lambda (a) a)
;;    db
;;    "SELECT DISTINCT state FROM tests WHERE item_path != '' AND state != 'DELETED' AND run_id=? AND testname=?"
;;    run-id test-name))
;; 
;; (define (db:get-all-item-statuses db run-id test-name)
;;   (sqlite3:map-row 
;;    (lambda (a) a)
;;    db
;;    "SELECT DISTINCT status FROM tests WHERE item_path != '' AND state != 'DELETED' AND state='COMPLETED' AND run_id=? AND testname=?"
;;    run-id test-name))

(define (db:test-get-logfile-info dbstruct run-id test-name)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (let ((res #f))
       (sqlite3:for-each-row 
	(lambda (path final_logf)
	  ;; (let ((path       (sdb:qry 'getstr path-id))
	  ;;       (final_logf (sdb:qry 'getstr final_logf-id)))
	  (set! logf final_logf)
	  (set! res (list path final_logf))
	  (if (directory? path)
	      (debug:print 2 *default-log-port* "Found path: " path)
	      (debug:print 2 *default-log-port* "No such path: " path))) ;; )
	db
	"SELECT rundir,final_logf FROM tests WHERE testname=? AND item_path='' AND run_id=?;"
	test-name run-id)
       res))))

;;======================================================================
;; A G R E G A T E D   T R A N S A C T I O N   D B   W R I T E S 
;;======================================================================

(define db:queries 
  (list '(update-run-duration     "UPDATE tests SET run_duration=? WHERE id=?;")

	;; TESTS
	'(register-test          "INSERT OR IGNORE INTO tests (run_id,testname,event_time,item_path,state,status) VALUES (?,?,strftime('%s','now'),?,'NOT_STARTED','n/a');")
	;; Test state and status
	'(set-test-state         "UPDATE tests SET state=?   WHERE id=?;")
	'(set-test-status        "UPDATE tests SET state=?   WHERE id=?;")
	'(state-status           "UPDATE tests SET state=?,status=? WHERE id=?;") ;; DONE
	'(state-status-msg       "UPDATE tests SET state=?,status=?,comment=? WHERE id=?;") ;; DONE
	;; Test comment
	'(set-test-comment       "UPDATE tests SET comment=? WHERE id=?;")
	'(set-test-start-time    "UPDATE tests SET event_time=strftime('%s','now') WHERE id=?;") ;; DONE
	'(pass-fail-counts       "UPDATE tests SET pass_count=?,fail_count=? WHERE id=?;")
	;; test_data-pf-rollup is used to set a tests PASS/FAIL based on the pass/fail info from the steps
	'(test_data-pf-rollup    "UPDATE tests
                                    SET status=CASE WHEN (SELECT fail_count FROM tests WHERE id=?) > 0 
                                      THEN 'FAIL'
                                    WHEN (SELECT pass_count FROM tests WHERE id=?) > 0 AND 
                                      (SELECT status FROM tests WHERE id=?) NOT IN ('WARN','FAIL')
                                    THEN 'PASS'
                                    ELSE status
                                    END WHERE id=?;") ;; DONE
	'(test-set-log            "UPDATE tests SET final_logf=? WHERE id=?;")      ;; DONE
	;; '(test-set-rundir-by-test-id "UPDATE tests SET rundir=? WHERE id=?")        ;; DONE
	;; '(test-set-rundir         "UPDATE tests SET rundir=? AND testname=? AND item_path=?;") ;; DONE
	'(test-set-rundir-shortdir "UPDATE tests SET rundir=?,shortdir=? WHERE testname=? AND item_path=? AND run_id=?;")    ;; BROKEN!!! NEEDS run-id
	'(delete-tests-in-state   ;; "DELETE FROM tests WHERE state=?;")                  ;; DONE
	  "UPDATE tests SET state='DELETED' WHERE state=?")
	'(tests:test-set-toplog   "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';")
	'(update-cpuload-diskfree "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;") ;; DONE
	'(update-uname-host       "UPDATE tests SET uname=?,host=? WHERE id=?;")       ;; DONE
        '(update-test-rundat      "INSERT INTO test_rundat (test_id,update_time,cpuload,diskfree,diskusage,run_duration) VALUES (?,?,?,?,?,?);")
	'(update-test-state       "UPDATE tests SET state=? WHERE state=? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	'(update-test-status      "UPDATE tests SET status=? WHERE status like ? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	;; stuff for set-state-status-and-roll-up-items
	'(update-pass-fail-counts "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('FAIL','CHECK','INCOMPLETE','ABORT')),
                 pass_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('PASS','WARN','WAIVED'))
             WHERE testname=? AND item_path='' AND run_id=?;") ;; DONE  ;; BROKEN!!! NEEDS run-id
	'(top-test-set-running  "UPDATE tests SET state='RUNNING' WHERE testname=? AND item_path='' AND run_id=?;") ;; DONE   ;; BROKEN!!! NEEDS run-id


	;; Might be the following top-test-set-per-pf-counts query could be better based off of something like this:
	;;
	;; select state,status,count(state) from tests where run_id=59 AND testname='runfirst' group by state,status;
	;;
	'(top-test-set-per-pf-counts "UPDATE tests
                       SET state=CASE 
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE testname=?
                                                     AND item_path != '' 
                                                     AND status NOT IN ('n/a')
                                                     AND state in ('NOT_STARTED')) > 0 THEN 'UNKNOWN'
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE testname=?
                                                     AND item_path != '' 
                                                     AND (status NOT IN ('TEN_STRIKES','BLOCKED') OR status IN ('INCOMPLETE'))
                                                     AND state in ('RUNNING','NOT_STARTED','LAUNCHED','REMOTEHOSTSTART')) > 0 THEN 'RUNNING'
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE testname=?
                                                     AND item_path != '' 
                                                     AND state NOT IN ('COMPLETED','DELETED')) = 0 THEN 'COMPLETED'
                                   WHEN (SELECT count(id) FROM tests 
                                                WHERE testname=?
                                                     AND item_path != '' 
                                                     AND state = 'NOT_STARTED') > 0 THEN 'NOT_STARTED'
                                   ELSE 'UNKNOWN' END,
                            status=CASE 
                                  WHEN fail_count > 0 THEN 'FAIL' 
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state IN ('BLOCKED','INCOMPLETE')) > 0 THEN 'FAIL'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status IN ('INCOMPLETE','ABORT')) > 0 THEN 'ABORT'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status = 'AUTO') > 0 THEN 'AUTO'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status IN ('STUCK/INCOMPLETE', 'INCOMPLETE')) > 0 THEN 'INCOMPLETE'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state IN ('COMPLETED','STUCK/INCOMPLETE','INCOMPLETE')
                                              AND status = 'FAIL') > 0 THEN 'FAIL'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status = 'CHECK') > 0 THEN 'CHECK'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status = 'SKIP') > 0 THEN 'SKIP'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status = 'WARN') > 0 THEN 'WARN'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status = 'WAIVED') > 0 THEN 'WAIVED'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=? 
                                              AND item_path != ''
                                              AND state NOT IN ('DELETED')
                                              AND status NOT IN ('PASS','FAIL','WARN','WAIVED')) > 0 THEN 'ABORT'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state='NOT_STARTED') > 0 THEN 'n/a'
                                  WHEN (SELECT count(id) FROM tests
                                         WHERE testname=?
                                              AND item_path != ''
                                              AND state = 'COMPLETED'
                                              AND status = 'PASS') > 0 THEN 'PASS'
                                  WHEN pass_count > 0 AND fail_count=0 THEN 'PASS' 
                                  ELSE 'UNKNOWN' END
                       WHERE testname=? AND item_path='';") ;; DONE  ;; BROKEN!!! NEEDS run-id

	;; STEPS
	'(delete-test-step-records "UPDATE test_steps SET status='DELETED' WHERE test_id=?;")
	'(delete-test-data-records "UPDATE test_data  SET status='DELETED' WHERE test_id=?;") ;; using status since no state field
	))

(define (db:lookup-query qry-name)
  (let ((q (alist-ref qry-name db:queries)))
    (if q (car q) #f)))

;; do not run these as part of the transaction
(define db:special-queries   '(rollup-tests-pass-fail
			       ;; db:set-state-status-and-roll-up-items  ;; WHY NOT!?
			       login
			       immediate
			       flush
			       sync
			       set-verbosity
			       killserver
			       ))

(define (db:login dbstruct calling-path calling-version client-signature)
  (cond 
   ((not (equal? calling-path *toppath*))
    (list #f "Login failed due to mismatch paths: " calling-path ", " *toppath*))
   ;; ((not (equal? *run-id* run-id))
   ;;  (list #f "Login failed due to mismatch run-id: " run-id ", " *run-id*))
   ((not (equal? megatest-version calling-version))
    (list #f "Login failed due to mismatch megatest version: " calling-version ", " megatest-version))
   (else
    (hash-table-set! *logged-in-clients* client-signature (current-seconds))
    '(#t "successful login"))))

(define (db:general-call dbstruct stmtname params)
  (let ((query (let ((q (alist-ref (if (string? stmtname)
				       (string->symbol stmtname)
				       stmtname)
				   db:queries)))
 		 (if q (car q) #f))))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (apply sqlite3:execute db query params)
       #t))))

;; get a summary of state and status counts to calculate a rollup
;;
(define (db:get-state-status-summary dbstruct run-id testname)
  (let ((res   '()))
    (db:with-db
     dbstruct #f #f
     (sqlite3:for-each-row
      (lambda (state status count)
	(set! res (cons (vector state status count) res)))
      db
      "SELECT state,status,count(state) FROM tests WHERE run_id=? AND testname=? AND item_path='' GROUP BY state,status;"
      run-id testname)
     res)))

(define (db:get-latest-host-load dbstruct raw-hostname)
  (let* ((hostname (string-substitute "\\..*$" "" raw-hostname))
         (res  (cons -1 0)))
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row
        (lambda (cpuload update-time)  (set! res (cons cpuload update-time)))
        db
        "SELECT tr.cpuload, tr.update_time FROM test_rundat tr, tests t WHERE t.host=? AND tr.cpuload != -1  AND tr.test_id=t.id ORDER BY tr.update_time DESC LIMIT 1;"
        hostname))) res ))

(define (db:set-top-level-from-items dbstruct run-id testname)
  (let* ((summ  (db:get-state-status-summary dbstruct run-id testname))
	 (find  (lambda (state status)
		  (if (null? summ) 
		      #f
		      (let loop ((hed (car summ))
				 (tal (cdr summ)))
			(if (and (string-match state  (vector-ref hed 0))
				 (string-match status (vector-ref hed 1)))
			    hed
			    (if (null? tal)
				#f
				(loop (car tal)(cdr tal)))))))))


      ;;;     E D I T M E ! !


    (cond
     ((> (find "COMPLETED" ".*") 0) #f))))
		   
    

;; get the previous records for when these tests were run where all keys match but runname
;; NB// Merge this with test:get-previous-test-run-records? This one looks for all matching tests
;; can use wildcards. Also can likely be factored in with get test paths?
;;
;; Run this remotely!!
;;
(define (db:get-matching-previous-test-run-records dbstruct run-id test-name item-path)
  (let* ((keys    (db:get-keys dbstruct))
	 (selstr  (string-intersperse keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND "))
	 (keyvals #f)
	 (tests-hash (make-hash-table)))
    ;; first look up the key values from the run selected by run-id
    (db:with-db
     dbstruct #f #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (a . b)
	  (set! keyvals (cons a b)))
	db
	(conc "SELECT " selstr " FROM runs WHERE id=? ORDER BY event_time DESC;") run-id)))
    (if (not keyvals)
	'()
	(let ((prev-run-ids '()))
	  (db:with-db
	   dbstruct #f #f
	   (lambda (db)
	     (apply sqlite3:for-each-row
		    (lambda (id)
		      (set! prev-run-ids (cons id prev-run-ids)))
		    db
		    (conc "SELECT id FROM runs WHERE " qrystr " AND id != ?;") (append keyvals (list run-id)))))
	  ;; collect all matching tests for the runs then
	  ;; extract the most recent test and return that.
	  (debug:print 4 *default-log-port* "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals 
		       ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) '()  ;; no previous runs? return null
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (db:get-tests-for-run dbstruct hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f #f 'normal)))
		  (debug:print 4 *default-log-port* "Got tests for run-id " run-id ", test-name " test-name 
			       ", item-path " item-path " results: " (intersperse results "\n"))
		  ;; Keep only the youngest of any test/item combination
		  (for-each 
		   (lambda (testdat)
		     (let* ((full-testname (conc (db:test-get-testname testdat) "/" (db:test-get-item-path testdat)))
			    (stored-test   (hash-table-ref/default tests-hash full-testname #f)))
		       (if (or (not stored-test)
			       (and stored-test
				    (> (db:test-get-event_time testdat)(db:test-get-event_time stored-test))))
			   ;; this test is younger, store it in the hash
			   (hash-table-set! tests-hash full-testname testdat))))
		   results)
		  (if (null? tal)
		      (map cdr (hash-table->alist tests-hash)) ;; return a list of the most recent tests
		      (loop (car tal)(cdr tal))))))))))

;; Function recursively checks if <db>.journal exists; if yes means db busy; call itself after delayed interval
;; return the sqlite3 db handle if possible
;; 
(define (db:delay-if-busy dbdat #!key (count 6))
  (if (not (configf:lookup *configdat* "server" "delay-on-busy")) 
      (and dbdat (db:dbdat-get-db dbdat))
      (if dbdat
	  (let* ((dbpath (db:dbdat-get-path dbdat))
		 (db     (db:dbdat-get-db   dbdat)) ;; we'll return this so (db:delay--if-busy can be called inline
		 (dbfj   (conc dbpath "-journal")))
	    (if (handle-exceptions
		 exn
		 (begin
		   (debug:print-info 0 *default-log-port* "WARNING: failed to test for existance of " dbfj)
		   (thread-sleep! 1)
		   (db:delay-if-busy count (- count 1))) 
		 (file-exists? dbfj))
		(case count
		  ((6)
		   (thread-sleep! 0.2)
		   (db:delay-if-busy count: 5))
		  ((5)
		   (thread-sleep! 0.4)
		   (db:delay-if-busy count: 4))
		  ((4)
		   (thread-sleep! 0.8)
		   (db:delay-if-busy count: 3))
		  ((3)
		   (thread-sleep! 1.6)
		   (db:delay-if-busy count: 2))
		  ((2)
		   (thread-sleep! 3.2)
		   (db:delay-if-busy count: 1))
		  ((1)
		   (thread-sleep! 6.4)
		   (db:delay-if-busy count: 0))
		  (else
		   (debug:print-info 0 *default-log-port* "delaying db access due to high database load.")
		   (thread-sleep! 12.8))))
	    db) 
	  "bogus result from db:delay-if-busy")))

(define (db:test-get-records-for-index-file dbstruct run-id test-name)
  (let ((res '()))
    (db:with-db
     dbstruct
     run-id
     #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (id itempath state status run_duration logf comment)
	  (set! res (cons (vector id itempath state status run_duration logf comment) res)))
	db
	"SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE testname=? AND item_path != '' AND run_id=?;" ;; BUG! WHY NO run_id?
	test-name
	run-id)
       res))))

;;======================================================================
;; Tests meta data
;;======================================================================

;; returns a hash table of tags to tests
;;
(define (db:get-tests-tags dbstruct)
  (db:with-db
   dbstruct #f #f
   (lambda (db)
     (let* ((res     (make-hash-table)))
       (sqlite3:for-each-row
	(lambda (testname tags-in)
	  (let ((tags (string-split tags-in ",")))
	    (for-each
	     (lambda (tag)
	       (hash-table-set! res tag
				(delete-duplicates
				 (cons testname (hash-table-ref/default res tag '())))))
	     tags)))
	db
	"SELECT testname,tags FROM test_meta")
       res))))

;; read the record given a testname
(define (db:testmeta-get-record dbstruct testname)
  (let ((res   #f))
    (db:with-db
     dbstruct
     #f
     #f
     (lambda (db)
       (sqlite3:for-each-row
	(lambda (id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)
	  (set! res (vector id testname author owner description reviewed iterated avg_runtime avg_disk tags jobgroup)))
	db
	"SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta WHERE testname=?;"
	testname)
       res))))

;; create a new record for a given testname
(define (db:testmeta-add-record dbstruct testname)
  (db:with-db dbstruct #f #f 
	      (lambda (db)
		(sqlite3:execute 
		 db
		 "INSERT OR IGNORE INTO test_meta (testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags) VALUES (?,'','','','','','','','');" testname))))

;; update one of the testmeta fields
(define (db:testmeta-update-field dbstruct testname field value)
  (db:with-db dbstruct #f #f 
	      (lambda (db)
		(sqlite3:execute 
		 db
		 (conc "UPDATE test_meta SET " field "=? WHERE testname=?;") value testname))))

(define (db:testmeta-get-all dbstruct)
  (db:with-db dbstruct #f #f 
	      (lambda (db)
		(let ((res '()))
		  (sqlite3:for-each-row
		   (lambda (a . b)
		     (set! res (cons (apply vector a b) res)))
		   db
		   "SELECT id,testname,author,owner,description,reviewed,iterated,avg_runtime,avg_disk,tags,jobgroup FROM test_meta;")
		  res))))

;;======================================================================
;; M I S C   M A N A G E M E N T   I T E M S 
;;======================================================================

;; A routine to map itempaths using a itemmap
;; patha and pathb must be strings or this will fail
;;
;; path-b is waiting on path-a
;;
(define (db:compare-itempaths test-b-name path-a path-b itemmaps )
  (debug:print-info 6 *default-log-port* "ITEMMAPS: " itemmaps)
  (let* ((itemmap    (tests:lookup-itemmap itemmaps test-b-name)))
    (if itemmap
	(let ((path-b-mapped (db:multi-pattern-apply path-b itemmap)))
	  (debug:print-info 6 *default-log-port* "ITEMMAP is " itemmap ", path: " path-b ", mapped path: " path-b-mapped)
	  (equal? path-a path-b-mapped))
	(equal? path-b path-a))))

;; A routine to convert test/itempath using a itemmap
;; NOTE: to process only an itempath (i.e. no prepended testname)
;;       just call db:multi-pattern-apply
;;
(define (db:convert-test-itempath path-in itemmap)
  (debug:print-info 6 *default-log-port* "ITEMMAP is " itemmap)
  (let* ((path-parts  (string-split path-in "/"))
	 (test-name   (if (null? path-parts) "" (car path-parts)))
	 (item-path   (string-intersperse (if (null? path-parts) '() (cdr path-parts)) "/")))
    (conc test-name "/" 
	  (db:multi-pattern-apply item-path itemmap))))

;; patterns are:
;;    "rx1"  "replacement1"\n
;;    "rx2"  "replacement2"
;; etc.
;;
(define (db:multi-pattern-apply item-path itemmap)
  (let ((all-patts (string-split itemmap "\n")))
    (if (null? all-patts)
	item-path
	(let loop ((hed (car all-patts))
		   (tal (cdr all-patts))
		   (res item-path))
	  (let* ((parts (string-split hed))
		 (patt  (car parts))
		 (repl  (if (> (length parts) 1)(cadr parts) ""))
		 (newr  (if (and patt repl)
			    (string-substitute patt repl res)
			    (begin
			      (debug:print 0 *default-log-port* "WARNING: itemmap has problem \"" itemmap "\", patt: " patt ", repl: " repl)
			      res))))
	    (if (null? tal)
		newr
		(loop (car tal)(cdr tal) newr)))))))

;; the new prereqs calculation, looks also at itempath if specified
;; all prereqs must be met
;;    if prereq test with itempath='' is COMPLETED and PASS, WARN, CHECK, or WAIVED then prereq is met
;;    if prereq test with itempath=ref-item-path and COMPLETED with PASS, WARN, CHECK, or WAIVED then prereq is met
;;
;; Note: mode 'normal means that tests must be COMPLETED and ok (i.e. PASS, WARN, CHECK, SKIP or WAIVED)
;;       mode 'toplevel means that tests must be COMPLETED only
;;       mode 'itemmatch or 'itemwait means that tests items must be COMPLETED and (PASS|WARN|WAIVED|CHECK) [[ NB// NOT IMPLEMENTED YET ]]
;; 
;; (define (db:get-prereqs-not-met dbstruct run-id waitons ref-item-path mode)
(define (db:get-prereqs-not-met dbstruct run-id waitons ref-test-name ref-item-path mode itemmaps) ;; #!key (mode '(normal))(itemmap #f))
  (if (or (not waitons)
	  (null? waitons))
      '()
      (let* ((unmet-pre-reqs '())
	     (result         '()))
	(for-each 
	 (lambda (waitontest-name)
	   ;; by getting the tests with matching name we are looking only at the matching test 
	   ;; and related sub items
	   ;; next should be using mt:get-tests-for-run?
	   (let ((tests             (db:get-tests-for-run-state-status dbstruct run-id waitontest-name))
		 (ever-seen         #f)
		 (parent-waiton-met #f)
		 (item-waiton-met   #f))
	     (for-each 
	      (lambda (test)
		;; (if (equal? waitontest-name (db:test-get-testname test)) ;; by defintion this had better be true ...
		(let* ((state             (db:test-get-state test))
		       (status            (db:test-get-status test))
		       (item-path         (db:test-get-item-path test))
		       (is-completed      (equal? state "COMPLETED"))
		       (is-running        (equal? state "RUNNING"))
		       (is-killed         (equal? state "KILLED"))
		       (is-ok             (member status '("PASS" "WARN" "CHECK" "WAIVED" "SKIP")))
		       ;;                                       testname-b    path-a    path-b
		       (same-itempath     (db:compare-itempaths ref-test-name item-path ref-item-path itemmaps))) ;; (equal? ref-item-path item-path)))
		  (set! ever-seen #t)
		  (cond
		   ;; case 1, non-item (parent test) is 
		   ((and (equal? item-path "") ;; this is the parent test of the waiton being examined
			 is-completed
			 (or is-ok (not (null? (lset-intersection eq? mode '(toplevel)))))) ;;  itemmatch itemwait))))))
		    (set! parent-waiton-met #t))
		   ;; Special case for toplevel and KILLED
		   ((and (equal? item-path "") ;; this is the parent test
			 is-killed
			 (member 'toplevel mode))
		    (set! parent-waiton-met #t))
		   ;; For itemwait mode IFF the previous matching item is good the set parent-waiton-met
		   ((and (not (null? (lset-intersection eq? mode '(itemmatch itemwait)))) ;; how is that different from (member mode '(itemmatch itemwait)) ?????
			 ;; (not (equal? item-path "")) ;; this applies to both top level (to allow launching of next batch) and items
			 same-itempath)
		    (if (and is-completed is-ok)
			(set! item-waiton-met #t))
		    (if (and (equal? item-path "")
			     (or is-completed is-running));; this is the parent, set it to run if completed or running
			(set! parent-waiton-met #t)))
		   ;; normal checking of parent items, any parent or parent item not ok blocks running
		   ((and is-completed
			 (or is-ok 
			     (member 'toplevel mode))              ;; toplevel does not block on FAIL
			 (and is-ok (member 'itemmatch mode))) ;; itemmatch blocks on not ok
		    (set! item-waiton-met #t)))))
		tests)
	     ;; both requirements, parent and item-waiton must be met to NOT add item to
	     ;; prereq's not met list
	     (if (not (or parent-waiton-met item-waiton-met))
		 (set! result (append (if (null? tests) (list waitontest-name) tests) result)))
	     ;; if the test is not found then clearly the waiton is not met...
	     ;; (if (not ever-seen)(set! result (cons waitontest-name result)))))
	     (if (not ever-seen)
		 (set! result (append (if (null? tests)(list waitontest-name) tests) result)))))
	 waitons)
	(delete-duplicates result))))

;;======================================================================
;; Extract ods file from the db
;;======================================================================

;; NOT REWRITTEN YET!!!!!

;; runspatt is a comma delimited list of run patterns
;; keypatt-alist must contain *all* keys with an associated pattern: '( ("KEY1" "%") .. )
(define (db:extract-ods-file dbstruct outputfile keypatt-alist runspatt pathmod)
  (let* ((keysstr  (string-intersperse (map car keypatt-alist) ","))
	 (keyqry   (string-intersperse (map (lambda (p)(conc (car p) " LIKE ? ")) keypatt-alist) " AND "))
	 (numkeys  (length keypatt-alist))
	 (test-ids '())
	 (dbdat    (db:get-db dbstruct))
	 (db       (db:dbdat-get-db dbdat))
	 (windows  (and pathmod (substring-index "\\" pathmod)))
	 (tempdir  (conc "/tmp/" (current-user-name) "/" runspatt "_" (random 10000) "_" (current-process-id)))
	 (runsheader (append (list "Run Id" "Runname") ; 0 1
			     (map car keypatt-alist)   ; + N = length keypatt-alist
			     (list "Testname"          ; 2
				   "Item Path"         ; 3 
				   "Description"       ; 4 
				   "State"             ; 5 
				   "Status"            ; 6  
				   "Final Log"         ; 7 
				   "Run Duration"      ; 8 
				   "When Run"          ; 9 
				   "Tags"              ; 10
				   "Run Owner"         ; 11
				   "Comment"           ; 12
				   "Author"            ; 13
				   "Test Owner"        ; 14
				   "Reviewed"          ; 15
				   "Diskfree"          ; 16
				   "Uname"             ; 17
				   "Rundir"            ; 18
				   "Host"              ; 19
				   "Cpu Load"          ; 20
				   )))
	 (results (list runsheader))			 
	 (testdata-header (list "Run Id" "Testname" "Item Path" "Category" "Variable" "Value" "Expected" "Tol" "Units" "Status" "Comment"))
	 (mainqry (conc "SELECT
              t.testname,r.id,runname," keysstr ",t.testname,
              t.item_path,tm.description,t.state,t.status,
              final_logf,run_duration, 
              strftime('%m/%d/%Y %H:%M:%S',datetime(t.event_time,'unixepoch'),'localtime'),
              tm.tags,r.owner,t.comment,
              author,
              tm.owner,reviewed,
              diskfree,uname,rundir,
              host,cpuload
            FROM tests AS t JOIN runs AS r ON t.run_id=r.id JOIN test_meta AS tm ON tm.testname=t.testname
            WHERE runname LIKE ? AND " keyqry ";")))
    (debug:print 2 *default-log-port* "Using " tempdir " for constructing the ods file. keyqry: " keyqry " keystr: " keysstr " with keys: " (map cadr keypatt-alist)
		 "\n      mainqry: " mainqry)
    ;; "Expected Value"
    ;; "Value Found"
    ;; "Tolerance"
    (apply sqlite3:for-each-row
	   (lambda (test-id . b)
	     (set! test-ids (cons test-id test-ids))   ;; test-id is now testname
	     (set! results (append results ;; note, drop the test-id
				   (list
				    (if pathmod
					(let* ((vb        (apply vector b))
					       (keyvals   (let loop ((i    0)
								     (res '()))
							    (if (>= i numkeys)
								res
								(loop (+ i 1)
								      (append res (list (vector-ref vb (+ i 2))))))))
					       (runname   (vector-ref vb 1))
					       (testname  (vector-ref vb (+  2 numkeys)))
					       (item-path (vector-ref vb (+  3 numkeys)))
					       (final-log (vector-ref vb (+  7 numkeys)))
					       (run-dir   (vector-ref vb (+ 18 numkeys)))
					       (log-fpath (conc run-dir "/"  final-log))) ;; (string-intersperse keyvals "/") "/" testname "/" item-path "/"
					  (debug:print 4 *default-log-port* "log: " log-fpath " exists: " (file-exists? log-fpath))
					  (vector-set! vb (+ 7 numkeys) (if (file-exists? log-fpath)
									    (let ((newpath (conc pathmod "/"
												 (string-intersperse keyvals "/")
												 "/" runname "/" testname "/"
												 (if (string=? item-path "") "" (conc "/" item-path))
												 final-log)))
									      ;; for now throw away newpath and use the log-fpath conc'd with pathmod
									      (set! newpath (conc pathmod log-fpath))
									      (if windows (string-translate newpath "/" "\\") newpath))
									    (if (debug:debug-mode 1)
										(conc final-log " not-found")
										"")))
					  (vector->list vb))
					b)))))
	   db
	   mainqry
	   runspatt (map cadr keypatt-alist))
    (debug:print 2 *default-log-port* "Found " (length test-ids) " records")
    (set! results (list (cons "Runs" results)))
    ;; now, for each test, collect the test_data info and add a new sheet
    (for-each
     (lambda (test-id)
       (let ((test-data (list testdata-header))
	     (curr-test-name #f))
	 (sqlite3:for-each-row
	  (lambda (run-id testname item-path category variable value expected tol units status comment)
	    (set! curr-test-name testname)
	    (set! test-data (append test-data (list (list run-id testname item-path category variable value expected tol units status comment)))))
	  db 
	  ;; "SELECT run_id,testname,item_path,category,variable,td.value AS value,expected,tol,units,td.status AS status,td.comment AS comment FROM test_data AS td INNER JOIN tests ON tests.id=td.test_id WHERE test_id=?;"
	  "SELECT run_id,testname,item_path,category,variable,td.value AS value,td.expected,td.tol,td.units,td.status AS status,td.comment AS comment FROM test_data AS td INNER JOIN tests ON tests.id=td.test_id WHERE testname=?;"
	  test-id)
	 (if curr-test-name
	     (set! results (append results (list (cons curr-test-name test-data)))))
	 ))
     (sort (delete-duplicates test-ids) string<=))
    (system (conc "mkdir -p " tempdir))
    ;; (pp results)
    (ods:list->ods 
     tempdir
     (if (string-match (regexp "^[/~]+.*") outputfile) ;; full path?
	 outputfile
	 (begin
	   (debug:print 0 *default-log-port* "WARNING: path given, " outputfile " is relative, prefixing with current directory")
	   (conc (current-directory) "/" outputfile)))
     results)
    ;; brutal clean up
    (system "rm -rf tempdir")))

;; (db:extract-ods-file db "outputfile.ods" '(("sysname" "%")("fsname" "%")("datapath" "%")) "%")


