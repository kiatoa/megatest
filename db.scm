;;======================================================================
;; Copyright 2006-2013, Matthew Welland.
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

(require-extension (srfi 18) extras tcp)
(use sqlite3 srfi-1 posix regex regex-case srfi-69 csv-xml s11n md5 message-digest base64 format dot-locking z3)
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
;; SQLITE3 HELPERS
;;======================================================================

;; convert to -inline
(define (db:first-result-default db stmt default . params)
  (handle-exceptions
   exn
   (let ((err-status ((condition-property-accessor 'sqlite3 'status #f) exn)))
     ;; check for (exn sqlite3) ((condition-property-accessor 'exn 'message) exn)
     (if (eq? err-status 'done)
	 default
	 (begin
	   (debug:print 0 "ERROR:  query " stmt " failed, params: " params ", error: " ((condition-property-accessor 'exn 'message) exn))
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
(define (db:get-db dbstruct run-id)
  (if (sqlite3:database? dbstruct) ;; pass sqlite3 databases on through
      dbstruct
      (begin
	(let ((dbdat (if (or (not run-id)
			     (eq? run-id 0))
			 (db:open-main dbstruct)
			 (db:open-rundb dbstruct run-id)
			 )))
	  dbdat))))

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
;;
(define (db:done-with dbstruct run-id mod-read)
  (if (not (sqlite3:database? dbstruct))
      (begin
	(mutex-lock! *rundb-mutex*)
	(if (eq? mod-read 'mod)
	    (dbr:dbstruct-set-mtime! dbstruct (current-milliseconds))
	    (dbr:dbstruct-set-rtime! dbstruct (current-milliseconds)))
	(dbr:dbstruct-set-inuse! dbstruct #f)
	(mutex-unlock! *rundb-mutex*))))

;; (db:with-db dbstruct run-id sqlite3:exec "select blah from blaz;")
;; r/w is a flag to indicate if the db is modified by this query #t = yes, #f = no
;;
(define (db:with-db dbstruct run-id r/w proc . params)
  (let* ((dbdat (if (vector? dbstruct)
		    (db:get-db dbstruct run-id)
		    dbstruct)) ;; cheat, allow for passing in a dbdat
	 (db    (db:dbdat-get-db dbdat)))
    (db:delay-if-busy dbdat)
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "ERROR: sqlite3 issue in db:with-db, dbstruct=" dbstruct ", run-id=" run-id ", proc=" proc ", params=" params " error: " ((condition-property-accessor 'exn 'message) exn))
       (print-call-chain (current-error-port)))
     (let ((res (apply proc db params)))
       (if (vector? dbstruct)(db:done-with dbstruct run-id r/w))
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
(define (db:dbfile-path run-id)
  (let* ((dbdir           (or (configf:lookup *configdat* "setup" "dbdir")
			      (conc (configf:lookup *configdat* "setup" "linktree") "/.db")))
	 (fname           (if run-id
			      (if (eq? run-id 0) "main.db" (conc run-id ".db"))
			      #f)))
    (handle-exceptions
     exn
     (begin
       (debug:print 0 "ERROR: Couldn't create path to " dbdir)
       (exit 1))
     (if (not (directory? dbdir))(create-directory dbdir #t)))
    (if fname
	(conc dbdir "/" fname)
	dbdir)))
	       
(define (db:set-sync db)
  (let ((syncprag (configf:lookup *configdat* "setup" "sychronous")))
    (sqlite3:execute db (conc "PRAGMA synchronous = " (or syncprag 1) ";"))))

;; open an sql database inside a file lock
;;
;; returns: db existed-prior-to-opening
;;
(define (db:lock-create-open fname initproc)
  ;; (if (file-exists? fname)
  ;;     (let ((db (sqlite3:open-database fname)))
  ;;       (sqlite3:set-busy-handler! db (make-busy-timeout 136000))
  ;;       (db:set-sync db) ;; (sqlite3:execute db "PRAGMA synchronous = 0;")
  ;;       db)
  (let* ((parent-dir   (pathname-directory fname))
	 (dir-writable (file-write-access? parent-dir))
	 (file-exists  (file-exists? fname))
	 (file-write   (if file-exists
			   (file-write-access? fname)
			   dir-writable )))
    (if file-write ;; dir-writable
	(let (;; (lock    (obtain-dot-lock fname 1 5 10))
	      (db      (sqlite3:open-database fname)))
	  (sqlite3:set-busy-handler! db (make-busy-timeout 136000))
	  (db:set-sync db) ;; (sqlite3:execute db "PRAGMA synchronous = 0;")
	  (if (not file-exists)(initproc db))
	  ;; (release-dot-lock fname)
	  db)
	(begin
	  (debug:print 2 "WARNING: opening db in non-writable dir " fname)
	  (sqlite3:open-database fname))))) ;; )

;; This routine creates the db. It is only called if the db is not already opened
;; 
(define (db:open-rundb dbstruct run-id #!key (attemptnum 0)(do-not-open #f)) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let* ((local  (dbr:dbstruct-get-local dbstruct))
	 (rdb    (if local
		     (dbr:dbstruct-get-localdb dbstruct run-id)
		     (dbr:dbstruct-get-inmem dbstruct)))) ;; (dbr:dbstruct-get-runrec dbstruct run-id 'inmem)))
    (if (or rdb
	    do-not-open)
	rdb
	(begin
	  (mutex-lock! *rundb-mutex*)
	  (let* ((dbpath       (db:dbfile-path run-id)) ;; (conc toppath "/db/" run-id ".db"))
		 (dbexists     (file-exists? dbpath))
		 (inmem        (if local #f (db:open-inmem-db)))
		 (refdb        (if local #f (db:open-inmem-db)))
		 (db           (db:lock-create-open dbpath ;; this is the database physically on disk
						    (lambda (db)
						      (handle-exceptions
						       exn
						       (begin
							 ;; (release-dot-lock dbpath)
							 (if (> attemptnum 2)
							     (debug:print 0 "ERROR: tried twice, cannot create/initialize db for run-id " run-id ", at path " dbpath)
							     (db:open-rundb dbstruct run-id attemptnum (+ attemptnum 1))))
						       (db:initialize-run-id-db db)
						       (sqlite3:execute 
							db
							"INSERT OR IGNORE INTO tests (id,run_id,testname,event_time,item_path,state,status) VALUES (?,?,'bogustest',strftime('%s','now'),'nowherepath','DELETED','n/a');"
							(* run-id 30000) ;; allow for up to 30k tests per run
							run-id)
						       ;; do a dummy query to test that the table exists and the db is truly readable
						       (sqlite3:execute db "SELECT * FROM tests WHERE id=?;" (* run-id 30000))
						       )))) ;; add strings db to rundb, not in use yet
		 ;;   )) ;; (sqlite3:open-database dbpath))
		 (olddb        (if *megatest-db*
				   *megatest-db* 
				   (let ((db (db:open-megatest-db)))
				     (set! *megatest-db* db)
				     db)))
		 (write-access (file-write-access? dbpath))
		 ;; (handler      (make-busy-timeout 136000))
		 )
	    (if (and dbexists (not write-access))
		(set! *db-write-access* #f)) ;; only unset so other db's also can use this control
	    (dbr:dbstruct-set-rundb!  dbstruct (cons db dbpath))
	    (dbr:dbstruct-set-inuse!  dbstruct #t)
	    (dbr:dbstruct-set-olddb!  dbstruct olddb)
	    ;; (dbr:dbstruct-set-run-id! dbstruct run-id)
	    (mutex-unlock! *rundb-mutex*)
	    (if local
		(begin
		  (dbr:dbstruct-set-localdb! dbstruct run-id db) ;; (dbr:dbstruct-set-inmem! dbstruct db) ;; direct access ...
		  db)
		(begin
		  (dbr:dbstruct-set-inmem!  dbstruct inmem)
		  ;; dec 14, 2014 - keep deleted records available. hunch is that they are needed for id placeholders
		  ;; (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED';") ;; they just slow us down in this context
		  (db:sync-tables db:sync-tests-only db inmem)
		  (db:delay-if-busy refdb) ;; dbpath: (db:dbdat-get-path refdb)) ;; What does delaying here achieve? 
		  (dbr:dbstruct-set-refdb!  dbstruct refdb)
		  (db:sync-tables db:sync-tests-only inmem refdb) ;; use inmem as the reference, don't read again from db
		  ;; sync once more to deal with delays?
		  ;; (db:sync-tables db:sync-tests-only db inmem)
		  ;; (db:sync-tables db:sync-tests-only inmem refdb)
		  inmem)))))))

;; This routine creates the db. It is only called if the db is not already ls opened
;;
(define (db:open-main dbstruct) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
  (let ((mdb (dbr:dbstruct-get-main dbstruct)))
    (if mdb
	mdb
	(begin
	  (mutex-lock! *rundb-mutex*)
	  (let* ((dbpath       (db:dbfile-path 0))
		 (dbexists     (file-exists? dbpath))
		 (db           (db:lock-create-open dbpath db:initialize-main-db))
		 (olddb        (db:open-megatest-db))
		 (write-access (file-write-access? dbpath))
		 (dbdat        (cons db dbpath)))
	    (if (and dbexists (not write-access))
		(set! *db-write-access* #f))
	    (dbr:dbstruct-set-main!   dbstruct dbdat)
	    (dbr:dbstruct-set-olddb!  dbstruct olddb) ;; olddb is already a (cons db path)
	    (mutex-unlock! *rundb-mutex*)
	    (if (and (not dbexists)
		     *db-write-access*) ;; did not have a prior db and do have write access
		(db:multi-db-sync #f 'old2new))  ;; migrate data from megatest.db automatically
	    dbdat)))))

;; Make the dbstruct, setup up auxillary db's and call for main db at least once
;;
(define (db:setup run-id #!key (local #f))
  (let* ((dbdir    (db:dbfile-path #f)) ;; (conc (configf:lookup *configdat* "setup" "linktree") "/.db"))
	 (dbstruct (make-dbr:dbstruct path: dbdir local: local)))
    dbstruct))

;; Open the classic megatest.db file in toppath
;;
(define (db:open-megatest-db)
  (let* ((dbpath       (conc *toppath* "/megatest.db"))
	 (dbexists     (file-exists? dbpath))
	 (db           (db:lock-create-open dbpath
					    (lambda (db)
					      (db:initialize-main-db db)
					      (db:initialize-run-id-db db))))
	 (write-access (file-write-access? dbpath)))
    (if (and dbexists (not write-access))
	(set! *db-write-access* #f))
    (cons db dbpath)))

;; sync run to disk if touched
;;
(define (db:sync-touched dbstruct run-id #!key (force-sync #f))
  (let ((mtime  (dbr:dbstruct-get-mtime dbstruct))
	(stime  (dbr:dbstruct-get-stime dbstruct))
	(rundb  (dbr:dbstruct-get-rundb dbstruct))
	(inmem  (dbr:dbstruct-get-inmem dbstruct))
	(maindb (dbr:dbstruct-get-main  dbstruct))
	(refdb  (dbr:dbstruct-get-refdb dbstruct))
	(olddb  (dbr:dbstruct-get-olddb dbstruct))
	;; (runid  (dbr:dbstruct-get-run-id dbstruct))
	)
    (debug:print-info 4 "Syncing for run-id: " run-id)
    ;; (mutex-lock! *http-mutex*)
    (if (eq? run-id 0)
	;; runid equal to 0 is main.db
	(if maindb
	    (if (or (not (number? mtime))
		    (not (number? stime))
		    (> mtime stime)
		    force-sync)
		(begin
		  (db:delay-if-busy maindb)
		  (db:delay-if-busy olddb)
		  (let ((num-synced (db:sync-tables (db:sync-main-list maindb) maindb olddb)))
		    (dbr:dbstruct-set-stime! dbstruct (current-milliseconds))
		    num-synced)
		  0))
	    (begin
	      ;; this can occur when using local access (i.e. not in a server)
	      ;; need a flag to turn it off.
	      ;;
	      (debug:print 3 "WARNING: call to sync main.db to megatest.db but main not initialized")
	      0))
	;; any other runid is a run
	(if (or (not (number? mtime))
		(not (number? stime))
		(> mtime stime)
		force-sync)
	    (begin
	      (db:delay-if-busy rundb)
	      (db:delay-if-busy olddb)
	      (dbr:dbstruct-set-stime! dbstruct (current-milliseconds))
	      (let ((num-synced (db:sync-tables db:sync-tests-only inmem refdb rundb olddb)))
		;; (mutex-unlock! *http-mutex*)
		num-synced)
	      (begin
		;; (mutex-unlock! *http-mutex*)
		0))))))

(define (db:close-main dbstruct)
  (let ((maindb (dbr:dbstruct-get-main dbstruct)))
    (if maindb
	(begin
	  (sqlite3:finalize! (db:dbdat-get-db maindb))
	  (dbr:dbstruct-set-main! dbstruct #f)))))

(define (db:close-run-db dbstruct run-id)
  (let ((rdb (db:open-rundb dbstruct run-id do-not-open: #t)))
    (if (and rdb
	     (sqlite3:database? rdb))
	(begin
	  (sqlite3:finalize! rdb)
	  (dbr:dbstruct-set-localdb! dbstruct run-id #f)
	  (dbr:dbstruct-set-inmem! dbstruct #f)))))

;; close all opened run-id dbs
(define (db:close-all dbstruct)
  ;; finalize main.db
  (db:sync-touched dbstruct 0 force-sync: #t)
  ;;(common:db-block-further-queries)
  ;; (mutex-lock! *db-sync-mutex*) ;; with this perhaps it isn't necessary to use the block-further-queries mechanism?

  (db:close-main dbstruct)
  
  (let ((locdbs (dbr:dbstruct-get-locdbs dbstruct)))
    (if (hash-table? locdbs)
	(for-each (lambda (run-id)
		    (db:close-run-db dbstruct run-id))
		  (hash-table-keys locdbs))))

  ;; (let* ((local (dbr:dbstruct-get-local dbstruct))
  ;;        (rundb (db:dbdat-get-db (dbr:dbstruct-get-rundb dbstruct))))
  ;;   (if local
  ;;       (for-each
  ;;        (lambda (dbdat)
  ;;          (let ((db (db:dbdat-get-db dbdat)))
  ;;            (if (sqlite3:database? db)
  ;;       	 (begin
  ;;       	   (sqlite3:interrupt! db)
  ;;       	   (sqlite3:finalize! db #t)))))
  ;;        ;; TODO: Come back to this and rework to delete from hashtable when finalized
  ;;        (hash-table-values (dbr:dbstruct-get-locdbs dbstruct))))
  ;;   (thread-sleep! 3)
  ;;   (if (and rundb
  ;;            (sqlite3:database? rundb))
  ;;       (handle-exceptions
  ;;        exn
  ;;        (begin 
  ;;          (debug:print 0 "WARNING: database files may not have been closed correctly. Consider running -cleanup-db")
  ;;          (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
  ;;          (debug:print 0 " db: " rundb)
  ;;          (print-call-chain (current-error-port))
  ;;          #f)
  ;;        (sqlite3:interrupt! rundb)
  ;;        (sqlite3:finalize! rundb #t))))
  ;; ;; (mutex-unlock! *db-sync-mutex*)
  )

(define (db:open-inmem-db)
  (let* ((db      (sqlite3:open-database ":memory:"))
	 (handler (make-busy-timeout 3600)))
    (sqlite3:set-busy-handler! db handler)
    (db:initialize-run-id-db db)
    (cons db #f)))

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
(define (db:sync-main-list db)
  (let ((keys  (db:get-keys db)))
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

;; use bunch of Unix commands to try to break the lock and recreate the db
;;
(define (db:move-and-recreate-db dbdat)
  (let* ((dbpath   (db:dbdat-get-path        dbdat))
	 (dbdir    (pathname-directory       dbpath))
	 (fname    (pathname-strip-directory dbpath))
	 (fnamejnl (conc fname "-journal"))
	 (tmpname  (conc fname "." (current-process-id)))
	 (tmpjnl   (conc fnamejnl "." (current-process-id))))
    (debug:print 0 "ERROR: " fname " appears corrupted. Making backup \"old/" fname "\"")
    (system (conc "cd " dbdir ";mkdir -p old;cat " fname " > old/" tmpname))
    (system (conc "rm -f " dbpath))
    (if (file-exists? fnamejnl)
	(begin
	  (debug:print 0 "ERROR: " fnamejnl " found, moving it to old dir as " tmpjnl)
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
    (debug:print-info 0 "Checking db " dbpath " for errors.")
    (cond
     ((not (file-write-access? dbdir))
      (debug:print 0 "WARNING: can't write to " dbdir ", can't fix " fname)
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
	 (debug:print 0 "FATAL: file " dbpath " was found corrupted, an attempt to fix has been made but you must start over.")
	 (debug:print 0
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
(define (db:sync-tables tbls fromdb todb . slave-dbs)
  (mutex-lock! *db-sync-mutex*)
  (handle-exceptions
   exn
   (begin
     (mutex-unlock! *db-sync-mutex*)
     (debug:print 0 "EXCEPTION: database probably overloaded or unreadable in db:sync-tables.")
     (print-call-chain (current-error-port))
     (debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
     (print "exn=" (condition->list exn))
     (debug:print 0 " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
     (debug:print 0 " src db:  " (db:dbdat-get-path fromdb))
     (for-each (lambda (dbdat)
		 (let ((dbpath (db:dbdat-get-path dbdat)))
		   (debug:print 0 " dbpath:  " dbpath)
		   (if (not (db:repair-db dbdat))
		       (begin
			 (debug:print 0 "ERROR: Failed to rebuild " dbpath ", exiting now.")
			 (exit)))))
	       (cons todb slave-dbs))
     
     0)
;;      (if *server-run* ;; we are inside a server, throw a sync-failed error
;; 	 (signal (make-composite-condition
;; 		 (make-property-condition 'sync-failed 'message "db:sync-tables failed in a server context.")))
;; 	 0)) ;; return zero for num synced

	 ;; (set! *time-to-exit* #t) ;; let watch dog know that it is time to die.
	 ;; (tasks:server-set-state! (db:delay-if-busy tdbdat) server-id "shutting-down")
	 ;; (portlogger:open-run-close portlogger:set-port port "released")
	 ;; (exit 1)))
   (cond
    ((not fromdb) (debug:print 3 "WARNING: db:sync-tables called with fromdb missing") -1)
    ((not todb)   (debug:print 3 "WARNING: db:sync-tables called with todb missing") -2)
    ((not (sqlite3:database? (db:dbdat-get-db fromdb)))
     (debug:print 0 "ERROR: db:sync-tables called with fromdb not a database " fromdb) -3)
    ((not (sqlite3:database? (db:dbdat-get-db todb)))
     (debug:print 0 "ERROR: db:sync-tables called with todb not a database " todb) -4)
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
		 (num-fields (length fields))
		 (field->num (make-hash-table))
		 (num->field (apply vector (map car fields)))
		 (full-sel   (conc "SELECT " (string-intersperse (map car fields) ",") 
				   " FROM " tablename ";"))
		 (full-ins   (conc "INSERT OR REPLACE INTO " tablename " ( " (string-intersperse (map car fields) ",") " ) "
				   " VALUES ( " (string-intersperse (make-list num-fields "?") ",") " );"))
		 (fromdat    '())
		 (fromdats   '())
		 (totrecords 0)
		 (batch-len  (string->number (or (configf:lookup *configdat* "sync" "batchsize") "10")))
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
		(debug:print-info 4 "found " totrecords " records to sync"))

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
		 ;; (db:delay-if-busy targdb) ;; NO WAITING
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
	      (should-print (common:low-noise-print 120 "db sync" (> runtime 500)))) ;; low and high sync times treated as separate.
	 (if should-print (debug:print 3 "INFO: db sync, total run time " runtime " ms"))
	 (for-each 
	  (lambda (dat)
	    (let ((tblname (car dat))
		  (count   (cdr dat)))
	      (set! tot-count (+ tot-count count))
	      (if (> count 0)
		  (if should-print (debug:print 0 (format #f "    ~10a ~5a" tblname count))))))
	  (sort (hash-table->alist numrecs)(lambda (a b)(> (cdr a)(cdr b))))))
       tot-count)))
   (mutex-unlock! *db-sync-mutex*)))

;; options:
;;
;;  'killservers  - kills all servers
;;  'dejunk       - removes junk records
;;  'adj-testids  - move test-ids into correct ranges
;;  'old2new      - sync megatest.db records to .db/{main,1,2 ...}.db
;;  'new2old      - sync .db/{main,1,2,3 ...}.db to megatest.db
;;  'closeall     - close all opened dbs
;;
;;  run-ids: '(1 2 3 ...) or #f (for all)
;;
(define (db:multi-db-sync run-ids . options)
  (let* ((toppath  (launch:setup-for-run))
	 (dbstruct (if toppath (make-dbr:dbstruct path: toppath) #f))
	 (mtdb     (if toppath (db:open-megatest-db)))
	 (allow-cleanup (if run-ids #f #t))
	 (run-ids  (if run-ids 
		       run-ids
		       (if toppath (begin
				     (db:delay-if-busy mtdb)
				     (db:get-all-run-ids mtdb)))))
	 (tdbdat  (tasks:open-db))
	 (servers (tasks:get-all-servers (db:delay-if-busy tdbdat))))
    
    ;; kill servers
    (if (member 'killservers options)
	(for-each
	 (lambda (server)
	   (tasks:server-delete-record (db:delay-if-busy tdbdat) (vector-ref server 0) "dbmigration")
	   (tasks:kill-server (vector-ref server 2)(vector-ref server 1)))
	 servers))

    ;; clear out junk records
    ;;
    (if (member 'dejunk options)
	(begin
	  (db:delay-if-busy mtdb)
	  (db:clean-up mtdb)))

    ;; adjust test-ids to fit into proper range
    ;;
    (if (member 'adj-testids options)
	(begin
	  (db:delay-if-busy mtdb)
	  (db:prep-megatest.db-for-migration mtdb)))

    ;; sync runs, test_meta etc.
    ;;
    (if (member 'old2new options)
	(begin
	  (db:sync-tables (db:sync-main-list mtdb) mtdb (db:get-db dbstruct #f))
	  (for-each 
	   (lambda (run-id)
	     (db:delay-if-busy mtdb)
	     (let ((testrecs (db:get-all-tests-info-by-run-id mtdb run-id))
		   (dbstruct (if toppath (make-dbr:dbstruct path: toppath local: #t) #f)))
	       (debug:print 0 "INFO: Propagating " (length testrecs) " records for run-id=" run-id " to run specific db")
	       (db:replace-test-records dbstruct run-id testrecs)
	       (sqlite3:finalize! (db:dbdat-get-db (dbr:dbstruct-get-rundb dbstruct)))))
	   run-ids)))

    ;; now ensure all newdb data are synced to megatest.db
    ;; do not use the run-ids list passed in to the function
    ;;
    (if (member 'new2old options)
	(let* ((maindb      (make-dbr:dbstruct path: toppath local: #t))
	       (src-run-ids (if run-ids run-ids (db:get-all-run-ids (db:dbdat-get-db (db:get-db maindb 0)))))
	       (all-run-ids (sort (delete-duplicates (cons 0 src-run-ids)) <))
	       (count       1)
	       (total       (length all-run-ids))
	       (dead-runs  '()))
	  (for-each
	   (lambda (run-id)
	     (debug:print 0 "Processing run " (if (eq? run-id 0) " main.db " run-id) ", " count " of " total)
	     (set! count (+ count 1))
	     (let* ((fromdb (if toppath (make-dbr:dbstruct path: toppath local: #t) #f))
		    (frundb (db:dbdat-get-db (db:get-db fromdb run-id))))
	       ;; (db:delay-if-busy frundb)
	       ;; (db:delay-if-busy mtdb)
	       ;; (db:clean-up frundb)
	       (if (eq? run-id 0)
		   (begin
		     (db:sync-tables (db:sync-main-list dbstruct) (db:get-db fromdb #f) mtdb)
		     (set! dead-runs (db:clean-up-maindb (db:get-db fromdb #f))))
		   (begin
		     ;; NB// must sync first to ensure deleted tests get marked as such in megatest.db
		     (db:sync-tables db:sync-tests-only (db:get-db fromdb run-id) mtdb)
		     (db:clean-up-rundb (db:get-db fromdb run-id))
		     ))))
	   all-run-ids)
	  ;; removed deleted runs
	  (let ((dbdir (tasks:get-task-db-path)))
	    (for-each (lambda (run-id)
			(let ((fullname (conc dbdir "/" run-id ".db")))
			  (if (file-exists? fullname)
			      (begin
				(debug:print 0 "Removing database file for deleted run " fullname)
				(delete-file fullname)))))
		      dead-runs))))
    ;; (db:close-all dbstruct)
    ;; (sqlite3:finalize! mdb)
    ))

;; keeping it around for debugging purposes only
(define (open-run-close-no-exception-handling  proc idb . params)
  (debug:print-info 11 "open-run-close-no-exception-handling START given a db=" (if idb "yes " "no ") ", params=" params)
  (if (or *db-write-access*
	  (not (member proc *db:all-write-procs*)))
      (let* ((db (cond
		  ((pair? idb)                 (db:dbdat-get-db idb))
		  ((sqlite3:database? idb)     idb)
		  ((not idb)                   (debug:print 0 "ERROR: cannot open-run-close with #f anymore"))
		  ((procedure? idb)            (idb))
		  (else   	               (debug:print 0 "ERROR: cannot open-run-close with #f anymore"))))
	     (res #f))
	(set! res (apply proc db params))
	(if (not idb)(sqlite3:finalize! dbstruct))
	(debug:print-info 11 "open-run-close-no-exception-handling END" )
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
	(debug:print 0 "EXCEPTION: database probably overloaded or unreadable.")
	(debug:print 0 " message: " ((condition-property-accessor 'exn 'message) exn))
	(print "exn=" (condition->list exn))
	(debug:print 0 " status:  " ((condition-property-accessor 'sqlite3 'status) exn))
	(print-call-chain (current-error-port))
	(thread-sleep! sleep-time)
	(debug:print-info 0 "trying db call one more time....this may never recover, if necessary kill process " (current-process-id) " on host " (get-host-name) " to clean up")))
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
			 CONSTRAINT runsconstraint UNIQUE (runname" (if havekeys "," "") keystr "));"))
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
       (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" "MEGATEST_VERSION" megatest-version)
       (debug:print-info 11 "db:initialize END")))))

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
                        CONSTRAINT testsconstraint UNIQUE (run_id, testname, item_path));")
     (sqlite3:execute db "CREATE INDEX IF NOT EXISTS tests_index ON tests (run_id, testname, item_path);")
     (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_steps 
                              (id INTEGER PRIMARY KEY,
                               test_id INTEGER, 
                               stepname TEXT, 
                               state TEXT DEFAULT 'NOT_STARTED', 
                               status TEXT DEFAULT 'n/a',
                               event_time TIMESTAMP,
                               comment TEXT DEFAULT '',
                               logfile TEXT DEFAULT '',
                               CONSTRAINT test_steps_constraint UNIQUE (test_id,stepname,state));")
     ;;   (sqlite3:execute db "CREATE TABLE IF NOT EXISTS test_data 
     ;;                               (id          INTEGER PRIMARY KEY,
     ;;                                      reviewed    TIMESTAMP DEFAULT (strftime('%s','now')),
     ;;                                      iterated    TEXT DEFAULT '',
     ;;                                      avg_runtime REAL DEFAULT -1,
     ;;                                      avg_disk    REAL DEFAULT -1,
     ;;                                      tags        TEXT DEFAULT '',
     ;;                                      jobgroup    TEXT DEFAULT 'default',
     ;;                                 CONSTRAINT test_meta_constraint UNIQUE (testname));")
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
                              CONSTRAINT test_data_constraint UNIQUE (test_id,category,variable));")
     ;; Why use FULL here? This data is not that critical
     ;; (sqlite3:execute db "PRAGMA synchronous = FULL;")
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
  (let* ((dbdat        (db:get-db dbstruct #f)) ;; archive tables are in main.db
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
  (let* ((dbdat        (db:get-db dbstruct #f)) ;; archive tables are in main.db
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
	  res)
	(begin
	  (sqlite3:execute
	   db
	   "INSERT OR REPLACE INTO archive_disks (archive_area_name,disk_path,last_df)
                VALUES (?,?,?);"
	   bdisk-name bdisk-path df)
	  (db:archive-register-disk dbstruct bdisk-name bdisk-path df)))))

;; record an archive path created on a given archive disk (identified by it's bdisk-id)
;; if path starts with / then it is full, otherwise it is relative to the archive disk
;; preference is to store the relative path.
;;
(define (db:archive-register-block-name dbstruct bdisk-id archive-path #!key (du #f))
  (let* ((dbdat        (db:get-db dbstruct #f)) ;; archive tables are in main.db
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

(define (open-logging-db) ;;  (conc *toppath* "/megatest.db") (car *configinfo*)))
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
  (let* ((dbdat        (db:get-db dbstruct run-id))
	 (db           (db:dbdat-get-db dbdat))
	 (incompleted '())
	 (oldlaunched '())
	 (toplevels   '())
	 (deadtime-str (configf:lookup *configdat* "setup" "deadtime"))
	 (deadtime     (if (and deadtime-str
				(string->number deadtime-str))
			   (string->number deadtime-str)
			   7200))) ;; two hours
    (if (number? ovr-deadtime)(set! deadtime ovr-deadtime))
    
    ;; in RUNNING or REMOTEHOSTSTART for more than 10 minutes
    ;;
    ;; HOWEVER: this code in run:test seems to work fine
    ;;              (> (- (current-seconds)(+ (db:test-get-event_time testdat)
    ;;                     (db:test-get-run_duration testdat)))
    ;;                    600) 
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row 
     (lambda (test-id run-dir uname testname item-path)
       (if (and (equal? uname "n/a")
		(equal? item-path "")) ;; this is a toplevel test
	   ;; what to do with toplevel? call rollup?
	   (begin
	     (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
	     (debug:print-info 0 "Found old toplevel test in RUNNING state, test-id=" test-id))
	   (set! incompleted (cons (list test-id run-dir uname testname item-path run-id) incompleted))))
     db
     "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > (run_duration + ?) AND state IN ('RUNNING','REMOTEHOSTSTART');"
     run-id deadtime)

    ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
    ;;
    (db:delay-if-busy dbdat)
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
    
    (debug:print-info 18 "Found " (length oldlaunched) " old LAUNCHED items, " (length toplevels) " old LAUNCHED toplevel tests and " (length incompleted) " tests marked RUNNING but apparently dead.")
    (if (and (null? incompleted)
	     (null? oldlaunched)
	     (null? toplevels))
	#f
	#t)))

;;  select end_time-now from
;;      (select testname,item_path,event_time+run_duration as
;;                          end_time,strftime('%s','now') as now from tests where state in
;;      ('RUNNING','REMOTEHOSTSTART','LAUNCED'));

(define (db:find-and-mark-incomplete dbstruct run-id ovr-deadtime)
  (let* ((dbdat        (db:get-db dbstruct run-id))
	 (db           (db:dbdat-get-db dbdat))
	 (incompleted '())
	 (oldlaunched '())
	 (toplevels   '())
	 (deadtime-str (configf:lookup *configdat* "setup" "deadtime"))
	 (deadtime     (if (and deadtime-str
				(string->number deadtime-str))
			   (string->number deadtime-str)
			   7200))) ;; two hours
    (if (number? ovr-deadtime)(set! deadtime ovr-deadtime))
    
    ;; in RUNNING or REMOTEHOSTSTART for more than 10 minutes
    ;;
    ;; HOWEVER: this code in run:test seems to work fine
    ;;              (> (- (current-seconds)(+ (db:test-get-event_time testdat)
    ;;                     (db:test-get-run_duration testdat)))
    ;;                    600) 
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row 
     (lambda (test-id run-dir uname testname item-path)
       (if (and (equal? uname "n/a")
		(equal? item-path "")) ;; this is a toplevel test
	   ;; what to do with toplevel? call rollup?
	   (begin
	     (set! toplevels   (cons (list test-id run-dir uname testname item-path run-id) toplevels))
	     (debug:print-info 0 "Found old toplevel test in RUNNING state, test-id=" test-id))
	   (set! incompleted (cons (list test-id run-dir uname testname item-path run-id) incompleted))))
     db
     "SELECT id,rundir,uname,testname,item_path FROM tests WHERE run_id=? AND (strftime('%s','now') - event_time) > (run_duration + ?) AND state IN ('RUNNING','REMOTEHOSTSTART');"
     run-id deadtime)

    ;; in LAUNCHED for more than one day. Could be long due to job queues TODO/BUG: Need override for this in config
    ;;
    (db:delay-if-busy dbdat)
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
    
    (debug:print-info 18 "Found " (length oldlaunched) " old LAUNCHED items, " (length toplevels) " old LAUNCHED toplevel tests and " (length incompleted) " tests marked RUNNING but apparently dead.")

    ;; These are defunct tests, do not do all the overhead of set-state-status. Force them to INCOMPLETE.
    ;;
    (db:delay-if-busy dbdat)
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
	    (debug:print 0 "WARNING: Marking test(s); " (string-intersperse (map conc all-ids) ", ") " as INCOMPLETE")
	    (sqlite3:execute 
	     db
	     (conc "UPDATE tests SET state='INCOMPLETE' WHERE id IN (" 
		   (string-intersperse (map conc all-ids) ",")
		   ");")))))

    ;; Now do rollups for the toplevel tests
    ;;
    (db:delay-if-busy dbdat)
    (for-each
     (lambda (toptest)
       (let ((test-name (list-ref toptest 3)))
;;	     (run-id    (list-ref toptest 5)))
	 (db:top-test-set-per-pf-counts dbstruct run-id test-name)))
     toplevels)))

(define (db:top-test-set-per-pf-counts dbstruct run-id test-name)
  (db:general-call (db:get-db dbstruct run-id) 'top-test-set-per-pf-counts (list test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name test-name))) 
 
		     
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
  ;; (debug:print 0 "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
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
    (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    (db:delay-if-busy dbdat)
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
  ;; (debug:print 0 "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
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
    (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    (db:delay-if-busy dbdat)
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
  ;; (debug:print 0 "WARNING: db clean up not fully ported to v1.60, cleanup action will be on megatest.db")
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
    (db:delay-if-busy dbdat)
    (sqlite3:with-transaction 
     db
     (lambda ()
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count before clean: " tot))
			     count-stmt)
       (map sqlite3:execute statements)
       (sqlite3:for-each-row (lambda (tot)
			       (debug:print-info 0 "Records count after  clean: " tot))
			     count-stmt)))
    (map sqlite3:finalize! statements)
    (sqlite3:finalize! count-stmt)
    ;; (db:find-and-mark-incomplete db)
    (db:delay-if-busy dbdat)
    (sqlite3:execute db "VACUUM;")
    dead-runs))

;;======================================================================
;; M E T A   G E T   A N D   S E T   V A R S
;;======================================================================

;; returns number if string->number is successful, string otherwise
;; also updates *global-delta*
;;
;; Operates on megatestdb
;;
(define (db:get-var dbstruct var)
  (let* ((start-ms (current-milliseconds))
         (throttle (let ((t  (config-lookup *configdat* "setup" "throttle")))
		     (if t (string->number t) t)))
	 (res      #f)
	 (dbdat    (db:get-db dbstruct #f))
	 (db       (db:dbdat-get-db dbdat)))
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row
     (lambda (val)
       (set! res val))
     db
     "SELECT val FROM metadat WHERE var=?;" var)
    ;; convert to number if can
    (if (string? res)
	(let ((valnum (string->number res)))
	  (if valnum (set! res valnum))))
    ;; scale by 10, average with current value.
    (set! *global-delta* (/ (+ *global-delta* (* (- (current-milliseconds) start-ms)
						 (if throttle throttle 0.01)))
			    2))
    (if (> (abs (- *last-global-delta-printed* *global-delta*)) 0.08) ;; don't print all the time, only if it changes a bit
	(begin
	  (debug:print-info 4 "launch throttle factor=" *global-delta*)
	  (set! *last-global-delta-printed* *global-delta*)))
    res))

(define (db:set-var dbstruct var val)
  (let ((dbdat (db:get-db dbstruct #f))
	(db    (db:dbdat-get-db dbdat)))
    (db:delay-if-busy dbdat)
    (sqlite3:execute db "INSERT OR REPLACE INTO metadat (var,val) VALUES (?,?);" var val)))

(define (db:del-var dbstruct var)
  ;; (db:delay-if-busy)
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
  (if (null? header) #f
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
  (let* ((dbdat     (db:get-db dbstruct #f))
	 (db        (db:dbdat-get-db dbdat))
	 (keys      (map car keyvals))
	 (keystr    (keys->keystr keys))	 
	 (comma     (if (> (length keys) 0) "," ""))
	 (andstr    (if (> (length keys) 0) " AND " ""))
	 (valslots  (keys->valslots keys)) ;; ?,?,? ...
	 (allvals   (append (list runname state status user) (map cadr keyvals)))
	 (qryvals   (append (list runname) (map cadr keyvals)))
	 (key=?str  (string-intersperse (map (lambda (k)(conc k "=?")) keys) " AND ")))
    (debug:print 3 "keys: " keys " allvals: " allvals " keyvals: " keyvals " key=?str is " key=?str)
    (debug:print 2 "NOTE: using target " (string-intersperse (map cadr keyvals) "/") " for this run")
    (if (and runname (null? (filter (lambda (x)(not x)) keyvals))) ;; there must be a better way to "apply and"
	(let ((res #f))
	  (db:delay-if-busy dbdat)
	  (apply sqlite3:execute db (conc "INSERT OR IGNORE INTO runs (runname,state,status,owner,event_time" comma keystr ") VALUES (?,?,?,?,strftime('%s','now')" comma valslots ");")
		 allvals)
	  (db:delay-if-busy dbdat)
	  (apply sqlite3:for-each-row 
		 (lambda (id)
		   (set! res id))
		 db
		 (let ((qry (conc "SELECT id FROM runs WHERE (runname=? " andstr key=?str ");")))
					;(debug:print 4 "qry: " qry) 
		   qry)
		 qryvals)
	  (db:delay-if-busy dbdat)
	  (sqlite3:execute db "UPDATE runs SET state=?,status=?,event_time=strftime('%s','now') WHERE id=? AND state='deleted';" state status res)
	  res) 
	(begin
	  (debug:print 0 "ERROR: Called without all necessary keys")
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
    (debug:print-info 11 "db:get-runs START qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (db:with-db dbstruct #f #f
		(lambda (db)		
		  (sqlite3:for-each-row
		   (lambda (a . x)
		     (set! res (cons (apply vector a x) res)))
		   db
		   qrystr
		   )))
    (debug:print-info 11 "db:get-runs END qrystr: " qrystr " keypatts: " keypatts " offset: " offset " limit: " count)
    (vector header res)))

(define (db:get-changed-run-ids since-time)
  (let* ((dbdir      (db:dbfile-path #f)) ;; (configf:lookup *configdat* "setup" "dbdir"))
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
		    (debug:print 2 "WARNING: Failed to process " dbfile " for run-id")
		    0))))
	  changed))))

;; db:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db:get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
;;  to extract info from the structure returned
;;
;; NOTE: THIS IS COMPLETELY UNFINISHED. IT GOES WITH rmt:get-get-paths-matching-keynames
;;
;; (define (db:get-run-ids-matching dbstruct keynames target res)
;; ;; (define (db:get-runs-by-patt dbstruct keys runnamepatt targpatt offset limit) ;; test-name)
;;   (let* ((tmp      (runs:get-std-run-fields keys '("id" "runname" "state" "status" "owner" "event_time")))
;; 	 (keystr   (car tmp))
;; 	 (header   (cadr tmp))
;; 	 (res     '())
;; 	 (key-patt "")
;; 	 (runwildtype (if (substring-index "%" runnamepatt) "like" "glob"))
;; 	 (qry-str  #f)
;; 	 (keyvals  (if targpatt (keys:target->keyval keys targpatt) '())))
;;     (for-each (lambda (keyval)
;; 		(let* ((key    (car keyval))
;; 		       (patt   (cadr keyval))
;; 		       (fulkey (conc ":" key))
;; 		       (wildtype (if (substring-index "%" patt) "like" "glob")))
;; 		  (if patt
;; 		      (set! key-patt (conc key-patt " AND " key " " wildtype " '" patt "'"))
;; 		      (begin
;; 			(debug:print 0 "ERROR: searching for runs with no pattern set for " fulkey)
;; 			(exit 6)))))
;; 	      keyvals)
;;     (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt " ORDER BY event_time "
;; 			(if limit  (conc " LIMIT " limit)   "")
;; 			(if offset (conc " OFFSET " offset) "")
;; 			";"))
;;     (debug:print-info 4 "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
;;     (db:with-db dbstruct #f #f ;; reads db, does not write to it.
;; 		(lambda (db)
;; 		  (sqlite3:for-each-row
;; 		   (lambda (a . r)
;; 		     (set! res (cons (list->vector (cons a r)) res)))
;; 		   (db:get-db dbstruct #f)
;; 		   qry-str
;; 		   runnamepatt)))
;;     (vector header res)))

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
       (debug:print-info 11 "db:get-targets END qrystr: " qrystr )
       (vector header res)))))

;; just get count of runs
(define (db:get-num-runs dbstruct runpatt)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (let ((numruns 0))
       (debug:print-info 11 "db:get-num-runs START " runpatt)
       (sqlite3:for-each-row 
	(lambda (count)
	  (set! numruns count))
	db
	"SELECT COUNT(id) FROM runs WHERE runname LIKE ? AND state != 'deleted';" runpatt)
       (debug:print-info 11 "db:get-num-runs END " runpatt)
       numruns))))

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
;; ( (runname (( state  count ) ... ))
;;   (   ...  
(define (db:get-run-stats dbstruct)
  (let* ((dbdat        (db:get-db dbstruct #f))
	 (db           (db:dbdat-get-db dbdat))
	 (totals       (make-hash-table))
	 (curr         (make-hash-table))
	 (res          '())
	 (runs-info    '()))
    ;; First get all the runname/run-ids
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row
     (lambda (run-id runname)
       (set! runs-info (cons (list run-id runname) runs-info)))
     db
     "SELECT id,runname FROM runs WHERE state != 'deleted' ORDER BY event_time DESC;") ;; If you change this to the more logical ASC please adjust calls to db:get-run-stats
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
	     "SELECT state,status,count(id) FROM tests AS t GROUP BY state,status ORDER BY state,status DESC;")
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
(define (db:get-runs-by-patt dbstruct keys runnamepatt targpatt offset limit fields) ;; test-name)
  (let* ((tmp      (runs:get-std-run-fields keys (or fields '("id" "runname" "state" "status" "owner" "event_time"))))
	 (keystr   (car tmp))
	 (header   (cadr tmp))
	 (res     '())
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
			(debug:print 0 "ERROR: searching for runs with no pattern set for " fulkey)
			(exit 6)))))
	      keyvals)
    (set! qry-str (conc "SELECT " keystr " FROM runs WHERE state != 'deleted' AND runname " runwildtype " ? " key-patt " ORDER BY event_time "
			(if limit  (conc " LIMIT " limit)   "")
			(if offset (conc " OFFSET " offset) "")
			";"))
    (debug:print-info 4 "runs:get-runs-by-patt qry=" qry-str " " runnamepatt)
    (db:with-db dbstruct #f #f ;; reads db, does not write to it.
		(lambda (db)
		  (sqlite3:for-each-row
		   (lambda (a . r)
		     (set! res (cons (list->vector (cons a r)) res)))
		   db
		   qry-str
		   runnamepatt)))
    (vector header res)))

;; use (get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
(define (db:get-run-info dbstruct run-id)
  ;;(if (hash-table-ref/default *run-info-cache* run-id #f)
  ;;    (hash-table-ref *run-info-cache* run-id)
  (let* ((dbdat     (db:get-db dbstruct #f))
	 (db        (db:dbdat-get-db dbdat))
	 (res       (vector #f #f #f #f))
	 (keys      (db:get-keys dbstruct))
	 (remfields (list "id" "runname" "state" "status" "owner" "event_time"))
	 (header    (append keys remfields))
	 (keystr    (conc (keys->keystr keys) ","
			  (string-intersperse remfields ","))))
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row
     (lambda (a . x)
       (set! res (apply vector a x)))
     db 
     (conc "SELECT " keystr " FROM runs WHERE id=? AND state != 'deleted';")
     run-id)
    (debug:print-info 11 "db:get-run-info run-id: " run-id " header: " header " keystr: " keystr)
    (let ((finalres (vector header res)))
      ;; (hash-table-set! *run-info-cache* run-id finalres)
      finalres)))

(define (db:set-comment-for-run dbstruct run-id comment)
  (db:with-db
   dbstruct
   #f
   #f
   (lambda (db)
     (sqlite3:execute db "UPDATE runs SET comment=? WHERE id=?;" comment ;; (sdb:qry 'getid comment)
		      run-id))))

;; does not (obviously!) removed dependent data. But why not!!?
(define (db:delete-run dbstruct run-id)
  ;; First set any related tests to DELETED
  (let* ((rdbdat (db:get-db dbstruct run-id))
	 (rdb    (db:dbdat-get-db rdbdat))
	 (dbdat  (db:get-db dbstruct #f))
	 (db     (db:dbdat-get-db dbdat)))
    (db:delay-if-busy rdbdat)
    (sqlite3:execute rdb "UPDATE tests SET state='DELETED',comment='';")
    (sqlite3:execute rdb "DELETE FROM test_steps;")
    (sqlite3:execute rdb "DELETE FROM test_data;")
    (db:delay-if-busy dbdat)
    (sqlite3:execute db "UPDATE runs SET state='deleted',comment='' WHERE id=?;" run-id)))

(define (db:update-run-event_time dbstruct run-id)
  (db:with-db
   dbstruct
   #f
   #t
   (lambda (db)
     (sqlite3:execute db "UPDATE runs SET event_time=strftime('%s','now') WHERE id=?;" run-id))))

(define (db:lock/unlock-run dbstruct run-id lock unlock user)
  (db:with-db
   dbstruct
   #f
   #t
   (lambda (db)
     (let ((newlockval (if lock "locked"
			   (if unlock
			       "unlocked"
			       "locked")))) ;; semi-failsafe
       (sqlite3:execute db "UPDATE runs SET state=? WHERE id=?;" newlockval run-id)
       (sqlite3:execute db "INSERT INTO access_log (user,accessed,args) VALUES(?,strftime('%s','now'),?);"
			user (conc newlockval " " run-id))
       (debug:print-info 1 "" newlockval " run number " run-id)))))

(define (db:set-run-status dbstruct run-id status msg)
  (let* ((dbdat (db:get-db dbstruct #f))
	 (db    (db:dbdat-get-db dbdat)))
    (db:delay-if-busy dbdat)
    (if msg
	(sqlite3:execute db "UPDATE runs SET status=?,comment=? WHERE id=?;" status msg run-id)
	(sqlite3:execute db "UPDATE runs SET status=? WHERE id=?;" status run-id))))

(define (db:get-run-status dbstruct run-id)
  (let ((res "n/a"))
    (db:with-db
     dbstruct
     #f
     #f
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
	 (res  '())
	 (dbdat  (db:get-db dbstruct #f))
	 (db     (db:dbdat-get-db dbdat)))
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	 (db:delay-if-busy dbdat)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons (list key key-val) res)))
	  db qry run-id)))
     keys)
    (reverse res)))

;; get key vals for a given run-id
(define (db:get-key-vals dbstruct run-id)
  (let* ((keys (db:get-keys dbstruct))
	 (res  '())
	 (dbdat  (db:get-db dbstruct #f))
	 (db     (db:dbdat-get-db dbdat)))
    (for-each 
     (lambda (key)
       (let ((qry (conc "SELECT " key " FROM runs WHERE id=?;")))
	 (db:delay-if-busy dbdat)
	 (sqlite3:for-each-row 
	  (lambda (key-val)
	    (set! res (cons key-val res)))
	  db qry run-id)))
     keys)
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
      (db:with-db dbstruct #f #f ;; #f means work with the zeroth db - i.e. the runs db
       (lambda (db)
	 (apply sqlite3:for-each-row
		(lambda (id)
		  (set! prev-run-ids (cons id prev-run-ids)))
		db
		(conc "SELECT id FROM runs WHERE " qrystr " AND state != 'deleted' AND id != ?;") (append kvalues (list run-id)))))
      prev-run-ids)))

;;======================================================================
;;  T E S T S
;;======================================================================

;; states and statuses are lists, turn them into ("PASS","FAIL"...) and use NOT IN
;; i.e. these lists define what to NOT show.
;; states and statuses are required to be lists, empty is ok
;; not-in #t = above behaviour, #f = must match
(define (db:get-tests-for-run dbstruct run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals)
  (if (not (number? run-id))
      (begin ;; no need to treat this as an error by default
	(debug:print 4 "WARNING: call to db:get-tests-for-run with bad run-id=" run-id)
	;; (print-call-chain (current-error-port))
	'())
      (let* ((qryvalstr       (case qryvals
				((shortlist) "id,run_id,testname,state,status,item_path")
				((#f)        db:test-record-qry-selector) ;; "id,run_id,testname,state,status,event_time,host,cpuload,diskfree,uname,rundir,item_path,run_duration,final_logf,comment")
				(else        qryvals)))
	     (res            '())
	     ;; if states or statuses are null then assume match all when not-in is false
	     (states-qry      (if (null? states) 
				  #f
				  (conc " state "  
					(if not-in
					    " NOT IN ('"
					    " IN ('") 
					(string-intersperse states   "','")
					"')")))
	     (statuses-qry    (if (null? statuses)
				  #f
				  (conc " status "
					(if not-in 
					    " NOT IN ('"
					    " IN ('") 
					(string-intersperse statuses "','")
					"')")))
	     (states-statuses-qry 
	      (cond 
	       ((and states-qry statuses-qry)
		(conc " AND ( " states-qry " AND " statuses-qry " ) "))
	       (states-qry  
		(conc " AND " states-qry))
	       (statuses-qry 
		(conc " AND " statuses-qry))
	       (else "")))
	     (tests-match-qry (tests:match->sqlqry testpatt))
	     (qry             (conc "SELECT " qryvalstr
				    " FROM tests WHERE run_id=? AND state != 'DELETED' "
				    states-statuses-qry
				    (if tests-match-qry (conc " AND (" tests-match-qry ") ") "")
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
	(debug:print-info 8 "db:get-tests-for-run run-id=" run-id ", qry=" qry)
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
    (debug:print-info 8 "db:get-tests-for-run qry=" qry)
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
(define (db:get-tests-for-runs-mindata dbstruct run-ids testpatt states statuses not-in)
  (debug:print 0 "ERROR: BROKN!")
  ;; (db:get-tests-for-runs dbstruct run-ids testpatt states statuses not-in: not-in qryvals: "id,run_id,testname,state,status,event_time,item_path"))
)

;; get a useful subset of the tests data (used in dashboard
;;
(define (db:get-tests-for-run-mindata dbstruct run-id testpatt states statuses not-in)
  (db:get-tests-for-run dbstruct run-id testpatt states statuses #f #f not-in #f #f "id,run_id,testname,state,status,event_time,item_path"))

;; do not use.
;;
(define (db:get-tests-for-runs dbstruct run-ids testpatt states statuses #!key (not-in #f)(qryvals #f))
  ;; (db:delay-if-busy)
  (let ((res '()))
    (for-each 
     (lambda (run-id)
       (set! res (append 
		  res 
		  (db:get-tests-for-run dbstruct run-id testpatt states statuses #f #f not-in #f #f qryvals))))
     (if run-ids
	 run-ids
	 (db:get-all-run-ids dbstruct)))
    res))

;; Convert calling routines to get list of run-ids and loop, do not use the get-tests-for-runs
;;

(define (db:delete-test-records dbstruct run-id test-id)
  (let* ((dbdat (db:get-db dbstruct run-id))
	 (db    (db:dbdat-get-db dbdat)))
    (db:general-call dbdat 'delete-test-step-records (list test-id))
    ;; (db:delay-if-busy)
    (db:general-call dbdat 'delete-test-data-records (list test-id))
    (sqlite3:execute db "UPDATE tests SET state='DELETED',status='n/a',comment='' WHERE id=?;" test-id)))

(define (db:delete-old-deleted-test-records dbstruct)
  (let ((run-ids  (db:get-all-run-ids dbstruct))
	(targtime (- (current-seconds)(* 30 24 60 60)))) ;; one month in the past
    (for-each
     (lambda (run-id)
       (db:with-db
	dbstruct
	run-id
	#t
	(lambda (db)
	  (sqlite3:execute db "DELETE FROM tests WHERE state='DELETED' AND event_time<?;" targtime))))
     run-ids)))

;; set tests with state currstate and status currstatus to newstate and newstatus
;; use currstate = #f and or currstatus = #f to apply to any state or status respectively
;; WARNING: SQL injection risk. NB// See new but not yet used "faster" version below
;;
;;  AND NOT (item_path='' AND testname in (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")))
;;  (debug:print 0 "QRY: " qry)
;;  (db:delay-if-busy)
;;
;; NB// This call only operates on toplevel tests. Consider replacing it with more general call
;;
(define (db:set-tests-state-status dbstruct run-id testnames currstate currstatus newstate newstatus)
  (for-each (lambda (testname)
	      (let ((qry (conc "UPDATE tests SET state=?,status=? WHERE "
			       (if currstate  (conc "state='" currstate "' AND ") "")
			       (if currstatus (conc "status='" currstatus "' AND ") "")
			       " run_id=? AND testname LIKE ?;")))
		(db:with-db
		 dbstruct
		 run-id
		 #t
		 (lambda (db)
		   (let ((test-id (db:get-test-id dbstruct run-id testname "")))
		     (sqlite3:execute db qry newstate newstatus run-id testname)
		     (if test-id (mt:process-triggers run-id test-id newstate newstatus)))
		   ))))
	    testnames))

;; speed up for common cases with a little logic
;; NB// Ultimately this will be deprecated in deference to mt:test-set-state-status-by-id
;;
(define (db:test-set-state-status-by-id dbstruct run-id test-id newstate newstatus newcomment)
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
				       test-id))))
     (mt:process-triggers run-id test-id newstate newstatus))))

;; NEW BEHAVIOR: Count tests running in only one run!
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
  (let* ((dbdat (db:get-db dbstruct #f))
	 (db    (db:dbdat-get-db dbdat)))
  (if (not jobgroup)
      0 ;; 
      (let ((testnames '()))
	;; get the testnames
	(db:delay-if-busy dbdat)
	(sqlite3:for-each-row
	 (lambda (testname)
	   (set! testnames (cons testname testnames)))
	 db
	 "SELECT testname FROM test_meta WHERE jobgroup=?"
	 jobgroup)
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
	    0)))))
             ;; DEBUG FIXME - need to merge this v.155 query correctly   
             ;; AND testname in (SELECT testname FROM test_meta WHERE jobgroup=?)
             ;; AND NOT (uname = 'n/a' AND item_path = '');"

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
      "SELECT count(id) FROM tests WHERE state in ('LAUNCHED','NOT_STARTED','REMOTEHOSTSTART','RUNNING','KILLREQ');"))))

;; map run-id, testname item-path to test-id
(define (db:get-test-id dbstruct run-id testname item-path)
  (db:with-db
   dbstruct
   run-id
   #f
   (lambda (db)
     (db:first-result-default
      db
      "SELECT id FROM tests WHERE testname=? AND item_path=?;"
      #f ;; the default
      testname item-path))))

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

(define db:test-record-fields '("id"           "run_id"        "testname"  "item_path" "state"      "status"      "event_time"
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
  (let* ((dbdat (if (vector? dbstruct)
		    (db:get-db dbstruct run-id)
		    dbstruct)) ;; still settling on when to use dbstruct or dbdat
	 (db    (db:dbdat-get-db dbdat))
	 (res '()))
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row
     (lambda (id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir attemptnum archived)
       ;;                 0    1       2      3      4        5       6      7        8     9     10      11          12          13       14     15        16
       (set! res (cons (vector id run-id testname state status event-time host cpuload diskfree uname rundir item-path run-duration final-logf comment shortdir attemptnum archived)
		       res)))
     db
     (conc "SELECT " db:test-record-qry-selector " FROM tests WHERE state != 'DELETED' AND run_id=?;")
     run-id)
    res))

(define (db:replace-test-records dbstruct run-id testrecs)
  (db:with-db dbstruct run-id #t 
	      (lambda (db)
		(let* ((qmarks (string-intersperse (make-list (length db:test-record-fields) "?") ","))
		       (qrystr (conc "INSERT OR REPLACE INTO tests (" db:test-record-qry-selector ") VALUES (" qmarks ");"))
		       (qry    (sqlite3:prepare db qrystr)))
		  (debug:print 0 "INFO: migrating test records for run with id " run-id)
		  (sqlite3:with-transaction
		   db
		   (lambda ()
		     (for-each 
		      (lambda (rec)
			;; (debug:print 0 "INFO: Inserting values: " (string-intersperse (map conc (vector->list rec)) ",") "\n")
			(apply sqlite3:execute qry (vector->list rec)))
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
		(debug:print-info 0 "New test id " new-id " selected for test with id " test-id)
		(sqlite3:execute mtdb "UPDATE tests SET id=? WHERE id=?;" new-id test-id)))))))

;; move test ids into the 30k * run_id range
;;
(define (db:prep-megatest.db-adj-test-ids mtdb run-id testrecs)
  (debug:print-info 0 "Adjusting test ids in megatest.db for run " run-id)
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

;; Get test data using test_id
(define (db:get-test-info-by-id dbstruct run-id test-id)
  (db:with-db
   dbstruct
   run-id
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

(define (db:get-test-info dbstruct run-id testname item-path)
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
	(conc "SELECT " db:test-record-qry-selector " FROM tests WHERE testname=? AND item_path=?;")
	test-name item-path)
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
	(lambda (id test-id stepname state status event-time logfile)
	  (set! res (cons (vector id test-id stepname state status event-time (if (string? logfile) logfile "")) res)))
	db
	"SELECT id,test_id,stepname,state,status,event_time,logfile FROM test_steps WHERE status != 'DELETED' AND test_id=? ORDER BY id ASC;" ;; event_time DESC,id ASC;
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
  (let* ((dbdat      (db:get-db dbstruct run-id))
	 (db         (db:dbdat-get-db dbdat))
	 (fail-count 0)
	 (pass-count 0))
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row
     (lambda (fcount pcount)
       (set! fail-count fcount)
       (set! pass-count pcount))
     db 
     "SELECT (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'fail') AS fail_count,
             (SELECT count(id) FROM test_data WHERE test_id=? AND status like 'pass') AS pass_count;"
     test-id test-id)
    ;; Now rollup the counts to the central megatest.db
    (db:general-call dbdat 'pass-fail-counts (list pass-count fail-count test-id))
    ;; if the test is not FAIL then set status based on the fail and pass counts.
    (db:general-call dbdat 'test_data-pf-rollup (list test-id test-id test-id test-id))))

;; NOT USED!?
;;
(define (db:csv->test-data dbstruct run-id test-id csvdata)
  (debug:print 4 "test-id " test-id ", csvdata: " csvdata)
  (let* ((dbdat   (db:get-db dbstruct run-id))
	 (db      (db:dbdat-get-db dbdat))
	 (csvlist (csv->list (make-csv-reader
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
	 (debug:print 4 "BEFORE: category: " category " variable: " variable " value: " value 
		      ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment " type: " type)
	 
	 (if (and (or (not expected)(equal? expected ""))
		  (or (not tol)     (equal? expected ""))
		  (or (not units)   (equal? expected "")))
	     (let-values (((new-expected new-tol new-units)(tdb:get-prev-tol-for-test #f test-id category variable)))
			 (set! expected new-expected)
			 (set! tol      new-tol)
			 (set! units    new-units)))
	 
	 (debug:print 4 "AFTER:  category: " category " variable: " variable " value: " value 
		      ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	 ;; calculate status if NOT specified
	 (if (and (not status)(number? expected)(number? value)) ;; need expected and value to be numbers
	     (if (number? tol) ;; if tol is a number then we do the standard comparison
		 (let* ((max-val (+ expected tol))
			(min-val (- expected tol))
			(result  (and (>=  value min-val)(<= value max-val))))
		   (debug:print 4 "max-val: " max-val " min-val: " min-val " result: " result)
		   (set! status (if result "pass" "fail")))
		 (set! status ;; NB// need to assess each one (i.e. not return operator since need to act if not valid op.
		       (case (string->symbol tol) ;; tol should be >, <, >=, <=
			 ((>)  (if (>  value expected) "pass" "fail"))
			 ((<)  (if (<  value expected) "pass" "fail"))
			 ((>=) (if (>= value expected) "pass" "fail"))
			 ((<=) (if (<= value expected) "pass" "fail"))
			 (else (conc "ERROR: bad tol comparator " tol))))))
	 (debug:print 4 "AFTER2: category: " category " variable: " variable " value: " value 
		      ", expected: " expected " tol: " tol " units: " units " status: " status " comment: " comment)
	 (db:delay-if-busy dbdat)
	 (sqlite3:execute db "INSERT OR REPLACE INTO test_data (test_id,category,variable,value,expected,tol,units,comment,status,type) VALUES (?,?,?,?,?,?,?,?,?,?);"
			  test-id category variable value expected tol units (if comment comment "") status type)))
     csvlist)))

;; This routine moved from tdb.scm, tdb:read-test-data
;;
(define (db:read-test-data dbstruct run-id test-id categorypatt)
  (let* ((dbdat      (db:get-db dbstruct run-id))
	 (db         (db:dbdat-get-db dbdat))
	 (res '()))
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row 
     (lambda (id test_id category variable value expected tol units comment status type)
       (set! res (cons (vector id test_id category variable value expected tol units comment status type) res)))
     db
     "SELECT id,test_id,category,variable,value,expected,tol,units,comment,status,type FROM test_data WHERE test_id=? AND category LIKE ? ORDER BY category,variable;" test-id categorypatt)
    (reverse res)))

;;======================================================================
;; Misc. test related queries
;;======================================================================

(define (db:get-run-ids-matching-target dbstruct keynames target res runname testpatt statepatt statuspatt)
  (let* ((dbdat    (db:get-db dbstruct #f))
	 (db       (db:dbdat-get-db dbdat))
	 (row-ids '())
	 (keystr (string-intersperse 
		  (map (lambda (key val)
			 (conc key " like '" val "'"))
		       keynames 
		       (string-split target "/"))
		  " AND "))
	 ;; (testqry (tests:match->sqlqry testpatt))
	 (runsqry (sqlite3:prepare db (conc "SELECT id FROM runs WHERE " keystr " AND runname LIKE '" runname "';"))))
    ;; (debug:print 8 "db:test-get-paths-matching-keynames-target-new\n  runsqry=" runsqry "\n  tstsqry=" testqry)
    (sqlite3:for-each-row
     (lambda (rid)
       (set! row-ids (cons rid row-ids)))
     runsqry)
    (sqlite3:finalize! runsqry)
    row-ids))

(define (db:test-get-paths-matching-keynames-target-new dbstruct run-id keynames target res testpatt statepatt statuspatt runname)
  (let* ((testqry (tests:match->sqlqry testpatt))
	 (tstsqry (conc "SELECT rundir FROM tests WHERE " testqry " AND state LIKE '" statepatt "' AND status LIKE '" statuspatt "' ORDER BY event_time ASC;")))
    (db:with-db
     dbstruct
     run-id
     #f
     (lambda (db)
       (sqlite3:for-each-row 
	(lambda (p)
	  (set! res (cons p res)))
	db
	tstsqry)
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
	  (lambda ()(serialize obj)))))
      #t))
    ((zmq nmsg)(with-output-to-string (lambda ()(serialize obj))))
    (else obj)))

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
	   (debug:print 0 "ERROR: reception failed. Received " msg " but cannot translate it.")
	   msg))) ;; crude reply for when things go awry
    ((zmq nmsg)(with-input-from-string msg (lambda ()(deserialize))))
    (else msg)))

(define (db:test-set-status-state dbstruct run-id test-id status state msg)
  (let ((dbdat  (db:get-db dbstruct run-id)))
    (if (member state '("LAUNCHED" "REMOTEHOSTSTART"))
	(db:general-call dbdat 'set-test-start-time (list test-id)))
    (if msg
	(db:general-call dbdat 'state-status-msg (list state status msg test-id))
	(db:general-call dbdat 'state-status     (list state status test-id)))
     (mt:process-triggers run-id test-id state status)))

;; call with state = #f to roll up with out accounting for state/status of this item
;;
(define (db:roll-up-pass-fail-counts dbstruct run-id test-name item-path state status)
  (if (not (equal? item-path ""))
      (let ((dbdat (db:get-db dbstruct run-id)))
	;;	(db    (db:dbdat-get-db dbdat)))
	(db:general-call dbdat 'update-pass-fail-counts (list test-name test-name test-name))
	(db:top-test-set-per-pf-counts dbstruct run-id test-name))))
  
;;     (case (string->symbol status)
;;       ((RUNNING)  (db:general-call dbdat 'top-test-set-running (list test-name)))
;;       ((LAUNCHED) (db:general-call dbdat 'top-test-set (list "LAUNCHED" test-name)))
;;       ((ABORT INCOMPLETE) (db:general-call dbdat 'top-test-set (list status test-name))))
    
;;     (if (or (not state)
;; 	    (not (equal? item-path "")))
;; 	;; just do a rollup
;; 	(begin
;; 	  (db:top-test-set-per-pf-counts dbdat run-id test-name)
;; 	  #f)
;; 	(begin
;; 	  ;; NOTE: No else clause needed for this case
;; 	  (case (string->symbol status)
;; 	    ((RUNNING)  (db:general-call dbdat 'top-test-set-running (list test-name)))
;; 	    ((LAUNCHED) (db:general-call dbdat 'top-test-set (list "LAUNCHED" test-name)))
;; 	    ((ABORT INCOMPLETE) (db:general-call dbdat 'top-test-set (list status test-name))))
;; 	  #f)
;; 	)))

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
	      (debug:print 2 "Found path: " path)
	      (debug:print 2 "No such path: " path))) ;; )
	db
	"SELECT rundir,final_logf FROM tests WHERE testname=? AND item_path='';"
	test-name)
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
	'(test-set-rundir-shortdir "UPDATE tests SET rundir=?,shortdir=? WHERE testname=? AND item_path=?;")
	'(delete-tests-in-state   ;; "DELETE FROM tests WHERE state=?;")                  ;; DONE
	  "UPDATE tests SET state='DELETED' WHERE state=?")
	'(tests:test-set-toplog   "UPDATE tests SET final_logf=? WHERE run_id=? AND testname=? AND item_path='';")
	'(update-cpuload-diskfree "UPDATE tests SET cpuload=?,diskfree=? WHERE id=?;") ;; DONE
	'(update-uname-host       "UPDATE tests SET uname=?,host=? WHERE id=?;")       ;; DONE
	'(update-test-state       "UPDATE tests SET state=? WHERE state=? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	'(update-test-status      "UPDATE tests SET status=? WHERE status like ? AND run_id=? AND testname=? AND NOT (item_path='' AND testname IN (SELECT DISTINCT testname FROM tests WHERE testname=? AND item_path != ''));")
	;; stuff for roll-up-pass-fail-counts
	'(update-pass-fail-counts "UPDATE tests 
             SET fail_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('FAIL','CHECK','INCOMPLETE','ABORT')),
                 pass_count=(SELECT count(id) FROM tests WHERE testname=? AND item_path != '' AND status IN ('PASS','WARN','WAIVED'))
             WHERE testname=? AND item_path='';") ;; DONE
	'(top-test-set          "UPDATE tests SET state=? WHERE testname=? AND item_path='';") ;; DONE
	'(top-test-set-running  "UPDATE tests SET state='RUNNING' WHERE testname=? AND item_path='';") ;; DONE


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
                       WHERE testname=? AND item_path='';") ;; DONE

	;; STEPS
	'(delete-test-step-records "UPDATE test_steps SET status='DELETED' WHERE test_id=?;")
	'(delete-test-data-records "UPDATE test_data  SET status='DELETED' WHERE test_id=?;") ;; using status since no state field
	))

(define (db:lookup-query qry-name)
  (let ((q (alist-ref qry-name db:queries)))
    (if q (car q) #f)))

;; do not run these as part of the transaction
(define db:special-queries   '(rollup-tests-pass-fail
			       ;; db:roll-up-pass-fail-counts  ;; WHY NOT!?
			       login
			       immediate
			       flush
			       sync
			       set-verbosity
			       killserver
			       ))

(define (db:login dbstruct calling-path calling-version run-id client-signature)
  (cond 
   ((not (equal? calling-path *toppath*))
    (list #f "Login failed due to mismatch paths: " calling-path ", " *toppath*))
   ((not (equal? *run-id* run-id))
    (list #f "Login failed due to mismatch run-id: " run-id ", " *run-id*))
   ((not (equal? megatest-version calling-version))
    (list #f "Login failed due to mismatch megatest version: " calling-version ", " megatest-version))
   (else
    (hash-table-set! *logged-in-clients* client-signature (current-seconds))
    '(#t "successful login"))))

(define (db:general-call dbdat stmtname params)
  (let ((query (let ((q (alist-ref (if (string? stmtname)
				       (string->symbol stmtname)
				       stmtname)
				   db:queries)))
 		 (if q (car q) #f))))
    (db:delay-if-busy dbdat)
    (apply sqlite3:execute (db:dbdat-get-db dbdat) query params)
    #t))

;; get a summary of state and status counts to calculate a rollup
;;
;; NOTE: takes a db, not a dbstruct
;;
(define (db:get-state-status-summary db run-id testname)
  (let ((res   '()))
    (sqlite3:for-each-row
     (lambda (state status count)
       (set! res (cons (vector state status count) res)))
     db
     "SELECT state,status,count(state) FROM tests WHERE run_id=? AND testname=? AND item_path='' GROUP BY state,status;"
     run-id testname)
    res))

(define (db:set-top-level-from-items dbstruct run-id testname)
  (let* ((dbdat (db:get-db dbstruct run-id))
	 (db    (db:dbdat-get-db dbdat))
	 (summ  (db:get-state-status-summary db run-id testname))
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
  (let* ((dbdat   (db:get-db dbstruct #f))
	 (db      (db:dbdat-get-db dbdat))
	 (keys    (db:get-keys db))
	 (selstr  (string-intersperse keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND "))
	 (keyvals #f)
	 (tests-hash (make-hash-table)))
    ;; first look up the key values from the run selected by run-id
    (db:delay-if-busy dbdat)
    (sqlite3:for-each-row 
     (lambda (a . b)
       (set! keyvals (cons a b)))
     db
     (conc "SELECT " selstr " FROM runs WHERE id=? ORDER BY event_time DESC;") run-id)
    (if (not keyvals)
	'()
	(let ((prev-run-ids '()))
	  (apply sqlite3:for-each-row
		 (lambda (id)
		   (set! prev-run-ids (cons id prev-run-ids)))
		 db
		 (conc "SELECT id FROM runs WHERE " qrystr " AND id != ?;") (append keyvals (list run-id)))
	  ;; collect all matching tests for the runs then
	  ;; extract the most recent test and return that.
	  (debug:print 4 "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals 
		       ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) '()  ;; no previous runs? return null
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (db:get-tests-for-run dbstruct hed (conc test-name "/" item-path) '() '() #f #f #f #f #f #f)))
		  (debug:print 4 "Got tests for run-id " run-id ", test-name " test-name 
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
		   (debug:print-info 0 "WARNING: failed to test for existance of " dbfj)
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
		   (debug:print-info 0 "delaying db access due to high database load.")
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
	"SELECT id,item_path,state,status,run_duration,final_logf,comment FROM tests WHERE testname=? AND item_path != '';"
	test-name)
       res))))

;;======================================================================
;; Tests meta data
;;======================================================================

;; read the record given a testname
(define (db:testmeta-get-record dbstruct testname)
  (let ((res #f))
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
  (debug:print-info 6 "ITEMMAPS: " itemmaps)
  (let* ((itemmap    (tests:lookup-itemmap itemmaps test-b-name)))
    (if itemmap
	(let ((path-b-mapped (db:multi-pattern-apply path-b itemmap)))
	  (debug:print-info 6 "ITEMMAP is " itemmap ", path: " path-b ", mapped path: " path-b-mapped)
	  (equal? path-a path-b-mapped))
	(equal? path-b path-a))))

;; A routine to convert test/itempath using a itemmap
;; NOTE: to process only an itempath (i.e. no prepended testname)
;;       just call db:multi-pattern-apply
;;
(define (db:convert-test-itempath path-in itemmap)
  (debug:print-info 6 "ITEMMAP is " itemmap)
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
			      (debug:print 0 "WARNING: itemmap has problem \"" itemmap "\", patt: " patt ", repl: " repl)
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
	 (dbdat    (db:get-db dbstruct #f))
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
    (debug:print 2 "Using " tempdir " for constructing the ods file. keyqry: " keyqry " keystr: " keysstr " with keys: " (map cadr keypatt-alist)
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
					  (debug:print 4 "log: " log-fpath " exists: " (file-exists? log-fpath))
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
    (debug:print 2 "Found " (length test-ids) " records")
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
	   (debug:print 0 "WARNING: path given, " outputfile " is relative, prefixing with current directory")
	   (conc (current-directory) "/" outputfile)))
     results)
    ;; brutal clean up
    (system "rm -rf tempdir")))

;; (db:extract-ods-file db "outputfile.ods" '(("sysname" "%")("fsname" "%")("datapath" "%")) "%")

;; This is a list of all procs that write to the db
;;
;; (define *db:all-write-procs*
;;   (list 
;;    db:set-var 
;;    db:del-var
;;    db:register-run
;;    db:set-comment-for-run
;;    db:delete-run
;;    db:update-run-event_time
;;    db:lock/unlock-run 
;;    db:delete-test-step-records
;;    db:delete-test-records
;;    db:delete-tests-for-run
;;    db:delete-old-deleted-test-records
;;    db:set-tests-state-status
;;    db:test-set-state-status-by-id
;;    db:test-set-state-status-by-run-id-testname
;;    db:testmeta-add-record
;;    db:csv->test-data
;;    ))

