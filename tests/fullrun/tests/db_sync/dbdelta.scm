
(use sql-de-lite)

(define megatest.db (conc (get-environment-variable "MT_RUN_AREA_HOME") "/megatest.db"))

(define runsquery "sysname||'/'||fsname||'/'||datapath||'/'||runname||'/'||runs.state||'-'||runs.status") 
(define bigquery
  (conc 
   "SELECT " runsquery "||testname||'/'||item_path||'-'||'-'||tests.state||'-'||tests.status||'-'||runs.id AS outdat FROM runs INNER JOIN tests ON runs.id=tests.run_id ORDER BY outdat ASC ;"))

(print "Creating file for legacy db")
(with-output-to-file "legacy-db-dump"
  (lambda ()
    (let ((db (open-database megatest.db)))
      (query (for-each-row
	      (lambda (res)
		(print res)))
	     (sql db bigquery))
      (close-database db))))

(define main.db (conc (get-environment-variable "MT_DBDIR") "/main.db"))

(print "Creating file for current db")
(with-output-to-file "current-db-dump"
  (lambda ()
    (let* ((mdb      (open-database main.db))
	   (run-ids  (query fetch-column (sql mdb (conc "select id," runsquery " AS rq from runs ORDER BY rq ASC;"))))
	   (dbdir    (get-environment-variable "MT_DBDIR")))
      (for-each
       (lambda (rid)
	 (let ((dbfile (conc dbdir "/" rid ".db")))
	   (if (file-exists? dbfile)
	       (begin
		 (exec (sql mdb (conc "ATTACH DATABASE '" dbfile "' AS testsdb;")))
		 (query (for-each-row
			 (lambda (res)
			   (print res)))
			(sql mdb bigquery))
		 (exec (sql mdb "DETACH DATABASE testsdb;")))
	       (print "ERROR: No file " dbfile " found"))))
       run-ids)
      (close-database mdb))))
	 
      