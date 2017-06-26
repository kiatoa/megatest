
(use (prefix sqlite3 sqlite3:) posix typed-records) 

(define (open-tail-db )
  (let* ((basedir   (create-directory (conc "/tmp/" (current-user-name))))
	 (dbpath    (conc basedir "/megatest_logs.db"))
	 (dbexists  (common:file-exists? dbpath))
	 (db        (sqlite3:open-database dbpath))
	 (handler   (sqlite3:make-busy-timeout 136000)))
    (sqlite3:set-busy-handler! db handler)
    (sqlite3:execute db "PRAGMA synchronous = 0;")
    (if (not dbexists)
	(begin
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS log_files (id INTEGER PRIMARY KEY,filename TEXT,event_time TIMESTAMP DEFAULT (strftime('%s','now')));")
	  (sqlite3:execute db "CREATE TABLE IF NOT EXISTS log_data  (id INTEGER PRIMARY KEY,fid INTEGER,line TEXT,event_time TIMESTAMP DEFAULT (strftime('%s','now')));")
	  ))
    db))

(define (tail-write db fid lines)
  (sqlite3:with-transaction
   db
   (lambda ()
     (for-each
      (lambda (line)
	(sqlite3:execute db "INSERT INTO log_data (fid,line) VALUES (?,?);" fid line))
      lines))))

(define (tail-get-fid db fname)
  (let ((fid   (handle-exceptions
		   exn
		   #f
		 (sqlite3:first-result db "SELECT id FROM log_files WHERE filename=?;" fname))))
    (if fid
	fid
	(begin
	  (sqlite3:execute db "INSERT INTO log_files (filename) VALUES (?);" fname)
	  (tail-get-fid db fname)))))

(define (file-tail fname #!key (db-in #f))
  (let* ((inp (open-input-file fname))
	 (db  (or db-in (open-tail-db)))
	 (fid (tail-get-fid db fname)))
    (let loop ((inl    (read-line inp))
	       (lines '())
	       (lastwr (current-seconds)))
      (if (eof-object? inl)
	  (let ((timed-out (> (- (current-seconds) lastwr) 60)))
	    (if timed-out (tail-write db fid (reverse lines)))
	    (sleep 1)
	    (if timed-out
		(loop (read-line inp) '() (current-seconds))
		(loop (read-line inp) lines lastwr)))
	  (let* ((savelines (> (length lines) 19)))
	    ;; (print inl)
	    (if savelines (tail-write db fid (reverse lines)))
	    (loop (read-line inp)
		  (if savelines
		      '()
		      (cons inl lines))
		  (if savelines
		      (current-seconds)
		      lastwr)))))))

;; offset -20 means get last 20 lines
;;
(define (tail-get-lines db fid offset count)
  (if (> offset 0)
      (map-row (lambda (id line)
		 (vector id line))
	       db
	       "SELECT id,line FROM log_data WHERE fid=? OFFSET ? LIMIT ?;" fid offset count)
      (reverse ;; get N from the end
       (map-row (lambda (id line)
		  (vector id line))
		db
		"SELECT id,line FROM log_data WHERE fid=? ORDER BY id DESC LIMIT ?;" fid (abs offset)))))
