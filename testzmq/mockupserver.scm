;; pub/sub with envelope address
;; Note that if you don't insert a sleep, the server will crash with SIGPIPE as soon
;; as a client disconnects.  Also a remaining client may receive tons of
;; messages afterward.

(use zmq srfi-18 sqlite3)

(define pub (make-socket 'pub))
(define pull (make-socket 'pull))
(define cname "server")

(bind-socket pub "tcp://*:5563")
(bind-socket pull "tcp://*:5564")

(define (open-db)
  (let* ((dbpath    "mockup.db")
	 (dbexists  (file-exists? dbpath))
	 (db        (open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler   (make-busy-timeout 10)))
    (set-busy-handler! db handler)
    (if (not dbexists)
	(for-each
	 (lambda (stmt)
	   (execute db stmt))
	 (list
	  "PRAGMA SYNCHRONOUS=0;"
	  "CREATE TABLE clients (id INTEGER PRIMARY KEY,name TEXT,num_accesses INTEGER DEFAULT 0);"
	  "CREATE TABLE vars    (var TEXT,val TEXT,CONSTRAINT vars_constraint UNIQUE (var));")))
    db))

(define cid-cache (make-hash-table))

(define (get-client-id db cname)
  (let ((cid (hash-table-ref/default cid-cache cname #f)))
    (if cid 
	cid
	(begin
	  (execute db "INSERT OR REPLACE INTO clients (name) VALUES(?);" cname)
	  (for-each-row 
	   (lambda (id)
	     (set! cid id))
	   db
	   "SELECT id FROM clients WHERE name=?;" cname)
	  (hash-table-set! cid-cache cname cid)
	  cid))))

(define (count-client db cname)
  (let ((cid (get-client-id db cname)))
    (execute db "UPDATE clients SET num_accesses=num_accesses+1 WHERE id=?;" cid)))

(define db (open-db))
;; (define queuelst '())
;; (define mx1 (make-mutex))

(define (process-queue queuelst)
  (let ((queuelen (length queuelst)))
    (for-each
     (lambda (item)
       (let ((cname (vector-ref item 1))
	     (clcmd (vector-ref item 2))
	     (cdata (vector-ref item 3)))
	 (send-message pub cname send-more: #t)
	 (send-message pub (case clcmd
			     ((sync)
			      (conc queuelen))
			     ((set)
			      (apply execute db "INSERT OR REPLACE INTO vars (var,val) VALUES (?,?);" (string-split cdata))
			      "ok")
			     ((get)
			      (let ((res "noval"))
				(for-each-row
				 (lambda (val)
				   (set! res val))
				 db 
				 "SELECT val FROM vars WHERE var=?;" cdata)
				res))
			     (else (conc "unk cmd: " clcmd))))))
     queuelst)))

(define th1 (make-thread 
	     (lambda ()
	       (let ((last-run 0)) ;; current-seconds when run last
		 (let loop ((queuelst '()))
		   (let* ((indat (receive-message* pull))
			  (parts (string-split indat ":"))
			  (cname (car parts))                   ;; client name
			  (clcmd (string->symbol (cadr parts))) ;; client cmd
			  (cdata (caddr parts))                 ;; client data
			  (svect (vector (current-seconds) cname clcmd cdata))) ;; record for the queue
		     (count-client db cname)
		     (case clcmd
		       ((sync) ;; just process the queue
			(print "Got sync from " cname)
			(process-queue (cons svect queuelst))
			(loop '()))
		       ((get)
			(process-queue (cons svect queuelst))
			(loop '()))
		       (else
			(loop (cons svect queuelst))))))))
	     "server thread"))

(include "mockupclientlib.scm")

;; send a sync to the pull port
(define th2 (make-thread
	     (lambda ()
	       (let ((last-action-time (current-seconds)))
		 (let loop ()
		   (thread-sleep! 5)
		   (let ((queuelen (string->number (dbaccess "server" 'sync "nada" #f)))
			 (last-action-delta (- (current-seconds) last-action-time)))
		     (print "Server: Got queuelen=" queuelen ", last-action-delta=" last-action-delta)
		     (if (> queuelen 1)(set! last-action-time (current-seconds)))
		     (if (< last-action-delta 15)
			 (loop)
			 (print "Server exiting, 15 seconds since last access"))))))
	     "sync thread"))

(thread-start! th1)
(thread-start! th2)
(thread-join! th2)

(print "Server exited!")