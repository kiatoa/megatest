;; pub/sub with envelope address
;; Note that if you don't insert a sleep, the server will crash with SIGPIPE as soon
;; as a client disconnects.  Also a remaining client may receive tons of
;; messages afterward.

(use zmq srfi-18 sqlite3 numbers)

(define pub (make-socket 'pub))
(define pull (make-socket 'pull))
(define cname "server")
(define total-db-accesses 0)
(define start-time (current-seconds))

(socket-option-set! pub 'hwm 1000)
(socket-option-set! pull 'hwm 1000)

(bind-socket pub "tcp://*:6563")
(bind-socket pull "tcp://*:6564")

(thread-sleep! 0.2)

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
	  (set! total-db-accesses (+ total-db-accesses 2))
	  cid))))

(define (count-client db cname)
  (let ((cid (get-client-id db cname)))
    (execute db "UPDATE clients SET num_accesses=num_accesses+1 WHERE id=?;" cid)
    (set! total-db-accesses (+ total-db-accesses 1))
    ))

(define db (open-db))
;; (define queuelst '())
;; (define mx1 (make-mutex))

(define max-queue-len 0)

(define (process-queue queuelst)
  (let ((queuelen (length queuelst)))
    (if (> queuelen max-queue-len)
	(set! max-queue-len queuelen))
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
			      (set! total-db-accesses (+ total-db-accesses 1))
			      (apply execute db "INSERT OR REPLACE INTO vars (var,val) VALUES (?,?);" (string-split cdata))
			      "ok")
			     ((get)
			      (set! total-db-accesses (+ total-db-accesses 1))
			      (let ((res "noval"))
				(for-each-row
				 (lambda (val)
				   (set! res val))
				 db 
				 "SELECT val FROM vars WHERE var=?;" cdata)
				res))
			     (else (conc "unk cmd: " clcmd))))))
     queuelst)))

;; SERVER THREAD
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
		     ;; (print "Server received message: " indat)
		     (count-client db cname)
		     (case clcmd
		       ((ping)
			(print "Got ping from " cname)
			(send-message pub cname send-more: #t)
			(send-message pub "Got ping")
			(loop queuelst))
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

;; SYNC THREAD
;; send a sync to the pull port
(define th2 (make-thread
	     (lambda ()
	       (let ((last-action-time (current-seconds)))
		 (let loop ()
		   (thread-sleep! 5)
		   (let ((queuelen (string->number (dbaccess "server" 'sync "nada" #f)))
			 (last-action-delta #f))
		     (if (> queuelen 1)(set! last-action-time (current-seconds)))
		     (set! last-action-delta (- (current-seconds) last-action-time))
		     (print "Server: Got queuelen=" queuelen ", last-action-delta=" last-action-delta)
		     (if (< last-action-delta 60)
			 (loop)
			 (print "Server exiting, 25 seconds since last access"))))))
	     "sync thread"))

(thread-start! th1)
(thread-start! th2)
(thread-join! th2)

(let* ((run-time       (- (current-seconds) start-time))
       (queries/second (/  total-db-accesses run-time)))
  (print "Server exited! Total db accesses=" total-db-accesses " in " run-time " seconds for " queries/second " queries/second with max queue length of: " max-queue-len))
