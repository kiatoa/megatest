;; pub/sub with envelope address
;; Note that if you don't insert a sleep, the server will crash with SIGPIPE as soon
;; as a client disconnects.  Also a remaining client may receive tons of
;; messages afterward.

(use zmq srfi-18 sqlite3)

(define pub (make-socket 'pub))
(define pull (make-socket 'pull))

(bind-socket pub "tcp://*:5563")
(bind-socket pull "tcp://*:5564")

(define (open-db)
  (let* ((dbpath    "mockupserver.db")
	 (dbexists  (file-exists? dbpath))
	 (db        (open-database dbpath)) ;; (never-give-up-open-db dbpath))
	 (handler   (make-busy-timeout 10)))
    (set-busy-handler! db handler)
    (if (not dbexists)
	(for-each
	 (lambda (stmt)
	   (execute db stmt))
	 (list
	  "CREATE TABLE clients (id INTEGER PRIMARY KEY,name TEXT,num_accesses INTEGER);"
	  "CREATE TABLE vars    (var TEXT,val TEXT,CONSTRAINT vars_constraint UNIQUE (var));")))
    db))

(define db (open-db))
;; (define queuelst '())
;; (define mx1 (make-mutex))

(define (process-queue queuelst)
  (for-each
   (lambda (item)
     (let ((cname (vector-ref item 1))
	   (clcmd (vector-ref item 2))
	   (cdata (vector-ref item 3)))
       (send-message pub cname send-more: #t)
       (send-message pub (case clcmd
			   ((setval)
			    (apply execute db "INSERT OR REPLACE INTO vars (var,val) VALUES (?,?);" (string-split cdata))
			    "ok")
			   ((getval)
			    (let ((res "noval"))
			      (for-each-row
			       (lambda (val)
				 (set! res val))
			       db 
			       "SELECT val FROM vars WHERE var=?;" cdata)
			      res))
			   (else (conc "unk cmd: " clcmd))))))
   queuelst))

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
		     ;; (print "Got indat=" indat)
		     (case clcmd
		       ((sync) ;; just process the queue
			(print "Got sync from " cname)
			(process-queue queuelst)
			(loop '()))
		       ((imediate)
			(process-queue (cons svect queuelst))
			(loop '()))
		       (else
			(loop (cons svect queuelst))))))))
	     "server thread"))

(define push (make-socket 'push))
(connect-socket push "tcp://localhost:5564")

;; send a sync to the pull port
(define th2 (make-thread
	     (lambda ()
	       (let loop ()
		 (thread-sleep! 3)
		 ;; (print "Sending sync from server")
		 (send-message push "server:sync:nodat")
		 (loop)))
	     "sync thread"))

(thread-start! th1)
(thread-start! th2)
(thread-join! th1)
