;;;; rpc-demo.scm
;;;; Simple database server / client

;;; start server thusly: ./rpctest server test.db
;;; you will need to init test.db:
;;; sqlite3 test.db "CREATE TABLE foo (id INTEGER PRIMARY KEY, var TEXT, val TEXT);"

(require-extension (srfi 18) extras tcp rpc sql-de-lite)

;;; Common things

(define total-queries 0)
(define start-time (current-seconds))

(define operation (string->symbol (car (command-line-arguments))))
(define param (cadr (command-line-arguments)))
(print "Operation: " operation ", param: " param)

;; have a pool of db's to pick from
(define *dbpool* '())
(define *pool-mutex* (make-mutex))

(define (get-db)
  (mutex-lock! *pool-mutex*)
  (if (null? *dbpool*)
      (begin
	(mutex-unlock! *pool-mutex*)
	(let ((db (open-database param)))
	  (set-busy-handler! db (busy-timeout 10000))
	  (exec (sql db "PRAGMA synchronous=0;"))
	  db))
      (let ((res (car *dbpool*)))
	(set! *dbpool* (cdr *dbpool*))
	(mutex-unlock! *pool-mutex*)
	res)))

(define (return-db db)
  (mutex-lock! *pool-mutex*)
  (set! *dbpool* (cons db *dbpool* ))
  (let ((res (length *dbpool*)))
    (mutex-unlock! *pool-mutex*)
    res))

(define rpc:listener
  (if (eq? operation 'server)
      (tcp-listen (rpc:default-server-port))
      (tcp-listen 0)))

;; Start server thread
(define rpc:server
  (make-thread
   (cute (rpc:make-server rpc:listener) "rpc:server")
   'rpc:server))

(thread-start! rpc:server)

;;; Server side

(define (server)
  (rpc:publish-procedure!
   'change-response-port
   (lambda (port)
     (rpc:default-server-port port))
   #f)
  ;;(let ((db  (get-db))(open-database param)))
  ;; (set-finalizer! db finalize!)
  (rpc:publish-procedure!
   'query
   (lambda (sqlstmt callback)
     (set! total-queries (+ total-queries 1))
     (print "Executing query '" sqlstmt "' ...")
     (let ((db (get-db)))
       (query (for-each-row
	       callback)
	      (sql db sqlstmt))
       (print "Query rate: " (/ total-queries (/ (- (current-seconds) start-time) 60)) " per minute")
       (print "num dbs: " (return-db db))
       )))
  (thread-join! rpc:server))

;;; Client side

(define (callback1 . columns)
  (let loop ((c columns) (i 0))
    (unless (null? c)
      (printf "~a=~s " i (car c))
      (loop (cdr c) (+ i 1))))
  (newline))

(define callback2-results '())

(define (callback2 . columns)
  (set! callback2-results (cons columns callback2-results)))

(define (client param)
  ((rpc:procedure 'change-response-port "localhost")
   (tcp-listener-port rpc:listener))
  ((rpc:procedure 'query "localhost") param callback1)
  (rpc:publish-procedure! 'callback2 callback2)
  ((rpc:procedure 'query "localhost") param callback2)
  (pp callback2-results)
  (rpc:close-all-connections!)
  ;; (rpc:close-connection! "localhost" (rpc:default-server-port))
  )

(define (run-query param)
  ((rpc:procedure 'query "localhost") param callback1)
  ((rpc:procedure 'query "localhost") param callback2)
  callback2-results)

(define (continuous-client #!key (duration 600)) ;; default - run for 10 minutes
  ((rpc:procedure 'change-response-port "localhost")
   (tcp-listener-port rpc:listener))
  (rpc:publish-procedure! 'callback2 callback2)
  (let loop ()
    (if (< (- (current-seconds) start-time) duration)
	(begin
	  (run-query (conc "INSERT INTO foo (var,val) VALUES (" (random 1000) "," (random 1000) ");"))
	  (let ((numrows (caaar (run-query "SELECT COUNT(id) FROM foo;"))))
	    (if (and (number? numrows)
		     (> numrows 300))
		(print (run-query (conc "DELETE FROM foo WHERE var > " (random 1000) ";")))))
	  (loop))))
  (rpc:close-all-connections!))

;;; Run it

(if (eq? operation 'server)
    (server)
    (continuous-client))

