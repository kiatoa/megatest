;;;; rpc-demo.scm
;;;; Simple database server / client

;;; start server thusly: ./rpctest server test.db
;;; you will need to init test.db:
;;; sqlite3 test.db "CREATE TABLE foo (id INTEGER PRIMARY KEY, var TEXT, val TEXT);"

(require-extension (srfi 18) extras tcp rpc sqlite3)

;;; Common things

(define total-queries 0)
(define start-time (current-seconds))

(define operation (string->symbol (car (command-line-arguments))))
(define param (cadr (command-line-arguments)))
(print "Operation: " operation ", param: " param)

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
  (let ((db (open-database param)))
    (set-finalizer! db finalize!)
    (rpc:publish-procedure!
     'query
     (lambda (sql callback)
       (set! total-queries (+ total-queries 1))
       (print "Executing query '" sql "' ...")
       (for-each-row
	callback
	db sql)
       (print "Query rate: " (/ total-queries (/ (- (current-seconds) start-time) 60)) " per minute")
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

(define (client)
  ((rpc:procedure 'change-response-port "localhost")
   (tcp-listener-port rpc:listener))
  ((rpc:procedure 'query "localhost") param callback1)
  (rpc:publish-procedure! 'callback2 callback2)
  ((rpc:procedure 'query "localhost") param callback2)
  (pp callback2-results))

;;; Run it

(if (eq? operation 'server)
    (server)
    (client))

