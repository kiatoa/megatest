(use zmq posix)

(define cname "Bob")
(define runtime 10)
(let ((args (argv)))
  (if (< (length args) 3)
      (begin
	(print "Usage: mockupclient clientname runtime")
	(exit))
      (begin
	(set! cname (cadr args))
	(set! runtime (string->number (caddr args))))))
      
;; (define start-delay (/ (random 100) 9))
;; (define runtime     (+ 1 (/ (random 200) 2)))

(print "Starting client " cname " with runtime " runtime)

(include "mockupclientlib.scm")

(set! endtime (+ (current-seconds) runtime))

;; first ping the server to ensure we have a connection
(if (server-ping cname 5)
    (print "SUCCESS: Client " cname " connected to server")
    (begin
      (print "ERROR: Client " cname " failed ping of server, exiting")
      (exit)))

(let loop ()
  (let ((x (random 15))
	(varname (list-ref (list "hello" "goodbye" "saluton" "kiaorana")(random 4))))
    (case x
      ;; ((1)(dbaccess cname 'sync "nodat"    #f))
      ((2 3 4 5)(dbaccess cname 'set varname (random 999)))
      ((6 7 8 9 10)(print cname ": Get \"" varname "\" " (dbaccess cname 'get varname #f)))
      (else
       (thread-sleep! 0.011)))
    (if (< (current-seconds) endtime)
	(loop))))

(print "Client " cname " all done!!")
