(use zmq posix)

(define cname "Bob")
(let ((args (argv)))
  (if (< (length args) 2)
      (begin
	(print "Usage: mockupclient clientname")
	(exit))
      (set! cname (cadr args))))
      
(define sub  (make-socket 'sub))
(define push (make-socket 'push))
(socket-option-set! sub 'subscribe cname)
(connect-socket sub "tcp://localhost:5563")
(connect-socket push "tcp://localhost:5564")

(define (dbaccess cmd var val)
  (let ((msg (conc cname ":" cmd ":" (if val (conc var " " val) var))))
    (print "Sending msg: " msg)
    (send-message push msg)
    (receive-message* sub)
    (receive-message* sub)))

(let loop ()
  (let ((x (random 15))
	(varname (list-ref (list "hello" "goodbye" "saluton" "kiaorana")(random 4))))
    (case x
      ((1)(dbaccess 'sync "nodat"    #f))
      ((2 3 4 5)(dbaccess 'set varname (random 999)))
      ((6 7 8 9 10)(print cname ": Get \"" varname "\" " (dbaccess 'get varname #f)))
      (else
       (thread-sleep! 0.01)))
    (loop)))


