(use zmq)

(define cname "Bob")
(let ((args (argv)))
  (if (< (length args) 3)
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
    (receive-message* sub)))

(let loop ()
  (case (random 5)
    ((1)(dbaccess sync "" #f))
    (else
     (thread-sleep! 1)))
  (loop))


