(use zmq posix srfi-18)

(define s (make-socket 'req))
(connect-socket s "tcp://*:5563")

(define myname (cadr (argv)))

(print "Start client...")

(do ((i 0 (+ i 1)))
    ((>= i 1000))
  (print "sending message #" i)
  (send-message s (conc "Hello from " myname))
  (print "sent \"Hello\", looking for a reply")
  (printf "Received reply ~a [~a]\n"
          i (receive-message s)))
