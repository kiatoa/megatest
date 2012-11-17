(use zmq posix)

(define cname "Bob")
(let ((args (argv)))
  (if (< (length args) 2)
      (begin
	(print "Usage: mockupclient clientname")
	(exit))
      (set! cname (cadr args))))
      
(randomize)
(define start-delay (/ (random 100) 9))
(define runtime     (+ 1 (/ (random 200) 2)))

(print "client " cname " with start delay " start-delay " and runtime " runtime)
(thread-sleep! start-delay)
(print "client " cname " started")

(include "mockupclientlib.scm")

(set! endtime (+ (current-seconds) runtime))

(let loop ()
  (let ((x (random 15))
	(varname (list-ref (list "hello" "goodbye" "saluton" "kiaorana")(random 4))))
    (case x
      ((1)(dbaccess cname 'sync "nodat"    #f))
      ((2 3 4 5)(dbaccess cname 'set varname (random 999)))
      ((6 7 8 9 10)(print cname ": Get \"" varname "\" " (dbaccess cname 'get varname #f)))
      (else
       (thread-sleep! 0.1)))
    (if (< (current-seconds) endtime)
	(loop))))

(print "Client " cname " all done!!")
