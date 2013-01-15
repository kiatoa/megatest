(use spiffy awful)

(tcp-buffer-size 2048)
(enable-sxml #t)

(define (hello-world)
  (define-page (main-page-path)
    (lambda ()
      (with-request-variables (foo)
        (conc foo " Hello, world! Goodbye Dolly")))))

(define (start-server #!key (portnum 8080))
  (handle-exceptions
   exn
   (begin
     (print-error-message exn)
     (if (< portnum 9000)
	 (begin 
	   (print "WARNING: failed to start on portnum: " portnum ", trying next port")
	   (sleep 1)
	   (start-server portnum: (+ portnum 1)))
	 (print "ERROR: Tried and tried but could not start the server")))
   (print "INFO: Trying to start server on portnum: " portnum)
   (awful-start hello-world port: portnum)))

(start-server)
