;;;; server.scm
(use rpc)

(rpc:publish-procedure!
 'foo
 (lambda (x)
   (print "foo: " x)
   #f))

(rpc:publish-procedure!
 'fini
 (lambda () (print "fini") (thread-start! (lambda () (thread-sleep! 3) (print "terminate") (exit))) #f))

((rpc:make-server (tcp-listen (rpc:default-server-port))) #t)

