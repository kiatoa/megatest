;;;; client.scm
(use rpc posix)

(define call (rpc:procedure 'foo "localhost"))

(do ((i 10 (sub1 i)))
    ((zero? i))
  (print "-> " (call (random 100))))
