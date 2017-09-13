;; watch nanomsg's pipeline load-balancer in action.
(use nanomsg)

;; client
(define req   (nn-socket 'req))
(nn-connect req  "inproc://test") 

(define (client-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))

;; server
(define rep   (nn-socket 'rep))
(nn-bind    rep  "inproc://test")

(define ((server soc))
  (let loop ((msg-in (nn-recv soc)))
    (if (not (equal? msg-in "quit"))
	(begin
	  (nn-send soc (conc "hello " msg-in))
	  (loop (nn-recv soc))))))

(thread-start! (server rep))

;; client 
(print (client-send-receive req "Matt"))
(print (client-send-receive req "Tom"))

;; (client-send-receive req "quit")

(nn-close req) ;; client
(nn-close rep) ;; server
(exit)
