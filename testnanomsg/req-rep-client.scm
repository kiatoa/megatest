;; watch nanomsg's pipeline load-balancer in action.
(use nanomsg posix regex)

(define req   (nn-socket 'req))

(nn-connect req  "tcp://localhost:22022")

;; (with-output-to-string (lambda ()(serialize obj)))
(define (client-send-receive soc msg)
  (nn-send soc msg)
  (nn-recv soc))

(define ((talk-to-server soc))
  (let loop ((cnt 200000))
    (let ((name (list-ref '("Matt" "Tom" "Bob" "Jill" "James" "Jane")(random 6))))
      ;; (print "Sending " name)
      ;; (print
      (client-send-receive req name) ;; )
      (if (> cnt 0)(loop (- cnt 1)))))
  (print (client-send-receive req "quit"))
  (nn-close req)
  (exit))

;; (thread-start! (lambda ()
;; 		 (thread-sleep! 20)
;; 		 (print "Give up on waiting for the server")
;; 		 (nn-close req)
;; 		 (exit)))

(thread-join! (thread-start! (talk-to-server req)))

