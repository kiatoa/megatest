;;======================================================================
;; S E R V E R
;;======================================================================

(test "setup for run" #t (begin (setup-for-run)
				(string? (getenv "MT_RUN_AREA_HOME"))))

(test "server-register, get-best-server" #t (let ((res #f))
					      (open-run-close tasks:server-register tasks:open-db 1 "bob" 1234 100 'live 'http)
					      (set! res (open-run-close tasks:get-best-server tasks:open-db))
					      (number? (vector-ref res 3))))

(test "de-register server" #t (let ((res #f))
				(open-run-close tasks:server-deregister tasks:open-db "bob" port: 1234)
				(vector? (open-run-close tasks:get-best-server tasks:open-db))))

(define server-pid #f)
(test "launch server" #t (let ((pid (process-fork (lambda ()
						    ;; (daemon:ize)
						    (server:launch 'http)))))
			   (set! server-pid pid)
			   (number? pid)))

(thread-sleep! 3) ;; need to wait for server to start. Yes, a better way is needed.
(test "get-best-server" #t (let ((dat (open-run-close tasks:get-best-server tasks:open-db)))
			     (set! *runremote* (list (vector-ref dat 1)(vector-ref dat 2))) ;; host ip pullport pubport
			     (and (string? (car  *runremote*))
			     	  (number? (cadr *runremote*)))))

(test #f #t (car (cdb:login *runremote* *toppath* *my-client-signature*)))
(test #f #t (let ((res (client:login *runremote*)))
	      (car res)))


(test #f #t (cdb:client-call *runremote* 'immediate #t 1 (lambda ()(display "Got here eh!?") #t)))

;; (set! *verbosity* 20)
(test #f *verbosity* (cadr (cdb:set-verbosity *runremote* *verbosity*)))
(test #f #f (cdb:roll-up-pass-fail-counts *runremote* 1 "test1" "" "PASS"))
;; (set! *verbosity* 1)
;; (cdb:set-verbosity *runremote* *verbosity*)



(test "get-keys" "SYSTEM" (car (db:get-keys *db*)))

(define remargs (args:get-args
		 '("bar" "foo" ":runname" "bob" ":SYSTEM" "ubuntu" ":RELEASE" "v1.2" ":datapath" "blah/foo" "nada")
		 (list ":runname" ":state" ":status")
		 (list "-h")
		 args:arg-hash
		 0))

(test "register-run" #t (number?
			 (db:register-run *db*
					  '(("SYSTEM" "key1")("RELEASE" "key2"))
					  "myrun" 
					  "new"
					  "n/a" 
					  "bob")))

(test #f #t             (cdb:tests-register-test *runremote* 1 "nada" ""))
(test #f 1              (cdb:remote-run db:get-test-id #f 1 "nada" ""))
(test #f "NOT_STARTED"  (vector-ref (open-run-close db:get-test-info #f 1 "nada" "") 3))
(test #f "NOT_STARTED"  (vector-ref (cdb:get-test-info *runremote* 1 "nada" "") 3))

;;======================================================================
;; D B
;;======================================================================

