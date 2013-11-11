;;======================================================================
;; S E R V E R
;;======================================================================

;; Run like this:
;;
;;  (cd ..;make && make install) && ./rununittest.sh server 1;(cd simplerun;megatest -stop-server 0)

(set! *transport-type* 'http)

(test "setup for run" #t (begin (setup-for-run)
				(string? (getenv "MT_RUN_AREA_HOME"))))

(test "server-register, get-best-server" #t (let ((res #f))
					      (open-run-close tasks:server-register tasks:open-db 1 "bob" 1234 100 'live 'http)
					      (set! res (open-run-close tasks:get-best-server tasks:open-db))
					      (number? (vector-ref res 3))))

(test "de-register server" #f (let ((res #f))
				(open-run-close tasks:server-deregister tasks:open-db "bob" port: 1234)
				(vector? (open-run-close tasks:get-best-server tasks:open-db))))

(define server-pid #f)

;; Not sure how the following should work, replacing it with system of megatest -server
;; (test "launch server" #t (let ((pid (process-fork (lambda ()
;; 						    ;; (daemon:ize)
;; 						    (server:launch 'http)))))
;; 			   (set! server-pid pid)
;; 			   (number? pid)))
(system "megatest -server - &")

(thread-sleep! 3) ;; need to wait for server to start. Yes, a better way is needed.
(test "get-best-server" #t (begin 
			     (client:launch)
			     (let ((dat (open-run-close tasks:get-best-server tasks:open-db)))
			       (vector? dat))))

(define *keys*               (keys:config-get-fields *configdat*))
(define *keyvals*            (keys:target->keyval *keys* "a/b/c"))

(test #f #t                       (string? (car *runremote*)))
(test #f '(#t "successful login") (rmt:login)) ;;  *runremote* *toppath* *my-client-signature*)))
(test #f #f                       (rmt:get-test-info-by-id 99)) ;; get non-existant test
(test #f 1                        (rmt:register-run  *keyvals* "firstrun" "new" "n/a" (current-user-name)))
(test "get run info"  "firstrun"  (let ((rinfo (rmt:get-run-info 1)))
				    (vector-ref (vector-ref rinfo 1) 3)))
(test "get tests (no data)" '()   (rmt:get-tests-for-run 1 "%" '() '() #f #f #f #f #f #f))
(test "register test"       #t    (rmt:general-call 'register-test 1 "test1" ""))
(test "get tests (some data)"  1  (length (rmt:get-tests-for-run 1 "%" '() '() #f #f #f #f #f #f)))

;; ;; (set! *verbosity* 20)
;; (test #f *verbosity* (cadr (cdb:set-verbosity *runremote* *verbosity*)))
;; (test #f #f (cdb:roll-up-pass-fail-counts *runremote* 1 "test1" "" "PASS"))
;; ;; (set! *verbosity* 1)
;; ;; (cdb:set-verbosity *runremote* *verbosity*)
;; 
;; 
;; 
;; (test "get-keys" "SYSTEM" (car (db:get-keys *db*)))
;; 
;; (define remargs (args:get-args
;; 		 '("bar" "foo" ":runname" "bob" ":SYSTEM" "ubuntu" ":RELEASE" "v1.2" ":datapath" "blah/foo" "nada")
;; 		 (list ":runname" ":state" ":status")
;; 		 (list "-h")
;; 		 args:arg-hash
;; 		 0))
;; 
;; (test "register-run" #t (number?
;; 			 (rmt:register-run '(("SYSTEM" "key1")("RELEASE" "key2"))
;; 					   "myrun" 
;; 					   "new"
;; 					   "n/a" 
;; 					   "bob")))
;; 
;; (test #f #t             (cdb:tests-register-test *runremote* 1 "nada" ""))
;; (test #f 1              (cdb:remote-run db:get-test-id #f 1 "nada" ""))
;; (test #f "NOT_STARTED"  (vector-ref (open-run-close db:get-test-info #f 1 "nada" "") 3))
;; (test #f "NOT_STARTED"  (vector-ref (cdb:get-test-info *runremote* 1 "nada" "") 3))

;;======================================================================
;; D B
;;======================================================================

(test #f '(#t "exit process started") (cdb:kill-server *runremote* #f)) ;; *toppath* *my-client-signature* #f)))

