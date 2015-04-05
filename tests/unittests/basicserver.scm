;;======================================================================
;; S E R V E R
;;======================================================================

;; Run like this:
;;
;;  ./rununittest.sh server 1;(cd simplerun;megatest -stop-server 0)

(delete-file* "logs/1.log")
(define run-id 1)

(test "setup for run" #t (begin (launch:setup-for-run *area-dat*)
 				(string? (getenv "MT_RUN_AREA_HOME"))))

;; NON Server tests go here

(test #f #f (db:dbdat-get-path *db* *area-dat*))
(test #f #f (db:get-run-name-from-id *db* run-id))
;; (test #f '("SYSTEM" "RELEASE") (rmt:get-keys))

;; (exit)

;; Server tests go here 
(for-each
 (lambda (run-id)
   (test #f #f (tasks:server-running-or-starting? (db:delay-if-busy (tasks:open-db)) run-id))
   (server:kind-run run-id)
   (test "did server start within 20 seconds?"
	 #t
	 (let loop ((remtries 20)
		    (running (tasks:server-running-or-starting? (db:delay-if-busy
								 (tasks:open-db))
								run-id)))
	   (if running 
	       (> running 0)
	       (if (> remtries 0)
		   (begin
		     (thread-sleep! 1)
		     (loop (- remtries 1)
			   (tasks:server-running-or-starting? (db:delay-if-busy
							       (tasks:open-db))
							      run-id)))))))
   
   (test "did server become available" #t
	 (let loop ((remtries 10)
		    (res      (tasks:get-server (db:delay-if-busy (tasks:open-db)) run-id)))
	   (if res
	       (vector? res)
	       (begin
		 (if (> remtries 0)
		     (begin
		       (thread-sleep! 1.1)
		       (loop (- remtries 1)(tasks:get-server (db:delay-if-busy (tasks:open-db)) run-id)))
		     res)))))
   )
 (list 0 1))

(define user    (current-user-name))
(define runname "mytestrun")
(define keys    (rmt:get-keys))
(define runinfo #f)
(define keyvals '(("SYSTEM" "abc")("RELEASE" "def")))
(define header  (list "SYSTEM" "RELEASE" "id" "runname" "state" "status" "owner" "event_time"))

;; Setup
;;
(test #f #f  (not (client:setup run-id)))
(test #f #f  (not (hash-table-ref/default *runremote* run-id #f)))

;; Login
;;
(test #f'(#t "successful login")
      (rmt:login-no-auto-client-setup (hash-table-ref/default *runremote* run-id #f) run-id))
(test #f '(#t "successful login")
      (rmt:login run-id))

;; Keys
;;
(test #f '("SYSTEM" "RELEASE")  (rmt:get-keys))

;; No data in db
;;
(test #f '() (rmt:get-all-run-ids))
(test #f #f  (rmt:get-run-name-from-id run-id))
(test #f 
      (vector
       header
       (vector #f #f #f #f))
      (rmt:get-run-info run-id))

;; Insert data into db
;;
(test #f 1 (rmt:register-run keyvals runname "new" "n/a" user))
;; (test #f #f (rmt:get-runs-by-patt keys runname))
(test #f #t (rmt:general-call 'register-test run-id run-id "test-one" ""))
(define test-one-id #f)
(test #f 30001  (let ((test-id (rmt:get-test-id run-id "test-one" "")))
	      (set! test-one-id test-id)
	      test-id))
(define test-one-rec #f)
(test #f "test-one" (let ((test-rec (rmt:get-test-info-by-id run-id test-one-id)))
		      (set! test-one-rec test-rec)
		      (vector-ref test-rec 2)))

;; With data in db
;;
(print "Using runame=" runname)
(test #f '(1)    (rmt:get-all-run-ids))
(test #f runname (rmt:get-run-name-from-id run-id))
(test #f 
      runname
      (let ((run-info (rmt:get-run-info run-id)))
	(db:get-value-by-header (db:get-rows run-info)
				(db:get-header run-info)
				"runname")))

(for-each (lambda (run-id)
;; test killing server
;;
(tasks:kill-server-run-id run-id)

(test #f #f (tasks:server-running-or-starting? (db:delay-if-busy (tasks:open-db)) run-id))
)
(list 0 1))

;; Tests to assess reading/writing while servers are starting/stopping
(define start-time (current-seconds))
(let loop ((test-state 'start))
  (let* ((server-dats (tasks:get-server-records (db:delay-if-busy (tasks:open-db)) run-id))
	 (first-dat   (if (not (null? server-dats))
			  (car server-dats)
			  #f)))
    (map (lambda (dat)
	   (apply print (intersperse (vector->list dat) ", ")))
	 server-dats)
    (test #f test-one-rec (rmt:get-test-info-by-id run-id test-one-id))
    (thread-sleep! 1)
    (case test-state
      ((start)
       (print "Trying to start server")
       (server:kind-run run-id)
       (loop 'server-started))
      ((server-started)
       (case (if first-dat (vector-ref first-dat 0) 'blah)
	 ((running)
	  (print "Server appears to be running. Now ask it to shutdown")
	  (rmt:kill-server run-id)
	  (loop 'server-shutdown))
	 ((shutting-down)
	  (loop test-state))
	 (else (print "Don't know what to do if get here"))))
      ((server-shutdown)
       (loop test-state)))))

;;======================================================================
;; END OF TESTS
;;======================================================================


;; (test #f #f (client:setup run-id))

;; (set! *transport-type* 'http)
;; 
;; (test "setup for run" #t (begin (launch:setup-for-run)
;; 				(string? (getenv "MT_RUN_AREA_HOME"))))
;; 
;; (test "server-register, get-best-server" #t (let ((res #f))
;; 					      (open-run-close tasks:server-register tasks:open-db 1 "bob" 1234 100 'live 'http)
;; 					      (set! res (open-run-close tasks:get-best-server tasks:open-db))
;; 					      (number? (vector-ref res 3))))
;; 
;; (test "de-register server" #f (let ((res #f))
;; 				(open-run-close tasks:server-deregister tasks:open-db "bob" port: 1234)
;; 				(vector? (open-run-close tasks:get-best-server tasks:open-db))))
;; 
;; (define server-pid #f)
;; 
;; ;; Not sure how the following should work, replacing it with system of megatest -server
;; ;; (test "launch server" #t (let ((pid (process-fork (lambda ()
;; ;; 						    ;; (daemon:ize)
;; ;; 						    (server:launch 'http)))))
;; ;; 			   (set! server-pid pid)
;; ;; 			   (number? pid)))
;; (system "../../bin/megatest -server - -debug 22 > server.log 2> server.log &")
;; 
;; (let loop ((n 10))
;;   (thread-sleep! 1) ;; need to wait for server to start.
;;   (let ((res (open-run-close tasks:get-best-server tasks:open-db)))
;;     (print "tasks:get-best-server returned " res)
;;     (if (and (not res)
;; 	     (> n 0))
;; 	(loop (- n 1)))))
;; 
;; (test "get-best-server" #t (begin 
;; 			     (client:launch)
;; 			     (let ((dat (open-run-close tasks:get-best-server tasks:open-db)))
;; 			       (vector? dat))))
;; 
;; (define *keys*               (keys:config-get-fields *configdat*))
;; (define *keyvals*            (keys:target->keyval *keys* "a/b/c"))
;; 
;; (test #f #t                       (string? (car *runremote*)))
;; (test #f '(#t "successful login") (rmt:login)) ;;  *runremote* *toppath* *my-client-signature*)))
;; 
;; (test #f #f                       (rmt:get-test-info-by-id 99)) ;; get non-existant test
;; 
;; ;; RUNS
;; (test #f 1                        (rmt:register-run  *keyvals* "firstrun" "new" "n/a" (current-user-name)))
;; (test "get run info"  "firstrun"  (let ((rinfo (rmt:get-run-info 1)))
;; 				    (vector-ref (vector-ref rinfo 1) 3)))
;; (test "get runname from id" "firstrun" (rmt:get-run-name-from-id 1))
;; 
;; ;; TESTS
;; (test "get tests (no data)" '()   (rmt:get-tests-for-run 1 "%" '() '() #f #f #f #f #f #f))
;; (test "register test"       #t    (rmt:general-call 'register-test 1 "test1" ""))
;; (test "get tests (some data)"  1  (length (rmt:get-tests-for-run 1 "%" '() '() #f #f #f #f #f #f)))
;; (test "get test id"            1  (rmt:get-test-id 1 "test1" ""))
;; (test "sync back"              #t (> (rmt:sync-inmem->db) 0))
;; (test "get test id from main"  1  (db:get-test-id *db* 1 "test1" ""))
;; (test "get keys"               #t (list? (rmt:get-keys)))
;; (test "set comment"            #t (begin (rmt:general-call 'set-test-comment "this is a comment" 1) #t))
;; (test "get comment" "this is a comment" (let ((trec (rmt:get-test-info-by-id 1)))
;; 					  (db:test-get-comment trec)))
;; 
;; ;; MORE RUNS
;; (test "get runs"  #t (let* ((runs   (rmt:get-runs "%" #f #f '()))
;; 			    (header (vector-ref runs 0))
;; 			    (data   (vector-ref runs 1)))
;; 		       (and (list?   header)
;; 			    (list?   data)
;; 			    (vector? (car data)))))
;; 
;; (test "get local testinfo" "test1" (vector-ref (db:get-testinfo-state-status *db* 1) 2))
;; (test "get testinfo"       "test1" (vector-ref (rmt:get-testinfo-state-status 1) 2))
;; 
;; ;;======================================================================
;; ;; D B
;; ;;======================================================================
;; 
;; (test "pass fail counts" #t (rmt:general-call 'pass-fail-counts 10 9 1))
;; (test "get pass fail counts" 19 (let ((dat (rmt:get-test-info-by-id 1)))
;; 				  (+ (db:test-get-pass_count dat)
;; 				     (db:test-get-fail_count dat))))
;; 
;; (define testregistry (make-hash-table))
;; (for-each
;;  (lambda (tname)
;;    (for-each
;;     (lambda (itempath)
;;       (let ((tkey  (conc tname "/" itempath))
;; 	    (rpass (random 10))
;; 	    (rfail (random 10)))
;; 	(hash-table-set! testregistry tkey (list tname itempath))
;; 	(rmt:general-call 'register-test 1 tname itempath)
;; 	(let* ((tid  (rmt:get-test-id 1 tname itempath))
;; 	       (tdat (rmt:get-test-info-by-id tid)))
;; 	  (rmt:general-call 'pass-fail-counts rpass rfail (db:test-get-id tdat))
;; 	  (let* ((resdat (rmt:get-test-info-by-id tid)))
;; 	    (test "set/get pass fail counts" (list rpass rfail)
;; 		  (list (db:test-get-pass_count resdat)
;; 			(db:test-get-fail_count resdat)))))))
;;     (list "" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")))
;;  (list "test1" "test2" "test3" "test4" "test5"))
;; 
;; 
;; (test #f '(#t "exit process started") (rmt:kill-server)) ;; *toppath* *my-client-signature* #f)))
;; 

(exit)
