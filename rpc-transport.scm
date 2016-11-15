
;; Copyright 2006-2016, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.

(require-extension (srfi 18) extras tcp s11n rpc)
(import (prefix rpc rpc:))

(use sqlite3 srfi-1 posix regex regex-case srfi-69 hostinfo md5 message-digest)
(import (prefix sqlite3 sqlite3:))

(declare (unit rpc-transport))

(declare (uses common))
(declare (uses db))
(declare (uses tests))
(declare (uses tasks)) ;; tasks are where stuff is maintained about what is running.

(include "common_records.scm")
(include "db_records.scm")

(define *heartbeat-mutex* (make-mutex))
(define *server-loop-heart-beat* (current-seconds))


;; procstr is the name of the procedure to be called as a string
(define (rpc-transport:autoremote procstr params)  ;; may be unused, I think api-exec deprecates this one.
  (let* ((procsym (if (symbol? procstr)
                     procstr
                     (string->symbol (->string procstr))))
        (res
         (begin
           (apply (eval procsym) params))))
    res))


;; rpc receiver
(define (rpc-transport:api-exec cmd params)
  (let* ( (resdat  (api:execute-requests *inmemdb* (vector cmd params))) ;; #( flag result )
          (flag    (vector-ref resdat 0))
          (res     (vector-ref resdat 1)))

    (mutex-lock! *heartbeat-mutex*)

    (set! *last-db-access* (current-seconds)) ;; bump *last-db-access*; this will renew keep-running thread's lease on life for another (server:get-timeout) seconds
    (BB> "in api-exec; last-db-access updated to "*last-db-access*)
    (mutex-unlock! *heartbeat-mutex*)

    res))


  ;; (handle-exceptions
  ;;  exn
  ;;  (begin
  ;;    (debug:print 0 *default-log-port* "Remote failed for " proc " " params " exn="exn)
  ;;    (apply (eval (string->symbol procstr)) params))
  ;;  ;; (if *runremote*
  ;;  ;;    (apply (eval (string->symbol (conc "remote:" procstr))) params)
  ;;  (apply (eval (string->symbol procstr)) params)))

;; retry an operation (depends on srfi-18)
;; ==================
;; idea here is to avoid spending time on coding retrying something.  Trying to be generic here.
;;
;; Exception handling:
;; -------------------
;; if evaluating the thunk results in exception, it will be retried.
;; on last try, if final-failure-returns-actual is true, the exception will be re-thrown to caller.
;;
;; look at options below #!key to see how to configure behavior
;;
;;
(define (retry-thunk
         the-thunk
         #!key ;;;; options below
         (accept-result?   (lambda (x) x)) ;; retry if predicate applied to thunk's result is false 
         (retries                       4) ;; how many tries
         (failure-value                #f) ;; return this on final failure, unless following option is enabled:
         (final-failure-returns-actual #f) ;; on failure, on the last try, just return the result, not failure-value

         (retry-delay                 0.1) ;; delay between tries
         (back-off-factor               1) ;; multiply retry-delay by this factor on retry
         (random-delay                0.1) ;; add a random portion of this value to wait

         (chatty                       #f) ;; print status as we go, for debugging.
         )
  
  (when chatty (print) (print "Entered retry-thunk") (print "-=-=-=-=-=-"))
  (let* ((guarded-thunk ;; we are guarding the thunk against exceptions.  We will record whether result of evaluation is an exception or a regular result.
          (lambda ()
           (let* ((EXCEPTION (gensym)) ;; using gensym to avoid potential collision
                  (res
                   (condition-case
                    (the-thunk) ;; this is what we are guarding the execution of
                    [x () (cons EXCEPTION x)]
                    )))
             (cond
              ((and (pair? res) (eq? (car res) EXCEPTION))
               (if chatty
                   (print " - the-thunk threw exception >"(cdr res)"<"))
               (cons 'exception (cdr res)))
               (else
                (if chatty
                    (print " - the-thunk returned result >"res"<"))
                (cons 'regular-result res)))))))
    
    (let loop ((guarded-res (guarded-thunk))
               (retries-left retries)
               (fail-wait retry-delay))
      (if chatty (print "   =========="))
      (let* ((wait-time (+ fail-wait (+ (* fail-wait back-off-factor)
                                        (* random-delay
                                           (/ (random 1024) 1024) ))))
             (res-type (car guarded-res))
             (res-value (cdr guarded-res)))
        (cond
         ((and (eq? res-type 'regular-result) (accept-result? res-value))
                   (if chatty (print " + return result that satisfied accept-result? >"res-value"<"))
                   res-value)

         ((> retries-left 0)
          (if chatty (print " - sleep "wait-time))
          (thread-sleep! wait-time)
          (if chatty (print " + retry ["retries-left" tries left]"))
          (loop (guarded-thunk)
                (sub1 retries-left)
                wait-time))
         
         ((eq? res-type 'regular-result)
          (if final-failure-returns-actual
              (begin
                (if chatty (print " + last try failed- return the result >"res-value"<"))
                res-value)
              (begin
                (if chatty (print " + last try failed- return canned failure value >"failure-value"<"))
              failure-value)))
         
         (else ;; no retries left; result was not accepted and res-type can only be 'exception
          (if final-failure-returns-actual 
              (begin
                (if chatty (print " + last try failed with exception- re-throw it >"res-value"<"))
                (abort res-value)); re-raise the exception. TODO: find a way for call-history to show as though from entry to this function
              (begin
                (if chatty (print " + last try failed with exception- return canned failure value >"failure-value"<"))
                failure-value))))))))


(define (rpc-transport:server-shutdown server-id rpc:listener #!key (from-on-exit #f))
  (on-exit (lambda () #t)) ;; turn off on-exit stuff
  ;;(tcp-close rpc:listener) ;; gotta exit nicely
  ;;(tasks:bb-server-set-state! server-id "stopped")


  ;; TODO: (low) the following is extraordinaritly slow.  Maybe we don't even need portlogger for rpc anyway??  the exception-based failover when ports are taken is fast!
  ;;(portlogger:open-run-close portlogger:set-port (rpc:default-server-port) "released")
  
  (set! *time-to-exit* #t)
  (if *inmemdb* (db:sync-touched *inmemdb* *run-id* force-sync: #t))
  (tasks:bb-server-delete-record server-id " rpc-transport:keep-running complete")
  (BB> "Before (exit) (from-on-exit="from-on-exit")")
  (unless from-on-exit (exit))  ;; sometimes we hang (around) here with 100% cpu.
  (BB> "After")
  ;; strace reveals endless:
  ;; getrusage(RUSAGE_SELF, {ru_utime={413, 917868}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 9874}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 13874}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 105880}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 109880}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 201886}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 205886}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 297892}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 301892}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 393898}, ru_stime={0, 60003}, ...}) = 0
  ;; getrusage(RUSAGE_SELF, {ru_utime={414, 397898}, ru_stime={0, 60003}, ...}) = 0
  ;; make a post to chicken-users w/ http://paste.call-cc.org/paste?id=60a4b66a29ccf7d11359ea866db642c970735978
  (if from-on-exit
      ;; avoid above condition!  End current process externally  since 1 in 20 (exit)'s result in hung, 100% cpu zombies. (see above)
      (system (conc  "kill -9  "(current-process-id))))
  )


;; all routes though here end in exit ...
;;
;; start_server? 
;;
(define (rpc-transport:launch run-id)
  (set! *run-id*   run-id)

  ;; ;; send to background if requested
  ;; (when (args:get-arg "-daemonize")
  ;;     (daemon:ize)
  ;;     (when *alt-log-file* ;; we should re-connect to this port, I think daemon:ize disrupts it
  ;;       (current-error-port *alt-log-file*)
  ;;       (current-output-port *alt-log-file*)))

  ;; double check we dont alrady have a running server for this run-id
  (when (server:check-if-running run-id)
    (debug:print 0 *default-log-port* "INFO: Server for run-id " run-id " already running")
    (exit 0))

  ;; let's get a server-id for this server
  ;;   if at first we do not suceed, try 3 more times.
  (let ((server-id (retry-thunk
                    (lambda () (tasks:bb-server-lock-slot run-id 'rpc))
                    chatty: #f
                    retries: 4)))
    (when (not server-id) ;; dang we couldn't get a server-id.
      ;; since we didn't get the server lock we are going to clean up and bail out
      (debug:print-info 2 *default-log-port* "INFO: server pid=" (current-process-id) ", hostname=" (get-host-name) " not starting due to other candidates ahead in start queue")
      (tasks:bb-server-delete-records-for-this-pid " rpc-transport:launch")
      (exit 1))

    ;; we got a server-id (and a corresponding entry in servers table in globally shared mdb)
    ;; all systems go.  Proceed to setup rpc server.  
    (rpc-transport:run
     (if (args:get-arg "-server")
         (args:get-arg "-server")
         "-")
     run-id
     server-id)
    (exit)))

(define *rpc-listener-port* #f)
(define *rpc-listener-port-bind-timestamp* #f)

(define *on-exit-flag #f)

(define (rpc-transport:server-dat-get-iface         vec)    (vector-ref  vec 0))
(define (rpc-transport:server-dat-get-port          vec)    (vector-ref  vec 1))
(define (rpc-transport:server-dat-get-last-access   vec)    (vector-ref  vec 5))
(define (rpc-transport:server-dat-get-transport     vec)    (vector-ref  vec 6))
(define (rpc-transport:server-dat-update-last-access vec)
  (if (vector? vec)
      (vector-set! vec 5 (current-seconds))
      (begin
	(print-call-chain (current-error-port))
	(debug:print-error 0 *default-log-port* "call to rpc-transport:server-dat-update-last-access with non-vector!!"))))


(define *api-exec-ht* (make-hash-table))

;; let's see if caching the rpc stub curbs thread-profusion on server side
(define (rpc-transport:get-api-exec iface port)
  (let* ((lu (hash-table-ref/default *api-exec-ht* '(iface . port) #f)))
    (if lu
        lu
        (let ((res (rpc:procedure 'api-exec iface port)))
          (hash-table-set! *api-exec-ht* '(iface . port) res)
          res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this client-side procedure makes rpc call to server and returns result
;;
(define (rpc-transport:client-api-send-receive run-id serverdat cmd params #!key (numretries 3))
  (if (not (vector? serverdat))
      (begin
        (BB> "WHAT?? for run-id="run-id", serverdat="serverdat)
        (print-call-chain)
        (exit 1)))
  (let* ((iface (rpc-transport:server-dat-get-iface serverdat))
         (port  (rpc-transport:server-dat-get-port serverdat))
         (res #f)
         (api-exec (rpc-transport:get-api-exec iface port))  
         (send-receive (lambda ()
                         (tcp-buffer-size 0)
                         (set! res (retry-thunk
                                    (lambda ()
                                      (condition-case
                                       ;;(vector #t (run-remote cmd params))
                                       (vector 'success (api-exec cmd params))
                                       [x (exn i/o net) (vector 'comms-fail (conc "communications fail ["(->string x)"]") x)]
                                       [x () (vector 'other-fail "other fail ["(->string x)"]" x)]))
                                    chatty: #f
                                    accept-result?: (lambda(x)
                                                      (and (vector? x) (vector-ref x 0)))
                                    retries: 4
                                    back-off-factor: 1.5
                                    random-wait: 0.2
                                    retry-delay: 0.1
                                    final-failure-returns-actual: #t))
                         res
                         ))
         (th1 (make-thread send-receive "send-receive"))
         (time-out-reached #f)
         (time-out     (lambda ()
			      (thread-sleep! 45)
                              (set! time-out-reached #t)
                              (thread-terminate! th1)
			      #f))

         (th2 (make-thread time-out     "time out")))
	 (thread-start! th1)
	 (thread-start! th2)
	 (thread-join! th1)
	 (thread-terminate! th2)
	 (debug:print-info 11 *default-log-port* "got res=" res)
	 (if (vector? res)
             (case (vector-ref res 0)
               ((success) (vector #t (vector-ref res 1)))
               ((comms-fail)
                (debug:print 0 *default-log-port* "WARNING: comms failure for rpc request")
                ;;(debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
                (vector #f (vector-ref res 1)))
               (else
                (debug:print-error 0 *default-log-port* "error occured at server, info=" (vector-ref res 1))
                (debug:print 0 *default-log-port* " client call chain:")
                (print-call-chain (current-error-port))
                (debug:print 0 *default-log-port* " server call chain:")
                (pp (vector-ref res 1) (current-error-port))
                (signal (vector-ref res 2))))
             (signal (make-composite-condition
                      (make-property-condition 
             	       'timeout
             	       'message "nmsg-transport:client-api-send-receive-raw timed out talking to server"))))))
        


(define (rpc-transport:run hostn run-id server-id)
  (debug:print 2 *default-log-port* "Attempting to start the rpc server ...")
   ;; (trace rpc:publish-procedure!)

  ;;======================================================================
  ;;	  start of publish-procedure section
  ;;======================================================================
  (rpc:publish-procedure! 'server:login server:login) ;; this allows client to validate it is the same megatest instance as the server.  No security here, just making sure we're in the right room.
  (rpc:publish-procedure!
   'testing
   (lambda ()
     "Just testing"))

  ;; procedure to receive arbitrary API request from client's rpc:send-receive/rpc-transport:client-api-send-receive 
  (rpc:publish-procedure! 'rpc-transport:autoremote rpc-transport:autoremote)
  ;; can use this to run most anything at the remote
  (rpc:publish-procedure! 'api-exec rpc-transport:api-exec)
  
  
  ;;======================================================================
  ;;	  end of publish-procedure section
  ;;======================================================================


  (let* ((db              #f)
	 (hostname        (let ((res (get-host-name)))  res))
         (server-start-time (current-seconds))
         (server-timeout (server:get-timeout))
	 (ipaddrstr       (let* ((ipstr (if (string=? "-" hostn)
					   ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
					   (server:get-best-guess-address hostname)
					   #f))
                                 (res (if ipstr ipstr hostn)))
                            res)) ;; hostname))) 
	 (start-port      (let ((res (portlogger:open-run-close portlogger:find-port)))      ;; BB> TODO: remove portlogger!
                            res))
	 (link-tree-path  (configf:lookup *configdat* "setup" "linktree"))

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;; rpc:listener is the tcp-listen result from inside the find-free-port-and-open complex.
         ;;   It is our handle on the listening tcp port
         ;;   We will attach this to our rpc server with rpc:make-server in thread th1 .
	 (rpc:listener    (rpc-transport:find-free-port-and-open start-port)) 
	 (th1             (make-thread
			   (lambda ()
			     ((rpc:make-server rpc:listener) #t) )
			   "rpc:server"))


         (hostname        (if (string=? "-" hostn)
			      (get-host-name) 
			      hostn))
	 (ipaddrstr       (if (string=? "-" hostn)
			      (server:get-best-guess-address hostname) ;; (string-intersperse (map number->string (u8vector->list (hostname->ip hostname))) ".")
			      #f))
	 (portnum         (let ((res (rpc:default-server-port)))  res))
	 (host:port       (conc (if ipaddrstr ipaddrstr hostname) ":" portnum)))


    ;; BB> TODO: remove portlogger!
    ;; if rpc found it needed a different port than portlogger provided, keep portlogger in the loop.
    ;; (when (not (equal? start-port portnum))
    ;;   (BB> "portlogger proffered "start-port" but rpc grabbed "portnum)
    ;;   (portlogger:open-run-close portlogger:set-port start-port "released")
    ;;   (portlogger:open-run-close portlogger:take-port portnum))

    (tasks:bb-server-set-interface-port server-id ipaddrstr portnum)

    ;;============================================================
    ;;  activate thread th1 to attach opened tcp port to rpc server
    ;;=============================================================
    (thread-start! th1)
    (set! db *inmemdb*)

    (debug:print 0 *default-log-port* "Server started on " host:port)
    

    (thread-sleep! 5)
    (if (rpc-transport:self-test run-id ipaddrstr portnum)
        (debug:print 0 *default-log-port* "INFO: rpc self test passed!")
        (begin
          (debug:print 0 *default-log-port* "Error: rpc listener did not pass self test.  Shutting down.  On: " host:port)
          (exit)))
    
    (on-exit (lambda ()
               (rpc-transport:server-shutdown server-id rpc:listener from-on-exit: #t)))
    
    ;; check again for running servers for this run-id in case one has snuck in since we checked last in rpc-transport:launch
    (if (not (equal? server-id (tasks:bb-server-am-i-the-server? run-id)));; try to ensure no double registering of servers
        (begin ;; i am not the server, another server snuck in and beat this one to the punch
          (tcp-close rpc:listener) ;; gotta exit nicely and free up that tcp port
          (tasks:bb-server-set-state! server-id "collision"))

        (begin ;; i am the server
          ;; setup the in-memory db
          (set! *inmemdb*  (db:setup run-id))
          (db:get-db *inmemdb* run-id)

          ;; let's make it official
          (set! *rpc:listener* rpc:listener) 
          (tasks:bb-server-set-state! server-id "running") ;; update our mdb servers entry

          
          
          ;; this let loop will hold open this thread until we want the server to shut down.
          ;;   if no requests received within the last 20 seconds :
          ;;   database hasnt changed in ??
          ;;

          ;; begin new loop
          ;; keep-running loop: polls last-db-access to see if we have timed out.  
          (let loop ((count          0)
                     (bad-sync-count 0))

            ;; Use this opportunity to sync the inmemdb to db
            (let ((start-time (current-milliseconds))
                  (sync-time  #f)
                  (rem-time   #f))
              ;; inmemdb is a dbstruct
              (condition-case
               (db:sync-touched *inmemdb* *run-id* force-sync: #t)
               ((sync-failed)(cond
                              ((> bad-sync-count 10) ;; time to give up
                               (rpc-transport:server-shutdown server-id rpc:listener))
                              (else ;; (> bad-sync-count 0)  ;; we've had a fail or two, delay and loop
                               (thread-sleep! 5)
                               (loop count (+ bad-sync-count 1)))))
               ((exn)
                (debug:print-error 0 *default-log-port* "error from sync code other than 'sync-failed. Attempting to gracefully shutdown the server")
                (rpc-transport:server-shutdown server-id rpc:listener)))
              (set! sync-time  (- (current-milliseconds) start-time))
              (set! rem-time (quotient (- 4000 sync-time) 1000))
              (debug:print 4 *default-log-port* "SYNC: time= " sync-time ", rem-time=" rem-time)
              
              (if (and (<= rem-time 4)
                       (> rem-time 0))
                  (thread-sleep! rem-time)
                  (thread-sleep! 4))) ;; fallback for if the math is changed ...
            
            (if (< count 1) ;; 3x3 = 9 secs aprox
                (loop (+ count 1) bad-sync-count))

            ;; BB: don't see how this is possible with RPC
            ;; ;; Check that iface and port have not changed (can happen if server port collides)
            ;; (mutex-lock! *heartbeat-mutex*)
            ;; (set! sdat *server-info*)
            ;; (mutex-unlock! *heartbeat-mutex*)
            
            ;; (if (or (not (equal? sdat (list iface port)))
            ;;         (not server-id))
            ;;     (begin 
            ;;       (debug:print-info 0 *default-log-port* "interface changed, refreshing iface and port info")
            ;;       (set! iface (car sdat))
            ;;       (set! port  (cadr sdat))))
            
            ;; Transfer *last-db-access* to last-access to use in checking that we are still alive
            (mutex-lock! *heartbeat-mutex*)
            (set! last-access *last-db-access*)
            (BB> "in rpc-transport:run ; last-access="last-access)
            (mutex-unlock! *heartbeat-mutex*)
            
            ;; (debug:print 11 *default-log-port* "last-access=" last-access ", server-timeout=" server-timeout)
            ;;
            ;; no_traffic, no running tests, if server 0, no running servers
            ;;
            ;; (let ((wait-on-running (configf:lookup *configdat* "server" b"wait-on-running"))) ;; wait on running tasks (if not true then exit on time out)
            ;;
            (let* ((hrs-since-start  (/ (- (current-seconds) server-start-time) 3600))
                   (adjusted-timeout (if (> hrs-since-start 1)
                                         (- server-timeout (inexact->exact (round (* hrs-since-start 60))))  ;; subtract 60 seconds per hour
                                         server-timeout)))
              (if (common:low-noise-print 120 "server timeout")
                  (debug:print-info 0 *default-log-port* "Adjusted server timeout: " adjusted-timeout))
              (if (and *server-run*
                       (> (+ last-access server-timeout)
                          (current-seconds)))
                  (begin
                    (if (common:low-noise-print 120 "server continuing")
                        (debug:print-info 0 *default-log-port* "Server continuing, seconds since last db access: " (- (current-seconds) last-access)))
                    ;;
                    ;; Consider implementing some smarts here to re-insert the record or kill self is
                    ;; the db indicates so
                    ;;
                    (if (tasks:bb-server-am-i-the-server? run-id)
                        (tasks:bb-server-set-state! server-id "running"))
                    ;;
                    (loop 0 bad-sync-count))
                  (begin
                    (BB> "SERVER SHUTDOWN CALLED!  last-access="last-access" current-seconds="(current-seconds)" server-timeout="server-timeout)
                    (rpc-transport:server-shutdown server-id rpc:listener)))))
          ;; end new loop
          ))))


(define (rpc-transport:find-free-port-and-open port #!key )
  (handle-exceptions
   exn
   (begin
     (print "Failed to bind to port " (rpc:default-server-port) ", trying next port")
     (rpc-transport:find-free-port-and-open (add1 port)))
   (rpc:default-server-port port)
   (set! *rpc-listener-port* port) ;; a bit paranoid about rpc:default-server-port parameter not changing across threads (as params are wont to do).  keeping this global in my back pocket in case this causes problems
   (set! *rpc-listener-port-bind-timestamp* (current-milliseconds)) ;; may want to test how long it has been since the last bind attempt happened...
   (tcp-read-timeout 240000)
   (tcp-buffer-size 0) ;; gotta do this because http-transport undoes it.
   (tcp-listen (rpc:default-server-port) 10000)
   ))
  
(define (rpc-transport:ping run-id host port)
  (handle-exceptions
   exn
   (begin
     (print "SERVER_NOT_FOUND exn="exn)
     (exit 1))
   (let ((login-res ((rpc:procedure 'server:login host port) *toppath*)))
     (if login-res
	 (begin
	   (print "LOGIN_OK")
	   (exit 0))
	 (begin
	   (print "LOGIN_FAILED")
	   (exit 1))))))

(define (rpc-transport:self-test run-id host port)
  (tcp-buffer-size 0) ;; gotta do this because http-transport undoes it.
  (let* ((testing-res ((rpc:procedure 'testing host port)))
         (login-res ((rpc:procedure 'server:login host port) *toppath*))
         (res (and login-res (equal? testing-res "Just testing"))))
    
    (if login-res
        (begin
          (BB> "Self test PASS.  login-res="login-res" testing-res="testing-res" *toppath*="*toppath*)
          #t)
        (begin
          (BB> "Self test fail.  login-res="login-res" testing-res="testing-res" *toppath*="*toppath*)
           
          #f))
    res))

(define (rpc-transport:client-setup run-id server-dat #!key (remtries 10))
  (BB> "entered rpc-transport:client-setup with run-id="run-id" and server-dat="server-dat" and retries="remtries)
  (tcp-buffer-size 0)
  (debug:print-info 0 *default-log-port* "rpc-transport:client-setup run-id="run-id" server-dat=" server-dat ", remaining-tries=" remtries)
  (let* ((iface     (tasks:hostinfo-get-interface server-dat))
         (hostname  (tasks:hostinfo-get-hostname  server-dat))
         (port      (tasks:hostinfo-get-port      server-dat))
         (runremote-server-dat (vector iface port #f #f #f (current-seconds) 'rpc)) ;; http version := (vector iface port api-uri api-url api-req (current-seconds) 'http  )
         (ping-res (retry-thunk (lambda ()  ;; make 3 attempts to ping.
                                  ((rpc:procedure 'server:login iface port) *toppath*))
                                chatty: #t
                                retries: 3)))
    ;; we got here from rmt:get-connection-info on the condition that *runremote* has no entry for run-id...
    (if ping-res
        (begin
          (debug:print-info 0 *default-log-port* "rpc-transport:client-setup CONNECTION ESTABLISHED run-id="run-id" server-dat=" server-dat)
          (rmt:set-cinfo run-id runremote-server-dat) ;; (hash-table-set! *runremote* run-id runremote-server-dat)  ;; side-effect - *runremote* cache init fpr rmt:*
          runremote-server-dat)
        (begin ;; login failed but have a server record, clean out the record and try again
          (debug:print-info 0 *default-log-port* "rpc-transport:client-setup UNABLE TO CONNECT run-id="run-id" server-dat=" server-dat)
          (tasks:kill-server-run-id run-id)
          (tasks:bb-server-force-clean-run-record  run-id iface port
                                                   " rpc-transport:client-setup (server-dat = #t)")
          (if (> remtries 2)
              (thread-sleep! (+ 1 (random 5))) ;; spread out the starts a little
              (thread-sleep! (+ 15 (random 20)))) ;; it isn't going well. give it plenty of time
          (server:try-running run-id)
          (thread-sleep! 5)   ;; give server a little time to start up
          (client:setup run-id remaining-tries: (sub1 remtries))
          " rpc-transport:client-setup (server-dat = #t)"))))
