;;======================================================================
;; Copyright 2006-2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

(use format typed-records) ;; RADT => purpose of json format??

(declare (unit mrmt))
(declare (uses api))
;; (declare (uses tdb))
(declare (uses http-transport))
;;(declare (uses nmsg-transport))
(include "common_records.scm")

;;
;; THESE ARE ALL CALLED ON THE CLIENT SIDE!!!
;;

;; generate entries for ~/.megatestrc with the following
;;
;;  grep define ../rmt.scm | grep mrmt: |perl -pi -e 's/\(define\s+\((\S+)\W.*$/\1/'|sort -u

;;======================================================================
;;  S U P P O R T   F U N C T I O N S
;;======================================================================

;; if a server is either running or in the process of starting call client:setup
;; else return #f to let the calling proc know that there is no server available
;;
(define (mrmt:get-connection-info areapath #!key (area-dat #f)) ;; TODO: push areapath down.
  (let* ((runremote (or area-dat *runremote*))
	 (cinfo     (if (remote? runremote)
			(remote-conndat runremote)
			#f)))
	  (if cinfo
	      cinfo
	      (if (server:check-if-running areapath)
		  (client:setup areapath)
		  #f))))

(define *send-receive-mutex* (make-mutex)) ;; should have separate mutex per run-id

;; RA => e.g. usage (mrmt:send-receive 'get-var #f (list varname))
;;
(define (mrmt:send-receive cmd rid params #!key (attemptnum 1)(area-dat #f)) ;; start attemptnum at 1 so the modulo below works as expected

  ;;DOT digraph megatest_state_status {
  ;;DOT   ranksep=0;
  ;;DOT   // rankdir=LR;
  ;;DOT   node [shape="box"];
  ;;DOT "mrmt:send-receive" -> MUTEXLOCK;
  ;;DOT { edge [style=invis];"case 1" -> "case 2" -> "case 3" -> "case 4" -> "case 5" -> "case 6" -> "case 7" -> "case 8" -> "case 9" -> "case 10" -> "case 11"; }
  ;; do all the prep locked under the rmt-mutex
  (mutex-lock! *rmt-mutex*)
  
  ;; 1. check if server is started IFF cmd is a write OR if we are not on the homehost, store in runremote
  ;; 2. check the age of the connections. refresh the connection if it is older than timeout-20 seconds.
  ;; 3. do the query, if on homehost use local access
  ;;
  (let* ((start-time    (current-seconds)) ;; snapshot time so all use cases get same value
         (areapath      *toppath*);; TODO - resolve from dbstruct to be compatible with multiple areas
	 (runremote     (or area-dat
			    *runremote*))
	 (readonly-mode (if (and runremote
				 (remote-ro-mode-checked runremote))
			    (remote-ro-mode runremote)
			    (let* ((dbfile  (conc *toppath* "/megatest.db"))
				   (ro-mode (not (file-write-access? dbfile)))) ;; TODO: use dbstruct or runremote to figure this out in future
			      (if runremote
				  (begin
				    (remote-ro-mode-set! runremote ro-mode)
				    (remote-ro-mode-checked-set! runremote #t)
				    ro-mode)
				  ro-mode)))))

    ;; DOT INIT_RUNREMOTE; // leaving off - doesn't really add to the clarity
    ;; DOT MUTEXLOCK -> INIT_RUNREMOTE [label="no remote?"];
    ;; DOT INIT_RUNREMOTE -> MUTEXLOCK;
    ;; ensure we have a record for our connection for given area
    (if (not runremote)                   ;; can remove this one. should never get here.         
	(begin
	  (set! *runremote* (make-remote))
	  (set! runremote   *runremote*))) ;; new runremote will come from this on next iteration
    
    ;; DOT SET_HOMEHOST; // leaving off - doesn't really add to the clarity
    ;; DOT MUTEXLOCK -> SET_HOMEHOST [label="no homehost?"];
    ;; DOT SET_HOMEHOST -> MUTEXLOCK;
    ;; ensure we have a homehost record
    (if (not (pair? (remote-hh-dat runremote)))  ;; not on homehost
	(thread-sleep! 0.1) ;; since we shouldn't get here, delay a little
	(remote-hh-dat-set! runremote (common:get-homehost)))
    
    ;;(print "BB> readonly-mode is "readonly-mode" dbfile is "dbfile)
    (cond
     ;;DOT EXIT;
     ;;DOT MUTEXLOCK -> EXIT [label="> 15 attempts"]; {rank=same "case 1" "EXIT" }
     ;; give up if more than 15 attempts
     ((> attemptnum 15)
      (debug:print 0 *default-log-port* "ERROR: 15 tries to start/connect to server. Giving up.")
      (exit 1))

     ;;DOT CASE2 [label="local\nreadonly\nquery"];
     ;;DOT MUTEXLOCK -> CASE2; {rank=same "case 2" CASE2}
     ;;DOT CASE2 -> "mrmt:open-qry-close-locally";
     ;; readonly mode, read request-  handle it - case 2
     ((and readonly-mode
           (member cmd api:read-only-queries)) 
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case 2")
      (mrmt:open-qry-close-locally cmd 0 params)
      )

     ;;DOT CASE3 [label="write in\nread-only mode"];
     ;;DOT MUTEXLOCK -> CASE3 [label="readonly\nmode?"]; {rank=same "case 3" CASE3}
     ;;DOT CASE3 -> "#f";
     ;; readonly mode, write request.  Do nothing, return #f
     (readonly-mode
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case 3")
      (debug:print 0 *default-log-port* "WARNING: write transaction requested on a readonly area.  cmd="cmd" params="params)
      #f)

     ;; This block was for pre-emptively resetting the connection if there had been no communication for some time.
     ;; I don't think it adds any value. If the server is not there, just fail and start a new connection.
     ;; also, the expire-time calculation might not be correct. We want, time-since-last-server-access > (server:get-timeout)
     ;;
     ;;DOT CASE4 [label="reset\nconnection"];
     ;;DOT MUTEXLOCK -> CASE4 [label="have connection,\nlast_access > expire_time"]; {rank=same "case 4" CASE4}
     ;;DOT CASE4 -> "mrmt:send-receive";
     ;; reset the connection if it has been unused too long
     ((and runremote
           (remote-conndat runremote)
	   (> (current-seconds) ;; if it has been more than server-timeout seconds since last contact, close this connection and start a new on
	      (+ (http-transport:server-dat-get-last-access (remote-conndat runremote))
		 (remote-server-timeout runremote))))
      (debug:print-info 0 *default-log-port* "Connection to " (remote-server-url runremote) " expired due to no accesses, forcing new connection.")
      (http-transport:close-connections area-dat: runremote)
      (remote-conndat-set! runremote #f) ;; invalidate the connection, thus forcing a new connection.
      (mutex-unlock! *rmt-mutex*)
      (mrmt:send-receive cmd rid params attemptnum: attemptnum))
     
     ;;DOT CASE5 [label="local\nread"];
     ;;DOT MUTEXLOCK -> CASE5 [label="server not required,\non homehost,\nread-only query"]; {rank=same "case 5" CASE5};
     ;;DOT CASE5 -> "mrmt:open-qry-close-locally";
     ;; on homehost and this is a read
     ((and (not (remote-force-server runremote)) ;; honor forced use of server, i.e. server NOT required
	   (cdr (remote-hh-dat runremote))       ;; on homehost
           (member cmd api:read-only-queries))   ;; this is a read
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  5")
      (mrmt:open-qry-close-locally cmd 0 params))

     ;;DOT CASE6 [label="init\nremote"];
     ;;DOT MUTEXLOCK -> CASE6 [label="on homehost,\nwrite query,\nhave server,\ncan't reach it"]; {rank=same "case 6" CASE6};
     ;;DOT CASE6 -> "mrmt:send-receive";
     ;; on homehost and this is a write, we already have a server, but server has died
     ((and (cdr (remote-hh-dat runremote))           ;; on homehost
           (not (member cmd api:read-only-queries))  ;; this is a write
           (remote-server-url runremote)             ;; have a server
           (not (server:ping (remote-server-url runremote))))  ;; server has died. NOTE: this is not a cheap call! Need better approach.
      (set! *runremote* (make-remote))
      (remote-force-server-set! runremote (common:force-server?))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  6")
      (mrmt:send-receive cmd rid params attemptnum: attemptnum))

     ;;DOT CASE7 [label="homehost\nwrite"];
     ;;DOT MUTEXLOCK -> CASE7 [label="server not required,\non homehost,\na write,\nhave a server"]; {rank=same "case 7" CASE7};
     ;;DOT CASE7 -> "mrmt:open-qry-close-locally";
     ;; on homehost and this is a write, we already have a server
     ((and (not (remote-force-server runremote))     ;; honor forced use of server, i.e. server NOT required
	   (cdr (remote-hh-dat runremote))           ;; on homehost
           (not (member cmd api:read-only-queries))  ;; this is a write
           (remote-server-url runremote))            ;; have a server
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  4.1")
      (mrmt:open-qry-close-locally cmd 0 params))

     ;;DOT CASE8 [label="force\nserver"];
     ;;DOT MUTEXLOCK -> CASE8 [label="server not required,\nhave homehost info,\nno connection yet,\nnot a read-only query"]; {rank=same "case 8" CASE8};
     ;;DOT CASE8 -> "mrmt:open-qry-close-locally";
     ;;  on homehost, no server contact made and this is a write, passively start a server 
     ((and (not (remote-force-server runremote))     ;; honor forced use of server, i.e. server NOT required
	   (cdr (remote-hh-dat runremote))           ;; have homehost
           (not (remote-server-url runremote))       ;; no connection yet
	   (not (member cmd api:read-only-queries))) ;; not a read-only query
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  8")
      (let ((server-url  (server:check-if-running *toppath*))) ;; (server:read-dotserver->url *toppath*))) ;; (server:check-if-running *toppath*))) ;; Do NOT want to run server:check-if-running - very expensive to do for every write call
	(if server-url
	    (remote-server-url-set! runremote server-url) ;; the string can be consumed by the client setup if needed
	    (if (common:force-server?)
		(server:start-and-wait *toppath*)
		(server:kind-run *toppath*))))
      (remote-force-server-set! runremote (common:force-server?))
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  8.1")
      (mrmt:open-qry-close-locally cmd 0 params))

     ;;DOT CASE9 [label="force server\nnot on homehost"];
     ;;DOT MUTEXLOCK -> CASE9 [label="no connection\nand either require server\nor not on homehost"]; {rank=same "case 9" CASE9};
     ;;DOT CASE9 -> "start\nserver" -> "mrmt:send-receive";
     ((or (and (remote-force-server runremote)              ;; we are forcing a server and don't yet have a connection to one
	       (not (remote-conndat runremote)))
	  (and (not (cdr (remote-hh-dat runremote)))        ;; not on a homehost 
	       (not (remote-conndat runremote))))           ;; and no connection
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case 9, hh-dat: " (remote-hh-dat runremote) " conndat: " (remote-conndat runremote))
      (mutex-unlock! *rmt-mutex*)
      (if (not (server:check-if-running *toppath*)) ;; who knows, maybe one has started up?
	  (server:start-and-wait *toppath*))
      (remote-conndat-set! runremote (mrmt:get-connection-info *toppath*)) ;; calls client:setup which calls client:setup-http
      (mrmt:send-receive cmd rid params attemptnum: attemptnum)) ;; TODO: add back-off timeout as

     ;;DOT CASE10 [label="on homehost"];
     ;;DOT MUTEXLOCK -> CASE10 [label="server not required,\non homehost"]; {rank=same "case 10" CASE10};
     ;;DOT CASE10 -> "mrmt:open-qry-close-locally";
     ;; all set up if get this far, dispatch the query
     ((and (not (remote-force-server runremote))
	   (cdr (remote-hh-dat runremote))) ;; we are on homehost
      (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case 10")
      (mrmt:open-qry-close-locally cmd (if rid rid 0) params))

     ;;DOT CASE11 [label="send_receive"];
     ;;DOT MUTEXLOCK -> CASE11 [label="else"]; {rank=same "case 11" CASE11};
     ;;DOT CASE11 -> "mrmt:send-receive" [label="call failed"];
     ;;DOT CASE11 -> "RESULT" [label="call succeeded"];
     ;; not on homehost, do server query
     (else
      ;; (mutex-unlock! *rmt-mutex*)
      (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  9")
      ;; (mutex-lock! *rmt-mutex*)
      (let* ((conninfo (remote-conndat runremote))
	     (dat      (case (remote-transport runremote)
			 ((http) (condition-case ;; handling here has caused a lot of problems. However it is needed to deal with attemtped communication to servers that have gone away
                                  (http-transport:client-api-send-receive 0 conninfo cmd params)
                                  ((commfail)(vector #f "communications fail"))
                                  ((exn)(vector #f "other fail" (print-call-chain)))))
			 (else
			  (debug:print 0 *default-log-port* "ERROR: transport " (remote-transport runremote) " not supported")
			  (exit))))
	     (success  (if (vector? dat) (vector-ref dat 0) #f))
	     (res      (if (vector? dat) (vector-ref dat 1) #f)))
	(if (and (vector? conninfo) (< 5 (vector-length conninfo)))
            (http-transport:server-dat-update-last-access conninfo) ;; refresh access time
	    (begin
              (debug:print 0 *default-log-port* "INFO: Should not get here! conninfo=" conninfo)
              (set! conninfo #f)
              (remote-conndat-set! *runremote* #f)
              (http-transport:close-connections  area-dat: runremote)))
	;; (mutex-unlock! *rmt-mutex*)
        (debug:print-info 13 *default-log-port* "mrmt:send-receive, case  9. conninfo=" conninfo " dat=" dat " runremote = " runremote)
	(mutex-unlock! *rmt-mutex*)
	(if success ;; success only tells us that the transport was successful, have to examine the data to see if there was a detected issue at the other end
	    (if (and (vector? res)
		     (eq? (vector-length res) 2)
		     (eq? (vector-ref res 1) 'overloaded)) ;; since we are looking at the data to carry the error we'll use a fairly obtuse combo to minimise the chances of some sort of collision.
                ;; this is the case where the returned data is bad or the server is overloaded and we want
                ;; to ease off the queries
		(let ((wait-delay (+ attemptnum (* attemptnum 10))))
		  (debug:print 0 *default-log-port* "WARNING: server is overloaded. Delaying " wait-delay " seconds and trying call again.")
		  (mutex-lock! *rmt-mutex*)
		  (http-transport:close-connections area-dat: runremote)
		  (set! *runremote* #f) ;; force starting over
		  (mutex-unlock! *rmt-mutex*)
		  (thread-sleep! wait-delay)
		  (mrmt:send-receive cmd rid params attemptnum: (+ attemptnum 1)))
		res) ;; All good, return res
	    (begin
	      (debug:print 0 *default-log-port* "WARNING: communication failed. Trying again, try num: " attemptnum)
	      (mutex-lock! *rmt-mutex*)
              (remote-conndat-set!    runremote #f)
	      (http-transport:close-connections area-dat: runremote)
	      (remote-server-url-set! runremote #f)
	      (mutex-unlock! *rmt-mutex*)
              (debug:print-info 12 *default-log-port* "mrmt:send-receive, case  9.1")
	      ;; (if (not (server:check-if-running *toppath*))
	      ;; 	  (server:start-and-wait *toppath*))
	      (mrmt:send-receive cmd rid params attemptnum: (+ attemptnum 1)))))))))

    ;;DOT }
    
;; (define (mrmt:update-db-stats run-id rawcmd params duration)
;;   (mutex-lock! *db-stats-mutex*)
;;   (handle-exceptions
;;    exn
;;    (begin
;;      (debug:print 0 *default-log-port* "WARNING: stats collection failed in update-db-stats")
;;      (debug:print 0 *default-log-port* " message: " ((condition-property-accessor 'exn 'message) exn))
;;      (print "exn=" (condition->list exn))
;;      #f) ;; if this fails we don't care, it is just stats
;;    (let* ((cmd      (conc "run-id=" run-id " " (if (eq? rawcmd 'general-call) (car params) rawcmd)))
;; 	  (stat-vec (hash-table-ref/default *db-stats* cmd #f)))
;;      (if (not (vector? stat-vec))
;; 	 (let ((newvec (vector 0 0)))
;; 	   (hash-table-set! *db-stats* cmd newvec)
;; 	   (set! stat-vec newvec)))
;;      (vector-set! stat-vec 0 (+ (vector-ref stat-vec 0) 1))
;;      (vector-set! stat-vec 1 (+ (vector-ref stat-vec 1) duration))))
;;   (mutex-unlock! *db-stats-mutex*))

(define (mrmt:print-db-stats)
  (let ((fmtstr "~40a~7-d~9-d~20,2-f")) ;; "~20,2-f"
    (debug:print 18 *default-log-port* "DB Stats\n========")
    (debug:print 18 *default-log-port* (format #f "~40a~8a~10a~10a" "Cmd" "Count" "TotTime" "Avg"))
    (for-each (lambda (cmd)
		(let ((cmd-dat (hash-table-ref *db-stats* cmd)))
		  (debug:print 18 *default-log-port* (format #f fmtstr cmd (vector-ref cmd-dat 0) (vector-ref cmd-dat 1) (/ (vector-ref cmd-dat 1)(vector-ref cmd-dat 0))))))
	      (sort (hash-table-keys *db-stats*)
		    (lambda (a b)
		      (> (vector-ref (hash-table-ref *db-stats* a) 0)
			 (vector-ref (hash-table-ref *db-stats* b) 0)))))))

(define (mrmt:get-max-query-average run-id)
  (mutex-lock! *db-stats-mutex*)
  (let* ((runkey (conc "run-id=" run-id " "))
	 (cmds   (filter (lambda (x)
			   (substring-index runkey x))
			 (hash-table-keys *db-stats*)))
	 (res    (if (null? cmds)
		     (cons 'none 0)
		     (let loop ((cmd (car cmds))
				(tal (cdr cmds))
				(max-cmd (car cmds))
				(res 0))
		       (let* ((cmd-dat (hash-table-ref *db-stats* cmd))
			      (tot     (vector-ref cmd-dat 0))
			      (curravg (/ (vector-ref cmd-dat 1) (vector-ref cmd-dat 0))) ;; count is never zero by construction
			      (currmax (max res curravg))
			      (newmax-cmd (if (> curravg res) cmd max-cmd)))
			 (if (null? tal)
			     (if (> tot 10)
				 (cons newmax-cmd currmax)
				 (cons 'none 0))
			     (loop (car tal)(cdr tal) newmax-cmd currmax)))))))
    (mutex-unlock! *db-stats-mutex*)
    res))

(define (mrmt:open-qry-close-locally cmd run-id params #!key (remretries 5))
  (let* ((qry-is-write   (not (member cmd api:read-only-queries)))
	 (db-file-path   (db:dbfile-path)) ;;  0))
	 (dbstruct-local (db:setup #t))  ;; make-dbr:dbstruct path:  dbdir local: #t)))
	 (read-only      (not (file-write-access? db-file-path)))
	 (start          (current-milliseconds))
	 (resdat         (if (not (and read-only qry-is-write))
			     (let ((v (api:execute-requests dbstruct-local (vector (symbol->string cmd) params))))
			       (handle-exceptions ;; there has been a long history of receiving strange errors from values returned by the client when things go wrong..
				exn               ;;  This is an attempt to detect that situation and recover gracefully
				(begin
				  (debug:print0 *default-log-port* "ERROR: bad data from server " v " message: "  ((condition-property-accessor 'exn 'message) exn))
				  (vector #t '())) ;; should always get a vector but if something goes wrong return a dummy
				(if (and (vector? v)
					 (> (vector-length v) 1))
				    (let ((newvec (vector (vector-ref v 0)(vector-ref v 1))))
				      newvec)           ;; by copying the vector while inside the error handler we should force the detection of a corrupted record
				    (vector #t '()))))  ;; we could also check that the returned types are valid
			     (vector #t '())))
	 (success        (vector-ref resdat 0))
	 (res            (vector-ref resdat 1))
	 (duration       (- (current-milliseconds) start)))
    (if (and read-only qry-is-write)
        (debug:print 0 *default-log-port* "ERROR: attempt to write to read-only database ignored. cmd=" cmd))
    (if (not success)
	(if (> remretries 0)
	    (begin
	      (debug:print-error 0 *default-log-port* "local query failed. Trying again.")
	      (thread-sleep! (/ (random 5000) 1000)) ;; some random delay 
	      (mrmt:open-qry-close-locally cmd run-id params remretries: (- remretries 1)))
	    (begin
	      (debug:print-error 0 *default-log-port* "too many retries in mrmt:open-qry-close-locally, giving up")
	      #f))
	(begin
	  ;; (mrmt:update-db-stats run-id cmd params duration)
	  ;; mark this run as dirty if this was a write, the watchdog is responsible for syncing it
	  (if qry-is-write
	      (let ((start-time (current-seconds)))
		(mutex-lock! *db-multi-sync-mutex*)
/		(set! *db-last-access* start-time)  ;; THIS IS PROBABLY USELESS? (we are on a client)
                (mutex-unlock! *db-multi-sync-mutex*)))))
    res))

(define (mrmt:send-receive-no-auto-client-setup connection-info cmd run-id params)
  (let* ((run-id   (if run-id run-id 0))
	 (res  	   (handle-exceptions
		    exn
		    #f
		    (http-transport:client-api-send-receive run-id connection-info cmd params))))
    (if (and res (vector-ref res 0))
	(vector-ref res 1) ;;; YES!! THIS IS CORRECT!! CHANGE IT HERE, THEN CHANGE mrmt:send-receive ALSO!!!
	#f)))

;; ;; Wrap json library for strings (why the ports crap in the first place?)
;; (define (mrmt:dat->json-str dat)
;;   (with-output-to-string 
;;     (lambda ()
;;       (json-write dat))))
;; 
;; (define (mrmt:json-str->dat json-str)
;;   (with-input-from-string json-str
;;     (lambda ()
;;       (json-read))))

;;======================================================================
;;
;; A C T U A L   A P I   C A L L S  
;;
;;======================================================================

;;======================================================================
;;  S E R V E R
;;======================================================================

(define (mrmt:kill-server run-id)
  (mrmt:send-receive 'kill-server run-id (list run-id)))

(define (mrmt:start-server run-id)
  (mrmt:send-receive 'start-server 0 (list run-id)))

;;======================================================================
;;  M I S C
;;======================================================================

(define (mrmt:login run-id)
  (mrmt:send-receive 'login run-id (list *toppath* megatest-version *my-client-signature*)))

;; This login does no retries under the hood - it acts a bit like a ping.
;; Deprecated for nmsg-transport.
;;
(define (mrmt:login-no-auto-client-setup connection-info)
  (case *transport-type* ;; run-id of 0 is just a placeholder
    ((http)(mrmt:send-receive-no-auto-client-setup connection-info 'login 0 (list *toppath* megatest-version *my-client-signature*)))
    ;;((nmsg)(nmsg-transport:client-api-send-receive run-id connection-info 'login (list *toppath* megatest-version run-id *my-client-signature*)))
    ))

;; hand off a call to one of the db:queries statements
;; added run-id to make looking up the correct db possible 
;;
(define (mrmt:general-call stmtname run-id . params)
  (mrmt:send-receive 'general-call run-id (append (list stmtname run-id) params)))


;; given a hostname, return a pair of cpu load and update time representing latest intelligence from tests running on that host
(define (mrmt:get-latest-host-load hostname)
  (mrmt:send-receive 'get-latest-host-load 0 (list hostname)))

;; (define (mrmt:sync-inmem->db run-id)
;;   (mrmt:send-receive 'sync-inmem->db run-id '()))

(define (mrmt:sdb-qry qry val run-id)
  ;; add caching if qry is 'getid or 'getstr
  (mrmt:send-receive 'sdb-qry run-id (list qry val)))

;; NOT COMPLETED
(define (mrmt:runtests user run-id testpatt params)
  (mrmt:send-receive 'runtests run-id testpatt))

(define (mrmt:get-changed-record-ids since-time)
  (mrmt:send-receive 'get-changed-record-ids #f (list since-time)) )

;;======================================================================
;;  T E S T   M E T A 
;;======================================================================

(define (mrmt:get-tests-tags)
  (mrmt:send-receive 'get-tests-tags #f '()))

;;======================================================================
;;  K E Y S 
;;======================================================================

;; These require run-id because the values come from the run!
;;
(define (mrmt:get-key-val-pairs run-id)
  (mrmt:send-receive 'get-key-val-pairs run-id (list run-id)))

(define (mrmt:get-keys)
  (if *db-keys* *db-keys* 
     (let ((res (mrmt:send-receive 'get-keys #f '())))
       (set! *db-keys* res)
       res)))

(define (mrmt:get-keys-write) ;; dummy query to force server start
  (let ((res (mrmt:send-receive 'get-keys-write #f '())))
    (set! *db-keys* res)
    res))

;; we don't reuse run-id's (except possibly *after* a db cleanup) so it is safe
;; to cache the resuls in a hash
;;
(define (mrmt:get-key-vals run-id)
  (or (hash-table-ref/default *keyvals* run-id #f)
      (let ((res (mrmt:send-receive 'get-key-vals #f (list run-id))))
        (hash-table-set! *keyvals* run-id res)
        res)))

(define (mrmt:get-targets)
  (mrmt:send-receive 'get-targets #f '()))

(define (mrmt:get-target run-id)
  (mrmt:send-receive 'get-target run-id (list run-id)))

;;======================================================================
;;  T E S T S
;;======================================================================

;; Just some syntatic sugar
(define (mrmt:register-test run-id test-name item-path)
  (mrmt:general-call 'register-test run-id run-id test-name item-path))

(define (mrmt:get-test-id run-id testname item-path)
  (mrmt:send-receive 'get-test-id run-id (list run-id testname item-path)))

;; run-id is NOT used
;;
(define (mrmt:get-test-info-by-id run-id test-id)
  (if (number? test-id)
      (mrmt:send-receive 'get-test-info-by-id run-id (list run-id test-id))
      (begin
	(debug:print 0 *default-log-port* "WARNING: Bad data handed to mrmt:get-test-info-by-id run-id=" run-id ", test-id=" test-id)
	(print-call-chain (current-error-port))
	#f)))

(define (mrmt:test-get-rundir-from-test-id run-id test-id)
  (mrmt:send-receive 'test-get-rundir-from-test-id run-id (list run-id test-id)))

(define (mrmt:open-test-db-by-test-id run-id test-id #!key (work-area #f))
  (let* ((test-path (if (string? work-area)
			work-area
			(mrmt:test-get-rundir-from-test-id run-id test-id))))
    (debug:print 3 *default-log-port* "TEST PATH: " test-path)
    (open-test-db test-path)))

;; WARNING: This currently bypasses the transaction wrapped writes system
(define (mrmt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment)
  (mrmt:send-receive 'test-set-state-status-by-id run-id (list run-id test-id newstate newstatus newcomment)))

(define (mrmt:set-tests-state-status run-id                      testnames currstate currstatus newstate newstatus)
  (mrmt:send-receive 'set-tests-state-status run-id (list run-id testnames currstate currstatus newstate newstatus)))

(define (mrmt:get-tests-for-run run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)
  ;; (if (number? run-id)
  (mrmt:send-receive 'get-tests-for-run run-id (list run-id testpatt states statuses offset limit not-in sort-by sort-order qryvals last-update mode)))
  ;;    (begin
  ;;	(debug:print-error 0 *default-log-port* "mrmt:get-tests-for-run called with bad run-id=" run-id)
  ;;	(print-call-chain (current-error-port))
  ;;	'())))

;; get stuff via synchash 
(define (mrmt:synchash-get run-id proc synckey keynum params)
  (mrmt:send-receive 'synchash-get run-id (list run-id proc synckey keynum params)))

;; IDEA: Threadify these - they spend a lot of time waiting ...
;;
(define (mrmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
  (let ((multi-run-mutex (make-mutex))
	(run-id-list (if run-ids
			 run-ids
			 (mrmt:get-all-run-ids)))
	(result      '()))
    (if (null? run-id-list)
	'()
	(let loop ((hed     (car run-id-list))
		   (tal     (cdr run-id-list))
		   (threads '()))
	  (if (> (length threads) 5)
	      (loop hed tal (filter (lambda (th)(not (member (thread-state th) '(terminated dead)))) threads))
	      (let* ((newthread (make-thread
				 (lambda ()
				   (let ((res (mrmt:send-receive 'get-tests-for-run-mindata hed (list hed testpatt states status not-in))))
				     (if (list? res)
					 (begin
					   (mutex-lock! multi-run-mutex)
					   (set! result (append result res))
					   (mutex-unlock! multi-run-mutex))
					 (debug:print-error 0 *default-log-port* "get-tests-for-run-mindata failed for run-id " hed ", testpatt " testpatt ", states " states ", status " status ", not-in " not-in))))
				 (conc "multi-run-thread for run-id " hed)))
		     (newthreads (cons newthread threads)))
		(thread-start! newthread)
		(thread-sleep! 0.05) ;; give that thread some time to start
		(if (null? tal)
		    newthreads
		    (loop (car tal)(cdr tal) newthreads))))))
    result))

;; ;; IDEA: Threadify these - they spend a lot of time waiting ...
;; ;;
;; (define (mrmt:get-tests-for-runs-mindata run-ids testpatt states status not-in)
;;   (let ((run-id-list (if run-ids
;; 			 run-ids
;; 			 (mrmt:get-all-run-ids))))
;;     (apply append (map (lambda (run-id)
;; 			 (mrmt:send-receive 'get-tests-for-run-mindata run-id (list run-ids testpatt states status not-in)))
;; 		       run-id-list))))

(define (mrmt:delete-test-records run-id test-id)
  (mrmt:send-receive 'delete-test-records run-id (list run-id test-id)))

;; This is not needed as test steps are deleted on test delete call
;;
;; (define (mrmt:delete-test-step-records run-id test-id)
;;   (mrmt:send-receive 'delete-test-step-records run-id (list run-id test-id)))

(define (mrmt:test-set-state-status run-id test-id state status msg)
  (mrmt:send-receive 'test-set-state-status run-id (list run-id test-id state status msg)))

(define (mrmt:test-toplevel-num-items run-id test-name)
  (mrmt:send-receive 'test-toplevel-num-items run-id (list run-id test-name)))

;; (define (mrmt:get-previous-test-run-record run-id test-name item-path)
;;   (mrmt:send-receive 'get-previous-test-run-record run-id (list run-id test-name item-path)))

(define (mrmt:get-matching-previous-test-run-records run-id test-name item-path)
  (mrmt:send-receive 'get-matching-previous-test-run-records run-id (list run-id test-name item-path)))

(define (mrmt:test-get-logfile-info run-id test-name)
  (mrmt:send-receive 'test-get-logfile-info run-id (list run-id test-name)))

(define (mrmt:test-get-records-for-index-file run-id test-name)
  (mrmt:send-receive 'test-get-records-for-index-file run-id (list run-id test-name)))

(define (mrmt:get-testinfo-state-status run-id test-id)
  (mrmt:send-receive 'get-testinfo-state-status run-id (list run-id test-id)))

(define (mrmt:test-set-log! run-id test-id logf)
  (if (string? logf)(mrmt:general-call 'test-set-log run-id logf test-id)))

(define (mrmt:test-set-top-process-pid run-id test-id pid)
  (mrmt:send-receive 'test-set-top-process-pid run-id (list run-id test-id pid)))

(define (mrmt:test-get-top-process-pid run-id test-id)
  (mrmt:send-receive 'test-get-top-process-pid run-id (list run-id test-id)))

(define (mrmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt)
  (mrmt:send-receive 'get-run-ids-matching-target #f (list keynames target res runname testpatt statepatt statuspatt)))

;; NOTE: This will open and access ALL run databases. 
;;
(define (mrmt:test-get-paths-matching-keynames-target-new keynames target res testpatt statepatt statuspatt runname)
  (let ((run-ids (mrmt:get-run-ids-matching-target keynames target res runname testpatt statepatt statuspatt)))
    (apply append 
	   (map (lambda (run-id)
		  (mrmt:send-receive 'test-get-paths-matching-keynames-target-new run-id (list run-id keynames target res testpatt statepatt statuspatt runname)))
	   run-ids))))

;; (define (mrmt:get-run-ids-matching keynames target res)
;;   (mrmt:send-receive #f 'get-run-ids-matching (list keynames target res)))

(define (mrmt:get-prereqs-not-met run-id waitons ref-test-name ref-item-path #!key (mode '(normal))(itemmaps #f))
  (mrmt:send-receive 'get-prereqs-not-met run-id (list run-id waitons ref-test-name ref-item-path mode itemmaps)))

(define (mrmt:get-count-tests-running-for-run-id run-id)
  (mrmt:send-receive 'get-count-tests-running-for-run-id run-id (list run-id)))

;; Statistical queries

(define (mrmt:get-count-tests-running run-id)
  (mrmt:send-receive 'get-count-tests-running run-id (list run-id)))

(define (mrmt:get-count-tests-running-for-testname run-id testname)
  (mrmt:send-receive 'get-count-tests-running-for-testname run-id (list run-id testname)))

(define (mrmt:get-count-tests-running-in-jobgroup run-id jobgroup)
  (mrmt:send-receive 'get-count-tests-running-in-jobgroup run-id (list run-id jobgroup)))

;; state and status are extra hints not usually used in the calculation
;;
(define (mrmt:set-state-status-and-roll-up-items run-id test-name item-path state status comment)
  (mrmt:send-receive 'set-state-status-and-roll-up-items run-id (list run-id test-name item-path state status comment)))

(define (mrmt:update-pass-fail-counts run-id test-name)
  (mrmt:general-call 'update-pass-fail-counts run-id test-name test-name test-name))

(define (mrmt:top-test-set-per-pf-counts run-id test-name)
  (mrmt:send-receive 'top-test-set-per-pf-counts run-id (list run-id test-name)))

(define (mrmt:get-raw-run-stats run-id)
  (mrmt:send-receive 'get-raw-run-stats run-id (list run-id)))

;;======================================================================
;;  R U N S
;;======================================================================

(define (mrmt:get-run-info run-id)
  (mrmt:send-receive 'get-run-info run-id (list run-id)))

(define (mrmt:get-num-runs runpatt)
  (mrmt:send-receive 'get-num-runs #f (list runpatt)))

;; Use the special run-id == #f scenario here since there is no run yet
(define (mrmt:register-run keyvals runname state status user contour)
  (mrmt:send-receive 'register-run #f (list keyvals runname state status user contour)))
    
(define (mrmt:get-run-name-from-id run-id)
  (mrmt:send-receive 'get-run-name-from-id run-id (list run-id)))

(define (mrmt:delete-run run-id)
  (mrmt:send-receive 'delete-run run-id (list run-id)))

(define (mrmt:update-run-stats run-id stats)
  (mrmt:send-receive 'update-run-stats #f (list run-id stats)))

(define (mrmt:delete-old-deleted-test-records)
  (mrmt:send-receive 'delete-old-deleted-test-records #f '()))

(define (mrmt:get-runs runpatt count offset keypatts)
  (mrmt:send-receive 'get-runs #f (list runpatt count offset keypatts)))

(define (mrmt:get-all-run-ids)
  (mrmt:send-receive 'get-all-run-ids #f '()))

(define (mrmt:get-prev-run-ids run-id)
  (mrmt:send-receive 'get-prev-run-ids #f (list run-id)))

(define (mrmt:lock/unlock-run run-id lock unlock user)
  (mrmt:send-receive 'lock/unlock-run #f (list run-id lock unlock user)))

;; set/get status
(define (mrmt:get-run-status run-id)
  (mrmt:send-receive 'get-run-status #f (list run-id)))

(define (mrmt:set-run-status run-id run-status #!key (msg #f))
  (mrmt:send-receive 'set-run-status #f (list run-id run-status msg)))

(define (mrmt:update-run-event_time run-id)
  (mrmt:send-receive 'update-run-event_time #f (list run-id)))

(define (mrmt:get-runs-by-patt  keys runnamepatt targpatt offset limit fields last-runs-update) ;; fields of #f uses default
  (mrmt:send-receive 'get-runs-by-patt #f (list keys runnamepatt targpatt offset limit fields last-runs-update)))

(define (mrmt:find-and-mark-incomplete run-id ovr-deadtime)
  ;; (if (mrmt:send-receive 'have-incompletes? run-id (list run-id ovr-deadtime))
  (mrmt:send-receive 'mark-incomplete run-id (list run-id ovr-deadtime))) ;; )

(define (mrmt:get-main-run-stats run-id)
  (mrmt:send-receive 'get-main-run-stats #f (list run-id)))

(define (mrmt:get-var varname)
  (mrmt:send-receive 'get-var #f (list varname)))

(define (mrmt:del-var varname)
  (mrmt:send-receive 'del-var #f (list varname)))

(define (mrmt:set-var varname value)
  (mrmt:send-receive 'set-var #f (list varname value)))

;;======================================================================
;; M U L T I R U N   Q U E R I E S
;;======================================================================

;; Need to move this to multi-run section and make associated changes
(define (mrmt:find-and-mark-incomplete-all-runs #!key (ovr-deadtime #f))
  (let ((run-ids (mrmt:get-all-run-ids)))
    (for-each (lambda (run-id)
	       (mrmt:find-and-mark-incomplete run-id ovr-deadtime))
	     run-ids)))

;; get the previous record for when this test was run where all keys match but runname
;; returns #f if no such test found, returns a single test record if found
;; 
;; Run this at the client end since we have to connect to multiple run-id dbs
;;
(define (mrmt:get-previous-test-run-record run-id test-name item-path)
  (let* ((keyvals (mrmt:get-key-val-pairs run-id))
	 (keys    (mrmt:get-keys))
	 (selstr  (string-intersperse  keys ","))
	 (qrystr  (string-intersperse (map (lambda (x)(conc x "=?")) keys) " AND ")))
    (if (not keyvals)
	#f
	(let ((prev-run-ids (mrmt:get-prev-run-ids run-id)))
	  ;; for each run starting with the most recent look to see if there is a matching test
	  ;; if found then return that matching test record
	  (debug:print 4 *default-log-port* "selstr: " selstr ", qrystr: " qrystr ", keyvals: " keyvals ", previous run ids found: " prev-run-ids)
	  (if (null? prev-run-ids) #f
	      (let loop ((hed (car prev-run-ids))
			 (tal (cdr prev-run-ids)))
		(let ((results (mrmt:get-tests-for-run hed (conc test-name "/" item-path) '() '() ;; run-id testpatt states statuses
						      #f #f #f               ;; offset limit not-in hide/not-hide
						      #f #f #f #f 'normal))) ;; sort-by sort-order qryvals last-update mode
		  (debug:print 4 *default-log-port* "Got tests for run-id " run-id ", test-name " test-name ", item-path " item-path ": " results)
		  (if (and (null? results)
			   (not (null? tal)))
		      (loop (car tal)(cdr tal))
		      (if (null? results) #f
			  (car results))))))))))

(define (mrmt:get-run-stats)
  (mrmt:send-receive 'get-run-stats #f '()))

;;======================================================================
;;  S T E P S
;;======================================================================

;; Getting steps is more complicated.
;;
;; If given work area 
;;  1. Find the testdat.db file
;;  2. Open the testdat.db file and do the query
;; If not given the work area
;;  1. Do a remote call to get the test path
;;  2. Continue as above
;; 
;;(define (mrmt:get-steps-for-test run-id test-id)
;;  (mrmt:send-receive 'get-steps-data run-id (list test-id)))

(define (mrmt:teststep-set-status! run-id test-id teststep-name state-in status-in comment logfile)
  (let* ((state     (items:check-valid-items "state" state-in))
	 (status    (items:check-valid-items "status" status-in)))
    (if (or (not state)(not status))
	(debug:print 3 *default-log-port* "WARNING: Invalid " (if status "status" "state")
		     " value \"" (if status state-in status-in) "\", update your validvalues section in megatest.config"))
    (mrmt:send-receive 'teststep-set-status! run-id (list run-id test-id teststep-name state-in status-in comment logfile))))

(define (mrmt:get-steps-for-test run-id test-id)
  (mrmt:send-receive 'get-steps-for-test run-id (list run-id test-id)))

;;======================================================================
;;  T E S T   D A T A 
;;======================================================================

(define (mrmt:read-test-data run-id test-id categorypatt #!key (work-area #f)) 
  (mrmt:send-receive 'read-test-data run-id (list run-id test-id categorypatt)))
(define (mrmt:read-test-data* run-id test-id categorypatt varpatt #!key (work-area #f)) 
  (mrmt:send-receive 'read-test-data* run-id (list run-id test-id categorypatt varpatt)))

;;   (let ((tdb  (mrmt:open-test-db-by-test-id run-id test-id work-area: work-area)))
;;     (if tdb
;; 	(tdb:read-test-data tdb test-id categorypatt)
;; 	'())))

(define (mrmt:testmeta-add-record testname)
  (mrmt:send-receive 'testmeta-add-record #f (list testname)))

(define (mrmt:testmeta-get-record testname)
  (mrmt:send-receive 'testmeta-get-record #f (list testname)))

(define (mrmt:testmeta-update-field test-name fld val)
  (mrmt:send-receive 'testmeta-update-field #f (list test-name fld val)))

(define (mrmt:test-data-rollup run-id test-id status)
  (mrmt:send-receive 'test-data-rollup run-id (list run-id test-id status)))

(define (mrmt:csv->test-data run-id test-id csvdata)
  (mrmt:send-receive 'csv->test-data run-id (list run-id test-id csvdata)))

;;======================================================================
;;  T A S K S
;;======================================================================

(define (mrmt:tasks-find-task-queue-records target run-name test-patt state-patt action-patt)
  (mrmt:send-receive 'find-task-queue-records #f (list target run-name test-patt state-patt action-patt)))

(define (mrmt:tasks-add action owner target runname testpatt params)
  (mrmt:send-receive 'tasks-add #f (list action owner target runname testpatt params)))

(define (mrmt:tasks-set-state-given-param-key param-key new-state)
  (mrmt:send-receive 'tasks-set-state-given-param-key #f (list  param-key new-state)))

(define (mrmt:tasks-get-last target runname)
  (mrmt:send-receive 'tasks-get-last #f (list target runname)))

;;======================================================================
;; N O   S Y N C   D B 
;;======================================================================

(define (mrmt:no-sync-set var val)
  (mrmt:send-receive 'no-sync-set #f `(,var ,val)))

(define (mrmt:no-sync-get/default var default)
  (mrmt:send-receive 'no-sync-get/default #f `(,var ,default)))

(define (mrmt:no-sync-del! var)
  (mrmt:send-receive 'no-sync-del! #f `(,var)))

(define (mrmt:no-sync-get-lock keyname)
  (mrmt:send-receive 'no-sync-get-lock #f `(,keyname)))

;;======================================================================
;; A R C H I V E S
;;======================================================================

(define (mrmt:archive-get-allocations  testname itempath dneeded)
  (mrmt:send-receive 'archive-get-allocations #f (list testname itempath dneeded)))

(define (mrmt:archive-register-block-name bdisk-id archive-path)
  (mrmt:send-receive 'archive-register-block-name #f (list bdisk-id archive-path)))

(define (mrmt:archive-allocate-testsuite/area-to-block block-id testsuite-name areakey)
  (mrmt:send-receive 'archive-allocate-test-to-block #f (list  block-id testsuite-name areakey)))

(define (mrmt:archive-register-disk bdisk-name bdisk-path df)
  (mrmt:send-receive 'archive-register-disk #f (list bdisk-name bdisk-path df)))

(define (mrmt:test-set-archive-block-id run-id test-id archive-block-id)
  (mrmt:send-receive 'test-set-archive-block-id run-id (list run-id test-id archive-block-id)))

(define (mrmt:test-get-archive-block-info archive-block-id)
  (mrmt:send-receive 'test-get-archive-block-info #f (list archive-block-id)))
