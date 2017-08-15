;; Copyright 2006-2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;
;;======================================================================
;;
;; Wrapper to enable running Megatest flows under teamcity
;;
;;  1. Run the megatest process and pass it all the needed parameters
;;  2. Every five seconds check for state/status changes and print the info
;;

(use srfi-1 posix srfi-69 srfi-18 regex defstruct)

(use trace)
;; (trace-call-sites #t)

(declare (uses margs))
(declare (uses rmt))
(declare (uses common))
(declare (uses megatest-version))

(include "megatest-fossil-hash.scm")
(include "db_records.scm")

(define origargs (cdr (argv)))
(define remargs (args:get-args
		 (argv)
		 `( "-target"
		    "-reqtarg"
		    "-runname"
		    "-delay"   ;; how long to wait for unexpected changes to 
		    )
		 `("-tc-repl"
		   )
		 args:arg-hash
		 0))

(defstruct testdat
  (tc-type #f)
  (state   #f)
  (status  #f)
  (overall #f)
  (flowid  #f)
  tctname
  tname
  (event-time #f)
  details
  comment
  duration
  (start-printed #f)
  (end-printed   #f))

;;======================================================================
;; GLOBALS
;;======================================================================

;; Gotta have a global? Stash it in the *global* hash table.
;;
(define *global* (make-hash-table))

(define (tcmt:print tdat flush-mode)
  (let* ((comment  (if (testdat-comment tdat)
		       (conc " message='" (testdat-comment tdat) "'")
		       ""))
	 (details  (if (testdat-details tdat)
		       (conc " details='" (testdat-details tdat) "'")
		       ""))
	 (flowid   (conc " flowId='" (testdat-flowid   tdat) "'"))
	 (duration (conc " duration='" (* 1e3 (testdat-duration tdat)) "'"))
	 (tcname   (conc " name='" (testdat-tctname  tdat) "'"))
	 (state    (string->symbol (testdat-state tdat)))
	 (status   (string->symbol (testdat-status tdat)))
	 (startp   (testdat-start-printed tdat))
	 (endp     (testdat-end-printed   tdat))
	 (etime    (testdat-event-time    tdat))
	 (overall  (case state
		     ((RUNNING)   state)
		     ((COMPLETED) state)
		     (else 'UNK)))
	 (tstmp    (conc " timestamp='" (time->string (seconds->local-time etime) "%FT%T.000") "'")))
    (case overall
      ((RUNNING)
       (if (not startp)
	   (begin
	     (print "##teamcity[testStarted "  tcname flowid tstmp "]")
	     (testdat-start-printed-set! tdat #t))))
      ((COMPLETED)
       (if (not startp) ;; start stanza never printed
	   (begin
	     (print "##teamcity[testStarted " tcname flowid tstmp "]")
	     (testdat-start-printed-set! tdat #t)))
       (if (not endp)
	   (begin
	     (if (not (member status '(PASS WARN SKIP WAIVED)))
		 (print "##teamcity[testFailed  " tcname flowid comment details "]"))
             (print "##teamcity[testFinished" tcname flowid comment details duration "]")
	     (testdat-end-printed-set! tdat #t))))
      (else
       (if flush-mode
	   (begin
	     (if (not startp)
		 (begin
		   (print "##teamcity[testStarted " tcname flowid tstmp "]")
		   (testdat-start-printed-set! tdat #t)))
	     (if (not endp)
		 (begin
		   (print "##teamcity[testFailed  " tcname flowid comment details "]")
                   (print "##teamcity[testFinished" tcname flowid comment details duration "]")
		   (testdat-end-printed-set! tdat #t)))))))
    ;; (print "ERROR: tc-type \"" (testdat-tc-type tdat) "\" not recognised for " tcname)))
    (flush-output)))

;; ;; returns values: flag newlst
;; (define (remove-duplicate-completed  tdats)
;;   (let* ((flag       #f)
;;          (state      (testdat-state      tdat))
;;          (status     (testdat-status     tdat))
;;          (event-time (testdat-event-time tdat))
;;          (tname      (testdat-tname      tdat)))
;;     (let loop ((hed (car tdats))
;;                (tal (cdr tdats))
;;                (new '()))
;;       (if (and (equal? state "COMPLETED")
;;                (equal? tname (testdat-tname hed))
;;                (equal? state (testdat-state hed))) ;; we have a duplicate COMPLETED call
;;           (begin
;;             (set! flag #t) ;; A changed completed
            
;; process the queue of tests gathered so far. List includes one entry for every test so far seen
;; the last record for a test is preserved. Items are only removed from the list if over 15 seconds
;; have passed since it happened. This allows for compression of COMPLETED/FAIL followed by some other
;; state/status
;;
(define (process-queue data age flush-mode)
  ;; here we process tqueue and gather those over 15 seconds (configurable?) old
  (let* ((print-time (- (current-seconds) age)) ;; print stuff over 15 seconds old
         (tqueue-raw (hash-table-ref/default data 'tqueue '()))
         (tqueue     (reverse (delete-duplicates tqueue-raw     ;; REMOVE duplicates by testname and state
                                                 (lambda (a b)
                                                   (and (equal? (testdat-tname a)(testdat-tname b))        ;; need oldest to newest
                                                        (equal? (testdat-state a) (testdat-state b)))))))) ;; "COMPLETED")
    ;; (equal? (testdat-state b) "COMPLETED")))))))
    (if (not (null? tqueue))
        (hash-table-set!
         data
         'tqueue
         (let loop ((hed (car tqueue)) ;; by this point all duplicates by state COMPLETED are removed
                    (tal (cdr tqueue))
                    (rem '()))
           (if (> print-time (testdat-event-time hed)) ;; event happened over 15 seconds ago
               (begin
                 (tcmt:print hed flush-mode)
                 (if (null? tal)
                     rem ;; return rem to be processed in the future
                     (loop (car tal)(cdr tal) rem)))
               (if (null? tal)
                   (cons hed rem) ;; return rem + hed for future processing
                   (loop (car tal)(cdr tal)(cons hed rem)))))))))

                            ;; ##teamcity[testStarted name='suite.testName']
;; ##teamcity[testStdOut name='suite.testName' out='text']
;; ##teamcity[testStdErr name='suite.testName' out='error text']
;; ##teamcity[testFailed name='suite.testName' message='failure message' details='message and stack trace']
;; ##teamcity[testFinished name='suite.testName' duration='50']
;; 
;; flush; #f, normal call. #t, last call, print out something for NOT_STARTED, etc.
;;

;;;;;;;   (begin
;;;;;;;     (case (string->symbol newstat)
;;;;;;;       ((UNK)       ) ;; do nothing
;;;;;;;       ((RUNNING)   (print "##teamcity[testStarted name='" tctname "' flowId='" flowid "']"))
;;;;;;;       ((PASS SKIP WARN WAIVED) (print "##teamcity[testFinished name='" tctname "' duration='" (* 1e3 duration) "'" cmtstr details " flowId='" flowid "']"))
;;;;;;;       (else
;;;;;;; 	(print "##teamcity[testFailed name='" tctname "' " cmtstr details " flowId='" flowid "']")))
;;;;;;;     (flush-output)

;; (trace rmt:get-tests-for-run)

(define (update-queue-since data run-ids last-update tsname target runname flowid flush) ;; 
  (let ((now   (current-seconds)))
;; (handle-exceptions
;; 	exn
;; 	(begin (print-call-chain) (print "Error message: " ((condition-property-accessor 'exn 'message) exn)))
      (for-each
       (lambda (run-id)
	 (let* ((tests (rmt:get-tests-for-run run-id "%" '() '() #f #f #f #f #f #f last-update #f)))
	   ;; (print "DEBUG: got tests=" tests)
	   (for-each
	    (lambda (test-rec)
	      (let* ((tqueue   (hash-table-ref/default data 'tqueue '())) ;; NOTE: the key is a symbol! This allows keeping disparate info in the one hash, lazy but a quick solution for right now.
		     (is-top   (db:test-get-is-toplevel  test-rec))
		     (tname    (db:test-get-fullname     test-rec))
		     (testname (db:test-get-testname     test-rec))
		     (itempath (db:test-get-item-path    test-rec))
		     (tctname  (if (string=? itempath "") testname (conc testname "." (string-translate itempath "/" "."))))
		     (state    (db:test-get-state        test-rec))
		     (status   (db:test-get-status       test-rec))
		     (etime    (db:test-get-event_time   test-rec))
		     (duration (or (any->number (db:test-get-run_duration test-rec)) 0))
		     (comment  (db:test-get-comment      test-rec))
		     (logfile  (db:test-get-final_logf   test-rec))
                     (hostn    (db:test-get-host         test-rec))
                     (pid      (db:test-get-process_id   test-rec))
		     (newstat  (cond
				((equal? state "RUNNING")   "RUNNING")
				((equal? state "COMPLETED") status)
				(flush   (conc state "/" status))
				(else "UNK")))
		     (cmtstr   (if (and (not flush) comment)
				   comment
				   (if flush
				       (conc "Test ended in state/status=" state "/" status  (if  (string-match "^\\s*$" comment)
												  ", no Megatest comment found."
												  (conc ", Megatest comment=\"" comment "\""))) ;; special case, we are handling stragglers
				       #f)))
		     (details  (if (string-match ".*html$" logfile)
				   (conc *toppath* "/lt/" target "/" runname "/" testname (if (equal? itempath "") "/" (conc "/" itempath "/")) logfile)
				   #f))
		     (prev-tdat (hash-table-ref/default data tname #f)) 
		     (tdat      (if is-top
				    #f
				    (let ((new (or prev-tdat (make-testdat)))) ;; recycle the record so we keep track of already printed items
				      (testdat-flowid-set!     new (or (testdat-flowid new)
                                                                       (if (eq? pid 0)
                                                                           tctname
                                                                           (conc hostn "-" pid))))
				      (testdat-tctname-set!    new tctname)
				      (testdat-tname-set!      new tname)
				      (testdat-state-set!      new state)
				      (testdat-status-set!     new status)
				      (testdat-comment-set!    new cmtstr)
				      (testdat-details-set!    new details)
				      (testdat-duration-set!   new duration)
				      (testdat-event-time-set! new etime) ;; (current-seconds))
				      (testdat-overall-set!    new newstat)
				      (hash-table-set! data tname new)
				      new))))
		(if (not is-top)
		    (hash-table-set! data 'tqueue (cons tdat tqueue)))
                (hash-table-set! data tname tdat)
                ))
            tests)))
       run-ids)
      now))
      
(define (monitor pid)
  (let* ((run-ids '())
	 (testdats (make-hash-table))  ;; each entry is a list of testdat structs
	 (keys    #f)
	 (last-update 0)
	 (target  (or (args:get-arg "-target")
		      (args:get-arg "-reqtarg")))
	 (runname (args:get-arg "-runname"))
	 (tsname  #f)
	 (flowid  (conc target "/" runname))
	 (tdelay  (string->number (or (args:get-arg "-delay") "15"))))
    (if (and target runname)
	(begin
	  (launch:setup)
	  (set! keys (rmt:get-keys))))
    (set! tsname  (common:get-testsuite-name))
    (print "TCMT: for testsuite=" tsname " found runname=" runname ", target=" target ", keys=" keys " and successfully ran launch:setup. Using " flowid " as the flowId.")
    (let loop ()
      ;;;;;; (handle-exceptions
      ;;;;;;  exn
      ;;;;;;  ;; (print "Process done.")
      ;;;;;;  (begin (print-call-chain) (print "Error message: " ((condition-property-accessor 'exn 'message) exn)))
       (let-values (((pidres exittype exitstatus)
		     (process-wait pid #t)))
	 (if (and keys
		  (or (not run-ids)
		      (null? run-ids)))
	     (let* ((runs (rmt:get-runs-by-patt keys
						runname 
						target
						#f ;; offset
						#f ;; limit
						#f ;; fields
						0  ;; last-update
						))
		    (header (db:get-header runs))
		    (rows   (db:get-rows   runs))
		    (run-ids-in (map (lambda (row)
				       (db:get-value-by-header row header "id"))
				     rows)))
	       (set! run-ids run-ids-in)))
	 ;; (print "TCMT: pidres=" pidres " exittype=" exittype " exitstatus=" exitstatus " run-ids=" run-ids)
	 (if (eq? pidres 0)
	     (begin
	       (if keys
                   (begin
                     (set! last-update (- (update-queue-since testdats run-ids last-update tsname target runname flowid #f) 5))
                     (process-queue testdats tdelay #f)))
               (thread-sleep! 3)
	       (loop))
	     (begin
	       ;; (print "TCMT: pidres=" pidres " exittype=" exittype " exitstatus=" exitstatus " run-ids=" run-ids)
	       (print "TCMT: processing any tests that did not formally complete.")
	       (update-queue-since testdats run-ids 0 tsname target runname flowid #t) ;; call in flush mode
               (process-queue testdats 0 #t)
	       (print "TCMT: All done.")
	       ))))))
;;;;; )

;; (trace print-changes-since)

;; (if (not (eq? pidres 0))	  ;; (not exitstatus))
;; 	  (begin
;; 	    (thread-sleep! 3)
;; 	    (loop))
;; 	  (print "Process: megatest " (string-intersperse origargs " ") " is done.")))))
(define (main)
  (let* ((mt-done #f)
	 (pid     #f)
	 (th1     (make-thread (lambda ()
				 (print "Running megatest " (string-intersperse origargs " "))
				 (set! pid (process-run "megatest" origargs)))
			       "Megatest job"))
	 (th2     (make-thread (lambda ()
				 (monitor pid))
			       "Monitor job")))
    (thread-start! th1)
    (thread-sleep! 1) ;; give the process time to get going
    (thread-start! th2)
    (thread-join! th2)))

(if (args:get-arg "-tc-repl")
    (repl)
    (main))

;; (process-wait)

