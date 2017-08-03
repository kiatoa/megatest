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
		    )
		 `("-tc-repl"
		   )
		 args:arg-hash
		 0))

(defstruct testdat
  tc-type
  state
  status
  flowid
  tctname
  event_time
  details
  comment
  duration)

(define (tcmt:print tdat)
  (let ((comment (if (testdat-comment tdat)
                     (conc " message='" (testdat-comment tdat))
                     ""))
        (details (if (testdat-details tdat)
                     (conc " details='" (testdat-details tdat))
                     "")))
    (case (testdat-tc-type tdat)
      ((test-start)
       (print "##teamcity[testStarted name='" (testdat-tctname tdat) "' flowId='" (testdat-flowid tdat) "']"))
      ((test-end)
       (print "##teamcity[testFinished name='" (testdat-tctname tdat) "' duration='" (* 1e3 (testdat-duration tdat)) "'"
              comment
              details
              " flowId='" flowid "']"))
      ((test-failed)
       (print "##teamcity[testFailed name='" (testdat-tctname tdat) "' " comment details " flowId='" flowid "']")))))

;; ##teamcity[testStarted name='suite.testName']
;; ##teamcity[testStdOut name='suite.testName' out='text']
;; ##teamcity[testStdErr name='suite.testName' out='error text']
;; ##teamcity[testFailed name='suite.testName' message='failure message' details='message and stack trace']
;; ##teamcity[testFinished name='suite.testName' duration='50']
;; 
;; flush; #f, normal call. #t, last call, print out something for NOT_STARTED, etc.
;;
(define (print-changes-since data run-ids last-update tsname target runname flowid flush) ;; 
  (let ((now   (current-seconds)))
    (handle-exceptions
     exn
     (begin (print-call-chain) (print "Error message: " ((condition-property-accessor 'exn 'message) exn)))
     (for-each
      (lambda (run-id)
	(let* ((tests (rmt:get-tests-for-run run-id "%" '() '() #f #f #f #f #f #f last-update #f)))
	  ;; (print "DEBUG: got tests=" tests)
	  (for-each
	   (lambda (testdat)
	     (let* ((testn    (db:test-get-fullname     testdat))
		    (testname (db:test-get-testname     testdat))
		    (itempath (db:test-get-item-path    testdat))
		    (tctname  (if (string=? itempath "") testname (conc testname "." (string-translate itempath "/" "."))))
		    (state    (db:test-get-state        testdat))
		    (status   (db:test-get-status       testdat))
		    (duration (or (any->number (db:test-get-run_duration testdat)) 0))
		    (comment  (db:test-get-comment      testdat))
		    (logfile  (db:test-get-final_logf   testdat))
		    (prevstat (hash-table-ref/default data testn #f))
		    (newstat  (cond
			       ((equal? state "RUNNING")   "RUNNING")
			       ((equal? state "COMPLETED") status)
			       (flush   (conc state "/" status))
			       (else "UNK")))
		    (cmtstr   (if (and (not flush) comment)
				  (conc " message='" comment "' ")
				  (if flush
				      (conc "message='Test ended in state/status=" state "/" status  (if  (string-match "^\\s*$" comment)
													  ", no Megatest comment found.' "
													  (conc ", Megatest comment='" comment "' "))) ;; special case, we are handling stragglers
				      " ")))
		    (details  (if (string-match ".*html$" logfile)
				  (conc " details='" *toppath* "/lt/" target "/" runname "/" testname (if (equal? itempath "") "/" (conc "/" itempath "/")) logfile "' ")
				  "")))
		    
	       ;; (print "DEBUG: testn=" testn " state=" state " status=" status " prevstat=" prevstat " newstat=" newstat)
	       (if (or (not prevstat)
		       (not (equal? prevstat newstat)))
		   (begin
		     (case (string->symbol newstat)
		       ((UNK)       ) ;; do nothing
		       ((RUNNING)   (print "##teamcity[testStarted name='" tctname "' flowId='" flowid "']"))
		       ((PASS SKIP WARN WAIVED) (print "##teamcity[testFinished name='" tctname "' duration='" (* 1e3 duration) "'" cmtstr details " flowId='" flowid "']"))
		       (else
			(print "##teamcity[testFailed name='" tctname "' " cmtstr details " flowId='" flowid "']")))
		     (flush-output)
		     (hash-table-set! data testn newstat)))))
	   tests)))
      run-ids))
    now))

(define (monitor pid)
  (let* ((run-ids #f)
	 (testdat (make-hash-table))
	 (keys    #f)
	 (last-update 0)
	 (target  (or (args:get-arg "-target")
		      (args:get-arg "-reqtarg")))
	 (runname (args:get-arg "-runname"))
	 (tsname  #f)
	 (flowid  (conc target "/" runname)))
    (if (and target runname)
	(begin
	  (launch:setup)
	  (set! keys (rmt:get-keys))))
    (set! tsname  (common:get-testsuite-name))
    (print "TCMT: for testsuite=" tsname " found runname=" runname ", target=" target ", keys=" keys " and successfully ran launch:setup. Using " flowid " as the flowId.")
    (let loop ()
      (handle-exceptions
       exn
       ;; (print "Process done.")
       (begin (print-call-chain) (print "Error message: " ((condition-property-accessor 'exn 'message) exn)))
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
		   (set! last-update (print-changes-since testdat run-ids last-update tsname target runname flowid #f)))
	       (thread-sleep! 3)
	       (loop))
	     (begin
	       ;; (print "TCMT: pidres=" pidres " exittype=" exittype " exitstatus=" exitstatus " run-ids=" run-ids)
	       (print "TCMT: processing any tests that did not formally complete.")
	       (print-changes-since testdat run-ids 0 tsname target runname flowid #t) ;; call in flush mode
	       (print "TCMT: All done.")
	       )))))))

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

