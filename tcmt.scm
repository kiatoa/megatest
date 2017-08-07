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
  (tc-type #f)
  (state   #f)
  (status  #f)
  (overall #f)
  flowid
  tctname
  tname
  (event-time #f)
  details
  comment
  duration)

(define (tcmt:print tdat)
  (let* ((comment  (if (testdat-comment tdat)
		       (conc " message='" (testdat-comment tdat))
		       ""))
	 (details  (if (testdat-details tdat)
		       (conc " details='" (testdat-details tdat))
		       ""))
	 (flowid   (conc " flowId='" (testdat-flowid   tdat) "'"))
	 (duration (conc " duration='" (* 1e3 (testdat-duration tdat)) "'"))
	 (tcname   (conc " name='" (testdat-tctname  tdat) "'")))
    (case (string->symbol (testdat-overall tdat)) ;; (testdat-tc-type tdat)
      ((RUNNING)      (print "##teamcity[testStarted "  tcname flowid "]"))
      ((COMPLETED)
       (if (member (testdat-status tdat) '("PASS" "WARN" "SKIP" "WAIVED"))
	   (print "##teamcity[testFinished " tcname flowid comment details duration "]")
	   (print "##teamcity[testFailed "   tcname flowid comment details "]")))
      ((ignore)        #f)
      (else            (print "ERROR: tc-type \"" (testdat-tc-type tdat) "\" not recognised for " tcname)))
    (flush-output)))

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

(define (print-changes-since data run-ids last-update tsname target runname flowid flush) ;; 
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
		     (tname    (db:test-get-fullname     test-rec))
		     (testname (db:test-get-testname     test-rec))
		     (itempath (db:test-get-item-path    test-rec))
		     (tctname  (if (string=? itempath "") testname (conc testname "." (string-translate itempath "/" "."))))
		     (state    (db:test-get-state        test-rec))
		     (status   (db:test-get-status       test-rec))
		     (duration (or (any->number (db:test-get-run_duration test-rec)) 0))
		     (comment  (db:test-get-comment      test-rec))
		     (logfile  (db:test-get-final_logf   test-rec))
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
		     (tdat      (let ((new (make-testdat)))
				  (testdat-flowid-set!     new flowid)
				  (testdat-tctname-set!    new tctname)
				  (testdat-tname-set!      new tname)
				  (testdat-comment-set!    new cmtstr)
				  (testdat-details-set!    new details)
				  (testdat-duration-set!   new duration)
				  (testdat-event-time-set! new (current-seconds))
				  (testdat-overall-set!    new newstat)
				  (hash-table-set! data tname new)
				  new))
		     (prevstat (if prev-tdat (testdat-overall prev-tdat) #f)))
		;; (print "DEBUG: tname=" tname " state=" state " status=" status " prevstat=" prevstat " newstat=" newstat)
		(if (or (not prevstat)
			(not (equal? prevstat newstat)))
		    ;; save this in the queue
		    (begin
		      (hash-table-set! data 'tqueue (cons tdat tqueue))
		      (hash-table-set! data tname tdat))))) ;; newstat
	    tests)))
       run-ids)
      ;; )

    ;; here we process tqueue and gather those over 15 seconds (configurable?) old
    (let* ((tqueue (hash-table-ref/default data 'tqueue '()))
	   (pqueue (make-hash-table)) ;; ( tname => ( tdat1 tdat2 ... ) ... )
	   (targtm (- now 15))
	   (leftov (if (null? tqueue)
		       '()
		       (let loop ((hed (car tqueue))
				  (tal  (cdr tqueue))
				  (new '()))
			 (let ((event-time (testdat-event-time hed))
			       (tname      (testdat-tname      hed)))
			   (if (> event-time targtm) ;; print this one
			       (begin
				 (hash-table-set! pqueue tname (cons hed (hash-table-ref/default pqueue tname '()))))
			       (if (null? tal)
				   new
				   (loop (car tal)(cdr tal) new)))
			   (if (null? tal)
			       new
			       (loop (car tal)(cdr tal)(cons hed new))))))))
      ;; Now have:
      ;;   leftov - items not processed, put these back in tqueue
      ;;   pqueue - candidates for printing. print but filter for COMPLETED changes
      (hash-table-set! data 'tqueue leftov)
      (hash-table-for-each
       pqueue
       (lambda (k v) ;; v is a list of tdats, if only one, just print it!
	 (if (eq? (length v) 1)
	     (tcmt:print (car v))
	     (begin
	       (print "MULTI: " v)
	       (for-each
		(lambda (tdat)
		  (tcmt:print tdat))
		v))))))
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
	 (flowid  (conc target "/" runname)))
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
		   (set! last-update (print-changes-since testdats run-ids last-update tsname target runname flowid #f)))
	       (thread-sleep! 3)
	       (loop))
	     (begin
	       ;; (print "TCMT: pidres=" pidres " exittype=" exittype " exitstatus=" exitstatus " run-ids=" run-ids)
	       (print "TCMT: processing any tests that did not formally complete.")
	       (print-changes-since testdats run-ids 0 tsname target runname flowid #t) ;; call in flush mode
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

