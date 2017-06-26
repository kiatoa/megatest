(use posix)

(include "db.scm")

;; define following in setup.scm
;;    *remotehost*  => host for "tests"
;;    *homehost*    => host for servers
;;    *homepath*    => directory from which to run
;;    *numtests*    => how many tests to simulate for each run
;;    *numruns*     => how many runs to simulate
;;    
(include "setup.scm")

(include "direct.scm") ;; direct db calls

;; RUN A TEST
(define (run-test dbconn run-id test-name)
  (rmt:create-test dbconn run-id test-name)
  (let ((test-id (rmt:get-test-id dbconn run-id test-name)))
    (rmt:test-set-state-status dbconn test-id "LAUNCHED" "na")
    (thread-sleep! *launchdelay*)
    (rmt:test-set-state-status dbconn test-id "RUNNING" "na")
    (let loop ((step-num 0))
      (let ((step-name (conc "step" step-num)))
       (rmt:create-step dbconn test-id step-name)
       (let ((step-id (get-step-id dbconn test-id step-name)))
	 (rmt:step-set-state-status dbconn step-id "START" -1)
	 (thread-sleep! *stepdelay*)
	 (rmt:step-set-state-status dbconn step-id "END" 0)
	 (print"   STEP: " step-name " done.")))
      (if (< step-num *numsteps*)
	  (loop (+ step-num 1))))
    (rmt:test-set-state-status dbconn test-id "COMPLETED" (if (> (random 10) 2) "PASS" "FAIL"))
    (print "TEST: " test-name " done.")
    test-id))

;; RUN A RUN
(define (run-run dbconn target run-name num-tests)
  (rmt:create-run dbconn target run-name)
  (let ((run-id (rmt:get-run-id dbconn target run-name)))
    (let loop ((test-num 0))
      (system (conc "NBFAKE_LOG=test-" test-num "-run-id-" run-id ".log NBFAKE_HOST=" *remotehost* " nbfake minimt runtest " run-id " test-" test-num))
      (if (< test-num num-tests)
	  (loop (+ test-num 1))))))

;; Do what is asked
(let ((args (cdr (argv))))
  (if (< (length args) 1)
      (print
       "Usage: minimt [options]" "
  runtest run-id testname
  runrun  target runname")
      (let ((cmd    (car args))
	    (dbconn (rmt:open-create-db *homepath* "mt.db" init-db)))
	(change-directory *homepath*)
	(case (string->symbol cmd)
	  ((runtest)
	   (let ((run-id    (string->number (cadr args)))
		 (test-name (caddr args)))
	     (print "Launching test " test-name " for run-id " run-id)
	     (run-test dbconn run-id test-name)))
	  ((runrun)
	   (let ((target   (cadr args))
		 (run-name (caddr args)))
	     (run-run dbconn target run-name *numtests*)
	     (print "Use: sqlite3 runtest/mt.db 'select max(end_time)-min(start_time) from tests;' to see the total run time")
	     ))
	  ((runall)
	   (for-each
	    (lambda (target)
	      (let loop ((run-num 0))
		(thread-sleep! *rundelay*)
		(system (conc "NBFAKE_LOG=run-" target "-" run-num ".log nbfake minimt runrun " target " run-" run-num))
		(if (< run-num *numruns*)
		    (loop (+ run-num 1)))))
	    *targets*))
	  (else
	   (print "Command: " cmd " not recognised. Run without params to see help.")))
	(close-database (dbconn-dat-dbh dbconn)))))
