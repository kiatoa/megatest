;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.


(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking (srfi 18) posix-extras directory-utils)
(import (prefix sqlite3 sqlite3:))

(declare (unit mt))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
(declare (uses tests))
(declare (uses server))
(declare (uses runs))

(include "common_records.scm")
(include "key_records.scm")
(include "db_records.scm")
(include "run_records.scm")
(include "test_records.scm")

;; This is the Megatest API. All generally "useful" routines will be wrapped or extended
;; here.

;;======================================================================
;;  R U N S
;;======================================================================

;; runs:get-runs-by-patt
;; get runs by list of criteria
;; register a test run with the db
;;
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-row runinfo))
;;  to extract info from the structure returned
;;
(define (mt:get-runs-by-patt keys runnamepatt targpatt)
  (let loop ((runsdat  (cdb:remote-run db:get-runs-by-patt #f keys runnamepatt targpatt 0 500))
	     (res      '())
	     (offset   0)
	     (limit    500))
    ;; (print "runsdat: " runsdat)
    (let* ((header    (vector-ref runsdat 0))
	   (runslst   (vector-ref runsdat 1))
	   (full-list (append res runslst))
	   (have-more (eq? (length runslst) limit)))
      ;; (debug:print 0 "header: " header " runslst: " runslst " have-more: " have-more)
      (if have-more 
	  (let ((new-offset (+ offset limit))
		(next-batch (cdb:remote-run db:get-runs-by-patt #f keys runnamepatt targpatt offset limit)))
	    (debug:print-info 4 "More than " limit " runs, have " (length full-list) " runs so far.")
	    (debug:print-info 0 "next-batch: " next-batch)
	    (loop next-batch
		  full-list
		  new-offset
		  limit))
	 (vector header full-list)))))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (mt:get-tests-for-run run-id testpatt states status #!key (not-in #t) (sort-by 'event_time) (sort-order "ASC") (qryvals #f))
  (let loop ((testsdat (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status 0 500 not-in sort-by sort-order qryvals: qryvals))
	     (res      '())
	     (offset   0)
	     (limit    500))
    (let* ((full-list (append res testsdat))
	   (have-more (eq? (length testsdat) limit)))
      (if have-more 
	  (let ((new-offset (+ offset limit)))
	    (debug:print-info 4 "More than " limit " tests, have " (length full-list) " tests so far.")
	    (loop (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status new-offset limit not-in sort-by sort-order qryvals: qryvals)
		  full-list
		  new-offset
		  limit))
	  full-list))))

(define (mt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode 'normal))
  (db:get-prereqs-not-met run-id waitons ref-item-path mode: mode))

(define (mt:get-run-stats)
  (cdb:remote-run db:get-run-stats #f))

(define (mt:discard-blocked-tests run-id failed-test tests test-records)
  (if (null? tests)
      tests
      (begin
	(debug:print-info 1 "Discarding tests from " tests " that are waiting on " failed-test)
	(let loop ((testn (car tests))
		   (remt  (cdr tests))
		   (res   '()))
	  (let ((waitons (vector-ref (hash-table-ref/default test-records testn (vector #f #f '())) 2)))
	    ;; (print "mt:discard-blocked-tests run-id: " run-id " failed-test: " failed-test " testn: " testn " with waitons: " waitons)
	    (if (null? remt)
		(let ((new-res (reverse res)))
		  ;; (print "       new-res: " new-res)
		  new-res)
		(loop (car remt)
		      (cdr remt)
		      (if (member failed-test waitons)
			  res
			  (cons testn res)))))))))

;;======================================================================
;;  T R I G G E R S
;;======================================================================

(define (mt:process-triggers test-id newstate newstatus)
  (let* ((test-dat      (mt:lazy-get-test-info-by-id test-id))
	 (test-rundir   (db:test-get-rundir test-dat))
	 (test-name     (db:test-get-testname test-dat))
	 (tconfig       #f)
	 (state         (if newstate  newstate  (db:test-get-state  test-dat)))
	 (status        (if newstatus newstatus (db:test-get-status test-dat))))
    (if (and (file-exists? test-rundir)
	     (directory? test-rundir))
	(begin
	  (push-directory test-rundir)
	  (set! tconfig (mt:lazy-read-test-config test-name))
	  (pop-directory)
	  (for-each (lambda (trigger)
		      (let ((cmd  (configf:lookup tconfig "triggers" trigger))
			    (logf (conc  test-rundir "/last-trigger.log")))
			(if cmd
			    ;; Putting the commandline into ( )'s means no control over the shell. 
			    ;; stdout and stderr will be caught in the NBFAKE or mt_launch.log files
			    ;; or equivalent. No need to do this. Just run it?
			    (let ((fullcmd (conc cmd " " test-id " " test-rundir " " trigger "&")))
			      (debug:print-info 0 "TRIGGERED on " trigger ", running command " fullcmd)
			      (process-run fullcmd)))))
		    (list
		     (conc state "/" status)
		     (conc state "/")
		     (conc "/" status)))))))

;;======================================================================
;;  S T A T E   A N D   S T A T U S   F O R   T E S T S 
;;======================================================================

(define (mt:roll-up-pass-fail-counts run-id test-name item-path status)
  (if (and (not (equal? item-path ""))
	   (member status '("PASS" "WARN" "FAIL" "WAIVED" "RUNNING" "CHECK" "SKIP")))
      (begin
	(cdb:update-pass-fail-counts *runremote* run-id test-name)
	(if (equal? status "RUNNING")
	    ;; This test is RUNNING, if the top test is not set to RUNNING then set it to RUNNING
	    (let ((state-status (cdb:remote-run db:test-get-state-status #f run-id test-name '')))
	      (if (not (equal? (vector-ref state-status 1) "RUNNING"))
		  (cdb:top-test-set-running *runremote* run-id test-name)))
	    ;; This following is a "big" query. Replace it with the multi-step sequence
	    ;; The fact that the replacement is not ACID may be a concern.
	    ;; (cdb:top-test-set-per-pf-counts *runremote* run-id test-name))
	    (let* ((num-running       0)
		   (num-items-running (cdb:remote-run db:get-count-test-items-running #f run-id test-name))
		   (num-items-skip    (cdb:remote-run db:get-count-test-items-matching-status #f run-id test-name "SKIP"))
		   (new-state         (if (> num-items-running 0) "RUNNING" "COMPLETED"))
		   (testinfo          (cdb:remote-run db:test-get-id-state-status-pass-fail-count #f testname ''))
		   (curr-state        (vector-ref testinfo 2))
		   (curr-status       (vector-ref testinfo 3))
		   (pcount            (vector-ref testinfo 4))
		   (fcount            (vector-ref testinfo 5))
		   (newstatus         #f))
	      (set! newstatus
		    (cond
		     ((> fcount 0)         "FAIL")
		     ((> num-items-skip 0) "SKIP")
		     ((> pass-count 0)     "PASS")))
	      (if (or (not (equal? curr-state new-state))
		      (not (equal? curr-status new-status)))
		  (cdb:test-set-state-status-by-name serverdat  status state msg)))))
	#f)
      #f)

;; speed up for common cases with a little logic
(define (mt:test-set-state-status-by-id test-id newstate newstatus newcomment)
  (cond
   ((and newstate newstatus newcomment)
    (cdb:client-call *runremote* 'state-status-msg #t *default-numtries* newstate newstatus newcomment test-id))
   ((and newstate newstatus)
    (cdb:client-call *runremote* 'state-status #t *default-numtries* newstate newstatus test-id))
   (else
    (if newstate   (cdb:client-call *runremote* 'set-test-state #t *default-numtries* newstate test-id))
    (if newstatus  (cdb:client-call *runremote* 'set-test-status #t *default-numtries* newstatus test-id))
    (if newcomment (cdb:client-call *runremote* 'set-test-comment #t *default-numtries* newcomment test-id))))
   (mt:process-triggers test-id newstate newstatus)
   #t)

(define (mt:lazy-get-test-info-by-id test-id)
  (let* ((tdat (hash-table-ref/default *test-info* test-id #f)))
    (if (and tdat 
	     (< (current-seconds)(+ (vector-ref tdat 0) 10)))
	(vector-ref tdat 1)
	;; no need to update *test-info* as that is done in cdb:get-test-info-by-id
	(cdb:get-test-info-by-id *runremote* test-id))))

(define (mt:lazy-read-test-config test-name)
  (let ((tconf (hash-table-ref/default *testconfigs* test-name #f)))
    (if tconf
	tconf
	(let ((test-dirs (tests:get-tests-search-path *configdat*)))
	  (let loop ((hed (car test-dirs))
		     (tal (cdr test-dirs)))
	    (let ((tconfig-file (conc hed "/" test-name "/testconfig")))
	      (if (and (file-exists? tconfig-file)
		       (file-read-access? tconfig-file))
		  (let ((newtcfg (read-config tconfig-file #f #f))) ;; NOTE: Does NOT run [system ...]
		    (hash-table-set! *testconfigs* test-name newtcfg)
		    newtcfg)
		  (if (null? tal)
		      (begin
			(debug:print 0 "ERROR: No readable testconfig found for " test-name)
			#f)
		      (loop (car tal)(cdr tal))))))))))

