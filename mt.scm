;; Copyright 2006-2013, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.


(use sqlite3 srfi-1 posix regex regex-case srfi-69 dot-locking (srfi 18) posix-extras directory-utils call-with-environment-variables)
(import (prefix sqlite3 sqlite3:))

(declare (unit mt))
(declare (uses db))
(declare (uses common))
(declare (uses items))
(declare (uses runconfig))
(declare (uses tests))
(declare (uses server))
(declare (uses runs))
(declare (uses rmt))
;; (declare (uses filedb))

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
;; Use: (db-get-value-by-header (db:get-header runinfo)(db:get-rows runinfo))
;;  to extract info from the structure returned
;;
(define (mt:get-runs-by-patt keys runnamepatt targpatt)
  (let loop ((runsdat  (rmt:get-runs-by-patt keys runnamepatt targpatt 0 500 #f))
	     (res      '())
	     (offset   0)
	     (limit    500))
    ;; (print "runsdat: " runsdat)
    (let* ((header    (vector-ref runsdat 0))
	   (runslst   (vector-ref runsdat 1))
	   (full-list (append res runslst))
	   (have-more (eq? (length runslst) limit)))
      ;; (debug:print 0 #f "header: " header " runslst: " runslst " have-more: " have-more)
      (if have-more 
	  (let ((new-offset (+ offset limit))
		(next-batch (rmt:get-runs-by-patt keys runnamepatt targpatt offset limit #f)))
	    (debug:print-info 4 #f "More than " limit " runs, have " (length full-list) " runs so far.")
	    (debug:print-info 0 #f "next-batch: " next-batch)
	    (loop next-batch
		  full-list
		  new-offset
		  limit))
	 (vector header full-list)))))

;;======================================================================
;;  T E S T S
;;======================================================================

(define (mt:get-tests-for-run run-id testpatt states status #!key (not-in #t) (sort-by 'event_time) (sort-order "ASC") (qryvals #f)(last-update #f))
  (let loop ((testsdat (rmt:get-tests-for-run run-id testpatt states status 0 500 not-in sort-by sort-order qryvals last-update 'normal))
	     (res      '())
	     (offset   0)
	     (limit    500))
    (let* ((full-list (append res testsdat))
	   (have-more (eq? (length testsdat) limit)))
      (if have-more 
	  (let ((new-offset (+ offset limit)))
	    (debug:print-info 4 #f "More than " limit " tests, have " (length full-list) " tests so far.")
	    (loop (rmt:get-tests-for-run run-id testpatt states status new-offset limit not-in sort-by sort-order qryvals last-update 'normal)
		  full-list
		  new-offset
		  limit))
	  full-list))))

(define (mt:lazy-get-prereqs-not-met run-id waitons ref-item-path #!key (mode '(normal))(itemmaps #f) )
  (let* ((key    (list run-id waitons ref-item-path mode))
	 (res    (hash-table-ref/default *pre-reqs-met-cache* key #f))
	 (useres (let ((last-time (if (vector? res) (vector-ref res 0) #f)))
		   (if last-time
		       (< (current-seconds)(+ last-time 5))
		       #f))))
    (if useres
	(let ((result (vector-ref res 1)))
	  (debug:print 4 #f "Using lazy value res: " result)
	  result)
	(let ((newres (rmt:get-prereqs-not-met run-id waitons ref-item-path mode: mode itemmaps: itemmaps)))
	  (hash-table-set! *pre-reqs-met-cache* key (vector (current-seconds) newres))
	  newres))))

(define (mt:get-run-stats dbstruct run-id)
;;  Get run stats from local access, move this ... but where?
  (db:get-run-stats dbstruct run-id))

(define (mt:discard-blocked-tests run-id failed-test tests test-records)
  (if (null? tests)
      tests
      (begin
	(debug:print-info 1 #f "Discarding tests from " tests " that are waiting on " failed-test)
	(let loop ((testn (car tests))
		   (remt  (cdr tests))
		   (res   '()))
	  (let* ((test-dat (hash-table-ref/default test-records testn (vector #f #f '())))
		 (waitons  (vector-ref test-dat 2)))
	    ;; (print "mt:discard-blocked-tests run-id: " run-id " failed-test: " failed-test " testn: " testn " with waitons: " waitons)
	    (if (null? remt)
		(let ((new-res (reverse res)))
		  ;; (print "       new-res: " new-res)
		  new-res)
		(loop (car remt)
		      (cdr remt)
		      (if (member failed-test waitons)
			  (begin
			    (debug:print 0 #f "Discarding test " testn "(" test-dat ") due to " failed-test)
			    res)
			  (cons testn res)))))))))

;;======================================================================
;;  T R I G G E R S
;;======================================================================

(define (mt:process-triggers run-id test-id newstate newstatus)
  (let* ((test-dat      (rmt:get-test-info-by-id run-id test-id)))
    (if test-dat
	(let* ((test-rundir   ;; (rmt:sdb-qry 'getstr ;; (filedb:get-path *fdb*
		(db:test-get-rundir test-dat)) ;; ) ;; )
	       (test-name     (db:test-get-testname test-dat))
	       (tconfig       #f)
	       (state         (if newstate  newstate  (db:test-get-state  test-dat)))
	       (status        (if newstatus newstatus (db:test-get-status test-dat))))
	  (if (and test-name
		   test-rundir   ;; #f means no dir set yet
		   (file-exists? test-rundir)
		   (directory? test-rundir))
	      (call-with-environment-variables
	       (list (cons "MT_TEST_NAME" test-name)
		     (cons "MT_TEST_RUN_DIR" test-rundir)
		     (cons "MT_ITEMPATH"     (db:test-get-item-path test-dat)))
	       (lambda ()
		 (push-directory test-rundir)
		 (set! tconfig (mt:lazy-read-test-config test-name))
		 (for-each (lambda (trigger)
			     (let ((cmd  (configf:lookup tconfig "triggers" trigger))
				   (logf (conc  test-rundir "/last-trigger.log")))
			       (if cmd
				   ;; Putting the commandline into ( )'s means no control over the shell. 
				   ;; stdout and stderr will be caught in the NBFAKE or mt_launch.log files
				   ;; or equivalent. No need to do this. Just run it?
				   (let ((fullcmd (conc cmd " " test-id " " test-rundir " " trigger "&")))
				     (debug:print-info 0 #f "TRIGGERED on " trigger ", running command " fullcmd)
				     (process-run fullcmd)))))
			   (list
			    (conc state "/" status)
			    (conc state "/")
			    (conc "/" status)))
		 (pop-directory))
	       ))))))

;;======================================================================
;;  S T A T E   A N D   S T A T U S   F O R   T E S T S 
;;======================================================================

;; speed up for common cases with a little logic
(define (mt:test-set-state-status-by-id run-id test-id newstate newstatus newcomment)
  (if (not (and run-id test-id))
      (begin
	(debug:print 0 #f "ERROR: bad data handed to mt:test-set-state-status-by-id, run-id=" run-id ", test-id=" test-id ", newstate=" newstate)
	(print-call-chain (current-error-port))
	#f)
      (begin
	(cond
	 ((and newstate newstatus newcomment)
	  (rmt:general-call 'state-status-msg run-id newstate newstatus newcomment test-id))
	 ((and newstate newstatus)
	  (rmt:general-call 'state-status run-id newstate newstatus test-id))
	 (else
	  (if newstate   (rmt:general-call 'set-test-state   run-id newstate   test-id))
	  (if newstatus  (rmt:general-call 'set-test-status  run-id newstatus  test-id))
	  (if newcomment (rmt:general-call 'set-test-comment run-id newcomment test-id))))
	(mt:process-triggers run-id test-id newstate newstatus)
	#t)))

(define (mt:test-set-state-status-by-testname run-id test-name item-path new-state new-status new-comment)
  (let ((test-id (rmt:get-test-id run-id test-name item-path)))
    (mt:test-set-state-status-by-id run-id test-id new-state new-status new-comment)))

(define (mt:lazy-read-test-config test-name)
  (let ((tconf (hash-table-ref/default *testconfigs* test-name #f)))
    (if tconf
	tconf
	(let ((test-dirs (tests:get-tests-search-path *configdat*)))
	  (let loop ((hed (car test-dirs))
		     (tal (cdr test-dirs)))
	    ;; Setting MT_LINKTREE here is almost certainly unnecessary. 
	    (let ((tconfig-file (conc hed "/" test-name "/testconfig")))
	      (if (and (file-exists? tconfig-file)
		       (file-read-access? tconfig-file))
		  (let ((link-tree-path (configf:lookup *configdat* "setup" "linktree"))
			(old-link-tree  (get-environment-variable "MT_LINKTREE")))
		    (if link-tree-path (setenv "MT_LINKTREE" link-tree-path))
		    (let ((newtcfg (read-config tconfig-file #f #f))) ;; NOTE: Does NOT run [system ...]
		      (hash-table-set! *testconfigs* test-name newtcfg)
		      (if old-link-tree 
			  (setenv "MT_LINKTREE" old-link-tree)
			  (unsetenv "MT_LINKTREE"))
		      newtcfg))
		  (if (null? tal)
		      (begin
			(debug:print 0 #f "ERROR: No readable testconfig found for " test-name)
			#f)
		      (loop (car tal)(cdr tal))))))))))

