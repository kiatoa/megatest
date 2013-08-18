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

(define (mt:get-tests-for-run run-id testpatt states status #!key (not-in #t) (sort-by #f) (qryvals #f))
  (let loop ((testsdat (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status 0 500 not-in sort-by qryvals: qryvals))
	     (res      '())
	     (offset   0)
	     (limit    500))
    (let* ((full-list (append res testsdat))
	   (have-more (eq? (length testsdat) limit)))
      (if have-more 
	  (let ((new-offset (+ offset limit)))
	    (debug:print-info 4 "More than " limit " tests, have " (length full-list) " tests so far.")
	    (loop (cdb:remote-run db:get-tests-for-run #f run-id testpatt states status new-offset limit not-in sort-by qryvals: qryvals)
		  full-list
		  new-offset
		  limit))
	  full-list))))

(define (mt:get-prereqs-not-met run-id waitons ref-item-path #!key (mode 'normal))
  (db:get-prereqs-not-met run-id waitons ref-item-path mode: mode))

(define (mt:get-run-stats)
  (cdb:remote-run db:get-run-stats #f))

;;======================================================================
;;  T R I G G E R S
;;======================================================================

(define (mt:process-triggers test-id newstate newstatus)
  (let* ((test-dat      (mt:lazy-get-test-info-by-id test-id))
	 (test-rundir   (db:test-get-rundir test-dat))
	 (tconfig       #f))
    (if (and (file-exists? test-rundir)
	     (directory? test-rundir))
	(begin
	  (push-directory test-rundir)
	  (set! tconfig (mt:lazy-read-test-config test-dat))
	  (pop-directory)
	  (for-each (lambda (trigger)
		      (let ((cmd  (configf:lookup tconfig "triggers" trigger)))
			(if cmd
			    (system (conc cmd " " test-id " " test-rundir " " trigger " 2&>1 " test-rundir "/last-trigger.log")))))
		    (list
		     (conc newstate "/" newstatus)
		     (conc newstate "/")
		     (conc "/" newstatus)))))))
    
;;======================================================================
;;  S T A T E   A N D   S T A T U S   F O R   T E S T S 
;;======================================================================

(define (mt:roll-up-pass-fail-counts run-id test-name item-path status)
  (if (and (not (equal? item-path ""))
	   (member status '("PASS" "WARN" "FAIL" "WAIVED" "RUNNING" "CHECK" "SKIP")))
      (begin
	(cdb:update-pass-fail-counts *runremote* run-id test-name)
	(if (equal? status "RUNNING")
	    (cdb:top-test-set-running *runremote* run-id test-name)
	    (cdb:top-test-set-per-pf-counts *runremote* run-id test-name))
	#f)
      #f))

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
  (let ((tdat (hash-table-ref/default *test-info* test-id #f)))
    (if tdat 
	tdat
	;; no need to update *test-info* as that is done in cdb:get-test-info-by-id
	(cdb:get-test-info-by-id *runremote* test-id))))

(define (mt:lazy-read-test-config test-dat)
  (let* ((test-id     (db:test-get-id test-dat))
	 (test-rundir (db:test-get-rundir test-dat))
	 (tconfig     (hash-table-ref/default *testconfigs* test-id #f)))
    (if tconfig 
	tconfig
	(let ((newtcfg (read-config (conc test-rundir "/testconfig") #f #f))) ;; NOTE: Does NOT run [system ...]
	  (hash-table-set! *testconfigs* test-id newtcfg)
	  newtcfg))))

